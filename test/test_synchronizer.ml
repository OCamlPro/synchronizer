(* SPDX-License-Identifier: AGPL-3.0-or-later *)

(** Unit tests for the Synchronizer library using Alcotest *)

(** Helper functions to reduce duplication *)

(** Create a simple queue-based synchronizer. *)
let make_queue_sync () =
  let queue = Queue.create () in
  let getter () =
    if Queue.is_empty queue then None else Some (Queue.pop queue)
  in
  let writer v = Queue.push v queue in
  Synchronizer.init getter writer

(** Create a thread-safe counter worker *)
let make_counter_worker count mutex sync =
  Synchronizer.work_while
    (fun _v _ ->
      Mutex.lock mutex;
      count := !count + 1;
      Mutex.unlock mutex )
    sync

(** Spawn and join multiple domains *)
let run_domains workers =
  let domains = Array.map Domain.spawn workers in
  Array.iter Domain.join domains

(** Basic operations tests *)

let test_empty_queue_no_pledges () =
  let sync = make_queue_sync () in
  let result = Synchronizer.get ~pledge:false sync in
  Alcotest.(check (option int)) "empty queue returns None" None result

let test_get_single_item () =
  let sync = make_queue_sync () in
  Synchronizer.write sync 42;
  let result = Synchronizer.get ~pledge:false sync in
  Alcotest.(check (option int)) "get returns Some 42" (Some 42) result

let test_get_multiple_items () =
  let sync = make_queue_sync () in
  List.iter (fun i -> Synchronizer.write sync i) [ 1; 2; 3 ];

  let r1 = Synchronizer.get ~pledge:false sync in
  let r2 = Synchronizer.get ~pledge:false sync in
  let r3 = Synchronizer.get ~pledge:false sync in

  Alcotest.(check (option int)) "first item" (Some 1) r1;
  Alcotest.(check (option int)) "second item" (Some 2) r2;
  Alcotest.(check (option int)) "third item" (Some 3) r3

(** Pledge mechanics tests *)

let test_manual_pledge () =
  let sync = make_queue_sync () in

  Synchronizer.new_pledge sync;
  Synchronizer.write sync 1;

  let result = Synchronizer.get ~pledge:true sync in
  Alcotest.(check (option int)) "get with pledge returns Some" (Some 1) result;

  (* End both pledges *)
  Synchronizer.end_pledge sync;
  Synchronizer.end_pledge sync;

  let result = Synchronizer.get ~pledge:false sync in
  Alcotest.(check (option int))
    "no pledges, empty queue returns None" None result

let test_get_creates_pledge () =
  let sync = make_queue_sync () in
  Synchronizer.write sync 1;

  let _ : int option = Synchronizer.get ~pledge:true sync in
  Synchronizer.end_pledge sync;

  let result = Synchronizer.get ~pledge:false sync in
  Alcotest.(check (option int)) "after ending pledge, returns None" None result

let test_blocking_on_pledge () =
  let sync = make_queue_sync () in
  Synchronizer.new_pledge sync;

  let adder () =
    Unix.sleepf 0.01;
    Synchronizer.write sync 42;
    Synchronizer.end_pledge sync
  in

  let adder_domain = Domain.spawn adder in
  let result = Synchronizer.get ~pledge:false sync in
  Domain.join adder_domain;

  Alcotest.(check (option int)) "blocked until work available" (Some 42) result

(** work_while tests *)

let test_work_while_simple () =
  let sync = make_queue_sync () in
  Synchronizer.write sync 1;

  let processed = ref [] in
  let worker v write_fn =
    processed := v :: !processed;
    if v < 3 then write_fn (v + 1)
  in

  Synchronizer.work_while worker sync;

  Alcotest.(check int) "processed 3 items" 3 (List.length !processed);
  List.iter
    (fun i ->
      Alcotest.(check bool)
        (Printf.sprintf "processed %d" i)
        true (List.mem i !processed) )
    [ 1; 2; 3 ]

let test_work_while_empty () =
  let sync = make_queue_sync () in
  let called = ref false in
  let worker _v _write_fn = called := true in

  Synchronizer.work_while worker sync;

  Alcotest.(check bool) "worker not called on empty" false !called

(** Close functionality tests *)

let test_close_returns_none () =
  let sync = make_queue_sync () in
  Synchronizer.new_pledge sync;
  Synchronizer.close sync;

  let result = Synchronizer.get ~pledge:false sync in
  Alcotest.(check (option int)) "closed returns None" None result;
  Synchronizer.end_pledge sync

let test_close_with_items () =
  let sync = make_queue_sync () in
  Synchronizer.write sync 42;
  Synchronizer.close sync;

  let result = Synchronizer.get ~pledge:false sync in
  Alcotest.(check (option int))
    "gets item before close takes effect" None result;

  let result2 = Synchronizer.get ~pledge:false sync in
  Alcotest.(check (option int)) "returns None after close" None result2

(** Concurrent operations tests *)

let test_producer_consumer () =
  let sync = make_queue_sync () in
  let sum = ref 0 in
  let mutex = Mutex.create () in

  let producer () =
    for i = 1 to 10 do
      Synchronizer.write sync i
    done
  in

  let consumer () =
    Synchronizer.work_while
      (fun v _ ->
        Mutex.lock mutex;
        sum := !sum + v;
        Mutex.unlock mutex )
      sync
  in

  run_domains [| producer; consumer |];

  Alcotest.(check int) "producer/consumer sum" 55 !sum

let test_multiple_workers () =
  let sync = make_queue_sync () in

  for i = 1 to 100 do
    Synchronizer.write sync i
  done;

  let results = Array.make 4 0 in
  let worker id () =
    Synchronizer.work_while
      (fun v _ ->
        results.(id) <- results.(id) + v;
        Unix.sleepf 0.0001 )
      sync
  in

  run_domains (Array.init 4 (fun i -> worker i));

  let total_sum = Array.fold_left ( + ) 0 results in
  Alcotest.(check int) "multiple workers total sum" 5050 total_sum;

  let all_worked = Array.for_all (fun s -> s > 0) results in
  Alcotest.(check bool) "all workers did work" true all_worked

let test_concurrent_write_get () =
  let sync = make_queue_sync () in
  let count = ref 0 in
  let mutex = Mutex.create () in

  for i = 1 to 100 do
    Synchronizer.write sync i
  done;

  run_domains
    (Array.init 4 (fun _ -> fun () -> make_counter_worker count mutex sync));

  Alcotest.(check int) "all items processed" 100 !count

let test_graph_traversal () =
  let graph = [| [ 1; 2 ]; [ 3 ]; [ 3 ]; [] |] in
  let visited = Array.make 4 false in
  let mutex = Mutex.create () in

  let queue = Queue.create () in
  let getter () =
    let rec find () =
      if Queue.is_empty queue then None
      else
        let node = Queue.pop queue in
        Mutex.lock mutex;
        let already_visited = visited.(node) in
        if not already_visited then visited.(node) <- true;
        Mutex.unlock mutex;
        if already_visited then find () else Some node
    in
    find ()
  in
  let writer v = Queue.push v queue in
  let sync = Synchronizer.init getter writer in

  Synchronizer.write sync 0;

  let worker () =
    Synchronizer.work_while
      (fun node write_fn ->
        List.iter write_fn graph.(node);
        Unix.sleepf 0.0001 )
      sync
  in

  run_domains [| worker; worker |];

  let all_visited = Array.for_all (fun v -> v) visited in
  Alcotest.(check bool) "all nodes visited" true all_visited

(** Stress test *)

let test_stress () =
  let sync = make_queue_sync () in

  for i = 1 to 1000 do
    Synchronizer.write sync i
  done;

  let count = ref 0 in
  let mutex = Mutex.create () in

  run_domains
    (Array.init 8 (fun _ -> fun () -> make_counter_worker count mutex sync));

  Alcotest.(check int) "stress test processed all" 1000 !count

(** Test suite definitions *)

let basic_tests =
  [ Alcotest.test_case "Empty queue no pledges" `Quick
      test_empty_queue_no_pledges
  ; Alcotest.test_case "Get single item" `Quick test_get_single_item
  ; Alcotest.test_case "Get multiple items" `Quick test_get_multiple_items
  ]

let pledge_tests =
  [ Alcotest.test_case "Manual pledge" `Quick test_manual_pledge
  ; Alcotest.test_case "Get creates pledge" `Quick test_get_creates_pledge
  ; Alcotest.test_case "Blocking on pledge" `Quick test_blocking_on_pledge
  ]

let work_while_tests =
  [ Alcotest.test_case "work_while simple" `Quick test_work_while_simple
  ; Alcotest.test_case "work_while empty" `Quick test_work_while_empty
  ]

let close_tests =
  [ Alcotest.test_case "Close returns None" `Quick test_close_returns_none
  ; Alcotest.test_case "Close with items" `Quick test_close_with_items
  ]

let concurrent_tests =
  [ Alcotest.test_case "Producer/consumer" `Quick test_producer_consumer
  ; Alcotest.test_case "Multiple workers" `Quick test_multiple_workers
  ; Alcotest.test_case "Concurrent write/get" `Quick test_concurrent_write_get
  ; Alcotest.test_case "Graph traversal" `Quick test_graph_traversal
  ]

let stress_tests = [ Alcotest.test_case "Stress test" `Slow test_stress ]

(** Run all tests *)

let () =
  Alcotest.run "Synchronizer"
    [ ("Basic operations", basic_tests)
    ; ("Pledge mechanics", pledge_tests)
    ; ("work_while", work_while_tests)
    ; ("Close functionality", close_tests)
    ; ("Concurrent operations", concurrent_tests)
    ; ("Stress tests", stress_tests)
    ]
