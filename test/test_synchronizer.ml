(* SPDX-License-Identifier: AGPL-3.0-or-later *)

(** Unit tests for the Synchronizer library using Alcotest *)

(** Helper functions to reduce duplication *)

(** Create a simple queue-based synchronizer.
    Uses Condition.signal since each write adds exactly one work unit. *)
let make_queue_sync () =
  let queue = Queue.create () in
  let getter () =
    if Queue.is_empty queue then None else Some (Queue.pop queue)
  in
  let writer v cond _state =
    Queue.push v queue;
    Condition.signal cond
  in
  Synchronizer.(
    init
      (create (fun () -> queue)
      |> add_popper (fun _state -> getter ())
      |> add_writer writer))

(** Create a thread-safe counter worker *)
let make_counter_worker count mutex (Synchronizer.Synchro sync) =
  let [ popper ] = sync.poppers in
  let rec loop () =
    match popper () with
    | None -> ()
    | Some _v ->
      Mutex.lock mutex;
      count := !count + 1;
      Mutex.unlock mutex;
      sync.end_pledge ();
      loop ()
  in
  loop ()

(** Spawn and join multiple domains *)
let run_domains workers =
  let domains = Array.map Domain.spawn workers in
  Array.iter Domain.join domains

(** Basic operations tests *)

let test_empty_queue_no_pledges () =
  let (Synchronizer.Synchro sync) = make_queue_sync () in
  let [ popper ] = sync.poppers in
  let result = popper ~pledge:false () in
  Alcotest.(check (option int)) "empty queue returns None" None result

let test_get_single_item () =
  let (Synchronizer.Synchro sync) = make_queue_sync () in
  let [ writer ] = sync.writers in
  let [ popper ] = sync.poppers in
  writer 42;
  let result = popper ~pledge:false () in
  Alcotest.(check (option int)) "get returns Some 42" (Some 42) result

let test_get_multiple_items () =
  let (Synchronizer.Synchro sync) = make_queue_sync () in
  let [ writer ] = sync.writers in
  let [ popper ] = sync.poppers in
  List.iter writer [ 1; 2; 3 ];

  let r1 = popper ~pledge:false () in
  let r2 = popper ~pledge:false () in
  let r3 = popper ~pledge:false () in

  Alcotest.(check (option int)) "first item" (Some 1) r1;
  Alcotest.(check (option int)) "second item" (Some 2) r2;
  Alcotest.(check (option int)) "third item" (Some 3) r3

(** Pledge mechanics tests *)

let test_manual_pledge () =
  let (Synchronizer.Synchro sync) = make_queue_sync () in
  let [ writer ] = sync.writers in
  let [ popper ] = sync.poppers in

  sync.make_pledge ();
  writer 1;

  let result = popper ~pledge:true () in
  Alcotest.(check (option int)) "get with pledge returns Some" (Some 1) result;

  (* End both pledges *)
  sync.end_pledge ();
  sync.end_pledge ();

  let result = popper ~pledge:false () in
  Alcotest.(check (option int)) "no pledges, empty queue returns None" None result

let test_get_creates_pledge () =
  let (Synchronizer.Synchro sync) = make_queue_sync () in
  let [ writer ] = sync.writers in
  let [ popper ] = sync.poppers in
  writer 1;

  let _ = popper () in
  sync.end_pledge ();

  let result = popper ~pledge:false () in
  Alcotest.(check (option int)) "after ending pledge, returns None" None result

let test_blocking_on_pledge () =
  let (Synchronizer.Synchro sync) = make_queue_sync () in
  let [ writer ] = sync.writers in
  let [ popper ] = sync.poppers in
  sync.make_pledge ();

  let adder () =
    Unix.sleepf 0.01;
    writer 42;
    sync.end_pledge ()
  in

  let adder_domain = Domain.spawn adder in
  let result = popper ~pledge:false () in
  Domain.join adder_domain;

  Alcotest.(check (option int)) "blocked until work available" (Some 42) result

(** work_while tests *)

let work_while popper end_pledge f =
  let rec loop () =
    match popper () with
    | None -> ()
    | Some v ->
      f v ();
      end_pledge ();
      loop ()
  in
  loop ()

let test_work_while_simple () =
  let (Synchronizer.Synchro sync) = make_queue_sync () in
  let [ writer ] = sync.writers in
  let [ popper ] = sync.poppers in
  writer 1;

  let processed = ref [] in
  let worker v () =
    processed := v :: !processed;
    if v < 3 then writer (v + 1)
  in

  work_while popper sync.end_pledge worker;

  Alcotest.(check int) "processed 3 items" 3 (List.length !processed);
  List.iter
    (fun i -> Alcotest.(check bool) (Printf.sprintf "processed %d" i) true (List.mem i !processed))
    [ 1; 2; 3 ]

let test_work_while_empty () =
  let (Synchronizer.Synchro sync) = make_queue_sync () in
  let [ popper ] = sync.poppers in
  let called = ref false in
  let worker _v () = called := true in

  work_while popper sync.end_pledge worker;

  Alcotest.(check bool) "worker not called on empty" false !called

(** Close functionality tests *)

let test_close_returns_none () =
  let (Synchronizer.Synchro sync) = make_queue_sync () in
  let [ popper ] = sync.poppers in
  sync.make_pledge ();
  sync.close ();

  let result = popper ~pledge:false () in
  Alcotest.(check (option int)) "closed returns None" None result;
  sync.end_pledge ()

let test_close_with_items () =
  let (Synchronizer.Synchro sync) = make_queue_sync () in
  let [ writer ] = sync.writers in
  let [ popper ] = sync.poppers in
  writer 42;
  sync.close ();

  let result = popper ~pledge:false () in
  Alcotest.(check (option int)) "gets item before close takes effect" (Some 42) result;

  let result2 = popper ~pledge:false () in
  Alcotest.(check (option int)) "returns None after close" None result2

(** Concurrent operations tests *)

let test_producer_consumer () =
  let (Synchronizer.Synchro sync) = make_queue_sync () in
  let [ writer ] = sync.writers in
  let [ popper ] = sync.poppers in
  let sum = ref 0 in
  let mutex = Mutex.create () in

  let producer () =
    for i = 1 to 10 do
      writer i
    done
  in

  let consumer () =
    work_while popper sync.end_pledge (fun v () ->
      Mutex.lock mutex;
      sum := !sum + v;
      Mutex.unlock mutex)
  in

  run_domains [| producer; consumer |];

  Alcotest.(check int) "producer/consumer sum" 55 !sum

let test_multiple_workers () =
  let (Synchronizer.Synchro sync) = make_queue_sync () in
  let [ writer ] = sync.writers in
  let [ popper ] = sync.poppers in

  for i = 1 to 100 do
    writer i
  done;

  let results = Array.make 4 0 in
  let worker id () =
    work_while popper sync.end_pledge (fun v () ->
      results.(id) <- results.(id) + v;
      Unix.sleepf 0.0001)
  in

  run_domains (Array.init 4 (fun i -> worker i));

  let total_sum = Array.fold_left ( + ) 0 results in
  Alcotest.(check int) "multiple workers total sum" 5050 total_sum;

  let all_worked = Array.for_all (fun s -> s > 0) results in
  Alcotest.(check bool) "all workers did work" true all_worked

let test_concurrent_write_get () =
  let (Synchronizer.Synchro sync) = make_queue_sync () in
  let [ writer ] = sync.writers in
  let count = ref 0 in
  let mutex = Mutex.create () in

  for i = 1 to 100 do
    writer i
  done;

  run_domains (Array.init 4 (fun _ -> fun () -> make_counter_worker count mutex (Synchro sync)));

  Alcotest.(check int) "all items processed" 100 !count

let test_graph_traversal () =
  let graph = [| [ 1; 2 ]; [ 3 ]; [ 3 ]; [] |] in
  let visited = Array.make 4 false in
  let mutex = Mutex.create () in

  let queue = Queue.create () in
  let getter _state =
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
  let writer v cond _state =
    Queue.push v queue;
    Condition.signal cond
  in
  let (Synchronizer.Synchro sync) =
    Synchronizer.(
      init
        (create (fun () -> ())
        |> add_popper getter
        |> add_writer writer))
  in
  let [ writer_fn ] = sync.writers in
  let [ popper ] = sync.poppers in

  writer_fn 0;

  let worker () =
    let rec loop () =
      match popper () with
      | None -> ()
      | Some node ->
        List.iter writer_fn graph.(node);
        Unix.sleepf 0.0001;
        sync.end_pledge ();
        loop ()
    in
    loop ()
  in

  run_domains [| worker; worker |];

  let all_visited = Array.for_all (fun v -> v) visited in
  Alcotest.(check bool) "all nodes visited" true all_visited

(** Stress test *)

let test_stress () =
  let (Synchronizer.Synchro sync) = make_queue_sync () in
  let [ writer ] = sync.writers in

  for i = 1 to 1000 do
    writer i
  done;

  let count = ref 0 in
  let mutex = Mutex.create () in

  run_domains (Array.init 8 (fun _ -> fun () -> make_counter_worker count mutex (Synchro sync)));

  Alcotest.(check int) "stress test processed all" 1000 !count

(** Test suite definitions *)

let basic_tests =
  [ Alcotest.test_case "Empty queue no pledges" `Quick test_empty_queue_no_pledges
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

let stress_tests =
  [ Alcotest.test_case "Stress test" `Slow test_stress ]

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
