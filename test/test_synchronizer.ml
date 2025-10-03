(* SPDX-License-Identifier: AGPL-3.0-or-later *)

(** Unit tests for the Synchronizer library using Alcotest *)

(** Basic queue operations - single thread *)
let test_empty_queue_no_pledges () =
  let queue = Queue.create () in
  let getter () =
    if Queue.is_empty queue then None else Some (Queue.pop queue)
  in
  let writer v cond =
    Queue.push v queue;
    Condition.signal cond
  in
  let sync = Synchronizer.init getter writer in
  let result = Synchronizer.get ~pledge:false sync in
  Alcotest.(check (option int)) "empty queue returns None" None result

let test_get_single_item () =
  let queue = Queue.create () in
  let getter () =
    if Queue.is_empty queue then None else Some (Queue.pop queue)
  in
  let writer v cond =
    Queue.push v queue;
    Condition.signal cond
  in
  let sync = Synchronizer.init getter writer in
  Synchronizer.write 42 sync;
  let result = Synchronizer.get ~pledge:false sync in
  Alcotest.(check (option int)) "get returns Some 42" (Some 42) result

let test_get_multiple_items () =
  let queue = Queue.create () in
  let getter () =
    if Queue.is_empty queue then None else Some (Queue.pop queue)
  in
  let writer v cond =
    Queue.push v queue;
    Condition.signal cond
  in
  let sync = Synchronizer.init getter writer in
  Synchronizer.write 1 sync;
  Synchronizer.write 2 sync;
  Synchronizer.write 3 sync;

  let r1 = Synchronizer.get ~pledge:false sync in
  let r2 = Synchronizer.get ~pledge:false sync in
  let r3 = Synchronizer.get ~pledge:false sync in

  Alcotest.(check (option int)) "first item" (Some 1) r1;
  Alcotest.(check (option int)) "second item" (Some 2) r2;
  Alcotest.(check (option int)) "third item" (Some 3) r3

(** Pledge mechanics *)
let test_manual_pledge () =
  let queue = Queue.create () in
  let getter () =
    if Queue.is_empty queue then None else Some (Queue.pop queue)
  in
  let writer v cond =
    Queue.push v queue;
    Condition.signal cond
  in
  let sync = Synchronizer.init getter writer in

  (* Make a pledge manually *)
  Synchronizer.make_pledge sync;
  Synchronizer.write 1 sync;

  (* Get with pledge *)
  let result = Synchronizer.get ~pledge:true sync in
  Alcotest.(check (option int)) "get with pledge returns Some" (Some 1) result;

  (* End both pledges *)
  Synchronizer.end_pledge sync;
  Synchronizer.end_pledge sync;

  (* Now should return None (no pledges, empty queue) *)
  let result = Synchronizer.get ~pledge:false sync in
  Alcotest.(check (option int)) "no pledges, empty queue returns None" None result

let test_get_creates_pledge () =
  let queue = Queue.create () in
  let getter () =
    if Queue.is_empty queue then None else Some (Queue.pop queue)
  in
  let writer v cond =
    Queue.push v queue;
    Condition.signal cond
  in
  let sync = Synchronizer.init getter writer in
  Synchronizer.write 1 sync;

  (* Get with default pledge=true *)
  let _ = Synchronizer.get sync in
  (* Must end the pledge before queue becomes truly empty *)
  Synchronizer.end_pledge sync;

  let result = Synchronizer.get ~pledge:false sync in
  Alcotest.(check (option int)) "after ending pledge, returns None" None result

(** work_while helper function *)
let test_work_while_simple () =
  let queue = Queue.create () in
  let getter () =
    if Queue.is_empty queue then None else Some (Queue.pop queue)
  in
  let writer v cond =
    Queue.push v queue;
    Condition.signal cond
  in
  let sync = Synchronizer.init getter writer in

  (* Add initial work *)
  Synchronizer.write 1 sync;

  let processed = ref [] in
  let worker v write_fn =
    processed := v :: !processed;
    (* Add more work for values < 3 *)
    if v < 3 then write_fn (v + 1)
  in

  Synchronizer.work_while worker sync;

  Alcotest.(check int) "processed 3 items" 3 (List.length !processed);
  Alcotest.(check bool) "processed 1" true (List.mem 1 !processed);
  Alcotest.(check bool) "processed 2" true (List.mem 2 !processed);
  Alcotest.(check bool) "processed 3" true (List.mem 3 !processed)

let test_work_while_empty () =
  let queue = Queue.create () in
  let getter () =
    if Queue.is_empty queue then None else Some (Queue.pop queue)
  in
  let writer v cond =
    Queue.push v queue;
    Condition.signal cond
  in
  let sync = Synchronizer.init getter writer in

  let called = ref false in
  let worker _v _write_fn = called := true in

  Synchronizer.work_while worker sync;

  Alcotest.(check bool) "worker not called on empty" false !called

(** Close functionality *)
let test_close_returns_none () =
  let queue = Queue.create () in
  let getter () =
    if Queue.is_empty queue then None else Some (Queue.pop queue)
  in
  let writer v cond =
    Queue.push v queue;
    Condition.signal cond
  in
  let sync = Synchronizer.init getter writer in

  (* Make a pledge to keep it alive *)
  Synchronizer.make_pledge sync;

  (* Close the synchronizer *)
  Synchronizer.close sync;

  (* Should return None even with active pledge *)
  let result = Synchronizer.get ~pledge:false sync in
  Alcotest.(check (option int)) "closed returns None" None result;

  Synchronizer.end_pledge sync

let test_close_with_items () =
  let queue = Queue.create () in
  let getter () =
    if Queue.is_empty queue then None else Some (Queue.pop queue)
  in
  let writer v cond =
    Queue.push v queue;
    Condition.signal cond
  in
  let sync = Synchronizer.init getter writer in

  Synchronizer.write 42 sync;
  Synchronizer.close sync;

  (* Should still get the item *)
  let result = Synchronizer.get ~pledge:false sync in
  Alcotest.(check (option int)) "gets item before close takes effect" (Some 42) result;

  (* Now returns None *)
  let result2 = Synchronizer.get ~pledge:false sync in
  Alcotest.(check (option int)) "returns None after close" None result2

(** Concurrent operations *)
let test_producer_consumer () =
  let queue = Queue.create () in
  let getter () =
    if Queue.is_empty queue then None else Some (Queue.pop queue)
  in
  let writer v cond =
    Queue.push v queue;
    Condition.signal cond
  in
  let sync = Synchronizer.init getter writer in

  let sum = ref 0 in
  let mutex = Mutex.create () in

  (* Producer: adds numbers 1..10 *)
  let producer () =
    for i = 1 to 10 do
      Synchronizer.write i sync
    done
  in

  (* Consumer: sums all numbers *)
  let consumer () =
    Synchronizer.work_while
      (fun v _ ->
        Mutex.lock mutex;
        sum := !sum + v;
        Mutex.unlock mutex)
      sync
  in

  let producer_domain = Domain.spawn producer in
  let consumer_domain = Domain.spawn consumer in

  Domain.join producer_domain;
  Domain.join consumer_domain;

  (* Sum of 1..10 is 55 *)
  Alcotest.(check int) "producer/consumer sum" 55 !sum

let test_multiple_workers () =
  let queue = Queue.create () in
  let getter () =
    if Queue.is_empty queue then None else Some (Queue.pop queue)
  in
  let writer v cond =
    Queue.push v queue;
    Condition.broadcast cond
  in
  let sync = Synchronizer.init getter writer in

  (* Add initial work *)
  for i = 1 to 100 do
    Synchronizer.write i sync
  done;

  let results = Array.make 4 0 in

  let worker id () =
    Synchronizer.work_while
      (fun v _ ->
        results.(id) <- results.(id) + v;
        Unix.sleepf 0.0001)
      sync
  in

  (* Spawn 4 worker domains *)
  let domains = Array.init 4 (fun i -> Domain.spawn (worker i)) in

  (* Wait for all workers *)
  Array.iter Domain.join domains;

  (* Total sum should be 1+2+...+100 = 5050 *)
  let total_sum = Array.fold_left ( + ) 0 results in
  Alcotest.(check int) "multiple workers total sum" 5050 total_sum;

  (* Each worker should have done some work *)
  let all_worked = Array.for_all (fun s -> s > 0) results in
  Alcotest.(check bool) "all workers did work" true all_worked

let test_concurrent_write_get () =
  let queue = Queue.create () in
  let getter () =
    if Queue.is_empty queue then None else Some (Queue.pop queue)
  in
  let writer v cond =
    Queue.push v queue;
    Condition.broadcast cond
  in
  let sync = Synchronizer.init getter writer in

  let count = ref 0 in
  let mutex = Mutex.create () in

  (* Pre-populate all work items *)
  for i = 1 to 100 do
    Synchronizer.write i sync
  done;

  let reader_thread () =
    Synchronizer.work_while
      (fun _v _ ->
        Mutex.lock mutex;
        count := !count + 1;
        Mutex.unlock mutex;
        (* Simulate some work *)
        Unix.sleepf 0.0001)
      sync
  in

  (* Multiple readers process the pre-populated work *)
  let r1 = Domain.spawn reader_thread in
  let r2 = Domain.spawn reader_thread in
  let r3 = Domain.spawn reader_thread in
  let r4 = Domain.spawn reader_thread in

  Domain.join r1;
  Domain.join r2;
  Domain.join r3;
  Domain.join r4;

  Alcotest.(check int) "all items processed" 100 !count

(** Graph traversal simulation *)
let test_graph_traversal () =
  (* Simple adjacency list: 0 -> [1,2], 1 -> [3], 2 -> [3], 3 -> [] *)
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
  let writer v cond =
    Queue.push v queue;
    Condition.broadcast cond
  in
  let sync = Synchronizer.init getter writer in

  (* Start from node 0 *)
  Synchronizer.write 0 sync;

  let worker () =
    Synchronizer.work_while
      (fun node write_fn ->
        (* Add neighbors *)
        List.iter write_fn graph.(node);
        Unix.sleepf 0.0001)
      sync
  in

  (* Use 2 workers for traversal *)
  let d1 = Domain.spawn worker in
  let d2 = Domain.spawn worker in
  Domain.join d1;
  Domain.join d2;

  (* All nodes should be visited *)
  let all_visited = Array.for_all (fun v -> v) visited in
  Alcotest.(check bool) "all nodes visited" true all_visited

(** Stress test *)
let test_stress () =
  let queue = Queue.create () in
  let getter () =
    if Queue.is_empty queue then None else Some (Queue.pop queue)
  in
  let writer v cond =
    Queue.push v queue;
    Condition.broadcast cond
  in
  let sync = Synchronizer.init getter writer in

  (* Add 1000 initial items *)
  for i = 1 to 1000 do
    Synchronizer.write i sync
  done;

  let count = ref 0 in
  let mutex = Mutex.create () in

  let worker () =
    Synchronizer.work_while
      (fun _v _ ->
        Mutex.lock mutex;
        count := !count + 1;
        Mutex.unlock mutex)
      sync
  in

  (* 8 workers *)
  let domains = Array.init 8 (fun _ -> Domain.spawn worker) in
  Array.iter Domain.join domains;

  Alcotest.(check int) "stress test processed all" 1000 !count

(** Empty start with pledge - blocking behavior *)
let test_blocking_on_pledge () =
  let queue = Queue.create () in
  let getter () =
    if Queue.is_empty queue then None else Some (Queue.pop queue)
  in
  let writer v cond =
    Queue.push v queue;
    Condition.signal cond
  in
  let sync = Synchronizer.init getter writer in

  Synchronizer.make_pledge sync;

  let adder () =
    Unix.sleepf 0.01;
    Synchronizer.write 42 sync;
    Synchronizer.end_pledge sync
  in

  let adder_domain = Domain.spawn adder in
  let result = Synchronizer.get ~pledge:false sync in

  Domain.join adder_domain;

  Alcotest.(check (option int)) "blocked until work available" (Some 42) result

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
