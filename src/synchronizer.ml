(* SPDX-License-Identifier: AGPL-3.0-or-later *)

type ('get, 'write) t =
  { mutex : Mutex.t
  ; cond : Condition.t
  ; getter : unit -> 'get option
  ; writer : 'write -> unit
  ; mutable pledges : int
  ; mutable closed : bool
  }

let init getter writer =
  { mutex = Mutex.create ()
  ; cond = Condition.create ()
  ; getter
  ; writer
  ; pledges = 0
  ; closed = false
  }

let get ~pledge synchro =
  let rec inner_loop pledge synchro =
    if synchro.closed then (true, None)
    else
      match synchro.getter () with
      | None ->
        if synchro.pledges = 0 then (true, None)
        else begin
          Condition.wait synchro.cond synchro.mutex;
          inner_loop pledge synchro
        end
      | next_element ->
        if pledge then synchro.pledges <- synchro.pledges + 1;
        (false, next_element)
  in
  Mutex.protect synchro.mutex (fun () ->
    let should_broadcast, res = inner_loop pledge synchro in
    if should_broadcast then Condition.broadcast synchro.cond;
    res )

let write v synchro =
  Mutex.protect synchro.mutex (fun () ->
    synchro.writer v;
    Condition.signal synchro.cond )

let make_pledge synchro =
  Mutex.protect synchro.mutex (fun () ->
    synchro.pledges <- synchro.pledges + 1 )

let end_pledge synchro =
  Mutex.protect synchro.mutex (fun () ->
    synchro.pledges <- synchro.pledges - 1;
    if synchro.pledges = 0 then Condition.broadcast synchro.cond )

let close synchro =
  Mutex.protect synchro.mutex (fun () ->
    synchro.closed <- true;
    Condition.broadcast synchro.cond )

let rec work_while f q =
  match get ~pledge:true q with
  | None -> ()
  | Some v ->
    Fun.protect
      ~finally:(fun () -> end_pledge q)
      (fun () -> f v (fun v -> write v q));
    work_while f q
