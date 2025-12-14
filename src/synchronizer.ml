(* SPDX-License-Identifier: AGPL-3.0-or-later *)

type ('get, 'write) t =
  { mutex : Mutex.t
  ; cond : Condition.t
  ; getter : unit -> 'get option
  ; writer : 'write -> unit
  ; pledges_count : int Atomic.t
  ; closed : bool Atomic.t
  }

let init getter writer =
  { mutex = Mutex.create ()
  ; cond = Condition.create ()
  ; getter
  ; writer
  ; pledges_count = Atomic.make 0
  ; closed = Atomic.make_contended false
  }

let new_pledge synchro = Atomic.incr synchro.pledges_count

let end_pledge synchro =
  let previous_pledges = Atomic.fetch_and_add synchro.pledges_count ~-1 in
  (* it means the current pledges is 0 because we removed 1. *)
  if previous_pledges = 1 then
    (* we notify one thread that was waiting
       it will take care of notifying other that there is nothing left to read *)
    Condition.signal synchro.cond

let close synchro =
  Atomic.set synchro.closed true;
  (* The synchronizer is closed, we notify one thread only
     it'll take care of notifying the others *)
  Condition.signal synchro.cond

let is_closed synchro = Atomic.get synchro.closed

let get ~pledge synchro =
  let rec inner_loop pledge synchro =
    if is_closed synchro then None
    else
      match synchro.getter () with
      | None ->
        let pledges_count = Atomic.get synchro.pledges_count in
        if pledges_count = 0 then begin
          (* if someone else was waiting to read, it should try again now
             we do not broadcast, because the next one is quickly going to notice
             there is nothing left to read and notify the next thread once it is done *)
          Condition.signal synchro.cond;
          None
        end
        else begin
          (* there was nothing to read but there may be more in the future
             we let other threads do stuff
             maybe one of them will write stuff we can read later *)
          Condition.wait synchro.cond synchro.mutex;
          inner_loop pledge synchro
        end
      | next_element ->
        if pledge then new_pledge synchro;
        next_element
  in
  Mutex.protect synchro.mutex (fun () -> inner_loop pledge synchro)

let write synchro v =
  if not @@ is_closed synchro then
    Mutex.protect synchro.mutex (fun () ->
      synchro.writer v;
      Condition.signal synchro.cond )

let rec work_while f q =
  match get ~pledge:true q with
  | None -> ()
  | Some v ->
    Fun.protect
      ~finally:(fun () ->
        (* If the function fails, we must end the pledge we created by `get`
           otherwise there will be a dangling pledge. *)
        end_pledge q )
      (fun () -> f v (write q));
    work_while f q
