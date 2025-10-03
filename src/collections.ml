(* SPDX-License-Identifier: AGPL-3.0-or-later *)

(** FIFO queue synchronizer *)
module Fifo = struct
  type 'a queue_state = { queue : 'a Queue.t }

  type 'a t =
    { push : 'a -> unit
    ; pop : ?pledge:bool -> unit -> 'a option
    ; length : unit -> int
    ; make_pledge : unit -> unit
    ; end_pledge : unit -> unit
    ; close : unit -> unit
    }

  let create () =
    let queue = Queue.create () in
    let getter state =
      if Queue.is_empty state.queue then None else Some (Queue.pop state.queue)
    in
    let pusher v cond _state =
      Queue.push v queue;
      Condition.signal cond
    in
    let length_reader state = Queue.length state.queue in
    let (Synchronizer.Synchro sync) =
      Synchronizer.(
        init
          (create (fun () -> { queue })
          |> add_popper getter
          |> add_writer pusher
          |> add_reader length_reader))
    in
    let [ popper ] = sync.poppers in
    let [ writer ] = sync.writers in
    let [ length_fn ] = sync.readers in
    { push = writer
    ; pop = popper
    ; length = length_fn
    ; make_pledge = sync.make_pledge
    ; end_pledge = sync.end_pledge
    ; close = sync.close
    }

  let push t = t.push
  let pop t = t.pop
  let length t = t.length
  let make_pledge t = t.make_pledge
  let end_pledge t = t.end_pledge
  let close t = t.close
end

(** LIFO stack synchronizer *)
module Lifo = struct
  type 'a stack_state = { stack : 'a Stack.t }

  type 'a t =
    { push : 'a -> unit
    ; pop : ?pledge:bool -> unit -> 'a option
    ; length : unit -> int
    ; make_pledge : unit -> unit
    ; end_pledge : unit -> unit
    ; close : unit -> unit
    }

  let create () =
    let stack = Stack.create () in
    let getter state =
      if Stack.is_empty state.stack then None else Some (Stack.pop state.stack)
    in
    let pusher v cond _state =
      Stack.push v stack;
      Condition.signal cond
    in
    let length_reader state = Stack.length state.stack in
    let (Synchronizer.Synchro sync) =
      Synchronizer.(
        init
          (create (fun () -> { stack })
          |> add_popper getter
          |> add_writer pusher
          |> add_reader length_reader))
    in
    let [ popper ] = sync.poppers in
    let [ writer ] = sync.writers in
    let [ length_fn ] = sync.readers in
    { push = writer
    ; pop = popper
    ; length = length_fn
    ; make_pledge = sync.make_pledge
    ; end_pledge = sync.end_pledge
    ; close = sync.close
    }

  let push t = t.push
  let pop t = t.pop
  let length t = t.length
  let make_pledge t = t.make_pledge
  let end_pledge t = t.end_pledge
  let close t = t.close
end
