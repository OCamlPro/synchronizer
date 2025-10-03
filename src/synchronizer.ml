(* SPDX-License-Identifier: AGPL-3.0-or-later *)

type _ synchro_builder =
  | Init :
      (unit -> 'state)
      -> < popper : unit ; writer : unit ; reader : unit ; state : 'state >
         synchro_builder
  | AddPopper :
      ('state -> 'get option)
      * < popper : 'p ; writer : 'w ; reader : 'r ; state : 'state >
        synchro_builder
      -> < popper : 'get * 'p ; writer : 'w ; reader : 'r ; state : 'state >
         synchro_builder
  | AddWriter :
      ('write -> Condition.t -> 'state -> unit)
      * < popper : 'p ; writer : 'w ; reader : 'r ; state : 'state >
        synchro_builder
      -> < popper : 'p ; writer : 'write * 'w ; reader : 'r ; state : 'state >
         synchro_builder
  | AddReader :
      ('state -> 'read)
      * < popper : 'p ; writer : 'w ; reader : 'r ; state : 'state >
        synchro_builder
      -> < popper : 'p ; writer : 'w ; reader : 'read * 'r ; state : 'state >
         synchro_builder

let create init_fn = Init init_fn
let add_popper getter builder = AddPopper (getter, builder)
let add_writer writer builder = AddWriter (writer, builder)
let add_reader reader builder = AddReader (reader, builder)

type 'state synchro =
  { mutex : Mutex.t
  ; cond : Condition.t
  ; state : 'state
  ; mutable pledges : int
  ; mutable closed : bool
  }

type _ poppers =
  | [] : unit poppers
  | ( :: ) :
      (?pledge:bool -> unit -> 'get option) * 'ps poppers
      -> ('get * 'ps) poppers

type _ writers =
  | [] : unit writers
  | ( :: ) :
      ('write -> unit) * 'ws writers
      -> ('write * 'ws) writers

type _ readers =
  | [] : unit readers
  | ( :: ) :
      (unit -> 'read) * 'rs readers
      -> ('read * 'rs) readers

let rec build_all : type p w r state.
     state synchro
  -> < popper : p ; writer : w ; reader : r ; state : state > synchro_builder
  -> p poppers * w writers * r readers =
 fun synchro builder ->
  match builder with
  | Init _ -> ([], [], [])
  | AddPopper (getter, prev) ->
    let prev_poppers, prev_writers, prev_readers = build_all synchro prev in
    let popper ?(pledge = true) () =
      let rec inner_loop pledge synchro =
        match getter synchro.state with
        | None when synchro.pledges = 0 || synchro.closed ->
          Condition.broadcast synchro.cond;
          None
        | None ->
          Condition.wait synchro.cond synchro.mutex;
          inner_loop pledge synchro
        | Some _ as v ->
          if pledge then synchro.pledges <- synchro.pledges + 1;
          v
      in
      Mutex.protect synchro.mutex (fun () -> inner_loop pledge synchro)
    in
    (popper :: prev_poppers, prev_writers, prev_readers)
  | AddWriter (writer, prev) ->
    let prev_poppers, prev_writers, prev_readers = build_all synchro prev in
    let write_fn value =
      Mutex.protect synchro.mutex (fun () ->
        writer value synchro.cond synchro.state )
    in
    (prev_poppers, write_fn :: prev_writers, prev_readers)
  | AddReader (reader, prev) ->
    let prev_poppers, prev_writers, prev_readers = build_all synchro prev in
    let read_fn () =
      Mutex.protect synchro.mutex (fun () -> reader synchro.state)
    in
    (prev_poppers, prev_writers, read_fn :: prev_readers)

type ('p, 'w, 'r) t = Synchro :
  { poppers : 'p poppers
  ; readers : 'r readers
  ; writers : 'w writers
  ; make_pledge : unit -> unit
  ; end_pledge : unit -> unit
  ; close : unit -> unit
  } -> ('p, 'w, 'r) t

let rec get_init_fn : type p w r state.
     < popper : p ; writer : w ; reader : r ; state : state > synchro_builder
  -> unit -> state =
 fun builder ->
  match builder with
  | Init init_fn -> init_fn
  | AddPopper (_, prev) -> get_init_fn prev
  | AddWriter (_, prev) -> get_init_fn prev
  | AddReader (_, prev) -> get_init_fn prev

let init builder =
  let init_fn = get_init_fn builder in
  let synchro =
    { mutex = Mutex.create ()
    ; cond = Condition.create ()
    ; state = init_fn ()
    ; pledges = 0
    ; closed = false
    }
  in
  let poppers, writers, readers = build_all synchro builder in
  Synchro { poppers
  ; readers
  ; writers
  ; make_pledge =
      (fun () ->
        Mutex.lock synchro.mutex;
        synchro.pledges <- synchro.pledges + 1;
        Mutex.unlock synchro.mutex)
  ; end_pledge =
      (fun () ->
        Mutex.lock synchro.mutex;
        synchro.pledges <- synchro.pledges - 1;
        if Int.equal synchro.pledges 0 then Condition.broadcast synchro.cond;
        Mutex.unlock synchro.mutex)
  ; close =
      (fun () ->
        Mutex.lock synchro.mutex;
        synchro.closed <- true;
        Condition.broadcast synchro.cond;
        Mutex.unlock synchro.mutex)
  }
