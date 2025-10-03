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

type 'state synchro

type (_, 'state) poppers =
  | Nil : (unit, _) poppers
  | Cons :
      (?pledge:bool -> 'state synchro -> 'get option) * ('ps, 'state) poppers
      -> ('get * 'ps, 'state) poppers

type (_, 'state) writers =
  | Nil : (unit, _) writers
  | Cons :
      ('write -> 'state synchro -> unit) * ('ws, 'state) writers
      -> ('read * 'ws, 'state) writers

type (_, 'state) readers =
  | Nil : (unit, _) readers
  | Cons :
      ('state synchro -> 'read) * ('rs, 'state) readers
      -> ('read * 'rs, 'state) readers

val init :
     < popper : 'p ; writer : 'w ; reader : 'r ; state : 'state > synchro_builder
  -> < synchro : 'state synchro
     ; poppers : ('p, 'state) poppers
     ; readers : ('p, 'state) readers
     ; writers : ('p, 'state) writers >

(** Make a new pledge to the synchronizer (see module doc).*)
val make_pledge : 'state synchro -> unit

(** End one pledge. *)
val end_pledge : 'state synchro -> unit

(** Mark the synchronizer closed.

    The synchronizer will return None on every subsequent get that would
    otherwise block.*)
val close : 'state synchro -> unit