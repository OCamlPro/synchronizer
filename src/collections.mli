(** Collection-based synchronizers built on top of the core Synchronizer *)

(** FIFO queue synchronizer *)
module Fifo : sig
  type 'a t

  val create : unit -> 'a t

  val push : 'a t -> 'a -> unit
  val pop : 'a t -> ?pledge:bool -> unit -> 'a option
  val length : 'a t -> unit -> int
  val make_pledge : 'a t -> unit -> unit
  val end_pledge : 'a t -> unit -> unit
  val close : 'a t -> unit -> unit
end

(** LIFO stack synchronizer *)
module Lifo : sig
  type 'a t

  val create : unit -> 'a t

  val push : 'a t -> 'a -> unit
  val pop : 'a t -> ?pledge:bool -> unit -> 'a option
  val length : 'a t -> unit -> int
  val make_pledge : 'a t -> unit -> unit
  val end_pledge : 'a t -> unit -> unit
  val close : 'a t -> unit -> unit
end
