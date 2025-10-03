(** This module offers a primitive which allows several threads to synchronize
    "getting" and "writing" work units to a common state keeper.

    The main idea of this module, is that it allows one to (more) easily go from
    a sequential algorithm to a parallel one.

    {2 "Get-work-to-do/write-work-done" sequential algorithms}

    The class of algorithms that this module helps make parallel are those that
    are built around a central "work to do" structure, and in a loop:

    + {i get} one work unit from the "work to do" pool
    + Process it
    + During processing, discover updates to {i write} to the pool, and do so
    + End the processing of this work unit

    Most, if not all algorithm built around queues, stacks, and priority queues
    can be formulated this way: {i getting} a work unit is simply popping from
    the queue, and {i writing} updates is simply pushing new work units.
    However, this algorithm is more general: for example the work pool could be
    a tree, {i getting} a new unit of work would entail some search of the tree,
    and {i writing} updates would be adding nodes to the tree, or marking
    existing ones.

    {2 Turning it into a parallel algorithm}

    {3 Adding "work in progress" semantics to the work pool}

    The first step to turn this sequential algorithm into a parallel one is to
    add a notion of "ongoing work" to our central data structure. When
    sequential, one cannot observe a work pool where some items are still being
    worked on, because each (sequential) iteration of the loop is the processing
    of exactly one work unit. Once parallel, the work pool should also (in most
    cases) encode a notion of the work tasks being processed.

    {3 Creating a synchronizer}

    Once it is done, the user should write two closures (notice that these two
    closures do not take the work pool as arguments, because they are closure,
    it is directly part of their environment).

    {[
      getter: unit -> get option
    ]}

    [getter] returns the next work unit, or None if their is (currently) no work
    unit to be scheduled. This is only {i currently} because another thread may
    later enqueue some new work unit. The work pool does not have to deal with
    how many threads are still live this is done by the synchronizer itself (it
    is actually its main added value). The type [get] is the one of the work
    unit.

    {[
      writer: write -> Condition.t -> unit
    ]}

    [writer] takes an update to apply to the work pool, of type [write], and a
    condition variable. If this update makes a single new work unit available,
    the condition variable should be signaled with [Condition.signal]. If more
    than one unit of work are made available, it should be broadcast-signaled
    with [Condition.broadcast].

    Finally, one can create the synchronizer with
    {[
      synchronizer = init getter writer
    ]}

    {3 The notion of pledges}

    As mentioned before, when going to non-sequential, the main difference is
    that work can now be "on going": threads may hence still {i write} updated
    while processing a work unit. Knowing whether a work pool is empty is hence
    not sufficient to know whether or not one should stop the loop: we must also
    know whether someone else may re-enqueue new work. The synchronizer supports
    this via {i pledges}. A {i pledge} is made to indicate that new {i updates}
    may be done to the work pool. Each started pledge must be ended once the
    current work unit won't lead to more updates (corresponding to the end of
    the loop in the sequential algorithm).

    {3 Rewriting the main loop}

    The loop outline above now becomes:

    + {i get} one work unit from the "work to do" pool, atomically pledging that
      you may still {i write} updates
    + Process the work unit
    + During processing, discover updates to {i write} to the pool, and do so
    + End the processing of this work unit by ending the pledge

    This loop can also be done using the provided [work_while] function. *)

(** A synchronizer with work units of type ['get] and updates of type ['write]
*)
type (+'get, -'write) t

(** Create a new synchronizer with the provided getter and writer (see module
    doc). *)
val init :
  (unit -> 'get option) -> ('write -> Condition.t -> unit) -> ('get, 'write) t

(** Get a work unit from the synchronizer. If pledge is true (its default
    value), atomically create a new pledge.

    Blocks if no work unit is available but some may become in the future (i.e.
    there are still active pledges on the synchronizer has not been marked
    closed).

    Returns None if no work unit is available and none will be in the future. *)
val get : ?pledge:bool -> ('get, 'write) t -> 'get option

(** Write the given update to the synchronizer.*)
val write : 'write -> ('get, 'write) t -> unit

(** Make a new pledge to the synchronizer (see module doc).*)
val make_pledge : ('get, 'write) t -> unit

(** End one pledge. *)
val end_pledge : ('get, 'write) t -> unit

(** Mark the synchronizer closed.

    The synchronizer will return None on every subsequent get that would
    otherwise block.*)
val close : ('get, 'write) t -> unit

(** Run the provided closure on the synchronizer until the synchronizer is
    exhausted (i.e. it returns None).*)
val work_while : ('get -> ('write -> unit) -> unit) -> ('get, 'write) t -> unit
