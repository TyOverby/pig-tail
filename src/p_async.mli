open! Core
open! Async

module Result : sig
  module Status : sig
    type t [@@deriving sexp]

    val is_ok : t -> unit Or_error.t
  end

  type t [@@deriving sexp_of]

  val status : t -> Status.t
  val nfields : t -> int
  val ntuples : t -> int
  val get_value : t -> int -> int -> string
  val get_escaped_value : t -> int -> int -> string
  val fname : t -> int -> string
end

type t

val create : host:string -> user:string -> port:int -> t Deferred.Or_error.t
val shutdown : t -> unit Deferred.Or_error.t
val fetch_result : ?stop:unit Deferred.t -> t -> Result.t option Deferred.Or_error.t
val fetch_single_result : ?stop:unit Deferred.t -> t -> Result.t Deferred.Or_error.t

val fetch_iter
  :  t
  -> f:(Result.t -> [ `Stop | `Continue ] Deferred.Or_error.t)
  -> unit Deferred.Or_error.t

val underlying : t -> Postgresql.connection
val fd : t -> Async.Fd.t
