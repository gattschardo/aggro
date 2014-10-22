signature DICT =
sig
  (* t is size * active-slots * max-slots * buddy-slot-offset * exp-size * con-size * empty * segs *)
  type k
  type v
  type t

  val new : t
  val is_key : k -> t -> bool
  val to_list : t -> (k * v) list
  (*val from_list : (k * v) list -> t*)
  val size : t -> int
  val is_empty : t -> bool

  val fetch : k -> t -> v
  val find : k -> t -> v option
  (*val fetch_keys
  val erase*)

  val store : k -> v -> t -> t
  (*val append
  val append_list
  val update
  val update'
  val update_counter*)

  val fold : ((k * v * 'a) -> 'a) -> 'a -> t -> 'a
  (*val map
  val filter
  val merge*)
end

signature ORDER =
sig
  type t
  val compare : t * t -> order
end

functor DictFn (structure K : ORDER type V) :> DICT
where type k = K.t
  and type v = V
  =
struct
  (** types **)
  type k = K.t
  type v = V
  type t = (k * v) list

  val new = []

  fun is_key k [] = false
    | is_key k ((k0, _)::d) =
        if K.compare (k, k0) = EQUAL then
          true
        else
          is_key k d

  fun to_list d = d

  fun size d = length d

  fun is_empty d = (size d = 0)

  fun find k [] = NONE
    | find k ((k0,v0)::d) =
        if K.compare (k, k0) = EQUAL then
          SOME v0
        else
          find k d

  fun fetch k d =
    case find k d of
      SOME v => v
  
  fun store k v d = (k, v)::d

  fun fold f a d =
    foldl (fn ((k, v), a) => f (k,v,a)) a d
end

(* vim: se ai sts=2 sw=2 et: *)
