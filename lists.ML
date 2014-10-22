signature LISTS =
sig
  val seq : int -> int -> int list
  val seq' : int -> int -> int -> int list
end

structure Lists : LISTS =
struct
  fun real_seq min max inc =
    if min > max then
      []
    else
      min::(real_seq (min + inc) max inc)

  fun seq' min max inc =
    if inc < 0 then
      rev (real_seq min max (0 - inc))
    else
      real_seq min max inc

  fun seq min max = real_seq min max 1
end

(* vim: se ai sts=2 sw=2 et: *)
