signature AGGRO =
sig
  (** types **)
  type symbol = char

  (* space is min-x * max-x * min-y * max-y *)
  type space = int * int * int * int

  structure DX : DICT
  structure DY : DICT

  type branch_store
  type rule_store

  (* child is symbol * pos *)
  datatype child = Child of symbol * (int * int)

  (* rule is cost * block * end * children *)
  datatype rule = Rule of int * real * symbol * (child list)

  (* branch is symbol * can-grow * food * children *)
  datatype branch = Branch of symbol * bool * real * (child list)

  (* plant is branch-store * space * rules *)
  datatype plant = Plant of branch_store * space * rule_store


  (** functions **)
  (* new creates plant from rules *)
  val new : (symbol * rule) list -> plant

  (* shine returns new p' after shining with strength on p *)
  val shine : real -> plant -> plant

  (*
  (* grow returns p' after growing p *)
  val grow : plant -> plant

  (* to_list returns a flat list of all branches in p *)
  val to_list : plant -> branch list

  (* to_tree ? *)
  (*val to_tree : plant -> TODO *)
  *)

  (* demo is a demo plant *)
  val demo : plant

  (* check_rules returns true if rules are valid *)
  val check_rules : rule list -> bool
end

structure Aggro : AGGRO =
struct
  (** types **)
  type symbol = char

  (* space is min-x * max-x * min-y * max-y *)
  type space = int * int * int * int

  (* child is symbol * pos *)
  datatype child = Child of symbol * (int * int)

  (* branch is symbol * can-grow * food * children *)
  datatype branch = Branch of symbol * bool * real * (child list)

  structure I = struct type t = int; val compare = Int.compare end
  structure DX = DictFn(structure K = I; type V = branch)
  structure DY = DictFn(structure K = I; type V = DX.t)
  type branch_store = DY.t

  (* rule is cost * block * end * children *)
  datatype rule = Rule of int * real * symbol * (child list)

  structure S = struct type t = symbol; val compare = Char.compare end
  structure DR = DictFn(structure K = S; type V = rule)
  type rule_store = DR.t

  (* plant is branch-store * space * rules *)
  datatype plant = Plant of branch_store * space * rule_store


  (** internal helpers **)
  val zero_pos = (0, 0)
  val empty_space = (0, 0, 0, 0)
  val empty_branch_store = DY.new

  fun new_branch sym =
    Branch (sym, true, 0.0, [])

  fun add_branch sym (x, y) store =
    let
      val ys = case DY.find y store of
                SOME ys => ys |
                NONE => DX.new
      val ys' = DX.store x (new_branch sym) ys
    in
      DY.store y ys' store
    end

  fun rules_to_dict rules =
    foldl (fn ((s, r), d) => DR.store s r d) DR.new rules


  (** public interface **)

  (* new creates plant from rules *)
  fun new rules =
    let
      val seed = 42 (* TODO: current time *)
      val rng = Lcg.seed seed Lcg.nr
      val (sym, _) = hd rules
    in
      Plant (add_branch sym zero_pos empty_branch_store, empty_space, rules_to_dict rules)
    end
  
  structure DS = DictFn(structure K = I; type V = real)

  fun sun_dict min max strength =
    foldl (fn (x, d) => DS.store x strength d) DS.new (Lists.seq min max)

  (* shine returns new p' after shining with strength on p *)
  fun shine strength (p as Plant (bs, m, rs)) =
    let
      val (min_x, max_x, min_y, max_y) = m
      fun fy (y, (r, sundict)) =
        let
          fun fx (x, Branch (sym, cg, f, cs), (rd, sund)) =
            let
              val Rule (_, bl, _, _) = DR.fetch sym rs
              val sun = DS.fetch x sund
              val get = sun * bl
              val drop = sun * (1.0 - bl)
            in
              (DX.store x (Branch (sym, cg, f + get, cs)) rd, DS.store x drop sund)
            end
          val xdict = DY.fetch y bs
          val (rxd, rsund) = DX.fold fx (DX.new, sundict) xdict
        in
          (DY.store y rxd r, rsund)
        end
      val sd = sun_dict min_x max_x strength
      val (r, _) = foldl fy (DY.new, sd) (Lists.seq' max_y min_y ~1)
    in
      Plant (r, m, rs)
    end

  (*
  (* grow returns p' after growing p *)
  val grow : plant -> plant

  (* to_list returns a flat list of all branches in p *)
  val to_list : plant -> branch list

  (* to_tree ? *)
  (*val to_tree : plant -> TODO *)
  *)

  (* demo is a demo plant *)
  val demo =
    let
      (* branch types *)
      val upr = #"|"  (* upright *)
      val lft = #"\\" (* left    *)
      val rgt = #"/"  (* right   *)
      val flt = #"_"  (* flat    *)
      val frt = #"O"  (* fruit   *)

      (* growth directions *)
      val ul = (~1,1)
      val us = ( 0,1)
      val ur = ( 1,1)
      val sl = (~1,0)
      val sr = ( 1,0)

      (* helpers *)
      fun crule cs = Rule (1, 0.5, frt, cs)
      fun ch s p = Child (s, p)
    in
      new [(upr, crule [ch flt ul, ch lft ul, ch upr us, ch rgt ur, ch rgt ur]),
           (lft, crule [ch flt ul, ch lft ul, ch upr ul, ch rgt us]),
           (rgt, crule [ch lft us, ch upr ur, ch rgt ur, ch flt ur]),
           (flt, crule [ch flt sl, ch lft sl, ch upr sl, ch upr sr, ch rgt sr, ch flt sr]),
           (frt, Rule (0, 0.5, frt, []))]
    end

  (* check_rules returns true if rules are valid *)
  fun check_rules _ = true
end

(* vim: se ai sts=2 sw=2 et: *)
