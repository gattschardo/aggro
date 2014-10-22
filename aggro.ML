signature AGGRO =
sig
  (** types **)
  type symbol = char

  type pos = int * int

  (* space is min-x * max-x * min-y * max-y *)
  type space = int * int * int * int

  structure DX : DICT
  structure DY : DICT

  type branch_store
  type rule_store

  (* child is symbol * pos *)
  datatype child = Child of symbol * pos

  (* rule is cost * block * end * children *)
  datatype rule = Rule of int * real * symbol * (child list)

  (* branch is symbol * can-grow * food * children *)
  datatype branch = Branch of symbol * bool * real * (pos list)

  (* plant is branch-store * space * rules *)
  datatype plant = Plant of branch_store * space * rule_store


  (** functions **)
  (* new creates plant from rules *)
  val new : (symbol * rule) list -> plant

  (* shine returns new p' after shining with strength on p *)
  val shine : real -> plant -> plant

  (* grow returns p' after growing p *)
  val grow : plant -> plant

  (* to_list returns a flat list of all branches in p *)
  val to_list : plant -> child list

  (* to_tree TODO *)
  (*val to_tree : plant -> TODO *)

  (* demo is a demo plant *)
  val demo : plant

  (* check_rules returns true if rules are valid *)
  val check_rules : rule list -> bool
end

structure Aggro : AGGRO =
struct
  (** types **)
  type symbol = char

  type pos = int * int

  (* space is min-x * max-x * min-y * max-y *)
  type space = int * int * int * int

  (* child is symbol * pos *)
  datatype child = Child of symbol * pos

  (* branch is symbol * can-grow * food * children *)
  datatype branch = Branch of symbol * bool * real * (pos list)

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

  (* branch storage related *)
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

  fun get_branch (x, y) bs =
    DX.fetch x (DY.fetch y bs)

  fun put_branch (x, y) bs b =
    DY.store y (DX.store x b (DY.fetch y bs)) bs

  fun has_branch (x, y) bs =
    case DY.find y bs of
      NONE => false |
      SOME ys =>
        (case DX.find x ys of
          NONE => false |
          SOME _ => true)

  (* growth related *)
  fun growth_options (x, y) s f bs rs =
    let
      val Rule (cost, _, _, cs) = DR.fetch s rs
      fun func (Child (cs, (cx, cy)), a as (r, _)) =
        let
          val np = (x + cx, y + cy)
        in
          if has_branch np bs then
            a
          else
            (if cost > floor f then
              (r, true)
             else
              ((np, f - real cost, cs)::r, true))
        end
    in
      foldl func ([], false) cs
    end

  fun min a b = Int.min (a, b)
  fun max a b = Int.max (a, b)

  fun grow_branch opts pos bs s cs (min_x, max_x, min_y, max_y) rs =
    let
      val l = length opts
      val (cpos as (cx, cy), f, csym) = hd opts (* TODO randomize *)
      val cg = (l > 1)
      val bsp = put_branch pos bs (Branch (s, cg, f, cpos::cs))
      val bsc = add_branch csym cpos bsp
      val m = (min min_x cx, max max_x cx, min min_y cy, max max_y cy)
    in
      Plant (bsc, m, rs)
    end

  fun grow_end pos s f bs rs =
    let
      val Rule (_, _, es, _) = DR.fetch s rs
    in
      put_branch pos bs (Branch (es, false, f, []))
    end

  (* high-level *)
  fun rules_to_dict rules =
    foldl (fn ((s, r), d) => DR.store s r d) DR.new rules

  (*
  fun insert_children pos bs =
    let
      val Branch (s, _, _, cs) = get_branch pos bs
      fun f (p, r) =
        insert_children p bs::r
    in
      (s, foldl f [] cs)
    end
  *)

  fun distribute_food fo cs ibs =
    let
      val food = fo / (real (length cs))
      fun f (c, bs) =
        let
          val Branch (s, cg, f0, ccs) = get_branch c bs
        in
          put_branch c bs (Branch (s, cg, f0 + food, ccs))
        end
    in
      foldl f ibs cs
    end

  fun update _ (Branch (_, false, _, [])) p = p
    | update pos (Branch (s, false, f, cs)) (Plant (bs, m, rs)) =
        let
          val nbs = distribute_food f cs bs
        in
          Plant (put_branch pos nbs (Branch (s, false, 0.0, cs)), m, rs)
        end
    | update pos (Branch (s, true, f, cs)) (p as Plant (bs, m, rs)) =
        let
          val (go, cg) = growth_options pos s f bs rs
        in
          if cg then
            (case go of
              [] => p |
              opts => grow_branch opts pos bs s cs m rs)
          else
            (case cs of
              [] => Plant (grow_end pos s f bs rs, m, rs) |
              _ =>
                let
                  val nb = Branch (s, false, f, cs)
                in
                  update pos nb (Plant (put_branch pos bs nb, m, rs))
                end)
        end

  fun let_grow pos (p as Plant (bs, m, rs)) =
    let
      val b as (Branch (_, _, _, cs)) = get_branch pos bs
    in
      foldl (fn (po, cp) => let_grow po cp) (update pos b p) cs
    end


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
  fun shine strength (Plant (bs, m, rs)) =
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

  (* grow returns p' after growing p *)
  fun grow p = let_grow zero_pos p

  (* to_list returns a flat list of all branches in p *)
  fun to_list (Plant (bs, _, _)) =
    let
      fun fy (y, ys, r) =
        let
          fun fx (x, (Branch (s, _, _, _)), ri) =
            Child (s, (x, y))::ri
        in
          DX.fold fx r ys
        end
    in
      DY.fold fy [] bs
    end

  (* to_tree TODO *)
  (*fun to_tree (Plant (bs, _, _)) =
    insert_children zero_pos bs*)

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
