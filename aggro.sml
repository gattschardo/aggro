signature AGGRO =
sig
  (** types **)
  type symbol = char

  type pos = int * int

  (* child is symbol * pos *)
  datatype child = Child of symbol * pos

  type rule = { cost : real, block : real, es : symbol, cs : child list }

  type plant

  (** functions **)
  (* new creates plant from rules *)
  val new : (symbol * rule) list -> plant

  (* shine returns new p' after shining with strength on p *)
  val shine : real -> plant -> plant

  (* grow returns p' after growing p *)
  val grow : plant -> plant

  (* to_list returns a flat list of all branches in p *)
  val to_list : plant -> child list

  datatype tree = Node of symbol * (tree list)

  (* to_tree returns the tree of symbols in p *)
  val to_tree : plant -> tree

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

  type rule = { cost : real, block : real, es : symbol, cs : child list }

  structure S = struct type t = symbol; val compare = Char.compare end
  structure DR = DictFn(structure K = S; type V = rule)
  type rule_store = DR.t

  (* plant is branch-store * space * rules *)
  datatype plant = Plant of branch_store * rule_store


  (** internal helpers **)

  fun min a b = Int.min (a, b)
  fun max a b = Int.max (a, b)

  fun add a b = a + b
  fun mul a b = a * b

  (* branch storage related *)
  val zero_pos = (0, 0)

  fun new_branch sym =
    Branch (sym, true, 0.0, [])

  fun end_of s rs = #es (DR.fetch s rs)

  fun block_of (Branch (s, _, _, _)) rs = #block (DR.fetch s rs)

  fun update_food f (Branch (s, cg, food, cs)) =
    Branch (s, cg, f food, cs)

  fun toggle_growth (Branch (s, cg, f, cs)) =
    Branch (s, not cg, f, cs)

  fun to_end rs (Branch (s, _, f, cs)) =
    Branch (end_of s rs, false, f, cs)

  fun add_branch sym (x, y) =
    DY.update' y (DX.store x (new_branch sym)) DX.new

  fun has_branch (x, y) bs =
    case DY.find y bs of
      NONE => false |
      SOME ys => DX.is_key x ys

  fun get_branch (x, y) bs =
    DX.fetch x (DY.fetch y bs)

  fun put_branch (x, y) b =
    DY.update y (DX.store x b)

  fun update_branch pos f bs =
    put_branch pos (f (get_branch pos bs)) bs

  fun fold_branches f =
    let
      fun fx y (x, b, a) = f ((x, y), b, a)
      fun fy (y, ys, a) = DX.fold (fx y) a ys
    in
      DY.fold fy
    end

  (* growth related *)
  fun growth_paths (pos as (x, y)) bs rs =
    let
      val Branch (s, _, f, _) = get_branch pos bs
      val {cost, cs, ...} = DR.fetch s rs
      fun check_child (Child (cs, (cx, cy)), paths) =
        let
          val np = (x + cx, y + cy)
          val gp = {pos = np, leftovers = f - cost, sym = cs}
        in
          if has_branch np bs orelse cost > f then
            paths
          else
            gp::paths
        end
    in
      foldl check_child [] cs
    end

  fun grow_branch paths ppos bs =
    let
      val l = length paths
      val {pos, leftovers, sym} = hd paths (* TODO randomize *)
      fun update_parent (Branch (s, _, _, cs)) = Branch (s, l > 1, leftovers, pos::cs)
    in
      add_branch sym pos (update_branch ppos update_parent bs)
    end

  fun grow_end pos rs =
    update_branch pos (to_end rs)

  (* high-level *)
  val rules_to_dict =
    foldl (fn ((s, r), d) => DR.store s r d) DR.new

  datatype tree = Node of symbol * (tree list)

  fun insert_children pos bs =
    let
      val Branch (s, _, _, cs) = get_branch pos bs
      fun f (p, r) =
        insert_children p bs::r
    in
      Node (s, foldl f [] cs)
    end

  fun distribute_food fo cs bs =
    let
      val food = fo / (real (length cs))
    in
      foldl (fn (c, bs) => update_branch c (update_food (add food)) bs) bs cs
    end

  fun try_grow _ (Branch (_, false, _, [])) p = p
    | try_grow pos (Branch (s, false, f, cs)) (Plant (bs, rs)) =
        let
          val nbs = distribute_food f cs bs
        in
          Plant (put_branch pos (Branch (s, false, 0.0, cs)) nbs, rs)
        end
    | try_grow pos (b as Branch (_, true, _, cs)) (Plant (bs, rs)) =
        let
          val paths = growth_paths pos bs rs
        in
          if length paths > 0 then
            Plant (grow_branch paths pos bs, rs)
          else if length cs > 0 then
            let
              val nb = toggle_growth b
            in
              try_grow pos nb (Plant (put_branch pos nb bs, rs))
            end
          else
            Plant (grow_end pos rs bs, rs)
        end

  fun let_grow pos (p as Plant (bs, rs)) =
    let
      val b as (Branch (_, _, _, cs)) = get_branch pos bs
    in
      foldl (fn (po, cp) => let_grow po cp) (try_grow pos b p) cs
    end

  structure DS = DictFn(structure K = I; type V = real)

  fun sun_dict (min, max) strength =
    foldl (fn (x, d) => DS.store x strength d) DS.new (Lists.seq min max)

  val dimensions =
    fold_branches (fn ((x, _), _, (min_x, max_x)) => (min x min_x, max x max_x)) (0, 0)


  (** public interface **)

  (* new creates plant from rules *)
  fun new rules =
    let
      val seed = 42 (* TODO: current time *)
      val rng = Lcg.seed seed Lcg.nr
      val (sym, _) = hd rules
    in
      Plant (add_branch sym zero_pos DY.new, rules_to_dict rules)
    end
  
  (* shine returns new p' after shining with strength on p *)
  fun shine strength (Plant (bs, rs)) =
    let
      fun shi (pos as (x, _), b, (bs, sd)) =
        let
          val block = block_of b rs
          val sun = DS.fetch x sd * block
        in
          (update_branch pos (update_food (add sun)) bs,
           DS.update x (mul (1.0 - block)) sd)
        end
      val sd = sun_dict (dimensions bs) strength
      val (bs, _) = fold_branches shi (bs, sd) bs
    in
      Plant (bs, rs)
    end

  (* grow returns p' after growing p *)
  val grow = let_grow zero_pos

  (* to_list returns a flat list of all branches in p *)
  fun to_list (Plant (bs, _)) =
    fold_branches (fn ((x, y), Branch (s, _, _, _), r) => Child (s, (x, y))::r) [] bs

  (* to_tree returns the tree of symbols in p *)
  fun to_tree (Plant (bs, _)) =
    insert_children zero_pos bs

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
      fun crule cs = {cost=1.0, block=0.5, es=frt, cs=cs}
      fun ch s p = Child (s, p)
    in
      new [(upr, crule [ch flt ul, ch lft ul, ch upr us, ch rgt ur, ch rgt ur]),
           (lft, crule [ch flt ul, ch lft ul, ch upr ul, ch rgt us]),
           (rgt, crule [ch lft us, ch upr ur, ch rgt ur, ch flt ur]),
           (flt, crule [ch flt sl, ch lft sl, ch upr sl, ch upr sr, ch rgt sr, ch flt sr]),
           (frt, {cost=0.0, block=0.5, es=frt, cs=[]})]
    end

  (* check_rules returns true if rules are valid *)
  fun check_rules _ = true
end

(* vim: se ai sts=2 sw=2 et: *)
