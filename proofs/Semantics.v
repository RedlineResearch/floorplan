Require Import Coq.Arith.EqNat.
Require Import Coq.Arith.PeanoNat. (* mod *)
Require Import Coq.Strings.String.
Require Import Coq.Bool.Bool.
Require Import Coq.Logic.Decidable.
Require Import Coq.Arith.Plus.
Require Import Coq.Logic.Classical_Prop.

Require Import Omega.

Require Import Common.
Require Import Binary.
Require Import Map.
Require Import ListSet.
Require Import Calculus.

Definition prim_add_one_byte (A : Type) (a : A) (cumm : set (value A)) (r : (value A)) : set (value A) :=
  set_addv _ cumm (T _ a (B1 _ a) r).

(* Definition tcp (A : Type) (annot : A) a := fix tcp (b : setv A) : setv A :=
  match b with
  | S0 _      => S0v _
  | St _ x xs => union2v _ (set_map_vv _ (fun y => T _ annot x y) a) (tcp xs)
  end. *)

Fixpoint tcp (A : Type) (annot : A) (xs : setv A) (ys : setv A) : setv A :=
  match xs with
  | S0 _        => S0v _
  | St _ x' xs' => union2v _ (set_map_vv _ (fun y => T _ annot x' y) ys) (tcp _ annot xs' ys)
  end.

Fixpoint prim_tree (A : Type) (a : A) (n : nat) : value A :=
  match n with
  | O => B0 _ a
  | S n' => T _ a (B1 _ a) (prim_tree _ a n')
  end.

(*  Computes all permutations with repetitions of length n of the set rjs *)
Fixpoint multi_tree_cross (A : Type) (a : A) (n : nat) (rjs : setv A) : setv A :=
  match n with
  | O    => singletonv _ (B0 _ a)
  | S n' => tcp _ a rjs (multi_tree_cross _ a n' rjs)
  end.

Definition multi_tree_cross_bytes (A : Type) (a : A) (bytes : nat) (n : nat) (rjs : setv A) : setv A :=
  filter _ (fun v => beq_nat (leaves _ v) bytes) (multi_tree_cross _ a n rjs).

Compute multi_tree_cross_bytes _ tt 4 2 (St _ (T _ tt B1tt B1tt) (S0v _)).

Definition annotateIt (A : Type) (annotator : value A -> A) (v : value A) : value A :=
  match v with
  | B0 _ a => B0 _ (annotator v)
  | B1 _ a => B1 _ (annotator v)
  | T _ a v1 v2 => T _ (annotator v) v1 v2
  | N _ a x v1 => N _ (annotator v) x v1
  end.

(* a : address, m : size in bytes, e : core-language expression *)
Definition spec_conc (A : Type) (a0 : A) (annotator : expr -> value A -> A) :=
  fix spec (e : expr) (* Must decrease on e *)
  (* fix spec_θ *) (θ : env) := (* Same e, must decrease on θ *)
  fix spec_isLM (isLM : bool) (* := *)
  (* fix spec_boundLMs *) (boundLMs : nat) := (* Exclusive upper bound on the number of LMs on any path from root to a leaf in the spec *)
  fix spec_LM (lm : nat)
  (* fix spec_a *) (a : nat)
  (* fix spec_m *) (m : nat) : setv A :=
    let rec :=
      (match e with
      | Prim n     => if beq_nat m n then singletonv _ (prim_tree _ a0 n) else S0v _
      | Con n e  => if beq_nat m n then spec e θ false boundLMs O a m else S0v _
      | Align e n  => if beq_nat (m mod n) O then spec e θ false boundLMs O a m else S0v _
      | Named x e  => set_map_vv _ (fun r => (N _ a0 x) r) (spec e θ false boundLMs O a m)
      | Alt e1 e2  => union2v _ (spec e1 θ false boundLMs O a m) (spec e2 θ false boundLMs O a m)
      | Exists x e => unionAllv _ m (fun i => spec e (Env (x, i) θ) false boundLMs O a m)
      | Seq e1 e2  => unionAllv _ m (fun i => tcp _ a0 (spec e1 θ false boundLMs O a i) (spec e2 θ false boundLMs O (a + i) (m - i)))
      | LM x e    => filter _ (fun v => beq_nat m (leaves _ v))
        (match env_lookup θ x with
        | None => S0v _ (* Not validly scoped *)
        | Some n => 
          (match m, n with
          | O, O    => spec e θ false boundLMs O a O (* 0 bytes with 0 instances is valid *)
          | _, O    => S0v _ (* m > 0 bytes with 0 instances is impossible *)
          | _, S n' =>
            (match isLM with
            | false =>
              (match boundLMs with
              | O => S0v _ (* Unreachable iff boundLMs is greater than the number of LM nodes in the expression *)
              | S boundLMs' => spec_isLM true boundLMs' n a m (* initialize lm=n for the true branch below, decrement number of LMs we've seen *)
              end)
            | true =>
              (match lm with
              | O     => singletonv _ (B0 _ a0) (* No more logical multiples, but we *wanted* m>0 bytes, so no dice. S0v _ *)
              | S lm' => filter _ (fun v => beq_nat m (leaves _ v)) (* Only trees with the right number of leaves *)
                         (unionAllv _ m (fun i => tcp _ a0 (spec e θ false boundLMs O a i) (spec_LM lm' (a + i) (m - i))))
              end)
            end)
          end)
        end)
      end)
    in set_map_vv _ (annotateIt _ (annotator e)) rec.

(* A value annotated with a set of expression *)
Definition value_e : Type := value (set expr).
Definition union2e (es0 es1 : set expr) : set expr := union2 _ beq_expr es0 es1.
Definition leaves_e (v : value_e) : nat := leaves _ v.

Fixpoint typeAt (n : nat) (v : value_e) :=
  match n with
  | O =>
    (match v with
    | B1 _ es => es
    | B0 _ es => es
    | N _ es x v => union2e es (typeAt O v)
    | T _ es v1 v2 => union2e es (union2e (typeAt O v1) (if beq_nat O (leaves_e v1) then typeAt O v2 else S0 _))
    end)
  | S n' =>
    (match v with
    | B1 _ es => S0 _
    | B0 _ es => S0 _
    | N _ es x v => typeAt n' v
    | T _ es v1 v2 => union2e (typeAt n' v1) (if beq_nat O (leaves_e v1) then typeAt n' v2 else S0 _)
    end)
  end.

(* Get the nth branch of a value which looks like (T v1 (T v2 (T ... (T v_{k-1} v_k)))) *)
(* Fixpoint nth (A : Type) (n : nat) (v : value A) : option (value A) :=
  match n, v with
  | O, T _ _ v1 v2 => Some v1
  | S n', T _ _ v1 v2 => nth _ n' v2
  | _, _ => None
  end. *)

(* Number of nodes satisfying a predicate: *)
Fixpoint nodes_pred (A : Type) (pred : value A -> bool) (v : value A) : nat :=
  let rec := 
    (match v with
    | B0 _ _  => O
    | B1 _ _  => O
    | N _ _ x v'  => nodes_pred _ pred v'
    | T _ _ t1 t2 => nodes_pred _ pred t1 + nodes_pred _ pred t2
    end)
  in (if pred v then S rec else rec).

(* Number of nodes total: *)
Definition nodes (A : Type) (v : value A) : nat := nodes_pred _ (fun x => true) v.

Fixpoint normalize' (A : Type) (mx : nat) :=
  fix normalize_v (v : value A) : value A :=
  match mx, v with
  | _, N _ a x t          => N _ a x (normalize_v t)
  | _, B0 _ _             => v
  | _, B1 _ _             => v
  | _, T _ _ (B0 _ _) t2  => normalize_v t2
  | _, T _ _ t1 (B0 _ _)  => normalize_v t1
  | O, T _ a t1 t2        => T _ a (normalize_v t1) (normalize_v t2)
  | S mx', T _ a t1 t2    => normalize' _ mx' (T _ a (normalize_v t1) (normalize_v t2))
  end.

Definition normalize (A : Type) (v : value A) : value A := normalize' _ (nodes _ v) v.

(* Number of nodes in an expr AST which satisfy the given predicate *)
Definition count_pred_exp (pred : expr -> bool) := fix pred_exp (e : expr) : nat :=
  let rec :=
    (match e with
    | Prim n      => O
    | Con n e   => pred_exp e
    | Align e n   => pred_exp e
    | Named x e   => pred_exp e
    | Alt e1 e2   => pred_exp e1 + pred_exp e2
    | Exists x e  => pred_exp e
    | Seq e1 e2   => pred_exp e1 + pred_exp e2
    | LM x e'     => pred_exp e'
    end)
  in (if pred e then S rec else rec).

Definition is_LM (e : expr) : bool :=
  match e with
  | LM x e'  => true
  | _         => false
  end.

Compute (fun x y => tt).

Definition gamma (e : expr) (a : nat) (m : nat) := set_map_vv _ (normalize _) (spec_conc unit tt (fun x y => tt) e E0 false (S (count_pred_exp is_LM e)) O a m).

Definition Ttt := T unit tt.
Compute normalize _ (Ttt (Ttt B0tt B0tt) (Ttt B0tt B0tt)).

Definition ex0_expr := (Exists "x" (LM "x" (Prim 3))).

Compute (gamma ex0_expr O 3).

(* All three-byte layouts which fit into m=3 bytes. *)
Example spec_ex0: (gamma ex0_expr O 3) = St _ (Ttt B1tt (Ttt B1tt B1tt)) (S0 _).
Proof. reflexivity. Qed.

Compute (gamma ex0_expr O 6).

Compute (gamma (Exists "x" (LM "x" (Prim 1))) O 6).

Definition set_diff (V : Type) (eq_V : V -> V -> bool) (vs1 vs2 : set V) : set V :=
  filter V (fun v => negb (set_mem _ eq_V vs2 v)) vs1.

Fixpoint freeVars (e : expr) : set string :=
  match e with
  | LM x e' => singleton _ x
  | Exists x e' => set_diff _ eq_string (freeVars e') (singleton _ x)
  | Named x e' =>  freeVars e'
  | Prim n => S0 _
  | Con n e' => freeVars e'
  | Align e' n => freeVars e'
  | Alt e1 e2 => union2 _ eq_string (freeVars e1) (freeVars e2)
  | Seq e1 e2 => union2 _ eq_string (freeVars e1) (freeVars e2)
  end.

Lemma eq_value_leaves: forall (A : Type) (t xs : value A),
  eq_value A t xs = true -> leaves A t = leaves A xs.
Proof. intros A t. induction t.
  - induction xs.
    + simpl. reflexivity.
    + simpl. intros H. inversion H.
    + simpl. intros H. inversion H.
    + simpl. intros H. inversion H.
  - induction xs.
    + simpl. intros H. inversion H.
    + simpl. reflexivity.
    + simpl. intros H. inversion H.
    + simpl. intros H. inversion H.
  - induction xs.
    + simpl. intros H. inversion H.
    + simpl. intros H. inversion H.
    + simpl. intros H. replace (leaves A xs1) with (leaves A t1). replace (leaves A xs2) with (leaves A t2).
      reflexivity. apply IHt2. symmetry in H. apply andb_true_eq in H. symmetry. apply H.
      apply IHt1. symmetry in H. apply andb_true_eq in H. symmetry. apply H.
    + simpl. intros H. inversion H.
  - induction xs.
    + simpl. intros H. inversion H.
    + simpl. intros H. inversion H.
    + simpl. intros H. inversion H.
    + simpl. intros H. symmetry in H. apply andb_true_eq in H. apply IHt. symmetry. apply H. Qed.

Lemma beq_nat_comm: forall (m x y : nat),
  x = y -> ((m =? x) = (m =? y)).
Proof. intros. rewrite H. reflexivity. Qed.

Lemma eq_value_leaves': forall (A : Type) (t1 t2 : value A) (m : nat),
  eq_value A t1 t2 = true -> (m =? leaves A t1) = (m =? leaves A t2).
Proof. intros. apply beq_nat_comm. apply eq_value_leaves. apply H. Qed.

Lemma leaves_annotateIt: forall (A : Type) (fncn : value A -> A) (v : value A),
  leaves A (annotateIt A fncn v) = leaves A v.
Proof. intros. induction v ; trivial. Qed.

Lemma leaves_prim_tree: forall (A : Type) (a0 : A) (m : nat),
  m = leaves A (prim_tree A a0 m).
Proof. intros. induction m.
  - trivial.
  - simpl. apply f_equal. apply IHm. Qed.

Lemma eq_value_comm: forall (A : Type) (t1 t2 : value A),
  eq_value A t1 t2 = eq_value A t2 t1.
Proof. intros A t1. induction t1.
  - simpl. destruct t2 ; reflexivity.
  - simpl. destruct t2 ; reflexivity.
  - intros t2. simpl. induction t2.
    + simpl. reflexivity.
    + simpl. reflexivity.
    + simpl. rewrite IHt1_1. rewrite IHt1_2. reflexivity.
    + simpl. reflexivity.
  - intros t2. simpl. induction t2.
    + simpl. reflexivity.
    + simpl. reflexivity.
    + simpl. reflexivity.
    + simpl. rewrite eq_string_correct. rewrite IHt1. reflexivity. Qed.

Lemma eq_value_annotateIt: forall (A : Type) (fncn : value A -> A) (t1 t2 : value A),
  eq_value A t1 t2 = eq_value A t1 (annotateIt A fncn t2).
Proof. intros. induction t1.
  - simpl. destruct t2 ; trivial.
  - simpl. destruct t2 ; trivial.
  - simpl. destruct t2 ; trivial.
  - simpl. destruct t2 ; trivial. Qed.

Lemma set_mem_map_annotateIt: forall (A : Type) (fncn : value A -> A) (xs : setv A) (t : value A),
  set_memv A (set_map_vv A (annotateIt A fncn) xs) t = set_memv A xs t.
Proof. intros. unfold set_memv. unfold set_map_vv. induction xs.
  - simpl. reflexivity.
  - simpl. rewrite <- eq_value_annotateIt. destruct (eq_value A t a) eqn:H0.
    * reflexivity.
    * apply IHxs. Qed.

Lemma set_mem_empty: forall (A : Type) (t : value A),
  set_memv A (S0v A) t = false.
Proof. intros. induction t ; trivial. Qed.

(* Adding something to a set means it's in there... *)
Lemma set_mem_add_true: forall (A : Type) (t a : value A) (xs : setv A),
  eq_value A t a = true -> set_mem (value A) (eq_value A) (set_add (value A) (eq_value A) xs a) t = true.
Proof. unfold set_addv. induction xs.
  - intros. simpl. rewrite -> H. reflexivity.
  - simpl. destruct (eq_value A a a0).
    * simpl. intros. rewrite H. reflexivity.
    * simpl. intros. destruct (eq_value A t a0). reflexivity.
      apply IHxs. apply H. Qed.

Lemma eq_value_refl_true: forall (A : Type) (c b a : value A),
  eq_value A a b = true
    -> eq_value A a c = eq_value A b c.
Proof. intro A. induction c.
  - intros. destruct a0.
    * destruct b. reflexivity. inversion H. inversion H. inversion H.
    * destruct b. inversion H. reflexivity. inversion H. inversion H.
    * destruct b. inversion H. inversion H. reflexivity. inversion H.
    * destruct b. inversion H. inversion H. inversion H. reflexivity.
  - intros. destruct a0.
    * destruct b. reflexivity. inversion H. inversion H. inversion H.
    * destruct b. inversion H. reflexivity. inversion H. inversion H.
    * destruct b. inversion H. inversion H. reflexivity. inversion H.
    * destruct b. inversion H. inversion H. inversion H. reflexivity.
  - intros. destruct a0.
    * destruct b. reflexivity. inversion H. inversion H. inversion H.
    * destruct b. inversion H. reflexivity. inversion H. inversion H.
    * destruct b.
      + inversion H.
      + inversion H.
      + simpl. simpl in H. rewrite andb_true_iff in H. inversion H.
        rewrite IHc1 with (a := a0_1) (b := b1).
        rewrite IHc2 with (a := a0_2) (b := b2).
        reflexivity. apply H1. apply H0.
      + inversion H.
    * destruct b.
      + inversion H.
      + inversion H.
      + inversion H.
      + reflexivity.
  - intros. destruct a0.
    * simpl. simpl in H. destruct b. reflexivity. reflexivity. inversion H. inversion H.
    * simpl. simpl in H. destruct b. inversion H. reflexivity. inversion H. inversion H.
    * simpl. simpl in H. destruct b. inversion H. inversion H. reflexivity. inversion H.
    * destruct b.
      + simpl. simpl in H. inversion H.
      + inversion H.
      + inversion H.
      + simpl. simpl in H. rewrite andb_true_iff in H. inversion H.
        rewrite IHc with (a := a1) (b := b). 2: { apply H1. }
        rewrite eq_string_refl with (b := s1). reflexivity. apply H0. Qed.

Lemma eq_value_refl_false: forall (A : Type) (y x z : value A),
  eq_value A y x = false
    -> eq_value A y z = true
      -> eq_value A x z = false.
Proof. intro A. induction y.
  - intros. destruct x.
    * simpl in H. inversion H.
    * destruct z. reflexivity. inversion H0. reflexivity. reflexivity.
    * simpl in H. destruct z. reflexivity. reflexivity. inversion H0. reflexivity.
    * simpl in H. destruct z. reflexivity. reflexivity. reflexivity. inversion H0.
  - intros. destruct x.
    * destruct z. inversion H0. reflexivity. reflexivity. reflexivity.
    * destruct z. reflexivity. inversion H. reflexivity. reflexivity.
    * simpl in H. destruct z. reflexivity. reflexivity. inversion H0. reflexivity.
    * simpl in H. destruct z. reflexivity. reflexivity. reflexivity. inversion H0.
  - intros. destruct x.
    * destruct z. inversion H0. reflexivity. reflexivity. reflexivity.
    * destruct z. reflexivity. inversion H0. reflexivity. reflexivity.
    * destruct z. reflexivity. reflexivity. simpl. simpl in H. simpl in H0.
      rewrite andb_true_iff in H0. inversion H0. rewrite eq_value_correct with (a := x1) (b := z1).
      rewrite eq_value_refl_true with (a := z1) (b := y1) (c := x1).
      rewrite eq_value_correct with (a := x2) (b := z2).
      rewrite eq_value_refl_true with (a := z2) (b := y2) (c := x2).
      apply H. rewrite eq_value_correct. apply H2. rewrite eq_value_correct. apply H1.
      reflexivity.
    * destruct z. reflexivity. reflexivity. reflexivity. simpl. simpl in H. simpl in H0. inversion H0.
  - intros. destruct x.
    * destruct z. inversion H0. reflexivity. reflexivity. reflexivity.
    * destruct z. reflexivity. inversion H0. reflexivity. reflexivity.
    * destruct z. reflexivity. reflexivity. inversion H0. reflexivity.
    * destruct z. reflexivity. reflexivity. reflexivity. simpl. simpl in H. simpl in H0.
      rewrite andb_true_iff in H0. inversion H0.
      rewrite eq_value_correct.
      rewrite eq_value_refl_true with (a := z) (c := x) (b := y). 2: { rewrite eq_value_correct. apply H2. }
      destruct (eq_value A y x).
      2: { destruct (eq_string s0 s1) eqn:EQs0s1 ; reflexivity. }
      rewrite andb_comm. simpl. rewrite andb_comm in H. simpl in H.
      apply eq_string_refl_false with (s := s). apply H. apply H1. Qed.

Lemma set_mem_add_false: forall (A : Type) (t a : value A) (xs : setv A),
  eq_value A t a = false -> set_mem _ (eq_value A) (set_add _ (eq_value A) xs a) t = set_mem _ (eq_value A) xs t.
Proof. intros. generalize dependent t. generalize dependent a. induction xs.
  - intros. simpl. rewrite H. reflexivity.
  - intros. simpl. destruct (eq_value A a a0) eqn:H0.
    + simpl. rewrite eq_value_correct. rewrite H0. rewrite -> eq_value_refl_false with (y := a0).
      simpl. rewrite -> H. auto. rewrite eq_value_correct. apply H. rewrite eq_value_correct. apply H0.
    + simpl. rewrite eq_value_correct. rewrite H0. simpl. destruct (eq_value A t a).
      auto. apply IHxs. apply H. Qed.

Lemma set_mem_union_or'': forall (A : Type) (xs ys : setv A) (t : value A),
  set_mem (value A) (eq_value A) (union2 (value A) (eq_value A) xs ys) t
  = orb (set_mem (value A) (eq_value A) xs t) (set_mem (value A) (eq_value A) ys t).
Proof. induction xs.
  - trivial.
  - simpl. intros. simpl. unfold set_memv in IHxs. unfold union2v in IHxs.
    rewrite -> IHxs. destruct (eq_value A t a) eqn:H0.
    + simpl. rewrite set_mem_add_true. rewrite orb_true_r. reflexivity. apply H0.
    + rewrite set_mem_add_false. auto. apply H0. Qed.

Lemma set_mem_union_or': forall (A : Type) (xs ys : setv A) (t : value A),
  set_memv A (union2 (value A) (eq_value A) xs ys) t = orb (set_memv A xs t) (set_memv A ys t).
Proof. unfold set_memv. apply set_mem_union_or''. Qed.

Lemma set_mem_union_or: forall (A : Type) (xs ys : setv A) (t : value A),
  set_memv A (union2v A xs ys) t = orb (set_memv A xs t) (set_memv A ys t).
Proof. unfold set_memv. unfold union2v. apply set_mem_union_or''. Qed.

Definition all_same_size (A : Type) (vs : setv A) :=
  match vs with
  | S0 _ => true
  | St _ v vs' => set_fold_left (value A) bool (fun bl v' => leaves A v' =? leaves A v) vs' true
  end.

Lemma contra: forall (a : bool) (b c : Type),
  (a = true -> b = c) -> (b <> c -> a = false).
Proof. intros. destruct a.
  - contradict H0. apply H. reflexivity.
  - reflexivity. Qed.

(* Definition tcp (A : Type) (a0 : A) (annotator : expr -> value A -> A)
  (e1 e2 : expr) (a lm boundLMs m : nat) (isLM : bool) (theta : env)
  := fun i : nat => tcp A a0
            (spec_conc A a0 annotator e1 theta isLM boundLMs lm a i)
            (spec_conc A a0 annotator e2 theta isLM boundLMs lm (a + i) (m - i)). *)

Lemma beq_eq_nat: forall (n m : nat),
  n = m -> ((n =? m) = true).
Proof. intros. induction n.
  - simpl. destruct m. reflexivity. inversion H.
  - simpl. destruct m. inversion H. inversion H. symmetry. apply beq_nat_refl. Qed.

Lemma lte_plus_minus: forall (j m : nat),
  j <= m -> j + (m - j) = m.
Proof. apply le_plus_minus_r. Qed.

Lemma negb_both_sides:
  forall (b1 b2 : bool),
  b1 <> b2 <-> negb b1 <> negb b2.
Proof. split. intros. destruct b1.
  * destruct b2 ; auto.
  * destruct b2 ; auto.
  * destruct b1. destruct b2 ; auto. destruct b2 ; auto. Qed.

Lemma set_mem_S0_false:
  forall (A : Type) (t : value A),
  set_memv A (S0 (value A)) t = false.
Proof. intros. auto. Qed.

Hint Rewrite set_mem_S0_false.

Lemma both_imply:
  forall (A B C : Prop),
  (A -> C) -> (B -> C) -> (A \/ B -> C).
Proof. intros. inversion H1.
  - apply H. apply H2.
  - apply H0. apply H2. Qed.

Lemma union2_leaves_preserve:
  forall (A : Type) (t : value A) (m : nat) (xs ys : setv A),
    (forall (t2 : value A), set_memv A xs t2 = true -> m = leaves A t2) ->
    (forall (t2 : value A), set_memv A ys t2 = true -> m = leaves A t2)
      -> set_memv A (union2v A xs ys) t = true
        -> m = leaves A t.
Proof. intros. rewrite set_mem_union_or in H1. induction xs.
  - simpl in H1. apply H0. apply H1.
  - induction ys.
    * rewrite set_mem_S0_false in H1. rewrite orb_comm in H1. simpl in H1.
      apply H. apply H1.
    * generalize dependent A. unfold set_memv. simpl. intros.
      destruct (eq_value A t a) eqn:taEq.
      destruct (eq_value A t a0) eqn:ta0Eq.
      rewrite orb_comm in IHxs. simpl in IHxs. apply IHxs.
      intros. rewrite H with (t2 := t2). reflexivity. rewrite -> H2.
      destruct (eq_value A t2 a) ; auto. auto.
      rewrite H with (t2 := t). reflexivity. rewrite taEq. reflexivity.
      destruct (eq_value A t a0) eqn:ta0Eq.
      rewrite orb_comm in IHxs. simpl in IHxs. apply IHxs.
      intros. rewrite H with (t2 := t2). reflexivity.
      destruct (eq_value A t2 a) eqn:t2aEq. reflexivity. apply H2.
      reflexivity. specialize (H t). specialize (H0 t).
      rewrite taEq in H. rewrite ta0Eq in H0.
      assert (Himpl: (set_mem (value A) (eq_value A) xs t = true
                  \/ set_mem (value A) (eq_value A) ys t = true) -> m = leaves A t).
      { intros. inversion H2. apply H. apply H3. apply H0. apply H3. }
      apply Himpl. rewrite <- orb_true_iff. apply H1. Qed.

Lemma unionAll_leaves_preserve:
  forall (A : Type) (t : value A) (m k : nat) (fncn : nat -> setv A),
    (* Some fncn produces only things with 'm' leaves: *)
    (forall (t2 : value A) (i : nat), set_memv A (fncn i) t2 = true -> m = leaves A t2)
      -> set_memv A (unionAll (value A) (eq_value A) k fncn) t = true
        -> m = leaves A t.
Proof. intros. induction k.
  - simpl in H0. destruct m.
    + apply H with (i := 0). apply H0.
    + apply H with (i := 0). apply H0.
  - simpl in H0. specialize (H t).
    assert (Himpl: (set_memv A (fncn (S k)) t = true
                \/ (set_memv A (unionAll (value A) (eq_value A) k fncn) t = true)) -> m = leaves A t).
    intros. inversion H1. apply H with (i := S k). apply H2. apply IHk. apply H2.
    apply Himpl. rewrite <- orb_true_iff.
    generalize dependent A. unfold set_memv. simpl. intros.
    rewrite set_mem_union_or'' in H0. apply H0. Qed.

Lemma unionAll_leaves_preserve_lt:
  forall (A : Type) (t : value A) (m k : nat) (fncn : nat -> setv A),
    (* Some fncn produces only things with 'm' leaves for inputs i<=m: *)
    k <= m ->
    (forall (t2 : value A) (i : nat), i <= m -> set_memv A (fncn i) t2 = true -> m = leaves A t2)
      -> set_memv A (unionAll (value A) (eq_value A) k fncn) t = true
        -> m = leaves A t.
Proof. intros. induction k.
  - simpl in H0. destruct m.
    * apply H0 with (i := 0). reflexivity. apply H1.
    * apply H0 with (i := 0). omega. apply H1.
  - simpl in H1. specialize (H0 t).
    assert (Himpl: (set_memv A (fncn (S k)) t = true)
                \/ (set_memv A (unionAll (value A) (eq_value A) k fncn) t = true) -> m = leaves A t).
    * intros. inversion H2. apply H0 with (i := S k). apply H. apply H3.
      apply IHk. omega. apply H3.
    * apply Himpl. rewrite <- orb_true_iff. rewrite set_mem_union_or' in H1.
      apply H1. Qed.

Lemma set_memv_filter_leaves:
  forall (A : Type) (t : value A) (xs : setv A) (m : nat) (fncn : value A -> bool),
  (forall (t1 : value A), fncn t1 = true -> m = leaves A t1)
    -> (forall (t1 t2 : value A), eq_value A t1 t2 = true -> fncn t1 = fncn t2)
      -> set_memv A (filter (value A) fncn xs) t = true -> m = leaves A t.
Proof. intros A t xs m fncn Hfncn Heq. unfold set_memv. induction xs.
  - simpl. intros. inversion H.
  - simpl. destruct (fncn a) eqn:fncnVal.
    * simpl. destruct (eq_value A t a) eqn:eqta.
      + intros. apply Hfncn. assert (Hfeq: fncn t = fncn a). apply Heq. apply eqta.
        rewrite Hfeq. apply fncnVal.
      + apply IHxs.
    * apply IHxs. Qed.

Compute tcp _ 0 (Stv _ (B1 _ 0) (S0v _)) (Stv _ (B1 _ 0) (S0v _)).

Lemma tcp_empty_l: forall (A : Type) (a0 : A) (xs : setv A),
  tcp A a0 (S0 (value A)) xs = S0 (value A).
Proof. simpl. unfold S0v. reflexivity. Qed.

Lemma tcp_empty_r: forall (A : Type) (a0 : A) (xs : setv A),
  tcp A a0 xs (S0 (value A)) = S0 (value A).
Proof. intros. induction xs.
  - auto.
  - simpl. rewrite -> IHxs. unfold union2v. simpl. reflexivity. Qed.

Lemma set_mem_leaves: forall (A : Type) (xs : setv A) (t : value A) (k : nat),
  (forall (x : value A), leaves A x = k)
  -> set_memv A xs t = true
  -> leaves A t = k.
Proof. intros. apply H with (x := t). Qed.

Lemma set_mem_map_leaves_2: forall (A : Type) (a0 : A) (j : nat) (pfx : value A) (t : value A) (ys: setv A),
  (forall y : value A, set_mem (value A) (eq_value A) ys y = true -> leaves A y = j)
  -> set_mem (value A) (eq_value A) (set_map_vv A (fun y : value A => T A a0 pfx y) ys) t = true
  -> leaves A t = leaves A pfx + j.
Proof. intros. induction ys.
  - inversion H0.
  - simpl in *. destruct (eq_value A t (T A a0 pfx a)) eqn:eqva.
    + rewrite eq_value_leaves with (t := t) (xs := T A a0 pfx a) ; auto.
      simpl. specialize (H a). rewrite eq_value_v_v in H. rewrite H ; auto.
    + apply IHys.
      * intro. specialize (H y). intros. apply H. destruct (eq_value A y a).
        auto. apply H1.
      * unfold set_map_vv. apply H0. Qed.

Lemma set_mem_tcp: forall (A : Type) (a0 : A) (xs ys : setv A) (t : value A) (i j : nat),
      (forall (x : value A), set_memv A xs x = true -> leaves A x = i)
  ->  (forall (y : value A), set_memv A ys y = true -> leaves A y = j)
  ->  set_memv A (tcp A a0 xs ys) t = true -> leaves A t = i + j.
Proof. unfold set_memv. intros A a0 xs ys t i j Hx Hy. induction xs.
  - simpl. intros. inversion H.
  - simpl. intro Hmem. unfold union2v in Hmem. rewrite set_mem_union_or'' in Hmem.
    rewrite orb_true_iff in Hmem. inversion Hmem.
    2: { apply IHxs. unfold set_memv in Hx. simpl in Hx. intro x. specialize (Hx x).
         intros. destruct (eq_value A x a) eqn:xaEq. apply Hx ; auto. apply Hx.
         unfold set_memv in H0. apply H0. apply H. }
    assert (Hlvsi: leaves A a = i). apply Hx. simpl. rewrite eq_value_v_v; auto.
    rewrite <- Hlvsi. apply set_mem_map_leaves_2 with (a0 := a0) (ys := ys).
    apply Hy. apply H. Qed.

Lemma set_mem_tcp': forall (A : Type) (a0 : A) (xs ys : setv A) (t : value A) (i j m : nat),
      (forall (x : value A), set_memv A xs x = true -> i = leaves A x)
  ->  (forall (y : value A), set_memv A ys y = true -> j = leaves A y)
  ->  i + j = m
  ->  set_memv A (tcp A a0 xs ys) t = true -> m = leaves A t.
Proof. intros. rewrite <- H1. symmetry. apply set_mem_tcp with (a0 := a0) (xs := xs) (ys := ys).
  symmetry in H. apply H. symmetry in H0. apply H0. apply H2. Qed.

(* Lemma seq_size: forall (A : Type) (annotator : expr -> value A -> A) (a0 : A)
  (t : value A) (e1 e2 : expr) (a lm boundLMs m : nat) (isLM : bool) (theta : env),
    (forall (x y : value A) (j : nat),
      j <= m
        -> set_memv A (spec_conc A a0 annotator e1 theta isLM boundLMs lm a j) x = true
          -> set_memv A (spec_conc A a0 annotator e2 theta isLM boundLMs lm (a + j) (m - j)) y = true
            -> leaves A x + leaves A y = m)
              -> (set_memv A (unionAllv A m (fun i : nat => tcp A a0
                (spec_conc A a0 annotator e1 theta isLM boundLMs lm a i)
                (spec_conc A a0 annotator e2 theta isLM boundLMs lm (a + i) (m - i)))) t = true
                -> m = (leaves A t)).
Proof. intros A annotator a0 t e1 e2 a lm boundLMs m isLM theta Hcond. apply unionAll_leaves_preserve_lt.
  reflexivity. intros t2 i iltem. intros. symmetry.
  apply set_mem_tcp' with (a0 := a0) (xs := (spec_conc A a0 annotator e1 theta isLM boundLMs lm a i))
                                     (ys := (spec_conc A a0 annotator e2 theta isLM boundLMs lm (a + i) (m - i)))
    (i := i) (j := m - i). *)

(* Lemma seq_size': forall (A : Type) (a0 : A) (t : value A) (xs ys : nat -> setv A) (m k : nat),
    (forall (x : value A), set_memv A (xs k) x = true -> leaves A x = k) ->
    (forall (y : value A), set_memv A (ys k) y = true -> leaves A y = m - k) ->
    (forall (x y : value A),
         set_memv A (xs k) x = true
      -> set_memv A (ys k) y = true
      -> leaves A x + leaves A y = m)
          -> set_memv A (unionAllv A m (fun i : nat => tcp A a0 (xs i) (ys i))) t = true
          -> m = (leaves A t).
Proof. intros A a0 t xs ys m k Hxs Hys lte Hcond. apply unionAll_leaves_preserve_lt; auto.
  intros t2 i lte' mem. symmetry. apply set_mem_tcp' with (a0 := a0) (xs := xs k) (ys := ys k) (i := k) (j := m - k).
  apply Hxs. apply Hys. inversion lte. omega. omega. apply mem. Qed.

Lemma seq_size:  forall (A : Type) (annotator : expr -> value A -> A) (a0 : A) (t : value A)
  (e1 e2 : expr) (a lm boundLMs : nat) (isLM : bool) (theta : env) (m k : nat),
    (forall x : value A, set_memv A (spec_conc A a0 annotator e1 theta isLM boundLMs lm a k) x = true -> leaves A x = k) ->
    (forall y : value A, set_memv A (spec_conc A a0 annotator e2 theta isLM boundLMs lm (a + k) (m - k)) y = true -> leaves A y = m - k) ->
    k <= m ->
    (forall (x y : value A),
         set_memv A (spec_conc A a0 annotator e1 theta isLM boundLMs lm a k) x = true
      -> set_memv A (spec_conc A a0 annotator e2 theta isLM boundLMs lm (a + k) (m - k)) y = true
      -> leaves A x + leaves A y = m)
          -> set_memv A (unionAllv A m (fun i : nat => tcp A a0 (spec_conc A a0 annotator e1 theta isLM boundLMs lm a i)
                                                                (spec_conc A a0 annotator e2 theta isLM boundLMs lm (a + i) (m - i)))) t = true
          -> m = (leaves A t).
Proof. intros. apply seq_size' with (a0 := a0)
  (xs := (spec_conc A a0 annotator e1 theta isLM boundLMs lm a k))
  (ys := (spec_conc A a0 annotator e2 theta isLM boundLMs lm (a + k) (m - k)))
  (k := k) ; auto. Qed. *)

(*  destruct (spec_conc A a0 annotator e2 theta isLM boundLMs lm (a + i) (m - i)) eqn:tcpRHS.
set_memv A (unionAllv m fncn) t = true  *)

(* Size-correctness: *)
Lemma size_correctness: forall
    (e : expr) (m : nat)
    (A : Type) (t : value A) (a0 : A) (annotator : expr -> value A -> A)
    (a lm boundLMs : nat) (isLM : bool) (θ : env),
  (set_memv _ (spec_conc A a0 annotator e θ isLM boundLMs lm a m) t = true
  -> beq_nat m (leaves _ t) = true).
Proof. intros. apply beq_eq_nat.
  generalize dependent H.
  generalize dependent θ.
  generalize dependent isLM.
  generalize dependent boundLMs.
  generalize dependent lm.
  generalize dependent a.
  generalize dependent annotator.
  generalize dependent a0.
  generalize dependent t.
  generalize dependent A.
  generalize dependent m.
  induction e.
  - unfold spec_conc. unfold set_map_vv. destruct boundLMs.
    + destruct lm.
      * intros isLM theta. unfold set_memv. destruct (m =? n) eqn:H0.
        { simpl. destruct (eq_value A t (annotateIt A (annotator (Prim n)) (prim_tree A a0 n))) eqn:H1.
          intros H2. replace (leaves A t) with (leaves A (annotateIt A (annotator (Prim n)) (prim_tree A a0 n))).
          apply beq_nat_true in H0. rewrite <- H0. rewrite leaves_annotateIt. rewrite <- leaves_prim_tree.
          symmetry. reflexivity. apply eq_value_leaves. rewrite eq_value_comm. apply H1.
          intros H2. inversion H2. }
        { simpl. intros H2. inversion H2. }
      * intros isLM theta. unfold set_memv. destruct (m =? n) eqn:H0.
        { simpl. destruct (eq_value A t (annotateIt A (annotator (Prim n)) (prim_tree A a0 n))) eqn:H1.
          intros H2. replace (leaves A t) with (leaves A (annotateIt A (annotator (Prim n)) (prim_tree A a0 n))).
          apply beq_nat_true in H0. rewrite <- H0. rewrite leaves_annotateIt. rewrite <- leaves_prim_tree.
          symmetry. reflexivity. apply eq_value_leaves. rewrite eq_value_comm. apply H1.
          intros H2. inversion H2. }
        { simpl. intros H2. inversion H2. }
    + destruct lm.
      * intros isLM theta. unfold set_memv. destruct (m =? n) eqn:H0.
        { simpl. destruct (eq_value A t (annotateIt A (annotator (Prim n)) (prim_tree A a0 n))) eqn:H1.
          intros H2. replace (leaves A t) with (leaves A (annotateIt A (annotator (Prim n)) (prim_tree A a0 n))).
          apply beq_nat_true in H0. rewrite <- H0. rewrite leaves_annotateIt. rewrite <- leaves_prim_tree.
          symmetry. reflexivity. apply eq_value_leaves. rewrite eq_value_comm. apply H1.
          intros H2. inversion H2. }
        { simpl. intros H2. inversion H2. }
      * intros isLM theta. unfold set_memv. destruct (m =? n) eqn:H0.
        { simpl. destruct (eq_value A t (annotateIt A (annotator (Prim n)) (prim_tree A a0 n))) eqn:H1.
          intros H2. replace (leaves A t) with (leaves A (annotateIt A (annotator (Prim n)) (prim_tree A a0 n))).
          apply beq_nat_true in H0. rewrite <- H0. rewrite leaves_annotateIt. rewrite <- leaves_prim_tree.
          symmetry. reflexivity. apply eq_value_leaves. rewrite eq_value_comm. apply H1.
          intros H2. inversion H2. }
        { simpl. intros H2. inversion H2. }
  - unfold spec_conc. destruct boundLMs.
    + destruct lm.
      * simpl. intros isLM theta. destruct (m =? n) eqn:H0.
        { simpl. rewrite set_mem_map_annotateIt. unfold spec_conc in IHe.
          apply IHe with (A := A) (a0 := a0) (annotator := annotator). }
        { rewrite set_mem_map_annotateIt. rewrite set_mem_empty. intros H. inversion H. }
      * simpl. intros isLM theta. destruct (m =? n) eqn:H0.
        { simpl. rewrite set_mem_map_annotateIt. unfold spec_conc in IHe.
          apply IHe with (A := A) (a0 := a0) (annotator := annotator). }
        { rewrite set_mem_map_annotateIt. rewrite set_mem_empty. intros H. inversion H. }
    + destruct lm.
      * simpl. intros isLM theta. destruct (m =? n) eqn:H0.
        { simpl. rewrite set_mem_map_annotateIt. unfold spec_conc in IHe.
          apply IHe with (A := A) (a0 := a0) (annotator := annotator). }
        { rewrite set_mem_map_annotateIt. rewrite set_mem_empty. intros H. inversion H. }
      * simpl. intros isLM theta. destruct (m =? n) eqn:H0.
        { simpl. rewrite set_mem_map_annotateIt. unfold spec_conc in IHe.
          apply IHe with (A := A) (a0 := a0) (annotator := annotator). }
        { rewrite set_mem_map_annotateIt. rewrite set_mem_empty. intros H. inversion H. }
  (* Align *)
  - unfold spec_conc. destruct boundLMs.
    + destruct lm.
      * destruct (m mod n =? 0).
        { intros isLM theta. rewrite set_mem_map_annotateIt. unfold spec_conc in IHe.
          apply IHe with (A := A) (a0 := a0) (annotator := annotator). }
        { rewrite set_mem_map_annotateIt. rewrite set_mem_empty. intros isLM theta H. inversion H. }
      * destruct (m mod n =? 0).
        { intros isLM theta. rewrite set_mem_map_annotateIt. unfold spec_conc in IHe.
          apply IHe with (A := A) (a0 := a0) (annotator := annotator). }
        { rewrite set_mem_map_annotateIt. rewrite set_mem_empty. intros isLM theta H. inversion H. }
    + destruct lm.
      * destruct (m mod n =? 0).
        { intros isLM theta. rewrite set_mem_map_annotateIt. unfold spec_conc in IHe.
          apply IHe with (A := A) (a0 := a0) (annotator := annotator). }
        { rewrite set_mem_map_annotateIt. rewrite set_mem_empty. intros isLM theta H. inversion H. }
      * destruct (m mod n =? 0).
        { intros isLM theta. rewrite set_mem_map_annotateIt. unfold spec_conc in IHe.
          apply IHe with (A := A) (a0 := a0) (annotator := annotator). }
        { rewrite set_mem_map_annotateIt. rewrite set_mem_empty. intros isLM theta H. inversion H. }
  (* Seq *)
  - unfold spec_conc. destruct boundLMs.
    + destruct lm.
      * intros isLM theta. rewrite set_mem_map_annotateIt.
        apply unionAll_leaves_preserve_lt. auto. intros t2 i lte. apply set_mem_tcp' with (i := i) (j := m - i).
        intro x. unfold spec_conc in IHe1. apply IHe1 with (m := i) (A := A)
          (θ := theta) (annotator := annotator) (isLM := false) (boundLMs := 0) (lm := 0) (a0 := a0).
        intro y. apply IHe2 with (m := m - i) (A := A)
          (θ := theta) (annotator := annotator) (isLM := false) (boundLMs := 0) (lm := 0) (a0 := a0).
        omega.
      * intros isLM theta. rewrite set_mem_map_annotateIt.
        apply unionAll_leaves_preserve_lt. auto. intros t2 i lte. apply set_mem_tcp' with (i := i) (j := m - i).
        intro x. unfold spec_conc in IHe1. apply IHe1 with (m := i) (A := A)
          (θ := theta) (annotator := annotator) (isLM := false) (boundLMs := 0) (lm := 0) (a0 := a0).
        intro y. apply IHe2 with (m := m - i) (A := A)
          (θ := theta) (annotator := annotator) (isLM := false) (boundLMs := 0) (lm := 0) (a0 := a0).
        omega.
    + destruct lm.
      * intros isLM theta. rewrite set_mem_map_annotateIt.
        apply unionAll_leaves_preserve_lt. auto. intros t2 i lte. apply set_mem_tcp' with (i := i) (j := m - i).
        intro x. unfold spec_conc in IHe1. apply IHe1 with (m := i) (A := A)
          (θ := theta) (annotator := annotator) (isLM := false) (boundLMs := S boundLMs) (lm := 0) (a0 := a0).
        intro y. apply IHe2 with (m := m - i) (A := A)
          (θ := theta) (annotator := annotator) (isLM := false) (boundLMs := S boundLMs) (lm := 0) (a0 := a0).
        omega.
      * intros isLM theta. rewrite set_mem_map_annotateIt.
        apply unionAll_leaves_preserve_lt. auto. intros t2 i lte. apply set_mem_tcp' with (i := i) (j := m - i).
        intro x. unfold spec_conc in IHe1. apply IHe1 with (m := i) (A := A)
          (θ := theta) (annotator := annotator) (isLM := false) (boundLMs := S boundLMs) (lm := 0) (a0 := a0).
        intro y. apply IHe2 with (m := m - i) (A := A)
          (θ := theta) (annotator := annotator) (isLM := false) (boundLMs := S boundLMs) (lm := 0) (a0 := a0).
        omega.
  (* Alt *)
  - simpl. destruct boundLMs.
    + destruct lm.
      * intros isLM theta. rewrite set_mem_map_annotateIt. rewrite set_mem_union_or. intros.
        destruct (set_memv A (spec_conc A a0 annotator e1 theta false 0 0 a m) t) eqn:H0.
        { apply IHe1 with (annotator := annotator) (A := A) (isLM := false) (lm := 0) (boundLMs := 0) (a0 := a0) (a := a) (θ := theta). apply H0. }
        { destruct (set_memv A (spec_conc A a0 annotator e2 theta false 0 0 a m) t) eqn:H1.
          apply IHe2 with (annotator := annotator) (A := A) (isLM := false) (lm := 0) (boundLMs := 0) (a0 := a0) (a := a) (θ := theta). apply H1.
          inversion H. }
      * intros isLM theta. rewrite set_mem_map_annotateIt. rewrite set_mem_union_or. intros.
        destruct (set_memv A (spec_conc A a0 annotator e1 theta false 0 0 a m) t) eqn:H0.
        { apply IHe1 with (annotator := annotator) (A := A) (isLM := false) (lm := 0) (boundLMs := 0) (a0 := a0) (a := a) (θ := theta). apply H0. }
        { destruct (set_memv A (spec_conc A a0 annotator e2 theta false 0 0 a m) t) eqn:H1.
          apply IHe2 with (annotator := annotator) (A := A) (isLM := false) (lm := 0) (boundLMs := 0) (a0 := a0) (a := a) (θ := theta). apply H1.
          inversion H. }
    + destruct lm.
      * intros isLM theta. rewrite set_mem_map_annotateIt. rewrite set_mem_union_or. intros.
        destruct (set_memv A (spec_conc A a0 annotator e1 theta false (S boundLMs) 0 a m) t) eqn:H0.
        { apply IHe1 with (annotator := annotator) (A := A) (isLM := false) (lm := 0) (boundLMs := S boundLMs) (a0 := a0) (a := a) (θ := theta). apply H0. }
        { destruct (set_memv A (spec_conc A a0 annotator e2 theta false (S boundLMs) 0 a m) t) eqn:H1.
          apply IHe2 with (annotator := annotator) (A := A) (isLM := false) (lm := 0) (boundLMs := S boundLMs) (a0 := a0) (a := a) (θ := theta). apply H1.
          inversion H. }
      * intros isLM theta. rewrite set_mem_map_annotateIt. rewrite set_mem_union_or. intros.
        destruct (set_memv A (spec_conc A a0 annotator e1 theta false (S boundLMs) 0 a m) t) eqn:H0.
        { apply IHe1 with (annotator := annotator) (A := A) (isLM := false) (lm := 0) (boundLMs := S boundLMs) (a0 := a0) (a := a) (θ := theta). apply H0. }
        { destruct (set_memv A (spec_conc A a0 annotator e2 theta false (S boundLMs) 0 a m) t) eqn:H1.
          apply IHe2 with (annotator := annotator) (A := A) (isLM := false) (lm := 0) (boundLMs := S boundLMs) (a0 := a0) (a := a) (θ := theta). apply H1.
          inversion H. }
  (* Named *)
  - simpl. destruct boundLMs.
    + destruct lm.
      * intros isLM theta. rewrite set_mem_map_annotateIt. intros. apply set_mem_map_leaves with
        (fncn := (fun r : value A => N A a0 s r)) (xs := (spec_conc A a0 annotator e theta false 0 0 a m)).
        intros.
        rewrite IHe with (m := m) (A := A) (t := x1) (a0 := a0) (annotator := annotator) (a := a) (lm := 0)
                         (boundLMs := 0) (isLM := false) (θ := theta). reflexivity. apply H0.
        simpl. reflexivity. unfold set_map_vv in H. apply H.
      * intros isLM theta. rewrite set_mem_map_annotateIt. intros. apply set_mem_map_leaves with
        (fncn := (fun r : value A => N A a0 s r)) (xs := (spec_conc A a0 annotator e theta false 0 0 a m)).
        intros.
        rewrite IHe with (m := m) (A := A) (t := x1) (a0 := a0) (annotator := annotator) (a := a) (lm := 0)
                         (boundLMs := 0) (isLM := false) (θ := theta). reflexivity. apply H0.
        simpl. reflexivity. unfold set_map_vv in H. apply H.
    + destruct lm.
      * intros isLM theta. rewrite set_mem_map_annotateIt. intros. apply set_mem_map_leaves with
        (fncn := (fun r : value A => N A a0 s r)) (xs := (spec_conc A a0 annotator e theta false (S boundLMs) 0 a m)).
        intros.
        rewrite IHe with (m := m) (A := A) (t := x1) (a0 := a0) (annotator := annotator) (a := a) (lm := 0)
                         (boundLMs := S boundLMs) (isLM := false) (θ := theta). reflexivity. apply H0.
        simpl. reflexivity. unfold set_map_vv in H. apply H.
      * intros isLM theta. rewrite set_mem_map_annotateIt. intros. apply set_mem_map_leaves with
        (fncn := (fun r : value A => N A a0 s r)) (xs := (spec_conc A a0 annotator e theta false (S boundLMs) 0 a m)).
        intros.
        rewrite IHe with (m := m) (A := A) (t := x1) (a0 := a0) (annotator := annotator) (a := a) (lm := 0)
                         (boundLMs := S boundLMs) (isLM := false) (θ := theta). reflexivity. apply H0.
        simpl. reflexivity. unfold set_map_vv in H. apply H.
  (* Exists *)
  - simpl. destruct boundLMs.
    + destruct lm.
      * intros isLM theta. rewrite set_mem_map_annotateIt. unfold unionAllv. apply unionAll_leaves_preserve.
        intro i. intro t0. apply IHe.
      * intros isLM theta. rewrite set_mem_map_annotateIt. unfold unionAllv. apply unionAll_leaves_preserve.
        intro i. intro t0. apply IHe.
    + destruct lm.
      * intros isLM theta. rewrite set_mem_map_annotateIt. unfold unionAllv. apply unionAll_leaves_preserve.
        intro i. intro t0. apply IHe.
      * intros isLM theta. rewrite set_mem_map_annotateIt. unfold unionAllv. apply unionAll_leaves_preserve.
        intro i. intro t0. apply IHe.
  (* LM *)
  - simpl. destruct boundLMs.
    + destruct lm.
      * intros isLM theta. rewrite set_mem_map_annotateIt. apply set_memv_filter_leaves.
        intro t1. apply beq_nat_true. intros t1 t2. apply eq_value_leaves'.
      * intros isLM theta. rewrite set_mem_map_annotateIt. apply set_memv_filter_leaves.
        intro t1. apply beq_nat_true. intros t1 t2. apply eq_value_leaves'.
    + destruct lm.
      * intros isLM theta. rewrite set_mem_map_annotateIt. apply set_memv_filter_leaves.
        intro t1. apply beq_nat_true. intros t1 t2. apply eq_value_leaves'.
      * intros isLM theta. rewrite set_mem_map_annotateIt. apply set_memv_filter_leaves.
        intro t1. apply beq_nat_true. intros t1 t2. apply eq_value_leaves'.
Qed.

(* Lemma constraint_equivocacy: forall (A : Type) (a0 : A) (annotator : expr -> value A -> A) (t : value A) (a m lm s : nat) (isLM : bool) (e : expr) (θ : env),
    (set_memv _ (spec_conc A a0 annotator e         θ isLM (S (count_pred_exp is_LM e        )) lm a m) t = true -> (beq_nat s (leaves _ t) = true
  -> set_memv _ (spec_conc A a0 annotator (Con s e) θ isLM (S (count_pred_exp is_LM (Con s e))) lm a m) t = true)).
Proof. *)

Theorem constraint_equivocacy: forall (A : Type) (a0 : A) (annotator : expr -> value A -> A) (t : value A) (a m lm s : nat) (isLM : bool) (e : expr) (θ : env),
    (set_memv _ (spec_conc A a0 annotator e         θ false (S (count_pred_exp is_LM e        )) O  a m) t = true
  -> set_memv _ (spec_conc A a0 annotator (Con m e) θ isLM  (S (count_pred_exp is_LM (Con s e))) lm a m) t = true).
Proof. intros. simpl. destruct lm eqn:lmVal.
  - rewrite set_mem_map_annotateIt. rewrite <- beq_nat_refl. apply H.
  - rewrite set_mem_map_annotateIt. rewrite <- beq_nat_refl. apply H. Qed.

(* Sum from 0 to max inclusive, over the given function: *)
Fixpoint sum (max : nat) (fncn : nat -> nat) : nat :=
  match max with
  | O => fncn O
  | S max' => (fncn max) + (sum max' fncn)
  end.

Fixpoint nth (A : Type) (n : nat) (v : value A) : value A :=
  match n, v with
  | O, T _ _ v1 v2      => v1
  | S n', T _ _ v1 v2   => nth _ n' v2
  | _, _                => v
  end.

Theorem array_mapping: forall (A : Type) (a0 : A) (annotator : expr -> value A -> A) (n : nat)
  (t : value A) (s a m lm c : nat) (isLM : bool) (e : expr) (θ : env) (x : string),
    (set_memv _ (spec_conc A a0 annotator (LM x (Con s e)) (Env (x, c) θ) isLM (S (count_pred_exp is_LM (LM x (Con s e)))) lm a m) t = true
  -> n <= ((leaves _ t) / s)
  -> sum (n - 1) (fun i => leaves _ (nth _ i t)) = s * n).
Proof. intros. induction n.
  - simpl.

  - simpl. simpl in H. rewrite eq_string_s_s in H. simpl in H. destruct lm.
    rewrite set_mem_map_annotateIt in H. destruct m. destruct c. simpl in H. destruct n.
    simpl.

  - destruct lm. destruct n. simpl. intros. rewrite set_mem_map_annotateIt in H.

