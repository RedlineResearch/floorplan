Require Import Coq.Strings.String.
Require Import Common.
Require Import ListSet.

Inductive value (annot : Type) : Type :=
  | B0 : annot -> value annot (* zero bytes *)
  | B1 : annot -> value annot (* one byte *)
  | T  : annot -> value annot -> value annot -> value annot (* tree of bytes *)
  | N  : annot -> string -> value annot -> value annot. (* named value *)

Inductive expr : Type :=
  | Prim    : nat -> expr
  | Con   : nat -> expr -> expr
  | Align   : expr -> nat -> expr
  | Seq     : expr -> expr -> expr
  | Alt     : expr -> expr -> expr
  | Named   : string -> expr -> expr
  | Exists  : string -> expr -> expr
  | LM      : string -> expr -> expr.

Theorem expr_eq_dec: forall a b : expr,
  {a = b} + {a <> b}.
Proof. intros. repeat (decide equality). Defined.

Definition beq_expr (e1 e2 : expr) : bool :=
  if expr_eq_dec e1 e2 then true else false.

(* An env is an environment, mapping identifiers (strings) to natural numbers: *)
Inductive env : Type :=
  | Env : (string * nat) -> env -> env (* θ [ x → i ] *)
  | E0  : env.

Fixpoint env_lookup (θ : env) (x : string) : option nat :=
  match θ with
  | E0 => None
  | Env (x', n) θ' => if eq_string x x' then Some n else env_lookup θ' x
  end.

Compute N _ tt "Karl" (B1 _ tt).

Compute B1 = B1.

Fixpoint eq_value (annot : Type) (v1 : value annot) (v2 : value annot) : bool :=
  match v1, v2 with
  | B1 _ _, B1 _ _ => true
  | B0 _ _, B0 _ _ => true
  | T  _ _ v1' v1'', T _ _ v2' v2'' => (eq_value _ v1' v2') && (eq_value _ v1'' v2'')
  | N  _ _ x1 v1', N _ _ x2 v2' => (eq_string x1 x2) && (eq_value _ v1' v2')
  | _, _ => false
  end.

Theorem eq_value_correct: forall (A : Type) (a b : value A),
  eq_value A a b = eq_value A b a.
Proof. intros A a. induction a.
  - simpl. destruct b ; reflexivity.
  - simpl. destruct b ; reflexivity.
  - simpl. destruct b.
    + simpl. reflexivity.
    + simpl. reflexivity.
    + simpl. rewrite IHa1. rewrite IHa2. reflexivity.
    + simpl. reflexivity.
  - simpl. destruct b.
    + reflexivity.
    + reflexivity.
    + simpl. reflexivity.
    + simpl. rewrite IHa. rewrite eq_string_correct. reflexivity. Qed.

Theorem eq_value_v_v: forall (A : Type) (v : value A),
  eq_value A v v = true.
Proof. induction v.
  - reflexivity.
  - reflexivity.
  - simpl. rewrite IHv1. rewrite IHv2. reflexivity.
  - simpl. rewrite IHv. rewrite eq_string_s_s. reflexivity. Qed.

Definition B1tt := B1 unit tt.
Definition B0tt := B0 unit tt.

Definition S0v (A : Type) := S0 (value A).
Definition Stv (A : Type) := St (value A).
Definition setv (A : Type) := set (value A).
Definition set_addv (A : Type) := set_add (value A) (eq_value A).
Definition set_memv (A : Type) := set_mem (value A) (eq_value A).
Definition set_fold_leftv (A : Type) := set_fold_left (value A).
Definition singletonv (A : Type) := set_addv A (S0v A).
Definition unionAllv (A : Type) := unionAll (value A) (eq_value A).
Definition just_B0 := Stv _ B0tt (S0v _).
Definition set_initv (A : Type) := set_init (value A) (eq_value A).
Definition union2v (A : Type) := union2 (value A) (eq_value A).
Definition set_map_vv (A : Type) := set_map (value A) (value A).

Example set_add_ex: set_add (value unit) (eq_value unit) (St _ (B1 _ tt) (S0 _)) (T _ tt (B1 _ tt) (B1 _ tt))
  = St _ (B1 _ tt) (St _ (T _ tt (B1 _ tt) (B1 _ tt)) (S0 _)).
Proof. simpl. reflexivity. Qed.

Example set_fold_left_ex: set_fold_left (value _) (set (value _))
  (set_addv _) (St _ B1tt (St _ B0tt (S0 _))) (S0 _) = (St _ B1tt (St _ B0tt (S0 _))).
Proof. reflexivity. Qed.

Compute (union2 (value _) (eq_value _)) (St _ B1tt (St _ B0tt (S0 _))) (St _ B1tt (St _ B0tt (S0 _))).

Lemma set_mem_map_preserve_v: forall (A : Type) (xs : set (value A))
  (x2 : value A) (fncn : value A -> value A) (prop : value A -> Prop),
    (forall (x1 : value A), set_memv A xs x1 = true -> prop x1)          (* induction hypothesis *)
        -> (forall (i : value A), prop (fncn i) = prop i)                (* fncn is prop-preserving *)
        -> (forall (i j : value A), eq_value A i j = true  -> prop i = prop j) (* eq_value is prop-preserving *)
          -> (set_memv A (set_map _ _ fncn xs) x2 = true -> prop x2).    (* then map is leaves preserving *)
Proof. intros. apply set_mem_map_preserve with (eq_X := eq_value A) (xs := xs) (fncn := fncn).
  - intros. unfold set_memv in H. apply H. apply H3.
  - intros. apply eq_value_v_v.
  - intros. apply H1. apply H3.
  - apply H0.
  - unfold set_memv in H2. apply H2. Qed.

Fixpoint leaves (A : Type) (v : value A) : nat :=
  match v with
  | B0 _ _ => O
  | B1 _ _ => S O
  | N  _ _ _ v' => leaves _ v'
  | T  _ _ v1 v2 => leaves _ v1 + leaves _ v2
  end.

Lemma eq_value_leaves: forall (A : Type) (i j : value A),
  eq_value A i j = true -> leaves A i = leaves A j.
Proof. intros. generalize dependent j. induction i.
  - intros. simpl. destruct j.
    * reflexivity.
    * inversion H.
    * inversion H.
    * inversion H.
  - intros. simpl. destruct j.
    * inversion H.
    * reflexivity.
    * inversion H.
    * inversion H.
  - intros. simpl. induction j.
    * inversion H.
    * inversion H.
    * simpl. rewrite IHi1 with (j := j1). rewrite IHi2 with (j := j2).
      + reflexivity.
      + simpl in H. destruct (eq_value A i2 j2). reflexivity. destruct (eq_value A i1 j1).
        inversion H. inversion H.
      + simpl in H. destruct (eq_value A i1 j1). reflexivity. destruct (eq_value A i2 j2).
        inversion H. inversion H.
    * inversion H.
  - intros. simpl. induction j.
    * inversion H.
    * inversion H.
    * inversion H.
    * simpl. rewrite IHi with (j := j). reflexivity. simpl in H. destruct (eq_value A i j).
      reflexivity. destruct (eq_string s s0). inversion H. inversion H. Qed.

Lemma eq_value_leaves': forall (A : Type) (i j : value A) (m : nat),
  eq_value A i j = true -> (m = leaves A i) = (m = leaves A j).
Proof. intros. rewrite eq_value_leaves with (j := j). reflexivity. apply H. Qed.

Lemma set_mem_map_leaves: forall (A : Type) (xs : set (value A)) (x : value A) (m : nat)
  (fncn : value A -> value A),
    (forall (x1 : value A), set_memv A xs x1 = true -> m = leaves A x1)             (* induction hypothesis *)
      -> (forall (i : value A), leaves A (fncn i) = leaves A i)                     (* fncn is leaves-preserving *)
      (* -> (forall (i j : value A), eq_value A i j = true -> leaves A i = leaves A j) (* eq_value is prop-preserving *) *)
        -> (set_memv A (set_map _ _ fncn xs) x = true -> m = leaves A x).                 (* then map is leaves preserving *)
Proof.
  intros. apply set_mem_map_preserve_v with (A := A) (xs := xs) (x2 := x) (fncn := fncn).
  - apply H.
  - intros. rewrite -> H0. reflexivity.
  - intros. rewrite eq_value_leaves' with (j := j). reflexivity. apply H2.
  - apply H1. Qed.
