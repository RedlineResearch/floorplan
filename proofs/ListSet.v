Require Import Common.
Require Import Coq.Bool.Bool.
Require Import Coq.Arith.EqNat.

Inductive set (A : Type) : Type :=
  | S0 : set A
  | St : A -> set A -> set A.

(* Is the value v syntactically equivalent to something in the set: *)
Fixpoint set_mem (A : Type) (eq_A : A -> A -> bool) (vs : set A) (v : A) : bool :=
  match vs with
  | S0 _ => false
  | St _ v2 vs' => if eq_A v v2 then true else (set_mem A eq_A) vs' v
  end.

Fixpoint set_add (A : Type) (eq_A : A -> A -> bool) (vs : set A) (v : A) : set A :=
  match vs with
  | S0 _        => St _ v (S0 _)
  | St _ v2 vs' => if eq_A v v2 then St _ v vs' else St _ v2 ((set_add A eq_A) vs' v)
  end.

(* Adding a value and then checking for its membership always returns true: *)
Theorem set_add_correct: forall (A : Type) (eq_A : A -> A -> bool) (vs : set A) (v : A),
  eq_A v v = true -> (set_mem A eq_A) ((set_add A eq_A) vs v) v = true.
Proof. intros A eq_A vs. induction vs.
  - intros v H. simpl. rewrite H. reflexivity.
  - intros v0. simpl. intros H. destruct (eq_A v0 a).
    * simpl. rewrite H. reflexivity.
    * simpl. rewrite IHvs. destruct (eq_A v0 a) ; reflexivity. apply H. Qed.

Fixpoint set_rem (A : Type) (eq_A : A -> A -> bool) (vs : set A) (v : A) : set A :=
  match vs with
  | S0 _        => S0 _
  | St _ v2 vs' => if eq_A v v2 then (set_rem A eq_A) vs' v else St _ v2 ((set_rem A eq_A) vs' v)
  end.

Theorem set_rem_correct: forall (A : Type) (eq_A : A -> A -> bool) (vs : set A) (v : A),
  eq_A v v = true -> (set_mem A eq_A) ((set_rem A eq_A) vs v) v = false.
Proof. intros A eq_A vs. induction vs.
  - intros v H. reflexivity.
  - intros v H. simpl. rewrite if_f_app. rewrite if_app. simpl.
    rewrite IHvs. destruct (eq_A v a) ; reflexivity. apply H. Qed.

(* set_fold_left {A B:Type} : (B -> A -> B) -> set A -> B -> B *)
Fixpoint set_fold_left (A : Type) (B : Type) (fncn : B -> A -> B) (s : set A) (init : B) : B :=
  match s with
  | S0 _ => init
  | St _ v s' => set_fold_left A B fncn s' (fncn init v)
  end.

Compute (set_add nat beq_nat).

Compute (set_fold_left _ _ (set_add _ beq_nat) (St _ 3 (St _ 4 (S0 _))) (St _ 42 (St _ 43 (S0 _)))).

Theorem set_fold_left_correct: forall (A : Type) (eq_A : A -> A -> bool) (s : set A),
  (forall v, eq_A v v = true) -> set_fold_left A (set A) (set_add A eq_A) (S0 _) s = s.
Proof. intros. destruct s ; auto. Qed.

Fixpoint union2 (A : Type) (eq_A : A -> A -> bool) (a b : set A) : set A :=
  match a with
  | S0 _ => b
  | St _ v s' => (union2 A eq_A) s' ((set_add A eq_A) b v)
  end.

Fixpoint set_init (A : Type) (eq_A : A -> A -> bool) (fncn : nat -> A) (n : nat) : set A :=
  match n with
  | O     => St _ (fncn O) (S0 _)
  | S n'  => set_add _ eq_A (set_init _ eq_A fncn n') (fncn n)
  end.

Fixpoint set_map (A : Type) (B : Type) (fncn : A -> B) (input : set A) : set B :=
  match input with
  | S0 _      => S0 _
  | St _ x xs => St _ (fncn x) (set_map _ _ fncn xs)
  end.

Definition singleton (V : Type) (v : V) : set V := St _ v (S0 _).

Fixpoint filter (V : Type) (pred : V -> bool) (vs : set V) : set V :=
  match vs with
  | S0 _ => S0 _
  | St _ v vs' => if pred v then St _ v (filter _ pred vs') else filter _ pred vs'
  end.

(* Union from [0,m] of the given function: *)
Fixpoint unionAll (A : Type) (eq_A : A -> A -> bool) (m : nat) (fncn : nat -> set A) : set A :=
  match m with
  | O     => fncn O
  | S m'  => union2 _ eq_A (fncn m) (unionAll _ eq_A m' fncn)
  end.

Fixpoint all_satisfy (X : Type) (pred : X -> bool) (xs : set X) : bool :=
  set_fold_left X bool (fun b x => b && pred x) xs true.

Compute all_satisfy nat (beq_nat 3) (St _ 3 (St _ 3 (St _ 4 (S0 _)))).

Lemma set_mem_map_preserve: forall (X : Type) (eq_X : X -> X -> bool) (xs : set X)
  (x2 : X) (fncn : X -> X) (prop : X -> Prop),
    (forall (x1 : X), set_mem X eq_X xs x1 = true -> prop x1)             (* induction hypothesis *)    (* YES *)
      -> (forall (i : X),   eq_X i i = true)                              (* eq_X is reflexive *)       (* YES *)
      -> (forall (i j : X), eq_X i j = true  -> prop i = prop j)          (* eq_X is prop-preserving *) (* YES *)
        -> (forall (i : X), prop (fncn i) = prop i)                       (* fncn is prop-preserving *) (* YES *)
          -> (set_mem X eq_X (set_map _ _ fncn xs) x2 = true -> prop x2). (* then map is leaves preserving *) (* OBV *)
Proof. induction xs.
  - simpl. intros. inversion H3.
  - simpl. intro x2. intro fncn. intro prop. intro IHx1. intro eq_refl.
    (* intro eq_symm. *) intro eq_prop_pres. (* intro eq_prop_pres_neg. *) intro fncn_prop_pres.
    (* intro eq_X_fncn_pres. *) intro set_mem_prop. destruct (eq_X x2 (fncn a)) eqn:X2_is_fna.
    (* x2 = fncn(a) *)
    + rewrite eq_prop_pres with (j := fncn a). rewrite -> fncn_prop_pres. apply IHx1. rewrite eq_refl. reflexivity.
      apply X2_is_fna.
    (* x2 /= fncn(a) *)
    + apply IHxs with (fncn := fncn).
      * intro x1. intro set_mem_x1. apply IHx1. rewrite -> set_mem_x1. destruct (eq_X x1 a) ; reflexivity.
      * apply eq_refl.
      * apply eq_prop_pres.
      * apply fncn_prop_pres.
      * apply set_mem_prop. Qed.
