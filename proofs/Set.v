Require Import Common.

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

Fixpoint set_fold_left (A : Type) (B : Type) (fncn : B -> A -> B) (s : set A) (init : B) : B :=
  match s with
  | S0 _ => init
  | St _ v s' => set_fold_left A B fncn s' (fncn init v)
  end.

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
