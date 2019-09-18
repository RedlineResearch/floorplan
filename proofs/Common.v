Require Import Coq.Strings.String.
From Coq Require Import Bool Ascii String.
Require Import Coq.Bool.Bool.

Theorem if_f_app: forall (X : Type) (Y : Type) (f : X -> Y) (cond : bool) (a : X) (b : X),
  f (if cond then a else b) = if cond then f a else f b.
Proof. intros X Y f cond a b. destruct cond. reflexivity. reflexivity. Qed.

Theorem if_app: forall (A : Type) (B : Type) (cond : bool) (a b : A -> B) (c : A),
  (if cond then a else b) c = if cond then a c else b c.
Proof. intros A B cond a b c. destruct cond ; reflexivity. Qed.

Definition eq_ascii (a1 a2 : ascii) :=
  match a1, a2 with
  | Ascii b1 b2 b3 b4 b5 b6 b7 b8, Ascii c1 c2 c3 c4 c5 c6 c7 c8 =>
    (eqb b1 c1) && (eqb b2 c2) && (eqb b3 c3) && (eqb b4 c4) &&
    (eqb b5 c5) && (eqb b6 c6) && (eqb b7 c7) && (eqb b8 c8)
  end.

Theorem eq_ascii_correct: forall (a b : ascii),
  eq_ascii a b = eq_ascii b a.
Proof. intros a. destruct a. destruct b.
  - destruct b0, b1, b2, b3, b4, b5, b6 ; reflexivity.
  - destruct b0, b1, b2, b3, b4, b5, b6 ; reflexivity. Qed.

Theorem eq_ascii_a_a: forall (a : ascii),
  eq_ascii a a = true.
Proof. destruct a.
  - destruct b, b0, b1, b2, b3, b4, b5, b6 ; reflexivity. Qed.

Fixpoint eq_string (s1 s2 : string) :=
  match s1, s2 with
  | EmptyString,  EmptyString  => true
  | String x1 s1, String x2 s2 => eq_ascii x1 x2 && eq_string s1 s2
  | _, _                       => false
  end.

Theorem eq_string_correct: forall (a b : string),
  eq_string a b = eq_string b a.
Proof. intros a. induction a.
  - destruct b ; reflexivity.
  - induction b.
    + simpl. reflexivity.
    + simpl. rewrite IHa. rewrite eq_ascii_correct. reflexivity. Qed.

Theorem eq_string_s_s: forall (s : string),
  eq_string s s = true.
Proof. induction s.
  - reflexivity.
  - simpl. rewrite IHs. rewrite eq_ascii_a_a. reflexivity. Qed.

Fixpoint ble_nat n m  :=
  match n, m  with
    | O, _ => true
    | S n', O => false
    | S n', S m' => ble_nat n' m'
  end.

Lemma eqb_eq': forall (a b : bool),
  eqb a b = true -> a = b.
Proof. apply eqb_true_iff. Qed.

Hint Resolve eqb_eq'.

(*
Hint Resolve -> eqb_true_iff.
Check eqb_true_iff. *)

(* == firstorder *)
Ltac break_conjs := repeat match goal with
                             | H : _ /\ _ |- _ => inversion H; subst; clear H
                             end.

Check eqb_true_iff.
Lemma eq_ascii_refl: forall (z y x : ascii),
  eq_ascii x y = true -> eq_ascii x z = eq_ascii y z.
Proof with auto. induction z.
  - destruct y.
    * destruct x. simpl. intros. repeat rewrite andb_true_iff in H.
      break_conjs.
      replace b7 with b15 ... (* ... == "; auto" *)
      replace b8 with b16 ...
      replace b9 with b17 ...
      replace b10 with b18 ...
      replace b11 with b19 ...
      replace b12 with b20 ...
      replace b13 with b21 ...
      replace b14 with b22 ... Qed.

Lemma eq_ascii_refl_false: forall (a a0 a1 : ascii),
  eq_ascii a a0 = false
    -> eq_ascii a a1 = true
      -> eq_ascii a0 a1 = false.
Proof. intros. rewrite eq_ascii_refl with (y := a1) in H. rewrite eq_ascii_correct.
  apply H. apply H0. Qed.

Theorem eq_string_refl: forall (c b a : string),
  eq_string a b = true -> eq_string a c = eq_string b c.
Proof. induction c.
  - destruct b.
    * destruct a. reflexivity. intros. inversion H.
    * intros. destruct a0. inversion H. reflexivity.
  - destruct b.
    * intros. destruct a0. reflexivity. inversion H.
    * intros. destruct a1. inversion H. simpl. simpl in H. rewrite andb_true_iff in H.
      inversion H. rewrite IHc with (a := a2) (b := b). 2: { apply H1. }
      rewrite eq_ascii_refl with (x := a1) (y := a0). 2: { apply H0. }
      reflexivity. Qed.

Lemma eq_string_refl_false: forall (s s0 s1 : string),
  eq_string s s0 = false
    -> eq_string s s1 = true
      -> eq_string s0 s1 = false.
Proof. induction s.
  - destruct s0.
    * intros. destruct s1. inversion H. reflexivity.
    * intros. destruct s1. reflexivity. inversion H0.
  - destruct s0.
    * intros. destruct s1. inversion H0. reflexivity.
    * intros. destruct s1.
      + reflexivity.
      + simpl. simpl in H. simpl in H0. rewrite andb_true_iff in H0. inversion H0.
        rewrite eq_string_correct. rewrite eq_string_refl with (a := s1) (c := s0) (b := s).
        2: { rewrite eq_string_correct. apply H2. }
        destruct (eq_string s s0). 2: { rewrite andb_comm. reflexivity. }
        rewrite andb_comm. simpl. rewrite andb_comm in H. simpl in H.
        rewrite eq_ascii_refl_false with (a := a). reflexivity. apply H. apply H1. Qed.
