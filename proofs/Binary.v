Require Import Coq.Arith.Plus.
Require Import Omega.

Inductive bin : Type :=
  | Z  : bin
  | D0 : bin -> bin
  | D1 : bin -> bin.

Fixpoint incr (n : bin) : bin :=
  match n with
    | Z => D1 Z
    | D0 m => D1 m
    | D1 m => D0 (incr m)
  end.

(* Drop the highest order bit *)
Fixpoint drop_hi (n : bin) :=
  match n with
  | Z => Z
  | D0 Z => Z
  | D1 Z => Z
  | D0 n' => D0 (drop_hi n')
  | D1 n' => D1 (drop_hi n')
  end.

(* Add a zero to the highest order bit *)
Fixpoint add_hi (n : bin) :=
  match n with
  | Z => D0 Z
  | D0 n' => D0 (add_hi n')
  | D1 n' => D1 (add_hi n')
  end.

(* Shift n to the left by v bits *)
Fixpoint lshift (n : bin) (v : nat) :=
  match v with
  | O => n
  | S v' => lshift (drop_hi (D0 n)) v'
  end.

(* Shift n to the right by v bits *)
Fixpoint rshift (n : bin) (v : nat) :=
  match v with
  | O => n
  | S v' =>
    match n with
    | Z     => rshift Z v'
    | D0 n' => rshift (add_hi n') v'
    | D1 n' => rshift (add_hi n') v'
    end
  end.

Compute rshift (D0 (D0 (D1 (D1 Z)))) 4.
Compute lshift (D1 (D0 (D1 (D1 Z)))) 2.

Notation "x << y" := (lshift x y)
  (at level 50, left associativity)
  : nat_scope.
Notation "x >> y" := (rshift x y)
  (at level 50, left associativity)
  : nat_scope.

Fixpoint is_zero (b : bin) :=
  match b with
  | Z => true
  | D1 _ => false
  | D0 b' => is_zero b'
  end.

Theorem is_zero_add: forall (n : bin),
  is_zero n = true -> is_zero (add_hi n) = true.
Proof. intros n. induction n.
  - reflexivity.
  - simpl. apply IHn.
  - intros H. inversion H. Qed.

Theorem rshift_id: forall (n : bin) (x : nat),
  is_zero n = true -> n >> x = n.
Proof. intros n x. generalize dependent n. induction x.
  - reflexivity.
  - intros n H. simpl. rewrite IHx.
    + induction n.
      * reflexivity.
      * destruct n.
        { simpl. apply IHx. reflexivity. }
        { simpl. rewrite <- IHn. rewrite IHx. apply f_equal. rewrite IHx. reflexivity.
          apply is_zero_add. apply H. apply is_zero_add. apply H. apply H. }
        { inversion H. }
      * destruct n.
        { inversion H. }
        { inversion H. }
        { inversion H. }
    + induction n.
      * reflexivity.
      * destruct n.
        { simpl. reflexivity. }
        { simpl. rewrite <- IHn. reflexivity. apply H. }
        { inversion H. }
      * destruct n.
        { inversion H. }
        { inversion H. }
        { inversion H. } Qed.

Compute (Z >> 4).

Fixpoint bits (n : bin) :=
  match n with
  | Z => O
  | D0 n' => S (bits n')
  | D1 n' => S (bits n')
  end.

Compute bits (D0 (D1 (D0 Z))) = 3.

Compute bits ((D0 (D0 (D1 (D0 Z)))) << 8).

Theorem D0_drop: forall (n : bin),
  ~(n = Z) -> D0 (drop_hi n) = drop_hi (D0 n).
Proof. intros n H. unfold not in H. induction n.
  - congruence.
  - simpl. reflexivity.
  - simpl. reflexivity. Qed.

Theorem D1_drop: forall (n : bin),
  ~(n = Z) -> D1 (drop_hi n) = drop_hi (D1 n).
Proof. intros n H. unfold not in H. induction n.
  - congruence.
  - simpl. reflexivity.
  - simpl. reflexivity. Qed.

Theorem drop_bits: forall (n : bin),
  bits (drop_hi n) = (bits n) - 1.
Proof. intros n. induction n.
  - reflexivity.
  - destruct n.
    + reflexivity.
    + simpl. apply f_equal. simpl in IHn. rewrite -> IHn. induction (bits n).
      * reflexivity.
      * simpl. reflexivity.
    + simpl. apply f_equal. simpl in IHn. rewrite -> IHn. induction (bits n).
      * reflexivity.
      * simpl. reflexivity.
  - destruct n.
    + reflexivity.
    + simpl. apply f_equal. simpl in IHn. rewrite -> IHn. induction (bits n).
      * reflexivity.
      * simpl. reflexivity.
    + simpl. apply f_equal. simpl in IHn. rewrite -> IHn. induction (bits n).
      * reflexivity.
      * simpl. reflexivity. Qed.

Lemma one_more_bit_D0: forall (n : bin) (v : nat),
  bits (D0 n << v) = S (bits (n << v)).
Proof. intros n v. generalize dependent n. induction v.
  - simpl. reflexivity.
  - induction n.
    * simpl. apply IHv.
    * simpl. apply IHv.
    * simpl. apply IHv. Qed.

Lemma one_more_bit_D1': forall (n : bin) (v : nat),
  bits (D1 n << v) = bits (D0 n << v).
Proof. intros n v. generalize dependent n. induction v.
  - simpl. reflexivity.
  - destruct n.
    * simpl. reflexivity.
    * simpl. rewrite -> one_more_bit_D0. rewrite -> one_more_bit_D0. apply f_equal.
      apply IHv.
    * simpl. rewrite -> one_more_bit_D0. rewrite -> one_more_bit_D0. apply f_equal.
      apply IHv. Qed.

Lemma one_more_bit_D1: forall (n : bin) (v : nat),
  bits (D1 n << v) = S (bits (n << v)).
Proof. intros n v. generalize dependent n. induction v.
  - simpl. reflexivity.
  - intros n. rewrite -> one_more_bit_D1'. apply one_more_bit_D0. Qed.

Theorem conserve_bits_lshift: forall (n : bin) (v : nat),
  bits (n << v) = bits n.
Proof. intros n v. induction n.
  - simpl. induction v.
    * reflexivity.
    * simpl. apply IHv.
  - simpl. replace (bits (D0 n << v)) with (S (bits (n << v))).
    * apply f_equal. apply IHn.
    * symmetry. apply one_more_bit_D0.
  - simpl. replace (bits (D1 n << v)) with (S (bits (n << v))).
    * apply f_equal. apply IHn.
    * symmetry. apply one_more_bit_D1. Qed.

Lemma rshift_one_more_bit: forall (n : bin) (v : nat),
  bits (D1 n >> v) = bits (D0 n >> v).
Proof. intros n v. induction v.
  - simpl. reflexivity.
  - simpl. reflexivity. Qed.

Lemma rshift_one_more_D0: forall (n : bin) (v : nat),
  bits (D0 n >> v) = S (bits (n >> v)).
Proof. intros n v. generalize dependent n. induction v.
  - simpl. reflexivity.
  - intros n. induction n.
    * apply IHv.
    * apply IHv.
    * simpl. destruct (add_hi n).
      + simpl. rewrite rshift_one_more_bit. apply IHv.
      + simpl. rewrite rshift_one_more_bit. apply IHv.
      + simpl. rewrite rshift_one_more_bit. apply IHv. Qed.

Lemma rshift_one_more_D1: forall (n : bin) (v : nat),
  bits (D1 n >> v) = S (bits (n >> v)).
Proof. intros n v. rewrite -> rshift_one_more_bit. apply rshift_one_more_D0. Qed.

Theorem conserve_bits_rshift: forall (n : bin) (v : nat),
  bits (n >> v) = bits n.
Proof. intros n v. induction n.
  - simpl. induction v.
    * reflexivity.
    * simpl. apply IHv.
  - simpl. rewrite -> rshift_one_more_D0. apply f_equal. apply IHn.
  - simpl. rewrite -> rshift_one_more_D1. apply f_equal. apply IHn. Qed.

Fixpoint bit_and (n1 : bin) (n2 : bin) :=
  match n1, n2 with
  | D0 n1', D0 n2' => D0 (bit_and n1' n2')
  | D0 n1', D1 n2' => D0 (bit_and n1' n2')
  | D1 n1', D0 n2' => D0 (bit_and n1' n2')
  | D1 n1', D1 n2' => D1 (bit_and n1' n2')
  | _, _           => Z
  end.

Compute bit_and (D0 (D0 (D1 (D0 Z)))) (D1 (D0 (D1 (D0 Z)))).

Notation "x & y" := (bit_and x y)
  (at level 40, left associativity)
  : nat_scope.

Fixpoint bit_or (n1 : bin) (n2 : bin) :=
  match n1, n2 with
  | D0 n1', D0 n2' => D0 (bit_or n1' n2')
  | D0 n1', D1 n2' => D1 (bit_or n1' n2')
  | D1 n1', D0 n2' => D1 (bit_or n1' n2')
  | D1 n1', D1 n2' => D1 (bit_or n1' n2')
  | Z, n2          => n2
  | n1, Z          => n1
  end.

Compute bit_or (D0 (D0 (D1 (D1 Z)))) (D1 (D0 (D1 (D0 Z)))).

Notation "x \ y" := (bit_or x y)
  (at level 40, left associativity)
  : nat_scope.

Theorem conserve_bits_and: forall (n1 n2 : bin),
  bits n1 = bits n2 -> bits (n1 & n2) = bits n1.
Proof. intros n1. induction n1.
  - simpl. reflexivity.
  - intros n2 H. simpl in H. simpl. rewrite H. destruct n2.
    + reflexivity.
    + simpl. rewrite -> IHn1.
      * rewrite H. simpl. reflexivity.
      * simpl in H. congruence.
    + simpl. apply f_equal. simpl in H. assert (forall (x y : nat), S x = S y -> x = y).
      * intros x y Hxy. inversion Hxy. reflexivity.
      * apply H0 in H. rewrite <- H. apply IHn1. apply H.
  - intros n2 H. simpl in H. simpl. rewrite H. destruct n2.
    + reflexivity.
    + simpl. rewrite -> IHn1.
      * rewrite H. simpl. reflexivity.
      * simpl in H. congruence.
    + simpl. apply f_equal. simpl in H. assert (forall (x y : nat), S x = S y -> x = y).
      * intros x y Hxy. inversion Hxy. reflexivity.
      * apply H0 in H. rewrite <- H. apply IHn1. apply H. Qed.

Theorem conserve_bits_or: forall (n1 n2 : bin),
  bits n1 = bits n2 -> bits (n1 \ n2) = bits n1.
Proof. intros n1 n2. generalize dependent n2. induction n1.
  - simpl. intros n2 H. symmetry. apply H.
  - simpl. intros n2 H. destruct n2.
    + simpl. reflexivity.
    + simpl. apply f_equal. apply IHn1. inversion H. reflexivity.
    + simpl. apply f_equal. apply IHn1. inversion H. reflexivity.
  - simpl. intros n2 H. destruct n2.
    + simpl. reflexivity.
    + simpl. apply f_equal. apply IHn1. inversion H. reflexivity.
    + simpl. apply f_equal. apply IHn1. inversion H. reflexivity. Qed.

Theorem conserve_bits_or': forall (n1 n2: bin),
  bits n1 = bits n2 -> bits (n1 \ n2) = bits n2.
Proof. intros n1 n2 H. rewrite conserve_bits_or.
  - apply H.
  - apply H. Qed.

Fixpoint bit_neg (n : bin) :=
  match n with
  | D0 n' => D1 (bit_neg n')
  | D1 n' => D0 (bit_neg n')
  | Z => Z
  end.

Theorem conserve_bits_neg: forall (n : bin),
  bits (bit_neg n) = bits n.
Proof.
  intros n. induction n.
  - reflexivity.
  - simpl. rewrite IHn. reflexivity.
  - simpl. rewrite IHn. reflexivity. Qed.

Notation "~~ x" := (bit_neg x)
  (at level 30, right associativity)
  : nat_scope.

Fixpoint pad (n : nat) (b : bin) :=
  match n with
  | O => Z
  | S n' =>
    match b with
    | Z => D0 (pad n' Z)
    | D0 b' => D0 (pad n' b')
    | D1 b' => D1 (pad n' b')
    end
  end.

Fixpoint lt (n1 n2 : nat) :=
  match n1, n2 with
  | _, O => false
  | O, _ => true
  | S n1', S n2' => lt n1' n2'
  end.

Theorem lt_correct: forall (n: nat),
  lt n (S n) = true.
Proof. intros n. induction n.
  - reflexivity.
  - simpl. apply IHn. Qed.

Theorem bits_pad: forall (n : nat) (b : bin),
  bits (pad n b) = n.
Proof. intros n b. generalize dependent n. induction b.
  - intros n. destruct n.
    * reflexivity.
    * simpl. induction n.
      + simpl. reflexivity.
      + simpl. apply f_equal. apply IHn.
  - intros n. destruct n.
    * reflexivity.
    * simpl. induction n.
      + destruct b.
        { simpl. reflexivity. }
        { reflexivity. }
        { simpl. reflexivity. }
      + apply f_equal. apply IHb.
  - intros n. destruct n.
    * reflexivity.
    * simpl. induction n.
      + destruct b.
        { simpl. reflexivity. }
        { reflexivity. }
        { simpl. reflexivity. }
      + apply f_equal. apply IHb. Qed.

(* ((1 << β) - 1) *)
Fixpoint mask (n β : nat) :=
  match n with
  | O => Z
  | S n' =>
    match β with
    | O => D0 (mask n' O)
    | S β' => D1 (mask n' β')
    end
  end.

Theorem conserve_bits_mask: forall (n b : nat),
  bits (mask n b) = n.
Proof. intros n. induction n.
  - simpl. reflexivity.
  - simpl. intros b. destruct b.
    * simpl. rewrite IHn. reflexivity.
    * simpl. rewrite IHn. reflexivity. Qed.

Fixpoint oneb (n : nat) := match n with
  | O => Z
  | _ => pad n (D1 Z)
  end.

Fixpoint bin_to_nat (n : bin) : nat :=
  match n with
    | Z => 0
    | D0 m => 2 * (bin_to_nat m)
    | D1 m => 2 * (bin_to_nat m) + 1
  end.

Fixpoint nat_to_bin (n : nat) : bin :=
  match n with
    | O => Z
    | S m => incr (nat_to_bin m)
  end.

Fixpoint pow (a b : nat) :=
  match b with
  | O => S O
  | S n' => a * (pow a n')
  end.

Compute bin_to_nat (mask 8 8).
Compute pow 0 0.
Compute bin_to_nat (mask 8 4).

Fixpoint lte (n1 n2 : nat) :=
  match n1, n2 with
  | O, _ => true
  | S n1', O => false
  | S n1', S n2' => lte n1' n2'
  end.

Theorem lte_correct: forall (n: nat),
  lte n (S n) = true.
Proof. intros n. induction n.
  - reflexivity.
  - simpl. apply IHn. Qed.

Theorem lte_correct': forall (n: nat),
  lte n n = true.
Proof. induction n.
  - reflexivity.
  - simpl. apply IHn. Qed.

Compute 2^2 - 1 + 1.
Search plus_assoc.

Fixpoint gt (a b : nat) := negb (lte a b).

Lemma gt_0_minus: forall (a b : nat),
  gt a 0 = true -> a - 1 + b = a + b - 1.
Proof. intros a. induction a.
  - intros b H. inversion H.
  - intros b H. simpl. rewrite <- minus_n_O. rewrite <- minus_n_O. reflexivity. Qed.

Lemma minus_1_plus_1: forall (a : nat),
  gt a 0 = true -> a - 1 + 1 = a.
Proof. intros a H. induction a.
  - inversion H.
  - simpl. rewrite <- minus_n_O. rewrite <- plus_n_Sm. rewrite <- plus_n_O. reflexivity. Qed.

Lemma gt_0_plus: forall (a b : nat),
  gt a 0 = true -> gt (a + b) 0 = true.
Proof. intros a. induction a.
  - intros b H. inversion H.
  - intros b H. reflexivity. Qed.

Lemma pow_0: forall b,
  gt (pow 2 b) 0 = true.
Proof. intros b. induction b.
  - reflexivity.
  - simpl. rewrite <- plus_n_O. rewrite gt_0_plus. reflexivity. apply IHb. Qed.

Theorem pow_1_1: forall (n : nat), 2^n - 1 + 1 = 2^n.
Proof. intros n. apply minus_1_plus_1. apply pow_0. Qed.

Lemma minus_Sn_m: forall (n m : nat),
  lte m n = true -> S (n - m) = S n - m.
Proof. intros n. induction n.
  - intros m H. induction m.
    + reflexivity.
    + inversion H.
  - intros m H. induction m.
    + simpl. reflexivity.
    + simpl. destruct m.
      { rewrite <- minus_n_O. reflexivity. }
      { rewrite IHn. simpl. reflexivity. simpl in H. simpl. apply H. } Qed.

Lemma ab_minus_1: forall (a b : nat),
  gt b 0 = true -> a + (b - 1) = (a + b) - 1.
Proof. intros a. induction a.
  - intros b H. simpl. reflexivity.
  - intros b H. simpl. rewrite <- minus_n_O. rewrite IHa.
    + rewrite minus_Sn_m. simpl. rewrite <- minus_n_O. reflexivity.
      induction b. inversion H. destruct a. reflexivity. reflexivity.
    + apply H. Qed.

Theorem mask_correct': forall (n b : nat),
  lte b n = true -> bin_to_nat (mask n b) = pow 2 b - 1.
Proof. intros n b. generalize dependent n. induction b.
  - intros n H. simpl. induction n.
    * reflexivity.
    * simpl. rewrite <- plus_n_O. rewrite IHn. reflexivity. reflexivity.
  - intros n H. simpl. rewrite <- plus_n_O. induction n.
    * inversion H.
    * simpl. rewrite <- plus_n_O. inversion H. rewrite IHb.
      + rewrite gt_0_minus. rewrite minus_1_plus_1.
        { rewrite ab_minus_1. reflexivity. destruct b. reflexivity. apply pow_0. }
        { apply gt_0_plus. apply pow_0. }
        { apply pow_0. }
      + apply H1. Qed.

Definition n2b := nat_to_bin.
Definition b2n := bin_to_nat.

(* c = carry bit *)
Fixpoint plus' (a b : bin) (c : nat) :=
  match c with
  | O => 
      match a, b with
      | Z, _ => b
      | _, Z => a
      | D0 a', D0 b' => D0 (plus' a' b' O)
      | D0 a', D1 b' => D1 (plus' a' b' O)
      | D1 a', D0 b' => D1 (plus' a' b' O)
      | D1 a', D1 b' => D0 (plus' a' b' (S O))
      end
  | S _ =>
      match a, b with
      | Z, _ => b
      | _, Z => a
      | D0 a', D0 b' => D1 (plus' a' b' O)
      | D0 a', D1 b' => D0 (plus' a' b' (S O))
      | D1 a', D0 b' => D0 (plus' a' b' (S O))
      | D1 a', D1 b' => D1 (plus' a' b' (S O))
      end
  end.

Definition plus (a b : bin) := plus' a b O.

Example binary_plus: (b2n (plus (pad 8 (n2b 15)) (pad 8 (n2b 15)))) = 30.
Proof. reflexivity. Qed.

(* c = borrow bit *)
Fixpoint minus' (a b : bin) (c : nat) :=
  match c with
  | O =>
      match a, b with
      | Z, _ => Z
      | _, Z => Z
      | D0 a', D0 b' => D0 (minus' a' b' O)
      | D0 a', D1 b' => D1 (minus' a' b' (S O))
      | D1 a', D0 b' => D1 (minus' a' b' O)
      | D1 a', D1 b' => D0 (minus' a' b' O)
      end
  | S _ => 
      match a, b with
      | Z, _ => Z
      | _, Z => Z
      | D0 a', D0 b' => D1 (minus' a' b' (S O))
      | D0 a', D1 b' => D0 (minus' a' b' (S O))
      | D1 a', D0 b' => D0 (minus' a' b' O)
      | D1 a', D1 b' => D1 (minus' a' b' (S O))
      end
  end.

Definition minus (a b : bin) := minus' a b O.

Notation "x -* y" := (minus x y)
  (at level 50, left associativity)
  : nat_scope.

Notation "x +* y" := (plus x y)
  (at level 50, left associativity)
  : nat_scope.

Compute (D1 (D0 (D1 (D0 Z)))) -* (D1 (D1 (D0 (D0 Z)))).
Example cascade:
    (D0 (D0 (D1 (D0 (D1 (D0 (D0 (D0 Z)))))))) -* (D1 (D0 (D1 (D0 (D0 (D0 (D0 (D0 Z))))))))
  = (D1 (D1 (D1 (D1 (D0 (D0 (D0 (D0 Z)))))))). Proof. reflexivity. Qed.

Compute (pad 8 (n2b 19)).
Compute (pad 8 (n2b 3)).

Example minus_ex: b2n ((pad 8 (n2b 19)) -* (pad 8 (n2b 3))) = 16.
Proof. simpl. reflexivity. Qed.

Example minus_ex2: b2n (((pad 8 (n2b 19)) -* (pad 8 (n2b 3))) +* (pad 8 (n2b 3))) = 19.
Proof. simpl. reflexivity. Qed.

Example binary_minus: (b2n (minus (pad 8 (n2b 73)) (pad 8 (n2b 56)))) = 17.
Proof. simpl. reflexivity. Qed.

Lemma conserve_bits_plus': forall (a b : bin) (c : nat),
  bits a = bits b -> bits (plus' a b c) = bits a.
Proof. intros a. induction a.
  - intros b c H. simpl in H. symmetry in H. induction b.
    * induction c. reflexivity. reflexivity.
    * inversion H.
    * inversion H.
  - intros b c H. simpl in H. symmetry in H. destruct c.
    * induction b.
      + reflexivity.
      + simpl. apply f_equal. apply IHa. inversion H. reflexivity.
      + simpl. apply f_equal. apply IHa. inversion H. reflexivity.
    * induction b.
      + reflexivity.
      + simpl. apply f_equal. apply IHa. inversion H. reflexivity.
      + simpl. apply f_equal. apply IHa. inversion H. reflexivity.
  - intros b c H. simpl in H. symmetry in H. destruct c.
    * induction b.
      + reflexivity.
      + simpl. apply f_equal. apply IHa. inversion H. reflexivity.
      + simpl. apply f_equal. apply IHa. inversion H. reflexivity.
    * induction b.
      + reflexivity.
      + simpl. apply f_equal. apply IHa. inversion H. reflexivity.
      + simpl. apply f_equal. apply IHa. inversion H. reflexivity. Qed.

Lemma conserve_bits_minus': forall (a b : bin) (c : nat),
  bits a = bits b -> bits (minus' a b c) = bits a.
Proof. intros a. induction a.
  - intros b c H. simpl in H. destruct c. reflexivity. reflexivity.
  - intros b c H. symmetry in H. simpl in H. destruct c.
    * induction b.
      + simpl in H. apply H.
      + simpl. apply f_equal. apply IHa. inversion H. reflexivity.
      + simpl. apply f_equal. apply IHa. inversion H. reflexivity.
    * induction b.
      + simpl in H. apply H.
      + simpl. apply f_equal. apply IHa. inversion H. reflexivity.
      + simpl. apply f_equal. apply IHa. inversion H. reflexivity.
  - intros b c H. symmetry in H. simpl in H. destruct c.
    * induction b.
      + simpl in H. apply H.
      + simpl. apply f_equal. apply IHa. inversion H. reflexivity.
      + simpl. apply f_equal. apply IHa. inversion H. reflexivity.
    * induction b.
      + simpl in H. apply H.
      + simpl. apply f_equal. apply IHa. inversion H. reflexivity.
      + simpl. apply f_equal. apply IHa. inversion H. reflexivity. Qed.

Theorem conserve_bits_plus: forall (a b : bin) (c : nat),
  bits a = bits b -> bits (plus a b) = bits a.
Proof. intros a b H. apply conserve_bits_plus'. Qed.

Theorem conserve_bits_minus: forall (a b : bin) (c : nat),
  bits a = bits b -> bits (minus a b) = bits a.
Proof. intros a b H. apply conserve_bits_minus'. Qed.

Theorem minus_correct_zero: forall (a : bin),
  is_zero (minus' a a 0) = true.
Proof. intros a. induction a.
  - reflexivity.
  - simpl. apply IHa.
  - simpl. apply IHa. Qed.

Definition zerob (b : nat) := pad b Z.

Theorem minus_id: forall (a : bin),
  minus' a a 0 = zerob (bits a).
Proof.
  induction a.
  - reflexivity.
  - simpl. unfold zerob. simpl. apply f_equal. unfold zerob in IHa. apply IHa.
  - unfold zerob. unfold zerob in IHa. simpl. apply f_equal. apply IHa. Qed.

Fixpoint gte (a b : nat) :=
  match a, b with
  | S a', S b' => gte a' b'
  | S _,  O    => true
  | O,    _    => false
  end.

Theorem gte_correct: forall (a b : nat),
  gte a b = true -> lt a b = false.
Proof. induction a.
  - induction b.
    + reflexivity.
    + intros H. inversion H.
  - induction b.
    + simpl. reflexivity.
    + intros H. simpl. apply IHa. inversion H. reflexivity. Qed.

Theorem bits_oneb: forall (n : nat),
  bits (oneb n) = n.
Proof. intros n. induction n.
  - reflexivity.
  - simpl. apply f_equal. apply bits_pad. Qed.

Theorem bits_zerob: forall (n : nat),
  bits (zerob n) = n.
Proof. intros n. apply bits_pad. Qed.

Theorem zero_and: forall (n : nat) (x : bin),
  n = bits x -> zerob n & x = zerob n.
Proof. intros n x H. generalize dependent x. induction n.
  - reflexivity.
  - intros x H. simpl. destruct x.
    * inversion H.
    * simpl in H. unfold zerob. simpl. apply f_equal. apply IHn. inversion H. reflexivity.
    * simpl in H. unfold zerob. simpl. apply f_equal. apply IHn. inversion H. reflexivity. Qed.

Theorem and_comm: forall (a b : bin),
  bits a = bits b -> a & b = b & a.
Proof. intros a. induction a. induction b.
  - reflexivity.
  - reflexivity.
  - reflexivity.
  - simpl. destruct b.
    * reflexivity.
    * simpl. intros H. apply f_equal. apply IHa. inversion H. reflexivity.
    * simpl. intros H. apply f_equal. apply IHa. inversion H. reflexivity.
  - simpl. destruct b.
    * reflexivity.
    * simpl. intros H. apply f_equal. apply IHa. inversion H. reflexivity.
    * simpl. intros H. apply f_equal. apply IHa. inversion H. reflexivity. Qed.

Theorem zero_and': forall (n : nat) (x : bin),
  n = bits x -> pad n Z & x = pad n Z.
Proof. apply zero_and. Qed.

Theorem and_zero: forall (n : nat) (x : bin),
  n = bits x -> x & zerob n = zerob n.
Proof. intros n x H. rewrite and_comm. apply zero_and. apply H. rewrite bits_zerob. symmetry. apply H. Qed.

Theorem and_zero': forall (n : nat) (x : bin),
  n = bits x -> x & pad n Z = pad n Z.
Proof. apply and_zero. Qed.

Theorem and_assoc: forall (a b c : bin),
  bits a = bits b -> bits b = bits c -> a & b & c = a & (b & c).
Proof. intros a. induction a.
  - reflexivity.
  - simpl. induction b.
    * reflexivity.
    * induction c.
      + reflexivity.
      + intros H1 H2. simpl. apply f_equal. apply IHa. simpl in H1. inversion H1. reflexivity. inversion H2. reflexivity.
      + intros H1 H2. simpl. apply f_equal. apply IHa. simpl in H1. inversion H1. reflexivity. inversion H2. reflexivity.
    * induction c.
      + reflexivity.
      + intros H1 H2. simpl. apply f_equal. apply IHa. simpl in H1. inversion H1. reflexivity. inversion H2. reflexivity.
      + intros H1 H2. simpl. apply f_equal. apply IHa. simpl in H1. inversion H1. reflexivity. inversion H2. reflexivity.
  - simpl. induction b.
    * reflexivity.
    * induction c.
      + reflexivity.
      + intros H1 H2. simpl. apply f_equal. apply IHa. simpl in H1. inversion H1. reflexivity. inversion H2. reflexivity.
      + intros H1 H2. simpl. apply f_equal. apply IHa. simpl in H1. inversion H1. reflexivity. inversion H2. reflexivity.
    * induction c.
      + reflexivity.
      + intros H1 H2. simpl. apply f_equal. apply IHa. simpl in H1. inversion H1. reflexivity. inversion H2. reflexivity.
      + intros H1 H2. simpl. apply f_equal. apply IHa. simpl in H1. inversion H1. reflexivity. inversion H2. reflexivity. Qed.

Theorem not_d0: forall (x : bin),
  ~~ (D0 x) = D1 (~~ x).
Proof. intros x. destruct x.
  - reflexivity.
  - reflexivity.
  - reflexivity. Qed.

Theorem not_d1: forall (x : bin),
  ~~ (D1 x) = D0 (~~ x).
Proof. intros x. destruct x.
  - reflexivity.
  - reflexivity.
  - reflexivity. Qed.

Theorem lshift_zero: forall (b : bin),
  b << 0 = b.
Proof. reflexivity. Qed.

(* Grab the first n bits in a, recursing on the rest: *)
Fixpoint grab (n : nat) (a : bin) (recurse : bin -> bin) :=
  match n, a with
  | S n', D0 a' => D0 (grab n' a' recurse)
  | S n', D1 a' => D1 (grab n' a' recurse)
  | S _, Z      => Z
  | O, _        => recurse a
  end.

(* Ignore the first n bits in a, recursing on the rest: *)
Fixpoint ignore (n : nat) (a : bin) (recurse : bin -> bin) :=
  match n, a with
  | S n', D0 a' => D0 (ignore n' a' recurse)
  | S n', D1 a' => D0 (ignore n' a' recurse)
  | S _, Z      => Z
  | O, _        => recurse a
  end.

(* Absolute value of the subtraction of two numbers: *)
Fixpoint abs_minus (a b : nat) :=
  match a, b with
  | S a', S b' => abs_minus a' b'
  | _, O       => a
  | O, _       => b
  end.

Theorem abs_minus_refl: forall (a b : nat),
  abs_minus a b = abs_minus b a.
Proof. induction a. induction b.
  - reflexivity.
  - reflexivity.
  - induction b. reflexivity. apply IHa. Qed.

Lemma pad_flip: forall (a : bin),
  D0 (pad (bits a) Z) = add_hi (pad (bits a) Z).
Proof. induction a.
  - reflexivity.
  - simpl. apply f_equal. apply IHa.
  - simpl. apply f_equal. apply IHa. Qed.

Lemma add_Z: forall (a : bin),
  is_zero (add_hi (pad (bits a) Z)) = true.
Proof. induction a.
  - reflexivity.
  - simpl. apply IHa.
  - simpl. apply IHa. Qed.

Lemma is_zero_pad_distr: forall (x : bin) (n : nat),
  is_zero x = true -> is_zero (pad n x) = true.
Proof. intros x. induction x.
  - induction n.
    + reflexivity.
    + simpl. apply IHn.
  - induction n.
    + simpl. reflexivity.
    + simpl. apply IHx.
  - intros n H. inversion H. Qed.

Fixpoint drop_low (n : nat) (a : bin) :=
  match n with
  | O => a
  | S n' => match a with
            | Z => Z
            | D0 a' | D1 a' => drop_low n' a'
            end
  end.

Theorem is_zero_a: forall (a : bin),
  is_zero a = true -> a = pad (bits a) Z.
Proof. intros a H. induction a.
  - reflexivity.
  - simpl. apply f_equal. apply IHa. apply H.
  - inversion H. Qed.

Theorem lshift_id: forall (n : bin) (x : nat),
  is_zero n = true -> n << x = n.
Proof. intros n x H. generalize dependent n. induction x.
  - reflexivity.
  - intros n H. simpl. rewrite IHx.
    + induction n.
      * reflexivity.
      * apply f_equal. apply IHn. apply H.
      * inversion H.
    + induction n.
      * reflexivity.
      * simpl. apply IHn. apply H.
      * inversion H. Qed.

Theorem pad_Z_lshift: forall (n x : nat),
  pad n Z << x = pad n Z.
Proof. intros n x. induction n. rewrite lshift_id.
  - reflexivity.
  - reflexivity.
  - simpl. rewrite lshift_id. reflexivity. simpl. apply is_zero_pad_distr. reflexivity. Qed.

Lemma is_zero_match: forall (b b2 : bin),
  is_zero b2 = true -> is_zero
    match b with
    | Z => Z
    | _ => b2
    end = true.
Proof. intros b b2 H. destruct b. reflexivity. apply H. apply H. Qed.

Lemma is_zero_drop: forall (a : bin),
  is_zero a = true -> is_zero (drop_hi a) = true.
Proof. induction a.
  - reflexivity.
  - simpl. intros H. apply is_zero_match. simpl. apply IHa. apply H.
  - intros H. inversion H. Qed.

Fixpoint repeat (foo : bin -> bin) (n : nat) (a : bin) :=
  match n with
  | S n => repeat foo n (foo a)
  | O   => a
  end.

Example repeat_add: repeat (fun x => x +* (oneb 8)) 63 (zerob 8) = pad 8 (n2b 63).
Proof. reflexivity. Qed.

Definition const (X : Type) (Y : Type) (x : X) (y : Y) := x.

Theorem const_id: forall (X : Type) (Y : Type) (x : X) (y : Y),
  const X Y x y = x.
Proof. intros X Y x y. unfold const. reflexivity. Qed.

Definition single_shift_test (a : bin) (gv : nat) := ((D0 a) << gv) = D0 (a << gv).
Example single_shift_ex0: single_shift_test (n2b 510) 3. Proof. unfold single_shift_test. simpl. reflexivity. Qed.

Theorem single_shift_d0: forall (a : bin) (gv : nat),
  ((D0 a) << gv) = D0 (a << gv).
Proof. intros a gv. generalize dependent a. induction gv.
  - simpl. reflexivity.
  - simpl. intros a. rewrite IHgv. reflexivity. Qed.

(* Pad with lower-order zero bits, maxing out at bts bits. *)
Fixpoint pad_low (x bts : nat) (a : bin) :=
  match x, bts with
  | S x', S bts' => D0 (pad_low x' bts' a)
  | S x', O      => pad_low x' O a
  | O, _         => a
  end.

Definition lshift_imp_test (x : nat) (a : bin) := a << x = pad_low x (bits a) (repeat drop_hi x a).
Example rit_ex0: lshift_imp_test 0 (n2b 63). Proof. reflexivity. Qed.
Example rit_ex1: lshift_imp_test 1 (n2b 63). Proof. simpl. simpl. reflexivity. Qed.
Example rit_ex2: lshift_imp_test 2 (n2b 63). Proof. reflexivity. Qed.
Example rit_ex3: lshift_imp_test 3 (n2b 63). Proof. reflexivity. Qed.
Example rit_ex4: lshift_imp_test 12 (n2b 63). Proof. reflexivity. Qed.

Lemma bits_match_d0: forall (a : bin),
  bits match a with
       | Z => Z
       | _ => D0 (drop_hi a)
       end = bits a.
Proof. intros a. induction a.
  - simpl. reflexivity.
  - simpl. apply f_equal. apply IHa.
  - simpl. apply f_equal. destruct a.
    + reflexivity.
    + simpl in IHa. simpl. apply IHa.
    + simpl in IHa. simpl. apply IHa. Qed.

Lemma bits_match_d1: forall (a : bin),
  bits match a with
       | Z => Z
       | _ => D1 (drop_hi a)
       end = bits a.
Proof. intros a. induction a.
  - simpl. reflexivity.
  - simpl. apply f_equal. destruct a. 
    + reflexivity.
    + simpl in IHa. simpl. apply IHa.
    + simpl in IHa. simpl. apply IHa.
  - simpl. apply f_equal. destruct a.
    + reflexivity.
    + simpl in IHa. simpl. apply IHa.
    + simpl in IHa. simpl. apply IHa. Qed.

Definition lshift_imp (a : bin) (x : nat) := pad_low x (bits a) (repeat drop_hi x a).

(* lshift is equivalent to padding with lower-order zeros, then repeatedly dropping
   the unnecessary higher-order bits: *)
Theorem lshift_imp_correct: forall (x : nat) (a : bin),
  a << x = lshift_imp a x.
Proof. unfold lshift_imp. induction x.
  - induction a ; reflexivity.
  - induction a.
    * simpl. replace 0 with (bits Z). apply IHx. reflexivity.
    * simpl. rewrite single_shift_d0. apply f_equal. simpl in IHa.
      replace (bits a) with (bits (match a with
                    | Z => Z
                    | _ => D0 (drop_hi a)
                    end)).
      + rewrite <- IHx. reflexivity.
      + apply bits_match_d0.
    * simpl. rewrite single_shift_d0. apply f_equal. simpl in IHa.
      replace (bits a) with (bits (match a with
                    | Z => Z
                    | _ => D1 (drop_hi a)
                    end)).
      + rewrite <- IHx. reflexivity.
      + apply bits_match_d1. Qed.

Definition rshift_imp (a : bin) (x : nat) := drop_low x (repeat add_hi x a).

Compute rshift_imp (n2b 255) 4.
Compute (n2b 255) >> 4.

Compute lshift_imp (n2b 255) 4.
Compute (n2b 255) << 4.

Theorem drop_low_pad: forall (x : nat) (a : bin),
  drop_low x (pad x a) = Z.
Proof. induction x.
  - reflexivity.
  - intros a. induction a.
    + simpl. apply IHx.
    + simpl. apply IHx.
    + simpl. apply IHx. Qed. 

Lemma pad_id: forall (b : bin),
  is_zero b = true -> b = pad (bits b) Z.
Proof. intros b H. induction b.
  - reflexivity.
  - simpl. apply f_equal. apply IHb. apply H.
  - inversion H. Qed.

Lemma repeat_add_hi_d0: forall (x : nat) (a : bin),
  repeat add_hi x (D0 a) = D0 (repeat add_hi x a).
Proof. intros x. induction x.
  - induction a ; reflexivity.
  - induction a.
    * simpl. apply IHx.
    * simpl. apply IHx.
    * simpl. apply IHx. Qed.

Lemma repeat_add_hi_d1: forall (x : nat) (a : bin),
  repeat add_hi x (D1 a) = D1 (repeat add_hi x a).
Proof. intros x. induction x.
  - induction a ; reflexivity.
  - induction a.
    * simpl. apply IHx.
    * simpl. apply IHx.
    * simpl. apply IHx. Qed.

Lemma repeat_add_hi: forall (x : nat) (b : bin),
  is_zero b = true -> repeat add_hi x b = pad (bits b + x) Z.
Proof. intros x. induction x.
  - simpl. induction b.
    * reflexivity.
    * simpl. intros H. apply f_equal. rewrite <- plus_n_O. apply pad_id. apply H.
    * simpl. intros H. inversion H.
  - simpl. intros b H. induction b.
    * simpl. replace x with (bits Z + x). rewrite <- IHx. simpl. apply repeat_add_hi_d0.
      reflexivity. reflexivity.
    * simpl. rewrite repeat_add_hi_d0. apply f_equal. apply IHb. apply H.
    * inversion H. Qed.

Theorem rshift_imp_correct: forall (x : nat) (a : bin),
  a >> x = rshift_imp a x.
Proof. unfold rshift_imp. induction x.
  - induction a ; reflexivity.
  - simpl. induction a.
    * simpl. rewrite rshift_id. assert (H: forall (x : nat), repeat add_hi x (D0 Z) = D0 (pad x Z)).
      { intros x0. induction x0. reflexivity. simpl. rewrite <- IHx0.
        symmetry. repeat rewrite repeat_add_hi_d0. repeat f_equal. }
      { rewrite repeat_add_hi_d0. rewrite repeat_add_hi. simpl. symmetry. apply drop_low_pad.
        reflexivity. }
      reflexivity.
    * simpl. rewrite repeat_add_hi_d0. apply IHx.
    * simpl. rewrite repeat_add_hi_d1. apply IHx. Qed.

Lemma mask_zerob: forall (n : nat),
  mask n 0 = zerob n.
Proof. unfold zerob. induction n.
  - reflexivity.
  - simpl. apply f_equal. apply IHn. Qed.

Lemma match_drop_is_lshift: forall (a : bin),
  is_zero a = true ->
  a = match a with
     | Z => Z
     | _ => D0 (drop_hi a)
     end.
Proof. induction a.
  - reflexivity.
  - intros H. apply f_equal. simpl. apply IHa. apply H.
  - intros H. inversion H. Qed.

Lemma minus_d0_d1_carry: forall (a b : bin),
  minus' a (D0 b) 1 = minus' a (D1 b) 0.
Proof. induction a.
  - induction b ; reflexivity.
  - induction b ; reflexivity.
  - induction b ; reflexivity. Qed.

Lemma minus_carry_mask: forall (n bc : nat),
  minus'
    (match pad n Z with
     | Z => Z
     | _ => D1 (drop_hi (pad n Z))
     end << bc) (pad n Z) 1
    = minus' (oneb n << bc) (oneb n) 0.
Proof. induction n.
  - induction bc.
    * reflexivity.
    * simpl. rewrite lshift_id ; reflexivity.
  - induction bc.
    * simpl. apply f_equal. rewrite minus_id.
      replace (match pad n Z with
       | Z => Z
       | _ => D0 (drop_hi (pad n Z))
       end) with
        (pad n Z). rewrite minus_id. reflexivity.
      apply match_drop_is_lshift. apply is_zero_pad_distr ; reflexivity.
    * simpl. rewrite <- match_drop_is_lshift. rewrite minus_d0_d1_carry.
      reflexivity. apply is_zero_pad_distr. reflexivity. Qed.

Theorem mask_correct: forall (bc n : nat),
  mask n bc = ((oneb n << bc) -* oneb n).
Proof. unfold minus. intros bc n. generalize dependent bc. induction n.
  - induction bc.
    * reflexivity.
    * simpl. rewrite lshift_id ; reflexivity.
  - induction bc.
    * simpl. apply f_equal. rewrite minus_id. rewrite bits_pad. apply mask_zerob.
    * simpl. rewrite single_shift_d0. simpl. apply f_equal. simpl in IHbc.
      rewrite minus_carry_mask. apply IHn. Qed.
