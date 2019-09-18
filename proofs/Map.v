Require Import Coq.Arith.Plus.
Require Import Binary.
Require Import Common.

Definition lookup (β_c β_k γ_v : nat) (α : bin) :=
  if gte β_k γ_v
  then ((α & ((mask (bits α) β_c) & (~~ (mask (bits α) β_k)))) >> (β_k - γ_v)) \ (α & (~~ (mask (bits α) β_c)))
  else ((α & ((mask (bits α) β_c) & (~~ (mask (bits α) β_k)))) << (γ_v - β_k)) \ (α & (~~ (mask (bits α) β_c))).

Theorem conserve_bits_lookup: forall (b0 b1 g : nat) (a : bin),
  bits (lookup b0 b1 g a) = bits a.
Proof.
  intros b0 b1 g a. unfold lookup. destruct (gte b1 g). { rewrite conserve_bits_or'. rewrite conserve_bits_and.
  - reflexivity.
  - rewrite conserve_bits_neg. rewrite conserve_bits_mask. reflexivity.
  - rewrite conserve_bits_rshift. rewrite conserve_bits_and.
    + rewrite conserve_bits_and.
      * reflexivity.
      * rewrite conserve_bits_neg. rewrite conserve_bits_mask. reflexivity.
    + rewrite conserve_bits_and.
      * rewrite conserve_bits_mask. reflexivity.
      * rewrite conserve_bits_neg. rewrite conserve_bits_mask. rewrite conserve_bits_mask. reflexivity. }
  { rewrite conserve_bits_or'. rewrite conserve_bits_and.
  - reflexivity.
  - rewrite conserve_bits_neg. rewrite conserve_bits_mask. reflexivity.
  - rewrite conserve_bits_lshift. rewrite conserve_bits_and.
    + rewrite conserve_bits_and.
      * reflexivity.
      * rewrite conserve_bits_neg. rewrite conserve_bits_mask. reflexivity.
    + rewrite conserve_bits_and.
      * rewrite conserve_bits_mask. reflexivity.
      * rewrite conserve_bits_neg. rewrite conserve_bits_mask. rewrite conserve_bits_mask. reflexivity. } Qed.

Compute (n2b (b2n (lookup 5 3 1 (n2b (pow 2 5 - pow 2 3))) + pow 2 1) & ~~(mask 5 3)).

Compute b2n (lookup 5 3 1 (n2b 16)).
Compute b2n (lookup 5 3 1 (n2b 24)).

(*
(* Adjacent blocks have block descriptors which are adjacent and non-overlapping (contiguous) *)
Theorem bdescr_correct: forall (a : bin),
  bdescr bc bk gv (a +* (oneb (bits a) << bk)) = (bdescr bc bk gv (a)) +* ((oneb (bits a)) << gv).
*)

Example m81: lookup 5 3 1 ((n2b 81) +* (oneb (bits (n2b 81)) << 3)) = (lookup 5 3 1 (n2b 81)) +* ((oneb (bits (n2b 81))) << 1).
Proof. reflexivity. Qed.

Compute (D1 (D1 Z)) +* (D1 (D1 Z)).

(* Mask off bits other than bits [n, m) zero-based. *)
Fixpoint get_n_m (n m : nat) (a : bin) :=
  match n, m, a with
  | S n, S m, D0 a => D0 (get_n_m n m a)
  | S n, S m, D1 a => D0 (get_n_m n m a)
  | O,   S m, D0 a => D0 (get_n_m n m a)
  | O,   S m, D1 a => D1 (get_n_m n m a)
  | O,     O, D0 a => D0 (get_n_m n m a)
  | O,     O, D1 a => D0 (get_n_m n m a)
  | S _,   O, D0 a => D0 (get_n_m n m a)
  | S _,   O, D1 a => D0 (get_n_m n m a)
  | _,     _,    Z => Z
  end.

Compute (get_n_m 3 5 (D0 (D0 (D1 (D1 (D1 Z)))))).

Fixpoint set_first_n (bts n : nat) :=
  match bts with
  | O => Z
  | S bts =>  match n with
              | O   => D0 (set_first_n bts n)
              | S n => D1 (set_first_n bts n)
              end
  end.
(*  match n, bts with
  | O, O => Z
  | O, S bts => D0 (set_first_n n bts)
  | S n, O => Z
  | S n, S bts => D1 (set_first_n n bts)
  end. *)

Compute set_first_n 8 4.

Example set_first_ex: set_first_n 12 8 = ((oneb 12) << 8) -* (oneb 12). Proof. unfold minus. reflexivity. Qed.

(* Set nth (zero-based) bit to one: *)
Fixpoint set_nth (bts n : nat) :=
  match bts with
  | O => Z
  | S bts =>  match n with
              | O   => D1 (pad     bts Z)
              | S n => D0 (set_nth bts n)
              end
  end.

Fixpoint set_nth' (bts n : nat) :=
  match n with
  | O =>  match bts with
          | O => Z
          | S bts' => D1 (pad bts' Z)
          end
  | S n' =>
    match bts with
    | O => Z
    | S bts' => D0 (set_nth' bts' n')
    end
  end.

Compute set_nth' 8 4.
Compute set_nth 8 4.
Compute ((oneb 8) << 4).
Compute ((oneb 0) << 8).
Compute set_nth 0 8.
Compute set_nth' 0 8.

Lemma get_nothing': forall (bts : nat),
  pad bts Z =
  match pad bts Z with
  | Z => Z
  | _ => D0 (drop_hi (pad bts Z))
  end.
Proof. induction bts.
  - reflexivity.
  - simpl. apply f_equal. apply IHbts. Qed.

Theorem oneb_is_drop_hi: forall (bts n : nat),
  match pad bts Z with
  | Z => Z
  | _ => D1 (drop_hi (pad bts Z))
  end << n = oneb bts << n.
Proof. intros bts. induction bts.
  - simpl. reflexivity.
  - simpl. induction n.
    + simpl. apply f_equal. 
      assert (H: forall (a : bin), is_zero a = true -> match a with
      | Z => Z
      | _ => D0 (drop_hi a)
      end = a).
      { intros a H. induction a.
        + reflexivity.
        + simpl. apply f_equal. apply IHa. apply H.
        + inversion H. }
      apply H. rewrite is_zero_pad_distr ; reflexivity.
    + simpl. rewrite single_shift_d0. rewrite single_shift_d0. apply f_equal.
      replace (match pad bts Z with
      | Z => Z
      | _ => D0 (drop_hi (pad bts Z))
      end) with (pad bts Z). reflexivity. apply get_nothing'. Qed.

Theorem set_nth_correct: forall (bts n : nat),
  (oneb bts) << n = set_nth bts n.
Proof. induction bts.
  - induction n.
    * reflexivity.
    * simpl. rewrite lshift_id ; reflexivity.
  - induction n.
    * reflexivity.
    * simpl. rewrite single_shift_d0. apply f_equal. rewrite <- IHbts.
      apply oneb_is_drop_hi. Qed.

Theorem set_first_n_correct: forall (n bts : nat),
  set_first_n bts n = ((oneb bts) << n) -* (oneb bts).
Proof. intros n bts. rewrite <- mask_correct. generalize dependent n.
  induction bts.
  - induction n.
    * reflexivity.
    * simpl. reflexivity.
  - induction n.
    * simpl. apply f_equal. apply IHbts.
    * simpl. apply f_equal. apply IHbts. Qed.

Fixpoint not_set_first_n (bts n : nat) :=
  match bts with
  | O => Z
  | S bts =>  match n with
              | O   => D1 (not_set_first_n bts n)
              | S n => D0 (not_set_first_n bts n)
              end
  end.

Theorem not_set_first_n_correct: forall (bts n : nat),
  not_set_first_n bts n = ~~(((oneb bts) << n) -* (oneb bts)).
Proof. intros bts n. rewrite <- set_first_n_correct. generalize dependent n. induction bts.
  - reflexivity.
  - simpl. induction n.
    * simpl. apply f_equal. apply IHbts.
    * simpl. apply f_equal. apply IHbts. Qed.

(* A binary number of length bts with only bits [n, m) set (zero-based) *)
Fixpoint set_n_m (bts n m : nat) :=
  match bts with
  | O => Z
  | S bts =>
    match n, m with
    | S n, S m => D0 (set_n_m bts n m)
    | O,   S m => D1 (set_n_m bts n m)
    | O,     O => D0 (set_n_m bts n m)
    | S _,   O => D0 (set_n_m bts n m)
    end
  end.

Lemma set_first_n_zero: forall (bts : nat),
  set_first_n bts 0 = pad bts Z.
Proof. induction bts.
  - reflexivity.
  - simpl. apply f_equal. apply IHbts. Qed.

Lemma zero_and: forall (a b : bin),
  bits a = bits b -> is_zero a = true -> a & b = a.
Proof. intros a b H1 H2. generalize dependent b. induction a.
  - induction b.
    * reflexivity.
    * reflexivity.
    * reflexivity.
  - intros b H3. induction b.
    * inversion H3.
    * simpl. apply f_equal. apply IHa. apply H2. inversion H3. reflexivity.
    * simpl. apply f_equal. apply IHa. apply H2. inversion H3. reflexivity.
  - intros b H3. induction b.
    * inversion H3.
    * simpl. inversion H2.
    * inversion H2. Qed.

Lemma set_n_O: forall (bts n : nat),
  set_n_m bts n 0 = pad bts Z.
Proof. intros bts. induction bts.
  - intros n. reflexivity.
  - intros n. induction n.
    * simpl. apply f_equal. apply IHbts.
    * simpl. apply f_equal. apply IHbts. Qed.

Lemma bits_not_set_first_n: forall (bts n : nat),
  bits (not_set_first_n bts n) = bts.
Proof. induction bts.
  - induction n ; reflexivity.
  - induction n.
    * simpl. apply f_equal. apply IHbts.
    * simpl. apply f_equal. apply IHbts. Qed.

Theorem set_n_m_correct: forall (bts n m : nat),
  set_n_m bts n m = (((oneb bts) << m) -* (oneb bts)) & ~~(((oneb bts) << n) -* (oneb bts)).
Proof. intros bts n m. rewrite <- not_set_first_n_correct. rewrite <- set_first_n_correct.
  generalize dependent m. generalize dependent n. induction bts.
  - induction m.
    * destruct n.
      + simpl. reflexivity.
      + simpl. reflexivity.
    * simpl. reflexivity.
  - induction m.
    * induction n.
      + simpl. apply f_equal. apply IHbts.
      + simpl. apply f_equal. rewrite set_first_n_zero. rewrite zero_and.
        apply set_n_O. rewrite bits_pad. symmetry. apply bits_not_set_first_n.
        rewrite is_zero_pad_distr ;reflexivity.
    * induction n.
      + simpl. apply f_equal. apply IHbts.
      + simpl. apply f_equal. apply IHbts. Qed.

Theorem get_n_m_correct: forall (n m : nat) (a : bin),
  get_n_m n m a = a & (set_n_m (bits a) n m).
Proof. intros n. induction n.
  - intros m. induction m.
    * induction a.
      + reflexivity.
      + simpl. apply f_equal. apply IHa.
      + simpl. apply f_equal. apply IHa.
    * induction a.
      + simpl. reflexivity.
      + simpl. apply f_equal. apply IHm.
      + simpl. apply f_equal. apply IHm.
  - intros m. induction m.
    * induction a.
      + reflexivity.
      + simpl. apply f_equal. apply IHa.
      + simpl. apply f_equal. apply IHa.
    * induction a.
      + simpl. reflexivity.
      + simpl. apply f_equal. apply IHn.
      + simpl. apply f_equal. apply IHn. Qed.

Theorem bits_get_n_m: forall (n m : nat) (a : bin),
  bits (get_n_m n m a) = bits a.
Proof. induction n.
  - induction m.
    + induction a.
      * reflexivity.
      * simpl. apply f_equal. apply IHa.
      * simpl. apply f_equal. apply IHa.
    + induction a.
      * reflexivity.
      * simpl. apply f_equal. apply IHm.
      * simpl. apply f_equal. apply IHm.
  - induction m.
    + induction a.
      * reflexivity.
      * simpl. apply f_equal. apply IHa.
      * simpl. apply f_equal. apply IHa.
    + induction a.
      * simpl. reflexivity.
      * simpl. apply f_equal. apply IHn.
      * simpl. apply f_equal. apply IHn. Qed.

Definition decr (n : nat) :=
  match n with
  | O => O
  | S n' => n'
  end.

Compute lookup 7 3 1 (D1 (D1 (D1 (D1 (D1 (D1 (D1 (D1 (D1 Z))))))))).
Definition b255 := (D1 (D1 (D1 (D1 (D1 (D1 (D1 (D1 Z)))))))).

Theorem get_n_O: forall (n : nat) (a : bin),
  get_n_m n 0 a = pad (bits a) Z.
Proof. intros n a. generalize dependent n. induction a.
  - destruct n. reflexivity. reflexivity.
  - induction n.
    * simpl. apply f_equal. apply IHa.
    * simpl. apply f_equal. apply IHa.
  - induction n.
    * simpl. apply f_equal. apply IHa.
    * simpl. apply f_equal. apply IHa. Qed.

(* If some function application (foo a) always produces binary numbers of length (bits a)
   equal to zero, then we can just rewrite that application as a literal zero-padding of
   that many bits: *)
Theorem is_zero_foo: forall (a : bin) (foo : bin -> bin),
  bits (foo a) = bits a -> is_zero (foo a) = true -> foo a = pad (bits a) Z.
Proof. intros a foo H1 H2. induction a.
  - simpl. simpl in H1. destruct (foo Z). reflexivity. inversion H1. inversion H2.
  - simpl. simpl in H1. destruct (foo (D0 a)).
    * inversion H1.
    * simpl in H1. simpl in H2. apply f_equal. inversion H1. apply is_zero_a. apply H2.
    * inversion H2.
  - simpl. simpl in H1. destruct (foo (D1 a)).
    * inversion H1.
    * simpl in H1. simpl in H2. inversion H1. apply f_equal. apply is_zero_a. apply H2.
    * inversion H2. Qed.

Definition ignore_a (x : nat) (foo : bin -> bin) (a : bin) := ignore x a foo.

Theorem ignore_id: forall (x : nat) (foo : bin -> bin) (a : bin),
  ignore_a x foo a = ignore x a foo.
Proof. intros x foo a. unfold ignore_a. reflexivity. Qed.

Theorem ignore_conserve_bits: forall (x : nat) (foo : bin -> bin) (a : bin),
      bits (foo (drop_low x a)) = bits (drop_low x a)
  ->  bits (ignore_a x foo a) = bits a.
Proof. intros x foo a H. generalize dependent x. induction a.
  - intros x H. simpl. induction x.
    + simpl. apply H.
    + simpl. reflexivity.
  - intros x H. simpl. induction x.
    + simpl. simpl in H. apply H.
    + simpl. apply f_equal. simpl in IHx. apply IHa. simpl in H. apply H.
  - intros x H. simpl. induction x.
    + simpl. simpl in H. apply H.
    + simpl. apply f_equal. simpl in IHx. apply IHa. simpl in H. apply H. Qed.

Theorem ignore_is_zero: forall (x : nat) (a : bin) (foo : bin -> bin),
      bits (foo (drop_low x a)) = bits (drop_low x a) (* conservation of bits *)
  ->  is_zero (foo (drop_low x a)) = true             (* foo only produces null things on the rest of (a : bin) *)
  ->  ignore x a foo = pad (bits a) Z.                (* the entire expression is zero *)
Proof. intros x a foo. generalize dependent x. induction a.
  - simpl. induction x.
    * simpl. apply is_zero_foo.
    * simpl. reflexivity.
  - simpl. induction x.
    * simpl. apply is_zero_foo.
    * simpl. intros H1 H2. apply f_equal. apply IHa ; auto.
  - simpl. induction x.
    * simpl. apply is_zero_foo.
    * simpl. intros H1 H2. apply f_equal. apply IHa. apply H1. apply H2. Qed.

(* Compute the address-based index of a value (sizes 2^gv) into the current chunk (of size 2^bc): *)
Definition get_idx (bc bk gv : nat) (a : bin) :=
  if gte bk gv
  then rshift_imp (get_n_m bk bc a) (bk - gv)
  else lshift_imp (get_n_m bk bc a) (gv - bk).

Compute (get_n_m 5 7  (D1 (D1 (D1 (D1 (D1 (D1 (D1 (D1 Z))))))))) >> (5 - 3).
Compute get_idx 7 5 3 (D1 (D1 (D1 (D1 (D1 (D1 (D1 (D1 Z)))))))).

Example get_0: (get_n_m 3 5 b255) >> (abs_minus 3 1) = get_idx 5 3 1 b255. Proof. unfold get_idx. unfold lshift_imp. simpl. reflexivity. Qed.
Example get_1: (get_n_m 3 5 b255) >> (abs_minus 3 2) = get_idx 5 3 2 b255. Proof. reflexivity. Qed.
Example get_2: (get_n_m 3 5 b255) >> (abs_minus 3 3) = get_idx 5 3 3 b255. Proof. reflexivity. Qed.

Compute (get_n_m 3 5 (D1 (D1 (D1 (D1 (D1 (D1 (D1 (D1 Z))))))))) >> (abs_minus 3 2).
Compute get_idx 5 3 2 (D1 (D1 (D1 (D1 (D1 (D1 (D1 (D1 Z)))))))).
Compute (get_n_m 3 5 (D1 (D1 (D1 (D1 (D1 (D1 (D1 (D1 Z))))))))) >> (abs_minus 3 3).
Compute get_idx 5 3 3 (D1 (D1 (D1 (D1 (D1 (D1 (D1 (D1 Z)))))))).
Compute (get_n_m 3 5 (D1 (D1 (D1 (D1 (D1 (D1 (D1 (D1 Z))))))))) << (abs_minus 3 4).
Compute get_idx 5 3 4 (D1 (D1 (D1 (D1 (D1 (D1 (D1 (D1 Z)))))))).

Example get_idx_0': get_idx 0 5 3 (n2b 63) = get_n_m 5 0 (n2b 63) >> (5 - 3).
Proof. simpl. reflexivity. Qed.

(*
Theorem get_idx_0: forall (bk gv : nat) (a : bin),
  get_idx 0 bk gv a = (get_n_m bk 0 a) >> (bk - gv).
Proof. intros bk gv a. generalize dependent bk. generalize dependent gv. induction a.
  - induction gv.
    * simpl. intros bk. rewrite rshift_id.
      + destruct bk. reflexivity. reflexivity.
      + destruct bk. reflexivity. reflexivity.
    * simpl. intros bk. rewrite rshift_id.
      + destruct bk. reflexivity. reflexivity.
      + destruct bk. reflexivity. reflexivity.
  - induction gv.
    * intros bk. simpl. rewrite get_n_O. unfold get_idx. simpl. rewrite get_n_O. rewrite rshift_id.
      + destruct bk. reflexivity. reflexivity.
      + destruct bk. simpl. apply is_zero_pad_distr. reflexivity.
                     simpl. apply is_zero_pad_distr. reflexivity.
    * intros bk. simpl. simpl in IHgv. rewrite get_n_O. rewrite rshift_id. unfold get_idx.
      + destruct bk. simpl. apply f_equal. apply ignore_is_zero. rewrite get_n_O. rewrite bits_pad. reflexivity.
        rewrite get_n_O. rewrite is_zero_pad_distr ; reflexivity. simpl. apply f_equal. apply ignore_is_zero.
        rewrite get_n_O. rewrite bits_pad. reflexivity. rewrite get_n_O. rewrite is_zero_pad_distr ; reflexivity.
      + destruct bk. simpl. apply is_zero_pad_distr ; reflexivity. apply is_zero_pad_distr ; reflexivity.
  - induction gv.
    * intros bk. simpl. rewrite get_n_O. unfold get_idx. simpl. rewrite get_n_O. rewrite rshift_id.
      + destruct bk. reflexivity. reflexivity.
      + destruct bk. simpl. apply is_zero_pad_distr. reflexivity.
                     simpl. apply is_zero_pad_distr. reflexivity.
    * intros bk. simpl. simpl in IHgv. rewrite get_n_O. rewrite rshift_id. unfold get_idx.
      + destruct bk. simpl. apply f_equal. apply ignore_is_zero. rewrite get_n_O. rewrite bits_pad. reflexivity.
        rewrite get_n_O. rewrite is_zero_pad_distr ; reflexivity. simpl. apply f_equal. apply ignore_is_zero.
        rewrite get_n_O. rewrite bits_pad. reflexivity. rewrite get_n_O. rewrite is_zero_pad_distr ; reflexivity.
      + destruct bk. simpl. apply is_zero_pad_distr ; reflexivity. apply is_zero_pad_distr ; reflexivity. Qed.
*)

Example get_idx_bc_0_ex: get_idx 5 0 1 (n2b 127) = (get_n_m 0 5 (n2b 127)) << abs_minus 0 1. Proof. simpl. reflexivity. Qed.

Lemma get_nothing: forall (a : bin),
  get_n_m 0 0 a =
    match get_n_m 0 0 a with
    | Z => Z
    | _ => D0 (drop_hi (get_n_m 0 0 a))
    end.
Proof. induction a.
  - reflexivity.
  - simpl. apply f_equal. apply IHa.
  - simpl. apply f_equal. apply IHa. Qed.

Definition get_idx_test bc bk gv a :=
  if gte bk gv
  then get_idx bc bk gv a = (get_n_m bk bc a) >> (bk - gv)
  else get_idx bc bk gv a = (get_n_m bk bc a) << (gv - bk).

Example get_idx_ex_0: get_idx_test 3 2 1 (n2b 511). Proof. reflexivity. Qed.
Example get_idx_ex_1: get_idx_test 1 2 3 (n2b 511). Proof. reflexivity. Qed.
Example get_idx_ex_2: get_idx_test 3 3 3 (n2b 511). Proof. reflexivity. Qed.
Example get_idx_ex_3: get_idx_test 1 1 1 (n2b 511). Proof. reflexivity. Qed.
Example get_idx_ex_4: get_idx_test 1 3 2 (n2b 511). Proof. reflexivity. Qed.
Example get_idx_ex_5: get_idx_test 2 5 4 (n2b 511). Proof. reflexivity. Qed.
Example get_idx_ex_6: get_idx_test 5 4 2 (n2b 511). Proof. reflexivity. Qed.
Example get_idx_ex_7: get_idx_test 4 2 5 (n2b 511). Proof. reflexivity. Qed.
Example get_idx_ex_8: get_idx_test 2 4 5 (n2b 511). Proof. reflexivity. Qed.
Example get_idx_ex_9: get_idx_test 4 5 2 (n2b 511). Proof. reflexivity. Qed.
Example get_idx_ex_10: get_idx_test 5 2 4 (n2b 511). Proof. reflexivity. Qed.

Definition get_is_ignore_n_test (n m : nat) (a : bin) := get_n_m n m a = ignore n a (get_n_m 0 (m - n)).

Example get_is_ignore_n_ex0: get_is_ignore_n_test 0 0 (n2b 511). Proof. reflexivity. Qed.
Example get_is_ignore_n_ex1: get_is_ignore_n_test 3 6 (n2b 511). Proof. reflexivity. Qed.
Example get_is_ignore_n_ex2: get_is_ignore_n_test 6 3 (n2b 511). Proof. reflexivity. Qed.
Example get_is_ignore_n_ex3: get_is_ignore_n_test 6 6 (n2b 511). Proof. reflexivity. Qed.
Example get_is_ignore_n_ex4: get_is_ignore_n_test 0 22 (n2b 511). Proof. reflexivity. Qed.
Example get_is_ignore_n_ex5: get_is_ignore_n_test 22 0 (n2b 511). Proof. reflexivity. Qed.

Lemma get_is_ignore_n: forall (n m : nat) (a : bin),
  get_n_m n m a = ignore n a (get_n_m 0 (m - n)).
Proof. intros n. induction n.
  - intros m a. generalize dependent m. induction a.
    * destruct m ; auto.
    * destruct m ; auto.
    * destruct m ; auto.
  - intros m a. generalize dependent m. induction a.
    * simpl. destruct m ; auto.
    * simpl. induction m.
      + apply f_equal. simpl. rewrite get_n_O. symmetry. apply ignore_is_zero.
        { rewrite bits_get_n_m. reflexivity. }
        { rewrite get_n_O. apply is_zero_pad_distr. reflexivity. }
      + apply f_equal. simpl. rewrite <- IHn. reflexivity.
    * simpl. induction m.
      + apply f_equal. simpl. rewrite get_n_O. symmetry. apply ignore_is_zero.
        { rewrite bits_get_n_m. reflexivity. }
        { rewrite get_n_O. apply is_zero_pad_distr. reflexivity. }
      + apply f_equal. simpl. rewrite <- IHn. reflexivity. Qed.

Fixpoint get_n_m_drop_hi (n m : nat) (a : bin) :=
  match get_n_m n m a with
  | Z => Z
  | _ => D0 (drop_hi (get_n_m n m a))
  end.

(* Get the first n bits: *)
Fixpoint get_n (n : nat) (a : bin) :=
  match n, a with
  | S n', D0 a' => D0 (get_n n' a')
  | S n', D1 a' => D1 (get_n n' a')
  | S _,  Z     => Z
  | O, _ => pad (bits a) Z
  end.

Theorem get_n_id: forall (n : nat) (a : bin),
  get_n_m 0 n a = get_n n a.
Proof. intros n. induction n.
  - induction a.
    * reflexivity.
    * simpl. apply f_equal. apply get_n_O.
    * simpl. apply f_equal. apply get_n_O.
  - induction a.
    * simpl. reflexivity.
    * simpl. apply f_equal. apply IHn.
    * simpl. apply f_equal. apply IHn. Qed.

Lemma is_zero_get_n: forall (n : nat) (x : bin),
  is_zero x = true -> is_zero (get_n n x) = true.
Proof. intros n x. generalize dependent n. induction x.
  - destruct n. reflexivity. reflexivity.
  - intros n H. destruct n.
    + simpl. rewrite is_zero_pad_distr ; reflexivity.
    + simpl. rewrite IHx. reflexivity. apply H.
  - intros n H. inversion H. Qed.

Theorem get_nothing'': forall (a : bin),
  get_n 0 a = pad (bits a) Z.
Proof. induction a ; repeat reflexivity. Qed.

Lemma match_get_n: forall (bc : nat) (a b : bin),
  match get_n bc a with
  | Z => Z
  | _ => b
  end =
  match a with
  | Z => Z
  | _ => b
  end.
Proof. intros bc a b. induction a.
  - destruct bc ; reflexivity.
  - induction bc ; reflexivity.
  - induction bc ; reflexivity.  Qed.

Definition get_idx_bc_0_test (bc gv : nat) (a : bin) := get_idx bc 0 gv a = (get_n_m 0 bc a) << (gv - 0).

(* Example ssd0_ex0: single_shift_d0_test 3 3 (n2b 511). Proof. unfold single_shift_d0_test. simpl. *)
Example gibc0_ex0: get_idx_bc_0_test 3 3 (n2b 511). Proof. reflexivity. Qed.
Example gibc0_ex1: get_idx_bc_0_test 0 0 (n2b 511). Proof. reflexivity. Qed.
Example gibc0_ex2: get_idx_bc_0_test 0 1 (n2b 511). Proof. reflexivity. Qed.
Example gibc0_ex3: get_idx_bc_0_test 1 0 (n2b 511). Proof. reflexivity. Qed.
Example gibc0_ex4: get_idx_bc_0_test 1 2 (n2b 511). Proof. reflexivity. Qed.
Example gibc0_ex5: get_idx_bc_0_test 2 1 (n2b 511). Proof. reflexivity. Qed.
Example gibc0_ex6: get_idx_bc_0_test 0 13 (n2b 511). Proof. reflexivity. Qed.
Example gibc0_ex7: get_idx_bc_0_test 13 0 (n2b 511). Proof. reflexivity. Qed.
Example gibc0_ex8: get_idx_bc_0_test 3 9 (n2b 511). Proof. reflexivity. Qed.

Fixpoint get_idx' (bc bk gv : nat) (a : bin) := 
  match gv, a with
  | S gv', D0 a' => D0 (get_idx' bc bk gv' a')
  | S gv', D1 a' => D0 (get_idx' bc bk gv' a')
  | S _,  Z      => Z
  | O, _         => get_n (bc - bk) a
  end.

Compute get_idx' 5 0 3 (n2b 511).
Compute (get_n_m 0 5 (n2b 511)) << 3.

(* a -> gv -> bk *)
(* ((cav, vac)), bk = 0 *)
Theorem get_idx_bc_0_0: forall (bc : nat) (a : bin),
  get_idx bc 0 0 a = get_n_m 0 bc a.
Proof. intros bc a. unfold get_idx. generalize dependent bc. induction a.
  - simpl. induction bc.
    * reflexivity.
    * destruct bc ; reflexivity.
  - intros bc. induction bc ; auto.
  - intros bc. induction bc ; auto. Qed.

Theorem get_idx_correct: forall (bc bk gv : nat) (a : bin),
  gte bk gv = true -> get_idx bc bk gv a = (get_n_m bk bc a) >> (bk - gv).
Proof. intros bc bk gv a H. rewrite rshift_imp_correct. unfold get_idx.
  destruct (gte bk gv). reflexivity. inversion H. Qed.

Theorem get_idx_correct': forall (bc bk gv : nat) (a : bin),
  gte bk gv = false -> get_idx bc bk gv a = (get_n_m bk bc a) << (gv - bk).
Proof. intros bc bk gv a H. rewrite lshift_imp_correct. unfold get_idx.
  destruct (gte bk gv). inversion H. reflexivity. Qed.

Definition chunk_idx (bc : nat) (a : bin) := ignore bc a id.

Lemma ignore_0_id: forall (a : bin),
  a = ignore 0 a id.
Proof. unfold id. induction a ; reflexivity. Qed.

Theorem chunk_idx_correct: forall (a : bin) (bc : nat),
  chunk_idx bc a = a & ~~ (mask (bits a) bc).
Proof. unfold chunk_idx. induction a.
  - induction bc ; reflexivity.
  - induction bc.
    * simpl. unfold id. apply f_equal. rewrite <- IHa. apply ignore_0_id.
    * simpl. apply f_equal. apply IHa.
  - induction bc.
    * simpl. unfold id. apply f_equal. rewrite <- IHa. apply ignore_0_id.
    * simpl. apply f_equal. apply IHa. Qed.

Definition lookup_imp (bc bk gv : nat) (a : bin) :=
  (get_idx bc bk gv a) \ (chunk_idx bc a).

Theorem lookup_imp_correct: forall (bc bk gv : nat) (a : bin),
  gte bk gv = true -> lookup_imp bc bk gv a = lookup bc bk gv a.
Proof. intros bc bk gv a H. unfold lookup_imp. rewrite chunk_idx_correct. rewrite get_idx_correct.
  unfold lookup. destruct (gte bk gv).
  { rewrite get_n_m_correct. rewrite set_n_m_correct. rewrite mask_correct. rewrite mask_correct. reflexivity. }
  { inversion H. }
  { apply H. } Qed.

Theorem lookup_imp_correct': forall (bc bk gv : nat) (a : bin),
  gte bk gv = false -> lookup_imp bc bk gv a = lookup bc bk gv a.
Proof. intros bc bk gv a H. unfold lookup_imp. rewrite chunk_idx_correct. rewrite get_idx_correct'.
  unfold lookup. destruct (gte bk gv).
  { inversion H. }
  { rewrite get_n_m_correct. rewrite set_n_m_correct. rewrite mask_correct. rewrite mask_correct. reflexivity. }
  { apply H. } Qed.

(* All mapped values are adjacent and of size ((oneb (bits a)) << gv) = 2^gv *)
Definition lookup_values_adjacent (bc bk gv : nat) (a : bin) :=
  is_zero (a & (~~ (mask (bits a) bk))) = false
  -> lookup bc bk gv (a -* ((oneb (bits a)) << bk)) = (lookup bc bk gv a) -* ((oneb (bits a)) << gv).

Definition lookup_chunks_symmetric (bc bk gv : nat) (a : bin) :=
  is_zero (a & (~~ (mask (bits a) bk))) = false
  -> lookup bc bk gv (a -* ((oneb (bits a)) << bc)) = (lookup bc bk gv a) -* ((oneb (bits a)) << bc).

Compute ((pad (bits (n2b 511)) (n2b 3)) +* (n2b 511)).

Example look_ex0: lookup_values_adjacent 5 3 2 (pad 8 (n2b 233)).
Proof. unfold lookup_values_adjacent. unfold lookup. unfold plus. simpl. reflexivity. Qed.
Example look_ex1: lookup_values_adjacent 5 3 2 (pad 8 (n2b 511)).
Proof. unfold lookup_values_adjacent. reflexivity. Qed.
Example look_ex2: lookup_values_adjacent 6 3 2 (pad 8 (n2b 0)).
Proof. unfold lookup_values_adjacent. intros H. inversion H. Qed.

Example look_ex3: lookup_chunks_symmetric 5 3 2 (pad 8 (n2b 233)).
Proof. unfold lookup_chunks_symmetric. unfold lookup. unfold plus. simpl. reflexivity. Qed.
Example look_ex4: lookup_chunks_symmetric 5 3 2 (pad 8 (n2b 511)).
Proof. unfold lookup_chunks_symmetric. reflexivity. Qed.
Example look_ex5: lookup_chunks_symmetric 6 3 2 (pad 8 (n2b 0)).
Proof. unfold lookup_chunks_symmetric. intros H. inversion H. Qed.

(*
Theorem lookup_correct: forall (bc bk gv : nat) (a : bin),
  lookup_values_adjacent bc bk gv a.
Proof. unfold lookup_values_adjacent. intros bc bk gv a H.
*)


Compute chunk_idx 5 (n2b 511).
Compute (n2b 511) & ~~(mask (bits (n2b 511)) 5).

(*
Compute zero_pad 4 (D1 (D1 (D0 Z))).
Compute zero_pad 8 (nat_to_bin 128). *)

Compute (mask 8 5) & (~~ mask 8 3).
(*
Compute (lookup 5 3 2 (zero_pad 8 (nat_to_bin 153))).
Compute bin_to_nat (lookup 5 3 2 (zero_pad 8 (nat_to_bin 4))).
Compute (bits (zero_pad 8 (nat_to_bin 251))).
Compute ((zero_pad 8 (nat_to_bin 255)) & (~~ (mask 8 5))). *)
