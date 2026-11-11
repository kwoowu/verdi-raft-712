From VerdiRaft Require Import Raft.
From Verdi Require Import VarD.
(* because we don't `Import`, `string` will refer to Rocq's standard string type.
   primstrings should always be written as PrimString.string *)
From Coq Require PrimString PString.

Definition pstring_eq_dec (s1 s2 : PrimString.string) : {s1 = s2} + {s1 <> s2}.
Proof.
    destruct (PrimString.compare s1 s2) eqn:H.
    (* case 1: Eq *)
    - left. apply PString.compare_eq. exact H.
    (* case 2: Lt 
       this is a proof-by-contradiction. our goal is s1 <> s2, so we assume s1 = s2 
       and use it to derive that s2 < s2, which contradicts the fact that s2 = s2 *)
    - right. intro Heq. subst. rewrite PString.compare_refl in H. discriminate.
    (* case 3: Gt
       same logic as Lt case *)
    - right. intro Heq. subst. rewrite PString.compare_refl in H. discriminate.
Defined.

Section VarDRaft.
  Variable n : nat.

  Instance raft_params : RaftParams vard_base_params :=
    {
      N := n;
      input_eq_dec := input_eq_dec;
      output_eq_dec := output_eq_dec;
      clientId := PrimString.string;
      clientId_eq_dec := pstring_eq_dec
    }.

  Definition vard_raft_base_params := base_params.
  Definition vard_raft_multi_params := multi_params.
  Definition vard_raft_failure_params := failure_params.
End VarDRaft.
