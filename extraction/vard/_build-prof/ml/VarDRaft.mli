
type __ = Obj.t

val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option

type comparison =
| Eq
| Lt
| Gt

val pred : int -> int

val add : int -> int -> int



module type DecidableTypeOrig =
 sig
  type t

  val eq_dec : t -> t -> bool
 end

module type EqLtLe =
 sig
  type t
 end

module MakeOrderTac :
 functor (O:EqLtLe) ->
 functor (P:sig
 end) ->
 sig
 end

module Nat :
 sig
 end

val list_eq_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool

val seq : int -> int -> int list

type positive =
| XI of positive
| XO of positive
| XH

module Pos :
 sig
  val compare_cont : comparison -> positive -> positive -> comparison

  val compare : positive -> positive -> comparison
 end

val assoc : ('a1 -> 'a1 -> bool) -> ('a1 * 'a2) list -> 'a1 -> 'a2 option

val assoc_default :
  ('a1 -> 'a1 -> bool) -> ('a1 * 'a2) list -> 'a1 -> 'a2 -> 'a2

val assoc_set :
  ('a1 -> 'a1 -> bool) -> ('a1 * 'a2) list -> 'a1 -> 'a2 -> ('a1 * 'a2) list

val dedup : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list

val filterMap : ('a1 -> 'a2 option) -> 'a1 list -> 'a2 list

type 'x compare0 =
| LT
| EQ
| GT

module type OrderedType =
 sig
  type t

  val compare : t -> t -> t compare0

  val eq_dec : t -> t -> bool
 end

module OrderedTypeFacts :
 functor (O:OrderedType) ->
 sig
  module TO :
   sig
    type t = O.t
   end

  module IsTO :
   sig
   end

  module OrderTac :
   sig
   end

  val eq_dec : O.t -> O.t -> bool

  val lt_dec : O.t -> O.t -> bool

  val eqb : O.t -> O.t -> bool
 end

module KeyOrderedType :
 functor (O:OrderedType) ->
 sig
  module MO :
   sig
    module TO :
     sig
      type t = O.t
     end

    module IsTO :
     sig
     end

    module OrderTac :
     sig
     end

    val eq_dec : O.t -> O.t -> bool

    val lt_dec : O.t -> O.t -> bool

    val eqb : O.t -> O.t -> bool
   end
 end

val string_dec : char list -> char list -> bool

module PositiveOrderedTypeBits :
 sig
  type t = positive

  val compare : t -> t -> positive compare0

  val eq_dec : positive -> positive -> bool
 end

type baseParams =
| Build_BaseParams

type data = __

type input = __

type output = __

type oneNodeParams = { init : data; handler : (input -> data -> output * data) }

type multiParams = { msg_eq_dec : (__ -> __ -> bool);
                     name_eq_dec : (__ -> __ -> bool); nodes : __ list;
                     init_handlers : (__ -> data);
                     net_handlers : (__ -> __ -> __ -> data -> (output
                                    list * data) * (__ * __) list);
                     input_handlers : (__ -> input -> data -> (output
                                      list * data) * (__ * __) list) }

type failureParams =
  data -> data
  (* singleton inductive, whose constructor was Build_FailureParams *)

module type DecidableType =
 DecidableTypeOrig

module type WS =
 sig
  module E :
   DecidableType

  type key = E.t

  type 'x t

  val empty : 'a1 t

  val is_empty : 'a1 t -> bool

  val add : key -> 'a1 -> 'a1 t -> 'a1 t

  val find : key -> 'a1 t -> 'a1 option

  val remove : key -> 'a1 t -> 'a1 t

  val mem : key -> 'a1 t -> bool

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

  val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

  val map2 :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

  val elements : 'a1 t -> (key * 'a1) list

  val cardinal : 'a1 t -> int

  val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

  val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
 end

val append : positive -> positive -> positive

module PositiveMap :
 sig
  module E :
   sig
    type t = positive

    val compare : t -> t -> positive compare0

    val eq_dec : positive -> positive -> bool
   end

  module ME :
   sig
    module MO :
     sig
      module TO :
       sig
        type t = positive
       end

      module IsTO :
       sig
       end

      module OrderTac :
       sig
       end

      val eq_dec : positive -> positive -> bool

      val lt_dec : positive -> positive -> bool

      val eqb : positive -> positive -> bool
     end
   end

  type key = positive

  type 'a tree =
  | Leaf
  | Node of 'a tree * 'a option * 'a tree

  type 'a t = 'a tree

  val empty : 'a1 t

  val is_empty : 'a1 t -> bool

  val find : key -> 'a1 t -> 'a1 option

  val mem : key -> 'a1 t -> bool

  val add : key -> 'a1 -> 'a1 t -> 'a1 t

  val remove : key -> 'a1 t -> 'a1 t

  val xelements : 'a1 t -> key -> (key * 'a1) list

  val elements : 'a1 t -> (key * 'a1) list

  val cardinal : 'a1 t -> int

  val xfind : key -> key -> 'a1 t -> 'a1 option

  val xmapi : (key -> 'a1 -> 'a2) -> 'a1 t -> key -> 'a2 t

  val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

  val xmap2_l : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t

  val xmap2_r : ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t

  val _map2 :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

  val map2 :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

  val xfoldi : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> key -> 'a2

  val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

  val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
 end

module WFacts_fun :
 functor (E:DecidableType) ->
 functor (M:sig
  type key = E.t

  type 'x t

  val empty : 'a1 t

  val is_empty : 'a1 t -> bool

  val add : key -> 'a1 -> 'a1 t -> 'a1 t

  val find : key -> 'a1 t -> 'a1 option

  val remove : key -> 'a1 t -> 'a1 t

  val mem : key -> 'a1 t -> bool

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

  val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

  val map2 :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

  val elements : 'a1 t -> (key * 'a1) list

  val cardinal : 'a1 t -> int

  val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

  val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
 end) ->
 sig
  val eqb : E.t -> E.t -> bool

  val coq_In_dec : 'a1 M.t -> M.key -> bool
 end

module WFacts :
 functor (M:WS) ->
 sig
  val eqb : M.E.t -> M.E.t -> bool

  val coq_In_dec : 'a1 M.t -> M.key -> bool
 end

module Facts :
 functor (M:WS) ->
 sig
  val eqb : M.E.t -> M.E.t -> bool

  val coq_In_dec : 'a1 M.t -> M.key -> bool
 end

module type VWS =
 sig
  module E :
   DecidableType

  type key = E.t

  type 'x t

  val empty : 'a1 t

  val add : key -> 'a1 -> 'a1 t -> 'a1 t

  val find : key -> 'a1 t -> 'a1 option

  val remove : key -> 'a1 t -> 'a1 t
 end

module type IndexedType =
 sig
  type t

  val index : t -> positive

  val eq : t -> t -> bool
 end

module IT_to_DT :
 functor (I:IndexedType) ->
 sig
  type t = I.t

  val eq_dec : I.t -> I.t -> bool
 end

module IndexedPositiveMap :
 functor (X:IndexedType) ->
 sig
  module E :
   sig
    type t = X.t

    val eq_dec : X.t -> X.t -> bool
   end

  module M :
   sig
    module E :
     sig
      type t = positive

      val compare : t -> t -> positive compare0

      val eq_dec : positive -> positive -> bool
     end

    module ME :
     sig
      module MO :
       sig
        module TO :
         sig
          type t = positive
         end

        module IsTO :
         sig
         end

        module OrderTac :
         sig
         end

        val eq_dec : positive -> positive -> bool

        val lt_dec : positive -> positive -> bool

        val eqb : positive -> positive -> bool
       end
     end

    type key = positive

    type 'a tree = 'a PositiveMap.tree =
    | Leaf
    | Node of 'a tree * 'a option * 'a tree

    type 'a t = 'a tree

    val empty : 'a1 t

    val is_empty : 'a1 t -> bool

    val find : key -> 'a1 t -> 'a1 option

    val mem : key -> 'a1 t -> bool

    val add : key -> 'a1 -> 'a1 t -> 'a1 t

    val remove : key -> 'a1 t -> 'a1 t

    val xelements : 'a1 t -> key -> (key * 'a1) list

    val elements : 'a1 t -> (key * 'a1) list

    val cardinal : 'a1 t -> int

    val xfind : key -> key -> 'a1 t -> 'a1 option

    val xmapi : (key -> 'a1 -> 'a2) -> 'a1 t -> key -> 'a2 t

    val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

    val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

    val xmap2_l : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t

    val xmap2_r : ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t

    val _map2 :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

    val map2 :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

    val xfoldi : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> key -> 'a2

    val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

    val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
   end

  module F :
   sig
    val eqb : positive -> positive -> bool

    val coq_In_dec : 'a1 M.t -> M.key -> bool
   end

  type key = X.t

  type 'a t = 'a M.t

  val empty : 'a1 M.t

  val add : key -> 'a1 -> 'a1 t -> 'a1 t

  val find : key -> 'a1 t -> 'a1 option

  val remove : key -> 'a1 t -> 'a1 t
 end

module IndexedString :
 sig
  type t = char list

  val eq : char list -> char list -> bool

  val positive_of_digits : bool list -> positive -> positive

  val list_bool_of_ascii : char -> bool list

  val index : char list -> positive
 end

module LogTimeStringMap :
 sig
  module E :
   sig
    type t = IndexedString.t

    val eq_dec : IndexedString.t -> IndexedString.t -> bool
   end

  module M :
   sig
    module E :
     sig
      type t = positive

      val compare : t -> t -> positive compare0

      val eq_dec : positive -> positive -> bool
     end

    module ME :
     sig
      module MO :
       sig
        module TO :
         sig
          type t = positive
         end

        module IsTO :
         sig
         end

        module OrderTac :
         sig
         end

        val eq_dec : positive -> positive -> bool

        val lt_dec : positive -> positive -> bool

        val eqb : positive -> positive -> bool
       end
     end

    type key = positive

    type 'a tree = 'a PositiveMap.tree =
    | Leaf
    | Node of 'a tree * 'a option * 'a tree

    type 'a t = 'a tree

    val empty : 'a1 t

    val is_empty : 'a1 t -> bool

    val find : key -> 'a1 t -> 'a1 option

    val mem : key -> 'a1 t -> bool

    val add : key -> 'a1 -> 'a1 t -> 'a1 t

    val remove : key -> 'a1 t -> 'a1 t

    val xelements : 'a1 t -> key -> (key * 'a1) list

    val elements : 'a1 t -> (key * 'a1) list

    val cardinal : 'a1 t -> int

    val xfind : key -> key -> 'a1 t -> 'a1 option

    val xmapi : (key -> 'a1 -> 'a2) -> 'a1 t -> key -> 'a2 t

    val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

    val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

    val xmap2_l : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t

    val xmap2_r : ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t

    val _map2 :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

    val map2 :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

    val xfoldi : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> key -> 'a2

    val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

    val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
   end

  module F :
   sig
    val eqb : positive -> positive -> bool

    val coq_In_dec : 'a1 M.t -> M.key -> bool
   end

  type key = IndexedString.t

  type 'a t = 'a M.t

  val empty : 'a1 M.t

  val add : key -> 'a1 -> 'a1 t -> 'a1 t

  val find : key -> 'a1 t -> 'a1 option

  val remove : key -> 'a1 t -> 'a1 t
 end

type ('s, 'a) genHandler1 = 's -> 'a * 's

val ret : 'a2 -> ('a1, 'a2) genHandler1

val bind :
  ('a1, 'a2) genHandler1 -> ('a2 -> ('a1, 'a3) genHandler1) -> ('a1, 'a3)
  genHandler1

val write_output : 'a2 -> ('a1, 'a2) genHandler1

val modify : ('a1 -> 'a1) -> ('a1, unit) genHandler1

val get : ('a1, 'a1) genHandler1

val runGenHandler1 : 'a1 -> ('a1, 'a2) genHandler1 -> 'a2 * 'a1

type key0 = char list

type value = char list

type input0 =
| Put of key0 * value
| Get of key0
| Del of key0
| CAS of key0 * value option * value
| CAD of key0 * value

type output0 =
| Response of key0 * value option * value option

module VarDFunctor :
 functor (Map:VWS with type E.t = char list) ->
 sig
  val key_eq_dec : char list -> char list -> bool

  val value_eq_dec : char list -> char list -> bool

  val val_eq_dec : value option -> value option -> bool

  val input_eq_dec : input0 -> input0 -> bool

  val output_eq_dec : output0 -> output0 -> bool

  type data = char list Map.t

  val beq_key : key0 -> key0 -> bool

  val getk : Map.key -> (data, value option) genHandler1

  val setk : Map.key -> char list -> (data, unit) genHandler1

  val delk : Map.key -> (data, unit) genHandler1

  val resp :
    key0 -> value option -> value option -> (data, output0) genHandler1

  val coq_VarDHandler' : input0 -> (data, output0) genHandler1

  val runHandler :
    (input0 -> (data, output0) genHandler1) -> input0 -> data ->
    output0 * data

  val coq_VarDHandler : input0 -> data -> output0 * data

  val init_map : char list Map.t

  val vard_base_params : baseParams

  val vard_one_node_params : oneNodeParams

  val input_key : input0 -> key0

  val operate : input0 -> value option -> value option * value option

  val interpret :
    key0 -> input0 list -> value option -> value option * value option

  val inputs_with_key : (input0 * output0) list -> key0 -> input0 list
 end

module LogTimeVarD :
 sig
  val key_eq_dec : char list -> char list -> bool

  val value_eq_dec : char list -> char list -> bool

  val val_eq_dec : value option -> value option -> bool

  val input_eq_dec : input0 -> input0 -> bool

  val output_eq_dec : output0 -> output0 -> bool

  type data = char list LogTimeStringMap.t

  val beq_key : key0 -> key0 -> bool

  val getk : LogTimeStringMap.key -> (data, value option) genHandler1

  val setk : LogTimeStringMap.key -> char list -> (data, unit) genHandler1

  val delk : LogTimeStringMap.key -> (data, unit) genHandler1

  val resp :
    key0 -> value option -> value option -> (data, output0) genHandler1

  val coq_VarDHandler' : input0 -> (data, output0) genHandler1

  val runHandler :
    (input0 -> (data, output0) genHandler1) -> input0 -> data ->
    output0 * data

  val coq_VarDHandler : input0 -> data -> output0 * data

  val init_map : char list LogTimeStringMap.t

  val vard_base_params : baseParams

  val vard_one_node_params : oneNodeParams

  val input_key : input0 -> key0

  val operate : input0 -> value option -> value option * value option

  val interpret :
    key0 -> input0 list -> value option -> value option * value option

  val inputs_with_key : (input0 * output0) list -> key0 -> input0 list
 end

module VarD :
 sig
  val key_eq_dec : char list -> char list -> bool

  val value_eq_dec : char list -> char list -> bool

  val val_eq_dec : value option -> value option -> bool

  val input_eq_dec : input0 -> input0 -> bool

  val output_eq_dec : output0 -> output0 -> bool

  type data = char list LogTimeStringMap.t

  val beq_key : key0 -> key0 -> bool

  val getk : LogTimeStringMap.key -> (data, value option) genHandler1

  val setk : LogTimeStringMap.key -> char list -> (data, unit) genHandler1

  val delk : LogTimeStringMap.key -> (data, unit) genHandler1

  val resp :
    key0 -> value option -> value option -> (data, output0) genHandler1

  val coq_VarDHandler' : input0 -> (data, output0) genHandler1

  val runHandler :
    (input0 -> (data, output0) genHandler1) -> input0 -> data ->
    output0 * data

  val coq_VarDHandler : input0 -> data -> output0 * data

  val init_map : char list LogTimeStringMap.t

  val vard_base_params : baseParams

  val vard_one_node_params : oneNodeParams

  val input_key : input0 -> key0

  val operate : input0 -> value option -> value option * value option

  val interpret :
    key0 -> input0 list -> value option -> value option * value option

  val inputs_with_key : (input0 * output0) list -> key0 -> input0 list
 end

type ('term, 'name, 'entry, 'logIndex, 'serverType, 'stateMachineData,
      'clientId, 'output) raft_data = { currentTerm : 'term;
                                        votedFor : 'name option;
                                        leaderId : 'name option;
                                        log : 'entry list;
                                        commitIndex : 'logIndex;
                                        lastApplied : 'logIndex;
                                        stateMachine : 'stateMachineData;
                                        nextIndex : ('name * 'logIndex) list;
                                        matchIndex : ('name * 'logIndex) list;
                                        shouldSend : bool;
                                        votesReceived : 'name list;
                                        type0 : 'serverType;
                                        clientCache : ('clientId * (int * 'output))
                                                      list;
                                        electoralVictories : (('term * 'name
                                                             list) * 'entry
                                                             list) list }

val set_raft_data_currentTerm :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a1 -> ('a1, 'a2,
  'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data

val set_raft_data_votedFor :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a2 option -> ('a1,
  'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data

val set_raft_data_leaderId :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a2 option -> ('a1,
  'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data

val set_raft_data_log :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a3 list -> ('a1,
  'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data

val set_raft_data_commitIndex :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a4 -> ('a1, 'a2,
  'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data

val set_raft_data_lastApplied :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a4 -> ('a1, 'a2,
  'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data

val set_raft_data_stateMachine :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a6 -> ('a1, 'a2,
  'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data

val set_raft_data_nextIndex :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> ('a2 * 'a4) list ->
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data

val set_raft_data_matchIndex :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> ('a2 * 'a4) list ->
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data

val set_raft_data_shouldSend :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> bool -> ('a1, 'a2,
  'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data

val set_raft_data_votesReceived :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a2 list -> ('a1,
  'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data

val set_raft_data_type :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a5 -> ('a1, 'a2,
  'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data

val set_raft_data_clientCache :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> ('a7 * (int * 'a8))
  list -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data

val set_raft_data_electoralVictories :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> (('a1 * 'a2
  list) * 'a3 list) list -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data

type raftParams = { n : int; input_eq_dec0 : (input -> input -> bool);
                    output_eq_dec0 : (output -> output -> bool);
                    clientId_eq_dec : (__ -> __ -> bool) }

type clientId = __

type term = int

type logIndex = int

type name = int

val nodes0 : baseParams -> raftParams -> name list

val name_eq_dec0 : baseParams -> raftParams -> name -> name -> bool

type entry = { eAt : name; eClient : clientId; eId : int; eIndex : logIndex;
               eTerm : term; eInput : input }

type msg =
| RequestVote of term * name * logIndex * term
| RequestVoteReply of term * bool
| AppendEntries of term * name * logIndex * term * entry list * logIndex
| AppendEntriesReply of term * entry list * bool

type raft_input =
| Timeout
| ClientRequest of clientId * int * input

type raft_output =
| NotLeader of clientId * int
| ClientResponse of clientId * int * output

type serverType =
| Follower
| Candidate
| Leader

val term_eq_dec : term -> term -> bool

val entry_eq_dec : baseParams -> raftParams -> entry -> entry -> bool

val msg_eq_dec0 : baseParams -> raftParams -> msg -> msg -> bool

type raft_data0 =
  (term, name, entry, logIndex, serverType, data, clientId, output) raft_data

val findAtIndex :
  baseParams -> raftParams -> entry list -> logIndex -> entry option

val findGtIndex :
  baseParams -> raftParams -> entry list -> logIndex -> entry list

val removeAfterIndex :
  baseParams -> raftParams -> entry list -> logIndex -> entry list

val maxIndex : baseParams -> raftParams -> entry list -> logIndex

val maxTerm : baseParams -> raftParams -> entry list -> term

val advanceCurrentTerm :
  baseParams -> raftParams -> (term, name, entry, logIndex, serverType, data,
  clientId, output) raft_data -> int -> (term, name, entry, logIndex,
  serverType, data, clientId, output) raft_data

val getNextIndex :
  baseParams -> raftParams -> (term, name, entry, logIndex, serverType, data,
  clientId, output) raft_data -> name -> logIndex

val tryToBecomeLeader :
  baseParams -> raftParams -> name -> raft_data0 -> (raft_output
  list * raft_data0) * (name * msg) list

val not_empty : 'a1 list -> bool

val haveNewEntries :
  baseParams -> raftParams -> raft_data0 -> entry list -> bool

val handleAppendEntries :
  baseParams -> raftParams -> name -> raft_data0 -> term -> name -> logIndex
  -> term -> entry list -> logIndex -> raft_data0 * msg

val handleAppendEntriesReply :
  baseParams -> raftParams -> name -> (term, name, entry, logIndex,
  serverType, data, clientId, output) raft_data -> name -> int -> entry list
  -> bool -> raft_data0 * (name * msg) list

val moreUpToDate : int -> int -> int -> int -> bool

val handleRequestVote :
  baseParams -> raftParams -> name -> (term, name, entry, logIndex,
  serverType, data, clientId, output) raft_data -> int -> int -> int -> int
  -> raft_data0 * msg

val div2 : int -> int

val wonElection : baseParams -> raftParams -> name list -> bool

val handleRequestVoteReply :
  baseParams -> raftParams -> name -> (term, name, entry, logIndex,
  serverType, data, clientId, output) raft_data -> name -> int -> bool ->
  raft_data0

val handleMessage :
  baseParams -> raftParams -> name -> name -> msg -> raft_data0 ->
  raft_data0 * (name * msg) list

val getLastId :
  baseParams -> raftParams -> (term, name, entry, logIndex, serverType, data,
  clientId, output) raft_data -> clientId -> (int * output) option

val applyEntry :
  baseParams -> oneNodeParams -> raftParams -> (term, name, entry, logIndex,
  serverType, data, clientId, output) raft_data -> entry -> output
  list * (term, name, entry, logIndex, serverType, data, clientId, output)
  raft_data

val cacheApplyEntry :
  baseParams -> oneNodeParams -> raftParams -> (term, name, entry, logIndex,
  serverType, data, clientId, output) raft_data -> entry -> output
  list * (term, name, entry, logIndex, serverType, data, clientId, output)
  raft_data

val applyEntries :
  baseParams -> oneNodeParams -> raftParams -> name -> raft_data0 -> entry
  list -> raft_output list * raft_data0

val doGenericServer :
  baseParams -> oneNodeParams -> raftParams -> name -> raft_data0 ->
  (raft_output list * raft_data0) * (name * msg) list

val replicaMessage :
  baseParams -> raftParams -> raft_data0 -> name -> name -> name * msg

val haveQuorum :
  baseParams -> raftParams -> raft_data0 -> name -> logIndex -> bool

val advanceCommitIndex :
  baseParams -> raftParams -> raft_data0 -> name -> raft_data0

val doLeader :
  baseParams -> raftParams -> raft_data0 -> name -> (raft_output
  list * raft_data0) * (name * msg) list

val raftNetHandler :
  baseParams -> oneNodeParams -> raftParams -> name -> name -> msg ->
  raft_data0 -> (raft_output list * raft_data0) * (name * msg) list

val handleClientRequest :
  baseParams -> raftParams -> name -> raft_data0 -> clientId -> int -> input
  -> (raft_output list * raft_data0) * (name * msg) list

val handleTimeout :
  baseParams -> raftParams -> name -> raft_data0 -> (raft_output
  list * raft_data0) * (name * msg) list

val handleInput :
  baseParams -> raftParams -> name -> raft_input -> raft_data0 ->
  (raft_output list * raft_data0) * (name * msg) list

val raftInputHandler :
  baseParams -> oneNodeParams -> raftParams -> name -> raft_input ->
  raft_data0 -> (raft_output list * raft_data0) * (name * msg) list

val reboot :
  baseParams -> raftParams -> (term, name, entry, logIndex, serverType, data,
  clientId, output) raft_data -> raft_data0

val init_handlers0 :
  baseParams -> oneNodeParams -> raftParams -> name -> raft_data0

val base_params : baseParams -> raftParams -> baseParams

val multi_params : baseParams -> oneNodeParams -> raftParams -> multiParams

val failure_params :
  baseParams -> oneNodeParams -> raftParams -> failureParams

type string0 = Pstring.t

val compare1 : string0 -> string0 -> comparison

val pstring_eq_dec : string0 -> string0 -> bool

val raft_params : int -> raftParams

val vard_raft_base_params : int -> baseParams

val vard_raft_multi_params : int -> multiParams

val vard_raft_failure_params : int -> failureParams
