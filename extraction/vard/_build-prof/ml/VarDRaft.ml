
type __ = Obj.t

(** val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option **)

let option_map f = function
| Some a -> Some (f a)
| None -> None

type comparison =
| Eq
| Lt
| Gt

(** val pred : int -> int **)

let pred = fun n -> Stdlib.max 0 (n-1)

module Coq__1 = struct
 (** val add : int -> int -> int **)

 let rec add = (+)
end
include Coq__1



module type DecidableTypeOrig =
 sig
  type t

  val eq_dec : t -> t -> bool
 end

module type EqLtLe =
 sig
  type t
 end

module MakeOrderTac =
 functor (O:EqLtLe) ->
 functor (P:sig
 end) ->
 struct
 end

module Nat =
 struct
 end

(** val list_eq_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool **)

let rec list_eq_dec eq_dec0 l l' =
  match l with
  | [] -> (match l' with
           | [] -> true
           | _ :: _ -> false)
  | y :: l0 ->
    (match l' with
     | [] -> false
     | a :: l1 -> if eq_dec0 y a then list_eq_dec eq_dec0 l0 l1 else false)

(** val seq : int -> int -> int list **)

let rec seq start len =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun len0 -> start :: (seq (Stdlib.Int.succ start) len0))
    len

type positive =
| XI of positive
| XO of positive
| XH

module Pos =
 struct
  (** val compare_cont : comparison -> positive -> positive -> comparison **)

  let rec compare_cont r x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> compare_cont r p q
       | XO q -> compare_cont Gt p q
       | XH -> Gt)
    | XO p ->
      (match y with
       | XI q -> compare_cont Lt p q
       | XO q -> compare_cont r p q
       | XH -> Gt)
    | XH -> (match y with
             | XH -> r
             | _ -> Lt)

  (** val compare : positive -> positive -> comparison **)

  let compare =
    compare_cont Eq
 end

(** val assoc :
    ('a1 -> 'a1 -> bool) -> ('a1 * 'a2) list -> 'a1 -> 'a2 option **)

let rec assoc k_eq_dec l k =
  match l with
  | [] -> None
  | p :: l' ->
    let (k', v) = p in if k_eq_dec k k' then Some v else assoc k_eq_dec l' k

(** val assoc_default :
    ('a1 -> 'a1 -> bool) -> ('a1 * 'a2) list -> 'a1 -> 'a2 -> 'a2 **)

let assoc_default k_eq_dec l k default =
  match assoc k_eq_dec l k with
  | Some x -> x
  | None -> default

(** val assoc_set :
    ('a1 -> 'a1 -> bool) -> ('a1 * 'a2) list -> 'a1 -> 'a2 -> ('a1 * 'a2) list **)

let rec assoc_set k_eq_dec l k v =
  match l with
  | [] -> (k, v) :: []
  | p :: l' ->
    let (k', v') = p in
    if k_eq_dec k k'
    then (k, v) :: l'
    else (k', v') :: (assoc_set k_eq_dec l' k v)

(** val dedup : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list **)

let rec dedup a_eq_dec = function
| [] -> []
| x :: xs0 ->
  let tail = dedup a_eq_dec xs0 in
  if (fun h -> List.mem) a_eq_dec x xs0 then tail else x :: tail

(** val filterMap : ('a1 -> 'a2 option) -> 'a1 list -> 'a2 list **)

let rec filterMap f = function
| [] -> []
| x :: xs ->
  (match f x with
   | Some y -> y :: (filterMap f xs)
   | None -> filterMap f xs)

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

module OrderedTypeFacts =
 functor (O:OrderedType) ->
 struct
  module TO =
   struct
    type t = O.t
   end

  module IsTO =
   struct
   end

  module OrderTac = MakeOrderTac(TO)(IsTO)

  (** val eq_dec : O.t -> O.t -> bool **)

  let eq_dec =
    O.eq_dec

  (** val lt_dec : O.t -> O.t -> bool **)

  let lt_dec x y =
    match O.compare x y with
    | LT -> true
    | _ -> false

  (** val eqb : O.t -> O.t -> bool **)

  let eqb x y =
    if eq_dec x y then true else false
 end

module KeyOrderedType =
 functor (O:OrderedType) ->
 struct
  module MO = OrderedTypeFacts(O)
 end

(** val string_dec : char list -> char list -> bool **)

let rec string_dec s x =
  match s with
  | [] -> (match x with
           | [] -> true
           | _::_ -> false)
  | a::s0 ->
    (match x with
     | [] -> false
     | a0::s1 -> if (=) a a0 then string_dec s0 s1 else false)

module PositiveOrderedTypeBits =
 struct
  type t = positive

  (** val compare : t -> t -> positive compare0 **)

  let rec compare p y =
    match p with
    | XI p0 -> (match y with
                | XI p1 -> compare p0 p1
                | _ -> GT)
    | XO p0 -> (match y with
                | XO p1 -> compare p0 p1
                | _ -> LT)
    | XH -> (match y with
             | XI _ -> LT
             | XO _ -> GT
             | XH -> EQ)

  (** val eq_dec : positive -> positive -> bool **)

  let eq_dec x y =
    match Pos.compare x y with
    | Eq -> true
    | _ -> false
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

(** val append : positive -> positive -> positive **)

let rec append i j =
  match i with
  | XI ii -> XI (append ii j)
  | XO ii -> XO (append ii j)
  | XH -> j

module PositiveMap =
 struct
  module E = PositiveOrderedTypeBits

  module ME = KeyOrderedType(E)

  type key = positive

  type 'a tree =
  | Leaf
  | Node of 'a tree * 'a option * 'a tree

  type 'a t = 'a tree

  (** val empty : 'a1 t **)

  let empty =
    Leaf

  (** val is_empty : 'a1 t -> bool **)

  let rec is_empty = function
  | Leaf -> true
  | Node (l, o, r) ->
    (match o with
     | Some _ -> false
     | None -> (&&) (is_empty l) (is_empty r))

  (** val find : key -> 'a1 t -> 'a1 option **)

  let rec find i = function
  | Leaf -> None
  | Node (l, o, r) ->
    (match i with
     | XI ii -> find ii r
     | XO ii -> find ii l
     | XH -> o)

  (** val mem : key -> 'a1 t -> bool **)

  let rec mem i = function
  | Leaf -> false
  | Node (l, o, r) ->
    (match i with
     | XI ii -> mem ii r
     | XO ii -> mem ii l
     | XH -> (match o with
              | Some _ -> true
              | None -> false))

  (** val add : key -> 'a1 -> 'a1 t -> 'a1 t **)

  let rec add i v = function
  | Leaf ->
    (match i with
     | XI ii -> Node (Leaf, None, (add ii v Leaf))
     | XO ii -> Node ((add ii v Leaf), None, Leaf)
     | XH -> Node (Leaf, (Some v), Leaf))
  | Node (l, o, r) ->
    (match i with
     | XI ii -> Node (l, o, (add ii v r))
     | XO ii -> Node ((add ii v l), o, r)
     | XH -> Node (l, (Some v), r))

  (** val remove : key -> 'a1 t -> 'a1 t **)

  let rec remove i m =
    match i with
    | XI ii ->
      (match m with
       | Leaf -> Leaf
       | Node (l, o, r) ->
         (match l with
          | Leaf ->
            (match o with
             | Some _ -> Node (l, o, (remove ii r))
             | None ->
               (match remove ii r with
                | Leaf -> Leaf
                | Node (t0, o0, t1) -> Node (Leaf, None, (Node (t0, o0, t1)))))
          | Node (_, _, _) -> Node (l, o, (remove ii r))))
    | XO ii ->
      (match m with
       | Leaf -> Leaf
       | Node (l, o, r) ->
         (match o with
          | Some _ -> Node ((remove ii l), o, r)
          | None ->
            (match r with
             | Leaf ->
               (match remove ii l with
                | Leaf -> Leaf
                | Node (t0, o0, t1) -> Node ((Node (t0, o0, t1)), None, Leaf))
             | Node (_, _, _) -> Node ((remove ii l), o, r))))
    | XH ->
      (match m with
       | Leaf -> Leaf
       | Node (l, _, r) ->
         (match l with
          | Leaf ->
            (match r with
             | Leaf -> Leaf
             | Node (_, _, _) -> Node (l, None, r))
          | Node (_, _, _) -> Node (l, None, r)))

  (** val xelements : 'a1 t -> key -> (key * 'a1) list **)

  let rec xelements m i =
    match m with
    | Leaf -> []
    | Node (l, o, r) ->
      (match o with
       | Some x ->
         List.append (xelements l (append i (XO XH))) ((i,
           x) :: (xelements r (append i (XI XH))))
       | None ->
         List.append (xelements l (append i (XO XH)))
           (xelements r (append i (XI XH))))

  (** val elements : 'a1 t -> (key * 'a1) list **)

  let elements m =
    xelements m XH

  (** val cardinal : 'a1 t -> int **)

  let rec cardinal = function
  | Leaf -> 0
  | Node (l, o, r) ->
    (match o with
     | Some _ -> Stdlib.Int.succ (Coq__1.add (cardinal l) (cardinal r))
     | None -> Coq__1.add (cardinal l) (cardinal r))

  (** val xfind : key -> key -> 'a1 t -> 'a1 option **)

  let rec xfind i j m =
    match i with
    | XI ii ->
      (match j with
       | XI jj -> xfind ii jj m
       | XO _ -> None
       | XH -> find i m)
    | XO ii ->
      (match j with
       | XI _ -> None
       | XO jj -> xfind ii jj m
       | XH -> find i m)
    | XH -> (match j with
             | XH -> find i m
             | _ -> None)

  (** val xmapi : (key -> 'a1 -> 'a2) -> 'a1 t -> key -> 'a2 t **)

  let rec xmapi f m i =
    match m with
    | Leaf -> Leaf
    | Node (l, o, r) ->
      Node ((xmapi f l (append i (XO XH))), (option_map (f i) o),
        (xmapi f r (append i (XI XH))))

  (** val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let mapi f m =
    xmapi f m XH

  (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map f m =
    mapi (fun _ -> f) m

  (** val xmap2_l :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t **)

  let rec xmap2_l f = function
  | Leaf -> Leaf
  | Node (l, o, r) -> Node ((xmap2_l f l), (f o None), (xmap2_l f r))

  (** val xmap2_r :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t **)

  let rec xmap2_r f = function
  | Leaf -> Leaf
  | Node (l, o, r) -> Node ((xmap2_r f l), (f None o), (xmap2_r f r))

  (** val _map2 :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t **)

  let rec _map2 f m1 m2 =
    match m1 with
    | Leaf -> xmap2_r f m2
    | Node (l1, o1, r1) ->
      (match m2 with
       | Leaf -> xmap2_l f m1
       | Node (l2, o2, r2) ->
         Node ((_map2 f l1 l2), (f o1 o2), (_map2 f r1 r2)))

  (** val map2 :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t **)

  let map2 f =
    _map2 (fun o1 o2 ->
      match o1 with
      | Some _ -> f o1 o2
      | None -> (match o2 with
                 | Some _ -> f o1 o2
                 | None -> None))

  (** val xfoldi :
      (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> key -> 'a2 **)

  let rec xfoldi f m v i =
    match m with
    | Leaf -> v
    | Node (l, o, r) ->
      (match o with
       | Some x ->
         xfoldi f r (f i x (xfoldi f l v (append i (XO XH))))
           (append i (XI XH))
       | None ->
         xfoldi f r (xfoldi f l v (append i (XO XH))) (append i (XI XH)))

  (** val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 **)

  let fold f m i =
    xfoldi f m i XH

  (** val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool **)

  let rec equal cmp m1 m2 =
    match m1 with
    | Leaf -> is_empty m2
    | Node (l1, o1, r1) ->
      (match m2 with
       | Leaf -> is_empty m1
       | Node (l2, o2, r2) ->
         (&&)
           ((&&)
             (match o1 with
              | Some v1 ->
                (match o2 with
                 | Some v2 -> cmp v1 v2
                 | None -> false)
              | None -> (match o2 with
                         | Some _ -> false
                         | None -> true)) (equal cmp l1 l2)) (equal cmp r1 r2))
 end

module WFacts_fun =
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
 struct
  (** val eqb : E.t -> E.t -> bool **)

  let eqb x y =
    if E.eq_dec x y then true else false

  (** val coq_In_dec : 'a1 M.t -> M.key -> bool **)

  let coq_In_dec m x =
    let b = M.mem x m in if b then true else false
 end

module WFacts =
 functor (M:WS) ->
 WFacts_fun(M.E)(M)

module Facts = WFacts

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

module IT_to_DT =
 functor (I:IndexedType) ->
 struct
  type t = I.t

  (** val eq_dec : I.t -> I.t -> bool **)

  let eq_dec =
    I.eq
 end

module IndexedPositiveMap =
 functor (X:IndexedType) ->
 struct
  module E = IT_to_DT(X)

  module M = PositiveMap

  module F = Facts(M)

  type key = X.t

  type 'a t = 'a M.t

  (** val empty : 'a1 M.t **)

  let empty =
    M.empty

  (** val add : key -> 'a1 -> 'a1 t -> 'a1 t **)

  let add i v m =
    M.add (X.index i) v m

  (** val find : key -> 'a1 t -> 'a1 option **)

  let find i m =
    M.find (X.index i) m

  (** val remove : key -> 'a1 t -> 'a1 t **)

  let remove i m =
    M.remove (X.index i) m
 end

module IndexedString =
 struct
  type t = char list

  (** val eq : char list -> char list -> bool **)

  let eq =
    string_dec

  (** val positive_of_digits : bool list -> positive -> positive **)

  let rec positive_of_digits l p =
    match l with
    | [] -> p
    | b :: l' ->
      if b then XI (positive_of_digits l' p) else XO (positive_of_digits l' p)

  (** val list_bool_of_ascii : char -> bool list **)

  let list_bool_of_ascii a =
    (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
      (fun a0 a1 a2 a3 a4 a5 a6 a7 ->
      a0 :: (a1 :: (a2 :: (a3 :: (a4 :: (a5 :: (a6 :: (a7 :: []))))))))
      a

  (** val index : char list -> positive **)

  let rec index = function
  | [] -> XH
  | a::s' -> positive_of_digits (list_bool_of_ascii a) (index s')
 end

module LogTimeStringMap = IndexedPositiveMap(IndexedString)

type ('s, 'a) genHandler1 = 's -> 'a * 's

(** val ret : 'a2 -> ('a1, 'a2) genHandler1 **)

let ret a s =
  (a, s)

(** val bind :
    ('a1, 'a2) genHandler1 -> ('a2 -> ('a1, 'a3) genHandler1) -> ('a1, 'a3)
    genHandler1 **)

let bind m f s =
  let (a, s') = m s in f a s'

(** val write_output : 'a2 -> ('a1, 'a2) genHandler1 **)

let write_output =
  ret

(** val modify : ('a1 -> 'a1) -> ('a1, unit) genHandler1 **)

let modify f s =
  ((), (f s))

(** val get : ('a1, 'a1) genHandler1 **)

let get s =
  (s, s)

(** val runGenHandler1 : 'a1 -> ('a1, 'a2) genHandler1 -> 'a2 * 'a1 **)

let runGenHandler1 s h =
  h s

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

module VarDFunctor =
 functor (Map:VWS with type E.t = char list) ->
 struct
  (** val key_eq_dec : char list -> char list -> bool **)

  let key_eq_dec =
    string_dec

  (** val value_eq_dec : char list -> char list -> bool **)

  let value_eq_dec =
    string_dec

  (** val val_eq_dec : value option -> value option -> bool **)

  let val_eq_dec v v' =
    match v with
    | Some a -> (match v' with
                 | Some v0 -> value_eq_dec a v0
                 | None -> false)
    | None -> (match v' with
               | Some _ -> false
               | None -> true)

  (** val input_eq_dec : input0 -> input0 -> bool **)

  let input_eq_dec x y =
    match x with
    | Put (k, v) ->
      (match y with
       | Put (k0, v0) ->
         if value_eq_dec k k0 then value_eq_dec v v0 else false
       | _ -> false)
    | Get k -> (match y with
                | Get k0 -> value_eq_dec k k0
                | _ -> false)
    | Del k -> (match y with
                | Del k0 -> value_eq_dec k k0
                | _ -> false)
    | CAS (k, o, v) ->
      (match y with
       | CAS (k0, o0, v0) ->
         if value_eq_dec k k0
         then if val_eq_dec o o0 then value_eq_dec v v0 else false
         else false
       | _ -> false)
    | CAD (k, v) ->
      (match y with
       | CAD (k0, v0) ->
         if value_eq_dec k k0 then value_eq_dec v v0 else false
       | _ -> false)

  (** val output_eq_dec : output0 -> output0 -> bool **)

  let output_eq_dec x y =
    let Response (k, o, o0) = x in
    let Response (k0, o1, o2) = y in
    if value_eq_dec k k0
    then if val_eq_dec o o1 then val_eq_dec o0 o2 else false
    else false

  type data = char list Map.t

  (** val beq_key : key0 -> key0 -> bool **)

  let beq_key k1 k2 =
    if string_dec k1 k2 then true else false

  (** val getk : Map.key -> (data, value option) genHandler1 **)

  let getk k =
    bind get (fun db -> ret (Map.find k db))

  (** val setk : Map.key -> char list -> (data, unit) genHandler1 **)

  let setk k v =
    modify (fun db -> Map.add k v db)

  (** val delk : Map.key -> (data, unit) genHandler1 **)

  let delk k =
    modify (fun db -> Map.remove k db)

  (** val resp :
      key0 -> value option -> value option -> (data, output0) genHandler1 **)

  let resp k v old =
    write_output (Response (k, v, old))

  (** val coq_VarDHandler' : input0 -> (data, output0) genHandler1 **)

  let coq_VarDHandler' = function
  | Put (k, v) ->
    bind (getk k) (fun old -> bind (setk k v) (fun _ -> resp k (Some v) old))
  | Get k -> bind (getk k) (fun v -> resp k v v)
  | Del k ->
    bind (getk k) (fun old -> bind (delk k) (fun _ -> resp k None old))
  | CAS (k, v, v') ->
    bind (getk k) (fun old ->
      if val_eq_dec old v
      then bind (setk k v') (fun _ -> resp k (Some v') old)
      else resp k old old)
  | CAD (k, v) ->
    bind (getk k) (fun old ->
      if val_eq_dec old (Some v)
      then bind (delk k) (fun _ -> resp k None old)
      else resp k old old)

  (** val runHandler :
      (input0 -> (data, output0) genHandler1) -> input0 -> data ->
      output0 * data **)

  let runHandler h inp d =
    runGenHandler1 d (h inp)

  (** val coq_VarDHandler : input0 -> data -> output0 * data **)

  let coq_VarDHandler =
    runHandler coq_VarDHandler'

  (** val init_map : char list Map.t **)

  let init_map =
    Map.empty

  (** val vard_base_params : baseParams **)

  let vard_base_params =
    Build_BaseParams

  (** val vard_one_node_params : oneNodeParams **)

  let vard_one_node_params =
    { init = (Obj.magic init_map); handler = (Obj.magic coq_VarDHandler) }

  (** val input_key : input0 -> key0 **)

  let input_key = function
  | Put (k, _) -> k
  | Get k -> k
  | Del k -> k
  | CAS (k, _, _) -> k
  | CAD (k, _) -> k

  (** val operate : input0 -> value option -> value option * value option **)

  let operate op curr =
    match op with
    | Put (_, v) -> ((Some v), curr)
    | Get _ -> (curr, curr)
    | Del _ -> (None, curr)
    | CAS (_, v, v') ->
      if val_eq_dec curr v then ((Some v'), curr) else (curr, curr)
    | CAD (_, v) ->
      if val_eq_dec curr (Some v) then (None, curr) else (curr, curr)

  (** val interpret :
      key0 -> input0 list -> value option -> value option * value option **)

  let rec interpret k ops init0 =
    match ops with
    | [] -> (init0, init0)
    | op :: ops0 -> operate op (fst (interpret k ops0 init0))

  (** val inputs_with_key : (input0 * output0) list -> key0 -> input0 list **)

  let inputs_with_key trace k =
    filterMap (fun ev ->
      if key_eq_dec k (input_key (fst ev)) then Some (fst ev) else None) trace
 end

module LogTimeVarD = VarDFunctor(LogTimeStringMap)

module VarD = LogTimeVarD

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

(** val set_raft_data_currentTerm :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a1 -> ('a1, 'a2,
    'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data **)

let set_raft_data_currentTerm a v =
  { currentTerm = v; votedFor = a.votedFor; leaderId = a.leaderId; log =
    a.log; commitIndex = a.commitIndex; lastApplied = a.lastApplied;
    stateMachine = a.stateMachine; nextIndex = a.nextIndex; matchIndex =
    a.matchIndex; shouldSend = a.shouldSend; votesReceived = a.votesReceived;
    type0 = a.type0; clientCache = a.clientCache; electoralVictories =
    a.electoralVictories }

(** val set_raft_data_votedFor :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a2 option -> ('a1,
    'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data **)

let set_raft_data_votedFor a v =
  { currentTerm = a.currentTerm; votedFor = v; leaderId = a.leaderId; log =
    a.log; commitIndex = a.commitIndex; lastApplied = a.lastApplied;
    stateMachine = a.stateMachine; nextIndex = a.nextIndex; matchIndex =
    a.matchIndex; shouldSend = a.shouldSend; votesReceived = a.votesReceived;
    type0 = a.type0; clientCache = a.clientCache; electoralVictories =
    a.electoralVictories }

(** val set_raft_data_leaderId :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a2 option -> ('a1,
    'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data **)

let set_raft_data_leaderId a v =
  { currentTerm = a.currentTerm; votedFor = a.votedFor; leaderId = v; log =
    a.log; commitIndex = a.commitIndex; lastApplied = a.lastApplied;
    stateMachine = a.stateMachine; nextIndex = a.nextIndex; matchIndex =
    a.matchIndex; shouldSend = a.shouldSend; votesReceived = a.votesReceived;
    type0 = a.type0; clientCache = a.clientCache; electoralVictories =
    a.electoralVictories }

(** val set_raft_data_log :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a3 list -> ('a1,
    'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data **)

let set_raft_data_log a v =
  { currentTerm = a.currentTerm; votedFor = a.votedFor; leaderId =
    a.leaderId; log = v; commitIndex = a.commitIndex; lastApplied =
    a.lastApplied; stateMachine = a.stateMachine; nextIndex = a.nextIndex;
    matchIndex = a.matchIndex; shouldSend = a.shouldSend; votesReceived =
    a.votesReceived; type0 = a.type0; clientCache = a.clientCache;
    electoralVictories = a.electoralVictories }

(** val set_raft_data_commitIndex :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a4 -> ('a1, 'a2,
    'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data **)

let set_raft_data_commitIndex a v =
  { currentTerm = a.currentTerm; votedFor = a.votedFor; leaderId =
    a.leaderId; log = a.log; commitIndex = v; lastApplied = a.lastApplied;
    stateMachine = a.stateMachine; nextIndex = a.nextIndex; matchIndex =
    a.matchIndex; shouldSend = a.shouldSend; votesReceived = a.votesReceived;
    type0 = a.type0; clientCache = a.clientCache; electoralVictories =
    a.electoralVictories }

(** val set_raft_data_lastApplied :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a4 -> ('a1, 'a2,
    'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data **)

let set_raft_data_lastApplied a v =
  { currentTerm = a.currentTerm; votedFor = a.votedFor; leaderId =
    a.leaderId; log = a.log; commitIndex = a.commitIndex; lastApplied = v;
    stateMachine = a.stateMachine; nextIndex = a.nextIndex; matchIndex =
    a.matchIndex; shouldSend = a.shouldSend; votesReceived = a.votesReceived;
    type0 = a.type0; clientCache = a.clientCache; electoralVictories =
    a.electoralVictories }

(** val set_raft_data_stateMachine :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a6 -> ('a1, 'a2,
    'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data **)

let set_raft_data_stateMachine a v =
  { currentTerm = a.currentTerm; votedFor = a.votedFor; leaderId =
    a.leaderId; log = a.log; commitIndex = a.commitIndex; lastApplied =
    a.lastApplied; stateMachine = v; nextIndex = a.nextIndex; matchIndex =
    a.matchIndex; shouldSend = a.shouldSend; votesReceived = a.votesReceived;
    type0 = a.type0; clientCache = a.clientCache; electoralVictories =
    a.electoralVictories }

(** val set_raft_data_nextIndex :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> ('a2 * 'a4) list ->
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data **)

let set_raft_data_nextIndex a v =
  { currentTerm = a.currentTerm; votedFor = a.votedFor; leaderId =
    a.leaderId; log = a.log; commitIndex = a.commitIndex; lastApplied =
    a.lastApplied; stateMachine = a.stateMachine; nextIndex = v; matchIndex =
    a.matchIndex; shouldSend = a.shouldSend; votesReceived = a.votesReceived;
    type0 = a.type0; clientCache = a.clientCache; electoralVictories =
    a.electoralVictories }

(** val set_raft_data_matchIndex :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> ('a2 * 'a4) list ->
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data **)

let set_raft_data_matchIndex a v =
  { currentTerm = a.currentTerm; votedFor = a.votedFor; leaderId =
    a.leaderId; log = a.log; commitIndex = a.commitIndex; lastApplied =
    a.lastApplied; stateMachine = a.stateMachine; nextIndex = a.nextIndex;
    matchIndex = v; shouldSend = a.shouldSend; votesReceived =
    a.votesReceived; type0 = a.type0; clientCache = a.clientCache;
    electoralVictories = a.electoralVictories }

(** val set_raft_data_shouldSend :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> bool -> ('a1, 'a2,
    'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data **)

let set_raft_data_shouldSend a v =
  { currentTerm = a.currentTerm; votedFor = a.votedFor; leaderId =
    a.leaderId; log = a.log; commitIndex = a.commitIndex; lastApplied =
    a.lastApplied; stateMachine = a.stateMachine; nextIndex = a.nextIndex;
    matchIndex = a.matchIndex; shouldSend = v; votesReceived =
    a.votesReceived; type0 = a.type0; clientCache = a.clientCache;
    electoralVictories = a.electoralVictories }

(** val set_raft_data_votesReceived :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a2 list -> ('a1,
    'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data **)

let set_raft_data_votesReceived a v =
  { currentTerm = a.currentTerm; votedFor = a.votedFor; leaderId =
    a.leaderId; log = a.log; commitIndex = a.commitIndex; lastApplied =
    a.lastApplied; stateMachine = a.stateMachine; nextIndex = a.nextIndex;
    matchIndex = a.matchIndex; shouldSend = a.shouldSend; votesReceived = v;
    type0 = a.type0; clientCache = a.clientCache; electoralVictories =
    a.electoralVictories }

(** val set_raft_data_type :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> 'a5 -> ('a1, 'a2,
    'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data **)

let set_raft_data_type a v =
  { currentTerm = a.currentTerm; votedFor = a.votedFor; leaderId =
    a.leaderId; log = a.log; commitIndex = a.commitIndex; lastApplied =
    a.lastApplied; stateMachine = a.stateMachine; nextIndex = a.nextIndex;
    matchIndex = a.matchIndex; shouldSend = a.shouldSend; votesReceived =
    a.votesReceived; type0 = v; clientCache = a.clientCache;
    electoralVictories = a.electoralVictories }

(** val set_raft_data_clientCache :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> ('a7 * (int * 'a8))
    list -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data **)

let set_raft_data_clientCache a v =
  { currentTerm = a.currentTerm; votedFor = a.votedFor; leaderId =
    a.leaderId; log = a.log; commitIndex = a.commitIndex; lastApplied =
    a.lastApplied; stateMachine = a.stateMachine; nextIndex = a.nextIndex;
    matchIndex = a.matchIndex; shouldSend = a.shouldSend; votesReceived =
    a.votesReceived; type0 = a.type0; clientCache = v; electoralVictories =
    a.electoralVictories }

(** val set_raft_data_electoralVictories :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) raft_data -> (('a1 * 'a2
    list) * 'a3 list) list -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8)
    raft_data **)

let set_raft_data_electoralVictories a v =
  { currentTerm = a.currentTerm; votedFor = a.votedFor; leaderId =
    a.leaderId; log = a.log; commitIndex = a.commitIndex; lastApplied =
    a.lastApplied; stateMachine = a.stateMachine; nextIndex = a.nextIndex;
    matchIndex = a.matchIndex; shouldSend = a.shouldSend; votesReceived =
    a.votesReceived; type0 = a.type0; clientCache = a.clientCache;
    electoralVictories = v }

type raftParams = { n : int; input_eq_dec0 : (input -> input -> bool);
                    output_eq_dec0 : (output -> output -> bool);
                    clientId_eq_dec : (__ -> __ -> bool) }

type clientId = __

type term = int

type logIndex = int

type name = int

(** val nodes0 : baseParams -> raftParams -> name list **)

let nodes0 _ raft_params0 =
  (fun n -> (Obj.magic (seq 0 n))) raft_params0.n

(** val name_eq_dec0 : baseParams -> raftParams -> name -> name -> bool **)

let name_eq_dec0 _ raft_params0 =
  (fun _ -> (=)) raft_params0.n

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

(** val term_eq_dec : term -> term -> bool **)

let term_eq_dec =
  (=)

(** val entry_eq_dec : baseParams -> raftParams -> entry -> entry -> bool **)

let entry_eq_dec orig_base_params raft_params0 x y =
  let { eAt = eAt0; eClient = eClient0; eId = eId0; eIndex = eIndex0; eTerm =
    eTerm0; eInput = eInput0 } = x
  in
  let { eAt = eAt1; eClient = eClient1; eId = eId1; eIndex = eIndex1; eTerm =
    eTerm1; eInput = eInput1 } = y
  in
  if name_eq_dec0 orig_base_params raft_params0 eAt0 eAt1
  then if raft_params0.clientId_eq_dec eClient0 eClient1
       then if term_eq_dec eId0 eId1
            then if term_eq_dec eIndex0 eIndex1
                 then if term_eq_dec eTerm0 eTerm1
                      then raft_params0.input_eq_dec0 eInput0 eInput1
                      else false
                 else false
            else false
       else false
  else false

(** val msg_eq_dec0 : baseParams -> raftParams -> msg -> msg -> bool **)

let msg_eq_dec0 orig_base_params raft_params0 x y =
  match x with
  | RequestVote (t0, n0, l, t1) ->
    (match y with
     | RequestVote (t2, n1, l0, t3) ->
       if term_eq_dec t0 t2
       then if name_eq_dec0 orig_base_params raft_params0 n0 n1
            then if term_eq_dec l l0 then term_eq_dec t1 t3 else false
            else false
       else false
     | _ -> false)
  | RequestVoteReply (t0, b) ->
    (match y with
     | RequestVoteReply (t1, b0) ->
       if term_eq_dec t0 t1 then (=) b b0 else false
     | _ -> false)
  | AppendEntries (t0, n0, l, t1, l0, l1) ->
    (match y with
     | AppendEntries (t2, n1, l2, t3, l3, l4) ->
       if term_eq_dec t0 t2
       then if name_eq_dec0 orig_base_params raft_params0 n0 n1
            then if term_eq_dec l l2
                 then if term_eq_dec t1 t3
                      then if list_eq_dec (fun x0 y0 ->
                                entry_eq_dec orig_base_params raft_params0 x0
                                  y0) l0 l3
                           then term_eq_dec l1 l4
                           else false
                      else false
                 else false
            else false
       else false
     | _ -> false)
  | AppendEntriesReply (t0, l, b) ->
    (match y with
     | AppendEntriesReply (t1, l0, b0) ->
       if term_eq_dec t0 t1
       then if list_eq_dec (fun x0 y0 ->
                 entry_eq_dec orig_base_params raft_params0 x0 y0) l l0
            then (=) b b0
            else false
       else false
     | _ -> false)

type raft_data0 =
  (term, name, entry, logIndex, serverType, data, clientId, output) raft_data

(** val findAtIndex :
    baseParams -> raftParams -> entry list -> logIndex -> entry option **)

let rec findAtIndex orig_base_params raft_params0 entries i =
  match entries with
  | [] -> None
  | e :: es ->
    if (=) e.eIndex i
    then Some e
    else if (<) e.eIndex i
         then None
         else findAtIndex orig_base_params raft_params0 es i

(** val findGtIndex :
    baseParams -> raftParams -> entry list -> logIndex -> entry list **)

let rec findGtIndex orig_base_params raft_params0 entries i =
  match entries with
  | [] -> []
  | e :: es ->
    if (<) i e.eIndex
    then e :: (findGtIndex orig_base_params raft_params0 es i)
    else []

(** val removeAfterIndex :
    baseParams -> raftParams -> entry list -> logIndex -> entry list **)

let rec removeAfterIndex orig_base_params raft_params0 entries i =
  match entries with
  | [] -> []
  | e :: es ->
    if (<=) e.eIndex i
    then e :: es
    else removeAfterIndex orig_base_params raft_params0 es i

(** val maxIndex : baseParams -> raftParams -> entry list -> logIndex **)

let maxIndex _ _ = function
| [] -> 0
| e :: _ -> e.eIndex

(** val maxTerm : baseParams -> raftParams -> entry list -> term **)

let maxTerm _ _ = function
| [] -> 0
| e :: _ -> e.eTerm

(** val advanceCurrentTerm :
    baseParams -> raftParams -> (term, name, entry, logIndex, serverType,
    data, clientId, output) raft_data -> int -> (term, name, entry, logIndex,
    serverType, data, clientId, output) raft_data **)

let advanceCurrentTerm _ _ state newTerm =
  if (<) state.currentTerm newTerm
  then set_raft_data_leaderId
         (set_raft_data_type
           (set_raft_data_votedFor (set_raft_data_currentTerm state newTerm)
             None) Follower) None
  else state

(** val getNextIndex :
    baseParams -> raftParams -> (term, name, entry, logIndex, serverType,
    data, clientId, output) raft_data -> name -> logIndex **)

let getNextIndex orig_base_params raft_params0 state h =
  assoc_default (name_eq_dec0 orig_base_params raft_params0) state.nextIndex
    h (maxIndex orig_base_params raft_params0 state.log)

(** val tryToBecomeLeader :
    baseParams -> raftParams -> name -> raft_data0 -> (raft_output
    list * raft_data0) * (name * msg) list **)

let tryToBecomeLeader orig_base_params raft_params0 me state =
  let t0 = Stdlib.Int.succ state.currentTerm in
  (([],
  (set_raft_data_currentTerm
    (set_raft_data_votesReceived
      (set_raft_data_votedFor (set_raft_data_type state Candidate) (Some me))
      (me :: [])) t0)),
  (List.map (fun node -> (node, (RequestVote (t0, me,
    (maxIndex orig_base_params raft_params0 state.log),
    (maxTerm orig_base_params raft_params0 state.log)))))
    (List.filter (fun h ->
      if name_eq_dec0 orig_base_params raft_params0 me h then false else true)
      (nodes0 orig_base_params raft_params0))))

(** val not_empty : 'a1 list -> bool **)

let not_empty = function
| [] -> false
| _ :: _ -> true

(** val haveNewEntries :
    baseParams -> raftParams -> raft_data0 -> entry list -> bool **)

let haveNewEntries orig_base_params raft_params0 state entries =
  (&&) (not_empty entries)
    (match findAtIndex orig_base_params raft_params0 state.log
             (maxIndex orig_base_params raft_params0 entries) with
     | Some e ->
       not ((=) (maxTerm orig_base_params raft_params0 entries) e.eTerm)
     | None -> true)

(** val handleAppendEntries :
    baseParams -> raftParams -> name -> raft_data0 -> term -> name ->
    logIndex -> term -> entry list -> logIndex -> raft_data0 * msg **)

let handleAppendEntries orig_base_params raft_params0 _ state t0 leaderId0 prevLogIndex prevLogTerm entries leaderCommit =
  if (<) t0 state.currentTerm
  then (state, (AppendEntriesReply (state.currentTerm, entries, false)))
  else if (=) prevLogIndex 0
       then if haveNewEntries orig_base_params raft_params0 state entries
            then ((set_raft_data_leaderId
                    (set_raft_data_type
                      (set_raft_data_commitIndex
                        (set_raft_data_log
                          (advanceCurrentTerm orig_base_params raft_params0
                            state t0) entries)
                        (Stdlib.max state.commitIndex
                          (Stdlib.min leaderCommit
                            (maxIndex orig_base_params raft_params0 entries))))
                      Follower) (Some leaderId0)), (AppendEntriesReply (t0,
                   entries, true)))
            else ((set_raft_data_leaderId
                    (set_raft_data_type
                      (advanceCurrentTerm orig_base_params raft_params0 state
                        t0) Follower) (Some leaderId0)), (AppendEntriesReply
                   (t0, entries, true)))
       else (match findAtIndex orig_base_params raft_params0 state.log
                     prevLogIndex with
             | Some e ->
               if not ((=) prevLogTerm e.eTerm)
               then (state, (AppendEntriesReply (state.currentTerm, entries,
                      false)))
               else if haveNewEntries orig_base_params raft_params0 state
                         entries
                    then let log' =
                           removeAfterIndex orig_base_params raft_params0
                             state.log prevLogIndex
                         in
                         let log'' = List.append entries log' in
                         ((set_raft_data_leaderId
                            (set_raft_data_type
                              (set_raft_data_commitIndex
                                (set_raft_data_log
                                  (advanceCurrentTerm orig_base_params
                                    raft_params0 state t0) log'')
                                (Stdlib.max state.commitIndex
                                  (Stdlib.min leaderCommit
                                    (maxIndex orig_base_params raft_params0
                                      log'')))) Follower) (Some leaderId0)),
                         (AppendEntriesReply (t0, entries, true)))
                    else ((set_raft_data_leaderId
                            (set_raft_data_type
                              (advanceCurrentTerm orig_base_params
                                raft_params0 state t0) Follower) (Some
                            leaderId0)), (AppendEntriesReply (t0, entries,
                           true)))
             | None ->
               (state, (AppendEntriesReply (state.currentTerm, entries,
                 false))))

(** val handleAppendEntriesReply :
    baseParams -> raftParams -> name -> (term, name, entry, logIndex,
    serverType, data, clientId, output) raft_data -> name -> int -> entry
    list -> bool -> raft_data0 * (name * msg) list **)

let handleAppendEntriesReply orig_base_params raft_params0 _ state src term0 entries result =
  if (=) state.currentTerm term0
  then if result
       then let index0 = maxIndex orig_base_params raft_params0 entries in
            ((set_raft_data_nextIndex
               (set_raft_data_matchIndex state
                 (assoc_set (name_eq_dec0 orig_base_params raft_params0)
                   state.matchIndex src
                   (Stdlib.max
                     (assoc_default
                       (name_eq_dec0 orig_base_params raft_params0)
                       state.matchIndex src 0) index0)))
               (assoc_set (name_eq_dec0 orig_base_params raft_params0)
                 state.nextIndex src
                 (Stdlib.max
                   (getNextIndex orig_base_params raft_params0 state src)
                   (Stdlib.Int.succ index0)))), [])
       else ((set_raft_data_nextIndex state
               (assoc_set (name_eq_dec0 orig_base_params raft_params0)
                 state.nextIndex src
                 (pred (getNextIndex orig_base_params raft_params0 state src)))),
              [])
  else if (<) state.currentTerm term0
       then ((advanceCurrentTerm orig_base_params raft_params0 state term0),
              [])
       else (state, [])

(** val moreUpToDate : int -> int -> int -> int -> bool **)

let moreUpToDate t1 i1 t2 i2 =
  (||) ((<) t2 t1) ((&&) ((=) t1 t2) ((<=) i2 i1))

(** val handleRequestVote :
    baseParams -> raftParams -> name -> (term, name, entry, logIndex,
    serverType, data, clientId, output) raft_data -> int -> int -> int -> int
    -> raft_data0 * msg **)

let handleRequestVote orig_base_params raft_params0 _ state t0 candidateId lastLogIndex lastLogTerm =
  if (<) t0 state.currentTerm
  then (state, (RequestVoteReply (state.currentTerm, false)))
  else let state0 = advanceCurrentTerm orig_base_params raft_params0 state t0
       in
       if (&&) (match state0.leaderId with
                | Some _ -> false
                | None -> true)
            (moreUpToDate lastLogTerm lastLogIndex
              (maxTerm orig_base_params raft_params0 state0.log)
              (maxIndex orig_base_params raft_params0 state0.log))
       then (match state0.votedFor with
             | Some candidateId' ->
               (state0, (RequestVoteReply (state0.currentTerm,
                 (if (fun _ -> (=)) raft_params0.n candidateId candidateId'
                  then true
                  else false))))
             | None ->
               ((set_raft_data_votedFor state0 (Some candidateId)),
                 (RequestVoteReply (state0.currentTerm, true))))
       else (state0, (RequestVoteReply (state0.currentTerm, false)))

(** val div2 : int -> int **)

let rec div2 a =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> 0)
    (fun n0 ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> 0)
      (fun n1 -> Stdlib.Int.succ (div2 n1))
      n0)
    a

(** val wonElection : baseParams -> raftParams -> name list -> bool **)

let wonElection orig_base_params raft_params0 votes =
  (<=) (Stdlib.Int.succ
    (div2 (List.length (nodes0 orig_base_params raft_params0))))
    (List.length votes)

(** val handleRequestVoteReply :
    baseParams -> raftParams -> name -> (term, name, entry, logIndex,
    serverType, data, clientId, output) raft_data -> name -> int -> bool ->
    raft_data0 **)

let handleRequestVoteReply orig_base_params raft_params0 me state src t0 voteGranted =
  if (<) state.currentTerm t0
  then set_raft_data_type
         (advanceCurrentTerm orig_base_params raft_params0 state t0) Follower
  else if (<) t0 state.currentTerm
       then state
       else let won =
              (&&) voteGranted
                (wonElection orig_base_params raft_params0
                  (dedup (name_eq_dec0 orig_base_params raft_params0)
                    (src :: state.votesReceived)))
            in
            (match state.type0 with
             | Candidate ->
               set_raft_data_electoralVictories
                 (set_raft_data_nextIndex
                   (set_raft_data_matchIndex
                     (set_raft_data_type
                       (set_raft_data_votesReceived state
                         (List.append (if voteGranted then src :: [] else [])
                           state.votesReceived))
                       (if won then Leader else state.type0))
                     (assoc_set (name_eq_dec0 orig_base_params raft_params0)
                       [] me
                       (maxIndex orig_base_params raft_params0 state.log)))
                   [])
                 (List.append
                   (if won
                    then ((state.currentTerm, (src :: state.votesReceived)),
                           state.log) :: []
                    else []) state.electoralVictories)
             | _ -> state)

(** val handleMessage :
    baseParams -> raftParams -> name -> name -> msg -> raft_data0 ->
    raft_data0 * (name * msg) list **)

let handleMessage orig_base_params raft_params0 src me m state =
  match m with
  | RequestVote (t0, _, lastLogIndex, lastLogTerm) ->
    let (st, r) =
      handleRequestVote orig_base_params raft_params0 me state t0 src
        lastLogIndex lastLogTerm
    in
    (st, ((src, r) :: []))
  | RequestVoteReply (t0, voteGranted) ->
    ((handleRequestVoteReply orig_base_params raft_params0 me state src t0
       voteGranted), [])
  | AppendEntries (t0, lid, prevLogIndex, prevLogTerm, entries, leaderCommit) ->
    let (st, r) =
      handleAppendEntries orig_base_params raft_params0 me state t0 lid
        prevLogIndex prevLogTerm entries leaderCommit
    in
    (st, ((src, r) :: []))
  | AppendEntriesReply (term0, entries, result) ->
    handleAppendEntriesReply orig_base_params raft_params0 me state src term0
      entries result

(** val getLastId :
    baseParams -> raftParams -> (term, name, entry, logIndex, serverType,
    data, clientId, output) raft_data -> clientId -> (int * output) option **)

let getLastId _ raft_params0 state client =
  assoc raft_params0.clientId_eq_dec state.clientCache client

(** val applyEntry :
    baseParams -> oneNodeParams -> raftParams -> (term, name, entry,
    logIndex, serverType, data, clientId, output) raft_data -> entry ->
    output list * (term, name, entry, logIndex, serverType, data, clientId,
    output) raft_data **)

let applyEntry _ one_node_params raft_params0 st e =
  let (out, d) = one_node_params.handler e.eInput st.stateMachine in
  ((out :: []),
  (set_raft_data_stateMachine
    (set_raft_data_clientCache st
      (assoc_set raft_params0.clientId_eq_dec st.clientCache e.eClient
        (e.eId, out))) d))

(** val cacheApplyEntry :
    baseParams -> oneNodeParams -> raftParams -> (term, name, entry,
    logIndex, serverType, data, clientId, output) raft_data -> entry ->
    output list * (term, name, entry, logIndex, serverType, data, clientId,
    output) raft_data **)

let cacheApplyEntry orig_base_params one_node_params raft_params0 st e =
  match getLastId orig_base_params raft_params0 st e.eClient with
  | Some p ->
    let (id, o) = p in
    if (<) e.eId id
    then ([], st)
    else if (=) e.eId id
         then ((o :: []), st)
         else applyEntry orig_base_params one_node_params raft_params0 st e
  | None -> applyEntry orig_base_params one_node_params raft_params0 st e

(** val applyEntries :
    baseParams -> oneNodeParams -> raftParams -> name -> raft_data0 -> entry
    list -> raft_output list * raft_data0 **)

let rec applyEntries orig_base_params one_node_params raft_params0 h st = function
| [] -> ([], st)
| e :: es ->
  let (out, st0) =
    cacheApplyEntry orig_base_params one_node_params raft_params0 st e
  in
  let out0 =
    if name_eq_dec0 orig_base_params raft_params0 e.eAt h
    then List.map (fun o -> ClientResponse (e.eClient, e.eId, o)) out
    else []
  in
  let (out', state) =
    applyEntries orig_base_params one_node_params raft_params0 h st0 es
  in
  ((List.append out0 out'), state)

(** val doGenericServer :
    baseParams -> oneNodeParams -> raftParams -> name -> raft_data0 ->
    (raft_output list * raft_data0) * (name * msg) list **)

let doGenericServer orig_base_params one_node_params raft_params0 h state =
  let (out, state0) =
    applyEntries orig_base_params one_node_params raft_params0 h state
      (List.rev
        (List.filter (fun x ->
          (&&) ((<) state.lastApplied x.eIndex)
            ((<=) x.eIndex state.commitIndex))
          (findGtIndex orig_base_params raft_params0 state.log
            state.lastApplied)))
  in
  ((out,
  (set_raft_data_lastApplied state0
    (if (<) state0.lastApplied state0.commitIndex
     then state0.commitIndex
     else state0.lastApplied))), [])

(** val replicaMessage :
    baseParams -> raftParams -> raft_data0 -> name -> name -> name * msg **)

let replicaMessage orig_base_params raft_params0 state me host =
  let prevIndex = pred (getNextIndex orig_base_params raft_params0 state host)
  in
  let prevTerm =
    match findAtIndex orig_base_params raft_params0 state.log prevIndex with
    | Some e -> e.eTerm
    | None -> 0
  in
  let newEntries =
    findGtIndex orig_base_params raft_params0 state.log prevIndex
  in
  (host, (AppendEntries (state.currentTerm, me, prevIndex, prevTerm,
  newEntries, state.commitIndex)))

(** val haveQuorum :
    baseParams -> raftParams -> raft_data0 -> name -> logIndex -> bool **)

let haveQuorum orig_base_params raft_params0 state _ n0 =
  (<) (div2 (List.length (nodes0 orig_base_params raft_params0)))
    (List.length
      (List.filter (fun h ->
        (<=) n0
          (assoc_default (name_eq_dec0 orig_base_params raft_params0)
            state.matchIndex h 0)) (nodes0 orig_base_params raft_params0)))

(** val advanceCommitIndex :
    baseParams -> raftParams -> raft_data0 -> name -> raft_data0 **)

let advanceCommitIndex orig_base_params raft_params0 state me =
  let entriesToCommit =
    List.filter (fun e ->
      (&&)
        ((&&) ((=) state.currentTerm e.eTerm)
          ((<) state.commitIndex e.eIndex))
        (haveQuorum orig_base_params raft_params0 state me e.eIndex))
      (findGtIndex orig_base_params raft_params0 state.log state.commitIndex)
  in
  set_raft_data_commitIndex state
    ((fun a b c -> List.fold_left a c b) Stdlib.max
      (List.map (fun e -> e.eIndex) entriesToCommit) state.commitIndex)

(** val doLeader :
    baseParams -> raftParams -> raft_data0 -> name -> (raft_output
    list * raft_data0) * (name * msg) list **)

let doLeader orig_base_params raft_params0 state me =
  match state.type0 with
  | Leader ->
    let state' = advanceCommitIndex orig_base_params raft_params0 state me in
    if state'.shouldSend
    then let state'0 = set_raft_data_shouldSend state' false in
         let replicaMessages =
           List.map (replicaMessage orig_base_params raft_params0 state'0 me)
             (List.filter (fun h ->
               if name_eq_dec0 orig_base_params raft_params0 me h
               then false
               else true) (nodes0 orig_base_params raft_params0))
         in
         (([], state'0), replicaMessages)
    else (([], state'), [])
  | _ -> (([], state), [])

(** val raftNetHandler :
    baseParams -> oneNodeParams -> raftParams -> name -> name -> msg ->
    raft_data0 -> (raft_output list * raft_data0) * (name * msg) list **)

let raftNetHandler orig_base_params one_node_params raft_params0 me src m state =
  let (state0, pkts) =
    handleMessage orig_base_params raft_params0 src me m state
  in
  let (p, leaderPkts) = doLeader orig_base_params raft_params0 state0 me in
  let (leaderOut, state1) = p in
  let (p0, genericPkts) =
    doGenericServer orig_base_params one_node_params raft_params0 me state1
  in
  let (genericOut, state2) = p0 in
  (((List.append leaderOut genericOut), state2),
  (List.append pkts (List.append leaderPkts genericPkts)))

(** val handleClientRequest :
    baseParams -> raftParams -> name -> raft_data0 -> clientId -> int ->
    input -> (raft_output list * raft_data0) * (name * msg) list **)

let handleClientRequest orig_base_params raft_params0 me state client id c =
  match state.type0 with
  | Leader ->
    let index0 = Stdlib.Int.succ
      (maxIndex orig_base_params raft_params0 state.log)
    in
    (([],
    (set_raft_data_shouldSend
      (set_raft_data_matchIndex
        (set_raft_data_log state ({ eAt = me; eClient = client; eId = id;
          eIndex = index0; eTerm = state.currentTerm; eInput =
          c } :: state.log))
        (assoc_set (name_eq_dec0 orig_base_params raft_params0)
          state.matchIndex me index0)) true)), [])
  | _ -> ((((NotLeader (client, id)) :: []), state), [])

(** val handleTimeout :
    baseParams -> raftParams -> name -> raft_data0 -> (raft_output
    list * raft_data0) * (name * msg) list **)

let handleTimeout orig_base_params raft_params0 me state =
  match state.type0 with
  | Leader -> (([], (set_raft_data_shouldSend state true)), [])
  | _ -> tryToBecomeLeader orig_base_params raft_params0 me state

(** val handleInput :
    baseParams -> raftParams -> name -> raft_input -> raft_data0 ->
    (raft_output list * raft_data0) * (name * msg) list **)

let handleInput orig_base_params raft_params0 me inp state =
  match inp with
  | Timeout -> handleTimeout orig_base_params raft_params0 me state
  | ClientRequest (client, id, c) ->
    handleClientRequest orig_base_params raft_params0 me state client id c

(** val raftInputHandler :
    baseParams -> oneNodeParams -> raftParams -> name -> raft_input ->
    raft_data0 -> (raft_output list * raft_data0) * (name * msg) list **)

let raftInputHandler orig_base_params one_node_params raft_params0 me inp state =
  let (p, pkts) = handleInput orig_base_params raft_params0 me inp state in
  let (handlerOut, state0) = p in
  let (p0, leaderPkts) = doLeader orig_base_params raft_params0 state0 me in
  let (leaderOut, state1) = p0 in
  let (p1, genericPkts) =
    doGenericServer orig_base_params one_node_params raft_params0 me state1
  in
  let (genericOut, state2) = p1 in
  (((List.append handlerOut (List.append leaderOut genericOut)), state2),
  (List.append pkts (List.append leaderPkts genericPkts)))

(** val reboot :
    baseParams -> raftParams -> (term, name, entry, logIndex, serverType,
    data, clientId, output) raft_data -> raft_data0 **)

let reboot _ _ state =
  { currentTerm = state.currentTerm; votedFor = state.votedFor; leaderId =
    state.leaderId; log = state.log; commitIndex = state.commitIndex;
    lastApplied = state.lastApplied; stateMachine = state.stateMachine;
    nextIndex = []; matchIndex = []; shouldSend = false; votesReceived = [];
    type0 = Follower; clientCache = state.clientCache; electoralVictories =
    state.electoralVictories }

(** val init_handlers0 :
    baseParams -> oneNodeParams -> raftParams -> name -> raft_data0 **)

let init_handlers0 _ one_node_params _ _ =
  { currentTerm = 0; votedFor = None; leaderId = None; log = [];
    commitIndex = 0; lastApplied = 0; stateMachine = one_node_params.init;
    nextIndex = []; matchIndex = []; shouldSend = false; votesReceived = [];
    type0 = Follower; clientCache = []; electoralVictories = [] }

(** val base_params : baseParams -> raftParams -> baseParams **)

let base_params _ _ =
  Build_BaseParams

(** val multi_params :
    baseParams -> oneNodeParams -> raftParams -> multiParams **)

let multi_params orig_base_params one_node_params raft_params0 =
  { msg_eq_dec = (Obj.magic msg_eq_dec0 orig_base_params raft_params0);
    name_eq_dec = (name_eq_dec0 orig_base_params raft_params0); nodes =
    (nodes0 orig_base_params raft_params0); init_handlers =
    (Obj.magic init_handlers0 orig_base_params one_node_params raft_params0);
    net_handlers =
    (Obj.magic raftNetHandler orig_base_params one_node_params raft_params0);
    input_handlers =
    (Obj.magic raftInputHandler orig_base_params one_node_params raft_params0) }

(** val failure_params :
    baseParams -> oneNodeParams -> raftParams -> failureParams **)

let failure_params orig_base_params _ raft_params0 =
  Obj.magic reboot orig_base_params raft_params0

type string0 = Pstring.t

(** val compare1 : string0 -> string0 -> comparison **)

let compare1 = (fun x y -> let c = Pstring.compare x y in if c = 0 then Eq else if c < 0 then Lt else Gt)

(** val pstring_eq_dec : string0 -> string0 -> bool **)

let pstring_eq_dec s1 s2 =
  let c = compare1 s1 s2 in (match c with
                             | Eq -> true
                             | _ -> false)

(** val raft_params : int -> raftParams **)

let raft_params n0 =
  { n = n0; input_eq_dec0 = (Obj.magic VarD.input_eq_dec); output_eq_dec0 =
    (Obj.magic VarD.output_eq_dec); clientId_eq_dec =
    (Obj.magic pstring_eq_dec) }

(** val vard_raft_base_params : int -> baseParams **)

let vard_raft_base_params n0 =
  base_params VarD.vard_base_params (raft_params n0)

(** val vard_raft_multi_params : int -> multiParams **)

let vard_raft_multi_params n0 =
  multi_params VarD.vard_base_params VarD.vard_one_node_params
    (raft_params n0)

(** val vard_raft_failure_params : int -> failureParams **)

let vard_raft_failure_params n0 =
  failure_params VarD.vard_base_params VarD.vard_one_node_params
    (raft_params n0)
