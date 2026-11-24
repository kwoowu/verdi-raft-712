open Str
open Printf
open VarDRaft
open VarD
open Util

let serializeOutput out =
  let (c, os) =
    match Obj.magic out with
    | NotLeader (client_id, request_id) ->
       (Pstring.to_string (Obj.magic client_id), sprintf "NotLeader %s" (string_of_int request_id))
    | ClientResponse (client_id, request_id, o) ->
       let Response (k, value, old) = Obj.magic o in
       (Pstring.to_string (Obj.magic client_id),
        match value, old with
        | Some v, Some o ->
           sprintf "Response %s %s %s %s"
             (string_of_int request_id)
             (Pstring.to_string k)
             (Pstring.to_string v)
             (Pstring.to_string o)
        | Some v, None ->
           sprintf "Response %s %s %s -"
             (string_of_int request_id)
             (Pstring.to_string k)
             (Pstring.to_string v)
        | None, Some o ->
           sprintf "Response %s %s - %s"
             (string_of_int request_id)
             (Pstring.to_string k)
             (Pstring.to_string o)
        | None, None ->
           sprintf "Response %s %s - -"
             (string_of_int request_id)
             (Pstring.to_string k))
  in (c, Bytes.of_string os)

(* Behold, my genius optimization! *)
let input_regex = regexp "\\([0-9]+\\) \\([A-Z]+\\) +\\([/A-Za-z0-9]+\\|-\\) +\\([/A-Za-z0-9]+\\|-\\) +\\([/A-Za-z0-9]+\\|-\\)[^/A-Za-z0-9]*"

let deserializeInp i =
  let inp = String.trim (Bytes.to_string i) in
  if string_match input_regex inp 0 then
    (match (matched_group 1 inp, matched_group 2 inp,
            matched_group 3 inp, matched_group 4 inp,
            matched_group 5 inp) with
     | (i, "GET", k, _, _) -> Some (int_of_string i, Get (Pstring.unsafe_of_string k))
     | (i, "DEL", k, _, _) -> Some (int_of_string i, Del (Pstring.unsafe_of_string k))
     | (i, "PUT", k, v, _) -> Some (int_of_string i, Put ((Pstring.unsafe_of_string k), (Pstring.unsafe_of_string v)))
     | (i, "CAD", k, o, _) -> Some (int_of_string i, CAD (Pstring.unsafe_of_string k, Pstring.unsafe_of_string o))
     | (i, "CAS", k, "-", v) -> Some (int_of_string i, CAS ((Pstring.unsafe_of_string k), None, (Pstring.unsafe_of_string v)))
     | (i, "CAS", k, o, v) -> Some (int_of_string i, CAS ((Pstring.unsafe_of_string k), Some (Pstring.unsafe_of_string o), (Pstring.unsafe_of_string v)))
     | _ -> None)
  else
    (print_endline "No match" ; None)

let deserializeInput inp client_id =
  match deserializeInp inp, Pstring.of_string client_id with
  | Some (request_id, input), Some cid ->
    Some (ClientRequest (Obj.magic cid, request_id, Obj.magic input))
  | _ -> None

let deserializeMsg (b : bytes) : VarDRaft.msg = Marshal.from_bytes b 0

let serializeMsg (msg : VarDRaft.msg) : bytes = Marshal.to_bytes msg []

let client_regex = regexp "^[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]4[0-9a-f][0-9a-f][0-9a-f][89ab][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$"

let deserializeClientId b =
  let s = Bytes.to_string b in
  (*let r = regexp "^[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]-4[0-9a-f][0-9a-f][0-9a-f]-[89ab][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$" in*)
  if string_match client_regex s 0 then
    Some s
  else
    None
