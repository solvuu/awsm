(** Secure Random Password protocol implementation for AWS *)

open Awsm
open! Import

module MyCryptokitBignum = struct
  (* This is a copy/paste of portions of the MyCryptokitBignum module.
     The module stopped being exported between releases of cryptokit.
     https://github.com/xavierleroy/cryptokit/pull/31

     This code may be separately licensed from the rest of awsm.
     See the cryptokit project.  *)
  type t = Z.t

  module Char = Stdlib.Char
  module Bytes = Stdlib.Bytes

  let wipe_bytes s = Bytes.fill s 0 (Bytes.length s) '\000'

  let of_bytes s =
    let l = String.length s in
    let t = Bytes.create l in
    for i = 0 to l - 1 do
      Bytes.set t i s.[l - 1 - i]
    done;
    let n = Z.of_bits (Bytes.unsafe_to_string t) in
    wipe_bytes t;
    n
  ;;

  let to_bytes ?numbits n =
    let s = Z.to_bits n in
    let l =
      match numbits with
      | None -> String.length s
      | Some nb ->
        assert (Z.numbits n <= nb);
        (nb + 7) / 8
    in
    let t = Bytes.make l '\000' in
    for i = 0 to String.length s - 1 do
      Bytes.set t (l - 1 - i) s.[i]
    done;
    wipe_bytes (Bytes.unsafe_of_string s);
    Bytes.unsafe_to_string t
  ;;

  let zero = Z.zero
  let compare = Z.compare
  let add = Z.add
  let sub = Z.sub
  let mult = Z.mul
  let mod_ = Z.rem
  let of_int = Z.of_int
  let mod_power = Z.powm_sec
  let change_byte s i f = Bytes.set s i (Char.chr (f (Char.code (Bytes.get s i))))

  let random ~rng ?(odd = false) numbits =
    let numbytes = (numbits + 7) / 8 in
    let buf = Bytes.create numbytes in
    rng buf 0 numbytes;
    (* adjust low byte if requested *)
    if odd then change_byte buf 0 (fun b -> b lor 1);
    (* adjust high byte so that the number is exactly numbits long *)
    let mask = 1 lsl ((numbits - 1) land 7) in
    change_byte buf (numbytes - 1) (fun b -> b land (mask - 1) lor mask);
    (* convert to a number *)
    let n = Z.of_bits (Bytes.unsafe_to_string buf) in
    wipe_bytes buf;
    assert (Z.numbits n = numbits);
    if odd then assert (Z.is_odd n);
    n
  ;;
end

(** As defined in python warrant aws-srp.py implemetation *)
let info_bits = "Caldera Derived Key"

let pad_hex (`Encoded_hex h) =
  let h_len = String.length h in
  if Caml.( mod ) h_len 2 = 1
  then `Encoded_hex (sprintf "0%s" h)
  else (
    match h.[0] with
    | '8' | '9' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' ->
      `Encoded_hex (sprintf "00%s" h)
    | _ -> `Encoded_hex h)
;;

let encoded_hex l = `Encoded_hex (String.concat l)

(* A large prime number known by both us and the AWS server. *)
let default_modulo_hex =
  encoded_hex
    [ "FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74"
    ; "020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F1437"
    ; "4FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED"
    ; "EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF05"
    ; "98DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB"
    ; "9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3B"
    ; "E39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF695581718"
    ; "3995497CEA956AE515D2261898FA051015728E5A8AAAC42DAD33170D04507A33"
    ; "A85521ABDF1CBA64ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7"
    ; "ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6BF12FFA06D98A0864"
    ; "D87602733EC86A64521F2B18177B200CBBE117577A615D6C770988C0BAD946E2"
    ; "08E24FA074E5AB3143DB5BFCE0FD108E4B82D120A93AD2CAFFFFFFFFFFFFFFFF"
    ]
;;

exception
  Hex_decoding_error of
    { hex : string
    ; cause : exn
    }
[@@deriving sexp]

let bignum_of_hex hex : MyCryptokitBignum.t =
  (match hex with
   | `Encoded_hex hex -> (
     try Cryptokit.transform_string (Cryptokit.Hexa.decode ()) hex with
     | cause -> raise (Hex_decoding_error { hex; cause }))
   | `Decoded_hex hex -> hex)
  |> MyCryptokitBignum.of_bytes
;;

let to_bytes t =
  let bytes = MyCryptokitBignum.to_bytes ?numbits:None t in
  let byte_len = String.length bytes in
  String.lfindi bytes ~f:(fun _i c -> not (Char.equal '\000' c))
  |> function
  | None -> bytes
  | Some pos ->
    if MyCryptokitBignum.compare t MyCryptokitBignum.zero = 0
    then String.of_char '\000'
    else String.slice bytes pos byte_len
;;

let hex_of_bignum c : [ `Encoded_hex of string ] =
  to_bytes c
  |> Cryptokit.transform_string (Cryptokit.Hexa.encode ())
  |> fun encoded -> `Encoded_hex encoded
;;

let ensure_hex_decoded s =
  match s with
  | `Decoded_hex _ as d -> d
  | `Encoded_hex encoded ->
    `Decoded_hex (Cryptokit.transform_string (Cryptokit.Hexa.decode ()) encoded)
;;

let rec ensure_hex_encoded ?padding s =
  match s with
  | `Encoded_hex hex as h -> (
    match padding with
    | None -> h
    | Some padding ->
      let pad' = padding - String.length hex in
      `Encoded_hex (String.concat [ String.init pad' ~f:(Fn.const '0'); hex ]))
  | `Decoded_hex decoded ->
    ensure_hex_encoded
      ?padding
      (`Encoded_hex (Cryptokit.transform_string (Cryptokit.Hexa.encode ()) decoded))
;;

let hex_hash s =
  let hash' raw =
    Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) raw
    |> fun hashed ->
    let decoded_hex = `Decoded_hex hashed in
    ensure_hex_encoded ~padding:64 decoded_hex
  in
  match s with
  | (`Decoded_hex _ | `Encoded_hex _) as hex ->
    let (`Decoded_hex decoded) = ensure_hex_decoded hex in
    hash' decoded
  | `String str -> hash' str
;;

let calculate_hkdf ~ikm_hex ~salt_hex =
  let (`Decoded_hex salt) = ensure_hex_decoded salt_hex in
  let (`Decoded_hex ikm) = ensure_hex_decoded ikm_hex in
  let hmac = Cryptokit.MAC.hmac_sha256 salt in
  let prk = Cryptokit.hash_string hmac ikm in
  let info_bits_update = sprintf "%s%c" info_bits '\x01' in
  let hmac = Cryptokit.MAC.hmac_sha256 prk in
  Cryptokit.hash_string hmac info_bits_update
  |> fun hmac_hash -> String.slice hmac_hash 0 16
;;

let hexdump_bignum n =
  let hexdump_line l =
    List.map l ~f:(fun c -> Printf.sprintf "%02x" (Char.to_int c)) |> String.concat
  in
  MyCryptokitBignum.to_bytes n
  |> String.to_list
  |> List.chunks_of ~length:32
  |> List.map ~f:hexdump_line
  |> String.concat ~sep:"\n"
  |> print_endline
;;

module Test_data = struct
  let a_hex =
    encoded_hex
      [ "7f42b7792c7607f92f7244c399c47639a319c5c5d998fd1f8596adcdf08796e9"
      ; "822a15d3339c09cde7a6b35431f0ca9663f4fc484c446cc06cc772ff94a0f018"
      ; "fd6aeaa0b9171d1dc1f78b3f13ae44526e47461453ad4f2e4b4dd9e01c363ed1"
      ; "2a555ff595ec60e7bdeee88a75880bc83f5e3b569cadc42420577db16b455459"
      ; "996296509afacc6dfaa1693bdcb6f8613b56816c348b1b23a013abd1441c4e68"
      ; "f67b7bf162f8c1d27ff3c3f4899e04bc247df2a125c0850f8feed3dbac2bf2d1"
      ; "6555ed15cba0427f0483d6fae5969dc0c8ee55aaf6e5956cba5690c328dff23e"
      ; "a93b9f1839f1ba68af2ddb992b41a12007e8ee9673f0288f036ef041f2f3e89f"
      ; "cdc650adf990859393ef83cc637391c4a8882b5a0165da5b5fcf4316bdc4f832"
      ; "8c9743858c0236ad7cfd5c7cf0ff3402ee61f35d81782b808e6e9b44f84545d2"
      ; "c8c45c4dedf83e329a8690c24028ac7e1c16ab828e4c85edc0d4fb84c0a455dc"
      ; "2e125d42bc0130c2a060abdfe3904516881889afda6938834aca32e0cfca1986"
      ]
  ;;

  let b_hex =
    encoded_hex
      [ "b97838832066a3ad6b776d8d54c2a09b324256465687ff4c2e00f2336a284cb0"
      ; "6088b1eae4e72c55368ed48552f4fc876d9aceda76ba772ef22ecddcabd3531a"
      ; "62273e954833f2001f20645da6b5e0d09f4c159980cdeff6b724a7c535dad03c"
      ; "98fa1845525198e6253eb0738d58147be31ba7540ef3e4cbc034513cae15bcf7"
      ; "1c3d69b5d432ada2b29ccd5838d6e51c5c9f2bc217056ebc4f17a89924c44866"
      ; "bad4e2c28cab08c06c4f80e06d30afc6909ab5eeaf8caae5e13920d1d5838491"
      ; "45517273a98a25a3db8e1dc9cf2f3784752321f1d4a89353009bac9a8922b0d2"
      ; "280a3333aa589e817a7f4a4db60d801b86b43272287e6e8559f76736b930dcce"
      ; "93fdec1e325bd1067633be9a55a4347ba2299d827a39965bc6bb65b3fa14158b"
      ; "8af27085de6fed0d20773148308ba1c3fa1c28d6e37235db08e2b3b05712dcc0"
      ; "051576743ff301e3396a9020c8ff54ef9f8a07a3b26b09a10590bc8df46a3713"
      ; "a8451c57a707e2a89964a18d3473e84e182152151f3e770ca615125d2dc12d2c"
      ]
  ;;

  let small_a_hex =
    encoded_hex
      [ "82efa1241178cad55526268a8cdb70ef723dfae50175923cb8061faf368fa01a"
      ; "e313057c9c397fc9c76ed2bf5f64bc833de36adfe63073dcc53072ef3adddf30"
      ; "9f2186f4176d6eec001cbfebf7ab1fd75fc5d74b5ea6d00c48d6e13600b9a4c6"
      ; "4357c6016e10de045936739578342c3e4d63b2ab0fdb01a5522ee90bf7f6c9e5"
      ]
  ;;

  let small_a = bignum_of_hex small_a_hex
  let salt_hex = `Encoded_hex "0ffaabbccdd7"
  let username = "awsm-test-user"
  let password = "awsm-test-password"
  let user_pool_id = "us-east-1_1aBcdeFgh"
end

let%expect_test "calculate_hkdf" =
  let open Test_data in
  let ikm_hex =
    `Encoded_hex "6e61edce3daf3452e8af06b593cecfa75bfece880f79b2205ca5330dc841d67d"
  in
  calculate_hkdf ~ikm_hex ~salt_hex |> MyCryptokitBignum.of_bytes |> hexdump_bignum;
  [%expect {| 262ab606d20c8e9f49f0af944955bd5d |}]
;;

let of_base64 (`Encoded_base64 b64) : MyCryptokitBignum.t =
  Cryptokit.transform_string (Cryptokit.Base64.decode ()) b64
  |> MyCryptokitBignum.of_bytes
;;

let to_base64_bytes (t : string) : [ `Encoded_base64 of string ] =
  `Encoded_base64 (Cryptokit.transform_string (Cryptokit.Base64.encode_compact ()) t)
;;

let default_g : MyCryptokitBignum.t = MyCryptokitBignum.of_int 2
let rng = Cryptokit.Random.secure_rng#random_bytes
let default_modulo = bignum_of_hex default_modulo_hex
let default_bits = 128

let pow_mod ?(modulo = default_modulo) base exp =
  MyCryptokitBignum.mod_power base exp modulo
;;

let rand_bits ?(bits = default_bits) ?(modulo = default_modulo) () =
  MyCryptokitBignum.random ~rng bits |> fun b -> MyCryptokitBignum.mod_ b modulo
;;

let canonical_user_pool_id user_pool_id =
  String.lsplit2_exn ~on:'_' user_pool_id
  |> function
  | _lhs, rhs -> rhs
;;

type params_a =
  { k : MyCryptokitBignum.t
  ; a_hex : [ `Encoded_hex of string ]
  ; small_a : MyCryptokitBignum.t
  }

let unwrap_encoded_hex (`Encoded_hex hex) = hex

let ephemeral_a ?bits ?(g = default_g) ?small_a ?(modulo = default_modulo) () =
  let k =
    hex_hash
      (`Encoded_hex
        (sprintf
           "00%s%s"
           (unwrap_encoded_hex (hex_of_bignum modulo))
           (unwrap_encoded_hex (hex_of_bignum g))))
    |> bignum_of_hex
  in
  let small_a =
    match small_a with
    | None -> rand_bits ?bits ~modulo ()
    | Some small_a -> small_a
  in
  let a = pow_mod ~modulo g small_a in
  { k; a_hex = hex_of_bignum a; small_a }
;;

let%expect_test "ephemeral_a" =
  let open Test_data in
  let { a_hex; k; _ } = ephemeral_a ~small_a () in
  bignum_of_hex a_hex |> hexdump_bignum;
  [%expect
    {|
8d17c2d302fa31ac257e36d0be7a2f5a6ed341b229fdae72415b932b394eb035
b9c427c96498cda5b0f1cbad618c432b56e90a368e6c8e9404f9e92fbb7130e1
54e3f13a502b5bb79ad119f55ec2a823f335d46b0909435174046e600081359a
f7ce68d7a6dd2e0c45b0a7ab582e0c4ba4872962997859318a7c485b593e19d7
7f1f02008f4f0ce79e0ca5159c48f90ff3e556be6aaee88a172dd40319acdbf3
cbeb79b74cd1f3665a045f983f02126881ad14fdb62f7a0b156314d106db147a
d97a976b8dc257aa367da69d8cb23584ff2b72864757579cbfd1f746fa9dfd51
5b6aeb6b15104dd13e199f5303db83996a9e90aa5ad0b3f1f9e1eb51fa8bae5f
9bb233b9cb348521c673f59bd67c512fec3cc803b51732477c2f35e983efcac6
eb659d5ff64035eb682dbc9582271add653e41ed89a6d8ff77ea6be2cf138b28
781f136d887c61f23af7ce9a8a1696838131b8d3e0820263f63f931e18f4ddc9
b724ce51dcc715f80c47a9f9fce318ba047001af1e1c39b9b2774becf55744a7
|}];
  hexdump_bignum k;
  [%expect {| 538282c4354742d7cbbde2359fcf67f9f5b3a6b08791e5011b43b8a5b66d9ee6 |}]
;;

let calculate_u ~a_hex ~b_hex =
  let (`Encoded_hex a) = a_hex in
  let (`Encoded_hex b) = b_hex in
  `Encoded_hex (String.concat [ a; b ]) |> hex_hash |> bignum_of_hex
;;

let%expect_test "calculate_u" =
  let open Test_data in
  hexdump_bignum (calculate_u ~a_hex ~b_hex);
  [%expect {| f9e55a6bd6d3f632bf439237c350223646422093aa4f7f9a27ab8fd71e2127ab |}]
;;

let calculate_x ~username ~password ~user_pool_id ~salt_hex =
  let canonical_user_pool_id = canonical_user_pool_id user_pool_id in
  let username_password_hash =
    sprintf "%s%s:%s" canonical_user_pool_id username password
    |> fun s -> hex_hash (`String s)
  in
  List.map [ pad_hex salt_hex; username_password_hash ] ~f:unwrap_encoded_hex
  |> String.concat
  |> fun x -> `Encoded_hex x |> hex_hash |> bignum_of_hex
;;

let%expect_test "calculate_x" =
  let open Test_data in
  hexdump_bignum (calculate_x ~salt_hex ~username ~user_pool_id ~password);
  [%expect {| a63ef5d43d9741e7b5706798c13f926678b70c085cc1b5ec3752995c3806bbca |}]
;;

let calculate_s ?modulo ?(g = default_g) ~small_a ~k ~x_value ~b_hex ~u_value () =
  let g_mod_pow_xn = pow_mod ?modulo g x_value in
  let int_value2 =
    MyCryptokitBignum.sub (b_hex |> bignum_of_hex) (MyCryptokitBignum.mult k g_mod_pow_xn)
  in
  pow_mod
    ?modulo
    int_value2
    (MyCryptokitBignum.add small_a (MyCryptokitBignum.mult u_value x_value))
;;

let%expect_test "calculate_s" =
  let open Test_data in
  let { k; a_hex; small_a } = ephemeral_a ~small_a () in
  let u_value = calculate_u ~a_hex ~b_hex in
  let x_value = calculate_x ~salt_hex ~username ~user_pool_id ~password in
  hexdump_bignum
    (calculate_s ?modulo:None ?g:None ~small_a ~k ~b_hex ~u_value ~x_value ());
  [%expect
    {|
      744f5b3234f44e0c7b09417e10879104c35a90c0cbc9d858cdfaa4ab711a1767
      68f918d01ceb83aaeed9a3c1724de0adc86fe0081ba1545d71cd6af82762d3a8
      22190dd11ca24a5250a1f67a09045f34ef182d68ad6aa09108027bf41548bab0
      ab9a34fd5926deceebf080d88c4592d081038b7e1f29946d11dedc85784e3d13
      e81931d30291fdd1375e6c69f27ae92c04c86e32e3b2395f225920b311440353
      60bed0199a82b1da9e3ca3b4d1ea6da7f0be61612505f1f010a2817ee0bd829c
      d37a5570dbb0b0169261e379ab167d26e5828850c38e0aa572c5cf887316bc36
      21ad9abb44c213fc875157ef8e317855a3391feaacd307e91a3cea6989d34b6f
      01b0e9870957d455d2c814de3d30d3253c120eb1b9a7ad6eb0b5a7639bf6c8d9
      5fede7c77508c87aea5daeee9714381fa10657ab93b24fc10674500bad225bd8
      c2595e58c5e420c2b288df87b7af0a0a887d892cb8cbd203c3301a633bbc8539
      04e20ed5431d68b28453ce1bc48d33ee4d3f4ae49a36eec670b1f4909da99bbd |}]
;;

let get_password_authentication_key
  ?modulo
  ?g
  ~username
  ~password
  ~user_pool_id
  ~salt_hex
  ~small_a
  ~k
  ~a_hex
  ~b_hex
  ()
  =
  let u_value = calculate_u ~a_hex ~b_hex in
  let x_value = calculate_x ~username ~password ~user_pool_id ~salt_hex in
  let s_value = calculate_s ?modulo ?g ~small_a ~k ~x_value ~b_hex ~u_value () in
  let hkdf =
    calculate_hkdf
      ~ikm_hex:(pad_hex (s_value |> hex_of_bignum))
      ~salt_hex:(pad_hex (hex_of_bignum u_value))
  in
  hkdf
;;

let%expect_test "get_password_authentication_key" =
  let open Test_data in
  let { k; a_hex; small_a } = ephemeral_a ~small_a () in
  get_password_authentication_key
    ?modulo:None
    ?g:None
    ~username
    ~password
    ~user_pool_id
    ~salt_hex
    ~small_a
    ~k
    ~a_hex
    ~b_hex
    ()
  |> MyCryptokitBignum.of_bytes
  |> hexdump_bignum;
  [%expect {| 162aba97382b3f5c2b4ecb106fa72b41 |}]
;;

let signature
  ?modulo
  ?g
  ~salt_hex
  ~secret_block_base64
  ~a_hex
  ~b_hex
  ~small_a
  ~username
  ~user_pool_id
  ~k
  ~password
  ~timestamp
  ()
  =
  let a_hex = pad_hex a_hex in
  let b_hex = pad_hex b_hex in
  let hkdf =
    get_password_authentication_key
      ~small_a
      ~user_pool_id
      ?modulo
      ~k
      ?g
      ~a_hex
      ~b_hex
      ~salt_hex
      ~username
      ~password
      ()
  in
  let secret_block_bytes = of_base64 secret_block_base64 |> to_bytes in
  let msg =
    String.concat
      [ canonical_user_pool_id user_pool_id; username; secret_block_bytes; timestamp ]
  in
  let hmac_hash = Cryptokit.MAC.hmac_sha256 hkdf in
  let digest = Cryptokit.hash_string hmac_hash msg in
  to_base64_bytes digest
;;
