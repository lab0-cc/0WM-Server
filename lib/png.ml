(** Adler-32 hash *)
let adler32 s =
  let (a, b) = String.fold_left (fun (a, b) c ->
    let a = (a + int_of_char c) mod 65521 in
    (a, (a + b) mod 65521)) (1, 0) s in
  (b lsl 16) lor a

(** CRC-32 table *)
let crc_table =
  let open Int32 in
  Array.init 256 (fun i ->
    let c = ref (of_int i) in
    for _ = 0 to 7 do
      c := if logand !c 1l = 1l
           then logxor (shift_right_logical !c 1) 0xEDB88320l
           else shift_right_logical !c 1
    done;
    !c)

(** CRC-32 computation *)
let crc32 s =
  let open Int32 in
  String.fold_left (fun crc c ->
    let i = int_of_char c |> of_int |> logxor crc |> logand 0xFFl |> to_int in
    shift_right_logical crc 8 |> logxor crc_table.(i)) 0xFFFFFFFFl s |> lognot

(** Add a BE u32 to a buffer *)
let add_u32_be ~f buf =
  f 24 |> Buffer.add_char buf;
  f 16 |> Buffer.add_char buf;
  f 8 |> Buffer.add_char buf;
  f 0 |> Buffer.add_char buf

(** Take 8 bits from a u32 *)
let char_of_u32 i n = (i lsr n) land 0xFF |> char_of_int

(** Add a BE int to a buffer *)
let add_u32_be_int buf i = add_u32_be ~f:(char_of_u32 i) buf

(** Add a BE int32 to a buffer *)
let add_u32_be_i32 buf i =
  add_u32_be ~f:(fun n -> Int32.(shift_right_logical i n |> logand 0xFFl |> to_int |> char_of_int))
             buf

(** Add a LE u16 to a buffer *)
let add_u16_le buf i =
  let i = i land 0xFFFF in
  i land 0xFF |> char_of_int |> Buffer.add_char buf;
  (i lsr 8) land 0xFF |> char_of_int |> Buffer.add_char buf

(** Add a chunk to a buffer *)
let add_chunk buf ~type_ ~data =
  String.length data |> add_u32_be_int buf;
  Buffer.add_string buf type_;
  Buffer.add_string buf data;
  crc32 (type_ ^ data) |> add_u32_be_i32 buf

(** zlib stream *)
let zlib s =
  let len = String.length s in
  let b = Buffer.create (len + 6 + (len / 65535 + 1) * 5) in
  (* zlib header: CMF=0x78 (deflate, 32K window), FLG=0x01 (no/fastest, and (CMF*256+FLG) % 31 == 0) *)
  Buffer.add_char b (char_of_int 0x78);
  Buffer.add_char b (char_of_int 0x01);
  (* Emit stored blocks of at most 65535 bytes *)
  let pos = ref 0 in
  while !pos < len do
    let chunk_len = min 65535 (len - !pos) in
    char_of_int (if !pos + chunk_len = len then 0x01 else 0x00) |> Buffer.add_char b;
    add_u16_le b chunk_len;
    add_u16_le b (0xFFFF - chunk_len);
    String.sub s !pos chunk_len |> Buffer.add_string b;
    pos := !pos + chunk_len
  done;
  adler32 s |> add_u32_be_int b;
  Buffer.contents b

let encode (img:Color.t array array) =
  let h = Array.length img in
  if h = 0 then invalid_arg "encode";
  let w = Array.length img.(0) in
  if w = 0 then invalid_arg "encode";
  let idat =
    let b = Buffer.create (h * (1 + 4 * w)) in
    for y = 0 to h - 1 do
      (* filter type 0 (None) *)
      Buffer.add_char b '\x00';
      for x = 0 to w - 1 do
        let p = img.(y).(x) in
        Buffer.add_char b (char_of_int (Math.clamp ~min:0 ~max:255 p.r));
        Buffer.add_char b (char_of_int (Math.clamp ~min:0 ~max:255 p.g));
        Buffer.add_char b (char_of_int (Math.clamp ~min:0 ~max:255 p.b));
        Buffer.add_char b (char_of_int (Math.clamp ~min:0 ~max:255 p.a));
      done
    done;
    Buffer.contents b |> zlib in
  let out = Buffer.create (8 + 25 + String.length idat + 12) in
  (* Signature *)
  Buffer.add_string out "\137PNG\r\n\026\n";
  (* IHDR *)
  let ihdr =
    let b = Bytes.create 13 in
    let set_u32_be off v =
      char_of_u32 v 24 |> Bytes.set b off;
      char_of_u32 v 16 |> Bytes.set b (off + 1);
      char_of_u32 v 8 |> Bytes.set b (off + 2);
      char_of_u32 v 0 |> Bytes.set b (off + 3) in
    set_u32_be 0 w;
    set_u32_be 4 h;
    char_of_int 8 |> Bytes.set b 8;  (* Bit depth *)
    char_of_int 6 |> Bytes.set b 9;  (* Color type RGBA *)
    char_of_int 0 |> Bytes.set b 10; (* Compression method 0 *)
    char_of_int 0 |> Bytes.set b 11; (* Filter method 0 *)
    char_of_int 0 |> Bytes.set b 12; (* Interlace method 0 *)
    Bytes.unsafe_to_string b in
  add_chunk out ~type_:"IHDR" ~data:ihdr;
  add_chunk out ~type_:"IDAT" ~data:idat;
  add_chunk out ~type_:"IEND" ~data:"";
  Buffer.contents out
