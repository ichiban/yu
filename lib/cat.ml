open Batteries
open Lwt

let filenames = function
  | [] -> ["-"]
  | x -> x

let input = function
  | "-" ->
    return Lwt_io.stdin
  | filename ->
    Lwt_io.open_file filename
      ~flags:Unix.([O_RDONLY; O_NONBLOCK])
      ~mode:Lwt_io.input

let is_blank line =
  line |> String.trim |> String.is_empty

let number_lines ?(blank = false) lines =
  let count = ref 0 in
  Lwt_stream.map (fun line ->
    count := Int.succ !count;
    if is_blank line && blank then line
    else
      Printf.sprintf "%6d\t%s" !count line
  ) lines

let append_ends lines =
  Lwt_stream.map (fun line ->
    line ^ "$"
  ) lines

let sqeeze_blank_lines lines =
  let is_squeezing = ref false in
  Lwt_stream.filter (fun line ->
    match is_blank line, !is_squeezing with
      | false, false -> true
      | false, true ->
	is_squeezing := false;
	true
      | true, false ->
	is_squeezing := true;
	true
      | true, true -> false
  ) lines

let is_control_char ?(include_tabs = true) char =
  match Char.code char with
    | 9 -> include_tabs (* HF *)
    | n when n < 32 -> true (* NUL to US *)
    | 127 -> true (* DEL *)
    | _ -> false

let is_meta_char char =
  Char.code char > 127

let caret_notation char =
  let offset = Char.code 'A' - 1 in
  let printable_char =
    if 0o177 = Char.code char then '?'
    else
      Char.code char + offset |> Char.chr
  in
  "^" ^ String.of_char printable_char

let meta_notation char =
  let offset = -0o200 in
  let char_with_offset =
    Char.code char + offset |> Char.chr
  in
  let string =
    if is_control_char char_with_offset then
      caret_notation char_with_offset
    else
      String.of_char char_with_offset
  in
  "M-" ^ string

let replace_tabs lines =
  Lwt_stream.map (fun line ->
    String.replace_chars (function
      | '\t' as c -> caret_notation c
      | c -> String.of_char c
    ) line
  ) lines

let replace_nonprinting lines =
  Lwt_stream.map (fun line ->
    String.replace_chars (fun c ->
      if is_control_char c
	~include_tabs:false
      then
	caret_notation c
      else if is_meta_char c then
	meta_notation c
      else
	String.of_char c
    ) line
  ) lines

let perform
    ?(number_nonblank = false)
    ?(show_ends = false)
    ?(number = false)
    ?(squeeze_blank = false)
    ?(show_tabs = false)
    ?(show_nonprinting = false)
    args =
  let file_names = filenames args in
  Lwt_list.map_p input file_names >>= fun inputs ->
  Lwt_list.iter_s (fun input ->
    let lines = Lwt_io.read_lines input
		      |> begin
			if number_nonblank || number then
			  number_lines ~blank:number_nonblank
			else identity
		      end
		      |> begin
			if show_ends then append_ends
			else identity
		      end
		      |> begin
			if squeeze_blank then sqeeze_blank_lines
			else identity
		      end
		      |> begin
			if show_tabs then replace_tabs
			else identity
		      end
		      |> begin
			if show_nonprinting then replace_nonprinting
			else identity
		      end
    in
    Lwt_io.write_lines Lwt_io.stdout lines
  ) inputs
