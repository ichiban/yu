open Batteries
open BatOptParse

(* helpers *)

let any_of flags =
  let enum = List.enum flags in
  let bool_enum = Enum.map (Opt.get) enum in
  Enum.fold (||) false bool_enum
    
(* cat *)

let show_all = StdOpt.store_true ()
let number_nonblank = StdOpt.store_true ()
let show_nonprinting_and_ends = StdOpt.store_true ()
let show_ends = StdOpt.store_true ()
let number = StdOpt.store_true ()
let squeeze_blank = StdOpt.store_true ()
let show_nonprinting_and_tabs = StdOpt.store_true ()
let show_tabs = StdOpt.store_true ()
let unbuffered = StdOpt.store_true ()
let show_nonprinting = StdOpt.store_true ()

let cat_parser =
  let parser = OptParser.make ()
    ~description:"\
      copies each file (‘-’ means standard input), \
      or standard input if none are given, to standard output"
    ~prog:"cat"
  in
  let add = OptParser.add parser in
  add show_all
    ~short_name:'A'
    ~long_name:"show-all"
    ~help:"equivalent to -vET";
  add number_nonblank
    ~short_name:'b'
    ~long_name:"number-nonblank"
    ~help:"number nonempty output lines, overrides -n";
  add show_nonprinting_and_ends
    ~short_name:'e'
    ~help:"Equivalent to -vE.";
  add show_ends
    ~short_name:'E'
    ~long_name:"show-ends"
    ~help:"display $ at end of each line";
  add number
    ~short_name:'n'
    ~long_name:"number"
    ~help:"number all output lines";
  add squeeze_blank
    ~short_name:'s'
    ~long_name:"squeeze-blank"
    ~help:"suppress repeated empty output lines";
  add show_nonprinting_and_tabs
    ~short_name:'t'
    ~help:"Equivalent to -vT.";
  add show_tabs
    ~short_name:'T'
    ~long_name:"show-tabs"
    ~help:"Display TAB characters as ‘^I’.";
  add unbuffered
    ~short_name:'u'
    ~help:"Ignored for POSIX compatibility.";
  add show_nonprinting
    ~short_name:'v'
    ~long_name:"show-nonprinting"
    ~help:"\
      Display control characters except for LFD and TAB using ‘^’ notation \
      and precede characters that have the high bit set with ‘M-’.";
  parser

let cat_command =
  let args = OptParser.parse_argv cat_parser in
  Cat.perform args
    ~number_nonblank:(any_of [
      number_nonblank;
    ])
    ~show_ends:(any_of [
      show_ends;
      show_nonprinting_and_ends;
    ])
    ~number:(any_of [
      number;
      number_nonblank;
    ])
    ~squeeze_blank:(any_of [
      squeeze_blank;
    ])
    ~show_tabs:(any_of [
      show_tabs;
      show_nonprinting_and_tabs;
    ])
    ~show_nonprinting:(any_of [
      show_nonprinting;
      show_nonprinting_and_ends;
      show_nonprinting_and_tabs;
    ])

let default_command =
  Lwt.return ()

let command_name =
  let _, program = String.rsplit Sys.executable_name ~by:"/" in
  try
    let base, _ = String.rsplit program ~by:"." in
    base
  with
    | Not_found -> program

let command =
  match command_name with
    | "cat" -> cat_command
    | _ -> default_command

let () =
  try
    Lwt_main.run command;
    exit 0
  with
    | _ -> exit 1
