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

let perform
    ?(number_nonblank = false)
    ?(show_ends = false)
    ?(number = false)
    ?(squeeze_blank = false)
    ?(show_tabs = false)
    ?(show_nonprinting = false)
    args =
  Lwt_main.run (
    Lwt_list.map_s input (filenames args) >>= fun inputs ->
    Lwt_list.iter_s (fun input ->
      let line_stream = Lwt_io.read_lines input in
      Lwt_io.write_lines Lwt_io.stdout line_stream
    ) inputs
  )
