open! Base
open! Stdio

let execute args _stdout =
  let open! Core in
  let (cmd, argsv) =
    match args with
    | h :: t -> (h, t)
    | _ -> failwith "impossible"
  in
  let f_pid = Unix.fork () in
  match f_pid with
  | `In_the_child -> Unix.exec ~prog:cmd ~argv:argsv ();
  | `In_the_parent l -> Unix.wait (`Pid l);
    
let rec shell_loop ?(s : int option = None) _stdin _stdout =
  let prompt = function
    | None -> "[ ] > "
    | Some t -> Printf.sprintf "[%d] > " t
  in
  let () = Out_channel.output_string _stdout @@ prompt s in
  let () = Out_channel.flush _stdout in
  let line_o = In_channel.input_line _stdin in
  let line =
    match line_o with
    | Some l -> l
    | None -> failwith "broken"
  in
  let args = String.split_on_chars ~on:[' '] line in
  let (_, t) = execute args _stdout in
  let r = match t with
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n -> n
  | Unix.WSTOPPED n -> n
  in
  shell_loop ~s:(Some r) _stdin _stdout

let () =
  let open! Unix in
  let _stdin = stdin |> in_channel_of_descr in
  let _stdout = stdout |> out_channel_of_descr in
  shell_loop _stdin _stdout
