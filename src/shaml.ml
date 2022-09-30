open! Base
open! Stdio

let execute args _stdout =
  let f_pid = Unix.fork () in
  if f_pid = 0 then
    let (cmd, a_args) =
      match args with
      | h :: t -> (h, Array.of_list t)
      | _ -> failwith "impossible"
    in
    let p_in = Unix.open_process_args_in cmd a_args in
    let p_len = In_channel.length p_in |> Int64.to_int_exn in
    let p_buff = Bytes.create p_len in
    let _ =
      In_channel.really_input
        p_in
        ~buf:p_buff
        ~pos:0
        ~len:p_len
    in
    Out_channel.output_string _stdout (Bytes.to_string p_buff);
    Out_channel.flush _stdout;
    (f_pid, Unix.WEXITED 0)
  else
    Unix.wait ()
    
let rec shell_loop ?(s : int option = None) _stdin _stdout =
  let prompt = function
    | None -> Out_channel.output_string _stdout "[]> "
    | Some n -> Printf.sprintf "[%d]> " n |> Out_channel.output_string _stdout
  in
  let () = prompt s in
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
  let open Unix in
  let _stdin = stdin |> in_channel_of_descr in
  let _stdout = stdout |> out_channel_of_descr in
  shell_loop _stdin _stdout
  
  
