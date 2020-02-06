open! Core
open! Async

module Input = struct
  type t =
    | TrainSpeed of { train : int; speed : int; lights : bool }
    | TriggerSensor of { bank : int; index : int }
    | Turnout of { turnout : int; curved : bool }
    | SolenoidOff
    | Query of { bank : int }
    | MultiQuery of { max_bank : int }
    | Unrecognized of int
  [@@deriving sexp]
end

module Sensors = struct
  type t = { values : int array }

  let create () = { values = Array.create ~len:5 0 }

  let update { values } bank index =
    let bitIndex = if index <= 8 then 8 - index else 24 - index in
    values.(bank) <- values.(bank) lor (1 lsl bitIndex)

  let get { values } bank =
    let v = values.(bank) in
    values.(bank) <- 0;
    v

  let get_bytes t bank =
    let v = get t bank in
    Bytes.init 2 ~f:(fun idx -> (v lsr (idx * 8)) land 0xff |> Char.of_int_exn)
end

let read_result_to_or_error = function
  | `Ok c -> Ok c
  | `Eof -> Or_error.error_s [%message "File reached EOF"]

let get_train_commands ~infile =
  let get_char () =
    Reader.read_char infile >>| read_result_to_or_error
    |> Deferred.Or_error.ok_exn >>| Char.to_int
  in
  Pipe.create_reader ~close_on_exception:false (fun writer ->
      Deferred.forever () (fun () ->
          let%bind input =
            match%bind get_char () with
            | c when c < 32 ->
                let speed = c % 16 in
                let lights = c >= 16 in
                let%map train = get_char () in
                Input.TrainSpeed { train; speed; lights }
            | (33 | 34) as c ->
                let%map turnout = get_char () in
                let curved = c = 34 in
                Input.Turnout { turnout; curved }
            | 32 -> return Input.SolenoidOff
            | c when 128 < c && c <= 133 ->
                let max_bank = c - 129 in
                return (Input.MultiQuery { max_bank })
            | c when 192 < c && c <= 197 ->
                let bank = c - 193 in
                return (Input.Query { bank })
            | c -> return (Input.Unrecognized c)
          in
          Pipe.write writer input);
      Deferred.never ())

let get_stdin_commands () =
  Pipe.create_reader ~close_on_exception:false (fun writer ->
      let stdin = Lazy.force Reader.stdin in
      Deferred.forever () (fun () ->
          let%bind input =
            Reader.read_line stdin >>| read_result_to_or_error
            |> Deferred.Or_error.ok_exn
          in
          let bank = input.[0] |> Char.to_int |> fun x -> x - 65 in
          let index = String.drop_prefix input 1 |> Int.of_string in
          Pipe.write writer (Input.TriggerSensor { bank; index }));
      Deferred.never ())

let run ~infile ~outfile =
  let train_commands = get_train_commands ~infile in
  let stdin_commands = get_stdin_commands () in
  let commands = Pipe.interleave [ train_commands; stdin_commands ] in
  let sensors = Sensors.create () in
  Pipe.iter commands ~f:(fun command ->
      Log.Global.info_s [%message "Received command" (command : Input.t)];
      ( match (command : Input.t) with
      | TriggerSensor { bank; index } -> Sensors.update sensors bank index
      | Query { bank } ->
          Writer.write_bytes outfile (Sensors.get_bytes sensors bank)
      | MultiQuery { max_bank } ->
          List.init (max_bank + 1) ~f:(Sensors.get_bytes sensors)
          |> List.iter ~f:(Writer.write_bytes outfile)
      | _ -> () );
      return ())

let command =
  Command.async ~summary:"Pretend to be marklin train box"
    (let%map_open.Command inpath = anon ("INFILE" %: string)
     and outpath = anon ("OUTFILE" %: string) in
     fun () ->
       let%bind infile, outfile =
         Deferred.both (Reader.open_file inpath)
           (Writer.open_file ~append:true outpath)
       in
       Log.Global.info_s [%message "Files opened"];
       Monitor.protect
         (fun () -> run ~infile ~outfile)
         ~finally:(fun () ->
           let%bind () = Reader.close infile in
           Writer.close outfile))

let () = Command.run command
