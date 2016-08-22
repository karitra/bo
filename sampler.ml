(*
	Reservoir sampler
*)
open Core.Std

module Sampler = struct
	
	let make fname k =

		let samples = Array.create ~len:k [] in
		let make_vec_list ln =
			String.split ~on:',' ln
			|> List.map
				~f:(fun tok -> 
					if String.length tok = 0 then 
						Float.nan 
					else 
						Float.of_string tok)
		in

		let f ic =
			let f ln =
				let li = make_vec_list ln in
				let j = Random.int k in
					if j < k then
						samples.(j) <- li
					else ()
			in
			
			let header = Option.value ~default:"" (In_channel.input_line ic) in
			
			for i = 0 to k - 1 do match In_channel.input_line ic with
				| Some ln -> samples.(i) <- make_vec_list ln
				| None -> ()
			done;
			In_channel.iter_lines ic ~f;
			(header, samples)
		in

		In_channel.with_file fname ~f

	let dump ?(delim=',') header samples =
		fprintf stdout "%s\n" header;
		let rec dump_row = function
			| hd::tl -> (printf "%f%c" hd delim; dump_row tl)
			| [] -> print_endline ""
		in
		Array.iter ~f:(fun l -> dump_row l) samples

end

let () =
	let cmd fname count () = 
		let (header, samples) = Sampler.make fname count in
			Sampler.dump header samples
	in
	Command.basic
		~summary:"sample K records from bosch kaggle.com dataset"
		Command.Spec.(
			empty
			+> flag "-input" (required string) ~doc:"filename CSV file name to smaple from"
			+> flag "-samples" (required int) ~doc:"K number of sampels" )
		cmd
	|> Command.run ~version:"1.0" 
