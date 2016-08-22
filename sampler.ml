(*
	Reservoir sampler
*)
open Core.Std

module Sampler = struct
	
	let make fname k =

		let samples = Array.create ~len:k "" in

		let f ic =
			let f ln =
				let j = Random.int k in
					if j < k then
						samples.(j) <- ln
					else ()
			in
			
			let header = Option.value ~default:"" (In_channel.input_line ic) in
			
			for i = 0 to k - 1 do match In_channel.input_line ic with
				| Some ln -> samples.(i) <- ln
				| None -> ()
			done;
			In_channel.iter_lines ic ~f;
			(header, samples)
		in

		In_channel.with_file fname ~f

	let dump ?(delim=',') header samples =
		printf "%s\n" header;
		Array.sort
			~cmp:(fun a b -> 
				let r1 = String.index a delim and
					r2 = String.index b delim in
					match Option.both r1 r2 with
					| Some (r1,r2) -> 
						let a = String.prefix a r1 |> Int.of_string and
							b = String.prefix b r2 |> Int.of_string in
								a - b
					| None -> 0) samples;
		Array.iter ~f:print_endline samples

end

let () =
	let cmd fname count () = 
		let (header, samples) = Sampler.make fname count in
			Sampler.dump header samples
	in
	Command.basic
		~summary:"sample K records from bosch kaggle's dataset"
		Command.Spec.(
			empty
			+> flag "-input" (required string) ~doc:"filename CSV file name to sample from"
			+> flag "-samples" (required int) ~doc:"K number of samples" )
		cmd
	|> Command.run ~version:"1.0" 
