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
			for i = 0 to k - 1 do match In_channel.input_line ic with
				| Some ln -> samples.(i) <- make_vec_list ln
				| None -> ()
			done;
			In_channel.iter_lines ic ~f
		in

		In_channel.with_file fname ~f
end
