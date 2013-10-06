open GL
open Glex

let out = open_out "out.txt"

type attribute = {mutable name : string; mutable value : int}

type geometry_params = {
				mutable input_type   : int;
			 	mutable output_type  : int;
				mutable vertices_out : int;
		       }

let init_geometry_params = {input_type = 4; output_type = 5; vertices_out = 3}

type shader_info = { 
			mutable vertex_shader   : shader_object;
	        	mutable fragment_shader : shader_object;
	       	 	mutable geometry_shader : shader_object option;
	        	mutable program         : shader_program;
			mutable attributes      : attribute array;
			mutable geometry_params : geometry_params option;
         	   }

module Resource_Map = Map.Make (struct type t = string let compare = String.compare end)

let shader_list = ref (Resource_Map.empty)



(* parameters:                                                            *)
(*	name        : key in map                                          *)
(*      shader_info : value in map                                        *)
(*                                                                        *)
(* return_value:                                                          *)
(*	unit                						  *)
let insert_shader_list (name:string) (shader:shader_info) = shader_list := Resource_Map.add name shader (!shader_list)


(* parameters:                                                            *)
(* 	source : shader source string value                               *)
(* 	source : GL_VERTEX_SHADER, GL_FRAGMENT_SHADER, GL_GEOMETRY_SHADER *)
(* 	prep   : optional                                                 *)
(*                                                                        *)
(* return value:                                                          *)
(*	shader_object							  *)
let compile_shader source shader_type prep = 
	let sh = GL.glCreateShader shader_type in
	begin
		match prep with
		   None -> GL.glShaderSource sh source
		|  Some p -> Glex.glShaderSources sh 2 (Some [|p; source|]) None 
	end;
	GL.glCompileShader sh;
	GL.glGetShaderCompileStatus_exn sh;
	sh


(* parameters:								    *)
(* 	vertex_program   : vertex shader source file name, not null         *)
(*	fragment_paogram : fragment shader source file name, not null       *)	
(*	geometry_program : geometry shader source file name, optional       *)
(*	geometry_params  : geometry shader parameter, optional              *)
(*      prep             : optional                                         *)
(*                                                                          *)
(* return value:                                                            *)

let create vertex_shader_file fragment_shader_file geometry_shader_file geometry_params prep = 
	let sc = open_in_gen [Open_text] 0o777 vertex_shader_file in
	let size = in_channel_length sc in
	let vertex_shader_src = String.create size in
	ignore (input sc vertex_shader_src 0 size);
(*	Printf.fprintf out "\n\n%s\n\n" vertex_shader_src;*)
	let vertex_shader = compile_shader vertex_shader_src GL_VERTEX_SHADER prep in 
	close_in sc;

	let sc = open_in_gen [Open_text] 0o777 fragment_shader_file in
	let size = in_channel_length sc in
	let fragment_shader_src = String.create size in
	ignore (input sc fragment_shader_src 0 size);
(*	Printf.fprintf out "\n\n%s\n\n" fragment_shader_src;*)
	let fragment_shader = compile_shader fragment_shader_src GL_FRAGMENT_SHADER prep in 
	close_in sc;

	let geometry_shader = ref None in
 	begin 
		match geometry_shader_file with
		    None -> ()
		|   Some gs ->  let sc = open_in_gen [Open_text] 0o777 gs in
				let size = in_channel_length sc in
				let geometry_shader_src = String.create size in
				ignore (input sc geometry_shader_src 0 size);
(*				Printf.fprintf out "\n\n%s\n\n" geometry_shader_src;*)
				geometry_shader := Some (compile_shader geometry_shader_src GL_GEOMETRY_SHADER prep);
				close_in sc;
	end;

	let program = glCreateProgram () in
	glAttachShader program vertex_shader;
	glAttachShader program fragment_shader;

	begin 
		match (!geometry_shader) with 
		    None -> ()
		|   Some sh ->  glAttachShader program sh;
				begin
					match geometry_params with
					   Some params -> Glex.glProgramParameteriEXT program GL_GEOMETRY_INPUT_TYPE_EXT params.input_type;
							  Glex.glProgramParameteriEXT program GL_GEOMETRY_OUTPUT_TYPE_EXT params.output_type;
							  Glex.glProgramParameteriEXT program GL_GEOMETRY_VERTICES_OUT_EXT params.vertices_out
					|  None -> ()
				end
	end;

	glLinkProgram program;

	let count = glGetProgrami program GL_ACTIVE_ATTRIBUTES in
	let max_length = glGetProgrami program GL_ACTIVE_ATTRIBUTE_MAX_LENGTH in
	let attributes = Array.mapi (fun i x -> let name = Glex.glGetActiveAttrib program i max_length in
						let value = GL.glGetAttribLocation program name in
	(*					Printf.fprintf out "\n\n%s  %d\n\n" name value;*)
						{name; value}) (Array.make count 0)
	in
	flush out;
	{vertex_shader; fragment_shader; geometry_shader = !geometry_shader; program; attributes; geometry_params}
	
