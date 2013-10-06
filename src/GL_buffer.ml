open GL
open VBO

type field_type = VEC3F | VEC2F | FLOAT | VEC4F | MAT4X4 | UNSIGNED_INTEGER | RGB | LAST_FIELD

type decl = {
		mutable field        : field_type;
		mutable binding_name : string;
	    }

let calc_decl_attribute = function
	   VEC3F -> (12, 3, VertArray.VAttr.GL_FLOAT)
	|  VEC2F -> (8, 2, VertArray.VAttr.GL_FLOAT)
	|  FLOAT -> (4, 1, VertArray.VAttr.GL_FLOAT)
	|  VEC4F -> (16, 4, VertArray.VAttr.GL_FLOAT)
	|  MAT4X4 -> (64, 16, VertArray.VAttr.GL_FLOAT)
	|  UNSIGNED_INTEGER -> (4, 1, VertArray.VAttr.GL_UNSIGNED_INT)
	|  _ -> raise (Failure "I don't know yet.")


type vertex_buffer = { 
			mutable buf                : VBO.vbo_id;
			mutable name               : string; 
			mutable length             : int;
			mutable element_size       : int;
			mutable buffer_size        : int;
			mutable buffer_decl_array  : decl array;
		     	mutable vertex_attributes  : int array;
		     }

module Resource_Map = Map.Make (struct type t = string let compare = String.compare end)

let buffer_list = ref (Resource_Map.empty)
 
let insert_buffer_list (k:string) (v:vertex_buffer) = buffer_list := Resource_Map.add k v (!buffer_list)


let create_vertex_buffer name buffer_decl_array length usage = 
	let element_size = Array.fold_left (fun ac e -> let (size, _ , _) = calc_decl_attribute (e.field) in ac+size) 0 buffer_decl_array in
	let buffer_size = element_size * length in
	if buffer_size = 0 then raise (Failure "invalid buffer declaration.");
	let buf = VBO.glGenBuffer () in
	VBO.glBindBuffer VBO.GL_ARRAY_BUFFER buf;
	VBO.glBufferDataNull VBO.GL_ARRAY_BUFFER buffer_size usage;
	VBO.glUnbindBuffer VBO.GL_ARRAY_BUFFER;
	{buf; name; length; element_size; buffer_size; buffer_decl_array; vertex_attributes = [||]}
;;


let bind_vertex_buffer vb = VBO.glBindBuffer (VBO.GL_ARRAY_BUFFER) vb.buf;;


let bind_vertex_buffer_with_shader vb shader = 
	VBO.glBindBuffer (VBO.GL_ARRAY_BUFFER) vb.buf;
	let buffer_decl = vb.buffer_decl_array in
	let vertex_attributes = Array.make (Array.length shader.Glsl_shader.attributes) 0 in
	let offset = ref 0 in
	Array.iter (fun {Glsl_shader.name=n; Glsl_shader.value=v}-> Printf.printf "%s: %d\n" n v) shader.Glsl_shader.attributes;
	let rec search na = function [] -> -1 
				   | {Glsl_shader.name=n; Glsl_shader.value=v}::tl -> if n = na then v
										  else search na tl
	in
	Array.iteri (fun i {field; binding_name} -> Printf.printf "%s\n" binding_name;
			   let v = search binding_name (Array.to_list shader.Glsl_shader.attributes) in
			   if v <> -1 then begin
			   	vertex_attributes.(i) <- v; 
			   	Glex.glEnableVertexAttribArray v;
			   	let (x, y, z) = calc_decl_attribute field in 
			   	VertArray.glVertexAttribPointerOfs8 v y z true vb.element_size (!offset);
			   	offset := (!offset) + x
			   end;
   		    ) buffer_decl;
	vb.vertex_attributes <- vertex_attributes;
	VBO.glUnbindBuffer (VBO.GL_ARRAY_BUFFER)
;;

		
let unbind_vertex_buffer () = VBO.glUnbindBuffer (VBO.GL_ARRAY_BUFFER);;


let unbind_vertex_buffer_with_shader vb =
	Array.iteri (fun i attrib -> Glex.glDisableVertexAttribArray attrib) vb.vertex_attributes
;;


let set_vertex_buffer_data offset size data = VBO.glBufferSubData (VBO.GL_ARRAY_BUFFER) offset size (Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout data)

