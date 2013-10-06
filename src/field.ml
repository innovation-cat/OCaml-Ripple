(*****************************************************************************)
(* create field                                                              *)
(*                                                                           *)
(*           cellspace                                                       *)
(*            |___|________________________                                  *)
(*            /   /   /   /   / ......    /                                  *)
(*	     /___/___/___/___/___________/                                   *)
(*	    /   /   /   /   / ......    /                                    *)
(*         /___/___/___/___/___________/                                     *)
(*        /                           /                                      *)
(*       /  .....................    /                                       *)
(*      /                           /                                        *)
(*     /___________________________/                                         *)
(*    /   /   /   /   / .....     /                                          *)
(*   /___/___/___/___/___________/                                           *)
(*                                                                           *)
(*                                                                           *)
(*                                                                           *)
(*                                                                           *)
(* Date:   2013-04-29                                                        *) 
(*****************************************************************************)

open Basic

type t = {
		vertex_id  : VBO.vbo_id;
		texture_id : VBO.vbo_id;
		indices_id : VBO.vbo_id;
	 } 

let waveFieldSize =  Basic.init_global_params.GlobalParameter.waveFieldSize ;;

let poolSize = Basic.init_global_params.GlobalParameter.poolSize ;;

let cellspace = poolSize /. (float waveFieldSize);;

let size2 = poolSize /. 2.0 ;;

(* create vertices list *)
let rec createVertices i j ac = 
    	if i = waveFieldSize then ac
	else if j = waveFieldSize then 
		createVertices (i+1) 0 ac
    	else 
		createVertices i (j+1) ((float i *. cellspace -. size2)::(0.0)::(float j *. cellspace -. size2)::ac)
;;

    
(* create texture coord list *)
let rec createTextureCoord i j ac = 
    	if i = waveFieldSize then ac
    	else if j = waveFieldSize then 
		createTextureCoord (i+1) 0 ac
    	else 
		createTextureCoord i (j+1)  ((float i /. (float waveFieldSize -. 1.0))::(float j /. (float waveFieldSize -. 1.0))::ac)
;;


(* create indices list *)
let rec createIndices i j ac = 
    	if i = (waveFieldSize-1) then ac
    	else if j = (waveFieldSize-1) then 
		createIndices (i+1) 0 ac
    	else 
		createIndices i (j+1)  (((i+1)*waveFieldSize+j+1)::(i*waveFieldSize+j+1)::((i+1)*waveFieldSize+j)::(i*waveFieldSize+j+1)::((i+1)*waveFieldSize+j)::(i*waveFieldSize+j)::ac)
;;


let create () = 
	let vertices = Array.of_list (List.rev (createVertices 0 0 [])) in  
    	let vertices_array = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout vertices in
    	let vertex_id = VBO.glGenBuffer () in 
    	VBO.glBindBuffer VBO.GL_ARRAY_BUFFER vertex_id;
	(*Printf.printf "vertices array: %d %d\n" (VBO.ba_sizeof vertices_array) (List.length (Array.to_list vertices));*)
    	VBO.glBufferData VBO.GL_ARRAY_BUFFER (VBO.ba_sizeof vertices_array) vertices_array VBO.GL_STATIC_DRAW;

	let texcoords = Array.of_list (List.rev (createTextureCoord 0 0 [])) in
    	let texcoords_array = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout texcoords in
    	let texture_id = VBO.glGenBuffer () in 
    	VBO.glBindBuffer VBO.GL_ARRAY_BUFFER texture_id;
	(*Printf.printf "texcoords array: %d\n" (VBO.ba_sizeof texcoords_array);*)
    	VBO.glBufferData VBO.GL_ARRAY_BUFFER (VBO.ba_sizeof texcoords_array) texcoords_array VBO.GL_STATIC_DRAW;

    	let indices = Array.of_list (List.rev (createIndices 0 0 [])) in
    	let indices_array = Bigarray.Array1.of_array Bigarray.int Bigarray.c_layout indices in
    	let indices_id = VBO.glGenBuffer () in
    	VBO.glBindBuffer VBO.GL_ARRAY_BUFFER indices_id;
	(*Printf.printf "indices array: %d\n" (VBO.ba_sizeof indices_array);*)
    	VBO.glBufferData VBO.GL_ARRAY_BUFFER (VBO.ba_sizeof indices_array) indices_array VBO.GL_STATIC_DRAW;
    	{vertex_id; texture_id; indices_id}
;;  
