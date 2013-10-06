(*****************************************************************************)
(* model.ml                                                                  *)
(*                                                                           *)
(* Read model data,  use "data.txt" as input                                 *)
(* 	  ______________                                                     *)
(*	 /|            /|                                                    *)
(*	/_|___________/ |                                                    *)
(*	| |___________|_|                                                    *)
(*	| /           | /                                                    *)
(*	|/____________|/                                                     *)
(*                                                                           *)
(* Author: Anbu Huang (wong4ever.1987@gmail.com)                             *)
(*                                                                           *)
(* Date: 2013-04-15                                                          *) 
(*****************************************************************************)


(* vertex coordinate: (x,y,z)                                                *)
(* texture coordinate: (u,v)                                                 *)
type vertex =
{
      	x : float;
      	y : float;
      	z : float;
      	u : float;
      	v : float;
}


type triangle = vertex * vertex * vertex

(* section information: number of triangles, triangles array *)
type section =
{
      	triangles_num : int;  (* number of triangles *)
      	triangles : triangle array 
}


let parse line =
    	let size = String.length line in 
    	let rec aux i lefthandside = 
      		if line.[i]=' ' || line.[i]='\t' then 
       			if lefthandside then aux (i+1) lefthandside
        		else aux (i-1) lefthandside
      		else i
    	in
    	let left = aux 0 true in let right =  aux (size-1) false in 
    	(String.sub line left (right-left+1))
;;

(* remove white space both in left-hand size and right-hand size, also,*)
(* we don't take blank line and comment line into consideration        *)
let read_str ic = 
	let rec aux () = 
      		let line = input_line ic in 
         	if line="" then aux ()
         	else if line.[0]='/' then aux ()
         	else parse line 
    	in
    	aux ()
;;

let create ic = 
	let triangles_num = 
      		let 
        		line = read_str ic 
      		in Scanf.sscanf line "NUMPOLLIES %d" (fun x -> x) 
    	in 
    	let triangles = Array.init triangles_num (fun i ->
        	let create_vertex () = 
            		let line = read_str ic in 
            		Scanf.sscanf line "%f %f %f %f %f" (fun x y z u v -> {x; y; z; u; v})
        	in 
        	let vertex1 = create_vertex () in
        	let vertex2 = create_vertex () in
        	let vertex3 = create_vertex () in
        	(vertex1 , vertex2 , vertex3) ) 
 	in
        {triangles_num; triangles}
;;
