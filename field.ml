(*****************************************************************************)
(*****************************************************************************)
(* ripple.ml                                                                 *)
(* ==========================================================================*)
(* Foreword:                                                                 *)
(* Implement ripple simulation                                               *)
(*                                                                           *)
(* ==========================================================================*)
(* AUTHOR: Anbu Huang (wong4ever.1987@gmail.com)                             *)
(*                                                                           *)
(* CREATED: 2013-04-29                                                       *) 
(*****************************************************************************)

open GL
open Glu
open VBO
open Bigarray

let planesize = 6;;
let texturesize = 64;;

(*********************************************************************************************)
(* create vertices list, we need to transform list to array, use Array.of_list function      *)
(* be careful, we must reverse list to get correct output                                    *)
let rec createVertices cellspace i j ac = 
    if i=64 then ac
    else if j=64 then createVertices cellspace (i+1) 0 ac
    else createVertices cellspace i (j+1) ((float i *. cellspace -. 3.0)::(0.0)::(float j *. cellspace -. 3.0)::ac)
;;
(*********************************************************************************************)

    
(*********************************************************************************************)
(* create texture coord list, we need to transform list to array, use Array.of_list function *)
(* be careful, we must reverse list to get correct output                                    *)
let rec createTextureCoord cellspace i j ac = 
    if i=64 then ac
    else if j=64 then createTextureCoord cellspace (i+1) 0 ac
    else createTextureCoord cellspace i (j+1)  ((float i /. cellspace)::(float j /. cellspace)::ac)
;;
(*********************************************************************************************)


(*********************************************************************************************)
(* create indices list, we need to transform list to array, use Array.of_list function       *)
(* be careful, we must reverse list to get correct output                                    *)
let rec createIndices cellspace i j ac = 
    if i=63 then ac
    else if j=63 then createIndices cellspace (i+1) 0 ac
    else createIndices cellspace i (j+1)  (((i+1)*64+j+1)::(i*64+j+1)::((i+1)*64+j)::(i*64+j+1)::((i+1)*64+j)::(i*64+j)::ac)
;;
(*********************************************************************************************)


let init () = 
    let cellspace = (float planesize) /. (float texturesize -. 1.0) in  
    let vertices = Array.of_list (List.rev (createVertices cellspace 0 0 [])) in  
    let vertices_array = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout vertices in
    let vertex_id = glGenBuffer () in 
    glBindBuffer GL_ARRAY_BUFFER vertex_id;
    glBufferData GL_ARRAY_BUFFER (ba_sizeof vertices_array) vertices_array GL_STATIC_DRAW;
    
    let texcoords = Array.of_list (List.rev (createTextureCoord cellspace 0 0 [])) in
    let texcoords_array = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout texcoords in
    let texture_id = glGenBuffer () in 
    glBindBuffer GL_ARRAY_BUFFER texture_id;
    glBufferData GL_ARRAY_BUFFER (ba_sizeof texcoords_array) texcoords_array GL_STATIC_DRAW;

    let indices = Array.of_list (List.rev (createIndices cellspace 0 0 [])) in
    let indices_array = Bigarray.Array1.of_array Bigarray.int16_unsigned Bigarray.c_layout indices in
    let indices_id = glGenBuffer () in
    glBindBuffer GL_ARRAY_BUFFER indices_id;
    glBufferData GL_ARRAY_BUFFER (ba_sizeof indices_array) indices_array GL_STATIC_DRAW;

    (vertex_id , texture_id , indices_id)
;;  
