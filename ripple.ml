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
open VertArray

let planesize = 6;;
let texturesize = 64;

(****************************************************************************************)
(* create vertices list, we need to transform list to array, use Array.of_list function *)
(* be careful, we must reverse list to get correct output                               *)
let rec createVertices cellspace i j ac = 
    if i=64 then ac
    else if j=64 then createVertices cellspace (i+1) 0 ac
    else createVertices cellspace i (j+1) ((i *. cellspace)::(0.0)::(j *. cellspace)::ac)
;;
(****************************************************************************************)

    
(****************************************************************************************)
(* create texture coord list, we need to transform to array, use Array.of_list function *)
(* be careful, we must reverse list to get correct output                               *)
let rec createTextureCoord cellspace i j ac = 
    if i=64 then ac
    else if j=64 then createTextureCoord cellspace (i+1) 0 ac
    else createTextureCoord cellspace i (j+1)  ((float i /. cellspace)::(float j /. cellspace)::ac)
;;
(****************************************************************************************)

let init () = 
    let cellspace = float planesize /. float texturesize in  
    let vertices = Array.of_list (List.rev (createVertices cellspae 0 0 [])) in  
    let vertex_id = glGenBuffer () in 
    glBindBuffer GL_ARRAY_BUFFER vertex_id;
    glBufferData GL_ARRAY_BUFFER (ba_sizeof vertices) vertices GL_STATIC_DRAW;
    
    let texcoords = Array.of_list (List.rev (createTextureCoord cellspace 0 0 [])) in
    let texture_id = glGenBuffer () in 
    glBindBuffer GL_ARRAY_BUFFER texture_id;
    glBUfferData GL_ARRAY_BUFFER (ba_sizeof texture) texture GL_STATIC_DRAW;

    let indices = Array.of_list (List.rev (createIndices cellspace 0 0 [])) in
    let indices = glGenBuffer () in
    glBindBuffer GL_ARRAY_BUFFER indices;
    glBufferData GL_ARRAY_BUFFER (ba_sizeof in) 

let createVBO () =  
    let vertex_id = glGenBuffer () in
    glBindBuffer GL_ARRAY.BUFFER vertexid;
    let 
;;
let drawplane () = 
    
