(*****************************************************************************)
(*****************************************************************************)
(* main.ml                                                                   *)
(* ==========================================================================*)
(* Ripple simulation implementation with OCaml and glMLite                   *)
(*                                                                           *)
(* AUTHOR: Anbu Huang (wong4ever.1987@gmail.com)                             *)
(*                                                                           *)
(* CREATED: 2013-04-15                                                       *) 
(*****************************************************************************)

#load "bigarray.cma"
#load "GL.cma"
#load "Glu.cma"
#load "Glut.cma"
#load "png_loader.cma"
#load "model.cma"
#load "VBO.cma"
#load "vertArray.cma"
#load "ripple.cma"

open GL
open Glu
open Glut
open Png_loader
open Model
open VBO
open VertArray
open Bigarray

let angle_x = ref 0.0 and angle_y = ref 0.0 and angle_z = ref 0.0  (* x , y , z rotation angle *) 
let rotate = ref true 
let speed_x = ref 0.0 and speed_y = ref 0.0 and speed_z = ref 0.0  (* x , y , z rotation speed *)

let lightAmbient = (0.5 , 0.5 , 0.5 , 1.0)  (* Ambient light *)
let lightDiffuse = (1.0 , 1.0 , 1.0 , 1.0)  (* diffuse light *)
let lightposition = (0.0 , 0.0 , 2.0 , 1.0) (* light position *)

let filter = ref 0 (* for different texture selection, from 0 - 5 *)
let light = ref 0 (* select whether we set switch light on *)

let blend = ref false  (* turn blending on/off *)

let hozentical = ref 0.0 (* left move hozentially *)
let vertical = ref 0.0 (* down move vertically *)

let yrot = ref 0.0 (* y rotation abgle *)
let up_down = ref 0.0 (* up and down angle *)
let xpos = ref 0.0 and ypos = ref 0.0 and zpos = ref (-6.0) (* x,y,z position *)
let y_ripple = ref 0.0  (* simulate *)

let loadTexture () =   (* Load GL Texture *)
   let load_image filename = let 
          image_data, sizeX, sizeY, _, _ = Png_loader.load_img (Filename filename) 
   in (sizeX , sizeY , image_data) in
   
   let files = [|"picture/1.png" ;
                 "picture/2.png" ;
                 "picture/3.png" ;
                 "picture/4.png" ;
                 "picture/5.png" ;
                 "picture/6.png"|] 
   in 
   
   let image_texture = Array.map load_image files in 
   
   let texture = glGenTextures 6 in Array.iteri (fun i (x, y , data) -> 
      glBindTexture BindTex.GL_TEXTURE_2D texture.(i);
      glTexImage2D TexTarget.GL_TEXTURE_2D 0 InternalFormat.GL_RGB x y GL_RGB GL_UNSIGNED_BYTE data;
      glTexParameter TexParam.GL_TEXTURE_2D (TexParam.GL_TEXTURE_MAG_FILTER Mag.GL_LINEAR);
      glTexParameter TexParam.GL_TEXTURE_2D (TexParam.GL_TEXTURE_MIN_FILTER Min.GL_LINEAR);
   ) image_texture ;
   
   texture
;;

let rec _glEnable = function
    | [] -> ()
    | a::b -> glEnable a; _glEnable b;
;;

let createVBO ~sector = 
   let triangles_list = Array.to_list sector.triangles in ();
   
   let vertex_id = glGenBuffer () in ();
   glBindBuffer GL_ARRAY_BUFFER vertex_id;
   let rec aux1 ac = function [] -> ac 
                | (vertex1,vertex2,vertex3)::b -> 
                  aux1 (vertex3.z::vertex3.y::vertex3.x::vertex2.z::vertex2.y::vertex2.x::vertex1.z::vertex1.y::vertex1.x::ac) b
   in let vertexes_array = Array.of_list (List.rev (aux1 [] triangles_list)) in
   let vertexes_coord = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout vertexes_array 
   in glBufferData GL_ARRAY_BUFFER (ba_sizeof vertexes_coord) vertexes_coord GL_STATIC_DRAW;

   let texture_id = glGenBuffer () in ();
   glBindBuffer GL_ARRAY_BUFFER texture_id;
   let rec aux2 ac = function [] -> ac 
                | (vertex1,vertex2,vertex3)::b -> 
                  aux2 (vertex3.v::vertex3.u::vertex2.v::vertex2.u::vertex1.v::vertex1.u::ac) b
   in let textures_coord_array = Array.of_list (List.rev (aux2 [] triangles_list)) in
   let textures_coord = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout textures_coord_array 
   in glBufferData GL_ARRAY_BUFFER (ba_sizeof textures_coord) textures_coord GL_STATIC_DRAW;
   (vertex_id,texture_id)
;;

let initGL () = 
(* initialize OpenGL development enviroment *)
    let texture = loadTexture () in
    let ic = open_in "picture/data.txt"  in
    let sector = setup ic in
    let (vertex_id,texture_id) = createVBO sector in
    let (ripple_vertices_id, ripple_texture_id, ripple_indices_id) = Ripple.init () in
    glShadeModel GL_SMOOTH;
    (* set background color (r,g,b,a) *)
    glClearColor ~r:0.0 ~g:0.0 ~b:0.0 ~a:0.0;
    
    (* *)
    glClearDepth ~depth:1.0;

    glLight (GL_LIGHT 1) (Light.GL_AMBIENT lightAmbient);
    glLight (GL_LIGHT 1) (Light.GL_DIFFUSE lightDiffuse);
    glLight (GL_LIGHT 1) (Light.GL_POSITION lightposition);
   
    _glEnable [GL_DEPTH_TEST ; GL_TEXTURE_2D ; GL_LIGHT1]; 
(*    glEnable ~cap:GL_DEPTH_TEST;*)
    
    glDepthFunc ~func:GL_LEQUAL;
    
    glBlendFunc Sfactor.GL_SRC_ALPHA Dfactor.GL_ONE;
    glColor4 1.0 1.0 1.0 0.5;
    
    glHint ~target:GL_PERSPECTIVE_CORRECTION_HINT ~mode:GL_NICEST;
    (texture,sector,vertex_id,texture_id, ripple_vertices_id, ripple_texture_id, ripple_indices_id)
;;

(**************************************************************************************)
(* Reshape callback function, this function will be trigger when a window is reshape  *)
let resize ~width ~height =
(* Reshpe *)
    let height = if height = 0 then 1 else height in
    
    glViewport 0 0 width height;
    
    glMatrixMode GL_PROJECTION;
    
    glLoadIdentity ();
    
    gluPerspective 45.0 ((float_of_int width)/.(float_of_int height)) 0.1 100.0;
    
    glMatrixMode GL_MODELVIEW;
;;
(**************************************************************************************)


(**************************************************************************************)
(* Keyboard callback function for current window,                                     *)
(* When a user types into the window,                                                 *)
(* each key press generating an ASCII character will generate a keyboard callback.    *)
let keyboard ~window ~key ~x ~y = match key with
    | '\113' | '\081' -> glutDestroyWindow window;
                exit 1;
    | 'L' | 'l' -> light := (!light + 1) mod 2;
                   if !light land 1 = 1 then glEnable GL_LIGHTING
                   else glDisable GL_LIGHTING;
    | 'M' | 'm' -> filter := (!filter + 1) mod 6;
    | 'B' | 'b' -> blend := not (!blend); 
                   if !blend then begin
                      glEnable GL_BLEND;  glDisable GL_DEPTH_TEST;
                   end else begin
                      glEnable GL_DEPTH_TEST; glDisable GL_BLEND;
                   end 
    | 'a' | 'A' -> xpos := !xpos -. 0.1;
    | 'D' | 'd' -> xpos := !xpos +. 0.1;
    | 'w' | 'W' -> ypos := !ypos +. 0.1;
    | 'S' | 's' -> ypos := !ypos -. 0.1;
    | _ -> ()
;;
(**************************************************************************************) 

let piover180 = 0.017453292519943295
   
let special ~key ~x ~y =
    match key with
    | GLUT_KEY_LEFT -> yrot := !yrot -. 1.0;
                       if !yrot < 0.0 then yrot := 359.0;
    | GLUT_KEY_RIGHT -> yrot := !yrot +. 1.0;
                       if !yrot > 359.0 then yrot := 0.0;
    | GLUT_KEY_PAGE_UP -> up_down := !up_down +. 1.0;
                          if !up_down > 359.0 then up_down := 0.0;
    | GLUT_KEY_PAGE_DOWN -> up_down := !up_down -. 1.0;
                            if !up_down < 0.0 then up_down := 359.0; 
    | GLUT_KEY_UP -> xpos := !xpos +. sin (!yrot *. piover180) *. 0.05;
                     zpos := !zpos +. cos (!yrot *. piover180) *. 0.05;
                     y_ripple := !y_ripple +. 20.0;
                     if !y_ripple > 359.0 then y_ripple := 0.0;
                     ypos := !ypos +. sin (!y_ripple *. piover180) *. 0.025;
    | GLUT_KEY_DOWN -> xpos := !xpos -. sin (!yrot *. piover180) *. 0.05;
                     zpos := !zpos -. cos (!yrot *. piover180) *. 0.05;
                     y_ripple := !y_ripple -. 20.0;
                     if !y_ripple < 0.0 then y_ripple := 359.0;
                     ypos := !ypos +. sin (!y_ripple *. piover180) *. 0.025;
    | _ -> ();
;; 

let drawscene (texture , sector , vertex_id , texture_id , ripple_vertex_id , ripple_texture_id , ripple_indices_id)= 
    glRotate (360.0 -. !up_down) 1.0 0.0 0.0;
    glRotate !yrot 0.0 1.0 0.0;
    glTranslate !xpos !ypos !zpos;         (* Move Left 1.5 Units And Into The Screen 6.0 *)
    
    glBindTexture BindTex.GL_TEXTURE_2D texture.(!filter); 
    
    glBindBuffer GL_ARRAY_BUFFER vertex_id;
    glVertexPointer0 3 Coord.GL_FLOAT 0;

    glBindBuffer GL_ARRAY_BUFFER texture_id;
    glTexCoordPointer0 2 Coord.GL_FLOAT 0;
   
    glEnableClientState GL_VERTEX_ARRAY;
    glEnableClientState GL_TEXTURE_COORD_ARRAY; 
    
    glDrawArrays GL_TRIANGLES 0 (28*3); 
    (*
    Array.iter (fun (vertex1 , vertex2 , vertex3) ->  
      glBegin GL_TRIANGLES;
      glNormal3 0.0 0.0 1.0;
      glTexCoord2 vertex1.u vertex1.v;
      glVertex3 vertex1.x vertex1.y vertex1.z;

      glTexCoord2 vertex2.u vertex2.v;
      glVertex3 vertex2.x vertex2.y vertex2.z;

      glTexCoord2 vertex3.u vertex3.v;
      glVertex3 vertex3.x vertex3.y vertex3.z;
      glEnd ();
    ) sector.triangles;
    *)
    
    glDisableClientState GL_VERTEX_ARRAY;
    glDisableClientState GL_TEXTURE_COORD_ARRAY; 
   (* glUnbindBuffer GL_ARRAY_BUFFER;*) 
  
    glDisable GL_TEXTURE_2D;
    glEnable GL_BLEND;
    glBlendFunc Sfactor.GL_SRC_ALPHA Dfactor.GL_ONE_MINUS_SRC_ALPHA;  
    glColor4 0.2 0.2 0.5 0.5;

    glEnableClientState GL_VERTEX_ARRAY;
    glTranslate 0.0 1.0 0.0;
    
    glBindBuffer GL_ARRAY_BUFFER ripple_vertex_id;
    glVertexPointer0 3 Coord.GL_FLOAT 0;

    glBindBuffer GL_ELEMENT_ARRAY_BUFFER ripple_indices_id;
    glDrawElements0 GL_TRIANGLES (63*63*6) (Elem.GL_UNSIGNED_SHORT);  
    glDisableClientState GL_VERTEX_ARRAY;
    glUnbindBuffer GL_ARRAY_BUFFER; 
    glDisable GL_BLEND;
    glEnable GL_TEXTURE_2D;
    glColor4 1.0 1.0 1.0 0.0; 
(* 
    glDisable GL_TEXTURE_2D;
    glEnable GL_BLEND;
    glBlendFunc Sfactor.GL_SRC_ALPHA Dfactor.GL_ONE_MINUS_SRC_ALPHA;  
    glColor4 0.2 0.2 0.5 0.5;
    glBegin GL_QUADS;
    glVertex3 (-3.0) 1.0 (-3.0);
    glVertex3 (-3.0) 1.0 (3.0);
    glVertex3 (3.0) 1.0 (3.0);
    glVertex3 (3.0) 1.0 (-3.0);
    glEnd ();
    glDisable GL_BLEND;
    glEnable GL_TEXTURE_2D;
    glColor4 1.0 1.0 1.0 0.0; *)
    
;;   

let display (texture , sector, vertex_id , texture_id , ripple_vertex_id , ripple_texture_id , ripple_indices_id) = fun () ->
(* Display function *)
    glClear [GL_COLOR_BUFFER_BIT ; GL_DEPTH_BUFFER_BIT];
    glLoadIdentity ();  
    
    drawscene (texture , sector , vertex_id , texture_id , ripple_vertex_id , ripple_texture_id , ripple_indices_id);

    glutSwapBuffers ();
;;

let () = 
    ignore (glutInit Sys.argv);

    glutInitDisplayMode [GLUT_RGBA ; GLUT_DOUBLE ; GLUT_ALPHA ; GLUT_DEPTH];
 
    glutInitWindowSize 1200 800;
 
    glutInitWindowPosition ~x:100 ~y:100;
  
    let window = glutCreateWindow ~title:"ocaml-opengl demo" in
   
    let (texture , sector , vertex_id , texture_id, ripple_vertex_id , ripple_texture_id , ripple_indices_id) = initGL () in
    
    glutDisplayFunc (display (texture , sector , vertex_id , texture_id , ripple_vertex_id , ripple_texture_id , ripple_indices_id));
   
    glutIdleFunc ~idle:glutPostRedisplay;
  
    glutKeyboardFunc (keyboard ~window);

    glutSpecialFunc ~special;

    glutReshapeFunc resize;

    glutMainLoop ();
;;
