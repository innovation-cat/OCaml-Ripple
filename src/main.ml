(*  
Ripple simulation implementation with OCaml and glMLite                 
                                                                        
Author: Anbu Huang (wong4ever.1987@gmail.com)                          
                                                                       
Modified : 2013-09-30                                                   
*)

open GL
open Glu
open Glut
open Png_loader
open VBO
open VertArray
open Bigarray
open Ripple
open Basic
open Field
open Model

(* x |> f = f x *)
let (|>) x f = f x

let pi = lazy (4.0 *. atan 1.0)

(* let pi = lazy (pi /. 180.0)   *)
let piover180 = 0.017453292519943295

(* light 0 *)
let lightAmbient = (0.1 , 0.1 , 0.1 , 1.0) 
let lightDiffuse = (0.3 , 0.3 , 0.3 , 1.0)
let lightspecular = (0.2, 0.2, 0.2, 1.0) 
let lightposition = (3.0 , 6.0, 0.0, 1.0)
 
(* light 1 *)
let lightAmbient_1 = (0.1, 0.1, 0.1, 1.0) 
let lightDiffuse_1 = (0.4, 0.3, 0.3, 1.0)
let lightspecular_1 = (0.1, 0.1, 0.1, 1.0) 
let lightposition_1 = (-3.0, 7.0, 4.0, 1.0)

let angle_x = ref 0.0 and angle_y = ref 0.0 and angle_z = ref 0.0  (* x , y , z rotation angle *) 
let rotate = ref true 
let speed_x = ref 0.0 and speed_y = ref 0.0 and speed_z = ref 0.0  (* x , y , z rotation speed *)


let filter = ref 0 (* for different texture selection, from 0 - 5 *)
let light = ref 0 (* select whether we set switch light on *)

let blend = ref false  (* turn blending on/off *)

let hozentical = ref 0.0 (* left move hozentially *)
let vertical = ref 0.0 (* down move vertically *)

let yrot = ref 0.0 (* y rotation abgle *)
let up_down = ref 0.0 (* up and down angle *)
let xpos = ref 0.0 and ypos = ref 0.0 and zpos = ref (-6.0) (* x,y,z position *)
let y_ripple = ref 0.0  (* simulate *)

let win_width = ref 800 and win_height = ref 600

let raindrop = ref []   (* raindrop array *)
let lasttime = ref 0.0

let model_vertex_id = ref None
let model_texture_id = ref None

let field_vertex_id = ref None
let field_texture_id = ref None
let field_indices_id = ref None

let fbo = ref None

let this_frame = ref 0 and prev_frame = ref 0 and frame_duration = ref 0
let this_time = ref 0.0 and last_time = ref 0.0 and time_duration = ref 0.0

let raindrop = ref [] 

let textures = ref None

let destruct = function None -> raise Not_found | Some a -> a

let loadTexture () =  
	let loadImage filename = 
		let image_pixel, sizeX, sizeY, _, _ = Png_loader.load_img (Filename filename) in 
		(sizeX , sizeY , image_pixel, (String.concat "_" ["wall"; filename])) 
	in
	let images = [1;2;3;4;5;6] |> List.map (fun i -> Printf.sprintf "../picture/%d.png" i) |> List.map loadImage in
	let texs = glGenTextures 6 in
	textures := Some texs;
	List.iteri ( fun i (width , height , pixels, name) ->
			let base = {Texture.tex_id = texs.(i); Texture.target = GL.BindTex.GL_TEXTURE_2D; Texture.name = name} in
			let params = {Texture.init_param_2d with Texture.Texture_Params_2D.min_filter = GL.Min.GL_LINEAR;
								 Texture.Texture_Params_2D.mag_filter = GL.Mag.GL_LINEAR;
								 Texture.Texture_Params_2D.internal_format = Glex.GL_RGB;
								 Texture.Texture_Params_2D.source_format = GL.GL_RGB;
								 Texture.Texture_Params_2D.n_type = GL.GL_UNSIGNED_BYTE
				     }
			in Texture.create_texture_2d base params width height (Some pixels); ) images
;;  

let createVBO (sector : Model.section) = 
   	let triangles_list = Array.to_list sector.Model.triangles in
	let id = glGenBuffer () in
	model_vertex_id := Some id;
   	glBindBuffer GL_ARRAY_BUFFER id;
   	let rec aux1 ac = function 
		  	    [] -> ac 
                	 | (vertex1,vertex2,vertex3)::b -> 
                  	    aux1 (vertex3.z::vertex3.y::vertex3.x::vertex2.z::vertex2.y::vertex2.x::vertex1.z::vertex1.y::vertex1.x::ac) b
   	in 
	let vertices_array = Array.of_list (List.rev (aux1 [] triangles_list)) in
   	let vertices_coord = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout vertices_array 
   	in glBufferData GL_ARRAY_BUFFER (ba_sizeof vertices_coord) vertices_coord GL_STATIC_DRAW;

   	let id = glGenBuffer () in
	model_texture_id := Some id;
   	glBindBuffer GL_ARRAY_BUFFER id;
   	let 
		rec aux2 ac = function 
			          [] -> ac 
                		| (vertex1,vertex2,vertex3)::b -> 
                  			aux2 (vertex3.v::vertex3.u::vertex2.v::vertex2.u::vertex1.v::vertex1.u::ac) b
   	in 
	let textures_coord_array = Array.of_list (List.rev (aux2 [] triangles_list)) in
   	let textures_coord = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout textures_coord_array in 
	glBufferData GL_ARRAY_BUFFER (ba_sizeof textures_coord) textures_coord GL_STATIC_DRAW;
;;

let initGL () = 
	(* initialize OpenGL development enviroment *)
    	GL.glShadeModel GL.GL_SMOOTH;
    	GL.glClearColor ~r:0.0 ~g:0.0 ~b:0.0 ~a:0.5;
   
    	GL.glClearDepth ~depth:1.0;
	GL.glEnable GL.GL_DEPTH_TEST;
	GL.glDepthFunc GL.GL_LEQUAL;
    	GL.glHint ~target:GL.GL_PERSPECTIVE_CORRECTION_HINT ~mode:GL.GL_NICEST;
	
	glEnable GL.GL_LIGHTING;
	glEnable GL.GL_LIGHT0;
    	glLight (GL_LIGHT 0) (Light.GL_AMBIENT lightAmbient);
    	glLight (GL_LIGHT 0) (Light.GL_DIFFUSE lightDiffuse);
    	glLight (GL_LIGHT 0) (Light.GL_SPECULAR lightspecular);
    	glLight (GL_LIGHT 0) (Light.GL_POSITION lightposition);

	glEnable GL.GL_LIGHT1;
    	glLight (GL_LIGHT 1) (Light.GL_AMBIENT lightAmbient_1);
    	glLight (GL_LIGHT 1) (Light.GL_DIFFUSE lightDiffuse_1);
    	glLight (GL_LIGHT 1) (Light.GL_SPECULAR lightspecular_1);
    	glLight (GL_LIGHT 1) (Light.GL_POSITION lightposition_1);

    	glViewport 0 0 !win_width !win_height;
    	glMatrixMode GL_PROJECTION;    
    	glLoadIdentity ();
    	gluPerspective 60.0 ((float !win_width)/.(float !win_height)) 0.1 10000.0;
    	glMatrixMode GL_MODELVIEW;
	glLoadIdentity ();

	(* load model texture *)
	loadTexture ();
	(* create model *)

    	open_in "../picture/data.txt" |> Model.create |> createVBO;
    	
	(* load ripple *)
	Ripple.create ();
	
	(* load field *)
	let {Field.vertex_id=vid; Field.texture_id=tid; Field.indices_id=iid}=Field.create () in
	field_vertex_id := Some vid;
	field_texture_id := Some tid;
	field_indices_id := Some iid;

	(* generate frame buffer object *)
	let id = FBO.glGenFrameBuffers 1 in
	fbo := Some id.(0)
;;

(* Reshape callback function *)
let resize ~width ~height =
    	let height = if height = 0 then 1 else height in
    	
	win_width := width;
	win_height := height;

    	glViewport 0 0 width height;
    	glMatrixMode GL_PROJECTION;    
    	glLoadIdentity ();
    	gluPerspective 45.0 ((float_of_int width)/.(float_of_int height)) 0.1 10000.0;
    	glMatrixMode GL_MODELVIEW;
	glLoadIdentity ();
;;


(* Keyboard callback function for current window,                                     *)
let keyboard ~window ~key ~x ~y = 
	match key with
    	  '\113' | '\081' -> glutDestroyWindow window;
                	     exit 1;
    	| 'L' | 'l' ->  light := (!light + 1) mod 2;
                        if !light land 1 = 1 then glEnable GL_LIGHTING
                        else glDisable GL_LIGHTING
    	| 'M' | 'm' ->  filter := (!filter + 1) mod 6;
    	| 'B' | 'b' ->  blend := not (!blend); 
                   	if !blend then begin
                      		glEnable GL_BLEND;  glDisable GL_DEPTH_TEST;
                   	end 
			else begin
                      		glEnable GL_DEPTH_TEST; glDisable GL_BLEND;
                   	end 
    	| 'a' | 'A' -> xpos := !xpos -. 0.1;
    	| 'D' | 'd' -> xpos := !xpos +. 0.1;
    	| 'w' | 'W' -> ypos := !ypos +. 0.1;
    	| 'S' | 's' -> ypos := !ypos -. 0.1;
    	| _ -> ()
;;

   
let special ~key ~x ~y =
    	match key with
      	  GLUT_KEY_LEFT -> yrot := !yrot -. 1.0;
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
(*
let drawscene (texture , sector , vertex_id , texture_id , ripple_vertex_id , ripple_texture_id , ripple_indices_id , ripple_tex_2d , ripple_program , fboId)= 
    glClearColor 0.5 0.5 0.5 0.0;
    glClear [GL_COLOR_BUFFER_BIT;GL_DEPTH_BUFFER_BIT];
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
    
   (* glUnbindBuffer GL_ARRAY_BUFFER;*) 

   (* glDisable GL_TEXTURE_2D;*)
   (* glEnable GL_BLEND;*)
    let time = Sys.time () in let (ripple_texture , new_raindrop) = Ripple.render_to_texture time (!lasttime) (!raindrop) ripple_tex_2d ripple_program fboId.(0) in
    raindrop := new_raindrop;
    lasttime := time;

    glBindTexture BindTex.GL_TEXTURE_2D ripple_texture;
    (*glBlendFunc Sfactor.GL_SRC_ALPHA Dfactor.GL_ONE_MINUS_SRC_ALPHA;  
    glColor4 0.2 0.2 0.5 0.5;*)

    glEnableClientState GL_VERTEX_ARRAY;
    glEnableClientState GL_TEXTURE_COORD_ARRAY;
    glTranslate 0.0 1.0 0.0;
    
    glBindBuffer GL_ARRAY_BUFFER ripple_vertex_id;
    glVertexPointer0 3 Coord.GL_FLOAT 0;
    glBindBuffer GL_ARRAY_BUFFER ripple_texture_id;
    glTexCoordPointer0 2 Coord.GL_FLOAT 0;

    glBindBuffer GL_ELEMENT_ARRAY_BUFFER ripple_indices_id;
    glDrawElements0 GL_TRIANGLES (63*63*6) (Elem.GL_UNSIGNED_SHORT);  
    glDisableClientState GL_VERTEX_ARRAY;
    glDisableClientState GL_TEXTURE_COORD_ARRAY;
    
    glUnbindBuffer GL_ARRAY_BUFFER; 
    glUnbindBuffer GL_ELEMENT_ARRAY_BUFFER;
  (*  glDisable GL_BLEND;*)
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
*)
let display () = 
	(* Display function *)
    	glClear [GL_COLOR_BUFFER_BIT ; GL_DEPTH_BUFFER_BIT];
    	glLoadIdentity ();  
    	
	glClearColor 0.0 0.4 0.7 1.0;

    	glRotate (360.0 -. !up_down) 1.0 0.0 0.0;
    	glRotate !yrot 0.0 1.0 0.0;
    	glTranslate !xpos !ypos !zpos;         (* Move Left 1.5 Units And Into The Screen 6.0 *)
	
	glEnable GL_TEXTURE_2D;
	let tex = destruct !textures in
    	glBindTexture BindTex.GL_TEXTURE_2D tex.(!filter); 
    
	let m_v_id = destruct !model_vertex_id in
    	glBindBuffer GL_ARRAY_BUFFER m_v_id;
    	glVertexPointer0 3 Coord.GL_FLOAT 0;

	let m_t_id = destruct !model_texture_id in
    	glBindBuffer GL_ARRAY_BUFFER m_t_id;
    	glTexCoordPointer0 2 Coord.GL_FLOAT 0;
   
    	glEnableClientState GL_VERTEX_ARRAY;
    	glEnableClientState GL_TEXTURE_COORD_ARRAY; 
    
    	glDrawArrays GL_TRIANGLES 0 (28*3);
	
	glDisable GL_TEXTURE_2D;
	glDisableClientState GL_VERTEX_ARRAY;
	glDisableClientState GL_TEXTURE_COORD_ARRAY;
	glUnbindBuffer GL_ARRAY_BUFFER;

	let fbo_id = destruct (!fbo) in
	raindrop := Ripple.render_to_texture (!this_time) (!last_time) (!raindrop) fbo_id;

	let fvid = destruct (!field_vertex_id) in
	let ftid = destruct (!field_texture_id) in
	let fiid = destruct (!field_indices_id) in
	Ripple.render fvid ftid fiid;

	glutSwapBuffers ();
;;

let idlefunc () =
	if !this_frame = 0 then begin
		prev_frame := Glut.glutGet(Glut.GLUT_ELAPSED_TIME);
		this_frame := !prev_frame
	end
	else begin
		prev_frame := !this_frame;
		this_frame := Glut.glutGet(Glut.GLUT_ELAPSED_TIME)
	end;
	
	frame_duration := !this_frame - !prev_frame;

	this_time := (float !this_frame) /. 10000.0;
	last_time := (float !prev_frame) /. 10000.0;
	time_duration := (float !frame_duration) /. 10000.0;
	
	glutPostRedisplay ();
;;

(* entry point *)
let () = 
    	ignore (glutInit Sys.argv);

    	glutInitDisplayMode [GLUT_RGBA ; GLUT_DOUBLE ; GLUT_ALPHA ; GLUT_DEPTH];
 
    	glutInitWindowSize !win_width !win_height;
 
    	glutInitWindowPosition ~x:100 ~y:100;
  
    	let window = glutCreateWindow ~title:"ocaml-ripple demo" in
   
    	initGL (); 
    
    	glutDisplayFunc display;
   
    	glutIdleFunc ~idle:idlefunc;
  
    	glutKeyboardFunc (keyboard ~window);

    	glutSpecialFunc ~special;

    	glutReshapeFunc resize;

    	glutMainLoop ();
;;
