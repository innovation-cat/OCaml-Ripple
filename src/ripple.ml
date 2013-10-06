open GL
open VertArray
open VBO
open Basic
open FBO

exception Shader_Error of string

module WaveSource = Basic.WaveSource;;
module GlobalParameter = Basic.GlobalParameter;;

let para = Basic.init_global_params;;
let cn = open_out "out.txt"
let random_Int_Max = (1 lsl 30) - 1;;
let random_Int32_Max = (1 lsl 31) -1;;
let random_Int64_Max = (1 lsl 63) - 1;;

type t = {
		mutable wavesource : Texture.texture;
		mutable wavefield  : Texture.texture;
	 }

let waveFieldSize = Basic.init_global_params.GlobalParameter.waveFieldSize;;

let create_texture () =
    	let base = {Texture.tex_id = GL.glGenTexture (); Texture.target = GL.BindTex.GL_TEXTURE_2D; Texture.name = "wavefield"} in
	let params = {Texture.init_param_2d with Texture.Texture_Params_2D.min_filter = GL.Min.GL_NEAREST; 
						 Texture.Texture_Params_2D.mag_filter = GL.Mag.GL_NEAREST;
						 Texture.Texture_Params_2D.wrap_s = GL.GL_CLAMP;
						 Texture.Texture_Params_2D.wrap_t = GL.GL_CLAMP;
						 Texture.Texture_Params_2D.internal_format = Glex.GL_RGBA32F;
						 Texture.Texture_Params_2D.source_format = GL.GL_RGBA;
						 Texture.Texture_Params_2D.n_type = GL.GL_FLOAT;
		     }
	in
	Texture.create_texture_2d base params waveFieldSize waveFieldSize None;
	
    	let base = {Texture.tex_id = GL.glGenTexture (); Texture.target = GL.BindTex.GL_TEXTURE_1D; Texture.name = "wavesource"} in
	let params = {Texture.init_param_1d with Texture.Texture_Params_1D.min_filter = GL.Min.GL_LINEAR; 
						 Texture.Texture_Params_1D.mag_filter = GL.Mag.GL_LINEAR;
						 Texture.Texture_Params_1D.wrap_s = GL.GL_CLAMP;
						 Texture.Texture_Params_1D.wrap_t = GL.GL_CLAMP;
						 Texture.Texture_Params_1D.internal_format = Glex.GL_RGBA32F;
						 Texture.Texture_Params_1D.source_format = GL.GL_RGBA;
						 Texture.Texture_Params_1D.n_type = GL.GL_FLOAT;
		     }
	in
	Texture.create_texture_1d base params 8 None;
;;


let create_shader () =
	let evaluate_vertex_shader_file = "../shader/evaluate.vs" in
	let evaluate_fragment_shader_file = "../shader/evaluate.fs" in
	let shader_info = Glsl_shader.create evaluate_vertex_shader_file evaluate_fragment_shader_file None None None in
	Glsl_shader.insert_shader_list "evaluate" shader_info;

	let evaluate_vertex_shader_file = "../shader/render.vs" in
	let evaluate_fragment_shader_file = "../shader/render.fs" in
	let shader_info = Glsl_shader.create evaluate_vertex_shader_file evaluate_fragment_shader_file None None None in
	Glsl_shader.insert_shader_list "render" shader_info;
;;

let create () = 
	create_texture ();
	create_shader ();
;;

(* parse each ripple source *)
let rec parse_source time dtime ac = function
      	    [] -> ac
    	| a::b -> if time -. a.WaveSource.initT > 7.0 && a.WaveSource.enabled then 
                  	parse_source time dtime ac b  (* this ripple has end, remove *)
                  else if not a.WaveSource.enabled then begin  (* raindrop is falling *)
                 	a.WaveSource.height <- a.WaveSource.height -. (a.WaveSource.dropVel *. dtime +. 0.5 *. para.GlobalParameter.gravity *. dtime *. dtime);
                 	a.WaveSource.dropVel <- a.WaveSource.dropVel +. para.GlobalParameter.gravity *. dtime ;
                 	if a.WaveSource.height < 0.0 then begin 
				a.WaveSource.enabled <- true; (* rain drop turns into ripple *) 
				a.WaveSource.initT <- time 
			end;
                 	parse_source time dtime (a::ac) b
                      end
              	  else begin 
                 	a.WaveSource.a <- para.GlobalParameter.amplitude;
                 	a.WaveSource.lambda <- para.GlobalParameter.lambda;
                 	a.WaveSource.omega <- para.GlobalParameter.omega;
                 	a.WaveSource.velocity <- para.GlobalParameter.waveVelocity;
                 	parse_source time dtime (a::ac) b
              	  end
;;

let rec add_raindrop ac = 
    	Random.self_init ();
    	if List.length ac = para.GlobalParameter.dropSamples then ac
    	else 
         	let raindrop = {
                           WaveSource.a = para.GlobalParameter.amplitude;
                           WaveSource.initT = 1000000000.0;
                           WaveSource.lambda = para.GlobalParameter.lambda;
                           WaveSource.omega = para.GlobalParameter.omega;
                           WaveSource.posX = float (Random.int random_Int_Max) /. (float random_Int_Max) *. para.GlobalParameter.poolSize;
                           WaveSource.posY = float (Random.int random_Int_Max) /. (float random_Int_Max) *. para.GlobalParameter.poolSize;
                           WaveSource.velocity = para.GlobalParameter.waveVelocity;
                           WaveSource.phases = (float (Random.int random_Int_Max) /. (float random_Int_Max)) *. (para.GlobalParameter.maxPhase -. para.GlobalParameter.minPhase) +. para.GlobalParameter.minPhase;
                           WaveSource.height = para.GlobalParameter.minHeight +. (float (Random.int random_Int_Max) /. (float random_Int_Max)) *. (para.GlobalParameter.maxHeight -. para.GlobalParameter.minHeight);
                           WaveSource.dropVel = para.GlobalParameter.minSpeed +. (float (Random.int random_Int_Max) /. (float random_Int_Max)) *. (para.GlobalParameter.maxSpeed -. para.GlobalParameter.minSpeed);
                           WaveSource.color = [| ((float (Random.int random_Int_Max) /. (float random_Int_Max))) *. 0.5 +. 0.5;
                                      ((float (Random.int random_Int_Max) /. (float random_Int_Max)))*.0.5 +. 0.5;
                                      ((float (Random.int random_Int_Max) /. (float random_Int_Max)))*.0.5 +. 0.5;
                                      ((float (Random.int random_Int_Max) /. (float random_Int_Max)))*.0.5 +. 0.5;
                                   |];
                           WaveSource.enabled = false 
                                                
                        } 
         	in add_raindrop (raindrop::ac)
;;

let rec render_1d_texture ac = function 
      	  [] -> (List.rev ac , List.length ac)
    	| a::b -> if a.WaveSource.enabled then 
                 render_1d_texture (a.WaveSource.phases::a.WaveSource.velocity::a.WaveSource.omega::a.WaveSource.lambda::a.WaveSource.a::a.WaveSource.posY::a.WaveSource.posX::a.WaveSource.initT::ac) b
              else render_1d_texture ac b
;;

let prepareWaveSourceTexture time lasttime raindrop = 
    	let dtime = time -. lasttime in
    	let tmp_raindrop = parse_source time dtime [] raindrop in
    	let new_raindrop = if List.length tmp_raindrop < para.GlobalParameter.dropSamples then add_raindrop tmp_raindrop 
                       	   else tmp_raindrop 
	in 
    	let (ripple_list, ripple_count) = render_1d_texture [] new_raindrop in
    	let ripple_array = Array.of_list ripple_list in
    	(ripple_array , ripple_count , new_raindrop)
;;
    
(***********************************************************************************************************)
let render_to_texture time lasttime raindrop fbo_id = 
try
	let Texture.Texture_2D (base, params) = Texture.Resource_Map.find "wavefield" (!Texture.texture_list) in    
	glBindFrameBuffer GL_FRAMEBUFFER fbo_id;
    	glFrameBufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D base.Texture.tex_id 0; 

	let Texture.Texture_1D (source_base, source_params) = Texture.Resource_Map.find "wavesource" (!Texture.texture_list) in
	glEnable GL.GL_TEXTURE_1D;
	glBindTexture source_base.Texture.target source_base.Texture.tex_id;  
    	let (source , count , new_raindrop) = prepareWaveSourceTexture time lasttime raindrop in
(*
	WaveSource.print new_raindrop;
	Printf.printf "\n\n";
*)
	let source_bigarray = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout source in 
	if count<=0 then
	    	Glex.glTexImage1DNoPixels TexTarget.GL_TEXTURE_1D 0 Glex.GL_RGBA32F 8 GL.GL_RGBA GL.GL_FLOAT
	else
		Glex.glTexImage1DWithPixels TexTarget.GL_TEXTURE_1D 0 source_params.Texture.Texture_Params_1D.internal_format (count*2) source_params.Texture.Texture_Params_1D.source_format source_params.Texture.Texture_Params_1D.n_type source_bigarray;
    	
	let shader = Glsl_shader.Resource_Map.find "evaluate" (!Glsl_shader.shader_list) in
	let program = shader.Glsl_shader.program in
	glUseProgram program;
    	let loc = glGetUniformLocation program "source" in
    	glUniform1i loc 0;
    	let loc = glGetUniformLocation program "n" in
    	glUniform1i loc count;
    	let loc = glGetUniformLocation program "InvN" in
    	glUniform1f loc (1.0 /. float (count*2-1));
    	let loc = glGetUniformLocation program "poolSize" in
    	glUniform1f loc para.GlobalParameter.poolSize;
    	let loc = glGetUniformLocation program "time" in
    	glUniform1f loc time;
    	let loc = glGetUniformLocation program "Lambda1" in
    	glUniform1f loc para.GlobalParameter.lambda1;
    	let loc = glGetUniformLocation program "Lambda2" in
    	glUniform1f loc para.GlobalParameter.lambda2;

    	let (view0 , view1 , view2 , view3) = glGetInteger4 Get.GL_VIEWPORT in 
	let quadsize = float (para.GlobalParameter.waveFieldSize) in
    	glViewport 0 0 para.GlobalParameter.waveFieldSize para.GlobalParameter.waveFieldSize;
    	glClear [GL_COLOR_BUFFER_BIT ; GL_DEPTH_BUFFER_BIT];
    	glClearColor 0.0 0.0 0.0 0.0;
    	glMatrixMode GL_PROJECTION;
    	glPushMatrix();
    	glLoadIdentity();
    	glOrtho 0.0 quadsize quadsize 0.0 (-1.0) 1.0;
    	glMatrixMode GL_MODELVIEW ;
   	glPushMatrix ();
    	glLoadIdentity ();
	
    	glBegin GL_QUADS;
    	glTexCoord2 0.0 1.0;  glVertex2 0.0 0.0;
    	glTexCoord2 0.0 0.0;  glVertex2 0.0 quadsize;
    	glTexCoord2 1.0 0.0;  glVertex2 quadsize quadsize;
    	glTexCoord2 1.0 1.0;  glVertex2 quadsize 0.0;
    	glEnd();

    	glMatrixMode GL_PROJECTION;
    	glPopMatrix ();
    	glMatrixMode GL_MODELVIEW;
    	glPopMatrix ();
    	glViewport view0 view1 view2 view3;
   
    	glUnuseProgram ();
    	glUnBindFrameBuffer GL_FRAMEBUFFER;
	glDisable GL.GL_TEXTURE_1D;
    	new_raindrop;
with
	e -> raise e
;;    

let render field_vertex_id field_texture_id field_indices_id = 
	let Texture.Texture_2D (base, params) = Texture.Resource_Map.find "wavefield" (!Texture.texture_list) in    
	glBindTexture base.Texture.target base.Texture.tex_id;
	
	let shader = Glsl_shader.Resource_Map.find "render" (!Glsl_shader.shader_list) in
	let program = shader.Glsl_shader.program in
	glUseProgram program;

	glDisable GL.GL_TEXTURE_2D;

	let loc = glGetUniformLocation program "height" in
	glUniform1i loc 0;
	let loc = glGetUniformLocation program "dx" in
	glUniform1f loc (1.0 /. (float (para.GlobalParameter.waveFieldSize - 1)));
	let loc = glGetUniformLocation program "dy" in
	glUniform1f loc (1.0 /. (float (para.GlobalParameter.waveFieldSize - 1)));
	let loc = glGetUniformLocation program "dpx" in
	glUniform1f loc (para.GlobalParameter.poolSize /. (float (para.GlobalParameter.waveFieldSize - 1)));
	let loc = glGetUniformLocation program "dpy" in
	glUniform1f loc  (para.GlobalParameter.poolSize /. (float (para.GlobalParameter.waveFieldSize - 1)));
	let loc = glGetUniformLocation program "LightColor" in
	glUniform3f loc 0.8 0.8 0.8;

    	glEnableClientState GL_VERTEX_ARRAY;
	glEnableClientState GL_TEXTURE_COORD_ARRAY;

	glBindBuffer GL_ARRAY_BUFFER field_vertex_id;
	glVertexPointer0 3 Coord.GL_FLOAT 0;
	glBindBuffer GL_ARRAY_BUFFER field_texture_id;
	glTexCoordPointer0 2 Coord.GL_FLOAT 0;

	glBindBuffer GL_ELEMENT_ARRAY_BUFFER field_indices_id;
	glDrawElements0 GL_TRIANGLES 390150 Elem.GL_UNSIGNED_INT;

	glDisableClientState GL_VERTEX_ARRAY;
	glDisableClientState GL_TEXTURE_COORD_ARRAY;

	glUnbindBuffer GL_ARRAY_BUFFER;
	glUnbindBuffer GL_ELEMENT_ARRAY_BUFFER;

	glUnuseProgram ()
;;
