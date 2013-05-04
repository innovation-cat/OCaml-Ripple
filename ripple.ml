open GL
open Glu
open VBO

module Parameter = struct
type t = 
{
    mutable dropSamples : int ;
    mutable maxHeight : float;
            minHeight : float;
    mutable minSpeed : float;
    mutable maxSpeed : float;
    mutable wavefieldsize : int;
    mutable poolSize : float;
    mutable amplitude : float;
    mutable lambda : float;
    mutable omega : float;
    mutable lambda1 : float;
    mutable lambda2 : float;
    mutable maxPhase : float;
    mutable waveVelocity : float;
    mutable extinctionLife : float;
    mutable gravity : float;
    mutable minPhase : float;
    mutable colorfulRain : bool;
	(*static LARGE_INTEGER LastCounter : ;*)
    mutable dropSound : bool 
}
end;;

module Wavesource = struct
type t = 
{
    mutable initT : float;
    mutable posX : float;
    mutable posY : float;
    mutable a : float;
    mutable lambda : float;
    mutable omega : float;
    mutable velocity : float;
    mutable height : float;
    mutable dropVel : float;
    mutable phases : float;
    mutable color : float array;
    mutable enabled : bool 
}
end;;

let cn = open_out "out.txt"

let pi = 3.1415926

let 
    para = {
                   Parameter.amplitude = 0.2;
	           Parameter.lambda = 0.63;
	           Parameter.lambda1 = 23.1;
	           Parameter.lambda2 = 0.5;
	           Parameter.omega = 54.4;
                   Parameter.waveVelocity = 3.4;
	           Parameter.wavefieldsize = 64;
	           Parameter.dropSamples = 50;
	           Parameter.maxHeight = 300.0;
                   Parameter.minHeight = 5.0;
	           Parameter.maxSpeed = 40.0;
	           Parameter.minSpeed = 10.0;
	           Parameter.maxPhase = 6.0 *. pi *. 2.0;
	           Parameter.minPhase = 1.0 *. pi *. 2.0;
	           Parameter.poolSize = 6.0;
	           Parameter.extinctionLife = 8.0;
	           Parameter.gravity = 0.5;
	           Parameter.dropSound = true;
	           Parameter.colorfulRain = true; 
           }
;; 

let inittexture () =
    let texture_2d = glGenTexture () in
    let data_null = Bigarray.Genarray.create Bigarray.int8_unsigned Bigarray.c_layout [|para.Parameter.wavefieldsize*4;para.Parameter.wavefieldsize*4|] in
    glBindTexture BindTex.GL_TEXTURE_2D texture_2d;
    glTexImage2D TexTarget.GL_TEXTURE_2D 0 InternalFormat.GL_RGBA para.Parameter.wavefieldsize para.Parameter.wavefieldsize GL_RGBA GL_FLOAT data_null;
    glTexParameter TexParam.GL_TEXTURE_2D  (TexParam.GL_TEXTURE_MIN_FILTER  Min.GL_LINEAR);
    glTexParameter TexParam.GL_TEXTURE_2D  (TexParam.GL_TEXTURE_MAG_FILTER  Mag.GL_LINEAR);
    glTexParameter TexParam.GL_TEXTURE_2D  (TexParam.GL_TEXTURE_WRAP_S  GL_CLAMP);
    glTexParameter TexParam.GL_TEXTURE_2D  (TexParam.GL_TEXTURE_WRAP_T  GL_CLAMP);

    let texture_1d = glGenTexture () in
    glBindTexture BindTex.GL_TEXTURE_1D  texture_1d;
    glTexParameter TexParam.GL_TEXTURE_1D  (TexParam.GL_TEXTURE_MIN_FILTER  Min.GL_NEAREST);
    glTexParameter TexParam.GL_TEXTURE_1D  (TexParam.GL_TEXTURE_MAG_FILTER  Mag.GL_NEAREST); 
    glTexParameter TexParam.GL_TEXTURE_2D  (TexParam.GL_TEXTURE_WRAP_S  GL_CLAMP);
    glTexParameter TexParam.GL_TEXTURE_2D  (TexParam.GL_TEXTURE_WRAP_T  GL_CLAMP);
    (texture_2d , texture_1d)
;;

let loadshader () =
    let vertexShader = glCreateShader GL_VERTEX_SHADER in
    let fragmentShader = glCreateShader GL_FRAGMENT_SHADER in
    let sc = open_in "./shader/evaluate.vs" in
    let size = in_channel_length sc in
    let vertex_shader_src = String.create size in
    ignore (input sc vertex_shader_src 0 size);
    glShaderSource vertexShader vertex_shader_src;

    let sc = open_in "./shader/evaluate.fs" in
    let size = in_channel_length sc in
    let fragment_shader_src = String.create size in
    ignore (input sc fragment_shader_src 0 size);
    glShaderSource fragmentShader fragment_shader_src;
    
    glCompileShader vertexShader;
    glCompileShader fragmentShader;
    
    let shader_program = glCreateProgram () in 
    glAttachShader shader_program vertexShader;
    glAttachShader shader_program fragmentShader;

    glLinkProgram shader_program;
    shader_program
;;

let init () = 
    let texture_2d , texture_1d = inittexture () in
    let program = loadshader () in
    (texture_2d , texture_1d , program)
;;

(* parse each ripple source *)
let rec parse_source time dtime ac = function
      [] -> ac
    | a::b -> if time -. a.Wavesource.initT > 7.0 && a.Wavesource.enabled then 
                 parse_source time dtime ac b  (* this ripple has end *)
              else if not a.Wavesource.enabled then begin  (* raindrop is falling *)
                 a.Wavesource.height <- a.Wavesource.height -. (a.Wavesource.dropVel *. dtime +. 0.5 *. para.Parameter.gravity *. dtime *. dtime);
                 a.Wavesource.dropVel <- a.Wavesource.dropVel +. para.Parameter.gravity *. dtime ;
                 if a.Wavesource.height < 0.0 then begin a.Wavesource.enabled <- true ; a.Wavesource.initT <- time end;
                 parse_source time dtime (a::ac) b;
                 end
              else begin  (* raindrop touch water-level, and from ripple *)
                 a.Wavesource.a <- para.Parameter.amplitude;
                 a.Wavesource.lambda <- para.Parameter.lambda;
                 a.Wavesource.omega <- para.Parameter.omega;
                 a.Wavesource.velocity <- para.Parameter.waveVelocity;
                 parse_source time dtime (a::ac) b;
              end
;;

let rec add_raindrop time dtime ac = 
    Random.self_init ();
    if List.length ac = para.Parameter.dropSamples then ac
    else 
         let raindrop = {
                           Wavesource.a = para.Parameter.amplitude;
                           Wavesource.initT = 1000000000.0;
                           Wavesource.lambda = para.Parameter.lambda;
                           Wavesource.omega = para.Parameter.omega;
                           Wavesource.posX = float (Random.int ((1 lsl 30) - 1)) /. (float (1 lsl 30)) *. para.Parameter.poolSize;
                           Wavesource.posY = float (Random.int ((1 lsl 30) - 1)) /. (float (1 lsl 30)) *. para.Parameter.poolSize;
                           Wavesource.velocity = para.Parameter.waveVelocity;
                           Wavesource.phases = (float (Random.int ((1 lsl 30) - 1)) /. (float (1 lsl 30))) *. (para.Parameter.maxPhase -. para.Parameter.minPhase) +. para.Parameter.minPhase;
                           Wavesource.height = para.Parameter.minHeight +. (float (Random.int ((1 lsl 30) - 1)) /. (float (1 lsl 30))) *. (para.Parameter.maxHeight -. para.Parameter.minHeight);
                           Wavesource.dropVel = para.Parameter.minSpeed +. (float (Random.int ((1 lsl 30) - 1)) /. (float (1 lsl 30))) *. (para.Parameter.maxSpeed -. para.Parameter.minSpeed);
                           Wavesource.color = [| ((float (Random.int (1 lsl 29)) /. (float (1 lsl 29)))) *. 0.5 +. 0.5;
                                      ((float (Random.int (1 lsl 29)) /. (float (1 lsl 29))))*.0.5 +. 0.5;
                                      ((float (Random.int (1 lsl 29)) /. (float (1 lsl 29))))*.0.5 +. 0.5;
                                      ((float (Random.int (1 lsl 29)) /. (float (1 lsl 29))))*.0.5 +. 0.5;
                                   |];
                           Wavesource.enabled = false 
                                                
                        } 
         in add_raindrop time dtime (raindrop::ac)
;;

let rec render_1d_texture ac = function 
      [] -> (List.rev ac , List.length ac)
    | a::b -> if a.Wavesource.enabled then 
                 render_1d_texture (a.Wavesource.phases::a.Wavesource.velocity::a.Wavesource.omega::a.Wavesource.lambda::a.Wavesource.a::a.Wavesource.posY::a.Wavesource.posX::a.Wavesource.initT::ac) b
              else render_1d_texture ac b
;;

let prepareWavesourceTexture time lasttime raindrop = 
    let dtime = time -. lasttime in
    let tmp_raindrop = parse_source time dtime [] raindrop in
    let new_raindrop = if List.length tmp_raindrop < para.Parameter.dropSamples then add_raindrop time dtime tmp_raindrop 
                       else tmp_raindrop in 
    let (ripple_list, ripple_count) = render_1d_texture [] new_raindrop in
    let ripple_array = Array.of_list ripple_list in
    (ripple_array , ripple_count , new_raindrop)
;;
    
(***********************************************************************************************************)
(*                                                                                                         *)
let render_to_texture time lasttime raindrop tex_2d program = 
    let (source , count , new_raindrop) = prepareWavesourceTexture time lasttime raindrop in

    glUseProgram program;
    let loc = glGetUniformLocation program "source" in
    glUniform1fv loc source;
    let loc = glGetUniformLocation program "n" in
    glUniform1i loc (count/8);
    let loc = glGetUniformLocation program "InvN" in
    glUniform1f loc (1.0 /. float (count*2-1));
    let loc = glGetUniformLocation program "poolSize" in
    glUniform1f loc para.Parameter.poolSize;
    let loc = glGetUniformLocation program "time" in
    glUniform1f loc time;
    let loc = glGetUniformLocation program "Lambda1" in
    glUniform1f loc para.Parameter.lambda1;
    let loc = glGetUniformLocation program "Lambda2" in
    glUniform1f loc para.Parameter.lambda2;

    glDisable GL_TEXTURE_2D;
    let (view0 , view1 , view2 , view3) = glGetInteger4 Get.GL_VIEWPORT in 
    glViewport 0 0 para.Parameter.wavefieldsize para.Parameter.wavefieldsize;
    glClearColor 1.0 0.0 0.0 0.0;
(*    glClear [GL_COLOR_BUFFER_BIT ; GL_DEPTH_BUFFER_BIT];
  *)  glPushAttrib [Attrib.GL_COLOR_BUFFER_BIT ; Attrib.GL_PIXEL_MODE_BIT];
    glDrawBuffer DrawBuffer.GL_BACK;
    glReadBuffer ReadBuffer.GL_BACK; 
    glMatrixMode GL_PROJECTION;
    glPushMatrix();
    glLoadIdentity();
    glOrtho 0.0 (float para.Parameter.wavefieldsize) (float para.Parameter.wavefieldsize) 0.0 (-1.0) 1.0;
    glMatrixMode GL_MODELVIEW ;
    glPushMatrix ();
    glLoadIdentity ();
	
    glBegin GL_QUADS;
    glTexCoord2 0.0 1.0;  glVertex2 0.0 0.0;
    glTexCoord2 0.0 0.0;  glVertex2 0.0 64.0;
    glTexCoord2 1.0 0.0;  glVertex2 64.0 64.0;
    glTexCoord2 1.0 1.0;  glVertex2 64.0 0.0;
    glEnd();
    

    glMatrixMode GL_PROJECTION;
    glPopMatrix ();
    glMatrixMode GL_MODELVIEW;
    glPopMatrix ();
    glViewport view0 view1 view2 view3;

    glEnable GL_TEXTURE_2D;
    glBindTexture BindTex.GL_TEXTURE_2D tex_2d;
    glCopyTexImage2D CopyTex.GL_TEXTURE_2D 0 InternalFormat.GL_RGBA 0 0 para.Parameter.wavefieldsize para.Parameter.wavefieldsize 0;
    glUnbindTexture2D ();
    glPopAttrib ();
    (tex_2d , new_raindrop);
;;    

