open GL
open Glu
open VBO

type parameter = 
{
    mutable dropSamples : int ;
    mutable maxHeight : float;
    mutable minSpeed : float;
    mutable maxSpeed : float;
    mutable wavefieldsize : int;
    mutable poolSize : float;
    mutable amplitude : float;
    mutable lambda_para : float;
    mutable omega_para : float;
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

type wavesource = 
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
(*	vec4 Color;*)
    mutable enabled : bool 
}

let inittexture (para:parameter) =
    let texture_2d = glGenTexture () in
    let data_null = Bigarray.Genarray.create Bigarray.int8_unsigned Bigarray.c_layout (Array.make (para.wavefieldsize * para.wavefieldsize * 4) 0) in
    glBindTexture BindTex.GL_TEXTURE_2D texture_2d;
    glTexImage2D TexTarget.GL_TEXTURE_2D 0 InternalFormat.GL_RGBA para.wavefieldsize para.wavefieldsize GL_RGBA GL_FLOAT data_null;
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

let pi = 3.1415926
let init () = 
    let 
         para = {
                   amplitude = 0.2;
	           lambda_para = 0.63;
	           lambda1 = 23.1;
	           lambda2 = 0.5;
	           omega_para = 54.4;
                   waveVelocity = 3.4;
	           wavefieldsize = 64;
	           dropSamples = 100;
	           maxHeight = 300.0;
	           maxSpeed = 40.0;
	           minSpeed = 10.0;
	           maxPhase = 6.0 *. pi *. 2.0;
	           minPhase = 1.0 *. pi *. 2.0;
	           poolSize = 40.0;
	           extinctionLife = 8.0;
	           gravity = 0.5;
	           dropSound = true;
	           colorfulRain = true; 
               } 
    in
    let texture_2d , texture_1d = inittexture para in
    let program = loadshader () in
    (texture_2d , texture_1d , program , para)
;;

let render_to_texture () = 
    let (tex_2d , tex_1d , program , para) = init () in

    glEnable GL_TEXTURE_1D;
    glBindTexture BindTex.GL_TEXTURE_1D tex_1d;
    glUseProgram program;
    let loc = glGetUniformLocation program "source" in
    glUniform1i loc 0;
    let loc = glGetUniformLocation program "n" in
    glUniform1i loc 100;
    let loc = glGetUniformLocation program "InvN" in
    glUniform1f loc (1.0 /. float (100*2-1));
    let loc = glGetUniformLocation program "poolSize" in
    glUniform1f loc para.poolSize;
    let loc = glGetUniformLocation program "time" in
    glUniform1f loc 0.0;
    let loc = glGetUniformLocation program "Lambda1" in
    glUniform1f loc para.lambda1;
    let loc = glGetUniformLocation program "Lambda2" in
    glUniform1f loc para.lambda2;

    let (view0 , view1 , view2 , view3) = glGetInteger4 Get.GL_VIEWPORT in 
    glViewport 0 0 64 64;
    glClearColor 0.0 0.0 0.0 0.0;
    glClear [GL_COLOR_BUFFER_BIT ; GL_DEPTH_BUFFER_BIT];
    glMatrixMode GL_PROJECTION;
    glPushMatrix();
    glLoadIdentity();
    glOrtho 0.0 64.0 64.0 0.0 (-1.0) 1.0;
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
    glCopyTexImage2D CopyTex.GL_TEXTURE_2D 0 InternalFormat.GL_RGBA 0 0 para.wavefieldsize para.wavefieldsize 0;
    glUnbindTexture BindTex.GL_TEXTURE_2D;
;;    

