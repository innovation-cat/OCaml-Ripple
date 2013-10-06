(* for more detail about frame buffer object, please refer to *)
type fbo_id

(* for more detail about render buffer object, please refer to *)
type rbo_id

type render_buffer_object_target = GL_RENDERBUFFER

type render_buffer_object_internal_format = GL_DEPTH_COMPONENT | GL_DEPTH_STENCIL

type frame_buffer_object_target = 
       GL_DRAW_FRAMEBUFFER
    |  GL_READ_FRAMEBUFFER
    |  GL_FRAMEBUFFER

type frame_buffer_object_texture_target = 
        GL_TEXTURE_1D
     |  GL_TEXTURE_2D
     |  GL_TEXTURE_3D

type frame_buffer_object_attachment = 
        GL_COLOR_ATTACHMENT0 
     |  GL_COLOR_ATTACHMENT1
     |  GL_COLOR_ATTACHMENT2
     |  GL_COLOR_ATTACHMENT3
     |  GL_COLOR_ATTACHMENT4
     |  GL_COLOR_ATTACHMENT5
     |  GL_COLOR_ATTACHMENT6
     |  GL_COLOR_ATTACHMENT7
     |  GL_COLOR_ATTACHMENT8 
     |  GL_COLOR_ATTACHMENT9
     |  GL_COLOR_ATTACHMENT10
     |  GL_COLOR_ATTACHMENT11
     |  GL_COLOR_ATTACHMENT12
     |  GL_COLOR_ATTACHMENT13 
     |  GL_COLOR_ATTACHMENT14
     |  GL_COLOR_ATTACHMENT15
     |  GL_DEPTH_ATTACHMENT
     |  GL_STENCIL_ATTACHMENT
     |  GL_DEPTH_STENCIL_ATTACHMENT

(* determine if a name corresponds to a render buffer object *)
external glIsRenderBuffer : rbo_id -> bool = "ml_glisrenderbuffer" 

(* generate render buffer objects names *)
external glGenRenderBuffers : int -> rbo_id array = "ml_glgenrenderbuffers"



(* bind a renderbuffer to a renderbuffer object *)
external glBindRenderBuffer : render_buffer_object_target -> rbo_id -> unit  = "ml_glbindrenderbuffer"



external glRenderbufferStorage :  render_buffer_object_target -> render_buffer_object_internal_format -> int -> int -> unit = "ml_glrenderbufferstorage"

external glRenderbufferStorageMultisample :  render_buffer_object_target -> int -> render_buffer_object_internal_format -> int -> int -> unit = "ml_glrenderbufferstoragemultisample"

external glDeleteRenderbuffers :  rbo_id array -> unit = "ml_gldeleterenderbuffers"

(* generate frame buffer objects *)
external glGenFrameBuffers : int -> fbo_id array = "ml_glgenframebuffers"

(* delete specific frame buffer object *)
external glDeleteFrameBuffer : fbo_id -> unit = "ml_gldeleteframebuffer" "noalloc"

(* delete frame buffer objects *)
let glDeleteFrameBuffers ~fbos = Array.iter (fun fbo -> glDeleteFrameBuffer fbo) fbos


      
external glBindFrameBuffer : frame_buffer_object_target -> fbo_id -> unit = "ml_glbindframebuffer" "noalloc"

external glUnBindFrameBuffer : frame_buffer_object_target -> unit = "ml_glunbindframebuffer" "noalloc"


external glFrameBufferTexture2D : frame_buffer_object_target -> frame_buffer_object_attachment -> frame_buffer_object_texture_target -> GL.texture_id -> int -> unit = "ml_glframebuffertexture2d" "noalloc"

external glFrameBufferTexture : frame_buffer_object_target -> frame_buffer_object_attachment -> GL.texture_id -> int -> unit = "ml_glframebuffertexture"

 
external glFramebufferRenderbuffer : frame_buffer_object_target -> frame_buffer_object_attachment -> render_buffer_object_target -> rbo_id -> unit = "ml_glframebufferrenderbuffer"
