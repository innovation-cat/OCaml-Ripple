
type fbo_id

external glGenFrameBuffers : n:int -> fbo_id array = "ml_glgenframebuffers"

external glDeleteFrameBuffer : fbo_id -> unit = "ml_gldeleteframebuffer" "noalloc"

val glDeleteFrameBuffers : fbos:fbo_id array -> unit

type frame_buffer_object_target = 
    |  GL_DRAW_FRAMEBUFFER_EXT
    |  GL_READ_FRAMEBUFFER_EXT
    |  GL_FRAMEBUFFER_EXT

external glBindFrameBuffer : target:frame_buffer_object_target -> fbo:fbo_id -> unit = "ml_glbindframebuffer" "noalloc"

external glUnBindFrameBuffer : target:frame_buffer_object_target -> unit = "ml_glunbindframebuffer" "noalloc"

type frame_buffer_object_texture_target = 
     |  GL_TEXTURE_1D
     |  GL_TEXTURE_2D
     |  GL_TEXTURE_3D

type frame_buffer_object_attachment =
     |  GL_COLOR_ATTACHMENT0_EXT
     |  GL_DEPTH_ATTACHMENT_EXT
     |  GL_STENCIL_ATTACHMENT_EXT
     |  GL_DEPTH_STENCIL_ATTACHMENT

type texture_id = private int

external glFrameBufferTexture2D : target:frame_buffer_object_target -> attachment:frame_buffer_object_attachment -> texturetarget:frame_buffer_object_texture_target -> texture:texture_id -> level:int -> unit = "ml_glframebuffertexture2d" "noalloc"


