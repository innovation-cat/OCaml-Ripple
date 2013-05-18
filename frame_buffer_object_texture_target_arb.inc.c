  static const GLenum conv_frame_buffer_object_texture_target_arb_table[] = {
    GL_TEXTURE_1D,
    GL_TEXTURE_2D,
    GL_TEXTURE_3D
  };
  frame_buffer_object_texture_target_arb = conv_frame_buffer_object_texture_target_arb_table[Int_val(_frame_buffer_object_texture_target_arb)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (frame_buffer_object_texture_target_arb == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif

