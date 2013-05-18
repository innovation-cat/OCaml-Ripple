  static const GLenum conv_frame_buffer_object_attachment_arb_table[] = {
    GL_COLOR_ATTACHMENT0_EXT,
    GL_DEPTH_ATTACHMENT_EXT,
    GL_STENCIL_ATTACHMENT_EXT,
    GL_DEPTH_STENCIL_ATTACHMENT
  };
  frame_buffer_object_attachment_arb = conv_frame_buffer_object_attachment_arb_table[Int_val(_frame_buffer_object_attachment_arb)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (frame_buffer_object_attachment_arb == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif

