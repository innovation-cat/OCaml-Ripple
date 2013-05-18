#include <GL/gl.h> 
#include <GL/glext.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/bigarray.h>

#define t_value CAMLprim value

t_value ml_glgenframebuffers(value _n)
{
    CAMLparam1(_n);
    CAMLlocal1(mlids);
    int i, n;
    n = Int_val(_n);
    GLuint *fboIds;
    fboIds = malloc(n * sizeof(GLuint));
    for (i=0; i<n; i++) fboIds[i] = 0;
    glGenFrameBuffersARB(n, fboIds);
    // TODO: is it possible that only one fail?
    //       if so what to do with the other id's ?
    for (i=0; i<n; i++)
      if (fboIds[i] == 0)
      { if (i >= 1) glDeleteFrameBuffersARB(i, fboIds);
        free(fboIds);
        caml_failwith("glGenBuffers");
      }
    mlids = caml_alloc(n, 0);
    for (i=0; i<n; i++)
    Store_field(mlids, i, Val_long(fboIds[i]));
    free(fboIds);
    CAMLreturn(mlids);
}

t_value ml_gldeleteframebuffer(value id)
{
   GLuint fboId = Long_val(id);
   glDeleteBuffersARB(1 , &fboId);
   return Val_unit;
}

t_value ml_glbindframebuffer(value _frame_buffer_object_target_arb , value id)
{
   GLenum frame_buffer_object_target_arb;
   #include "frame_buffer_object_target_arb.inc.c"
   glBindFrameBufferARB(frame_buffer_object_target_arb, Long_val(id));
   return ((intnat)1);
}

t_value ml_glunbindframebuffer(value _frame_buffer_object_target_arb)
{
   
   GLenum frame_buffer_object_target_arb;
   #include "frame_buffer_object_target_arb.inc.c"
   glBindFrameBufferARB(frame_buffer_object_target_arb,0);
   return ((intnat)1);
}

t_value ml_glframebuffertexture2d(value _frame_buffer_object_target_arb, 
                                  value _frame_buffer_object_attachment_arb,
                                  value _frame_buffer_object_texture_target_arb, 
                                  value texture, 
                                  value level)
{
   GLenum frame_buffer_object_target_arb;
   GLenum frame_buffer_object_attachment_arb;
   GLenum frame_buffer_object_texture_target_arb;
   #include "frame_buffer_object_target_arb.inc.c"
   #include "frame_buffer_object_attachment_arb.inc.c"
   #include "frame_buffer_object_texture_target_arb.inc.c"
   glFrameBufferTexture2DARB(frame_buffer_object_target_arb, 
                             frame_buffer_object_attachment_arb, 
                             frame_buffer_object_texture_target_arb,
                             Long_val(texture), 
                             Int_val(level));
   return ((intnat)1);  
}

