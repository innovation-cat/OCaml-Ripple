#include <GL/gl.h> 
#include <GL/glext.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/bigarray.h>

#define t_value CAMLprim value
static const GLenum conv_frame_buffer_object_attachment_table[] = {
    GL_COLOR_ATTACHMENT0 
,   GL_COLOR_ATTACHMENT1
,   GL_COLOR_ATTACHMENT2
,   GL_COLOR_ATTACHMENT3
,   GL_COLOR_ATTACHMENT4
,   GL_COLOR_ATTACHMENT5
,   GL_COLOR_ATTACHMENT6
,   GL_COLOR_ATTACHMENT7
,   GL_COLOR_ATTACHMENT8 
,   GL_COLOR_ATTACHMENT9
,   GL_COLOR_ATTACHMENT10
,   GL_COLOR_ATTACHMENT11
,   GL_COLOR_ATTACHMENT12
,   GL_COLOR_ATTACHMENT13 
,   GL_COLOR_ATTACHMENT14
,   GL_COLOR_ATTACHMENT15
,   GL_DEPTH_ATTACHMENT
,   GL_STENCIL_ATTACHMENT
,   GL_DEPTH_STENCIL_ATTACHMENT
};

static const GLenum conv_frame_buffer_object_target_table[] = {
    GL_DRAW_FRAMEBUFFER,
    GL_READ_FRAMEBUFFER,
    GL_FRAMEBUFFER
};

static const GLenum conv_frame_buffer_object_texture_target_table[] = {
    GL_TEXTURE_1D,
    GL_TEXTURE_2D,
    GL_TEXTURE_3D
};

static const GLenum conv_render_buffer_object_target_table[] = { GL_RENDERBUFFER };

static const GLenum conv_render_buffer_object_internal_format_table[] = { GL_DEPTH_COMPONENT, GL_DEPTH_STENCIL };

t_value ml_glisrenderbuffer (value _id)
{
	CAMLparam1 (_id);
	GLuint id = Long_val (_id);
	GLboolean res = glIsRenderbuffer(id);
	CAMLreturn (Val_bool(res));
}

t_value ml_glgenrenderbuffers (value _n)
{
	CAMLparam1 (_n);
	CAMLlocal1 (caml_rboid);
	int n = Int_val(_n);
	GLuint * rboid = (GLuint *)malloc(n * sizeof(GLuint));
	int i=0;
	for(i=0;i<n;++i)
		rboid[i]=0;
	glGenRenderbuffers(n, rboid);
	caml_rboid = caml_alloc(n, 0);
	for(i=0;i<n;++i)
		Store_field(caml_rboid,i,Val_long(rboid[i]));
	free (rboid);
	CAMLreturn (caml_rboid);
}

t_value ml_glbindrenderbuffer(value _target, value _id)
{
	CAMLparam2 (_target, _id);
	GLuint id = Long_val(_id);
	GLenum target = conv_render_buffer_object_target_table[Int_val(_target)];
	
	glBindRenderbuffer(target, id);
	CAMLreturn (Val_unit);
}

t_value ml_glrenderbufferstorage (value _target, value _format, value _w, value _h)
{
	CAMLparam4 (_target, _format, _w, _h);
	GLenum target = conv_render_buffer_object_target_table[Int_val(_target)];
	GLenum format = conv_render_buffer_object_internal_format_table[Int_val(_format)];

	glRenderbufferStorage(target, format, Int_val(_w), Int_val(_h));
	CAMLreturn (Val_unit);
}

t_value ml_glrenderbufferstoragemultisample (value _target, value _samples, value _format, value _w, value _h)
{
	CAMLparam5 (_target, _samples, _format, _w, _h);
	GLenum target = conv_render_buffer_object_target_table[Int_val(_target)];
	GLenum format = conv_render_buffer_object_internal_format_table[Int_val(_format)];

	glRenderbufferStorageMultisample(target, Int_val(_samples), format, Int_val(_w), Int_val(_h));
	CAMLreturn (Val_unit);
}

t_value ml_gldeleterenderbuffers (value _ids)
{
	CAMLparam1 (_ids);
	int size = Wosize_val (_ids);
	GLuint * ids = (GLuint *) malloc (sizeof(GLuint) * size);
	int i;
	for(i=0; i<size; ++i)
		ids[i] = Long_val(Field(_ids, i));
	
	glDeleteRenderbuffers(size, ids);
	free(ids);
	CAMLreturn (Val_unit);
}

t_value ml_glgenframebuffers(value _n)
{
    	CAMLparam1(_n);
    	CAMLlocal1(mlids);
    	int i, n;
    	n = Int_val(_n);
    	GLuint *fboIds;
    	fboIds = malloc(n * sizeof(GLuint));
    	for(i=0; i<n; i++) fboIds[i] = 0;
    	glGenFramebuffers(n, fboIds);
    	for(i=0; i<n; i++)
      		if (fboIds[i] == 0)
      		{ if (i >= 1) glDeleteFramebuffers(i, fboIds);
        		free(fboIds);
        		caml_failwith("glGenBuffers");
      		}
    	mlids = caml_alloc(n, 0);
    	for (i=0; i<n; i++)
    		Store_field(mlids, i, Val_long(fboIds[i]));
    	free(fboIds);
    	CAMLreturn(mlids);
}

t_value ml_gldeleteframebuffer(value _id)
{
	CAMLparam1 (_id);
   	GLuint id = Long_val(_id);
   	glDeleteFramebuffers(1 , &id);
   	CAMLreturn (Val_unit);
}

t_value ml_glbindframebuffer(value _frame_buffer_object_target , value _id)
{
	CAMLparam2 (_frame_buffer_object_target, _id);
   	GLenum frame_buffer_object_target = conv_frame_buffer_object_target_table[Int_val (_frame_buffer_object_target)];
	GLuint id = Long_val(_id);

   	glBindFramebuffer(frame_buffer_object_target, id);
   	CAMLreturn (Val_unit);
}

t_value ml_glunbindframebuffer(value _frame_buffer_object_target)
{
	CAMLparam1 (_frame_buffer_object_target);
   	GLenum frame_buffer_object_target = conv_frame_buffer_object_target_table[Int_val (_frame_buffer_object_target)];
   	glBindFramebuffer(frame_buffer_object_target,0);
   	CAMLreturn (Val_unit);
}

t_value ml_glframebuffertexture2d(value _frame_buffer_object_target, 
                                  value _frame_buffer_object_attachment,
                                  value _frame_buffer_object_texture_target, 
                                  value _texture, 
                                  value _level)
{
	CAMLparam5 ( _frame_buffer_object_target,
		     _frame_buffer_object_attachment,
		     _frame_buffer_object_texture_target,
		     _texture , _level);

  	GLenum frame_buffer_object_target = conv_frame_buffer_object_target_table[Int_val (_frame_buffer_object_target)];
   	GLenum frame_buffer_object_attachment = conv_frame_buffer_object_attachment_table[Int_val (_frame_buffer_object_attachment)];
   	GLenum frame_buffer_object_texture_target = conv_frame_buffer_object_texture_target_table[Int_val (_frame_buffer_object_texture_target)];
   	
	GLuint texture = Long_val(_texture);
	GLuint level = Long_val(_level);

   	glFramebufferTexture2D( frame_buffer_object_target, 
                                frame_buffer_object_attachment, 
                                frame_buffer_object_texture_target,
                                texture, 
                                level);
   	CAMLreturn (Val_unit);
}

t_value ml_glframebuffertexture(value _frame_buffer_object_target, 
                                value _frame_buffer_object_attachment,
                                value _texture, 
                                value _level)
{
	CAMLparam4 ( _frame_buffer_object_target,
		     _frame_buffer_object_attachment,
		     _texture , _level);

  	GLenum frame_buffer_object_target = conv_frame_buffer_object_target_table[Int_val (_frame_buffer_object_target)];
   	GLenum frame_buffer_object_attachment = conv_frame_buffer_object_attachment_table[Int_val (_frame_buffer_object_attachment)];
   	
	GLuint texture = Long_val(_texture);
	GLuint level = Long_val(_level);

   	glFramebufferTexture( frame_buffer_object_target, 
                                frame_buffer_object_attachment, 
                                texture, 
                                level);
   	CAMLreturn (Val_unit);
}

t_value ml_glframebufferrenderbuffer (value _frame_buffer_object_target, 
                                      value _frame_buffer_object_attachment,
                                      value _render_buffer_object_target, 
                                      value _rbo_id )
{
	CAMLparam4 ( _frame_buffer_object_target,
		     _frame_buffer_object_attachment,
		     _render_buffer_object_target,
		     _rbo_id);

  	GLenum frame_buffer_object_target = conv_frame_buffer_object_target_table[Int_val (_frame_buffer_object_target)];
   	GLenum frame_buffer_object_attachment = conv_frame_buffer_object_attachment_table[Int_val (_frame_buffer_object_attachment)];
   	GLenum render_buffer_object_target = conv_render_buffer_object_target_table[Int_val (_render_buffer_object_target)];

	glFramebufferRenderbuffer ( frame_buffer_object_target,
				    frame_buffer_object_attachment,
				    render_buffer_object_target,
				    Long_val(_rbo_id) );

	CAMLreturn (Val_unit);

}
