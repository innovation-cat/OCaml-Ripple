#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <GL/gl.h>
#include <GL/glext.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/bigarray.h>

#define t_value CAMLprim value

t_value ml_glshadersources (value shader, value cnt, value str, value len)
{
	CAMLparam4 (shader, cnt, str, len);
	CAMLlocal2 (some_str, some_len);
	if(Is_long(str) && Is_long(len))
	{
		glShaderSource(Long_val(shader), Int_val(cnt), NULL, NULL);
	}
	else if(Is_long(str))
	{
		some_len = Field(len,0);
		int size = Wosize_val(some_len);
		int * length = (int *)malloc(sizeof(int) * size);
		int i;
		for(i=0;i<size;++i)
		{
			length[i] = Int_val(Field(some_len, i));
		}
		glShaderSource(Long_val(shader), Int_val(cnt), NULL, length);
		for(i=0;i<size;++i)	
			free(length);
	}
	else if(Is_long(len))
	{
		some_str = Field(str,0);
		int size = Wosize_val(some_str);
		int i=0;
		char ** vp;
		vp = (char **)malloc(sizeof(char *) * size);
		for(i=0;i<size;++i)
		{
			char *tmp = String_val(Field (some_str,i));
			vp[i] = (char *)malloc(sizeof(char) * strlen(tmp));
			memcpy(vp[i], tmp, strlen(tmp));
		}
		glShaderSource(Long_val(shader), Int_val(cnt), vp, NULL);
		for(i=0;i<size;++i)
			free(vp[i]);
		free(vp);
	}
	else
	{	
		some_str = Field(str,0);
		some_len = Field(len,0);
		int size = Wosize_val(some_str);
		int i=0;
		char ** vp;
		vp = (char **)malloc(sizeof(char *) * size);
		for(i=0;i<size;++i)
		{
			char *tmp = String_val(Field (some_str,i));
			vp[i] = (char *)malloc(sizeof(char) * strlen(tmp));
			memcpy(vp[i], tmp, strlen(tmp));
		}
		int size2 = Wosize_val(some_len);
		int * length = (int *)malloc(sizeof(int) * size2);
		for(i=0;i<size2;++i)
		{
			length[i] = Int_val(Field(some_len, i));
		}
		glShaderSource(Long_val(shader), Int_val(cnt), vp, length);
		for(i=0;i<size;++i)
			free(vp[i]);
		free(vp);
		for(i=0;i<size2;++i)	
			free(length);
	}
	CAMLreturn (Val_unit);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static const GLenum conv_pixel_data_format_table[] = {
    GL_COLOR_INDEX,
    GL_RED,
    GL_GREEN,
    GL_BLUE,
    GL_ALPHA,
    GL_RGB,
    GL_RGBA,
    GL_LUMINANCE,
    GL_LUMINANCE_ALPHA,
};

static const GLenum conv_pixel_data_type_table[] = {
    GL_UNSIGNED_BYTE,
    GL_BYTE,
    GL_BITMAP,
    GL_UNSIGNED_SHORT,
    GL_SHORT,
    GL_UNSIGNED_INT,
    GL_INT,
    GL_FLOAT,
    GL_UNSIGNED_BYTE_3_3_2,
    GL_UNSIGNED_BYTE_2_3_3_REV,
    GL_UNSIGNED_SHORT_5_6_5,
    GL_UNSIGNED_SHORT_5_6_5_REV,
    GL_UNSIGNED_SHORT_4_4_4_4,
    GL_UNSIGNED_SHORT_4_4_4_4_REV,
    GL_UNSIGNED_SHORT_5_5_5_1,
    GL_UNSIGNED_SHORT_1_5_5_5_REV,
    GL_UNSIGNED_INT_8_8_8_8,
    GL_UNSIGNED_INT_8_8_8_8_REV,
    GL_UNSIGNED_INT_10_10_10_2,
    GL_UNSIGNED_INT_2_10_10_10_REV,
};

static const GLenum conv_target_2d_table[] = {
    GL_TEXTURE_2D,
    GL_PROXY_TEXTURE_2D,
    GL_TEXTURE_CUBE_MAP_POSITIVE_X,
    GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
    GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
    GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Z,
    GL_PROXY_TEXTURE_CUBE_MAP,
};

static const GLenum conv_target_3d_table[] = {
    GL_TEXTURE_3D,
    GL_PROXY_TEXTURE_3D,
};

static const GLenum conv_target_1d_table[] = {
    GL_TEXTURE_1D,
    GL_PROXY_TEXTURE_1D,
};

static const GLenum conv_internal_format_table[] = {
    GL_ALPHA,
    GL_ALPHA4,
    GL_ALPHA8,
    GL_ALPHA12,
    GL_ALPHA16,
    GL_COMPRESSED_ALPHA,
    GL_COMPRESSED_LUMINANCE,
    GL_COMPRESSED_LUMINANCE_ALPHA,
    GL_COMPRESSED_INTENSITY,
    GL_COMPRESSED_RGB,
    GL_COMPRESSED_RGBA,
    GL_DEPTH_COMPONENT,
    GL_DEPTH_COMPONENT16,
    GL_DEPTH_COMPONENT24,
    GL_DEPTH_COMPONENT32,
    GL_LUMINANCE,
    GL_LUMINANCE4,
    GL_LUMINANCE8,
    GL_LUMINANCE12,
    GL_LUMINANCE16,
    GL_LUMINANCE_ALPHA,
    GL_LUMINANCE4_ALPHA4,
    GL_LUMINANCE6_ALPHA2,
    GL_LUMINANCE8_ALPHA8,
    GL_LUMINANCE12_ALPHA4,
    GL_LUMINANCE12_ALPHA12,
    GL_LUMINANCE16_ALPHA16,
    GL_INTENSITY,
    GL_INTENSITY4,
    GL_INTENSITY8,
    GL_INTENSITY12,
    GL_INTENSITY16,
    GL_R3_G3_B2,
    GL_RGB,
    GL_RGB4,
    GL_RGB5,
    GL_RGB8,
    GL_RGB10,
    GL_RGB12,
    GL_RGB16,
    GL_RGBA,
    GL_RGBA2,
    GL_RGBA4,
    GL_RGB5_A1,
    GL_RGBA8,
    GL_RGB10_A2,
    GL_RGBA12,
    GL_RGBA16,
    GL_SLUMINANCE,
    GL_SLUMINANCE8,
    GL_SLUMINANCE_ALPHA,
    GL_SLUMINANCE8_ALPHA8,
    GL_SRGB,
    GL_SRGB8,
    GL_SRGB_ALPHA,
    GL_SRGB8_ALPHA8,
    GL_RG16F,
    GL_R16F,
    GL_RGBA16F,
    GL_RGBA32F,
};

t_value ml_glteximage1dwithpixels_native (
                   value _target_1d,
                   value level,
                   value _internal_format,
                   value width,
                   value _pixel_data_format,
                   value _pixel_data_type,
                   value pixels )
{
	CAMLparam5 (_target_1d, level, _internal_format, width, _pixel_data_format);
	CAMLxparam2 (_pixel_data_type, pixels);
	GLenum pixel_data_format = conv_pixel_data_format_table[Int_val(_pixel_data_format)];
    	GLenum pixel_data_type = conv_pixel_data_type_table[Int_val(_pixel_data_type)];
    	GLenum target_1d = conv_target_1d_table[Int_val(_target_1d)];
    	GLint  internal_format = conv_internal_format_table[Int_val(_internal_format)]; 
	
	
    	glTexImage1D( target_1d, Int_val(level), internal_format, Int_val(width), 0, pixel_data_format, pixel_data_type, (const GLvoid *) Data_bigarray_val(pixels) );
		
	CAMLreturn (Val_unit);
}

t_value ml_glteximage1dwithpixels_bytecode( value * argv, int argn )
{ 
	return ml_glteximage1dwithpixels_native( argv[0], argv[1], argv[2], argv[3],
                                     argv[4], argv[5], argv[6]); 
}

t_value ml_glteximage1dnopixels_native (
                   value _target_2d,
                   value level,
                   value _internal_format,
                   value width,
                   value _pixel_data_format,
                   value _pixel_data_type)
{
	CAMLparam5 (_target_2d, level, _internal_format, width, _pixel_data_format);
	CAMLxparam1 (_pixel_data_type);
	GLenum pixel_data_format = conv_pixel_data_format_table[Int_val(_pixel_data_format)];
    	GLenum pixel_data_type = conv_pixel_data_type_table[Int_val(_pixel_data_type)];
    	GLenum target_2d = conv_target_2d_table[Int_val(_target_2d)];
    	GLint  internal_format = conv_internal_format_table[Int_val(_internal_format)]; 
	
    	glTexImage1D( target_2d, Int_val(level), internal_format, Int_val(width), 0, pixel_data_format, pixel_data_type, NULL );
	CAMLreturn (Val_unit);
}

t_value ml_glteximage1dnopixels_bytecode( value * argv, int argn )
{ 
	return ml_glteximage1dnopixels_native( argv[0], argv[1], argv[2], argv[3],
                                     argv[4], argv[5]); 
}






t_value ml_glteximage2dwithpixels_native (
                   value _target_2d,
                   value level,
                   value _internal_format,
                   value width,
                   value height,
                   value _pixel_data_format,
                   value _pixel_data_type,
                   value pixels )
{
	CAMLparam5 (_target_2d, level, _internal_format, width, height);
	CAMLxparam3 (_pixel_data_format, _pixel_data_type, pixels);
	GLenum pixel_data_format = conv_pixel_data_format_table[Int_val(_pixel_data_format)];
    	GLenum pixel_data_type = conv_pixel_data_type_table[Int_val(_pixel_data_type)];
    	GLenum target_2d = conv_target_2d_table[Int_val(_target_2d)];
    	GLint  internal_format = conv_internal_format_table[Int_val(_internal_format)]; 
	
	
    	glTexImage2D( target_2d, Int_val(level), internal_format, Int_val(width), Int_val(height), 0, pixel_data_format, pixel_data_type, (const GLvoid *) Data_bigarray_val(pixels) );
		
	CAMLreturn (Val_unit);
}

t_value ml_glteximage2dwithpixels_bytecode( value * argv, int argn )
{ 
	return ml_glteximage2dwithpixels_native( argv[0], argv[1], argv[2], argv[3],
                                     argv[4], argv[5], argv[6], argv[7] ); 
}

t_value ml_glteximage2dnopixels_native (
                   value _target_2d,
                   value level,
                   value _internal_format,
                   value width,
                   value height,
                   value _pixel_data_format,
                   value _pixel_data_type)
{
	CAMLparam5 (_target_2d, level, _internal_format, width, height);
	CAMLxparam2 (_pixel_data_format, _pixel_data_type);
	GLenum pixel_data_format = conv_pixel_data_format_table[Int_val(_pixel_data_format)];
    	GLenum pixel_data_type = conv_pixel_data_type_table[Int_val(_pixel_data_type)];
    	GLenum target_2d = conv_target_2d_table[Int_val(_target_2d)];
    	GLint  internal_format = conv_internal_format_table[Int_val(_internal_format)]; 
	
    	glTexImage2D( target_2d, Int_val(level), internal_format, Int_val(width), Int_val(height), 0, pixel_data_format, pixel_data_type, NULL );
	CAMLreturn (Val_unit);
}

t_value ml_glteximage2dnopixels_bytecode( value * argv, int argn )
{ 
	return ml_glteximage2dnopixels_native( argv[0], argv[1], argv[2], argv[3],
                                     argv[4], argv[5], argv[6]); 
}

t_value ml_glteximage3dwithpixels_native (
                   value _target_3d,
                   value level,
                   value _internal_format,
                   value width,
                   value height,
		   value depth,
                   value _pixel_data_format,
                   value _pixel_data_type,
                   value pixels )
{
	CAMLparam5 (_target_3d, level, _internal_format, width, height);
	CAMLxparam4 (depth, _pixel_data_format, _pixel_data_type, pixels);
	GLenum pixel_data_format = conv_pixel_data_format_table[Int_val(_pixel_data_format)];
    	GLenum pixel_data_type = conv_pixel_data_type_table[Int_val(_pixel_data_type)];
    	GLenum target_3d = conv_target_3d_table[Int_val(_target_3d)];
    	GLint  internal_format = conv_internal_format_table[Int_val(_internal_format)]; 
	
	
    	glTexImage3D( target_3d, Int_val(level), internal_format, Int_val(width), Int_val(height), Int_val(depth), 0, pixel_data_format, pixel_data_type, (const GLvoid *) Data_bigarray_val(pixels) );
		
	CAMLreturn (Val_unit);
}

t_value ml_glteximage3dwithpixels_bytecode( value * argv, int argn )
{ 
	return ml_glteximage3dwithpixels_native( argv[0], argv[1], argv[2], argv[3],
                                     argv[4], argv[5], argv[6], argv[7], argv[8]); 
}

t_value ml_glteximage3dnopixels_native (
                   value _target_3d,
                   value level,
                   value _internal_format,
                   value width,
                   value height,
		   value depth,
                   value _pixel_data_format,
                   value _pixel_data_type)
{
	CAMLparam5 (_target_3d, level, _internal_format, width, height);
	CAMLxparam3 (depth, _pixel_data_format, _pixel_data_type);
	GLenum pixel_data_format = conv_pixel_data_format_table[Int_val(_pixel_data_format)];
    	GLenum pixel_data_type = conv_pixel_data_type_table[Int_val(_pixel_data_type)];
    	GLenum target_3d = conv_target_3d_table[Int_val(_target_3d)];
    	GLint  internal_format = conv_internal_format_table[Int_val(_internal_format)]; 
	
    	glTexImage3D( target_3d, Int_val(level), internal_format, Int_val(width), Int_val(height), Int_val(depth),  0, pixel_data_format, pixel_data_type, NULL );
	CAMLreturn (Val_unit);
}

t_value ml_glteximage3dnopixels_bytecode( value * argv, int argn )
{ 
	return ml_glteximage3dnopixels_native( argv[0], argv[1], argv[2], argv[3],
                                     argv[4], argv[5], argv[6], argv[7]); 
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static const GLenum conv_texture_binding_table[] = {GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP};

t_value ml_glgeneratemipmapext (value target)
{
	CAMLparam1 (target);
	GLenum _target = conv_texture_binding_table[Int_val(target)]; 
	glGenerateMipmapEXT(_target);
	CAMLreturn (Val_unit);	
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


static const GLenum conv_buffer_table[] = {
  	 GL_NONE
    ,    GL_FRONT_LEFT
    ,    GL_FRONT_RIGHT
    ,    GL_BACK_LEFT
    ,    GL_BACK_RIGHT
    ,    GL_COLOR_ATTACHMENT0
    ,    GL_COLOR_ATTACHMENT1
    ,    GL_COLOR_ATTACHMENT2
    ,    GL_COLOR_ATTACHMENT3
    ,    GL_COLOR_ATTACHMENT4
    ,    GL_COLOR_ATTACHMENT5
    ,    GL_COLOR_ATTACHMENT6
    ,    GL_COLOR_ATTACHMENT7
    ,    GL_COLOR_ATTACHMENT8
    ,    GL_COLOR_ATTACHMENT9
    ,    GL_COLOR_ATTACHMENT10
    ,    GL_COLOR_ATTACHMENT11
    ,    GL_COLOR_ATTACHMENT12
    ,    GL_COLOR_ATTACHMENT13
    ,    GL_COLOR_ATTACHMENT14
    ,    GL_COLOR_ATTACHMENT15
};

t_value ml_gldrawbuffers (value n, value bufs)
{
	CAMLparam2 (n, bufs);
	int _n = Int_val(n);
	GLenum * _bufs = (GLenum *) malloc (sizeof(GLenum) * _n);
	glDrawBuffers(_n, _bufs);
	free(_bufs);
	CAMLreturn (Val_unit);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////

static const GLenum conv_geometry_type_table[] = {
	GL_GEOMETRY_INPUT_TYPE_EXT
,	GL_GEOMETRY_OUTPUT_TYPE_EXT
,	GL_GEOMETRY_VERTICES_OUT_EXT
};

t_value ml_glprogramparameteriext (value program, value type, value v)
{
	CAMLparam3 (program, type, v);
	GLenum pname = conv_geometry_type_table[Int_val(type)];
	glProgramParameteriEXT(Long_val(program), pname, Int_val(v));
	CAMLreturn (Val_unit);
}

t_value ml_glgetactiveattrib (value program, value index, value bufsize)
{
	CAMLparam3 (program, index, bufsize);
	CAMLlocal1 (name);
	GLsizei len;
	GLint size;
	GLenum type;
	char *buffer = (char *)malloc (sizeof(char) * bufsize);
	glGetActiveAttrib (Long_val(program), Int_val(index), Int_val(bufsize), &len, &size, &type, buffer);
	name = caml_copy_string (buffer);
	CAMLreturn (name);
}

t_value ml_glenablevertexattribarray (value index)
{
	glEnableVertexAttribArray(Int_val(index));
	return (Val_unit);
}

t_value ml_gldisablevertexattribarray (value index)
{
	glDisableVertexAttribArray(Int_val(index));
	return (Val_unit);
}

t_value ml_gltexparameterforanisotropy (value target, value num)
{
	glTexParameteri(conv_texture_binding_table[Int_val(target)], GL_TEXTURE_MAX_ANISOTROPY_EXT, Int_val(num));
	return (Val_unit);
}


static const GLenum conv_primitive_type_table[] = {GL_POINTS, GL_LINES, GL_LINE_LOOP, 
						GL_LINE_STRIP, GL_TRIANGLES, GL_TRIANGLE_STRIP, 
						GL_TRIANGLE_FAN, GL_QUADS, GL_QUAD_STRIP, GL_POLYGON};

t_value ml_gldrawarraysinstanced (value mode, value first, value count, value primcount)
{
	glDrawArraysInstanced( conv_primitive_type_table[Int_val(mode)], 
				Int_val(first),
				Int_val(count),
				Int_val(primcount) );
	return (Val_unit);
}
