open GL

type internal_format_ex =
  | GL_ALPHA
  | GL_ALPHA4
  | GL_ALPHA8
  | GL_ALPHA12
  | GL_ALPHA16
  | GL_COMPRESSED_ALPHA
  | GL_COMPRESSED_LUMINANCE
  | GL_COMPRESSED_LUMINANCE_ALPHA
  | GL_COMPRESSED_INTENSITY
  | GL_COMPRESSED_RGB
  | GL_COMPRESSED_RGBA
  | GL_DEPTH_COMPONENT
  | GL_DEPTH_COMPONENT16
  | GL_DEPTH_COMPONENT24
  | GL_DEPTH_COMPONENT32
  | GL_LUMINANCE
  | GL_LUMINANCE4
  | GL_LUMINANCE8
  | GL_LUMINANCE12
  | GL_LUMINANCE16
  | GL_LUMINANCE_ALPHA
  | GL_LUMINANCE4_ALPHA4
  | GL_LUMINANCE6_ALPHA2
  | GL_LUMINANCE8_ALPHA8
  | GL_LUMINANCE12_ALPHA4
  | GL_LUMINANCE12_ALPHA12
  | GL_LUMINANCE16_ALPHA16
  | GL_INTENSITY
  | GL_INTENSITY4
  | GL_INTENSITY8
  | GL_INTENSITY12
  | GL_INTENSITY16
  | GL_R3_G3_B2
  | GL_RGB
  | GL_RGB4
  | GL_RGB5
  | GL_RGB8
  | GL_RGB10
  | GL_RGB12
  | GL_RGB16
  | GL_RGBA
  | GL_RGBA2
  | GL_RGBA4
  | GL_RGB5_A1
  | GL_RGBA8
  | GL_RGB10_A2
  | GL_RGBA12
  | GL_RGBA16
  | GL_SLUMINANCE
  | GL_SLUMINANCE8
  | GL_SLUMINANCE_ALPHA
  | GL_SLUMINANCE8_ALPHA8
  | GL_SRGB
  | GL_SRGB8
  | GL_SRGB_ALPHA
  | GL_SRGB8_ALPHA8
  | GL_RG16F
  | GL_R16F
  | GL_RGBA16F
  | GL_RGBA32F

type image_data_float_1D = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

type image_data_float_2D = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t

type image_data_float_3D = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array3.t

type image_data_float_nD = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Genarray.t

external glShaderSources : GL.shader_object -> int -> string array option -> int array option -> unit = "ml_glshadersources"

external glTexImage1DNoPixels: TexTarget.target_1d -> int -> internal_format_ex -> int -> pixel_data_format -> pixel_data_type -> unit = "ml_glteximage1dnopixels_bytecode" "ml_glteximage1dnopixels_native"

external glTexImage1DWithPixels: TexTarget.target_1d -> int -> internal_format_ex -> int -> pixel_data_format -> pixel_data_type -> image_data_float_1D -> unit = "ml_glteximage1dwithpixels_bytecode" "ml_glteximage1dwithpixels_native"

external glTexImage2DNoPixels: TexTarget.target_2d -> int -> internal_format_ex -> int -> int -> pixel_data_format -> pixel_data_type -> unit = "ml_glteximage2dnopixels_bytecode" "ml_glteximage2dnopixels_native"

external glTexImage2DWithPixels: TexTarget.target_2d -> int -> internal_format_ex -> int -> int -> pixel_data_format -> pixel_data_type -> GL.image_data -> unit = "ml_glteximage2dwithpixels_bytecode" "ml_glteximage2dwithpixels_native"

external glTexImage3DNoPixels: TexTarget.target_3d -> int -> internal_format_ex -> int -> int -> int -> pixel_data_format -> pixel_data_type -> unit = "ml_glteximage3dnopixels_bytecode" "ml_glteximage3dnopixels_native"

external glTexImage3DWithPixels: TexTarget.target_3d -> int -> internal_format_ex -> int -> int -> int -> pixel_data_format -> pixel_data_type -> GL.image_data -> unit = "ml_glteximage3dwithpixels_bytecode" "ml_glteximage3dwithpixels_native"

external glGenerateMipmapEXT : GL.BindTex.texture_binding -> unit = "ml_glgeneratemipmapext"



type buffer =
	GL_NONE
     |  GL_FRONT_LEFT 
     |  GL_FRONT_RIGHT
     |  GL_BACK_LEFT
     |  GL_BACK_RIGHT
     |  GL_COLOR_ATTACHMENT0 
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

external glDrawBuffers : int -> buffer array -> unit = "ml_gldrawbuffers"

(* glsl geometry shader *)
type geometry_type = 
	   GL_GEOMETRY_INPUT_TYPE_EXT
	|  GL_GEOMETRY_OUTPUT_TYPE_EXT
	|  GL_GEOMETRY_VERTICES_OUT_EXT

external glProgramParameteriEXT : GL.shader_program -> geometry_type -> int -> unit = "ml_glprogramparameteriext"

external glGetActiveAttrib : GL.shader_program -> int -> int -> string = "ml_glgetactiveattrib"

external glEnableVertexAttribArray : int -> unit = "ml_glenablevertexattribarray"

external glDisableVertexAttribArray : int -> unit = "ml_gldisablevertexattribarray"

external glTexParameterForAnisotropy : GL.BindTex.texture_binding -> int -> unit = "ml_gltexparameterforanisotropy"

external glDrawArraysInstanced : GL.primitive -> int -> int -> int -> unit = "ml_gldrawarraysinstanced" 
