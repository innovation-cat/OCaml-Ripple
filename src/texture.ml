open GL
open Glex

exception FBO_Not_Support;;

module Resource_Map = Map.Make (struct type t = string let compare = String.compare end);;


type texture_base = {
			mutable tex_id 	      : texture_id; (* type texture_id = private int *)
			mutable target 	      : BindTex.texture_binding;
			mutable name          : string;
	       	    }

module Texture_Params_1D = struct
	type texture_params_1d = {
					mutable target               : TexParam.tex_param_target;
					mutable wrap_s               : wrap_param;
					mutable wrap_t 		     : wrap_param;
					mutable mag_filter 	     : GL.Mag.mag_filter;
					mutable min_filter 	     : GL.Min.min_filter; 
					mutable source_format        : pixel_data_format;
					(* use Glex.internal_format instead of GL.InternalFormat.internal_format *)
					mutable internal_format      : Glex.internal_format_ex;
					mutable n_type               : pixel_data_type;
			 	}

end

let init_param_1d = {
			Texture_Params_1D.target = TexParam.GL_TEXTURE_1D;
			Texture_Params_1D.wrap_s = GL.GL_CLAMP;
			Texture_Params_1D.wrap_t = GL.GL_CLAMP;
			Texture_Params_1D.mag_filter = GL.Mag.GL_NEAREST;
			Texture_Params_1D.min_filter = GL.Min.GL_NEAREST;
			Texture_Params_1D.source_format = GL.GL_RGB;
			Texture_Params_1D.internal_format = Glex.GL_RGB;
			Texture_Params_1D.n_type = GL.GL_UNSIGNED_BYTE;
		    }

module Texture_Params_2D = struct
	type texture_params_2d = {
					mutable target               : TexParam.tex_param_target;
					mutable wrap_s               : wrap_param;
					mutable wrap_t 		     : wrap_param;
					mutable mag_filter 	     : GL.Mag.mag_filter;
					mutable min_filter 	     : GL.Min.min_filter; 
					mutable source_format        : pixel_data_format;
					(* use Glex.internal_format instead of GL.InternalFormat.internal_format *)
					mutable internal_format      : Glex.internal_format_ex;
					mutable n_type               : pixel_data_type;
			 	}

end

let init_param_2d = {
			Texture_Params_2D.target = TexParam.GL_TEXTURE_2D;
			Texture_Params_2D.wrap_s = GL.GL_CLAMP_TO_EDGE;
			Texture_Params_2D.wrap_t = GL.GL_CLAMP_TO_EDGE;
			Texture_Params_2D.mag_filter = GL.Mag.GL_LINEAR;
			Texture_Params_2D.min_filter = GL.Min.GL_LINEAR;
			Texture_Params_2D.source_format = GL.GL_RGB;
			Texture_Params_2D.internal_format = Glex.GL_RGB;
			Texture_Params_2D.n_type = GL.GL_UNSIGNED_BYTE;
		    }

module Texture_Params_3D = struct
	type texture_params_3d = {			
					mutable target               : TexParam.tex_param_target;
					mutable wrap_r               : wrap_param;
					mutable wrap_s               : wrap_param;
					mutable wrap_t 		     : wrap_param;
					mutable mag_filter 	     : GL.Mag.mag_filter;
					mutable min_filter 	     : GL.Min.min_filter; 
					mutable source_format        : pixel_data_format;
					mutable internal_format      : Glex.internal_format_ex;
					mutable n_type               : pixel_data_type;
			 	}
end

let init_param_3d = {
			Texture_Params_3D.target = TexParam.GL_TEXTURE_3D;
			Texture_Params_3D.wrap_s = GL.GL_CLAMP;
			Texture_Params_3D.wrap_t = GL.GL_CLAMP;
			Texture_Params_3D.wrap_r = GL.GL_CLAMP;
			Texture_Params_3D.mag_filter = GL.Mag.GL_LINEAR;
			Texture_Params_3D.min_filter = GL.Min.GL_LINEAR;
			Texture_Params_3D.source_format = GL.GL_RGB;
			Texture_Params_3D.internal_format = Glex.GL_RGB;
			Texture_Params_3D.n_type = GL_UNSIGNED_BYTE;
		   }


type texture = Texture_1D of texture_base*Texture_Params_1D.texture_params_1d | Texture_2D of texture_base*Texture_Params_2D.texture_params_2d | Texture_3D of texture_base*Texture_Params_3D.texture_params_3d

let texture_list = ref Resource_Map.empty

let print_texture_info = function
	   Texture_2D (base, _) | Texture_3D (base, _) -> 
			Printf.printf "base: tex_id: %d\n" (base.tex_id :> int);
			Printf.printf "      target: %s\n"  (match base.target with BindTex.GL_TEXTURE_2D -> "GL_TEXTURE_2D"
										|   BindTex.GL_TEXTURE_3D -> "GL_TEXTURE_3D"
										|   BindTex.GL_TEXTURE_1D -> "GL_TEXTURE_1D"
										|   BindTex.GL_TEXTURE_CUBE_MAP -> "GL_TEXTURE_CUBE_MAP");
			Printf.printf "      name:  %s\n"  (base.name)
;;

let update_texture_1d base params = texture_list := Resource_Map.add base.name (Texture_1D (base,params)) (!texture_list);;

let update_texture_2d base params = texture_list := Resource_Map.add base.name (Texture_2D (base,params)) (!texture_list);;

let update_texture_3d base params = texture_list := Resource_Map.add base.name (Texture_3D (base,params)) (!texture_list);;


(* create new 2d texture, and insert into texture_list *)
let create_texture_1d base params width pixels =
	 (* check fbo support *)
	glBindTexture base.target base.tex_id;	
	glTexParameter params.Texture_Params_1D.target (TexParam.GL_TEXTURE_WRAP_S params.Texture_Params_1D.wrap_s);
	glTexParameter params.Texture_Params_1D.target (TexParam.GL_TEXTURE_WRAP_T params.Texture_Params_1D.wrap_t);
	glTexParameter params.Texture_Params_1D.target (TexParam.GL_TEXTURE_MAG_FILTER params.Texture_Params_1D.mag_filter);
	glTexParameter params.Texture_Params_1D.target (TexParam.GL_TEXTURE_MIN_FILTER params.Texture_Params_1D.min_filter);
	
	begin
	match pixels with
	    None -> Glex.glTexImage1DNoPixels TexTarget.GL_TEXTURE_1D 0 params.Texture_Params_1D.internal_format width params.Texture_Params_1D.source_format params.Texture_Params_1D.n_type
	|   Some p -> Glex.glTexImage1DWithPixels TexTarget.GL_TEXTURE_1D 0 params.Texture_Params_1D.internal_format width params.Texture_Params_1D.source_format params.Texture_Params_1D.n_type p
	end;

	glUnbindTexture base.target;
	texture_list := Resource_Map.add base.name (Texture_1D (base,params)) (!texture_list)
;;




(* create new 2d texture, and insert into texture_list *)
let create_texture_2d base params width height pixels =
	 (* check fbo support *)
	glBindTexture base.target base.tex_id;
(*	glTexParameter params.Texture_Params_2D.target (TexParam.GL_TEXTURE_WRAP_S params.Texture_Params_2D.wrap_s);
	glTexParameter params.Texture_Params_2D.target (TexParam.GL_TEXTURE_WRAP_T params.Texture_Params_2D.wrap_t);
*)	glTexParameter params.Texture_Params_2D.target (TexParam.GL_TEXTURE_MAG_FILTER params.Texture_Params_2D.mag_filter);
	glTexParameter params.Texture_Params_2D.target (TexParam.GL_TEXTURE_MIN_FILTER params.Texture_Params_2D.min_filter);

	begin
	match pixels with
	    None -> Glex.glTexImage2DNoPixels TexTarget.GL_TEXTURE_2D 0 params.Texture_Params_2D.internal_format width height params.Texture_Params_2D.source_format params.Texture_Params_2D.n_type
	|   Some p -> Glex.glTexImage2DWithPixels TexTarget.GL_TEXTURE_2D 0 params.Texture_Params_2D.internal_format width height params.Texture_Params_2D.source_format params.Texture_Params_2D.n_type p
	end;
	let min_filter = params.Texture_Params_2D.min_filter in
	if min_filter = GL.Min.GL_NEAREST_MIPMAP_NEAREST || min_filter = GL.Min.GL_NEAREST_MIPMAP_LINEAR ||
	   min_filter = GL.Min.GL_LINEAR_MIPMAP_NEAREST || min_filter = GL.Min.GL_LINEAR_MIPMAP_LINEAR then
		glGenerateMipmapEXT base.target;
	
	glUnbindTexture base.target;
	texture_list := Resource_Map.add base.name (Texture_2D (base,params)) (!texture_list)
;;


(* parameters:                                                                         *)
(* 	base: {texture_id; target; name}                                               *)
(*	parameter:                                                                     *)
(*	width, height, depth: 3-d of 3d texture                                        *)
(*  	pixels: optional                                                               *)
let create_texture_3d base params width height depth pixels =  
	glBindTexture base.target base.tex_id;
	glTexParameter params.Texture_Params_3D.target (TexParam.GL_TEXTURE_WRAP_S params.Texture_Params_3D.wrap_s);
	glTexParameter params.Texture_Params_3D.target (TexParam.GL_TEXTURE_WRAP_T params.Texture_Params_3D.wrap_t);
	glTexParameter params.Texture_Params_3D.target (TexParam.GL_TEXTURE_WRAP_R params.Texture_Params_3D.wrap_r);
	glTexParameter params.Texture_Params_3D.target (TexParam.GL_TEXTURE_MAG_FILTER params.Texture_Params_3D.mag_filter);
	glTexParameter params.Texture_Params_3D.target (TexParam.GL_TEXTURE_MIN_FILTER params.Texture_Params_3D.min_filter);
	
	begin
	match pixels with
	    None -> Glex.glTexImage3DNoPixels TexTarget.GL_TEXTURE_3D 0 params.Texture_Params_3D.internal_format width height depth params.Texture_Params_3D.source_format params.Texture_Params_3D.n_type
	|   Some p -> Glex.glTexImage3DWithPixels TexTarget.GL_TEXTURE_3D 0 params.Texture_Params_3D.internal_format width height depth params.Texture_Params_3D.source_format params.Texture_Params_3D.n_type p
	end;
	if params.Texture_Params_3D.min_filter = GL.Min.GL_LINEAR_MIPMAP_LINEAR then
		Glex.glGenerateMipmapEXT base.target;
	glUnbindTexture base.target;
	texture_list := Resource_Map.add base.name (Texture_3D (base,params)) (!texture_list)
;;

