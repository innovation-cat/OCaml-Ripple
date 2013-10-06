type t = private 
{
  	vertex_id : VBO.vbo_id;
 	texture_id : VBO.vbo_id;
  	indices_id : VBO.vbo_id;
}

val create : unit -> t
