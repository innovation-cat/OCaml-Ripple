type vertex = private { x : float; y : float; z : float; u : float; v : float; }

type triangle = vertex * vertex * vertex 

type section = private { triangles_num : int; triangles : triangle array; }

val create : in_channel -> section
