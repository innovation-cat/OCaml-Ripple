#version 120
uniform sampler2D height;
uniform float dx;
uniform float dy;
uniform float dpx;
uniform float dpy;
varying vec4 pos;
varying vec4 normal;
varying vec4 eye;

void main()
{
	vec4 h = texture2D(height, gl_MultiTexCoord0.xy);
	vec4 h1 = texture2D(height, gl_MultiTexCoord0.xy+vec2(dx,0));
	vec4 h2 = texture2D(height, gl_MultiTexCoord0.xy+vec2(-dx,0));
	vec4 h3 = texture2D(height, gl_MultiTexCoord0.xy+vec2(0,dy));
	vec4 h4 = texture2D(height, gl_MultiTexCoord0.xy+vec2(0,-dy));
	pos = vec4(gl_Vertex.x, h.y, gl_Vertex.z, 1);
	vec3 pos1 = vec3(gl_Vertex.x+dpx, h1.y, gl_Vertex.z);
	vec3 pos2 = vec3(gl_Vertex.x-dpx, h2.y, gl_Vertex.z);
	vec3 pos3 = vec3(gl_Vertex.x, h3.y, gl_Vertex.z+dpy);
	vec3 pos4 = vec3(gl_Vertex.x, h4.y, gl_Vertex.z-dpy);

	vec3 n1 = cross(pos1-pos.xyz, pos3-pos.xyz);
	vec3 n3 = cross(pos2-pos.xyz, pos4-pos.xyz);

	normal = -vec4(0.5*(normalize(n1)+normalize(n3)),1);
	eye = gl_ModelViewMatrixInverse * vec4(0,0,0,1);
	pos = gl_ModelViewMatrix * pos;
	gl_Position = gl_ProjectionMatrix * pos;
	gl_TexCoord[0] = gl_MultiTexCoord0;
}