varying vec4 normal;
varying vec4 pos;
varying vec4 eye;
uniform vec3 LightColor;

const vec3 lightPosition = vec3(10.0, 28.0, -25.0);

const vec3 LightSpecularColor = vec3(0.3,0.3,0.3);
void main()
{
	vec3 lp = lightPosition;
        // vec3 eyedir=(eye-pos).xyz;
	vec3 eyedir = (pos-eye).xyz;
	vec3 n = normalize(normal.xyz);
	vec3 I = normalize(lp - pos.xyz);
	vec3 R = normalize(-reflect(-normalize(eyedir), n));
	vec3 color = LightColor * max(0.0,dot(I, n)) + 
		 LightSpecularColor * pow(max(0.0,dot(R,I)),4.0);
	gl_FragColor = vec4(color,1.0);
}