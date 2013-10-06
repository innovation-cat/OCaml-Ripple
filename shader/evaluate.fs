#version 120
#extension GL_ARB_draw_buffers : enable

uniform sampler1D source; // 波源缓存，由函数PrepareWaveSourceTex打包成一维纹理
//uniform float lambda;
//uniform float sized;
uniform float Lambda1;
uniform float Lambda2;

uniform int n;  // 波源的个数
uniform float InvN;
uniform float poolSize;
uniform float time;



const float PI = 3.1415926;
const int SourceSize = 2;


void main()
{
	vec2 cpos = gl_TexCoord[0].xy * poolSize;
	int i;
	float Amplitude = 0.0;
	for (i=0; i<n-1; i++)
	{
		int base = i * SourceSize;
		vec4 v1 = texture1D(source, (base * InvN)*0.9998+0.0001);
		vec4 v2 = texture1D(source, ((base + 1) * InvN)*0.9998+0.0001);
		float InitT		= v1.x;
		float PosX		= v1.y;
		float PosY		= v1.z;
		float A			= v1.w;
		float Lambda	= v2.x;
		float Omega		= v2.y;
		float Velocity	= v2.z;
		float MaxPhase  = v2.w;
		float dTime = time - InitT;
		float dist = distance(cpos, v1.yz);
		float x = dTime - dist / Velocity;
		float phase = Omega/(1.0+Lambda2*dTime) * x - PI/2;
		Amplitude += A /(1+Lambda*dist*dist+Lambda1*dTime*dTime) * (cos(phase)) * (x>0?1:0)*(phase<MaxPhase?1:0);       //  在某一点的所有波的振幅的叠加
	}

	gl_FragData[0] = vec4(Amplitude,Amplitude,Amplitude,1.0) ; 	
}