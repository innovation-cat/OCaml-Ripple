## OCaml-Ripple ##
Implement wave superposition with OCaml, a strong static functional programming language.

In this project, on one hand, I want to consolidate my OCaml programming skill, on the other hand, I try to build a large project which integrates OCaml with GPU programming.

I implement another larger OCaml project in [GI-LPV](https://github.com/wong4ever/GI-LPV).

if you have any comments or suggestions, please feel free to let me know.

## Prerequisites ##
1. Make sure you have installed OCaml (4.0) and glMLite, glMLite is a package which provide OpenGL bindings for  OCaml, for more detail about that, please refer to [glMLite]( http://www.linux-nantes.org/~fmonnier/ocaml/GL/index.html "glMLite")

2. Make sure your graphic card support OpenGL 3.0 with **EXT _ GPU _ shader4** extension.


## Abstract ##
when ripple touch water level, I try to save the wave superposition information as height field, and inject to 1-D texture. 

### wave model ###
![image](http://photo2.bababian.com/upload6/20131013/54EA62DC5A2DA4DDD83C78016E21303A.jpg)



### snippet ###

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
		Amplitude += A /(1 + Lambda * dist * dist + Lambda1 * dTime * dTime) * (cos(phase)) * (x > 0 ? 1:0) * ( phase < MaxPhase ? 1 : 0);
	}


