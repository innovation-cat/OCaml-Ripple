let pi = lazy (4.0 *. atan 1.0)

module GlobalParameter = struct
	type t = 
	{
    		mutable dropSamples 	: int ;
    		mutable maxHeight 	: float;
            	mutable	minHeight 	: float;
    		mutable minSpeed 	: float;
    		mutable maxSpeed 	: float;
    		mutable waveFieldSize 	: int;
    		mutable poolSize 	: float;
    		mutable amplitude 	: float;
    		mutable lambda 		: float;
    		mutable omega 		: float;
    		mutable lambda1 	: float;
    		mutable lambda2 	: float;
    		mutable maxPhase 	: float;
    		mutable waveVelocity 	: float;
    		mutable extinctionLife  : float;
    		mutable gravity         : float;
    		mutable minPhase 	: float;
    		mutable colorfulRain 	: bool;
    		mutable dropSound 	: bool 
	}
end;;


let init_global_params = { 
				GlobalParameter.amplitude = 0.2; GlobalParameter.lambda = 0.63; GlobalParameter.lambda1 = 23.1;
				GlobalParameter.lambda2 = 0.5; GlobalParameter.omega = 54.4; GlobalParameter.waveVelocity = 3.4; 
				GlobalParameter.waveFieldSize = 256; GlobalParameter.dropSamples = 100; GlobalParameter.maxHeight = 300.0;
				GlobalParameter.minHeight = 5.0; GlobalParameter.maxSpeed = 40.0; GlobalParameter.minSpeed = 10.0; 
				GlobalParameter.maxPhase = 6.0 *. (Lazy.force pi) *. 2.0; GlobalParameter.minPhase = 1.0 *. (Lazy.force pi) *. 2.0; 
				GlobalParameter.poolSize = 40.0; GlobalParameter.extinctionLife = 8.0; 
			   	GlobalParameter.gravity = 0.5; GlobalParameter.dropSound = false; GlobalParameter.colorfulRain = false 
                   	 } 

module WaveSource = struct
	type t = {
			mutable initT    : float;
    			mutable posX 	 : float;
    			mutable posY 	 : float;
    			mutable a 	 : float;
    			mutable lambda   : float;
    			mutable omega 	 : float;
    			mutable velocity : float;
    			mutable height 	 : float;
    			mutable dropVel  : float;
   	 		mutable phases 	 : float;
    			mutable color 	 : float array;
    			mutable enabled  : bool 
		}

(*
	let create ~initT ~posX ~posY ~a ~lambda ~omega ~velocity ~height ~dropVel ~phases ~color ~enabled = 
		{initT; posX; posY; a; lambda; omega; velocity; height; dropVel; phases; color; enabled}
*)

	let print wave = Printf.printf "number: %d\n" (List.length wave); 
			 List.iter (fun x -> 
	Printf.printf "initT: %f posX: %f posY: %f a: %f lambda: %f omega: %f velocity: %f height: %f dropVel: %f phases: %f enable: %b\n" x.initT x.posX x.posY x.a x.lambda x.omega x.velocity x.height x.dropVel x.phases x.enabled) wave 

end;;


