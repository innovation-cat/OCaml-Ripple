val pi : float lazy_t

module GlobalParameter :
  sig
    type t = {
      mutable dropSamples : int;
      mutable maxHeight : float;
      mutable minHeight : float;
      mutable minSpeed : float;
      mutable maxSpeed : float;
      mutable waveFieldSize : int;
      mutable poolSize : float;
      mutable amplitude : float;
      mutable lambda : float;
      mutable omega : float;
      mutable lambda1 : float;
      mutable lambda2 : float;
      mutable maxPhase : float;
      mutable waveVelocity : float;
      mutable extinctionLife : float;
      mutable gravity : float;
      mutable minPhase : float;
      mutable colorfulRain : bool;
      mutable dropSound : bool;
    }
  end

val init_global_params : GlobalParameter.t

module WaveSource :
  sig
    type t = {
      mutable initT : float;
      mutable posX : float;
      mutable posY : float;
      mutable a : float;
      mutable lambda : float;
      mutable omega : float;
      mutable velocity : float;
      mutable height : float;
      mutable dropVel : float;
      mutable phases : float;
      mutable color : float array;
      mutable enabled : bool;
    }
    val print : t list -> unit
  end
