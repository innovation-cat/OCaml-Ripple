exception Shader_Error of string

module WaveSource :
  sig
    type t =
      Basic.WaveSource.t = {
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

module GlobalParameter :
  sig
    type t =
      Basic.GlobalParameter.t = {
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

type t = private
{
  mutable wavesource : Texture.texture;
  mutable wavefield : Texture.texture;
}

val create : unit -> unit

val render_to_texture :
  float -> float -> WaveSource.t list -> FBO.fbo_id -> WaveSource.t list

val render : VBO.vbo_id -> VBO.vbo_id -> VBO.vbo_id -> unit
