type vertex =
   {
      x : float;
      y : float;
      z : float;
      u : float;
      v : float;
   };;

type triangle = vertex * vertex * vertex;;

type section = 
   {
      numtriangles : int;
      triangles : triangle array;
   };;

let parse ~line =
    let size = String.length line in 
    let rec aux i isforward = 
      if line.[i]=' ' || line.[i]='\t' then 
        if isforward then aux (i+1) isforward
        else aux (i-1) isforward
      else i
    in
    let left = aux 0 true in let right =  aux (size-1) false in 
    (String.sub line left (right-left+1))
;;

(* remove white line and comment line *)
let read_str ~ic = 
    let rec aux () = 
      let line = input_line ic in 
         if line="" then aux ()
         else if line.[0]='/' then aux ()
         else parse line 
    in
    aux ()
;;

let setup ~ic = 
    let trianglenum = 
      let 
        line = read_str ~ic 
      in Scanf.sscanf line "NUMPOLLIES %d" (fun x -> x) 
    in 
    let triangles = Array.init trianglenum (fun i ->
        let create_vertex () = 
            let line = read_str ~ic in 
            let x , y , z , u , v = Scanf.sscanf line "%f %f %f %f %f" (fun a b c d e -> a , b, c , d , e) in
            {x=x ; y=y ; z=z ; u=u ; v=v}
        in 
        let vertex1 = create_vertex () in
        let vertex2 = create_vertex () in
        let vertex3 = create_vertex () in
        (vertex1 , vertex2 , vertex3)) 
    in
    let sector = 
        {
           numtriangles = trianglenum;
           triangles = triangles
        }
    in
    sector
;;
