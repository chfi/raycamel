open Core.Std
open Tsdl
open Tsdl_image

(** Raycasting renderer **)

(* TODO: rewrite this using more types and split up into more modules *)

(* it would be nice to rewrite such that the types in Map are abstract... *)

let pi = 4.0 *. atan 1.0
let two_pi = 2. *. pi

type angle = float

type screen = { proj_width : int;
                proj_height : int;
                proj_dist : float;
                fov_rad : float;
                ray_angle : float ;}

let create_screen w h d fov =
  { proj_width = w;
    proj_height = h;
    proj_dist = (float_of_int (w / 2)) /. (tan (fov /. 2.));
    fov_rad = fov;
    ray_angle = fov /. (float_of_int w); }

module Map : sig
  type map =
    { width : int;
      depth : int;
      g_width : float;
      g_depth : float;
      g_height : float;
      grid : int array array; }

  type map_point = { mx : int;
                     mz : int; }

  type world_point = { wx : float;
                       wz : float; }

  val tile_of_map_point : map -> map_point -> int

  val world_of_map_point : map -> map_point -> world_point
  val map_of_world_point : map -> world_point -> map_point


end = struct
  type map =
    { width : int;
      depth : int;
      g_width : float;
      g_depth : float;
      g_height : float;
      grid : int array array; }

  type map_point = { mx : int;
                     mz : int; }

  type world_point = { wx : float;
                       wz : float; }

  let tile_of_map_point map {mx;mz} = map.grid.(mz).(mx)

  let world_of_map_point map {mx;mz} =
    { wx = (float_of_int mx) *. map.g_width;
      wz = (float_of_int mz) *. map.g_depth; }

  let map_of_world_point map {wx;wz} =
    { mx = Float.(to_int (round (wx /. map.g_width)));
      mz = Float.(to_int (round (wz /. map.g_depth))); }

end

type intersection =
  { point : Map.world_point;
    normal : Map.world_point;
    dist : float;
    wall_type : int;
  }

type ray_type = Ver | Hor


(* given any angle, returns the equivalent angle within [0,2pi) *)
let norm_angle a =
  let a' = (atan2 (sin a) (cos a)) in
  if a' < 0. then a' +. two_pi else a'


let find_intersection m (wp : Map.world_point) a =
  let open Map in
  let a = norm_angle a in
  (* need to deal with the fact that tan is weird; if a > pi,
     then we need to mirror across the y-axis. *)
  let h_xa = if (a > pi)
    then (-. m.g_depth /. (tan a))
    else (m.g_depth /. (tan a))
  in
  let h_za = if (a > pi) then (-. m.g_depth) else m.g_depth in

  (* if looking northward, we want to round down, and make sure that
     it is the block to the north that is collided with *)
  (* however, since the coordinate system has y increasing southward,
     pi radians is to the south. *)
  let h_z_orig = if (a > pi)
    then ((Float.round_down (Map.(wp.wz) /. m.g_depth)) *. m.g_depth) -. 0.001
    else ((Float.round_down (Map.(wp.wz) /. m.g_depth)) *. m.g_depth) +. 64.
  in
  let h_x_orig = wp.wx +. ((wp.wz -. h_z_orig) /. (tan (-. a))) in

  (* these are used to find vertical intersections, i.e. ones
     where X is a multiple of 64 *)

  (* if looking west, we want to round down and move in the negative direction. *)
  let v_xa = if (a > (pi /. 2.)) && (a < ((3. *. pi) /. 2.))
    then (-. m.g_width)
    else m.g_width
  in
  let v_za = if (a > (pi /. 2.)) && (a < ((3. *. pi) /. 2.))
    then (-. m.g_width *. (tan a))
    else (m.g_width *. (tan a))
  in

  (* if looking west, round down and look at the first block in the
     negative direction *)
  let v_x_orig = if (a > (pi /. 2.)) && (a < ((3. *. pi) /. 2.))
    then ((Float.round_down (wp.wx /. m.g_width)) *. m.g_width) -. 0.001
    else ((Float.round_down (wp.wx /. m.g_width)) *. m.g_width) +. 64.
  in
  let v_z_orig = wp.wz +. ((wp.wx -. v_x_orig) *. (tan (-. a))) in

  let rec check_ray ray_type x_orig z_orig x_d z_d i =
    let i' = float_of_int i in
    let x = x_orig +. (x_d *. i') in
    let z = z_orig +. (z_d *. i') in
    (* kill the ray if it's out of bounds and moving away from the map *)
    if (x_d > 0. && x > (float_of_int m.width) *. m.g_width) ||
       (x_d < 0. && x < 0.) ||
       (z_d > 0. && z > (float_of_int m.depth) *. m.g_depth) ||
       (z_d < 0. && z < 0.)
    then
      None
    else
      let gx = Int.clamp_exn ~min:0 ~max:(m.width-1)
          (int_of_float (Float.round_down (x /. m.g_width))) in
      let gz = Int.clamp_exn ~min:0 ~max:(m.depth-1)
          (int_of_float (Float.round_down (z /. m.g_depth))) in
      let gp = m.grid.(gz).(gx) in

    if gp <> 0 then
      let dist = (((x -. wp.wx) ** 2.) +. ((z -. wp.wz) ** 2.)) ** 0.5 in
      let normal = match ray_type with
        | Ver -> {wx = 0.; wz = 1.}
        | _ -> {wx = 1.; wz = 0.}
      in
      Some { point = { wx = x; wz = z}; normal = normal;
             dist = dist; wall_type = gp }
    else
      check_ray ray_type x_orig z_orig x_d z_d (i+1)

  in

  let check_h_ray = check_ray Hor h_x_orig h_z_orig h_xa h_za in
  let check_v_ray = check_ray Ver v_x_orig v_z_orig v_xa v_za in

  let h_intersect = check_h_ray 0 in
  let v_intersect = check_v_ray 0 in
  (* return the first intersection, i.e. if the snd of h_intersect is
     smaller than that of v_intersect, return v_intersect, or vice versa;
     if one of them is None, return the other; if both are None, return None. *)

  match (h_intersect, v_intersect) with
  | (None,v) -> v
  | (h,None) -> h
  | (Some h, Some v) ->
    if h.dist < v.dist then Some h else Some v



(* returns (angle * intersection) list *)
let cast_rays screen map (wp : Map.world_point) a =
  let a = norm_angle a in
  let half_w = (float_of_int (screen.proj_width / 2)) in
  let min = norm_angle (a -. (half_w *. screen.ray_angle)) in
  (* contains the angle for each column on the projection plane *)
  let angles = (List.init screen.proj_width
                  ~f:(fun i ->
                      let da = (float_of_int i) *. screen.ray_angle in
                      norm_angle (min +. da))) in
  let intersections = List.map angles
      ~f:(fun a' ->
          match find_intersection map wp a' with
          (* d' is the distance with distortion removed *)
          | Some is -> let d' = is.dist *. (cos (a' -. a)) in
            (a', Some { point = is.point; normal = is.normal;
            dist = d'; wall_type = is.wall_type })
          | None -> (a', None))
  in
  intersections
