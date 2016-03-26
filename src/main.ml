open Core.Std
open Tsdl


let pi = 4.0 *. atan 1.0
let proj_width = 320
let proj_height = 200
let fov_d = 60
let fov_r = pi /. 3.
let proj_dist = 277
let d_angle = 0.1875

let p_height = 32


(* factor out a function for initializing SDL, creating window, creating renderer *)

(* could just use set_render_draw_color and render_draw functions, but that seems
   a bit ridiculous. *)

(* TODO: pretty silly to just wrap everything in ignore... there's a better way,
   i'm sure, but i don't know the language well enough *)


(* TODO: add a map, that is, an int array array (or an int matrix?), together
   with, at least, multipliers for tile width and depth *)

let tiles =
  [|
    [|0;0;0;0;0;0|];
    [|0;1;1;1;1;0|];
    [|0;1;0;0;1;0|];
    [|0;1;0;0;1;0|];
    [|0;1;0;0;1;0|];
    [|0;0;0;0;0;0|]
  |]

type map =
  { width : int;
    depth : int;
    g_width : float;
    g_depth : float;
    grid : int array array; }


let level =
  { width = 6;
    depth = 6;
    g_width = 64.;
    g_depth = 64.;
    grid = tiles }

type grid_point = { gx : int; gz : int }

type world_point = { wx : float; wz : float }

type angle = float

(* map coordinates - the coordinates in the tile map *)
(* world coordinates - the coordinates in the world, that the player will use;
   got by multiplying tile coords with t_width & t_depth *)

(* functions to translate between map coordinates and world coordinates *)

let coords_world_to_grid m (wx,wz) =
  { gx = int_of_float (Float.round (wx /. m.g_width));
    gz = int_of_float (Float.round (wz /. m.g_depth)) }

let coords_grid_of_world m (gx,gz) =
  { wx = (float_of_int gx) *. m.g_width;
    wz = (float_of_int gz) *. m.g_depth }


(* given any angle, returns the equivalent angle within [0,2pi) *)
let norm_angle a =
  if (a > 0.) then
    a -. (Float.mod_float (2. *. pi) a)
  else
    a +. ((2. *. pi) *. Float.round (Float.mod_float (2. *. pi) a))

(* return grid_point * float option, where grid_point is the coordinates of
   the wall collided with, and the float is the distance to that wall;
   None is returned if there was no intersection *)
let find_intersection m (wp : world_point) a =
  let h_xa = m.g_depth /. (tan a) in
  let h_za = if (a < pi) then (-. m.g_depth) else m.g_depth in

  let h_x_orig = if (a < (pi /. 2.)) || (a > ((3. *. pi) /. 2.))
    then (Float.round_down (wp.wx /. m.g_width)) -. 1.
    else (Float.round_down (wp.wx /. m.g_width)) +. 64.
  in
  let h_z_orig = wp.wz +. (wp.wx -. h_x_orig) /. (tan a) in

  (* these are used to find vertical intersections, i.e. ones
     where X is a multiple of 64 *)
  (* let v_za = if (a < pi) *)
  (*   then m.g_width /. (tan a) *)
  (*   else (-. m.g_depth) /. (tan a) *)
  (* in *)
  let v_z_orig = if (a < (pi /. 2.)) || (a > ((3. *. pi) /. 2.))
    then (Float.round_down (wp.wz /. m.g_depth)) -. 1.
    else (Float.round_down (wp.wz /. m.g_depth)) +. 64.
  in
  let v_x_orig = wp.wx +. (wp.wz -. v_z_orig) /. (tan a) in
  let v_xa = if (a < (pi /. 2.)) || (a > ((3. *. pi) /. 2.))
    then (-. m.g_width)
    else m.g_width
  in
  let v_za = m.g_width /. (tan a) in

  (* TODO: fold these two functions into one *)
  (* find horizontal intersection; return grid_point * float *)
  let rec check_h_ray i =
    let x = h_x_orig +. (h_xa *. (float_of_int i)) in
    let z = h_z_orig +. (h_za *. (float_of_int i)) in
    (* if we're looking outside the map, we won't hit anything, so return None *)
    if x < 0. || x > (float_of_int m.width) *. m.g_width ||
       z < 0. || z > (float_of_int m.depth) *. m.g_depth
    then
      None
        (* else we actually need to look and see what's at the current point *)
    else
    let gx = int_of_float (x /. m.g_width) in
    let gz = int_of_float (z /. m.g_depth) in
    (* the tile of the wall that we hit *)
    let gp = m.grid.(gz).(gx) in
    if gp <> 0 then
      let dist = (((x -. wp.wx) ** 2.) +. ((z -. wp.wz) ** 2.)) ** 0.5 in
      Some (gp, dist)
    else
      check_h_ray (i+1)
  in

  (* find vertical intersection; return grid_point * float *)
  let rec check_v_ray i =
    let x = v_x_orig +. (v_xa *. (float_of_int i)) in
    let z = v_z_orig +. (v_za *. (float_of_int i)) in
    (* if we're looking outside the map, we won't hit anything, so return None *)
    if x < 0. || x > (float_of_int m.width) *. m.g_width ||
       z < 0. || z > (float_of_int m.depth) *. m.g_depth
    then
      None
        (* else we actually need to look and see what's at the current point *)
    else
    let gx = int_of_float (x /. m.g_width) in
    let gz = int_of_float (z /. m.g_depth) in
    (* the tile of the wall that we hit *)
    let gp = m.grid.(gz).(gx) in
    if gp <> 0 then
      let dist = (((x -. wp.wx) ** 2.) +. ((z -. wp.wz) ** 2.)) ** 0.5 in
      Some (gp, dist)
    else
      check_v_ray (i+1)
  in

  let h_intersect = check_h_ray 0 in
  let v_intersect = check_v_ray 0 in

  (* return the first intersection, i.e. if the snd of h_intersect is
     smaller than that of v_intersect, return v_intersect, or vice versa;
     if one of them is None, return the other; if both are None, return None. *)

  match (h_intersect, v_intersect) with
  | (None,v) -> v
  | (h,None) -> h
  | (Some (p1,dh), Some (p2,dv)) ->
    if dh > dv then Some (p1,dh) else Some (p2,dv)


let cast_rays m (wp : world_point) a =
  (* contains the angle for each column on the projection plane *)
  let a = norm_angle a in
  let angles = List.init proj_width
      ~f:(fun i -> ((float_of_int i) *. d_angle) -. a) in
  let dists = List.map angles ~f:(fun a' -> find_intersection m wp a') in

  ()



(* TODO: add function to, given a set of (possible) intersect points and
   a direction, return a set of map coordinates *)
(* TODO: a function to get world distance between two world points *)
(* TODO: the fucking raycaster *)


let running = ref true

let set_color renderer color =
  let (r,g,b) = color in
  ignore (Sdl.set_render_draw_color renderer r g b 0xff)

let draw_line r x1 y1 x2 y2 =
  ignore (Sdl.render_draw_line r x1 y1 x2 y2)

(* draws a single column based on calculated top and bottom pixels of the wall
   (if any) the ray struck *)
let render_column ~renderer ~h ~x ~top ~btm ~color =
  set_color renderer (0,0,0);
  draw_line renderer x 0 x top;
  set_color renderer color;
  draw_line renderer x top x btm;
  set_color renderer (0,0,0);
  draw_line renderer x btm x h

(* why does this not block, but the exact same code when written
   inline in the while loop does block? *)
let process_event e =
  let module E = Sdl.Event in
  let key_scancode e = Sdl.Scancode.enum E.(get e keyboard_scancode) in
  match Sdl.poll_event (Some e) with
  | false -> ();
  | true -> match E.(enum (get e typ)) with
    | `Quit -> running := false
    | `Key_down when key_scancode e = `Escape -> running := false
    | _ -> ()


let main () = match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok () ->
    match Sdl.create_window ~w:width ~h:height "SDL OpenGL" Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok w ->
      match Sdl.create_renderer w with
      | Error (`Msg e) -> Sdl.log "Create renderer error: %s" e; exit 1
      | Ok r ->
        while !running do
          let e = Sdl.Event.create () in
          process_event e;
          (* let ticks = Sdl.get_ticks () in *)
        done;
        Sdl.destroy_renderer r;
        Sdl.destroy_window w;
        Sdl.quit ();
        exit 0

let () = main ()
