open Core.Std
open Tsdl


let pi = 4.0 *. atan 1.0
let two_pi = 2. *. pi
let proj_width = 320
let proj_height = 200
let fov_d = 60
let fov_r = pi /. 3.
let proj_dist = 277
let r_angle = fov_r /. (float_of_int proj_width)

(* let height = 600 *)
(* let width = 800 *)

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
    [|0;2;3;2;2;0|];
    [|0;1;0;0;2;0|];
    [|0;2;0;0;2;0|];
    [|0;2;2;2;2;0|];
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
let rec norm_angle a =
  let a' = (atan2 (sin a) (cos a)) in
  if a' < 0. then a' +. two_pi else a'

(* return grid_point * float * ( float * float ) option, where grid_point is the
   coordinates of the wall collided with, and the float is the distance to that
   wall; None is returned if there was no intersection. The last float pair is
   a normalized normal vector to the wall hit *)

   let find_intersection m (wp : world_point) a =
  let a = norm_angle a in

  let h_xa = m.g_depth /. (tan a) in
  let h_za = if (a < pi) then m.g_depth else (-. m.g_depth) in

  (* let h_z_orig = if (a > (pi /. 2.)) && (a < ((3. *. pi) /. 2.)) *)
  let h_z_orig = if (a < pi)
    then ((Float.round_down (wp.wz /. m.g_depth)) *. m.g_depth) +. 64.
    else ((Float.round_down (wp.wz /. m.g_depth)) *. m.g_depth) -. 1.
  in
  let h_x_orig = wp.wx +. ((wp.wz -. h_z_orig) /. (tan (-. a))) in

  (* these are used to find vertical intersections, i.e. ones
     where X is a multiple of 64 *)

  let v_xa = if (a < (pi /. 2.)) || (a > ((3. *. pi) /. 2.))
    then m.g_width
    else (-. m.g_width)
  in
  let v_za = m.g_width *. (tan (-. a)) in

  let v_x_orig = if (a < (pi /. 2.)) || (a > ((3. *. pi) /. 2.))
    then ((Float.round_down (wp.wx /. m.g_width)) *. m.g_width) +. 64.
    else ((Float.round_down (wp.wx /. m.g_width)) *. m.g_width) -. 1.
  in
  let v_z_orig = wp.wz +. ((wp.wx -. v_x_orig) *. (tan a)) in


  (* TODO: fold these two functions into one *)
  (* find horizontal intersection; return grid_point * float *)
  let rec check_h_ray i =
    let x = h_x_orig +. (h_xa *. (float_of_int i)) in
    let z = h_z_orig +. (h_za *. (float_of_int i)) in
    (* print_endline ("h_orig_x: " ^ (string_of_float h_x_orig)); *)
    (* print_endline ("h_xa: " ^ (string_of_float h_xa)); *)
    (* print_endline ("h_x: " ^ (string_of_float x)); *)
    (* print_endline ("h_orig_z: " ^ (string_of_float h_z_orig)); *)
    (* print_endline ("h_za: " ^ (string_of_float h_za)); *)
    (* print_endline ("h_z: " ^ (string_of_float z)); *)
    (* if we're looking outside the map, we won't hit anything, so return None *)
    if x < 0. || x > (float_of_int m.width) *. m.g_width ||
       z < 0. || z > (float_of_int m.depth) *. m.g_depth
    then
      None
        (* else we actually need to look and see what's at the current point *)
    else
      let gx = Int.clamp_exn ~min:0 ~max:(m.width - 1)
          (int_of_float (Float.round_down (x /. m.g_width))) in
      let gz = Int.clamp_exn ~min:0 ~max:(m.depth - 1)
          (int_of_float (Float.round_down (z /. m.g_depth))) in
      (* let gz = int_of_float (z /. m.g_depth) in *)
    (* the tile of the wall that we hit *)
    let gp = m.grid.(gx).(gz) in
    if gp <> 0 then
      let dist = (((x -. wp.wx) ** 2.) +. ((z -. wp.wz) ** 2.)) ** 0.5 in
      print_endline ("h: (" ^ (string_of_int gx) ^ "," ^ (string_of_int gz) ^ ")");
      Some (gp, dist, (0.,1.))
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
      (* let gx = int_of_float (x /. m.g_width) in *)
      (* let gz = int_of_float (z /. m.g_depth) in *)
      let gx = Int.clamp_exn ~min:0 ~max:(m.width-1)
          (int_of_float (Float.round_down (x /. m.g_width))) in
      let gz = Int.clamp_exn ~min:0 ~max:(m.depth-1)
          (int_of_float (Float.round_down (z /. m.g_depth))) in
      (* print_endline ("g: (" ^ (string_of_int gx) ^ ", " ^ (string_of_int gz) ^ ")"); *)
    (* the tile of the wall that we hit *)
    let gp = m.grid.(gx).(gz) in
    if gp <> 0 then
      let dist = (((x -. wp.wx) ** 2.) +. ((z -. wp.wz) ** 2.)) ** 0.5 in
      print_endline ("v: (" ^ (string_of_int gx) ^ "," ^ (string_of_int gz) ^ ")");
      Some (gp, dist, (1.,0.))
    else
      check_v_ray (i+1)
  in

  let h_intersect = check_h_ray 0 in
  let v_intersect = check_v_ray 0 in
  (* let h_intersect = None in *)
  (* let v_intersect = None in *)


  (* return the first intersection, i.e. if the snd of h_intersect is
     smaller than that of v_intersect, return v_intersect, or vice versa;
     if one of them is None, return the other; if both are None, return None. *)

  match (h_intersect, v_intersect) with
  | (None,v) -> v
  | (h,None) -> h
  | (Some (p1,dh,n1), Some (p2,dv,n2)) ->
    if dh < dv then Some (p1,dh,n1) else Some (p2,dv,n2)


let cast_rays m (wp : world_point) a =
  (* contains the angle for each column on the projection plane *)
  let a = norm_angle a in
  let angles = List.rev (List.init proj_width
      ~f:(fun i -> ((float_of_int i) *. r_angle) -. a)) in
  let dists = List.map angles ~f:(fun a' -> find_intersection m wp a') in
  dists


let running = ref true

let set_color renderer color =
  let (r,g,b) = color in
  ignore (Sdl.set_render_draw_color renderer r g b 0xff)

let draw_line r x1 y1 x2 y2 =
  ignore (Sdl.render_draw_line r x1 y1 x2 y2)

let draw_rect r x1 y1 x2 y2 =
  let rect = Sdl.Rect.create ~x:x1 ~y:y1
      ~w:(Int.abs (x2 - x1)) ~h:(Int.abs (y2 - y1))
  in
  ignore (Sdl.render_fill_rect r (Some rect))

(* draws a single column based on calculated top and bottom pixels of the wall
   (if any) the ray struck *)
let render_column ~renderer ~h ~x ~top ~btm ~color =
  set_color renderer (0,0,0);
  draw_line renderer x 0 x top;
  set_color renderer color;
  draw_line renderer x top x btm;
  set_color renderer (0,0,0);
  draw_line renderer x btm x h

let grid_to_color i =
  match i with
  | 1 -> (120,120,120)
  | 2 -> (0,120,0)
  | 3 -> (0,0,120)
  | 0 -> (0,0,0)
  | _ -> (255,255,255)

let grid_to_shaded_color i s =
  match i with
  | 1 -> (s*120,s*120,s*120)
  | 2 -> (0,s*120,0)
  | 3 -> (0,0,s*120)
  | 0 -> (0,0,0)
  | _ -> (255,255,255)

let draw_map renderer m width height =
  set_color renderer (255,255,255);
  (* first horizontal, then vertical *)
  let rows = List.init m.depth ~f:(fun i -> i) in
  let cols = List.init m.width ~f:(fun i -> i) in
  (*
  List.iter rows ~f:(fun r ->
      let row_h = int_of_float m.g_depth in
      draw_line renderer 0 (r * row_h) width (r * row_h));
  List.iter cols ~f:(fun c ->
      let col_w = int_of_float m.g_width in
      draw_line renderer (c * col_w) 0 (c * col_w) height);
     *)

  List.iter rows ~f:(fun r ->
      let row_h = int_of_float m.g_depth in
      List.iter cols ~f:(fun c ->
          let col_w = int_of_float m.g_width in
          let i = m.grid.(r).(c) in
          set_color renderer (grid_to_color i);
          draw_rect renderer (c * col_w) (r * row_h) ((c+1) * col_w) ((r+1) * row_h);
          set_color renderer (255,255,255);
          draw_line renderer (c * col_w) 0 (c * col_w) height
        );
        draw_line renderer 0 (r * row_h) width (r * row_h))


let polar_to_cart (a,l) =
  let x = (cos a) *. l in
  let y = (sin a) *. l in
  (x,y)

let cart_to_polar (x,y) =
  (* let a = norm_angle (atan2 y x) in *)
  let a = (atan2 y x) in
  let l = ((x**2.) +. (y**2.)) ** 0.5 in
  (a,l)


let v = ref 0.0
let a = ref 2.4
let da = ref 0.0

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
    | `Key_down when key_scancode e = `Up -> v := 0.2
    | `Key_down when key_scancode e = `Down -> v := (-. 0.2)
    | `Key_down when key_scancode e = `Left -> da := -. 0.0075
    | `Key_down when key_scancode e = `Right -> da := 0.0075
    | `Key_up when key_scancode e = `Up -> v := 0.
    | `Key_up when key_scancode e = `Down -> v := 0.
    | `Key_up when key_scancode e = `Left -> da := 0.
    | `Key_up when key_scancode e = `Right -> da := 0.
    | _ -> ()


let p = ref { wx = 166.; wz = 154.; }

(* start the topdown view *)
let topdown () = match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok () ->
    let s_width = level.width * (int_of_float level.g_width) in
    let s_height = level.depth * (int_of_float level.g_depth) in
    match Sdl.create_window ~w:s_width ~h:s_height
            "SDL OpenGL" Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok w ->
      match Sdl.create_renderer w with
      | Error (`Msg e) -> Sdl.log "Create renderer error: %s" e; exit 1
      | Ok r ->

        while !running do
          let e = Sdl.Event.create () in
          process_event e;

          a := norm_angle (!a +. !da);

          p := { wx = !p.wx +. ((cos !a) *. !v);
                 wz = !p.wz +. ((sin !a) *. !v);};

          ignore (Sdl.render_clear r);

          draw_map r level s_width s_height;
          let intersection = find_intersection level !p !a in
          let d = match intersection with
            | None -> 2000. (* large enough number to cover the screen *)
            | Some (_,d,_) -> d
          in
          (* let (rx,ry) = polar_to_cart (!a, 40.) in *)
          let (rx,ry) = polar_to_cart (!a, d) in
          let px = int_of_float !p.wx in
          let py = int_of_float !p.wz in
          let rx = int_of_float rx in
          let ry = int_of_float ry in

          set_color r (255,0,0);
          draw_line r px py (px+rx) (py+ry);

          (* print_endline ("angle: " ^ (string_of_float !a)); *)
          set_color r (0,0,0);
          Sdl.render_present r;

        done;
        Sdl.destroy_renderer r;
        Sdl.destroy_window w;
        Sdl.quit ();
        exit 0



let raycaster () = match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok () ->
    match Sdl.create_window ~w:proj_width ~h:proj_height
            "SDL OpenGL" Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok w ->
      match Sdl.create_renderer w with
      | Error (`Msg e) -> Sdl.log "Create renderer error: %s" e; exit 1
      | Ok r ->
        while !running do

          let e = Sdl.Event.create () in
          process_event e;

          a := norm_angle (!a +. !da);

          p := { wx = !p.wx +. ((cos !a) *. !v);
                 wz = !p.wz +. ((sin !a) *. !v);};

          print_endline ("angle: " ^ (string_of_float !a));
          ignore (Sdl.render_clear r);
          let intersects = cast_rays level !p !a in
          List.iteri intersects
            ~f:(fun col is ->
                match is with
                | None -> ()
                | Some (i,d,(nx,ny)) ->
                  let ch = int_of_float (200. -. (d *. 3.)) in
                  let mid = proj_height / 2 in
                  let color = match i with
                    | 1 -> (255,0,0)
                    | 2 -> (0,255,0)
                    | 3 -> (0,0,255)
                    | _ -> (130,130,130)
                  in
                  render_column ~renderer:r ~h:proj_height ~x:col
                    ~top:(mid - ch) ~btm:(mid + ch)  ~color:color);
          Sdl.render_present r;

        done;
        Sdl.destroy_renderer r;
        Sdl.destroy_window w;
        Sdl.quit ();
        exit 0

let () = raycaster ()
