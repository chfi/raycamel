open Core.Std
open Tsdl


let height = 600
let width = 800

let fov = 60

let pi = 4.0 *. atan 1.0

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
    height : int;
    t_width : float;
    t_depth : float;
    tiles : int array array; }


let level =
  { width = 6;
    height = 6;
    t_width = 64.;
    t_depth = 64.;
    tiles = tiles }

type map_point = { mx : int; mz : int }

type world_point = { wx : float; wz : float }

type angle = float

(* map coordinates - the coordinates in the tile map *)
(* world coordinates - the coordinates in the world, that the player will use;
   got by multiplying tile coords with t_width & t_depth *)

(* functions to translate between map coordinates and world coordinates *)

let coords_world_to_map m (wx,wz) =
  { mx = int_of_float (Float.round (wx /. m.t_width));
    mz = int_of_float (Float.round (wz /. m.t_depth)) }

let coords_map_of_world m (mx,mz) =
  { wx = (float_of_int mx) *. m.t_width;
    wz = (float_of_int mz) *. m.t_depth }

(* TODO: add function to, given world (float) coordinates and an angle,
   find the possible intersecting points, in world coordinates, with walls
   on the map *)
(* let find_tile_map_intersects (m : map) { wx; wz } (a : angle) = *)
(* finds the potential intersection points along the x axes *)
  (* let find_x_intersects wx *)


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
