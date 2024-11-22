open Raylib

let screen_width = 800.
let screen_height = 450.

module Dynarray = struct
  include Dynarray

  let filter_inplace (t : 'a Dynarray.t) ~(predicate : 'a -> bool) =
    let i = ref 0 in
    let j = ref 0 in
    while !i < length t do
      let element = get t !i in
      if predicate element then (
        set t !j element;
        incr j);
      incr i
    done;
    truncate t !j
end

module Spaceship = struct
  let size = 20.
  let speed = 7.

  type t = { rect : Rectangle.t }

  let init x y = { rect = Rectangle.create x y size size }
  let draw t = draw_rectangle_rec t.rect Color.black

  let move_left t =
    let dx = Rectangle.x t.rect -. speed in
    let dx_capped_to_left = max dx 0. in
    Rectangle.set_x t.rect dx_capped_to_left

  let move_right t =
    let dx = Rectangle.x t.rect +. speed in
    let dx_capped_to_right = min dx (screen_width -. size) in
    Rectangle.set_x t.rect dx_capped_to_right
end

module Laser = struct
  let width = 4.
  let height = 13.
  let speed = 7.

  type t = { rect : Rectangle.t }

  let init x y = { rect = Rectangle.create x y width height }
  let draw t = draw_rectangle_rec t.rect Color.orange

  let update t =
    let dy = Rectangle.y t.rect -. speed in
    Rectangle.set_y t.rect dy
end

module Alien = struct
  let size = 20.
  let rows = 4
  let per_row = 10

  type t = { rect : Rectangle.t }

  let init x y = { rect = Rectangle.create x y size size }
  let draw t = draw_rectangle_rec t.rect Color.red
end

module Game = struct
  let fire_cooldown = Mtime.Span.( * ) 250 Mtime.Span.ms

  type command = MoveLeft | MoveRight | FireLaser

  let command_keys =
    [| (Key.A, MoveLeft); (Key.D, MoveRight); (Key.Space, FireLaser) |]

  type t = {
    spaceship : Spaceship.t;
    lasers : Laser.t Dynarray.t;
    mutable last_fire : Mtime_clock.counter;
    aliens : Alien.t Dynarray.t;
  }

  let init () =
    init_window 800 450 "OCaml Space Invaders!";
    set_target_fps 60;

    {
      spaceship =
        (let xf = (screen_width +. Spaceship.size) /. 2.0 in
         let yf = screen_height -. Spaceship.size in
         Spaceship.init xf yf);
      lasers =
        (let l = Dynarray.create () in
         Dynarray.ensure_capacity l 1000;
         l);
      last_fire = Mtime_clock.counter ();
      aliens =
        (let count = Alien.rows * Alien.per_row in
         let breaks = Alien.per_row + 1 in
         let col_paddings =
           let alien_space = Alien.per_row * Int.of_float Alien.size in
           let space_left = get_screen_width () - alien_space in
           space_left / breaks
         in
         Dynarray.init count (fun i ->
             let x =
               let col = i mod Alien.per_row in
               let prev_aliens = col * Int.of_float Alien.size in
               let prev_breaks = (col + 1) * col_paddings in
               prev_aliens + prev_breaks
             in
             let y =
               let row = i / Alien.per_row in
               let alien_row_padding = 15 in
               let prev_rows = row * Int.of_float Alien.size in
               let prev_row_paddings = (row + 1) * alien_row_padding in
               prev_rows + prev_row_paddings
             in
             Alien.init (Float.of_int x) (Float.of_int y)));
    }

  let _fire_laser t : unit =
    let elpased = Mtime_clock.count t.last_fire in
    match Mtime.Span.is_longer elpased ~than:fire_cooldown with
    | false -> ()
    | true ->
        let x =
          Rectangle.x t.spaceship.rect +. ((Spaceship.size -. Laser.width) /. 2.)
        in
        let y = Rectangle.y t.spaceship.rect -. Laser.height -. 7. in
        Dynarray.add_last t.lasers (Laser.init x y);
        t.last_fire <- Mtime_clock.counter ()

  let update t =
    Array.iter
      (fun (key, command) ->
        match (command, is_key_down key) with
        | MoveLeft, true -> Spaceship.move_left t.spaceship
        | MoveRight, true -> Spaceship.move_right t.spaceship
        | FireLaser, true -> _fire_laser t
        | _, _ -> ())
      command_keys;

    Dynarray.iter Laser.update t.lasers;

    Dynarray.filter_inplace t.lasers ~predicate:(fun laser ->
        let leave_laser = ref true in
        Dynarray.filter_inplace t.aliens ~predicate:(fun alien ->
            match check_collision_recs laser.rect alien.rect with
            | false -> true
            | true ->
                leave_laser := false;
                false);
        !leave_laser);
    t

  let draw t =
    begin_drawing ();

    clear_background Color.raywhite;
    Spaceship.draw t.spaceship;
    Dynarray.iter Alien.draw t.aliens;
    Dynarray.iter Laser.draw t.lasers;

    end_drawing ();

    t
end

let rec loop (game : Game.t) =
  match window_should_close () with
  | true -> close_window ()
  | false -> game |> Game.update |> Game.draw |> loop

let () = Game.init () |> loop
