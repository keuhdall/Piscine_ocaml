open Tsdl
open Result

let log fmt = Format.printf (fmt ^^ "@.")
let log_err fmt = Format.eprintf (fmt ^^ "@.")

let event_loop () =
    let e = Sdl.Event.create () in
    let rec loop () = match Sdl.wait_event (Some e) with
        | Error (`Msg e) -> log_err " Could not wait event: %s" e; ()
        | Ok () ->
            match Sdl.Event.(enum (get e typ)) with
                | `Quit -> ()
                | `Drop_file -> Sdl.Event.drop_file_free e; loop ()
                | _ -> loop ()
            in
            Sdl.start_text_input ();
            loop ()

let main () = match Sdl.init Sdl.Init.video with
    | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
    | Ok () ->
      match Sdl.create_window ~w:640 ~h:480 "SDL OpenGL" Sdl.Window.opengl with
        | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
        | Ok w ->
          match Sdl.load_bmp "lena.bmp" with
            | Error (`Msg e) -> Sdl.log "Error loading image: %s" e; exit 1
            | Ok (surface) ->
              match Sdl.create_renderer w with
                | Error (`Msg e) -> Sdl.log "Error creating renderer: %s" e; exit 1
                | Ok (renderer) ->
                  let rect1 = Sdl.Rect.create 10 10 50 100 in
                  let rect1 = Some rect1 in
                  match Sdl.set_render_draw_color renderer 0 0 255 255 with
                    | Error (`Msg e) -> Sdl.log "Error setting rectangles color: %s" e; exit 1
                    | Ok () ->
                      match Sdl.render_fill_rect renderer rect1 with
                        | Error (`Msg e) -> Sdl.log "Error filling rectangle color: %s" e; exit 1
                        | Ok () ->
                          Sdl.render_present renderer ;
                          Sdl.delay 4000l;
                          event_loop ();
                          Sdl.destroy_window w;
                          Sdl.quit ();
                          exit 0

let () = main ()