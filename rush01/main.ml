open Tsdl
open Result

let mouse_click win =
    let (_, (x, y)) = Tsdl.Sdl.get_mouse_state () in (*TODO check button*)
    win#mouse_action (x, y)

let rec push_second id () =
    let event = Sdl.Event.create () in
    Sdl.Event.set event Sdl.Event.typ id;
    match Sdl.push_event event (* by sguillia *) with
    | Error (`Msg e) -> failwith (Printf.sprintf "Could push event: %s" e)
    | Ok (_)         -> (Sdl.delay (Int32.of_int 1000); push_second id (); ())


let event_loop win =
    match Sdl.register_event () with
    | None                      -> failwith (Printf.sprintf "Could not create event")
    | Some (eatch_second_event) ->
        let e = Sdl.Event.create () in
        let rec loop win = match Sdl.wait_event (Some e) with
            | Error (`Msg e) -> failwith (Printf.sprintf "Could not wait event: %s" e)
            | Ok () ->
                match Sdl.Event.(enum (get e typ)) with
                    | `Quit                    -> ()
                    | `Mouse_button_down       -> begin loop (mouse_click win) end
                    | `User_event              -> loop (win#eatch_second ())
                    | `Drop_file               -> begin Sdl.Event.drop_file_free e; loop win end
                    | _                        -> loop win in
        Sdl.start_text_input ();
        ignore (Thread.create (push_second eatch_second_event) ());
        loop win

let start () = match Sdl.init Sdl.Init.video with
    | Error (`Msg e) -> failwith (Printf.sprintf "Init error: %s" e)
    | Ok () ->
      match Sdl.create_window ~w:800 ~h:600 "Tama" Sdl.Window.opengl with
        | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
        | Ok w ->
            let tama = new Tama.tama in
            let tama = try tama#load () with
                       | _ -> begin print_endline "Can not load file, creat new tama !"; tama end in
            let win = new Window.window w tama in
            let win = win#add_image "lena" "lena.bmp" in
            let win = win#add_button Save (0, 0, 10, 10) in

            let win = win#add_rectangle (Sdl.Rect.create 40 30 150 40) (0, 0, 0, 0) in
            let win = win#add_rectangle (Sdl.Rect.create 230 30 150 40) (0, 0, 0, 0) in
            let win = win#add_rectangle (Sdl.Rect.create 420 30 150 40) (0, 0, 0, 0) in
            let win = win#add_rectangle (Sdl.Rect.create 610 30 150 40) (0, 0, 0, 0) in

            let win = win#add_rectangle (Sdl.Rect.create 38 28 154 44) (255, 255, 255, 0) in
            let win = win#add_rectangle (Sdl.Rect.create 228 28 154 44) (255, 255, 255, 0) in
            let win = win#add_rectangle (Sdl.Rect.create 418 28 154 44) (255, 255, 255, 0) in
            let win = win#add_rectangle (Sdl.Rect.create 608 28 154 44) (255, 255, 255, 0) in


            win#render_all () ;
            event_loop win;
            Sdl.destroy_window w;
            Sdl.quit ();
            exit 0

let () =
  try start () with
  | _  -> print_endline "Internal error"
