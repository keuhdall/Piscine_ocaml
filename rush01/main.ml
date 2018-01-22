let mouse_click win =
    let (b, (x, y)) = Tsdl.Sdl.get_mouse_state () in
    match Int32.to_int b with
    | 1   ->  win#mouse_action (x, y)
    | _   ->  win

let rec push_second id () =
    let event = Tsdl.Sdl.Event.create () in
    Tsdl.Sdl.Event.set event Tsdl.Sdl.Event.typ id;
    match Tsdl.Sdl.push_event event (* by sguillia *) with
    | Error (`Msg e) -> failwith (Printf.sprintf "Could push event: %s" e)
    | Ok (_)         -> (Tsdl.Sdl.delay (Int32.of_int 1000); push_second id (); ())

let event_loop win =
    match Tsdl.Sdl.register_event () with
    | None                      -> failwith (Printf.sprintf "Could not create event")
    | Some (eatch_second_event) ->
        let e = Tsdl.Sdl.Event.create () in
        let rec loop win =
            win#clear () ;
            win#render_all () ;
            match Tsdl.Sdl.wait_event (Some e) with
            | Error (`Msg e) -> failwith (Printf.sprintf "Could not wait event: %s" e)
            | Ok () ->
                match Tsdl.Sdl.Event.(enum (get e typ)) with
                    | `Quit                    -> ()
                    | `Mouse_button_down       -> begin loop (mouse_click win) end
                    | `User_event              -> loop (win#eatch_second ())
                    | `Drop_file               -> begin Tsdl.Sdl.Event.drop_file_free e; loop win end
                    | _                        -> loop win in
        Tsdl.Sdl.start_text_input ();
        ignore (Thread.create (push_second eatch_second_event) ());
        loop win

let start () = match Tsdl.Sdl.init Tsdl.Sdl.Init.video with
    | Error (`Msg e) -> failwith (Printf.sprintf "Init error: %s" e)
    | Ok () ->
    match Tsdl_ttf.Ttf.init () with
        | Error (`Msg e) -> Tsdl.Sdl.quit (); failwith (Printf.sprintf "Init error: %s" e)
        | Ok () ->
      match Tsdl.Sdl.create_window ~w:800 ~h:600 "Tama" Tsdl.Sdl.Window.opengl with
        | Error (`Msg e) -> failwith (Printf.sprintf "Create window error: %s" e)
        | Ok w ->
            let tama = new Tama.tama in
            let tama = try tama#load () with
                | _ -> begin print_endline "Can not load file, creat new tama !"; tama end in
            let win = new Window.window w tama in
            let win = win#add_image "tama" "tama.bmp" in
            let win = win#add_text "heal" in
            let win = win#add_text "thunder" in
            let win = win#add_text "bath" in
            let win = win#add_text "kill" in
            let win = win#add_text "save" in
            let win = win#add_text "dead" in
            let win = win#add_text "health" in
            let win = win#add_text "energy" in
            let win = win#add_text "hygiene" in
            let win = win#add_text "happyness" in
            let win = win#add_button Save (700, 400, 50, 50) in
            let win = win#add_button Eat (38, 470, 154, 90) in
            let win = win#add_button Thunder (228, 470, 154, 90) in
            let win = win#add_button Bath (418, 470, 154, 90) in
            let win = win#add_button Kill (608, 470, 154, 90) in
            let win = win#add_rectangle (Tsdl.Sdl.Rect.create 38 28 154 44) (0, 0, 0, 0) in
            let win = win#add_rectangle (Tsdl.Sdl.Rect.create 228 28 154 44) (0, 0, 0, 0) in
            let win = win#add_rectangle (Tsdl.Sdl.Rect.create 418 28 154 44) (0, 0, 0, 0) in
            let win = win#add_rectangle (Tsdl.Sdl.Rect.create 608 28 154 44) (0, 0, 0, 0) in
            let win = win#add_rectangle (Tsdl.Sdl.Rect.create 38 500 154 70) (0, 0, 0, 0) in
            let win = win#add_rectangle (Tsdl.Sdl.Rect.create 228 500 154 70) (0, 0, 0, 0) in
            let win = win#add_rectangle (Tsdl.Sdl.Rect.create 418 500 154 70) (0, 0, 0, 0) in
            let win = win#add_rectangle (Tsdl.Sdl.Rect.create 608 500 154 70) (0, 0, 0, 0) in
            let win = win#add_rectangle (Tsdl.Sdl.Rect.create 700 400 50 50) (0, 0, 0, 0) in
            win#render_all () ;
            event_loop win;
            Tsdl.Sdl.destroy_window w;
            Tsdl_ttf.Ttf.quit ();
            Tsdl.Sdl.quit ()

let () =
  try start () with
  | Failure (m)  -> print_endline m
  | _            -> print_endline "Internal error"
