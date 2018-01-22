type action_type = Save | Eat | Thunder | Bath | Kill

class window win new_tama =

  object (self)
  val tama : Tama.tama = new_tama
  val loaded_image_tama = []
  val rectangles = []
  val button_surface : (action_type * (int * int * int * int)) list = []
  val renderer =
    begin
      match Tsdl.Sdl.create_renderer win with
        | Error (`Msg e) -> failwith (Printf.sprintf "Error creating renderer: %s" e)
        | Ok (renderer) -> renderer
    end
  val font =
      begin
          match Tsdl_ttf.Ttf.open_font "arial.ttf" 20 with
          | Error (`Msg e) -> failwith (Printf.sprintf "Error creating font: %s" e)
          | Ok (font) -> font
      end

 method load_text text =
    match Tsdl_ttf.Ttf.render_text_solid font text (Tsdl.Sdl.Color.create 0 0 255 0) with
      | Error (`Msg e) -> failwith (Printf.sprintf "Error creating font image: %s" e)
      | Ok (surface) ->
     match Tsdl.Sdl.create_texture_from_surface renderer surface with
     | Error (`Msg e) -> begin Tsdl.Sdl.free_surface surface; failwith (Printf.sprintf "Error creating texture : %s" e) end
     | Ok (texture) -> begin Tsdl.Sdl.free_surface surface; texture end

  method private load_image img_path =
    match Tsdl.Sdl.load_bmp img_path with
      | Error (`Msg e) -> failwith (Printf.sprintf "Error loading image: %s" e)
      | Ok (surface) ->
        match Tsdl.Sdl.create_texture_from_surface renderer surface with
          | Error (`Msg e) -> begin Tsdl.Sdl.free_surface surface; failwith (Printf.sprintf "Error creating texture : %s" e) end
          | Ok (texture) -> begin Tsdl.Sdl.free_surface surface; texture end

  method private get_image img_name =
    let rec find images =
      match images with
        | (name, img)::queue when name = img_name -> img
        | tete::queue   -> find queue
        | []            -> failwith "Window.window.get_image: Image not found" in
    find loaded_image_tama

  method private get_button (click_x, click_y) : 'a option =
      let rec loop buttons =
          match buttons with
          | (utile, (x, y, w, h))::queue when ((click_x >= x)
                                               && (click_y >= y)
                                               && (click_x <= x + w)
                                               && (click_y <= y + h)) -> Some (utile)
          | tete::queue -> loop queue
          | []          -> None in
      loop button_surface

  method add_button action (x, y, w, h) =
    {< button_surface = (action, (x, y, w, h))::button_surface >}

  method add_image img_name img_path =
    {< loaded_image_tama = (img_name, self#load_image img_path)::loaded_image_tama >}
  method add_text text =
    {< loaded_image_tama = (text, self#load_text text)::loaded_image_tama >}

  method add_rectangle rect color =
    {< rectangles = (rect, color)::rectangles >}
    
  method private render_image () =
    match Tsdl.Sdl.render_copy renderer (self#get_image "tama") ~dst:(Tsdl.Sdl.Rect.create 200 100 400 400) with
      | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
      | Ok () -> ()

  method private render_text () =
    match Tsdl.Sdl.set_render_draw_color renderer 0 255 0 0 with
    | Error (`Msg e)  -> failwith (Printf.sprintf "Failed to set text color (%s)" e)
    | Ok () ->
      match Tsdl.Sdl.render_copy renderer (self#get_image "heal") ~dst:(Tsdl.Sdl.Rect.create 38 500 154 70) with
        | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
        | Ok () ->
        match Tsdl.Sdl.render_copy renderer (self#get_image "thunder") ~dst:(Tsdl.Sdl.Rect.create 228 500 154 70) with
        | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
        | Ok () ->
        match Tsdl.Sdl.render_copy renderer (self#get_image "bath") ~dst:(Tsdl.Sdl.Rect.create 418 500 154 70) with
        | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
        | Ok () ->
        match Tsdl.Sdl.render_copy renderer (self#get_image "kill") ~dst:(Tsdl.Sdl.Rect.create 608 500 154 70) with
        | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
        | Ok () ->
        match Tsdl.Sdl.render_copy renderer (self#get_image "save") ~dst:(Tsdl.Sdl.Rect.create 700 400 50 50) with
        | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
        | Ok () ->
        match Tsdl.Sdl.render_copy renderer (self#get_image "health") ~dst:(Tsdl.Sdl.Rect.create 38 68 154 44) with
        | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
        | Ok () ->
        match Tsdl.Sdl.render_copy renderer (self#get_image "energy") ~dst:(Tsdl.Sdl.Rect.create 228 68 154 44) with
        | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
        | Ok () ->
        match Tsdl.Sdl.render_copy renderer (self#get_image "hygiene") ~dst:(Tsdl.Sdl.Rect.create 418 68 154 44) with
        | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
        | Ok () ->
        match Tsdl.Sdl.render_copy renderer (self#get_image "happyness") ~dst:(Tsdl.Sdl.Rect.create 608 68 154 44) with
        | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
        | Ok () -> ()
      

  method private render_rectangles () =
    let rec render_rec rect_list =
      match rect_list with
        | []  -> ()
        | (rect, (r, g, b, a))::queue  ->
          match Tsdl.Sdl.set_render_draw_color renderer r g b a with
            | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
            | Ok () ->
              match Tsdl.Sdl.render_fill_rect renderer (Some rect) with
                | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
                | Ok () -> render_rec queue in
    render_rec rectangles

  method private render_gauges () =
    let hp_gauge = int_of_float ((float_of_int tama#health) *. 1.5) in
    let energy_gauge = int_of_float ((float_of_int tama#energy) *. 1.5) in
    let hygiene_gauge = int_of_float ((float_of_int tama#hygiene) *. 1.5) in
    let happyness_gauge = int_of_float ((float_of_int tama#happyness) *. 1.5) in
    let gauges = [(Tsdl.Sdl.Rect.create 40 30 hp_gauge 40) ;
                  (Tsdl.Sdl.Rect.create 230 30 energy_gauge 40) ;
                  (Tsdl.Sdl.Rect.create 420 30 hygiene_gauge 40) ;
                  (Tsdl.Sdl.Rect.create 610 30 happyness_gauge 40)] in
    let rec render_gauges_loop g = match g with
      | []          -> ()
      | tete::queue ->
        match Tsdl.Sdl.set_render_draw_color renderer 255 255 255 0 with
          | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
          | Ok () ->
            match Tsdl.Sdl.render_fill_rect renderer (Some tete) with
              | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
              | Ok () -> render_gauges_loop queue in
      render_gauges_loop gauges

  method render_all () =
    match Tsdl.Sdl.render_clear renderer with
      | Error (`Msg e)  ->  failwith (Printf.sprintf "Window.window.render_all : Failed cleaning window (%s)" e)
      | Ok ()           ->
          self#render_image () ;
          self#render_rectangles () ;
          self#render_gauges () ;
          self#render_text () ;
          if tama#is_live () = false
          then
            begin
              match Tsdl.Sdl.set_render_draw_color renderer 0 0 0 0 with
              | Error (`Msg e)  -> failwith (Printf.sprintf "Failed to set text color (%s)" e)
              | Ok () ->
              match Tsdl.Sdl.render_copy renderer (self#get_image "dead") ~dst:(Tsdl.Sdl.Rect.create 0 0 800 600) with
                | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
                | Ok () -> ();
            end;
            Tsdl.Sdl.render_present renderer

  method mouse_action (click_x, click_y) =
      {< tama = match self#get_button (click_x, click_y) with
             | Some (Save)    -> begin begin try tama#save () with _ -> print_endline "Can not save file" end; tama end
             | Some (Eat)     -> tama#eat ()
             | Some (Thunder) -> tama#thunder ()
             | Some (Bath)    -> tama#bath ()
             | Some (Kill)    -> tama#kill ()
             | None           -> tama >}

  method eatch_second () =
      {< tama = (tama#second_passe ()) >}

  method clear () =
    match Tsdl.Sdl.set_render_draw_color renderer 255 255 255 0 with
      | Error (`Msg e)  -> failwith (Printf.sprintf "Failed to set render color (%s)" e)
      | Ok () ->
        match Tsdl.Sdl.render_clear renderer with
          | Error (`Msg e) -> failwith (Printf.sprintf "Failed to clear window : %s" e)
          | Ok () -> ()

end
