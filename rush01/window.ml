type action_type = Save | Eat | Thunder | Bath | Kill

open Tsdl
class window win new_tama =

  object (self)
  val tama : Tama.tama = new_tama
  val loaded_image_tama = []
  val rectangles = []
  val button_surface : (action_type * (int * int * int * int)) list = []
  val renderer =
    begin
      match Sdl.create_renderer win with
        | Error (`Msg e) -> failwith (Printf.sprintf "Error creating renderer: %s" e)
        | Ok (renderer) -> renderer
    end

  method private load_image img_path =
    match Sdl.load_bmp img_path with
      | Error (`Msg e) -> failwith (Printf.sprintf "Error loading image: %s" e)
      | Ok (surface) ->
        match Sdl.create_texture_from_surface renderer surface with
          | Error (`Msg e) -> begin Sdl.free_surface surface; failwith (Printf.sprintf "Error creating texture : %s" e) end
          | Ok (texture) -> begin Sdl.free_surface surface; texture end

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

  method add_rectangle rect color =
    {< rectangles = (rect, color)::rectangles >}
    
  method private render_image () =
    match Sdl.render_copy renderer (self#get_image "lena") with
      | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
      | Ok ()           -> ()

  method private render_rectangles () =
    let rec render_rec rect_list =
      match rect_list with
        | []  -> ()
        | (rect, (r, g, b, a))::queue  ->
          match Sdl.set_render_draw_color renderer r g b a with
            | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
            | Ok () ->
              match Sdl.render_fill_rect renderer (Some rect) with
                | Error (`Msg e)  -> failwith (Printf.sprintf "Window.window.render_image : failed to render image (%s)" e)
                | Ok () -> render_rec queue in
    render_rec rectangles

  method render_all () =
    match Sdl.render_clear renderer with
      | Error (`Msg e)  ->  failwith (Printf.sprintf "Window.window.render_all : Failed cleaning window (%s)" e)
      | Ok ()           ->
          self#render_image () ;
          self#render_rectangles ();
          Sdl.render_present renderer

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

end
