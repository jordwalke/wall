open Wall
module C = Wall_canvas
module B2 = Gg.Box2
module P2 = Gg.P2

(* how text on a control is aligned *)
type text_alignment = [ `LEFT | `CENTER ]

(* states altering the styling of a widget *)
type widget_state =
  [ (* not interacting *)
    `DEFAULT
  | (* the mouse is hovering over the control *)
    `HOVER
  | (* the widget is activated (pressed) or in an active state (toggled) *)
    `ACTIVE
  ]

module Icon = struct
  type t = { x : int; y : int; w : int; h : int }
end

type corner_flags =
  [ (* sharp top left corner *)
    `TOP_LEFT
  | (* sharp top right corner *)
    `TOP_RIGHT
  | (* sharp bottom right corner *)
    `DOWN_RIGHT
  | (* sharp bottom left corner *)
    `DOWN_LEFT
  ] list

module Theme = struct
  type color_offset = float (* Ranging from -100 to 100 *)

  type widget = {
    outline        : color;
    item           : color;
    inner          : color;
    inner_selected : color;
    text           : color;
    text_selected  : color;
    shade_top      : color_offset;
    shade_down     : color_offset;
  }

  type node = {
    node_selected : color;
    wire          : color;
    wire_selected : color;
    node_text_selected : color;
    node_active   : color;
    node_backdrop : color;
    noodle_curving : float (* from 0.0 to 1.0 *);
  }

  (* describes the theme used to draw widgets *)
  type t = {
    background   : color; (* the background color of panels and windows *)
    regular      : widget; (* theme for labels *)
    tool_button  : widget; (* theme for tool buttons *)
    radio_button : widget; (* theme for radio buttons *)
    text_field   : widget; (* theme for text fields *)
    option       : widget; (* theme for option buttons (checkboxes) *)
    choice       : widget; (* theme for choice buttons (comboboxes) *)
    number_field : widget; (* theme for number fields *)
    slider       : widget; (* theme for slider controls *)
    scrollbar    : widget; (* theme for scrollbars *)
    tooltip      : widget; (* theme for tooltips *)
    menu         : widget; (*theme for menu backgrounds *)
    menu_item    : widget; (* theme for menu items *)
    node         : node;   (* theme for nodes *)
  }
end
open Theme

module Default = struct
  let widget_height           = 21.0
  let tool_button_width       = 20
  let node_port_radius        = 5.0
  let node_margin_top         = 25.0
  let node_margin_down        = 5.0
  let node_margin_side        = 10.0
  let node_title_height       = 20.0
  let node_arrow_area_width   = 20.0
  let splitter_area_size      = 12
  let scrollbar_width         = 13
  let scrollbar_height        = 14
  let vspacing                = 1
  let vspacing_group          = 8
  let hspacing                = 8
  let label_font_size         = 13.0
  let pad_left                = 8.0
  let pad_right               = 8.0
  let label_separator         = ": "
  let transparent_alpha       = 0.643
  let bevel_shade             = 0.30
  let inset_bevel_shade       = 0.30
  let hover_shade             = 0.15
  let splitter_shade          = 0.4
  let icon_sheet_width        = 602
  let icon_sheet_height       = 640
  let icon_sheet_grid         = 21
  let icon_sheet_offset_x     = 5
  let icon_sheet_offset_y     = 10
  let icon_sheet_res          = 16.0
  let number_arrow_size       = 4.0
  let tool_radius             = 4.0
  let option_radius           = 4.0
  let option_width            = 14.0
  let option_height           = 15.0
  let text_radius             = 4.0
  let number_radius           = 10.0
  let menu_radius             = 3.0
  let shadow_feather          = 12.0
  let shadow_alpha            = 0.5
  let scrollbar_radius        = 7.0
  let scrollbar_active_shade  = 0.15
  let max_glyphs              = 1024
  let max_rows                = 32
  let text_pad_down           = 7.0
  let node_wire_outline_width = 4.0
  let node_wire_width         = 2.0
  let node_radius             = 8.0
  let node_title_feather      = 1.0
  let node_arrow_size         = 9.0
end
open Default

let theme =
  let open Color in
  let c_0_098 = gray 0.098 in
  let c_0_275 = gray 0.275 in
  let c_0_353 = gray 0.353 in
  let c_0_392 = gray 0.392 in
  let c_0_447 = gray 0.447 in
  let c_0_502 = gray 0.502 in
  let c_0_600 = gray 0.600 in
  let c_0_706 = gray 0.706 in
  let c_0_800 = gray 0.800 in
  let c_text = black in
  let c_text_selected = white in
  let tooltip_and_menu = {
    outline        = black;
    item           = c_0_392;
    inner          = gray ~a:0.902 0.098;
    inner_selected = gray ~a:0.902 0.176;
    text           = gray 0.627;
    text_selected  = c_text_selected;
    shade_top      = 0.0;
    shade_down     = 0.0;
  }
  in
  {
    background = c_0_447;
    regular = {
      outline        = c_0_098;
      item           = c_0_098;
      inner          = c_0_600;
      inner_selected = c_0_392;
      text           = c_text;
      text_selected  = c_text_selected;
      shade_top      = 0.0;
      shade_down     = 0.0;
    };

    tool_button = {
      outline        = c_0_098;
      item           = c_0_098;
      inner          = c_0_600;
      inner_selected = c_0_392;
      text           = c_text;
      text_selected  = c_text_selected;
      shade_top      = 0.15;
      shade_down     = -0.15;
    };

    radio_button = {
      outline        = black;
      item           = white;
      inner          = c_0_275;
      inner_selected = v 0.337 0.502 0.761 1.0;
      text           = c_text;
      text_selected  = c_text_selected;
      shade_top      = 0.15;
      shade_down     = -0.15;
    };

    text_field = {
      outline        = c_0_098;
      item           = c_0_353;
      inner          = c_0_600;
      inner_selected = c_0_600;
      text           = c_text;
      text_selected  = c_text_selected;
      shade_top      = 0.0;
      shade_down     = 0.25;
    };

    option = {
      outline        = black;
      item           = white;
      inner          = c_0_275;
      inner_selected = c_0_275;
      text           = c_text;
      text_selected  = c_text_selected;
      shade_top      = 0.15;
      shade_down     = -0.15;
    };

    choice = {
      outline        = black;
      item           = white;
      inner          = c_0_275;
      inner_selected = c_0_275;
      text           = c_text_selected;
      text_selected  = c_0_800; (*  color_text_selected *)
      shade_top      = 0.15;
      shade_down     = -0.15;
    };

    number_field = {
      outline        = c_0_098;
      item           = c_0_353;
      inner          = c_0_706;
      inner_selected = c_0_600;
      text           = c_text;
      text_selected  = c_text_selected;
      shade_top      = -0.20;
      shade_down     = 0.0;
    };

    slider = {
      outline        = c_0_098;
      item           = c_0_502;
      inner          = c_0_706;
      inner_selected = c_0_600;
      text           = c_text;
      text_selected  = c_text_selected;
      shade_top      = -0.20;
      shade_down     = 0.0;
    };

    scrollbar = {
      outline        = gray 0.196;
      item           = c_0_502;
      inner          = gray ~a:0.706 0.314;
      inner_selected = gray ~a:0.706 0.392;
      text           = c_text;
      text_selected  = c_text_selected;
      shade_top      = 0.5;
      shade_down     = -0.5;
    };

    tooltip = tooltip_and_menu;

    menu = tooltip_and_menu;

    menu_item = {
      outline        = black;
      item           = gray ~a:0.502 0.675;
      inner          = gray ~a:0.0 0.0;
      inner_selected = v 0.337 0.502 0.761 1.0;
      text           = c_text_selected;
      text_selected  = c_text;
      shade_top      = 0.38;
      shade_down     = 0.0;
    };

    node = {
      node_selected      = v 0.945 0.345 0.0 1.0;
      wire               = black;
      node_text_selected = v 0.498 0.439 0.439 1.0;
      node_active        = v 1.0 0.667 0.251 1.0;
      wire_selected      = white;
      node_backdrop      = gray ~a:0.627 0.608;
      noodle_curving     = 0.5;
    }
  }

type icon = unit

type canvas = Wall_canvas.t

let minf a b : float = if a < b then a else b
let maxf a b : float = if a > b then a else b

let clampf x a b = maxf (minf x b) a

let draw_icon t xf x y icon =
  () (* TODO *)

let icon_label_value t xf box ?icon ?label ?value color ~align ~fontsize =
  let x = B2.minx box and y = B2.miny box in
  let pleft = Default.pad_left in
  match label with
  | Some label ->
    let pleft = match icon with
      | None -> pleft
      | Some icon ->
        draw_icon t xf (x +. 4.0) (y +. 2.0) icon;
        pleft +. Default.icon_sheet_res
    in
    (* TODO *)
    ignore pleft

  | None -> begin match icon with
      | None -> ()
      | Some icon -> draw_icon t xf (x +. 2.0) (y +. 2.0) icon
    end

let label t box ?icon label =
  icon_label_value t box ?icon ~label

let select_corners (corners : corner_flags) r =
  let test x = if List.mem x corners then r else 0.0 in
  (test `TOP_LEFT, test `TOP_RIGHT, test `DOWN_LEFT, test `DOWN_RIGHT)

let offset_color color = function
  | 0.0 -> color
  | delta ->
    let f x = clampf (x +. delta) 0.0 1.0 in
    let (r,g,b,a) = Gg.V4.to_tuple color in
    Color.v (f r) (f g) (f b) a

let transparent color =
  Color.with_a color (Color.a color *. Default.transparent_alpha)

let draw_bevel_inset t xf box cr2 cr3 =
  let x1 = B2.minx box in
  let y1 = B2.miny box -. 0.5 in
  let x2 = B2.maxx box in
  let y2 = B2.maxy box -. 0.5 in
  let d = minf (B2.w box) (B2.h box) in
  let cr2 =  minf cr2 (d /. 2.0) in
  let cr3 =  minf cr3 (d /. 2.0) in
  C.new_path t xf;
  C.move_to t x2 (y2-.cr2);
  C.arc_to t x1 y2 x1 y1 cr3;
  let bevel_color =
    offset_color theme.background Default.inset_bevel_shade
  in
  C.stroke t (Paint.linear_gradient
                ~sx:x1 ~sy:(y2 -. maxf cr2 cr3 -. 1.0)
                ~ex:x1 ~ey:(y2 -. 1.0)
                ~inner:(Color.with_a bevel_color 0.0)
                ~outer:bevel_color)
    {Outline.default with Outline.stroke_width = 1.0}

let draw_bevel t xf box =
  let x1 = B2.minx box +. 0.5 and y1 = B2.miny box +. 0.5 in
  let x2 = B2.maxx box -. 0.5 and y2 = B2.maxy box -. 0.5 in
  C.new_path t xf;
  C.move_to t ~x:x1 ~y:y2;
  C.line_to t ~x:x2 ~y:y2;
  C.line_to t ~x:x1 ~y:y2;
  let color = offset_color theme.background (-.Default.bevel_shade) in
  C.stroke t (Paint.color (transparent color)) Outline.default;
  C.new_path t xf;
  C.move_to t ~x:x1 ~y:y2;
  C.line_to t ~x:x1 ~y:y1;
  C.line_to t ~x:x2 ~y:y1;
  let color = offset_color theme.background Default.bevel_shade in
  C.stroke t (Paint.color (transparent color)) Outline.default

let inner_colors {inner; inner_selected; shade_top; shade_down}
    (state : widget_state) ~flip =
  match state with
  | `DEFAULT ->
    (offset_color inner shade_top, offset_color inner shade_down)
  | `HOVER   ->
    let color = offset_color inner Default.hover_shade in
    (offset_color color shade_top, offset_color color shade_down)
  | `ACTIVE  ->
    let a, b =
      if flip then (shade_down, shade_top) else (shade_top, shade_down)
    in
    (offset_color inner_selected a, offset_color inner_selected b)

let rounded_box t box ~corners:(cr0, cr1, cr2, cr3) =
  let w = B2.w box and h = B2.h box in
  if w > 0.0 && h > 0.0 then (
    let d = if w < h then w else h in
    let x1 = B2.minx box and y1 = B2.miny box in
    let x2 = B2.maxx box and y2 = B2.maxy box in
    C.move_to t x1 (B2.midy box);
    C.arc_to t x1 y1 x2 y1 (minf cr0 (d/.2.0));
    C.arc_to t x2 y1 x2 y2 (minf cr1 (d/.2.0));
    C.arc_to t x2 y2 x1 y2 (minf cr2 (d/.2.0));
    C.arc_to t x1 y2 x1 y1 (minf cr3 (d/.2.0));
    C.close_path t;
  )

let offset_box box x1 y1 x2 y2 =
  let p1 = B2.tl_pt box in
  B2.v
    (P2.v (P2.x p1 +. x1) (P2.y p1 +. y1))
    (Gg.Size2.v (B2.w box +. x2 -. x1) (B2.h box +. y2 -. y1))

let inner_box t xf ?frame box (cr0, cr1, cr2, cr3) inner outer =
  let x1 = B2.minx box and y1 = B2.miny box in
  let x2 = B2.maxx box and y2 = B2.maxy box in
  C.new_path t xf;
  rounded_box t (offset_box box 1.0 1.0 (-2.0) (-3.0))
    ~corners:(max 0.0 cr0, max 0.0 cr1, max 0.0 cr2, max 0.0 cr3);
  C.fill t ?frame
    (if B2.h box -. 2.0 > B2.w box
     then Paint.linear_gradient ~sx:x1 ~sy:y1 ~ex:x2 ~ey:y1 ~inner ~outer
     else Paint.linear_gradient ~sx:x1 ~sy:y1 ~ex:x1 ~ey:y2 ~inner ~outer)

let outline_box t xf box corners color =
  C.new_path t xf;
  rounded_box t (offset_box box 0.5 0.5 (-1.0) (-2.0)) ~corners;
  C.stroke t (Paint.color color)
    {Outline.default with Outline.stroke_width = 1.0}

let tool_button t xf box ~corners state ?icon text =
  let (_,_,cr2,cr3) as corners =
    select_corners corners Default.text_radius
  in
  let (shade_top, shade_down) =
    inner_colors theme.radio_button state ~flip:true
  in
  draw_bevel_inset t xf box cr2 cr3;
  inner_box t xf box corners shade_top shade_down;
  outline_box t xf box corners theme.radio_button.outline;
  icon_label_value t xf box ?icon

let radio_button t xf box ~corners state ?icon ?label () =
  let (_, _, cr2, cr3) as corners =
    select_corners corners Default.option_radius in
  draw_bevel_inset t xf box cr2 cr3;
  let shade_top, shade_down =
    inner_colors theme.tool_button state ~flip:true in
  inner_box t xf box corners shade_top shade_down;
  outline_box t xf box corners (transparent theme.tool_button.outline);
  icon_label_value t xf box ?icon ?label
    theme.regular.text
    ~align:`CENTER ~fontsize:Default.label_font_size

let icon_label_text_position t xf box ?icon ~fontsize text pt =
  let pleft = match icon with
    | None -> Default.text_radius
    | Some _ -> Default.text_radius +. Default.icon_sheet_res
  in
  let x = B2.minx box +. pleft in
  let y = B2.miny box +. Default.(widget_height -. text_pad_down) in
  (* TODO
     nvgFontFaceId(ctx, bnd_font);
     nvgFontSize(ctx, fontsize);
     nvgTextAlign(ctx, NVG_ALIGN_LEFT | NVG_ALIGN_BASELINE); *)
  let w = B2.w box -. (Default.text_radius +. pleft) in
  (* TODO
     float asc, desc, lh;
     static NVGtextRow rows[BND_MAX_ROWS];
     int nrows = nvgTextBreakLines(
         ctx, label, NULL, w, rows, BND_MAX_ROWS);
     if (nrows == 0) return 0;
     nvgTextBoxBounds(ctx, x, y, w, label, NULL, bounds);
     nvgTextMetrics(ctx, &asc, &desc, &lh);

     // calculate vertical position
     int row = bnd_clamp((int)((float)(py - bounds[1]) / lh), 0, nrows - 1);
     // search horizontal position
     static NVGglyphPosition glyphs[BND_MAX_GLYPHS];
     int nglyphs = nvgTextGlyphPositions(
         ctx, x, y, rows[row].start, rows[row].end + 1, glyphs, BND_MAX_GLYPHS);
     int col, p = 0;
     for (col = 0; col < nglyphs && glyphs[col].x < px; ++col)
         p = glyphs[col].str - label;
     // see if we should move one character further
     if (col > 0 && col < nglyphs && glyphs[col].x - px < px - glyphs[col - 1].x)
         p = glyphs[col].str - label;
     return p; *)
  ignore (x,y,w)



let text_field_text_position t xf box ?icon text pt =
  icon_label_text_position t xf box ?icon text pt
    ~fontsize:Default.label_font_size

let icon_label_caret t xf box ?icon color1 ~fontsize text color2 ~caret =
  (* TODO *)
  ()

let text_color theme = function
  | `ACTIVE -> theme.text_selected
  | _ -> theme.text

let text_field t xf box ~corners state ?icon text ~caret =
  let (_, _, cr2, cr3) as corners =
    select_corners corners Default.text_radius in
  let shade_top, shade_down =
    inner_colors theme.text_field state ~flip:false in
  draw_bevel_inset t xf box cr2 cr3;
  inner_box t xf box corners shade_top shade_down;
  outline_box t xf box corners
    (transparent theme.text_field.outline);
  let caret = if state <> `ACTIVE then (fst caret, -1) else caret in
  icon_label_caret t xf box ?icon
    (text_color theme.text_field state)
    ~fontsize:Default.label_font_size
    text theme.text_field.item ~caret

let draw_check t xf ~x ~y color =
  C.new_path t xf;
  C.move_to t (x+.4.0) (y+.5.0);
  C.line_to t (x+.7.0) (y+.8.0);
  C.line_to t (x+.14.0) (y+.1.0);
  C.stroke t (Paint.color color)
    Outline.({default with
              line_cap = `BUTT; line_join = `MITER;
              stroke_width = 2.0})

let option_button t xf box state label =
  let ox = B2.minx box in
  let oy = B2.maxy box +. Default.option_height -. 3.0 in
  let box' = B2.v (P2.v ox oy)
      Default.(Gg.Size2.v option_width option_height) in
  draw_bevel_inset t xf box'
    Default.option_radius Default.option_radius;
  let corners =
    Default.(option_radius, option_radius, option_radius, option_radius) in
  let shade_top, shade_down =
    inner_colors theme.option state ~flip:true in
  inner_box t xf box' corners shade_top shade_down;
  outline_box t xf box' corners
    (transparent theme.option.outline);
  if state = `ACTIVE then
    draw_check t xf (transparent theme.option.item) ~x:ox ~y:oy;
  icon_label_value t xf (offset_box box 12.0 0.0 (-12.0) (-1.0))
    (text_color theme.option state)
    ~align:`LEFT ~fontsize:Default.label_font_size ~label

let draw_up_down_arrow t xf ~x ~y ~size color =
  let w = 1.1 *. size in
  C.new_path t xf;
  C.move_to t x (y-.1.0);
  C.line_to t (x+.0.5*.w) (y-.size-.1.0);
  C.line_to t (x+.w) (y-.1.0);
  C.close_path t;
  C.move_to t x (y+.1.);
  C.line_to t (x+.0.5*.w) (y+.size+.1.0);
  C.close_path t;
  (* CHECK: two close paths?! *)
  C.fill t (Paint.color color)

let choice_button t xf box ~corners state ?icon label =
  let (_, _, cr2, cr3) as corners =
    select_corners corners Default.option_radius in
  let shade_top, shade_down =
    inner_colors theme.choice state ~flip:true in
  draw_bevel_inset t xf box cr2 cr3;
  inner_box t xf box corners shade_top shade_down;
  outline_box t xf box corners
    (transparent theme.choice.outline);
  icon_label_value t xf box ?icon
    (text_color theme.choice state)
    ~align:`LEFT ~fontsize:Default.label_font_size ~label;
  let x = B2.maxx box -. 10.0 and y = B2.miny box +. 10.0 in
  draw_up_down_arrow t xf ~x ~y ~size:5.0
    (transparent theme.choice.item)

let color_button t xf box ~corners color =
  let (_, _, cr2, cr3) as corners =
    select_corners corners Default.tool_radius in
  draw_bevel_inset t xf box cr2 cr3;
  inner_box t xf box corners color color;
  outline_box t xf box corners
    (transparent theme.tool_button.outline)

let draw_arrow t xf ~x ~y ~size color =
  C.new_path t xf;
  C.move_to t x y;
  C.line_to t (x-.size) (y+.size);
  C.line_to t (x-.size) (y-.size);
  C.close_path t;
  C.fill t (Paint.color color)

let number_field t xf box ~corners state label value =
  let (_, _, cr2, cr3) as corners =
    select_corners corners Default.tool_radius in
  let shade_top, shade_down =
    inner_colors theme.choice state ~flip:true in
  draw_bevel_inset t xf box cr2 cr3;
  inner_box t xf box corners shade_top shade_down;
  outline_box t xf box corners
    (transparent theme.number_field.outline);
  icon_label_value t xf box
    (text_color theme.number_field state)
    ~align:`CENTER
    ~fontsize:Default.label_font_size
    ~label ~value;
  let y = B2.miny box +. 10.0 in
  let x1 = B2.minx box +. 8.0 and x2 = B2.maxx box -. 8.0 in
  draw_arrow t xf ~x:x1 ~y ~size:(-.Default.number_arrow_size)
    (transparent theme.number_field.item);
  draw_arrow t xf ~x:x2 ~y ~size:Default.number_arrow_size
    (transparent theme.number_field.item)

let slider t xf box ~corners state ~progress ~label ~value =
  let (_, _, cr2, cr3) as corners =
    select_corners corners Default.number_radius in
  let shade_top, shade_down =
    inner_colors theme.slider state ~flip:false in
  inner_box t xf box corners shade_top shade_down;

  let shade_top =
    (offset_color theme.slider.item theme.slider.shade_top)
  and shade_down =
    (offset_color theme.slider.item theme.slider.shade_down)
  in
  let shade_top, shade_down =
    if state = `ACTIVE
    then (shade_top, shade_down)
    else (shade_down, shade_top)
  in
  (* TODO nvgScissor(ctx,x,y,8+(w-8)*bnd_clamp(progress,0,1),h); *)
  inner_box t xf box corners shade_top shade_down;
  (* TODO nvgResetScissor(ctx); *)
  outline_box t xf box corners (transparent theme.slider.outline);
  icon_label_value t xf box
    (text_color theme.slider state)
    ~align:`CENTER
    ~fontsize:Default.label_font_size
    ?label ?value

let scroll_handle box ~offset ~size =
  let size = clampf size 0.0 1.0 in
  let offset = clampf offset 0.0 1.0 in
  let x = B2.minx box and y = B2.miny box in
  let w = B2.w box and h = B2.h box in
  if h > w then (
    let hs = maxf (size *. h) (w +. 1.0) in
    B2.v (P2.v x (y +. (h -. hs) *. offset))
      (Gg.Size2.v w hs)
  ) else (
    let ws = maxf (size *. w) (h -. 1.0) in
    B2.v (P2.v (x +. (w -. ws) *. offset) y)
      (Gg.Size2.v ws h)
  )

let draw_drop_shadow t xf box ~r ~feather ~alpha =
  C.new_path t xf;
  let x1 = B2.minx box and y1 = B2.miny box in
  let x2 = B2.maxx box and y2 = B2.maxy box in
  C.move_to t (x1 -. feather) y1;
  C.line_to t x1 y1;
  C.line_to t x1 (y2 -. feather);
  C.arc_to t ~x1 ~y1:y2 ~x2:(x1+.r) ~y2 ~r;
  C.arc_to t ~x1:x2 ~y1:y2 ~x2 ~y2:(y2-.r) ~r;
  C.line_to t x2 y1;
  C.line_to t (x2+.feather) y1;
  C.line_to t (x2+.feather) (y2+.feather);
  C.line_to t (x1-.feather) (y2+.feather);
  C.close_path t;
  C.fill t (Paint.box_gradient
              ~x:(x1 -. feather *. 0.5)
              ~y:(y1 +. feather *. 0.5)
              ~w:(x2 -. x1 +. feather)
              ~h:(y2 -. y1)
              ~r:(r +. feather *. 0.5)
              ~f:feather
              ~inner:(Color.gray ~a:(alpha*.alpha) 0.0)
              ~outer:(Color.gray ~a:0.0 0.0))

let scroll_bar t xf box state ~offset ~size =
  let corners =
    Default.(scrollbar_radius,scrollbar_radius,scrollbar_radius,scrollbar_radius) in
  draw_bevel_inset t xf box Default.scrollbar_radius Default.scrollbar_radius;
  inner_box t xf box corners
    (offset_color theme.scrollbar.inner (3.0 *. theme.scrollbar.shade_down))
    (offset_color theme.scrollbar.inner (3.0 *. theme.scrollbar.shade_top));
  outline_box t xf box corners
    (transparent theme.scrollbar.outline);

  let box = scroll_handle box ~offset ~size in
  let item_color = theme.scrollbar.item in
  let item_color = if state = `ACTIVE
    then offset_color item_color Default.scrollbar_active_shade
    else item_color
  in
  inner_box t xf box corners
    (offset_color item_color (3.0 *. theme.scrollbar.shade_top))
    (offset_color item_color (3.0 *. theme.scrollbar.shade_down));
  outline_box t xf box corners
    (transparent theme.scrollbar.outline)

let menu_background t xf box ~corners =
  let corners = select_corners corners Default.menu_radius in
  let shade_top, shade_down =
    inner_colors theme.menu `DEFAULT ~flip:false in
  let box' = offset_box box 0.0 0.0 0.0 1.0 in
  inner_box t xf box' corners shade_top shade_down;
  outline_box t xf box' corners (transparent theme.menu.outline);
  draw_drop_shadow t xf box
    ~r:Default.menu_radius
    ~feather:Default.shadow_feather
    ~alpha:Default.shadow_alpha

let menu_label t xf box ?icon label =
  icon_label_value t xf box theme.menu.text
    ?icon ~align:`LEFT ~fontsize:Default.label_font_size ~label

let menu_item t xf box state ?icon label =
  let state =
    if state = `DEFAULT then state else (
      inner_box t xf box (0.0,0.0,0.0,0.0)
        (offset_color theme.menu_item.inner_selected theme.menu_item.shade_top)
        (offset_color theme.menu_item.inner_selected theme.menu_item.shade_down);
      `ACTIVE
    )
  in
  icon_label_value t xf box ?icon
    (text_color theme.menu_item state)
    ~align:`LEFT
    ~fontsize:Default.label_font_size
    ~label

let tooltip_background t xf box =
  let shade_top, shade_down =
    inner_colors theme.tooltip `DEFAULT ~flip:false in
  let corners = Default.(menu_radius,menu_radius,menu_radius,menu_radius) in
  let box' = offset_box box 0.0 0.0 0.0 1.0 in
  inner_box t xf box' corners shade_top shade_down;
  outline_box t xf box' corners (transparent theme.tooltip.outline);
  draw_drop_shadow t xf box ~r:Default.menu_radius
    ~feather:Default.shadow_feather ~alpha:Default.shadow_alpha

let node_port t xf ~x ~y state color =
  C.new_path t xf;
  C.circle t ~cx:x ~cy:y ~r:Default.node_port_radius;
  C.stroke t
    (Paint.color theme.node.wire)
    {Outline.default with Outline.stroke_width = 1.0};
  C.fill t (Paint.color (if state = `DEFAULT then color
                         else offset_color color Default.hover_shade))

let colored_node_wire t xf x0 y0 c0 x1 y1 c1 =
  let length = maxf (abs_float (x1 -. x0)) (abs_float (y1 -. y0)) in
  let delta = length *. theme.node.noodle_curving in
  C.new_path t xf;
  C.move_to t x0 y0;
  C.bezier_to t
    ~c1x:(x0 +. delta) ~c1y:y0 ~c2x:(x1 -. delta) ~c2y:y1 ~x:x1 ~y:y1;
  let colorw =
    Color.with_a theme.node.wire
      (minf (Color.a c0) (Color.a c1))
  in
  C.stroke t (Paint.color colorw)
    Outline.({default with stroke_width = Default.node_wire_outline_width});
  C.stroke t (Paint.linear_gradient x0 y0 x1 y1 c0 c1)
    Outline.({default with stroke_width = Default.node_wire_width})

let node_wire_color =
  let gray = Color.gray 0.5 in
  fun theme -> function
    | `DEFAULT -> gray
    | `HOVER   -> theme.wire_selected
    | `ACTIVE  -> theme.node_active

let node_wire t xf x0 y0 s0 x1 y1 s1 =
  colored_node_wire t xf
    x0 y0 (node_wire_color theme.node s0)
    x1 y1 (node_wire_color theme.node s1)

let b2_with_h box h =
  let tl = B2.tl_pt box and w = B2.w box in
  B2.v tl (Gg.Size2.v w h)

let node_icon_label t xf box ?icon c0 c1 ~align ~fontsize label =
  (* TODO *)
  ()

let node_background t xf box state ?icon label color =
  inner_box t xf
    (b2_with_h box (Default.node_title_height +. 2.0))
    Default.(node_radius,node_radius,0.0,0.0)
    (transparent (offset_color color Default.bevel_shade))
    (transparent color);
  inner_box t xf
    (offset_box box
       0.0 (Default.node_title_height -. 1.0)
       0.0 (2.0 -. Default.node_title_height))
    Default.(0.0,0.0,node_radius,node_radius)
    (transparent theme.node.node_backdrop)
    (transparent theme.node.node_backdrop);
  node_icon_label t xf
    (offset_box (b2_with_h box Default.node_title_height)
       Default.node_arrow_area_width 0.0
       Default.(-. node_arrow_area_width -. node_margin_side) 0.0)
    theme.regular.text
    (offset_color color Default.bevel_shade)
    ?icon ~align:`LEFT ~fontsize:Default.label_font_size
    label;
  let border_color, arrow_color = match state with
    | `DEFAULT -> Color.black, offset_color color (-. Default.bevel_shade)
    | `HOVER   -> (theme.node.node_selected,theme.node.node_selected)
    | `ACTIVE  -> (theme.node.node_active,theme.node.node_selected)
  in
  outline_box t xf (offset_box box 0.0 0.0 0.0 1.0)
    Default.(node_radius,node_radius,node_radius,node_radius)
    (transparent border_color);
  draw_drop_shadow t xf box ~r:Default.node_radius
    ~feather:Default.shadow_feather ~alpha:Default.shadow_alpha

let splitter_widgets t xf box =
  let inset = transparent theme.background in
  let inset_light =
    transparent (offset_color inset Default.splitter_shade)
  and inset_dark =
    transparent (offset_color inset (-. Default.splitter_shade))
  in
  let x1 = B2.minx box and y1 = B2.miny box
  and x2 = B2.maxx box and y2 = B2.maxy box in

  C.new_path t xf;
  C.move_to t (x1 +. 0.0 ) (y2 -. 13.0);
  C.line_to t (x1 +. 13.0) (y2 +. 0.0 );
  C.move_to t (x1        ) (y2 -. 9.0 );
  C.line_to t (x1 +. 9.0 ) (y2        );
  C.move_to t (x1        ) (y2 -. 5.0 );
  C.line_to t (x1 +. 5.0 ) (y2        );

  C.move_to t (x2 -. 11.0) (y1        );
  C.line_to t (x2        ) (y1 +. 11.0);
  C.move_to t (x2 -. 7.0 ) (y1        );
  C.line_to t (x2        ) (y1 +. 7.0 );
  C.move_to t (x2 -. 3.0 ) (y1        );
  C.line_to t (x2        ) (y1 +. 3.0 );

  C.stroke t (Paint.color inset_dark) Outline.default;

  C.new_path t xf;
  C.move_to t (x1        ) (y2 -. 11.0);
  C.line_to t (x1 +. 11.0) (y2        );
  C.move_to t (x1        ) (y2 -. 7.0 );
  C.line_to t (x1 +. 7.0 ) (y2        );
  C.move_to t (x1        ) (y2 -. 3.0 );
  C.line_to t (x1 +. 3.0 ) (y2        );

  C.move_to t (x2 -. 13.0) (y1        );
  C.line_to t (x2        ) (y1 +. 13.0);
  C.move_to t (x2 -. 9.0 ) (y1        );
  C.line_to t (x2        ) (y1 +. 9.0 );
  C.move_to t (x2 -. 5.0 ) (y1        );
  C.line_to t (x2        ) (y1 +. 5.0 );

  C.stroke t (Paint.color inset_light) Outline.default;

  C.new_path t xf;
  C.move_to t (x1        ) (y2 -. 12.0);
  C.line_to t (x1 +. 12.0) (y2        );
  C.move_to t (x1        ) (y2 -. 8.0 );
  C.line_to t (x1 +. 8.0 ) (y2        );
  C.move_to t (x1        ) (y2 -. 4.0 );
  C.line_to t (x1 +. 4.0 ) (y2        );

  C.move_to t (x2 -. 12.0) (y1        );
  C.line_to t (x2        ) (y1 +. 12.0);
  C.move_to t (x2 -. 8.0 ) (y1        );
  C.line_to t (x2        ) (y1 +. 8.0 );
  C.move_to t (x2 -. 4.0 ) (y1        );
  C.line_to t (x2        ) (y1 +. 4.0 );

  C.stroke t (Paint.color inset)

let join_area_overlay t xf box ~vertical ~mirror =
  let x = B2.minx box and y = B2.miny box in
  let w, h =
    let w = B2.w box and h = B2.h box in
    if vertical then h, w else w, h
  in
  let s = minf w h in
  let x0, y0, x1, y1, s =
    if mirror
    then (w, h, 0.0, 0.0, -.s)
    else (0.0, 0.0, w, h, s)
  in
  let s2 = s /. 2.0 and s4 = s /. 4.0 and s8 = s /. 8.0 in
  let yc = (y0 +. y1) *. 0.5 and x4 = x0 +. s4 in
  let points = [|
    x0       ; y0       ;
    x1       ; y0       ;
    x1       ; y1       ;
    x0       ; y1       ;
    x0       ; yc +. s8 ;
    x4       ; yc +. s8 ;
    x4       ; yc +. s4 ;
    x0 +. s2 ; yc       ;
    x4       ; yc -. s4 ;
    x4       ; yc -. s8 ;
    x0       ; yc -. s8 ;
  |] in
  C.new_path t xf;
  let vertical = if vertical then 1 else 0 in
  C.move_to t (x +. points.(vertical)) (y +. points.(1-vertical));
  for i = 1 to Array.length points / 2 - 1 do
    C.line_to t
      (x +. points.(2 * i + vertical))
      (y +. points.(2 * i + 1 - vertical))
  done;
  C.fill t (Paint.color (Color.gray ~a:0.3 0.0))

let label_width t ?icon label =
  let w = Default.(pad_left +. pad_right) in
  let w = match icon with
    | None -> w
    | Some _ -> w +. Default.icon_sheet_res
  in
  (*if (label && (bnd_font >= 0)) {
      nvgFontFaceId(ctx, bnd_font);
      nvgFontSize(ctx, BND_LABEL_FONT_SIZE);
      w += nvgTextBounds(ctx, 1, 1, label, NULL, NULL);
    }*)
  w

let label_height t ?icon label ~width =
  let h = Default.widget_height in
  let width = width -. Default.text_radius *. 2.0 in
  let width = match icon with
    | None -> width
    | Some _ -> width +. Default.icon_sheet_res
  in
  (*if (label && (bnd_font >= 0)) {
      nvgFontFaceId(ctx, bnd_font);
      nvgFontSize(ctx, BND_LABEL_FONT_SIZE);
      float bounds[4];
      nvgTextBoxBounds(ctx, 1, 1, width, label, NULL, bounds);
      int bh = int(bounds[3] - bounds[1]) + BND_TEXT_PAD_DOWN;
      if (bh > h)
      h = bh;
    }*)
  ignore h;
  width

let draw_background t xf box =
  C.new_path t xf;
  C.rect t (B2.minx box) (B2.miny box) (B2.w box) (B2.h box);
  C.fill t (Paint.color theme.background)

let draw_node_arrow_down t xf ~x ~y ~size color =
  C.new_path t xf;
  C.move_to t x y;
  C.line_to t (x +. size *. 0.5) (y -. size);
  C.line_to t (x -. size *. 0.5) (y -. size);
  C.close_path t;
  C.fill t (Paint.color color)
