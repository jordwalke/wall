open Tsdl

open Wall
module C = Wall_canvas

let normalize (dx, dy) =
  let d = sqrt (dx *. dx +. dy *. dy) in
  if d > 1.0 then
    (dx /. d, dy /. d)
  else
    (dx, dy)

let load_font name =
  let ic = open_in_bin name in
  let dim = in_channel_length ic in
  let fd = Unix.descr_of_in_channel ic in
  let buffer = Bigarray.Array1.map_file fd Bigarray.int8_unsigned Bigarray.c_layout false dim in
  let offset = List.hd (Stb_truetype.enum buffer) in
  match Stb_truetype.init buffer offset with
  | None -> assert false
  | Some font -> font

let font_icons = lazy (load_font "entypo.ttf")
let font_sans = lazy (load_font "Roboto-Regular.ttf")
let font_sans_bold = lazy (load_font "Roboto-Bold.ttf")
let font_emoji = lazy (load_font "NotoEmoji-Regular.ttf")

module Demo = struct
  let draw_eyes vg xf x y w h mx my t =
    let ex = w *. 0.23 in
    let ey = h *. 0.5 in
    let lx = x +. ex in
    let ly = y +. ey in
    let rx = x +. w -. ex in
    let ry = y +. ey in
    let br = min ex ey *. 0.5 in
    let blink = 1.0 -. (sin (t *. 0.5) ** 200.0) *. 0.8 in

    C.new_path vg xf;
    C.ellipse vg ~cx:(lx +. 3.0) ~cy:(ly +. 16.0) ~rx:ex ~ry:ey;
    C.ellipse vg ~cx:(rx +. 3.0) ~cy:(ry +. 16.0) ~rx:ex ~ry:ey;
    C.fill vg (Paint.linear_gradient
                 ~sx:x ~sy:(y +. h *. 0.5) ~ex:(x +. w *. 0.1) ~ey:(y +. h)
                 ~inner:(Color.v 0.0 0.0 0.0 0.125)
                 ~outer:(Color.v 0.0 0.0 0.0 0.0625));

    C.new_path vg xf;
    C.ellipse vg ~cx:lx ~cy:ly ~rx:ex ~ry:ey;
    C.ellipse vg ~cx:rx ~cy:ry ~rx:ex ~ry:ey;
    C.fill vg (Paint.linear_gradient
                 ~sx:x ~sy:(y +. h *. 0.25) ~ex:(x +. w *. 0.1) ~ey:(y +. h)
                 ~inner:(Color.v 0.86 0.86 0.86 1.0)
                 ~outer:(Color.v 0.5 0.5 0.5 1.0));
    let dx, dy = normalize
        ((mx -. rx) /. (ex *. 10.0), (my -. ry) /. (ey *. 10.0)) in
    let dx = dx *. ex *. 0.4 in
    let dy = dy *. ey *. 0.5 in

    C.new_path vg xf;
    C.ellipse vg ~cx:(lx +. dx) ~cy:(ly +. dy +. ey *. 0.25 *. (1.0 -. blink))
      ~rx:br ~ry:(br *. blink);
    C.fill vg (Paint.color (Color.v 0.125 0.125 0.125 1.0));

    C.new_path vg xf;
    C.ellipse vg ~cx:(rx +. dx) ~cy:(ry +. dy +. ey *. 0.25 *. (1.0 -. blink))
      ~rx:br ~ry:(br *. blink);
    C.fill vg (Paint.color (Color.v 0.125 0.125 0.125 1.0));

    let gloss = Paint.radial_gradient
        ~cx:(lx -. ex *. 0.25) ~cy:(ry -. ey *. 0.5)
        ~inr:(ex *. 0.1) ~outr:(ex *. 0.75)
        ~inner:(Color.v 1.0 1.0 1.0 0.5)
        ~outer:(Color.v 1.0 1.0 1.0 0.0)
    in
    C.new_path vg xf;
    C.ellipse vg ~cx:lx ~cy:ly ~rx:ex ~ry:ey;
    C.fill vg gloss;

    let gloss = Paint.radial_gradient
        ~cx:(rx -. ex *. 0.25) ~cy:(ry -. ey *. 0.5)
        ~inr:(ex *. 0.1) ~outr:(ex *. 0.75)
        ~inner:(Color.v 1.0 1.0 1.0 0.5)
        ~outer:(Color.v 1.0 1.0 1.0 0.0)
    in
    C.new_path vg xf;
    C.ellipse vg ~cx:rx ~cy:ry ~rx:ex ~ry:ey;
    C.fill vg gloss

  let draw_graph vg xf x y w h t =
    let samples = [|
      (1.0 +. sin (t *. 1.2345  +. cos (t *. 0.33457) *. 0.44 )) *. 0.5;
      (1.0 +. sin (t *. 0.68363 +. cos (t *. 1.3    ) *. 1.55 )) *. 0.5;
      (1.0 +. sin (t *. 1.1642  +. cos (t *. 0.33457) *. 1.24 )) *. 0.5;
      (1.0 +. sin (t *. 0.56345 +. cos (t *. 1.63   ) *. 0.14 )) *. 0.5;
      (1.0 +. sin (t *. 1.6245  +. cos (t *. 0.254  ) *. 0.3  )) *. 0.5;
      (1.0 +. sin (t *. 0.345   +. cos (t *. 0.03   ) *. 0.6  )) *. 0.5;
    |] in
    let dx = w /. 5.0 in
    let sx i = x +. float i *. dx in
    let sy i = y +. h *. samples.(i) *. 0.8 in
    (* Graph background *)
    C.new_path vg xf;
    C.move_to vg ~x:(sx 0) ~y:(sy 0);
    for i = 1 to 5 do
      C.bezier_to vg
        ~c1x:(sx (i - 1) +. dx *. 0.5) ~c1y:(sy (i - 1))
        ~c2x:(sx i -. dx *. 0.5) ~c2y:(sy i)
        ~x:(sx i) ~y:(sy i)
    done;
    C.line_to vg ~x:(x +. w) ~y:(y +. h);
    C.line_to vg ~x ~y:(y +. h);
    C.fill vg (Paint.linear_gradient ~sx:x ~sy:y ~ex:x ~ey:(y +. h)
                 ~inner:(Color.v 0.00 0.60 0.75 0.00)
                 ~outer:(Color.v 0.00 0.60 0.75 0.25));

    (* Graph line *)
    C.new_path vg xf;
    C.move_to vg (sx 0) (sy 0 +. 2.0);
    for i = 1 to 5 do
      C.bezier_to vg
        ~c1x:(sx (i - 1) +. dx *. 0.5) ~c1y:(sy (i - 1) +. 2.0)
        ~c2x:(sx i -. dx *. 0.5) ~c2y:(sy i +. 2.0)
        ~x:(sx i) ~y:(sy i +. 2.0)
    done;
    C.stroke vg
      (Paint.color (Color.v 0.0 0.0 0.0 0.125))
      {Outline.default with Outline.stroke_width = 3.0};

    C.new_path vg xf;
    C.move_to vg (sx 0) (sy 0);
    for i = 1 to 5 do
      C.bezier_to vg
        ~c1x:(sx (i - 1) +. dx *. 0.5) ~c1y:(sy (i - 1))
        ~c2x:(sx i -. dx *. 0.5) ~c2y:(sy i)
        ~x:(sx i) ~y:(sy i)
    done;
    C.stroke vg
      (Paint.color (Color.v 0.0 0.60 0.75 1.0))
      {Outline.default with Outline.stroke_width = 3.0};

    (* Graph sample pos *)
    for i = 0 to 5 do
      C.new_path vg xf;
      C.rect vg ~x:(sx i -. 10.0) ~y:(sy i -. 10.0 +. 2.0) ~w:20.0 ~h:20.0;
      C.fill vg
        (Paint.radial_gradient ~cx:(sx i) ~cy:(sy i +. 2.0) ~inr:3.0 ~outr:8.0
           ~inner:(Color.v 0.0 0.0 0.0 0.125) ~outer:(Color.v 0.0 0.0 0.0 0.0))
    done;
    C.new_path vg xf;
    for i = 0 to 5 do
      C.circle vg ~cx:(sx i) ~cy:(sy i) ~r:4.0;
    done;
    C.fill vg (Paint.color (Color.v 0.0 0.6 0.75 1.0));
    C.new_path vg xf;
    for i = 0 to 5 do
      C.circle vg ~cx:(sx i) ~cy:(sy i) ~r:2.0;
    done;
    C.fill vg (Paint.color (Color.v 0.8 0.8 0.8 1.0))

  let draw_spinner ?frame vg xf cx cy r t =
    let a0 = 0.0 +. t *. 6.0 in
    let a1 = C.pi +. t *. 6.0 in
    let r0 = r in
    let r1 = r *. 0.75 in
    C.new_path vg xf;
    C.arc vg ~cx ~cy ~r:r0 ~a0:a0 ~a1:a1 `CW;
    C.arc vg ~cx ~cy ~r:r1 ~a0:a1 ~a1:a0 `CCW;
    C.close_path vg;
    let sx = cx +. cos a0 *. (r0 +. r1) *. 0.5 in
    let sy = cy +. sin a0 *. (r0 +. r1) *. 0.5 in
    let ex = cx +. cos a1 *. (r0 +. r1) *. 0.5 in
    let ey = cy +. sin a1 *. (r0 +. r1) *. 0.5 in
    C.fill ?frame vg
      (Paint.linear_gradient ~sx ~sy ~ex ~ey
         ~inner:(Color.v 0.0 0.0 0.0 0.0)
         ~outer:(Color.v 0.0 0.0 0.0 0.5))

  let draw_colorwheel vg xf x y w h t =
    let cx = x +. w *. 0.5 in
    let cy = y +. h *. 0.5 in
    let hue = sin (t *. 0.12) in
    let r1 = min w h *. 0.5 -. 5.0 in
    let r0 = r1 -. 20.0 in
    let aeps = 0.5 /. r1 in
    for i = 0 to 5 do
      let a0 = float i /. 6.0 *. C.pi *. 2.0 -. aeps in
      let a1 = (float i +. 1.0) /. 6.0 *. C.pi *. 2.0 +. aeps in
      C.new_path vg xf;
      C.arc vg ~cx ~cy ~r:r0 ~a0:a0 ~a1:a1 `CW;
      C.arc vg ~cx ~cy ~r:r1 ~a0:a1 ~a1:a0 `CCW;
      C.close_path vg;
      let sx = cx +. cos a0 *. (r0 +. r1) *. 0.5 in
      let sy = cy +. sin a0 *. (r0 +. r1) *. 0.5 in
      let ex = cx +. cos a1 *. (r0 +. r1) *. 0.5 in
      let ey = cy +. sin a1 *. (r0 +. r1) *. 0.5 in
      (*Printf.printf "sx=%f, sy=%f, ex=%f, ey=%f\n%!" sx sy ex ey;*)
      C.fill vg (Paint.linear_gradient
                   ~sx ~sy ~ex ~ey
                   ~inner:(Color.hsl (a0 /. (2.0 *. C.pi)) 1.0 0.55)
                   ~outer:(Color.hsl (a1 /. (2.0 *. C.pi)) 1.0 0.55));
    done;
    C.new_path vg xf;
    C.circle vg ~cx ~cy ~r:(r0 -. 0.5);
    C.circle vg ~cx ~cy ~r:(r1 +. 0.5);
    C.stroke vg (Paint.color (Color.v 0.0 0.0 0.0 0.25))
      Outline.{default with stroke_width = 1.0};

    let xf = Transform.(rotate (hue *. 2.0 *. C.pi) (translate ~x:cx ~y:cy xf)) in
    (* Selector *)
    C.new_path vg xf;
    C.rect vg (r0 -. 1.0) (-3.0) (r1-.r0+.2.) 6.0;
    C.stroke vg (Paint.color (Color.gray ~a:0.75 1.0))
      Outline.{default with stroke_width = 2.0};

    C.new_path vg xf;
    C.rect vg ~x:(r0-.2.0-.10.0) ~y:(-.4.0-.10.0)
      ~w:(r1-.r0+.4.0+.20.0) ~h:(8.0+.20.0);
    C.rect vg ~x:(r0-.2.0) ~y:(-4.0) ~w:(r1-.r0+.4.0) ~h:8.0;
    C.set_winding vg `HOLE;
    C.fill vg (Paint.box_gradient ~x:(r0-.3.0) ~y:(-5.0)
                 ~w:(r1-.r0+.6.0) ~h:10.0 ~r:2.0 ~f:4.0
                 ~inner:(Color.gray ~a:0.5 0.0) ~outer:(Color.gray ~a:0.0 0.0));

    (* Center triangle *)
    let r = r0 -. 6.0 in
    let ax = cos (120.0/.180.0 *. C.pi) *. r in
    let ay = sin (120.0/.180.0 *. C.pi) *. r in
    let bx = cos (-.120.0/.180.0 *. C.pi) *. r in
    let by = sin (-.120.0/.180.0 *. C.pi) *. r in
    C.new_path vg xf;
    C.move_to vg r 0.0;
    C.line_to vg ax ay;
    C.line_to vg bx by;
    C.close_path vg;
    (*Printf.printf "sx=%f, sy=%f, ex=%f, ey=%f\n%!" r 0.0 ax ay;*)
    C.fill vg (Paint.linear_gradient ~sx:r ~sy:0.0 ~ex:ax ~ey:ay
                 ~inner:(Color.hsl hue 1.0 0.5) ~outer:Color.white);
    C.fill vg (Paint.linear_gradient ~sx:((r+.ax)*.0.5) ~sy:((0.0+.ay)*.0.5)
                 ~ex:bx ~ey:by
                 ~inner:(Color.gray ~a:0.0 0.0) ~outer:(Color.gray ~a:1.0 0.0));
    C.stroke vg (Paint.color (Color.gray ~a:0.25 0.0)) Outline.default;

    (* Select circle on triangle *)
    let ax = cos (120.0/.180.0*.C.pi) *. r *. 0.3 in
    let ay = sin (120.0/.180.0*.C.pi) *. r *. 0.4 in
    C.new_path vg xf;
    C.circle vg ~cx:ax ~cy:ay ~r:5.0;
    C.stroke vg (Paint.color (Color.gray ~a:0.75 1.0))
      Outline.{default with stroke_width = 2.0};

    C.new_path vg xf;
    C.rect vg ~x:(ax -. 20.0) ~y:(ay -. 20.0) ~w:40.0 ~h:40.0;
    C.circle vg ~cx:ax ~cy:ay ~r:7.0;
    C.set_winding vg `HOLE;
    C.fill vg (Paint.radial_gradient ~cx:ax ~cy:ay ~inr:7.0 ~outr:9.0
                 ~inner:(Color.gray ~a:0.25 0.0) ~outer:(Color.gray ~a:0.0 0.0))

  let draw_lines vg xf x y w _h t =
    let pad = 5.0 in
    let s = w /. 9.0 -. pad *. 2.0 in
    let joins = [|`MITER; `ROUND; `BEVEL|] in
    let caps = [|`BUTT; `ROUND; `SQUARE|] in
    let px = function
      | 0 -> -.s*.0.25 +. cos (t*.0.3) *. s*.0.5
      | 1 -> -.s*.0.25
      | 2 -> s*.0.25
      | 3 -> s*.0.25 +. cos (-.t*.0.3) *. s*.0.5
      | _ -> assert false
    in
    let py = function
      | 0 -> sin (t*.0.3) *. s*.0.5
      | 1 -> 0.0
      | 2 -> 0.0
      | 3 -> sin (-.t*.0.3) *. s*.0.5
      | _ -> assert false
    in
    for i = 0 to 2 do
      for j = 0 to 2 do
        let fx = x +. s *. 0.5 +. float (i * 3 + j) /. 9.0 *. w +. pad in
        let fy = y -. s *. 0.5 +. pad in
        let px i = fx +. px i in
        let py i = fy +. py i in

        C.new_path vg xf;
        C.move_to vg (px 0) (py 0);
        C.line_to vg (px 1) (py 1);
        C.line_to vg (px 2) (py 2);
        C.line_to vg (px 3) (py 3);
        C.stroke vg (Paint.color (Color.gray ~a:0.625 0.0))
          Outline.{default with stroke_width = s *. 0.3;
                                line_cap = caps.(i);
                                line_join = joins.(j) };

        C.new_path vg xf;
        C.move_to vg (px 0) (py 0);
        C.line_to vg (px 1) (py 1);
        C.line_to vg (px 2) (py 2);
        C.line_to vg (px 3) (py 3);
        C.stroke vg (Paint.color (Color.v 0.0 0.75 1.0 1.0))
          Outline.{default with stroke_width = 1.0;
                                line_cap = `BUTT;
                                line_join = `BEVEL};
      done
    done

  let draw_widths vg xf x y w =
    let paint = Paint.color Color.black in
    let y = ref y in
    for i = 0 to 19 do
      C.new_path vg xf;
      C.move_to vg x !y;
      C.line_to vg (x+.w) (!y+.w*.0.3);
      let stroke_width = (float i +. 0.5) /. 10.0 in
      C.stroke vg paint Outline.{default with stroke_width};
      y := !y +. 10.0;
    done

  let draw_caps vg xf x y w =
    let caps = [| `BUTT; `ROUND; `SQUARE |] in
    let stroke_width = 8.0 in

    C.new_path vg xf;
    C.rect vg x y w 40.0;
    C.rect vg (x-.stroke_width/.2.0) y (w+.stroke_width) 40.0;
    C.fill vg (Paint.color (Color.gray ~a:0.125 1.0));

    for i = 0 to 2 do
      C.new_path vg xf;
      C.move_to vg x (y +. float (i * 10 + 5));
      C.line_to vg (x +. w) (y +. float (i * 10 + 5));
      C.stroke vg Paint.black
        Outline.{default with stroke_width; line_cap = caps.(i)};
    done

  let draw_scissor vg xf x y t =
    let frame = Frame.default in
    let xf = Transform.(rotate (5.0 /. 180.0 *. C.pi) (translate ~x ~y xf)) in

    (* Draw first rect and set scissor to it's area. *)
    C.new_path vg xf;
    C.rect vg (-20.0) (-20.0) (60.0) (40.0);
    C.fill vg (Paint.color (Color.v 1.0 0.0 0.0 1.0));

    (* Draw second rectangle with offset and rotation. *)
    let frame = Frame.set_scissor (-20.0) (-20.0) 60.0 40.0 xf frame in
    let xf = Transform.(rotate t (translate 40.0 0.0 xf)) in

    (* Draw the intended second rectangle without any scissoring. *)
    C.new_path vg xf;
    C.rect vg (-20.0) (-10.0) 60.0 30.0;
    C.fill vg (Paint.color (Color.v 1.0 0.5 0.0 0.25));
    (* Draw second rectangle with combined scissoring. *)
    let frame = Frame.intersect_scissor (-20.0) (-10.0) 60.0 30.0 xf frame in
    C.fill vg ~frame (Paint.color (Color.v 1.0 0.5 0.0 1.0))

  let draw_window vg xf title x y w h =
    let cornerRadius = 3.0 in
    (* Window *)
    C.new_path vg xf;
    C.round_rect vg x y w h cornerRadius;
    C.fill vg (Paint.color (Color.v 0.110 0.118 0.133 0.75));

    (* Drop shadow *)
    C.new_path vg xf;
    C.rect vg (x -. 10.0) (y -. 10.0) (w +. 20.0) (h +. 30.0);
    C.round_rect vg x y w h cornerRadius;
    C.set_winding vg `HOLE;
    C.fill vg (Paint.box_gradient x (y+.2.0) w h (cornerRadius*.2.0) 10.0
                 (Color.gray ~a:0.5 0.0) (Color.gray ~a:0.0 0.0));

    (* Header *)
    C.new_path vg xf;
    C.round_rect vg (x+.1.0) (y+.1.0) (w-.2.0) 30.0 (cornerRadius -. 1.0);
    C.fill vg (Paint.linear_gradient x y x (y+.15.0)
                 (Color.gray ~a:0.04 1.0) (Color.gray ~a:0.08 1.0));
    C.new_path vg xf;
    C.move_to vg (x+.0.5) (y+.0.5+.30.0);
    C.line_to vg (x+.0.5+.w-.1.0) (y+.0.5+.30.0);
    C.stroke vg (Paint.color (Color.gray ~a:0.125 0.0)) Outline.default;

    C.new_path vg xf;

    let font = Lazy.force font_sans_bold in

    C.text vg (Paint.color (Gg.Color.gray ~a:0.5 0.0))
      (Font.make ~blur:0.9 ~size:18.0 font)
      ~valign:`MIDDLE ~halign:`CENTER
      ~x:(x+.w/.2.) ~y:(y+.16.+.1.0) title;

    C.text vg (Paint.color (Gg.Color.gray ~a:0.6 0.9))
      (Font.make ~size:18.0 font)
      ~valign:`MIDDLE ~halign:`CENTER
      ~x:(x+.w/.2.) ~y:(y+.16.) title

  let draw_searchbox vg xf text x y w h =
    let cornerRadius = h /. 2.0 -. 1.0 in
    (* Edit *)
    C.new_path vg xf;
    C.round_rect vg x y w h cornerRadius;
    C.fill vg (Paint.box_gradient x (y +. 1.5) w h (h /. 2.0) 5.0
                 (Color.gray ~a:0.08 0.0) (Color.gray ~a:0.375 0.0));

    C.new_path vg xf;
    C.round_rect vg (x+.0.5) (y+.0.5) (w-.1.0) (h-.1.0) (cornerRadius-.0.5);
    C.stroke vg (Paint.color (Color.gray ~a:0.2 0.0)) Outline.default;

    C.text vg (Paint.color (Gg.Color.gray ~a:0.25 1.0))
      (Font.make ~size:(h*.1.3) (Lazy.force font_icons))
      ~valign:`MIDDLE ~halign:`CENTER
      ~x:(x+.h*.0.55) ~y:(y+.h*.0.55) "🔍";

    C.text vg (Paint.color (Gg.Color.gray ~a:0.125 1.0))
      (Font.make ~size:20.0 (Lazy.force font_sans))
      ~valign:`MIDDLE ~halign:`LEFT
      ~x:(x+.h*.1.05) ~y:(y+.h*.0.5) text;

    C.text vg (Paint.color (Gg.Color.gray ~a:0.125 1.0))
      (Font.make ~size:(h*.1.3) (Lazy.force font_icons))
      ~valign:`MIDDLE ~halign:`CENTER
      ~x:(x+.w-.h*.0.55) ~y:(y+.h*.0.55) "✖"

  let draw_dropdown vg xf text x y w h =
    let cornerRadius = 4.0 in

    C.new_path vg xf;
    C.round_rect vg (x+.1.0) (y+.1.0) (w-.2.0) (h-.2.0) (cornerRadius-.1.0);
    C.fill vg (Paint.linear_gradient x y x (y+.h)
                 (Color.gray ~a:0.08 1.0) (Color.gray ~a:0.08 0.0));

    C.new_path vg xf;
    C.round_rect vg (x+.0.5) (y+.0.5) (w-.1.0) (h-.1.0) (cornerRadius-.0.5);
    C.stroke vg (Paint.color (Color.gray ~a:0.1875 0.0)) Outline.default;

    C.new_path vg xf;
    C.text vg (Paint.color (Gg.Color.gray ~a:0.8 1.0))
      (Font.make ~size:20.0 (Lazy.force font_sans))
      ~valign:`MIDDLE ~halign:`LEFT
      ~x:(x+.h*.0.3) ~y:(y+.h*.0.5) text;

    C.new_path vg xf;
    C.text vg (Paint.color (Gg.Color.gray ~a:0.8 1.0))
      (Font.make ~size:(h*.1.3) (Lazy.force font_icons))
      ~valign:`MIDDLE ~halign:`CENTER
      ~x:(x+.w-.h*.0.5) ~y:(y+.h*.0.5) " "

  let draw_label vg xf text x y w h =
    C.new_path vg xf;
    C.text vg (Paint.color (Gg.Color.gray ~a:0.5 1.0))
      (Font.make ~size:18.0 (Lazy.force font_sans))
      ~valign:`MIDDLE ~halign:`LEFT
      ~x ~y:(y+.h*.0.5) text

  let draw_editboxbase vg xf x y w h =
    C.new_path vg xf;
    C.round_rect vg (x+.1.0) (y+.1.0) (w-.2.0) (h-.2.0) (4.0-.1.0);
    C.fill vg (Paint.box_gradient (x+.1.0) (y+.1.0+.1.5) (w-.2.0) (h-.2.0) 3.0 4.0
                 (Color.gray ~a:0.125 1.0) (Color.gray ~a:0.125 0.125));
    C.new_path vg xf;
    C.round_rect vg (x+.0.5) (y+.0.5) (w-.1.0) (h-.1.0) (4.0-.0.5);
    C.stroke vg (Paint.color (Color.gray ~a:0.1875 0.0)) Outline.default

  let draw_editbox vg xf text x y w h =
    draw_editboxbase vg xf x y w h;

    C.new_path vg xf;
    C.text vg (Paint.color (Gg.Color.gray ~a:0.25 1.0))
      (Font.make ~size:20.0 (Lazy.force font_sans))
      ~valign:`MIDDLE ~halign:`LEFT
      ~x:(x+.h*.0.3) ~y:(y+.h*.0.5) text

  let draw_editboxnum vg xf text units x y w h =
    draw_editboxbase vg xf x y w h;

    let ufont = Font.make ~size:18.0 (Lazy.force font_sans) in
    C.text vg (Paint.color (Gg.Color.gray ~a:0.25 1.0))
      ~valign:`MIDDLE ufont ~halign:`RIGHT
      ~x:(x+.w-.h*.0.3) ~y:(y+.h*.0.5) units;

    let uw = Font.text_width ufont units in
    C.text vg (Paint.color (Gg.Color.gray ~a:0.5 1.0))
      (Font.make ~size:20.0 (Lazy.force font_sans))
      ~valign:`MIDDLE ~halign:`RIGHT
      ~x:(x+.w-.uw-.h*.0.5) ~y:(y+.h*.0.5) text

  let draw_checkbox vg xf text x y w h =
    C.new_path vg xf;

    C.text vg (Paint.color (Gg.Color.gray ~a:0.66 1.0))
      (Font.make ~size:18.0 (Lazy.force font_sans))
      ~valign:`MIDDLE
      ~x:(x+.28.) ~y:(y+.h*.0.5) text;

    C.round_rect vg (x+.1.0) (y+.floor(h/.2.0)-.9.0) 18.0 18.0 3.0;
    C.fill vg (Paint.box_gradient (x+.1.0) (y+.floor(h/.2.0)-.9.0+.1.0)
                 18.0 18.0 3.0 3.0
                 (Color.gray ~a:0.125 0.0) (Color.gray ~a:0.375 0.0));

    C.text vg (Paint.color (Gg.Color.gray ~a:0.5 1.0))
      (Font.make ~size:40.0 (Lazy.force font_icons))
      ~valign:`MIDDLE ~halign:`CENTER
      ~x:(x+.11.) ~y:(y+.h*.0.5) "✓"

  let cp_to_utf8 cp =
    let n =
      if cp < 0x80 then 1
      else if (cp < 0x800) then 2
      else if (cp < 0x10000) then 3
      else if (cp < 0x200000) then 4
      else if (cp < 0x4000000) then 5
      else if (cp <= 0x7fffffff) then 6
      else assert false
    in
    let str= Bytes.create n in
    let cp = ref cp in
    begin try
        if n > 5 then (str.[5] <- Char.chr (0x80 lor (!cp land 0x3f));
                       cp := (!cp lsr 6) lor 0x4000000);
        if n > 4 then (str.[4] <- Char.chr (0x80 lor (!cp land 0x3f));
                       cp := (!cp lsr 6) lor 0x200000);
        if n > 3 then (str.[3] <- Char.chr (0x80 lor (!cp land 0x3f));
                       cp := (!cp lsr 6) lor 0x10000);
        if n > 2 then (str.[2] <- Char.chr (0x80 lor (!cp land 0x3f));
                       cp := (!cp lsr 6) lor 0x800);
        if n > 1 then (str.[1] <- Char.chr (0x80 lor (!cp land 0x3f));
                       cp := (!cp lsr 6) lor 0xc0);
        str.[0] <- Char.chr !cp;
      with exn ->
        prerr_endline ("cp: " ^ string_of_int !cp);
        raise exn
    end;
    Bytes.to_string str


  let draw_button vg xf preicon text x y w h col =
    let is_black = Color.a col > 0.0 in
    let cornerRadius = 4.0 in
    C.new_path vg xf;
    C.round_rect vg (x+.1.0) (y+.1.0) (w-.2.0) (h-.2.0) (cornerRadius-.1.0);
    if is_black then (
      C.fill vg (Paint.color col);
    );
    C.fill vg (Paint.linear_gradient x y x (y+.h)
                 (Color.gray 1.0 ~a:(if is_black then 0.125 else 0.25))
                 (Color.gray 0.0 ~a:(if is_black then 0.125 else 0.25)));
    C.new_path vg xf;
    C.round_rect vg (x+.0.5) (y+.0.5) (w-.1.0) (h-.1.0) (cornerRadius-.0.5);
    C.stroke vg (Paint.color (Color.gray ~a:0.375 0.0)) Outline.default;
    let font = Font.make ~size:20.0 (Lazy.force font_sans_bold) in
    let font_blur = Font.make ~blur:2.0 ~size:20.0 (Lazy.force font_sans_bold) in
    let tw = Font.text_width font text in
    let iw = if preicon = 0 then 0.0 else
        let font = Font.make ~size:(h*.1.3) (Lazy.force font_icons) in
        let icon = cp_to_utf8 preicon in
        let iw = Font.text_width font icon in
        C.text vg (Paint.color (Gg.Color.gray ~a:0.40 1.0)) font
          ~halign:`LEFT ~valign:`MIDDLE
          ~x:(x+.w*.0.5-.tw*.0.5-.iw*.0.75) ~y:(y+.h*.0.5)
          icon;
        iw
    in
    C.text vg (Paint.color (Gg.Color.gray ~a:0.66 0.0)) font_blur
      ~valign:`MIDDLE ~halign:`LEFT
      ~x:(x+.w*.0.5-.tw*.0.5+.iw*.0.25) ~y:(y+.h*.0.5) text;
    C.text vg (Paint.color (Gg.Color.gray ~a:0.66 1.0)) font
      ~valign:`MIDDLE ~halign:`LEFT
      ~x:(x+.w*.0.5-.tw*.0.5+.iw*.0.25) ~y:(y+.h*.0.5) text

  let draw_slider vg xf pos x y w h =
    let cy = y +. floor (h*.0.5) in
    let kr = floor (h*.0.25) in

    (* Slot *)
    C.new_path vg xf;
    C.round_rect vg x (cy-.2.) w 4.0 2.0;
    C.fill vg (Paint.box_gradient x (cy-.2.0+.1.0) w 4.0 2.0 2.0
                 (Color.gray ~a:0.125 0.0) (Color.gray ~a:0.5 0.0));

    (* Knob Shadow *)
    C.new_path vg xf;
    C.rect vg (x+.floor(pos*.w)-.kr-.5.0) (cy-.kr-.5.0)
      (kr*.2.0+.5.0+.5.0) (kr*.2.0+.5.0+.5.0+.3.0);
    C.circle vg (x+.floor(pos*.w)) cy kr;
    C.set_winding vg `HOLE;
    C.fill vg (Paint.radial_gradient (x+.floor(pos*.w)) (cy+.1.0) (kr-.3.0) (kr+.3.0)
                 (Color.gray ~a:0.25 0.0) (Color.gray ~a:0.0 0.0));

    (* Knob *)
    C.new_path vg xf;
    C.circle vg (x+.floor(pos*.w)) cy (kr-.1.0);
    C.fill vg (Paint.color (Color.v_srgbi 40 43 48));
    C.fill vg (Paint.linear_gradient x (cy-.kr) x (cy+.kr)
                 (Color.gray ~a:0.0625 1.0) (Color.gray ~a:0.0625 0.0));

    C.new_path vg xf;
    C.circle vg (x+.floor(pos*.w)) cy (kr-.0.5);
    C.stroke vg (Paint.color (Color.gray ~a:0.375 0.0)) Outline.default;

    ()

  let image_size image = Wall_tex.width image, Wall_tex.height image
  let image_texture image = image

  let load_demo_data () =
    Array.init 12 (fun i ->
        let name = Printf.sprintf "images/image%d.jpg" (i+1)
        in
        match Wall_tex.load_image ~alpha:false ~name name with
        | Result.Ok image -> image
        | Result.Error (`Msg msg) ->
          Printf.eprintf "error loading %s: %s\n%!" name msg;
          exit 1
      )

  let draw_thumbnails vg xf x y w h images t =
    let cornerRadius = 3.0 and thumb = 60.0 and arry = 30.5 in
    let stackh = float (Array.length images / 2) *. (thumb +. 10.0) +. 10.0 in
    let u = (1.0 +. cos (t*.0.5)) *. 0.5 in
    let u2 = (1.0 -. cos (t*.0.2)) *. 0.5 in

    (* Drop shadow *)
    C.new_path vg xf;
    C.rect vg (x-.10.0) (y-.10.0) (w+.20.0) (h+.30.0);
    C.round_rect vg x y w h cornerRadius;
    C.set_winding vg `HOLE;
    C.fill vg (Paint.box_gradient x (y+.4.0) w h (cornerRadius*.2.0) 20.0
                 (Color.gray ~a:0.5 0.0) (Color.gray ~a:0.0 0.0) );

    (* Window *)
    C.new_path vg xf;
    C.round_rect vg x y w h cornerRadius;
    C.move_to vg (x -. 10.0) (y +. arry);
    C.line_to vg (x +. 1.0) (y +. arry -. 11.0);
    C.line_to vg (x +. 1.0) (y +. arry +. 11.0);
    C.fill vg (Paint.color (Color.gray 0.8));

    let frame = Frame.set_scissor x y w h xf Frame.default in
    let xf' = Transform.translate 0.0 (-. (stackh -. h) *. u) xf in
    let dv = 1.0 /. float (Array.length images - 1) in

    Array.iteri (fun i image ->
        let tx = x +. 10.0 +. float (i mod 2) *. (thumb +. 10.0) in
        let ty = y +. 10.0 +. float (i / 2) *. (thumb +. 10.0) in

        let imgw, imgh = image_size image in
        let imgw, imgh = float imgw, float imgh in
        let iw, ih, ix, iy =
          if imgw < imgh then
            let iw = thumb in
            let ih = iw *. imgh /. imgw in
            (iw, ih, 0.0, -.(ih -. thumb) *. 0.5)
          else
            let ih = thumb in
            let iw = ih *. imgw /. imgh in
            (iw, ih, -.(iw -. thumb) *. 0.5, 0.0)
        in
        let v = float i *. dv in
        let a = max 0.0 (min 1.0 ((u2 -. v) /. dv)) in

        if a < 1.0 then
          draw_spinner ~frame vg xf'
            (tx +. thumb /. 2.0) (ty +. thumb /. 2.0) (thumb*.0.25) t;

        C.new_path vg xf';
        C.round_rect vg tx ty thumb thumb 5.0;
        C.fill ~frame vg (Paint.image_pattern
                            (Gg.P2.v (tx+.ix) (ty+.iy)) (Gg.Size2.v iw ih)
                            0.0 a (image_texture image));

        C.new_path vg xf';
        C.rect vg (tx-.5.0) (ty-.5.0) (thumb+.10.0) (thumb+.10.0);
        C.round_rect vg tx ty thumb thumb 6.0;
        C.set_winding vg `HOLE;
        C.fill ~frame vg (Paint.box_gradient (tx-.1.0) ty (thumb+.2.0) (thumb+.2.0) 5.0 3.0
                            (Color.gray ~a:0.5 0.0) (Color.gray ~a:0.0 0.0));

        C.new_path vg xf';
        C.round_rect vg (tx+.0.5) (ty+.0.5) (thumb-.1.0) (thumb-.1.0) (4.0-.0.5);
        C.stroke ~frame vg (Paint.color (Color.gray ~a:0.75 1.0))
          Outline.{default with stroke_width = 1.0};
      ) images;

    (* Hide fades *)
    C.new_path vg xf;
    C.rect vg (x+.4.0) y (w-.8.0) 6.0;
    C.fill vg (Paint.linear_gradient x y x (y+.6.0)
                 (Color.gray ~a:1.0 0.8) (Color.gray ~a:0.0 0.8));

    C.new_path vg xf;
    C.rect vg (x+.4.0) (y+.h-.6.0) (w-.8.0) 6.0;
    C.fill vg (Paint.linear_gradient x (y+.h-.6.0) x (y+.6.0)
                 (Color.gray ~a:1.0 0.8) (Color.gray ~a:0.0 0.8));

    (* Scroll bar *)
    C.new_path vg xf;
    C.round_rect vg (x+.w-.12.0) (y+.4.0) 8.0 (h-.8.0) 3.0;
    C.fill vg (Paint.box_gradient (x+.w-.12.0+.1.0) (y+.4.0+.1.0) 8.0 (h-.8.0)
                 3.0 4.0 (Color.gray ~a:0.125 0.0) (Color.gray ~a:0.375 0.0));

    let scrollh = (h/.stackh) *. (h-.8.0) in
    C.new_path vg xf;
    C.round_rect vg (x+.w-.12.+.1.) (y+.4.+.1. +. (h-.8.-.scrollh)*.u)
      (8.-.2.) (scrollh-.2.) 2.;
    C.fill vg (Paint.box_gradient (x+.w-.12.-.1.) (y+.4.+.(h-.8.-.scrollh)*.u-.1.)
                 8. scrollh 3. 4.
                 (Color.gray ~a:0.9 1.0) (Color.gray ~a:0.5 1.0))

  let images = lazy (load_demo_data ())

  let draw vg xf mx my w h t = (
    draw_eyes vg xf (w -. 250.0) 50.0 150.0 100.0 mx my t;
    draw_graph vg xf 0.0 (h /. 2.0) w (h /. 2.0) t;
    draw_colorwheel vg xf (w -. 300.0) (h -. 300.0) 250.0 250.0 t;
    draw_lines vg xf 120.0 (h -. 50.0) 600.0 50.0 t;
    draw_widths vg xf 10.0 50.0 30.0;
    draw_caps vg xf 10.0 300.0 30.0;
    draw_scissor vg xf 50.0 (h-.80.0) t;

    (* Widgets *)
    draw_window vg xf "Widgets `n Stuff" 50.0 50.0 300.0 400.0;
    let x = 60.0 and y = 95.0 in
    draw_searchbox vg xf "Search" x y 280.0 25.0;
    let y = y +. 40.0 in
    draw_dropdown vg xf "Effects" x y 280.0 28.0;
    let popy = y +. 14.0 in
    let y = y +. 45.0 in

    (* Form *)
    draw_label vg xf "login" x y 280.0 20.0;
    let y = y +. 25.0 in
    draw_editbox vg xf "Email" x y 280.0 28.0;
    let y = y +. 35.0 in
    draw_editbox vg xf "Password" x y 280.0 28.0;
    let y = y +. 38.0 in
    draw_checkbox vg xf "Remember me" x y 140.0 28.0;
    draw_button vg xf (*ICON_LOGIN*)0xE740 "Sign in" (x+.138.0) y 140.0 28.0
      (Color.v 0.0 0.375 0.5 1.0);
    let y = y +. 45.0 in

    (* Slider *)
    draw_label vg xf "Diameter" x y 280.0 20.0;
    let y = y +. 25.0 in
    draw_editboxnum vg xf "123.00" "px" (x+.180.0) y 100.0 28.0;
    draw_slider vg xf 0.4 x y 170.0 28.0;
    let y = y +. 55.0 in

    draw_button vg xf (*ICON_TRASH*)0xE729 "Delete" x y 160.0 28.0 (Color.v 0.5 0.0625 0.03125 1.0);
    draw_button vg xf 0 "Cancel" (x+.170.0) y 110.0 28.0 (Color.gray ~a:0.0 0.0);

    draw_thumbnails vg xf 365.0 (popy-.30.0) 160.0 300.0 (Lazy.force images) t;
    ()
  )
end

module Blender = struct

  (* describes the theme used to draw widgets *)
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

    open Color
    module Colors = struct
      let c_0_098 = gray 0.098
      let c_0_275 = gray 0.275
      let c_0_353 = gray 0.353
      let c_0_392 = gray 0.392
      let c_0_447 = gray 0.447
      let c_0_502 = gray 0.502
      let c_0_600 = gray 0.600
      let c_0_706 = gray 0.706
      let c_0_800 = gray 0.800
      let c_text = black
      let c_text_selected = white
    end
    open Colors

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

    let background = c_0_447

    let regular = {
      outline        = c_0_098;
      item           = c_0_098;
      inner          = c_0_600;
      inner_selected = c_0_392;
      text           = c_text;
      text_selected  = c_text_selected;
      shade_top      = 0.0;
      shade_down     = 0.0;
    }

    let tool_button = {
      outline        = c_0_098;
      item           = c_0_098;
      inner          = c_0_600;
      inner_selected = c_0_392;
      text           = c_text;
      text_selected  = c_text_selected;
      shade_top      = 0.15;
      shade_down     = -0.15;
    }

    let radio_button = {
      outline        = black;
      item           = white;
      inner          = c_0_275;
      inner_selected = v 0.337 0.502 0.761 1.0;
      text           = c_text;
      text_selected  = c_text_selected;
      shade_top      = 0.15;
      shade_down     = -0.15;
    }

    let text_field = {
      outline        = c_0_098;
      item           = c_0_353;
      inner          = c_0_600;
      inner_selected = c_0_600;
      text           = c_text;
      text_selected  = c_text_selected;
      shade_top      = 0.0;
      shade_down     = 0.25;
    }

    let option = {
      outline        = black;
      item           = white;
      inner          = c_0_275;
      inner_selected = c_0_275;
      text           = c_text;
      text_selected  = c_text_selected;
      shade_top      = 0.15;
      shade_down     = -0.15;
    }

    let choice = {
      outline        = black;
      item           = white;
      inner          = c_0_275;
      inner_selected = c_0_275;
      text           = c_text_selected;
      text_selected  = c_0_800; (*  color_text_selected *)
      shade_top      = 0.15;
      shade_down     = -0.15;
    }

    let number_field = {
      outline        = c_0_098;
      item           = c_0_353;
      inner          = c_0_706;
      inner_selected = c_0_600;
      text           = c_text;
      text_selected  = c_text_selected;
      shade_top      = -0.20;
      shade_down     = 0.0;
    }

    let slider = {
      outline        = c_0_098;
      item           = c_0_502;
      inner          = c_0_706;
      inner_selected = c_0_600;
      text           = c_text;
      text_selected  = c_text_selected;
      shade_top      = -0.20;
      shade_down     = 0.0;
    }

    let scrollbar = {
      outline        = gray 0.196;
      item           = c_0_502;
      inner          = gray ~a:0.706 0.314;
      inner_selected = gray ~a:0.706 0.392;
      text           = c_text;
      text_selected  = c_text_selected;
      shade_top      = 0.5;
      shade_down     = -0.5;
    }

    let tooltip = tooltip_and_menu

    let menu = tooltip_and_menu

    let menu_item = {
      outline        = black;
      item           = gray ~a:0.502 0.675;
      inner          = gray ~a:0.0 0.0;
      inner_selected = v 0.337 0.502 0.761 1.0;
      text           = c_text_selected;
      text_selected  = c_text;
      shade_top      = 0.38;
      shade_down     = 0.0;
    }

    let node = {
      node_selected      = v 0.945 0.345 0.0 1.0;
      wire               = black;
      node_text_selected = v 0.498 0.439 0.439 1.0;
      node_active        = v 1.0 0.667 0.251 1.0;
      wire_selected      = white;
      node_backdrop      = gray ~a:0.627 0.608;
      noodle_curving     = 0.5;
    }
  end

  module B2 = Gg.Box2
  module P2 = Gg.P2

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

  let minf a b : float = if a < b then a else b
  let maxf a b : float = if a > b then a else b

  let clampf x a b = maxf (minf x b) a

  (* states altering the styling of a widget *)
  type widget_state =
    [ (* not interacting *)
      `DEFAULT
    | (* the mouse is hovering over the control *)
      `HOVER
    | (* the widget is activated (pressed) or in an active state (toggled) *)
      `ACTIVE
    ]

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

  let draw_bevel_inset t xf box (_, _, cr2, cr3) =
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
      offset_color Theme.background Default.inset_bevel_shade
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
    let color = offset_color Theme.background (-.Default.bevel_shade) in
    C.stroke t (Paint.color (transparent color)) Outline.default;
    C.new_path t xf;
    C.move_to t ~x:x1 ~y:y2;
    C.line_to t ~x:x1 ~y:y1;
    C.line_to t ~x:x2 ~y:y1;
    let color = offset_color Theme.background Default.bevel_shade in
    C.stroke t (Paint.color (transparent color)) Outline.default

  let inner_colors {Theme. inner; inner_selected; shade_top; shade_down} ?(flip=false) (state : widget_state) =
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

  let draw_rounded_box t box ~corners:(cr0, cr1, cr2, cr3) =
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
    let p1 = B2.o box in
    B2.v
      (P2.v (P2.x p1 +. x1) (P2.y p1 +. y1))
      (Gg.Size2.v (B2.w box +. x2 -. x1) (B2.h box +. y2 -. y1))

  let draw_inner_box t xf ?frame box (cr0, cr1, cr2, cr3) inner outer =
    let x1 = B2.minx box and y1 = B2.miny box in
    let x2 = B2.maxx box and y2 = B2.maxy box in
    C.new_path t xf;
    draw_rounded_box t (offset_box box 1.0 1.0 (-2.0) (-3.0))
      ~corners:(max 0.0 cr0, max 0.0 cr1, max 0.0 cr2, max 0.0 cr3);
    C.fill t ?frame
      (if B2.h box -. 2.0 > B2.w box
       then Paint.linear_gradient ~sx:x1 ~sy:y1 ~ex:x2 ~ey:y1 ~inner ~outer
       else Paint.linear_gradient ~sx:x1 ~sy:y1 ~ex:x1 ~ey:y2 ~inner ~outer)

  let draw_outline_box t xf box corners color =
    C.new_path t xf;
    draw_rounded_box t (offset_box box 0.5 0.5 (-1.0) (-2.0)) ~corners;
    C.stroke t (Paint.color color)
      {Outline.default with Outline.stroke_width = 1.0}

  let draw_check t xf ~x ~y color =
    C.new_path t xf;
    C.move_to t (x+.4.0) (y+.5.0);
    C.line_to t (x+.7.0) (y+.8.0);
    C.line_to t (x+.14.0) (y+.1.0);
    C.stroke t (Paint.color color)
      Outline.({default with
                line_cap = `BUTT; line_join = `MITER;
                stroke_width = 2.0})

  let draw_up_down_arrow t xf ~x ~y ~size color =
    let w = 1.1 *. size in
    C.new_path t xf;
    C.move_to t x (y-.1.0);
    C.line_to t (x+.0.5*.w) (y-.size-.1.0);
    C.line_to t (x+.w) (y-.1.0);
    C.move_to t x (y+.1.);
    C.line_to t (x+.0.5*.w) (y+.size+.1.0);
    C.line_to t (x+.w) (y+.1.0);
    C.close_path t;
    C.fill t (Paint.color color)

  let draw_arrow t xf ~x ~y ~size color =
    C.new_path t xf;
    C.move_to t x y;
    C.line_to t (x-.size) (y+.size);
    C.line_to t (x-.size) (y-.size);
    C.close_path t;
    C.fill t (Paint.color color)

  let draw_node_port t xf ~x ~y state color =
    C.new_path t xf;
    C.circle t ~cx:x ~cy:y ~r:Default.node_port_radius;
    C.stroke t
      (Paint.color Theme.node.Theme.wire)
      {Outline.default with Outline.stroke_width = 1.0};
    C.fill t (Paint.color (if state = `DEFAULT then color
                           else offset_color color Default.hover_shade))

  let draw_colored_node_wire t xf x0 y0 c0 x1 y1 c1 =
    let length = maxf (abs_float (x1 -. x0)) (abs_float (y1 -. y0)) in
    let delta = length *. Theme.node.Theme.noodle_curving in
    C.new_path t xf;
    C.move_to t x0 y0;
    C.bezier_to t
      ~c1x:(x0 +. delta) ~c1y:y0 ~c2x:(x1 -. delta) ~c2y:y1 ~x:x1 ~y:y1;
    let colorw =
      Color.with_a Theme.node.Theme.wire
        (minf (Color.a c0) (Color.a c1))
    in
    C.stroke t (Paint.color colorw)
      Outline.({default with stroke_width = Default.node_wire_outline_width});
    C.stroke t (Paint.linear_gradient x0 y0 x1 y1 c0 c1)
      Outline.({default with stroke_width = Default.node_wire_width})

  let gray = Color.gray 0.5
  let node_wire_color =
    fun theme -> function
      | `DEFAULT -> gray
      | `HOVER   -> theme.Theme.wire_selected
      | `ACTIVE  -> theme.Theme.node_active

  let draw_node_wire t xf x0 y0 s0 x1 y1 s1 =
    draw_colored_node_wire t xf
      x0 y0 (node_wire_color Theme.node s0)
      x1 y1 (node_wire_color Theme.node s1)

  let b2_with_h box h =
    let tl = B2.o box and w = B2.w box in
    B2.v tl (Gg.Size2.v w h)

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

  let draw_tooltip_background t xf box =
    let shade_top, shade_down = inner_colors Theme.tooltip `DEFAULT in
    let corners = Default.(menu_radius,menu_radius,menu_radius,menu_radius) in
    let box' = offset_box box 0.0 0.0 0.0 1.0 in
    draw_inner_box t xf box' corners shade_top shade_down;
    draw_outline_box t xf box' corners Theme.(transparent tooltip.outline);
    draw_drop_shadow t xf box ~r:Default.menu_radius
      ~feather:Default.shadow_feather ~alpha:Default.shadow_alpha

  type icon = {
    tex: Wall_tex.t;
    x: int;
    y: int;
    w: int;
    h: int;
  }

  let draw_icon t xf x y icon =
    C.new_path t xf;
    C.rect t ~x ~y ~w:(float icon.w) ~h:(float icon.h);
    C.fill t (Paint.image_pattern
                (P2.v (float icon.x) (float icon.y))
                (Gg.Size2.v
                   (float (Wall_tex.width icon.tex))
                   (float (Wall_tex.height icon.tex)))
                ~angle:0.0 ~alpha:1.0
                icon.tex)

  let draw_node_icon_label t xf box ?icon c0 c1 ~align ~font label =
    begin match font, label with
      | Some font, Some label ->
        let font' = {font with Font.blur = Default.node_title_feather} in
        C.new_path t xf;
        C.text t (Paint.color c1) font' label
          ~halign:`LEFT ~valign:`BASELINE
          ~x:(B2.minx box +. 1.0) ~y:(B2.maxy box +. 3.0 -. Default.text_pad_down);
        C.text t (Paint.color c0) font label
          ~halign:`LEFT ~valign:`BASELINE
          ~x:(B2.minx box +. 0.0) ~y:(B2.maxy box +. 2.0 -. Default.text_pad_down)
      | _ -> ()
    end;
    begin match icon with
      | None -> ()
      | Some icon ->
        draw_icon t xf (B2.maxx box -. float icon.w) (B2.miny box +. 3.0) icon
    end

  let draw_node_background t xf box state ?icon ?font label color =
    draw_inner_box t xf
      (b2_with_h box (Default.node_title_height +. 2.0))
      Default.(node_radius,node_radius,0.0,0.0)
      (transparent (offset_color color Default.bevel_shade))
      (transparent color);
    draw_inner_box t xf
      (offset_box box
         0.0 (Default.node_title_height -. 1.0)
         0.0 (2.0 -. Default.node_title_height))
      Default.(0.0,0.0,node_radius,node_radius)
      (transparent Theme.node.Theme.node_backdrop)
      (transparent Theme.node.Theme.node_backdrop);
    draw_node_icon_label t xf
      (offset_box (b2_with_h box Default.node_title_height)
         Default.node_arrow_area_width 0.0
         Default.(-. node_arrow_area_width -. node_margin_side) 0.0)
      Theme.regular.Theme.text
      (offset_color color Default.bevel_shade)
      ?icon ~align:`LEFT ~font
      label;
    let border_color, arrow_color = match state with
      | `DEFAULT -> Color.black, offset_color color (-. Default.bevel_shade)
      | `HOVER   -> Theme.(node.node_selected, node.node_selected)
      | `ACTIVE  -> Theme.(node.node_active, node.node_selected)
    in
    draw_outline_box t xf (offset_box box 0.0 0.0 0.0 1.0)
      Default.(node_radius,node_radius,node_radius,node_radius)
      (transparent border_color);
    draw_drop_shadow t xf box ~r:Default.node_radius
      ~feather:Default.shadow_feather ~alpha:Default.shadow_alpha

  let draw_splitter_widgets t xf box =
    let inset = transparent Theme.background in
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
    C.move_to t (x1        ) (y2 -. 5.0 ); C.line_to t (x1 +. 5.0 ) (y2        );

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

    C.stroke t (Paint.color inset) Outline.default

  let draw_join_area_overlay t xf box ~vertical ~mirror =
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

  let b2 x y w h = B2.v (P2.v x y) (Gg.Size2.v w h)

  let draw_icon_label_value t xf box ?font ?icon ?(halign=`LEFT) ?valign ?label ?value color =
    let x = B2.minx box and y = B2.miny box in
    match font, label with
    | Some font, Some label ->
      let x = x +. Default.pad_left in
      let x = match icon with
        | None -> x
        | Some icon ->
          draw_icon t xf (x +. 4.0) (y +. 2.0) icon;
          (x +. float icon.w)
      in
      let paint = Paint.color color in
      C.new_path t xf;
      begin match value with
        | Some value ->
          (* let lw = Font.text_width font label in
             let sw = Font.text_width font Default.label_separator in *)
          C.text ~halign ?valign t paint font
            ~x ~y:(y +. Default.widget_height -. Default.text_pad_down)
            (label ^ Default.label_separator ^ value)
        | None ->
          C.text ~halign ?valign t paint font
            ~x ~y:(y +. Default.widget_height -. Default.text_pad_down)
            label
      end
    | _, _ -> begin match icon with
        | None -> ()
        | Some icon -> draw_icon t xf (x +. 2.0) (y +. 2.0) icon
      end

  let text_color theme = function
    | `ACTIVE -> theme.Theme.text_selected
    | _ -> theme.Theme.text

  let draw_tool_button t xf box ~corners state ~font ?icon text =
    let corners = select_corners corners Default.text_radius in
    draw_bevel_inset t xf box corners;
    let (shade_top, shade_down) = inner_colors Theme.radio_button state ~flip:true in
    draw_inner_box t xf box corners shade_top shade_down;
    draw_outline_box t xf box corners Theme.radio_button.Theme.outline;
    draw_icon_label_value t xf box ?icon ~halign:`CENTER ~font ~label:text
      (text_color Theme.tool_button state)

  let draw_radio_button t xf box ~font ~corners state ?icon ?label () =
    let corners = select_corners corners Default.option_radius in
    draw_bevel_inset t xf box corners;
    let shade_top, shade_down = inner_colors Theme.tool_button state ~flip:true in
    draw_inner_box t xf box corners shade_top shade_down;
    draw_outline_box t xf box corners Theme.(transparent tool_button.outline);
    draw_icon_label_value t xf box ?icon ?label
      Theme.regular.Theme.text ~halign:`CENTER ~font

  let draw_label t box ?icon label =
    draw_icon_label_value t box ?icon ~label

  let draw_background t xf box =
    C.new_path t xf;
    C.rect t (B2.minx box) (B2.miny box) (B2.w box) (B2.h box);
    C.fill t (Paint.color Theme.background)

  let draw_node_arrow_down t xf ~x ~y ~size color =
    C.new_path t xf;
    C.move_to t x y;
    C.line_to t (x +. size *. 0.5) (y -. size);
    C.line_to t (x -. size *. 0.5) (y -. size);
    C.close_path t;
    C.fill t (Paint.color color)

  let draw_menu_item t xf box state ?icon ~font label =
    let state =
      if state = `DEFAULT then state else (
        draw_inner_box t xf box (0.0,0.0,0.0,0.0)
          Theme.(offset_color menu_item.inner_selected menu_item.shade_top)
          Theme.(offset_color menu_item.inner_selected menu_item.shade_down);
        `ACTIVE
      )
    in
    draw_icon_label_value t xf box ?icon ~font ~label (text_color Theme.menu_item state) ~halign:`LEFT

  let draw_menu_background t xf box ~corners =
    let corners = select_corners corners Default.menu_radius in
    let shade_top, shade_down = inner_colors Theme.menu `DEFAULT in
    let box' = offset_box box 0.0 0.0 0.0 1.0 in
    draw_inner_box t xf box' corners shade_top shade_down;
    draw_outline_box t xf box' corners Theme.(transparent menu.outline);
    draw_drop_shadow t xf box
      ~r:Default.menu_radius
      ~feather:Default.shadow_feather
      ~alpha:Default.shadow_alpha

  let draw_menu_label t xf box ~font ?icon label =
    draw_icon_label_value t xf box Theme.menu.Theme.text
      ?icon ~halign:`LEFT ~font ~label

  let draw_scroll_bar t xf box state ~offset ~size =
    let corners =
      Default.(scrollbar_radius,scrollbar_radius,scrollbar_radius,scrollbar_radius) in
    draw_bevel_inset t xf box corners;
    draw_inner_box t xf box corners
      Theme.(offset_color scrollbar.inner (3.0 *. scrollbar.shade_down))
      Theme.(offset_color scrollbar.inner (3.0 *. scrollbar.shade_top));
    draw_outline_box t xf box corners
      Theme.(transparent scrollbar.outline);

    let scroll_handle_rect box ~offset ~size =
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
    in
    let box = scroll_handle_rect box ~offset ~size in
    let item_color = Theme.(scrollbar.item) in
    let item_color = if state = `ACTIVE
      then offset_color item_color Default.scrollbar_active_shade
      else item_color
    in
    draw_inner_box t xf box corners
      (offset_color item_color Theme.(3.0 *. scrollbar.shade_top))
      (offset_color item_color Theme.(3.0 *. scrollbar.shade_down));
    draw_outline_box t xf box corners
      Theme.(transparent scrollbar.outline)

  let draw_text_field t xf box ~corners state ?icon text ~font ~caret =
    let corners = select_corners corners Default.text_radius in
    draw_bevel_inset t xf box corners;
    let shade_top, shade_down = inner_colors Theme.text_field state in
    draw_inner_box t xf box corners shade_top shade_down;
    draw_outline_box t xf box corners
      Theme.(transparent text_field.outline);
    (* FIXME
       let caret = if state <> `ACTIVE then (fst caret, -1) else caret in
       draw_icon_label_caret t xf box ?icon ~font
         Theme.(text_color text_field state)
         text Theme.(text_field.item) ~caret *)
    draw_icon_label_value t xf box ~font
         Theme.(text_color text_field state)
         ~label:text

  let draw_option_button t xf box state label ~font =
    let ox = B2.minx box in
    let oy = B2.maxy box -. Default.option_height -. 3.0 in
    let box' = B2.v (P2.v ox oy)
        Default.(Gg.Size2.v option_width option_height) in
    let corners =
      Default.(option_radius, option_radius, option_radius, option_radius) in
    draw_bevel_inset t xf box' corners;
    let shade_top, shade_down = inner_colors Theme.option state ~flip:true in
    draw_inner_box t xf box' corners shade_top shade_down;
    draw_outline_box t xf box' corners
      Theme.(transparent option.outline);
    if state = `ACTIVE then
      draw_check t xf Theme.(transparent option.item) ~x:ox ~y:oy;
    draw_icon_label_value t xf (offset_box box 12.0 0.0 (-12.0) (-1.0))
      (text_color Theme.option state)
      ~halign:`LEFT ~font ~label

  let draw_choice_button t xf box ~corners ~font state ?icon label =
    let corners = select_corners corners Default.option_radius in
    draw_bevel_inset t xf box corners;
    let shade_top, shade_down = inner_colors Theme.choice state ~flip:true in
    draw_inner_box t xf box corners shade_top shade_down;
    draw_outline_box t xf box corners Theme.(transparent choice.outline);
    draw_icon_label_value t xf box ?icon
      (text_color Theme.choice state)
      ~halign:`LEFT ~font:{font with Wall.Font.size = Default.label_font_size} ~label;
    let x = B2.maxx box -. 10.0 and y = B2.miny box +. 10.0 in
    draw_up_down_arrow t xf ~x ~y ~size:5.0
      Theme.(transparent choice.item)

  let draw_color_button t xf box ~corners color =
    let corners = select_corners corners Default.tool_radius in
    draw_bevel_inset t xf box corners;
    draw_inner_box t xf box corners color color;
    draw_outline_box t xf box corners
      (transparent Theme.tool_button.Theme.outline)

  let draw_number_field t xf box ~corners state ~font label value =
    let corners = select_corners corners Default.number_radius in
    draw_bevel_inset t xf box corners;
    let shade_top, shade_down = inner_colors Theme.choice state ~flip:true in
    draw_inner_box t xf box corners shade_top shade_down;
    draw_outline_box t xf box corners Theme.(transparent number_field.outline);
    draw_icon_label_value t xf box
      (text_color Theme.number_field state)
      ~halign:`CENTER ~font ~label ~value;
    let y = B2.miny box +. 10.0 in
    let x1 = B2.minx box +. 8.0 and x2 = B2.maxx box -. 8.0 in
    draw_arrow t xf ~x:x1 ~y ~size:(-.Default.number_arrow_size)
      Theme.(transparent number_field.item);
    draw_arrow t xf ~x:x2 ~y ~size:Default.number_arrow_size
      Theme.(transparent number_field.item)

  let corners = [`DOWN_LEFT; `DOWN_RIGHT; `TOP_LEFT; `TOP_RIGHT]

  let draw_slider t xf box ~corners state ~progress ~font ~label ~value =
    let corners = select_corners corners Default.number_radius in
    let shade_top, shade_down = inner_colors Theme.slider state in
    draw_inner_box t xf box corners shade_top shade_down;
    let shade_top = Theme.(offset_color slider.item slider.shade_top)
    and shade_down = Theme.(offset_color slider.item slider.shade_down)
    in
    let shade_top, shade_down =
      if state = `ACTIVE
      then (shade_top, shade_down)
      else (shade_down, shade_top)
    in
    (* TODO nvgScissor(ctx,x,y,8+(w-8)*bnd_clamp(progress,0,1),h); *)
    let x = B2.minx box and y = B2.miny box and w = B2.w box and h = B2.h box in
    draw_inner_box t xf box corners shade_top shade_down
      ~frame:(Frame.set_scissor ~x ~y ~w:(8.+.(w-.8.)*.progress) ~h xf Frame.default);
    draw_outline_box t xf box corners Theme.(transparent slider.outline);
    draw_icon_label_value t xf box
      (text_color Theme.slider state)
      ~halign:`CENTER ~font ?label ?value

  let draw vg xf =
    let font = Font.make ~size:Default.label_font_size (Lazy.force font_sans) in
    draw_check vg xf ~x:40. ~y:40. (transparent Theme.(option.item));
    draw_up_down_arrow vg xf ~x:80. ~y:40. ~size:10.0 (transparent Theme.(choice.item));
    draw_arrow vg xf ~x:100. ~y:40.0 ~size:(-10.0) (transparent Theme.(number_field.item));
    draw_arrow vg xf ~x:120. ~y:40.0 ~size:10.0 (transparent Theme.(number_field.item));
    draw_outline_box vg xf (b2 160. 40. 36. 36.) (0.,0.,0.,0.) (transparent Theme.(tool_button.outline));
    let a, b = inner_colors Theme.slider `ACTIVE in
    draw_inner_box vg xf (b2 200. 40. 36. 36.) (0.,0.,0.,0.) a b;
    draw_splitter_widgets vg xf (b2 40. 80. 80. 40.);
    draw_node_background vg xf (b2 160. 80. 160. 40.) `DEFAULT
      ~font (Some "Welcome to the Jungle") Theme.node.Theme.node_backdrop;
    draw_node_background vg xf (b2 160. 140. 160. 40.) `DEFAULT
      ~font (Some "Gun's N Roses - Hello World") Theme.node.Theme.node_backdrop;
    draw_node_arrow_down vg xf ~x:300. ~y:95. Theme.Colors.c_0_447 ~size:8.0;
    draw_node_wire vg xf 200. 120. `DEFAULT 200. 140. `DEFAULT;
    draw_tool_button vg xf (b2 160. 300. 40. 40.) ~corners:[`TOP_LEFT;`TOP_RIGHT] `DEFAULT ~font "HA";
    draw_radio_button vg xf (b2 160. 350. 120. 40.) ~font ~corners:[`TOP_LEFT;`TOP_RIGHT] `DEFAULT ~label:"HAHA" ();
    draw_label vg xf "À poil" ~font (b2 160. 400. 120. 40.) Theme.menu.Theme.text;
    draw_join_area_overlay vg xf (b2 280. 400. 120. 40.) ~vertical:false ~mirror:true;
    draw_tooltip_background vg xf (b2 20. 400. 120. 40.);
    draw_menu_background vg xf (b2 20. 450. 120. 40.) ~corners:[];
    draw_menu_item vg xf (b2 20. 450. 120. 40.) `DEFAULT ~font "Foo bar baz";
    draw_menu_label vg xf (b2 20. 480. 120. 40.) ~font "Bla";
    draw_scroll_bar vg xf (b2 0. 0. 200. 60.) `DEFAULT ~offset:0.0 ~size:2.0;
    draw_text_field vg xf (b2 400. 20. 100. 40.) ~corners `DEFAULT "Content" ~font ~caret:();
    draw_option_button vg xf (b2 400. 70. 100. 20.) `DEFAULT "Option" ~font;
    draw_choice_button vg xf (b2 400. 100. 100. 20.) `DEFAULT "Option" ~corners ~font;
    draw_color_button vg xf (b2 400. 130. 20. 20.) ~corners Color.red;
    draw_number_field vg xf (b2 400. 160. 100. 20.) `DEFAULT ~corners ~font "Height" "100.0";
    draw_slider vg xf (b2 400. 190. 100. 20.) ~corners `DEFAULT ~progress:0.5 ~font ~label:(Some "one") ~value:(Some "body")


end

let w = 1000
let h = 600
let f = (try float_of_string Sys.argv.(1) with _ -> 1.0)
let fw = int_of_float (f *. float w)
let fh = int_of_float (f *. float h)

let lw = float w
let lh = float h
let pw = lw *. f
let ph = lh *. f

let render vg t =
  C.new_frame vg;
  let _, (x, y) = Sdl.get_mouse_state () in
  if false then
    let x = float x /. f and y = float y /. f in
    Demo.draw vg (Transform.scale f f) x y lw lh t;
  else
    Blender.draw vg (Transform.scale f f);
  C.flush_frame vg (Gg.V2.v pw ph)

open Tgles2

let main () =
  Printexc.record_backtrace true;
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok () ->
    match Sdl.create_window ~w:fw ~h:fh "SDL OpenGL" Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok w ->
      (*Sdl.gl_set_attribute Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core;*)
      (*Sdl.gl_set_attribute Sdl.Gl.context_major_version 2;*)
      (*Sdl.gl_set_attribute Sdl.Gl.context_minor_version 1;*)
      ignore (Sdl.gl_set_attribute Sdl.Gl.stencil_size 1);
      match Sdl.gl_create_context w with
      | Error (`Msg e) -> Sdl.log "Create context error: %s" e; exit 1
      | Ok ctx ->
        let vg = C.create_gl ~antialias:false in
        let t = ref 0.0 in
        let quit = ref false in
        let event = Sdl.Event.create () in
        while not !quit do
          while Sdl.poll_event (Some event) do
            match Sdl.Event.enum (Sdl.Event.get event Sdl.Event.typ) with
            | `Quit -> quit := true
            | _ -> ()
          done;
          Unix.sleepf 0.020;
          t := !t +. 0.050;
          Gl.viewport 0 0 fw fh;
          let c = Blender.Theme.background in
          Gg.Color.(Gl.clear_color (r c) (g c) (b c) (a c));
          (*Gl.clear_color  0.3 0.3 0.32 1.0;*)
          Gl.(clear (color_buffer_bit lor depth_buffer_bit lor stencil_buffer_bit));
          Gl.enable Gl.blend;
          Gl.blend_func_separate Gl.one Gl.src_alpha Gl.one Gl.one_minus_src_alpha;
          Gl.enable Gl.cull_face_enum;
          Gl.disable Gl.depth_test;
          render vg !t;
          Sdl.gl_swap_window w;
        done;
        Sdl.gl_delete_context ctx;
        Sdl.destroy_window w;
        Sdl.quit ();
        exit 0

let () = main ()
