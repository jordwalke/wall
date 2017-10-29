open Wall

let draw_slider t xf box ~corners state ~progress ~label ~value =
  let corners = select_corners corners Default.number_radius in
  let shade_top, shade_down = inner_colors theme.slider state in
  draw_inner_box t xf box corners shade_top shade_down;

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
  draw_inner_box t xf box corners shade_top shade_down;
  (* TODO nvgResetScissor(ctx); *)
  draw_outline_box t xf box corners (transparent theme.slider.outline);
  draw_icon_label_value t xf box
    (text_color theme.slider state)
    ~align:`CENTER
    ~fontsize:Default.label_font_size
    ?label ?value
