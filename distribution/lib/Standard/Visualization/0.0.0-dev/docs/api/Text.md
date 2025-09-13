## Enso Signatures 1.0
## module Standard.Visualization.Text
- type Message
    - Value chunks:Standard.Base.Any.Any line_count:Standard.Base.Any.Any max_line_length:Standard.Base.Any.Any
    - to_json self -> Standard.Base.Any.Any
- get_chunk_from_line text:Standard.Base.Any.Any chunk_size:Standard.Base.Any.Any ix:Standard.Base.Any.Any -> Standard.Base.Any.Any
- get_item_from text:Standard.Base.Any.Any chunk_size:Standard.Base.Any.Any index:Standard.Base.Any.Any -> Standard.Base.Any.Any
- get_lazy_visualization_text_window text:Standard.Base.Any.Any pos:Standard.Base.Any.Any size:Standard.Base.Any.Any chunk_width:Standard.Base.Any.Any -> Standard.Base.Any.Any
- make_grid_visualization_response chunks:Standard.Base.Any.Any lines:Standard.Base.Any.Any max_line_length:Standard.Base.Any.Any -> Standard.Base.Any.Any
