## Enso Signatures 1.0
## module Standard.Base.Data.Text.Line_Ending_Style
- type Custom_Line_Ending
    - Value line_ending:Standard.Base.Data.Text.Text
- type Line_Ending_Style
    - Mac_Legacy
    - Unix
    - Windows
    - to_text self -> Standard.Base.Any.Any
- type No_Line_Ending
