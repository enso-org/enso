## Enso Signatures 1.0
## module Standard.Table.Set_Mode
- type Set_Mode
    - Add
    - Add_Or_Update
    - Update
- type Set_Mode_Many
    - Add as:Standard.Base.Data.Text.Text=
    - Add_Or_Update as:Standard.Base.Data.Text.Text=
    - Update
    - as_value self -> Standard.Base.Any.Any
- Standard.Table.Set_Mode.Set_Mode.from that:Standard.Table.Set_Mode.Set_Mode_Many -> Standard.Table.Set_Mode.Set_Mode
- Standard.Table.Set_Mode.Set_Mode_Many.from that:Standard.Table.Set_Mode.Set_Mode -> Standard.Table.Set_Mode.Set_Mode_Many
