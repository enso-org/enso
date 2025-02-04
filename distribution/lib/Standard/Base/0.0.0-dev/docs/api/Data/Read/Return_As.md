## Enso Signatures 1.0
## module Standard.Base.Data.Read.Return_As
- type Return_As
    - Instance underlying:Standard.Base.Any.Any
    - default_widget display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
    - make_return self input:Standard.Base.Data.Read.Many_Files_List.Many_Files_List objects:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any) on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Data.Text.Text
    - to_text self -> Standard.Base.Data.Text.Text
- Standard.Base.Data.Read.Return_As.Return_As_Base.get_dropdown_options -> Standard.Base.Any.Any
- Standard.Base.Data.Read.Return_As.Return_As_Base.make_return self input:Standard.Base.Data.Read.Many_Files_List.Many_Files_List objects:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any) on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
- Standard.Base.Data.Read.Return_As.Return_As_Base.resolve value:Standard.Base.Any.Any -> Standard.Base.Any.Any
- type Return_As_Base
    - As_Vector
    - get_dropdown_options -> Standard.Base.Any.Any
    - make_return self input:Standard.Base.Data.Read.Many_Files_List.Many_Files_List objects:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any) on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
    - resolve value:Standard.Base.Any.Any -> Standard.Base.Any.Any
- type Return_As_Table_Mock
- replace_with_nothing_and_propagate vector:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any) on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
- Standard.Base.Data.Read.Return_As.Return_As.from that:Standard.Base.Data.Read.Return_As.Return_As_Base -> Standard.Base.Data.Read.Return_As.Return_As
