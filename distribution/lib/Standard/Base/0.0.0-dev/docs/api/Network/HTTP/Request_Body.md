## Enso Signatures 1.0
## module Standard.Base.Network.HTTP.Request_Body
- type Request_Body
    - Binary file:Standard.Base.System.File.File=
    - Byte_Array bytes:Standard.Base.Any.Any
    - Empty
    - Form_Data form_data:Standard.Base.Data.Dictionary.Dictionary= url_encoded:Standard.Base.Data.Boolean.Boolean=
    - Json x:Standard.Base.Any.Any=
    - Text text:Standard.Base.Data.Text.Text= encoding:(Standard.Base.Data.Text.Encoding.Encoding|Standard.Base.Nothing.Nothing)= content_type:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)=
    - default_content_type_header self -> Standard.Base.Any.Any
- dictionary_widget -> Standard.Base.Metadata.Widget
- make_all_with_json -> Standard.Base.Any.Any
- Standard.Base.Network.HTTP.Request_Body.Request_Body.from that:Standard.Base.Data.Text.Text -> Standard.Base.Network.HTTP.Request_Body.Request_Body
- Standard.Base.Network.HTTP.Request_Body.Request_Body.from that:Standard.Base.System.File.File -> Standard.Base.Network.HTTP.Request_Body.Request_Body
- Standard.Base.Network.HTTP.Request_Body.Request_Body.from that:Standard.Base.Any.Any -> Standard.Base.Network.HTTP.Request_Body.Request_Body
