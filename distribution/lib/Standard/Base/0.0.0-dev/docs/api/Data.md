## Enso Signatures 1.0
## module Standard.Base.Data
- type Raw_Response
    - get_dropdown_options -> Standard.Base.Any.Any
- download uri:(Standard.Base.Network.URI.URI|Standard.Base.Data.Text.Text)= file:Standard.Base.System.File.Generic.Writable_File.Writable_File replace_existing:Standard.Base.Data.Download.Download_Mode.Download_Mode= method:Standard.Base.Network.HTTP.HTTP_Method.HTTP_Method= headers:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any)= -> Standard.Base.Any.Any
- fetch uri:(Standard.Base.Network.URI.URI|Standard.Base.Data.Text.Text)= method:Standard.Base.Network.HTTP.HTTP_Method.HTTP_Method= headers:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any)= format:Standard.Base.Any.Any= cache_policy:Standard.Base.Network.HTTP.Cache_Policy.Cache_Policy= -> Standard.Base.Any.Any
- list directory:Standard.Base.Any.Any= name_filter:Standard.Base.Data.Text.Text= recursive:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Any.Any
- post uri:(Standard.Base.Network.URI.URI|Standard.Base.Data.Text.Text)= body:Standard.Base.Network.HTTP.Request_Body.Request_Body= method:Standard.Base.Network.HTTP.HTTP_Method.HTTP_Method= headers:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any)= response_format:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- read path:Standard.Base.Any.Any= format:Standard.Base.Any.Any= on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any
- read_many paths:Standard.Base.Data.Read.Many_Files_List.Many_Files_List= format:Standard.Base.Any.Any= return:Standard.Base.Any.Any= on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any
- read_text path:Standard.Base.Any.Any= encoding:Standard.Base.Data.Text.Encoding.Encoding= on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any
