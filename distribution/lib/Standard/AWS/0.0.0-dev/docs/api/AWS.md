## Enso Signatures 1.0
## module Standard.AWS.AWS
- type AWS
    - hash_bytes bytes:Standard.Base.Data.Vector.Vector -> Standard.Base.Any.Any
    - resolve_region_and_service uri:Standard.Base.Network.URI.URI= -> Standard.Base.Any.Any
    - signed_fetch uri:Standard.Base.Network.URI.URI= method:Standard.Base.Network.HTTP.HTTP_Method.HTTP_Method= headers:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any)= format:Standard.Base.Any.Any= credentials:Standard.AWS.AWS_Credential.AWS_Credential= region_service:Standard.AWS.AWS.AWS_Region_Service= -> Standard.Base.Any.Any
    - signed_post uri:Standard.Base.Network.URI.URI= body:Standard.Base.Network.HTTP.Request_Body.Request_Body= method:Standard.Base.Network.HTTP.HTTP_Method.HTTP_Method= headers:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any)= response_format:Standard.Base.Any.Any= credentials:Standard.AWS.AWS_Credential.AWS_Credential= region_service:Standard.AWS.AWS.AWS_Region_Service= -> Standard.Base.Any.Any
- type AWS_Region_Service
    - Region_Service region:Standard.Base.Data.Text.Text service:Standard.Base.Data.Text.Text
