## Enso Signatures 1.0
## module Standard.Base.Network.HTTP.Request
- type Request
    - Value method:Standard.Base.Network.HTTP.HTTP_Method.HTTP_Method uri:Standard.Base.Network.URI.URI headers:Standard.Base.Data.Vector.Vector body:Standard.Base.Network.HTTP.Request_Body.Request_Body
    - delete url:Standard.Base.Any.Any headers:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - get url:Standard.Base.Any.Any headers:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - head url:Standard.Base.Any.Any headers:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - new method:Standard.Base.Network.HTTP.HTTP_Method.HTTP_Method url:Standard.Base.Network.URI.URI headers:(Standard.Base.Data.Vector.Vector Standard.Base.Network.HTTP.Header.Header)= body:Standard.Base.Network.HTTP.Request_Body.Request_Body= -> Standard.Base.Any.Any
    - options url:Standard.Base.Any.Any headers:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - post url:Standard.Base.Any.Any body:Standard.Base.Any.Any headers:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - put url:Standard.Base.Any.Any body:Standard.Base.Any.Any headers:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - with_body self new_body:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - with_form self parts:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - with_header self key:Standard.Base.Any.Any val:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - with_headers self new_headers:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - with_json self json_body:Standard.Base.Any.Any -> Standard.Base.Any.Any
