## Enso Signatures 1.0
## module Standard.Microsoft.Microsoft365
- type Microsoft365
    - base_url self -> Standard.Base.Any.Any
    - fetch self url:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - fetch_json self url:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - fetch_text self url:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - initialize credentials:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - onedrive_root self -> Standard.Base.Any.Any
    - to_js_object self -> Standard.Base.Any.Any
    - user self -> Standard.Base.Any.Any
