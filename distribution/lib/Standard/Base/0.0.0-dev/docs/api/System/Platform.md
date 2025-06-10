## Enso Signatures 1.0
## module Standard.Base.System.Platform
- type OS
    - Linux
    - Mac_OS
    - Unknown
    - Windows
- from_text os:Standard.Base.Any.Any -> Standard.Base.Any.Any
- is_unix -> Standard.Base.Any.Any
- os -> Standard.Base.Any.Any
