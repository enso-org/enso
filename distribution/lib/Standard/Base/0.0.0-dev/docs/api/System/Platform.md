## Enso Signatures 1.0
## module Standard.Base.System.Platform
- type OS
    - Linux
    - Mac_OS
    - Unknown
    - Windows
- from_text os:Standard.Base.Data.Text.Text -> Standard.Base.System.Platform.OS
- is_unix -> Standard.Base.Data.Boolean.Boolean
- os -> Standard.Base.System.Platform.OS
