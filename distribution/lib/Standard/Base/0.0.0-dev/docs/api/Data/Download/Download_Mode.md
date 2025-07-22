## Enso Signatures 1.0
## module Standard.Base.Data.Download.Download_Mode
- type Download_Mode
    - Always
    - If_Not_Exists
    - If_Older_Than age:Standard.Base.Data.Time.Duration.Duration
    - should_download self file:Standard.Base.System.File.Generic.Writable_File.Writable_File -> Standard.Base.Data.Boolean.Boolean
