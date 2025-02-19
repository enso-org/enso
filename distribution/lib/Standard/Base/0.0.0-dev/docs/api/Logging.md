## Enso Signatures 1.0
## module Standard.Base.Logging
- type Log_Level
    - Fine
    - Finest
    - Info
    - Severe
    - Warning
- Standard.Base.Any.Any.log_message self ~message:Standard.Base.Data.Text.Text level:Standard.Base.Logging.Log_Level= -> Standard.Base.Any.Any
