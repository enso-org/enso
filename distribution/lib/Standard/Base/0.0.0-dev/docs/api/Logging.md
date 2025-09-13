## Enso Signatures 1.0
## module Standard.Base.Logging
- type Log_Level
    - Fine
    - Finest
    - Info
    - Severe
    - Warning
- type Progress
    - advance self amount:Standard.Base.Data.Numbers.Integer= -> Standard.Base.Logging.Progress
    - close self -> Standard.Base.Any.Any
    - log self detail:Standard.Base.Data.Text.Text -> Standard.Base.Logging.Progress
    - run label:Standard.Base.Data.Text.Text up_to:Standard.Base.Data.Numbers.Integer action:Standard.Base.Any.Any step:Standard.Base.Data.Numbers.Integer= handle_name:Standard.Base.Data.Text.Text= -> Standard.Base.Any.Any
    - to_text self -> Standard.Base.Any.Any
- Standard.Base.Any.Any.log_message self ~message:Standard.Base.Data.Text.Text level:Standard.Base.Logging.Log_Level= -> Standard.Base.Any.Any
