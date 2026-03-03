## Enso Signatures 1.0
## module Standard.Database.Internal.In_Transaction
- type In_Transaction
    - ensure_in_transaction -> Nothing ! Not_In_Transaction_Error
    - is_in_transaction -> Standard.Base.Any.Any
