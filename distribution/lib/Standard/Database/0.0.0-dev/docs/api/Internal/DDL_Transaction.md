## Enso Signatures 1.0
## module Standard.Database.Internal.DDL_Transaction
- type Support_Level
    - Allowed
    - Causes_Commit
    - Ignored
    - Unsupported
    - get_from connection:Standard.Base.Any.Any -> Standard.Database.Internal.DDL_Transaction.Support_Level
- type Transactional_Table_Description
    - Value name:Standard.Base.Data.Text.Text temporary:Standard.Base.Data.Boolean.Boolean structure:(Standard.Base.Data.Vector.Vector Standard.Database.Column_Description.Column_Description) primary_key:(Standard.Base.Any.Any|Standard.Base.Nothing.Nothing) remove_after_transaction:Standard.Base.Data.Boolean.Boolean on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior
- check_table_integrity created_table:Standard.Base.Any.Any -> Standard.Base.Any.Any
- run_transaction_with_tables connection:Standard.Base.Any.Any tables:(Standard.Base.Data.Vector.Vector Standard.Database.Internal.DDL_Transaction.Transactional_Table_Description) callback:Standard.Base.Any.Any -> Standard.Base.Any.Any
