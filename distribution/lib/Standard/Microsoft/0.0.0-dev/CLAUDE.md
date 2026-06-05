# Standard.Microsoft

Microsoft cloud integrations: SQL Server / Azure SQL via `Standard.Database`,
Azure Blob Storage, OneDrive file access, and Microsoft 365 credentials.

## Main entry points

- `SQLServer_Details` — `Database.connect` argument:
  `SQLServer host credentials port=1433 database=…`.
- `SQLServer_Connection` — connection type (returned by `Database.connect`).
- `OneDrive` — OneDrive root entry; `OneDrive.root credentials`.
- `OneDrive_File` — file abstraction over OneDrive paths.
- `Azure_Storage` — Blob Storage operations: `Azure_Storage.blob_containers`.
- `Azure_Credential` — Azure credential variants.
- `Microsoft365_Credential` — M365 credential variant (used by OneDrive).

## Common usage

```
from Standard.Microsoft import SQLServer_Details, OneDrive

conn = Database.connect (SQLServer_Details.SQLServer "localhost" creds port=1433 database="mydb")
table = conn.query "users"
in_memory = table.read

secret = Enso_Secret.get 'Microsoft365_ci_test_credential'
root = OneDrive.root secret
files = root.list

containers = Azure_Storage.blob_containers account="myaccount" cred=Azure_Credential.Default
```

## Layout

- `src/SQLServer_Connection.enso` — connection type.
- `src/Connection/` — `SQLServer_Details`, dialect details.
- `src/OneDrive.enso`, `src/OneDrive_File.enso` — OneDrive integration.
- `src/Azure.enso`, `src/Azure_Storage.enso`, `src/Azure_Credential.enso`,
  `src/Azure_Environment.enso` — Azure services.
- `src/Microsoft365_Credential.enso` — M365 auth.
- `src/SQLServer_Data_Link.enso` — data-link persistence.

## Where to read more

- `src/SQLServer_Connection.enso` — DB connection API.
- `src/OneDrive.enso` — OneDrive file access.
- `src/Azure_Storage.enso` — Blob Storage.
- `test/Microsoft_Tests/src/SQLServer_Spec.enso`, `OneDrive_Spec.enso`,
  `Azure_Storage_Spec.enso` — examples.
