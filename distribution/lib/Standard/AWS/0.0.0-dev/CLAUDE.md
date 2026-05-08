# Standard.AWS

Amazon Web Services integrations: S3 buckets and objects, Redshift data
warehouse connections via `Standard.Database`, AWS credential handling, and
Simple Email Service (SES).

## Main entry points

- `AWS_Credential` — credential variants: `Default`, `Profile name`,
  `Key access_key secret_key`, `With_Configuration cred config`.
- `AWS_Region` — region selection.
- `S3` — top-level S3 operations: `list_buckets`, `list_objects`, `get_object`.
- `S3_File` — file abstraction over `s3://` URIs (works with `Data.read`,
  `Data.write` like a regular `File`).
- `Redshift_Details` — `Database.connect` argument for Redshift warehouses.

## Common usage

```
from Standard.AWS import AWS_Credential, S3, S3_File

buckets = S3.list_buckets AWS_Credential.Default

files = S3.list_objects "my-bucket" prefix="data/" credentials=AWS_Credential.Default

file = S3_File.new "s3://my-bucket/data/users.csv"
table = file.read

cred = AWS_Credential.Profile "my-aws-profile"
```

## Layout

- `src/AWS_Credential.enso` — credential types.
- `src/AWS_Region.enso` — region selection.
- `src/S3/` — S3 bucket and object operations.
- `src/Database/` — Redshift connector for `Standard.Database`.
- `src/SES/` — Simple Email Service.
- `src/Errors.enso` — AWS-specific errors.

## Things to avoid in generated code

- For credentials, prefer `AWS_Credential.Default` (env/instance role) or
  `AWS_Credential.Profile`; the universal `Enso_Secret` rule applies for stored
  access keys.
- Catching `S3_Error` / `S3_Bucket_Not_Found` / `S3_Key_Not_Found` swallows
  legitimate failures — let dataflow errors propagate unless you have a specific
  recovery.

## Where to read more

- `src/AWS_Credential.enso` — credential constructors.
- `src/S3/S3.enso` — bucket/object operations.
- `test/AWS_Tests/src/S3_Spec.enso` — S3 examples.
- `test/AWS_Tests/src/Credentials_Spec.enso` — credential usage.
