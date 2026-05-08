# Standard.Saas

SaaS service integrations. Currently includes Salesforce CRM, Strava activity
data, and SMTP email sending.

## Main entry points

- `Salesforce` — Salesforce CRM client; `Salesforce.initialize credentials`.
- `Strava` — Strava client; `Strava.initialize credentials`.
- `Email_Provider_SMTP` — SMTP-backed `Email_Provider`.

## Common usage

```
from Standard.Saas import Salesforce, Strava, Email_Provider_SMTP

sf = Salesforce.initialize (Enso_Secret.get "salesforce_creds")
user = sf.get_user
report = sf.get_report "Quarterly Pipeline"

strava = Strava.initialize (Enso_Secret.get "strava_creds")
me = strava.user

provider = Email_Provider_SMTP.SMTP server="smtp.gmail.com" username="user@gmail.com" password=(Enso_Secret.get "gmail_app_password") port=587
```

## Layout

- `src/Salesforce.enso` — CRM client.
- `src/Strava.enso` — Strava client.
- `src/Email_Provider_SMTP.enso` — SMTP email provider, registered as an
  `Email_Provider` SPI.

## Things to avoid in generated code

- Ignoring token-refresh failures — re-initialize the client when an
  authentication error fires rather than retrying with a stale token.
- Salesforce: grouped or multi-dimensional reports are not supported through
  this client; flatten in Salesforce or query a different report variant.

## Where to read more

- `src/Salesforce.enso`, `src/Strava.enso`, `src/Email_Provider_SMTP.enso` —
  full type docs.
- `test/Saas_Tests/src/Salesforce_Spec.enso` — Salesforce examples.
