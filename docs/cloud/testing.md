---
layout: developer-doc
title: Cloud Testing
category: cloud
tags: [cloud, infrastructure]
order: 1
---

# Development Setup

- [Environment Variables](#environment-variables)
- [SBT Configuration](#sbt-configuration)
- [GUI Configuration](#gui-configuration)
- [Start The App](#start-the-app)
- [Authenticatng](#authenticating)

## Environemt Variables

### Staging

```
export AWS_PROFILE=cloud-staging
export ENSO_RUN_REAL_CLOUD_TEST=1
export ENSO_CLOUD_API_URL=https://kb6aej61n3.execute-api.eu-west-1.amazonaws.com
export ENSO_CLOUD_ENV=staging
```

### Production

```
export AWS_PROFILE=default
export ENSO_RUN_REAL_CLOUD_TEST=1
unset ENSO_CLOUD_API_URL ; export ENSO_CLOUD_API_URL
export ENSO_CLOUD_ENV=prod
```

## SBT Configuration

Some env vars need to be set directy within SBT before building the engine.

### Staging

```
eval System.setProperty("ENSO_CLOUD_API_URL", "https://kb6aej61n3.execute-api.eu-west-1.amazonaws.com")
eval System.setProperty("ENSO_CLOUD_ENV", "staging")
eval System.setProperty("ENSO_RUN_REAL_CLOUD_TEST","1")
```

You can check the values of these vars like this:

```
eval System.getProperty("ENSO_CLOUD_API_URL")
eval System.getProperty("ENSO_CLOUD_ENV")
eval System.getProperty("ENSO_RUN_REAL_CLOUD_TEST")
```

### Production

Nothing to change, just use the defaults.

## GUI Configuration

For staging, you must modify the symbolic link in the `app/gui/.dev-env`
submodule. The default configuration is:

```
$ ls -al app/gui/.dev-env/
...
lrwxr-xr-x   1 gmt  staff    12 Dec 12 13:42 .env.development@ -> .env.staging
-rw-r--r--   1 gmt  staff   747 Oct 24 12:12 .env.production
-rw-r--r--   1 gmt  staff  1452 Oct 24 12:12 .env.staging
...
```

(Note that any permanent changes to the files in `app/gui/.dev-env` are
submodule changes, and must be integrated into the main repo as a submodule
change.)

### Staging

For staging, link to `.env.staging`:

```
$ ls -al app/gui/.dev-env/
...
lrwxr-xr-x   1 gmt  staff    12 Dec 15 15:05 .env.development@ -> .env.staging
-rw-r--r--   1 gmt  staff   747 Oct 24 12:12 .env.production
-rw-r--r--   1 gmt  staff  1452 Oct 24 12:12 .env.staging
```

### Production

For prod, link to `.env.production`:

```
$ ls -al app/gui/.dev-env/
...
lrwxr-xr-x   1 gmt  staff    15 Dec 15 15:07 .env.development@ -> .env.production
-rw-r--r--   1 gmt  staff   747 Oct 24 12:12 .env.production
-rw-r--r--   1 gmt  staff  1452 Oct 24 12:12 .env.staging
```

## Start The App

### Staging

```
corepack pnpm i && corepack pnpm run compile && corepack pnpm -w dev:gui --mode staging
```

### Production

```
corepack pnpm i && corepack pnpm run compile && corepack pnpm -w dev:gui
```

## Authenticating

You must authenticate to the specific Enso Cloud backend you are using. If
you're using the staging cloud, the credentials lifetime is short and you must
reauthenticate multiple times a day.

1. Start the IDE
1. Log out
1. Open the network tab and filter on "token"
1. Log back in using your usual credentials
1. Copy the response to the token endpoint (shown below)
1. Update the values of `access_token` and `refresh_token` using the values
   taken from the token endpoint response
1. Make sure the `client_id` value in your `.credentials` matches the
   corresponding value of `cognito_client_id` in the
   [Terraform config files](https://github.com/enso-org/cloud-v2/tree/main/terraform).

<img width="604" height="261" alt="creds" src="https://github.com/user-attachments/assets/afe4dc88-b68a-4dc4-9cb3-6e7d8bf541d9" />
