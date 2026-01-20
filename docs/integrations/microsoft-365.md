---
layout: developer-doc
title: Microsoft 365 Integration
category: libraries
tags: [libraries, integrations]
order: 2
---

# Overview

OAuth integration requires an application registered in the Azure portal.

## Registering An OAuth Integration Application

- Go to the [Azure Portal](https://portal.azure.com/)
- Select service "App registrations"
- Select tab "All applications"
- Click "New Registration" upper left
  - Enter a name, such as "OAuth Integration"
  - For "Supported account types", select "Accounts in any organizational
    directory". (This will appear in the Overview section as "All Microsoft
    account users" after app registration.)
  - For "Redirect URI":
    - For the "Select a platform" dropdown, pick "Web"
    - Enter the cloud endpoint URL, as well as "http://localhost:PORT" for local
      debugging
  - Finish by clicking "Register"

## Create A Client Secret

- Go to the application in the [Azure Portal](https://portal.azure.com/)
- Select "Overview" on the left
- Copy the "Application (client) ID" value
- Click on the link next to "Client Credentials"
- Select "New Client Secret"
- Enter name and expiration and create the secret
- Add the secrets to the
  [staging](https://github.com/enso-org/cloud-v2/blob/main/terraform/secrets.enc.staging.yaml)
  and
  [production](https://github.com/enso-org/cloud-v2/blob/main/terraform/secrets.enc.production.yaml)
  files, as shown below. These files are encrypted and must be edited with the
  [`sops`](https://github.com/enso-org/cloud-v2/blob/main/docs/SECRETS.md) tool.

```
# Microsoft 365 OAuth integration
ms365_integration_client_id: [CLIENT ID]
ms365_integration_client_secret: [CLIENT SECRET]
```

## Add an MPN ID

And MPN ID is required for OAuth to work, and involves uploaded documents
proving ownership of the domain, such as domain registraiton recrods. Opening a
support ticket might be required if the request to add the MPN ID is initially
denied.

- Go to the application in the [Azure Portal](https://portal.azure.com/)
- Select "Branding & Properties" on the left
- Change "Publisher domain" to be "ensoanalytics.com" (it should be available in
  the drop-down list)
- If there is an existing MPN ID (see "Getting the MPD IDs" below), enter it by
  clicking "Add MPN ID to verify publisher"
- If there is no MPN ID yet:
  - Under "Publisher Verification", click "Add MPN ID to verify publisher"
  - In the pop-up dialog, read the requirements, then select "Sign up for
    Microsoft Partner Network (MPN)" which will take you to the
    [Partner Center](https://partner.microsoft.com/)
  - In the Partner Center, select "Become a partner". You will have to click
    this button every time you return to this site
  - Follow the directions, and good luck.

## Adding API Permissions

During OAuth, end-users will have to select Graph API scopes to authorize Enso
to use. Any such scope must be added to the app registration.

- Go to the application in the [Azure Portal](https://portal.azure.com/)
- Select "API permissions" on the left
- Click "Add a permission"
- In pop-up, select "Microsoft Graph"
- Click "Delegated permissions"
- Enter the scope name (e.g. "Mail.Read") in the search box
- Click "expand all" to see the list of matching scopes
- Select scopes to add
- Click "Add permissions"

## Getting the MPD IDs

- Sign in to the [Partner Center](https://partner.microsoft.com/)
- Click "Partner Center" at the top
- Click "Account settings" under the gear icon
- Select "Legal Info" on the left
- Click "View Partner IDs"
- Select the "Microsoft AI Cloud Partner Program" tab
- Copy the ID of the "PartnerGlobal" ID type

## Adding / Modifying Redirect URIs

- Go to the application in the [Azure Portal](https://portal.azure.com/)
- Select "Authentication" on the left
- The platformd displayed should be "Web", but if it's not, select "Add a
  platform" and choose “Public client/native”
- Add new URIs to the Web platform
- Click Save

## Granting admin constent for unattended integration test use

For integration tests, you must grant "admin consent" to API permissions, since
the oauth process will be automated and the test user cannot grant consent via
the browser.

(Before doing this step, first do "Adding API Permissions", above.)

- Go to the application in the [Azure Portal](https://portal.azure.com/)
- Select "API permissions" on the left
- Click "Grant admin consent for Default Directory"

## Running OneDrive tests

OneDrive tests require an existing credential at
'enso://Users/CloudTests./Microsoft365_ci_test_credential'. This credential must
be created by a Microsoft OneDrive user that has an SPO license.
