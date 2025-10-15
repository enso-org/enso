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

* Go to [the Azure Portal](https://portal.azure.com/)
* Select service "App registrations"
* Select tab "All applications"
* Click "New Registration" upper right
   * Enter a name, such as "OAuth Integration"
   * For "Supported account types", select "All Microsoft account users"
   * For "Redirect URI", enter the staging and prod cloud endpoints, as well as "http://localhost:\[SOME PORT\]" for local debugging

## Create A Client Secret

* Go to the application in [the Azure Portal](https://portal.azure.com/)
* Select "Overview" on the left
* Copy the "Application (client) ID" value
* Select "Client Credentials"
* Select "New Client Secret"
* Enter name and expiration and create the secret
* Add the secrets to the [staging](https://github.com/enso-org/cloud-v2/blob/main/terraform/secrets.enc.staging.yaml)
  and [production](https://github.com/enso-org/cloud-v2/blob/main/terraform/secrets.enc.production.yaml) files, as shown
  below. These files are encrypted and must be edited with the [`sops`](https://github.com/enso-org/cloud-v2/blob/main/docs/SECRETS.md) tool.

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

* Go to the application in [the Azure Portal](https://portal.azure.com/)
* Select "Branding & Properties" on the left
* Under "Publisher Verification", click "Add MPN ID to verify publisher"
* In the pop-up dialog, read the requirements, then select "Sign up for Microsoft Partner Network (MPN)" which will take you to the [Partner Center](https://partner.microsoft.com/)
* In the Partner Center, select "Become a partner". You will have to click this button every time you return to this site
* Follow the directions, and good luck.
