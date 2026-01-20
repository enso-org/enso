---
layout: developer-doc
title: Strava Integration
category: libraries
tags: [libraries, integrations]
order: 1
---

# Overview

The current Strava integration uses an application-level credential behind the
scenes to access Strava APIs. Until Enso goes through the Strava app validation
and branding process, this credential _only_ allows access to Enso's own Strava
account, rather than the end-user's Strava account. Nonetheless, an end-user can
authentication through their own Strava account to access the Enso account data.

## Creating A Strava Credential In The IDE

1. Select **Cloud** projects
1. **New Credential** (to the right of New Project)
1. Select **Strava** from the drop-down
1. Give it name, e.g. "mycred"
1. Create

## Connecting To Strava In The IDE

In the IDE, create the following graph:

```
    secret_uri = "enso://Users/\[YOUR LOGIN\]/mycred"
    mycred = Enso_Secret.get secret_uri
    strava_service = Saas.Strava.initialize mycred
    strava_user = strava_service.user
```
