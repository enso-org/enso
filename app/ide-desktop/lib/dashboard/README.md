# Dashboard

The dashboard is the entrypoint into the application. It includes project
management, project sharing, and user accounts and authentication.

## Folder structure

- `mock/`: Overrides for specific files in `src/` when running Playwright tests.
- `test-component/`: Contains component tests.
- `test-e2e/`: Contains end-to-end tests.
- `**/__tests__/`: Contains all unit tests. Unit tests MUST be in a `__tests__/`
  subfolder, not beside the module they are testing.
- `src/`: The dashboard application.
  - `index.html`: The sole HTML file used by this SPA. It imports the TS entry
    point.
  - `authentication/src/`: The main body of the app.
    - `index.tsx`: The TS entry point.
    - `providers/`: Contains React `Context`s used by the main app.
    - `components/`: Contains the root component for the app.
    - `dashboard/`: The main body of the app. Directly in the folder, there are
      some utility modules that do not belong elsewhere.
      - `components/`: Contains all components used by the main app.
      - `events/`: Custom discriminated unions used to communicate messages
        between unrelated components.
    - `authentication/`: The authentication flow. This includes login,
      registration, and changing passwords.
      - `components/`: Contains all components used by the authentication flow.
      - `providers/`: Contains React `Context`s required for authentication, and
        used by the main app.

## Cloud environment variables

These are environment variables related to the cloud backend. If these variables
are not set, the build will still work, however access to the cloud backend will
be disabled.

- `ENSO_CLOUD_REDIRECT`: The domain (or `localhost:8080`) where the login link
  should redirect. Should include neither a path, nor a trailing slash.
- `ENSO_CLOUD_ENVIRONMENT`: The name of backend environment matching the
  provided configuration keys. For most builds this should be `production`,
  meaning that requests go to the production cloud backend.
- `ENSO_CLOUD_API_URL`: The root path for all API endpoints. Should not include
  a trailing slash.
- `ENSO_CLOUD_SENTRY_DSN`: The Sentry Data Source Name (DSN) for this
  environment. This should normally be the same for all environments.
- `ENSO_CLOUD_STRIPE_KEY`: Stripe's publishable client-side key.
- `ENSO_CLOUD_CHAT_URL`: The URL for the WebSocket server serving as the chat
  backend.
- `ENSO_CLOUD_AMPLIFY_USER_POOL_ID`: The ID of the Amplify user pool.
- `ENSO_CLOUD_AMPLIFY_USER_POOL_WEB_CLIENT_ID`: The client-side key of the
  Amplify user pool.
- `ENSO_CLOUD_AMPLIFY_DOMAIN`: The domain which all Amplify requests should go
  to.
- `ENSO_CLOUD_AMPLIFY_REGION`: The AWS region for which Amplify is configured.
  Should match the region of the domain in `ENSO_CLOUD_AMPLIFY_DOMAIN`.
