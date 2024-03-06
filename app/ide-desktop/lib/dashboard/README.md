# Dashboard

The dashboard is the entrypoint into the application. It includes project
management, project sharing, and user accounts and authentication.

## Folder structure

- `mock/`: Overrides for specific files in `src/` when running Playwright tests.
- `e2e/`: Contains end-to-end tests.
- `**/__tests__/`: Contains all unit tests. Unit tests MUST be in a `__tests__/`
  subfolder, not beside (and not inside) the module they are testing.
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
- `index.html`: The entrypoint, in the format required by Vite.
- `404.html`: A copy of the entrypoint. This is served on unknown routes by
  certain static hosting providers.
- `esbuild-config.ts`: Configuration for ESBuild based on the environment
  variables. This is a dependency of `esbuild-config.ts` in sibling modules.

## Cloud environment variables

These are environment variables related to the cloud backend. If these variables
are not set, the build will still work, however access to the cloud backend will
be disabled.

Note that `ENSO_CLOUD_ENVIRONMENT` may be set to instead load the files from a
`.env` file. If `ENSO_CLOUD_ENVIRONMENT` is not set, or it is `production` or
`''`, then variables are attempted to be read from `.env`. If it is set to any
other value (say, `foo`), then it is loaded from `.foo.env`.

(While the convention in the Node.js ecosystem is to name the variants like
`.env.foo`, `.foo.env` has been chosen here because `.env` should be more like
a file extension. Visual Studio Code also understands `.foo.env` but not
`.env.foo`.)

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
- `ENSO_CLOUD_COGNITO_USER_POOL_ID`: The ID of the Cognito user pool.
- `ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID`: The client-side key of the
  Cognito user pool.
- `ENSO_CLOUD_COGNITO_DOMAIN`: The domain which all Cognito requests should go
  to.
- `ENSO_CLOUD_COGNITO_REGION`: The AWS region for which Cognito is configured.
  Should match the region of the domain in `ENSO_CLOUD_COGNITO_DOMAIN`.
