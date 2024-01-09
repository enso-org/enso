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
