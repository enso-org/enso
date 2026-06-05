# Standard.Google_Api

**Deprecated placeholder.** This library was renamed to `Standard.Google`. It is
retained only because workflows on disk may still import it; it must not be
extended.

`src/Main.enso` is empty save for the placeholder note.

## What to do instead

For new code, use `Standard.Google`:

```
from Standard.Google import Google_Sheets, Google_Analytics, Google_Credential
```

## Things to avoid in generated code

- Do not import `Standard.Google_Api` in newly generated code.
- Do not add functionality here.
- If you encounter an existing import of `Google_Api`, prefer `Standard.Google`
  unless preserving compatibility with an older workflow is the explicit goal.

## Where to read more

- `distribution/lib/Standard/Google/0.0.0-dev/CLAUDE.md` — the live library.
