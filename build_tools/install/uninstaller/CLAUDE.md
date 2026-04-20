# enso-uninstaller

Windows uninstaller binary for the Enso IDE. Uses the `self-replace` crate so it
can remove itself mid-uninstall (Windows doesn't let a running `.exe` delete its
own file — `self-replace` hot-swaps the process).

Reads the install manifest written by `enso-installer`, removes files, cleans
registry entries via `enso-install`, and exits. Invoked from Add/Remove
Programs.
