# GUI Debugging

Unlike [runtime debugging document](runtime-debugging.md) this one focuses on
debugging the Enso GUI - e.g. mostly TypeScript code - in VSCode with a few
picturous steps.

Make sure to follow [running IDE steps](../CONTRIBUTING.md#running-ide). Make
sure you have `corepack pnpm dev:gui` task working. It is then used by the
VSCode Enso GUI debugger integration.

Open the root of the repository as a folder in VSCode.

<img width="170" height="380" alt="open" src="https://github.com/user-attachments/assets/11bf9ca7-d968-4f5a-85d8-a45c54e84b31"/>

Choose _"Enso GUI"_ as debug configuration.

<img width="170" height="170" alt="Enso GUI" src="https://github.com/user-attachments/assets/f4e65e7e-dbfc-4255-ada3-c926b00a0d16" />

Such a configuration launches the Enso GUI via `corepack pnpm dev:gui` behind
the scene. Then it connects a VSCode debugger to it and finally opens it all in
the integrated VSCode browser.

<img width="1288" height="587" alt="integrated browser" src="https://github.com/user-attachments/assets/9c1d0be9-fe34-4228-b5a9-916088fdd037" />

Now just place a breakpoint and debug the GUI code as usual!

<img width="1286" height="455" alt="breakpoint" src="https://github.com/user-attachments/assets/7248a85d-5193-4693-86e9-eb87abf32788" />
