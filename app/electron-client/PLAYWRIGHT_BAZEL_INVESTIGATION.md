# Playwright + Bazel Integration Investigation

## Problem Statement

`bazel test //app/electron-client:test` fails, while
`pnpm run ide-integration-test` works.

**Error:**

```
Error: Playwright Test did not expect test() to be called here.
Most common reasons include:
- You are calling test() in a configuration file.
- You are calling test() in a file that is imported by the configuration file.
- You have two different versions of @playwright/test.
```

## Root Cause Analysis

### Trigger Condition (Confirmed)

The `"type": "module"` field in package.json triggers the issue. Controlled
experiments with `rules_playwright/examples/rules_js` (a separate repo used only
for testing, not part of electron-client) confirmed:

| Change to baseline (no `"type": "module"`) | Result                         |
| ------------------------------------------ | ------------------------------ |
| Add fixture wrapper file                   | ✅ PASSES                      |
| Add `"type": "module"` alone               | ❌ FAILS (dual instance error) |
| Fixture wrapper + `"type": "module"`       | ❌ FAILS                       |

**Note:** electron-client uses the generated `playwright_bin` from
`@npm//app/electron-client:playwright/package_json.bzl` directly. It does not
use `rules_playwright`.

### Initial Theory: `--preserve-symlinks-main` (DISPROVEN)

We initially believed the issue was `--preserve-symlinks-main` being passed by
aspect_rules_js. However, **debug logs show this flag is NOT being passed**:

```
DEBUG: aspect_rules_js[js_test]: JS_BINARY__NODE_OPTIONS
INFO: aspect_rules_js[js_test]: running .../test_node_bin/node -- .../playwright/cli.js test
```

The `--` separator comes immediately after the node path, meaning no node flags
are added. Setting `preserve_symlinks_main = False` on the rule was tried and
**did not fix the issue**.

### Previous Theory: Node FS Patches (DISPROVEN)

The debug logs reveal that aspect_rules_js applies **filesystem patches** to
Node.js:

```
DEBUG: aspect_rules_js[js_test]: JS_BINARY__NODE_PATCHES .../aspect_rules_js+/js/private/node-patches/register.cjs
DEBUG: aspect_rules_js[js_test]: node fs patches will be applied with roots: ...
```

The node wrapper script (`test_node_bin/node`) does:

```bash
exec "$JS_BINARY__NODE_BINARY" --require "$JS_BINARY__NODE_PATCHES" "$@"
```

**We initially theorized** that these patches intercept `realpath`, `lstat`,
`readlink`, and other fs operations, causing ESM module identity issues.
However, **debug logging disproved this theory.**

### Debug Logging Results (2026-01-21)

Added comprehensive debug logging across config, fixture, and test files:

**Module Resolution URLs (all identical):**

```
[playwright.config] playwright/test resolved to: file://.../bin/node_modules/.aspect_rules_js/playwright@1.55.1/node_modules/playwright/test.mjs
[electronTest] playwright/test resolved to: file://.../bin/node_modules/.aspect_rules_js/playwright@1.55.1/node_modules/playwright/test.mjs
[cloudWorkflow.spec] playwright/test resolved to: file://.../bin/node_modules/.aspect_rules_js/playwright@1.55.1/node_modules/playwright/test.mjs
```

**Object Identity Check:**

```
[cloudWorkflow.spec] base === globalThis.__playwrightTestFromConfig: true
```

**Critical Finding: There is NO dual module instance.** The `test` object is the
SAME instance across config, fixture, and test files. The error message is
misleading.

### Previous Theory: Suite Context Not Initialized (SUPERSEDED)

The error "Playwright Test did not expect test() to be called here" was
initially thought to be about suite context. However, the CommonJS experiment
revealed the TRUE root cause.

### Previous Theory: Sandbox Path Mismatch (PARTIALLY CORRECT)

Initial analysis suggested sandbox paths (`sandbox/processwrapper-sandbox/N/`)
caused the issue. However, testing with `tags = ["local"]` and
`--strategy=TestRunner=local,standalone` revealed the sandbox is NOT the root
cause.

### Current Theory: Dual node_modules Locations (2026-01-21)

Running with local execution (no sandbox) still shows the dual-module error. The
paths reveal the TRUE issue:

**First load:** `bin/node_modules/.aspect_rules_js/playwright@1.55.1/...`
**Second load:**
`bin/app/electron-client/test_/test.runfiles/_main/node_modules/.aspect_rules_js/playwright@1.55.1/...`

Bazel creates TWO copies of node_modules:

1. **Direct:** `bazel-out/darwin_arm64-opt/bin/node_modules/`
2. **Runfiles:**
   `bazel-out/darwin_arm64-opt/bin/app/electron-client/test_/test.runfiles/_main/node_modules/`

When Playwright loads:

1. Config file resolution uses the direct `bin/node_modules/` path
2. Test file resolution uses the runfiles `test.runfiles/_main/node_modules/`
   path

Node.js sees these as two different modules (different paths = different cache
entries), causing Playwright's dual-instance detection to trigger.

### The Dilemma (Revised)

| Approach          | Result                                            |
| ----------------- | ------------------------------------------------- |
| Default (sandbox) | sandbox path + runfiles path mismatch             |
| Local execution   | bin/node_modules + runfiles/node_modules mismatch |
| FS patches OFF    | "No tests found"                                  |

The issue is fundamental to how aspect_rules_js sets up the runfiles tree with a
separate copy of node_modules.

## Why electron-client Requires ESM

- Codebase uses ES module syntax throughout
- `electronTest.ts` uses `import.meta.dirname`
- Many dependencies expect ESM
- Removing `"type": "module"` would require significant refactoring

## What Was Tried (All Failed)

| Approach                                                      | Result           | Why It Failed                                                                       |
| ------------------------------------------------------------- | ---------------- | ----------------------------------------------------------------------------------- |
| `preserve_symlinks_main = False`                              | Same error       | Flag wasn't the issue (not being passed anyway)                                     |
| Remove top-level await                                        | Same error       | Not the cause                                                                       |
| Direct import pattern                                         | Same error       | Module identity issue is at fs level, not import level                              |
| Custom test runner (clear `JS_BINARY__*` env vars)            | "No tests found" | Module resolution breaks without Bazel wrapper                                      |
| `NODE_OPTIONS="--no-preserve-symlinks"`                       | No effect        | Flag wasn't being passed via NODE_OPTIONS                                           |
| `JS_BINARY__PATCH_NODE_FS=0`                                  | "No tests found" | Playwright can't traverse symlinks to find tests                                    |
| Force CommonJS for tests                                      | Different error  | Dual-module issue persists due to path mismatch (see below)                         |
| `tags = ["local"]` + `--strategy=TestRunner=local`            | Same error       | Not sandbox-related; bin/node_modules vs runfiles/node_modules                      |
| `NODE_PATH` via env (relative path)                           | Same error       | NODE_PATH doesn't override normal resolution; config symlink still resolves to bin/ |
| `NODE_PATH` via env (`$$JS_BINARY__RUNFILES`)                 | Script error     | Variable not defined when env vars are exported (line 15 vs line 273)               |
| `NODE_OPTIONS="--preserve-symlinks --preserve-symlinks-main"` | Different error  | Breaks pnpm-style nested symlinks; `Cannot find module 'playwright-core'`           |

### CommonJS Approach Results (2026-01-21)

Created `tests/package.json` with `{"type": "commonjs"}` and replaced
`import.meta.dirname` with `__dirname` in `electronTest.ts`.

**Result:** Different error that reveals the TRUE dual-module issue:

```
Error: Requiring @playwright/test second time,
First:
   at cloudWorkflow.spec.ts:3
> 3 | import { test as base, expect } from 'playwright/test'
    at Object.<anonymous> (.../execroot/_main/bazel-out/.../playwright/lib/index.js:60:33)
    ...

Second:
    at Object.<anonymous> (.../execroot/_main/bazel-out/.../playwright/lib/index.js:60:33)
    at Object.<anonymous> (.../sandbox/processwrapper-sandbox/5/execroot/_main/.../tests/cloudWorkflow.spec.ts:3:1)
```

**Key Insight:** The paths reveal why dual-module occurs:

- **Config loading:** Uses `execroot/_main/bazel-out/...`
- **Test file loading:** Uses
  `sandbox/processwrapper-sandbox/5/execroot/_main/bazel-out/...`

Node.js CommonJS module caching is path-based. When the same physical module is
accessed through different paths (execroot vs sandbox), Node treats them as
separate modules. Playwright detects this and throws an error.

This confirms the issue is NOT about ESM vs CommonJS loaders - it's
fundamentally about **Bazel's sandbox creating distinct path prefixes** that
break Node.js module identity.

### NODE_PATH Experiment (2026-01-21)

Attempted to use `NODE_PATH` environment variable to force consistent module
resolution.

**Attempt 1: Bazel variable expansion**

```python
env = {
    "NODE_PATH": "$$JS_BINARY__RUNFILES/_main/node_modules",
},
```

**Result:** Script error - `JS_BINARY__RUNFILES: unbound variable`

**Why it failed:** The generated shell script has `set -o nounset` and exports
env vars from the `env` parameter on line 15, but `JS_BINARY__RUNFILES` isn't
computed until line 273. The Bazel `$$` correctly expands to `$` at runtime, but
the variable doesn't exist yet when the export happens.

**Attempt 2: Relative path**

```python
env = {
    "NODE_PATH": "../../node_modules",
},
```

**Result:** Same dual-module error.

**Why it failed:** NODE_PATH is only consulted AFTER normal module resolution
fails. When a file imports `@playwright/test`, Node.js first:

1. Resolves the importing file's path (following symlinks)
2. Walks up from that resolved location looking for `node_modules/`
3. Only if that fails, checks NODE_PATH

Since `playwright.config.ts` is a symlink to
`bin/app/electron-client/playwright.config.ts`, Node resolves `@playwright/test`
from `bin/node_modules/` before ever checking NODE_PATH.

### --preserve-symlinks Experiment (2026-01-21)

Attempted to prevent Node from resolving symlinks when determining file
locations:

```python
env = {
    "NODE_OPTIONS": "--preserve-symlinks --preserve-symlinks-main",
},
```

**Result:** Different error - `Cannot find module 'playwright-core'`

**Partial success:** The dual-module error was eliminated! With
`--preserve-symlinks`, the config file's location is treated as
`runfiles/_main/app/electron-client/playwright.config.ts` (the symlink path)
rather than `bin/app/electron-client/playwright.config.ts` (the target path).

**Why it ultimately failed:** The `--preserve-symlinks` flag affects ALL
symlinks, including those used by pnpm/aspect_rules_js for the nested
node_modules structure:

```
app/electron-client/node_modules/playwright -> ../../../node_modules/.aspect_rules_js/playwright@1.55.1/node_modules/playwright
```

When Node doesn't follow this symlink, it can't find `playwright-core` which is
a peer dependency located at:

```
node_modules/.aspect_rules_js/playwright@1.55.1/node_modules/playwright-core
```

**Key insight:** We need SELECTIVE symlink preservation - preserve symlinks for
source files (config, tests) but follow symlinks for node_modules. Node.js
doesn't support this granularity.

### Symlink Structure Analysis (2026-01-21)

Verified that both config and test files are symlinks to `bin/`:

```
runfiles/_main/app/electron-client/playwright.config.ts
  -> bin/app/electron-client/playwright.config.ts

runfiles/_main/app/electron-client/tests/cloudWorkflow.spec.ts
  -> bin/app/electron-client/tests/cloudWorkflow.spec.ts
```

Local node_modules in runfiles uses relative symlinks to the pnpm store:

```
runfiles/_main/app/electron-client/node_modules/playwright
  -> ../../../node_modules/.aspect_rules_js/playwright@1.55.1/node_modules/playwright
```

## Potential Solutions (Revised)

### ~~1. Debug Module Resolution Paths~~ ✅ Done

CommonJS error output revealed the exact paths causing dual-module issues.

### ~~2. Force CommonJS for Tests~~ ❌ Failed

Tried `tests/package.json` with `{"type": "commonjs"}`. Same underlying issue -
path mismatch causes dual-module.

### ~~3. Disable Bazel Sandboxing~~ ❌ Failed

Tried `tags = ["local"]` + `--strategy=TestRunner=local,standalone`. Issue is
NOT sandboxing - it's bin/node_modules vs runfiles/node_modules.

### ~~4. Ensure Consistent node_modules Resolution via NODE_PATH~~ ❌ Failed

Tried setting `NODE_PATH` via env parameter. Two issues:

1. Using `$$JS_BINARY__RUNFILES` fails because the variable isn't defined when
   env vars are exported
2. Using relative paths doesn't help because NODE_PATH is only consulted after
   normal resolution fails

### ~~5. Use --preserve-symlinks~~ ❌ Partial Success, Ultimately Failed

Setting `NODE_OPTIONS="--preserve-symlinks --preserve-symlinks-main"` fixes the
dual-module error but breaks pnpm-style nested symlinks in node_modules, causing
`Cannot find module 'playwright-core'`.

**Key insight:** We need selective symlink preservation - preserve for source
files, follow for node_modules. Node.js doesn't support this.

### 6. Ensure Consistent node_modules Resolution (Revised)

The core problem remains: Playwright resolves `playwright/test` from two
locations:

- `bin/node_modules/` (when loading config, after symlink resolution)
- `test.runfiles/_main/node_modules/` (when loading test files)

Remaining approaches to try:

- Copy files instead of symlinking (if aspect_rules_js supports this)
- Use a custom Node.js loader that selectively handles symlinks
- Patch the generated shell script to set NODE_PATH after JS_BINARY\_\_RUNFILES
  is computed

### 7. Pre-compile TypeScript

Compile tests to JavaScript first. This is
[officially documented by Playwright](https://playwright.dev/docs/test-typescript).

**Blocker:** TypeScript errors in test files need fixing first.

**Note:** Bundling test files with esbuild is NOT a proven approach. The
Playwright team stated
["Playwright isn't ready to be bundled"](https://github.com/microsoft/playwright/issues/35479).

### 8. File Issue with aspect_rules_js

The fundamental issue is that aspect_rules_js creates two node_modules
locations:

- Direct: `bin/node_modules/`
- Runfiles: `test.runfiles/_main/node_modules/`

When a tool like Playwright resolves the same package from both locations,
Node.js treats them as separate modules. This may require upstream fixes.

### 9. Investigate `include_npm_sources` or Similar Options

Check if aspect_rules_js has options to control how node_modules are
linked/copied into the runfiles tree.

## Current BUILD.bazel State

```python
playwright_bin.playwright_test(
    name = "test",
    data = npm_link_targets() + glob(["tests/*.ts"]) + [
        "package.json",
        "playwright.config.ts",
        "tests/package.json",  # Added for CommonJS experiment
    ],
    args = ["test"],
    chdir = package_name(),
    visibility = ["//visibility:public"],
    preserve_symlinks_main = False,
    log_level = "debug",
    tags = ["local"],  # Tried to bypass sandboxing - didn't fix issue
)
```

**Note:** Requires `--strategy=TestRunner=local,standalone` flag when running.

## Current Test Files State

- `tests/package.json` - Contains `{"type": "commonjs"}` (for CommonJS
  experiment)
- `tests/headless/package.json` - Contains `{"type": "module"}` (preserves ESM
  for Vitest)
- `tests/electronTest.ts` - Uses `__dirname` instead of `import.meta.dirname`

## Key Files

- `app/electron-client/BUILD.bazel` - Bazel config
- `app/electron-client/tests/electronTest.ts` - Test fixtures
- `app/electron-client/playwright.config.ts` - Playwright config
- `test_node_bin/node` - Generated node wrapper that applies fs patches
- `aspect_rules_js+/js/private/node-patches/register.cjs` - FS patch
  registration
- `aspect_rules_js+/js/private/node-patches/fs.cjs` - FS patch implementation

## Debug Logs Reference

Enable debug logs with:

```bash
JS_BINARY__LOG_DEBUG=1 bazel test //app/electron-client:test
```

Key environment variables:

- `JS_BINARY__NODE_BINARY` - Path to actual Node.js binary
- `JS_BINARY__NODE_WRAPPER` - Path to wrapper script
- `JS_BINARY__NODE_PATCHES` - Path to fs patches module
- `JS_BINARY__PATCH_NODE_FS` - Set to "0" to disable fs patches
- `JS_BINARY__FS_PATCH_ROOTS` - Allowed roots for fs operations

## Next Steps

### Completed

1. ~~**Add debug logging** to trace module resolution paths~~ ✅ Done - URLs and
   instances are identical
2. ~~**Try `--workers=1`**~~ ✅ Done - no effect (config already sets
   `workers: 1`)
3. ~~**Verify module instance identity**~~ ✅ Done - confirmed same instance via
   globalThis check
4. ~~**Try adding `tests/package.json` with `"type": "commonjs"`**~~ ✅ Done -
   revealed path mismatch issue
5. ~~**Try `tags = ["local"]`**~~ ✅ Done - confirmed issue is NOT sandboxing,
   but bin vs runfiles node_modules
6. ~~**Try setting `NODE_PATH`**~~ ✅ Done - doesn't work (consulted after
   normal resolution)
7. ~~**Try `--preserve-symlinks`**~~ ✅ Done - partial success, breaks pnpm
   nested symlinks

### Remaining

8. **Investigate aspect_rules_js node_modules linking** - understand why two
   locations exist and if it can be configured
9. **File issue with aspect_rules_js** - report bin/node_modules vs
   runfiles/node_modules incompatibility with Playwright
10. **Check if there's a `copy_data_to_bin` option** - or similar to control
    whether files are symlinked or copied
11. **Try custom Node.js loader** - selectively handle symlinks for source files
    vs node_modules
12. **Investigate patching the generated shell script** - set NODE_PATH after
    JS_BINARY\_\_RUNFILES is computed
13. **Pre-compile TypeScript** - may avoid the issue by not using Playwright's
    TS transform

---

## Wrapper Script Approach (2026-01-21)

### Approach

Created a custom JavaScript wrapper (`playwright-wrapper.mjs`) that:

1. Runs after Bazel sets up `JS_BINARY__RUNFILES`
2. Sets `NODE_PATH` at runtime to force consistent module resolution
3. Spawns Playwright CLI with the corrected environment

### Implementation

**`playwright-wrapper.mjs`:**

```javascript
#!/usr/bin/env node
import { spawn } from "node:child_process";
import path from "node:path";

const runfilesDir = process.env.JS_BINARY__RUNFILES;
if (!runfilesDir) {
  console.error("ERROR: JS_BINARY__RUNFILES not set. Run via Bazel.");
  process.exit(1);
}

const nodeModulesPath = path.join(runfilesDir, "_main", "node_modules");
const newNodePath = process.env.NODE_PATH
  ? `${nodeModulesPath}:${process.env.NODE_PATH}`
  : nodeModulesPath;

const playwrightCli = path.join(
  nodeModulesPath,
  ".aspect_rules_js",
  "playwright@1.55.1",
  "node_modules",
  "playwright",
  "cli.js",
);

const child = spawn(
  process.execPath,
  [playwrightCli, ...process.argv.slice(2)],
  {
    stdio: "inherit",
    env: { ...process.env, NODE_PATH: newNodePath },
    cwd: process.cwd(),
  },
);

child.on("exit", (code, signal) => {
  if (signal) process.kill(process.pid, signal);
  else process.exit(code ?? 1);
});
```

**BUILD.bazel change:** Replaced `playwright_bin.playwright_test` with
`js_test`:

```python
load("@aspect_rules_js//js:defs.bzl", "js_test")

js_test(
    name = "test",
    entry_point = "playwright-wrapper.mjs",
    data = npm_link_targets() + glob(["tests/*.ts"]) + [
        "package.json",
        "playwright.config.ts",
        "tests/package.json",
    ],
    args = ["test"],
    chdir = package_name(),
    visibility = ["//visibility:public"],
    log_level = "debug",
    tags = ["local"],
)
```

### Known Limitations

1. **Playwright version hardcoded**: The path includes `playwright@1.55.1`. If
   version changes, wrapper needs update.
2. **NODE_PATH priority**: NODE_PATH is consulted after normal resolution. If
   symlinks still resolve first, may need to combine with `--preserve-symlinks`.
3. **Windows paths**: Uses `:` as PATH separator. Use `path.delimiter` if
   Windows support needed.

### Result

**FAILED** - Same dual-module error persists.

**Error output:**

```
Error: Requiring @playwright/test second time,
First:
    at Object.<anonymous> (.../bin/node_modules/.aspect_rules_js/playwright@1.55.1/node_modules/playwright/lib/index.js:60:33)
    at Object.<anonymous> (.../bin/node_modules/.aspect_rules_js/playwright@1.55.1/node_modules/playwright/test.js:17:13)
    at Object.<anonymous> (.../test.runfiles/_main/app/electron-client/tests/cloudWorkflow.spec.ts:3:1)

Second:
    at Object.<anonymous> (.../bin/node_modules/.aspect_rules_js/playwright@1.55.1/node_modules/playwright/lib/index.js:60:33)
    at Object.<anonymous> (.../bin/node_modules/.aspect_rules_js/playwright@1.55.1/node_modules/playwright/test.js:17:13)
    at Object.<anonymous> (.../test.runfiles/_main/app/electron-client/tests/cloudWorkflow.spec.ts:3:1)
```

**Why it failed:** Setting `NODE_PATH` in the spawned child process doesn't
prevent normal module resolution. Node.js module resolution order is:

1. Normal resolution (walk up from file location to find `node_modules/`)
2. Only if step 1 fails, consult `NODE_PATH`

The config file `playwright.config.ts` is a symlink pointing to
`bin/app/electron-client/playwright.config.ts`. When Playwright loads it and
resolves `playwright/test`:

1. Node resolves the symlink to `bin/...` location
2. Node walks up from `bin/app/electron-client/` and finds `bin/node_modules/`
3. `NODE_PATH` is never consulted because step 2 succeeded

The test files also resolve `playwright/test`:

1. Test file is in `runfiles/_main/app/electron-client/tests/`
2. Node walks up and finds `runfiles/_main/node_modules/`

Result: Same module, two paths, dual-instance error.

**Conclusion:** The wrapper script approach cannot solve the dual-module issue
because `NODE_PATH` doesn't override normal module resolution - it's a fallback,
not a priority override.

---

## Custom ESM Loader Approach (2026-01-21)

### Approach

Created a custom ESM loader that selectively rewrites module resolution paths:

- **Preserves symlinks for source files** - config and tests stay in
  `runfiles/_main/`
- **Follows symlinks for node_modules** - pnpm's nested structure works normally

This approach uses the `--import` flag to register a custom `resolve` hook
before Playwright runs.

### Implementation

**`selective-symlink-loader.mjs`:**

```javascript
// Rewrite bin/ paths back to runfiles for source files (not node_modules)
export async function resolve(specifier, context, nextResolve) {
  const result = await nextResolve(specifier, context);

  // Only modify source files, not node_modules
  if (
    result.url &&
    result.url.includes("/bin/") &&
    !result.url.includes("node_modules")
  ) {
    // Rewrite: bin/app/electron-client/... -> runfiles/_main/app/electron-client/...
    const rewritten = result.url.replace(
      /\/bin\/(app\/electron-client\/)/,
      "/test.runfiles/_main/$1",
    );
    if (rewritten !== result.url) {
      return { ...result, url: rewritten };
    }
  }

  return result;
}
```

**Updated `playwright-wrapper.mjs`:**

```javascript
const loaderPath = path.join(
  runfilesDir,
  "_main",
  "app",
  "electron-client",
  "selective-symlink-loader.mjs",
);

const child = spawn(
  process.execPath,
  ["--import", loaderPath, playwrightCli, ...process.argv.slice(2)],
  {
    /* ... */
  },
);
```

**Updated BUILD.bazel:**

```python
js_test(
    name = "test",
    entry_point = "playwright-wrapper.mjs",
    data = npm_link_targets() + glob(["tests/*.ts"]) + [
        "package.json",
        "playwright.config.ts",
        "tests/package.json",
        "selective-symlink-loader.mjs",  # Added loader
    ],
    # ...
)
```

### Verification

```bash
bazel test //app/electron-client:test --strategy=TestRunner=local,standalone
```

### Result

**SUCCESS** - The dual-module error is resolved!

The initial ESM-only approach (using `export async function resolve`) didn't
work because:

1. ESM hooks exported from a module loaded via `--import` aren't automatically
   registered
2. Playwright uses CommonJS internally (via pirates) for TypeScript
   transformation

The solution required:

1. Use `module.register()` to properly register ESM hooks from a separate file
2. Patch both CJS (`Module._resolveFilename`) and ESM (`resolve` hook) paths
3. Rewrite `bin/node_modules/` paths to `runfiles/_main/node_modules/` using the
   `JS_BINARY__RUNFILES` env var

**Final Implementation:**

- `selective-symlink-loader.mjs` - Main loader that patches CJS and registers
  ESM hooks
- `selective-symlink-loader-hooks.mjs` - ESM hooks file registered via
  `module.register()`

### New Error (Unrelated)

After fixing the dual-module issue, tests now fail with:

```
Error: Package subpath './src/text' is not defined by "exports" in .../node_modules/enso-common/package.json
```

This is a **separate issue** - the `electronTest.ts` file imports
`enso-common/src/text` which isn't exported from the package. This needs to be
fixed by either:

1. Adding `./src/text` to `enso-common`'s `exports` field
2. Changing the import in `electronTest.ts` to use an exported path

### Files Created/Modified

- `selective-symlink-loader.mjs` - Custom module loader (CJS hook + ESM
  registration)
- `selective-symlink-loader-hooks.mjs` - ESM loader hooks
- `playwright-wrapper.mjs` - Updated to use `--import` flag with the loader
- `BUILD.bazel` - Added loader files to `data` dependencies

---

## `enso-common/src/text` Import Error Investigation (2026-01-21)

### Problem

After fixing the dual-module issue, tests fail with:

```
Error: Package subpath './src/text' is not defined by "exports" in .../node_modules/enso-common/package.json
```

### Initial Plan: Use `--conditions=source` Flag

The plan was to add `--conditions=source` to Node.js arguments in
`playwright-wrapper.mjs`. The theory was:

1. `enso-common/package.json` has exports with a `"source"` condition that
   points to TypeScript files
2. Adding `--conditions=source` would make Node.js use this condition
3. Playwright already transforms TypeScript on-the-fly

### Result of `--conditions=source` Approach

**FAILED** - Error changed to:

```
Error: Cannot find module '.../enso-common/src/text.ts'
```

The `--conditions=source` flag DID work for exports resolution (Node.js now
tried to load `.ts` files), but the source files don't exist in the Bazel
sandbox.

### Key Findings

#### 1. The Bazel Sandbox Structure

The `enso-common` npm_package only includes:

- `package.json`
- `dist/*.js` (compiled TypeScript output)

It does NOT include:

- `src/*.ts` (TypeScript source files)

This is because the `npm_package` rule in `app/common/BUILD.bazel` only includes
`:tsc` (compiled output), not the source files.

#### 2. Two Resolution Paths Verified

**Direct test outside Bazel (works):**

```bash
cd app/electron-client && node -e "import('enso-common/src/text')"
# Resolves to: .../enso-common/dist/text.js (correct!)
```

**Inside Bazel via Playwright (fails):**

```
Error: Package subpath './src/text' is not defined by "exports"
```

#### 3. CJS vs ESM Resolution Difference

Debug logging revealed the critical issue:

```
[DEBUG] CJS resolving: "enso-common/src/text" from: .../tests/electronTest.ts
[DEBUG] CJS resolve FAILED for "enso-common/src/text": Package subpath './src/text' is not defined by "exports"
```

The resolution is happening via **CommonJS `Module._resolveFilename`**, NOT ESM
`import()`. This is because Playwright uses CommonJS internally for TypeScript
transformation (via pirates).

#### 4. The Root Cause: Missing "require" Export Condition

The `enso-common/package.json` exports field:

```json
{
  "./src/*": {
    "source": "./src/*.ts",
    "types": "./src/*.ts",
    "import": "./dist/*.js"
  }
}
```

**Problem:** There is NO `"require"` condition!

- ESM `import()` uses the `"import"` condition → `./dist/*.js` ✅
- CJS `require()` uses the `"require"` condition → **not defined** ❌

Node.js CJS resolution for exports patterns requires a `"require"` condition.
Without it, the pattern matching fails even though the pattern syntax is
correct.

#### 5. Why Direct Test Works

When testing directly with `node -e "import('enso-common/src/text')"`:

- Uses ESM `import()` semantics
- Uses the `"import"` condition
- Pattern `./src/*` matches `./src/text` → resolves to `./dist/text.js`

When Playwright loads via CJS:

- Uses CJS `require()` semantics
- Looks for `"require"` condition
- No `"require"` condition defined → fails with "subpath not defined"

### Verified Facts

1. ✅ The `dist/text.js` file EXISTS in the Bazel sandbox
2. ✅ The `package.json` with correct exports IS present in sandbox
3. ✅ The wildcard pattern `./src/*` is syntactically correct
4. ✅ ESM resolution works correctly (verified with direct `node -e "import()"`
   test)
5. ❌ CJS resolution fails due to missing `"require"` condition

### Solutions to Explore

#### Option A: Add "require" Condition to Workspace Packages

Update `app/common/package.json` to include a `"require"` condition:

```json
{
  "./src/*": {
    "source": "./src/*.ts",
    "types": "./src/*.ts",
    "import": "./dist/*.js",
    "require": "./dist/*.js"
  }
}
```

**Pros:**

- Simple fix
- Standard Node.js pattern
- Works for all workspace packages

**Cons:**

- Need to update ALL workspace packages (enso-common, ydoc-shared, etc.)
- May need to verify CJS compatibility of compiled output

#### Option B: Change Import Syntax in Test Files

Change `electronTest.ts` from:

```typescript
import { TEXTS } from "enso-common/src/text";
```

To:

```typescript
import { TEXTS } from "enso-common"; // If TEXTS is re-exported from index
```

Or use dynamic import:

```typescript
const { TEXTS } = await import("enso-common/src/text");
```

**Pros:**

- No package.json changes needed

**Cons:**

- Requires TEXTS to be exported from main entry point
- Dynamic import changes code structure

#### Option C: Include Source Files in npm_package

Modify `app/common/BUILD.bazel` to include `src/*.ts` in the npm_package:

```python
npm_package(
    name = "pkg",
    srcs = [
        "package.json",
        ":tsc",
    ] + glob(["src/**/*.ts"]),  # Add source files
    visibility = ["//visibility:public"],
)
```

Then `--conditions=source` would work.

**Pros:**

- `--conditions=source` approach becomes viable
- Source maps would work better

**Cons:**

- Increases package size
- Need to update all workspace packages
- May cause confusion about which files are used

#### Option D: Force ESM for Test Files

Investigate why Playwright uses CJS for test file transformation. Options:

- Configure Playwright to use ESM loader for TypeScript
- Use a different TypeScript transformer that preserves ESM

**Pros:**

- Fixes root cause of CJS/ESM mismatch

**Cons:**

- May require Playwright configuration not documented
- Playwright's TS handling is internal implementation detail

### Recommended Next Step

**Option A (Add "require" condition)** is the most straightforward fix:

1. Update `app/common/package.json`:

   ```json
   {
     "./src/*": {
       "source": "./src/*.ts",
       "types": "./src/*.ts",
       "import": "./dist/*.js",
       "require": "./dist/*.js"
     }
   }
   ```

2. Verify with test:

   ```bash
   bazel test //app/electron-client:test --strategy=TestRunner=local,standalone
   ```

3. If successful, update other workspace packages that may have similar imports:
   - `ydoc-shared`
   - Other packages with `./src/*` exports

### Current State

- `playwright-wrapper.mjs` does NOT have `--conditions=source` (reverted)
- Debug logging in `selective-symlink-loader.mjs` has been removed
- Tests still fail with "Package subpath not defined" error

---

## SOLUTION FOUND (2026-01-21)

### Root Cause Identified

The `tests/package.json` file had `"type": "commonjs"` leftover from a previous
debugging experiment. This was forcing ALL test files to use CommonJS
resolution, which:

1. Used the `"require"` condition instead of `"import"` for exports
2. The workspace packages (`enso-common`, etc.) only had `"import"` condition
3. Result: "Package subpath not defined" error

### The Fix

**Two simple changes:**

1. **`tests/package.json`**: Changed from `"type": "commonjs"` to
   `"type": "module"`
2. **`tests/electronTest.ts`**: Changed `__dirname` back to
   `import.meta.dirname` (was also changed during the CommonJS experiment)

### Why It Works Now

With `"type": "module"` in `tests/package.json`:

- Playwright uses ESM resolution for test files
- ESM resolution uses the `"import"` condition from exports
- `enso-common/src/text` correctly resolves to `enso-common/dist/text.js`

### Simplified Loader

The CJS hook in `selective-symlink-loader.mjs` was removed since it's not needed
for ESM projects. The loader now only registers ESM hooks via
`module.register()`.

### Final Files

**`tests/package.json`:**

```json
{
  "type": "module"
}
```

**`selective-symlink-loader.mjs`:**

```javascript
import { register } from "node:module";
register("./selective-symlink-loader-hooks.mjs", import.meta.url);
```

**`selective-symlink-loader-hooks.mjs`:** (unchanged - rewrites bin/node_modules
to runfiles/node_modules for ESM)

**BUILD.bazel:**

```python
js_test(
    name = "test",
    entry_point = "playwright-wrapper.mjs",
    data = npm_link_targets() + glob(["tests/*.ts"]) + [
        "package.json",
        "playwright.config.ts",
        "tests/package.json",
        "selective-symlink-loader.mjs",
        "selective-symlink-loader-hooks.mjs",
    ],
    args = ["test"],
    chdir = package_name(),
    visibility = ["//visibility:public"],
    log_level = "debug",
    tags = ["local"],
)
```

### Test Results

Tests now run successfully (no module resolution errors). They fail with "Cannot
find Enso package executable" which is expected - the Electron app needs to be
built separately. The Playwright + Bazel integration issue is **RESOLVED**.

### Key Learnings

1. **Always check for leftover debugging changes** - the `tests/package.json`
   with `"type": "commonjs"` was from an earlier experiment and was never
   reverted
2. **ESM vs CJS resolution differs significantly** - ESM uses `"import"`
   condition, CJS uses `"require"` condition
3. **The custom ESM loader IS still needed** - it handles Playwright's
   dual-module detection by ensuring consistent module paths
4. **No changes to workspace packages required** - the fix was entirely in the
   test configuration

## Future Alternatives

If the current loader-based approach becomes too brittle, consider these
longer-term options:

1. **Investigate dual node_modules in aspect_rules_js** - check why runfiles
   include a second node_modules tree and see if it can be avoided.
2. **Pre-compile Playwright tests** - compile TS to JS ahead of time and point
   Playwright at the compiled output.
