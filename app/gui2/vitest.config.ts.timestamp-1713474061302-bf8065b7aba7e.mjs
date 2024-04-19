var __defProp = Object.defineProperty;
var __getOwnPropNames = Object.getOwnPropertyNames;
var __esm = (fn, res) => function __init() {
  return fn && (res = (0, fn[__getOwnPropNames(fn)[0]])(fn = 0)), res;
};
var __export = (target, all) => {
  for (var name in all)
    __defProp(target, name, { get: all[name], enumerable: true });
};

// ../ide-desktop/lib/project-manager-shim/src/projectManagement.ts
import * as crypto from "node:crypto";
import * as fs from "node:fs";
import * as os from "node:os";
import * as pathModule from "node:path";
import * as tar from "tar";
import * as common from "enso-common";
import * as buildUtils from "enso-common/src/buildUtils";
async function uploadBundle(bundle, directory, name = null) {
  directory ??= getProjectsDirectory();
  logger.log(`Uploading project from bundle${name != null ? ` as '${name}'` : ""}.`);
  const targetPath = generateDirectoryName(name ?? "Project", directory);
  fs.mkdirSync(targetPath, { recursive: true });
  await new Promise((resolve2) => {
    bundle.pipe(tar.extract({ cwd: targetPath })).on("finish", resolve2);
  });
  const entries = fs.readdirSync(targetPath);
  const firstEntry = entries[0];
  if (entries.length === 1 && firstEntry != null) {
    if (fs.statSync(pathModule.join(targetPath, firstEntry)).isDirectory()) {
      const temporaryDirectoryName = targetPath + `_${crypto.randomUUID().split("-")[0] ?? ""}`;
      fs.renameSync(targetPath, temporaryDirectoryName);
      fs.renameSync(pathModule.join(temporaryDirectoryName, firstEntry), targetPath);
      fs.rmdirSync(temporaryDirectoryName);
    }
  }
  return bumpMetadata(targetPath, name ?? null);
}
function isProjectMetadata(value) {
  return typeof value === "object" && value != null && "id" in value && typeof value.id === "string";
}
function updatePackageName(projectRoot, name) {
  const path2 = pathModule.join(projectRoot, PACKAGE_METADATA_RELATIVE_PATH);
  const contents = fs.readFileSync(path2, { encoding: "utf-8" });
  const newContents = contents.replace(/^name: .*/, `name: ${name}`);
  fs.writeFileSync(path2, newContents);
}
function createMetadata() {
  return {
    id: generateId(),
    kind: "UserProject",
    created: (/* @__PURE__ */ new Date()).toISOString(),
    lastOpened: (/* @__PURE__ */ new Date()).toISOString()
  };
}
function getMetadata(projectRoot) {
  const metadataPath = pathModule.join(projectRoot, PROJECT_METADATA_RELATIVE_PATH);
  try {
    const jsonText = fs.readFileSync(metadataPath, "utf8");
    const metadata2 = JSON.parse(jsonText);
    return isProjectMetadata(metadata2) ? metadata2 : null;
  } catch {
    return null;
  }
}
function writeMetadata(projectRoot, metadata2) {
  const metadataPath = pathModule.join(projectRoot, PROJECT_METADATA_RELATIVE_PATH);
  fs.mkdirSync(pathModule.dirname(metadataPath), { recursive: true });
  fs.writeFileSync(metadataPath, JSON.stringify(metadata2, null, buildUtils.INDENT_SIZE));
}
function updateMetadata(projectRoot, updater) {
  const metadata2 = getMetadata(projectRoot);
  const updatedMetadata = updater(metadata2 ?? createMetadata());
  writeMetadata(projectRoot, updatedMetadata);
  return updatedMetadata;
}
function generateDirectoryName(name, directory = getProjectsDirectory()) {
  name = pathModule.parse(name).name;
  const matches = name.match(/^(.*)_(\d+)$/);
  const initialSuffix = -1;
  let suffix = initialSuffix;
  const [matchedName, matchedSuffix] = matches?.slice(1) ?? [];
  if (typeof matchedName !== "undefined" && typeof matchedSuffix !== "undefined") {
    name = matchedName;
    suffix = parseInt(matchedSuffix);
  }
  let finalPath;
  while (true) {
    suffix++;
    const newName = `${name}${suffix === 0 ? "" : `_${suffix}`}`;
    const candidatePath = pathModule.join(directory, newName);
    if (!fs.existsSync(candidatePath)) {
      finalPath = candidatePath;
      break;
    }
  }
  return finalPath;
}
function getProjectsDirectory() {
  return pathModule.join(os.homedir(), "enso", "projects");
}
function generateId() {
  return crypto.randomUUID();
}
function bumpMetadata(projectRoot, name) {
  if (name != null) {
    updatePackageName(projectRoot, name);
  }
  return updateMetadata(projectRoot, (metadata2) => ({
    ...metadata2,
    id: generateId(),
    lastOpened: (/* @__PURE__ */ new Date()).toISOString()
  })).id;
}
var logger, PACKAGE_METADATA_RELATIVE_PATH, PROJECT_METADATA_RELATIVE_PATH;
var init_projectManagement = __esm({
  "../ide-desktop/lib/project-manager-shim/src/projectManagement.ts"() {
    "use strict";
    logger = console;
    PACKAGE_METADATA_RELATIVE_PATH = "package.yaml";
    PROJECT_METADATA_RELATIVE_PATH = ".enso/project.json";
  }
});

// ../ide-desktop/lib/project-manager-shim/src/projectManagerShimMiddleware.ts
var projectManagerShimMiddleware_exports = {};
__export(projectManagerShimMiddleware_exports, {
  FileSystemEntryType: () => FileSystemEntryType,
  default: () => projectManagerShimMiddleware
});
import * as fs2 from "node:fs/promises";
import * as fsSync from "node:fs";
import * as os2 from "node:os";
import * as path from "node:path";
import * as tar2 from "tar";
import * as yaml from "file:///C:/Projects/enso/enso/node_modules/.pnpm/yaml@2.4.1/node_modules/yaml/dist/index.js";
import * as common2 from "enso-common";
function projectManagerShimMiddleware(request, response, next) {
  const requestUrl = request.url;
  const requestPath = requestUrl?.split("?")[0]?.split("#")[0];
  if (request.method === "POST") {
    switch (requestPath) {
      case "/api/upload-file": {
        const url = new URL(`https://example.com/${requestUrl}`);
        const fileName = url.searchParams.get("file_name");
        const directory = url.searchParams.get("directory") ?? PROJECTS_ROOT_DIRECTORY;
        if (fileName == null) {
          response.writeHead(HTTP_STATUS_BAD_REQUEST, common2.COOP_COEP_CORP_HEADERS).end("Request is missing search parameter `file_name`.");
        } else {
          const filePath = path.join(directory, fileName);
          void fs2.writeFile(filePath, request).then(() => {
            response.writeHead(HTTP_STATUS_OK, [
              ["Content-Length", String(filePath.length)],
              ["Content-Type", "text/plain"],
              ...common2.COOP_COEP_CORP_HEADERS
            ]).end(filePath);
          }).catch((e) => {
            console.error(e);
            response.writeHead(HTTP_STATUS_BAD_REQUEST, common2.COOP_COEP_CORP_HEADERS).end();
          });
        }
        break;
      }
      case "/api/upload-project": {
        const url = new URL(`https://example.com/${requestUrl}`);
        const directory = url.searchParams.get("directory");
        const name = url.searchParams.get("name");
        void uploadBundle(request, directory, name).then((id) => {
          response.writeHead(HTTP_STATUS_OK, [
            ["Content-Length", String(id.length)],
            ["Content-Type", "text/plain"],
            ...common2.COOP_COEP_CORP_HEADERS
          ]).end(id);
        }).catch(() => {
          response.writeHead(HTTP_STATUS_BAD_REQUEST, common2.COOP_COEP_CORP_HEADERS).end();
        });
        break;
      }
      case "/api/run-project-manager-command": {
        const cliArguments = JSON.parse(
          new URL(`https://example.com/${requestUrl}`).searchParams.get(
            "cli-arguments"
          ) ?? "[]"
        );
        if (!Array.isArray(cliArguments) || !cliArguments.every((item) => typeof item === "string")) {
          response.writeHead(HTTP_STATUS_BAD_REQUEST, common2.COOP_COEP_CORP_HEADERS).end("Command arguments must be an array of strings.");
        } else {
          void (async () => {
            const toJSONRPCResult = (result2) => JSON.stringify({ jsonrpc: "2.0", id: 0, result: result2 });
            const toJSONRPCError = (message, data) => JSON.stringify({
              jsonrpc: "2.0",
              id: 0,
              error: { code: 0, message, ...data != null ? { data } : {} }
            });
            let result = toJSONRPCError(`Error running Project Manager command.`, {
              command: cliArguments
            });
            try {
              switch (cliArguments[0]) {
                case "--filesystem-list": {
                  const directoryPath = cliArguments[1];
                  if (directoryPath != null) {
                    const entryNames = await fs2.readdir(directoryPath);
                    const entries = [];
                    for (const entryName of entryNames) {
                      const entryPath = path.join(directoryPath, entryName);
                      const stat2 = await fs2.stat(entryPath);
                      const attributes = {
                        byteSize: stat2.size,
                        creationTime: new Date(stat2.ctimeMs).toISOString(),
                        lastAccessTime: new Date(
                          stat2.atimeMs
                        ).toISOString(),
                        lastModifiedTime: new Date(
                          stat2.mtimeMs
                        ).toISOString()
                      };
                      if (stat2.isFile()) {
                        entries.push({
                          type: "FileEntry" /* FileEntry */,
                          path: entryPath,
                          attributes
                        });
                      } else {
                        try {
                          const packageMetadataPath = path.join(
                            entryPath,
                            "package.yaml"
                          );
                          const projectMetadataPath = path.join(
                            entryPath,
                            PROJECT_METADATA_RELATIVE_PATH
                          );
                          const packageMetadataContents = await fs2.readFile(packageMetadataPath);
                          const projectMetadataContents = await fs2.readFile(projectMetadataPath);
                          const metadata2 = extractProjectMetadata(
                            yaml.parse(
                              packageMetadataContents.toString()
                            ),
                            JSON.parse(
                              projectMetadataContents.toString()
                            )
                          );
                          if (metadata2 != null) {
                            entries.push({
                              type: "ProjectEntry" /* ProjectEntry */,
                              path: entryPath,
                              attributes,
                              metadata: metadata2
                            });
                          } else {
                            throw new Error("Invalid project metadata.");
                          }
                        } catch {
                          entries.push({
                            type: "DirectoryEntry" /* DirectoryEntry */,
                            path: entryPath,
                            attributes
                          });
                        }
                      }
                    }
                    result = toJSONRPCResult({ entries });
                  }
                  break;
                }
                case "--filesystem-create-directory": {
                  const directoryPath = cliArguments[1];
                  if (directoryPath != null) {
                    await fs2.mkdir(directoryPath, { recursive: true });
                    result = toJSONRPCResult(null);
                  }
                  break;
                }
                case "--filesystem-write-path": {
                  const filePath = cliArguments[1];
                  if (filePath != null) {
                    await new Promise((resolve2, reject) => {
                      request.pipe(fsSync.createWriteStream(filePath), {
                        end: true
                      }).on("close", resolve2).on("error", reject);
                    });
                    result = toJSONRPCResult(null);
                  }
                  break;
                }
                case "--filesystem-move-from": {
                  const sourcePath = cliArguments[1];
                  const destinationPath = cliArguments[3];
                  if (sourcePath != null && cliArguments[2] === "--filesystem-move-to" && destinationPath != null) {
                    await fs2.rename(sourcePath, destinationPath);
                    result = toJSONRPCResult(null);
                  }
                  break;
                }
                case "--filesystem-delete": {
                  const fileOrDirectoryPath = cliArguments[1];
                  if (fileOrDirectoryPath != null) {
                    await fs2.rm(fileOrDirectoryPath, { recursive: true });
                    result = toJSONRPCResult(null);
                  }
                  break;
                }
                default: {
                }
              }
            } catch {
            }
            const buffer = Buffer.from(result);
            response.writeHead(HTTP_STATUS_OK, [
              ["Content-Length", String(buffer.byteLength)],
              ["Content-Type", "application/json"],
              ...common2.COOP_COEP_CORP_HEADERS
            ]).end(buffer);
          })();
        }
        break;
      }
      default: {
        const downloadProjectMatch = requestPath?.match(
          /^[/]api[/]project-manager[/]projects[/]([^/]+)[/]enso-project$/
        );
        if (downloadProjectMatch) {
          const uuid = downloadProjectMatch[1];
          void fs2.readdir(PROJECTS_ROOT_DIRECTORY).then(async (filenames) => {
            let success = false;
            for (const filename of filenames) {
              try {
                const projectRoot = path.join(PROJECTS_ROOT_DIRECTORY, filename);
                const stat2 = await fs2.stat(projectRoot);
                if (stat2.isDirectory()) {
                  const metadataPath = path.join(
                    projectRoot,
                    PROJECT_METADATA_RELATIVE_PATH
                  );
                  const metadataContents = await fs2.readFile(metadataPath);
                  const metadata2 = JSON.parse(
                    metadataContents.toString()
                  );
                  if (typeof metadata2 === "object" && metadata2 != null && "id" in metadata2 && metadata2.id === uuid) {
                    response.writeHead(HTTP_STATUS_OK, [
                      ["Content-Type", "application/gzip+x-enso-project"],
                      ...common2.COOP_COEP_CORP_HEADERS
                    ]);
                    tar2.create({ gzip: true, cwd: projectRoot }, [
                      projectRoot
                    ]).pipe(response, { end: true });
                    success = true;
                    break;
                  }
                }
              } catch {
              }
            }
            if (!success) {
              response.writeHead(HTTP_STATUS_NOT_FOUND, common2.COOP_COEP_CORP_HEADERS).end();
            }
          });
          break;
        }
        response.writeHead(HTTP_STATUS_NOT_FOUND, common2.COOP_COEP_CORP_HEADERS).end();
        break;
      }
    }
  } else if (request.method === "GET" && requestPath === "/api/root-directory") {
    response.writeHead(HTTP_STATUS_OK, [
      ["Content-Length", String(PROJECTS_ROOT_DIRECTORY.length)],
      ["Content-Type", "text/plain"],
      ...common2.COOP_COEP_CORP_HEADERS
    ]).end(PROJECTS_ROOT_DIRECTORY);
  } else {
    next();
  }
}
function extractProjectMetadata(yamlObj, jsonObj) {
  if (typeof yamlObj !== "object" || yamlObj == null || typeof jsonObj !== "object" || jsonObj == null) {
    return null;
  } else {
    const validDateString = (string) => {
      const date = new Date(string);
      return !Number.isNaN(Number(date)) ? date.toString() : null;
    };
    const name = "name" in yamlObj && typeof yamlObj.name === "string" ? yamlObj.name : null;
    const namespace = "namespace" in yamlObj && typeof yamlObj.namespace === "string" ? yamlObj.namespace : null;
    const engineVersion = "edition" in yamlObj && typeof yamlObj.edition === "string" ? yamlObj.edition : null;
    const id = "id" in jsonObj && typeof jsonObj.id === "string" ? jsonObj.id : null;
    const created = "created" in jsonObj && typeof jsonObj.created === "string" ? validDateString(jsonObj.created) : null;
    const lastOpened = "lastOpened" in jsonObj && typeof jsonObj.lastOpened === "string" ? validDateString(jsonObj.lastOpened) : null;
    if (name != null && namespace != null && id != null && created != null) {
      return {
        name,
        namespace,
        id,
        ...engineVersion != null ? { engineVersion } : {},
        created,
        ...lastOpened != null ? { lastOpened } : {}
      };
    } else {
      return null;
    }
  }
}
var HTTP_STATUS_OK, HTTP_STATUS_BAD_REQUEST, HTTP_STATUS_NOT_FOUND, PROJECTS_ROOT_DIRECTORY, FileSystemEntryType;
var init_projectManagerShimMiddleware = __esm({
  "../ide-desktop/lib/project-manager-shim/src/projectManagerShimMiddleware.ts"() {
    "use strict";
    init_projectManagement();
    HTTP_STATUS_OK = 200;
    HTTP_STATUS_BAD_REQUEST = 400;
    HTTP_STATUS_NOT_FOUND = 404;
    PROJECTS_ROOT_DIRECTORY = path.join(os2.homedir(), "enso/projects");
    FileSystemEntryType = /* @__PURE__ */ ((FileSystemEntryType2) => {
      FileSystemEntryType2["DirectoryEntry"] = "DirectoryEntry";
      FileSystemEntryType2["ProjectEntry"] = "ProjectEntry";
      FileSystemEntryType2["FileEntry"] = "FileEntry";
      return FileSystemEntryType2;
    })(FileSystemEntryType || {});
  }
});

// vitest.config.ts
import { fileURLToPath as fileURLToPath2 } from "node:url";
import { configDefaults, defineConfig as defineConfig2, mergeConfig } from "file:///C:/Projects/enso/enso/node_modules/.pnpm/vitest@1.5.0_@types+node@20.12.7_jsdom@22.1.0/node_modules/vitest/dist/config.js";

// vite.config.ts
import vue from "file:///C:/Projects/enso/enso/node_modules/.pnpm/@vitejs+plugin-vue@5.0.4_vite@5.2.9_vue@3.4.23/node_modules/@vitejs/plugin-vue/dist/index.mjs";
import { getDefines, readEnvironmentFromFile } from "file:///C:/Projects/enso/enso/app/ide-desktop/lib/common/src/appConfig.js";
import { fileURLToPath } from "node:url";
import postcssNesting from "file:///C:/Projects/enso/enso/node_modules/.pnpm/postcss-nesting@12.1.1_postcss@8.4.38/node_modules/postcss-nesting/dist/index.mjs";
import tailwindcss from "file:///C:/Projects/enso/enso/node_modules/.pnpm/tailwindcss@3.4.3/node_modules/tailwindcss/lib/index.js";
import tailwindcssNesting from "file:///C:/Projects/enso/enso/node_modules/.pnpm/tailwindcss@3.4.3/node_modules/tailwindcss/nesting/index.js";
import { defineConfig } from "file:///C:/Projects/enso/enso/node_modules/.pnpm/vite@5.2.9_@types+node@20.12.7/node_modules/vite/dist/node/index.js";
import * as tailwindConfig from "file:///C:/Projects/enso/enso/app/ide-desktop/lib/dashboard/tailwind.config.js";

// ydoc-server/index.ts
import { parse as parse4 } from "url";
import { WebSocketServer } from "file:///C:/Projects/enso/enso/node_modules/.pnpm/ws@8.16.0/node_modules/ws/wrapper.mjs";

// shared/ast/ffi.ts
import { createXXHash128 } from "file:///C:/Projects/enso/enso/node_modules/.pnpm/hash-wasm@4.11.0/node_modules/hash-wasm/dist/index.umd.js";

// rust-ffi/pkg/rust_ffi.js
var __vite_injected_original_import_meta_url = "file:///C:/Projects/enso/enso/app/gui2/rust-ffi/pkg/rust_ffi.js";
var wasm;
var heap = new Array(128).fill(void 0);
heap.push(void 0, null, true, false);
function getObject(idx) {
  return heap[idx];
}
var heap_next = heap.length;
function dropObject(idx) {
  if (idx < 132)
    return;
  heap[idx] = heap_next;
  heap_next = idx;
}
function takeObject(idx) {
  const ret = getObject(idx);
  dropObject(idx);
  return ret;
}
var WASM_VECTOR_LEN = 0;
var cachedUint8Memory0 = null;
function getUint8Memory0() {
  if (cachedUint8Memory0 === null || cachedUint8Memory0.byteLength === 0) {
    cachedUint8Memory0 = new Uint8Array(wasm.memory.buffer);
  }
  return cachedUint8Memory0;
}
var cachedTextEncoder = typeof TextEncoder !== "undefined" ? new TextEncoder("utf-8") : { encode: () => {
  throw Error("TextEncoder not available");
} };
var encodeString = typeof cachedTextEncoder.encodeInto === "function" ? function(arg, view) {
  return cachedTextEncoder.encodeInto(arg, view);
} : function(arg, view) {
  const buf = cachedTextEncoder.encode(arg);
  view.set(buf);
  return {
    read: arg.length,
    written: buf.length
  };
};
function passStringToWasm0(arg, malloc, realloc) {
  if (realloc === void 0) {
    const buf = cachedTextEncoder.encode(arg);
    const ptr2 = malloc(buf.length, 1) >>> 0;
    getUint8Memory0().subarray(ptr2, ptr2 + buf.length).set(buf);
    WASM_VECTOR_LEN = buf.length;
    return ptr2;
  }
  let len = arg.length;
  let ptr = malloc(len, 1) >>> 0;
  const mem = getUint8Memory0();
  let offset = 0;
  for (; offset < len; offset++) {
    const code = arg.charCodeAt(offset);
    if (code > 127)
      break;
    mem[ptr + offset] = code;
  }
  if (offset !== len) {
    if (offset !== 0) {
      arg = arg.slice(offset);
    }
    ptr = realloc(ptr, len, len = offset + arg.length * 3, 1) >>> 0;
    const view = getUint8Memory0().subarray(ptr + offset, ptr + len);
    const ret = encodeString(arg, view);
    offset += ret.written;
    ptr = realloc(ptr, len, offset, 1) >>> 0;
  }
  WASM_VECTOR_LEN = offset;
  return ptr;
}
var cachedInt32Memory0 = null;
function getInt32Memory0() {
  if (cachedInt32Memory0 === null || cachedInt32Memory0.byteLength === 0) {
    cachedInt32Memory0 = new Int32Array(wasm.memory.buffer);
  }
  return cachedInt32Memory0;
}
var cachedTextDecoder = typeof TextDecoder !== "undefined" ? new TextDecoder("utf-8", { ignoreBOM: true, fatal: true }) : { decode: () => {
  throw Error("TextDecoder not available");
} };
if (typeof TextDecoder !== "undefined") {
  cachedTextDecoder.decode();
}
function getStringFromWasm0(ptr, len) {
  ptr = ptr >>> 0;
  return cachedTextDecoder.decode(getUint8Memory0().subarray(ptr, ptr + len));
}
function getArrayU8FromWasm0(ptr, len) {
  ptr = ptr >>> 0;
  return getUint8Memory0().subarray(ptr / 1, ptr / 1 + len);
}
function parse(code) {
  try {
    const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
    const ptr0 = passStringToWasm0(code, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    wasm.parse(retptr, ptr0, len0);
    var r0 = getInt32Memory0()[retptr / 4 + 0];
    var r1 = getInt32Memory0()[retptr / 4 + 1];
    var v2 = getArrayU8FromWasm0(r0, r1).slice();
    wasm.__wbindgen_free(r0, r1 * 1, 1);
    return v2;
  } finally {
    wasm.__wbindgen_add_to_stack_pointer(16);
  }
}
function is_ident_or_operator(code) {
  const ptr0 = passStringToWasm0(code, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
  const len0 = WASM_VECTOR_LEN;
  const ret = wasm.is_ident_or_operator(ptr0, len0);
  return ret >>> 0;
}
function addHeapObject(obj) {
  if (heap_next === heap.length)
    heap.push(heap.length + 1);
  const idx = heap_next;
  heap_next = heap[idx];
  heap[idx] = obj;
  return idx;
}
async function __wbg_load(module, imports) {
  if (typeof Response === "function" && module instanceof Response) {
    if (typeof WebAssembly.instantiateStreaming === "function") {
      try {
        return await WebAssembly.instantiateStreaming(module, imports);
      } catch (e) {
        if (module.headers.get("Content-Type") != "application/wasm") {
          console.warn("`WebAssembly.instantiateStreaming` failed because your server does not serve wasm with `application/wasm` MIME type. Falling back to `WebAssembly.instantiate` which is slower. Original error:\n", e);
        } else {
          throw e;
        }
      }
    }
    const bytes = await module.arrayBuffer();
    return await WebAssembly.instantiate(bytes, imports);
  } else {
    const instance = await WebAssembly.instantiate(module, imports);
    if (instance instanceof WebAssembly.Instance) {
      return { instance, module };
    } else {
      return instance;
    }
  }
}
function __wbg_get_imports() {
  const imports = {};
  imports.wbg = {};
  imports.wbg.__wbg_error_f851667af71bcfc6 = function(arg0, arg1) {
    let deferred0_0;
    let deferred0_1;
    try {
      deferred0_0 = arg0;
      deferred0_1 = arg1;
      console.error(getStringFromWasm0(arg0, arg1));
    } finally {
      wasm.__wbindgen_free(deferred0_0, deferred0_1, 1);
    }
  };
  imports.wbg.__wbg_new_abda76e883ba8a5f = function() {
    const ret = new Error();
    return addHeapObject(ret);
  };
  imports.wbg.__wbg_stack_658279fe44541cf6 = function(arg0, arg1) {
    const ret = getObject(arg1).stack;
    const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    getInt32Memory0()[arg0 / 4 + 1] = len1;
    getInt32Memory0()[arg0 / 4 + 0] = ptr1;
  };
  imports.wbg.__wbindgen_object_drop_ref = function(arg0) {
    takeObject(arg0);
  };
  return imports;
}
function __wbg_init_memory(imports, maybe_memory) {
}
function __wbg_finalize_init(instance, module) {
  wasm = instance.exports;
  __wbg_init.__wbindgen_wasm_module = module;
  cachedInt32Memory0 = null;
  cachedUint8Memory0 = null;
  wasm.__wbindgen_start();
  return wasm;
}
async function __wbg_init(input) {
  if (wasm !== void 0)
    return wasm;
  if (typeof input === "undefined") {
    input = new URL("rust_ffi_bg.wasm", __vite_injected_original_import_meta_url);
  }
  const imports = __wbg_get_imports();
  if (typeof input === "string" || typeof Request === "function" && input instanceof Request || typeof URL === "function" && input instanceof URL) {
    input = fetch(input);
  }
  __wbg_init_memory(imports);
  const { instance, module } = await __wbg_load(await input, imports);
  return __wbg_finalize_init(instance, module);
}
var rust_ffi_default = __wbg_init;

// shared/util/assert.ts
function assert(condition, message) {
  if (!condition)
    bail(message ? `Assertion failed: ${message}` : "Assertion failed");
}
function assertEqual(actual, expected, message) {
  const messagePrefix = message ? message + " " : "";
  assert(actual === expected, `${messagePrefix}Expected ${expected}, got ${actual}.`);
}
function assertDefined(x, message) {
  const messagePrefix = message ? message + " " : "";
  assert(x !== void 0, `${messagePrefix}Expected value to be defined.`);
}
function assertUnreachable() {
  bail("Unreachable code");
}
function bail(message) {
  throw new Error(message);
}

// shared/util/detect.ts
var isNode = typeof global !== "undefined" && global[Symbol.toStringTag] === "global";
var isDevMode = process.env.NODE_ENV === "development";

// shared/ast/ffi.ts
var __vite_injected_original_import_meta_url2 = "file:///C:/Projects/enso/enso/app/gui2/shared/ast/ffi.ts";
var xxHasher128;
function xxHash128(input) {
  assertDefined(xxHasher128, "Module should have been loaded with `initializeFFI`.");
  xxHasher128.init();
  xxHasher128.update(input);
  return xxHasher128.digest();
}
async function initializeFFI(path2) {
  if (isNode) {
    const fs3 = await import("node:fs/promises");
    const { fileURLToPath: fileURLToPath3, URL: nodeURL } = await import("node:url");
    const buffer = fs3.readFile(
      path2 ?? fileURLToPath3(new nodeURL("../../rust-ffi/pkg/rust_ffi_bg.wasm", __vite_injected_original_import_meta_url2))
    );
    await rust_ffi_default(buffer);
  } else {
    await rust_ffi_default();
  }
  xxHasher128 = await createXXHash128();
}

// ydoc-server/ydoc.ts
import {
  applyAwarenessUpdate,
  Awareness,
  encodeAwarenessUpdate,
  removeAwarenessStates
} from "file:///C:/Projects/enso/enso/node_modules/.pnpm/y-protocols@1.0.6_yjs@13.6.14/node_modules/y-protocols/awareness.js";
import { readSyncMessage, writeSyncStep1, writeUpdate } from "file:///C:/Projects/enso/enso/node_modules/.pnpm/y-protocols@1.0.6_yjs@13.6.14/node_modules/y-protocols/sync.js";
import * as Y4 from "file:///C:/Projects/enso/enso/node_modules/.pnpm/yjs@13.6.14/node_modules/yjs/dist/yjs.mjs";
import * as decoding from "file:///C:/Projects/enso/enso/node_modules/.pnpm/lib0@0.2.93/node_modules/lib0/decoding.js";
import * as encoding from "file:///C:/Projects/enso/enso/node_modules/.pnpm/lib0@0.2.93/node_modules/lib0/encoding.js";
import { ObservableV2 as ObservableV23 } from "file:///C:/Projects/enso/enso/node_modules/.pnpm/lib0@0.2.93/node_modules/lib0/observable.js";
import { WebSocket } from "file:///C:/Projects/enso/enso/node_modules/.pnpm/ws@8.16.0/node_modules/ws/wrapper.mjs";

// ydoc-server/languageServerSession.ts
import { Client, RequestManager, WebSocketTransport } from "file:///C:/Projects/enso/enso/node_modules/.pnpm/@open-rpc+client-js@1.8.1/node_modules/@open-rpc/client-js/build/index.js";
import * as json3 from "file:///C:/Projects/enso/enso/node_modules/.pnpm/lib0@0.2.93/node_modules/lib0/json.js";
import * as map2 from "file:///C:/Projects/enso/enso/node_modules/.pnpm/lib0@0.2.93/node_modules/lib0/map.js";
import { ObservableV2 as ObservableV22 } from "file:///C:/Projects/enso/enso/node_modules/.pnpm/lib0@0.2.93/node_modules/lib0/observable.js";
import * as random4 from "file:///C:/Projects/enso/enso/node_modules/.pnpm/lib0@0.2.93/node_modules/lib0/random.js";
import * as Y3 from "file:///C:/Projects/enso/enso/node_modules/.pnpm/yjs@13.6.14/node_modules/yjs/dist/yjs.mjs";

// shared/ast/index.ts
import * as random3 from "file:///C:/Projects/enso/enso/node_modules/.pnpm/lib0@0.2.93/node_modules/lib0/random.js";

// shared/util/data/result.ts
function Ok(data) {
  return { ok: true, value: data };
}
function Err(error) {
  return { ok: false, error: new ResultError(error) };
}
var ResultError = class {
  payload;
  context;
  constructor(payload) {
    this.payload = payload;
    this.context = [];
  }
  log(preamble = "Error") {
    console.error(this.message(preamble));
  }
  message(preamble = "error") {
    const ctx = this.context.length > 0 ? `
${Array.from(this.context, (ctx2) => ctx2()).join("\n")}` : "";
    return `${preamble}: ${this.payload}${ctx}`;
  }
};

// shared/yjsModel.ts
import * as object from "file:///C:/Projects/enso/enso/node_modules/.pnpm/lib0@0.2.93/node_modules/lib0/object.js";
import * as random from "file:///C:/Projects/enso/enso/node_modules/.pnpm/lib0@0.2.93/node_modules/lib0/random.js";
import * as Y from "file:///C:/Projects/enso/enso/node_modules/.pnpm/yjs@13.6.14/node_modules/yjs/dist/yjs.mjs";
function visMetadataEquals(a, b) {
  return !a && !b || a && b && a.visible === b.visible && a.fullscreen == b.fullscreen && a.width == b.width && visIdentifierEquals(a.identifier, b.identifier);
}
function visIdentifierEquals(a, b) {
  return !a && !b || a && b && a.name === b.name && object.equalFlat(a.module, b.module);
}
var DistributedProject = class {
  doc;
  name;
  modules;
  settings;
  constructor(doc) {
    this.doc = doc;
    this.name = this.doc.getText("name");
    this.modules = this.doc.getMap("modules");
    this.settings = this.doc.getMap("settings");
  }
  moduleNames() {
    return Array.from(this.modules.keys());
  }
  findModuleByDocId(id) {
    for (const [name, doc] of this.modules.entries()) {
      if (doc.guid === id)
        return name;
    }
    return null;
  }
  async openModule(name) {
    const doc = this.modules.get(name);
    if (doc == null)
      return null;
    return await DistributedModule.load(doc);
  }
  openUnloadedModule(name) {
    const doc = this.modules.get(name);
    if (doc == null)
      return null;
    return new DistributedModule(doc);
  }
  createUnloadedModule(name, doc) {
    this.modules.set(name, doc);
    return new DistributedModule(doc);
  }
  createNewModule(name) {
    return this.createUnloadedModule(name, new Y.Doc());
  }
  deleteModule(name) {
    this.modules.delete(name);
  }
  dispose() {
    this.doc.destroy();
  }
};
var ModuleDoc = class {
  ydoc;
  nodes;
  constructor(ydoc) {
    this.ydoc = ydoc;
    this.nodes = ydoc.getMap("nodes");
  }
};
var DistributedModule = class _DistributedModule {
  doc;
  undoManager;
  static async load(ydoc) {
    ydoc.load();
    await ydoc.whenLoaded;
    return new _DistributedModule(ydoc);
  }
  constructor(ydoc) {
    this.doc = new ModuleDoc(ydoc);
    this.undoManager = new Y.UndoManager([this.doc.nodes]);
  }
  dispose() {
    this.doc.ydoc.destroy();
  }
};
var localUserActionOrigins = ["local:userAction", "local:userAction:CodeEditor"];
var defaultLocalOrigin = "local:userAction";
function isLocalUserActionOrigin(origin) {
  const localOriginNames = localUserActionOrigins;
  return localOriginNames.includes(origin);
}
function tryAsOrigin(origin) {
  if (isLocalUserActionOrigin(origin))
    return origin;
  if (origin === "local:autoLayout")
    return origin;
  if (origin === "remote")
    return origin;
}
function sourceRangeKey(range) {
  return `${range[0].toString(16)}:${range[1].toString(16)}`;
}
function sourceRangeFromKey(key) {
  return key.split(":").map((x) => parseInt(x, 16));
}
var IdMap = class _IdMap {
  rangeToExpr;
  constructor(entries) {
    this.rangeToExpr = new Map(entries ?? []);
  }
  static Mock() {
    return new _IdMap([]);
  }
  insertKnownId(range, id) {
    const key = sourceRangeKey(range);
    this.rangeToExpr.set(key, id);
  }
  getIfExist(range) {
    const key = sourceRangeKey(range);
    return this.rangeToExpr.get(key);
  }
  getOrInsertUniqueId(range) {
    const key = sourceRangeKey(range);
    const val = this.rangeToExpr.get(key);
    if (val !== void 0) {
      return val;
    } else {
      const newId = random.uuidv4();
      this.rangeToExpr.set(key, newId);
      return newId;
    }
  }
  entries() {
    return [...this.rangeToExpr];
  }
  get size() {
    return this.rangeToExpr.size;
  }
  clear() {
    this.rangeToExpr.clear();
  }
  isEqual(other) {
    if (other.size !== this.size)
      return false;
    for (const [key, value] of this.rangeToExpr.entries()) {
      const oldValue = other.rangeToExpr.get(key);
      if (oldValue !== value)
        return false;
    }
    return true;
  }
  validate() {
    const uniqueValues = new Set(this.rangeToExpr.values());
    if (uniqueValues.size < this.rangeToExpr.size) {
      console.warn(`Duplicate UUID in IdMap`);
    }
  }
  clone() {
    return new _IdMap(this.entries());
  }
  // Debugging.
  compare(other) {
    console.info(`IdMap.compare -------`);
    const allKeys2 = /* @__PURE__ */ new Set();
    for (const key of this.rangeToExpr.keys())
      allKeys2.add(key);
    for (const key of other.rangeToExpr.keys())
      allKeys2.add(key);
    for (const key of allKeys2) {
      const mine = this.rangeToExpr.get(key);
      const yours = other.rangeToExpr.get(key);
      if (mine !== yours) {
        console.info(`IdMap.compare[${key}]: ${mine} -> ${yours}`);
      }
    }
  }
};
var uuidRegex = /^[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}$/;
function isUuid(x) {
  return typeof x === "string" && x.length === 36 && uuidRegex.test(x);
}
function rangeLength(a) {
  return a[1] - a[0];
}
function rangeEncloses(a, b) {
  return a[0] <= b[0] && a[1] >= b[1];
}

// shared/ast/parserSupport.ts
var LazyObject = class {
  _v;
  constructor(view) {
    if (view == null)
      throw new Error("WTF?");
    this._v = view;
  }
  visitChildren(_visitor) {
    return false;
  }
  children() {
    const children = [];
    this.visitChildren((child) => {
      children.push(child);
    });
    return children;
  }
};
function makeDataView(buffer, address) {
  return new DataView(buffer, address);
}
function readU8(view, address) {
  return view.getUint8(address);
}
function readU32(view, address) {
  return view.getUint32(address, true);
}
function readI32(view, address) {
  return view.getInt32(address, true);
}
function readBool(view, address) {
  return readU8(view, address) !== 0;
}
function readOffset(view, offset) {
  return makeDataView(view.buffer, view.byteOffset + offset);
}
function readPointer(view, address) {
  return makeDataView(view.buffer, readU32(view, address));
}
var textDecoder = new TextDecoder();
function readOption(view, address, readElement) {
  let result = void 0;
  visitOption(view, address, (view2, address2) => {
    result = readElement(view2, address2);
  });
  return result;
}
function visitOption(view, address, visitor) {
  const discriminant = readU8(view, address);
  switch (discriminant) {
    case 0:
      return false;
    case 1:
      return !!visitor(readPointer(view, address + 1), 0);
    default:
      throw new Error(`Invalid Option discriminant: 0x${discriminant.toString(16)}.`);
  }
}
function readResult(view, address, readOk, readErr) {
  const data = readPointer(view, address);
  const discriminant = readU32(data, 0);
  switch (discriminant) {
    case 0:
      return Ok(readOk(data, 4));
    case 1:
      return Err(readErr(data, 4));
    default:
      throw new Error(`Invalid Result discriminant: 0x${discriminant.toString(16)}.`);
  }
}
function visitResult(view, address, visitOk, visitErr) {
  const data = readPointer(view, address);
  const discriminant = readU32(data, 0);
  switch (discriminant) {
    case 0:
      if (visitOk?.(data, 4))
        return true;
      return false;
    case 1:
      if (visitErr?.(data, 4))
        return true;
      return false;
    default:
      throw new Error(`Invalid Result discriminant: 0x${discriminant.toString(16)}.`);
  }
}
function visitSequence(view, address, size, visitor) {
  const data = readPointer(view, address);
  let offset = 4;
  const end = offset + size * readU32(data, 0);
  while (offset != end) {
    if (visitor(data, offset) === true)
      return true;
    offset += size;
  }
  return false;
}
function readSequence(view, address, size, reader) {
  const data = readPointer(view, address);
  const offset = 4;
  const end = offset + size * readU32(data, 0);
  return new LazySequence(offset, size, end, (offset2) => reader(data, offset2));
}
var LazySequence = class {
  offset;
  step;
  end;
  read;
  constructor(offset, step, end, read) {
    this.read = read;
    this.offset = offset;
    this.step = step;
    this.end = end;
  }
  [Symbol.iterator]() {
    return this;
  }
  next() {
    if (this.offset >= this.end) {
      return { done: true, value: void 0 };
    }
    const value = this.read(this.offset);
    this.offset += this.step;
    return { done: false, value };
  }
};
function readString(view, address) {
  const data = readPointer(view, address);
  const len = readU32(data, 0);
  const bytes = new Uint8Array(data.buffer, data.byteOffset + 4, len);
  return textDecoder.decode(bytes);
}
function readEnum(readers, view, address) {
  const data = readPointer(view, address);
  const discriminant = readU32(data, 0);
  const reader = readers[discriminant] ?? bail(`Invalid enum discriminant: ${discriminant}`);
  return reader(data, 4);
}

// shared/ast/generated/ast.ts
var Tree;
((Tree2) => {
  class AbstractBase extends LazyObject {
    constructor(view) {
      super(view);
    }
    get spanLeftOffsetVisible() {
      return readU32(this._v, 0);
    }
    get spanLeftOffsetCodeReprBegin() {
      return readU32(this._v, 4);
    }
    get spanLeftOffsetCodeReprLen() {
      return readU32(this._v, 8);
    }
    get spanLeftOffsetCodeStartUtf8() {
      return readU32(this._v, 12);
    }
    get whitespaceStartInCodeParsed() {
      return readU32(this._v, 16);
    }
    get spanLeftOffsetCodeStartLine() {
      return readU32(this._v, 20);
    }
    get spanLeftOffsetCodeStartCol16() {
      return readU32(this._v, 24);
    }
    get whitespaceLengthInCodeParsed() {
      return readU32(this._v, 28);
    }
    get spanLeftOffsetCodeLenNewlines() {
      return readU32(this._v, 32);
    }
    get spanLeftOffsetCodeLenLineChars16() {
      return readU32(this._v, 36);
    }
    get childrenLengthInCodeParsed() {
      return readU32(this._v, 40);
    }
    get spanCodeLengthNewlines() {
      return readU32(this._v, 44);
    }
    get spanCodeLengthLineChars16() {
      return readU32(this._v, 48);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Tree2.AbstractBase = AbstractBase;
  let Type;
  ((Type2) => {
    Type2[Type2["Invalid"] = 0] = "Invalid";
    Type2[Type2["BodyBlock"] = 1] = "BodyBlock";
    Type2[Type2["ArgumentBlockApplication"] = 2] = "ArgumentBlockApplication";
    Type2[Type2["OperatorBlockApplication"] = 3] = "OperatorBlockApplication";
    Type2[Type2["Ident"] = 4] = "Ident";
    Type2[Type2["Private"] = 5] = "Private";
    Type2[Type2["Number"] = 6] = "Number";
    Type2[Type2["Wildcard"] = 7] = "Wildcard";
    Type2[Type2["SuspendedDefaultArguments"] = 8] = "SuspendedDefaultArguments";
    Type2[Type2["TextLiteral"] = 9] = "TextLiteral";
    Type2[Type2["App"] = 10] = "App";
    Type2[Type2["NamedApp"] = 11] = "NamedApp";
    Type2[Type2["OprApp"] = 12] = "OprApp";
    Type2[Type2["UnaryOprApp"] = 13] = "UnaryOprApp";
    Type2[Type2["AutoscopedIdentifier"] = 14] = "AutoscopedIdentifier";
    Type2[Type2["OprSectionBoundary"] = 15] = "OprSectionBoundary";
    Type2[Type2["TemplateFunction"] = 16] = "TemplateFunction";
    Type2[Type2["MultiSegmentApp"] = 17] = "MultiSegmentApp";
    Type2[Type2["TypeDef"] = 18] = "TypeDef";
    Type2[Type2["Assignment"] = 19] = "Assignment";
    Type2[Type2["Function"] = 20] = "Function";
    Type2[Type2["ForeignFunction"] = 21] = "ForeignFunction";
    Type2[Type2["Import"] = 22] = "Import";
    Type2[Type2["Export"] = 23] = "Export";
    Type2[Type2["Group"] = 24] = "Group";
    Type2[Type2["TypeSignature"] = 25] = "TypeSignature";
    Type2[Type2["TypeAnnotated"] = 26] = "TypeAnnotated";
    Type2[Type2["CaseOf"] = 27] = "CaseOf";
    Type2[Type2["Lambda"] = 28] = "Lambda";
    Type2[Type2["Array"] = 29] = "Array";
    Type2[Type2["Tuple"] = 30] = "Tuple";
    Type2[Type2["Annotated"] = 31] = "Annotated";
    Type2[Type2["AnnotatedBuiltin"] = 32] = "AnnotatedBuiltin";
    Type2[Type2["Documented"] = 33] = "Documented";
    Type2[Type2["ConstructorDefinition"] = 34] = "ConstructorDefinition";
  })(Type = Tree2.Type || (Tree2.Type = {}));
  Tree2.typeNames = ["Invalid", "BodyBlock", "ArgumentBlockApplication", "OperatorBlockApplication", "Ident", "Private", "Number", "Wildcard", "SuspendedDefaultArguments", "TextLiteral", "App", "NamedApp", "OprApp", "UnaryOprApp", "AutoscopedIdentifier", "OprSectionBoundary", "TemplateFunction", "MultiSegmentApp", "TypeDef", "Assignment", "Function", "ForeignFunction", "Import", "Export", "Group", "TypeSignature", "TypeAnnotated", "CaseOf", "Lambda", "Array", "Tuple", "Annotated", "AnnotatedBuiltin", "Documented", "ConstructorDefinition"];
  class Invalid2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 0 /* Invalid */;
    }
    static read(view, address) {
      return new Invalid2(readOffset(view, address));
    }
    get error() {
      return readString(this._v, 52);
    }
    get ast() {
      return Tree2.read(this._v, 56);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.ast);
    }
  }
  Tree2.Invalid = Invalid2;
  class BodyBlock2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 1 /* BodyBlock */;
    }
    static read(view, address) {
      return new BodyBlock2(readOffset(view, address));
    }
    get statements() {
      return readSequence(this._v, 52, 81, Line.read);
    }
    visitStatements(visitor) {
      return visitSequence(this._v, 52, 81, (view, address) => visitor(Line.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!this.visitStatements(visitor);
    }
  }
  Tree2.BodyBlock = BodyBlock2;
  class ArgumentBlockApplication extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 2 /* ArgumentBlockApplication */;
    }
    static read(view, address) {
      return new ArgumentBlockApplication(readOffset(view, address));
    }
    get lhs() {
      return readOption(this._v, 52, Tree2.read);
    }
    get arguments() {
      return readSequence(this._v, 57, 81, Line.read);
    }
    visitLhs(visitor) {
      return visitOption(this._v, 52, (view, address) => visitor(Tree2.read(view, address)));
    }
    visitArguments(visitor) {
      return visitSequence(this._v, 57, 81, (view, address) => visitor(Line.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!this.visitLhs(visitor) || !!this.visitArguments(visitor);
    }
  }
  Tree2.ArgumentBlockApplication = ArgumentBlockApplication;
  class OperatorBlockApplication extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 3 /* OperatorBlockApplication */;
    }
    static read(view, address) {
      return new OperatorBlockApplication(readOffset(view, address));
    }
    get lhs() {
      return readOption(this._v, 52, Tree2.read);
    }
    get expressions() {
      return readSequence(this._v, 57, 81, OperatorLine.read);
    }
    get excess() {
      return readSequence(this._v, 61, 81, Line.read);
    }
    visitLhs(visitor) {
      return visitOption(this._v, 52, (view, address) => visitor(Tree2.read(view, address)));
    }
    visitExpressions(visitor) {
      return visitSequence(this._v, 57, 81, (view, address) => visitor(OperatorLine.read(view, address)));
    }
    visitExcess(visitor) {
      return visitSequence(this._v, 61, 81, (view, address) => visitor(Line.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!this.visitLhs(visitor) || !!this.visitExpressions(visitor) || !!this.visitExcess(visitor);
    }
  }
  Tree2.OperatorBlockApplication = OperatorBlockApplication;
  class Ident2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 4 /* Ident */;
    }
    static read(view, address) {
      return new Ident2(readOffset(view, address));
    }
    get token() {
      return Token.Ident.read(this._v, 52);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.token);
    }
  }
  Tree2.Ident = Ident2;
  class Private extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 5 /* Private */;
    }
    static read(view, address) {
      return new Private(readOffset(view, address));
    }
    get keyword() {
      return Token.Private.read(this._v, 52);
    }
    get body() {
      return readOption(this._v, 128, Tree2.read);
    }
    visitBody(visitor) {
      return visitOption(this._v, 128, (view, address) => visitor(Tree2.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.keyword) || !!this.visitBody(visitor);
    }
  }
  Tree2.Private = Private;
  class Number2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 6 /* Number */;
    }
    static read(view, address) {
      return new Number2(readOffset(view, address));
    }
    get base() {
      return readOption(this._v, 52, Token.NumberBase.read);
    }
    get integer() {
      return readOption(this._v, 57, Token.Digits.read);
    }
    get fractionalDigits() {
      return readOption(this._v, 62, FractionalDigits.read);
    }
    visitBase(visitor) {
      return visitOption(this._v, 52, (view, address) => visitor(Token.NumberBase.read(view, address)));
    }
    visitInteger(visitor) {
      return visitOption(this._v, 57, (view, address) => visitor(Token.Digits.read(view, address)));
    }
    visitFractionalDigits(visitor) {
      return visitOption(this._v, 62, (view, address) => visitor(FractionalDigits.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!this.visitBase(visitor) || !!this.visitInteger(visitor) || !!this.visitFractionalDigits(visitor);
    }
  }
  Tree2.Number = Number2;
  class Wildcard2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 7 /* Wildcard */;
    }
    static read(view, address) {
      return new Wildcard2(readOffset(view, address));
    }
    get token() {
      return Token.Wildcard.read(this._v, 52);
    }
    get deBruijnIndex() {
      return readI32(this._v, 132);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.token);
    }
  }
  Tree2.Wildcard = Wildcard2;
  class SuspendedDefaultArguments extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 8 /* SuspendedDefaultArguments */;
    }
    static read(view, address) {
      return new SuspendedDefaultArguments(readOffset(view, address));
    }
    get token() {
      return Token.SuspendedDefaultArguments.read(this._v, 52);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.token);
    }
  }
  Tree2.SuspendedDefaultArguments = SuspendedDefaultArguments;
  class TextLiteral2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 9 /* TextLiteral */;
    }
    static read(view, address) {
      return new TextLiteral2(readOffset(view, address));
    }
    get open() {
      return readOption(this._v, 52, Token.TextStart.read);
    }
    get newline() {
      return readOption(this._v, 57, Token.Newline.read);
    }
    get elements() {
      return readSequence(this._v, 62, 4, TextElement.read);
    }
    get close() {
      return readOption(this._v, 66, Token.TextEnd.read);
    }
    visitOpen(visitor) {
      return visitOption(this._v, 52, (view, address) => visitor(Token.TextStart.read(view, address)));
    }
    visitNewline(visitor) {
      return visitOption(this._v, 57, (view, address) => visitor(Token.Newline.read(view, address)));
    }
    visitElements(visitor) {
      return visitSequence(this._v, 62, 4, (view, address) => visitor(TextElement.read(view, address)));
    }
    visitClose(visitor) {
      return visitOption(this._v, 66, (view, address) => visitor(Token.TextEnd.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!this.visitOpen(visitor) || !!this.visitNewline(visitor) || !!this.visitElements(visitor) || !!this.visitClose(visitor);
    }
  }
  Tree2.TextLiteral = TextLiteral2;
  class App2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 10 /* App */;
    }
    static read(view, address) {
      return new App2(readOffset(view, address));
    }
    get func() {
      return Tree2.read(this._v, 52);
    }
    get arg() {
      return Tree2.read(this._v, 56);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.func) || !!visitor(this.arg);
    }
  }
  Tree2.App = App2;
  class NamedApp extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 11 /* NamedApp */;
    }
    static read(view, address) {
      return new NamedApp(readOffset(view, address));
    }
    get func() {
      return Tree2.read(this._v, 52);
    }
    get open() {
      return readOption(this._v, 56, Token.OpenSymbol.read);
    }
    get name() {
      return Token.Ident.read(this._v, 61);
    }
    get equals() {
      return Token.Operator.read(this._v, 144);
    }
    get arg() {
      return Tree2.read(this._v, 220);
    }
    get close() {
      return readOption(this._v, 224, Token.CloseSymbol.read);
    }
    visitOpen(visitor) {
      return visitOption(this._v, 56, (view, address) => visitor(Token.OpenSymbol.read(view, address)));
    }
    visitClose(visitor) {
      return visitOption(this._v, 224, (view, address) => visitor(Token.CloseSymbol.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.func) || !!this.visitOpen(visitor) || !!visitor(this.name) || !!visitor(this.equals) || !!visitor(this.arg) || !!this.visitClose(visitor);
    }
  }
  Tree2.NamedApp = NamedApp;
  class OprApp2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 12 /* OprApp */;
    }
    static read(view, address) {
      return new OprApp2(readOffset(view, address));
    }
    get lhs() {
      return readOption(this._v, 52, Tree2.read);
    }
    get opr() {
      return readResult(this._v, 57, Token.Operator.read, MultipleOperatorError.read);
    }
    get rhs() {
      return readOption(this._v, 61, Tree2.read);
    }
    visitLhs(visitor) {
      return visitOption(this._v, 52, (view, address) => visitor(Tree2.read(view, address)));
    }
    visitOpr(visitor) {
      return visitResult(this._v, 57, (view, address) => visitor(Token.Operator.read(view, address)), (view, address) => visitor(MultipleOperatorError.read(view, address)));
    }
    visitRhs(visitor) {
      return visitOption(this._v, 61, (view, address) => visitor(Tree2.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!this.visitLhs(visitor) || !!this.visitOpr(visitor) || !!this.visitRhs(visitor);
    }
  }
  Tree2.OprApp = OprApp2;
  class UnaryOprApp2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 13 /* UnaryOprApp */;
    }
    static read(view, address) {
      return new UnaryOprApp2(readOffset(view, address));
    }
    get opr() {
      return Token.Operator.read(this._v, 52);
    }
    get rhs() {
      return readOption(this._v, 128, Tree2.read);
    }
    visitRhs(visitor) {
      return visitOption(this._v, 128, (view, address) => visitor(Tree2.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.opr) || !!this.visitRhs(visitor);
    }
  }
  Tree2.UnaryOprApp = UnaryOprApp2;
  class AutoscopedIdentifier extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 14 /* AutoscopedIdentifier */;
    }
    static read(view, address) {
      return new AutoscopedIdentifier(readOffset(view, address));
    }
    get opr() {
      return Token.Operator.read(this._v, 52);
    }
    get ident() {
      return Token.Ident.read(this._v, 128);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.opr) || !!visitor(this.ident);
    }
  }
  Tree2.AutoscopedIdentifier = AutoscopedIdentifier;
  class OprSectionBoundary extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 15 /* OprSectionBoundary */;
    }
    static read(view, address) {
      return new OprSectionBoundary(readOffset(view, address));
    }
    get arguments() {
      return readU32(this._v, 52);
    }
    get ast() {
      return Tree2.read(this._v, 56);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.ast);
    }
  }
  Tree2.OprSectionBoundary = OprSectionBoundary;
  class TemplateFunction extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 16 /* TemplateFunction */;
    }
    static read(view, address) {
      return new TemplateFunction(readOffset(view, address));
    }
    get arguments() {
      return readU32(this._v, 52);
    }
    get ast() {
      return Tree2.read(this._v, 56);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.ast);
    }
  }
  Tree2.TemplateFunction = TemplateFunction;
  class MultiSegmentApp extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 17 /* MultiSegmentApp */;
    }
    static read(view, address) {
      return new MultiSegmentApp(readOffset(view, address));
    }
    get segments() {
      return readSequence(this._v, 52, 9, MultiSegmentAppSegment.read);
    }
    visitSegments(visitor) {
      return visitSequence(this._v, 52, 9, (view, address) => visitor(MultiSegmentAppSegment.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!this.visitSegments(visitor);
    }
  }
  Tree2.MultiSegmentApp = MultiSegmentApp;
  class TypeDef extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 18 /* TypeDef */;
    }
    static read(view, address) {
      return new TypeDef(readOffset(view, address));
    }
    get keyword() {
      return Token.Ident.read(this._v, 52);
    }
    get name() {
      return Token.Ident.read(this._v, 135);
    }
    get params() {
      return readSequence(this._v, 218, 39, ArgumentDefinition.read);
    }
    get body() {
      return readSequence(this._v, 222, 81, Line.read);
    }
    visitParams(visitor) {
      return visitSequence(this._v, 218, 39, (view, address) => visitor(ArgumentDefinition.read(view, address)));
    }
    visitBody(visitor) {
      return visitSequence(this._v, 222, 81, (view, address) => visitor(Line.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.keyword) || !!visitor(this.name) || !!this.visitParams(visitor) || !!this.visitBody(visitor);
    }
  }
  Tree2.TypeDef = TypeDef;
  class Assignment2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 19 /* Assignment */;
    }
    static read(view, address) {
      return new Assignment2(readOffset(view, address));
    }
    get pattern() {
      return Tree2.read(this._v, 52);
    }
    get equals() {
      return Token.Operator.read(this._v, 56);
    }
    get expr() {
      return Tree2.read(this._v, 132);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.pattern) || !!visitor(this.equals) || !!visitor(this.expr);
    }
  }
  Tree2.Assignment = Assignment2;
  class Function2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 20 /* Function */;
    }
    static read(view, address) {
      return new Function2(readOffset(view, address));
    }
    get name() {
      return Tree2.read(this._v, 52);
    }
    get args() {
      return readSequence(this._v, 56, 39, ArgumentDefinition.read);
    }
    get returns() {
      return readOption(this._v, 60, ReturnSpecification.read);
    }
    get equals() {
      return Token.Operator.read(this._v, 65);
    }
    get body() {
      return readOption(this._v, 141, Tree2.read);
    }
    visitArgs(visitor) {
      return visitSequence(this._v, 56, 39, (view, address) => visitor(ArgumentDefinition.read(view, address)));
    }
    visitReturns(visitor) {
      return visitOption(this._v, 60, (view, address) => visitor(ReturnSpecification.read(view, address)));
    }
    visitBody(visitor) {
      return visitOption(this._v, 141, (view, address) => visitor(Tree2.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.name) || !!this.visitArgs(visitor) || !!this.visitReturns(visitor) || !!visitor(this.equals) || !!this.visitBody(visitor);
    }
  }
  Tree2.Function = Function2;
  class ForeignFunction extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 21 /* ForeignFunction */;
    }
    static read(view, address) {
      return new ForeignFunction(readOffset(view, address));
    }
    get foreign() {
      return Token.Ident.read(this._v, 52);
    }
    get language() {
      return Token.Ident.read(this._v, 135);
    }
    get name() {
      return Token.Ident.read(this._v, 218);
    }
    get args() {
      return readSequence(this._v, 301, 39, ArgumentDefinition.read);
    }
    get equals() {
      return Token.Operator.read(this._v, 305);
    }
    get body() {
      return Tree2.read(this._v, 381);
    }
    visitArgs(visitor) {
      return visitSequence(this._v, 301, 39, (view, address) => visitor(ArgumentDefinition.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.foreign) || !!visitor(this.language) || !!visitor(this.name) || !!this.visitArgs(visitor) || !!visitor(this.equals) || !!visitor(this.body);
    }
  }
  Tree2.ForeignFunction = ForeignFunction;
  class Import2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 22 /* Import */;
    }
    static read(view, address) {
      return new Import2(readOffset(view, address));
    }
    get polyglot() {
      return readOption(this._v, 52, MultiSegmentAppSegment.read);
    }
    get from() {
      return readOption(this._v, 57, MultiSegmentAppSegment.read);
    }
    get import() {
      return MultiSegmentAppSegment.read(this._v, 62);
    }
    get all() {
      return readOption(this._v, 71, Token.Ident.read);
    }
    get as() {
      return readOption(this._v, 76, MultiSegmentAppSegment.read);
    }
    get hiding() {
      return readOption(this._v, 81, MultiSegmentAppSegment.read);
    }
    visitPolyglot(visitor) {
      return visitOption(this._v, 52, (view, address) => visitor(MultiSegmentAppSegment.read(view, address)));
    }
    visitFrom(visitor) {
      return visitOption(this._v, 57, (view, address) => visitor(MultiSegmentAppSegment.read(view, address)));
    }
    visitAll(visitor) {
      return visitOption(this._v, 71, (view, address) => visitor(Token.Ident.read(view, address)));
    }
    visitAs(visitor) {
      return visitOption(this._v, 76, (view, address) => visitor(MultiSegmentAppSegment.read(view, address)));
    }
    visitHiding(visitor) {
      return visitOption(this._v, 81, (view, address) => visitor(MultiSegmentAppSegment.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!this.visitPolyglot(visitor) || !!this.visitFrom(visitor) || !!visitor(this.import) || !!this.visitAll(visitor) || !!this.visitAs(visitor) || !!this.visitHiding(visitor);
    }
  }
  Tree2.Import = Import2;
  class Export extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 23 /* Export */;
    }
    static read(view, address) {
      return new Export(readOffset(view, address));
    }
    get from() {
      return readOption(this._v, 52, MultiSegmentAppSegment.read);
    }
    get export() {
      return MultiSegmentAppSegment.read(this._v, 57);
    }
    get all() {
      return readOption(this._v, 66, Token.Ident.read);
    }
    get as() {
      return readOption(this._v, 71, MultiSegmentAppSegment.read);
    }
    get hiding() {
      return readOption(this._v, 76, MultiSegmentAppSegment.read);
    }
    visitFrom(visitor) {
      return visitOption(this._v, 52, (view, address) => visitor(MultiSegmentAppSegment.read(view, address)));
    }
    visitAll(visitor) {
      return visitOption(this._v, 66, (view, address) => visitor(Token.Ident.read(view, address)));
    }
    visitAs(visitor) {
      return visitOption(this._v, 71, (view, address) => visitor(MultiSegmentAppSegment.read(view, address)));
    }
    visitHiding(visitor) {
      return visitOption(this._v, 76, (view, address) => visitor(MultiSegmentAppSegment.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!this.visitFrom(visitor) || !!visitor(this.export) || !!this.visitAll(visitor) || !!this.visitAs(visitor) || !!this.visitHiding(visitor);
    }
  }
  Tree2.Export = Export;
  class Group2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 24 /* Group */;
    }
    static read(view, address) {
      return new Group2(readOffset(view, address));
    }
    get open() {
      return readOption(this._v, 52, Token.OpenSymbol.read);
    }
    get body() {
      return readOption(this._v, 57, Tree2.read);
    }
    get close() {
      return readOption(this._v, 62, Token.CloseSymbol.read);
    }
    visitOpen(visitor) {
      return visitOption(this._v, 52, (view, address) => visitor(Token.OpenSymbol.read(view, address)));
    }
    visitBody(visitor) {
      return visitOption(this._v, 57, (view, address) => visitor(Tree2.read(view, address)));
    }
    visitClose(visitor) {
      return visitOption(this._v, 62, (view, address) => visitor(Token.CloseSymbol.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!this.visitOpen(visitor) || !!this.visitBody(visitor) || !!this.visitClose(visitor);
    }
  }
  Tree2.Group = Group2;
  class TypeSignature extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 25 /* TypeSignature */;
    }
    static read(view, address) {
      return new TypeSignature(readOffset(view, address));
    }
    get variable() {
      return Tree2.read(this._v, 52);
    }
    get operator() {
      return Token.Operator.read(this._v, 56);
    }
    get typeNode() {
      return Tree2.read(this._v, 132);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.variable) || !!visitor(this.operator) || !!visitor(this.typeNode);
    }
  }
  Tree2.TypeSignature = TypeSignature;
  class TypeAnnotated extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 26 /* TypeAnnotated */;
    }
    static read(view, address) {
      return new TypeAnnotated(readOffset(view, address));
    }
    get expression() {
      return Tree2.read(this._v, 52);
    }
    get operator() {
      return Token.Operator.read(this._v, 56);
    }
    get typeNode() {
      return Tree2.read(this._v, 132);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.expression) || !!visitor(this.operator) || !!visitor(this.typeNode);
    }
  }
  Tree2.TypeAnnotated = TypeAnnotated;
  class CaseOf extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 27 /* CaseOf */;
    }
    static read(view, address) {
      return new CaseOf(readOffset(view, address));
    }
    get case() {
      return Token.Ident.read(this._v, 52);
    }
    get expression() {
      return readOption(this._v, 135, Tree2.read);
    }
    get of() {
      return Token.Ident.read(this._v, 140);
    }
    get cases() {
      return readSequence(this._v, 223, 10, CaseLine.read);
    }
    visitExpression(visitor) {
      return visitOption(this._v, 135, (view, address) => visitor(Tree2.read(view, address)));
    }
    visitCases(visitor) {
      return visitSequence(this._v, 223, 10, (view, address) => visitor(CaseLine.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.case) || !!this.visitExpression(visitor) || !!visitor(this.of) || !!this.visitCases(visitor);
    }
  }
  Tree2.CaseOf = CaseOf;
  class Lambda extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 28 /* Lambda */;
    }
    static read(view, address) {
      return new Lambda(readOffset(view, address));
    }
    get operator() {
      return Token.Operator.read(this._v, 52);
    }
    get arrow() {
      return readOption(this._v, 128, Tree2.read);
    }
    visitArrow(visitor) {
      return visitOption(this._v, 128, (view, address) => visitor(Tree2.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.operator) || !!this.visitArrow(visitor);
    }
  }
  Tree2.Lambda = Lambda;
  class Array2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 29 /* Array */;
    }
    static read(view, address) {
      return new Array2(readOffset(view, address));
    }
    get left() {
      return Token.OpenSymbol.read(this._v, 52);
    }
    get first() {
      return readOption(this._v, 128, Tree2.read);
    }
    get rest() {
      return readSequence(this._v, 133, 81, OperatorDelimitedTree.read);
    }
    get right() {
      return Token.CloseSymbol.read(this._v, 137);
    }
    visitFirst(visitor) {
      return visitOption(this._v, 128, (view, address) => visitor(Tree2.read(view, address)));
    }
    visitRest(visitor) {
      return visitSequence(this._v, 133, 81, (view, address) => visitor(OperatorDelimitedTree.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.left) || !!this.visitFirst(visitor) || !!this.visitRest(visitor) || !!visitor(this.right);
    }
  }
  Tree2.Array = Array2;
  class Tuple extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 30 /* Tuple */;
    }
    static read(view, address) {
      return new Tuple(readOffset(view, address));
    }
    get left() {
      return Token.OpenSymbol.read(this._v, 52);
    }
    get first() {
      return readOption(this._v, 128, Tree2.read);
    }
    get rest() {
      return readSequence(this._v, 133, 81, OperatorDelimitedTree.read);
    }
    get right() {
      return Token.CloseSymbol.read(this._v, 137);
    }
    visitFirst(visitor) {
      return visitOption(this._v, 128, (view, address) => visitor(Tree2.read(view, address)));
    }
    visitRest(visitor) {
      return visitSequence(this._v, 133, 81, (view, address) => visitor(OperatorDelimitedTree.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.left) || !!this.visitFirst(visitor) || !!this.visitRest(visitor) || !!visitor(this.right);
    }
  }
  Tree2.Tuple = Tuple;
  class Annotated extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 31 /* Annotated */;
    }
    static read(view, address) {
      return new Annotated(readOffset(view, address));
    }
    get token() {
      return Token.Operator.read(this._v, 52);
    }
    get annotation() {
      return Token.Ident.read(this._v, 128);
    }
    get argument() {
      return readOption(this._v, 211, Tree2.read);
    }
    get newlines() {
      return readSequence(this._v, 216, 76, Token.Newline.read);
    }
    get expression() {
      return readOption(this._v, 220, Tree2.read);
    }
    visitArgument(visitor) {
      return visitOption(this._v, 211, (view, address) => visitor(Tree2.read(view, address)));
    }
    visitNewlines(visitor) {
      return visitSequence(this._v, 216, 76, (view, address) => visitor(Token.Newline.read(view, address)));
    }
    visitExpression(visitor) {
      return visitOption(this._v, 220, (view, address) => visitor(Tree2.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.token) || !!visitor(this.annotation) || !!this.visitArgument(visitor) || !!this.visitNewlines(visitor) || !!this.visitExpression(visitor);
    }
  }
  Tree2.Annotated = Annotated;
  class AnnotatedBuiltin extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 32 /* AnnotatedBuiltin */;
    }
    static read(view, address) {
      return new AnnotatedBuiltin(readOffset(view, address));
    }
    get token() {
      return Token.Operator.read(this._v, 52);
    }
    get annotation() {
      return Token.Ident.read(this._v, 128);
    }
    get newlines() {
      return readSequence(this._v, 211, 76, Token.Newline.read);
    }
    get expression() {
      return readOption(this._v, 215, Tree2.read);
    }
    visitNewlines(visitor) {
      return visitSequence(this._v, 211, 76, (view, address) => visitor(Token.Newline.read(view, address)));
    }
    visitExpression(visitor) {
      return visitOption(this._v, 215, (view, address) => visitor(Tree2.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.token) || !!visitor(this.annotation) || !!this.visitNewlines(visitor) || !!this.visitExpression(visitor);
    }
  }
  Tree2.AnnotatedBuiltin = AnnotatedBuiltin;
  class Documented2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 33 /* Documented */;
    }
    static read(view, address) {
      return new Documented2(readOffset(view, address));
    }
    get documentation() {
      return DocComment.read(this._v, 52);
    }
    get expression() {
      return readOption(this._v, 136, Tree2.read);
    }
    visitExpression(visitor) {
      return visitOption(this._v, 136, (view, address) => visitor(Tree2.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.documentation) || !!this.visitExpression(visitor);
    }
  }
  Tree2.Documented = Documented2;
  class ConstructorDefinition extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 34 /* ConstructorDefinition */;
    }
    static read(view, address) {
      return new ConstructorDefinition(readOffset(view, address));
    }
    get ident() {
      return Token.Ident.read(this._v, 52);
    }
    get arguments() {
      return readSequence(this._v, 135, 39, ArgumentDefinition.read);
    }
    get block() {
      return readSequence(this._v, 139, 81, ArgumentDefinitionLine.read);
    }
    visitArguments(visitor) {
      return visitSequence(this._v, 135, 39, (view, address) => visitor(ArgumentDefinition.read(view, address)));
    }
    visitBlock(visitor) {
      return visitSequence(this._v, 139, 81, (view, address) => visitor(ArgumentDefinitionLine.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.ident) || !!this.visitArguments(visitor) || !!this.visitBlock(visitor);
    }
  }
  Tree2.ConstructorDefinition = ConstructorDefinition;
  const VARIANT_READERS = [Invalid2.read, BodyBlock2.read, ArgumentBlockApplication.read, OperatorBlockApplication.read, Ident2.read, Private.read, Number2.read, Wildcard2.read, SuspendedDefaultArguments.read, TextLiteral2.read, App2.read, NamedApp.read, OprApp2.read, UnaryOprApp2.read, AutoscopedIdentifier.read, OprSectionBoundary.read, TemplateFunction.read, MultiSegmentApp.read, TypeDef.read, Assignment2.read, Function2.read, ForeignFunction.read, Import2.read, Export.read, Group2.read, TypeSignature.read, TypeAnnotated.read, CaseOf.read, Lambda.read, Array2.read, Tuple.read, Annotated.read, AnnotatedBuiltin.read, Documented2.read, ConstructorDefinition.read];
  function read(view, address) {
    return readEnum(VARIANT_READERS, view, address);
  }
  Tree2.read = read;
  function isInstance(obj) {
    return obj instanceof AbstractBase;
  }
  Tree2.isInstance = isInstance;
})(Tree || (Tree = {}));
var MultiSegmentAppSegment = class _MultiSegmentAppSegment extends LazyObject {
  constructor(view) {
    super(view);
  }
  static read(view, address) {
    return new _MultiSegmentAppSegment(readOffset(view, address));
  }
  get header() {
    return Token.read(this._v, 0);
  }
  get body() {
    return readOption(this._v, 4, Tree.read);
  }
  visitBody(visitor) {
    return visitOption(this._v, 4, (view, address) => visitor(Tree.read(view, address)));
  }
  visitChildren(visitor) {
    return super.visitChildren(visitor) || !!visitor(this.header) || !!this.visitBody(visitor);
  }
};
var CaseLine = class _CaseLine extends LazyObject {
  constructor(view) {
    super(view);
  }
  static read(view, address) {
    return new _CaseLine(readOffset(view, address));
  }
  get newline() {
    return readOption(this._v, 0, Token.Newline.read);
  }
  get case() {
    return readOption(this._v, 5, Case.read);
  }
  visitNewline(visitor) {
    return visitOption(this._v, 0, (view, address) => visitor(Token.Newline.read(view, address)));
  }
  visitCase(visitor) {
    return visitOption(this._v, 5, (view, address) => visitor(Case.read(view, address)));
  }
  visitChildren(visitor) {
    return super.visitChildren(visitor) || !!this.visitNewline(visitor) || !!this.visitCase(visitor);
  }
};
var Base;
((Base2) => {
  class AbstractBase extends LazyObject {
    constructor(view) {
      super(view);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Base2.AbstractBase = AbstractBase;
  let Type;
  ((Type2) => {
    Type2[Type2["Binary"] = 0] = "Binary";
    Type2[Type2["Octal"] = 1] = "Octal";
    Type2[Type2["Hexadecimal"] = 2] = "Hexadecimal";
  })(Type = Base2.Type || (Base2.Type = {}));
  Base2.typeNames = ["Binary", "Octal", "Hexadecimal"];
  class Binary extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 0 /* Binary */;
    }
    static read(view, address) {
      return new Binary(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Base2.Binary = Binary;
  class Octal extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 1 /* Octal */;
    }
    static read(view, address) {
      return new Octal(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Base2.Octal = Octal;
  class Hexadecimal extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 2 /* Hexadecimal */;
    }
    static read(view, address) {
      return new Hexadecimal(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Base2.Hexadecimal = Hexadecimal;
  const VARIANT_READERS = [Binary.read, Octal.read, Hexadecimal.read];
  function read(view, address) {
    return readEnum(VARIANT_READERS, view, address);
  }
  Base2.read = read;
  function isInstance(obj) {
    return obj instanceof AbstractBase;
  }
  Base2.isInstance = isInstance;
})(Base || (Base = {}));
var ArgumentDefinitionLine = class _ArgumentDefinitionLine extends LazyObject {
  constructor(view) {
    super(view);
  }
  static read(view, address) {
    return new _ArgumentDefinitionLine(readOffset(view, address));
  }
  get newline() {
    return Token.Newline.read(this._v, 0);
  }
  get argument() {
    return readOption(this._v, 76, ArgumentDefinition.read);
  }
  visitArgument(visitor) {
    return visitOption(this._v, 76, (view, address) => visitor(ArgumentDefinition.read(view, address)));
  }
  visitChildren(visitor) {
    return super.visitChildren(visitor) || !!visitor(this.newline) || !!this.visitArgument(visitor);
  }
};
var ArgumentDefault = class _ArgumentDefault extends LazyObject {
  constructor(view) {
    super(view);
  }
  static read(view, address) {
    return new _ArgumentDefault(readOffset(view, address));
  }
  get equals() {
    return Token.Operator.read(this._v, 0);
  }
  get expression() {
    return Tree.read(this._v, 76);
  }
  visitChildren(visitor) {
    return super.visitChildren(visitor) || !!visitor(this.equals) || !!visitor(this.expression);
  }
};
var OperatorBlockExpression = class _OperatorBlockExpression extends LazyObject {
  constructor(view) {
    super(view);
  }
  static read(view, address) {
    return new _OperatorBlockExpression(readOffset(view, address));
  }
  get operator() {
    return readResult(this._v, 0, Token.Operator.read, MultipleOperatorError.read);
  }
  get expression() {
    return Tree.read(this._v, 4);
  }
  visitOperator(visitor) {
    return visitResult(this._v, 0, (view, address) => visitor(Token.Operator.read(view, address)), (view, address) => visitor(MultipleOperatorError.read(view, address)));
  }
  visitChildren(visitor) {
    return super.visitChildren(visitor) || !!this.visitOperator(visitor) || !!visitor(this.expression);
  }
};
var ReturnSpecification = class _ReturnSpecification extends LazyObject {
  constructor(view) {
    super(view);
  }
  static read(view, address) {
    return new _ReturnSpecification(readOffset(view, address));
  }
  get arrow() {
    return Token.Operator.read(this._v, 0);
  }
  get typeNode() {
    return Tree.read(this._v, 76);
  }
  visitChildren(visitor) {
    return super.visitChildren(visitor) || !!visitor(this.arrow) || !!visitor(this.typeNode);
  }
};
var Line = class _Line extends LazyObject {
  constructor(view) {
    super(view);
  }
  static read(view, address) {
    return new _Line(readOffset(view, address));
  }
  get newline() {
    return Token.Newline.read(this._v, 0);
  }
  get expression() {
    return readOption(this._v, 76, Tree.read);
  }
  visitExpression(visitor) {
    return visitOption(this._v, 76, (view, address) => visitor(Tree.read(view, address)));
  }
  visitChildren(visitor) {
    return super.visitChildren(visitor) || !!visitor(this.newline) || !!this.visitExpression(visitor);
  }
};
var OperatorLine = class _OperatorLine extends LazyObject {
  constructor(view) {
    super(view);
  }
  static read(view, address) {
    return new _OperatorLine(readOffset(view, address));
  }
  get newline() {
    return Token.Newline.read(this._v, 0);
  }
  get expression() {
    return readOption(this._v, 76, OperatorBlockExpression.read);
  }
  visitExpression(visitor) {
    return visitOption(this._v, 76, (view, address) => visitor(OperatorBlockExpression.read(view, address)));
  }
  visitChildren(visitor) {
    return super.visitChildren(visitor) || !!visitor(this.newline) || !!this.visitExpression(visitor);
  }
};
var ArgumentType = class _ArgumentType extends LazyObject {
  constructor(view) {
    super(view);
  }
  static read(view, address) {
    return new _ArgumentType(readOffset(view, address));
  }
  get operator() {
    return Token.Operator.read(this._v, 0);
  }
  get typeNode() {
    return Tree.read(this._v, 76);
  }
  visitChildren(visitor) {
    return super.visitChildren(visitor) || !!visitor(this.operator) || !!visitor(this.typeNode);
  }
};
var Case = class _Case extends LazyObject {
  constructor(view) {
    super(view);
  }
  static read(view, address) {
    return new _Case(readOffset(view, address));
  }
  get documentation() {
    return readOption(this._v, 0, DocComment.read);
  }
  get pattern() {
    return readOption(this._v, 5, Tree.read);
  }
  get arrow() {
    return readOption(this._v, 10, Token.Operator.read);
  }
  get expression() {
    return readOption(this._v, 15, Tree.read);
  }
  visitDocumentation(visitor) {
    return visitOption(this._v, 0, (view, address) => visitor(DocComment.read(view, address)));
  }
  visitPattern(visitor) {
    return visitOption(this._v, 5, (view, address) => visitor(Tree.read(view, address)));
  }
  visitArrow(visitor) {
    return visitOption(this._v, 10, (view, address) => visitor(Token.Operator.read(view, address)));
  }
  visitExpression(visitor) {
    return visitOption(this._v, 15, (view, address) => visitor(Tree.read(view, address)));
  }
  visitChildren(visitor) {
    return super.visitChildren(visitor) || !!this.visitDocumentation(visitor) || !!this.visitPattern(visitor) || !!this.visitArrow(visitor) || !!this.visitExpression(visitor);
  }
};
var Token;
((Token3) => {
  class AbstractBase extends LazyObject {
    constructor(view) {
      super(view);
    }
    get leftOffsetVisible() {
      return readU32(this._v, 0);
    }
    get leftOffsetCodeReprBegin() {
      return readU32(this._v, 4);
    }
    get leftOffsetCodeReprLen() {
      return readU32(this._v, 8);
    }
    get leftOffsetCodeStartUtf8() {
      return readU32(this._v, 12);
    }
    get whitespaceStartInCodeBuffer() {
      return readU32(this._v, 16);
    }
    get leftOffsetCodeStartLine() {
      return readU32(this._v, 20);
    }
    get leftOffsetCodeStartCol16() {
      return readU32(this._v, 24);
    }
    get whitespaceLengthInCodeBuffer() {
      return readU32(this._v, 28);
    }
    get leftOffsetCodeLenNewlines() {
      return readU32(this._v, 32);
    }
    get leftOffsetCodeLenLineChars16() {
      return readU32(this._v, 36);
    }
    get codeReprBegin() {
      return readU32(this._v, 40);
    }
    get codeReprLen() {
      return readU32(this._v, 44);
    }
    get codeStartUtf8() {
      return readU32(this._v, 48);
    }
    get startInCodeBuffer() {
      return readU32(this._v, 52);
    }
    get codeStartLine() {
      return readU32(this._v, 56);
    }
    get codeStartCol16() {
      return readU32(this._v, 60);
    }
    get lengthInCodeBuffer() {
      return readU32(this._v, 64);
    }
    get codeLenNewlines() {
      return readU32(this._v, 68);
    }
    get codeLenLineChars16() {
      return readU32(this._v, 72);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.AbstractBase = AbstractBase;
  let Type;
  ((Type2) => {
    Type2[Type2["Newline"] = 0] = "Newline";
    Type2[Type2["OpenSymbol"] = 1] = "OpenSymbol";
    Type2[Type2["CloseSymbol"] = 2] = "CloseSymbol";
    Type2[Type2["BlockStart"] = 3] = "BlockStart";
    Type2[Type2["BlockEnd"] = 4] = "BlockEnd";
    Type2[Type2["Wildcard"] = 5] = "Wildcard";
    Type2[Type2["SuspendedDefaultArguments"] = 6] = "SuspendedDefaultArguments";
    Type2[Type2["Ident"] = 7] = "Ident";
    Type2[Type2["Operator"] = 8] = "Operator";
    Type2[Type2["Digits"] = 9] = "Digits";
    Type2[Type2["NumberBase"] = 10] = "NumberBase";
    Type2[Type2["Private"] = 11] = "Private";
    Type2[Type2["TextStart"] = 12] = "TextStart";
    Type2[Type2["TextEnd"] = 13] = "TextEnd";
    Type2[Type2["TextSection"] = 14] = "TextSection";
    Type2[Type2["TextEscape"] = 15] = "TextEscape";
    Type2[Type2["TextInitialNewline"] = 16] = "TextInitialNewline";
    Type2[Type2["TextNewline"] = 17] = "TextNewline";
    Type2[Type2["Invalid"] = 18] = "Invalid";
  })(Type = Token3.Type || (Token3.Type = {}));
  Token3.typeNames = ["Newline", "OpenSymbol", "CloseSymbol", "BlockStart", "BlockEnd", "Wildcard", "SuspendedDefaultArguments", "Ident", "Operator", "Digits", "NumberBase", "Private", "TextStart", "TextEnd", "TextSection", "TextEscape", "TextInitialNewline", "TextNewline", "Invalid"];
  class Newline extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 0 /* Newline */;
    }
    static read(view, address) {
      return new Newline(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.Newline = Newline;
  class OpenSymbol extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 1 /* OpenSymbol */;
    }
    static read(view, address) {
      return new OpenSymbol(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.OpenSymbol = OpenSymbol;
  class CloseSymbol extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 2 /* CloseSymbol */;
    }
    static read(view, address) {
      return new CloseSymbol(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.CloseSymbol = CloseSymbol;
  class BlockStart extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 3 /* BlockStart */;
    }
    static read(view, address) {
      return new BlockStart(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.BlockStart = BlockStart;
  class BlockEnd extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 4 /* BlockEnd */;
    }
    static read(view, address) {
      return new BlockEnd(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.BlockEnd = BlockEnd;
  class Wildcard2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 5 /* Wildcard */;
    }
    static read(view, address) {
      return new Wildcard2(readOffset(view, address));
    }
    get liftLevel() {
      return readU32(this._v, 76);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.Wildcard = Wildcard2;
  class SuspendedDefaultArguments extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 6 /* SuspendedDefaultArguments */;
    }
    static read(view, address) {
      return new SuspendedDefaultArguments(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.SuspendedDefaultArguments = SuspendedDefaultArguments;
  class Ident2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 7 /* Ident */;
    }
    static read(view, address) {
      return new Ident2(readOffset(view, address));
    }
    get isFree() {
      return readBool(this._v, 76);
    }
    get liftLevel() {
      return readU32(this._v, 77);
    }
    get isTypeOrConstructor() {
      return readBool(this._v, 81);
    }
    get isOperatorLexically() {
      return readBool(this._v, 82);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.Ident = Ident2;
  class Operator extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 8 /* Operator */;
    }
    static read(view, address) {
      return new Operator(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.Operator = Operator;
  class Digits extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 9 /* Digits */;
    }
    static read(view, address) {
      return new Digits(readOffset(view, address));
    }
    get base() {
      return readOption(this._v, 76, Base.read);
    }
    visitBase(visitor) {
      return visitOption(this._v, 76, (view, address) => visitor(Base.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!this.visitBase(visitor);
    }
  }
  Token3.Digits = Digits;
  class NumberBase extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 10 /* NumberBase */;
    }
    static read(view, address) {
      return new NumberBase(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.NumberBase = NumberBase;
  class Private extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 11 /* Private */;
    }
    static read(view, address) {
      return new Private(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.Private = Private;
  class TextStart extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 12 /* TextStart */;
    }
    static read(view, address) {
      return new TextStart(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.TextStart = TextStart;
  class TextEnd extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 13 /* TextEnd */;
    }
    static read(view, address) {
      return new TextEnd(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.TextEnd = TextEnd;
  class TextSection extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 14 /* TextSection */;
    }
    static read(view, address) {
      return new TextSection(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.TextSection = TextSection;
  class TextEscape extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 15 /* TextEscape */;
    }
    static read(view, address) {
      return new TextEscape(readOffset(view, address));
    }
    get value() {
      return readU32(this._v, 76);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.TextEscape = TextEscape;
  class TextInitialNewline extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 16 /* TextInitialNewline */;
    }
    static read(view, address) {
      return new TextInitialNewline(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.TextInitialNewline = TextInitialNewline;
  class TextNewline extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 17 /* TextNewline */;
    }
    static read(view, address) {
      return new TextNewline(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.TextNewline = TextNewline;
  class Invalid2 extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 18 /* Invalid */;
    }
    static read(view, address) {
      return new Invalid2(readOffset(view, address));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  Token3.Invalid = Invalid2;
  const VARIANT_READERS = [Newline.read, OpenSymbol.read, CloseSymbol.read, BlockStart.read, BlockEnd.read, Wildcard2.read, SuspendedDefaultArguments.read, Ident2.read, Operator.read, Digits.read, NumberBase.read, Private.read, TextStart.read, TextEnd.read, TextSection.read, TextEscape.read, TextInitialNewline.read, TextNewline.read, Invalid2.read];
  function read(view, address) {
    return readEnum(VARIANT_READERS, view, address);
  }
  Token3.read = read;
  function isInstance(obj) {
    return obj instanceof AbstractBase;
  }
  Token3.isInstance = isInstance;
})(Token || (Token = {}));
var TextElement;
((TextElement2) => {
  class AbstractBase extends LazyObject {
    constructor(view) {
      super(view);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor);
    }
  }
  TextElement2.AbstractBase = AbstractBase;
  let Type;
  ((Type2) => {
    Type2[Type2["Section"] = 0] = "Section";
    Type2[Type2["Escape"] = 1] = "Escape";
    Type2[Type2["Newline"] = 2] = "Newline";
    Type2[Type2["Splice"] = 3] = "Splice";
  })(Type = TextElement2.Type || (TextElement2.Type = {}));
  TextElement2.typeNames = ["Section", "Escape", "Newline", "Splice"];
  class Section extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 0 /* Section */;
    }
    static read(view, address) {
      return new Section(readOffset(view, address));
    }
    get text() {
      return Token.TextSection.read(this._v, 0);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.text);
    }
  }
  TextElement2.Section = Section;
  class Escape extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 1 /* Escape */;
    }
    static read(view, address) {
      return new Escape(readOffset(view, address));
    }
    get token() {
      return Token.TextEscape.read(this._v, 0);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.token);
    }
  }
  TextElement2.Escape = Escape;
  class Newline extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 2 /* Newline */;
    }
    static read(view, address) {
      return new Newline(readOffset(view, address));
    }
    get newline() {
      return Token.Newline.read(this._v, 0);
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.newline);
    }
  }
  TextElement2.Newline = Newline;
  class Splice extends AbstractBase {
    type;
    constructor(view) {
      super(view);
      this.type = 3 /* Splice */;
    }
    static read(view, address) {
      return new Splice(readOffset(view, address));
    }
    get open() {
      return Token.OpenSymbol.read(this._v, 0);
    }
    get expression() {
      return readOption(this._v, 76, Tree.read);
    }
    get close() {
      return Token.CloseSymbol.read(this._v, 81);
    }
    visitExpression(visitor) {
      return visitOption(this._v, 76, (view, address) => visitor(Tree.read(view, address)));
    }
    visitChildren(visitor) {
      return super.visitChildren(visitor) || !!visitor(this.open) || !!this.visitExpression(visitor) || !!visitor(this.close);
    }
  }
  TextElement2.Splice = Splice;
  const VARIANT_READERS = [Section.read, Escape.read, Newline.read, Splice.read];
  function read(view, address) {
    return readEnum(VARIANT_READERS, view, address);
  }
  TextElement2.read = read;
  function isInstance(obj) {
    return obj instanceof AbstractBase;
  }
  TextElement2.isInstance = isInstance;
})(TextElement || (TextElement = {}));
var FractionalDigits = class _FractionalDigits extends LazyObject {
  constructor(view) {
    super(view);
  }
  static read(view, address) {
    return new _FractionalDigits(readOffset(view, address));
  }
  get dot() {
    return Token.Operator.read(this._v, 0);
  }
  get digits() {
    return Token.Digits.read(this._v, 76);
  }
  visitChildren(visitor) {
    return super.visitChildren(visitor) || !!visitor(this.dot) || !!visitor(this.digits);
  }
};
var MultipleOperatorError = class _MultipleOperatorError extends LazyObject {
  constructor(view) {
    super(view);
  }
  static read(view, address) {
    return new _MultipleOperatorError(readOffset(view, address));
  }
  get operators() {
    return readSequence(this._v, 0, 76, Token.Operator.read);
  }
  visitOperators(visitor) {
    return visitSequence(this._v, 0, 76, (view, address) => visitor(Token.Operator.read(view, address)));
  }
  visitChildren(visitor) {
    return super.visitChildren(visitor) || !!this.visitOperators(visitor);
  }
};
var OperatorDelimitedTree = class _OperatorDelimitedTree extends LazyObject {
  constructor(view) {
    super(view);
  }
  static read(view, address) {
    return new _OperatorDelimitedTree(readOffset(view, address));
  }
  get operator() {
    return Token.Operator.read(this._v, 0);
  }
  get body() {
    return readOption(this._v, 76, Tree.read);
  }
  visitBody(visitor) {
    return visitOption(this._v, 76, (view, address) => visitor(Tree.read(view, address)));
  }
  visitChildren(visitor) {
    return super.visitChildren(visitor) || !!visitor(this.operator) || !!this.visitBody(visitor);
  }
};
var ArgumentDefinition = class _ArgumentDefinition extends LazyObject {
  constructor(view) {
    super(view);
  }
  static read(view, address) {
    return new _ArgumentDefinition(readOffset(view, address));
  }
  get open() {
    return readOption(this._v, 0, Token.OpenSymbol.read);
  }
  get open2() {
    return readOption(this._v, 5, Token.OpenSymbol.read);
  }
  get suspension() {
    return readOption(this._v, 10, Token.Operator.read);
  }
  get pattern() {
    return Tree.read(this._v, 15);
  }
  get typeNode() {
    return readOption(this._v, 19, ArgumentType.read);
  }
  get close2() {
    return readOption(this._v, 24, Token.CloseSymbol.read);
  }
  get default() {
    return readOption(this._v, 29, ArgumentDefault.read);
  }
  get close() {
    return readOption(this._v, 34, Token.CloseSymbol.read);
  }
  visitOpen(visitor) {
    return visitOption(this._v, 0, (view, address) => visitor(Token.OpenSymbol.read(view, address)));
  }
  visitOpen2(visitor) {
    return visitOption(this._v, 5, (view, address) => visitor(Token.OpenSymbol.read(view, address)));
  }
  visitSuspension(visitor) {
    return visitOption(this._v, 10, (view, address) => visitor(Token.Operator.read(view, address)));
  }
  visitTypeNode(visitor) {
    return visitOption(this._v, 19, (view, address) => visitor(ArgumentType.read(view, address)));
  }
  visitClose2(visitor) {
    return visitOption(this._v, 24, (view, address) => visitor(Token.CloseSymbol.read(view, address)));
  }
  visitDefault(visitor) {
    return visitOption(this._v, 29, (view, address) => visitor(ArgumentDefault.read(view, address)));
  }
  visitClose(visitor) {
    return visitOption(this._v, 34, (view, address) => visitor(Token.CloseSymbol.read(view, address)));
  }
  visitChildren(visitor) {
    return super.visitChildren(visitor) || !!this.visitOpen(visitor) || !!this.visitOpen2(visitor) || !!this.visitSuspension(visitor) || !!visitor(this.pattern) || !!this.visitTypeNode(visitor) || !!this.visitClose2(visitor) || !!this.visitDefault(visitor) || !!this.visitClose(visitor);
  }
};
var DocComment = class _DocComment extends LazyObject {
  constructor(view) {
    super(view);
  }
  static read(view, address) {
    return new _DocComment(readOffset(view, address));
  }
  get open() {
    return Token.TextStart.read(this._v, 0);
  }
  get elements() {
    return readSequence(this._v, 76, 4, TextElement.read);
  }
  get newlines() {
    return readSequence(this._v, 80, 76, Token.Newline.read);
  }
  visitElements(visitor) {
    return visitSequence(this._v, 76, 4, (view, address) => visitor(TextElement.read(view, address)));
  }
  visitNewlines(visitor) {
    return visitSequence(this._v, 80, 76, (view, address) => visitor(Token.Newline.read(view, address)));
  }
  visitChildren(visitor) {
    return super.visitChildren(visitor) || !!visitor(this.open) || !!this.visitElements(visitor) || !!this.visitNewlines(visitor);
  }
};

// shared/ast/parse.ts
import * as map from "file:///C:/Projects/enso/enso/node_modules/.pnpm/lib0@0.2.93/node_modules/lib0/map.js";

// shared/util/data/iterable.ts
function* zip(left, right) {
  const leftIterator = left[Symbol.iterator]();
  const rightIterator = right[Symbol.iterator]();
  while (true) {
    const leftResult = leftIterator.next();
    const rightResult = rightIterator.next();
    if (leftResult.done || rightResult.done)
      break;
    yield [leftResult.value, rightResult.value];
  }
}
function tryGetSoleValue(iter) {
  const iterator = iter[Symbol.iterator]();
  const result = iterator.next();
  if (result.done)
    return;
  const excessResult = iterator.next();
  if (!excessResult.done)
    return;
  return result.value;
}
var Resumable = class {
  iterator;
  current;
  constructor(iterable) {
    this.iterator = iterable[Symbol.iterator]();
    this.current = this.iterator.next();
  }
  /** The given function peeks at the current value. If the function returns `true`, the current value will be advanced
   *  and the function called again; if it returns `false`, the peeked value remains current and `advanceWhile` returns.
   */
  advanceWhile(f) {
    while (!this.current.done && f(this.current.value)) {
      this.current = this.iterator.next();
    }
  }
  /** Apply the given function to all values remaining in the iterator. */
  forEach(f) {
    while (!this.current.done) {
      f(this.current.value);
      this.current = this.iterator.next();
    }
  }
};

// shared/util/data/text.ts
import diff from "file:///C:/Projects/enso/enso/node_modules/.pnpm/fast-diff@1.3.0/node_modules/fast-diff/diff.js";
function applyTextEdits(oldText, textEdits) {
  textEdits.sort((a, b) => a.range[0] - b.range[0]);
  let start = 0;
  let newText = "";
  for (const textEdit of textEdits) {
    newText += oldText.slice(start, textEdit.range[0]);
    newText += textEdit.insert;
    start = textEdit.range[1];
  }
  newText += oldText.slice(start);
  return newText;
}
function textChangeToEdits(before, after) {
  const textEdits = [];
  let nextEdit;
  let pos = 0;
  for (const [op, text] of diff(before, after)) {
    switch (op) {
      case diff.INSERT:
        if (!nextEdit)
          nextEdit = { range: [pos, pos], insert: "" };
        nextEdit.insert = text;
        break;
      case diff.EQUAL:
        if (nextEdit) {
          textEdits.push(nextEdit);
          nextEdit = void 0;
        }
        pos += text.length;
        break;
      case diff.DELETE: {
        if (nextEdit)
          textEdits.push(nextEdit);
        const endPos = pos + text.length;
        nextEdit = { range: [pos, endPos], insert: "" };
        pos = endPos;
        break;
      }
    }
  }
  if (nextEdit)
    textEdits.push(nextEdit);
  return textEdits;
}
function applyTextEditsToSpans(textEdits, spansBefore) {
  const numerically = (a, b) => a - b;
  const starts = new Resumable(spansBefore.map(([start, _end]) => start).sort(numerically));
  const ends = new Resumable(spansBefore.map(([_start, end]) => end).sort(numerically));
  const startMap = /* @__PURE__ */ new Map();
  const endMap = /* @__PURE__ */ new Map();
  let offset = 0;
  for (const { range, insert } of textEdits) {
    starts.advanceWhile((start) => {
      if (start < range[0]) {
        startMap.set(start, start + offset);
        return true;
      } else if (start <= range[1]) {
        startMap.set(start, range[0] + offset + insert.length);
        return true;
      }
      return false;
    });
    ends.advanceWhile((end) => {
      if (end <= range[0]) {
        endMap.set(end, end + offset);
        return true;
      } else if (end <= range[1]) {
        endMap.set(end, range[0] + offset);
        return true;
      }
      return false;
    });
    offset += insert.length - rangeLength(range);
  }
  starts.forEach((start) => startMap.set(start, start + offset));
  ends.forEach((end) => endMap.set(end, end + offset));
  const spansBeforeAndAfter = new Array();
  for (const spanBefore of spansBefore) {
    const startAfter = startMap.get(spanBefore[0]);
    const endAfter = endMap.get(spanBefore[1]);
    if (endAfter > startAfter)
      spansBeforeAndAfter.push([spanBefore, [startAfter, endAfter]]);
  }
  return spansBeforeAndAfter;
}
function enclosingSpans(tree, ranges, resultsOut) {
  const results = resultsOut ?? [];
  for (const child of tree.children()) {
    const childSpan = child.span();
    const childRanges = [];
    ranges = ranges.filter((range) => {
      if (rangeEncloses(childSpan, range)) {
        childRanges.push(range);
        return false;
      }
      return true;
    });
    if (childRanges.length)
      enclosingSpans(child, childRanges, results);
  }
  if (ranges.length)
    results.push([tree.id(), ranges]);
  return results;
}
function trimEnd(range, text) {
  const trimmedLength = text.slice(range[0], range[1]).search(/ +$/);
  return trimmedLength === -1 ? range : [range[0], range[0] + trimmedLength];
}

// shared/ast/debug.ts
function graphParentPointers(ast) {
  const sanitize = (id) => id.replace("ast:", "").replace(/[^A-Za-z0-9]/g, "");
  const parentToChild = new Array();
  const childToParent = new Array();
  ast.visitRecursiveAst((ast2) => {
    for (const child of ast2.children()) {
      if (child instanceof Ast)
        parentToChild.push({ child: sanitize(child.id), parent: sanitize(ast2.id) });
    }
    const parent = ast2.parentId;
    if (parent)
      childToParent.push({ child: sanitize(ast2.id), parent: sanitize(parent) });
  });
  let result = "digraph parentPointers {\n";
  for (const { parent, child } of parentToChild)
    result += `${parent} -> ${child};
`;
  for (const { child, parent } of childToParent)
    result += `${child} -> ${parent} [weight=0; color=red; style=dotted];
`;
  result += "}\n";
  return result;
}

// shared/ast/mutableModule.ts
import * as random2 from "file:///C:/Projects/enso/enso/node_modules/.pnpm/lib0@0.2.93/node_modules/lib0/random.js";
import * as Y2 from "file:///C:/Projects/enso/enso/node_modules/.pnpm/yjs@13.6.14/node_modules/yjs/dist/yjs.mjs";
var MutableModule = class _MutableModule {
  nodes;
  get ydoc() {
    const ydoc = this.nodes.doc;
    assert(ydoc != null);
    return ydoc;
  }
  /** Return this module's copy of `ast`, if this module was created by cloning `ast`'s module. */
  getVersion(ast) {
    const instance = this.get(ast.id);
    return instance;
  }
  edit() {
    const doc = new Y2.Doc();
    Y2.applyUpdateV2(doc, Y2.encodeStateAsUpdateV2(this.ydoc));
    return new _MutableModule(doc);
  }
  applyEdit(edit, origin = defaultLocalOrigin) {
    Y2.applyUpdateV2(this.ydoc, Y2.encodeStateAsUpdateV2(edit.ydoc), origin);
  }
  transact(f, origin = defaultLocalOrigin) {
    return this.ydoc.transact(f, origin);
  }
  root() {
    return this.rootPointer()?.expression;
  }
  replaceRoot(newRoot) {
    if (newRoot) {
      const rootPointer = this.rootPointer();
      if (rootPointer) {
        return rootPointer.expression.replace(newRoot);
      } else {
        invalidFields(this, this.baseObject("Invalid", void 0, ROOT_ID), {
          whitespace: "",
          node: newRoot
        });
        return void 0;
      }
    } else {
      const oldRoot = this.root();
      if (!oldRoot)
        return;
      this.nodes.delete(ROOT_ID);
      oldRoot.fields.set("parent", void 0);
      return asOwned(oldRoot);
    }
  }
  syncRoot(root) {
    this.replaceRoot(root);
    this.gc();
  }
  syncToCode(code) {
    const root = this.root();
    if (root) {
      root.syncToCode(code);
    } else {
      this.replaceRoot(Ast.parse(code, this));
    }
  }
  /** Update the module according to changes to its corresponding source code. */
  applyTextEdits(textEdits, metadataSource) {
    const root = this.root();
    assertDefined(root);
    root.applyTextEdits(textEdits, metadataSource);
  }
  gc() {
    const live = /* @__PURE__ */ new Set();
    const active = new Array();
    let next = this.root();
    while (next) {
      for (const child of next.children()) {
        if (child instanceof Ast)
          active.push(child);
      }
      live.add(next.id);
      next = active.pop();
    }
    const all = Array.from(this.nodes.keys());
    for (const id of all) {
      if (id === ROOT_ID)
        continue;
      assert(isAstId(id));
      if (!live.has(id))
        this.nodes.delete(id);
    }
  }
  /** Copy the given node into the module. */
  copy(ast) {
    const id = newAstId(ast.typeName());
    const fields = ast.fields.clone();
    this.nodes.set(id, fields);
    fields.set("id", id);
    fields.set("parent", void 0);
    const ast_ = materializeMutable(this, fields);
    ast_.importReferences(ast.module);
    return ast_;
  }
  static Transient() {
    return new this(new Y2.Doc());
  }
  observe(observer) {
    const handle = (events, transaction) => {
      observer(this.observeEvents(events, tryAsOrigin(transaction.origin)));
    };
    this.nodes.observeDeep(handle);
    observer(this.getStateAsUpdate());
    return handle;
  }
  unobserve(handle) {
    this.nodes.unobserveDeep(handle);
  }
  getStateAsUpdate() {
    const updateBuilder = new UpdateBuilder(this, this.nodes, void 0);
    for (const id of this.nodes.keys())
      updateBuilder.addNode(id);
    return updateBuilder.finish();
  }
  applyUpdate(update, origin) {
    let summary;
    const observer = (events) => {
      summary = this.observeEvents(events, origin);
    };
    this.nodes.observeDeep(observer);
    Y2.applyUpdate(this.ydoc, update, origin);
    this.nodes.unobserveDeep(observer);
    return summary;
  }
  observeEvents(events, origin) {
    const updateBuilder = new UpdateBuilder(this, this.nodes, origin);
    for (const event of events) {
      if (event.target === this.nodes) {
        for (const [key, change] of event.changes.keys) {
          const id = key;
          switch (change.action) {
            case "add":
              updateBuilder.addNode(id);
              break;
            case "update":
              updateBuilder.updateAllFields(id);
              break;
            case "delete":
              updateBuilder.deleteNode(id);
              break;
          }
        }
      } else if (event.target.parent === this.nodes) {
        assert(event.target instanceof Y2.Map);
        const id = event.target.get("id");
        const node = this.nodes.get(id);
        if (!node)
          continue;
        const changes = Array.from(event.changes.keys, ([key]) => [
          key,
          node.get(key)
        ]);
        updateBuilder.updateFields(id, changes);
      } else if (event.target.parent.parent === this.nodes) {
        const id = event.target.parent.get("id");
        const node = this.nodes.get(id);
        if (!node)
          continue;
        const metadata2 = node.get("metadata");
        const changes = Array.from(event.changes.keys, ([key]) => [
          key,
          metadata2.get(key)
        ]);
        updateBuilder.updateMetadata(id, changes);
      }
    }
    return updateBuilder.finish();
  }
  clear() {
    this.nodes.clear();
  }
  get(id) {
    if (!id)
      return void 0;
    const ast = this.tryGet(id);
    assert(ast !== void 0, "id in module");
    return ast;
  }
  tryGet(id) {
    if (!id)
      return void 0;
    const nodeData = this.nodes.get(id);
    if (!nodeData)
      return void 0;
    const fields = nodeData;
    return materializeMutable(this, fields);
  }
  replace(id, value) {
    return this.tryGet(id)?.replace(value);
  }
  replaceValue(id, value) {
    return this.tryGet(id)?.replaceValue(value);
  }
  take(id) {
    return this.replace(id, Wildcard.new(this)) || asOwned(this.get(id));
  }
  updateValue(id, f) {
    return this.tryGet(id)?.updateValue(f);
  }
  /////////////////////////////////////////////
  constructor(doc) {
    this.nodes = doc.getMap("nodes");
  }
  rootPointer() {
    const rootPointer = this.tryGet(ROOT_ID);
    if (rootPointer)
      return rootPointer;
  }
  /** @internal */
  baseObject(type, externalId, overrideId) {
    const map3 = new Y2.Map();
    const map_ = map3;
    const id = overrideId ?? newAstId(type);
    const metadata2 = new Y2.Map();
    const metadataFields = setAll(metadata2, {
      externalId: externalId ?? newExternalId()
    });
    const fields = setAll(map_, {
      id,
      type,
      parent: void 0,
      metadata: metadataFields
    });
    const fieldObject = composeFieldData(fields, {});
    this.nodes.set(id, fieldObject);
    return fieldObject;
  }
  getToken(token) {
    if (!token)
      return token;
    if (token instanceof Token2)
      return token;
    return Token2.withId(token.code_, token.tokenType_, token.id);
  }
  getAny(node) {
    return isTokenId(node) ? this.getToken(node) : this.get(node);
  }
  getConcrete(child) {
    if (isTokenId(child.node))
      return { whitespace: child.whitespace, node: this.getToken(child.node) };
    else
      return { whitespace: child.whitespace, node: this.get(child.node) };
  }
  copyIfForeign(ast) {
    if (!ast)
      return ast;
    if (ast.module === this)
      return ast;
    return this.copy(ast);
  }
  /** @internal */
  delete(id) {
    this.nodes.delete(id);
  }
  /** @internal */
  has(id) {
    return this.nodes.has(id);
  }
};
function newAstId(type) {
  return `ast:${type}#${random2.uint53()}`;
}
function isAstId(value) {
  return /ast:[A-Za-z]*#[0-9]*/.test(value);
}
var ROOT_ID = `Root`;
var UpdateBuilder = class {
  nodesAdded = /* @__PURE__ */ new Set();
  nodesDeleted = /* @__PURE__ */ new Set();
  nodesUpdated = /* @__PURE__ */ new Set();
  metadataUpdated = [];
  origin;
  module;
  nodes;
  constructor(module, nodes, origin) {
    this.module = module;
    this.nodes = nodes;
    this.origin = origin;
  }
  addNode(id) {
    this.nodesAdded.add(id);
  }
  updateAllFields(id) {
    this.updateFields(id, this.nodes.get(id).entries());
  }
  updateFields(id, changes) {
    let fieldsChanged = false;
    let metadataChanges = void 0;
    for (const entry of changes) {
      const [key, value] = entry;
      if (key === "metadata") {
        assert(value instanceof Y2.Map);
        metadataChanges = new Map(value.entries());
      } else {
        assert(!(value instanceof Y2.AbstractType));
        fieldsChanged = true;
      }
    }
    if (fieldsChanged)
      this.nodesUpdated.add(id);
    if (metadataChanges)
      this.metadataUpdated.push({ id, changes: metadataChanges });
  }
  updateMetadata(id, changes) {
    const changeMap = /* @__PURE__ */ new Map();
    for (const [key, value] of changes)
      changeMap.set(key, value);
    this.metadataUpdated.push({ id, changes: changeMap });
  }
  deleteNode(id) {
    this.nodesDeleted.add(id);
  }
  finish() {
    const dirtyNodes = new Set(this.nodesUpdated);
    this.nodesAdded.forEach((node) => dirtyNodes.add(node));
    const updateRoots = subtreeRoots(this.module, dirtyNodes);
    return { ...this, updateRoots };
  }
};

// shared/ast/parse.ts
function parseEnso(code) {
  const blob = parse(code);
  const tree = Tree.read(new DataView(blob.buffer), blob.byteLength - 4);
  assert(tree.type === Tree.Type.BodyBlock);
  return tree;
}
function abstract(module, tree, code, substitutor) {
  const abstractor = new Abstractor(module, code, substitutor);
  const root = abstractor.abstractTree(tree).node;
  const spans = { tokens: abstractor.tokens, nodes: abstractor.nodes };
  return { root, spans, toRaw: abstractor.toRaw };
}
var Abstractor = class {
  module;
  code;
  substitutor;
  nodes;
  tokens;
  toRaw;
  /**
   *  @param module - Where to allocate the new nodes.
   *  @param code - Source code that will be used to resolve references in any passed `RawAst` objects.
   *  @param substitutor - A function that can inject subtrees for some spans, instead of the abstractor producing them.
   *    This can be used for incremental abstraction.
   */
  constructor(module, code, substitutor) {
    this.module = module;
    this.code = code;
    this.substitutor = substitutor;
    this.nodes = /* @__PURE__ */ new Map();
    this.tokens = /* @__PURE__ */ new Map();
    this.toRaw = /* @__PURE__ */ new Map();
  }
  abstractTree(tree) {
    const whitespaceStart = tree.whitespaceStartInCodeParsed;
    const whitespaceEnd = whitespaceStart + tree.whitespaceLengthInCodeParsed;
    const whitespace = this.code.substring(whitespaceStart, whitespaceEnd);
    const codeStart = whitespaceEnd;
    const codeEnd = codeStart + tree.childrenLengthInCodeParsed;
    const spanKey = nodeKey(codeStart, codeEnd - codeStart);
    const substitute = this.substitutor?.(spanKey);
    if (substitute)
      return { node: substitute, whitespace };
    let node;
    switch (tree.type) {
      case Tree.Type.BodyBlock: {
        const lines = Array.from(tree.statements, (line) => {
          const newline = this.abstractToken(line.newline);
          const expression = line.expression ? this.abstractTree(line.expression) : void 0;
          return { newline, expression };
        });
        node = BodyBlock.concrete(this.module, lines);
        break;
      }
      case Tree.Type.Function: {
        const name = this.abstractTree(tree.name);
        const argumentDefinitions = Array.from(tree.args, (arg) => this.abstractChildren(arg));
        const equals = this.abstractToken(tree.equals);
        const body = tree.body !== void 0 ? this.abstractTree(tree.body) : void 0;
        node = Function.concrete(this.module, name, argumentDefinitions, equals, body);
        break;
      }
      case Tree.Type.Ident: {
        const token = this.abstractToken(tree.token);
        node = Ident.concrete(this.module, token);
        break;
      }
      case Tree.Type.Assignment: {
        const pattern = this.abstractTree(tree.pattern);
        const equals = this.abstractToken(tree.equals);
        const value = this.abstractTree(tree.expr);
        node = Assignment.concrete(this.module, pattern, equals, value);
        break;
      }
      case Tree.Type.App: {
        const func = this.abstractTree(tree.func);
        const arg = this.abstractTree(tree.arg);
        node = App.concrete(this.module, func, void 0, void 0, arg);
        break;
      }
      case Tree.Type.NamedApp: {
        const func = this.abstractTree(tree.func);
        const open = tree.open ? this.abstractToken(tree.open) : void 0;
        const name = this.abstractToken(tree.name);
        const equals = this.abstractToken(tree.equals);
        const arg = this.abstractTree(tree.arg);
        const close = tree.close ? this.abstractToken(tree.close) : void 0;
        const parens = open && close ? { open, close } : void 0;
        const nameSpecification2 = { name, equals };
        node = App.concrete(this.module, func, parens, nameSpecification2, arg);
        break;
      }
      case Tree.Type.UnaryOprApp: {
        const opr = this.abstractToken(tree.opr);
        const arg = tree.rhs ? this.abstractTree(tree.rhs) : void 0;
        if (arg && opr.node.code() === "-") {
          node = NegationApp.concrete(this.module, opr, arg);
        } else {
          node = UnaryOprApp.concrete(this.module, opr, arg);
        }
        break;
      }
      case Tree.Type.OprApp: {
        const lhs = tree.lhs ? this.abstractTree(tree.lhs) : void 0;
        const opr = tree.opr.ok ? [this.abstractToken(tree.opr.value)] : Array.from(tree.opr.error.payload.operators, this.abstractToken.bind(this));
        const rhs = tree.rhs ? this.abstractTree(tree.rhs) : void 0;
        const soleOpr = tryGetSoleValue(opr);
        if (soleOpr?.node.code() === "." && rhs?.node instanceof MutableIdent) {
          const rhs_ = { ...rhs, node: rhs.node };
          node = PropertyAccess.concrete(this.module, lhs, soleOpr, rhs_);
        } else {
          node = OprApp.concrete(this.module, lhs, opr, rhs);
        }
        break;
      }
      case Tree.Type.Number: {
        const tokens = [];
        if (tree.base)
          tokens.push(this.abstractToken(tree.base));
        if (tree.integer)
          tokens.push(this.abstractToken(tree.integer));
        if (tree.fractionalDigits) {
          tokens.push(this.abstractToken(tree.fractionalDigits.dot));
          tokens.push(this.abstractToken(tree.fractionalDigits.digits));
        }
        node = NumericLiteral.concrete(this.module, tokens);
        break;
      }
      case Tree.Type.Wildcard: {
        const token = this.abstractToken(tree.token);
        node = Wildcard.concrete(this.module, token);
        break;
      }
      case Tree.Type.OprSectionBoundary:
      case Tree.Type.TemplateFunction:
        return { whitespace, node: this.abstractTree(tree.ast).node };
      case Tree.Type.Invalid: {
        const expression = this.abstractTree(tree.ast);
        node = Invalid.concrete(this.module, expression);
        break;
      }
      case Tree.Type.Group: {
        const open = tree.open ? this.abstractToken(tree.open) : void 0;
        const expression = tree.body ? this.abstractTree(tree.body) : void 0;
        const close = tree.close ? this.abstractToken(tree.close) : void 0;
        node = Group.concrete(this.module, open, expression, close);
        break;
      }
      case Tree.Type.TextLiteral: {
        const open = tree.open ? this.abstractToken(tree.open) : void 0;
        const newline = tree.newline ? this.abstractToken(tree.newline) : void 0;
        const elements = Array.from(tree.elements, (raw) => this.abstractTextElement(raw));
        const close = tree.close ? this.abstractToken(tree.close) : void 0;
        node = TextLiteral.concrete(this.module, open, newline, elements, close);
        break;
      }
      case Tree.Type.Documented: {
        const open = this.abstractToken(tree.documentation.open);
        const elements = Array.from(
          tree.documentation.elements,
          (raw) => this.abstractTextToken(raw)
        );
        const newlines = Array.from(tree.documentation.newlines, this.abstractToken.bind(this));
        const expression = tree.expression ? this.abstractTree(tree.expression) : void 0;
        node = Documented.concrete(this.module, open, elements, newlines, expression);
        break;
      }
      case Tree.Type.Import: {
        const recurseBody = (tree2) => {
          const body = this.abstractTree(tree2);
          if (body.node instanceof Invalid && body.node.code() === "")
            return void 0;
          return body;
        };
        const recurseSegment = (segment) => ({
          header: this.abstractToken(segment.header),
          body: segment.body ? recurseBody(segment.body) : void 0
        });
        const polyglot = tree.polyglot ? recurseSegment(tree.polyglot) : void 0;
        const from = tree.from ? recurseSegment(tree.from) : void 0;
        const import_ = recurseSegment(tree.import);
        const all = tree.all ? this.abstractToken(tree.all) : void 0;
        const as = tree.as ? recurseSegment(tree.as) : void 0;
        const hiding = tree.hiding ? recurseSegment(tree.hiding) : void 0;
        node = Import.concrete(this.module, polyglot, from, import_, all, as, hiding);
        break;
      }
      case Tree.Type.Array: {
        const left = this.abstractToken(tree.left);
        const elements = [];
        if (tree.first)
          elements.push({ value: this.abstractTree(tree.first) });
        for (const rawElement of tree.rest) {
          elements.push({
            delimiter: this.abstractToken(rawElement.operator),
            value: rawElement.body && this.abstractTree(rawElement.body)
          });
        }
        const right = this.abstractToken(tree.right);
        node = Vector.concrete(this.module, left, elements, right);
        break;
      }
      default: {
        node = Generic.concrete(this.module, this.abstractChildren(tree));
      }
    }
    this.toRaw.set(node.id, tree);
    map.setIfUndefined(this.nodes, spanKey, () => []).unshift(node);
    return { node, whitespace };
  }
  abstractToken(token) {
    const whitespaceStart = token.whitespaceStartInCodeBuffer;
    const whitespaceEnd = whitespaceStart + token.whitespaceLengthInCodeBuffer;
    const whitespace = this.code.substring(whitespaceStart, whitespaceEnd);
    const codeStart = token.startInCodeBuffer;
    const codeEnd = codeStart + token.lengthInCodeBuffer;
    const tokenCode = this.code.substring(codeStart, codeEnd);
    const key = tokenKey(codeStart, codeEnd - codeStart);
    const node = Token2.new(tokenCode, token.type);
    this.tokens.set(key, node);
    return { whitespace, node };
  }
  abstractChildren(tree) {
    const children = [];
    const visitor = (child) => {
      if (Tree.isInstance(child)) {
        children.push(this.abstractTree(child));
      } else if (Token.isInstance(child)) {
        children.push(this.abstractToken(child));
      } else {
        child.visitChildren(visitor);
      }
    };
    tree.visitChildren(visitor);
    return children;
  }
  abstractTextElement(raw) {
    switch (raw.type) {
      case TextElement.Type.Newline:
      case TextElement.Type.Escape:
      case TextElement.Type.Section:
        return this.abstractTextToken(raw);
      case TextElement.Type.Splice:
        return {
          type: "splice",
          open: this.abstractToken(raw.open),
          expression: raw.expression && this.abstractTree(raw.expression),
          close: this.abstractToken(raw.close)
        };
    }
  }
  abstractTextToken(raw) {
    switch (raw.type) {
      case TextElement.Type.Newline:
        return { type: "token", token: this.abstractToken(raw.newline) };
      case TextElement.Type.Escape: {
        const negativeOneU32 = 4294967295;
        return {
          type: "token",
          token: this.abstractToken(raw.token),
          interpreted: raw.token.value !== negativeOneU32 ? String.fromCodePoint(raw.token.value) : void 0
        };
      }
      case TextElement.Type.Section:
        return { type: "token", token: this.abstractToken(raw.text) };
      case TextElement.Type.Splice:
        throw new Error("Unreachable: Splice in non-interpolated text field");
    }
  }
};
function nodeKey(start, length2) {
  return sourceRangeKey([start, start + length2]);
}
function tokenKey(start, length2) {
  return sourceRangeKey([start, start + length2]);
}
function spanMapToIdMap(spans) {
  const idMap2 = new IdMap();
  for (const [key, token] of spans.tokens.entries()) {
    assert(isUuid(token.id));
    idMap2.insertKnownId(sourceRangeFromKey(key), token.id);
  }
  for (const [key, asts] of spans.nodes.entries()) {
    for (const ast of asts) {
      assert(isUuid(ast.externalId));
      idMap2.insertKnownId(sourceRangeFromKey(key), ast.externalId);
    }
  }
  return idMap2;
}
function print(ast) {
  const info = {
    nodes: /* @__PURE__ */ new Map(),
    tokens: /* @__PURE__ */ new Map()
  };
  const code = ast.printSubtree(info, 0, void 0);
  return { info, code };
}
function printAst(ast, info, offset, parentIndent, verbatim) {
  let code = "";
  for (const child of ast.concreteChildren(verbatim)) {
    if (!isTokenId(child.node) && ast.module.get(child.node) === void 0)
      continue;
    if (child.whitespace != null) {
      code += child.whitespace;
    } else if (code.length != 0) {
      code += " ";
    }
    if (isTokenId(child.node)) {
      const tokenStart = offset + code.length;
      const token = ast.module.getToken(child.node);
      const span2 = tokenKey(tokenStart, token.code().length);
      info.tokens.set(span2, token);
      code += token.code();
    } else {
      const childNode = ast.module.get(child.node);
      code += childNode.printSubtree(info, offset + code.length, parentIndent, verbatim);
      assertEqual(childNode.id, child.node);
      if (parentId(childNode) !== ast.id) {
        console.error(
          `Inconsistent parent pointer (expected ${ast.id})`,
          childNode,
          graphParentPointers(ast.module.root())
        );
      }
      assertEqual(parentId(childNode), ast.id);
    }
  }
  const span = nodeKey(offset, code.length);
  map.setIfUndefined(info.nodes, span, () => []).unshift(ast);
  return code;
}
function printBlock(block, info, offset, parentIndent, verbatim) {
  let blockIndent;
  let code = "";
  for (const line of block.fields.get("lines")) {
    code += line.newline.whitespace ?? "";
    const newlineCode = block.module.getToken(line.newline.node).code();
    if (offset || code || newlineCode.startsWith("#")) {
      code += newlineCode || "\n";
    }
    if (line.expression) {
      if (blockIndent === void 0) {
        if ((line.expression.whitespace?.length ?? 0) > (parentIndent?.length ?? 0)) {
          blockIndent = line.expression.whitespace;
        } else if (parentIndent !== void 0) {
          blockIndent = parentIndent + "    ";
        } else {
          blockIndent = "";
        }
      }
      const validIndent = (line.expression.whitespace?.length ?? 0) > (parentIndent?.length ?? 0);
      code += validIndent ? line.expression.whitespace : blockIndent;
      const lineNode = block.module.get(line.expression.node);
      assertEqual(lineNode.id, line.expression.node);
      assertEqual(parentId(lineNode), block.id);
      code += lineNode.printSubtree(info, offset + code.length, blockIndent, verbatim);
    }
  }
  const span = nodeKey(offset, code.length);
  map.setIfUndefined(info.nodes, span, () => []).unshift(block);
  return code;
}
function printDocumented(documented, info, offset, parentIndent, verbatim) {
  const open = documented.fields.get("open");
  const topIndent = parentIndent ?? open.whitespace ?? "";
  let code = "";
  code += open.node.code_;
  const minWhitespaceLength = topIndent.length + 1;
  let preferredWhitespace = topIndent + "  ";
  documented.fields.get("elements").forEach(({ token }, i) => {
    if (i === 0) {
      const whitespace = token.whitespace ?? " ";
      code += whitespace;
      code += token.node.code_;
      preferredWhitespace += whitespace;
    } else if (token.node.tokenType_ === Token.Type.TextSection) {
      if (token.whitespace && (verbatim || token.whitespace.length >= minWhitespaceLength))
        code += token.whitespace;
      else
        code += preferredWhitespace;
      code += token.node.code_;
    } else {
      code += token.whitespace ?? "";
      code += token.node.code_;
    }
  });
  code += documented.fields.get("newlines").map(({ whitespace, node }) => (whitespace ?? "") + node.code_).join("");
  if (documented.expression) {
    code += documented.fields.get("expression")?.whitespace ?? topIndent;
    code += documented.expression.printSubtree(info, offset + code.length, topIndent, verbatim);
  }
  const span = nodeKey(offset, code.length);
  map.setIfUndefined(info.nodes, span, () => []).unshift(documented);
  return code;
}
function parseBlock(code, inModule) {
  return parseBlockWithSpans(code, inModule).root;
}
function parse2(code, module) {
  const module_ = module ?? MutableModule.Transient();
  const ast = parseBlock(code, module_);
  const soleStatement = tryGetSoleValue(ast.statements());
  if (!soleStatement)
    return ast;
  const parent = parentId(soleStatement);
  if (parent)
    module_.delete(parent);
  soleStatement.fields.set("parent", void 0);
  return asOwned(soleStatement);
}
function parseBlockWithSpans(code, inModule) {
  const tree = parseEnso(code);
  const module = inModule ?? MutableModule.Transient();
  return abstract(module, tree, code);
}
function astCount(ast) {
  let count = 0;
  ast.visitRecursiveAst((_subtree) => {
    count += 1;
  });
  return count;
}
function setExternalIds(edit, spans, ids) {
  let astsMatched = 0;
  for (const [key, externalId] of ids.entries()) {
    const asts = spans.nodes.get(key);
    if (asts) {
      for (const ast of asts) {
        astsMatched += 1;
        const editAst = edit.getVersion(ast);
        if (editAst.externalId !== externalId)
          editAst.setExternalId(externalId);
      }
    }
  }
  return astsMatched;
}
function checkSpans(expected, encountered, code) {
  const lost = new Array();
  for (const [key, asts] of expected) {
    const outermostPrinted = asts[0];
    if (!outermostPrinted)
      continue;
    for (let i = 1; i < asts.length; ++i)
      assertEqual(asts[i]?.parentId, asts[i - 1]?.id);
    const encounteredAsts = encountered.get(key);
    if (encounteredAsts === void 0)
      lost.push([key, outermostPrinted]);
  }
  const lostInline = new Array();
  const lostBlock = new Array();
  for (const [key, ast] of lost) {
    const [start, end] = sourceRangeFromKey(key);
    (code.substring(start, end).match(/[\r\n]/) ? lostBlock : lostInline).push(ast);
  }
  return { lostInline, lostBlock };
}
function repair(root, module) {
  const printed = print(root);
  const reparsed = parseBlockWithSpans(printed.code);
  const { lostInline, lostBlock } = checkSpans(
    printed.info.nodes,
    reparsed.spans.nodes,
    printed.code
  );
  if (lostInline.length === 0) {
    if (lostBlock.length !== 0) {
      console.warn(`repair: Bad block elements, but all inline elements OK?`);
      const fixes2 = module ?? root.module.edit();
      resync(lostBlock, printed.info.nodes, reparsed.spans.nodes, fixes2);
      return { code: printed.code, fixes: fixes2 };
    }
    return { code: printed.code, fixes: void 0 };
  }
  const fixes = module ?? root.module.edit();
  for (const ast of lostInline) {
    if (ast instanceof Group)
      continue;
    fixes.getVersion(ast).update((ast2) => Group.new(fixes, ast2));
  }
  const printed2 = print(fixes.getVersion(root));
  const reparsed2 = parseBlockWithSpans(printed2.code);
  const { lostInline: lostInline2, lostBlock: lostBlock2 } = checkSpans(
    printed2.info.nodes,
    reparsed2.spans.nodes,
    printed2.code
  );
  if (lostInline2.length !== 0 || lostBlock2.length !== 0)
    resync([...lostInline2, ...lostBlock2], printed2.info.nodes, reparsed2.spans.nodes, fixes);
  return { code: printed2.code, fixes };
}
function resync(badAsts, badSpans, goodSpans, edit) {
  const parentsOfBadSubtrees = /* @__PURE__ */ new Set();
  const badAstIds = new Set(Array.from(badAsts, (ast) => ast.id));
  for (const id of subtreeRoots(edit, badAstIds)) {
    const parent = edit.get(id)?.parentId;
    if (parent)
      parentsOfBadSubtrees.add(parent);
  }
  const spanOfBadParent = new Array();
  for (const [span, asts] of badSpans) {
    for (const ast of asts) {
      if (parentsOfBadSubtrees.has(ast.id))
        spanOfBadParent.push([ast.id, span]);
    }
  }
  assertEqual(spanOfBadParent.length, parentsOfBadSubtrees.size);
  for (const [id, span] of spanOfBadParent) {
    const parent = edit.get(id);
    const goodAst = goodSpans.get(span)?.[0];
    assertDefined(goodAst);
    parent.syncToCode(goodAst.code());
  }
  console.warn(
    `repair: Replaced ${parentsOfBadSubtrees.size} subtrees with their reparsed equivalents.`,
    parentsOfBadSubtrees
  );
}
function hashSubtreeSyntax(ast, hashesOut) {
  let content = "";
  content += ast.typeName + ":";
  for (const child of ast.concreteChildren()) {
    content += child.whitespace ?? "?";
    if (isTokenId(child.node)) {
      content += "Token:" + hashString(ast.module.getToken(child.node).code());
    } else {
      content += hashSubtreeSyntax(ast.module.get(child.node), hashesOut);
    }
  }
  const astHash = hashString(content);
  map.setIfUndefined(hashesOut, astHash, () => []).unshift(ast);
  return astHash;
}
function hashString(input) {
  return xxHash128(input);
}
function syntaxHash(root) {
  const hashes = /* @__PURE__ */ new Map();
  const rootHash = hashSubtreeSyntax(root, hashes);
  return { root: rootHash, hashes };
}
function rawBlockToInline(tree) {
  if (tree.type !== Tree.Type.BodyBlock)
    return tree;
  return tryGetSoleValue(tree.statements)?.expression ?? tree;
}
function syncToCode(ast, code, metadataSource) {
  const codeBefore = ast.code();
  const textEdits = textChangeToEdits(codeBefore, code);
  applyTextEditsToAst(ast, textEdits, metadataSource ?? ast.module);
}
function calculateCorrespondence(ast, astSpans, parsedRoot, parsedSpans, textEdits, codeAfter) {
  const newSpans = /* @__PURE__ */ new Map();
  for (const [key, asts] of parsedSpans) {
    for (const ast2 of asts)
      newSpans.set(ast2.id, sourceRangeFromKey(key));
  }
  const toSync = /* @__PURE__ */ new Map();
  const candidates = /* @__PURE__ */ new Map();
  const allSpansBefore = Array.from(astSpans.keys(), sourceRangeFromKey);
  const spansBeforeAndAfter = applyTextEditsToSpans(textEdits, allSpansBefore).map(
    ([before, after]) => [before, trimEnd(after, codeAfter)]
  );
  const partAfterToAstBefore = /* @__PURE__ */ new Map();
  for (const [spanBefore, partAfter] of spansBeforeAndAfter) {
    const astBefore = astSpans.get(sourceRangeKey(spanBefore))?.[0];
    partAfterToAstBefore.set(sourceRangeKey(partAfter), astBefore);
  }
  const matchingPartsAfter = spansBeforeAndAfter.map(([_before, after]) => after);
  const parsedSpanTree = new AstWithSpans(parsedRoot, (id) => newSpans.get(id));
  const astsMatchingPartsAfter = enclosingSpans(parsedSpanTree, matchingPartsAfter);
  for (const [astAfter, partsAfter] of astsMatchingPartsAfter) {
    for (const partAfter of partsAfter) {
      const astBefore = partAfterToAstBefore.get(sourceRangeKey(partAfter));
      if (astBefore.typeName() === astAfter.typeName()) {
        ;
        (rangeLength(newSpans.get(astAfter.id)) === rangeLength(partAfter) ? toSync : candidates).set(astBefore.id, astAfter);
        break;
      }
    }
  }
  const oldIdsMatched = /* @__PURE__ */ new Set();
  const newIdsMatched = /* @__PURE__ */ new Set();
  for (const [oldId, newAst] of toSync) {
    oldIdsMatched.add(oldId);
    newIdsMatched.add(newAst.id);
  }
  const newHashes = syntaxHash(parsedRoot).hashes;
  const oldHashes = syntaxHash(ast).hashes;
  for (const [hash, newAsts] of newHashes) {
    const unmatchedNewAsts = newAsts.filter((ast2) => !newIdsMatched.has(ast2.id));
    const unmatchedOldAsts = oldHashes.get(hash)?.filter((ast2) => !oldIdsMatched.has(ast2.id)) ?? [];
    for (const [unmatchedNew, unmatchedOld] of zip(unmatchedNewAsts, unmatchedOldAsts)) {
      toSync.set(unmatchedOld.id, unmatchedNew);
      oldIdsMatched.add(unmatchedOld.id);
      newIdsMatched.add(unmatchedNew.id);
    }
  }
  for (const [beforeId, after] of candidates) {
    if (oldIdsMatched.has(beforeId) || newIdsMatched.has(after.id))
      continue;
    toSync.set(beforeId, after);
  }
  return toSync;
}
function applyTextEditsToAst(ast, textEdits, metadataSource) {
  const printed = print(ast);
  const code = applyTextEdits(printed.code, textEdits);
  const rawParsedBlock = parseEnso(code);
  const rawParsed = ast instanceof MutableBodyBlock ? rawParsedBlock : rawBlockToInline(rawParsedBlock);
  const parsed = abstract(ast.module, rawParsed, code);
  const toSync = calculateCorrespondence(
    ast,
    printed.info.nodes,
    parsed.root,
    parsed.spans.nodes,
    textEdits,
    code
  );
  syncTree(ast, parsed.root, toSync, ast.module, metadataSource);
}
function syncTree(target, newContent, toSync, edit, metadataSource) {
  const newIdToEquivalent = /* @__PURE__ */ new Map();
  for (const [beforeId, after] of toSync)
    newIdToEquivalent.set(after.id, beforeId);
  const childReplacerFor = (parentId3) => (id) => {
    const original = newIdToEquivalent.get(id);
    if (original) {
      const replacement = edit.get(original);
      if (replacement.parentId !== parentId3)
        replacement.fields.set("parent", parentId3);
      return original;
    } else {
      const child = edit.get(id);
      if (child.parentId !== parentId3)
        child.fields.set("parent", parentId3);
    }
  };
  const parentId2 = target.fields.get("parent");
  assertDefined(parentId2);
  const parent = edit.get(parentId2);
  const targetSyncEquivalent = toSync.get(target.id);
  const syncRoot = targetSyncEquivalent?.id === newContent.id ? targetSyncEquivalent : void 0;
  if (!syncRoot) {
    parent.replaceChild(target.id, newContent);
    newContent.fields.set("metadata", target.fields.get("metadata").clone());
    target.fields.get("metadata").set("externalId", newExternalId());
  }
  const newRoot = syncRoot ? target : newContent;
  newRoot.visitRecursiveAst((ast) => {
    const syncFieldsFrom = toSync.get(ast.id);
    const editAst = edit.getVersion(ast);
    if (syncFieldsFrom) {
      const originalAssignmentExpression = ast instanceof Assignment ? metadataSource.get(ast.fields.get("expression").node) : void 0;
      syncFields(edit.getVersion(ast), syncFieldsFrom, childReplacerFor(ast.id));
      if (editAst instanceof MutableAssignment && originalAssignmentExpression) {
        if (editAst.expression.externalId !== originalAssignmentExpression.externalId)
          editAst.expression.setExternalId(originalAssignmentExpression.externalId);
        syncNodeMetadata(
          editAst.expression.mutableNodeMetadata(),
          originalAssignmentExpression.nodeMetadata
        );
      }
    } else {
      rewriteRefs(editAst, childReplacerFor(ast.id));
    }
    return true;
  });
  return newRoot;
}
var AstWithSpans = class _AstWithSpans {
  ast;
  getSpan;
  constructor(ast, getSpan) {
    this.ast = ast;
    this.getSpan = getSpan;
  }
  id() {
    return this.ast;
  }
  span() {
    return this.getSpan(this.ast.id);
  }
  *children() {
    for (const child of this.ast.children()) {
      if (child instanceof Ast)
        yield new _AstWithSpans(child, this.getSpan);
    }
  }
};

// shared/ast/tree.ts
function allKeys(keys) {
  return Object.keys(keys);
}
var astFieldKeys = allKeys({
  id: null,
  type: null,
  parent: null,
  metadata: null
});
var Ast = class {
  module;
  /** @internal */
  fields;
  get id() {
    return this.fields.get("id");
  }
  get externalId() {
    const id = this.fields.get("metadata").get("externalId");
    assert(id != null);
    return id;
  }
  get nodeMetadata() {
    const metadata2 = this.fields.get("metadata");
    return metadata2;
  }
  typeName() {
    return this.fields.get("type");
  }
  /**
   * Return whether `this` and `other` are the same object, possibly in different modules.
   */
  is(other) {
    return this.id === other.id;
  }
  innerExpression() {
    return this;
  }
  code() {
    return print(this).code;
  }
  visitRecursive(visit) {
    visit(this);
    for (const child of this.children()) {
      if (isToken(child)) {
        visit(child);
      } else {
        child.visitRecursive(visit);
      }
    }
  }
  visitRecursiveAst(visit) {
    if (visit(this) === false)
      return;
    for (const child of this.children()) {
      if (!isToken(child))
        child.visitRecursiveAst(visit);
    }
  }
  printSubtree(info, offset, parentIndent, verbatim) {
    return printAst(this, info, offset, parentIndent, verbatim);
  }
  /** Returns child subtrees, without information about the whitespace between them. */
  *children() {
    for (const child of this.concreteChildren()) {
      if (isTokenId(child.node)) {
        yield this.module.getToken(child.node);
      } else {
        const node = this.module.get(child.node);
        if (node)
          yield node;
      }
    }
  }
  get parentId() {
    const parentId2 = this.fields.get("parent");
    if (parentId2 !== ROOT_ID)
      return parentId2;
  }
  parent() {
    return this.module.get(this.parentId);
  }
  static parseBlock(source, inModule) {
    return parseBlock(source, inModule);
  }
  static parse(source, module) {
    return parse2(source, module);
  }
  ////////////////////
  constructor(module, fields) {
    this.module = module;
    this.fields = fields;
  }
};
var MutableAst3 = class extends Ast {
  setExternalId(id) {
    this.fields.get("metadata").set("externalId", id);
  }
  mutableNodeMetadata() {
    const metadata2 = this.fields.get("metadata");
    return metadata2;
  }
  setNodeMetadata(nodeMeta) {
    const metadata2 = this.fields.get("metadata");
    for (const [key, value] of Object.entries(nodeMeta))
      if (value !== void 0)
        metadata2.set(key, value);
  }
  /** Modify the parent of this node to refer to a new object instead. Return the object, which now has no parent. */
  replace(replacement) {
    const parentId2 = this.fields.get("parent");
    if (parentId2) {
      const parent = this.module.get(parentId2);
      parent.replaceChild(this.id, replacement);
      this.fields.set("parent", void 0);
    }
    return asOwned(this);
  }
  /** Change the value of the object referred to by the `target` ID. (The initial ID of `replacement` will be ignored.)
   *  Returns the old value, with a new (unreferenced) ID.
   */
  replaceValue(replacement) {
    const replacement_ = this.module.copyIfForeign(replacement);
    const old = this.replace(replacement_);
    replacement_.fields.set("metadata", old.fields.get("metadata").clone());
    old.setExternalId(newExternalId());
    return old;
  }
  replaceValueChecked(replacement) {
    const parentId2 = this.fields.get("parent");
    assertDefined(parentId2);
    return this.replaceValue(replacement);
  }
  /** Replace the parent of this object with a reference to a new placeholder object.
   *  Returns the object, now parentless, and the placeholder. */
  takeToReplace() {
    if (parentId(this)) {
      const placeholder = Wildcard.new(this.module);
      const node = this.replace(placeholder);
      return { node, placeholder };
    } else {
      return { node: asOwned(this), placeholder: void 0 };
    }
  }
  /** Replace the parent of this object with a reference to a new placeholder object.
   *  Returns the object, now parentless. */
  take() {
    return this.replace(Wildcard.new(this.module));
  }
  takeIfParented() {
    const parent = parentId(this);
    if (parent) {
      const parentAst = this.module.get(parent);
      const placeholder = Wildcard.new(this.module);
      parentAst.replaceChild(this.id, placeholder);
      this.fields.set("parent", void 0);
    }
    return asOwned(this);
  }
  /** Replace the value assigned to the given ID with a placeholder.
   *  Returns the removed value, with a new unreferenced ID.
   **/
  takeValue() {
    const placeholder = Wildcard.new(this.module);
    const node = this.replaceValue(placeholder);
    return { node, placeholder };
  }
  /** Take this node from the tree, and replace it with the result of applying the given function to it.
   *
   *  Note that this is a modification of the *parent* node. Any `Ast` objects or `AstId`s that pointed to the old value
   *  will still point to the old value.
   */
  update(f) {
    const taken = this.takeToReplace();
    assertDefined(taken.placeholder, "To replace an `Ast`, it must have a parent.");
    const replacement = f(taken.node);
    taken.placeholder.replace(replacement);
    return replacement;
  }
  /** Take this node from the tree, and replace it with the result of applying the given function to it; transfer the
   *  metadata from this node to the replacement.
   *
   *  Note that this is a modification of the *parent* node. Any `Ast` objects or `AstId`s that pointed to the old value
   *  will still point to the old value.
   */
  updateValue(f) {
    const taken = this.takeValue();
    assertDefined(taken.placeholder, "To replace an `Ast`, it must have a parent.");
    const replacement = f(taken.node);
    taken.placeholder.replaceValue(replacement);
    return replacement;
  }
  mutableParent() {
    const parentId2 = this.fields.get("parent");
    if (parentId2 === "ROOT_ID")
      return;
    return this.module.get(parentId2);
  }
  /** Modify this tree to represent the given code, while minimizing changes from the current set of `Ast`s. */
  syncToCode(code, metadataSource) {
    syncToCode(this, code, metadataSource);
  }
  /** Update the AST according to changes to its corresponding source code. */
  applyTextEdits(textEdits, metadataSource) {
    applyTextEditsToAst(this, textEdits, metadataSource ?? this.module);
  }
  ///////////////////
  /** @internal */
  importReferences(module) {
    if (module === this.module)
      return;
    for (const child of this.concreteChildren()) {
      if (!isTokenId(child.node)) {
        const childInForeignModule = module.get(child.node);
        assert(childInForeignModule !== void 0);
        const importedChild = this.module.copy(childInForeignModule);
        importedChild.fields.set("parent", void 0);
        this.replaceChild(child.node, asOwned(importedChild));
      }
    }
  }
  /** @internal */
  replaceChild(target, replacement) {
    const replacementId = this.claimChild(replacement);
    const changes = rewriteRefs(this, (id) => id === target ? replacementId : void 0);
    assertEqual(changes, 1);
  }
  claimChild(child) {
    return child ? claimChild(this.module, child, this.id) : void 0;
  }
};
function* fieldDataEntries(map3) {
  for (const entry of map3.entries()) {
    if (!astFieldKeys.includes(entry[0]))
      yield entry;
  }
}
function idRewriter(f) {
  return (field) => {
    if (typeof field !== "object")
      return;
    if (!("node" in field))
      return;
    if (isTokenId(field.node))
      return;
    const newId = f(field.node);
    if (!newId)
      return;
    return { whitespace: field.whitespace, node: newId };
  };
}
function rewriteRefs(ast, f) {
  let fieldsChanged = 0;
  for (const [key, value] of fieldDataEntries(ast.fields)) {
    const newValue = rewriteFieldRefs(value, idRewriter(f));
    if (newValue !== void 0) {
      ast.fields.set(key, newValue);
      fieldsChanged += 1;
    }
  }
  return fieldsChanged;
}
function syncFields(ast1, ast2, f) {
  for (const [key, value] of fieldDataEntries(ast2.fields)) {
    const newValue = mapRefs(value, idRewriter(f));
    if (!fieldEqual(ast1.fields.get(key), newValue))
      ast1.fields.set(key, newValue);
  }
}
function syncNodeMetadata(target, source) {
  const oldPos = target.get("position");
  const newPos = source.get("position");
  if (oldPos?.x !== newPos?.x || oldPos?.y !== newPos?.y)
    target.set("position", newPos);
  const newVis = source.get("visualization");
  if (!visMetadataEquals(target.get("visualization"), newVis))
    target.set("visualization", newVis);
}
function rewriteFieldRefs(field, f) {
  const newValue = f(field);
  if (newValue)
    return newValue;
  if (typeof field !== "object")
    return;
  if ("forEach" in field) {
    const newValues = /* @__PURE__ */ new Map();
    field.forEach((subfield, i) => {
      const newValue2 = rewriteFieldRefs(subfield, f);
      if (newValue2 !== void 0)
        newValues.set(i, newValue2);
    });
    if (newValues.size)
      return Array.from(field, (oldValue, i) => newValues.get(i) ?? oldValue);
  } else {
    const fieldObject = field;
    const newValues = /* @__PURE__ */ new Map();
    for (const [key, value] of Object.entries(fieldObject)) {
      const newValue2 = rewriteFieldRefs(value, f);
      if (newValue2 !== void 0)
        newValues.set(key, newValue2);
    }
    if (newValues.size)
      return Object.fromEntries(
        Object.entries(fieldObject).map(([key, oldValue]) => [key, newValues.get(key) ?? oldValue])
      );
  }
}
function mapRefs(field, f) {
  return rewriteFieldRefs(field, f) ?? field;
}
function fieldEqual(field1, field2) {
  if (typeof field1 !== "object")
    return field1 === field2;
  if (typeof field2 !== "object")
    return false;
  if ("node" in field1 && "node" in field2) {
    if (field1["whitespace"] !== field2["whitespace"])
      return false;
    if (isTokenId(field1.node) && isTokenId(field2.node))
      return Token2.equal(field1.node, field2.node);
    else
      return field1.node === field2.node;
  } else if ("node" in field1 || "node" in field2) {
    return false;
  } else if (Array.isArray(field1) && Array.isArray(field2)) {
    return field1.length === field2.length && field1.every((value1, i) => fieldEqual(value1, field2[i]));
  } else if (Array.isArray(field1) || Array.isArray(field2)) {
    return false;
  } else {
    const fieldObject1 = field1;
    const fieldObject2 = field2;
    const keys = /* @__PURE__ */ new Set();
    for (const key of Object.keys(fieldObject1))
      keys.add(key);
    for (const key of Object.keys(fieldObject2))
      keys.add(key);
    for (const key of keys)
      if (!fieldEqual(fieldObject1[key], fieldObject2[key]))
        return false;
    return true;
  }
}
function applyMixins(derivedCtor, constructors) {
  constructors.forEach((baseCtor) => {
    Object.getOwnPropertyNames(baseCtor.prototype).forEach((name) => {
      Object.defineProperty(
        derivedCtor.prototype,
        name,
        Object.getOwnPropertyDescriptor(baseCtor.prototype, name) || /* @__PURE__ */ Object.create(null)
      );
    });
  });
}
var App = class _App extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static tryParse(source, module) {
    const parsed = parse2(source, module);
    if (parsed instanceof MutableApp)
      return parsed;
  }
  static concrete(module, func, parens, nameSpecification2, argument) {
    const base = module.baseObject("App");
    const id_ = base.get("id");
    const fields = composeFieldData(base, {
      function: concreteChild(module, func, id_),
      parens,
      nameSpecification: nameSpecification2,
      argument: concreteChild(module, argument, id_)
    });
    return asOwned(new MutableApp(module, fields));
  }
  static new(module, func, argumentName, argument) {
    return _App.concrete(
      module,
      autospaced(func),
      void 0,
      nameSpecification(argumentName),
      autospaced(argument)
    );
  }
  static positional(func, argument, module) {
    return _App.new(module ?? MutableModule.Transient(), func, void 0, argument);
  }
  static PositionalSequence(func, args) {
    return args.reduce(
      (expression, argument) => _App.new(func.module, expression, void 0, argument),
      func
    );
  }
  get function() {
    return this.module.get(this.fields.get("function").node);
  }
  get argumentName() {
    return this.module.getToken(this.fields.get("nameSpecification")?.name.node);
  }
  get argument() {
    return this.module.get(this.fields.get("argument").node);
  }
  *concreteChildren(verbatim) {
    const { function: function_, parens, nameSpecification: nameSpecification2, argument } = getAll(this.fields);
    yield ensureUnspaced(function_, verbatim);
    const useParens = !!(parens && (nameSpecification2 || verbatim));
    const spacedEquals = useParens && !!nameSpecification2?.equals.whitespace;
    if (useParens)
      yield ensureSpaced(parens.open, verbatim);
    if (nameSpecification2) {
      yield useParens ? preferUnspaced(nameSpecification2.name) : ensureSpaced(nameSpecification2.name, verbatim);
      yield ensureSpacedOnlyIf(nameSpecification2.equals, spacedEquals, verbatim);
    }
    yield ensureSpacedOnlyIf(argument, !nameSpecification2 || spacedEquals, verbatim);
    if (useParens)
      yield preferUnspaced(parens.close);
  }
  printSubtree(info, offset, parentIndent, verbatim) {
    const verbatim_ = verbatim ?? (this.function instanceof Invalid || this.argument instanceof Invalid);
    return super.printSubtree(info, offset, parentIndent, verbatim_);
  }
};
function ensureSpacedOnlyIf(child, condition, verbatim) {
  return condition ? ensureSpaced(child, verbatim) : ensureUnspaced(child, verbatim);
}
function isConcrete(child) {
  return child.whitespace !== void 0;
}
function tryAsConcrete(child) {
  return isConcrete(child) ? child : void 0;
}
function ensureSpaced(child, verbatim) {
  const concreteInput = tryAsConcrete(child);
  if (verbatim && concreteInput)
    return concreteInput;
  return concreteInput?.whitespace ? concreteInput : { ...child, whitespace: " " };
}
function ensureUnspaced(child, verbatim) {
  const concreteInput = tryAsConcrete(child);
  if (verbatim && concreteInput)
    return concreteInput;
  return concreteInput?.whitespace === "" ? concreteInput : { ...child, whitespace: "" };
}
function preferSpacedIf(child, condition) {
  return condition ? preferSpaced(child) : preferUnspaced(child);
}
function preferUnspaced(child) {
  return tryAsConcrete(child) ?? { ...child, whitespace: "" };
}
function preferSpaced(child) {
  return tryAsConcrete(child) ?? { ...child, whitespace: " " };
}
var MutableApp = class extends App {
  setFunction(value) {
    setNode(this.fields, "function", this.claimChild(value));
  }
  setArgumentName(name) {
    this.fields.set("nameSpecification", nameSpecification(name));
  }
  setArgument(value) {
    setNode(this.fields, "argument", this.claimChild(value));
  }
};
applyMixins(MutableApp, [MutableAst3]);
var UnaryOprApp = class extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static tryParse(source, module) {
    const parsed = parse2(source, module);
    if (parsed instanceof MutableUnaryOprApp)
      return parsed;
  }
  static concrete(module, operator, argument) {
    const base = module.baseObject("UnaryOprApp");
    const id_ = base.get("id");
    const fields = composeFieldData(base, {
      operator,
      argument: concreteChild(module, argument, id_)
    });
    return asOwned(new MutableUnaryOprApp(module, fields));
  }
  static new(module, operator, argument) {
    return this.concrete(module, unspaced(operator), argument ? autospaced(argument) : void 0);
  }
  get operator() {
    return this.module.getToken(this.fields.get("operator").node);
  }
  get argument() {
    return this.module.get(this.fields.get("argument")?.node);
  }
  *concreteChildren(_verbatim) {
    const { operator, argument } = getAll(this.fields);
    yield operator;
    if (argument)
      yield argument;
  }
};
var MutableUnaryOprApp = class extends UnaryOprApp {
  setOperator(value) {
    this.fields.set("operator", unspaced(value));
  }
  setArgument(argument) {
    setNode(this.fields, "argument", this.claimChild(argument));
  }
};
applyMixins(MutableUnaryOprApp, [MutableAst3]);
var NegationApp = class extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static tryParse(source, module) {
    const parsed = parse2(source, module);
    if (parsed instanceof MutableNegationApp)
      return parsed;
  }
  static concrete(module, operator, argument) {
    const base = module.baseObject("NegationApp");
    const id_ = base.get("id");
    const fields = composeFieldData(base, {
      operator,
      argument: concreteChild(module, argument, id_)
    });
    return asOwned(new MutableNegationApp(module, fields));
  }
  static new(module, operator, argument) {
    return this.concrete(module, unspaced(operator), autospaced(argument));
  }
  get operator() {
    return this.module.getToken(this.fields.get("operator").node);
  }
  get argument() {
    return this.module.get(this.fields.get("argument").node);
  }
  *concreteChildren(_verbatim) {
    const { operator, argument } = getAll(this.fields);
    yield operator;
    if (argument)
      yield argument;
  }
};
var MutableNegationApp = class extends NegationApp {
  setArgument(value) {
    setNode(this.fields, "argument", this.claimChild(value));
  }
};
applyMixins(MutableNegationApp, [MutableAst3]);
var OprApp = class _OprApp extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static tryParse(source, module) {
    const parsed = parse2(source, module);
    if (parsed instanceof MutableOprApp)
      return parsed;
  }
  static concrete(module, lhs, operators, rhs) {
    const base = module.baseObject("OprApp");
    const id_ = base.get("id");
    const fields = composeFieldData(base, {
      lhs: concreteChild(module, lhs, id_),
      operators,
      rhs: concreteChild(module, rhs, id_)
    });
    return asOwned(new MutableOprApp(module, fields));
  }
  static new(module, lhs, operator, rhs) {
    const operatorToken = operator instanceof Token2 ? operator : Token2.new(operator, Token.Type.Operator);
    return _OprApp.concrete(module, unspaced(lhs), [autospaced(operatorToken)], autospaced(rhs));
  }
  get lhs() {
    return this.module.get(this.fields.get("lhs")?.node);
  }
  get operator() {
    const operators = this.fields.get("operators");
    const operators_ = operators.map((child) => ({
      ...child,
      node: this.module.getToken(child.node)
    }));
    const [opr] = operators_;
    return opr ? Ok(opr.node) : Err(operators_);
  }
  get rhs() {
    return this.module.get(this.fields.get("rhs")?.node);
  }
  *concreteChildren(_verbatim) {
    const { lhs, operators, rhs } = getAll(this.fields);
    if (lhs)
      yield lhs;
    yield* operators;
    if (rhs)
      yield rhs;
  }
};
var MutableOprApp = class extends OprApp {
  setLhs(value) {
    setNode(this.fields, "lhs", this.claimChild(value));
  }
  setOperator(value) {
    this.fields.set("operators", [unspaced(value)]);
  }
  setRhs(value) {
    setNode(this.fields, "rhs", this.claimChild(value));
  }
};
applyMixins(MutableOprApp, [MutableAst3]);
var PropertyAccess = class extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static tryParse(source, module) {
    const parsed = parse2(source, module);
    if (parsed instanceof MutablePropertyAccess)
      return parsed;
  }
  static new(module, lhs, rhs, style) {
    const dot = Token2.new(".", Token.Type.Operator);
    const whitespace = style?.spaced ? " " : "";
    return this.concrete(
      module,
      unspaced(lhs),
      { whitespace, node: dot },
      { whitespace, node: Ident.newAllowingOperators(module, toIdent(rhs)) }
    );
  }
  static Sequence(segments, module) {
    let path2;
    let operatorInNonFinalSegment = false;
    segments.forEach((s, i) => {
      const t = toIdent(s);
      if (i !== segments.length - 1 && !isIdentifier(t.code()))
        operatorInNonFinalSegment = true;
      path2 = path2 ? this.new(module, path2, t) : Ident.newAllowingOperators(module, t);
    });
    if (!operatorInNonFinalSegment)
      return path2;
  }
  static concrete(module, lhs, operator, rhs) {
    const base = module.baseObject("PropertyAccess");
    const id_ = base.get("id");
    const fields = composeFieldData(base, {
      lhs: concreteChild(module, lhs, id_),
      operator,
      rhs: concreteChild(module, rhs, id_)
    });
    return asOwned(new MutablePropertyAccess(module, fields));
  }
  get lhs() {
    return this.module.get(this.fields.get("lhs")?.node);
  }
  get operator() {
    return this.module.getToken(this.fields.get("operator").node);
  }
  get rhs() {
    const ast = this.module.get(this.fields.get("rhs").node);
    assert(ast instanceof Ident);
    return ast.token;
  }
  *concreteChildren(_verbatim) {
    const { lhs, operator, rhs } = getAll(this.fields);
    if (lhs)
      yield lhs;
    yield operator;
    yield rhs;
  }
};
var MutablePropertyAccess = class extends PropertyAccess {
  setLhs(value) {
    setNode(this.fields, "lhs", this.claimChild(value));
  }
  setRhs(ident) {
    const node = this.claimChild(Ident.newAllowingOperators(this.module, ident));
    const old = this.fields.get("rhs");
    this.fields.set("rhs", old ? { ...old, node } : unspaced(node));
  }
};
applyMixins(MutablePropertyAccess, [MutableAst3]);
var Generic = class extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static concrete(module, children) {
    const base = module.baseObject("Generic");
    const id_ = base.get("id");
    const fields = composeFieldData(base, {
      children: children.map((child) => concreteChild(module, child, id_))
    });
    return asOwned(new MutableGeneric(module, fields));
  }
  concreteChildren(_verbatim) {
    return this.fields.get("children")[Symbol.iterator]();
  }
};
var MutableGeneric = class extends Generic {
};
applyMixins(MutableGeneric, [MutableAst3]);
function multiSegmentAppSegment(header, body) {
  return {
    header: autospaced(Token2.new(header, Token.Type.Ident)),
    body: spaced(body ? body : void 0)
  };
}
function multiSegmentAppSegmentToRaw(module, msas, parent) {
  if (!msas)
    return void 0;
  return {
    ...msas,
    body: concreteChild(module, msas.body, parent)
  };
}
var Import = class extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static tryParse(source, module) {
    const parsed = parse2(source, module);
    if (parsed instanceof MutableImport)
      return parsed;
  }
  get polyglot() {
    return this.module.get(this.fields.get("polyglot")?.body?.node);
  }
  get from() {
    return this.module.get(this.fields.get("from")?.body?.node);
  }
  get import_() {
    return this.module.get(this.fields.get("import").body?.node);
  }
  get all() {
    return this.module.getToken(this.fields.get("all")?.node);
  }
  get as() {
    return this.module.get(this.fields.get("as")?.body?.node);
  }
  get hiding() {
    return this.module.get(this.fields.get("hiding")?.body?.node);
  }
  static concrete(module, polyglot, from, import_, all, as, hiding) {
    const base = module.baseObject("Import");
    const id_ = base.get("id");
    const ownedFields = {
      polyglot,
      from,
      import: import_,
      all,
      as,
      hiding
    };
    const rawFields = mapRefs(ownedFields, ownedToRaw(module, id_));
    const fields = composeFieldData(base, rawFields);
    return asOwned(new MutableImport(module, fields));
  }
  static Qualified(path2, module) {
    const path_ = PropertyAccess.Sequence(path2, module);
    if (!path_)
      return;
    return MutableImport.concrete(
      module,
      void 0,
      void 0,
      multiSegmentAppSegment("import", path_),
      void 0,
      void 0,
      void 0
    );
  }
  static Unqualified(path2, name, module) {
    const path_ = PropertyAccess.Sequence(path2, module);
    if (!path_)
      return;
    const name_ = Ident.newAllowingOperators(module, name);
    return MutableImport.concrete(
      module,
      void 0,
      multiSegmentAppSegment("from", path_),
      multiSegmentAppSegment("import", name_),
      void 0,
      void 0,
      void 0
    );
  }
  *concreteChildren(_verbatim) {
    const segment = (segment2) => {
      const parts = [];
      if (segment2)
        parts.push(segment2.header);
      if (segment2?.body)
        parts.push(segment2.body);
      return parts;
    };
    const { polyglot, from, import: import_, all, as, hiding } = getAll(this.fields);
    yield* segment(polyglot);
    yield* segment(from);
    yield* segment(import_);
    if (all)
      yield all;
    yield* segment(as);
    yield* segment(hiding);
  }
};
var MutableImport = class extends Import {
  toRaw(msas) {
    return multiSegmentAppSegmentToRaw(this.module, msas, this.id);
  }
  setPolyglot(value) {
    this.fields.set(
      "polyglot",
      value ? this.toRaw(multiSegmentAppSegment("polyglot", value)) : void 0
    );
  }
  setFrom(value) {
    this.fields.set("from", value ? this.toRaw(multiSegmentAppSegment("from", value)) : value);
  }
  setImport(value) {
    this.fields.set("import", this.toRaw(multiSegmentAppSegment("import", value)));
  }
  setAll(value) {
    this.fields.set("all", spaced(value));
  }
  setAs(value) {
    this.fields.set("as", this.toRaw(multiSegmentAppSegment("as", value)));
  }
  setHiding(value) {
    this.fields.set("hiding", this.toRaw(multiSegmentAppSegment("hiding", value)));
  }
};
applyMixins(MutableImport, [MutableAst3]);
function ownedToRaw(module, parentId2) {
  return (child) => {
    if (typeof child !== "object")
      return;
    if (!("node" in child))
      return;
    if (isToken(child.node))
      return;
    return { ...child, node: claimChild(module, child.node, parentId2) };
  };
}
function rawToConcrete(module) {
  return (child) => {
    if (typeof child !== "object")
      return;
    if (!("node" in child))
      return;
    if (isTokenId(child.node))
      return { ...child, node: module.getToken(child.node) };
    else
      return { ...child, node: module.get(child.node) };
  };
}
function concreteToOwned(module) {
  return (child) => {
    if (typeof child !== "object")
      return;
    if (!("node" in child))
      return;
    if (isTokenChild(child))
      return child;
    else
      return { ...child, node: module.copy(child.node) };
  };
}
function textElementValue(element) {
  switch (element.type) {
    case "token": {
      if (element.interpreted != null)
        return element.interpreted;
      if (element.token.node.tokenType_ === Token.Type.TextNewline)
        return "\n";
      return element.token.node.code();
    }
    case "splice": {
      let s = "";
      s += element.open.node.code();
      if (element.expression) {
        s += element.expression.whitespace ?? "";
        s += element.expression.node.code();
      }
      s += element.close.whitespace ?? "";
      s += element.close.node.code();
      return s;
    }
  }
}
function rawTextElementValue(raw, module) {
  return textElementValue(mapRefs(raw, rawToConcrete(module)));
}
function uninterpolatedText(elements, module) {
  return elements.reduce((s, e) => s + rawTextElementValue(e, module), "");
}
function fieldConcreteChildren(field) {
  const children = new Array();
  rewriteFieldRefs(field, (subfield) => {
    if (typeof subfield === "object" && "node" in subfield)
      children.push(subfield);
  });
  return children;
}
var TextLiteral = class _TextLiteral extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static tryParse(source, module) {
    const parsed = parse2(source, module);
    if (parsed instanceof MutableTextLiteral)
      return parsed;
  }
  static concrete(module, open, newline, elements, close) {
    const base = module.baseObject("TextLiteral");
    const id_ = base.get("id");
    const fields = composeFieldData(base, {
      open,
      newline,
      elements: elements.map((e) => mapRefs(e, ownedToRaw(module, id_))),
      close
    });
    return asOwned(new MutableTextLiteral(module, fields));
  }
  static new(rawText, module) {
    const escaped = escapeTextLiteral(rawText);
    const parsed = parse2(`'${escaped}'`, module);
    if (!(parsed instanceof MutableTextLiteral)) {
      console.error(`Failed to escape string for interpolated text`, rawText, escaped, parsed);
      const safeText = rawText.replaceAll(/[^-+A-Za-z0-9_. ]/g, "");
      return _TextLiteral.new(safeText, module);
    }
    return parsed;
  }
  /**
   * Return the literal value of the string with all escape sequences applied, but without
   * evaluating any interpolated expressions.
   */
  get rawTextContent() {
    return uninterpolatedText(this.fields.get("elements"), this.module);
  }
  *concreteChildren(_verbatim) {
    const { open, newline, elements, close } = getAll(this.fields);
    if (open)
      yield open;
    if (newline)
      yield newline;
    for (const e of elements)
      yield* fieldConcreteChildren(e);
    if (close)
      yield close;
  }
  boundaryTokenCode() {
    return (this.open || this.close)?.code();
  }
  isInterpolated() {
    const token = this.boundaryTokenCode();
    return token === "'" || token === "'''";
  }
  get open() {
    return this.module.getToken(this.fields.get("open")?.node);
  }
  get close() {
    return this.module.getToken(this.fields.get("close")?.node);
  }
  get elements() {
    return this.fields.get("elements").map((e) => mapRefs(e, rawToConcrete(this.module)));
  }
};
var MutableTextLiteral = class extends TextLiteral {
  setBoundaries(code) {
    this.fields.set("open", unspaced(Token2.new(code)));
    this.fields.set("close", unspaced(Token2.new(code)));
  }
  setElements(elements) {
    this.fields.set(
      "elements",
      elements.map((e) => mapRefs(e, ownedToRaw(this.module, this.id)))
    );
  }
  /**
   * Set literal value of the string. The code representation of assigned text will be automatically
   * transformed to use escape sequences when necessary.
   */
  setRawTextContent(rawText) {
    let boundary = this.boundaryTokenCode();
    const isInterpolated = this.isInterpolated();
    const mustBecomeInterpolated = !isInterpolated && (!boundary || rawText.match(/["\n\r]/));
    if (mustBecomeInterpolated) {
      boundary = "'";
      this.setBoundaries(boundary);
    }
    const literalContents = isInterpolated || mustBecomeInterpolated ? escapeTextLiteral(rawText) : rawText;
    const parsed = parse2(`${boundary}${literalContents}${boundary}`);
    assert(parsed instanceof TextLiteral);
    const elements = parsed.elements.map((e) => mapRefs(e, concreteToOwned(this.module)));
    this.setElements(elements);
  }
};
applyMixins(MutableTextLiteral, [MutableAst3]);
var Documented = class extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static tryParse(source, module) {
    const parsed = parse2(source, module);
    if (parsed instanceof MutableDocumented)
      return parsed;
  }
  static new(text, expression) {
    return this.concrete(
      expression.module,
      void 0,
      textToUninterpolatedElements(text),
      void 0,
      autospaced(expression)
    );
  }
  static concrete(module, open, elements, newlines, expression) {
    const base = module.baseObject("Documented");
    const id_ = base.get("id");
    const fields = composeFieldData(base, {
      open: open ?? unspaced(Token2.new("##", Token.Type.Operator)),
      elements: elements.map((e) => mapRefs(e, ownedToRaw(module, id_))),
      newlines: newlines ?? [unspaced(Token2.new("\n", Token.Type.Newline))],
      expression: concreteChild(module, expression, id_)
    });
    return asOwned(new MutableDocumented(module, fields));
  }
  get expression() {
    return this.module.get(this.fields.get("expression")?.node);
  }
  /** Return the string value of the documentation. */
  documentation() {
    const raw = uninterpolatedText(this.fields.get("elements"), this.module);
    return raw.startsWith(" ") ? raw.slice(1) : raw;
  }
  *concreteChildren(_verbatim) {
    const { open, elements, newlines, expression } = getAll(this.fields);
    yield open;
    for (const { token } of elements)
      yield token;
    yield* newlines;
    if (expression)
      yield expression;
  }
  printSubtree(info, offset, parentIndent, verbatim) {
    return printDocumented(this, info, offset, parentIndent, verbatim);
  }
};
var MutableDocumented = class extends Documented {
  setDocumentationText(text) {
    this.fields.set(
      "elements",
      textToUninterpolatedElements(text).map(
        (owned) => mapRefs(owned, ownedToRaw(this.module, this.id))
      )
    );
  }
  setExpression(value) {
    this.fields.set("expression", unspaced(this.claimChild(value)));
  }
};
applyMixins(MutableDocumented, [MutableAst3]);
function textToUninterpolatedElements(text) {
  const elements = new Array();
  text.split("\n").forEach((line, i) => {
    if (i)
      elements.push({
        type: "token",
        token: unspaced(Token2.new("\n", Token.Type.TextNewline))
      });
    elements.push({
      type: "token",
      token: autospaced(Token2.new(line, Token.Type.TextSection))
    });
  });
  return elements;
}
var Invalid = class extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static concrete(module, expression) {
    const base = module.baseObject("Invalid");
    return asOwned(new MutableInvalid2(module, invalidFields(module, base, expression)));
  }
  get expression() {
    return this.module.get(this.fields.get("expression").node);
  }
  *concreteChildren(_verbatim) {
    yield this.fields.get("expression");
  }
  printSubtree(info, offset, parentIndent, _verbatim) {
    return super.printSubtree(info, offset, parentIndent, true);
  }
};
function invalidFields(module, base, expression) {
  const id_ = base.get("id");
  return composeFieldData(base, { expression: concreteChild(module, expression, id_) });
}
var MutableInvalid2 = class extends Invalid {
};
applyMixins(MutableInvalid2, [MutableAst3]);
var Group = class extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static tryParse(source, module) {
    const parsed = parse2(source, module);
    if (parsed instanceof MutableGroup)
      return parsed;
  }
  static concrete(module, open, expression, close) {
    const base = module.baseObject("Group");
    const id_ = base.get("id");
    const fields = composeFieldData(base, {
      open,
      expression: concreteChild(module, expression, id_),
      close
    });
    return asOwned(new MutableGroup(module, fields));
  }
  static new(module, expression) {
    const open = unspaced(Token2.new("(", Token.Type.OpenSymbol));
    const close = unspaced(Token2.new(")", Token.Type.CloseSymbol));
    return this.concrete(module, open, unspaced(expression), close);
  }
  get expression() {
    return this.module.get(this.fields.get("expression")?.node);
  }
  *concreteChildren(_verbatim) {
    const { open, expression, close } = getAll(this.fields);
    if (open)
      yield open;
    if (expression)
      yield expression;
    if (close)
      yield close;
  }
};
var MutableGroup = class extends Group {
  setExpression(value) {
    this.fields.set("expression", unspaced(this.claimChild(value)));
  }
};
applyMixins(MutableGroup, [MutableAst3]);
var NumericLiteral = class extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static tryParse(source, module) {
    const parsed = parse2(source, module);
    if (parsed instanceof MutableNumericLiteral)
      return parsed;
  }
  static concrete(module, tokens) {
    const base = module.baseObject("NumericLiteral");
    const fields = composeFieldData(base, { tokens });
    return asOwned(new MutableNumericLiteral(module, fields));
  }
  concreteChildren(_verbatim) {
    return this.fields.get("tokens")[Symbol.iterator]();
  }
};
var MutableNumericLiteral = class extends NumericLiteral {
};
applyMixins(MutableNumericLiteral, [MutableAst3]);
var Function = class extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static tryParse(source, module) {
    const parsed = parse2(source, module);
    if (parsed instanceof MutableFunction)
      return parsed;
  }
  get name() {
    return this.module.get(this.fields.get("name").node);
  }
  get body() {
    return this.module.get(this.fields.get("body")?.node);
  }
  get argumentDefinitions() {
    return this.fields.get("argumentDefinitions").map((raw) => raw.map((part) => this.module.getConcrete(part)));
  }
  static concrete(module, name, argumentDefinitions, equals, body) {
    const base = module.baseObject("Function");
    const id_ = base.get("id");
    const fields = composeFieldData(base, {
      name: concreteChild(module, name, id_),
      argumentDefinitions: argumentDefinitions.map((def) => mapRefs(def, ownedToRaw(module, id_))),
      equals,
      body: concreteChild(module, body, id_)
    });
    return asOwned(new MutableFunction(module, fields));
  }
  static new(module, name, argumentDefinitions, body) {
    return MutableFunction.concrete(
      module,
      unspaced(Ident.newAllowingOperators(module, name)),
      argumentDefinitions,
      spaced(makeEquals()),
      autospaced(body)
    );
  }
  /** Construct a function with simple (name-only) arguments and a body block. */
  static fromStatements(module, name, argumentNames, statements) {
    const statements_ = statements.map((statement) => ({
      expression: unspaced(statement)
    }));
    const argumentDefinitions = argumentNames.map((name2) => [spaced(Ident.new(module, name2))]);
    const body = BodyBlock.new(statements_, module);
    return MutableFunction.new(module, name, argumentDefinitions, body);
  }
  *bodyExpressions() {
    const body = this.body;
    if (body instanceof BodyBlock) {
      yield* body.statements();
    } else if (body) {
      yield body;
    }
  }
  *concreteChildren(_verbatim) {
    const { name, argumentDefinitions, equals, body } = getAll(this.fields);
    yield name;
    for (const def of argumentDefinitions)
      yield* def;
    yield { whitespace: equals.whitespace ?? " ", node: this.module.getToken(equals.node) };
    if (body)
      yield preferSpacedIf(body, this.module.tryGet(body.node) instanceof BodyBlock);
  }
};
var MutableFunction = class extends Function {
  setName(value) {
    this.fields.set("name", unspaced(this.claimChild(value)));
  }
  setBody(value) {
    this.fields.set("body", unspaced(this.claimChild(value)));
  }
  setArgumentDefinitions(defs) {
    this.fields.set(
      "argumentDefinitions",
      defs.map((def) => mapRefs(def, ownedToRaw(this.module, this.id)))
    );
  }
  /** Returns the body, after converting it to a block if it was empty or an inline expression. */
  bodyAsBlock() {
    const oldBody = this.body;
    if (oldBody instanceof MutableBodyBlock)
      return oldBody;
    const newBody = BodyBlock.new([], this.module);
    if (oldBody)
      newBody.push(oldBody.take());
    return newBody;
  }
};
applyMixins(MutableFunction, [MutableAst3]);
var Assignment = class _Assignment extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static tryParse(source, module) {
    const parsed = parse2(source, module);
    if (parsed instanceof MutableAssignment)
      return parsed;
  }
  static concrete(module, pattern, equals, expression) {
    const base = module.baseObject("Assignment");
    const id_ = base.get("id");
    const fields = composeFieldData(base, {
      pattern: concreteChild(module, pattern, id_),
      equals,
      expression: concreteChild(module, expression, id_)
    });
    return asOwned(new MutableAssignment(module, fields));
  }
  static new(module, ident, expression) {
    return _Assignment.concrete(
      module,
      unspaced(Ident.new(module, ident)),
      spaced(makeEquals()),
      spaced(expression)
    );
  }
  get pattern() {
    return this.module.get(this.fields.get("pattern").node);
  }
  get expression() {
    return this.module.get(this.fields.get("expression").node);
  }
  *concreteChildren(verbatim) {
    const { pattern, equals, expression } = getAll(this.fields);
    yield ensureUnspaced(pattern, verbatim);
    yield ensureSpacedOnlyIf(equals, expression.whitespace !== "", verbatim);
    yield preferSpaced(expression);
  }
};
var MutableAssignment = class extends Assignment {
  setPattern(value) {
    this.fields.set("pattern", unspaced(this.claimChild(value)));
  }
  setExpression(value) {
    setNode(this.fields, "expression", this.claimChild(value));
  }
};
applyMixins(MutableAssignment, [MutableAst3]);
var BodyBlock = class _BodyBlock extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static tryParse(source, module) {
    const parsed = parse2(source, module);
    if (parsed instanceof MutableBodyBlock)
      return parsed;
  }
  static concrete(module, lines) {
    const base = module.baseObject("BodyBlock");
    const id_ = base.get("id");
    const fields = composeFieldData(base, {
      lines: lines.map((line) => lineToRaw(line, module, id_))
    });
    return asOwned(new MutableBodyBlock(module, fields));
  }
  static new(lines, module) {
    return _BodyBlock.concrete(module, lines);
  }
  get lines() {
    return this.fields.get("lines").map((line) => lineFromRaw(line, this.module));
  }
  *statements() {
    for (const line of this.lines) {
      if (line.expression)
        yield line.expression.node;
    }
  }
  *concreteChildren(_verbatim) {
    for (const line of this.fields.get("lines")) {
      yield preferUnspaced(line.newline);
      if (line.expression)
        yield line.expression;
    }
  }
  printSubtree(info, offset, parentIndent, verbatim) {
    return printBlock(this, info, offset, parentIndent, verbatim);
  }
};
var MutableBodyBlock = class extends BodyBlock {
  updateLines(map3) {
    return this.setLines(map3(this.takeLines()));
  }
  takeLines() {
    return this.fields.get("lines").map((line) => ownedLineFromRaw(line, this.module));
  }
  setLines(lines) {
    this.fields.set(
      "lines",
      lines.map((line) => lineToRaw(line, this.module, this.id))
    );
  }
  /** Insert the given statement(s) starting at the specified line index. */
  insert(index, ...statements) {
    const before = this.fields.get("lines").slice(0, index);
    const insertions = statements.map((statement) => ({
      newline: unspaced(Token2.new("\n", Token.Type.Newline)),
      expression: statement && unspaced(this.claimChild(statement))
    }));
    const after = this.fields.get("lines").slice(index);
    this.fields.set("lines", [...before, ...insertions, ...after]);
  }
  push(statement) {
    const oldLines = this.fields.get("lines");
    const newLine = {
      newline: unspaced(Token2.new("\n", Token.Type.Newline)),
      expression: unspaced(this.claimChild(statement))
    };
    this.fields.set("lines", [...oldLines, newLine]);
  }
  filter(keep) {
    const oldLines = this.fields.get("lines");
    const filteredLines = oldLines.filter((line) => {
      if (!line.expression)
        return true;
      return keep(this.module.get(line.expression.node));
    });
    this.fields.set("lines", filteredLines);
  }
};
applyMixins(MutableBodyBlock, [MutableAst3]);
function lineFromRaw(raw, module) {
  const expression = raw.expression ? module.get(raw.expression.node) : void 0;
  return {
    newline: { ...raw.newline, node: module.getToken(raw.newline.node) },
    expression: expression ? {
      whitespace: raw.expression?.whitespace,
      node: expression
    } : void 0
  };
}
function ownedLineFromRaw(raw, module) {
  const expression = raw.expression ? module.get(raw.expression.node).takeIfParented() : void 0;
  return {
    newline: { ...raw.newline, node: module.getToken(raw.newline.node) },
    expression: expression ? {
      whitespace: raw.expression?.whitespace,
      node: expression
    } : void 0
  };
}
function lineToRaw(line, module, block) {
  return {
    newline: line.newline ?? unspaced(Token2.new("\n", Token.Type.Newline)),
    expression: line.expression ? {
      whitespace: line.expression?.whitespace,
      node: claimChild(module, line.expression.node, block)
    } : void 0
  };
}
var Ident = class _Ident extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static tryParse(source, module) {
    const parsed = parse2(source, module);
    if (parsed instanceof MutableIdent)
      return parsed;
  }
  get token() {
    return this.module.getToken(this.fields.get("token").node);
  }
  static concrete(module, token) {
    const base = module.baseObject("Ident");
    const fields = composeFieldData(base, { token });
    return asOwned(new MutableIdent(module, fields));
  }
  static new(module, ident) {
    return _Ident.concrete(module, unspaced(toIdentStrict(ident)));
  }
  /** @internal */
  static newAllowingOperators(module, ident) {
    return _Ident.concrete(module, unspaced(toIdent(ident)));
  }
  *concreteChildren(_verbatim) {
    yield this.fields.get("token");
  }
  code() {
    return this.token.code();
  }
};
var MutableIdent = class extends Ident {
  setToken(ident) {
    this.fields.set("token", unspaced(toIdent(ident)));
  }
  code() {
    return this.token.code();
  }
};
applyMixins(MutableIdent, [MutableAst3]);
var Wildcard = class extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static tryParse(source, module) {
    const parsed = parse2(source, module);
    if (parsed instanceof MutableWildcard)
      return parsed;
  }
  get token() {
    return this.module.getToken(this.fields.get("token").node);
  }
  static concrete(module, token) {
    const base = module.baseObject("Wildcard");
    const fields = composeFieldData(base, { token });
    return asOwned(new MutableWildcard(module, fields));
  }
  static new(module) {
    const token = Token2.new("_", Token.Type.Wildcard);
    return this.concrete(module ?? MutableModule.Transient(), unspaced(token));
  }
  *concreteChildren(_verbatim) {
    yield this.fields.get("token");
  }
};
var MutableWildcard = class extends Wildcard {
};
applyMixins(MutableWildcard, [MutableAst3]);
function delimitVectorElement(element) {
  return {
    ...element,
    delimiter: element.delimiter ?? unspaced(Token2.new(",", Token.Type.Operator))
  };
}
var Vector = class _Vector extends Ast {
  constructor(module, fields) {
    super(module, fields);
  }
  static tryParse(source, module) {
    const parsed = parse2(source, module);
    if (parsed instanceof MutableVector)
      return parsed;
  }
  static concrete(module, open, elements, close) {
    const base = module.baseObject("Vector");
    const id_ = base.get("id");
    const fields = composeFieldData(base, {
      open: open ?? unspaced(Token2.new("[", Token.Type.OpenSymbol)),
      elements: elements.map(delimitVectorElement).map((e) => mapRefs(e, ownedToRaw(module, id_))),
      close: close ?? unspaced(Token2.new("]", Token.Type.CloseSymbol))
    });
    return asOwned(new MutableVector(module, fields));
  }
  static new(module, elements) {
    return this.concrete(
      module,
      void 0,
      elements.map((value) => ({ value: autospaced(value) })),
      void 0
    );
  }
  static tryBuild(inputs, valueBuilder, edit) {
    const module = edit ?? MutableModule.Transient();
    const elements = new Array();
    for (const input of inputs) {
      const value = valueBuilder(input, module);
      if (!value)
        return;
      elements.push({ value: autospaced(value) });
    }
    return _Vector.concrete(module, void 0, elements, void 0);
  }
  static build(inputs, elementBuilder, edit) {
    return _Vector.tryBuild(inputs, elementBuilder, edit);
  }
  *concreteChildren(verbatim) {
    const { open, elements, close } = getAll(this.fields);
    yield ensureUnspaced(open, verbatim);
    let isFirst = true;
    for (const { delimiter, value } of elements) {
      if (isFirst && value) {
        yield preferUnspaced(value);
      } else {
        yield preferUnspaced(delimiter);
        if (value)
          yield preferSpaced(value);
      }
      isFirst = false;
    }
    yield preferUnspaced(close);
  }
  *values() {
    for (const element of this.fields.get("elements"))
      if (element.value)
        yield this.module.get(element.value.node);
  }
};
var MutableVector = class extends Vector {
  push(value) {
    const elements = this.fields.get("elements");
    const element = mapRefs(
      delimitVectorElement({ value: autospaced(value) }),
      ownedToRaw(this.module, this.id)
    );
    this.fields.set("elements", [...elements, element]);
  }
  keep(predicate) {
    const elements = this.fields.get("elements");
    const filtered = elements.filter(
      (element) => element.value && predicate(this.module.get(element.value.node))
    );
    this.fields.set("elements", filtered);
  }
};
applyMixins(MutableVector, [MutableAst3]);
function materializeMutable(module, fields) {
  const type = fields.get("type");
  const fieldsForType = fields;
  switch (type) {
    case "App":
      return new MutableApp(module, fieldsForType);
    case "Assignment":
      return new MutableAssignment(module, fieldsForType);
    case "BodyBlock":
      return new MutableBodyBlock(module, fieldsForType);
    case "Documented":
      return new MutableDocumented(module, fieldsForType);
    case "Function":
      return new MutableFunction(module, fieldsForType);
    case "Generic":
      return new MutableGeneric(module, fieldsForType);
    case "Group":
      return new MutableGroup(module, fieldsForType);
    case "Ident":
      return new MutableIdent(module, fieldsForType);
    case "Import":
      return new MutableImport(module, fieldsForType);
    case "Invalid":
      return new MutableInvalid2(module, fieldsForType);
    case "NegationApp":
      return new MutableNegationApp(module, fieldsForType);
    case "NumericLiteral":
      return new MutableNumericLiteral(module, fieldsForType);
    case "OprApp":
      return new MutableOprApp(module, fieldsForType);
    case "PropertyAccess":
      return new MutablePropertyAccess(module, fieldsForType);
    case "TextLiteral":
      return new MutableTextLiteral(module, fieldsForType);
    case "UnaryOprApp":
      return new MutableUnaryOprApp(module, fieldsForType);
    case "Vector":
      return new MutableVector(module, fieldsForType);
    case "Wildcard":
      return new MutableWildcard(module, fieldsForType);
  }
  bail(`Invalid type: ${type}`);
}
function getAll(map3) {
  return Object.fromEntries(map3.entries());
}
function setAll(map3, fields) {
  const map_ = map3;
  for (const [k, v] of Object.entries(fields)) {
    const k_ = k;
    map_.set(k_, v);
  }
  return map_;
}
function composeFieldData(map3, fields) {
  return setAll(map3, fields);
}
function claimChild(module, child, parent) {
  if (child.module === module)
    assertEqual(child.fields.get("parent"), void 0);
  const child_ = module.copyIfForeign(child);
  child_.fields.set("parent", parent);
  return child_.id;
}
function concreteChild(module, child, parent) {
  if (!child)
    return void 0;
  if (isTokenId(child.node))
    return child;
  return { ...child, node: claimChild(module, child.node, parent) };
}
function toIdentStrict(ident) {
  return ident ? isToken(ident) ? ident : Token2.new(ident, Token.Type.Ident) : void 0;
}
function toIdent(ident) {
  return ident ? isToken(ident) ? ident : Token2.new(ident, Token.Type.Ident) : void 0;
}
function makeEquals() {
  return Token2.new("=", Token.Type.Operator);
}
function nameSpecification(name) {
  return name && { name: autospaced(toIdentStrict(name)), equals: unspaced(makeEquals()) };
}
function setNode(map3, key, node) {
  const old = map3.get(key);
  const updated = old ? { ...old, node } : autospaced(node);
  map3.set(key, updated);
}
function spaced(node) {
  if (node === void 0)
    return node;
  return { whitespace: " ", node };
}
function unspaced(node) {
  if (node === void 0)
    return node;
  return { whitespace: "", node };
}
function autospaced(node) {
  if (node === void 0)
    return node;
  return { whitespace: void 0, node };
}

// shared/ast/text.ts
var escapeSequences = [
  ["0", "\0"],
  ["a", "\x07"],
  ["b", "\b"],
  ["f", "\f"],
  ["n", "\n"],
  ["r", "\r"],
  ["t", "	"],
  ["v", "\v"],
  ["e", "\x1B"],
  ["\\", "\\"],
  ['"', '"'],
  ["'", "'"],
  ["`", "`"]
];
function escapeAsCharCodes(str) {
  let out = "";
  for (let i = 0; i < str.length; i += 1)
    out += `\\u{${str?.charCodeAt(i).toString(16)}}`;
  return out;
}
var escapeRegex = new RegExp(
  `${escapeSequences.map(([_, raw]) => escapeAsCharCodes(raw)).join("|")}`,
  "gu"
);
var unescapeRegex = new RegExp(
  `\\\\(?:${escapeSequences.map(([escape]) => escapeAsCharCodes(escape)).join("|")}|x[0-9a-fA-F]{0,2}|u\\{[0-9a-fA-F]{0,4}\\}?|u[0-9a-fA-F]{0,4}|U[0-9a-fA-F]{0,8})`,
  "gu"
);
var escapeMapping = Object.fromEntries(
  escapeSequences.map(([escape, raw]) => [raw, `\\${escape}`])
);
var unescapeMapping = Object.fromEntries(
  escapeSequences.map(([escape, raw]) => [`\\${escape}`, raw])
);
function escapeTextLiteral(rawString) {
  return rawString.replace(escapeRegex, (match) => escapeMapping[match] ?? assertUnreachable());
}

// shared/ast/token.ts
function isToken(t) {
  return t instanceof Token2;
}
function isTokenChild(child) {
  return isToken(child.node);
}
function newTokenId() {
  return newExternalId();
}
var Token2 = class {
  id;
  code_;
  tokenType_;
  constructor(code, type, id) {
    this.id = id;
    this.code_ = code;
    this.tokenType_ = type;
  }
  get externalId() {
    return this.id;
  }
  static new(code, type) {
    return new this(code, type, newTokenId());
  }
  static withId(code, type, id) {
    assert(isUuid(id));
    return new this(code, type, id);
  }
  static equal(a, b) {
    return a.tokenType_ === b.tokenType_ && a.code_ === b.code_;
  }
  code() {
    return this.code_;
  }
  typeName() {
    if (this.tokenType_)
      return Token.typeNames[this.tokenType_];
    else
      return "Raw";
  }
};
function isIdentifier(code) {
  return is_ident_or_operator(code) === 1;
}
function isTokenId(t) {
  return typeof t === "object" && !(t instanceof Ast);
}

// shared/ast/index.ts
function asOwned(t) {
  return t;
}
function newExternalId() {
  return random3.uuidv4();
}
function parentId(ast) {
  return ast.fields.get("parent");
}
function subtreeRoots(module, ids) {
  const roots = /* @__PURE__ */ new Set();
  for (const id of ids) {
    const astInModule = module.tryGet(id);
    if (!astInModule)
      continue;
    let ast = astInModule.parent();
    let hasParentInSet;
    while (ast != null) {
      if (ids.has(ast.id)) {
        hasParentInSet = true;
        break;
      }
      ast = ast.parent();
    }
    if (!hasParentInSet)
      roots.add(id);
  }
  return roots;
}

// shared/ensoFile.ts
var META_TAG = "\n\n\n#### METADATA ####";
function splitFileContents(content) {
  const splitPoint = content.lastIndexOf(META_TAG);
  if (splitPoint < 0) {
    return {
      code: content,
      idMapJson: null,
      metadataJson: null
    };
  }
  const code = content.slice(0, splitPoint);
  const metadataString = content.slice(splitPoint + META_TAG.length);
  const metaLines = metadataString.trim().split("\n");
  const idMapJson = metaLines[0] ?? null;
  const metadataJson = metaLines[1] ?? null;
  return { code, idMapJson, metadataJson };
}
function combineFileParts(parts) {
  const hasMeta = parts.idMapJson != null || parts.metadataJson != null;
  if (hasMeta) {
    return `${parts.code}${META_TAG}
${parts.idMapJson ?? ""}
${parts.metadataJson ?? ""}`;
  } else {
    if (parts.code.includes(META_TAG)) {
      return `${parts.code}${META_TAG}`;
    } else {
      return parts.code;
    }
  }
}

// shared/languageServer.ts
import { sha3_224 as SHA3 } from "file:///C:/Projects/enso/enso/node_modules/.pnpm/@noble+hashes@1.4.0/node_modules/@noble/hashes/esm/sha3.js";
import { bytesToHex } from "file:///C:/Projects/enso/enso/node_modules/.pnpm/@noble+hashes@1.4.0/node_modules/@noble/hashes/esm/utils.js";
import { ObservableV2 } from "file:///C:/Projects/enso/enso/node_modules/.pnpm/lib0@0.2.93/node_modules/lib0/observable.js";
import { uuidv4 as uuidv43 } from "file:///C:/Projects/enso/enso/node_modules/.pnpm/lib0@0.2.93/node_modules/lib0/random.js";
import { z } from "file:///C:/Projects/enso/enso/node_modules/.pnpm/zod@3.22.4/node_modules/zod/lib/index.mjs";

// shared/languageServer/files.ts
async function walkFs(ls, path2, cb) {
  for (const file of (await ls.listFiles(path2)).paths) {
    const filePath = {
      rootId: file.path.rootId,
      segments: [...file.path.segments, file.name]
    };
    cb(file.type, filePath);
    switch (file.type) {
      case "Directory":
      case "DirectoryTruncated": {
        walkFs(ls, filePath, cb);
        break;
      }
      case "File":
      case "Other":
      case "SymlinkLoop": {
        break;
      }
      default: {
        const unexpected = file;
        throw new Error("Unexpected object: " + JSON.stringify(unexpected));
      }
    }
  }
}

// shared/languageServer.ts
var DEBUG_LOG_RPC = false;
var RPC_TIMEOUT_MS = 15e3;
var ErrorCode = /* @__PURE__ */ ((ErrorCode2) => {
  ErrorCode2[ErrorCode2["ACCESS_DENIED"] = 100] = "ACCESS_DENIED";
  ErrorCode2[ErrorCode2["FILE_SYSTEM_ERROR"] = 1e3] = "FILE_SYSTEM_ERROR";
  ErrorCode2[ErrorCode2["CONTENT_ROOT_NOT_FOUND"] = 1001] = "CONTENT_ROOT_NOT_FOUND";
  ErrorCode2[ErrorCode2["FILE_NOT_FOUND"] = 1003] = "FILE_NOT_FOUND";
  ErrorCode2[ErrorCode2["FILE_EXISTS"] = 1004] = "FILE_EXISTS";
  ErrorCode2[ErrorCode2["OPERATION_TIMEOUT"] = 1005] = "OPERATION_TIMEOUT";
  ErrorCode2[ErrorCode2["NOT_DIRECTORY"] = 1006] = "NOT_DIRECTORY";
  ErrorCode2[ErrorCode2["NOT_FILE"] = 1007] = "NOT_FILE";
  ErrorCode2[ErrorCode2["CANNOT_OVERWRITE"] = 1008] = "CANNOT_OVERWRITE";
  ErrorCode2[ErrorCode2["READ_OUT_OF_BOUNDS"] = 1009] = "READ_OUT_OF_BOUNDS";
  ErrorCode2[ErrorCode2["CANNOT_DECODE"] = 1010] = "CANNOT_DECODE";
  ErrorCode2[ErrorCode2["STACK_ITEM_NOT_FOUND"] = 2001] = "STACK_ITEM_NOT_FOUND";
  ErrorCode2[ErrorCode2["CONTEXT_NOT_FOUND"] = 2002] = "CONTEXT_NOT_FOUND";
  ErrorCode2[ErrorCode2["EMPTY_STACK"] = 2003] = "EMPTY_STACK";
  ErrorCode2[ErrorCode2["INVALID_STACK_ITEM"] = 2004] = "INVALID_STACK_ITEM";
  ErrorCode2[ErrorCode2["MODULE_NOT_FOUND"] = 2005] = "MODULE_NOT_FOUND";
  ErrorCode2[ErrorCode2["VISUALIZATION_NOT_FOUND"] = 2006] = "VISUALIZATION_NOT_FOUND";
  ErrorCode2[ErrorCode2["VISUALIZATION_EXPRESSION_ERROR"] = 2007] = "VISUALIZATION_EXPRESSION_ERROR";
  ErrorCode2[ErrorCode2["FILE_NOT_OPENED"] = 3001] = "FILE_NOT_OPENED";
  ErrorCode2[ErrorCode2["TEXT_EDIT_VALIDATION_ERROR"] = 3002] = "TEXT_EDIT_VALIDATION_ERROR";
  ErrorCode2[ErrorCode2["INVALID_VERSION"] = 3003] = "INVALID_VERSION";
  ErrorCode2[ErrorCode2["WRITE_DENIED"] = 3004] = "WRITE_DENIED";
  ErrorCode2[ErrorCode2["CAPABILITY_NOT_ACQUIRED"] = 5001] = "CAPABILITY_NOT_ACQUIRED";
  ErrorCode2[ErrorCode2["SESSION_NOT_INITIALIZED"] = 6001] = "SESSION_NOT_INITIALIZED";
  ErrorCode2[ErrorCode2["SESSION_ALREADY_INITIALIZED"] = 6002] = "SESSION_ALREADY_INITIALIZED";
  ErrorCode2[ErrorCode2["RESOURCES_INITIALIZATION_ERROR"] = 6003] = "RESOURCES_INITIALIZATION_ERROR";
  ErrorCode2[ErrorCode2["SUGGESTION_DATABASE_ERROR"] = 7001] = "SUGGESTION_DATABASE_ERROR";
  ErrorCode2[ErrorCode2["PROJECT_NOT_FOUND"] = 7002] = "PROJECT_NOT_FOUND";
  ErrorCode2[ErrorCode2["MODULE_NAME_NOT_RESOLVED"] = 7003] = "MODULE_NAME_NOT_RESOLVED";
  ErrorCode2[ErrorCode2["SUGGESTION_NOT_FOUND"] = 7004] = "SUGGESTION_NOT_FOUND";
  ErrorCode2[ErrorCode2["EDITION_NOT_FOUND"] = 8001] = "EDITION_NOT_FOUND";
  ErrorCode2[ErrorCode2["LIBRARY_ALREADY_EXISTS"] = 8002] = "LIBRARY_ALREADY_EXISTS";
  ErrorCode2[ErrorCode2["LIBRARY_REPOSITORY_AUTHENTICATION_ERROR"] = 8003] = "LIBRARY_REPOSITORY_AUTHENTICATION_ERROR";
  ErrorCode2[ErrorCode2["LIBRARY_PUBLISH_ERROR"] = 8004] = "LIBRARY_PUBLISH_ERROR";
  ErrorCode2[ErrorCode2["LIBRARY_UPLOAD_ERROR"] = 8005] = "LIBRARY_UPLOAD_ERROR";
  ErrorCode2[ErrorCode2["LIBRARY_DOWNLOAD_ERROR"] = 8006] = "LIBRARY_DOWNLOAD_ERROR";
  ErrorCode2[ErrorCode2["LOCAL_LIBRARY_NOT_FOUND"] = 8007] = "LOCAL_LIBRARY_NOT_FOUND";
  ErrorCode2[ErrorCode2["LIBRARY_NOT_RESOLVED"] = 8008] = "LIBRARY_NOT_RESOLVED";
  ErrorCode2[ErrorCode2["INVALID_LIBRARY_NAME"] = 8009] = "INVALID_LIBRARY_NAME";
  ErrorCode2[ErrorCode2["DEPENDENCY_DISCOVERY_ERROR"] = 8010] = "DEPENDENCY_DISCOVERY_ERROR";
  ErrorCode2[ErrorCode2["INVALID_SEMVER_VERSION"] = 8011] = "INVALID_SEMVER_VERSION";
  ErrorCode2[ErrorCode2["EXPRESSION_NOT_FOUND"] = 9001] = "EXPRESSION_NOT_FOUND";
  ErrorCode2[ErrorCode2["FAILED_TO_APPLY_EDITS"] = 9002] = "FAILED_TO_APPLY_EDITS";
  ErrorCode2[ErrorCode2["REFACTORING_NOT_SUPPORTED"] = 9003] = "REFACTORING_NOT_SUPPORTED";
  return ErrorCode2;
})(ErrorCode || {});
var RemoteRpcErrorSchema = z.object({
  code: z.nativeEnum(ErrorCode),
  message: z.string(),
  data: z.optional(z.any())
});
var RemoteRpcError = class {
  code;
  message;
  data;
  constructor(error) {
    this.code = error.code;
    this.message = error.message;
    this.data = error.data;
  }
};
var LsRpcError = class extends Error {
  cause;
  request;
  params;
  constructor(cause, request, params) {
    super(`Language server request '${request}' failed.`);
    this.cause = cause;
    this.request = request;
    this.params = params;
  }
};
var LanguageServer = class extends ObservableV2 {
  client;
  handlers;
  retainCount = 1;
  constructor(client) {
    super();
    this.client = client;
    this.handlers = /* @__PURE__ */ new Map();
    client.onNotification((notification) => {
      this.emit(notification.method, [notification.params]);
    });
    client.onError((error) => {
      console.error(`Unexpected LS connection error:`, error);
    });
  }
  // The "magic bag of holding" generic that is only present in the return type is UNSOUND.
  // However, it is SAFE, as the return type of the API is statically known.
  async request(method, params) {
    if (this.retainCount === 0)
      return Promise.reject(new Error("LanguageServer disposed"));
    const uuid = uuidv43();
    const now = performance.now();
    try {
      if (DEBUG_LOG_RPC) {
        console.log(`LS [${uuid}] ${method}:`);
        console.dir(params);
      }
      return await this.client.request({ method, params }, RPC_TIMEOUT_MS);
    } catch (error) {
      const remoteError = RemoteRpcErrorSchema.safeParse(error);
      if (remoteError.success) {
        throw new LsRpcError(new RemoteRpcError(remoteError.data), method, params);
      } else if (error instanceof Error) {
        throw new LsRpcError(error, method, params);
      }
      throw error;
    } finally {
      if (DEBUG_LOG_RPC) {
        console.log(`LS [${uuid}] ${method} took ${performance.now() - now}ms`);
      }
    }
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#capabilityacquire) */
  acquireCapability(method, registerOptions) {
    return this.request("capability/acquire", { method, registerOptions });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filereceivestreeupdates) */
  acquireReceivesTreeUpdates(path2) {
    return this.acquireCapability("file/receivesTreeUpdates", { path: path2 });
  }
  acquireExecutionContextCanModify(contextId) {
    return this.acquireCapability("executionContext/canModify", { contextId });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#sessioninitprotocolconnection) */
  initProtocolConnection(clientId) {
    return this.request("session/initProtocolConnection", { clientId });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#textopenfile) */
  openTextFile(path2) {
    return this.request("text/openFile", { path: path2 });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#textclosefile) */
  closeTextFile(path2) {
    return this.request("text/closeFile", { path: path2 });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#textsave) */
  saveTextFile(path2, currentVersion) {
    return this.request("text/save", { path: path2, currentVersion });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#textapplyedit) */
  applyEdit(edit, execute) {
    return this.request("text/applyEdit", { edit, execute });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filewrite) */
  writeFile(path2, contents) {
    return this.request("file/write", { path: path2, contents });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#fileread) */
  readFile(path2) {
    return this.request("file/read", { path: path2 });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filecreate) */
  createFile(object2) {
    return this.request("file/create", { object: object2 });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filedelete) */
  deleteFile(path2) {
    return this.request("file/delete", { path: path2 });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filecopy) */
  copyFile(from, to) {
    return this.request("file/copy", { from, to });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filemove) */
  moveFile(from, to) {
    return this.request("file/move", { from, to });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#fileexists) */
  fileExists(path2) {
    return this.request("file/exists", { path: path2 });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filetree) */
  fileTree(path2, depth) {
    return this.request("file/tree", { path: path2, depth });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filelist) */
  listFiles(path2) {
    return this.request("file/list", { path: path2 });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#fileinfo) */
  fileInfo(path2) {
    return this.request("file/info", { path: path2 });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filechecksum) */
  fileChecksum(path2) {
    return this.request("file/checksum", { path: path2 });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#vcsinit) */
  vcsInit(root) {
    return this.request("vcs/init", { root });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#vcssave) */
  vcsSave(root, name) {
    return this.request("vcs/save", { root, name });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#vcsstatus) */
  vcsStatus(root) {
    return this.request("vcs/status", { root });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#vcsrestore) */
  vcsRestore(root, commitId) {
    return this.request("vcs/restore", { root, commitId });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#vcslist) */
  vcsList(root, limit) {
    return this.request("vcs/list", { root, limit });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextcreate) */
  createExecutionContext(contextId) {
    return this.request("executionContext/create", { contextId });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextdestroy) */
  destroyExecutionContext(contextId) {
    return this.request("executionContext/destroy", { contextId });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextfork) */
  forkExecutionContext(contextId) {
    return this.request("executionContext/fork", { contextId });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextpush) */
  pushExecutionContextItem(contextId, stackItem) {
    return this.request("executionContext/push", { contextId, stackItem });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextpop) */
  popExecutionContextItem(contextId) {
    return this.request("executionContext/pop", { contextId });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextrecompute) */
  recomputeExecutionContext(contextId, invalidatedExpressions, executionEnvironment) {
    return this.request("executionContext/recompute", {
      contextId,
      invalidatedExpressions,
      executionEnvironment
    });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextinterrupt) */
  interruptExecutionContext(contextId) {
    return this.request("executionContext/interrupt", { contextId });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextsetexecutionenvironment) */
  setExecutionEnvironment(contextId, executionEnvironment) {
    return this.request("executionContext/setExecutionEnvironment", {
      contextId,
      executionEnvironment
    });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextexecuteexpression) */
  executeExpression(executionContextId, visualizationId, expressionId, expression) {
    return this.request("executionContext/executeExpression", {
      executionContextId,
      visualizationId,
      expressionId,
      expression
    });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextattachvisualization) */
  attachVisualization(visualizationId, expressionId, visualizationConfig) {
    return this.request("executionContext/attachVisualization", {
      visualizationId,
      expressionId,
      visualizationConfig
    });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextdetachvisualization) */
  detachVisualization(visualizationId, expressionId, contextId) {
    return this.request("executionContext/detachVisualization", {
      visualizationId,
      expressionId,
      contextId
    });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextmodifyvisualization) */
  modifyVisualization(visualizationId, visualizationConfig) {
    return this.request("executionContext/modifyVisualization", {
      visualizationId,
      visualizationConfig
    });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#searchgetsuggestionsdatabase) */
  getSuggestionsDatabase() {
    return this.request("search/getSuggestionsDatabase", {});
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#runtimegetcomponentgroups) */
  getComponentGroups() {
    return this.request("runtime/getComponentGroups", {});
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#profilingstart) */
  profilingStart(memorySnapshot) {
    return this.request("profiling/start", { memorySnapshot });
  }
  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#profilingstop) */
  profilingStop() {
    return this.request("profiling/stop", {});
  }
  aiCompletion(prompt, stopSequence) {
    return this.request("ai/completion", { prompt, stopSequence });
  }
  /** A helper function to subscribe to file updates.
   * Please use `ls.on('file/event')` directly if the initial `'Added'` notifications are not
   * needed. */
  watchFiles(rootId, segments, callback, retry = (f) => f()) {
    let running = true;
    const self = this;
    return {
      promise: (async () => {
        self.on("file/event", callback);
        await retry(async () => running && self.acquireReceivesTreeUpdates({ rootId, segments }));
        await walkFs(self, { rootId, segments }, (type, path2) => {
          if (!running || type !== "File" || path2.segments.length < segments.length || segments.some((segment, i) => segment !== path2.segments[i]))
            return;
          callback({
            path: { rootId: path2.rootId, segments: path2.segments.slice(segments.length) },
            kind: "Added"
          });
        });
      })(),
      unsubscribe() {
        running = false;
        self.off("file/event", callback);
      }
    };
  }
  retain() {
    if (this.retainCount === 0) {
      throw new Error("Trying to retain already disposed language server.");
    }
    this.retainCount += 1;
  }
  release() {
    if (this.retainCount > 0) {
      this.retainCount -= 1;
      if (this.retainCount === 0) {
        this.client.close();
      }
    } else {
      throw new Error("Released already disposed language server.");
    }
  }
};
function computeTextChecksum(text) {
  return bytesToHex(SHA3.create().update(text).digest());
}

// shared/retry.ts
import { wait } from "file:///C:/Projects/enso/enso/node_modules/.pnpm/lib0@0.2.93/node_modules/lib0/promise.js";
var defaultBackoffOptions = {
  maxRetries: 3,
  retryDelay: 1e3,
  retryDelayMultiplier: 2,
  retryDelayMax: 1e4,
  onBeforeRetry: () => {
  },
  onSuccess: () => {
  },
  onFailure: () => {
  }
};
async function exponentialBackoff(f, backoffOptions) {
  const {
    maxRetries,
    retryDelay,
    retryDelayMultiplier,
    retryDelayMax,
    onBeforeRetry,
    onSuccess,
    onFailure
  } = {
    ...defaultBackoffOptions,
    ...backoffOptions
  };
  for (let retries = 0, delay = retryDelay; ; retries += 1, delay = Math.min(retryDelayMax, delay * retryDelayMultiplier)) {
    try {
      const result = await f();
      onSuccess(retries);
      return result;
    } catch (error) {
      if (retries >= maxRetries) {
        onFailure(error, retries);
        throw error;
      }
      if (onBeforeRetry(error, retries, maxRetries, delay) === false)
        throw error;
      await wait(delay);
    }
  }
}
function defaultOnBeforeRetry(description) {
  return (error, retryCount, maxRetries, delay) => {
    console.error(
      "Could not " + description + ` (${retryCount}/${maxRetries} retries), retrying after ${delay}ms...`
    );
    console.error(error);
  };
}
function defaultOnFailure(description) {
  return (error, retryCount) => {
    console.error(
      "Could not " + description + ` (${retryCount}/${retryCount} retries), throwing error.`
    );
    console.error(error);
  };
}
function defaultOnSuccess(description) {
  return (retryCount) => {
    if (retryCount === 0)
      return;
    console.info(
      "Successfully " + description + ` after ${retryCount} ${retryCount === 1 ? "failure" : "failures"}.`
    );
  };
}
function printingCallbacks(successDescription, errorDescription) {
  return {
    onBeforeRetry: defaultOnBeforeRetry(errorDescription),
    onSuccess: defaultOnSuccess(successDescription),
    onFailure: defaultOnFailure(errorDescription)
  };
}

// shared/util/net.ts
var AbortScope = class {
  ctrl = new AbortController();
  get signal() {
    return this.ctrl.signal;
  }
  dispose(reason) {
    this.ctrl.abort(reason);
  }
  handleDispose(disposable) {
    this.signal.throwIfAborted();
    this.onAbort(disposable.dispose.bind(disposable));
  }
  onAbort(listener) {
    if (this.signal.aborted) {
      setTimeout(listener, 0);
    } else {
      this.signal.addEventListener("abort", listener, { once: true });
    }
  }
  handleObserve(observable, name, f) {
    if (this.signal.aborted)
      return;
    observable.on(name, f);
    this.onAbort(() => observable.off(name, f));
    return f;
  }
};

// ydoc-server/edits.ts
import diff2 from "file:///C:/Projects/enso/enso/node_modules/.pnpm/fast-diff@1.3.0/node_modules/fast-diff/diff.js";
var MAX_SIZE_FOR_NORMAL_DIFF = 3e4;
function applyDocumentUpdates(doc, synced, update) {
  const codeChanged = update.nodesUpdated.size || update.nodesAdded.size || update.nodesDeleted.size;
  let idsChanged = false;
  let metadataChanged = false;
  for (const { changes } of update.metadataUpdated) {
    for (const [key] of changes) {
      if (key === "externalId") {
        idsChanged = true;
      } else {
        metadataChanged = true;
      }
    }
    if (idsChanged && metadataChanged)
      break;
  }
  let newIdMap = void 0;
  let newCode = void 0;
  let newMetadata = void 0;
  const syncModule = new MutableModule(doc.ydoc);
  const root = syncModule.root();
  assert(root != null);
  if (codeChanged || idsChanged || synced.idMapJson == null) {
    const { code, info } = print(root);
    if (codeChanged)
      newCode = code;
    newIdMap = spanMapToIdMap(info);
  }
  if (codeChanged || idsChanged || metadataChanged) {
    newMetadata = {};
    root.visitRecursiveAst((ast) => {
      let pos = ast.nodeMetadata.get("position");
      const vis = ast.nodeMetadata.get("visualization");
      const colorOverride = ast.nodeMetadata.get("colorOverride");
      if (vis && !pos)
        pos = { x: 0, y: 0 };
      if (pos) {
        newMetadata[ast.externalId] = {
          position: { vector: [Math.round(pos.x), Math.round(-pos.y)] },
          visualization: vis && translateVisualizationToFile(vis),
          colorOverride
        };
      }
    });
  }
  return { newCode, newIdMap, newMetadata };
}
function translateVisualizationToFile(vis) {
  let project = void 0;
  switch (vis.identifier?.module.kind) {
    case "Builtin":
      project = { project: "Builtin" };
      break;
    case "CurrentProject":
      project = { project: "CurrentProject" };
      break;
    case "Library":
      project = { project: "Library", contents: vis.identifier.module.name };
      break;
  }
  return {
    show: vis.visible,
    fullscreen: vis.fullscreen,
    width: vis.width ?? void 0,
    ...project == null || vis.identifier == null ? {} : {
      project,
      name: vis.identifier.name
    }
  };
}
function translateVisualizationFromFile(vis) {
  let module;
  switch (vis.project?.project) {
    case "Builtin":
      module = { kind: "Builtin" };
      break;
    case "CurrentProject":
      module = { kind: "CurrentProject" };
      break;
    case "Library":
      module = { kind: "Library", name: vis.project.contents };
      break;
    default:
      module = null;
  }
  return {
    identifier: module && vis.name ? { name: vis.name, module } : null,
    visible: vis.show,
    fullscreen: vis.fullscreen ?? false,
    width: vis.width ?? null
  };
}
function stupidFastDiff(oldString, newString) {
  const minLength = Math.min(oldString.length, newString.length);
  let commonPrefixLen, commonSuffixLen;
  for (commonPrefixLen = 0; commonPrefixLen < minLength; ++commonPrefixLen)
    if (oldString[commonPrefixLen] !== newString[commonPrefixLen])
      break;
  if (oldString.length === newString.length && oldString.length === commonPrefixLen)
    return [[0, oldString]];
  for (commonSuffixLen = 0; commonSuffixLen < minLength - commonPrefixLen; ++commonSuffixLen)
    if (oldString.at(-1 - commonSuffixLen) !== newString.at(-1 - commonSuffixLen))
      break;
  const commonPrefix = oldString.substring(0, commonPrefixLen);
  const removed = oldString.substring(commonPrefixLen, oldString.length - commonSuffixLen);
  const added = newString.substring(commonPrefixLen, newString.length - commonSuffixLen);
  const commonSuffix = oldString.substring(oldString.length - commonSuffixLen, oldString.length);
  return (commonPrefix ? [[0, commonPrefix]] : []).concat(removed ? [[-1, removed]] : []).concat(added ? [[1, added]] : []).concat(commonSuffix ? [[0, commonSuffix]] : []);
}
function applyDiffAsTextEdits(lineOffset, oldString, newString) {
  const changes = oldString.length + newString.length > MAX_SIZE_FOR_NORMAL_DIFF ? stupidFastDiff(oldString, newString) : diff2(oldString, newString);
  let newIndex = 0;
  let lineNum = lineOffset;
  let lineStartIdx = 0;
  const edits = [];
  for (const [op, text] of changes) {
    if (op === 1) {
      const pos = {
        character: newIndex - lineStartIdx,
        line: lineNum
      };
      edits.push({ range: { start: pos, end: pos }, text });
      const numLineBreaks = (text.match(/\n/g) ?? []).length;
      if (numLineBreaks > 0) {
        lineNum += numLineBreaks;
        lineStartIdx = newIndex + text.lastIndexOf("\n") + 1;
      }
      newIndex += text.length;
    } else if (op === -1) {
      const start = {
        character: newIndex - lineStartIdx,
        line: lineNum
      };
      const numLineBreaks = (text.match(/\n/g) ?? []).length;
      const character = numLineBreaks > 0 ? text.length - (text.lastIndexOf("\n") + 1) : newIndex - lineStartIdx + text.length;
      const end = {
        character,
        line: lineNum + numLineBreaks
      };
      edits.push({ range: { start, end }, text: "" });
    } else if (op === 0) {
      const numLineBreaks = (text.match(/\n/g) ?? []).length;
      lineNum += numLineBreaks;
      if (numLineBreaks > 0) {
        lineStartIdx = newIndex + text.lastIndexOf("\n") + 1;
      }
      newIndex += text.length;
    }
  }
  return edits;
}
function prettyPrintDiff(from, to) {
  const colReset = "\x1B[0m";
  const colRed = "\x1B[31m";
  const colGreen = "\x1B[32m";
  const diffs = from.length + to.length > MAX_SIZE_FOR_NORMAL_DIFF ? stupidFastDiff(from, to) : diff2(from, to);
  if (diffs.length === 1 && diffs[0][0] === 0)
    return "No changes";
  let content = "";
  for (let i = 0; i < diffs.length; i++) {
    const [op, text] = diffs[i];
    if (op === 1) {
      content += colGreen + text;
    } else if (op === -1) {
      content += colRed + text;
    } else if (op === 0) {
      content += colReset;
      const numNewlines = (text.match(/\n/g) ?? []).length;
      if (numNewlines < 2) {
        content += text;
      } else {
        const firstNewline = text.indexOf("\n");
        const lastNewline = text.lastIndexOf("\n");
        const firstLine = text.slice(0, firstNewline + 1);
        const lastLine = text.slice(lastNewline + 1);
        const isFirst = i === 0;
        const isLast = i === diffs.length - 1;
        if (!isFirst)
          content += firstLine;
        if (!isFirst && !isLast)
          content += "...\n";
        if (!isLast)
          content += lastLine;
      }
    }
  }
  content += colReset;
  return content;
}
if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest;
  test.each`
    oldStr     | newStr      | expected
    ${""}      | ${"foo"}    | ${[[1, "foo"]]}
    ${"foo"}   | ${""}       | ${[[-1, "foo"]]}
    ${"foo"}   | ${"foo"}    | ${[[0, "foo"]]}
    ${"foo"}   | ${"bar"}    | ${[[-1, "foo"], [1, "bar"]]}
    ${"ababx"} | ${"acacx"}  | ${[[0, "a"], [-1, "bab"], [1, "cac"], [0, "x"]]}
    ${"ax"}    | ${"acacx"}  | ${[[0, "a"], [1, "cac"], [0, "x"]]}
    ${"ababx"} | ${"ax"}     | ${[[0, "a"], [-1, "bab"], [0, "x"]]}
    ${"ababx"} | ${"abacax"} | ${[[0, "aba"], [-1, "b"], [1, "ca"], [0, "x"]]}
    ${"axxxa"} | ${"a"}      | ${[[0, "a"], [-1, "xxxa"]]}
  `("Stupid diff of $oldStr and $newStr", ({ oldStr, newStr, expected }) => {
    expect(stupidFastDiff(oldStr, newStr)).toEqual(expected);
  });
}

// ydoc-server/fileFormat.ts
import * as json from "file:///C:/Projects/enso/enso/node_modules/.pnpm/lib0@0.2.93/node_modules/lib0/json.js";
import z2 from "file:///C:/Projects/enso/enso/node_modules/.pnpm/zod@3.22.4/node_modules/zod/lib/index.mjs";
var vector = z2.tuple([z2.number(), z2.number()]);
var visualizationProject = z2.discriminatedUnion("project", [
  z2.object({ project: z2.literal("Builtin") }),
  z2.object({ project: z2.literal("CurrentProject") }),
  z2.object({ project: z2.literal("Library"), contents: z2.string() })
]);
var visualizationMetadata = z2.object({
  show: z2.boolean().default(true),
  width: z2.number().optional(),
  fullscreen: z2.boolean().optional(),
  project: visualizationProject.optional(),
  name: z2.string().optional()
}).passthrough();
var nodeMetadata = z2.object({
  position: z2.object({ vector }).catch((ctx) => {
    printError(ctx);
    return { vector: [0, 0] };
  }),
  visualization: visualizationMetadata.optional().catch(() => void 0),
  colorOverride: z2.string().optional()
}).passthrough();
var importMetadata = z2.object({}).passthrough();
var ideMetadata = z2.object({
  node: z2.record(z2.string().uuid(), nodeMetadata),
  import: z2.record(z2.string(), importMetadata)
}).passthrough().default(() => defaultMetadata().ide).catch((ctx) => {
  printError(ctx);
  return defaultMetadata().ide;
});
var metadata = z2.object({
  ide: ideMetadata
}).passthrough().catch((ctx) => {
  printError(ctx);
  return defaultMetadata();
});
var idMapValue = z2.object({
  value: z2.number()
});
var idMapRange = z2.object({
  index: idMapValue,
  size: idMapValue
});
var idMapEntry = z2.tuple([idMapRange, z2.string().uuid()]);
var idMap = z2.array(idMapEntry).catch((ctx) => {
  printError(ctx);
  return [];
});
function defaultMetadata() {
  return {
    ide: {
      node: {},
      import: {}
    }
  };
}
function printError(ctx) {
  console.error("=== METADATA PARSE ERROR ===");
  console.error("Error:", ctx.error.issues);
  console.error("Input:", ctx.input);
  console.error("============================");
}
function tryParseMetadataOrFallback(metadataJson) {
  if (metadataJson == null)
    return defaultMetadata();
  const parsedMeta = tryParseJson(metadataJson);
  return metadata.parse(parsedMeta);
}
function tryParseIdMapOrFallback(idMapJson) {
  if (idMapJson == null)
    return [];
  const parsedIdMap = tryParseJson(idMapJson);
  return idMap.parse(parsedIdMap);
}
function tryParseJson(jsonString) {
  try {
    return json.parse(jsonString);
  } catch (e) {
    console.error("Failed to parse metadata JSON:");
    console.error(e);
    return null;
  }
}

// ydoc-server/serialization.ts
import * as json2 from "file:///C:/Projects/enso/enso/node_modules/.pnpm/lib0@0.2.93/node_modules/lib0/json.js";
function deserializeIdMap(idMapJson) {
  const idMapMeta = tryParseIdMapOrFallback(idMapJson);
  const idMap2 = new IdMap();
  for (const [{ index, size }, id] of idMapMeta) {
    const range = [index.value, index.value + size.value];
    if (typeof range[0] !== "number" || typeof range[1] !== "number") {
      console.error(`Invalid range for id ${id}:`, range);
      continue;
    }
    idMap2.insertKnownId([index.value, index.value + size.value], id);
  }
  return idMap2;
}
function serializeIdMap(map3) {
  map3.validate();
  return json2.stringify(idMapToArray(map3));
}
function idMapToArray(map3) {
  const entries = [];
  map3.entries().forEach(([rangeBuffer, id]) => {
    const decoded = sourceRangeFromKey(rangeBuffer);
    const index = decoded[0];
    const endIndex = decoded[1];
    if (index == null || endIndex == null)
      return;
    const size = endIndex - index;
    entries.push([{ index: { value: index }, size: { value: size } }, id]);
  });
  entries.sort(idMapCmp);
  return entries;
}
function idMapCmp(a, b) {
  const val1 = a[0]?.index?.value ?? 0;
  const val2 = b[0]?.index?.value ?? 0;
  if (val1 === val2) {
    const size1 = a[0]?.size.value ?? 0;
    const size2 = b[0]?.size.value ?? 0;
    return size1 - size2;
  }
  return val1 - val2;
}

// ydoc-server/languageServerSession.ts
var SOURCE_DIR = "src";
var EXTENSION = ".enso";
var DEBUG_LOG_SYNC = false;
function createOpenRPCClient(url) {
  const transport = new WebSocketTransport(url);
  const requestManager = new RequestManager([transport]);
  transport.connection.on(
    "error",
    (error) => console.error("Language Server transport error:", error)
  );
  return new Client(requestManager);
}
var LanguageServerSession = class _LanguageServerSession {
  clientId;
  indexDoc;
  docs;
  retainCount;
  url;
  client;
  ls;
  connection;
  model;
  projectRootId;
  authoritativeModules;
  clientScope;
  constructor(url) {
    this.clientScope = new AbortScope();
    this.clientId = random4.uuidv4();
    this.docs = /* @__PURE__ */ new Map();
    this.retainCount = 0;
    this.url = url;
    console.log("new session with", url);
    this.indexDoc = new WSSharedDoc();
    this.docs.set("index", this.indexDoc);
    this.model = new DistributedProject(this.indexDoc.doc);
    this.projectRootId = null;
    this.authoritativeModules = /* @__PURE__ */ new Map();
    this.indexDoc.doc.on("subdocs", (subdocs) => {
      for (const doc of subdocs.loaded) {
        const name = this.model.findModuleByDocId(doc.guid);
        if (!name)
          continue;
        const persistence = this.authoritativeModules.get(name);
        if (!persistence)
          continue;
      }
    });
    const { client, ls } = this.setupClient();
    this.client = client;
    this.ls = ls;
  }
  static sessions = /* @__PURE__ */ new Map();
  static get(url) {
    const session = map2.setIfUndefined(
      _LanguageServerSession.sessions,
      url,
      () => new _LanguageServerSession(url)
    );
    session.retain();
    return session;
  }
  restartClient() {
    this.clientScope.dispose("Client restarted.");
    this.clientScope = new AbortScope();
    this.connection = void 0;
    this.setupClient();
  }
  setupClient() {
    this.client = createOpenRPCClient(this.url);
    this.ls = new LanguageServer(this.client);
    this.clientScope.onAbort(() => this.ls.release());
    this.ls.on("file/event", async (event) => {
      if (DEBUG_LOG_SYNC) {
        console.log("file/event", event);
      }
      const path2 = event.path.segments.join("/");
      try {
        switch (event.kind) {
          case "Added": {
            if (isSourceFile(event.path)) {
              const fileInfo = await this.ls.fileInfo(event.path);
              if (fileInfo.attributes.kind.type == "File") {
                await exponentialBackoff(
                  () => this.getModuleModel(event.path).open(),
                  printingCallbacks(`opened new file '${path2}'`, `open new file '${path2}'`)
                );
              }
            }
            break;
          }
          case "Modified": {
            await exponentialBackoff(
              async () => this.tryGetExistingModuleModel(event.path)?.reload(),
              printingCallbacks(`reloaded file '${path2}'`, `reload file '${path2}'`)
            );
            break;
          }
        }
      } catch {
        this.restartClient();
      }
    });
    this.ls.on("text/fileModifiedOnDisk", async (event) => {
      const path2 = event.path.segments.join("/");
      try {
        await exponentialBackoff(
          async () => this.tryGetExistingModuleModel(event.path)?.reload(),
          printingCallbacks(`reloaded file '${path2}'`, `reload file '${path2}'`)
        );
      } catch {
        this.restartClient();
      }
    });
    exponentialBackoff(
      () => this.readInitialState(),
      printingCallbacks("read initial state", "read initial state")
    ).catch((error) => {
      console.error("Could not read initial state.");
      console.error(error);
      exponentialBackoff(
        async () => this.restartClient(),
        printingCallbacks("restarted RPC client", "restart RPC client")
      );
    });
    return { client: this.client, ls: this.ls };
  }
  assertProjectRoot() {
    if (this.projectRootId == null)
      throw new Error("Missing project root");
  }
  async readInitialState() {
    let moduleOpenPromises = [];
    try {
      const connection = this.connection ?? await this.ls.initProtocolConnection(this.clientId);
      this.connection = connection;
      const projectRoot = connection.contentRoots.find((root) => root.type === "Project");
      if (!projectRoot)
        throw new Error("Missing project root");
      this.projectRootId = projectRoot.id;
      await this.ls.acquireReceivesTreeUpdates({ rootId: this.projectRootId, segments: [] });
      const files = await this.scanSourceFiles();
      moduleOpenPromises = this.indexDoc.doc.transact(
        () => files.map((file) => this.getModuleModel(pushPathSegment(file.path, file.name)).open()),
        this
      );
      await Promise.all(moduleOpenPromises);
    } catch (error) {
      console.error("LS initialization failed.");
      throw error;
    }
    console.log("LS connection initialized.");
  }
  async scanSourceFiles() {
    this.assertProjectRoot();
    const sourceDir = { rootId: this.projectRootId, segments: [SOURCE_DIR] };
    const srcModules = await this.ls.listFiles(sourceDir);
    return srcModules.paths.filter((file) => file.type === "File" && file.name.endsWith(EXTENSION));
  }
  tryGetExistingModuleModel(path2) {
    const name = pathToModuleName(path2);
    return this.authoritativeModules.get(name);
  }
  getModuleModel(path2) {
    const name = pathToModuleName(path2);
    return map2.setIfUndefined(this.authoritativeModules, name, () => {
      const wsDoc = new WSSharedDoc();
      this.docs.set(wsDoc.doc.guid, wsDoc);
      this.model.createUnloadedModule(name, wsDoc.doc);
      const mod = new ModulePersistence(this.ls, path2, wsDoc.doc);
      mod.once("removed", () => {
        const index = this.model.findModuleByDocId(wsDoc.doc.guid);
        this.docs.delete(wsDoc.doc.guid);
        this.authoritativeModules.delete(name);
        if (index != null)
          this.model.deleteModule(index);
      });
      return mod;
    });
  }
  retain() {
    this.retainCount += 1;
  }
  async release() {
    this.retainCount -= 1;
    if (this.retainCount !== 0)
      return;
    const modules = this.authoritativeModules.values();
    const moduleDisposePromises = Array.from(modules, (mod) => mod.dispose());
    this.authoritativeModules.clear();
    this.model.doc.destroy();
    this.clientScope.dispose("LangueServerSession disposed.");
    _LanguageServerSession.sessions.delete(this.url);
    await Promise.all(moduleDisposePromises);
  }
  getYDoc(guid) {
    return this.docs.get(guid);
  }
};
function isSourceFile(path2) {
  return path2.segments[0] === SOURCE_DIR && path2.segments[path2.segments.length - 1].endsWith(EXTENSION);
}
function pathToModuleName(path2) {
  if (path2.segments[0] === SOURCE_DIR)
    return path2.segments.slice(1).join("/");
  else
    return "//" + path2.segments.join("/");
}
function pushPathSegment(path2, segment) {
  return { rootId: path2.rootId, segments: [...path2.segments, segment] };
}
var LsSyncState = /* @__PURE__ */ ((LsSyncState2) => {
  LsSyncState2[LsSyncState2["Closed"] = 0] = "Closed";
  LsSyncState2[LsSyncState2["Opening"] = 1] = "Opening";
  LsSyncState2[LsSyncState2["Synchronized"] = 2] = "Synchronized";
  LsSyncState2[LsSyncState2["WritingFile"] = 3] = "WritingFile";
  LsSyncState2[LsSyncState2["WriteError"] = 4] = "WriteError";
  LsSyncState2[LsSyncState2["Reloading"] = 5] = "Reloading";
  LsSyncState2[LsSyncState2["Closing"] = 6] = "Closing";
  LsSyncState2[LsSyncState2["Disposed"] = 7] = "Disposed";
  return LsSyncState2;
})(LsSyncState || {});
var ModulePersistence = class extends ObservableV22 {
  ls;
  path;
  doc = new ModuleDoc(new Y3.Doc());
  state = 0 /* Closed */;
  lastAction = Promise.resolve();
  updateToApply = null;
  syncedCode = null;
  syncedIdMap = null;
  syncedMetaJson = null;
  syncedContent = null;
  syncedVersion = null;
  syncedMeta = tryParseMetadataOrFallback(null);
  queuedAction = null;
  cleanup = () => {
  };
  constructor(ls, path2, sharedDoc) {
    super();
    this.ls = ls;
    this.path = path2;
    const onRemoteUpdate = this.queueRemoteUpdate.bind(this);
    const onLocalUpdate = (update, origin) => {
      if (origin === "file")
        Y3.applyUpdate(sharedDoc, update, this);
    };
    const onFileModified = this.handleFileModified.bind(this);
    const onFileRemoved = this.handleFileRemoved.bind(this);
    this.doc.ydoc.on("update", onLocalUpdate);
    sharedDoc.on("update", onRemoteUpdate);
    this.ls.on("text/fileModifiedOnDisk", onFileModified);
    this.ls.on("file/rootRemoved", onFileRemoved);
    this.cleanup = () => {
      this.doc.ydoc.off("update", onLocalUpdate);
      sharedDoc.off("update", onRemoteUpdate);
      this.ls.off("text/fileModifiedOnDisk", onFileModified);
      this.ls.off("file/rootRemoved", onFileRemoved);
    };
  }
  inState(...states) {
    return states.includes(this.state);
  }
  setState(state) {
    if (this.state !== 7 /* Disposed */) {
      if (DEBUG_LOG_SYNC) {
        console.debug("State change:", LsSyncState[this.state], "->", LsSyncState[state]);
      }
      this.state = state;
      if (state === 2 /* Synchronized */)
        this.trySyncRemoveUpdates();
    } else {
      throw new Error("LsSync disposed");
    }
  }
  setLastAction(lastAction) {
    this.lastAction = lastAction.then(
      () => {
      },
      () => {
      }
    );
    return lastAction;
  }
  /** Set the current state to the given state while the callback is running.
   * Set the current state back to {@link LsSyncState.Synchronized} when the callback finishes. */
  async withState(state, callback) {
    this.setState(state);
    await callback();
    this.setState(2 /* Synchronized */);
  }
  async open() {
    this.queuedAction = 0 /* Open */;
    switch (this.state) {
      case 7 /* Disposed */:
      case 3 /* WritingFile */:
      case 2 /* Synchronized */:
      case 4 /* WriteError */:
      case 5 /* Reloading */: {
        return;
      }
      case 6 /* Closing */: {
        await this.lastAction;
        if (this.queuedAction === 0 /* Open */)
          await this.open();
        return;
      }
      case 1 /* Opening */: {
        await this.lastAction;
        return;
      }
      case 0 /* Closed */: {
        await this.withState(1 /* Opening */, async () => {
          const promise = this.ls.openTextFile(this.path);
          this.setLastAction(promise.catch(() => this.setState(0 /* Closed */)));
          const result = await promise;
          if (!result.writeCapability) {
            console.error("Could not acquire write capability for module:", this.path);
            throw new Error(
              `Could not acquire write capability for module '${this.path.segments.join("/")}'`
            );
          }
          this.syncFileContents(result.content, result.currentVersion);
        });
        return;
      }
      default: {
        this.state;
        return;
      }
    }
  }
  handleFileRemoved() {
    if (this.inState(0 /* Closed */))
      return;
    this.close();
  }
  handleFileModified() {
    if (this.inState(0 /* Closed */))
      return;
  }
  queueRemoteUpdate(update, origin) {
    if (origin === this)
      return;
    if (this.updateToApply != null) {
      this.updateToApply = Y3.mergeUpdates([this.updateToApply, update]);
    } else {
      this.updateToApply = update;
    }
    this.trySyncRemoveUpdates();
  }
  trySyncRemoveUpdates() {
    if (this.updateToApply == null)
      return;
    if (!this.inState(2 /* Synchronized */))
      return;
    const update = this.updateToApply;
    this.updateToApply = null;
    const syncModule = new MutableModule(this.doc.ydoc);
    const moduleUpdate = syncModule.applyUpdate(update, "remote");
    if (moduleUpdate && this.syncedContent) {
      const synced = splitFileContents(this.syncedContent);
      const { newCode, newIdMap, newMetadata } = applyDocumentUpdates(
        this.doc,
        synced,
        moduleUpdate
      );
      this.sendLsUpdate(synced, newCode, newIdMap, newMetadata);
    }
  }
  sendLsUpdate(synced, newCode, newIdMap, newMetadata) {
    if (this.syncedContent == null || this.syncedVersion == null)
      return;
    const code = newCode ?? synced.code;
    const newMetadataJson = newMetadata && json3.stringify({ ...this.syncedMeta, ide: { ...this.syncedMeta.ide, node: newMetadata } });
    const newIdMapJson = newIdMap && serializeIdMap(newIdMap);
    const newContent = combineFileParts({
      code,
      idMapJson: newIdMapJson ?? synced.idMapJson ?? "[]",
      metadataJson: newMetadataJson ?? synced.metadataJson ?? "{}"
    });
    const edits = [];
    if (newCode)
      edits.push(...applyDiffAsTextEdits(0, synced.code, newCode));
    if (newIdMap || newMetadata) {
      const oldMetaContent = this.syncedContent.slice(synced.code.length);
      const metaContent = newContent.slice(code.length);
      const metaStartLine = (code.match(/\n/g) ?? []).length;
      edits.push(...applyDiffAsTextEdits(metaStartLine, oldMetaContent, metaContent));
    }
    const newVersion = computeTextChecksum(newContent);
    if (DEBUG_LOG_SYNC) {
      console.debug(" === changes === ");
      console.debug("number of edits:", edits.length);
      if (edits.length > 0) {
        console.debug("version:", this.syncedVersion, "->", newVersion);
        console.debug("Content diff:");
        console.debug(prettyPrintDiff(this.syncedContent, newContent));
      }
      console.debug(" =============== ");
    }
    this.setState(3 /* WritingFile */);
    const execute = newCode != null || newIdMap != null;
    const edit = { path: this.path, edits, oldVersion: this.syncedVersion, newVersion };
    const apply = this.ls.applyEdit(edit, execute);
    const promise = apply.then(
      () => {
        this.syncedContent = newContent;
        this.syncedVersion = newVersion;
        if (newMetadata)
          this.syncedMeta.ide.node = newMetadata;
        if (newCode)
          this.syncedCode = newCode;
        if (newIdMapJson)
          this.syncedIdMap = newIdMapJson;
        if (newMetadataJson)
          this.syncedMetaJson = newMetadataJson;
        this.setState(2 /* Synchronized */);
      },
      (error) => {
        console.error("Could not apply edit:", error);
        this.setState(4 /* WriteError */);
        this.syncedContent = null;
        this.syncedVersion = null;
        this.syncedCode = null;
        this.syncedIdMap = null;
        this.syncedMetaJson = null;
        return this.reload();
      }
    );
    this.setLastAction(promise);
    return promise;
  }
  syncFileContents(content, version) {
    const contentsReceived = splitFileContents(content);
    let unsyncedIdMap;
    this.doc.ydoc.transact(() => {
      const { code, idMapJson, metadataJson } = contentsReceived;
      const metadata2 = tryParseMetadataOrFallback(metadataJson);
      const nodeMeta = Object.entries(metadata2.ide.node);
      let parsedSpans;
      const syncModule = new MutableModule(this.doc.ydoc);
      if (code !== this.syncedCode) {
        const syncRoot = syncModule.root();
        if (syncRoot) {
          const edit = syncModule.edit();
          edit.getVersion(syncRoot).syncToCode(code);
          const editedRoot = edit.root();
          if (editedRoot instanceof BodyBlock)
            repair(editedRoot, edit);
          syncModule.applyEdit(edit);
        } else {
          const { root, spans } = parseBlockWithSpans(code, syncModule);
          syncModule.syncRoot(root);
          parsedSpans = spans;
        }
      }
      const astRoot = syncModule.root();
      if (!astRoot)
        return;
      if ((code !== this.syncedCode || idMapJson !== this.syncedIdMap) && idMapJson) {
        const idMap2 = deserializeIdMap(idMapJson);
        const spans = parsedSpans ?? print(astRoot).info;
        const idsAssigned = setExternalIds(syncModule, spans, idMap2);
        const numberOfAsts = astCount(astRoot);
        const idsNotSetByMap = numberOfAsts - idsAssigned;
        if (idsNotSetByMap > 0) {
          if (code !== this.syncedCode) {
            unsyncedIdMap = spanMapToIdMap(spans);
          } else {
            console.warn(
              `The LS sent an IdMap-only edit that is missing ${idsNotSetByMap} of our expected ASTs.`
            );
          }
        }
      }
      if ((code !== this.syncedCode || idMapJson !== this.syncedIdMap || metadataJson !== this.syncedMetaJson) && nodeMeta.length !== 0) {
        const externalIdToAst = /* @__PURE__ */ new Map();
        astRoot.visitRecursiveAst((ast) => {
          if (!externalIdToAst.has(ast.externalId))
            externalIdToAst.set(ast.externalId, ast);
        });
        const missing = /* @__PURE__ */ new Set();
        for (const [id, meta] of nodeMeta) {
          if (typeof id !== "string")
            continue;
          const ast = externalIdToAst.get(id);
          if (!ast) {
            missing.add(id);
            continue;
          }
          const metadata3 = syncModule.getVersion(ast).mutableNodeMetadata();
          const oldPos = metadata3.get("position");
          const newPos = { x: meta.position.vector[0], y: -meta.position.vector[1] };
          if (oldPos?.x !== newPos.x || oldPos?.y !== newPos.y)
            metadata3.set("position", newPos);
          const oldVis = metadata3.get("visualization");
          const newVis = meta.visualization && translateVisualizationFromFile(meta.visualization);
          if (!visMetadataEquals(newVis, oldVis))
            metadata3.set("visualization", newVis);
          const oldColorOverride = metadata3.get("colorOverride");
          const newColorOverride = meta.colorOverride;
          if (oldColorOverride !== newColorOverride)
            metadata3.set("colorOverride", newColorOverride);
        }
      }
      this.syncedCode = code;
      this.syncedIdMap = unsyncedIdMap ? null : idMapJson;
      this.syncedContent = content;
      this.syncedVersion = version;
      this.syncedMeta = metadata2;
      this.syncedMetaJson = metadataJson;
    }, "file");
    if (unsyncedIdMap)
      this.sendLsUpdate(contentsReceived, void 0, unsyncedIdMap, void 0);
  }
  async close() {
    this.queuedAction = 1 /* Close */;
    switch (this.state) {
      case 7 /* Disposed */:
      case 0 /* Closed */: {
        return;
      }
      case 6 /* Closing */: {
        await this.lastAction;
        return;
      }
      case 1 /* Opening */:
      case 3 /* WritingFile */:
      case 5 /* Reloading */: {
        await this.lastAction;
        if (this.queuedAction === 1 /* Close */) {
          await this.close();
        }
        return;
      }
      case 4 /* WriteError */:
      case 2 /* Synchronized */: {
        this.setState(6 /* Closing */);
        const promise = this.ls.closeTextFile(this.path);
        const state = this.state;
        this.setLastAction(promise.catch(() => this.setState(state)));
        await promise;
        this.setState(0 /* Closed */);
        return;
      }
      default: {
        this.state;
        return;
      }
    }
  }
  async reload() {
    this.queuedAction = 2 /* Reload */;
    switch (this.state) {
      case 1 /* Opening */:
      case 7 /* Disposed */:
      case 0 /* Closed */:
      case 6 /* Closing */: {
        return;
      }
      case 5 /* Reloading */: {
        await this.lastAction;
        return;
      }
      case 3 /* WritingFile */: {
        await this.lastAction;
        if (this.queuedAction === 2 /* Reload */)
          await this.reload();
        return;
      }
      case 2 /* Synchronized */: {
        this.withState(5 /* Reloading */, async () => {
          const promise = Promise.all([
            this.ls.readFile(this.path),
            this.ls.fileChecksum(this.path)
          ]);
          this.setLastAction(promise);
          const [contents, checksum] = await promise;
          this.syncFileContents(contents.contents, checksum.checksum);
        });
        return;
      }
      case 4 /* WriteError */: {
        this.withState(5 /* Reloading */, async () => {
          const path2 = this.path.segments.join("/");
          const reloading = this.ls.closeTextFile(this.path).catch((error) => {
            console.error("Could not close file after write error:");
            console.error(error);
          }).then(
            () => exponentialBackoff(
              async () => {
                const result2 = await this.ls.openTextFile(this.path);
                if (!result2.writeCapability) {
                  const message = `Could not acquire write capability for module '${this.path.segments.join(
                    "/"
                  )}'`;
                  console.error(message);
                  throw new Error(message);
                }
                return result2;
              },
              printingCallbacks(
                `opened file '${path2}' for writing`,
                `open file '${path2}' for writing`
              )
            ),
            (error) => {
              console.error("Could not reopen file after write error:");
              console.error(error);
              throw error;
            }
          );
          this.setLastAction(reloading);
          const result = await reloading;
          this.syncFileContents(result.content, result.currentVersion);
        });
        return;
      }
      default: {
        this.state;
        return;
      }
    }
  }
  async dispose() {
    this.cleanup();
    const alreadyClosed = this.inState(6 /* Closing */, 0 /* Closed */);
    this.setState(7 /* Disposed */);
    if (alreadyClosed)
      return Promise.resolve();
    return this.ls.closeTextFile(this.path);
  }
};

// ydoc-server/ydoc.ts
var pingTimeout = 3e4;
var messageSync = 0;
var messageAwareness = 1;
var WSSharedDoc = class {
  doc;
  /**
   * Maps from connection id to set of controlled user ids.
   * Delete all user ids from awareness when this conn is closed.
   */
  conns;
  awareness;
  constructor(gc = true) {
    this.doc = new Y4.Doc({ gc });
    this.conns = /* @__PURE__ */ new Map();
    this.awareness = new Awareness(this.doc);
    this.awareness.setLocalState(null);
    this.awareness.on(
      "update",
      ({ added, updated, removed }, conn) => {
        const changedClients = added.concat(updated, removed);
        if (conn !== null) {
          const connControlledIDs = this.conns.get(conn);
          if (connControlledIDs !== void 0) {
            for (const clientID of added)
              connControlledIDs.add(clientID);
            for (const clientID of removed)
              connControlledIDs.delete(clientID);
          }
        }
        const encoder = encoding.createEncoder();
        encoding.writeVarUint(encoder, messageAwareness);
        const update = encodeAwarenessUpdate(this.awareness, changedClients);
        encoding.writeVarUint8Array(encoder, update);
        this.broadcast(encoding.toUint8Array(encoder));
      }
    );
    this.doc.on("update", (update, origin) => this.updateHandler(update, origin));
  }
  broadcast(message) {
    for (const [conn] of this.conns) {
      if (typeof conn !== "string")
        conn.send(message);
    }
  }
  updateHandler(update, _origin) {
    const encoder = encoding.createEncoder();
    encoding.writeVarUint(encoder, messageSync);
    writeUpdate(encoder, update);
    this.broadcast(encoding.toUint8Array(encoder));
  }
};
function setupGatewayClient(ws, lsUrl, docName2) {
  const lsSession = LanguageServerSession.get(lsUrl);
  const wsDoc = lsSession.getYDoc(docName2);
  if (wsDoc == null) {
    console.error(`Document '${docName2}' not found in language server session '${lsUrl}'.`);
    ws.close();
    return;
  }
  const connection = new YjsConnection(ws, wsDoc);
  connection.once("close", async () => {
    try {
      await lsSession.release();
    } catch (error) {
      console.error("Session release failed.\n", error);
    }
  });
}
var YjsConnection = class extends ObservableV23 {
  ws;
  wsDoc;
  constructor(ws, wsDoc) {
    super();
    this.ws = ws;
    this.wsDoc = wsDoc;
    const isLoaded = wsDoc.conns.size > 0;
    wsDoc.conns.set(this, /* @__PURE__ */ new Set());
    ws.binaryType = "arraybuffer";
    ws.on("message", (message) => this.messageListener(new Uint8Array(message)));
    ws.on("close", () => this.close());
    if (!isLoaded)
      wsDoc.doc.load();
    this.initPing();
    this.sendSyncMessage();
  }
  initPing() {
    let pongReceived = true;
    const pingInterval = setInterval(() => {
      if (!pongReceived) {
        if (this.wsDoc.conns.has(this))
          this.close();
        clearInterval(pingInterval);
      } else if (this.wsDoc.conns.has(this)) {
        pongReceived = false;
        try {
          this.ws.ping();
        } catch (error) {
          console.error("Error sending ping:", error);
          this.close();
          clearInterval(pingInterval);
        }
      }
    }, pingTimeout);
    this.ws.on("close", () => clearInterval(pingInterval));
    this.ws.on("pong", () => pongReceived = true);
  }
  sendSyncMessage() {
    const encoder = encoding.createEncoder();
    encoding.writeVarUint(encoder, messageSync);
    writeSyncStep1(encoder, this.wsDoc.doc);
    this.send(encoding.toUint8Array(encoder));
    const awarenessStates = this.wsDoc.awareness.getStates();
    if (awarenessStates.size > 0) {
      const encoder2 = encoding.createEncoder();
      encoding.writeVarUint(encoder2, messageAwareness);
      encoding.writeVarUint8Array(
        encoder2,
        encodeAwarenessUpdate(this.wsDoc.awareness, Array.from(awarenessStates.keys()))
      );
      this.send(encoding.toUint8Array(encoder2));
    }
  }
  send(message) {
    if (this.ws.readyState !== WebSocket.CONNECTING && this.ws.readyState !== WebSocket.OPEN) {
      this.close();
    }
    try {
      this.ws.send(message, (error) => error && this.close());
    } catch (e) {
      this.close();
    }
  }
  messageListener(message) {
    try {
      const encoder = encoding.createEncoder();
      const decoder = decoding.createDecoder(message);
      const messageType = decoding.readVarUint(decoder);
      switch (messageType) {
        case messageSync: {
          encoding.writeVarUint(encoder, messageSync);
          readSyncMessage(decoder, encoder, this.wsDoc.doc, this);
          if (encoding.length(encoder) > 1) {
            this.send(encoding.toUint8Array(encoder));
          }
          break;
        }
        case messageAwareness: {
          const update = decoding.readVarUint8Array(decoder);
          applyAwarenessUpdate(this.wsDoc.awareness, update, this);
          break;
        }
      }
    } catch (err) {
      console.error(err);
      this.wsDoc.doc.emit("error", [err]);
    }
  }
  close() {
    const controlledIds = this.wsDoc.conns.get(this);
    this.wsDoc.conns.delete(this);
    if (controlledIds != null) {
      removeAwarenessStates(this.wsDoc.awareness, Array.from(controlledIds), null);
    }
    this.ws.close();
    this.emit("close", []);
    if (this.wsDoc.conns.size === 0) {
      this.wsDoc.doc.emit("unload", []);
    }
  }
};

// ydoc-server/index.ts
async function createGatewayServer(httpServer, rustFFIPath) {
  await initializeFFI(rustFFIPath);
  const wss = new WebSocketServer({ noServer: true });
  wss.on("connection", (ws, _request, data) => {
    ws.on("error", onWebSocketError);
    setupGatewayClient(ws, data.lsUrl, data.doc);
  });
  httpServer.on("upgrade", (request, socket, head) => {
    socket.on("error", onHttpSocketError);
    authenticate(request, function next(err, data) {
      if (err != null) {
        socket.write("HTTP/1.1 401 Unauthorized\r\n\r\n");
        socket.destroy();
        return;
      }
      socket.removeListener("error", onHttpSocketError);
      if (data != null) {
        wss.handleUpgrade(request, socket, head, function done(ws) {
          wss.emit("connection", ws, request, data);
        });
      }
    });
  });
}
function onWebSocketError(err) {
  console.log("WebSocket error:", err);
}
function onHttpSocketError(err) {
  console.log("HTTP socket error:", err);
}
function authenticate(request, callback) {
  const user = "mock-user";
  if (request.url == null)
    return callback(null, null);
  const { pathname, query } = parse4(request.url, true);
  if (pathname == null)
    return callback(null, null);
  const doc = docName(pathname);
  const lsUrl = query.ls;
  const data = doc != null && typeof lsUrl === "string" ? { lsUrl, doc, user } : null;
  callback(null, data);
}
var docNameRegex = /^[a-z0-9/-]+$/i;
function docName(pathname) {
  const prefix = "/project/";
  if (pathname != null && pathname.startsWith(prefix)) {
    const docName2 = pathname.slice(prefix.length);
    if (docNameRegex.test(docName2)) {
      return docName2;
    }
  }
  return null;
}

// vite.config.ts
var __vite_injected_original_import_meta_url3 = "file:///C:/Projects/enso/enso/app/gui2/vite.config.ts";
var localServerPort = 8080;
var projectManagerUrl = "ws://127.0.0.1:30535";
var IS_CLOUD_BUILD = process.env.CLOUD_BUILD === "true";
await readEnvironmentFromFile();
var entrypoint = process.env.E2E === "true" ? "./src/e2e-entrypoint.ts" : "./src/entrypoint.ts";
var vite_config_default = defineConfig({
  root: fileURLToPath(new URL(".", __vite_injected_original_import_meta_url3)),
  cacheDir: fileURLToPath(new URL("../../node_modules/.cache/vite", __vite_injected_original_import_meta_url3)),
  publicDir: fileURLToPath(new URL("./public", __vite_injected_original_import_meta_url3)),
  envDir: fileURLToPath(new URL(".", __vite_injected_original_import_meta_url3)),
  plugins: [
    vue(),
    gatewayServer(),
    ...process.env.ELECTRON_DEV_MODE === "true" ? [
      (await import("file:///C:/Projects/enso/enso/node_modules/.pnpm/@vitejs+plugin-react@4.2.1_vite@5.2.9/node_modules/@vitejs/plugin-react/dist/index.mjs")).default({
        include: fileURLToPath(new URL("../ide-desktop/lib/dashboard/**/*.tsx", __vite_injected_original_import_meta_url3)),
        babel: { plugins: ["@babel/plugin-syntax-import-assertions"] }
      })
    ] : process.env.NODE_ENV === "development" ? [await projectManagerShim()] : []
  ],
  optimizeDeps: {
    entries: fileURLToPath(new URL("./index.html", __vite_injected_original_import_meta_url3))
  },
  server: {
    watch: {},
    headers: {
      "Cross-Origin-Embedder-Policy": "require-corp",
      "Cross-Origin-Opener-Policy": "same-origin",
      "Cross-Origin-Resource-Policy": "same-origin"
    }
  },
  resolve: {
    alias: {
      "/src/entrypoint.ts": fileURLToPath(new URL(entrypoint, __vite_injected_original_import_meta_url3)),
      shared: fileURLToPath(new URL("./shared", __vite_injected_original_import_meta_url3)),
      "@": fileURLToPath(new URL("./src", __vite_injected_original_import_meta_url3))
    }
  },
  define: {
    ...getDefines(localServerPort),
    IS_CLOUD_BUILD: JSON.stringify(IS_CLOUD_BUILD),
    PROJECT_MANAGER_URL: JSON.stringify(projectManagerUrl),
    RUNNING_VITEST: false,
    "import.meta.vitest": false,
    // Single hardcoded usage of `global` in aws-amplify.
    "global.TYPED_ARRAY_SUPPORT": true
  },
  assetsInclude: ["**/*.yaml", "**/*.svg"],
  css: {
    postcss: {
      plugins: [
        tailwindcssNesting(postcssNesting()),
        tailwindcss({
          ...tailwindConfig.default,
          content: tailwindConfig.default.content.map(
            (glob) => glob.replace(
              /^[.][/]/,
              fileURLToPath(new URL("../ide-desktop/lib/dashboard/", __vite_injected_original_import_meta_url3))
            )
          )
        })
      ]
    }
  },
  build: {
    // dashboard chunk size is larger than the default warning limit
    chunkSizeWarningLimit: 700,
    rollupOptions: {
      output: {
        manualChunks: {
          fontawesome: ["@fortawesome/react-fontawesome", "@fortawesome/free-brands-svg-icons"]
        }
      }
    }
  }
});
function gatewayServer() {
  return {
    name: "gateway-server",
    configureServer(server) {
      if (server.httpServer == null)
        return;
      createGatewayServer(server.httpServer, void 0);
    }
  };
}
async function projectManagerShim() {
  const module = await Promise.resolve().then(() => (init_projectManagerShimMiddleware(), projectManagerShimMiddleware_exports));
  return {
    name: "project-manager-shim",
    configureServer(server) {
      server.middlewares.use(module.default);
    }
  };
}

// vitest.config.ts
var __vite_injected_original_import_meta_url4 = "file:///C:/Projects/enso/enso/app/gui2/vitest.config.ts";
var vitest_config_default = mergeConfig(
  vite_config_default,
  defineConfig2({
    test: {
      environment: "jsdom",
      includeSource: ["./{src,shared,ydoc-server}/**/*.{ts,vue}"],
      exclude: [...configDefaults.exclude, "e2e/*"],
      root: fileURLToPath2(new URL("./", __vite_injected_original_import_meta_url4)),
      restoreMocks: true
    },
    define: {
      RUNNING_VITEST: true
    }
  })
);
export {
  vitest_config_default as default
};
//# sourceMappingURL=data:application/json;base64,ewogICJ2ZXJzaW9uIjogMywKICAic291cmNlcyI6IFsiLi4vaWRlLWRlc2t0b3AvbGliL3Byb2plY3QtbWFuYWdlci1zaGltL3NyYy9wcm9qZWN0TWFuYWdlbWVudC50cyIsICIuLi9pZGUtZGVza3RvcC9saWIvcHJvamVjdC1tYW5hZ2VyLXNoaW0vc3JjL3Byb2plY3RNYW5hZ2VyU2hpbU1pZGRsZXdhcmUudHMiLCAidml0ZXN0LmNvbmZpZy50cyIsICJ2aXRlLmNvbmZpZy50cyIsICJ5ZG9jLXNlcnZlci9pbmRleC50cyIsICJzaGFyZWQvYXN0L2ZmaS50cyIsICJydXN0LWZmaS9wa2cvcnVzdF9mZmkuanMiLCAic2hhcmVkL3V0aWwvYXNzZXJ0LnRzIiwgInNoYXJlZC91dGlsL2RldGVjdC50cyIsICJ5ZG9jLXNlcnZlci95ZG9jLnRzIiwgInlkb2Mtc2VydmVyL2xhbmd1YWdlU2VydmVyU2Vzc2lvbi50cyIsICJzaGFyZWQvYXN0L2luZGV4LnRzIiwgInNoYXJlZC91dGlsL2RhdGEvcmVzdWx0LnRzIiwgInNoYXJlZC95anNNb2RlbC50cyIsICJzaGFyZWQvYXN0L3BhcnNlclN1cHBvcnQudHMiLCAic2hhcmVkL2FzdC9nZW5lcmF0ZWQvYXN0LnRzIiwgInNoYXJlZC9hc3QvcGFyc2UudHMiLCAic2hhcmVkL3V0aWwvZGF0YS9pdGVyYWJsZS50cyIsICJzaGFyZWQvdXRpbC9kYXRhL3RleHQudHMiLCAic2hhcmVkL2FzdC9kZWJ1Zy50cyIsICJzaGFyZWQvYXN0L211dGFibGVNb2R1bGUudHMiLCAic2hhcmVkL2FzdC90cmVlLnRzIiwgInNoYXJlZC9hc3QvdGV4dC50cyIsICJzaGFyZWQvYXN0L3Rva2VuLnRzIiwgInNoYXJlZC9lbnNvRmlsZS50cyIsICJzaGFyZWQvbGFuZ3VhZ2VTZXJ2ZXIudHMiLCAic2hhcmVkL2xhbmd1YWdlU2VydmVyL2ZpbGVzLnRzIiwgInNoYXJlZC9yZXRyeS50cyIsICJzaGFyZWQvdXRpbC9uZXQudHMiLCAieWRvYy1zZXJ2ZXIvZWRpdHMudHMiLCAieWRvYy1zZXJ2ZXIvZmlsZUZvcm1hdC50cyIsICJ5ZG9jLXNlcnZlci9zZXJpYWxpemF0aW9uLnRzIl0sCiAgInNvdXJjZXNDb250ZW50IjogWyJjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfZGlybmFtZSA9IFwiQzpcXFxcUHJvamVjdHNcXFxcZW5zb1xcXFxlbnNvXFxcXGFwcFxcXFxpZGUtZGVza3RvcFxcXFxsaWJcXFxccHJvamVjdC1tYW5hZ2VyLXNoaW1cXFxcc3JjXCI7Y29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2ZpbGVuYW1lID0gXCJDOlxcXFxQcm9qZWN0c1xcXFxlbnNvXFxcXGVuc29cXFxcYXBwXFxcXGlkZS1kZXNrdG9wXFxcXGxpYlxcXFxwcm9qZWN0LW1hbmFnZXItc2hpbVxcXFxzcmNcXFxccHJvamVjdE1hbmFnZW1lbnQudHNcIjtjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfaW1wb3J0X21ldGFfdXJsID0gXCJmaWxlOi8vL0M6L1Byb2plY3RzL2Vuc28vZW5zby9hcHAvaWRlLWRlc2t0b3AvbGliL3Byb2plY3QtbWFuYWdlci1zaGltL3NyYy9wcm9qZWN0TWFuYWdlbWVudC50c1wiOy8qKiBAZmlsZSBUaGlzIG1vZHVsZSBjb250YWlucyBmdW5jdGlvbnMgZm9yIGltcG9ydGluZyBwcm9qZWN0cyBpbnRvIHRoZSBQcm9qZWN0IE1hbmFnZXIuXG4gKlxuICogRXZlbnR1YWxseSB0aGlzIG1vZHVsZSBzaG91bGQgYmUgcmVwbGFjZWQgd2l0aCBhIG5ldyBQcm9qZWN0IE1hbmFnZXIgQVBJIHRoYXQgc3VwcG9ydHNcbiAqIGltcG9ydGluZyBwcm9qZWN0cy5cbiAqIEZvciBub3csIHdlIGJhc2ljYWxseSBkbyB0aGUgZm9sbG93aW5nOlxuICogLSBpZiB0aGUgcHJvamVjdCBpcyBhbHJlYWR5IGluIHRoZSBQcm9qZWN0IE1hbmFnZXIncyBsb2NhdGlvbiwgd2UganVzdCBvcGVuIGl0O1xuICogLSBpZiB0aGUgcHJvamVjdCBpcyBpbiBhIGRpZmZlcmVudCBsb2NhdGlvbiwgd2UgY29weSBpdCB0byB0aGUgUHJvamVjdCBNYW5hZ2VyJ3MgbG9jYXRpb25cbiAqIGFuZCBvcGVuIGl0LlxuICogLSBpZiB0aGUgcHJvamVjdCBpcyBhIGJ1bmRsZSwgd2UgZXh0cmFjdCBpdCB0byB0aGUgUHJvamVjdCBNYW5hZ2VyJ3MgbG9jYXRpb24gYW5kIG9wZW4gaXQuICovXG5pbXBvcnQgKiBhcyBjcnlwdG8gZnJvbSAnbm9kZTpjcnlwdG8nXG5pbXBvcnQgKiBhcyBmcyBmcm9tICdub2RlOmZzJ1xuaW1wb3J0ICogYXMgb3MgZnJvbSAnbm9kZTpvcydcbmltcG9ydCAqIGFzIHBhdGhNb2R1bGUgZnJvbSAnbm9kZTpwYXRoJ1xuaW1wb3J0IHR5cGUgKiBhcyBzdHJlYW0gZnJvbSAnbm9kZTpzdHJlYW0nXG5cbmltcG9ydCAqIGFzIHRhciBmcm9tICd0YXInXG5cbmltcG9ydCAqIGFzIGNvbW1vbiBmcm9tICdlbnNvLWNvbW1vbidcbmltcG9ydCAqIGFzIGJ1aWxkVXRpbHMgZnJvbSAnZW5zby1jb21tb24vc3JjL2J1aWxkVXRpbHMnXG5cbmNvbnN0IGxvZ2dlciA9IGNvbnNvbGVcblxuLy8gPT09PT09PT09PT09PT09PT1cbi8vID09PSBDb25zdGFudHMgPT09XG4vLyA9PT09PT09PT09PT09PT09PVxuXG5leHBvcnQgY29uc3QgUEFDS0FHRV9NRVRBREFUQV9SRUxBVElWRV9QQVRIID0gJ3BhY2thZ2UueWFtbCdcbmV4cG9ydCBjb25zdCBQUk9KRUNUX01FVEFEQVRBX1JFTEFUSVZFX1BBVEggPSAnLmVuc28vcHJvamVjdC5qc29uJ1xuLyoqIFRoZSBmaWxlbmFtZSBzdWZmaXggZm9yIHRoZSBwcm9qZWN0IGJ1bmRsZSwgaW5jbHVkaW5nIHRoZSBsZWFkaW5nIHBlcmlvZCBjaGFyYWN0ZXIuICovXG5jb25zdCBCVU5ETEVEX1BST0pFQ1RfU1VGRklYID0gYC5lbnNvLXByb2plY3RgXG5cbi8vID09PT09PT09PT09PT09PT09PT09PT1cbi8vID09PSBQcm9qZWN0IEltcG9ydCA9PT1cbi8vID09PT09PT09PT09PT09PT09PT09PT1cblxuLyoqIE9wZW4gYSBwcm9qZWN0IGZyb20gdGhlIGdpdmVuIHBhdGguIFBhdGggY2FuIGJlIGVpdGhlciBhIHNvdXJjZSBmaWxlIHVuZGVyIHRoZSBwcm9qZWN0IHJvb3QsXG4gKiBvciB0aGUgcHJvamVjdCBidW5kbGUuIElmIG5lZWRlZCwgdGhlIHByb2plY3Qgd2lsbCBiZSBpbXBvcnRlZCBpbnRvIHRoZSBQcm9qZWN0IE1hbmFnZXItZW5hYmxlZFxuICogbG9jYXRpb24uXG4gKiBAcmV0dXJucyBQcm9qZWN0IElEIChmcm9tIFByb2plY3QgTWFuYWdlcidzIG1ldGFkYXRhKSBpZGVudGlmeWluZyB0aGUgaW1wb3J0ZWQgcHJvamVjdC5cbiAqIEB0aHJvd3Mge0Vycm9yfSBpZiB0aGUgcGF0aCBkb2VzIG5vdCBiZWxvbmcgdG8gYSB2YWxpZCBwcm9qZWN0LiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGltcG9ydFByb2plY3RGcm9tUGF0aChcbiAgICBvcGVuZWRQYXRoOiBzdHJpbmcsXG4gICAgZGlyZWN0b3J5Pzogc3RyaW5nIHwgbnVsbCxcbiAgICBuYW1lOiBzdHJpbmcgfCBudWxsID0gbnVsbFxuKTogc3RyaW5nIHtcbiAgICBkaXJlY3RvcnkgPz89IGdldFByb2plY3RzRGlyZWN0b3J5KClcbiAgICBpZiAocGF0aE1vZHVsZS5leHRuYW1lKG9wZW5lZFBhdGgpLmVuZHNXaXRoKEJVTkRMRURfUFJPSkVDVF9TVUZGSVgpKSB7XG4gICAgICAgIGxvZ2dlci5sb2coYFBhdGggJyR7b3BlbmVkUGF0aH0nIGRlbm90ZXMgYSBidW5kbGVkIHByb2plY3QuYClcbiAgICAgICAgLy8gVGhlIHNlY29uZCBwYXJ0IG9mIGNvbmRpdGlvbiBpcyBmb3IgdGhlIGNhc2Ugd2hlbiBzb21lb25lIG5hbWVzIGEgZGlyZWN0b3J5XG4gICAgICAgIC8vIGxpa2UgYG15LXByb2plY3QuZW5zby1wcm9qZWN0YCBhbmQgc3RvcmVzIHRoZSBwcm9qZWN0IHRoZXJlLlxuICAgICAgICAvLyBOb3QgdGhlIG1vc3QgZm9ydHVuYXRlIG1vdmUsIGJ1dC4uLlxuICAgICAgICBpZiAoaXNQcm9qZWN0Um9vdChvcGVuZWRQYXRoKSkge1xuICAgICAgICAgICAgcmV0dXJuIGltcG9ydERpcmVjdG9yeShvcGVuZWRQYXRoLCBkaXJlY3RvcnksIG5hbWUpXG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAvLyBQcm9qZWN0IGJ1bmRsZSB3YXMgcHJvdmlkZWQsIHNvIHdlIG5lZWQgdG8gZXh0cmFjdCBpdCBmaXJzdC5cbiAgICAgICAgICAgIHJldHVybiBpbXBvcnRCdW5kbGUob3BlbmVkUGF0aCwgZGlyZWN0b3J5LCBuYW1lKVxuICAgICAgICB9XG4gICAgfSBlbHNlIHtcbiAgICAgICAgbG9nZ2VyLmxvZyhgT3BlbmluZyBub24tYnVuZGxlZCBmaWxlOiAnJHtvcGVuZWRQYXRofScuYClcbiAgICAgICAgY29uc3Qgcm9vdFBhdGggPSBnZXRQcm9qZWN0Um9vdChvcGVuZWRQYXRoKVxuICAgICAgICAvLyBDaGVjayBpZiB0aGUgcHJvamVjdCByb290IGlzIHVuZGVyIHRoZSBwcm9qZWN0cyBkaXJlY3RvcnkuIElmIGl0IGlzLCB3ZSBjYW4gb3BlbiBpdC5cbiAgICAgICAgLy8gT3RoZXJ3aXNlLCB3ZSBuZWVkIHRvIGluc3RhbGwgaXQgZmlyc3QuXG4gICAgICAgIGlmIChyb290UGF0aCA9PSBudWxsKSB7XG4gICAgICAgICAgICBjb25zdCBwcm9kdWN0TmFtZSA9IGNvbW1vbi5QUk9EVUNUX05BTUVcbiAgICAgICAgICAgIGNvbnN0IG1lc3NhZ2UgPSBgRmlsZSAnJHtvcGVuZWRQYXRofScgZG9lcyBub3QgYmVsb25nIHRvIHRoZSAke3Byb2R1Y3ROYW1lfSBwcm9qZWN0LmBcbiAgICAgICAgICAgIHRocm93IG5ldyBFcnJvcihtZXNzYWdlKVxuICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgcmV0dXJuIGltcG9ydERpcmVjdG9yeShyb290UGF0aCwgZGlyZWN0b3J5LCBuYW1lKVxuICAgICAgICB9XG4gICAgfVxufVxuXG4vKiogSW1wb3J0IHRoZSBwcm9qZWN0IGZyb20gYSBidW5kbGUuXG4gKiBAcmV0dXJucyBQcm9qZWN0IElEIChmcm9tIFByb2plY3QgTWFuYWdlcidzIG1ldGFkYXRhKSBpZGVudGlmeWluZyB0aGUgaW1wb3J0ZWQgcHJvamVjdC4gKi9cbmV4cG9ydCBmdW5jdGlvbiBpbXBvcnRCdW5kbGUoXG4gICAgYnVuZGxlUGF0aDogc3RyaW5nLFxuICAgIGRpcmVjdG9yeT86IHN0cmluZyB8IG51bGwsXG4gICAgbmFtZTogc3RyaW5nIHwgbnVsbCA9IG51bGxcbik6IHN0cmluZyB7XG4gICAgZGlyZWN0b3J5ID8/PSBnZXRQcm9qZWN0c0RpcmVjdG9yeSgpXG4gICAgbG9nZ2VyLmxvZyhcbiAgICAgICAgYEltcG9ydGluZyBwcm9qZWN0ICcke2J1bmRsZVBhdGh9JyBmcm9tIGJ1bmRsZSR7bmFtZSAhPSBudWxsID8gYCBhcyAnJHtuYW1lfSdgIDogJyd9LmBcbiAgICApXG4gICAgLy8gVGhlIGJ1bmRsZSBpcyBhIHRhcmJhbGwsIHNvIHdlIGp1c3QgbmVlZCB0byBleHRyYWN0IGl0IHRvIHRoZSByaWdodCBsb2NhdGlvbi5cbiAgICBjb25zdCBidW5kbGVQcmVmaXggPSBwcmVmaXhJbkJ1bmRsZShidW5kbGVQYXRoKVxuICAgIC8vIFdlIGNhcmUgYWJvdXQgc3B1cmlvdXMgJy4nIGFuZCAnLi4nIHdoZW4gc3RyaXBwaW5nIHBhdGhzIGJ1dCBub3Qgd2hlbiBnZW5lcmF0aW5nIG5hbWUuXG4gICAgY29uc3Qgbm9ybWFsaXplZEJ1bmRsZVByZWZpeCA9XG4gICAgICAgIGJ1bmRsZVByZWZpeCAhPSBudWxsXG4gICAgICAgICAgICA/IHBhdGhNb2R1bGUubm9ybWFsaXplKGJ1bmRsZVByZWZpeCkucmVwbGFjZSgvW1xcXFwvXSskLywgJycpIC8vIEFsc28gc3RyaXAgdHJhaWxpbmcgc2xhc2guXG4gICAgICAgICAgICA6IG51bGxcbiAgICBjb25zdCBkaXJOYW1lQmFzZSA9XG4gICAgICAgIG5vcm1hbGl6ZWRCdW5kbGVQcmVmaXggIT0gbnVsbCAmJlxuICAgICAgICBub3JtYWxpemVkQnVuZGxlUHJlZml4ICE9PSAnLicgJiZcbiAgICAgICAgbm9ybWFsaXplZEJ1bmRsZVByZWZpeCAhPT0gJy4uJ1xuICAgICAgICAgICAgPyBub3JtYWxpemVkQnVuZGxlUHJlZml4XG4gICAgICAgICAgICA6IGJ1bmRsZVBhdGhcbiAgICBsb2dnZXIubG9nKGBCdW5kbGUgbm9ybWFsaXplZCBwcmVmaXg6ICcke1N0cmluZyhub3JtYWxpemVkQnVuZGxlUHJlZml4KX0nLmApXG4gICAgY29uc3QgdGFyZ2V0UGF0aCA9IGdlbmVyYXRlRGlyZWN0b3J5TmFtZShkaXJOYW1lQmFzZSwgZGlyZWN0b3J5KVxuICAgIGxvZ2dlci5sb2coYEltcG9ydGluZyBwcm9qZWN0IGFzICcke3RhcmdldFBhdGh9Jy5gKVxuICAgIGZzLm1rZGlyU3luYyh0YXJnZXRQYXRoLCB7IHJlY3Vyc2l2ZTogdHJ1ZSB9KVxuICAgIC8vIFRvIGJlIG1vcmUgcmVzaWxpZW50IGFnYWluc3QgZGlmZmVyZW50IHdheXMgdGhhdCB1c2VyIG1pZ2h0IGF0dGVtcHQgdG8gY3JlYXRlIGEgYnVuZGxlLFxuICAgIC8vIHdlIHRyeSB0byBzdXBwb3J0IGJvdGggYXJjaGl2ZXMgdGhhdDpcbiAgICAvLyAqIGNvbnRhaW4gYSBzaW5nbGUgZGlyZWN0b3J5IHdpdGggdGhlIHByb2plY3QgZmlsZXMgLSB0aGF0IGRpcmVjdG9yeSBuYW1lIHdpbGwgYmUgdXNlZFxuICAgIC8vICAgdG8gZ2VuZXJhdGUgYSBuZXcgdGFyZ2V0IGRpcmVjdG9yeSBuYW1lO1xuICAgIC8vICogY29udGFpbiB0aGUgcHJvamVjdCBmaWxlcyBkaXJlY3RseSAtIGluIHRoaXMgY2FzZSwgdGhlIGFyY2hpdmUgZmlsZW5hbWUgd2lsbCBiZSB1c2VkXG4gICAgLy8gICB0byBnZW5lcmF0ZSBhIG5ldyB0YXJnZXQgZGlyZWN0b3J5IG5hbWUuXG4gICAgLy8gV2UgdHJ5IHRvIHRlbGwgYXBhcnQgdGhlc2UgdHdvIGNhc2VzIGJ5IGxvb2tpbmcgYXQgdGhlIGNvbW1vbiBwcmVmaXggb2YgdGhlIHBhdGhzXG4gICAgLy8gb2YgdGhlIGZpbGVzIGluIHRoZSBhcmNoaXZlLiBJZiB0aGVyZSBpcyBhbnksIGV2ZXJ5dGhpbmcgaXMgdW5kZXIgYSBzaW5nbGUgZGlyZWN0b3J5LFxuICAgIC8vIGFuZCB3ZSBuZWVkIHRvIHN0cmlwIGl0LlxuICAgIC8vXG4gICAgLy8gQWRkaXRpb25hbGx5LCB3ZSBuZWVkIHRvIHRha2UgaW50byBhY2NvdW50IHRoYXQgcGF0aHMgbWlnaHQgYmUgcHJlZml4ZWQgd2l0aCBgLi9gIG9yIG5vdC5cbiAgICAvLyBUaHVzLCB3ZSBuZWVkIHRvIGFkanVzdCB0aGUgbnVtYmVyIG9mIHBhdGggY29tcG9uZW50cyB0byBzdHJpcCBhY2NvcmRpbmdseS5cblxuICAgIGxvZ2dlci5sb2coYEV4dHJhY3RpbmcgYnVuZGxlOiAnJHtidW5kbGVQYXRofScgLT4gJyR7dGFyZ2V0UGF0aH0nLmApXG5cbiAgICAvLyBTdHJpcCB0cmFpbGluZyBzZXBhcmF0b3IgYW5kIHNwbGl0IHRoZSBwYXRoIGludG8gcGllY2VzLlxuICAgIGNvbnN0IHJvb3RQaWVjZXMgPSBidW5kbGVQcmVmaXggIT0gbnVsbCA/IGJ1bmRsZVByZWZpeC5zcGxpdCgvW1xcXFwvXS8pIDogW11cblxuICAgIC8vIElmIHRoZSBsYXN0IGVsZW1lbnQgaXMgZW1wdHkgc3RyaW5nIChpLmUuIHdlIGhhZCB0cmFpbGluZyBzZXBhcmF0b3IpLCBkcm9wIGl0LlxuICAgIGlmIChyb290UGllY2VzLmxlbmd0aCA+IDAgJiYgcm9vdFBpZWNlc1tyb290UGllY2VzLmxlbmd0aCAtIDFdID09PSAnJykge1xuICAgICAgICByb290UGllY2VzLnBvcCgpXG4gICAgfVxuXG4gICAgdGFyLmV4dHJhY3Qoe1xuICAgICAgICBmaWxlOiBidW5kbGVQYXRoLFxuICAgICAgICBjd2Q6IHRhcmdldFBhdGgsXG4gICAgICAgIHN5bmM6IHRydWUsXG4gICAgICAgIHN0cmlwOiByb290UGllY2VzLmxlbmd0aCxcbiAgICB9KVxuICAgIHJldHVybiBidW1wTWV0YWRhdGEodGFyZ2V0UGF0aCwgbmFtZSA/PyBudWxsKVxufVxuXG4vKiogVXBsb2FkIHRoZSBwcm9qZWN0IGZyb20gYSBidW5kbGUuICovXG5leHBvcnQgYXN5bmMgZnVuY3Rpb24gdXBsb2FkQnVuZGxlKFxuICAgIGJ1bmRsZTogc3RyZWFtLlJlYWRhYmxlLFxuICAgIGRpcmVjdG9yeT86IHN0cmluZyB8IG51bGwsXG4gICAgbmFtZTogc3RyaW5nIHwgbnVsbCA9IG51bGxcbik6IFByb21pc2U8c3RyaW5nPiB7XG4gICAgZGlyZWN0b3J5ID8/PSBnZXRQcm9qZWN0c0RpcmVjdG9yeSgpXG4gICAgbG9nZ2VyLmxvZyhgVXBsb2FkaW5nIHByb2plY3QgZnJvbSBidW5kbGUke25hbWUgIT0gbnVsbCA/IGAgYXMgJyR7bmFtZX0nYCA6ICcnfS5gKVxuICAgIGNvbnN0IHRhcmdldFBhdGggPSBnZW5lcmF0ZURpcmVjdG9yeU5hbWUobmFtZSA/PyAnUHJvamVjdCcsIGRpcmVjdG9yeSlcbiAgICBmcy5ta2RpclN5bmModGFyZ2V0UGF0aCwgeyByZWN1cnNpdmU6IHRydWUgfSlcbiAgICBhd2FpdCBuZXcgUHJvbWlzZTx2b2lkPihyZXNvbHZlID0+IHtcbiAgICAgICAgYnVuZGxlLnBpcGUodGFyLmV4dHJhY3QoeyBjd2Q6IHRhcmdldFBhdGggfSkpLm9uKCdmaW5pc2gnLCByZXNvbHZlKVxuICAgIH0pXG4gICAgY29uc3QgZW50cmllcyA9IGZzLnJlYWRkaXJTeW5jKHRhcmdldFBhdGgpXG4gICAgY29uc3QgZmlyc3RFbnRyeSA9IGVudHJpZXNbMF1cbiAgICAvLyBJZiB0aGUgZGlyZWN0b3J5IG9ubHkgY29udGFpbnMgb25lIHN1YmRpcmVjdG9yeSwgcmVwbGFjZSB0aGUgZGlyZWN0b3J5IHdpdGggaXRzIHNvbGVcbiAgICAvLyBzdWJkaXJlY3RvcnkuXG4gICAgaWYgKGVudHJpZXMubGVuZ3RoID09PSAxICYmIGZpcnN0RW50cnkgIT0gbnVsbCkge1xuICAgICAgICBpZiAoZnMuc3RhdFN5bmMocGF0aE1vZHVsZS5qb2luKHRhcmdldFBhdGgsIGZpcnN0RW50cnkpKS5pc0RpcmVjdG9yeSgpKSB7XG4gICAgICAgICAgICBjb25zdCB0ZW1wb3JhcnlEaXJlY3RvcnlOYW1lID1cbiAgICAgICAgICAgICAgICB0YXJnZXRQYXRoICsgYF8ke2NyeXB0by5yYW5kb21VVUlEKCkuc3BsaXQoJy0nKVswXSA/PyAnJ31gXG4gICAgICAgICAgICBmcy5yZW5hbWVTeW5jKHRhcmdldFBhdGgsIHRlbXBvcmFyeURpcmVjdG9yeU5hbWUpXG4gICAgICAgICAgICBmcy5yZW5hbWVTeW5jKHBhdGhNb2R1bGUuam9pbih0ZW1wb3JhcnlEaXJlY3RvcnlOYW1lLCBmaXJzdEVudHJ5KSwgdGFyZ2V0UGF0aClcbiAgICAgICAgICAgIGZzLnJtZGlyU3luYyh0ZW1wb3JhcnlEaXJlY3RvcnlOYW1lKVxuICAgICAgICB9XG4gICAgfVxuICAgIHJldHVybiBidW1wTWV0YWRhdGEodGFyZ2V0UGF0aCwgbmFtZSA/PyBudWxsKVxufVxuXG4vKiogSW1wb3J0IHRoZSBwcm9qZWN0IHNvIGl0IGJlY29tZXMgdmlzaWJsZSB0byB0aGUgUHJvamVjdCBNYW5hZ2VyLlxuICogQHJldHVybnMgVGhlIHByb2plY3QgSUQgKGZyb20gdGhlIFByb2plY3QgTWFuYWdlcidzIG1ldGFkYXRhKSBpZGVudGlmeWluZyB0aGUgaW1wb3J0ZWQgcHJvamVjdC5cbiAqIEB0aHJvd3Mge0Vycm9yfSBpZiBhIHJhY2UgY29uZGl0aW9uIG9jY3VycyB3aGVuIGdlbmVyYXRpbmcgYSB1bmlxdWUgcHJvamVjdCBkaXJlY3RvcnkgbmFtZS4gKi9cbmV4cG9ydCBmdW5jdGlvbiBpbXBvcnREaXJlY3RvcnkoXG4gICAgcm9vdFBhdGg6IHN0cmluZyxcbiAgICBkaXJlY3Rvcnk/OiBzdHJpbmcgfCBudWxsLFxuICAgIG5hbWU6IHN0cmluZyB8IG51bGwgPSBudWxsXG4pOiBzdHJpbmcge1xuICAgIGRpcmVjdG9yeSA/Pz0gZ2V0UHJvamVjdHNEaXJlY3RvcnkoKVxuICAgIGlmIChpc1Byb2plY3RJbnN0YWxsZWQocm9vdFBhdGgsIGRpcmVjdG9yeSkpIHtcbiAgICAgICAgLy8gUHJvamVjdCBpcyBhbHJlYWR5IHZpc2libGUgdG8gUHJvamVjdCBNYW5hZ2VyLCBzbyB3ZSBjYW4ganVzdCByZXR1cm4gaXRzIElELlxuICAgICAgICBsb2dnZXIubG9nKGBQcm9qZWN0IGFscmVhZHkgaW5zdGFsbGVkIGF0ICcke3Jvb3RQYXRofScuYClcbiAgICAgICAgY29uc3QgaWQgPSBnZXRQcm9qZWN0SWQocm9vdFBhdGgpXG4gICAgICAgIGlmIChpZCAhPSBudWxsKSB7XG4gICAgICAgICAgICByZXR1cm4gaWRcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIHRocm93IG5ldyBFcnJvcihgUHJvamVjdCBhbHJlYWR5IGluc3RhbGxlZCwgYnV0IG1pc3NpbmcgbWV0YWRhdGEuYClcbiAgICAgICAgfVxuICAgIH0gZWxzZSB7XG4gICAgICAgIGxvZ2dlci5sb2coXG4gICAgICAgICAgICBgSW1wb3J0aW5nIGEgcHJvamVjdCBjb3B5IGZyb20gJyR7cm9vdFBhdGh9JyR7bmFtZSAhPSBudWxsID8gYCBhcyAnJHtuYW1lfSdgIDogJyd9LmBcbiAgICAgICAgKVxuICAgICAgICBjb25zdCB0YXJnZXRQYXRoID0gZ2VuZXJhdGVEaXJlY3RvcnlOYW1lKHJvb3RQYXRoLCBkaXJlY3RvcnkpXG4gICAgICAgIGlmIChmcy5leGlzdHNTeW5jKHRhcmdldFBhdGgpKSB7XG4gICAgICAgICAgICB0aHJvdyBuZXcgRXJyb3IoYFByb2plY3QgZGlyZWN0b3J5ICcke3RhcmdldFBhdGh9JyBhbHJlYWR5IGV4aXN0cy5gKVxuICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgbG9nZ2VyLmxvZyhgQ29weWluZzogJyR7cm9vdFBhdGh9JyAtPiAnJHt0YXJnZXRQYXRofScuYClcbiAgICAgICAgICAgIGZzLmNwU3luYyhyb290UGF0aCwgdGFyZ2V0UGF0aCwgeyByZWN1cnNpdmU6IHRydWUgfSlcbiAgICAgICAgICAgIC8vIFVwZGF0ZSB0aGUgcHJvamVjdCBJRCwgc28gd2UgYXJlIGNlcnRhaW4gdGhhdCBpdCBpcyB1bmlxdWUuXG4gICAgICAgICAgICAvLyBUaGlzIHdvdWxkIGJlIHZpb2xhdGVkLCBpZiB3ZSBpbXBvcnRlZCB0aGUgc2FtZSBwcm9qZWN0IG11bHRpcGxlIHRpbWVzLlxuICAgICAgICAgICAgcmV0dXJuIGJ1bXBNZXRhZGF0YSh0YXJnZXRQYXRoLCBuYW1lID8/IG51bGwpXG4gICAgICAgIH1cbiAgICB9XG59XG5cbi8vID09PT09PT09PT09PT09PT1cbi8vID09PSBNZXRhZGF0YSA9PT1cbi8vID09PT09PT09PT09PT09PT1cblxuLyoqIFRoZSBQcm9qZWN0IE1hbmFnZXIncyBtZXRhZGF0YSBhc3NvY2lhdGVkIHdpdGggYSBwcm9qZWN0LiAqL1xuaW50ZXJmYWNlIFByb2plY3RNZXRhZGF0YSB7XG4gICAgLyoqIFRoZSBJRCBvZiB0aGUgcHJvamVjdC4gSXQgaXMgb25seSB1c2VkIGluIGNvbW11bmljYXRpb24gd2l0aCBwcm9qZWN0IG1hbmFnZXI7XG4gICAgICogaXQgaGFzIG5vIHNlbWFudGljIG1lYW5pbmcuICovXG4gICAgcmVhZG9ubHkgaWQ6IHN0cmluZ1xuICAgIC8qKiBUaGUgcHJvamVjdCB2YXJpYW50LiBUaGlzIGlzIGN1cnJlbnRseSBhbHdheXMgYFVzZXJQcm9qZWN0YC4gKi9cbiAgICByZWFkb25seSBraW5kOiAnVXNlclByb2plY3QnXG4gICAgLyoqIFRoZSBkYXRlIGF0IHdoaWNoIHRoZSBwcm9qZWN0IHdhcyBjcmVhdGVkLCBpbiBSRkMzMzM5IGZvcm1hdC4gKi9cbiAgICByZWFkb25seSBjcmVhdGVkOiBzdHJpbmdcbiAgICAvKiogVGhlIGRhdGUgYXQgd2hpY2ggdGhlIHByb2plY3Qgd2FzIGxhc3Qgb3BlbmVkLCBpbiBSRkMzMzM5IGZvcm1hdC4gKi9cbiAgICByZWFkb25seSBsYXN0T3BlbmVkOiBzdHJpbmdcbn1cblxuLyoqIEEgdHlwZSBndWFyZCBmdW5jdGlvbiB0byBjaGVjayBpZiBhbiBvYmplY3QgY29uZm9ybXMgdG8gdGhlIHtAbGluayBQcm9qZWN0TWV0YWRhdGF9IGludGVyZmFjZS5cbiAqXG4gKiBUaGlzIGZ1bmN0aW9uIGNoZWNrcyBpZiB0aGUgaW5wdXQgb2JqZWN0IGhhcyB0aGUgcmVxdWlyZWQgcHJvcGVydGllcyBhbmQgY29ycmVjdCB0eXBlc1xuICogdG8gbWF0Y2ggdGhlIHtAbGluayBQcm9qZWN0TWV0YWRhdGF9IGludGVyZmFjZS4gSXQgY2FuIGJlIHVzZWQgYXQgcnVudGltZSB0byB2YWxpZGF0ZSB0aGF0XG4gKiBhIGdpdmVuIG9iamVjdCBoYXMgdGhlIGV4cGVjdGVkIHNoYXBlLlxuICogQHBhcmFtIHZhbHVlIC0gVGhlIG9iamVjdCB0byBjaGVjayBhZ2FpbnN0IHRoZSBQcm9qZWN0TWV0YWRhdGEgaW50ZXJmYWNlLlxuICogQHJldHVybnMgQSBib29sZWFuIHZhbHVlIGluZGljYXRpbmcgd2hldGhlciB0aGUgb2JqZWN0IG1hdGNoZXNcbiAqIHRoZSB7QGxpbmsgUHJvamVjdE1ldGFkYXRhfSBpbnRlcmZhY2UuICovXG5mdW5jdGlvbiBpc1Byb2plY3RNZXRhZGF0YSh2YWx1ZTogdW5rbm93bik6IHZhbHVlIGlzIFByb2plY3RNZXRhZGF0YSB7XG4gICAgcmV0dXJuIChcbiAgICAgICAgdHlwZW9mIHZhbHVlID09PSAnb2JqZWN0JyAmJiB2YWx1ZSAhPSBudWxsICYmICdpZCcgaW4gdmFsdWUgJiYgdHlwZW9mIHZhbHVlLmlkID09PSAnc3RyaW5nJ1xuICAgIClcbn1cblxuLyoqIEdldCB0aGUgSUQgZnJvbSB0aGUgcHJvamVjdCBtZXRhZGF0YS4gKi9cbmV4cG9ydCBmdW5jdGlvbiBnZXRQcm9qZWN0SWQocHJvamVjdFJvb3Q6IHN0cmluZyk6IHN0cmluZyB8IG51bGwge1xuICAgIHJldHVybiBnZXRNZXRhZGF0YShwcm9qZWN0Um9vdCk/LmlkID8/IG51bGxcbn1cblxuLyoqIFVwZGF0ZSB0aGUgcGFja2FnZSBuYW1lLiAqL1xuZXhwb3J0IGZ1bmN0aW9uIHVwZGF0ZVBhY2thZ2VOYW1lKHByb2plY3RSb290OiBzdHJpbmcsIG5hbWU6IHN0cmluZykge1xuICAgIGNvbnN0IHBhdGggPSBwYXRoTW9kdWxlLmpvaW4ocHJvamVjdFJvb3QsIFBBQ0tBR0VfTUVUQURBVEFfUkVMQVRJVkVfUEFUSClcbiAgICBjb25zdCBjb250ZW50cyA9IGZzLnJlYWRGaWxlU3luYyhwYXRoLCB7IGVuY29kaW5nOiAndXRmLTgnIH0pXG4gICAgY29uc3QgbmV3Q29udGVudHMgPSBjb250ZW50cy5yZXBsYWNlKC9ebmFtZTogLiovLCBgbmFtZTogJHtuYW1lfWApXG4gICAgZnMud3JpdGVGaWxlU3luYyhwYXRoLCBuZXdDb250ZW50cylcbn1cblxuLyoqIENyZWF0ZSBhIHByb2plY3QncyBtZXRhZGF0YS4gKi9cbmV4cG9ydCBmdW5jdGlvbiBjcmVhdGVNZXRhZGF0YSgpOiBQcm9qZWN0TWV0YWRhdGEge1xuICAgIHJldHVybiB7XG4gICAgICAgIGlkOiBnZW5lcmF0ZUlkKCksXG4gICAgICAgIGtpbmQ6ICdVc2VyUHJvamVjdCcsXG4gICAgICAgIGNyZWF0ZWQ6IG5ldyBEYXRlKCkudG9JU09TdHJpbmcoKSxcbiAgICAgICAgbGFzdE9wZW5lZDogbmV3IERhdGUoKS50b0lTT1N0cmluZygpLFxuICAgIH1cbn1cblxuLyoqIFJldHJpZXZlIHRoZSBwcm9qZWN0J3MgbWV0YWRhdGEuICovXG5leHBvcnQgZnVuY3Rpb24gZ2V0TWV0YWRhdGEocHJvamVjdFJvb3Q6IHN0cmluZyk6IFByb2plY3RNZXRhZGF0YSB8IG51bGwge1xuICAgIGNvbnN0IG1ldGFkYXRhUGF0aCA9IHBhdGhNb2R1bGUuam9pbihwcm9qZWN0Um9vdCwgUFJPSkVDVF9NRVRBREFUQV9SRUxBVElWRV9QQVRIKVxuICAgIHRyeSB7XG4gICAgICAgIGNvbnN0IGpzb25UZXh0ID0gZnMucmVhZEZpbGVTeW5jKG1ldGFkYXRhUGF0aCwgJ3V0ZjgnKVxuICAgICAgICBjb25zdCBtZXRhZGF0YTogdW5rbm93biA9IEpTT04ucGFyc2UoanNvblRleHQpXG4gICAgICAgIHJldHVybiBpc1Byb2plY3RNZXRhZGF0YShtZXRhZGF0YSkgPyBtZXRhZGF0YSA6IG51bGxcbiAgICB9IGNhdGNoIHtcbiAgICAgICAgcmV0dXJuIG51bGxcbiAgICB9XG59XG5cbi8qKiBXcml0ZSB0aGUgcHJvamVjdCdzIG1ldGFkYXRhLiAqL1xuZXhwb3J0IGZ1bmN0aW9uIHdyaXRlTWV0YWRhdGEocHJvamVjdFJvb3Q6IHN0cmluZywgbWV0YWRhdGE6IFByb2plY3RNZXRhZGF0YSk6IHZvaWQge1xuICAgIGNvbnN0IG1ldGFkYXRhUGF0aCA9IHBhdGhNb2R1bGUuam9pbihwcm9qZWN0Um9vdCwgUFJPSkVDVF9NRVRBREFUQV9SRUxBVElWRV9QQVRIKVxuICAgIGZzLm1rZGlyU3luYyhwYXRoTW9kdWxlLmRpcm5hbWUobWV0YWRhdGFQYXRoKSwgeyByZWN1cnNpdmU6IHRydWUgfSlcbiAgICBmcy53cml0ZUZpbGVTeW5jKG1ldGFkYXRhUGF0aCwgSlNPTi5zdHJpbmdpZnkobWV0YWRhdGEsIG51bGwsIGJ1aWxkVXRpbHMuSU5ERU5UX1NJWkUpKVxufVxuXG4vKiogVXBkYXRlIHRoZSBwcm9qZWN0J3MgbWV0YWRhdGEuXG4gKiBJZiB0aGUgcHJvdmlkZWQgdXBkYXRlciBkb2VzIG5vdCByZXR1cm4gYW55dGhpbmcsIHRoZSBtZXRhZGF0YSBmaWxlIGlzIGxlZnQgaW50YWN0LlxuICpcbiAqIFJldHVybnMgdGhlIG1ldGFkYXRhIHJldHVybmVkIGZyb20gdGhlIHVwZGF0ZXIgZnVuY3Rpb24uICovXG5leHBvcnQgZnVuY3Rpb24gdXBkYXRlTWV0YWRhdGEoXG4gICAgcHJvamVjdFJvb3Q6IHN0cmluZyxcbiAgICB1cGRhdGVyOiAoaW5pdGlhbE1ldGFkYXRhOiBQcm9qZWN0TWV0YWRhdGEpID0+IFByb2plY3RNZXRhZGF0YVxuKTogUHJvamVjdE1ldGFkYXRhIHtcbiAgICBjb25zdCBtZXRhZGF0YSA9IGdldE1ldGFkYXRhKHByb2plY3RSb290KVxuICAgIGNvbnN0IHVwZGF0ZWRNZXRhZGF0YSA9IHVwZGF0ZXIobWV0YWRhdGEgPz8gY3JlYXRlTWV0YWRhdGEoKSlcbiAgICB3cml0ZU1ldGFkYXRhKHByb2plY3RSb290LCB1cGRhdGVkTWV0YWRhdGEpXG4gICAgcmV0dXJuIHVwZGF0ZWRNZXRhZGF0YVxufVxuXG4vLyA9PT09PT09PT09PT09PT09PT09PT09PT09XG4vLyA9PT0gUHJvamVjdCBEaXJlY3RvcnkgPT09XG4vLyA9PT09PT09PT09PT09PT09PT09PT09PT09XG5cbi8qKiBDaGVjayBpZiB0aGUgZ2l2ZW4gcGF0aCByZXByZXNlbnRzIHRoZSByb290IG9mIGFuIEVuc28gcHJvamVjdC5cbiAqIFRoaXMgaXMgZGVjaWRlZCBieSB0aGUgcHJlc2VuY2Ugb2YgdGhlIFByb2plY3QgTWFuYWdlcidzIG1ldGFkYXRhLiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGlzUHJvamVjdFJvb3QoY2FuZGlkYXRlUGF0aDogc3RyaW5nKTogYm9vbGVhbiB7XG4gICAgY29uc3QgcHJvamVjdEpzb25QYXRoID0gcGF0aE1vZHVsZS5qb2luKGNhbmRpZGF0ZVBhdGgsIFBST0pFQ1RfTUVUQURBVEFfUkVMQVRJVkVfUEFUSClcbiAgICB0cnkge1xuICAgICAgICBmcy5hY2Nlc3NTeW5jKHByb2plY3RKc29uUGF0aCwgZnMuY29uc3RhbnRzLlJfT0spXG4gICAgICAgIHJldHVybiB0cnVlXG4gICAgfSBjYXRjaCB7XG4gICAgICAgIHJldHVybiBmYWxzZVxuICAgIH1cbn1cblxuLyoqIENoZWNrIGlmIHRoaXMgYnVuZGxlIGlzIGEgY29tcHJlc3NlZCBkaXJlY3RvcnkgKHJhdGhlciB0aGFuIGRpcmVjdGx5IGNvbnRhaW5pbmcgdGhlIHByb2plY3RcbiAqIGZpbGVzKS4gSWYgaXQgaXMsIHdlIHJldHVybiB0aGUgcGF0aCB0byB0aGUgZGlyZWN0b3J5LiBPdGhlcndpc2UsIHdlIHJldHVybiBgbnVsbGAuICovXG5leHBvcnQgZnVuY3Rpb24gcHJlZml4SW5CdW5kbGUoYnVuZGxlUGF0aDogc3RyaW5nKTogc3RyaW5nIHwgbnVsbCB7XG4gICAgLy8gV2UgbmVlZCB0byBsb29rIHVwIHRoZSByb290IGRpcmVjdG9yeSBhbW9uZyB0aGUgdGFyYmFsbCBlbnRyaWVzLlxuICAgIGxldCBjb21tb25QcmVmaXg6IHN0cmluZyB8IG51bGwgPSBudWxsXG4gICAgdGFyLmxpc3Qoe1xuICAgICAgICBmaWxlOiBidW5kbGVQYXRoLFxuICAgICAgICBzeW5jOiB0cnVlLFxuICAgICAgICBvbmVudHJ5OiBlbnRyeSA9PiB7XG4gICAgICAgICAgICBjb25zdCBwYXRoID0gZW50cnkucGF0aFxuICAgICAgICAgICAgY29tbW9uUHJlZml4ID1cbiAgICAgICAgICAgICAgICBjb21tb25QcmVmaXggPT0gbnVsbCA/IHBhdGggOiBidWlsZFV0aWxzLmdldENvbW1vblByZWZpeChjb21tb25QcmVmaXgsIHBhdGgpXG4gICAgICAgIH0sXG4gICAgfSlcblxuICAgIC8vIEVTTGludCBkb2Vzbid0IGtub3cgdGhhdCBgY29tbW9uUHJlZml4YCBjYW4gYmUgbm90IGBudWxsYCBoZXJlIGR1ZSB0byB0aGUgYG9uZW50cnlgIGNhbGxiYWNrLlxuICAgIC8vIGVzbGludC1kaXNhYmxlLW5leHQtbGluZSBAdHlwZXNjcmlwdC1lc2xpbnQvbm8tdW5uZWNlc3NhcnktY29uZGl0aW9uXG4gICAgcmV0dXJuIGNvbW1vblByZWZpeCAhPSBudWxsICYmIGNvbW1vblByZWZpeCAhPT0gJycgPyBjb21tb25QcmVmaXggOiBudWxsXG59XG5cbi8qKiBHZW5lcmF0ZSBhIG5hbWUgZm9yIGEgcHJvamVjdCB1c2luZyBnaXZlbiBiYXNlIHN0cmluZy4gQSBzdWZmaXggaXMgYWRkZWQgaWYgdGhlcmUgaXMgYVxuICogY29sbGlzaW9uLlxuICpcbiAqIEZvciBleGFtcGxlIGBOYW1lYCB3aWxsIGJlY29tZSBgTmFtZV8xYCBpZiB0aGVyZSdzIGFscmVhZHkgYSBkaXJlY3RvcnkgbmFtZWQgYE5hbWVgLlxuICogSWYgZ2l2ZW4gYSBuYW1lIGxpa2UgYE5hbWVfMWAgaXQgd2lsbCBiZWNvbWUgYE5hbWVfMmAgaWYgdGhlcmUgaXMgYWxyZWFkeSBhIGRpcmVjdG9yeSBuYW1lZFxuICogYE5hbWVfMWAuIElmIGEgcGF0aCBjb250YWluaW5nIG11bHRpcGxlIGNvbXBvbmVudHMgaXMgZ2l2ZW4sIG9ubHkgdGhlIGxhc3QgY29tcG9uZW50IGlzIHVzZWRcbiAqIGZvciB0aGUgbmFtZS4gKi9cbmV4cG9ydCBmdW5jdGlvbiBnZW5lcmF0ZURpcmVjdG9yeU5hbWUobmFtZTogc3RyaW5nLCBkaXJlY3RvcnkgPSBnZXRQcm9qZWN0c0RpcmVjdG9yeSgpKTogc3RyaW5nIHtcbiAgICAvLyBVc2Ugb25seSB0aGUgbGFzdCBwYXRoIGNvbXBvbmVudC5cbiAgICBuYW1lID0gcGF0aE1vZHVsZS5wYXJzZShuYW1lKS5uYW1lXG5cbiAgICAvLyBJZiB0aGUgbmFtZSBhbHJlYWR5IGNvbnNpc3RzIGEgc3VmZml4LCByZXVzZSBpdC5cbiAgICBjb25zdCBtYXRjaGVzID0gbmFtZS5tYXRjaCgvXiguKilfKFxcZCspJC8pXG4gICAgY29uc3QgaW5pdGlhbFN1ZmZpeCA9IC0xXG4gICAgbGV0IHN1ZmZpeCA9IGluaXRpYWxTdWZmaXhcbiAgICAvLyBNYXRjaGVzIHN0YXJ0IHdpdGggdGhlIHdob2xlIG1hdGNoLCBzbyB3ZSBuZWVkIHRvIHNraXAgaXQuIFRoZW4gY29tZSBvdXIgdHdvIGNhcHR1cmUgZ3JvdXBzLlxuICAgIGNvbnN0IFttYXRjaGVkTmFtZSwgbWF0Y2hlZFN1ZmZpeF0gPSBtYXRjaGVzPy5zbGljZSgxKSA/PyBbXVxuICAgIGlmICh0eXBlb2YgbWF0Y2hlZE5hbWUgIT09ICd1bmRlZmluZWQnICYmIHR5cGVvZiBtYXRjaGVkU3VmZml4ICE9PSAndW5kZWZpbmVkJykge1xuICAgICAgICBuYW1lID0gbWF0Y2hlZE5hbWVcbiAgICAgICAgc3VmZml4ID0gcGFyc2VJbnQobWF0Y2hlZFN1ZmZpeClcbiAgICB9XG5cbiAgICBsZXQgZmluYWxQYXRoOiBzdHJpbmdcbiAgICB3aGlsZSAodHJ1ZSkge1xuICAgICAgICBzdWZmaXgrK1xuICAgICAgICBjb25zdCBuZXdOYW1lID0gYCR7bmFtZX0ke3N1ZmZpeCA9PT0gMCA/ICcnIDogYF8ke3N1ZmZpeH1gfWBcbiAgICAgICAgY29uc3QgY2FuZGlkYXRlUGF0aCA9IHBhdGhNb2R1bGUuam9pbihkaXJlY3RvcnksIG5ld05hbWUpXG4gICAgICAgIGlmICghZnMuZXhpc3RzU3luYyhjYW5kaWRhdGVQYXRoKSkge1xuICAgICAgICAgICAgZmluYWxQYXRoID0gY2FuZGlkYXRlUGF0aFxuICAgICAgICAgICAgYnJlYWtcbiAgICAgICAgfVxuICAgIH1cbiAgICByZXR1cm4gZmluYWxQYXRoXG59XG5cbi8qKiBUYWtlIGEgcGF0aCB0byBhIGZpbGUsIHByZXN1bWFibHkgbG9jYXRlZCBpbiBhIHByb2plY3QncyBzdWJ0cmVlLlJldHVybnMgdGhlIHBhdGhcbiAqIHRvIHRoZSBwcm9qZWN0J3Mgcm9vdCBkaXJlY3Rvcnkgb3IgYG51bGxgIGlmIHRoZSBmaWxlIGlzIG5vdCBsb2NhdGVkIGluIGEgcHJvamVjdC4gKi9cbmV4cG9ydCBmdW5jdGlvbiBnZXRQcm9qZWN0Um9vdChzdWJ0cmVlUGF0aDogc3RyaW5nKTogc3RyaW5nIHwgbnVsbCB7XG4gICAgbGV0IGN1cnJlbnRQYXRoID0gc3VidHJlZVBhdGhcbiAgICB3aGlsZSAoIWlzUHJvamVjdFJvb3QoY3VycmVudFBhdGgpKSB7XG4gICAgICAgIGNvbnN0IHBhcmVudCA9IHBhdGhNb2R1bGUuZGlybmFtZShjdXJyZW50UGF0aClcbiAgICAgICAgaWYgKHBhcmVudCA9PT0gY3VycmVudFBhdGgpIHtcbiAgICAgICAgICAgIC8vIGVzbGludC1kaXNhYmxlLW5leHQtbGluZSBuby1yZXN0cmljdGVkLXN5bnRheFxuICAgICAgICAgICAgcmV0dXJuIG51bGxcbiAgICAgICAgfVxuICAgICAgICBjdXJyZW50UGF0aCA9IHBhcmVudFxuICAgIH1cbiAgICByZXR1cm4gY3VycmVudFBhdGhcbn1cblxuLyoqIEdldCB0aGUgZGlyZWN0b3J5IHRoYXQgc3RvcmVzIEVuc28gcHJvamVjdHMuICovXG5leHBvcnQgZnVuY3Rpb24gZ2V0UHJvamVjdHNEaXJlY3RvcnkoKTogc3RyaW5nIHtcbiAgICByZXR1cm4gcGF0aE1vZHVsZS5qb2luKG9zLmhvbWVkaXIoKSwgJ2Vuc28nLCAncHJvamVjdHMnKVxufVxuXG4vKiogQ2hlY2sgaWYgdGhlIGdpdmVuIHByb2plY3QgaXMgaW5zdGFsbGVkLCBpLmUuIGNhbiBiZSBvcGVuZWQgd2l0aCB0aGUgUHJvamVjdCBNYW5hZ2VyLiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGlzUHJvamVjdEluc3RhbGxlZChcbiAgICBwcm9qZWN0Um9vdDogc3RyaW5nLFxuICAgIGRpcmVjdG9yeSA9IGdldFByb2plY3RzRGlyZWN0b3J5KClcbik6IGJvb2xlYW4ge1xuICAgIGNvbnN0IHByb2plY3RSb290UGFyZW50ID0gcGF0aE1vZHVsZS5kaXJuYW1lKHByb2plY3RSb290KVxuICAgIC8vIFNob3VsZCByZXNvbHZlIHN5bWxpbmtzIGFuZCByZWxhdGl2ZSBwYXRocy4gTm9ybWFsaXplIGJlZm9yZSBjb21wYXJpc29uLlxuICAgIHJldHVybiBwYXRoTW9kdWxlLnJlc29sdmUocHJvamVjdFJvb3RQYXJlbnQpID09PSBwYXRoTW9kdWxlLnJlc29sdmUoZGlyZWN0b3J5KVxufVxuXG4vLyA9PT09PT09PT09PT09PT09PT1cbi8vID09PSBQcm9qZWN0IElEID09PVxuLy8gPT09PT09PT09PT09PT09PT09XG5cbi8qKiBHZW5lcmF0ZSBhIHVuaXF1ZSBVVUlEIGZvciBhIHByb2plY3QuICovXG5leHBvcnQgZnVuY3Rpb24gZ2VuZXJhdGVJZCgpOiBzdHJpbmcge1xuICAgIHJldHVybiBjcnlwdG8ucmFuZG9tVVVJRCgpXG59XG5cbi8qKiBVcGRhdGUgdGhlIHByb2plY3QncyBJRCB0byBhIG5ldywgdW5pcXVlIHZhbHVlLCBhbmQgaXRzIGxhc3Qgb3BlbmVkIGRhdGUgdG8gdGhlIGN1cnJlbnQgZGF0ZS4gKi9cbmV4cG9ydCBmdW5jdGlvbiBidW1wTWV0YWRhdGEocHJvamVjdFJvb3Q6IHN0cmluZywgbmFtZTogc3RyaW5nIHwgbnVsbCk6IHN0cmluZyB7XG4gICAgaWYgKG5hbWUgIT0gbnVsbCkge1xuICAgICAgICB1cGRhdGVQYWNrYWdlTmFtZShwcm9qZWN0Um9vdCwgbmFtZSlcbiAgICB9XG4gICAgcmV0dXJuIHVwZGF0ZU1ldGFkYXRhKHByb2plY3RSb290LCBtZXRhZGF0YSA9PiAoe1xuICAgICAgICAuLi5tZXRhZGF0YSxcbiAgICAgICAgaWQ6IGdlbmVyYXRlSWQoKSxcbiAgICAgICAgbGFzdE9wZW5lZDogbmV3IERhdGUoKS50b0lTT1N0cmluZygpLFxuICAgIH0pKS5pZFxufVxuIiwgImNvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9kaXJuYW1lID0gXCJDOlxcXFxQcm9qZWN0c1xcXFxlbnNvXFxcXGVuc29cXFxcYXBwXFxcXGlkZS1kZXNrdG9wXFxcXGxpYlxcXFxwcm9qZWN0LW1hbmFnZXItc2hpbVxcXFxzcmNcIjtjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfZmlsZW5hbWUgPSBcIkM6XFxcXFByb2plY3RzXFxcXGVuc29cXFxcZW5zb1xcXFxhcHBcXFxcaWRlLWRlc2t0b3BcXFxcbGliXFxcXHByb2plY3QtbWFuYWdlci1zaGltXFxcXHNyY1xcXFxwcm9qZWN0TWFuYWdlclNoaW1NaWRkbGV3YXJlLnRzXCI7Y29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2ltcG9ydF9tZXRhX3VybCA9IFwiZmlsZTovLy9DOi9Qcm9qZWN0cy9lbnNvL2Vuc28vYXBwL2lkZS1kZXNrdG9wL2xpYi9wcm9qZWN0LW1hbmFnZXItc2hpbS9zcmMvcHJvamVjdE1hbmFnZXJTaGltTWlkZGxld2FyZS50c1wiOy8qKiBAZmlsZSBBIHNpbXBsZSBIVFRQIHNlcnZlciB3aGljaCBzZXJ2ZXMgYXBwbGljYXRpb24gZGF0YSB0byB0aGUgRWxlY3Ryb24gd2ViLXZpZXcuICovXG5cbmltcG9ydCAqIGFzIGZzIGZyb20gJ25vZGU6ZnMvcHJvbWlzZXMnXG5pbXBvcnQgKiBhcyBmc1N5bmMgZnJvbSAnbm9kZTpmcydcbmltcG9ydCB0eXBlICogYXMgaHR0cCBmcm9tICdub2RlOmh0dHAnXG5pbXBvcnQgKiBhcyBvcyBmcm9tICdub2RlOm9zJ1xuaW1wb3J0ICogYXMgcGF0aCBmcm9tICdub2RlOnBhdGgnXG5cbmltcG9ydCAqIGFzIHRhciBmcm9tICd0YXInXG5pbXBvcnQgKiBhcyB5YW1sIGZyb20gJ3lhbWwnXG5cbmltcG9ydCAqIGFzIGNvbW1vbiBmcm9tICdlbnNvLWNvbW1vbidcbmltcG9ydCAqIGFzIHByb2plY3RNYW5hZ2VtZW50IGZyb20gJy4vcHJvamVjdE1hbmFnZW1lbnQnXG5cbi8vID09PT09PT09PT09PT09PT09XG4vLyA9PT0gQ29uc3RhbnRzID09PVxuLy8gPT09PT09PT09PT09PT09PT1cblxuY29uc3QgSFRUUF9TVEFUVVNfT0sgPSAyMDBcbmNvbnN0IEhUVFBfU1RBVFVTX0JBRF9SRVFVRVNUID0gNDAwXG5jb25zdCBIVFRQX1NUQVRVU19OT1RfRk9VTkQgPSA0MDRcbmNvbnN0IFBST0pFQ1RTX1JPT1RfRElSRUNUT1JZID0gcGF0aC5qb2luKG9zLmhvbWVkaXIoKSwgJ2Vuc28vcHJvamVjdHMnKVxuXG4vLyA9PT09PT09PT09PT09XG4vLyA9PT0gVHlwZXMgPT09XG4vLyA9PT09PT09PT09PT09XG5cbi8qKiBEZXRhaWxzIG9mIGEgcHJvamVjdC4gKi9cbmludGVyZmFjZSBQcm9qZWN0TWV0YWRhdGEge1xuICAgIC8qKiBUaGUgbmFtZSBvZiB0aGUgcHJvamVjdC4gKi9cbiAgICByZWFkb25seSBuYW1lOiBzdHJpbmdcbiAgICAvKiogVGhlIG5hbWVzcGFjZSBvZiB0aGUgcHJvamVjdC4gKi9cbiAgICByZWFkb25seSBuYW1lc3BhY2U6IHN0cmluZ1xuICAgIC8qKiBUaGUgcHJvamVjdCBpZC4gKi9cbiAgICByZWFkb25seSBpZDogc3RyaW5nXG4gICAgLyoqIFRoZSBFbnNvIEVuZ2luZSB2ZXJzaW9uIHRvIHVzZSBmb3IgdGhlIHByb2plY3QsIHJlcHJlc2VudGVkIGJ5IGEgc2VtdmVyIHZlcnNpb25cbiAgICAgKiBzdHJpbmcuXG4gICAgICpcbiAgICAgKiBJZiB0aGUgZWRpdGlvbiBhc3NvY2lhdGVkIHdpdGggdGhlIHByb2plY3QgY291bGQgbm90IGJlIHJlc29sdmVkLCB0aGVcbiAgICAgKiBlbmdpbmUgdmVyc2lvbiBtYXkgYmUgbWlzc2luZy4gKi9cbiAgICByZWFkb25seSBlbmdpbmVWZXJzaW9uPzogc3RyaW5nXG4gICAgLyoqIFRoZSBwcm9qZWN0IGNyZWF0aW9uIHRpbWUuICovXG4gICAgcmVhZG9ubHkgY3JlYXRlZDogc3RyaW5nXG4gICAgLyoqIFRoZSBsYXN0IG9wZW5lZCBkYXRldGltZS4gKi9cbiAgICByZWFkb25seSBsYXN0T3BlbmVkPzogc3RyaW5nXG59XG5cbi8qKiBBdHRyaWJ1dGVzIG9mIGEgZmlsZSBvciBmb2xkZXIuICovXG5pbnRlcmZhY2UgQXR0cmlidXRlcyB7XG4gICAgcmVhZG9ubHkgY3JlYXRpb25UaW1lOiBzdHJpbmdcbiAgICByZWFkb25seSBsYXN0QWNjZXNzVGltZTogc3RyaW5nXG4gICAgcmVhZG9ubHkgbGFzdE1vZGlmaWVkVGltZTogc3RyaW5nXG4gICAgcmVhZG9ubHkgYnl0ZVNpemU6IG51bWJlclxufVxuXG4vKiogTWV0YWRhdGEgZm9yIGFuIGFyYml0cmFyeSBmaWxlIHN5c3RlbSBlbnRyeS4gKi9cbnR5cGUgRmlsZVN5c3RlbUVudHJ5ID0gRGlyZWN0b3J5RW50cnkgfCBGaWxlRW50cnkgfCBQcm9qZWN0RW50cnlcblxuLyoqIFRoZSBkaXNjcmltaW5hdG9yIHZhbHVlIGZvciB7QGxpbmsgRmlsZVN5c3RlbUVudHJ5fS4gKi9cbmV4cG9ydCBlbnVtIEZpbGVTeXN0ZW1FbnRyeVR5cGUge1xuICAgIERpcmVjdG9yeUVudHJ5ID0gJ0RpcmVjdG9yeUVudHJ5JyxcbiAgICBQcm9qZWN0RW50cnkgPSAnUHJvamVjdEVudHJ5JyxcbiAgICBGaWxlRW50cnkgPSAnRmlsZUVudHJ5Jyxcbn1cblxuLyoqIE1ldGFkYXRhIGZvciBhIGZpbGUuICovXG5pbnRlcmZhY2UgRmlsZUVudHJ5IHtcbiAgICByZWFkb25seSB0eXBlOiBGaWxlU3lzdGVtRW50cnlUeXBlLkZpbGVFbnRyeVxuICAgIHJlYWRvbmx5IHBhdGg6IHN0cmluZ1xuICAgIHJlYWRvbmx5IGF0dHJpYnV0ZXM6IEF0dHJpYnV0ZXNcbn1cblxuLyoqIE1ldGFkYXRhIGZvciBhIGRpcmVjdG9yeS4gKi9cbmludGVyZmFjZSBEaXJlY3RvcnlFbnRyeSB7XG4gICAgcmVhZG9ubHkgdHlwZTogRmlsZVN5c3RlbUVudHJ5VHlwZS5EaXJlY3RvcnlFbnRyeVxuICAgIHJlYWRvbmx5IHBhdGg6IHN0cmluZ1xuICAgIHJlYWRvbmx5IGF0dHJpYnV0ZXM6IEF0dHJpYnV0ZXNcbn1cblxuLyoqIE1ldGFkYXRhIGZvciBhIHByb2plY3QuICovXG5pbnRlcmZhY2UgUHJvamVjdEVudHJ5IHtcbiAgICByZWFkb25seSB0eXBlOiBGaWxlU3lzdGVtRW50cnlUeXBlLlByb2plY3RFbnRyeVxuICAgIHJlYWRvbmx5IHBhdGg6IHN0cmluZ1xuICAgIHJlYWRvbmx5IG1ldGFkYXRhOiBQcm9qZWN0TWV0YWRhdGFcbiAgICByZWFkb25seSBhdHRyaWJ1dGVzOiBBdHRyaWJ1dGVzXG59XG5cbi8vID09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PVxuLy8gPT09IHByb2plY3RNYW5hZ2VyU2hpbU1pZGRsZXdhcmUgPT09XG4vLyA9PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT1cblxuLyoqIEEgbWlkZGxld2FyZSB0aGF0IGhhbmRsZXMgICovXG5leHBvcnQgZGVmYXVsdCBmdW5jdGlvbiBwcm9qZWN0TWFuYWdlclNoaW1NaWRkbGV3YXJlKFxuICAgIHJlcXVlc3Q6IGh0dHAuSW5jb21pbmdNZXNzYWdlLFxuICAgIHJlc3BvbnNlOiBodHRwLlNlcnZlclJlc3BvbnNlLFxuICAgIG5leHQ6ICgpID0+IHZvaWRcbikge1xuICAgIGNvbnN0IHJlcXVlc3RVcmwgPSByZXF1ZXN0LnVybFxuICAgIGNvbnN0IHJlcXVlc3RQYXRoID0gcmVxdWVzdFVybD8uc3BsaXQoJz8nKVswXT8uc3BsaXQoJyMnKVswXVxuICAgIGlmIChyZXF1ZXN0Lm1ldGhvZCA9PT0gJ1BPU1QnKSB7XG4gICAgICAgIHN3aXRjaCAocmVxdWVzdFBhdGgpIHtcbiAgICAgICAgICAgIGNhc2UgJy9hcGkvdXBsb2FkLWZpbGUnOiB7XG4gICAgICAgICAgICAgICAgY29uc3QgdXJsID0gbmV3IFVSTChgaHR0cHM6Ly9leGFtcGxlLmNvbS8ke3JlcXVlc3RVcmx9YClcbiAgICAgICAgICAgICAgICBjb25zdCBmaWxlTmFtZSA9IHVybC5zZWFyY2hQYXJhbXMuZ2V0KCdmaWxlX25hbWUnKVxuICAgICAgICAgICAgICAgIGNvbnN0IGRpcmVjdG9yeSA9IHVybC5zZWFyY2hQYXJhbXMuZ2V0KCdkaXJlY3RvcnknKSA/PyBQUk9KRUNUU19ST09UX0RJUkVDVE9SWVxuICAgICAgICAgICAgICAgIGlmIChmaWxlTmFtZSA9PSBudWxsKSB7XG4gICAgICAgICAgICAgICAgICAgIHJlc3BvbnNlXG4gICAgICAgICAgICAgICAgICAgICAgICAud3JpdGVIZWFkKEhUVFBfU1RBVFVTX0JBRF9SRVFVRVNULCBjb21tb24uQ09PUF9DT0VQX0NPUlBfSEVBREVSUylcbiAgICAgICAgICAgICAgICAgICAgICAgIC5lbmQoJ1JlcXVlc3QgaXMgbWlzc2luZyBzZWFyY2ggcGFyYW1ldGVyIGBmaWxlX25hbWVgLicpXG4gICAgICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICAgICAgY29uc3QgZmlsZVBhdGggPSBwYXRoLmpvaW4oZGlyZWN0b3J5LCBmaWxlTmFtZSlcbiAgICAgICAgICAgICAgICAgICAgdm9pZCBmc1xuICAgICAgICAgICAgICAgICAgICAgICAgLndyaXRlRmlsZShmaWxlUGF0aCwgcmVxdWVzdClcbiAgICAgICAgICAgICAgICAgICAgICAgIC50aGVuKCgpID0+IHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICByZXNwb25zZVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAud3JpdGVIZWFkKEhUVFBfU1RBVFVTX09LLCBbXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbJ0NvbnRlbnQtTGVuZ3RoJywgU3RyaW5nKGZpbGVQYXRoLmxlbmd0aCldLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgWydDb250ZW50LVR5cGUnLCAndGV4dC9wbGFpbiddLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLi4uY29tbW9uLkNPT1BfQ09FUF9DT1JQX0hFQURFUlMsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC5lbmQoZmlsZVBhdGgpXG4gICAgICAgICAgICAgICAgICAgICAgICB9KVxuICAgICAgICAgICAgICAgICAgICAgICAgLmNhdGNoKGUgPT4ge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNvbnNvbGUuZXJyb3IoZSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICByZXNwb25zZVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAud3JpdGVIZWFkKEhUVFBfU1RBVFVTX0JBRF9SRVFVRVNULCBjb21tb24uQ09PUF9DT0VQX0NPUlBfSEVBREVSUylcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLmVuZCgpXG4gICAgICAgICAgICAgICAgICAgICAgICB9KVxuICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICBicmVha1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgLy8gVGhpcyBlbmRwb2ludCBzaG91bGQgb25seSBiZSB1c2VkIHdoZW4gYWNjZXNzaW5nIHRoZSBhcHAgZnJvbSB0aGUgYnJvd3Nlci5cbiAgICAgICAgICAgIC8vIFdoZW4gYWNjZXNzaW5nIHRoZSBhcHAgZnJvbSBFbGVjdHJvbiwgdGhlIGZpbGUgaW5wdXQgZXZlbnQgd2lsbCBoYXZlIHRoZVxuICAgICAgICAgICAgLy8gZnVsbCBzeXN0ZW0gcGF0aC5cbiAgICAgICAgICAgIGNhc2UgJy9hcGkvdXBsb2FkLXByb2plY3QnOiB7XG4gICAgICAgICAgICAgICAgY29uc3QgdXJsID0gbmV3IFVSTChgaHR0cHM6Ly9leGFtcGxlLmNvbS8ke3JlcXVlc3RVcmx9YClcbiAgICAgICAgICAgICAgICBjb25zdCBkaXJlY3RvcnkgPSB1cmwuc2VhcmNoUGFyYW1zLmdldCgnZGlyZWN0b3J5JylcbiAgICAgICAgICAgICAgICBjb25zdCBuYW1lID0gdXJsLnNlYXJjaFBhcmFtcy5nZXQoJ25hbWUnKVxuICAgICAgICAgICAgICAgIHZvaWQgcHJvamVjdE1hbmFnZW1lbnRcbiAgICAgICAgICAgICAgICAgICAgLnVwbG9hZEJ1bmRsZShyZXF1ZXN0LCBkaXJlY3RvcnksIG5hbWUpXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGlkID0+IHtcbiAgICAgICAgICAgICAgICAgICAgICAgIHJlc3BvbnNlXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgLndyaXRlSGVhZChIVFRQX1NUQVRVU19PSywgW1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbJ0NvbnRlbnQtTGVuZ3RoJywgU3RyaW5nKGlkLmxlbmd0aCldLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbJ0NvbnRlbnQtVHlwZScsICd0ZXh0L3BsYWluJ10sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC4uLmNvbW1vbi5DT09QX0NPRVBfQ09SUF9IRUFERVJTLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgLmVuZChpZClcbiAgICAgICAgICAgICAgICAgICAgfSlcbiAgICAgICAgICAgICAgICAgICAgLmNhdGNoKCgpID0+IHtcbiAgICAgICAgICAgICAgICAgICAgICAgIHJlc3BvbnNlXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgLndyaXRlSGVhZChIVFRQX1NUQVRVU19CQURfUkVRVUVTVCwgY29tbW9uLkNPT1BfQ09FUF9DT1JQX0hFQURFUlMpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgLmVuZCgpXG4gICAgICAgICAgICAgICAgICAgIH0pXG4gICAgICAgICAgICAgICAgYnJlYWtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIGNhc2UgJy9hcGkvcnVuLXByb2plY3QtbWFuYWdlci1jb21tYW5kJzoge1xuICAgICAgICAgICAgICAgIGNvbnN0IGNsaUFyZ3VtZW50czogdW5rbm93biA9IEpTT04ucGFyc2UoXG4gICAgICAgICAgICAgICAgICAgIG5ldyBVUkwoYGh0dHBzOi8vZXhhbXBsZS5jb20vJHtyZXF1ZXN0VXJsfWApLnNlYXJjaFBhcmFtcy5nZXQoXG4gICAgICAgICAgICAgICAgICAgICAgICAnY2xpLWFyZ3VtZW50cydcbiAgICAgICAgICAgICAgICAgICAgKSA/PyAnW10nXG4gICAgICAgICAgICAgICAgKVxuICAgICAgICAgICAgICAgIGlmIChcbiAgICAgICAgICAgICAgICAgICAgIUFycmF5LmlzQXJyYXkoY2xpQXJndW1lbnRzKSB8fFxuICAgICAgICAgICAgICAgICAgICAhY2xpQXJndW1lbnRzLmV2ZXJ5KChpdGVtKTogaXRlbSBpcyBzdHJpbmcgPT4gdHlwZW9mIGl0ZW0gPT09ICdzdHJpbmcnKVxuICAgICAgICAgICAgICAgICkge1xuICAgICAgICAgICAgICAgICAgICByZXNwb25zZVxuICAgICAgICAgICAgICAgICAgICAgICAgLndyaXRlSGVhZChIVFRQX1NUQVRVU19CQURfUkVRVUVTVCwgY29tbW9uLkNPT1BfQ09FUF9DT1JQX0hFQURFUlMpXG4gICAgICAgICAgICAgICAgICAgICAgICAuZW5kKCdDb21tYW5kIGFyZ3VtZW50cyBtdXN0IGJlIGFuIGFycmF5IG9mIHN0cmluZ3MuJylcbiAgICAgICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgICAgICB2b2lkIChhc3luYyAoKSA9PiB7XG4gICAgICAgICAgICAgICAgICAgICAgICBjb25zdCB0b0pTT05SUENSZXN1bHQgPSAocmVzdWx0OiB1bmtub3duKSA9PlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIEpTT04uc3RyaW5naWZ5KHsganNvbnJwYzogJzIuMCcsIGlkOiAwLCByZXN1bHQgfSlcbiAgICAgICAgICAgICAgICAgICAgICAgIGNvbnN0IHRvSlNPTlJQQ0Vycm9yID0gKG1lc3NhZ2U6IHN0cmluZywgZGF0YT86IHVua25vd24pID0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgSlNPTi5zdHJpbmdpZnkoe1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBqc29ucnBjOiAnMi4wJyxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgaWQ6IDAsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGVycm9yOiB7IGNvZGU6IDAsIG1lc3NhZ2UsIC4uLihkYXRhICE9IG51bGwgPyB7IGRhdGEgfSA6IHt9KSB9LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIH0pXG4gICAgICAgICAgICAgICAgICAgICAgICBsZXQgcmVzdWx0ID0gdG9KU09OUlBDRXJyb3IoYEVycm9yIHJ1bm5pbmcgUHJvamVjdCBNYW5hZ2VyIGNvbW1hbmQuYCwge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNvbW1hbmQ6IGNsaUFyZ3VtZW50cyxcbiAgICAgICAgICAgICAgICAgICAgICAgIH0pXG4gICAgICAgICAgICAgICAgICAgICAgICB0cnkge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHN3aXRjaCAoY2xpQXJndW1lbnRzWzBdKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNhc2UgJy0tZmlsZXN5c3RlbS1saXN0Jzoge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29uc3QgZGlyZWN0b3J5UGF0aCA9IGNsaUFyZ3VtZW50c1sxXVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgaWYgKGRpcmVjdG9yeVBhdGggIT0gbnVsbCkge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNvbnN0IGVudHJ5TmFtZXMgPSBhd2FpdCBmcy5yZWFkZGlyKGRpcmVjdG9yeVBhdGgpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29uc3QgZW50cmllczogRmlsZVN5c3RlbUVudHJ5W10gPSBbXVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGZvciAoY29uc3QgZW50cnlOYW1lIG9mIGVudHJ5TmFtZXMpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29uc3QgZW50cnlQYXRoID0gcGF0aC5qb2luKGRpcmVjdG9yeVBhdGgsIGVudHJ5TmFtZSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29uc3Qgc3RhdCA9IGF3YWl0IGZzLnN0YXQoZW50cnlQYXRoKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBjb25zdCBhdHRyaWJ1dGVzOiBBdHRyaWJ1dGVzID0ge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYnl0ZVNpemU6IHN0YXQuc2l6ZSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNyZWF0aW9uVGltZTogbmV3IERhdGUoc3RhdC5jdGltZU1zKS50b0lTT1N0cmluZygpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbGFzdEFjY2Vzc1RpbWU6IG5ldyBEYXRlKFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHN0YXQuYXRpbWVNc1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKS50b0lTT1N0cmluZygpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbGFzdE1vZGlmaWVkVGltZTogbmV3IERhdGUoXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc3RhdC5tdGltZU1zXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICApLnRvSVNPU3RyaW5nKCksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgaWYgKHN0YXQuaXNGaWxlKCkpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGVudHJpZXMucHVzaCh7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdHlwZTogRmlsZVN5c3RlbUVudHJ5VHlwZS5GaWxlRW50cnksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgcGF0aDogZW50cnlQYXRoLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGF0dHJpYnV0ZXMsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9IHNhdGlzZmllcyBGaWxlRW50cnkpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB0cnkge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNvbnN0IHBhY2thZ2VNZXRhZGF0YVBhdGggPSBwYXRoLmpvaW4oXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGVudHJ5UGF0aCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgJ3BhY2thZ2UueWFtbCdcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICApXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29uc3QgcHJvamVjdE1ldGFkYXRhUGF0aCA9IHBhdGguam9pbihcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgZW50cnlQYXRoLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBwcm9qZWN0TWFuYWdlbWVudC5QUk9KRUNUX01FVEFEQVRBX1JFTEFUSVZFX1BBVEhcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICApXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29uc3QgcGFja2FnZU1ldGFkYXRhQ29udGVudHMgPVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBhd2FpdCBmcy5yZWFkRmlsZShwYWNrYWdlTWV0YWRhdGFQYXRoKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNvbnN0IHByb2plY3RNZXRhZGF0YUNvbnRlbnRzID1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYXdhaXQgZnMucmVhZEZpbGUocHJvamVjdE1ldGFkYXRhUGF0aClcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBjb25zdCBtZXRhZGF0YSA9IGV4dHJhY3RQcm9qZWN0TWV0YWRhdGEoXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHlhbWwucGFyc2UoXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBwYWNrYWdlTWV0YWRhdGFDb250ZW50cy50b1N0cmluZygpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIEpTT04ucGFyc2UoXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBwcm9qZWN0TWV0YWRhdGFDb250ZW50cy50b1N0cmluZygpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIClcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICApXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgaWYgKG1ldGFkYXRhICE9IG51bGwpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLy8gVGhpcyBpcyBhIHByb2plY3QuXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGVudHJpZXMucHVzaCh7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB0eXBlOiBGaWxlU3lzdGVtRW50cnlUeXBlLlByb2plY3RFbnRyeSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHBhdGg6IGVudHJ5UGF0aCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGF0dHJpYnV0ZXMsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtZXRhZGF0YSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfSBzYXRpc2ZpZXMgUHJvamVjdEVudHJ5KVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC8vIFRoaXMgZXJyb3IgbW92ZXMgY29udHJvbCBmbG93IHRvIHRoZVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAvLyBgY2F0Y2hgIGNsYXVzZSBkaXJlY3RseSBiZWxvdy5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLy8gZXNsaW50LWRpc2FibGUtbmV4dC1saW5lIG5vLXJlc3RyaWN0ZWQtc3ludGF4XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHRocm93IG5ldyBFcnJvcignSW52YWxpZCBwcm9qZWN0IG1ldGFkYXRhLicpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfSBjYXRjaCB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLy8gVGhpcyBpcyBhIHJlZ3VsYXIgZGlyZWN0b3J5LCBub3QgYSBwcm9qZWN0LlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGVudHJpZXMucHVzaCh7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHR5cGU6IEZpbGVTeXN0ZW1FbnRyeVR5cGUuRGlyZWN0b3J5RW50cnksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHBhdGg6IGVudHJ5UGF0aCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYXR0cmlidXRlcyxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9IHNhdGlzZmllcyBEaXJlY3RvcnlFbnRyeSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICByZXN1bHQgPSB0b0pTT05SUENSZXN1bHQoeyBlbnRyaWVzIH0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBicmVha1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNhc2UgJy0tZmlsZXN5c3RlbS1jcmVhdGUtZGlyZWN0b3J5Jzoge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29uc3QgZGlyZWN0b3J5UGF0aCA9IGNsaUFyZ3VtZW50c1sxXVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgaWYgKGRpcmVjdG9yeVBhdGggIT0gbnVsbCkge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGF3YWl0IGZzLm1rZGlyKGRpcmVjdG9yeVBhdGgsIHsgcmVjdXJzaXZlOiB0cnVlIH0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgcmVzdWx0ID0gdG9KU09OUlBDUmVzdWx0KG51bGwpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBicmVha1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNhc2UgJy0tZmlsZXN5c3RlbS13cml0ZS1wYXRoJzoge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29uc3QgZmlsZVBhdGggPSBjbGlBcmd1bWVudHNbMV1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGlmIChmaWxlUGF0aCAhPSBudWxsKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYXdhaXQgbmV3IFByb21pc2UoKHJlc29sdmUsIHJlamVjdCkgPT4ge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICByZXF1ZXN0XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAucGlwZShmc1N5bmMuY3JlYXRlV3JpdGVTdHJlYW0oZmlsZVBhdGgpLCB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgZW5kOiB0cnVlLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC5vbignY2xvc2UnLCByZXNvbHZlKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLm9uKCdlcnJvcicsIHJlamVjdClcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9KVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHJlc3VsdCA9IHRvSlNPTlJQQ1Jlc3VsdChudWxsKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYnJlYWtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBjYXNlICctLWZpbGVzeXN0ZW0tbW92ZS1mcm9tJzoge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29uc3Qgc291cmNlUGF0aCA9IGNsaUFyZ3VtZW50c1sxXVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29uc3QgZGVzdGluYXRpb25QYXRoID0gY2xpQXJndW1lbnRzWzNdXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBpZiAoXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc291cmNlUGF0aCAhPSBudWxsICYmXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY2xpQXJndW1lbnRzWzJdID09PSAnLS1maWxlc3lzdGVtLW1vdmUtdG8nICYmXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgZGVzdGluYXRpb25QYXRoICE9IG51bGxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICkge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGF3YWl0IGZzLnJlbmFtZShzb3VyY2VQYXRoLCBkZXN0aW5hdGlvblBhdGgpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgcmVzdWx0ID0gdG9KU09OUlBDUmVzdWx0KG51bGwpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBicmVha1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNhc2UgJy0tZmlsZXN5c3RlbS1kZWxldGUnOiB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBjb25zdCBmaWxlT3JEaXJlY3RvcnlQYXRoID0gY2xpQXJndW1lbnRzWzFdXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBpZiAoZmlsZU9yRGlyZWN0b3J5UGF0aCAhPSBudWxsKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYXdhaXQgZnMucm0oZmlsZU9yRGlyZWN0b3J5UGF0aCwgeyByZWN1cnNpdmU6IHRydWUgfSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICByZXN1bHQgPSB0b0pTT05SUENSZXN1bHQobnVsbClcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGJyZWFrXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgZGVmYXVsdDoge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLy8gSWdub3JlZC4gYHJlc3VsdGAgcmV0YWlucyBpdHMgb3JpZ2luYWwgdmFsdWUgaW5kaWNhdGluZyBhbiBlcnJvci5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgICAgIH0gY2F0Y2gge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIC8vIElnbm9yZWQuIGByZXN1bHRgIHJldGFpbnMgaXRzIG9yaWdpbmFsIHZhbHVlIGluZGljYXRpbmcgYW4gZXJyb3IuXG4gICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICBjb25zdCBidWZmZXIgPSBCdWZmZXIuZnJvbShyZXN1bHQpXG4gICAgICAgICAgICAgICAgICAgICAgICByZXNwb25zZVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIC53cml0ZUhlYWQoSFRUUF9TVEFUVVNfT0ssIFtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgWydDb250ZW50LUxlbmd0aCcsIFN0cmluZyhidWZmZXIuYnl0ZUxlbmd0aCldLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbJ0NvbnRlbnQtVHlwZScsICdhcHBsaWNhdGlvbi9qc29uJ10sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC4uLmNvbW1vbi5DT09QX0NPRVBfQ09SUF9IRUFERVJTLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgLmVuZChidWZmZXIpXG4gICAgICAgICAgICAgICAgICAgIH0pKClcbiAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgYnJlYWtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIGRlZmF1bHQ6IHtcbiAgICAgICAgICAgICAgICBjb25zdCBkb3dubG9hZFByb2plY3RNYXRjaCA9IHJlcXVlc3RQYXRoPy5tYXRjaChcbiAgICAgICAgICAgICAgICAgICAgL15bL11hcGlbL11wcm9qZWN0LW1hbmFnZXJbL11wcm9qZWN0c1svXShbXi9dKylbL11lbnNvLXByb2plY3QkL1xuICAgICAgICAgICAgICAgIClcbiAgICAgICAgICAgICAgICBpZiAoZG93bmxvYWRQcm9qZWN0TWF0Y2gpIHtcbiAgICAgICAgICAgICAgICAgICAgY29uc3QgdXVpZCA9IGRvd25sb2FkUHJvamVjdE1hdGNoWzFdXG4gICAgICAgICAgICAgICAgICAgIHZvaWQgZnMucmVhZGRpcihQUk9KRUNUU19ST09UX0RJUkVDVE9SWSkudGhlbihhc3luYyBmaWxlbmFtZXMgPT4ge1xuICAgICAgICAgICAgICAgICAgICAgICAgbGV0IHN1Y2Nlc3MgPSBmYWxzZVxuICAgICAgICAgICAgICAgICAgICAgICAgZm9yIChjb25zdCBmaWxlbmFtZSBvZiBmaWxlbmFtZXMpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB0cnkge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBjb25zdCBwcm9qZWN0Um9vdCA9IHBhdGguam9pbihQUk9KRUNUU19ST09UX0RJUkVDVE9SWSwgZmlsZW5hbWUpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNvbnN0IHN0YXQgPSBhd2FpdCBmcy5zdGF0KHByb2plY3RSb290KVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBpZiAoc3RhdC5pc0RpcmVjdG9yeSgpKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBjb25zdCBtZXRhZGF0YVBhdGggPSBwYXRoLmpvaW4oXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgcHJvamVjdFJvb3QsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgcHJvamVjdE1hbmFnZW1lbnQuUFJPSkVDVF9NRVRBREFUQV9SRUxBVElWRV9QQVRIXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICApXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBjb25zdCBtZXRhZGF0YUNvbnRlbnRzID0gYXdhaXQgZnMucmVhZEZpbGUobWV0YWRhdGFQYXRoKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29uc3QgbWV0YWRhdGE6IHVua25vd24gPSBKU09OLnBhcnNlKFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG1ldGFkYXRhQ29udGVudHMudG9TdHJpbmcoKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgaWYgKFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHR5cGVvZiBtZXRhZGF0YSA9PT0gJ29iamVjdCcgJiZcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtZXRhZGF0YSAhPSBudWxsICYmXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgJ2lkJyBpbiBtZXRhZGF0YSAmJlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG1ldGFkYXRhLmlkID09PSB1dWlkXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICApIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICByZXNwb25zZS53cml0ZUhlYWQoSFRUUF9TVEFUVVNfT0ssIFtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgWydDb250ZW50LVR5cGUnLCAnYXBwbGljYXRpb24vZ3ppcCt4LWVuc28tcHJvamVjdCddLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAuLi5jb21tb24uQ09PUF9DT0VQX0NPUlBfSEVBREVSUyxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHRhci5jcmVhdGUoeyBnemlwOiB0cnVlLCBjd2Q6IHByb2plY3RSb290IH0sIFtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgcHJvamVjdFJvb3QsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSkucGlwZShyZXNwb25zZSwgeyBlbmQ6IHRydWUgfSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzdWNjZXNzID0gdHJ1ZVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGJyZWFrXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB9IGNhdGNoIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLy8gSWdub3JlZC5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICBpZiAoIXN1Y2Nlc3MpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICByZXNwb25zZVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAud3JpdGVIZWFkKEhUVFBfU1RBVFVTX05PVF9GT1VORCwgY29tbW9uLkNPT1BfQ09FUF9DT1JQX0hFQURFUlMpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC5lbmQoKVxuICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICB9KVxuICAgICAgICAgICAgICAgICAgICBicmVha1xuICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICByZXNwb25zZS53cml0ZUhlYWQoSFRUUF9TVEFUVVNfTk9UX0ZPVU5ELCBjb21tb24uQ09PUF9DT0VQX0NPUlBfSEVBREVSUykuZW5kKClcbiAgICAgICAgICAgICAgICBicmVha1xuICAgICAgICAgICAgfVxuICAgICAgICB9XG4gICAgfSBlbHNlIGlmIChyZXF1ZXN0Lm1ldGhvZCA9PT0gJ0dFVCcgJiYgcmVxdWVzdFBhdGggPT09ICcvYXBpL3Jvb3QtZGlyZWN0b3J5Jykge1xuICAgICAgICByZXNwb25zZVxuICAgICAgICAgICAgLndyaXRlSGVhZChIVFRQX1NUQVRVU19PSywgW1xuICAgICAgICAgICAgICAgIFsnQ29udGVudC1MZW5ndGgnLCBTdHJpbmcoUFJPSkVDVFNfUk9PVF9ESVJFQ1RPUlkubGVuZ3RoKV0sXG4gICAgICAgICAgICAgICAgWydDb250ZW50LVR5cGUnLCAndGV4dC9wbGFpbiddLFxuICAgICAgICAgICAgICAgIC4uLmNvbW1vbi5DT09QX0NPRVBfQ09SUF9IRUFERVJTLFxuICAgICAgICAgICAgXSlcbiAgICAgICAgICAgIC5lbmQoUFJPSkVDVFNfUk9PVF9ESVJFQ1RPUlkpXG4gICAgfSBlbHNlIHtcbiAgICAgICAgbmV4dCgpXG4gICAgfVxufVxuXG4vKiogUmV0dXJuIGEge0BsaW5rIFByb2plY3RNZXRhZGF0YX0gaWYgdGhlIG1ldGFkYXRhIGlzIGEgdmFsaWQgbWV0YWRhdGEgb2JqZWN0LFxuICogZWxzZSByZXR1cm4gYG51bGxgLiAqL1xuZnVuY3Rpb24gZXh0cmFjdFByb2plY3RNZXRhZGF0YSh5YW1sT2JqOiB1bmtub3duLCBqc29uT2JqOiB1bmtub3duKTogUHJvamVjdE1ldGFkYXRhIHwgbnVsbCB7XG4gICAgaWYgKFxuICAgICAgICB0eXBlb2YgeWFtbE9iaiAhPT0gJ29iamVjdCcgfHxcbiAgICAgICAgeWFtbE9iaiA9PSBudWxsIHx8XG4gICAgICAgIHR5cGVvZiBqc29uT2JqICE9PSAnb2JqZWN0JyB8fFxuICAgICAgICBqc29uT2JqID09IG51bGxcbiAgICApIHtcbiAgICAgICAgcmV0dXJuIG51bGxcbiAgICB9IGVsc2Uge1xuICAgICAgICBjb25zdCB2YWxpZERhdGVTdHJpbmcgPSAoc3RyaW5nOiBzdHJpbmcpID0+IHtcbiAgICAgICAgICAgIGNvbnN0IGRhdGUgPSBuZXcgRGF0ZShzdHJpbmcpXG4gICAgICAgICAgICByZXR1cm4gIU51bWJlci5pc05hTihOdW1iZXIoZGF0ZSkpID8gZGF0ZS50b1N0cmluZygpIDogbnVsbFxuICAgICAgICB9XG4gICAgICAgIGNvbnN0IG5hbWUgPSAnbmFtZScgaW4geWFtbE9iaiAmJiB0eXBlb2YgeWFtbE9iai5uYW1lID09PSAnc3RyaW5nJyA/IHlhbWxPYmoubmFtZSA6IG51bGxcbiAgICAgICAgY29uc3QgbmFtZXNwYWNlID1cbiAgICAgICAgICAgICduYW1lc3BhY2UnIGluIHlhbWxPYmogJiYgdHlwZW9mIHlhbWxPYmoubmFtZXNwYWNlID09PSAnc3RyaW5nJ1xuICAgICAgICAgICAgICAgID8geWFtbE9iai5uYW1lc3BhY2VcbiAgICAgICAgICAgICAgICA6IG51bGxcbiAgICAgICAgY29uc3QgZW5naW5lVmVyc2lvbiA9XG4gICAgICAgICAgICAnZWRpdGlvbicgaW4geWFtbE9iaiAmJiB0eXBlb2YgeWFtbE9iai5lZGl0aW9uID09PSAnc3RyaW5nJyA/IHlhbWxPYmouZWRpdGlvbiA6IG51bGxcbiAgICAgICAgY29uc3QgaWQgPSAnaWQnIGluIGpzb25PYmogJiYgdHlwZW9mIGpzb25PYmouaWQgPT09ICdzdHJpbmcnID8ganNvbk9iai5pZCA6IG51bGxcbiAgICAgICAgY29uc3QgY3JlYXRlZCA9XG4gICAgICAgICAgICAnY3JlYXRlZCcgaW4ganNvbk9iaiAmJiB0eXBlb2YganNvbk9iai5jcmVhdGVkID09PSAnc3RyaW5nJ1xuICAgICAgICAgICAgICAgID8gdmFsaWREYXRlU3RyaW5nKGpzb25PYmouY3JlYXRlZClcbiAgICAgICAgICAgICAgICA6IG51bGxcbiAgICAgICAgY29uc3QgbGFzdE9wZW5lZCA9XG4gICAgICAgICAgICAnbGFzdE9wZW5lZCcgaW4ganNvbk9iaiAmJiB0eXBlb2YganNvbk9iai5sYXN0T3BlbmVkID09PSAnc3RyaW5nJ1xuICAgICAgICAgICAgICAgID8gdmFsaWREYXRlU3RyaW5nKGpzb25PYmoubGFzdE9wZW5lZClcbiAgICAgICAgICAgICAgICA6IG51bGxcbiAgICAgICAgaWYgKG5hbWUgIT0gbnVsbCAmJiBuYW1lc3BhY2UgIT0gbnVsbCAmJiBpZCAhPSBudWxsICYmIGNyZWF0ZWQgIT0gbnVsbCkge1xuICAgICAgICAgICAgcmV0dXJuIHtcbiAgICAgICAgICAgICAgICBuYW1lLFxuICAgICAgICAgICAgICAgIG5hbWVzcGFjZSxcbiAgICAgICAgICAgICAgICBpZCxcbiAgICAgICAgICAgICAgICAuLi4oZW5naW5lVmVyc2lvbiAhPSBudWxsID8geyBlbmdpbmVWZXJzaW9uIH0gOiB7fSksXG4gICAgICAgICAgICAgICAgY3JlYXRlZCxcbiAgICAgICAgICAgICAgICAuLi4obGFzdE9wZW5lZCAhPSBudWxsID8geyBsYXN0T3BlbmVkIH0gOiB7fSksXG4gICAgICAgICAgICB9IHNhdGlzZmllcyBQcm9qZWN0TWV0YWRhdGFcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIHJldHVybiBudWxsXG4gICAgICAgIH1cbiAgICB9XG59XG4iLCAiY29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2Rpcm5hbWUgPSBcIkM6XFxcXFByb2plY3RzXFxcXGVuc29cXFxcZW5zb1xcXFxhcHBcXFxcZ3VpMlwiO2NvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9maWxlbmFtZSA9IFwiQzpcXFxcUHJvamVjdHNcXFxcZW5zb1xcXFxlbnNvXFxcXGFwcFxcXFxndWkyXFxcXHZpdGVzdC5jb25maWcudHNcIjtjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfaW1wb3J0X21ldGFfdXJsID0gXCJmaWxlOi8vL0M6L1Byb2plY3RzL2Vuc28vZW5zby9hcHAvZ3VpMi92aXRlc3QuY29uZmlnLnRzXCI7aW1wb3J0IHsgZmlsZVVSTFRvUGF0aCB9IGZyb20gJ25vZGU6dXJsJ1xuaW1wb3J0IHsgY29uZmlnRGVmYXVsdHMsIGRlZmluZUNvbmZpZywgbWVyZ2VDb25maWcgfSBmcm9tICd2aXRlc3QvY29uZmlnJ1xuaW1wb3J0IHZpdGVDb25maWcgZnJvbSAnLi92aXRlLmNvbmZpZydcblxuZXhwb3J0IGRlZmF1bHQgbWVyZ2VDb25maWcoXG4gIHZpdGVDb25maWcsXG4gIGRlZmluZUNvbmZpZyh7XG4gICAgdGVzdDoge1xuICAgICAgZW52aXJvbm1lbnQ6ICdqc2RvbScsXG4gICAgICBpbmNsdWRlU291cmNlOiBbJy4ve3NyYyxzaGFyZWQseWRvYy1zZXJ2ZXJ9LyoqLyoue3RzLHZ1ZX0nXSxcbiAgICAgIGV4Y2x1ZGU6IFsuLi5jb25maWdEZWZhdWx0cy5leGNsdWRlLCAnZTJlLyonXSxcbiAgICAgIHJvb3Q6IGZpbGVVUkxUb1BhdGgobmV3IFVSTCgnLi8nLCBpbXBvcnQubWV0YS51cmwpKSxcbiAgICAgIHJlc3RvcmVNb2NrczogdHJ1ZSxcbiAgICB9LFxuICAgIGRlZmluZToge1xuICAgICAgUlVOTklOR19WSVRFU1Q6IHRydWUsXG4gICAgfSxcbiAgfSksXG4pXG4iLCAiY29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2Rpcm5hbWUgPSBcIkM6XFxcXFByb2plY3RzXFxcXGVuc29cXFxcZW5zb1xcXFxhcHBcXFxcZ3VpMlwiO2NvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9maWxlbmFtZSA9IFwiQzpcXFxcUHJvamVjdHNcXFxcZW5zb1xcXFxlbnNvXFxcXGFwcFxcXFxndWkyXFxcXHZpdGUuY29uZmlnLnRzXCI7Y29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2ltcG9ydF9tZXRhX3VybCA9IFwiZmlsZTovLy9DOi9Qcm9qZWN0cy9lbnNvL2Vuc28vYXBwL2d1aTIvdml0ZS5jb25maWcudHNcIjsvLy8gPHJlZmVyZW5jZSB0eXBlcz1cImhpc3RvaXJlXCIgLz5cblxuaW1wb3J0IHZ1ZSBmcm9tICdAdml0ZWpzL3BsdWdpbi12dWUnXG5pbXBvcnQgeyBnZXREZWZpbmVzLCByZWFkRW52aXJvbm1lbnRGcm9tRmlsZSB9IGZyb20gJ2Vuc28tY29tbW9uL3NyYy9hcHBDb25maWcnXG5pbXBvcnQgeyBmaWxlVVJMVG9QYXRoIH0gZnJvbSAnbm9kZTp1cmwnXG5pbXBvcnQgcG9zdGNzc05lc3RpbmcgZnJvbSAncG9zdGNzcy1uZXN0aW5nJ1xuaW1wb3J0IHRhaWx3aW5kY3NzIGZyb20gJ3RhaWx3aW5kY3NzJ1xuaW1wb3J0IHRhaWx3aW5kY3NzTmVzdGluZyBmcm9tICd0YWlsd2luZGNzcy9uZXN0aW5nJ1xuaW1wb3J0IHsgZGVmaW5lQ29uZmlnLCB0eXBlIFBsdWdpbiB9IGZyb20gJ3ZpdGUnXG4vLyBAdHMtZXhwZWN0LWVycm9yXG5pbXBvcnQgKiBhcyB0YWlsd2luZENvbmZpZyBmcm9tICdlbnNvLWRhc2hib2FyZC90YWlsd2luZC5jb25maWcnXG5pbXBvcnQgeyBjcmVhdGVHYXRld2F5U2VydmVyIH0gZnJvbSAnLi95ZG9jLXNlcnZlcidcbmNvbnN0IGxvY2FsU2VydmVyUG9ydCA9IDgwODBcbmNvbnN0IHByb2plY3RNYW5hZ2VyVXJsID0gJ3dzOi8vMTI3LjAuMC4xOjMwNTM1J1xuXG5jb25zdCBJU19DTE9VRF9CVUlMRCA9IHByb2Nlc3MuZW52LkNMT1VEX0JVSUxEID09PSAndHJ1ZSdcblxuYXdhaXQgcmVhZEVudmlyb25tZW50RnJvbUZpbGUoKVxuXG5jb25zdCBlbnRyeXBvaW50ID0gcHJvY2Vzcy5lbnYuRTJFID09PSAndHJ1ZScgPyAnLi9zcmMvZTJlLWVudHJ5cG9pbnQudHMnIDogJy4vc3JjL2VudHJ5cG9pbnQudHMnXG5cbi8vIGh0dHBzOi8vdml0ZWpzLmRldi9jb25maWcvXG5leHBvcnQgZGVmYXVsdCBkZWZpbmVDb25maWcoe1xuICByb290OiBmaWxlVVJMVG9QYXRoKG5ldyBVUkwoJy4nLCBpbXBvcnQubWV0YS51cmwpKSxcbiAgY2FjaGVEaXI6IGZpbGVVUkxUb1BhdGgobmV3IFVSTCgnLi4vLi4vbm9kZV9tb2R1bGVzLy5jYWNoZS92aXRlJywgaW1wb3J0Lm1ldGEudXJsKSksXG4gIHB1YmxpY0RpcjogZmlsZVVSTFRvUGF0aChuZXcgVVJMKCcuL3B1YmxpYycsIGltcG9ydC5tZXRhLnVybCkpLFxuICBlbnZEaXI6IGZpbGVVUkxUb1BhdGgobmV3IFVSTCgnLicsIGltcG9ydC5tZXRhLnVybCkpLFxuICBwbHVnaW5zOiBbXG4gICAgdnVlKCksXG4gICAgZ2F0ZXdheVNlcnZlcigpLFxuICAgIC4uLihwcm9jZXNzLmVudi5FTEVDVFJPTl9ERVZfTU9ERSA9PT0gJ3RydWUnID9cbiAgICAgIFtcbiAgICAgICAgKGF3YWl0IGltcG9ydCgnQHZpdGVqcy9wbHVnaW4tcmVhY3QnKSkuZGVmYXVsdCh7XG4gICAgICAgICAgaW5jbHVkZTogZmlsZVVSTFRvUGF0aChuZXcgVVJMKCcuLi9pZGUtZGVza3RvcC9saWIvZGFzaGJvYXJkLyoqLyoudHN4JywgaW1wb3J0Lm1ldGEudXJsKSksXG4gICAgICAgICAgYmFiZWw6IHsgcGx1Z2luczogWydAYmFiZWwvcGx1Z2luLXN5bnRheC1pbXBvcnQtYXNzZXJ0aW9ucyddIH0sXG4gICAgICAgIH0pLFxuICAgICAgXVxuICAgIDogcHJvY2Vzcy5lbnYuTk9ERV9FTlYgPT09ICdkZXZlbG9wbWVudCcgPyBbYXdhaXQgcHJvamVjdE1hbmFnZXJTaGltKCldXG4gICAgOiBbXSksXG4gIF0sXG4gIG9wdGltaXplRGVwczoge1xuICAgIGVudHJpZXM6IGZpbGVVUkxUb1BhdGgobmV3IFVSTCgnLi9pbmRleC5odG1sJywgaW1wb3J0Lm1ldGEudXJsKSksXG4gIH0sXG4gIHNlcnZlcjoge1xuICAgIHdhdGNoOiB7fSxcbiAgICBoZWFkZXJzOiB7XG4gICAgICAnQ3Jvc3MtT3JpZ2luLUVtYmVkZGVyLVBvbGljeSc6ICdyZXF1aXJlLWNvcnAnLFxuICAgICAgJ0Nyb3NzLU9yaWdpbi1PcGVuZXItUG9saWN5JzogJ3NhbWUtb3JpZ2luJyxcbiAgICAgICdDcm9zcy1PcmlnaW4tUmVzb3VyY2UtUG9saWN5JzogJ3NhbWUtb3JpZ2luJyxcbiAgICB9LFxuICB9LFxuICByZXNvbHZlOiB7XG4gICAgYWxpYXM6IHtcbiAgICAgICcvc3JjL2VudHJ5cG9pbnQudHMnOiBmaWxlVVJMVG9QYXRoKG5ldyBVUkwoZW50cnlwb2ludCwgaW1wb3J0Lm1ldGEudXJsKSksXG4gICAgICBzaGFyZWQ6IGZpbGVVUkxUb1BhdGgobmV3IFVSTCgnLi9zaGFyZWQnLCBpbXBvcnQubWV0YS51cmwpKSxcbiAgICAgICdAJzogZmlsZVVSTFRvUGF0aChuZXcgVVJMKCcuL3NyYycsIGltcG9ydC5tZXRhLnVybCkpLFxuICAgIH0sXG4gIH0sXG4gIGRlZmluZToge1xuICAgIC4uLmdldERlZmluZXMobG9jYWxTZXJ2ZXJQb3J0KSxcbiAgICBJU19DTE9VRF9CVUlMRDogSlNPTi5zdHJpbmdpZnkoSVNfQ0xPVURfQlVJTEQpLFxuICAgIFBST0pFQ1RfTUFOQUdFUl9VUkw6IEpTT04uc3RyaW5naWZ5KHByb2plY3RNYW5hZ2VyVXJsKSxcbiAgICBSVU5OSU5HX1ZJVEVTVDogZmFsc2UsXG4gICAgJ2ltcG9ydC5tZXRhLnZpdGVzdCc6IGZhbHNlLFxuICAgIC8vIFNpbmdsZSBoYXJkY29kZWQgdXNhZ2Ugb2YgYGdsb2JhbGAgaW4gYXdzLWFtcGxpZnkuXG4gICAgJ2dsb2JhbC5UWVBFRF9BUlJBWV9TVVBQT1JUJzogdHJ1ZSxcbiAgfSxcbiAgYXNzZXRzSW5jbHVkZTogWycqKi8qLnlhbWwnLCAnKiovKi5zdmcnXSxcbiAgY3NzOiB7XG4gICAgcG9zdGNzczoge1xuICAgICAgcGx1Z2luczogW1xuICAgICAgICB0YWlsd2luZGNzc05lc3RpbmcocG9zdGNzc05lc3RpbmcoKSksXG4gICAgICAgIHRhaWx3aW5kY3NzKHtcbiAgICAgICAgICAuLi50YWlsd2luZENvbmZpZy5kZWZhdWx0LFxuICAgICAgICAgIGNvbnRlbnQ6IHRhaWx3aW5kQ29uZmlnLmRlZmF1bHQuY29udGVudC5tYXAoKGdsb2I6IHN0cmluZykgPT5cbiAgICAgICAgICAgIGdsb2IucmVwbGFjZShcbiAgICAgICAgICAgICAgL15bLl1bL10vLFxuICAgICAgICAgICAgICBmaWxlVVJMVG9QYXRoKG5ldyBVUkwoJy4uL2lkZS1kZXNrdG9wL2xpYi9kYXNoYm9hcmQvJywgaW1wb3J0Lm1ldGEudXJsKSksXG4gICAgICAgICAgICApLFxuICAgICAgICAgICksXG4gICAgICAgIH0pLFxuICAgICAgXSxcbiAgICB9LFxuICB9LFxuICBidWlsZDoge1xuICAgIC8vIGRhc2hib2FyZCBjaHVuayBzaXplIGlzIGxhcmdlciB0aGFuIHRoZSBkZWZhdWx0IHdhcm5pbmcgbGltaXRcbiAgICBjaHVua1NpemVXYXJuaW5nTGltaXQ6IDcwMCxcbiAgICByb2xsdXBPcHRpb25zOiB7XG4gICAgICBvdXRwdXQ6IHtcbiAgICAgICAgbWFudWFsQ2h1bmtzOiB7XG4gICAgICAgICAgZm9udGF3ZXNvbWU6IFsnQGZvcnRhd2Vzb21lL3JlYWN0LWZvbnRhd2Vzb21lJywgJ0Bmb3J0YXdlc29tZS9mcmVlLWJyYW5kcy1zdmctaWNvbnMnXSxcbiAgICAgICAgfSxcbiAgICAgIH0sXG4gICAgfSxcbiAgfSxcbn0pXG5cbmZ1bmN0aW9uIGdhdGV3YXlTZXJ2ZXIoKTogUGx1Z2luIHtcbiAgcmV0dXJuIHtcbiAgICBuYW1lOiAnZ2F0ZXdheS1zZXJ2ZXInLFxuICAgIGNvbmZpZ3VyZVNlcnZlcihzZXJ2ZXIpIHtcbiAgICAgIGlmIChzZXJ2ZXIuaHR0cFNlcnZlciA9PSBudWxsKSByZXR1cm5cblxuICAgICAgY3JlYXRlR2F0ZXdheVNlcnZlcihzZXJ2ZXIuaHR0cFNlcnZlciwgdW5kZWZpbmVkKVxuICAgIH0sXG4gIH1cbn1cblxuYXN5bmMgZnVuY3Rpb24gcHJvamVjdE1hbmFnZXJTaGltKCk6IFByb21pc2U8UGx1Z2luPiB7XG4gIGNvbnN0IG1vZHVsZSA9IGF3YWl0IGltcG9ydChcbiAgICAnLi4vaWRlLWRlc2t0b3AvbGliL3Byb2plY3QtbWFuYWdlci1zaGltL3NyYy9wcm9qZWN0TWFuYWdlclNoaW1NaWRkbGV3YXJlJ1xuICApXG4gIHJldHVybiB7XG4gICAgbmFtZTogJ3Byb2plY3QtbWFuYWdlci1zaGltJyxcbiAgICBjb25maWd1cmVTZXJ2ZXIoc2VydmVyKSB7XG4gICAgICBzZXJ2ZXIubWlkZGxld2FyZXMudXNlKG1vZHVsZS5kZWZhdWx0KVxuICAgIH0sXG4gIH1cbn1cbiIsICJjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfZGlybmFtZSA9IFwiQzpcXFxcUHJvamVjdHNcXFxcZW5zb1xcXFxlbnNvXFxcXGFwcFxcXFxndWkyXFxcXHlkb2Mtc2VydmVyXCI7Y29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2ZpbGVuYW1lID0gXCJDOlxcXFxQcm9qZWN0c1xcXFxlbnNvXFxcXGVuc29cXFxcYXBwXFxcXGd1aTJcXFxceWRvYy1zZXJ2ZXJcXFxcaW5kZXgudHNcIjtjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfaW1wb3J0X21ldGFfdXJsID0gXCJmaWxlOi8vL0M6L1Byb2plY3RzL2Vuc28vZW5zby9hcHAvZ3VpMi95ZG9jLXNlcnZlci9pbmRleC50c1wiOy8qKlxuICogQGZpbGUgQW4gZW50cnkgcG9pbnQgZm9yIHRoZSBZanMgZ2F0ZXdheSBzZXJ2ZXIuIFRoZSBnYXRld2F5IHNlcnZlciBpcyBhIFdlYlNvY2tldCBzZXJ2ZXIgdGhhdFxuICogc3luY2hyb25pemVzIGRvY3VtZW50IHJlcXVlc3RzIGFuZCB1cGRhdGVzIGJldHdlZW4gbGFuZ3VhZ2Ugc2VydmVyIGFuZCBjbGllbnRzIGNvbm5lY3RlZCB0byB0aGVcbiAqIFlqcyBkb2N1bWVudCBtZXNoLiBJdCBhbHNvIHNlcnZlcyBhcyBhIGNlbnRyYWwgcG9pbnQgZm9yIHN5bmNocm9uaXppbmcgZG9jdW1lbnQgZGF0YSBhbmRcbiAqIGF3YXJlbmVzcyB1cGRhdGVzIGJldHdlZW4gY2xpZW50cy5cbiAqXG4gKiBDdXJyZW50bHksIHRoaXMgc2VydmVyIGlzIGJlaW5nIHJ1biBhdXRvbWF0aWNhbGx5IGluIGJhY2tncm91bmQgYXMgcGFydCBvZiB0aGUgdml0ZSBkZXZlbG9wbWVudFxuICogc2VydmVyLiBJdCBpcyBub3QgeWV0IGRlcGxveWVkIHRvIGFueSBvdGhlciBlbnZpcm9ubWVudC5cbiAqL1xuXG5pbXBvcnQgeyBTZXJ2ZXIgfSBmcm9tICdodHRwJ1xuaW1wb3J0IHsgSW5jb21pbmdNZXNzYWdlIH0gZnJvbSAnbm9kZTpodHRwJ1xuaW1wb3J0IHsgcGFyc2UgfSBmcm9tICd1cmwnXG5pbXBvcnQgeyBXZWJTb2NrZXQsIFdlYlNvY2tldFNlcnZlciB9IGZyb20gJ3dzJ1xuaW1wb3J0IHsgaW5pdGlhbGl6ZUZGSSB9IGZyb20gJy4uL3NoYXJlZC9hc3QvZmZpJ1xuaW1wb3J0IHsgc2V0dXBHYXRld2F5Q2xpZW50IH0gZnJvbSAnLi95ZG9jJ1xuXG50eXBlIENvbm5lY3Rpb25EYXRhID0ge1xuICBsc1VybDogc3RyaW5nXG4gIGRvYzogc3RyaW5nXG4gIHVzZXI6IHN0cmluZ1xufVxuXG5leHBvcnQgYXN5bmMgZnVuY3Rpb24gY3JlYXRlR2F0ZXdheVNlcnZlcihodHRwU2VydmVyOiBTZXJ2ZXIsIHJ1c3RGRklQYXRoOiBzdHJpbmcgfCB1bmRlZmluZWQpIHtcbiAgYXdhaXQgaW5pdGlhbGl6ZUZGSShydXN0RkZJUGF0aClcbiAgY29uc3Qgd3NzID0gbmV3IFdlYlNvY2tldFNlcnZlcih7IG5vU2VydmVyOiB0cnVlIH0pXG4gIHdzcy5vbignY29ubmVjdGlvbicsICh3czogV2ViU29ja2V0LCBfcmVxdWVzdDogSW5jb21pbmdNZXNzYWdlLCBkYXRhOiBDb25uZWN0aW9uRGF0YSkgPT4ge1xuICAgIHdzLm9uKCdlcnJvcicsIG9uV2ViU29ja2V0RXJyb3IpXG4gICAgc2V0dXBHYXRld2F5Q2xpZW50KHdzLCBkYXRhLmxzVXJsLCBkYXRhLmRvYylcbiAgfSlcblxuICBodHRwU2VydmVyLm9uKCd1cGdyYWRlJywgKHJlcXVlc3QsIHNvY2tldCwgaGVhZCkgPT4ge1xuICAgIHNvY2tldC5vbignZXJyb3InLCBvbkh0dHBTb2NrZXRFcnJvcilcbiAgICBhdXRoZW50aWNhdGUocmVxdWVzdCwgZnVuY3Rpb24gbmV4dChlcnIsIGRhdGEpIHtcbiAgICAgIGlmIChlcnIgIT0gbnVsbCkge1xuICAgICAgICBzb2NrZXQud3JpdGUoJ0hUVFAvMS4xIDQwMSBVbmF1dGhvcml6ZWRcXHJcXG5cXHJcXG4nKVxuICAgICAgICBzb2NrZXQuZGVzdHJveSgpXG4gICAgICAgIHJldHVyblxuICAgICAgfVxuICAgICAgc29ja2V0LnJlbW92ZUxpc3RlbmVyKCdlcnJvcicsIG9uSHR0cFNvY2tldEVycm9yKVxuICAgICAgaWYgKGRhdGEgIT0gbnVsbCkge1xuICAgICAgICB3c3MuaGFuZGxlVXBncmFkZShyZXF1ZXN0LCBzb2NrZXQsIGhlYWQsIGZ1bmN0aW9uIGRvbmUod3MpIHtcbiAgICAgICAgICB3c3MuZW1pdCgnY29ubmVjdGlvbicsIHdzLCByZXF1ZXN0LCBkYXRhKVxuICAgICAgICB9KVxuICAgICAgfVxuICAgIH0pXG4gIH0pXG59XG5cbmZ1bmN0aW9uIG9uV2ViU29ja2V0RXJyb3IoZXJyOiBFcnJvcikge1xuICBjb25zb2xlLmxvZygnV2ViU29ja2V0IGVycm9yOicsIGVycilcbn1cblxuZnVuY3Rpb24gb25IdHRwU29ja2V0RXJyb3IoZXJyOiBFcnJvcikge1xuICBjb25zb2xlLmxvZygnSFRUUCBzb2NrZXQgZXJyb3I6JywgZXJyKVxufVxuXG5mdW5jdGlvbiBhdXRoZW50aWNhdGUoXG4gIHJlcXVlc3Q6IEluY29taW5nTWVzc2FnZSxcbiAgY2FsbGJhY2s6IChlcnI6IEVycm9yIHwgbnVsbCwgYXV0aERhdGE6IENvbm5lY3Rpb25EYXRhIHwgbnVsbCkgPT4gdm9pZCxcbikge1xuICAvLyBGSVhNRTogU3R1Yi4gV2UgZG9uJ3QgaW1wbGVtZW50IGF1dGhlbnRpY2F0aW9uIGZvciBub3cuIE5lZWQgdG8gYmUgaW1wbGVtZW50ZWQgaW4gY29tYmluYXRpb25cbiAgLy8gd2l0aCB0aGUgbGFuZ3VhZ2Ugc2VydmVyLlxuICBjb25zdCB1c2VyID0gJ21vY2stdXNlcidcblxuICBpZiAocmVxdWVzdC51cmwgPT0gbnVsbCkgcmV0dXJuIGNhbGxiYWNrKG51bGwsIG51bGwpXG4gIGNvbnN0IHsgcGF0aG5hbWUsIHF1ZXJ5IH0gPSBwYXJzZShyZXF1ZXN0LnVybCwgdHJ1ZSlcbiAgaWYgKHBhdGhuYW1lID09IG51bGwpIHJldHVybiBjYWxsYmFjayhudWxsLCBudWxsKVxuICBjb25zdCBkb2MgPSBkb2NOYW1lKHBhdGhuYW1lKVxuICBjb25zdCBsc1VybCA9IHF1ZXJ5LmxzXG4gIGNvbnN0IGRhdGEgPSBkb2MgIT0gbnVsbCAmJiB0eXBlb2YgbHNVcmwgPT09ICdzdHJpbmcnID8geyBsc1VybCwgZG9jLCB1c2VyIH0gOiBudWxsXG4gIGNhbGxiYWNrKG51bGwsIGRhdGEpXG59XG5cbmNvbnN0IGRvY05hbWVSZWdleCA9IC9eW2EtejAtOS8tXSskL2lcbmZ1bmN0aW9uIGRvY05hbWUocGF0aG5hbWU6IHN0cmluZykge1xuICBjb25zdCBwcmVmaXggPSAnL3Byb2plY3QvJ1xuICBpZiAocGF0aG5hbWUgIT0gbnVsbCAmJiBwYXRobmFtZS5zdGFydHNXaXRoKHByZWZpeCkpIHtcbiAgICBjb25zdCBkb2NOYW1lID0gcGF0aG5hbWUuc2xpY2UocHJlZml4Lmxlbmd0aClcbiAgICBpZiAoZG9jTmFtZVJlZ2V4LnRlc3QoZG9jTmFtZSkpIHtcbiAgICAgIHJldHVybiBkb2NOYW1lXG4gICAgfVxuICB9XG4gIHJldHVybiBudWxsXG59XG4iLCAiY29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2Rpcm5hbWUgPSBcIkM6XFxcXFByb2plY3RzXFxcXGVuc29cXFxcZW5zb1xcXFxhcHBcXFxcZ3VpMlxcXFxzaGFyZWRcXFxcYXN0XCI7Y29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2ZpbGVuYW1lID0gXCJDOlxcXFxQcm9qZWN0c1xcXFxlbnNvXFxcXGVuc29cXFxcYXBwXFxcXGd1aTJcXFxcc2hhcmVkXFxcXGFzdFxcXFxmZmkudHNcIjtjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfaW1wb3J0X21ldGFfdXJsID0gXCJmaWxlOi8vL0M6L1Byb2plY3RzL2Vuc28vZW5zby9hcHAvZ3VpMi9zaGFyZWQvYXN0L2ZmaS50c1wiO2ltcG9ydCB7IGNyZWF0ZVhYSGFzaDEyOCB9IGZyb20gJ2hhc2gtd2FzbSdcbmltcG9ydCB0eXBlIHsgSURhdGFUeXBlIH0gZnJvbSAnaGFzaC13YXNtL2Rpc3QvbGliL3V0aWwnXG5pbXBvcnQgaW5pdCwgeyBpc19pZGVudF9vcl9vcGVyYXRvciwgcGFyc2UsIHBhcnNlX2RvY190b19qc29uIH0gZnJvbSAnLi4vLi4vcnVzdC1mZmkvcGtnL3J1c3RfZmZpJ1xuaW1wb3J0IHsgYXNzZXJ0RGVmaW5lZCB9IGZyb20gJy4uL3V0aWwvYXNzZXJ0J1xuaW1wb3J0IHsgaXNOb2RlIH0gZnJvbSAnLi4vdXRpbC9kZXRlY3QnXG5cbmxldCB4eEhhc2hlcjEyODogQXdhaXRlZDxSZXR1cm5UeXBlPHR5cGVvZiBjcmVhdGVYWEhhc2gxMjg+PiB8IHVuZGVmaW5lZFxuZXhwb3J0IGZ1bmN0aW9uIHh4SGFzaDEyOChpbnB1dDogSURhdGFUeXBlKSB7XG4gIGFzc2VydERlZmluZWQoeHhIYXNoZXIxMjgsICdNb2R1bGUgc2hvdWxkIGhhdmUgYmVlbiBsb2FkZWQgd2l0aCBgaW5pdGlhbGl6ZUZGSWAuJylcbiAgeHhIYXNoZXIxMjguaW5pdCgpXG4gIHh4SGFzaGVyMTI4LnVwZGF0ZShpbnB1dClcbiAgcmV0dXJuIHh4SGFzaGVyMTI4LmRpZ2VzdCgpXG59XG5cbmV4cG9ydCBhc3luYyBmdW5jdGlvbiBpbml0aWFsaXplRkZJKHBhdGg/OiBzdHJpbmcgfCB1bmRlZmluZWQpIHtcbiAgaWYgKGlzTm9kZSkge1xuICAgIGNvbnN0IGZzID0gYXdhaXQgaW1wb3J0KCdub2RlOmZzL3Byb21pc2VzJylcbiAgICBjb25zdCB7IGZpbGVVUkxUb1BhdGgsIFVSTDogbm9kZVVSTCB9ID0gYXdhaXQgaW1wb3J0KCdub2RlOnVybCcpXG4gICAgY29uc3QgYnVmZmVyID0gZnMucmVhZEZpbGUoXG4gICAgICBwYXRoID8/IGZpbGVVUkxUb1BhdGgobmV3IG5vZGVVUkwoJy4uLy4uL3J1c3QtZmZpL3BrZy9ydXN0X2ZmaV9iZy53YXNtJywgaW1wb3J0Lm1ldGEudXJsKSksXG4gICAgKVxuICAgIGF3YWl0IGluaXQoYnVmZmVyKVxuICB9IGVsc2Uge1xuICAgIGF3YWl0IGluaXQoKVxuICB9XG4gIHh4SGFzaGVyMTI4ID0gYXdhaXQgY3JlYXRlWFhIYXNoMTI4KClcbn1cblxuLy8gVE9ET1thb106IFdlIGNhbm5vdCB0byB0aGF0LCBiZWNhdXNlIHRoZSBmZmkgaXMgdXNlZCBieSBjanMgbW9kdWxlcy5cbi8vIGF3YWl0IGluaXRpYWxpemVGRkkoKVxuXG4vKiBlc2xpbnQtZGlzYWJsZS1uZXh0LWxpbmUgY2FtZWxjYXNlICovXG5leHBvcnQgeyBpc19pZGVudF9vcl9vcGVyYXRvciwgcGFyc2VfZG9jX3RvX2pzb24sIHBhcnNlIGFzIHBhcnNlX3RyZWUgfVxuIiwgImNvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9kaXJuYW1lID0gXCJDOlxcXFxQcm9qZWN0c1xcXFxlbnNvXFxcXGVuc29cXFxcYXBwXFxcXGd1aTJcXFxccnVzdC1mZmlcXFxccGtnXCI7Y29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2ZpbGVuYW1lID0gXCJDOlxcXFxQcm9qZWN0c1xcXFxlbnNvXFxcXGVuc29cXFxcYXBwXFxcXGd1aTJcXFxccnVzdC1mZmlcXFxccGtnXFxcXHJ1c3RfZmZpLmpzXCI7Y29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2ltcG9ydF9tZXRhX3VybCA9IFwiZmlsZTovLy9DOi9Qcm9qZWN0cy9lbnNvL2Vuc28vYXBwL2d1aTIvcnVzdC1mZmkvcGtnL3J1c3RfZmZpLmpzXCI7bGV0IHdhc207XG5cbmNvbnN0IGhlYXAgPSBuZXcgQXJyYXkoMTI4KS5maWxsKHVuZGVmaW5lZCk7XG5cbmhlYXAucHVzaCh1bmRlZmluZWQsIG51bGwsIHRydWUsIGZhbHNlKTtcblxuZnVuY3Rpb24gZ2V0T2JqZWN0KGlkeCkgeyByZXR1cm4gaGVhcFtpZHhdOyB9XG5cbmxldCBoZWFwX25leHQgPSBoZWFwLmxlbmd0aDtcblxuZnVuY3Rpb24gZHJvcE9iamVjdChpZHgpIHtcbiAgICBpZiAoaWR4IDwgMTMyKSByZXR1cm47XG4gICAgaGVhcFtpZHhdID0gaGVhcF9uZXh0O1xuICAgIGhlYXBfbmV4dCA9IGlkeDtcbn1cblxuZnVuY3Rpb24gdGFrZU9iamVjdChpZHgpIHtcbiAgICBjb25zdCByZXQgPSBnZXRPYmplY3QoaWR4KTtcbiAgICBkcm9wT2JqZWN0KGlkeCk7XG4gICAgcmV0dXJuIHJldDtcbn1cblxubGV0IFdBU01fVkVDVE9SX0xFTiA9IDA7XG5cbmxldCBjYWNoZWRVaW50OE1lbW9yeTAgPSBudWxsO1xuXG5mdW5jdGlvbiBnZXRVaW50OE1lbW9yeTAoKSB7XG4gICAgaWYgKGNhY2hlZFVpbnQ4TWVtb3J5MCA9PT0gbnVsbCB8fCBjYWNoZWRVaW50OE1lbW9yeTAuYnl0ZUxlbmd0aCA9PT0gMCkge1xuICAgICAgICBjYWNoZWRVaW50OE1lbW9yeTAgPSBuZXcgVWludDhBcnJheSh3YXNtLm1lbW9yeS5idWZmZXIpO1xuICAgIH1cbiAgICByZXR1cm4gY2FjaGVkVWludDhNZW1vcnkwO1xufVxuXG5jb25zdCBjYWNoZWRUZXh0RW5jb2RlciA9ICh0eXBlb2YgVGV4dEVuY29kZXIgIT09ICd1bmRlZmluZWQnID8gbmV3IFRleHRFbmNvZGVyKCd1dGYtOCcpIDogeyBlbmNvZGU6ICgpID0+IHsgdGhyb3cgRXJyb3IoJ1RleHRFbmNvZGVyIG5vdCBhdmFpbGFibGUnKSB9IH0gKTtcblxuY29uc3QgZW5jb2RlU3RyaW5nID0gKHR5cGVvZiBjYWNoZWRUZXh0RW5jb2Rlci5lbmNvZGVJbnRvID09PSAnZnVuY3Rpb24nXG4gICAgPyBmdW5jdGlvbiAoYXJnLCB2aWV3KSB7XG4gICAgcmV0dXJuIGNhY2hlZFRleHRFbmNvZGVyLmVuY29kZUludG8oYXJnLCB2aWV3KTtcbn1cbiAgICA6IGZ1bmN0aW9uIChhcmcsIHZpZXcpIHtcbiAgICBjb25zdCBidWYgPSBjYWNoZWRUZXh0RW5jb2Rlci5lbmNvZGUoYXJnKTtcbiAgICB2aWV3LnNldChidWYpO1xuICAgIHJldHVybiB7XG4gICAgICAgIHJlYWQ6IGFyZy5sZW5ndGgsXG4gICAgICAgIHdyaXR0ZW46IGJ1Zi5sZW5ndGhcbiAgICB9O1xufSk7XG5cbmZ1bmN0aW9uIHBhc3NTdHJpbmdUb1dhc20wKGFyZywgbWFsbG9jLCByZWFsbG9jKSB7XG5cbiAgICBpZiAocmVhbGxvYyA9PT0gdW5kZWZpbmVkKSB7XG4gICAgICAgIGNvbnN0IGJ1ZiA9IGNhY2hlZFRleHRFbmNvZGVyLmVuY29kZShhcmcpO1xuICAgICAgICBjb25zdCBwdHIgPSBtYWxsb2MoYnVmLmxlbmd0aCwgMSkgPj4+IDA7XG4gICAgICAgIGdldFVpbnQ4TWVtb3J5MCgpLnN1YmFycmF5KHB0ciwgcHRyICsgYnVmLmxlbmd0aCkuc2V0KGJ1Zik7XG4gICAgICAgIFdBU01fVkVDVE9SX0xFTiA9IGJ1Zi5sZW5ndGg7XG4gICAgICAgIHJldHVybiBwdHI7XG4gICAgfVxuXG4gICAgbGV0IGxlbiA9IGFyZy5sZW5ndGg7XG4gICAgbGV0IHB0ciA9IG1hbGxvYyhsZW4sIDEpID4+PiAwO1xuXG4gICAgY29uc3QgbWVtID0gZ2V0VWludDhNZW1vcnkwKCk7XG5cbiAgICBsZXQgb2Zmc2V0ID0gMDtcblxuICAgIGZvciAoOyBvZmZzZXQgPCBsZW47IG9mZnNldCsrKSB7XG4gICAgICAgIGNvbnN0IGNvZGUgPSBhcmcuY2hhckNvZGVBdChvZmZzZXQpO1xuICAgICAgICBpZiAoY29kZSA+IDB4N0YpIGJyZWFrO1xuICAgICAgICBtZW1bcHRyICsgb2Zmc2V0XSA9IGNvZGU7XG4gICAgfVxuXG4gICAgaWYgKG9mZnNldCAhPT0gbGVuKSB7XG4gICAgICAgIGlmIChvZmZzZXQgIT09IDApIHtcbiAgICAgICAgICAgIGFyZyA9IGFyZy5zbGljZShvZmZzZXQpO1xuICAgICAgICB9XG4gICAgICAgIHB0ciA9IHJlYWxsb2MocHRyLCBsZW4sIGxlbiA9IG9mZnNldCArIGFyZy5sZW5ndGggKiAzLCAxKSA+Pj4gMDtcbiAgICAgICAgY29uc3QgdmlldyA9IGdldFVpbnQ4TWVtb3J5MCgpLnN1YmFycmF5KHB0ciArIG9mZnNldCwgcHRyICsgbGVuKTtcbiAgICAgICAgY29uc3QgcmV0ID0gZW5jb2RlU3RyaW5nKGFyZywgdmlldyk7XG5cbiAgICAgICAgb2Zmc2V0ICs9IHJldC53cml0dGVuO1xuICAgICAgICBwdHIgPSByZWFsbG9jKHB0ciwgbGVuLCBvZmZzZXQsIDEpID4+PiAwO1xuICAgIH1cblxuICAgIFdBU01fVkVDVE9SX0xFTiA9IG9mZnNldDtcbiAgICByZXR1cm4gcHRyO1xufVxuXG5sZXQgY2FjaGVkSW50MzJNZW1vcnkwID0gbnVsbDtcblxuZnVuY3Rpb24gZ2V0SW50MzJNZW1vcnkwKCkge1xuICAgIGlmIChjYWNoZWRJbnQzMk1lbW9yeTAgPT09IG51bGwgfHwgY2FjaGVkSW50MzJNZW1vcnkwLmJ5dGVMZW5ndGggPT09IDApIHtcbiAgICAgICAgY2FjaGVkSW50MzJNZW1vcnkwID0gbmV3IEludDMyQXJyYXkod2FzbS5tZW1vcnkuYnVmZmVyKTtcbiAgICB9XG4gICAgcmV0dXJuIGNhY2hlZEludDMyTWVtb3J5MDtcbn1cblxuY29uc3QgY2FjaGVkVGV4dERlY29kZXIgPSAodHlwZW9mIFRleHREZWNvZGVyICE9PSAndW5kZWZpbmVkJyA/IG5ldyBUZXh0RGVjb2RlcigndXRmLTgnLCB7IGlnbm9yZUJPTTogdHJ1ZSwgZmF0YWw6IHRydWUgfSkgOiB7IGRlY29kZTogKCkgPT4geyB0aHJvdyBFcnJvcignVGV4dERlY29kZXIgbm90IGF2YWlsYWJsZScpIH0gfSApO1xuXG5pZiAodHlwZW9mIFRleHREZWNvZGVyICE9PSAndW5kZWZpbmVkJykgeyBjYWNoZWRUZXh0RGVjb2Rlci5kZWNvZGUoKTsgfTtcblxuZnVuY3Rpb24gZ2V0U3RyaW5nRnJvbVdhc20wKHB0ciwgbGVuKSB7XG4gICAgcHRyID0gcHRyID4+PiAwO1xuICAgIHJldHVybiBjYWNoZWRUZXh0RGVjb2Rlci5kZWNvZGUoZ2V0VWludDhNZW1vcnkwKCkuc3ViYXJyYXkocHRyLCBwdHIgKyBsZW4pKTtcbn1cbi8qKlxuKiBAcGFyYW0ge3N0cmluZ30gZG9jc1xuKiBAcmV0dXJucyB7c3RyaW5nfVxuKi9cbmV4cG9ydCBmdW5jdGlvbiBwYXJzZV9kb2NfdG9fanNvbihkb2NzKSB7XG4gICAgbGV0IGRlZmVycmVkMl8wO1xuICAgIGxldCBkZWZlcnJlZDJfMTtcbiAgICB0cnkge1xuICAgICAgICBjb25zdCByZXRwdHIgPSB3YXNtLl9fd2JpbmRnZW5fYWRkX3RvX3N0YWNrX3BvaW50ZXIoLTE2KTtcbiAgICAgICAgY29uc3QgcHRyMCA9IHBhc3NTdHJpbmdUb1dhc20wKGRvY3MsIHdhc20uX193YmluZGdlbl9tYWxsb2MsIHdhc20uX193YmluZGdlbl9yZWFsbG9jKTtcbiAgICAgICAgY29uc3QgbGVuMCA9IFdBU01fVkVDVE9SX0xFTjtcbiAgICAgICAgd2FzbS5wYXJzZV9kb2NfdG9fanNvbihyZXRwdHIsIHB0cjAsIGxlbjApO1xuICAgICAgICB2YXIgcjAgPSBnZXRJbnQzMk1lbW9yeTAoKVtyZXRwdHIgLyA0ICsgMF07XG4gICAgICAgIHZhciByMSA9IGdldEludDMyTWVtb3J5MCgpW3JldHB0ciAvIDQgKyAxXTtcbiAgICAgICAgZGVmZXJyZWQyXzAgPSByMDtcbiAgICAgICAgZGVmZXJyZWQyXzEgPSByMTtcbiAgICAgICAgcmV0dXJuIGdldFN0cmluZ0Zyb21XYXNtMChyMCwgcjEpO1xuICAgIH0gZmluYWxseSB7XG4gICAgICAgIHdhc20uX193YmluZGdlbl9hZGRfdG9fc3RhY2tfcG9pbnRlcigxNik7XG4gICAgICAgIHdhc20uX193YmluZGdlbl9mcmVlKGRlZmVycmVkMl8wLCBkZWZlcnJlZDJfMSwgMSk7XG4gICAgfVxufVxuXG5mdW5jdGlvbiBnZXRBcnJheVU4RnJvbVdhc20wKHB0ciwgbGVuKSB7XG4gICAgcHRyID0gcHRyID4+PiAwO1xuICAgIHJldHVybiBnZXRVaW50OE1lbW9yeTAoKS5zdWJhcnJheShwdHIgLyAxLCBwdHIgLyAxICsgbGVuKTtcbn1cbi8qKlxuKiBAcGFyYW0ge3N0cmluZ30gY29kZVxuKiBAcmV0dXJucyB7VWludDhBcnJheX1cbiovXG5leHBvcnQgZnVuY3Rpb24gcGFyc2UoY29kZSkge1xuICAgIHRyeSB7XG4gICAgICAgIGNvbnN0IHJldHB0ciA9IHdhc20uX193YmluZGdlbl9hZGRfdG9fc3RhY2tfcG9pbnRlcigtMTYpO1xuICAgICAgICBjb25zdCBwdHIwID0gcGFzc1N0cmluZ1RvV2FzbTAoY29kZSwgd2FzbS5fX3diaW5kZ2VuX21hbGxvYywgd2FzbS5fX3diaW5kZ2VuX3JlYWxsb2MpO1xuICAgICAgICBjb25zdCBsZW4wID0gV0FTTV9WRUNUT1JfTEVOO1xuICAgICAgICB3YXNtLnBhcnNlKHJldHB0ciwgcHRyMCwgbGVuMCk7XG4gICAgICAgIHZhciByMCA9IGdldEludDMyTWVtb3J5MCgpW3JldHB0ciAvIDQgKyAwXTtcbiAgICAgICAgdmFyIHIxID0gZ2V0SW50MzJNZW1vcnkwKClbcmV0cHRyIC8gNCArIDFdO1xuICAgICAgICB2YXIgdjIgPSBnZXRBcnJheVU4RnJvbVdhc20wKHIwLCByMSkuc2xpY2UoKTtcbiAgICAgICAgd2FzbS5fX3diaW5kZ2VuX2ZyZWUocjAsIHIxICogMSwgMSk7XG4gICAgICAgIHJldHVybiB2MjtcbiAgICB9IGZpbmFsbHkge1xuICAgICAgICB3YXNtLl9fd2JpbmRnZW5fYWRkX3RvX3N0YWNrX3BvaW50ZXIoMTYpO1xuICAgIH1cbn1cblxuLyoqXG4qIEBwYXJhbSB7c3RyaW5nfSBjb2RlXG4qIEByZXR1cm5zIHtudW1iZXJ9XG4qL1xuZXhwb3J0IGZ1bmN0aW9uIGlzX2lkZW50X29yX29wZXJhdG9yKGNvZGUpIHtcbiAgICBjb25zdCBwdHIwID0gcGFzc1N0cmluZ1RvV2FzbTAoY29kZSwgd2FzbS5fX3diaW5kZ2VuX21hbGxvYywgd2FzbS5fX3diaW5kZ2VuX3JlYWxsb2MpO1xuICAgIGNvbnN0IGxlbjAgPSBXQVNNX1ZFQ1RPUl9MRU47XG4gICAgY29uc3QgcmV0ID0gd2FzbS5pc19pZGVudF9vcl9vcGVyYXRvcihwdHIwLCBsZW4wKTtcbiAgICByZXR1cm4gcmV0ID4+PiAwO1xufVxuXG4vKipcbiovXG5leHBvcnQgZnVuY3Rpb24gbWFpbigpIHtcbiAgICB3YXNtLm1haW4oKTtcbn1cblxuZnVuY3Rpb24gYWRkSGVhcE9iamVjdChvYmopIHtcbiAgICBpZiAoaGVhcF9uZXh0ID09PSBoZWFwLmxlbmd0aCkgaGVhcC5wdXNoKGhlYXAubGVuZ3RoICsgMSk7XG4gICAgY29uc3QgaWR4ID0gaGVhcF9uZXh0O1xuICAgIGhlYXBfbmV4dCA9IGhlYXBbaWR4XTtcblxuICAgIGhlYXBbaWR4XSA9IG9iajtcbiAgICByZXR1cm4gaWR4O1xufVxuXG5hc3luYyBmdW5jdGlvbiBfX3diZ19sb2FkKG1vZHVsZSwgaW1wb3J0cykge1xuICAgIGlmICh0eXBlb2YgUmVzcG9uc2UgPT09ICdmdW5jdGlvbicgJiYgbW9kdWxlIGluc3RhbmNlb2YgUmVzcG9uc2UpIHtcbiAgICAgICAgaWYgKHR5cGVvZiBXZWJBc3NlbWJseS5pbnN0YW50aWF0ZVN0cmVhbWluZyA9PT0gJ2Z1bmN0aW9uJykge1xuICAgICAgICAgICAgdHJ5IHtcbiAgICAgICAgICAgICAgICByZXR1cm4gYXdhaXQgV2ViQXNzZW1ibHkuaW5zdGFudGlhdGVTdHJlYW1pbmcobW9kdWxlLCBpbXBvcnRzKTtcblxuICAgICAgICAgICAgfSBjYXRjaCAoZSkge1xuICAgICAgICAgICAgICAgIGlmIChtb2R1bGUuaGVhZGVycy5nZXQoJ0NvbnRlbnQtVHlwZScpICE9ICdhcHBsaWNhdGlvbi93YXNtJykge1xuICAgICAgICAgICAgICAgICAgICBjb25zb2xlLndhcm4oXCJgV2ViQXNzZW1ibHkuaW5zdGFudGlhdGVTdHJlYW1pbmdgIGZhaWxlZCBiZWNhdXNlIHlvdXIgc2VydmVyIGRvZXMgbm90IHNlcnZlIHdhc20gd2l0aCBgYXBwbGljYXRpb24vd2FzbWAgTUlNRSB0eXBlLiBGYWxsaW5nIGJhY2sgdG8gYFdlYkFzc2VtYmx5Lmluc3RhbnRpYXRlYCB3aGljaCBpcyBzbG93ZXIuIE9yaWdpbmFsIGVycm9yOlxcblwiLCBlKTtcblxuICAgICAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgICAgIHRocm93IGU7XG4gICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgfVxuICAgICAgICB9XG5cbiAgICAgICAgY29uc3QgYnl0ZXMgPSBhd2FpdCBtb2R1bGUuYXJyYXlCdWZmZXIoKTtcbiAgICAgICAgcmV0dXJuIGF3YWl0IFdlYkFzc2VtYmx5Lmluc3RhbnRpYXRlKGJ5dGVzLCBpbXBvcnRzKTtcblxuICAgIH0gZWxzZSB7XG4gICAgICAgIGNvbnN0IGluc3RhbmNlID0gYXdhaXQgV2ViQXNzZW1ibHkuaW5zdGFudGlhdGUobW9kdWxlLCBpbXBvcnRzKTtcblxuICAgICAgICBpZiAoaW5zdGFuY2UgaW5zdGFuY2VvZiBXZWJBc3NlbWJseS5JbnN0YW5jZSkge1xuICAgICAgICAgICAgcmV0dXJuIHsgaW5zdGFuY2UsIG1vZHVsZSB9O1xuXG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICByZXR1cm4gaW5zdGFuY2U7XG4gICAgICAgIH1cbiAgICB9XG59XG5cbmZ1bmN0aW9uIF9fd2JnX2dldF9pbXBvcnRzKCkge1xuICAgIGNvbnN0IGltcG9ydHMgPSB7fTtcbiAgICBpbXBvcnRzLndiZyA9IHt9O1xuICAgIGltcG9ydHMud2JnLl9fd2JnX2Vycm9yX2Y4NTE2NjdhZjcxYmNmYzYgPSBmdW5jdGlvbihhcmcwLCBhcmcxKSB7XG4gICAgICAgIGxldCBkZWZlcnJlZDBfMDtcbiAgICAgICAgbGV0IGRlZmVycmVkMF8xO1xuICAgICAgICB0cnkge1xuICAgICAgICAgICAgZGVmZXJyZWQwXzAgPSBhcmcwO1xuICAgICAgICAgICAgZGVmZXJyZWQwXzEgPSBhcmcxO1xuICAgICAgICAgICAgY29uc29sZS5lcnJvcihnZXRTdHJpbmdGcm9tV2FzbTAoYXJnMCwgYXJnMSkpO1xuICAgICAgICB9IGZpbmFsbHkge1xuICAgICAgICAgICAgd2FzbS5fX3diaW5kZ2VuX2ZyZWUoZGVmZXJyZWQwXzAsIGRlZmVycmVkMF8xLCAxKTtcbiAgICAgICAgfVxuICAgIH07XG4gICAgaW1wb3J0cy53YmcuX193YmdfbmV3X2FiZGE3NmU4ODNiYThhNWYgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgY29uc3QgcmV0ID0gbmV3IEVycm9yKCk7XG4gICAgICAgIHJldHVybiBhZGRIZWFwT2JqZWN0KHJldCk7XG4gICAgfTtcbiAgICBpbXBvcnRzLndiZy5fX3diZ19zdGFja182NTgyNzlmZTQ0NTQxY2Y2ID0gZnVuY3Rpb24oYXJnMCwgYXJnMSkge1xuICAgICAgICBjb25zdCByZXQgPSBnZXRPYmplY3QoYXJnMSkuc3RhY2s7XG4gICAgICAgIGNvbnN0IHB0cjEgPSBwYXNzU3RyaW5nVG9XYXNtMChyZXQsIHdhc20uX193YmluZGdlbl9tYWxsb2MsIHdhc20uX193YmluZGdlbl9yZWFsbG9jKTtcbiAgICAgICAgY29uc3QgbGVuMSA9IFdBU01fVkVDVE9SX0xFTjtcbiAgICAgICAgZ2V0SW50MzJNZW1vcnkwKClbYXJnMCAvIDQgKyAxXSA9IGxlbjE7XG4gICAgICAgIGdldEludDMyTWVtb3J5MCgpW2FyZzAgLyA0ICsgMF0gPSBwdHIxO1xuICAgIH07XG4gICAgaW1wb3J0cy53YmcuX193YmluZGdlbl9vYmplY3RfZHJvcF9yZWYgPSBmdW5jdGlvbihhcmcwKSB7XG4gICAgICAgIHRha2VPYmplY3QoYXJnMCk7XG4gICAgfTtcblxuICAgIHJldHVybiBpbXBvcnRzO1xufVxuXG5mdW5jdGlvbiBfX3diZ19pbml0X21lbW9yeShpbXBvcnRzLCBtYXliZV9tZW1vcnkpIHtcblxufVxuXG5mdW5jdGlvbiBfX3diZ19maW5hbGl6ZV9pbml0KGluc3RhbmNlLCBtb2R1bGUpIHtcbiAgICB3YXNtID0gaW5zdGFuY2UuZXhwb3J0cztcbiAgICBfX3diZ19pbml0Ll9fd2JpbmRnZW5fd2FzbV9tb2R1bGUgPSBtb2R1bGU7XG4gICAgY2FjaGVkSW50MzJNZW1vcnkwID0gbnVsbDtcbiAgICBjYWNoZWRVaW50OE1lbW9yeTAgPSBudWxsO1xuXG4gICAgd2FzbS5fX3diaW5kZ2VuX3N0YXJ0KCk7XG4gICAgcmV0dXJuIHdhc207XG59XG5cbmZ1bmN0aW9uIGluaXRTeW5jKG1vZHVsZSkge1xuICAgIGlmICh3YXNtICE9PSB1bmRlZmluZWQpIHJldHVybiB3YXNtO1xuXG4gICAgY29uc3QgaW1wb3J0cyA9IF9fd2JnX2dldF9pbXBvcnRzKCk7XG5cbiAgICBfX3diZ19pbml0X21lbW9yeShpbXBvcnRzKTtcblxuICAgIGlmICghKG1vZHVsZSBpbnN0YW5jZW9mIFdlYkFzc2VtYmx5Lk1vZHVsZSkpIHtcbiAgICAgICAgbW9kdWxlID0gbmV3IFdlYkFzc2VtYmx5Lk1vZHVsZShtb2R1bGUpO1xuICAgIH1cblxuICAgIGNvbnN0IGluc3RhbmNlID0gbmV3IFdlYkFzc2VtYmx5Lkluc3RhbmNlKG1vZHVsZSwgaW1wb3J0cyk7XG5cbiAgICByZXR1cm4gX193YmdfZmluYWxpemVfaW5pdChpbnN0YW5jZSwgbW9kdWxlKTtcbn1cblxuYXN5bmMgZnVuY3Rpb24gX193YmdfaW5pdChpbnB1dCkge1xuICAgIGlmICh3YXNtICE9PSB1bmRlZmluZWQpIHJldHVybiB3YXNtO1xuXG4gICAgaWYgKHR5cGVvZiBpbnB1dCA9PT0gJ3VuZGVmaW5lZCcpIHtcbiAgICAgICAgaW5wdXQgPSBuZXcgVVJMKCdydXN0X2ZmaV9iZy53YXNtJywgaW1wb3J0Lm1ldGEudXJsKTtcbiAgICB9XG4gICAgY29uc3QgaW1wb3J0cyA9IF9fd2JnX2dldF9pbXBvcnRzKCk7XG5cbiAgICBpZiAodHlwZW9mIGlucHV0ID09PSAnc3RyaW5nJyB8fCAodHlwZW9mIFJlcXVlc3QgPT09ICdmdW5jdGlvbicgJiYgaW5wdXQgaW5zdGFuY2VvZiBSZXF1ZXN0KSB8fCAodHlwZW9mIFVSTCA9PT0gJ2Z1bmN0aW9uJyAmJiBpbnB1dCBpbnN0YW5jZW9mIFVSTCkpIHtcbiAgICAgICAgaW5wdXQgPSBmZXRjaChpbnB1dCk7XG4gICAgfVxuXG4gICAgX193YmdfaW5pdF9tZW1vcnkoaW1wb3J0cyk7XG5cbiAgICBjb25zdCB7IGluc3RhbmNlLCBtb2R1bGUgfSA9IGF3YWl0IF9fd2JnX2xvYWQoYXdhaXQgaW5wdXQsIGltcG9ydHMpO1xuXG4gICAgcmV0dXJuIF9fd2JnX2ZpbmFsaXplX2luaXQoaW5zdGFuY2UsIG1vZHVsZSk7XG59XG5cbmV4cG9ydCB7IGluaXRTeW5jIH1cbmV4cG9ydCBkZWZhdWx0IF9fd2JnX2luaXQ7XG4iLCAiY29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2Rpcm5hbWUgPSBcIkM6XFxcXFByb2plY3RzXFxcXGVuc29cXFxcZW5zb1xcXFxhcHBcXFxcZ3VpMlxcXFxzaGFyZWRcXFxcdXRpbFwiO2NvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9maWxlbmFtZSA9IFwiQzpcXFxcUHJvamVjdHNcXFxcZW5zb1xcXFxlbnNvXFxcXGFwcFxcXFxndWkyXFxcXHNoYXJlZFxcXFx1dGlsXFxcXGFzc2VydC50c1wiO2NvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9pbXBvcnRfbWV0YV91cmwgPSBcImZpbGU6Ly8vQzovUHJvamVjdHMvZW5zby9lbnNvL2FwcC9ndWkyL3NoYXJlZC91dGlsL2Fzc2VydC50c1wiO2V4cG9ydCBmdW5jdGlvbiBhc3NlcnROZXZlcih4OiBuZXZlcik6IG5ldmVyIHtcbiAgYmFpbCgnVW5leHBlY3RlZCBvYmplY3Q6ICcgKyBKU09OLnN0cmluZ2lmeSh4KSlcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGFzc2VydChjb25kaXRpb246IGJvb2xlYW4sIG1lc3NhZ2U/OiBzdHJpbmcpOiBhc3NlcnRzIGNvbmRpdGlvbiB7XG4gIGlmICghY29uZGl0aW9uKSBiYWlsKG1lc3NhZ2UgPyBgQXNzZXJ0aW9uIGZhaWxlZDogJHttZXNzYWdlfWAgOiAnQXNzZXJ0aW9uIGZhaWxlZCcpXG59XG5cbi8qKlxuICogQ2hlY2tzIGlmIHRoZSBnaXZlbiBpdGVyYWJsZSBoYXMgdGhlIHNwZWNpZmllZCBsZW5ndGggYW5kIHRocm93cyBhbiBhc3NlcnRpb24gZXJyb3JcbiAqIGlmIHRoZSBsZW5ndGhzIGRvIG5vdCBtYXRjaC5cbiAqXG4gKiBAcGFyYW0gaXRlcmFibGUgVGhlIGl0ZXJhYmxlIHRvIGNoZWNrLlxuICogQHBhcmFtIGxlbmd0aCBUaGUgZXhwZWN0ZWQgbGVuZ3RoIG9mIHRoZSBpdGVyYWJsZS5cbiAqIEBwYXJhbSBtZXNzYWdlIE9wdGlvbmFsIG1lc3NhZ2UgZm9yIHRoZSBhc3NlcnRpb24gZXJyb3IuXG4gKiBAcmV0dXJuIHZvaWRcbiAqIEB0aHJvd3MgRXJyb3IgV2lsbCB0aHJvdyBhbiBlcnJvciBpZiB0aGUgbGVuZ3RoIGRvZXMgbm90IG1hdGNoLlxuICpcbiAqIFRoZSBmaXJzdCBmaXZlIGVsZW1lbnRzIG9mIHRoZSBpdGVyYWJsZSB3aWxsIGJlIGRpc3BsYXllZCBpbiB0aGUgZXJyb3IgbWVzc2FnZVxuICogaWYgdGhlIGFzc2VydGlvbiBmYWlscy4gSWYgdGhlIGl0ZXJhYmxlIGNvbnRhaW5zIG1vcmUgdGhhbiBmaXZlIGVsZW1lbnRzLFxuICogdGhlIHJlbWFpbmluZyBlbGVtZW50cyB3aWxsIGJlIHJlcHJlc2VudGVkIGFzICcuLi4nLlxuICovXG5leHBvcnQgZnVuY3Rpb24gYXNzZXJ0TGVuZ3RoPFQ+KGl0ZXJhYmxlOiBJdGVyYWJsZTxUPiwgbGVuZ3RoOiBudW1iZXIsIG1lc3NhZ2U/OiBzdHJpbmcpOiB2b2lkIHtcbiAgY29uc3QgY29udmVydGVkQXJyYXkgPSBBcnJheS5mcm9tKGl0ZXJhYmxlKVxuICBjb25zdCBtZXNzYWdlUHJlZml4ID0gbWVzc2FnZSA/IG1lc3NhZ2UgKyAnICcgOiAnJ1xuICBjb25zdCBlbGVtZW50UmVwcmVzZW50YXRpb24gPVxuICAgIGNvbnZlcnRlZEFycmF5Lmxlbmd0aCA+IDUgP1xuICAgICAgYCR7Y29udmVydGVkQXJyYXkuc2xpY2UoMCwgNSkuam9pbignLCAnKX0sLi4uYFxuICAgIDogY29udmVydGVkQXJyYXkuam9pbignLCAnKVxuICBhc3NlcnQoXG4gICAgY29udmVydGVkQXJyYXkubGVuZ3RoID09PSBsZW5ndGgsXG4gICAgYCR7bWVzc2FnZVByZWZpeH1FeHBlY3RlZCBpdGVyYWJsZSBvZiBsZW5ndGggJHtsZW5ndGh9LCBnb3QgbGVuZ3RoICR7Y29udmVydGVkQXJyYXkubGVuZ3RofS4gRWxlbWVudHM6IFske2VsZW1lbnRSZXByZXNlbnRhdGlvbn1dYCxcbiAgKVxufVxuXG5leHBvcnQgZnVuY3Rpb24gYXNzZXJ0RW1wdHk8VD4oaXRlcmFibGU6IEl0ZXJhYmxlPFQ+LCBtZXNzYWdlPzogc3RyaW5nKTogdm9pZCB7XG4gIGFzc2VydExlbmd0aChpdGVyYWJsZSwgMCwgbWVzc2FnZSlcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGFzc2VydEVxdWFsPFQ+KGFjdHVhbDogVCwgZXhwZWN0ZWQ6IFQsIG1lc3NhZ2U/OiBzdHJpbmcpIHtcbiAgY29uc3QgbWVzc2FnZVByZWZpeCA9IG1lc3NhZ2UgPyBtZXNzYWdlICsgJyAnIDogJydcbiAgYXNzZXJ0KGFjdHVhbCA9PT0gZXhwZWN0ZWQsIGAke21lc3NhZ2VQcmVmaXh9RXhwZWN0ZWQgJHtleHBlY3RlZH0sIGdvdCAke2FjdHVhbH0uYClcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGFzc2VydE5vdEVxdWFsPFQ+KGFjdHVhbDogVCwgdW5leHBlY3RlZDogVCwgbWVzc2FnZT86IHN0cmluZykge1xuICBjb25zdCBtZXNzYWdlUHJlZml4ID0gbWVzc2FnZSA/IG1lc3NhZ2UgKyAnICcgOiAnJ1xuICBhc3NlcnQoYWN0dWFsICE9PSB1bmV4cGVjdGVkLCBgJHttZXNzYWdlUHJlZml4fUV4cGVjdGVkIG5vdCAke3VuZXhwZWN0ZWR9LCBnb3QgJHthY3R1YWx9LmApXG59XG5cbmV4cG9ydCBmdW5jdGlvbiBhc3NlcnREZWZpbmVkPFQ+KHg6IFQgfCB1bmRlZmluZWQsIG1lc3NhZ2U/OiBzdHJpbmcpOiBhc3NlcnRzIHggaXMgVCB7XG4gIGNvbnN0IG1lc3NhZ2VQcmVmaXggPSBtZXNzYWdlID8gbWVzc2FnZSArICcgJyA6ICcnXG4gIGFzc2VydCh4ICE9PSB1bmRlZmluZWQsIGAke21lc3NhZ2VQcmVmaXh9RXhwZWN0ZWQgdmFsdWUgdG8gYmUgZGVmaW5lZC5gKVxufVxuXG5leHBvcnQgZnVuY3Rpb24gYXNzZXJ0VW5yZWFjaGFibGUoKTogbmV2ZXIge1xuICBiYWlsKCdVbnJlYWNoYWJsZSBjb2RlJylcbn1cblxuLyoqXG4gKiBUaHJvdyBhbiBlcnJvciB3aXRoIHByb3ZpZGVkIG1lc3NhZ2UuXG4gKlxuICogSXQgaXMgY29udmVuaWVudCB0byB1c2UgYXQgdGhlIGVuZCBvZiBhIG51bGxhYmxlIGNoYWluOlxuICogYGBgdHNcbiAqIGNvbnN0IHggPSBmb28/LmJhci5iYXo/LigpID8/IGJhaWwoJ0V4cGVjdGVkIGZvby5iYXIuYmF6IHRvIGV4aXN0JylcbiAqIGBgYFxuICovXG5leHBvcnQgZnVuY3Rpb24gYmFpbChtZXNzYWdlOiBzdHJpbmcpOiBuZXZlciB7XG4gIHRocm93IG5ldyBFcnJvcihtZXNzYWdlKVxufVxuIiwgImNvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9kaXJuYW1lID0gXCJDOlxcXFxQcm9qZWN0c1xcXFxlbnNvXFxcXGVuc29cXFxcYXBwXFxcXGd1aTJcXFxcc2hhcmVkXFxcXHV0aWxcIjtjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfZmlsZW5hbWUgPSBcIkM6XFxcXFByb2plY3RzXFxcXGVuc29cXFxcZW5zb1xcXFxhcHBcXFxcZ3VpMlxcXFxzaGFyZWRcXFxcdXRpbFxcXFxkZXRlY3QudHNcIjtjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfaW1wb3J0X21ldGFfdXJsID0gXCJmaWxlOi8vL0M6L1Byb2plY3RzL2Vuc28vZW5zby9hcHAvZ3VpMi9zaGFyZWQvdXRpbC9kZXRlY3QudHNcIjsvLyBFbnZpcm9ubWVudCBkZXRlY3Rpb25cblxuLy8gTm9ybWFsbHkgYGdsb2JhbGAgaXMgbm9kZS1zcGVjaWZpYywgYnV0IGEgd29ya2Fyb3VuZCByZXF1aXJlcyBgZ2xvYmFsYCB0byBhbHNvIGV4aXN0XG4vLyBpbiB0aGUgYnJvd3NlciBmb3IgQW1wbGlmeSB0byB3b3JrLlxuZXhwb3J0IGNvbnN0IGlzTm9kZSA9XG4gIHR5cGVvZiBnbG9iYWwgIT09ICd1bmRlZmluZWQnICYmIChnbG9iYWwgYXMgYW55KVtTeW1ib2wudG9TdHJpbmdUYWddID09PSAnZ2xvYmFsJ1xuXG5leHBvcnQgY29uc3QgaXNEZXZNb2RlID0gcHJvY2Vzcy5lbnYuTk9ERV9FTlYgPT09ICdkZXZlbG9wbWVudCdcbiIsICJjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfZGlybmFtZSA9IFwiQzpcXFxcUHJvamVjdHNcXFxcZW5zb1xcXFxlbnNvXFxcXGFwcFxcXFxndWkyXFxcXHlkb2Mtc2VydmVyXCI7Y29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2ZpbGVuYW1lID0gXCJDOlxcXFxQcm9qZWN0c1xcXFxlbnNvXFxcXGVuc29cXFxcYXBwXFxcXGd1aTJcXFxceWRvYy1zZXJ2ZXJcXFxceWRvYy50c1wiO2NvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9pbXBvcnRfbWV0YV91cmwgPSBcImZpbGU6Ly8vQzovUHJvamVjdHMvZW5zby9lbnNvL2FwcC9ndWkyL3lkb2Mtc2VydmVyL3lkb2MudHNcIjtpbXBvcnQge1xuICBhcHBseUF3YXJlbmVzc1VwZGF0ZSxcbiAgQXdhcmVuZXNzLFxuICBlbmNvZGVBd2FyZW5lc3NVcGRhdGUsXG4gIHJlbW92ZUF3YXJlbmVzc1N0YXRlcyxcbn0gZnJvbSAneS1wcm90b2NvbHMvYXdhcmVuZXNzJ1xuaW1wb3J0IHsgcmVhZFN5bmNNZXNzYWdlLCB3cml0ZVN5bmNTdGVwMSwgd3JpdGVVcGRhdGUgfSBmcm9tICd5LXByb3RvY29scy9zeW5jJ1xuaW1wb3J0ICogYXMgWSBmcm9tICd5anMnXG5cbmltcG9ydCAqIGFzIGRlY29kaW5nIGZyb20gJ2xpYjAvZGVjb2RpbmcnXG5pbXBvcnQgKiBhcyBlbmNvZGluZyBmcm9tICdsaWIwL2VuY29kaW5nJ1xuaW1wb3J0IHsgT2JzZXJ2YWJsZVYyIH0gZnJvbSAnbGliMC9vYnNlcnZhYmxlJ1xuaW1wb3J0IHsgV2ViU29ja2V0IH0gZnJvbSAnd3MnXG5pbXBvcnQgeyBMYW5ndWFnZVNlcnZlclNlc3Npb24gfSBmcm9tICcuL2xhbmd1YWdlU2VydmVyU2Vzc2lvbidcblxuY29uc3QgcGluZ1RpbWVvdXQgPSAzMDAwMFxuXG5jb25zdCBtZXNzYWdlU3luYyA9IDBcbmNvbnN0IG1lc3NhZ2VBd2FyZW5lc3MgPSAxXG5cbmludGVyZmFjZSBBd2FyZW5lc3NVcGRhdGUge1xuICBhZGRlZDogbnVtYmVyW11cbiAgdXBkYXRlZDogbnVtYmVyW11cbiAgcmVtb3ZlZDogbnVtYmVyW11cbn1cblxudHlwZSBDb25uZWN0aW9uSWQgPSBZanNDb25uZWN0aW9uIHwgc3RyaW5nXG5cbi8qKlxuICogQSBZanMgZG9jdW1lbnQgdGhhdCBpcyBzaGFyZWQgb3ZlciBtdWx0aXBsZSB3ZWJzb2NrZXQgY29ubmVjdGlvbnMuXG4gKi9cbmV4cG9ydCBjbGFzcyBXU1NoYXJlZERvYyB7XG4gIGRvYzogWS5Eb2NcbiAgLyoqXG4gICAqIE1hcHMgZnJvbSBjb25uZWN0aW9uIGlkIHRvIHNldCBvZiBjb250cm9sbGVkIHVzZXIgaWRzLlxuICAgKiBEZWxldGUgYWxsIHVzZXIgaWRzIGZyb20gYXdhcmVuZXNzIHdoZW4gdGhpcyBjb25uIGlzIGNsb3NlZC5cbiAgICovXG4gIGNvbm5zOiBNYXA8Q29ubmVjdGlvbklkLCBTZXQ8bnVtYmVyPj5cbiAgYXdhcmVuZXNzOiBBd2FyZW5lc3NcblxuICBjb25zdHJ1Y3RvcihnYyA9IHRydWUpIHtcbiAgICB0aGlzLmRvYyA9IG5ldyBZLkRvYyh7IGdjIH0pXG4gICAgLy8gdGhpcy5uYW1lID0gbmFtZVxuICAgIHRoaXMuY29ubnMgPSBuZXcgTWFwKClcblxuICAgIHRoaXMuYXdhcmVuZXNzID0gbmV3IEF3YXJlbmVzcyh0aGlzLmRvYylcbiAgICB0aGlzLmF3YXJlbmVzcy5zZXRMb2NhbFN0YXRlKG51bGwpXG5cbiAgICB0aGlzLmF3YXJlbmVzcy5vbihcbiAgICAgICd1cGRhdGUnLFxuICAgICAgKHsgYWRkZWQsIHVwZGF0ZWQsIHJlbW92ZWQgfTogQXdhcmVuZXNzVXBkYXRlLCBjb25uOiBDb25uZWN0aW9uSWQgfCBudWxsKSA9PiB7XG4gICAgICAgIGNvbnN0IGNoYW5nZWRDbGllbnRzID0gYWRkZWQuY29uY2F0KHVwZGF0ZWQsIHJlbW92ZWQpXG4gICAgICAgIGlmIChjb25uICE9PSBudWxsKSB7XG4gICAgICAgICAgY29uc3QgY29ubkNvbnRyb2xsZWRJRHMgPSB0aGlzLmNvbm5zLmdldChjb25uKVxuICAgICAgICAgIGlmIChjb25uQ29udHJvbGxlZElEcyAhPT0gdW5kZWZpbmVkKSB7XG4gICAgICAgICAgICBmb3IgKGNvbnN0IGNsaWVudElEIG9mIGFkZGVkKSBjb25uQ29udHJvbGxlZElEcy5hZGQoY2xpZW50SUQpXG4gICAgICAgICAgICBmb3IgKGNvbnN0IGNsaWVudElEIG9mIHJlbW92ZWQpIGNvbm5Db250cm9sbGVkSURzLmRlbGV0ZShjbGllbnRJRClcbiAgICAgICAgICB9XG4gICAgICAgIH1cbiAgICAgICAgLy8gYnJvYWRjYXN0IGF3YXJlbmVzcyB1cGRhdGVcbiAgICAgICAgY29uc3QgZW5jb2RlciA9IGVuY29kaW5nLmNyZWF0ZUVuY29kZXIoKVxuICAgICAgICBlbmNvZGluZy53cml0ZVZhclVpbnQoZW5jb2RlciwgbWVzc2FnZUF3YXJlbmVzcylcbiAgICAgICAgY29uc3QgdXBkYXRlID0gZW5jb2RlQXdhcmVuZXNzVXBkYXRlKHRoaXMuYXdhcmVuZXNzLCBjaGFuZ2VkQ2xpZW50cylcbiAgICAgICAgZW5jb2Rpbmcud3JpdGVWYXJVaW50OEFycmF5KGVuY29kZXIsIHVwZGF0ZSlcbiAgICAgICAgdGhpcy5icm9hZGNhc3QoZW5jb2RpbmcudG9VaW50OEFycmF5KGVuY29kZXIpKVxuICAgICAgfSxcbiAgICApXG4gICAgdGhpcy5kb2Mub24oJ3VwZGF0ZScsICh1cGRhdGUsIG9yaWdpbikgPT4gdGhpcy51cGRhdGVIYW5kbGVyKHVwZGF0ZSwgb3JpZ2luKSlcbiAgfVxuXG4gIGJyb2FkY2FzdChtZXNzYWdlOiBVaW50OEFycmF5KSB7XG4gICAgZm9yIChjb25zdCBbY29ubl0gb2YgdGhpcy5jb25ucykge1xuICAgICAgaWYgKHR5cGVvZiBjb25uICE9PSAnc3RyaW5nJykgY29ubi5zZW5kKG1lc3NhZ2UpXG4gICAgfVxuICB9XG5cbiAgdXBkYXRlSGFuZGxlcih1cGRhdGU6IFVpbnQ4QXJyYXksIF9vcmlnaW46IGFueSkge1xuICAgIGNvbnN0IGVuY29kZXIgPSBlbmNvZGluZy5jcmVhdGVFbmNvZGVyKClcbiAgICBlbmNvZGluZy53cml0ZVZhclVpbnQoZW5jb2RlciwgbWVzc2FnZVN5bmMpXG4gICAgd3JpdGVVcGRhdGUoZW5jb2RlciwgdXBkYXRlKVxuICAgIHRoaXMuYnJvYWRjYXN0KGVuY29kaW5nLnRvVWludDhBcnJheShlbmNvZGVyKSlcbiAgfVxufVxuXG4vKipcbiAqIEhhbmRsZSBzZXJ2aWNpbmcgaW5jb21pbmcgV2ViU29ja2V0IGNvbm5lY3Rpb24gbGlzdGVuaW5nIGZvciBnaXZlbiBkb2N1bWVudCB1cGRhdGVzLlxuICogQHBhcmFtIHdzIFRoZSBuZXdseSBjb25uZWN0ZWQgV2ViU29ja2V0IHJlcXVlc3RpbmcgWWpzIGRvY3VtZW50IHN5bmNocm9uaXphdGlvblxuICogQHBhcmFtIGxzVXJsIEFkZHJlc3Mgb2YgdGhlIGxhbmd1YWdlIHNlcnZlciB0byBjb25uZWN0IHRvLiBFYWNoIHVuaXF1ZSBsYW5ndWFnZSBzZXJ2ZXIgYWRkcmVzc1xuICogd2lsbCBiZSBhc3NpZ25lZCBpdHMgb3duIGBEaXN0cmlidXRlZFByb2plY3RgIGluc3RhbmNlIHdpdGggYSB1bmlxdWUgbmFtZXNwYWNlIG9mIFlqcyBkb2N1bWVudHMuXG4gKiBAcGFyYW0gZG9jTmFtZSBUaGUgbmFtZSBvZiB0aGUgZG9jdW1lbnQgdG8gc3luY2hyb25pemUuIFdoZW4gdGhlIGRvY3VtZW50IG5hbWUgaXMgYGluZGV4YCwgdGhlXG4gKiBkb2N1bWVudCBpcyBjb25zaWRlcmVkIHRvIGJlIHRoZSByb290IGRvY3VtZW50IG9mIHRoZSBgRGlzdHJpYnV0ZWRQcm9qZWN0YCBkYXRhIG1vZGVsLlxuICovXG5leHBvcnQgZnVuY3Rpb24gc2V0dXBHYXRld2F5Q2xpZW50KHdzOiBXZWJTb2NrZXQsIGxzVXJsOiBzdHJpbmcsIGRvY05hbWU6IHN0cmluZykge1xuICBjb25zdCBsc1Nlc3Npb24gPSBMYW5ndWFnZVNlcnZlclNlc3Npb24uZ2V0KGxzVXJsKVxuICBjb25zdCB3c0RvYyA9IGxzU2Vzc2lvbi5nZXRZRG9jKGRvY05hbWUpXG4gIGlmICh3c0RvYyA9PSBudWxsKSB7XG4gICAgY29uc29sZS5lcnJvcihgRG9jdW1lbnQgJyR7ZG9jTmFtZX0nIG5vdCBmb3VuZCBpbiBsYW5ndWFnZSBzZXJ2ZXIgc2Vzc2lvbiAnJHtsc1VybH0nLmApXG4gICAgd3MuY2xvc2UoKVxuICAgIHJldHVyblxuICB9XG4gIGNvbnN0IGNvbm5lY3Rpb24gPSBuZXcgWWpzQ29ubmVjdGlvbih3cywgd3NEb2MpXG4gIGNvbm5lY3Rpb24ub25jZSgnY2xvc2UnLCBhc3luYyAoKSA9PiB7XG4gICAgdHJ5IHtcbiAgICAgIGF3YWl0IGxzU2Vzc2lvbi5yZWxlYXNlKClcbiAgICB9IGNhdGNoIChlcnJvcikge1xuICAgICAgY29uc29sZS5lcnJvcignU2Vzc2lvbiByZWxlYXNlIGZhaWxlZC5cXG4nLCBlcnJvcilcbiAgICB9XG4gIH0pXG59XG5cbmNsYXNzIFlqc0Nvbm5lY3Rpb24gZXh0ZW5kcyBPYnNlcnZhYmxlVjI8eyBjbG9zZSgpOiB2b2lkIH0+IHtcbiAgd3M6IFdlYlNvY2tldFxuICB3c0RvYzogV1NTaGFyZWREb2NcbiAgY29uc3RydWN0b3Iod3M6IFdlYlNvY2tldCwgd3NEb2M6IFdTU2hhcmVkRG9jKSB7XG4gICAgc3VwZXIoKVxuICAgIHRoaXMud3MgPSB3c1xuICAgIHRoaXMud3NEb2MgPSB3c0RvY1xuICAgIGNvbnN0IGlzTG9hZGVkID0gd3NEb2MuY29ubnMuc2l6ZSA+IDBcbiAgICB3c0RvYy5jb25ucy5zZXQodGhpcywgbmV3IFNldCgpKVxuICAgIHdzLmJpbmFyeVR5cGUgPSAnYXJyYXlidWZmZXInXG4gICAgd3Mub24oJ21lc3NhZ2UnLCAobWVzc2FnZTogQXJyYXlCdWZmZXIpID0+IHRoaXMubWVzc2FnZUxpc3RlbmVyKG5ldyBVaW50OEFycmF5KG1lc3NhZ2UpKSlcbiAgICB3cy5vbignY2xvc2UnLCAoKSA9PiB0aGlzLmNsb3NlKCkpXG4gICAgaWYgKCFpc0xvYWRlZCkgd3NEb2MuZG9jLmxvYWQoKVxuICAgIHRoaXMuaW5pdFBpbmcoKVxuICAgIHRoaXMuc2VuZFN5bmNNZXNzYWdlKClcbiAgfVxuXG4gIHByaXZhdGUgaW5pdFBpbmcoKSB7XG4gICAgLy8gQ2hlY2sgaWYgY29ubmVjdGlvbiBpcyBzdGlsbCBhbGl2ZVxuICAgIGxldCBwb25nUmVjZWl2ZWQgPSB0cnVlXG4gICAgY29uc3QgcGluZ0ludGVydmFsID0gc2V0SW50ZXJ2YWwoKCkgPT4ge1xuICAgICAgaWYgKCFwb25nUmVjZWl2ZWQpIHtcbiAgICAgICAgaWYgKHRoaXMud3NEb2MuY29ubnMuaGFzKHRoaXMpKSB0aGlzLmNsb3NlKClcbiAgICAgICAgY2xlYXJJbnRlcnZhbChwaW5nSW50ZXJ2YWwpXG4gICAgICB9IGVsc2UgaWYgKHRoaXMud3NEb2MuY29ubnMuaGFzKHRoaXMpKSB7XG4gICAgICAgIHBvbmdSZWNlaXZlZCA9IGZhbHNlXG4gICAgICAgIHRyeSB7XG4gICAgICAgICAgdGhpcy53cy5waW5nKClcbiAgICAgICAgfSBjYXRjaCAoZXJyb3IpIHtcbiAgICAgICAgICBjb25zb2xlLmVycm9yKCdFcnJvciBzZW5kaW5nIHBpbmc6JywgZXJyb3IpXG4gICAgICAgICAgdGhpcy5jbG9zZSgpXG4gICAgICAgICAgY2xlYXJJbnRlcnZhbChwaW5nSW50ZXJ2YWwpXG4gICAgICAgIH1cbiAgICAgIH1cbiAgICB9LCBwaW5nVGltZW91dClcbiAgICB0aGlzLndzLm9uKCdjbG9zZScsICgpID0+IGNsZWFySW50ZXJ2YWwocGluZ0ludGVydmFsKSlcbiAgICB0aGlzLndzLm9uKCdwb25nJywgKCkgPT4gKHBvbmdSZWNlaXZlZCA9IHRydWUpKVxuICB9XG5cbiAgc2VuZFN5bmNNZXNzYWdlKCkge1xuICAgIGNvbnN0IGVuY29kZXIgPSBlbmNvZGluZy5jcmVhdGVFbmNvZGVyKClcbiAgICBlbmNvZGluZy53cml0ZVZhclVpbnQoZW5jb2RlciwgbWVzc2FnZVN5bmMpXG4gICAgd3JpdGVTeW5jU3RlcDEoZW5jb2RlciwgdGhpcy53c0RvYy5kb2MpXG4gICAgdGhpcy5zZW5kKGVuY29kaW5nLnRvVWludDhBcnJheShlbmNvZGVyKSlcbiAgICBjb25zdCBhd2FyZW5lc3NTdGF0ZXMgPSB0aGlzLndzRG9jLmF3YXJlbmVzcy5nZXRTdGF0ZXMoKVxuICAgIGlmIChhd2FyZW5lc3NTdGF0ZXMuc2l6ZSA+IDApIHtcbiAgICAgIGNvbnN0IGVuY29kZXIgPSBlbmNvZGluZy5jcmVhdGVFbmNvZGVyKClcbiAgICAgIGVuY29kaW5nLndyaXRlVmFyVWludChlbmNvZGVyLCBtZXNzYWdlQXdhcmVuZXNzKVxuICAgICAgZW5jb2Rpbmcud3JpdGVWYXJVaW50OEFycmF5KFxuICAgICAgICBlbmNvZGVyLFxuICAgICAgICBlbmNvZGVBd2FyZW5lc3NVcGRhdGUodGhpcy53c0RvYy5hd2FyZW5lc3MsIEFycmF5LmZyb20oYXdhcmVuZXNzU3RhdGVzLmtleXMoKSkpLFxuICAgICAgKVxuICAgICAgdGhpcy5zZW5kKGVuY29kaW5nLnRvVWludDhBcnJheShlbmNvZGVyKSlcbiAgICB9XG4gIH1cblxuICBzZW5kKG1lc3NhZ2U6IFVpbnQ4QXJyYXkpIHtcbiAgICBpZiAodGhpcy53cy5yZWFkeVN0YXRlICE9PSBXZWJTb2NrZXQuQ09OTkVDVElORyAmJiB0aGlzLndzLnJlYWR5U3RhdGUgIT09IFdlYlNvY2tldC5PUEVOKSB7XG4gICAgICB0aGlzLmNsb3NlKClcbiAgICB9XG4gICAgdHJ5IHtcbiAgICAgIHRoaXMud3Muc2VuZChtZXNzYWdlLCAoZXJyb3IpID0+IGVycm9yICYmIHRoaXMuY2xvc2UoKSlcbiAgICB9IGNhdGNoIChlKSB7XG4gICAgICB0aGlzLmNsb3NlKClcbiAgICB9XG4gIH1cblxuICBtZXNzYWdlTGlzdGVuZXIobWVzc2FnZTogVWludDhBcnJheSkge1xuICAgIHRyeSB7XG4gICAgICBjb25zdCBlbmNvZGVyID0gZW5jb2RpbmcuY3JlYXRlRW5jb2RlcigpXG4gICAgICBjb25zdCBkZWNvZGVyID0gZGVjb2RpbmcuY3JlYXRlRGVjb2RlcihtZXNzYWdlKVxuICAgICAgY29uc3QgbWVzc2FnZVR5cGUgPSBkZWNvZGluZy5yZWFkVmFyVWludChkZWNvZGVyKVxuICAgICAgc3dpdGNoIChtZXNzYWdlVHlwZSkge1xuICAgICAgICBjYXNlIG1lc3NhZ2VTeW5jOiB7XG4gICAgICAgICAgZW5jb2Rpbmcud3JpdGVWYXJVaW50KGVuY29kZXIsIG1lc3NhZ2VTeW5jKVxuICAgICAgICAgIHJlYWRTeW5jTWVzc2FnZShkZWNvZGVyLCBlbmNvZGVyLCB0aGlzLndzRG9jLmRvYywgdGhpcylcbiAgICAgICAgICAvLyBJZiB0aGUgYGVuY29kZXJgIG9ubHkgY29udGFpbnMgdGhlIHR5cGUgb2YgcmVwbHkgbWVzc2FnZSBhbmQgbm9cbiAgICAgICAgICAvLyBtZXNzYWdlLCB0aGVyZSBpcyBubyBuZWVkIHRvIHNlbmQgdGhlIG1lc3NhZ2UuIFdoZW4gYGVuY29kZXJgIG9ubHlcbiAgICAgICAgICAvLyBjb250YWlucyB0aGUgdHlwZSBvZiByZXBseSwgaXRzIGxlbmd0aCBpcyAxLlxuICAgICAgICAgIGlmIChlbmNvZGluZy5sZW5ndGgoZW5jb2RlcikgPiAxKSB7XG4gICAgICAgICAgICB0aGlzLnNlbmQoZW5jb2RpbmcudG9VaW50OEFycmF5KGVuY29kZXIpKVxuICAgICAgICAgIH1cbiAgICAgICAgICBicmVha1xuICAgICAgICB9XG4gICAgICAgIGNhc2UgbWVzc2FnZUF3YXJlbmVzczoge1xuICAgICAgICAgIGNvbnN0IHVwZGF0ZSA9IGRlY29kaW5nLnJlYWRWYXJVaW50OEFycmF5KGRlY29kZXIpXG4gICAgICAgICAgYXBwbHlBd2FyZW5lc3NVcGRhdGUodGhpcy53c0RvYy5hd2FyZW5lc3MsIHVwZGF0ZSwgdGhpcylcbiAgICAgICAgICBicmVha1xuICAgICAgICB9XG4gICAgICB9XG4gICAgfSBjYXRjaCAoZXJyKSB7XG4gICAgICBjb25zb2xlLmVycm9yKGVycilcbiAgICAgIHRoaXMud3NEb2MuZG9jLmVtaXQoJ2Vycm9yJywgW2Vycl0pXG4gICAgfVxuICB9XG5cbiAgY2xvc2UoKSB7XG4gICAgY29uc3QgY29udHJvbGxlZElkcyA9IHRoaXMud3NEb2MuY29ubnMuZ2V0KHRoaXMpXG4gICAgdGhpcy53c0RvYy5jb25ucy5kZWxldGUodGhpcylcbiAgICBpZiAoY29udHJvbGxlZElkcyAhPSBudWxsKSB7XG4gICAgICByZW1vdmVBd2FyZW5lc3NTdGF0ZXModGhpcy53c0RvYy5hd2FyZW5lc3MsIEFycmF5LmZyb20oY29udHJvbGxlZElkcyksIG51bGwpXG4gICAgfVxuICAgIHRoaXMud3MuY2xvc2UoKVxuICAgIHRoaXMuZW1pdCgnY2xvc2UnLCBbXSlcbiAgICBpZiAodGhpcy53c0RvYy5jb25ucy5zaXplID09PSAwKSB7XG4gICAgICB0aGlzLndzRG9jLmRvYy5lbWl0KCd1bmxvYWQnLCBbXSlcbiAgICB9XG4gIH1cbn1cbiIsICJjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfZGlybmFtZSA9IFwiQzpcXFxcUHJvamVjdHNcXFxcZW5zb1xcXFxlbnNvXFxcXGFwcFxcXFxndWkyXFxcXHlkb2Mtc2VydmVyXCI7Y29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2ZpbGVuYW1lID0gXCJDOlxcXFxQcm9qZWN0c1xcXFxlbnNvXFxcXGVuc29cXFxcYXBwXFxcXGd1aTJcXFxceWRvYy1zZXJ2ZXJcXFxcbGFuZ3VhZ2VTZXJ2ZXJTZXNzaW9uLnRzXCI7Y29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2ltcG9ydF9tZXRhX3VybCA9IFwiZmlsZTovLy9DOi9Qcm9qZWN0cy9lbnNvL2Vuc28vYXBwL2d1aTIveWRvYy1zZXJ2ZXIvbGFuZ3VhZ2VTZXJ2ZXJTZXNzaW9uLnRzXCI7aW1wb3J0IHsgQ2xpZW50LCBSZXF1ZXN0TWFuYWdlciwgV2ViU29ja2V0VHJhbnNwb3J0IH0gZnJvbSAnQG9wZW4tcnBjL2NsaWVudC1qcydcbmltcG9ydCAqIGFzIGpzb24gZnJvbSAnbGliMC9qc29uJ1xuaW1wb3J0ICogYXMgbWFwIGZyb20gJ2xpYjAvbWFwJ1xuaW1wb3J0IHsgT2JzZXJ2YWJsZVYyIH0gZnJvbSAnbGliMC9vYnNlcnZhYmxlJ1xuaW1wb3J0ICogYXMgcmFuZG9tIGZyb20gJ2xpYjAvcmFuZG9tJ1xuaW1wb3J0ICogYXMgWSBmcm9tICd5anMnXG5pbXBvcnQgKiBhcyBBc3QgZnJvbSAnLi4vc2hhcmVkL2FzdCdcbmltcG9ydCB7IGFzdENvdW50IH0gZnJvbSAnLi4vc2hhcmVkL2FzdCdcbmltcG9ydCB7IEVuc29GaWxlUGFydHMsIGNvbWJpbmVGaWxlUGFydHMsIHNwbGl0RmlsZUNvbnRlbnRzIH0gZnJvbSAnLi4vc2hhcmVkL2Vuc29GaWxlJ1xuaW1wb3J0IHsgTGFuZ3VhZ2VTZXJ2ZXIsIGNvbXB1dGVUZXh0Q2hlY2tzdW0gfSBmcm9tICcuLi9zaGFyZWQvbGFuZ3VhZ2VTZXJ2ZXInXG5pbXBvcnQgeyBDaGVja3N1bSwgRmlsZUVkaXQsIFBhdGgsIFRleHRFZGl0LCByZXNwb25zZSB9IGZyb20gJy4uL3NoYXJlZC9sYW5ndWFnZVNlcnZlclR5cGVzJ1xuaW1wb3J0IHsgZXhwb25lbnRpYWxCYWNrb2ZmLCBwcmludGluZ0NhbGxiYWNrcyB9IGZyb20gJy4uL3NoYXJlZC9yZXRyeSdcbmltcG9ydCB7IEFib3J0U2NvcGUgfSBmcm9tICcuLi9zaGFyZWQvdXRpbC9uZXQnXG5pbXBvcnQge1xuICBEaXN0cmlidXRlZFByb2plY3QsXG4gIEV4dGVybmFsSWQsXG4gIElkTWFwLFxuICBNb2R1bGVEb2MsXG4gIHZpc01ldGFkYXRhRXF1YWxzLFxuICB0eXBlIFV1aWQsXG59IGZyb20gJy4uL3NoYXJlZC95anNNb2RlbCdcbmltcG9ydCB7XG4gIGFwcGx5RGlmZkFzVGV4dEVkaXRzLFxuICBhcHBseURvY3VtZW50VXBkYXRlcyxcbiAgcHJldHR5UHJpbnREaWZmLFxuICB0cmFuc2xhdGVWaXN1YWxpemF0aW9uRnJvbUZpbGUsXG59IGZyb20gJy4vZWRpdHMnXG5pbXBvcnQgKiBhcyBmaWxlRm9ybWF0IGZyb20gJy4vZmlsZUZvcm1hdCdcbmltcG9ydCB7IGRlc2VyaWFsaXplSWRNYXAsIHNlcmlhbGl6ZUlkTWFwIH0gZnJvbSAnLi9zZXJpYWxpemF0aW9uJ1xuaW1wb3J0IHsgV1NTaGFyZWREb2MgfSBmcm9tICcuL3lkb2MnXG5cbmNvbnN0IFNPVVJDRV9ESVIgPSAnc3JjJ1xuY29uc3QgRVhURU5TSU9OID0gJy5lbnNvJ1xuXG5jb25zdCBERUJVR19MT0dfU1lOQyA9IGZhbHNlXG5cbmZ1bmN0aW9uIGNyZWF0ZU9wZW5SUENDbGllbnQodXJsOiBzdHJpbmcpIHtcbiAgY29uc3QgdHJhbnNwb3J0ID0gbmV3IFdlYlNvY2tldFRyYW5zcG9ydCh1cmwpXG4gIGNvbnN0IHJlcXVlc3RNYW5hZ2VyID0gbmV3IFJlcXVlc3RNYW5hZ2VyKFt0cmFuc3BvcnRdKVxuICB0cmFuc3BvcnQuY29ubmVjdGlvbi5vbignZXJyb3InLCAoZXJyb3IpID0+XG4gICAgY29uc29sZS5lcnJvcignTGFuZ3VhZ2UgU2VydmVyIHRyYW5zcG9ydCBlcnJvcjonLCBlcnJvciksXG4gIClcbiAgcmV0dXJuIG5ldyBDbGllbnQocmVxdWVzdE1hbmFnZXIpXG59XG5cbmV4cG9ydCBjbGFzcyBMYW5ndWFnZVNlcnZlclNlc3Npb24ge1xuICBjbGllbnRJZDogVXVpZFxuICBpbmRleERvYzogV1NTaGFyZWREb2NcbiAgZG9jczogTWFwPHN0cmluZywgV1NTaGFyZWREb2M+XG4gIHJldGFpbkNvdW50OiBudW1iZXJcbiAgdXJsOiBzdHJpbmdcbiAgY2xpZW50OiBDbGllbnRcbiAgbHM6IExhbmd1YWdlU2VydmVyXG4gIGNvbm5lY3Rpb246IHJlc3BvbnNlLkluaXRQcm90b2NvbENvbm5lY3Rpb24gfCB1bmRlZmluZWRcbiAgbW9kZWw6IERpc3RyaWJ1dGVkUHJvamVjdFxuICBwcm9qZWN0Um9vdElkOiBVdWlkIHwgbnVsbFxuICBhdXRob3JpdGF0aXZlTW9kdWxlczogTWFwPHN0cmluZywgTW9kdWxlUGVyc2lzdGVuY2U+XG4gIGNsaWVudFNjb3BlOiBBYm9ydFNjb3BlXG5cbiAgY29uc3RydWN0b3IodXJsOiBzdHJpbmcpIHtcbiAgICB0aGlzLmNsaWVudFNjb3BlID0gbmV3IEFib3J0U2NvcGUoKVxuICAgIHRoaXMuY2xpZW50SWQgPSByYW5kb20udXVpZHY0KCkgYXMgVXVpZFxuICAgIHRoaXMuZG9jcyA9IG5ldyBNYXAoKVxuICAgIHRoaXMucmV0YWluQ291bnQgPSAwXG4gICAgdGhpcy51cmwgPSB1cmxcbiAgICBjb25zb2xlLmxvZygnbmV3IHNlc3Npb24gd2l0aCcsIHVybClcbiAgICB0aGlzLmluZGV4RG9jID0gbmV3IFdTU2hhcmVkRG9jKClcbiAgICB0aGlzLmRvY3Muc2V0KCdpbmRleCcsIHRoaXMuaW5kZXhEb2MpXG4gICAgdGhpcy5tb2RlbCA9IG5ldyBEaXN0cmlidXRlZFByb2plY3QodGhpcy5pbmRleERvYy5kb2MpXG4gICAgdGhpcy5wcm9qZWN0Um9vdElkID0gbnVsbFxuICAgIHRoaXMuYXV0aG9yaXRhdGl2ZU1vZHVsZXMgPSBuZXcgTWFwKClcblxuICAgIHRoaXMuaW5kZXhEb2MuZG9jLm9uKCdzdWJkb2NzJywgKHN1YmRvY3M6IHsgbG9hZGVkOiBTZXQ8WS5Eb2M+IH0pID0+IHtcbiAgICAgIGZvciAoY29uc3QgZG9jIG9mIHN1YmRvY3MubG9hZGVkKSB7XG4gICAgICAgIGNvbnN0IG5hbWUgPSB0aGlzLm1vZGVsLmZpbmRNb2R1bGVCeURvY0lkKGRvYy5ndWlkKVxuICAgICAgICBpZiAoIW5hbWUpIGNvbnRpbnVlXG4gICAgICAgIGNvbnN0IHBlcnNpc3RlbmNlID0gdGhpcy5hdXRob3JpdGF0aXZlTW9kdWxlcy5nZXQobmFtZSlcbiAgICAgICAgaWYgKCFwZXJzaXN0ZW5jZSkgY29udGludWVcbiAgICAgIH1cbiAgICB9KVxuICAgIGNvbnN0IHsgY2xpZW50LCBscyB9ID0gdGhpcy5zZXR1cENsaWVudCgpXG4gICAgdGhpcy5jbGllbnQgPSBjbGllbnRcbiAgICB0aGlzLmxzID0gbHNcbiAgfVxuXG4gIHN0YXRpYyBzZXNzaW9ucyA9IG5ldyBNYXA8c3RyaW5nLCBMYW5ndWFnZVNlcnZlclNlc3Npb24+KClcbiAgc3RhdGljIGdldCh1cmw6IHN0cmluZyk6IExhbmd1YWdlU2VydmVyU2Vzc2lvbiB7XG4gICAgY29uc3Qgc2Vzc2lvbiA9IG1hcC5zZXRJZlVuZGVmaW5lZChcbiAgICAgIExhbmd1YWdlU2VydmVyU2Vzc2lvbi5zZXNzaW9ucyxcbiAgICAgIHVybCxcbiAgICAgICgpID0+IG5ldyBMYW5ndWFnZVNlcnZlclNlc3Npb24odXJsKSxcbiAgICApXG4gICAgc2Vzc2lvbi5yZXRhaW4oKVxuICAgIHJldHVybiBzZXNzaW9uXG4gIH1cblxuICBwcml2YXRlIHJlc3RhcnRDbGllbnQoKSB7XG4gICAgdGhpcy5jbGllbnRTY29wZS5kaXNwb3NlKCdDbGllbnQgcmVzdGFydGVkLicpXG4gICAgdGhpcy5jbGllbnRTY29wZSA9IG5ldyBBYm9ydFNjb3BlKClcbiAgICB0aGlzLmNvbm5lY3Rpb24gPSB1bmRlZmluZWRcbiAgICB0aGlzLnNldHVwQ2xpZW50KClcbiAgfVxuXG4gIHByaXZhdGUgc2V0dXBDbGllbnQoKSB7XG4gICAgdGhpcy5jbGllbnQgPSBjcmVhdGVPcGVuUlBDQ2xpZW50KHRoaXMudXJsKVxuICAgIHRoaXMubHMgPSBuZXcgTGFuZ3VhZ2VTZXJ2ZXIodGhpcy5jbGllbnQpXG4gICAgdGhpcy5jbGllbnRTY29wZS5vbkFib3J0KCgpID0+IHRoaXMubHMucmVsZWFzZSgpKVxuICAgIHRoaXMubHMub24oJ2ZpbGUvZXZlbnQnLCBhc3luYyAoZXZlbnQpID0+IHtcbiAgICAgIGlmIChERUJVR19MT0dfU1lOQykge1xuICAgICAgICBjb25zb2xlLmxvZygnZmlsZS9ldmVudCcsIGV2ZW50KVxuICAgICAgfVxuICAgICAgY29uc3QgcGF0aCA9IGV2ZW50LnBhdGguc2VnbWVudHMuam9pbignLycpXG4gICAgICB0cnkge1xuICAgICAgICBzd2l0Y2ggKGV2ZW50LmtpbmQpIHtcbiAgICAgICAgICBjYXNlICdBZGRlZCc6IHtcbiAgICAgICAgICAgIGlmIChpc1NvdXJjZUZpbGUoZXZlbnQucGF0aCkpIHtcbiAgICAgICAgICAgICAgY29uc3QgZmlsZUluZm8gPSBhd2FpdCB0aGlzLmxzLmZpbGVJbmZvKGV2ZW50LnBhdGgpXG4gICAgICAgICAgICAgIGlmIChmaWxlSW5mby5hdHRyaWJ1dGVzLmtpbmQudHlwZSA9PSAnRmlsZScpIHtcbiAgICAgICAgICAgICAgICBhd2FpdCBleHBvbmVudGlhbEJhY2tvZmYoXG4gICAgICAgICAgICAgICAgICAoKSA9PiB0aGlzLmdldE1vZHVsZU1vZGVsKGV2ZW50LnBhdGgpLm9wZW4oKSxcbiAgICAgICAgICAgICAgICAgIHByaW50aW5nQ2FsbGJhY2tzKGBvcGVuZWQgbmV3IGZpbGUgJyR7cGF0aH0nYCwgYG9wZW4gbmV3IGZpbGUgJyR7cGF0aH0nYCksXG4gICAgICAgICAgICAgICAgKVxuICAgICAgICAgICAgICB9XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICBicmVha1xuICAgICAgICAgIH1cbiAgICAgICAgICBjYXNlICdNb2RpZmllZCc6IHtcbiAgICAgICAgICAgIGF3YWl0IGV4cG9uZW50aWFsQmFja29mZihcbiAgICAgICAgICAgICAgYXN5bmMgKCkgPT4gdGhpcy50cnlHZXRFeGlzdGluZ01vZHVsZU1vZGVsKGV2ZW50LnBhdGgpPy5yZWxvYWQoKSxcbiAgICAgICAgICAgICAgcHJpbnRpbmdDYWxsYmFja3MoYHJlbG9hZGVkIGZpbGUgJyR7cGF0aH0nYCwgYHJlbG9hZCBmaWxlICcke3BhdGh9J2ApLFxuICAgICAgICAgICAgKVxuICAgICAgICAgICAgYnJlYWtcbiAgICAgICAgICB9XG4gICAgICAgIH1cbiAgICAgIH0gY2F0Y2gge1xuICAgICAgICB0aGlzLnJlc3RhcnRDbGllbnQoKVxuICAgICAgfVxuICAgIH0pXG4gICAgdGhpcy5scy5vbigndGV4dC9maWxlTW9kaWZpZWRPbkRpc2snLCBhc3luYyAoZXZlbnQpID0+IHtcbiAgICAgIGNvbnN0IHBhdGggPSBldmVudC5wYXRoLnNlZ21lbnRzLmpvaW4oJy8nKVxuICAgICAgdHJ5IHtcbiAgICAgICAgYXdhaXQgZXhwb25lbnRpYWxCYWNrb2ZmKFxuICAgICAgICAgIGFzeW5jICgpID0+IHRoaXMudHJ5R2V0RXhpc3RpbmdNb2R1bGVNb2RlbChldmVudC5wYXRoKT8ucmVsb2FkKCksXG4gICAgICAgICAgcHJpbnRpbmdDYWxsYmFja3MoYHJlbG9hZGVkIGZpbGUgJyR7cGF0aH0nYCwgYHJlbG9hZCBmaWxlICcke3BhdGh9J2ApLFxuICAgICAgICApXG4gICAgICB9IGNhdGNoIHtcbiAgICAgICAgdGhpcy5yZXN0YXJ0Q2xpZW50KClcbiAgICAgIH1cbiAgICB9KVxuICAgIGV4cG9uZW50aWFsQmFja29mZihcbiAgICAgICgpID0+IHRoaXMucmVhZEluaXRpYWxTdGF0ZSgpLFxuICAgICAgcHJpbnRpbmdDYWxsYmFja3MoJ3JlYWQgaW5pdGlhbCBzdGF0ZScsICdyZWFkIGluaXRpYWwgc3RhdGUnKSxcbiAgICApLmNhdGNoKChlcnJvcikgPT4ge1xuICAgICAgY29uc29sZS5lcnJvcignQ291bGQgbm90IHJlYWQgaW5pdGlhbCBzdGF0ZS4nKVxuICAgICAgY29uc29sZS5lcnJvcihlcnJvcilcbiAgICAgIGV4cG9uZW50aWFsQmFja29mZihcbiAgICAgICAgYXN5bmMgKCkgPT4gdGhpcy5yZXN0YXJ0Q2xpZW50KCksXG4gICAgICAgIHByaW50aW5nQ2FsbGJhY2tzKCdyZXN0YXJ0ZWQgUlBDIGNsaWVudCcsICdyZXN0YXJ0IFJQQyBjbGllbnQnKSxcbiAgICAgIClcbiAgICB9KVxuICAgIHJldHVybiB7IGNsaWVudDogdGhpcy5jbGllbnQsIGxzOiB0aGlzLmxzIH1cbiAgfVxuXG4gIHByaXZhdGUgYXNzZXJ0UHJvamVjdFJvb3QoKTogYXNzZXJ0cyB0aGlzIGlzIHsgcHJvamVjdFJvb3RJZDogVXVpZCB9IHtcbiAgICBpZiAodGhpcy5wcm9qZWN0Um9vdElkID09IG51bGwpIHRocm93IG5ldyBFcnJvcignTWlzc2luZyBwcm9qZWN0IHJvb3QnKVxuICB9XG5cbiAgcHJpdmF0ZSBhc3luYyByZWFkSW5pdGlhbFN0YXRlKCkge1xuICAgIGxldCBtb2R1bGVPcGVuUHJvbWlzZXM6IFByb21pc2U8dm9pZD5bXSA9IFtdXG4gICAgdHJ5IHtcbiAgICAgIGNvbnN0IGNvbm5lY3Rpb24gPSB0aGlzLmNvbm5lY3Rpb24gPz8gKGF3YWl0IHRoaXMubHMuaW5pdFByb3RvY29sQ29ubmVjdGlvbih0aGlzLmNsaWVudElkKSlcbiAgICAgIHRoaXMuY29ubmVjdGlvbiA9IGNvbm5lY3Rpb25cbiAgICAgIGNvbnN0IHByb2plY3RSb290ID0gY29ubmVjdGlvbi5jb250ZW50Um9vdHMuZmluZCgocm9vdCkgPT4gcm9vdC50eXBlID09PSAnUHJvamVjdCcpXG4gICAgICBpZiAoIXByb2plY3RSb290KSB0aHJvdyBuZXcgRXJyb3IoJ01pc3NpbmcgcHJvamVjdCByb290JylcbiAgICAgIHRoaXMucHJvamVjdFJvb3RJZCA9IHByb2plY3RSb290LmlkXG4gICAgICBhd2FpdCB0aGlzLmxzLmFjcXVpcmVSZWNlaXZlc1RyZWVVcGRhdGVzKHsgcm9vdElkOiB0aGlzLnByb2plY3RSb290SWQsIHNlZ21lbnRzOiBbXSB9KVxuICAgICAgY29uc3QgZmlsZXMgPSBhd2FpdCB0aGlzLnNjYW5Tb3VyY2VGaWxlcygpXG4gICAgICBtb2R1bGVPcGVuUHJvbWlzZXMgPSB0aGlzLmluZGV4RG9jLmRvYy50cmFuc2FjdChcbiAgICAgICAgKCkgPT5cbiAgICAgICAgICBmaWxlcy5tYXAoKGZpbGUpID0+IHRoaXMuZ2V0TW9kdWxlTW9kZWwocHVzaFBhdGhTZWdtZW50KGZpbGUucGF0aCwgZmlsZS5uYW1lKSkub3BlbigpKSxcbiAgICAgICAgdGhpcyxcbiAgICAgIClcbiAgICAgIGF3YWl0IFByb21pc2UuYWxsKG1vZHVsZU9wZW5Qcm9taXNlcylcbiAgICB9IGNhdGNoIChlcnJvcikge1xuICAgICAgY29uc29sZS5lcnJvcignTFMgaW5pdGlhbGl6YXRpb24gZmFpbGVkLicpXG4gICAgICB0aHJvdyBlcnJvclxuICAgIH1cbiAgICBjb25zb2xlLmxvZygnTFMgY29ubmVjdGlvbiBpbml0aWFsaXplZC4nKVxuICB9XG5cbiAgYXN5bmMgc2NhblNvdXJjZUZpbGVzKCkge1xuICAgIHRoaXMuYXNzZXJ0UHJvamVjdFJvb3QoKVxuICAgIGNvbnN0IHNvdXJjZURpcjogUGF0aCA9IHsgcm9vdElkOiB0aGlzLnByb2plY3RSb290SWQsIHNlZ21lbnRzOiBbU09VUkNFX0RJUl0gfVxuICAgIGNvbnN0IHNyY01vZHVsZXMgPSBhd2FpdCB0aGlzLmxzLmxpc3RGaWxlcyhzb3VyY2VEaXIpXG4gICAgcmV0dXJuIHNyY01vZHVsZXMucGF0aHMuZmlsdGVyKChmaWxlKSA9PiBmaWxlLnR5cGUgPT09ICdGaWxlJyAmJiBmaWxlLm5hbWUuZW5kc1dpdGgoRVhURU5TSU9OKSlcbiAgfVxuXG4gIHRyeUdldEV4aXN0aW5nTW9kdWxlTW9kZWwocGF0aDogUGF0aCk6IE1vZHVsZVBlcnNpc3RlbmNlIHwgdW5kZWZpbmVkIHtcbiAgICBjb25zdCBuYW1lID0gcGF0aFRvTW9kdWxlTmFtZShwYXRoKVxuICAgIHJldHVybiB0aGlzLmF1dGhvcml0YXRpdmVNb2R1bGVzLmdldChuYW1lKVxuICB9XG5cbiAgZ2V0TW9kdWxlTW9kZWwocGF0aDogUGF0aCk6IE1vZHVsZVBlcnNpc3RlbmNlIHtcbiAgICBjb25zdCBuYW1lID0gcGF0aFRvTW9kdWxlTmFtZShwYXRoKVxuICAgIHJldHVybiBtYXAuc2V0SWZVbmRlZmluZWQodGhpcy5hdXRob3JpdGF0aXZlTW9kdWxlcywgbmFtZSwgKCkgPT4ge1xuICAgICAgY29uc3Qgd3NEb2MgPSBuZXcgV1NTaGFyZWREb2MoKVxuICAgICAgdGhpcy5kb2NzLnNldCh3c0RvYy5kb2MuZ3VpZCwgd3NEb2MpXG4gICAgICB0aGlzLm1vZGVsLmNyZWF0ZVVubG9hZGVkTW9kdWxlKG5hbWUsIHdzRG9jLmRvYylcbiAgICAgIGNvbnN0IG1vZCA9IG5ldyBNb2R1bGVQZXJzaXN0ZW5jZSh0aGlzLmxzLCBwYXRoLCB3c0RvYy5kb2MpXG4gICAgICBtb2Qub25jZSgncmVtb3ZlZCcsICgpID0+IHtcbiAgICAgICAgY29uc3QgaW5kZXggPSB0aGlzLm1vZGVsLmZpbmRNb2R1bGVCeURvY0lkKHdzRG9jLmRvYy5ndWlkKVxuICAgICAgICB0aGlzLmRvY3MuZGVsZXRlKHdzRG9jLmRvYy5ndWlkKVxuICAgICAgICB0aGlzLmF1dGhvcml0YXRpdmVNb2R1bGVzLmRlbGV0ZShuYW1lKVxuICAgICAgICBpZiAoaW5kZXggIT0gbnVsbCkgdGhpcy5tb2RlbC5kZWxldGVNb2R1bGUoaW5kZXgpXG4gICAgICB9KVxuICAgICAgcmV0dXJuIG1vZFxuICAgIH0pXG4gIH1cblxuICByZXRhaW4oKSB7XG4gICAgdGhpcy5yZXRhaW5Db3VudCArPSAxXG4gIH1cblxuICBhc3luYyByZWxlYXNlKCk6IFByb21pc2U8dm9pZD4ge1xuICAgIHRoaXMucmV0YWluQ291bnQgLT0gMVxuICAgIGlmICh0aGlzLnJldGFpbkNvdW50ICE9PSAwKSByZXR1cm5cbiAgICBjb25zdCBtb2R1bGVzID0gdGhpcy5hdXRob3JpdGF0aXZlTW9kdWxlcy52YWx1ZXMoKVxuICAgIGNvbnN0IG1vZHVsZURpc3Bvc2VQcm9taXNlcyA9IEFycmF5LmZyb20obW9kdWxlcywgKG1vZCkgPT4gbW9kLmRpc3Bvc2UoKSlcbiAgICB0aGlzLmF1dGhvcml0YXRpdmVNb2R1bGVzLmNsZWFyKClcbiAgICB0aGlzLm1vZGVsLmRvYy5kZXN0cm95KClcbiAgICB0aGlzLmNsaWVudFNjb3BlLmRpc3Bvc2UoJ0xhbmd1ZVNlcnZlclNlc3Npb24gZGlzcG9zZWQuJylcbiAgICBMYW5ndWFnZVNlcnZlclNlc3Npb24uc2Vzc2lvbnMuZGVsZXRlKHRoaXMudXJsKVxuICAgIGF3YWl0IFByb21pc2UuYWxsKG1vZHVsZURpc3Bvc2VQcm9taXNlcylcbiAgfVxuXG4gIGdldFlEb2MoZ3VpZDogc3RyaW5nKTogV1NTaGFyZWREb2MgfCB1bmRlZmluZWQge1xuICAgIHJldHVybiB0aGlzLmRvY3MuZ2V0KGd1aWQpXG4gIH1cbn1cblxuZnVuY3Rpb24gaXNTb3VyY2VGaWxlKHBhdGg6IFBhdGgpOiBib29sZWFuIHtcbiAgcmV0dXJuIChcbiAgICBwYXRoLnNlZ21lbnRzWzBdID09PSBTT1VSQ0VfRElSICYmIHBhdGguc2VnbWVudHNbcGF0aC5zZWdtZW50cy5sZW5ndGggLSAxXS5lbmRzV2l0aChFWFRFTlNJT04pXG4gIClcbn1cblxuZnVuY3Rpb24gcGF0aFRvTW9kdWxlTmFtZShwYXRoOiBQYXRoKTogc3RyaW5nIHtcbiAgaWYgKHBhdGguc2VnbWVudHNbMF0gPT09IFNPVVJDRV9ESVIpIHJldHVybiBwYXRoLnNlZ21lbnRzLnNsaWNlKDEpLmpvaW4oJy8nKVxuICBlbHNlIHJldHVybiAnLy8nICsgcGF0aC5zZWdtZW50cy5qb2luKCcvJylcbn1cblxuZnVuY3Rpb24gcHVzaFBhdGhTZWdtZW50KHBhdGg6IFBhdGgsIHNlZ21lbnQ6IHN0cmluZyk6IFBhdGgge1xuICByZXR1cm4geyByb290SWQ6IHBhdGgucm9vdElkLCBzZWdtZW50czogWy4uLnBhdGguc2VnbWVudHMsIHNlZ21lbnRdIH1cbn1cblxuZW51bSBMc1N5bmNTdGF0ZSB7XG4gIENsb3NlZCxcbiAgT3BlbmluZyxcbiAgU3luY2hyb25pemVkLFxuICBXcml0aW5nRmlsZSxcbiAgV3JpdGVFcnJvcixcbiAgUmVsb2FkaW5nLFxuICBDbG9zaW5nLFxuICBEaXNwb3NlZCxcbn1cblxuZW51bSBMc0FjdGlvbiB7XG4gIE9wZW4sXG4gIENsb3NlLFxuICBSZWxvYWQsXG59XG5cbmNsYXNzIE1vZHVsZVBlcnNpc3RlbmNlIGV4dGVuZHMgT2JzZXJ2YWJsZVYyPHsgcmVtb3ZlZDogKCkgPT4gdm9pZCB9PiB7XG4gIGxzOiBMYW5ndWFnZVNlcnZlclxuICBwYXRoOiBQYXRoXG4gIGRvYzogTW9kdWxlRG9jID0gbmV3IE1vZHVsZURvYyhuZXcgWS5Eb2MoKSlcbiAgcmVhZG9ubHkgc3RhdGU6IExzU3luY1N0YXRlID0gTHNTeW5jU3RhdGUuQ2xvc2VkXG4gIHJlYWRvbmx5IGxhc3RBY3Rpb24gPSBQcm9taXNlLnJlc29sdmUoKVxuICB1cGRhdGVUb0FwcGx5OiBVaW50OEFycmF5IHwgbnVsbCA9IG51bGxcbiAgc3luY2VkQ29kZTogc3RyaW5nIHwgbnVsbCA9IG51bGxcbiAgc3luY2VkSWRNYXA6IHN0cmluZyB8IG51bGwgPSBudWxsXG4gIHN5bmNlZE1ldGFKc29uOiBzdHJpbmcgfCBudWxsID0gbnVsbFxuICBzeW5jZWRDb250ZW50OiBzdHJpbmcgfCBudWxsID0gbnVsbFxuICBzeW5jZWRWZXJzaW9uOiBDaGVja3N1bSB8IG51bGwgPSBudWxsXG4gIHN5bmNlZE1ldGE6IGZpbGVGb3JtYXQuTWV0YWRhdGEgPSBmaWxlRm9ybWF0LnRyeVBhcnNlTWV0YWRhdGFPckZhbGxiYWNrKG51bGwpXG4gIHF1ZXVlZEFjdGlvbjogTHNBY3Rpb24gfCBudWxsID0gbnVsbFxuICBjbGVhbnVwID0gKCkgPT4ge31cblxuICBjb25zdHJ1Y3RvcihsczogTGFuZ3VhZ2VTZXJ2ZXIsIHBhdGg6IFBhdGgsIHNoYXJlZERvYzogWS5Eb2MpIHtcbiAgICBzdXBlcigpXG4gICAgdGhpcy5scyA9IGxzXG4gICAgdGhpcy5wYXRoID0gcGF0aFxuXG4gICAgY29uc3Qgb25SZW1vdGVVcGRhdGUgPSB0aGlzLnF1ZXVlUmVtb3RlVXBkYXRlLmJpbmQodGhpcylcbiAgICBjb25zdCBvbkxvY2FsVXBkYXRlID0gKHVwZGF0ZTogVWludDhBcnJheSwgb3JpZ2luOiB1bmtub3duKSA9PiB7XG4gICAgICBpZiAob3JpZ2luID09PSAnZmlsZScpIFkuYXBwbHlVcGRhdGUoc2hhcmVkRG9jLCB1cGRhdGUsIHRoaXMpXG4gICAgfVxuICAgIGNvbnN0IG9uRmlsZU1vZGlmaWVkID0gdGhpcy5oYW5kbGVGaWxlTW9kaWZpZWQuYmluZCh0aGlzKVxuICAgIGNvbnN0IG9uRmlsZVJlbW92ZWQgPSB0aGlzLmhhbmRsZUZpbGVSZW1vdmVkLmJpbmQodGhpcylcbiAgICB0aGlzLmRvYy55ZG9jLm9uKCd1cGRhdGUnLCBvbkxvY2FsVXBkYXRlKVxuICAgIHNoYXJlZERvYy5vbigndXBkYXRlJywgb25SZW1vdGVVcGRhdGUpXG4gICAgdGhpcy5scy5vbigndGV4dC9maWxlTW9kaWZpZWRPbkRpc2snLCBvbkZpbGVNb2RpZmllZClcbiAgICB0aGlzLmxzLm9uKCdmaWxlL3Jvb3RSZW1vdmVkJywgb25GaWxlUmVtb3ZlZClcbiAgICB0aGlzLmNsZWFudXAgPSAoKSA9PiB7XG4gICAgICB0aGlzLmRvYy55ZG9jLm9mZigndXBkYXRlJywgb25Mb2NhbFVwZGF0ZSlcbiAgICAgIHNoYXJlZERvYy5vZmYoJ3VwZGF0ZScsIG9uUmVtb3RlVXBkYXRlKVxuICAgICAgdGhpcy5scy5vZmYoJ3RleHQvZmlsZU1vZGlmaWVkT25EaXNrJywgb25GaWxlTW9kaWZpZWQpXG4gICAgICB0aGlzLmxzLm9mZignZmlsZS9yb290UmVtb3ZlZCcsIG9uRmlsZVJlbW92ZWQpXG4gICAgfVxuICB9XG5cbiAgcHJpdmF0ZSBpblN0YXRlKC4uLnN0YXRlczogTHNTeW5jU3RhdGVbXSk6IGJvb2xlYW4ge1xuICAgIHJldHVybiBzdGF0ZXMuaW5jbHVkZXModGhpcy5zdGF0ZSlcbiAgfVxuXG4gIHByaXZhdGUgc2V0U3RhdGUoc3RhdGU6IExzU3luY1N0YXRlKSB7XG4gICAgaWYgKHRoaXMuc3RhdGUgIT09IExzU3luY1N0YXRlLkRpc3Bvc2VkKSB7XG4gICAgICBpZiAoREVCVUdfTE9HX1NZTkMpIHtcbiAgICAgICAgY29uc29sZS5kZWJ1ZygnU3RhdGUgY2hhbmdlOicsIExzU3luY1N0YXRlW3RoaXMuc3RhdGVdLCAnLT4nLCBMc1N5bmNTdGF0ZVtzdGF0ZV0pXG4gICAgICB9XG4gICAgICAvLyBUaGlzIGlzIFNBRkUuIGB0aGlzLnN0YXRlYCBpcyBvbmx5IGByZWFkb25seWAgdG8gZW5zdXJlIHRoYXQgdGhpcyBpcyB0aGUgb25seSBwbGFjZVxuICAgICAgLy8gd2hlcmUgaXQgaXMgbXV0YXRlZC5cbiAgICAgIC8vIEB0cy1leHBlY3QtZXJyb3JcbiAgICAgIHRoaXMuc3RhdGUgPSBzdGF0ZVxuICAgICAgaWYgKHN0YXRlID09PSBMc1N5bmNTdGF0ZS5TeW5jaHJvbml6ZWQpIHRoaXMudHJ5U3luY1JlbW92ZVVwZGF0ZXMoKVxuICAgIH0gZWxzZSB7XG4gICAgICB0aHJvdyBuZXcgRXJyb3IoJ0xzU3luYyBkaXNwb3NlZCcpXG4gICAgfVxuICB9XG5cbiAgcHJpdmF0ZSBzZXRMYXN0QWN0aW9uPFQ+KGxhc3RBY3Rpb246IFByb21pc2U8VD4pIHtcbiAgICAvLyBUaGlzIGlzIFNBRkUuIGB0aGlzLmxhc3RBY3Rpb25gIGlzIG9ubHkgYHJlYWRvbmx5YCB0byBlbnN1cmUgdGhhdCB0aGlzIGlzIHRoZSBvbmx5IHBsYWNlXG4gICAgLy8gd2hlcmUgaXQgaXMgbXV0YXRlZC5cbiAgICAvLyBAdHMtZXhwZWN0LWVycm9yXG4gICAgdGhpcy5sYXN0QWN0aW9uID0gbGFzdEFjdGlvbi50aGVuKFxuICAgICAgKCkgPT4ge30sXG4gICAgICAoKSA9PiB7fSxcbiAgICApXG4gICAgcmV0dXJuIGxhc3RBY3Rpb25cbiAgfVxuXG4gIC8qKiBTZXQgdGhlIGN1cnJlbnQgc3RhdGUgdG8gdGhlIGdpdmVuIHN0YXRlIHdoaWxlIHRoZSBjYWxsYmFjayBpcyBydW5uaW5nLlxuICAgKiBTZXQgdGhlIGN1cnJlbnQgc3RhdGUgYmFjayB0byB7QGxpbmsgTHNTeW5jU3RhdGUuU3luY2hyb25pemVkfSB3aGVuIHRoZSBjYWxsYmFjayBmaW5pc2hlcy4gKi9cbiAgcHJpdmF0ZSBhc3luYyB3aXRoU3RhdGUoc3RhdGU6IExzU3luY1N0YXRlLCBjYWxsYmFjazogKCkgPT4gdm9pZCB8IFByb21pc2U8dm9pZD4pIHtcbiAgICB0aGlzLnNldFN0YXRlKHN0YXRlKVxuICAgIGF3YWl0IGNhbGxiYWNrKClcbiAgICB0aGlzLnNldFN0YXRlKExzU3luY1N0YXRlLlN5bmNocm9uaXplZClcbiAgfVxuXG4gIGFzeW5jIG9wZW4oKSB7XG4gICAgdGhpcy5xdWV1ZWRBY3Rpb24gPSBMc0FjdGlvbi5PcGVuXG4gICAgc3dpdGNoICh0aGlzLnN0YXRlKSB7XG4gICAgICBjYXNlIExzU3luY1N0YXRlLkRpc3Bvc2VkOlxuICAgICAgY2FzZSBMc1N5bmNTdGF0ZS5Xcml0aW5nRmlsZTpcbiAgICAgIGNhc2UgTHNTeW5jU3RhdGUuU3luY2hyb25pemVkOlxuICAgICAgY2FzZSBMc1N5bmNTdGF0ZS5Xcml0ZUVycm9yOlxuICAgICAgY2FzZSBMc1N5bmNTdGF0ZS5SZWxvYWRpbmc6IHtcbiAgICAgICAgcmV0dXJuXG4gICAgICB9XG4gICAgICBjYXNlIExzU3luY1N0YXRlLkNsb3Npbmc6IHtcbiAgICAgICAgYXdhaXQgdGhpcy5sYXN0QWN0aW9uXG4gICAgICAgIGlmICh0aGlzLnF1ZXVlZEFjdGlvbiA9PT0gTHNBY3Rpb24uT3BlbikgYXdhaXQgdGhpcy5vcGVuKClcbiAgICAgICAgcmV0dXJuXG4gICAgICB9XG4gICAgICBjYXNlIExzU3luY1N0YXRlLk9wZW5pbmc6IHtcbiAgICAgICAgYXdhaXQgdGhpcy5sYXN0QWN0aW9uXG4gICAgICAgIHJldHVyblxuICAgICAgfVxuICAgICAgY2FzZSBMc1N5bmNTdGF0ZS5DbG9zZWQ6IHtcbiAgICAgICAgYXdhaXQgdGhpcy53aXRoU3RhdGUoTHNTeW5jU3RhdGUuT3BlbmluZywgYXN5bmMgKCkgPT4ge1xuICAgICAgICAgIGNvbnN0IHByb21pc2UgPSB0aGlzLmxzLm9wZW5UZXh0RmlsZSh0aGlzLnBhdGgpXG4gICAgICAgICAgdGhpcy5zZXRMYXN0QWN0aW9uKHByb21pc2UuY2F0Y2goKCkgPT4gdGhpcy5zZXRTdGF0ZShMc1N5bmNTdGF0ZS5DbG9zZWQpKSlcbiAgICAgICAgICBjb25zdCByZXN1bHQgPSBhd2FpdCBwcm9taXNlXG4gICAgICAgICAgaWYgKCFyZXN1bHQud3JpdGVDYXBhYmlsaXR5KSB7XG4gICAgICAgICAgICBjb25zb2xlLmVycm9yKCdDb3VsZCBub3QgYWNxdWlyZSB3cml0ZSBjYXBhYmlsaXR5IGZvciBtb2R1bGU6JywgdGhpcy5wYXRoKVxuICAgICAgICAgICAgdGhyb3cgbmV3IEVycm9yKFxuICAgICAgICAgICAgICBgQ291bGQgbm90IGFjcXVpcmUgd3JpdGUgY2FwYWJpbGl0eSBmb3IgbW9kdWxlICcke3RoaXMucGF0aC5zZWdtZW50cy5qb2luKCcvJyl9J2AsXG4gICAgICAgICAgICApXG4gICAgICAgICAgfVxuICAgICAgICAgIHRoaXMuc3luY0ZpbGVDb250ZW50cyhyZXN1bHQuY29udGVudCwgcmVzdWx0LmN1cnJlbnRWZXJzaW9uKVxuICAgICAgICB9KVxuICAgICAgICByZXR1cm5cbiAgICAgIH1cbiAgICAgIGRlZmF1bHQ6IHtcbiAgICAgICAgdGhpcy5zdGF0ZSBzYXRpc2ZpZXMgbmV2ZXJcbiAgICAgICAgcmV0dXJuXG4gICAgICB9XG4gICAgfVxuICB9XG5cbiAgaGFuZGxlRmlsZVJlbW92ZWQoKSB7XG4gICAgaWYgKHRoaXMuaW5TdGF0ZShMc1N5bmNTdGF0ZS5DbG9zZWQpKSByZXR1cm5cbiAgICB0aGlzLmNsb3NlKClcbiAgfVxuXG4gIGhhbmRsZUZpbGVNb2RpZmllZCgpIHtcbiAgICBpZiAodGhpcy5pblN0YXRlKExzU3luY1N0YXRlLkNsb3NlZCkpIHJldHVyblxuICB9XG5cbiAgcXVldWVSZW1vdGVVcGRhdGUodXBkYXRlOiBVaW50OEFycmF5LCBvcmlnaW46IHVua25vd24pIHtcbiAgICBpZiAob3JpZ2luID09PSB0aGlzKSByZXR1cm5cbiAgICBpZiAodGhpcy51cGRhdGVUb0FwcGx5ICE9IG51bGwpIHtcbiAgICAgIHRoaXMudXBkYXRlVG9BcHBseSA9IFkubWVyZ2VVcGRhdGVzKFt0aGlzLnVwZGF0ZVRvQXBwbHksIHVwZGF0ZV0pXG4gICAgfSBlbHNlIHtcbiAgICAgIHRoaXMudXBkYXRlVG9BcHBseSA9IHVwZGF0ZVxuICAgIH1cbiAgICB0aGlzLnRyeVN5bmNSZW1vdmVVcGRhdGVzKClcbiAgfVxuXG4gIHRyeVN5bmNSZW1vdmVVcGRhdGVzKCkge1xuICAgIGlmICh0aGlzLnVwZGF0ZVRvQXBwbHkgPT0gbnVsbCkgcmV0dXJuXG4gICAgLy8gYXBwbHkgdXBkYXRlcyB0byB0aGUgbHMtcmVwcmVzZW50YXRpb24gZG9jIGlmIHdlIGFyZSBhbHJlYWR5IGluIHN5bmMgd2l0aCB0aGUgTFMuXG4gICAgaWYgKCF0aGlzLmluU3RhdGUoTHNTeW5jU3RhdGUuU3luY2hyb25pemVkKSkgcmV0dXJuXG4gICAgY29uc3QgdXBkYXRlID0gdGhpcy51cGRhdGVUb0FwcGx5XG4gICAgdGhpcy51cGRhdGVUb0FwcGx5ID0gbnVsbFxuXG4gICAgY29uc3Qgc3luY01vZHVsZSA9IG5ldyBBc3QuTXV0YWJsZU1vZHVsZSh0aGlzLmRvYy55ZG9jKVxuICAgIGNvbnN0IG1vZHVsZVVwZGF0ZSA9IHN5bmNNb2R1bGUuYXBwbHlVcGRhdGUodXBkYXRlLCAncmVtb3RlJylcbiAgICBpZiAobW9kdWxlVXBkYXRlICYmIHRoaXMuc3luY2VkQ29udGVudCkge1xuICAgICAgY29uc3Qgc3luY2VkID0gc3BsaXRGaWxlQ29udGVudHModGhpcy5zeW5jZWRDb250ZW50KVxuICAgICAgY29uc3QgeyBuZXdDb2RlLCBuZXdJZE1hcCwgbmV3TWV0YWRhdGEgfSA9IGFwcGx5RG9jdW1lbnRVcGRhdGVzKFxuICAgICAgICB0aGlzLmRvYyxcbiAgICAgICAgc3luY2VkLFxuICAgICAgICBtb2R1bGVVcGRhdGUsXG4gICAgICApXG4gICAgICB0aGlzLnNlbmRMc1VwZGF0ZShzeW5jZWQsIG5ld0NvZGUsIG5ld0lkTWFwLCBuZXdNZXRhZGF0YSlcbiAgICB9XG4gIH1cblxuICBwcml2YXRlIHNlbmRMc1VwZGF0ZShcbiAgICBzeW5jZWQ6IEVuc29GaWxlUGFydHMsXG4gICAgbmV3Q29kZTogc3RyaW5nIHwgdW5kZWZpbmVkLFxuICAgIG5ld0lkTWFwOiBJZE1hcCB8IHVuZGVmaW5lZCxcbiAgICBuZXdNZXRhZGF0YTogZmlsZUZvcm1hdC5JZGVNZXRhZGF0YVsnbm9kZSddIHwgdW5kZWZpbmVkLFxuICApIHtcbiAgICBpZiAodGhpcy5zeW5jZWRDb250ZW50ID09IG51bGwgfHwgdGhpcy5zeW5jZWRWZXJzaW9uID09IG51bGwpIHJldHVyblxuXG4gICAgY29uc3QgY29kZSA9IG5ld0NvZGUgPz8gc3luY2VkLmNvZGVcbiAgICBjb25zdCBuZXdNZXRhZGF0YUpzb24gPVxuICAgICAgbmV3TWV0YWRhdGEgJiZcbiAgICAgIGpzb24uc3RyaW5naWZ5KHsgLi4udGhpcy5zeW5jZWRNZXRhLCBpZGU6IHsgLi4udGhpcy5zeW5jZWRNZXRhLmlkZSwgbm9kZTogbmV3TWV0YWRhdGEgfSB9KVxuICAgIGNvbnN0IG5ld0lkTWFwSnNvbiA9IG5ld0lkTWFwICYmIHNlcmlhbGl6ZUlkTWFwKG5ld0lkTWFwKVxuICAgIGNvbnN0IG5ld0NvbnRlbnQgPSBjb21iaW5lRmlsZVBhcnRzKHtcbiAgICAgIGNvZGUsXG4gICAgICBpZE1hcEpzb246IG5ld0lkTWFwSnNvbiA/PyBzeW5jZWQuaWRNYXBKc29uID8/ICdbXScsXG4gICAgICBtZXRhZGF0YUpzb246IG5ld01ldGFkYXRhSnNvbiA/PyBzeW5jZWQubWV0YWRhdGFKc29uID8/ICd7fScsXG4gICAgfSlcblxuICAgIGNvbnN0IGVkaXRzOiBUZXh0RWRpdFtdID0gW11cbiAgICBpZiAobmV3Q29kZSkgZWRpdHMucHVzaCguLi5hcHBseURpZmZBc1RleHRFZGl0cygwLCBzeW5jZWQuY29kZSwgbmV3Q29kZSkpXG4gICAgaWYgKG5ld0lkTWFwIHx8IG5ld01ldGFkYXRhKSB7XG4gICAgICBjb25zdCBvbGRNZXRhQ29udGVudCA9IHRoaXMuc3luY2VkQ29udGVudC5zbGljZShzeW5jZWQuY29kZS5sZW5ndGgpXG4gICAgICBjb25zdCBtZXRhQ29udGVudCA9IG5ld0NvbnRlbnQuc2xpY2UoY29kZS5sZW5ndGgpXG4gICAgICBjb25zdCBtZXRhU3RhcnRMaW5lID0gKGNvZGUubWF0Y2goL1xcbi9nKSA/PyBbXSkubGVuZ3RoXG4gICAgICBlZGl0cy5wdXNoKC4uLmFwcGx5RGlmZkFzVGV4dEVkaXRzKG1ldGFTdGFydExpbmUsIG9sZE1ldGFDb250ZW50LCBtZXRhQ29udGVudCkpXG4gICAgfVxuXG4gICAgY29uc3QgbmV3VmVyc2lvbiA9IGNvbXB1dGVUZXh0Q2hlY2tzdW0obmV3Q29udGVudClcblxuICAgIGlmIChERUJVR19MT0dfU1lOQykge1xuICAgICAgY29uc29sZS5kZWJ1ZygnID09PSBjaGFuZ2VzID09PSAnKVxuICAgICAgY29uc29sZS5kZWJ1ZygnbnVtYmVyIG9mIGVkaXRzOicsIGVkaXRzLmxlbmd0aClcbiAgICAgIGlmIChlZGl0cy5sZW5ndGggPiAwKSB7XG4gICAgICAgIGNvbnNvbGUuZGVidWcoJ3ZlcnNpb246JywgdGhpcy5zeW5jZWRWZXJzaW9uLCAnLT4nLCBuZXdWZXJzaW9uKVxuICAgICAgICBjb25zb2xlLmRlYnVnKCdDb250ZW50IGRpZmY6JylcbiAgICAgICAgY29uc29sZS5kZWJ1ZyhwcmV0dHlQcmludERpZmYodGhpcy5zeW5jZWRDb250ZW50LCBuZXdDb250ZW50KSlcbiAgICAgIH1cbiAgICAgIGNvbnNvbGUuZGVidWcoJyA9PT09PT09PT09PT09PT0gJylcbiAgICB9XG5cbiAgICB0aGlzLnNldFN0YXRlKExzU3luY1N0YXRlLldyaXRpbmdGaWxlKVxuXG4gICAgY29uc3QgZXhlY3V0ZSA9IG5ld0NvZGUgIT0gbnVsbCB8fCBuZXdJZE1hcCAhPSBudWxsXG4gICAgY29uc3QgZWRpdDogRmlsZUVkaXQgPSB7IHBhdGg6IHRoaXMucGF0aCwgZWRpdHMsIG9sZFZlcnNpb246IHRoaXMuc3luY2VkVmVyc2lvbiwgbmV3VmVyc2lvbiB9XG4gICAgY29uc3QgYXBwbHkgPSB0aGlzLmxzLmFwcGx5RWRpdChlZGl0LCBleGVjdXRlKVxuICAgIGNvbnN0IHByb21pc2UgPSBhcHBseS50aGVuKFxuICAgICAgKCkgPT4ge1xuICAgICAgICB0aGlzLnN5bmNlZENvbnRlbnQgPSBuZXdDb250ZW50XG4gICAgICAgIHRoaXMuc3luY2VkVmVyc2lvbiA9IG5ld1ZlcnNpb25cbiAgICAgICAgaWYgKG5ld01ldGFkYXRhKSB0aGlzLnN5bmNlZE1ldGEuaWRlLm5vZGUgPSBuZXdNZXRhZGF0YVxuICAgICAgICBpZiAobmV3Q29kZSkgdGhpcy5zeW5jZWRDb2RlID0gbmV3Q29kZVxuICAgICAgICBpZiAobmV3SWRNYXBKc29uKSB0aGlzLnN5bmNlZElkTWFwID0gbmV3SWRNYXBKc29uXG4gICAgICAgIGlmIChuZXdNZXRhZGF0YUpzb24pIHRoaXMuc3luY2VkTWV0YUpzb24gPSBuZXdNZXRhZGF0YUpzb25cbiAgICAgICAgdGhpcy5zZXRTdGF0ZShMc1N5bmNTdGF0ZS5TeW5jaHJvbml6ZWQpXG4gICAgICB9LFxuICAgICAgKGVycm9yKSA9PiB7XG4gICAgICAgIGNvbnNvbGUuZXJyb3IoJ0NvdWxkIG5vdCBhcHBseSBlZGl0OicsIGVycm9yKVxuICAgICAgICAvLyBUcnkgdG8gcmVjb3ZlciBieSByZWxvYWRpbmcgdGhlIGZpbGUuXG4gICAgICAgIC8vIERyb3AgdGhlIGF0dGVtcHRlZCB1cGRhdGVzLCBzaW5jZSBhcHBseWluZyB0aGVtIGhhdmUgZmFpbGVkLlxuICAgICAgICB0aGlzLnNldFN0YXRlKExzU3luY1N0YXRlLldyaXRlRXJyb3IpXG4gICAgICAgIHRoaXMuc3luY2VkQ29udGVudCA9IG51bGxcbiAgICAgICAgdGhpcy5zeW5jZWRWZXJzaW9uID0gbnVsbFxuICAgICAgICB0aGlzLnN5bmNlZENvZGUgPSBudWxsXG4gICAgICAgIHRoaXMuc3luY2VkSWRNYXAgPSBudWxsXG4gICAgICAgIHRoaXMuc3luY2VkTWV0YUpzb24gPSBudWxsXG4gICAgICAgIHJldHVybiB0aGlzLnJlbG9hZCgpXG4gICAgICB9LFxuICAgIClcbiAgICB0aGlzLnNldExhc3RBY3Rpb24ocHJvbWlzZSlcbiAgICByZXR1cm4gcHJvbWlzZVxuICB9XG5cbiAgcHJpdmF0ZSBzeW5jRmlsZUNvbnRlbnRzKGNvbnRlbnQ6IHN0cmluZywgdmVyc2lvbjogQ2hlY2tzdW0pIHtcbiAgICBjb25zdCBjb250ZW50c1JlY2VpdmVkID0gc3BsaXRGaWxlQ29udGVudHMoY29udGVudClcbiAgICBsZXQgdW5zeW5jZWRJZE1hcDogSWRNYXAgfCB1bmRlZmluZWRcbiAgICB0aGlzLmRvYy55ZG9jLnRyYW5zYWN0KCgpID0+IHtcbiAgICAgIGNvbnN0IHsgY29kZSwgaWRNYXBKc29uLCBtZXRhZGF0YUpzb24gfSA9IGNvbnRlbnRzUmVjZWl2ZWRcbiAgICAgIGNvbnN0IG1ldGFkYXRhID0gZmlsZUZvcm1hdC50cnlQYXJzZU1ldGFkYXRhT3JGYWxsYmFjayhtZXRhZGF0YUpzb24pXG4gICAgICBjb25zdCBub2RlTWV0YSA9IE9iamVjdC5lbnRyaWVzKG1ldGFkYXRhLmlkZS5ub2RlKVxuXG4gICAgICBsZXQgcGFyc2VkU3BhbnNcbiAgICAgIGNvbnN0IHN5bmNNb2R1bGUgPSBuZXcgQXN0Lk11dGFibGVNb2R1bGUodGhpcy5kb2MueWRvYylcbiAgICAgIGlmIChjb2RlICE9PSB0aGlzLnN5bmNlZENvZGUpIHtcbiAgICAgICAgY29uc3Qgc3luY1Jvb3QgPSBzeW5jTW9kdWxlLnJvb3QoKVxuICAgICAgICBpZiAoc3luY1Jvb3QpIHtcbiAgICAgICAgICBjb25zdCBlZGl0ID0gc3luY01vZHVsZS5lZGl0KClcbiAgICAgICAgICBlZGl0LmdldFZlcnNpb24oc3luY1Jvb3QpLnN5bmNUb0NvZGUoY29kZSlcbiAgICAgICAgICBjb25zdCBlZGl0ZWRSb290ID0gZWRpdC5yb290KClcbiAgICAgICAgICBpZiAoZWRpdGVkUm9vdCBpbnN0YW5jZW9mIEFzdC5Cb2R5QmxvY2spIEFzdC5yZXBhaXIoZWRpdGVkUm9vdCwgZWRpdClcbiAgICAgICAgICBzeW5jTW9kdWxlLmFwcGx5RWRpdChlZGl0KVxuICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgIGNvbnN0IHsgcm9vdCwgc3BhbnMgfSA9IEFzdC5wYXJzZUJsb2NrV2l0aFNwYW5zKGNvZGUsIHN5bmNNb2R1bGUpXG4gICAgICAgICAgc3luY01vZHVsZS5zeW5jUm9vdChyb290KVxuICAgICAgICAgIHBhcnNlZFNwYW5zID0gc3BhbnNcbiAgICAgICAgfVxuICAgICAgfVxuICAgICAgY29uc3QgYXN0Um9vdCA9IHN5bmNNb2R1bGUucm9vdCgpXG4gICAgICBpZiAoIWFzdFJvb3QpIHJldHVyblxuICAgICAgaWYgKChjb2RlICE9PSB0aGlzLnN5bmNlZENvZGUgfHwgaWRNYXBKc29uICE9PSB0aGlzLnN5bmNlZElkTWFwKSAmJiBpZE1hcEpzb24pIHtcbiAgICAgICAgY29uc3QgaWRNYXAgPSBkZXNlcmlhbGl6ZUlkTWFwKGlkTWFwSnNvbilcbiAgICAgICAgY29uc3Qgc3BhbnMgPSBwYXJzZWRTcGFucyA/PyBBc3QucHJpbnQoYXN0Um9vdCkuaW5mb1xuICAgICAgICBjb25zdCBpZHNBc3NpZ25lZCA9IEFzdC5zZXRFeHRlcm5hbElkcyhzeW5jTW9kdWxlLCBzcGFucywgaWRNYXApXG4gICAgICAgIGNvbnN0IG51bWJlck9mQXN0cyA9IGFzdENvdW50KGFzdFJvb3QpXG4gICAgICAgIGNvbnN0IGlkc05vdFNldEJ5TWFwID0gbnVtYmVyT2ZBc3RzIC0gaWRzQXNzaWduZWRcbiAgICAgICAgaWYgKGlkc05vdFNldEJ5TWFwID4gMCkge1xuICAgICAgICAgIGlmIChjb2RlICE9PSB0aGlzLnN5bmNlZENvZGUpIHtcbiAgICAgICAgICAgIHVuc3luY2VkSWRNYXAgPSBBc3Quc3Bhbk1hcFRvSWRNYXAoc3BhbnMpXG4gICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIGNvbnNvbGUud2FybihcbiAgICAgICAgICAgICAgYFRoZSBMUyBzZW50IGFuIElkTWFwLW9ubHkgZWRpdCB0aGF0IGlzIG1pc3NpbmcgJHtpZHNOb3RTZXRCeU1hcH0gb2Ygb3VyIGV4cGVjdGVkIEFTVHMuYCxcbiAgICAgICAgICAgIClcbiAgICAgICAgICB9XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICAgIGlmIChcbiAgICAgICAgKGNvZGUgIT09IHRoaXMuc3luY2VkQ29kZSB8fFxuICAgICAgICAgIGlkTWFwSnNvbiAhPT0gdGhpcy5zeW5jZWRJZE1hcCB8fFxuICAgICAgICAgIG1ldGFkYXRhSnNvbiAhPT0gdGhpcy5zeW5jZWRNZXRhSnNvbikgJiZcbiAgICAgICAgbm9kZU1ldGEubGVuZ3RoICE9PSAwXG4gICAgICApIHtcbiAgICAgICAgY29uc3QgZXh0ZXJuYWxJZFRvQXN0ID0gbmV3IE1hcDxFeHRlcm5hbElkLCBBc3QuQXN0PigpXG4gICAgICAgIGFzdFJvb3QudmlzaXRSZWN1cnNpdmVBc3QoKGFzdCkgPT4ge1xuICAgICAgICAgIGlmICghZXh0ZXJuYWxJZFRvQXN0Lmhhcyhhc3QuZXh0ZXJuYWxJZCkpIGV4dGVybmFsSWRUb0FzdC5zZXQoYXN0LmV4dGVybmFsSWQsIGFzdClcbiAgICAgICAgfSlcbiAgICAgICAgY29uc3QgbWlzc2luZyA9IG5ldyBTZXQ8c3RyaW5nPigpXG4gICAgICAgIGZvciAoY29uc3QgW2lkLCBtZXRhXSBvZiBub2RlTWV0YSkge1xuICAgICAgICAgIGlmICh0eXBlb2YgaWQgIT09ICdzdHJpbmcnKSBjb250aW51ZVxuICAgICAgICAgIGNvbnN0IGFzdCA9IGV4dGVybmFsSWRUb0FzdC5nZXQoaWQgYXMgRXh0ZXJuYWxJZClcbiAgICAgICAgICBpZiAoIWFzdCkge1xuICAgICAgICAgICAgbWlzc2luZy5hZGQoaWQpXG4gICAgICAgICAgICBjb250aW51ZVxuICAgICAgICAgIH1cbiAgICAgICAgICBjb25zdCBtZXRhZGF0YSA9IHN5bmNNb2R1bGUuZ2V0VmVyc2lvbihhc3QpLm11dGFibGVOb2RlTWV0YWRhdGEoKVxuICAgICAgICAgIGNvbnN0IG9sZFBvcyA9IG1ldGFkYXRhLmdldCgncG9zaXRpb24nKVxuICAgICAgICAgIGNvbnN0IG5ld1BvcyA9IHsgeDogbWV0YS5wb3NpdGlvbi52ZWN0b3JbMF0sIHk6IC1tZXRhLnBvc2l0aW9uLnZlY3RvclsxXSB9XG4gICAgICAgICAgaWYgKG9sZFBvcz8ueCAhPT0gbmV3UG9zLnggfHwgb2xkUG9zPy55ICE9PSBuZXdQb3MueSkgbWV0YWRhdGEuc2V0KCdwb3NpdGlvbicsIG5ld1BvcylcbiAgICAgICAgICBjb25zdCBvbGRWaXMgPSBtZXRhZGF0YS5nZXQoJ3Zpc3VhbGl6YXRpb24nKVxuICAgICAgICAgIGNvbnN0IG5ld1ZpcyA9IG1ldGEudmlzdWFsaXphdGlvbiAmJiB0cmFuc2xhdGVWaXN1YWxpemF0aW9uRnJvbUZpbGUobWV0YS52aXN1YWxpemF0aW9uKVxuICAgICAgICAgIGlmICghdmlzTWV0YWRhdGFFcXVhbHMobmV3VmlzLCBvbGRWaXMpKSBtZXRhZGF0YS5zZXQoJ3Zpc3VhbGl6YXRpb24nLCBuZXdWaXMpXG4gICAgICAgICAgY29uc3Qgb2xkQ29sb3JPdmVycmlkZSA9IG1ldGFkYXRhLmdldCgnY29sb3JPdmVycmlkZScpXG4gICAgICAgICAgY29uc3QgbmV3Q29sb3JPdmVycmlkZSA9IG1ldGEuY29sb3JPdmVycmlkZVxuICAgICAgICAgIGlmIChvbGRDb2xvck92ZXJyaWRlICE9PSBuZXdDb2xvck92ZXJyaWRlKSBtZXRhZGF0YS5zZXQoJ2NvbG9yT3ZlcnJpZGUnLCBuZXdDb2xvck92ZXJyaWRlKVxuICAgICAgICB9XG4gICAgICB9XG5cbiAgICAgIHRoaXMuc3luY2VkQ29kZSA9IGNvZGVcbiAgICAgIHRoaXMuc3luY2VkSWRNYXAgPSB1bnN5bmNlZElkTWFwID8gbnVsbCA6IGlkTWFwSnNvblxuICAgICAgdGhpcy5zeW5jZWRDb250ZW50ID0gY29udGVudFxuICAgICAgdGhpcy5zeW5jZWRWZXJzaW9uID0gdmVyc2lvblxuICAgICAgdGhpcy5zeW5jZWRNZXRhID0gbWV0YWRhdGFcbiAgICAgIHRoaXMuc3luY2VkTWV0YUpzb24gPSBtZXRhZGF0YUpzb25cbiAgICB9LCAnZmlsZScpXG4gICAgaWYgKHVuc3luY2VkSWRNYXApIHRoaXMuc2VuZExzVXBkYXRlKGNvbnRlbnRzUmVjZWl2ZWQsIHVuZGVmaW5lZCwgdW5zeW5jZWRJZE1hcCwgdW5kZWZpbmVkKVxuICB9XG5cbiAgYXN5bmMgY2xvc2UoKSB7XG4gICAgdGhpcy5xdWV1ZWRBY3Rpb24gPSBMc0FjdGlvbi5DbG9zZVxuICAgIHN3aXRjaCAodGhpcy5zdGF0ZSkge1xuICAgICAgY2FzZSBMc1N5bmNTdGF0ZS5EaXNwb3NlZDpcbiAgICAgIGNhc2UgTHNTeW5jU3RhdGUuQ2xvc2VkOiB7XG4gICAgICAgIHJldHVyblxuICAgICAgfVxuICAgICAgY2FzZSBMc1N5bmNTdGF0ZS5DbG9zaW5nOiB7XG4gICAgICAgIGF3YWl0IHRoaXMubGFzdEFjdGlvblxuICAgICAgICByZXR1cm5cbiAgICAgIH1cbiAgICAgIGNhc2UgTHNTeW5jU3RhdGUuT3BlbmluZzpcbiAgICAgIGNhc2UgTHNTeW5jU3RhdGUuV3JpdGluZ0ZpbGU6XG4gICAgICBjYXNlIExzU3luY1N0YXRlLlJlbG9hZGluZzoge1xuICAgICAgICBhd2FpdCB0aGlzLmxhc3RBY3Rpb25cbiAgICAgICAgaWYgKHRoaXMucXVldWVkQWN0aW9uID09PSBMc0FjdGlvbi5DbG9zZSkge1xuICAgICAgICAgIGF3YWl0IHRoaXMuY2xvc2UoKVxuICAgICAgICB9XG4gICAgICAgIHJldHVyblxuICAgICAgfVxuICAgICAgY2FzZSBMc1N5bmNTdGF0ZS5Xcml0ZUVycm9yOlxuICAgICAgY2FzZSBMc1N5bmNTdGF0ZS5TeW5jaHJvbml6ZWQ6IHtcbiAgICAgICAgdGhpcy5zZXRTdGF0ZShMc1N5bmNTdGF0ZS5DbG9zaW5nKVxuICAgICAgICBjb25zdCBwcm9taXNlID0gdGhpcy5scy5jbG9zZVRleHRGaWxlKHRoaXMucGF0aClcbiAgICAgICAgY29uc3Qgc3RhdGUgPSB0aGlzLnN0YXRlXG4gICAgICAgIHRoaXMuc2V0TGFzdEFjdGlvbihwcm9taXNlLmNhdGNoKCgpID0+IHRoaXMuc2V0U3RhdGUoc3RhdGUpKSlcbiAgICAgICAgYXdhaXQgcHJvbWlzZVxuICAgICAgICB0aGlzLnNldFN0YXRlKExzU3luY1N0YXRlLkNsb3NlZClcbiAgICAgICAgcmV0dXJuXG4gICAgICB9XG4gICAgICBkZWZhdWx0OiB7XG4gICAgICAgIHRoaXMuc3RhdGUgc2F0aXNmaWVzIG5ldmVyXG4gICAgICAgIHJldHVyblxuICAgICAgfVxuICAgIH1cbiAgfVxuXG4gIGFzeW5jIHJlbG9hZCgpIHtcbiAgICB0aGlzLnF1ZXVlZEFjdGlvbiA9IExzQWN0aW9uLlJlbG9hZFxuICAgIHN3aXRjaCAodGhpcy5zdGF0ZSkge1xuICAgICAgY2FzZSBMc1N5bmNTdGF0ZS5PcGVuaW5nOlxuICAgICAgY2FzZSBMc1N5bmNTdGF0ZS5EaXNwb3NlZDpcbiAgICAgIGNhc2UgTHNTeW5jU3RhdGUuQ2xvc2VkOlxuICAgICAgY2FzZSBMc1N5bmNTdGF0ZS5DbG9zaW5nOiB7XG4gICAgICAgIHJldHVyblxuICAgICAgfVxuICAgICAgY2FzZSBMc1N5bmNTdGF0ZS5SZWxvYWRpbmc6IHtcbiAgICAgICAgYXdhaXQgdGhpcy5sYXN0QWN0aW9uXG4gICAgICAgIHJldHVyblxuICAgICAgfVxuICAgICAgY2FzZSBMc1N5bmNTdGF0ZS5Xcml0aW5nRmlsZToge1xuICAgICAgICBhd2FpdCB0aGlzLmxhc3RBY3Rpb25cbiAgICAgICAgaWYgKHRoaXMucXVldWVkQWN0aW9uID09PSBMc0FjdGlvbi5SZWxvYWQpIGF3YWl0IHRoaXMucmVsb2FkKClcbiAgICAgICAgcmV0dXJuXG4gICAgICB9XG4gICAgICBjYXNlIExzU3luY1N0YXRlLlN5bmNocm9uaXplZDoge1xuICAgICAgICB0aGlzLndpdGhTdGF0ZShMc1N5bmNTdGF0ZS5SZWxvYWRpbmcsIGFzeW5jICgpID0+IHtcbiAgICAgICAgICBjb25zdCBwcm9taXNlID0gUHJvbWlzZS5hbGwoW1xuICAgICAgICAgICAgdGhpcy5scy5yZWFkRmlsZSh0aGlzLnBhdGgpLFxuICAgICAgICAgICAgdGhpcy5scy5maWxlQ2hlY2tzdW0odGhpcy5wYXRoKSxcbiAgICAgICAgICBdKVxuICAgICAgICAgIHRoaXMuc2V0TGFzdEFjdGlvbihwcm9taXNlKVxuICAgICAgICAgIGNvbnN0IFtjb250ZW50cywgY2hlY2tzdW1dID0gYXdhaXQgcHJvbWlzZVxuICAgICAgICAgIHRoaXMuc3luY0ZpbGVDb250ZW50cyhjb250ZW50cy5jb250ZW50cywgY2hlY2tzdW0uY2hlY2tzdW0pXG4gICAgICAgIH0pXG4gICAgICAgIHJldHVyblxuICAgICAgfVxuICAgICAgY2FzZSBMc1N5bmNTdGF0ZS5Xcml0ZUVycm9yOiB7XG4gICAgICAgIHRoaXMud2l0aFN0YXRlKExzU3luY1N0YXRlLlJlbG9hZGluZywgYXN5bmMgKCkgPT4ge1xuICAgICAgICAgIGNvbnN0IHBhdGggPSB0aGlzLnBhdGguc2VnbWVudHMuam9pbignLycpXG4gICAgICAgICAgY29uc3QgcmVsb2FkaW5nID0gdGhpcy5sc1xuICAgICAgICAgICAgLmNsb3NlVGV4dEZpbGUodGhpcy5wYXRoKVxuICAgICAgICAgICAgLmNhdGNoKChlcnJvcikgPT4ge1xuICAgICAgICAgICAgICBjb25zb2xlLmVycm9yKCdDb3VsZCBub3QgY2xvc2UgZmlsZSBhZnRlciB3cml0ZSBlcnJvcjonKVxuICAgICAgICAgICAgICBjb25zb2xlLmVycm9yKGVycm9yKVxuICAgICAgICAgICAgfSlcbiAgICAgICAgICAgIC50aGVuKFxuICAgICAgICAgICAgICAoKSA9PlxuICAgICAgICAgICAgICAgIGV4cG9uZW50aWFsQmFja29mZihcbiAgICAgICAgICAgICAgICAgIGFzeW5jICgpID0+IHtcbiAgICAgICAgICAgICAgICAgICAgY29uc3QgcmVzdWx0ID0gYXdhaXQgdGhpcy5scy5vcGVuVGV4dEZpbGUodGhpcy5wYXRoKVxuICAgICAgICAgICAgICAgICAgICBpZiAoIXJlc3VsdC53cml0ZUNhcGFiaWxpdHkpIHtcbiAgICAgICAgICAgICAgICAgICAgICBjb25zdCBtZXNzYWdlID0gYENvdWxkIG5vdCBhY3F1aXJlIHdyaXRlIGNhcGFiaWxpdHkgZm9yIG1vZHVsZSAnJHt0aGlzLnBhdGguc2VnbWVudHMuam9pbihcbiAgICAgICAgICAgICAgICAgICAgICAgICcvJyxcbiAgICAgICAgICAgICAgICAgICAgICApfSdgXG4gICAgICAgICAgICAgICAgICAgICAgY29uc29sZS5lcnJvcihtZXNzYWdlKVxuICAgICAgICAgICAgICAgICAgICAgIHRocm93IG5ldyBFcnJvcihtZXNzYWdlKVxuICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgIHJldHVybiByZXN1bHRcbiAgICAgICAgICAgICAgICAgIH0sXG4gICAgICAgICAgICAgICAgICBwcmludGluZ0NhbGxiYWNrcyhcbiAgICAgICAgICAgICAgICAgICAgYG9wZW5lZCBmaWxlICcke3BhdGh9JyBmb3Igd3JpdGluZ2AsXG4gICAgICAgICAgICAgICAgICAgIGBvcGVuIGZpbGUgJyR7cGF0aH0nIGZvciB3cml0aW5nYCxcbiAgICAgICAgICAgICAgICAgICksXG4gICAgICAgICAgICAgICAgKSxcbiAgICAgICAgICAgICAgKGVycm9yKSA9PiB7XG4gICAgICAgICAgICAgICAgY29uc29sZS5lcnJvcignQ291bGQgbm90IHJlb3BlbiBmaWxlIGFmdGVyIHdyaXRlIGVycm9yOicpXG4gICAgICAgICAgICAgICAgY29uc29sZS5lcnJvcihlcnJvcilcbiAgICAgICAgICAgICAgICAvLyBUaGlzIGVycm9yIGlzIHVucmVjb3ZlcmFibGUuXG4gICAgICAgICAgICAgICAgdGhyb3cgZXJyb3JcbiAgICAgICAgICAgICAgfSxcbiAgICAgICAgICAgIClcbiAgICAgICAgICB0aGlzLnNldExhc3RBY3Rpb24ocmVsb2FkaW5nKVxuICAgICAgICAgIGNvbnN0IHJlc3VsdCA9IGF3YWl0IHJlbG9hZGluZ1xuICAgICAgICAgIHRoaXMuc3luY0ZpbGVDb250ZW50cyhyZXN1bHQuY29udGVudCwgcmVzdWx0LmN1cnJlbnRWZXJzaW9uKVxuICAgICAgICB9KVxuICAgICAgICByZXR1cm5cbiAgICAgIH1cbiAgICAgIGRlZmF1bHQ6IHtcbiAgICAgICAgdGhpcy5zdGF0ZSBzYXRpc2ZpZXMgbmV2ZXJcbiAgICAgICAgcmV0dXJuXG4gICAgICB9XG4gICAgfVxuICB9XG5cbiAgYXN5bmMgZGlzcG9zZSgpOiBQcm9taXNlPHZvaWQ+IHtcbiAgICB0aGlzLmNsZWFudXAoKVxuICAgIGNvbnN0IGFscmVhZHlDbG9zZWQgPSB0aGlzLmluU3RhdGUoTHNTeW5jU3RhdGUuQ2xvc2luZywgTHNTeW5jU3RhdGUuQ2xvc2VkKVxuICAgIHRoaXMuc2V0U3RhdGUoTHNTeW5jU3RhdGUuRGlzcG9zZWQpXG4gICAgaWYgKGFscmVhZHlDbG9zZWQpIHJldHVybiBQcm9taXNlLnJlc29sdmUoKVxuICAgIHJldHVybiB0aGlzLmxzLmNsb3NlVGV4dEZpbGUodGhpcy5wYXRoKVxuICB9XG59XG4iLCAiY29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2Rpcm5hbWUgPSBcIkM6XFxcXFByb2plY3RzXFxcXGVuc29cXFxcZW5zb1xcXFxhcHBcXFxcZ3VpMlxcXFxzaGFyZWRcXFxcYXN0XCI7Y29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2ZpbGVuYW1lID0gXCJDOlxcXFxQcm9qZWN0c1xcXFxlbnNvXFxcXGVuc29cXFxcYXBwXFxcXGd1aTJcXFxcc2hhcmVkXFxcXGFzdFxcXFxpbmRleC50c1wiO2NvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9pbXBvcnRfbWV0YV91cmwgPSBcImZpbGU6Ly8vQzovUHJvamVjdHMvZW5zby9lbnNvL2FwcC9ndWkyL3NoYXJlZC9hc3QvaW5kZXgudHNcIjtpbXBvcnQgKiBhcyByYW5kb20gZnJvbSAnbGliMC9yYW5kb20nXG5pbXBvcnQgeyByZWFjaGFibGUgfSBmcm9tICcuLi91dGlsL2RhdGEvZ3JhcGgnXG5pbXBvcnQgdHlwZSB7IEV4dGVybmFsSWQgfSBmcm9tICcuLi95anNNb2RlbCdcbmltcG9ydCB0eXBlIHsgTW9kdWxlIH0gZnJvbSAnLi9tdXRhYmxlTW9kdWxlJ1xuaW1wb3J0IHR5cGUgeyBTeW5jVG9rZW5JZCB9IGZyb20gJy4vdG9rZW4nXG5pbXBvcnQgdHlwZSB7IEFzdElkIH0gZnJvbSAnLi90cmVlJ1xuaW1wb3J0IHsgQXBwLCBBc3QsIEdyb3VwLCBNdXRhYmxlQXN0LCBPcHJBcHAsIFdpbGRjYXJkIH0gZnJvbSAnLi90cmVlJ1xuXG5leHBvcnQgKiBmcm9tICcuL211dGFibGVNb2R1bGUnXG5leHBvcnQgKiBmcm9tICcuL3BhcnNlJ1xuZXhwb3J0ICogZnJvbSAnLi90ZXh0J1xuZXhwb3J0ICogZnJvbSAnLi90b2tlbidcbmV4cG9ydCAqIGZyb20gJy4vdHJlZSdcblxuZGVjbGFyZSBjb25zdCBicmFuZE93bmVkOiB1bmlxdWUgc3ltYm9sXG4vKiogVXNlZCB0byBtYXJrIHJlZmVyZW5jZXMgcmVxdWlyZWQgdG8gYmUgdW5pcXVlLlxuICpcbiAqICBOb3RlIHRoYXQgdGhlIHR5cGVzeXN0ZW0gY2Fubm90IHN0b3AgeW91IGZyb20gY29weWluZyBhbiBgT3duZWRgLFxuICogIGJ1dCB0aGF0IGlzIGFuIGVhc3kgbWlzdGFrZSB0byBzZWUgKGJlY2F1c2UgaXQgb2NjdXJzIGxvY2FsbHkpLlxuICpcbiAqICBXZSBjYW4gYXQgbGVhc3QgcmVxdWlyZSAqb2J0YWluaW5nKiBhbiBgT3duZWRgLFxuICogIHdoaWNoIHN0YXRpY2FsbHkgcHJldmVudHMgdGhlIG90aGVyd2lzZSBtb3N0IGxpa2VseSB1c2FnZSBlcnJvcnMgd2hlbiByZWFycmFuZ2luZyBBU1RzLlxuICovXG5leHBvcnQgdHlwZSBPd25lZDxUID0gTXV0YWJsZUFzdD4gPSBUICYgeyBbYnJhbmRPd25lZF06IG5ldmVyIH1cbi8qKiBAaW50ZXJuYWwgKi9cbmV4cG9ydCBmdW5jdGlvbiBhc093bmVkPFQ+KHQ6IFQpOiBPd25lZDxUPiB7XG4gIHJldHVybiB0IGFzIE93bmVkPFQ+XG59XG5cbmV4cG9ydCB0eXBlIE5vZGVDaGlsZDxUPiA9IHsgd2hpdGVzcGFjZTogc3RyaW5nIHwgdW5kZWZpbmVkOyBub2RlOiBUIH1cbmV4cG9ydCB0eXBlIFJhd05vZGVDaGlsZCA9IE5vZGVDaGlsZDxBc3RJZD4gfCBOb2RlQ2hpbGQ8U3luY1Rva2VuSWQ+XG5cbmV4cG9ydCBmdW5jdGlvbiBuZXdFeHRlcm5hbElkKCk6IEV4dGVybmFsSWQge1xuICByZXR1cm4gcmFuZG9tLnV1aWR2NCgpIGFzIEV4dGVybmFsSWRcbn1cblxuLyoqIEBpbnRlcm5hbCAqL1xuZXhwb3J0IGZ1bmN0aW9uIHBhcmVudElkKGFzdDogQXN0KTogQXN0SWQgfCB1bmRlZmluZWQge1xuICByZXR1cm4gYXN0LmZpZWxkcy5nZXQoJ3BhcmVudCcpXG59XG5cbi8qKiBSZXR1cm5zIHRoZSBnaXZlbiBJRHMsIGFuZCB0aGUgSURzIG9mIGFsbCB0aGVpciBhbmNlc3RvcnMuICovXG5leHBvcnQgZnVuY3Rpb24gc3VidHJlZXMobW9kdWxlOiBNb2R1bGUsIGlkczogSXRlcmFibGU8QXN0SWQ+KSB7XG4gIHJldHVybiByZWFjaGFibGUoaWRzLCAoaWQpID0+IHtcbiAgICBjb25zdCBwYXJlbnQgPSBtb2R1bGUudHJ5R2V0KGlkKT8ucGFyZW50KClcbiAgICByZXR1cm4gcGFyZW50ID8gW3BhcmVudC5pZF0gOiBbXVxuICB9KVxufVxuXG4vKiogUmV0dXJucyB0aGUgSURzIG9mIHRoZSBBU1RzIHRoYXQgYXJlIG5vdCBkZXNjZW5kYW50cyBvZiBhbnkgb3RoZXJzIGluIHRoZSBnaXZlbiBzZXQuICovXG5leHBvcnQgZnVuY3Rpb24gc3VidHJlZVJvb3RzKG1vZHVsZTogTW9kdWxlLCBpZHM6IFNldDxBc3RJZD4pOiBTZXQ8QXN0SWQ+IHtcbiAgY29uc3Qgcm9vdHMgPSBuZXcgU2V0PEFzdElkPigpXG4gIGZvciAoY29uc3QgaWQgb2YgaWRzKSB7XG4gICAgY29uc3QgYXN0SW5Nb2R1bGUgPSBtb2R1bGUudHJ5R2V0KGlkKVxuICAgIGlmICghYXN0SW5Nb2R1bGUpIGNvbnRpbnVlXG4gICAgbGV0IGFzdCA9IGFzdEluTW9kdWxlLnBhcmVudCgpXG4gICAgbGV0IGhhc1BhcmVudEluU2V0XG4gICAgd2hpbGUgKGFzdCAhPSBudWxsKSB7XG4gICAgICBpZiAoaWRzLmhhcyhhc3QuaWQpKSB7XG4gICAgICAgIGhhc1BhcmVudEluU2V0ID0gdHJ1ZVxuICAgICAgICBicmVha1xuICAgICAgfVxuICAgICAgYXN0ID0gYXN0LnBhcmVudCgpXG4gICAgfVxuICAgIGlmICghaGFzUGFyZW50SW5TZXQpIHJvb3RzLmFkZChpZClcbiAgfVxuICByZXR1cm4gcm9vdHNcbn1cblxuZnVuY3Rpb24gdW53cmFwR3JvdXBzKGFzdDogQXN0KSB7XG4gIHdoaWxlIChhc3QgaW5zdGFuY2VvZiBHcm91cCAmJiBhc3QuZXhwcmVzc2lvbikgYXN0ID0gYXN0LmV4cHJlc3Npb25cbiAgcmV0dXJuIGFzdFxufVxuXG4vKiogVHJpZXMgdG8gcmVjb2duaXplIGlucHV0cyB0aGF0IGFyZSBzZW1hbnRpY2FsbHktZXF1aXZhbGVudCB0byBhIHNlcXVlbmNlIG9mIGBBcHBgcywgYW5kIHJldHVybnMgdGhlIGFyZ3VtZW50c1xuICogIGlkZW50aWZpZWQgYW5kIExIUyBvZiB0aGUgYW5hbHl6YWJsZSBjaGFpbi5cbiAqXG4gKiAgSW4gcGFydGljdWxhciwgdGhpcyBmdW5jdGlvbiBjdXJyZW50bHkgcmVjb2duaXplcyBzeW50YXggdXNlZCBpbiB2aXN1YWxpemF0aW9uLXByZXByb2Nlc3NvciBleHByZXNzaW9ucy5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGFuYWx5emVBcHBMaWtlKGFzdDogQXN0KTogeyBmdW5jOiBBc3Q7IGFyZ3M6IEFzdFtdIH0ge1xuICBjb25zdCBkZWZlcnJlZE9wZXJhbmRzID0gbmV3IEFycmF5PEFzdD4oKVxuICB3aGlsZSAoXG4gICAgYXN0IGluc3RhbmNlb2YgT3ByQXBwICYmXG4gICAgYXN0Lm9wZXJhdG9yLm9rICYmXG4gICAgYXN0Lm9wZXJhdG9yLnZhbHVlLmNvZGUoKSA9PT0gJzx8JyAmJlxuICAgIGFzdC5saHMgJiZcbiAgICBhc3QucmhzXG4gICkge1xuICAgIGRlZmVycmVkT3BlcmFuZHMucHVzaCh1bndyYXBHcm91cHMoYXN0LnJocykpXG4gICAgYXN0ID0gdW53cmFwR3JvdXBzKGFzdC5saHMpXG4gIH1cbiAgZGVmZXJyZWRPcGVyYW5kcy5yZXZlcnNlKClcbiAgY29uc3QgYXJncyA9IG5ldyBBcnJheTxBc3Q+KClcbiAgd2hpbGUgKGFzdCBpbnN0YW5jZW9mIEFwcCkge1xuICAgIGNvbnN0IGRlZmVycmVkT3BlcmFuZCA9IGFzdC5hcmd1bWVudCBpbnN0YW5jZW9mIFdpbGRjYXJkID8gZGVmZXJyZWRPcGVyYW5kcy5wb3AoKSA6IHVuZGVmaW5lZFxuICAgIGFyZ3MucHVzaChkZWZlcnJlZE9wZXJhbmQgPz8gdW53cmFwR3JvdXBzKGFzdC5hcmd1bWVudCkpXG4gICAgYXN0ID0gYXN0LmZ1bmN0aW9uXG4gIH1cbiAgYXJncy5yZXZlcnNlKClcbiAgcmV0dXJuIHsgZnVuYzogYXN0LCBhcmdzIH1cbn1cbiIsICJjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfZGlybmFtZSA9IFwiQzpcXFxcUHJvamVjdHNcXFxcZW5zb1xcXFxlbnNvXFxcXGFwcFxcXFxndWkyXFxcXHNoYXJlZFxcXFx1dGlsXFxcXGRhdGFcIjtjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfZmlsZW5hbWUgPSBcIkM6XFxcXFByb2plY3RzXFxcXGVuc29cXFxcZW5zb1xcXFxhcHBcXFxcZ3VpMlxcXFxzaGFyZWRcXFxcdXRpbFxcXFxkYXRhXFxcXHJlc3VsdC50c1wiO2NvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9pbXBvcnRfbWV0YV91cmwgPSBcImZpbGU6Ly8vQzovUHJvamVjdHMvZW5zby9lbnNvL2FwcC9ndWkyL3NoYXJlZC91dGlsL2RhdGEvcmVzdWx0LnRzXCI7LyoqIEBmaWxlIEEgZ2VuZXJpYyB0eXBlIHRoYXQgY2FuIGVpdGhlciBob2xkIGEgdmFsdWUgcmVwcmVzZW50aW5nIGEgc3VjY2Vzc2Z1bCByZXN1bHQsXG4gKiBvciBhbiBlcnJvci4gKi9cblxuaW1wb3J0IHsgaXNTb21lLCB0eXBlIE9wdCB9IGZyb20gJy4vb3B0J1xuXG5leHBvcnQgdHlwZSBSZXN1bHQ8VCA9IHVuZGVmaW5lZCwgRSA9IHN0cmluZz4gPVxuICB8IHsgb2s6IHRydWU7IHZhbHVlOiBUIH1cbiAgfCB7IG9rOiBmYWxzZTsgZXJyb3I6IFJlc3VsdEVycm9yPEU+IH1cblxuZXhwb3J0IGZ1bmN0aW9uIE9rPFQ+KGRhdGE6IFQpOiBSZXN1bHQ8VCwgbmV2ZXI+IHtcbiAgcmV0dXJuIHsgb2s6IHRydWUsIHZhbHVlOiBkYXRhIH1cbn1cblxuZXhwb3J0IGZ1bmN0aW9uIEVycjxFPihlcnJvcjogRSk6IFJlc3VsdDxuZXZlciwgRT4ge1xuICByZXR1cm4geyBvazogZmFsc2UsIGVycm9yOiBuZXcgUmVzdWx0RXJyb3IoZXJyb3IpIH1cbn1cblxuZXhwb3J0IGZ1bmN0aW9uIG9rT3I8VCwgRT4oZGF0YTogT3B0PFQ+LCBlcnJvcjogRSk6IFJlc3VsdDxULCBFPiB7XG4gIGlmIChpc1NvbWUoZGF0YSkpIHJldHVybiBPayhkYXRhKVxuICBlbHNlIHJldHVybiBFcnIoZXJyb3IpXG59XG5cbmV4cG9ydCBmdW5jdGlvbiB1bndyYXA8VCwgRT4ocmVzdWx0OiBSZXN1bHQ8VCwgRT4pOiBUIHtcbiAgaWYgKHJlc3VsdC5vaykgcmV0dXJuIHJlc3VsdC52YWx1ZVxuICBlbHNlIHRocm93IHJlc3VsdC5lcnJvclxufVxuXG5leHBvcnQgZnVuY3Rpb24gbWFwT2s8VCwgVSwgRT4ocmVzdWx0OiBSZXN1bHQ8VCwgRT4sIGY6ICh2YWx1ZTogVCkgPT4gVSk6IFJlc3VsdDxVLCBFPiB7XG4gIGlmIChyZXN1bHQub2spIHJldHVybiBPayhmKHJlc3VsdC52YWx1ZSkpXG4gIGVsc2UgcmV0dXJuIHJlc3VsdFxufVxuXG5leHBvcnQgZnVuY3Rpb24gaXNSZXN1bHQodjogdW5rbm93bik6IHYgaXMgUmVzdWx0IHtcbiAgcmV0dXJuIChcbiAgICB2ICE9IG51bGwgJiZcbiAgICB0eXBlb2YgdiA9PT0gJ29iamVjdCcgJiZcbiAgICAnb2snIGluIHYgJiZcbiAgICB0eXBlb2Ygdi5vayA9PT0gJ2Jvb2xlYW4nICYmXG4gICAgKCd2YWx1ZScgaW4gdiB8fCAoJ2Vycm9yJyBpbiB2ICYmIHYuZXJyb3IgaW5zdGFuY2VvZiBSZXN1bHRFcnJvcikpXG4gIClcbn1cblxuZXhwb3J0IGNsYXNzIFJlc3VsdEVycm9yPEUgPSBzdHJpbmc+IHtcbiAgcGF5bG9hZDogRVxuICBjb250ZXh0OiAoKCkgPT4gc3RyaW5nKVtdXG5cbiAgY29uc3RydWN0b3IocGF5bG9hZDogRSkge1xuICAgIHRoaXMucGF5bG9hZCA9IHBheWxvYWRcbiAgICB0aGlzLmNvbnRleHQgPSBbXVxuICB9XG5cbiAgbG9nKHByZWFtYmxlOiBzdHJpbmcgPSAnRXJyb3InKSB7XG4gICAgY29uc29sZS5lcnJvcih0aGlzLm1lc3NhZ2UocHJlYW1ibGUpKVxuICB9XG5cbiAgbWVzc2FnZShwcmVhbWJsZTogc3RyaW5nID0gJ2Vycm9yJykge1xuICAgIGNvbnN0IGN0eCA9XG4gICAgICB0aGlzLmNvbnRleHQubGVuZ3RoID4gMCA/IGBcXG4ke0FycmF5LmZyb20odGhpcy5jb250ZXh0LCAoY3R4KSA9PiBjdHgoKSkuam9pbignXFxuJyl9YCA6ICcnXG4gICAgcmV0dXJuIGAke3ByZWFtYmxlfTogJHt0aGlzLnBheWxvYWR9JHtjdHh9YFxuICB9XG59XG5cbmV4cG9ydCBmdW5jdGlvbiB3aXRoQ29udGV4dDxULCBFPihjb250ZXh0OiAoKSA9PiBzdHJpbmcsIGY6ICgpID0+IFJlc3VsdDxULCBFPik6IFJlc3VsdDxULCBFPiB7XG4gIGNvbnN0IHJlc3VsdCA9IGYoKVxuICBpZiAocmVzdWx0ID09IG51bGwpIHtcbiAgICB0aHJvdyBuZXcgRXJyb3IoJ3dpdGhDb250ZXh0OiBmKCkgcmV0dXJuZWQgbnVsbCBvciB1bmRlZmluZWQnKVxuICB9XG4gIGlmICghcmVzdWx0Lm9rKSByZXN1bHQuZXJyb3IuY29udGV4dC5zcGxpY2UoMCwgMCwgY29udGV4dClcbiAgcmV0dXJuIHJlc3VsdFxufVxuXG4vKipcbiAqIENhdGNoIHByb21pc2UgcmVqZWN0aW9uIG9mIHByb3ZpZGVkIHR5cGVzIGFuZCBjb252ZXJ0IHRoZW0gdG8gYSBSZXN1bHQgdHlwZS5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIHJlamVjdGlvblRvUmVzdWx0PEVycm9yS2luZCBleHRlbmRzIG5ldyAoLi4uYXJnczogYW55W10pID0+IGFueT4oXG4gIGVycm9yS2luZHM6IEVycm9yS2luZCB8IEVycm9yS2luZFtdLFxuKTogPFQ+KHByb21pc2U6IFByb21pc2U8VD4pID0+IFByb21pc2U8UmVzdWx0PFQsIEluc3RhbmNlVHlwZTxFcnJvcktpbmQ+Pj4ge1xuICBjb25zdCBlcnJvcktpbmRBcnJheSA9IEFycmF5LmlzQXJyYXkoZXJyb3JLaW5kcykgPyBlcnJvcktpbmRzIDogW2Vycm9yS2luZHNdXG4gIHJldHVybiBhc3luYyAocHJvbWlzZSkgPT4ge1xuICAgIHRyeSB7XG4gICAgICByZXR1cm4gT2soYXdhaXQgcHJvbWlzZSlcbiAgICB9IGNhdGNoIChlcnJvcikge1xuICAgICAgZm9yIChjb25zdCBlcnJvcktpbmQgb2YgZXJyb3JLaW5kQXJyYXkpIHtcbiAgICAgICAgaWYgKGVycm9yIGluc3RhbmNlb2YgZXJyb3JLaW5kKSByZXR1cm4gRXJyKGVycm9yKVxuICAgICAgfVxuICAgICAgdGhyb3cgZXJyb3JcbiAgICB9XG4gIH1cbn1cbiIsICJjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfZGlybmFtZSA9IFwiQzpcXFxcUHJvamVjdHNcXFxcZW5zb1xcXFxlbnNvXFxcXGFwcFxcXFxndWkyXFxcXHNoYXJlZFwiO2NvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9maWxlbmFtZSA9IFwiQzpcXFxcUHJvamVjdHNcXFxcZW5zb1xcXFxlbnNvXFxcXGFwcFxcXFxndWkyXFxcXHNoYXJlZFxcXFx5anNNb2RlbC50c1wiO2NvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9pbXBvcnRfbWV0YV91cmwgPSBcImZpbGU6Ly8vQzovUHJvamVjdHMvZW5zby9lbnNvL2FwcC9ndWkyL3NoYXJlZC95anNNb2RlbC50c1wiO2ltcG9ydCAqIGFzIG9iamVjdCBmcm9tICdsaWIwL29iamVjdCdcbmltcG9ydCAqIGFzIHJhbmRvbSBmcm9tICdsaWIwL3JhbmRvbSdcbmltcG9ydCAqIGFzIFkgZnJvbSAneWpzJ1xuXG5leHBvcnQgdHlwZSBVdWlkID0gYCR7c3RyaW5nfS0ke3N0cmluZ30tJHtzdHJpbmd9LSR7c3RyaW5nfS0ke3N0cmluZ31gXG5cbmRlY2xhcmUgY29uc3QgYnJhbmRFeHRlcm5hbElkOiB1bmlxdWUgc3ltYm9sXG4vKiogSWRlbnRpZmllcyBhbiBBU1Qgbm9kZSBvciB0b2tlbi4gVXNlZCBpbiBtb2R1bGUgc2VyaWFsaXphdGlvbiBhbmQgY29tbXVuaWNhdGlvbiB3aXRoIHRoZSBsYW5ndWFnZSBzZXJ2ZXIuICovXG5leHBvcnQgdHlwZSBFeHRlcm5hbElkID0gVXVpZCAmIHsgW2JyYW5kRXh0ZXJuYWxJZF06IG5ldmVyIH1cblxuZXhwb3J0IHR5cGUgVmlzdWFsaXphdGlvbk1vZHVsZSA9XG4gIHwgeyBraW5kOiAnQnVpbHRpbicgfVxuICB8IHsga2luZDogJ0N1cnJlbnRQcm9qZWN0JyB9XG4gIHwgeyBraW5kOiAnTGlicmFyeSc7IG5hbWU6IHN0cmluZyB9XG5cbmV4cG9ydCBpbnRlcmZhY2UgVmlzdWFsaXphdGlvbklkZW50aWZpZXIge1xuICBtb2R1bGU6IFZpc3VhbGl6YXRpb25Nb2R1bGVcbiAgbmFtZTogc3RyaW5nXG59XG5cbmV4cG9ydCBpbnRlcmZhY2UgVmlzdWFsaXphdGlvbk1ldGFkYXRhIHtcbiAgaWRlbnRpZmllcjogVmlzdWFsaXphdGlvbklkZW50aWZpZXIgfCBudWxsXG4gIHZpc2libGU6IGJvb2xlYW5cbiAgZnVsbHNjcmVlbjogYm9vbGVhblxuICB3aWR0aDogbnVtYmVyIHwgbnVsbFxufVxuXG5leHBvcnQgZnVuY3Rpb24gdmlzTWV0YWRhdGFFcXVhbHMoXG4gIGE6IFZpc3VhbGl6YXRpb25NZXRhZGF0YSB8IG51bGwgfCB1bmRlZmluZWQsXG4gIGI6IFZpc3VhbGl6YXRpb25NZXRhZGF0YSB8IG51bGwgfCB1bmRlZmluZWQsXG4pIHtcbiAgcmV0dXJuIChcbiAgICAoIWEgJiYgIWIpIHx8XG4gICAgKGEgJiZcbiAgICAgIGIgJiZcbiAgICAgIGEudmlzaWJsZSA9PT0gYi52aXNpYmxlICYmXG4gICAgICBhLmZ1bGxzY3JlZW4gPT0gYi5mdWxsc2NyZWVuICYmXG4gICAgICBhLndpZHRoID09IGIud2lkdGggJiZcbiAgICAgIHZpc0lkZW50aWZpZXJFcXVhbHMoYS5pZGVudGlmaWVyLCBiLmlkZW50aWZpZXIpKVxuICApXG59XG5cbmV4cG9ydCBmdW5jdGlvbiB2aXNJZGVudGlmaWVyRXF1YWxzKFxuICBhOiBWaXN1YWxpemF0aW9uSWRlbnRpZmllciB8IG51bGwgfCB1bmRlZmluZWQsXG4gIGI6IFZpc3VhbGl6YXRpb25JZGVudGlmaWVyIHwgbnVsbCB8IHVuZGVmaW5lZCxcbikge1xuICByZXR1cm4gKCFhICYmICFiKSB8fCAoYSAmJiBiICYmIGEubmFtZSA9PT0gYi5uYW1lICYmIG9iamVjdC5lcXVhbEZsYXQoYS5tb2R1bGUsIGIubW9kdWxlKSlcbn1cblxuZXhwb3J0IHR5cGUgUHJvamVjdFNldHRpbmcgPSBzdHJpbmdcblxuZXhwb3J0IGNsYXNzIERpc3RyaWJ1dGVkUHJvamVjdCB7XG4gIGRvYzogWS5Eb2NcbiAgbmFtZTogWS5UZXh0XG4gIG1vZHVsZXM6IFkuTWFwPFkuRG9jPlxuICBzZXR0aW5nczogWS5NYXA8UHJvamVjdFNldHRpbmc+XG5cbiAgY29uc3RydWN0b3IoZG9jOiBZLkRvYykge1xuICAgIHRoaXMuZG9jID0gZG9jXG4gICAgdGhpcy5uYW1lID0gdGhpcy5kb2MuZ2V0VGV4dCgnbmFtZScpXG4gICAgdGhpcy5tb2R1bGVzID0gdGhpcy5kb2MuZ2V0TWFwKCdtb2R1bGVzJylcbiAgICB0aGlzLnNldHRpbmdzID0gdGhpcy5kb2MuZ2V0TWFwKCdzZXR0aW5ncycpXG4gIH1cblxuICBtb2R1bGVOYW1lcygpOiBzdHJpbmdbXSB7XG4gICAgcmV0dXJuIEFycmF5LmZyb20odGhpcy5tb2R1bGVzLmtleXMoKSlcbiAgfVxuXG4gIGZpbmRNb2R1bGVCeURvY0lkKGlkOiBzdHJpbmcpOiBzdHJpbmcgfCBudWxsIHtcbiAgICBmb3IgKGNvbnN0IFtuYW1lLCBkb2NdIG9mIHRoaXMubW9kdWxlcy5lbnRyaWVzKCkpIHtcbiAgICAgIGlmIChkb2MuZ3VpZCA9PT0gaWQpIHJldHVybiBuYW1lXG4gICAgfVxuICAgIHJldHVybiBudWxsXG4gIH1cblxuICBhc3luYyBvcGVuTW9kdWxlKG5hbWU6IHN0cmluZyk6IFByb21pc2U8RGlzdHJpYnV0ZWRNb2R1bGUgfCBudWxsPiB7XG4gICAgY29uc3QgZG9jID0gdGhpcy5tb2R1bGVzLmdldChuYW1lKVxuICAgIGlmIChkb2MgPT0gbnVsbCkgcmV0dXJuIG51bGxcbiAgICByZXR1cm4gYXdhaXQgRGlzdHJpYnV0ZWRNb2R1bGUubG9hZChkb2MpXG4gIH1cblxuICBvcGVuVW5sb2FkZWRNb2R1bGUobmFtZTogc3RyaW5nKTogRGlzdHJpYnV0ZWRNb2R1bGUgfCBudWxsIHtcbiAgICBjb25zdCBkb2MgPSB0aGlzLm1vZHVsZXMuZ2V0KG5hbWUpXG4gICAgaWYgKGRvYyA9PSBudWxsKSByZXR1cm4gbnVsbFxuICAgIHJldHVybiBuZXcgRGlzdHJpYnV0ZWRNb2R1bGUoZG9jKVxuICB9XG5cbiAgY3JlYXRlVW5sb2FkZWRNb2R1bGUobmFtZTogc3RyaW5nLCBkb2M6IFkuRG9jKTogRGlzdHJpYnV0ZWRNb2R1bGUge1xuICAgIHRoaXMubW9kdWxlcy5zZXQobmFtZSwgZG9jKVxuICAgIHJldHVybiBuZXcgRGlzdHJpYnV0ZWRNb2R1bGUoZG9jKVxuICB9XG5cbiAgY3JlYXRlTmV3TW9kdWxlKG5hbWU6IHN0cmluZyk6IERpc3RyaWJ1dGVkTW9kdWxlIHtcbiAgICByZXR1cm4gdGhpcy5jcmVhdGVVbmxvYWRlZE1vZHVsZShuYW1lLCBuZXcgWS5Eb2MoKSlcbiAgfVxuXG4gIGRlbGV0ZU1vZHVsZShuYW1lOiBzdHJpbmcpOiB2b2lkIHtcbiAgICB0aGlzLm1vZHVsZXMuZGVsZXRlKG5hbWUpXG4gIH1cblxuICBkaXNwb3NlKCk6IHZvaWQge1xuICAgIHRoaXMuZG9jLmRlc3Ryb3koKVxuICB9XG59XG5cbmV4cG9ydCBjbGFzcyBNb2R1bGVEb2Mge1xuICB5ZG9jOiBZLkRvY1xuICBub2RlczogWS5NYXA8YW55PlxuICBjb25zdHJ1Y3Rvcih5ZG9jOiBZLkRvYykge1xuICAgIHRoaXMueWRvYyA9IHlkb2NcbiAgICB0aGlzLm5vZGVzID0geWRvYy5nZXRNYXAoJ25vZGVzJylcbiAgfVxufVxuXG5leHBvcnQgY2xhc3MgRGlzdHJpYnV0ZWRNb2R1bGUge1xuICBkb2M6IE1vZHVsZURvY1xuICB1bmRvTWFuYWdlcjogWS5VbmRvTWFuYWdlclxuXG4gIHN0YXRpYyBhc3luYyBsb2FkKHlkb2M6IFkuRG9jKTogUHJvbWlzZTxEaXN0cmlidXRlZE1vZHVsZT4ge1xuICAgIHlkb2MubG9hZCgpXG4gICAgYXdhaXQgeWRvYy53aGVuTG9hZGVkXG4gICAgcmV0dXJuIG5ldyBEaXN0cmlidXRlZE1vZHVsZSh5ZG9jKVxuICB9XG5cbiAgY29uc3RydWN0b3IoeWRvYzogWS5Eb2MpIHtcbiAgICB0aGlzLmRvYyA9IG5ldyBNb2R1bGVEb2MoeWRvYylcbiAgICB0aGlzLnVuZG9NYW5hZ2VyID0gbmV3IFkuVW5kb01hbmFnZXIoW3RoaXMuZG9jLm5vZGVzXSlcbiAgfVxuXG4gIGRpc3Bvc2UoKTogdm9pZCB7XG4gICAgdGhpcy5kb2MueWRvYy5kZXN0cm95KClcbiAgfVxufVxuXG5leHBvcnQgY29uc3QgbG9jYWxVc2VyQWN0aW9uT3JpZ2lucyA9IFsnbG9jYWw6dXNlckFjdGlvbicsICdsb2NhbDp1c2VyQWN0aW9uOkNvZGVFZGl0b3InXSBhcyBjb25zdFxuZXhwb3J0IHR5cGUgTG9jYWxVc2VyQWN0aW9uT3JpZ2luID0gKHR5cGVvZiBsb2NhbFVzZXJBY3Rpb25PcmlnaW5zKVtudW1iZXJdXG5leHBvcnQgdHlwZSBPcmlnaW4gPSBMb2NhbFVzZXJBY3Rpb25PcmlnaW4gfCAncmVtb3RlJyB8ICdsb2NhbDphdXRvTGF5b3V0J1xuLyoqIExvY2FsbHktb3JpZ2luYXRlZCBjaGFuZ2VzIG5vdCBvdGhlcndpc2Ugc3BlY2lmaWVkLiAqL1xuZXhwb3J0IGNvbnN0IGRlZmF1bHRMb2NhbE9yaWdpbjogTG9jYWxVc2VyQWN0aW9uT3JpZ2luID0gJ2xvY2FsOnVzZXJBY3Rpb24nXG5leHBvcnQgZnVuY3Rpb24gaXNMb2NhbFVzZXJBY3Rpb25PcmlnaW4ob3JpZ2luOiBzdHJpbmcpOiBvcmlnaW4gaXMgTG9jYWxVc2VyQWN0aW9uT3JpZ2luIHtcbiAgY29uc3QgbG9jYWxPcmlnaW5OYW1lczogcmVhZG9ubHkgc3RyaW5nW10gPSBsb2NhbFVzZXJBY3Rpb25PcmlnaW5zXG4gIHJldHVybiBsb2NhbE9yaWdpbk5hbWVzLmluY2x1ZGVzKG9yaWdpbilcbn1cbmV4cG9ydCBmdW5jdGlvbiB0cnlBc09yaWdpbihvcmlnaW46IHN0cmluZyk6IE9yaWdpbiB8IHVuZGVmaW5lZCB7XG4gIGlmIChpc0xvY2FsVXNlckFjdGlvbk9yaWdpbihvcmlnaW4pKSByZXR1cm4gb3JpZ2luXG4gIGlmIChvcmlnaW4gPT09ICdsb2NhbDphdXRvTGF5b3V0JykgcmV0dXJuIG9yaWdpblxuICBpZiAob3JpZ2luID09PSAncmVtb3RlJykgcmV0dXJuIG9yaWdpblxufVxuXG5leHBvcnQgdHlwZSBTb3VyY2VSYW5nZSA9IHJlYWRvbmx5IFtzdGFydDogbnVtYmVyLCBlbmQ6IG51bWJlcl1cbmRlY2xhcmUgY29uc3QgYnJhbmRTb3VyY2VSYW5nZUtleTogdW5pcXVlIHN5bWJvbFxuZXhwb3J0IHR5cGUgU291cmNlUmFuZ2VLZXkgPSBzdHJpbmcgJiB7IFticmFuZFNvdXJjZVJhbmdlS2V5XTogbmV2ZXIgfVxuXG5leHBvcnQgZnVuY3Rpb24gc291cmNlUmFuZ2VLZXkocmFuZ2U6IFNvdXJjZVJhbmdlKTogU291cmNlUmFuZ2VLZXkge1xuICByZXR1cm4gYCR7cmFuZ2VbMF0udG9TdHJpbmcoMTYpfToke3JhbmdlWzFdLnRvU3RyaW5nKDE2KX1gIGFzIFNvdXJjZVJhbmdlS2V5XG59XG5leHBvcnQgZnVuY3Rpb24gc291cmNlUmFuZ2VGcm9tS2V5KGtleTogU291cmNlUmFuZ2VLZXkpOiBTb3VyY2VSYW5nZSB7XG4gIHJldHVybiBrZXkuc3BsaXQoJzonKS5tYXAoKHgpID0+IHBhcnNlSW50KHgsIDE2KSkgYXMgW251bWJlciwgbnVtYmVyXVxufVxuXG5leHBvcnQgY2xhc3MgSWRNYXAge1xuICBwcml2YXRlIHJlYWRvbmx5IHJhbmdlVG9FeHByOiBNYXA8c3RyaW5nLCBFeHRlcm5hbElkPlxuXG4gIGNvbnN0cnVjdG9yKGVudHJpZXM/OiBbc3RyaW5nLCBFeHRlcm5hbElkXVtdKSB7XG4gICAgdGhpcy5yYW5nZVRvRXhwciA9IG5ldyBNYXAoZW50cmllcyA/PyBbXSlcbiAgfVxuXG4gIHN0YXRpYyBNb2NrKCk6IElkTWFwIHtcbiAgICByZXR1cm4gbmV3IElkTWFwKFtdKVxuICB9XG5cbiAgaW5zZXJ0S25vd25JZChyYW5nZTogU291cmNlUmFuZ2UsIGlkOiBFeHRlcm5hbElkKSB7XG4gICAgY29uc3Qga2V5ID0gc291cmNlUmFuZ2VLZXkocmFuZ2UpXG4gICAgdGhpcy5yYW5nZVRvRXhwci5zZXQoa2V5LCBpZClcbiAgfVxuXG4gIGdldElmRXhpc3QocmFuZ2U6IFNvdXJjZVJhbmdlKTogRXh0ZXJuYWxJZCB8IHVuZGVmaW5lZCB7XG4gICAgY29uc3Qga2V5ID0gc291cmNlUmFuZ2VLZXkocmFuZ2UpXG4gICAgcmV0dXJuIHRoaXMucmFuZ2VUb0V4cHIuZ2V0KGtleSlcbiAgfVxuXG4gIGdldE9ySW5zZXJ0VW5pcXVlSWQocmFuZ2U6IFNvdXJjZVJhbmdlKTogRXh0ZXJuYWxJZCB7XG4gICAgY29uc3Qga2V5ID0gc291cmNlUmFuZ2VLZXkocmFuZ2UpXG4gICAgY29uc3QgdmFsID0gdGhpcy5yYW5nZVRvRXhwci5nZXQoa2V5KVxuICAgIGlmICh2YWwgIT09IHVuZGVmaW5lZCkge1xuICAgICAgcmV0dXJuIHZhbFxuICAgIH0gZWxzZSB7XG4gICAgICBjb25zdCBuZXdJZCA9IHJhbmRvbS51dWlkdjQoKSBhcyBFeHRlcm5hbElkXG4gICAgICB0aGlzLnJhbmdlVG9FeHByLnNldChrZXksIG5ld0lkKVxuICAgICAgcmV0dXJuIG5ld0lkXG4gICAgfVxuICB9XG5cbiAgZW50cmllcygpOiBbU291cmNlUmFuZ2VLZXksIEV4dGVybmFsSWRdW10ge1xuICAgIHJldHVybiBbLi4udGhpcy5yYW5nZVRvRXhwcl0gYXMgW1NvdXJjZVJhbmdlS2V5LCBFeHRlcm5hbElkXVtdXG4gIH1cblxuICBnZXQgc2l6ZSgpOiBudW1iZXIge1xuICAgIHJldHVybiB0aGlzLnJhbmdlVG9FeHByLnNpemVcbiAgfVxuXG4gIGNsZWFyKCk6IHZvaWQge1xuICAgIHRoaXMucmFuZ2VUb0V4cHIuY2xlYXIoKVxuICB9XG5cbiAgaXNFcXVhbChvdGhlcjogSWRNYXApOiBib29sZWFuIHtcbiAgICBpZiAob3RoZXIuc2l6ZSAhPT0gdGhpcy5zaXplKSByZXR1cm4gZmFsc2VcbiAgICBmb3IgKGNvbnN0IFtrZXksIHZhbHVlXSBvZiB0aGlzLnJhbmdlVG9FeHByLmVudHJpZXMoKSkge1xuICAgICAgY29uc3Qgb2xkVmFsdWUgPSBvdGhlci5yYW5nZVRvRXhwci5nZXQoa2V5KVxuICAgICAgaWYgKG9sZFZhbHVlICE9PSB2YWx1ZSkgcmV0dXJuIGZhbHNlXG4gICAgfVxuICAgIHJldHVybiB0cnVlXG4gIH1cblxuICB2YWxpZGF0ZSgpIHtcbiAgICBjb25zdCB1bmlxdWVWYWx1ZXMgPSBuZXcgU2V0KHRoaXMucmFuZ2VUb0V4cHIudmFsdWVzKCkpXG4gICAgaWYgKHVuaXF1ZVZhbHVlcy5zaXplIDwgdGhpcy5yYW5nZVRvRXhwci5zaXplKSB7XG4gICAgICBjb25zb2xlLndhcm4oYER1cGxpY2F0ZSBVVUlEIGluIElkTWFwYClcbiAgICB9XG4gIH1cblxuICBjbG9uZSgpOiBJZE1hcCB7XG4gICAgcmV0dXJuIG5ldyBJZE1hcCh0aGlzLmVudHJpZXMoKSlcbiAgfVxuXG4gIC8vIERlYnVnZ2luZy5cbiAgY29tcGFyZShvdGhlcjogSWRNYXApIHtcbiAgICBjb25zb2xlLmluZm8oYElkTWFwLmNvbXBhcmUgLS0tLS0tLWApXG4gICAgY29uc3QgYWxsS2V5cyA9IG5ldyBTZXQ8c3RyaW5nPigpXG4gICAgZm9yIChjb25zdCBrZXkgb2YgdGhpcy5yYW5nZVRvRXhwci5rZXlzKCkpIGFsbEtleXMuYWRkKGtleSlcbiAgICBmb3IgKGNvbnN0IGtleSBvZiBvdGhlci5yYW5nZVRvRXhwci5rZXlzKCkpIGFsbEtleXMuYWRkKGtleSlcbiAgICBmb3IgKGNvbnN0IGtleSBvZiBhbGxLZXlzKSB7XG4gICAgICBjb25zdCBtaW5lID0gdGhpcy5yYW5nZVRvRXhwci5nZXQoa2V5KVxuICAgICAgY29uc3QgeW91cnMgPSBvdGhlci5yYW5nZVRvRXhwci5nZXQoa2V5KVxuICAgICAgaWYgKG1pbmUgIT09IHlvdXJzKSB7XG4gICAgICAgIGNvbnNvbGUuaW5mbyhgSWRNYXAuY29tcGFyZVske2tleX1dOiAke21pbmV9IC0+ICR7eW91cnN9YClcbiAgICAgIH1cbiAgICB9XG4gIH1cbn1cblxuY29uc3QgdXVpZFJlZ2V4ID0gL15bMC05YS1mXXs4fS0oPzpbMC05YS1mXXs0fS0pezN9WzAtOWEtZl17MTJ9JC9cbmV4cG9ydCBmdW5jdGlvbiBpc1V1aWQoeDogdW5rbm93bik6IHggaXMgVXVpZCB7XG4gIHJldHVybiB0eXBlb2YgeCA9PT0gJ3N0cmluZycgJiYgeC5sZW5ndGggPT09IDM2ICYmIHV1aWRSZWdleC50ZXN0KHgpXG59XG5cbmV4cG9ydCBmdW5jdGlvbiByYW5nZUVxdWFscyhhOiBTb3VyY2VSYW5nZSwgYjogU291cmNlUmFuZ2UpOiBib29sZWFuIHtcbiAgcmV0dXJuIGFbMF0gPT0gYlswXSAmJiBhWzFdID09IGJbMV1cbn1cblxuZXhwb3J0IGZ1bmN0aW9uIHJhbmdlSW5jbHVkZXMoYTogU291cmNlUmFuZ2UsIGI6IG51bWJlcik6IGJvb2xlYW4ge1xuICByZXR1cm4gYVswXSA8PSBiICYmIGFbMV0gPj0gYlxufVxuXG5leHBvcnQgZnVuY3Rpb24gcmFuZ2VMZW5ndGgoYTogU291cmNlUmFuZ2UpOiBudW1iZXIge1xuICByZXR1cm4gYVsxXSAtIGFbMF1cbn1cblxuZXhwb3J0IGZ1bmN0aW9uIHJhbmdlRW5jbG9zZXMoYTogU291cmNlUmFuZ2UsIGI6IFNvdXJjZVJhbmdlKTogYm9vbGVhbiB7XG4gIHJldHVybiBhWzBdIDw9IGJbMF0gJiYgYVsxXSA+PSBiWzFdXG59XG5cbmV4cG9ydCBmdW5jdGlvbiByYW5nZUludGVyc2VjdHMoYTogU291cmNlUmFuZ2UsIGI6IFNvdXJjZVJhbmdlKTogYm9vbGVhbiB7XG4gIHJldHVybiBhWzBdIDw9IGJbMV0gJiYgYVsxXSA+PSBiWzBdXG59XG5cbi8qKiBXaGV0aGVyIHRoZSBnaXZlbiByYW5nZSBpcyBiZWZvcmUgdGhlIG90aGVyIHJhbmdlLiAqL1xuZXhwb3J0IGZ1bmN0aW9uIHJhbmdlSXNCZWZvcmUoYTogU291cmNlUmFuZ2UsIGI6IFNvdXJjZVJhbmdlKTogYm9vbGVhbiB7XG4gIHJldHVybiBhWzFdIDw9IGJbMF1cbn1cbiIsICJjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfZGlybmFtZSA9IFwiQzpcXFxcUHJvamVjdHNcXFxcZW5zb1xcXFxlbnNvXFxcXGFwcFxcXFxndWkyXFxcXHNoYXJlZFxcXFxhc3RcIjtjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfZmlsZW5hbWUgPSBcIkM6XFxcXFByb2plY3RzXFxcXGVuc29cXFxcZW5zb1xcXFxhcHBcXFxcZ3VpMlxcXFxzaGFyZWRcXFxcYXN0XFxcXHBhcnNlclN1cHBvcnQudHNcIjtjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfaW1wb3J0X21ldGFfdXJsID0gXCJmaWxlOi8vL0M6L1Byb2plY3RzL2Vuc28vZW5zby9hcHAvZ3VpMi9zaGFyZWQvYXN0L3BhcnNlclN1cHBvcnQudHNcIjsvKiogVGhpcyBmaWxlIHN1cHBvcnRzIHRoZSBtb2R1bGUgaW4gYGdlbmVyYXRlZC9hc3QudHNgIHRoYXQgaXMgcHJvZHVjZWQgYnkgYHBhcnNlci1jb2RlZ2VuYC4gKi9cblxuZXhwb3J0IHsgdHlwZSBSZXN1bHQgfSBmcm9tICcuLi91dGlsL2RhdGEvcmVzdWx0J1xuaW1wb3J0IHsgYmFpbCB9IGZyb20gJy4uL3V0aWwvYXNzZXJ0J1xuaW1wb3J0IHsgRXJyLCBPaywgdHlwZSBSZXN1bHQgfSBmcm9tICcuLi91dGlsL2RhdGEvcmVzdWx0J1xuXG5leHBvcnQgdHlwZSBPYmplY3RWaXNpdG9yID0gKG9iamVjdDogTGF6eU9iamVjdCkgPT4gYm9vbGVhbiB8IHZvaWRcbmV4cG9ydCB0eXBlIE9iamVjdEFkZHJlc3NWaXNpdG9yID0gKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpID0+IGJvb2xlYW4gfCB2b2lkXG5cbi8qKiBCYXNlIGNsYXNzIGZvciBvYmplY3RzIHRoYXQgbGF6aWx5IGRlc2VyaWFsaXplIGZpZWxkcyB3aGVuIGFjY2Vzc2VkLiAqL1xuZXhwb3J0IGFic3RyYWN0IGNsYXNzIExhenlPYmplY3Qge1xuICBwcm90ZWN0ZWQgcmVhZG9ubHkgX3Y6IERhdGFWaWV3XG5cbiAgcHJvdGVjdGVkIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7XG4gICAgaWYgKHZpZXcgPT0gbnVsbCkgdGhyb3cgbmV3IEVycm9yKCdXVEY/JylcbiAgICB0aGlzLl92ID0gdmlld1xuICB9XG5cbiAgdmlzaXRDaGlsZHJlbihfdmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4ge1xuICAgIHJldHVybiBmYWxzZVxuICB9XG5cbiAgY2hpbGRyZW4oKTogTGF6eU9iamVjdFtdIHtcbiAgICBjb25zdCBjaGlsZHJlbjogTGF6eU9iamVjdFtdID0gW11cbiAgICB0aGlzLnZpc2l0Q2hpbGRyZW4oKGNoaWxkKSA9PiB7XG4gICAgICBjaGlsZHJlbi5wdXNoKGNoaWxkKVxuICAgIH0pXG4gICAgcmV0dXJuIGNoaWxkcmVuXG4gIH1cbn1cblxudHlwZSBSZWFkZXI8VD4gPSAodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcikgPT4gVFxuXG5mdW5jdGlvbiBtYWtlRGF0YVZpZXcoYnVmZmVyOiBBcnJheUJ1ZmZlciwgYWRkcmVzczogbnVtYmVyKSB7XG4gIHJldHVybiBuZXcgRGF0YVZpZXcoYnVmZmVyLCBhZGRyZXNzKVxufVxuXG5leHBvcnQgZnVuY3Rpb24gcmVhZFU4KHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpIHtcbiAgcmV0dXJuIHZpZXcuZ2V0VWludDgoYWRkcmVzcylcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIHJlYWRVMzIodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcikge1xuICByZXR1cm4gdmlldy5nZXRVaW50MzIoYWRkcmVzcywgdHJ1ZSlcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIHJlYWRJMzIodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcikge1xuICByZXR1cm4gdmlldy5nZXRJbnQzMihhZGRyZXNzLCB0cnVlKVxufVxuXG5leHBvcnQgZnVuY3Rpb24gcmVhZFU2NCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKSB7XG4gIHJldHVybiB2aWV3LmdldEJpZ1VpbnQ2NChhZGRyZXNzLCB0cnVlKVxufVxuXG5leHBvcnQgZnVuY3Rpb24gcmVhZEk2NCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKSB7XG4gIHJldHVybiB2aWV3LmdldEJpZ0ludDY0KGFkZHJlc3MsIHRydWUpXG59XG5cbmV4cG9ydCBmdW5jdGlvbiByZWFkQm9vbCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKSB7XG4gIHJldHVybiByZWFkVTgodmlldywgYWRkcmVzcykgIT09IDBcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIHJlYWRPZmZzZXQodmlldzogRGF0YVZpZXcsIG9mZnNldDogbnVtYmVyKSB7XG4gIHJldHVybiBtYWtlRGF0YVZpZXcodmlldy5idWZmZXIsIHZpZXcuYnl0ZU9mZnNldCArIG9mZnNldClcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIHJlYWRQb2ludGVyKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBEYXRhVmlldyB7XG4gIHJldHVybiBtYWtlRGF0YVZpZXcodmlldy5idWZmZXIsIHJlYWRVMzIodmlldywgYWRkcmVzcykpXG59XG5cbmNvbnN0IHRleHREZWNvZGVyID0gbmV3IFRleHREZWNvZGVyKClcblxuZXhwb3J0IGZ1bmN0aW9uIHJlYWRPcHRpb248VD4oXG4gIHZpZXc6IERhdGFWaWV3LFxuICBhZGRyZXNzOiBudW1iZXIsXG4gIHJlYWRFbGVtZW50OiBSZWFkZXI8VD4sXG4pOiBUIHwgdW5kZWZpbmVkIHtcbiAgbGV0IHJlc3VsdCA9IHVuZGVmaW5lZFxuICB2aXNpdE9wdGlvbih2aWV3LCBhZGRyZXNzLCAodmlldywgYWRkcmVzcykgPT4ge1xuICAgIHJlc3VsdCA9IHJlYWRFbGVtZW50KHZpZXcsIGFkZHJlc3MpXG4gIH0pXG4gIHJldHVybiByZXN1bHRcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIHZpc2l0T3B0aW9uKFxuICB2aWV3OiBEYXRhVmlldyxcbiAgYWRkcmVzczogbnVtYmVyLFxuICB2aXNpdG9yOiBPYmplY3RBZGRyZXNzVmlzaXRvcixcbik6IGJvb2xlYW4ge1xuICBjb25zdCBkaXNjcmltaW5hbnQgPSByZWFkVTgodmlldywgYWRkcmVzcylcbiAgc3dpdGNoIChkaXNjcmltaW5hbnQpIHtcbiAgICBjYXNlIDA6XG4gICAgICByZXR1cm4gZmFsc2VcbiAgICBjYXNlIDE6XG4gICAgICByZXR1cm4gISF2aXNpdG9yKHJlYWRQb2ludGVyKHZpZXcsIGFkZHJlc3MgKyAxKSwgMClcbiAgICBkZWZhdWx0OlxuICAgICAgdGhyb3cgbmV3IEVycm9yKGBJbnZhbGlkIE9wdGlvbiBkaXNjcmltaW5hbnQ6IDB4JHtkaXNjcmltaW5hbnQudG9TdHJpbmcoMTYpfS5gKVxuICB9XG59XG5cbmV4cG9ydCBmdW5jdGlvbiByZWFkUmVzdWx0PE9rLCBFcnI+KFxuICB2aWV3OiBEYXRhVmlldyxcbiAgYWRkcmVzczogbnVtYmVyLFxuICByZWFkT2s6IFJlYWRlcjxPaz4sXG4gIHJlYWRFcnI6IFJlYWRlcjxFcnI+LFxuKTogUmVzdWx0PE9rLCBFcnI+IHtcbiAgY29uc3QgZGF0YSA9IHJlYWRQb2ludGVyKHZpZXcsIGFkZHJlc3MpXG4gIGNvbnN0IGRpc2NyaW1pbmFudCA9IHJlYWRVMzIoZGF0YSwgMClcbiAgc3dpdGNoIChkaXNjcmltaW5hbnQpIHtcbiAgICBjYXNlIDA6XG4gICAgICByZXR1cm4gT2socmVhZE9rKGRhdGEsIDQpKVxuICAgIGNhc2UgMTpcbiAgICAgIHJldHVybiBFcnIocmVhZEVycihkYXRhLCA0KSlcbiAgICBkZWZhdWx0OlxuICAgICAgdGhyb3cgbmV3IEVycm9yKGBJbnZhbGlkIFJlc3VsdCBkaXNjcmltaW5hbnQ6IDB4JHtkaXNjcmltaW5hbnQudG9TdHJpbmcoMTYpfS5gKVxuICB9XG59XG5cbmV4cG9ydCBmdW5jdGlvbiB2aXNpdFJlc3VsdChcbiAgdmlldzogRGF0YVZpZXcsXG4gIGFkZHJlc3M6IG51bWJlcixcbiAgdmlzaXRPazogT2JqZWN0QWRkcmVzc1Zpc2l0b3IgfCBudWxsLFxuICB2aXNpdEVycjogT2JqZWN0QWRkcmVzc1Zpc2l0b3IgfCBudWxsLFxuKTogYm9vbGVhbiB7XG4gIGNvbnN0IGRhdGEgPSByZWFkUG9pbnRlcih2aWV3LCBhZGRyZXNzKVxuICBjb25zdCBkaXNjcmltaW5hbnQgPSByZWFkVTMyKGRhdGEsIDApXG4gIHN3aXRjaCAoZGlzY3JpbWluYW50KSB7XG4gICAgY2FzZSAwOlxuICAgICAgaWYgKHZpc2l0T2s/LihkYXRhLCA0KSkgcmV0dXJuIHRydWVcbiAgICAgIHJldHVybiBmYWxzZVxuICAgIGNhc2UgMTpcbiAgICAgIGlmICh2aXNpdEVycj8uKGRhdGEsIDQpKSByZXR1cm4gdHJ1ZVxuICAgICAgcmV0dXJuIGZhbHNlXG4gICAgZGVmYXVsdDpcbiAgICAgIHRocm93IG5ldyBFcnJvcihgSW52YWxpZCBSZXN1bHQgZGlzY3JpbWluYW50OiAweCR7ZGlzY3JpbWluYW50LnRvU3RyaW5nKDE2KX0uYClcbiAgfVxufVxuXG5leHBvcnQgZnVuY3Rpb24gdmlzaXRTZXF1ZW5jZShcbiAgdmlldzogRGF0YVZpZXcsXG4gIGFkZHJlc3M6IG51bWJlcixcbiAgc2l6ZTogbnVtYmVyLFxuICB2aXNpdG9yOiBPYmplY3RBZGRyZXNzVmlzaXRvcixcbik6IGJvb2xlYW4ge1xuICBjb25zdCBkYXRhID0gcmVhZFBvaW50ZXIodmlldywgYWRkcmVzcylcbiAgbGV0IG9mZnNldCA9IDRcbiAgY29uc3QgZW5kID0gb2Zmc2V0ICsgc2l6ZSAqIHJlYWRVMzIoZGF0YSwgMClcbiAgd2hpbGUgKG9mZnNldCAhPSBlbmQpIHtcbiAgICBpZiAodmlzaXRvcihkYXRhLCBvZmZzZXQpID09PSB0cnVlKSByZXR1cm4gdHJ1ZVxuICAgIG9mZnNldCArPSBzaXplXG4gIH1cbiAgcmV0dXJuIGZhbHNlXG59XG5cbmV4cG9ydCBmdW5jdGlvbiByZWFkU2VxdWVuY2U8VD4oXG4gIHZpZXc6IERhdGFWaWV3LFxuICBhZGRyZXNzOiBudW1iZXIsXG4gIHNpemU6IG51bWJlcixcbiAgcmVhZGVyOiBSZWFkZXI8VD4sXG4pOiBJdGVyYWJsZUl0ZXJhdG9yPFQ+IHtcbiAgY29uc3QgZGF0YSA9IHJlYWRQb2ludGVyKHZpZXcsIGFkZHJlc3MpXG4gIGNvbnN0IG9mZnNldCA9IDRcbiAgY29uc3QgZW5kID0gb2Zmc2V0ICsgc2l6ZSAqIHJlYWRVMzIoZGF0YSwgMClcbiAgcmV0dXJuIG5ldyBMYXp5U2VxdWVuY2Uob2Zmc2V0LCBzaXplLCBlbmQsIChvZmZzZXQ6IG51bWJlcikgPT4gcmVhZGVyKGRhdGEsIG9mZnNldCkpXG59XG5cbmV4cG9ydCBjbGFzcyBMYXp5U2VxdWVuY2U8VD4gaW1wbGVtZW50cyBJdGVyYWJsZUl0ZXJhdG9yPFQ+IHtcbiAgcHJpdmF0ZSBvZmZzZXQ6IG51bWJlclxuICBwcml2YXRlIHJlYWRvbmx5IHN0ZXA6IG51bWJlclxuICBwcml2YXRlIHJlYWRvbmx5IGVuZDogbnVtYmVyXG4gIHByaXZhdGUgcmVhZG9ubHkgcmVhZDogKGFkZHJlc3M6IG51bWJlcikgPT4gVFxuXG4gIGNvbnN0cnVjdG9yKG9mZnNldDogbnVtYmVyLCBzdGVwOiBudW1iZXIsIGVuZDogbnVtYmVyLCByZWFkOiAoYWRkcmVzczogbnVtYmVyKSA9PiBUKSB7XG4gICAgdGhpcy5yZWFkID0gcmVhZFxuICAgIHRoaXMub2Zmc2V0ID0gb2Zmc2V0XG4gICAgdGhpcy5zdGVwID0gc3RlcFxuICAgIHRoaXMuZW5kID0gZW5kXG4gIH1cblxuICBbU3ltYm9sLml0ZXJhdG9yXSgpIHtcbiAgICByZXR1cm4gdGhpc1xuICB9XG5cbiAgcHVibGljIG5leHQoKTogSXRlcmF0b3JSZXN1bHQ8VD4ge1xuICAgIGlmICh0aGlzLm9mZnNldCA+PSB0aGlzLmVuZCkge1xuICAgICAgcmV0dXJuIHsgZG9uZTogdHJ1ZSwgdmFsdWU6IHVuZGVmaW5lZCB9XG4gICAgfVxuICAgIGNvbnN0IHZhbHVlID0gdGhpcy5yZWFkKHRoaXMub2Zmc2V0KVxuICAgIHRoaXMub2Zmc2V0ICs9IHRoaXMuc3RlcFxuICAgIHJldHVybiB7IGRvbmU6IGZhbHNlLCB2YWx1ZTogdmFsdWUgfVxuICB9XG59XG5cbmV4cG9ydCBmdW5jdGlvbiByZWFkU3RyaW5nKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBzdHJpbmcge1xuICBjb25zdCBkYXRhID0gcmVhZFBvaW50ZXIodmlldywgYWRkcmVzcylcbiAgY29uc3QgbGVuID0gcmVhZFUzMihkYXRhLCAwKVxuICBjb25zdCBieXRlcyA9IG5ldyBVaW50OEFycmF5KGRhdGEuYnVmZmVyLCBkYXRhLmJ5dGVPZmZzZXQgKyA0LCBsZW4pXG4gIHJldHVybiB0ZXh0RGVjb2Rlci5kZWNvZGUoYnl0ZXMpXG59XG5cbmV4cG9ydCBmdW5jdGlvbiByZWFkRW51bTxUPihyZWFkZXJzOiBSZWFkZXI8VD5bXSwgdmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IFQge1xuICBjb25zdCBkYXRhID0gcmVhZFBvaW50ZXIodmlldywgYWRkcmVzcylcbiAgY29uc3QgZGlzY3JpbWluYW50ID0gcmVhZFUzMihkYXRhLCAwKVxuICBjb25zdCByZWFkZXIgPSByZWFkZXJzW2Rpc2NyaW1pbmFudF0gPz8gYmFpbChgSW52YWxpZCBlbnVtIGRpc2NyaW1pbmFudDogJHtkaXNjcmltaW5hbnR9YClcbiAgcmV0dXJuIHJlYWRlcihkYXRhLCA0KVxufVxuIiwgImNvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9kaXJuYW1lID0gXCJDOlxcXFxQcm9qZWN0c1xcXFxlbnNvXFxcXGVuc29cXFxcYXBwXFxcXGd1aTJcXFxcc2hhcmVkXFxcXGFzdFxcXFxnZW5lcmF0ZWRcIjtjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfZmlsZW5hbWUgPSBcIkM6XFxcXFByb2plY3RzXFxcXGVuc29cXFxcZW5zb1xcXFxhcHBcXFxcZ3VpMlxcXFxzaGFyZWRcXFxcYXN0XFxcXGdlbmVyYXRlZFxcXFxhc3QudHNcIjtjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfaW1wb3J0X21ldGFfdXJsID0gXCJmaWxlOi8vL0M6L1Byb2plY3RzL2Vuc28vZW5zby9hcHAvZ3VpMi9zaGFyZWQvYXN0L2dlbmVyYXRlZC9hc3QudHNcIjsvLyAqKiogVEhJUyBGSUxFIEdFTkVSQVRFRCBCWSBgcGFyc2VyLWNvZGVnZW5gICoqKlxuaW1wb3J0IHsgTGF6eU9iamVjdCwgdHlwZSBPYmplY3RWaXNpdG9yLCB0eXBlIE9iamVjdEFkZHJlc3NWaXNpdG9yLCB0eXBlIFJlc3VsdCwgcmVhZFU4LCByZWFkVTMyLCByZWFkSTMyLCByZWFkVTY0LCByZWFkSTY0LCByZWFkQm9vbCwgcmVhZE9mZnNldCwgcmVhZFBvaW50ZXIsIHJlYWRPcHRpb24sIHJlYWRSZXN1bHQsIHJlYWRFbnVtLCByZWFkU2VxdWVuY2UsIHJlYWRTdHJpbmcsIHZpc2l0U2VxdWVuY2UsIHZpc2l0T3B0aW9uLCB2aXNpdFJlc3VsdCB9IGZyb20gJy4uL3BhcnNlclN1cHBvcnQnXG5leHBvcnQgbW9kdWxlIFRyZWUge1xuICAgIGV4cG9ydCBhYnN0cmFjdCBjbGFzcyBBYnN0cmFjdEJhc2UgZXh0ZW5kcyBMYXp5T2JqZWN0IHtcbiAgICAgICAgcHJvdGVjdGVkIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB9XG4gICAgICAgIGdldCBzcGFuTGVmdE9mZnNldFZpc2libGUoKTogbnVtYmVyIHsgcmV0dXJuIHJlYWRVMzIodGhpcy5fdiwgMCk7IH1cbiAgICAgICAgZ2V0IHNwYW5MZWZ0T2Zmc2V0Q29kZVJlcHJCZWdpbigpOiBudW1iZXIgeyByZXR1cm4gcmVhZFUzMih0aGlzLl92LCA0KTsgfVxuICAgICAgICBnZXQgc3BhbkxlZnRPZmZzZXRDb2RlUmVwckxlbigpOiBudW1iZXIgeyByZXR1cm4gcmVhZFUzMih0aGlzLl92LCA4KTsgfVxuICAgICAgICBnZXQgc3BhbkxlZnRPZmZzZXRDb2RlU3RhcnRVdGY4KCk6IG51bWJlciB7IHJldHVybiByZWFkVTMyKHRoaXMuX3YsIDEyKTsgfVxuICAgICAgICBnZXQgd2hpdGVzcGFjZVN0YXJ0SW5Db2RlUGFyc2VkKCk6IG51bWJlciB7IHJldHVybiByZWFkVTMyKHRoaXMuX3YsIDE2KTsgfVxuICAgICAgICBnZXQgc3BhbkxlZnRPZmZzZXRDb2RlU3RhcnRMaW5lKCk6IG51bWJlciB7IHJldHVybiByZWFkVTMyKHRoaXMuX3YsIDIwKTsgfVxuICAgICAgICBnZXQgc3BhbkxlZnRPZmZzZXRDb2RlU3RhcnRDb2wxNigpOiBudW1iZXIgeyByZXR1cm4gcmVhZFUzMih0aGlzLl92LCAyNCk7IH1cbiAgICAgICAgZ2V0IHdoaXRlc3BhY2VMZW5ndGhJbkNvZGVQYXJzZWQoKTogbnVtYmVyIHsgcmV0dXJuIHJlYWRVMzIodGhpcy5fdiwgMjgpOyB9XG4gICAgICAgIGdldCBzcGFuTGVmdE9mZnNldENvZGVMZW5OZXdsaW5lcygpOiBudW1iZXIgeyByZXR1cm4gcmVhZFUzMih0aGlzLl92LCAzMik7IH1cbiAgICAgICAgZ2V0IHNwYW5MZWZ0T2Zmc2V0Q29kZUxlbkxpbmVDaGFyczE2KCk6IG51bWJlciB7IHJldHVybiByZWFkVTMyKHRoaXMuX3YsIDM2KTsgfVxuICAgICAgICBnZXQgY2hpbGRyZW5MZW5ndGhJbkNvZGVQYXJzZWQoKTogbnVtYmVyIHsgcmV0dXJuIHJlYWRVMzIodGhpcy5fdiwgNDApOyB9XG4gICAgICAgIGdldCBzcGFuQ29kZUxlbmd0aE5ld2xpbmVzKCk6IG51bWJlciB7IHJldHVybiByZWFkVTMyKHRoaXMuX3YsIDQ0KTsgfVxuICAgICAgICBnZXQgc3BhbkNvZGVMZW5ndGhMaW5lQ2hhcnMxNigpOiBudW1iZXIgeyByZXR1cm4gcmVhZFUzMih0aGlzLl92LCA0OCk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjb25zdCBlbnVtIFR5cGUge1xuICAgICAgICBJbnZhbGlkID0gMCxcbiAgICAgICAgQm9keUJsb2NrID0gMSxcbiAgICAgICAgQXJndW1lbnRCbG9ja0FwcGxpY2F0aW9uID0gMixcbiAgICAgICAgT3BlcmF0b3JCbG9ja0FwcGxpY2F0aW9uID0gMyxcbiAgICAgICAgSWRlbnQgPSA0LFxuICAgICAgICBQcml2YXRlID0gNSxcbiAgICAgICAgTnVtYmVyID0gNixcbiAgICAgICAgV2lsZGNhcmQgPSA3LFxuICAgICAgICBTdXNwZW5kZWREZWZhdWx0QXJndW1lbnRzID0gOCxcbiAgICAgICAgVGV4dExpdGVyYWwgPSA5LFxuICAgICAgICBBcHAgPSAxMCxcbiAgICAgICAgTmFtZWRBcHAgPSAxMSxcbiAgICAgICAgT3ByQXBwID0gMTIsXG4gICAgICAgIFVuYXJ5T3ByQXBwID0gMTMsXG4gICAgICAgIEF1dG9zY29wZWRJZGVudGlmaWVyID0gMTQsXG4gICAgICAgIE9wclNlY3Rpb25Cb3VuZGFyeSA9IDE1LFxuICAgICAgICBUZW1wbGF0ZUZ1bmN0aW9uID0gMTYsXG4gICAgICAgIE11bHRpU2VnbWVudEFwcCA9IDE3LFxuICAgICAgICBUeXBlRGVmID0gMTgsXG4gICAgICAgIEFzc2lnbm1lbnQgPSAxOSxcbiAgICAgICAgRnVuY3Rpb24gPSAyMCxcbiAgICAgICAgRm9yZWlnbkZ1bmN0aW9uID0gMjEsXG4gICAgICAgIEltcG9ydCA9IDIyLFxuICAgICAgICBFeHBvcnQgPSAyMyxcbiAgICAgICAgR3JvdXAgPSAyNCxcbiAgICAgICAgVHlwZVNpZ25hdHVyZSA9IDI1LFxuICAgICAgICBUeXBlQW5ub3RhdGVkID0gMjYsXG4gICAgICAgIENhc2VPZiA9IDI3LFxuICAgICAgICBMYW1iZGEgPSAyOCxcbiAgICAgICAgQXJyYXkgPSAyOSxcbiAgICAgICAgVHVwbGUgPSAzMCxcbiAgICAgICAgQW5ub3RhdGVkID0gMzEsXG4gICAgICAgIEFubm90YXRlZEJ1aWx0aW4gPSAzMixcbiAgICAgICAgRG9jdW1lbnRlZCA9IDMzLFxuICAgICAgICBDb25zdHJ1Y3RvckRlZmluaXRpb24gPSAzNFxuICAgIH1cbiAgICBleHBvcnQgY29uc3QgdHlwZU5hbWVzID0gW1wiSW52YWxpZFwiLCBcIkJvZHlCbG9ja1wiLCBcIkFyZ3VtZW50QmxvY2tBcHBsaWNhdGlvblwiLCBcIk9wZXJhdG9yQmxvY2tBcHBsaWNhdGlvblwiLCBcIklkZW50XCIsIFwiUHJpdmF0ZVwiLCBcIk51bWJlclwiLCBcIldpbGRjYXJkXCIsIFwiU3VzcGVuZGVkRGVmYXVsdEFyZ3VtZW50c1wiLCBcIlRleHRMaXRlcmFsXCIsIFwiQXBwXCIsIFwiTmFtZWRBcHBcIiwgXCJPcHJBcHBcIiwgXCJVbmFyeU9wckFwcFwiLCBcIkF1dG9zY29wZWRJZGVudGlmaWVyXCIsIFwiT3ByU2VjdGlvbkJvdW5kYXJ5XCIsIFwiVGVtcGxhdGVGdW5jdGlvblwiLCBcIk11bHRpU2VnbWVudEFwcFwiLCBcIlR5cGVEZWZcIiwgXCJBc3NpZ25tZW50XCIsIFwiRnVuY3Rpb25cIiwgXCJGb3JlaWduRnVuY3Rpb25cIiwgXCJJbXBvcnRcIiwgXCJFeHBvcnRcIiwgXCJHcm91cFwiLCBcIlR5cGVTaWduYXR1cmVcIiwgXCJUeXBlQW5ub3RhdGVkXCIsIFwiQ2FzZU9mXCIsIFwiTGFtYmRhXCIsIFwiQXJyYXlcIiwgXCJUdXBsZVwiLCBcIkFubm90YXRlZFwiLCBcIkFubm90YXRlZEJ1aWx0aW5cIiwgXCJEb2N1bWVudGVkXCIsIFwiQ29uc3RydWN0b3JEZWZpbml0aW9uXCJdIGFzIGNvbnN0O1xuICAgIGV4cG9ydCBjbGFzcyBJbnZhbGlkIGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5JbnZhbGlkO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5JbnZhbGlkOyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBJbnZhbGlkIHsgcmV0dXJuIG5ldyBJbnZhbGlkKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIGdldCBlcnJvcigpOiBzdHJpbmcgeyByZXR1cm4gcmVhZFN0cmluZyh0aGlzLl92LCA1Mik7IH1cbiAgICAgICAgZ2V0IGFzdCgpOiBUcmVlIHsgcmV0dXJuIFRyZWUucmVhZCh0aGlzLl92LCA1Nik7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLmFzdCk7IH1cbiAgICB9XG4gICAgZXhwb3J0IGNsYXNzIEJvZHlCbG9jayBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuQm9keUJsb2NrO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5Cb2R5QmxvY2s7IH1cbiAgICAgICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IEJvZHlCbG9jayB7IHJldHVybiBuZXcgQm9keUJsb2NrKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIGdldCBzdGF0ZW1lbnRzKCk6IEl0ZXJhYmxlSXRlcmF0b3I8TGluZT4geyByZXR1cm4gcmVhZFNlcXVlbmNlKHRoaXMuX3YsIDUyLCA4MSwgTGluZS5yZWFkKTsgfVxuICAgICAgICB2aXNpdFN0YXRlbWVudHModmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRTZXF1ZW5jZSh0aGlzLl92LCA1MiwgODEsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKExpbmUucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKSB8fCAhIXRoaXMudmlzaXRTdGF0ZW1lbnRzKHZpc2l0b3IpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBBcmd1bWVudEJsb2NrQXBwbGljYXRpb24gZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLkFyZ3VtZW50QmxvY2tBcHBsaWNhdGlvbjtcbiAgICAgICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IHRoaXMudHlwZSA9IFR5cGUuQXJndW1lbnRCbG9ja0FwcGxpY2F0aW9uOyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBBcmd1bWVudEJsb2NrQXBwbGljYXRpb24geyByZXR1cm4gbmV3IEFyZ3VtZW50QmxvY2tBcHBsaWNhdGlvbihyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgICAgICBnZXQgbGhzKCk6IFRyZWUgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCA1MiwgVHJlZS5yZWFkKTsgfVxuICAgICAgICBnZXQgYXJndW1lbnRzKCk6IEl0ZXJhYmxlSXRlcmF0b3I8TGluZT4geyByZXR1cm4gcmVhZFNlcXVlbmNlKHRoaXMuX3YsIDU3LCA4MSwgTGluZS5yZWFkKTsgfVxuICAgICAgICB2aXNpdExocyh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCA1MiwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoVHJlZS5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRBcmd1bWVudHModmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRTZXF1ZW5jZSh0aGlzLl92LCA1NywgODEsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKExpbmUucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKSB8fCAhIXRoaXMudmlzaXRMaHModmlzaXRvcikgfHwgISF0aGlzLnZpc2l0QXJndW1lbnRzKHZpc2l0b3IpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBPcGVyYXRvckJsb2NrQXBwbGljYXRpb24gZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLk9wZXJhdG9yQmxvY2tBcHBsaWNhdGlvbjtcbiAgICAgICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IHRoaXMudHlwZSA9IFR5cGUuT3BlcmF0b3JCbG9ja0FwcGxpY2F0aW9uOyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBPcGVyYXRvckJsb2NrQXBwbGljYXRpb24geyByZXR1cm4gbmV3IE9wZXJhdG9yQmxvY2tBcHBsaWNhdGlvbihyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgICAgICBnZXQgbGhzKCk6IFRyZWUgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCA1MiwgVHJlZS5yZWFkKTsgfVxuICAgICAgICBnZXQgZXhwcmVzc2lvbnMoKTogSXRlcmFibGVJdGVyYXRvcjxPcGVyYXRvckxpbmU+IHsgcmV0dXJuIHJlYWRTZXF1ZW5jZSh0aGlzLl92LCA1NywgODEsIE9wZXJhdG9yTGluZS5yZWFkKTsgfVxuICAgICAgICBnZXQgZXhjZXNzKCk6IEl0ZXJhYmxlSXRlcmF0b3I8TGluZT4geyByZXR1cm4gcmVhZFNlcXVlbmNlKHRoaXMuX3YsIDYxLCA4MSwgTGluZS5yZWFkKTsgfVxuICAgICAgICB2aXNpdExocyh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCA1MiwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoVHJlZS5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRFeHByZXNzaW9ucyh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdFNlcXVlbmNlKHRoaXMuX3YsIDU3LCA4MSwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoT3BlcmF0b3JMaW5lLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdEV4Y2Vzcyh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdFNlcXVlbmNlKHRoaXMuX3YsIDYxLCA4MSwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoTGluZS5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdExocyh2aXNpdG9yKSB8fCAhIXRoaXMudmlzaXRFeHByZXNzaW9ucyh2aXNpdG9yKSB8fCAhIXRoaXMudmlzaXRFeGNlc3ModmlzaXRvcik7IH1cbiAgICB9XG4gICAgZXhwb3J0IGNsYXNzIElkZW50IGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5JZGVudDtcbiAgICAgICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IHRoaXMudHlwZSA9IFR5cGUuSWRlbnQ7IH1cbiAgICAgICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IElkZW50IHsgcmV0dXJuIG5ldyBJZGVudChyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgICAgICBnZXQgdG9rZW4oKTogVG9rZW4uSWRlbnQgeyByZXR1cm4gVG9rZW4uSWRlbnQucmVhZCh0aGlzLl92LCA1Mik7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLnRva2VuKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgUHJpdmF0ZSBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuUHJpdmF0ZTtcbiAgICAgICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IHRoaXMudHlwZSA9IFR5cGUuUHJpdmF0ZTsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogUHJpdmF0ZSB7IHJldHVybiBuZXcgUHJpdmF0ZShyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgICAgICBnZXQga2V5d29yZCgpOiBUb2tlbi5Qcml2YXRlIHsgcmV0dXJuIFRva2VuLlByaXZhdGUucmVhZCh0aGlzLl92LCA1Mik7IH1cbiAgICAgICAgZ2V0IGJvZHkoKTogVHJlZSB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDEyOCwgVHJlZS5yZWFkKTsgfVxuICAgICAgICB2aXNpdEJvZHkodmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgMTI4LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUcmVlLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMua2V5d29yZCkgfHwgISF0aGlzLnZpc2l0Qm9keSh2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgTnVtYmVyIGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5OdW1iZXI7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLk51bWJlcjsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogTnVtYmVyIHsgcmV0dXJuIG5ldyBOdW1iZXIocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgZ2V0IGJhc2UoKTogVG9rZW4uTnVtYmVyQmFzZSB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDUyLCBUb2tlbi5OdW1iZXJCYXNlLnJlYWQpOyB9XG4gICAgICAgIGdldCBpbnRlZ2VyKCk6IFRva2VuLkRpZ2l0cyB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDU3LCBUb2tlbi5EaWdpdHMucmVhZCk7IH1cbiAgICAgICAgZ2V0IGZyYWN0aW9uYWxEaWdpdHMoKTogRnJhY3Rpb25hbERpZ2l0cyB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDYyLCBGcmFjdGlvbmFsRGlnaXRzLnJlYWQpOyB9XG4gICAgICAgIHZpc2l0QmFzZSh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCA1MiwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoVG9rZW4uTnVtYmVyQmFzZS5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRJbnRlZ2VyKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0T3B0aW9uKHRoaXMuX3YsIDU3LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUb2tlbi5EaWdpdHMucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgICAgIHZpc2l0RnJhY3Rpb25hbERpZ2l0cyh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCA2MiwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoRnJhY3Rpb25hbERpZ2l0cy5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdEJhc2UodmlzaXRvcikgfHwgISF0aGlzLnZpc2l0SW50ZWdlcih2aXNpdG9yKSB8fCAhIXRoaXMudmlzaXRGcmFjdGlvbmFsRGlnaXRzKHZpc2l0b3IpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBXaWxkY2FyZCBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuV2lsZGNhcmQ7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLldpbGRjYXJkOyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBXaWxkY2FyZCB7IHJldHVybiBuZXcgV2lsZGNhcmQocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgZ2V0IHRva2VuKCk6IFRva2VuLldpbGRjYXJkIHsgcmV0dXJuIFRva2VuLldpbGRjYXJkLnJlYWQodGhpcy5fdiwgNTIpOyB9XG4gICAgICAgIGdldCBkZUJydWlqbkluZGV4KCk6IG51bWJlciB7IHJldHVybiByZWFkSTMyKHRoaXMuX3YsIDEzMik7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLnRva2VuKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgU3VzcGVuZGVkRGVmYXVsdEFyZ3VtZW50cyBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuU3VzcGVuZGVkRGVmYXVsdEFyZ3VtZW50cztcbiAgICAgICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IHRoaXMudHlwZSA9IFR5cGUuU3VzcGVuZGVkRGVmYXVsdEFyZ3VtZW50czsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogU3VzcGVuZGVkRGVmYXVsdEFyZ3VtZW50cyB7IHJldHVybiBuZXcgU3VzcGVuZGVkRGVmYXVsdEFyZ3VtZW50cyhyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgICAgICBnZXQgdG9rZW4oKTogVG9rZW4uU3VzcGVuZGVkRGVmYXVsdEFyZ3VtZW50cyB7IHJldHVybiBUb2tlbi5TdXNwZW5kZWREZWZhdWx0QXJndW1lbnRzLnJlYWQodGhpcy5fdiwgNTIpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKSB8fCAhIXZpc2l0b3IodGhpcy50b2tlbik7IH1cbiAgICB9XG4gICAgZXhwb3J0IGNsYXNzIFRleHRMaXRlcmFsIGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5UZXh0TGl0ZXJhbDtcbiAgICAgICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IHRoaXMudHlwZSA9IFR5cGUuVGV4dExpdGVyYWw7IH1cbiAgICAgICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IFRleHRMaXRlcmFsIHsgcmV0dXJuIG5ldyBUZXh0TGl0ZXJhbChyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgICAgICBnZXQgb3BlbigpOiBUb2tlbi5UZXh0U3RhcnQgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCA1MiwgVG9rZW4uVGV4dFN0YXJ0LnJlYWQpOyB9XG4gICAgICAgIGdldCBuZXdsaW5lKCk6IFRva2VuLk5ld2xpbmUgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCA1NywgVG9rZW4uTmV3bGluZS5yZWFkKTsgfVxuICAgICAgICBnZXQgZWxlbWVudHMoKTogSXRlcmFibGVJdGVyYXRvcjxUZXh0RWxlbWVudD4geyByZXR1cm4gcmVhZFNlcXVlbmNlKHRoaXMuX3YsIDYyLCA0LCBUZXh0RWxlbWVudC5yZWFkKTsgfVxuICAgICAgICBnZXQgY2xvc2UoKTogVG9rZW4uVGV4dEVuZCB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDY2LCBUb2tlbi5UZXh0RW5kLnJlYWQpOyB9XG4gICAgICAgIHZpc2l0T3Blbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCA1MiwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoVG9rZW4uVGV4dFN0YXJ0LnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdE5ld2xpbmUodmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgNTcsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKFRva2VuLk5ld2xpbmUucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgICAgIHZpc2l0RWxlbWVudHModmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRTZXF1ZW5jZSh0aGlzLl92LCA2MiwgNCwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoVGV4dEVsZW1lbnQucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgICAgIHZpc2l0Q2xvc2UodmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgNjYsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKFRva2VuLlRleHRFbmQucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKSB8fCAhIXRoaXMudmlzaXRPcGVuKHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdE5ld2xpbmUodmlzaXRvcikgfHwgISF0aGlzLnZpc2l0RWxlbWVudHModmlzaXRvcikgfHwgISF0aGlzLnZpc2l0Q2xvc2UodmlzaXRvcik7IH1cbiAgICB9XG4gICAgZXhwb3J0IGNsYXNzIEFwcCBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuQXBwO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5BcHA7IH1cbiAgICAgICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IEFwcCB7IHJldHVybiBuZXcgQXBwKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIGdldCBmdW5jKCk6IFRyZWUgeyByZXR1cm4gVHJlZS5yZWFkKHRoaXMuX3YsIDUyKTsgfVxuICAgICAgICBnZXQgYXJnKCk6IFRyZWUgeyByZXR1cm4gVHJlZS5yZWFkKHRoaXMuX3YsIDU2KTsgfVxuICAgICAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMuZnVuYykgfHwgISF2aXNpdG9yKHRoaXMuYXJnKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgTmFtZWRBcHAgZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLk5hbWVkQXBwO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5OYW1lZEFwcDsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogTmFtZWRBcHAgeyByZXR1cm4gbmV3IE5hbWVkQXBwKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIGdldCBmdW5jKCk6IFRyZWUgeyByZXR1cm4gVHJlZS5yZWFkKHRoaXMuX3YsIDUyKTsgfVxuICAgICAgICBnZXQgb3BlbigpOiBUb2tlbi5PcGVuU3ltYm9sIHwgdW5kZWZpbmVkIHsgcmV0dXJuIHJlYWRPcHRpb24odGhpcy5fdiwgNTYsIFRva2VuLk9wZW5TeW1ib2wucmVhZCk7IH1cbiAgICAgICAgZ2V0IG5hbWUoKTogVG9rZW4uSWRlbnQgeyByZXR1cm4gVG9rZW4uSWRlbnQucmVhZCh0aGlzLl92LCA2MSk7IH1cbiAgICAgICAgZ2V0IGVxdWFscygpOiBUb2tlbi5PcGVyYXRvciB7IHJldHVybiBUb2tlbi5PcGVyYXRvci5yZWFkKHRoaXMuX3YsIDE0NCk7IH1cbiAgICAgICAgZ2V0IGFyZygpOiBUcmVlIHsgcmV0dXJuIFRyZWUucmVhZCh0aGlzLl92LCAyMjApOyB9XG4gICAgICAgIGdldCBjbG9zZSgpOiBUb2tlbi5DbG9zZVN5bWJvbCB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDIyNCwgVG9rZW4uQ2xvc2VTeW1ib2wucmVhZCk7IH1cbiAgICAgICAgdmlzaXRPcGVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0T3B0aW9uKHRoaXMuX3YsIDU2LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUb2tlbi5PcGVuU3ltYm9sLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdENsb3NlKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0T3B0aW9uKHRoaXMuX3YsIDIyNCwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoVG9rZW4uQ2xvc2VTeW1ib2wucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKSB8fCAhIXZpc2l0b3IodGhpcy5mdW5jKSB8fCAhIXRoaXMudmlzaXRPcGVuKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLm5hbWUpIHx8ICEhdmlzaXRvcih0aGlzLmVxdWFscykgfHwgISF2aXNpdG9yKHRoaXMuYXJnKSB8fCAhIXRoaXMudmlzaXRDbG9zZSh2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgT3ByQXBwIGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5PcHJBcHA7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLk9wckFwcDsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogT3ByQXBwIHsgcmV0dXJuIG5ldyBPcHJBcHAocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgZ2V0IGxocygpOiBUcmVlIHwgdW5kZWZpbmVkIHsgcmV0dXJuIHJlYWRPcHRpb24odGhpcy5fdiwgNTIsIFRyZWUucmVhZCk7IH1cbiAgICAgICAgZ2V0IG9wcigpOiBSZXN1bHQ8VG9rZW4uT3BlcmF0b3IsIE11bHRpcGxlT3BlcmF0b3JFcnJvcj4geyByZXR1cm4gcmVhZFJlc3VsdCh0aGlzLl92LCA1NywgVG9rZW4uT3BlcmF0b3IucmVhZCwgTXVsdGlwbGVPcGVyYXRvckVycm9yLnJlYWQpOyB9XG4gICAgICAgIGdldCByaHMoKTogVHJlZSB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDYxLCBUcmVlLnJlYWQpOyB9XG4gICAgICAgIHZpc2l0TGhzKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0T3B0aW9uKHRoaXMuX3YsIDUyLCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUcmVlLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdE9wcih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdFJlc3VsdCh0aGlzLl92LCA1NywgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoVG9rZW4uT3BlcmF0b3IucmVhZCh2aWV3LCBhZGRyZXNzKSksICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKE11bHRpcGxlT3BlcmF0b3JFcnJvci5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRSaHModmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgNjEsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKFRyZWUucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKSB8fCAhIXRoaXMudmlzaXRMaHModmlzaXRvcikgfHwgISF0aGlzLnZpc2l0T3ByKHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdFJocyh2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgVW5hcnlPcHJBcHAgZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLlVuYXJ5T3ByQXBwO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5VbmFyeU9wckFwcDsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogVW5hcnlPcHJBcHAgeyByZXR1cm4gbmV3IFVuYXJ5T3ByQXBwKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIGdldCBvcHIoKTogVG9rZW4uT3BlcmF0b3IgeyByZXR1cm4gVG9rZW4uT3BlcmF0b3IucmVhZCh0aGlzLl92LCA1Mik7IH1cbiAgICAgICAgZ2V0IHJocygpOiBUcmVlIHwgdW5kZWZpbmVkIHsgcmV0dXJuIHJlYWRPcHRpb24odGhpcy5fdiwgMTI4LCBUcmVlLnJlYWQpOyB9XG4gICAgICAgIHZpc2l0UmhzKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0T3B0aW9uKHRoaXMuX3YsIDEyOCwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoVHJlZS5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLm9wcikgfHwgISF0aGlzLnZpc2l0UmhzKHZpc2l0b3IpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBBdXRvc2NvcGVkSWRlbnRpZmllciBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuQXV0b3Njb3BlZElkZW50aWZpZXI7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLkF1dG9zY29wZWRJZGVudGlmaWVyOyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBBdXRvc2NvcGVkSWRlbnRpZmllciB7IHJldHVybiBuZXcgQXV0b3Njb3BlZElkZW50aWZpZXIocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgZ2V0IG9wcigpOiBUb2tlbi5PcGVyYXRvciB7IHJldHVybiBUb2tlbi5PcGVyYXRvci5yZWFkKHRoaXMuX3YsIDUyKTsgfVxuICAgICAgICBnZXQgaWRlbnQoKTogVG9rZW4uSWRlbnQgeyByZXR1cm4gVG9rZW4uSWRlbnQucmVhZCh0aGlzLl92LCAxMjgpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKSB8fCAhIXZpc2l0b3IodGhpcy5vcHIpIHx8ICEhdmlzaXRvcih0aGlzLmlkZW50KTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgT3ByU2VjdGlvbkJvdW5kYXJ5IGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5PcHJTZWN0aW9uQm91bmRhcnk7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLk9wclNlY3Rpb25Cb3VuZGFyeTsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogT3ByU2VjdGlvbkJvdW5kYXJ5IHsgcmV0dXJuIG5ldyBPcHJTZWN0aW9uQm91bmRhcnkocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgZ2V0IGFyZ3VtZW50cygpOiBudW1iZXIgeyByZXR1cm4gcmVhZFUzMih0aGlzLl92LCA1Mik7IH1cbiAgICAgICAgZ2V0IGFzdCgpOiBUcmVlIHsgcmV0dXJuIFRyZWUucmVhZCh0aGlzLl92LCA1Nik7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLmFzdCk7IH1cbiAgICB9XG4gICAgZXhwb3J0IGNsYXNzIFRlbXBsYXRlRnVuY3Rpb24gZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLlRlbXBsYXRlRnVuY3Rpb247XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLlRlbXBsYXRlRnVuY3Rpb247IH1cbiAgICAgICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IFRlbXBsYXRlRnVuY3Rpb24geyByZXR1cm4gbmV3IFRlbXBsYXRlRnVuY3Rpb24ocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgZ2V0IGFyZ3VtZW50cygpOiBudW1iZXIgeyByZXR1cm4gcmVhZFUzMih0aGlzLl92LCA1Mik7IH1cbiAgICAgICAgZ2V0IGFzdCgpOiBUcmVlIHsgcmV0dXJuIFRyZWUucmVhZCh0aGlzLl92LCA1Nik7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLmFzdCk7IH1cbiAgICB9XG4gICAgZXhwb3J0IGNsYXNzIE11bHRpU2VnbWVudEFwcCBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuTXVsdGlTZWdtZW50QXBwO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5NdWx0aVNlZ21lbnRBcHA7IH1cbiAgICAgICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IE11bHRpU2VnbWVudEFwcCB7IHJldHVybiBuZXcgTXVsdGlTZWdtZW50QXBwKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIGdldCBzZWdtZW50cygpOiBJdGVyYWJsZUl0ZXJhdG9yPE11bHRpU2VnbWVudEFwcFNlZ21lbnQ+IHsgcmV0dXJuIHJlYWRTZXF1ZW5jZSh0aGlzLl92LCA1MiwgOSwgTXVsdGlTZWdtZW50QXBwU2VnbWVudC5yZWFkKTsgfVxuICAgICAgICB2aXNpdFNlZ21lbnRzKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0U2VxdWVuY2UodGhpcy5fdiwgNTIsIDksICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKE11bHRpU2VnbWVudEFwcFNlZ21lbnQucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKSB8fCAhIXRoaXMudmlzaXRTZWdtZW50cyh2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgVHlwZURlZiBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuVHlwZURlZjtcbiAgICAgICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IHRoaXMudHlwZSA9IFR5cGUuVHlwZURlZjsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogVHlwZURlZiB7IHJldHVybiBuZXcgVHlwZURlZihyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgICAgICBnZXQga2V5d29yZCgpOiBUb2tlbi5JZGVudCB7IHJldHVybiBUb2tlbi5JZGVudC5yZWFkKHRoaXMuX3YsIDUyKTsgfVxuICAgICAgICBnZXQgbmFtZSgpOiBUb2tlbi5JZGVudCB7IHJldHVybiBUb2tlbi5JZGVudC5yZWFkKHRoaXMuX3YsIDEzNSk7IH1cbiAgICAgICAgZ2V0IHBhcmFtcygpOiBJdGVyYWJsZUl0ZXJhdG9yPEFyZ3VtZW50RGVmaW5pdGlvbj4geyByZXR1cm4gcmVhZFNlcXVlbmNlKHRoaXMuX3YsIDIxOCwgMzksIEFyZ3VtZW50RGVmaW5pdGlvbi5yZWFkKTsgfVxuICAgICAgICBnZXQgYm9keSgpOiBJdGVyYWJsZUl0ZXJhdG9yPExpbmU+IHsgcmV0dXJuIHJlYWRTZXF1ZW5jZSh0aGlzLl92LCAyMjIsIDgxLCBMaW5lLnJlYWQpOyB9XG4gICAgICAgIHZpc2l0UGFyYW1zKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0U2VxdWVuY2UodGhpcy5fdiwgMjE4LCAzOSwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoQXJndW1lbnREZWZpbml0aW9uLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdEJvZHkodmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRTZXF1ZW5jZSh0aGlzLl92LCAyMjIsIDgxLCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihMaW5lLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMua2V5d29yZCkgfHwgISF2aXNpdG9yKHRoaXMubmFtZSkgfHwgISF0aGlzLnZpc2l0UGFyYW1zKHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdEJvZHkodmlzaXRvcik7IH1cbiAgICB9XG4gICAgZXhwb3J0IGNsYXNzIEFzc2lnbm1lbnQgZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLkFzc2lnbm1lbnQ7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLkFzc2lnbm1lbnQ7IH1cbiAgICAgICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IEFzc2lnbm1lbnQgeyByZXR1cm4gbmV3IEFzc2lnbm1lbnQocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgZ2V0IHBhdHRlcm4oKTogVHJlZSB7IHJldHVybiBUcmVlLnJlYWQodGhpcy5fdiwgNTIpOyB9XG4gICAgICAgIGdldCBlcXVhbHMoKTogVG9rZW4uT3BlcmF0b3IgeyByZXR1cm4gVG9rZW4uT3BlcmF0b3IucmVhZCh0aGlzLl92LCA1Nik7IH1cbiAgICAgICAgZ2V0IGV4cHIoKTogVHJlZSB7IHJldHVybiBUcmVlLnJlYWQodGhpcy5fdiwgMTMyKTsgfVxuICAgICAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMucGF0dGVybikgfHwgISF2aXNpdG9yKHRoaXMuZXF1YWxzKSB8fCAhIXZpc2l0b3IodGhpcy5leHByKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgRnVuY3Rpb24gZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLkZ1bmN0aW9uO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5GdW5jdGlvbjsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogRnVuY3Rpb24geyByZXR1cm4gbmV3IEZ1bmN0aW9uKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIGdldCBuYW1lKCk6IFRyZWUgeyByZXR1cm4gVHJlZS5yZWFkKHRoaXMuX3YsIDUyKTsgfVxuICAgICAgICBnZXQgYXJncygpOiBJdGVyYWJsZUl0ZXJhdG9yPEFyZ3VtZW50RGVmaW5pdGlvbj4geyByZXR1cm4gcmVhZFNlcXVlbmNlKHRoaXMuX3YsIDU2LCAzOSwgQXJndW1lbnREZWZpbml0aW9uLnJlYWQpOyB9XG4gICAgICAgIGdldCByZXR1cm5zKCk6IFJldHVyblNwZWNpZmljYXRpb24gfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCA2MCwgUmV0dXJuU3BlY2lmaWNhdGlvbi5yZWFkKTsgfVxuICAgICAgICBnZXQgZXF1YWxzKCk6IFRva2VuLk9wZXJhdG9yIHsgcmV0dXJuIFRva2VuLk9wZXJhdG9yLnJlYWQodGhpcy5fdiwgNjUpOyB9XG4gICAgICAgIGdldCBib2R5KCk6IFRyZWUgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCAxNDEsIFRyZWUucmVhZCk7IH1cbiAgICAgICAgdmlzaXRBcmdzKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0U2VxdWVuY2UodGhpcy5fdiwgNTYsIDM5LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihBcmd1bWVudERlZmluaXRpb24ucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgICAgIHZpc2l0UmV0dXJucyh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCA2MCwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoUmV0dXJuU3BlY2lmaWNhdGlvbi5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRCb2R5KHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0T3B0aW9uKHRoaXMuX3YsIDE0MSwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoVHJlZS5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLm5hbWUpIHx8ICEhdGhpcy52aXNpdEFyZ3ModmlzaXRvcikgfHwgISF0aGlzLnZpc2l0UmV0dXJucyh2aXNpdG9yKSB8fCAhIXZpc2l0b3IodGhpcy5lcXVhbHMpIHx8ICEhdGhpcy52aXNpdEJvZHkodmlzaXRvcik7IH1cbiAgICB9XG4gICAgZXhwb3J0IGNsYXNzIEZvcmVpZ25GdW5jdGlvbiBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuRm9yZWlnbkZ1bmN0aW9uO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5Gb3JlaWduRnVuY3Rpb247IH1cbiAgICAgICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IEZvcmVpZ25GdW5jdGlvbiB7IHJldHVybiBuZXcgRm9yZWlnbkZ1bmN0aW9uKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIGdldCBmb3JlaWduKCk6IFRva2VuLklkZW50IHsgcmV0dXJuIFRva2VuLklkZW50LnJlYWQodGhpcy5fdiwgNTIpOyB9XG4gICAgICAgIGdldCBsYW5ndWFnZSgpOiBUb2tlbi5JZGVudCB7IHJldHVybiBUb2tlbi5JZGVudC5yZWFkKHRoaXMuX3YsIDEzNSk7IH1cbiAgICAgICAgZ2V0IG5hbWUoKTogVG9rZW4uSWRlbnQgeyByZXR1cm4gVG9rZW4uSWRlbnQucmVhZCh0aGlzLl92LCAyMTgpOyB9XG4gICAgICAgIGdldCBhcmdzKCk6IEl0ZXJhYmxlSXRlcmF0b3I8QXJndW1lbnREZWZpbml0aW9uPiB7IHJldHVybiByZWFkU2VxdWVuY2UodGhpcy5fdiwgMzAxLCAzOSwgQXJndW1lbnREZWZpbml0aW9uLnJlYWQpOyB9XG4gICAgICAgIGdldCBlcXVhbHMoKTogVG9rZW4uT3BlcmF0b3IgeyByZXR1cm4gVG9rZW4uT3BlcmF0b3IucmVhZCh0aGlzLl92LCAzMDUpOyB9XG4gICAgICAgIGdldCBib2R5KCk6IFRyZWUgeyByZXR1cm4gVHJlZS5yZWFkKHRoaXMuX3YsIDM4MSk7IH1cbiAgICAgICAgdmlzaXRBcmdzKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0U2VxdWVuY2UodGhpcy5fdiwgMzAxLCAzOSwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoQXJndW1lbnREZWZpbml0aW9uLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMuZm9yZWlnbikgfHwgISF2aXNpdG9yKHRoaXMubGFuZ3VhZ2UpIHx8ICEhdmlzaXRvcih0aGlzLm5hbWUpIHx8ICEhdGhpcy52aXNpdEFyZ3ModmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMuZXF1YWxzKSB8fCAhIXZpc2l0b3IodGhpcy5ib2R5KTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgSW1wb3J0IGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5JbXBvcnQ7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLkltcG9ydDsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogSW1wb3J0IHsgcmV0dXJuIG5ldyBJbXBvcnQocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgZ2V0IHBvbHlnbG90KCk6IE11bHRpU2VnbWVudEFwcFNlZ21lbnQgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCA1MiwgTXVsdGlTZWdtZW50QXBwU2VnbWVudC5yZWFkKTsgfVxuICAgICAgICBnZXQgZnJvbSgpOiBNdWx0aVNlZ21lbnRBcHBTZWdtZW50IHwgdW5kZWZpbmVkIHsgcmV0dXJuIHJlYWRPcHRpb24odGhpcy5fdiwgNTcsIE11bHRpU2VnbWVudEFwcFNlZ21lbnQucmVhZCk7IH1cbiAgICAgICAgZ2V0IGltcG9ydCgpOiBNdWx0aVNlZ21lbnRBcHBTZWdtZW50IHsgcmV0dXJuIE11bHRpU2VnbWVudEFwcFNlZ21lbnQucmVhZCh0aGlzLl92LCA2Mik7IH1cbiAgICAgICAgZ2V0IGFsbCgpOiBUb2tlbi5JZGVudCB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDcxLCBUb2tlbi5JZGVudC5yZWFkKTsgfVxuICAgICAgICBnZXQgYXMoKTogTXVsdGlTZWdtZW50QXBwU2VnbWVudCB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDc2LCBNdWx0aVNlZ21lbnRBcHBTZWdtZW50LnJlYWQpOyB9XG4gICAgICAgIGdldCBoaWRpbmcoKTogTXVsdGlTZWdtZW50QXBwU2VnbWVudCB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDgxLCBNdWx0aVNlZ21lbnRBcHBTZWdtZW50LnJlYWQpOyB9XG4gICAgICAgIHZpc2l0UG9seWdsb3QodmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgNTIsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKE11bHRpU2VnbWVudEFwcFNlZ21lbnQucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgICAgIHZpc2l0RnJvbSh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCA1NywgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoTXVsdGlTZWdtZW50QXBwU2VnbWVudC5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRBbGwodmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgNzEsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKFRva2VuLklkZW50LnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdEFzKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0T3B0aW9uKHRoaXMuX3YsIDc2LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihNdWx0aVNlZ21lbnRBcHBTZWdtZW50LnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdEhpZGluZyh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCA4MSwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoTXVsdGlTZWdtZW50QXBwU2VnbWVudC5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdFBvbHlnbG90KHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdEZyb20odmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMuaW1wb3J0KSB8fCAhIXRoaXMudmlzaXRBbGwodmlzaXRvcikgfHwgISF0aGlzLnZpc2l0QXModmlzaXRvcikgfHwgISF0aGlzLnZpc2l0SGlkaW5nKHZpc2l0b3IpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBFeHBvcnQgZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLkV4cG9ydDtcbiAgICAgICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IHRoaXMudHlwZSA9IFR5cGUuRXhwb3J0OyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBFeHBvcnQgeyByZXR1cm4gbmV3IEV4cG9ydChyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgICAgICBnZXQgZnJvbSgpOiBNdWx0aVNlZ21lbnRBcHBTZWdtZW50IHwgdW5kZWZpbmVkIHsgcmV0dXJuIHJlYWRPcHRpb24odGhpcy5fdiwgNTIsIE11bHRpU2VnbWVudEFwcFNlZ21lbnQucmVhZCk7IH1cbiAgICAgICAgZ2V0IGV4cG9ydCgpOiBNdWx0aVNlZ21lbnRBcHBTZWdtZW50IHsgcmV0dXJuIE11bHRpU2VnbWVudEFwcFNlZ21lbnQucmVhZCh0aGlzLl92LCA1Nyk7IH1cbiAgICAgICAgZ2V0IGFsbCgpOiBUb2tlbi5JZGVudCB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDY2LCBUb2tlbi5JZGVudC5yZWFkKTsgfVxuICAgICAgICBnZXQgYXMoKTogTXVsdGlTZWdtZW50QXBwU2VnbWVudCB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDcxLCBNdWx0aVNlZ21lbnRBcHBTZWdtZW50LnJlYWQpOyB9XG4gICAgICAgIGdldCBoaWRpbmcoKTogTXVsdGlTZWdtZW50QXBwU2VnbWVudCB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDc2LCBNdWx0aVNlZ21lbnRBcHBTZWdtZW50LnJlYWQpOyB9XG4gICAgICAgIHZpc2l0RnJvbSh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCA1MiwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoTXVsdGlTZWdtZW50QXBwU2VnbWVudC5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRBbGwodmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgNjYsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKFRva2VuLklkZW50LnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdEFzKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0T3B0aW9uKHRoaXMuX3YsIDcxLCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihNdWx0aVNlZ21lbnRBcHBTZWdtZW50LnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdEhpZGluZyh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCA3NiwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoTXVsdGlTZWdtZW50QXBwU2VnbWVudC5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdEZyb20odmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMuZXhwb3J0KSB8fCAhIXRoaXMudmlzaXRBbGwodmlzaXRvcikgfHwgISF0aGlzLnZpc2l0QXModmlzaXRvcikgfHwgISF0aGlzLnZpc2l0SGlkaW5nKHZpc2l0b3IpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBHcm91cCBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuR3JvdXA7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLkdyb3VwOyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBHcm91cCB7IHJldHVybiBuZXcgR3JvdXAocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgZ2V0IG9wZW4oKTogVG9rZW4uT3BlblN5bWJvbCB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDUyLCBUb2tlbi5PcGVuU3ltYm9sLnJlYWQpOyB9XG4gICAgICAgIGdldCBib2R5KCk6IFRyZWUgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCA1NywgVHJlZS5yZWFkKTsgfVxuICAgICAgICBnZXQgY2xvc2UoKTogVG9rZW4uQ2xvc2VTeW1ib2wgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCA2MiwgVG9rZW4uQ2xvc2VTeW1ib2wucmVhZCk7IH1cbiAgICAgICAgdmlzaXRPcGVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0T3B0aW9uKHRoaXMuX3YsIDUyLCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUb2tlbi5PcGVuU3ltYm9sLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdEJvZHkodmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgNTcsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKFRyZWUucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgICAgIHZpc2l0Q2xvc2UodmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgNjIsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKFRva2VuLkNsb3NlU3ltYm9sLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF0aGlzLnZpc2l0T3Blbih2aXNpdG9yKSB8fCAhIXRoaXMudmlzaXRCb2R5KHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdENsb3NlKHZpc2l0b3IpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBUeXBlU2lnbmF0dXJlIGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5UeXBlU2lnbmF0dXJlO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5UeXBlU2lnbmF0dXJlOyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBUeXBlU2lnbmF0dXJlIHsgcmV0dXJuIG5ldyBUeXBlU2lnbmF0dXJlKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIGdldCB2YXJpYWJsZSgpOiBUcmVlIHsgcmV0dXJuIFRyZWUucmVhZCh0aGlzLl92LCA1Mik7IH1cbiAgICAgICAgZ2V0IG9wZXJhdG9yKCk6IFRva2VuLk9wZXJhdG9yIHsgcmV0dXJuIFRva2VuLk9wZXJhdG9yLnJlYWQodGhpcy5fdiwgNTYpOyB9XG4gICAgICAgIGdldCB0eXBlTm9kZSgpOiBUcmVlIHsgcmV0dXJuIFRyZWUucmVhZCh0aGlzLl92LCAxMzIpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKSB8fCAhIXZpc2l0b3IodGhpcy52YXJpYWJsZSkgfHwgISF2aXNpdG9yKHRoaXMub3BlcmF0b3IpIHx8ICEhdmlzaXRvcih0aGlzLnR5cGVOb2RlKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgVHlwZUFubm90YXRlZCBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuVHlwZUFubm90YXRlZDtcbiAgICAgICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IHRoaXMudHlwZSA9IFR5cGUuVHlwZUFubm90YXRlZDsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogVHlwZUFubm90YXRlZCB7IHJldHVybiBuZXcgVHlwZUFubm90YXRlZChyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgICAgICBnZXQgZXhwcmVzc2lvbigpOiBUcmVlIHsgcmV0dXJuIFRyZWUucmVhZCh0aGlzLl92LCA1Mik7IH1cbiAgICAgICAgZ2V0IG9wZXJhdG9yKCk6IFRva2VuLk9wZXJhdG9yIHsgcmV0dXJuIFRva2VuLk9wZXJhdG9yLnJlYWQodGhpcy5fdiwgNTYpOyB9XG4gICAgICAgIGdldCB0eXBlTm9kZSgpOiBUcmVlIHsgcmV0dXJuIFRyZWUucmVhZCh0aGlzLl92LCAxMzIpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKSB8fCAhIXZpc2l0b3IodGhpcy5leHByZXNzaW9uKSB8fCAhIXZpc2l0b3IodGhpcy5vcGVyYXRvcikgfHwgISF2aXNpdG9yKHRoaXMudHlwZU5vZGUpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBDYXNlT2YgZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLkNhc2VPZjtcbiAgICAgICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IHRoaXMudHlwZSA9IFR5cGUuQ2FzZU9mOyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBDYXNlT2YgeyByZXR1cm4gbmV3IENhc2VPZihyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgICAgICBnZXQgY2FzZSgpOiBUb2tlbi5JZGVudCB7IHJldHVybiBUb2tlbi5JZGVudC5yZWFkKHRoaXMuX3YsIDUyKTsgfVxuICAgICAgICBnZXQgZXhwcmVzc2lvbigpOiBUcmVlIHwgdW5kZWZpbmVkIHsgcmV0dXJuIHJlYWRPcHRpb24odGhpcy5fdiwgMTM1LCBUcmVlLnJlYWQpOyB9XG4gICAgICAgIGdldCBvZigpOiBUb2tlbi5JZGVudCB7IHJldHVybiBUb2tlbi5JZGVudC5yZWFkKHRoaXMuX3YsIDE0MCk7IH1cbiAgICAgICAgZ2V0IGNhc2VzKCk6IEl0ZXJhYmxlSXRlcmF0b3I8Q2FzZUxpbmU+IHsgcmV0dXJuIHJlYWRTZXF1ZW5jZSh0aGlzLl92LCAyMjMsIDEwLCBDYXNlTGluZS5yZWFkKTsgfVxuICAgICAgICB2aXNpdEV4cHJlc3Npb24odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgMTM1LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUcmVlLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdENhc2VzKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0U2VxdWVuY2UodGhpcy5fdiwgMjIzLCAxMCwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoQ2FzZUxpbmUucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKSB8fCAhIXZpc2l0b3IodGhpcy5jYXNlKSB8fCAhIXRoaXMudmlzaXRFeHByZXNzaW9uKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLm9mKSB8fCAhIXRoaXMudmlzaXRDYXNlcyh2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgTGFtYmRhIGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5MYW1iZGE7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLkxhbWJkYTsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogTGFtYmRhIHsgcmV0dXJuIG5ldyBMYW1iZGEocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgZ2V0IG9wZXJhdG9yKCk6IFRva2VuLk9wZXJhdG9yIHsgcmV0dXJuIFRva2VuLk9wZXJhdG9yLnJlYWQodGhpcy5fdiwgNTIpOyB9XG4gICAgICAgIGdldCBhcnJvdygpOiBUcmVlIHwgdW5kZWZpbmVkIHsgcmV0dXJuIHJlYWRPcHRpb24odGhpcy5fdiwgMTI4LCBUcmVlLnJlYWQpOyB9XG4gICAgICAgIHZpc2l0QXJyb3codmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgMTI4LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUcmVlLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMub3BlcmF0b3IpIHx8ICEhdGhpcy52aXNpdEFycm93KHZpc2l0b3IpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBBcnJheSBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuQXJyYXk7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLkFycmF5OyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBBcnJheSB7IHJldHVybiBuZXcgQXJyYXkocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgZ2V0IGxlZnQoKTogVG9rZW4uT3BlblN5bWJvbCB7IHJldHVybiBUb2tlbi5PcGVuU3ltYm9sLnJlYWQodGhpcy5fdiwgNTIpOyB9XG4gICAgICAgIGdldCBmaXJzdCgpOiBUcmVlIHwgdW5kZWZpbmVkIHsgcmV0dXJuIHJlYWRPcHRpb24odGhpcy5fdiwgMTI4LCBUcmVlLnJlYWQpOyB9XG4gICAgICAgIGdldCByZXN0KCk6IEl0ZXJhYmxlSXRlcmF0b3I8T3BlcmF0b3JEZWxpbWl0ZWRUcmVlPiB7IHJldHVybiByZWFkU2VxdWVuY2UodGhpcy5fdiwgMTMzLCA4MSwgT3BlcmF0b3JEZWxpbWl0ZWRUcmVlLnJlYWQpOyB9XG4gICAgICAgIGdldCByaWdodCgpOiBUb2tlbi5DbG9zZVN5bWJvbCB7IHJldHVybiBUb2tlbi5DbG9zZVN5bWJvbC5yZWFkKHRoaXMuX3YsIDEzNyk7IH1cbiAgICAgICAgdmlzaXRGaXJzdCh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCAxMjgsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKFRyZWUucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgICAgIHZpc2l0UmVzdCh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdFNlcXVlbmNlKHRoaXMuX3YsIDEzMywgODEsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKE9wZXJhdG9yRGVsaW1pdGVkVHJlZS5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLmxlZnQpIHx8ICEhdGhpcy52aXNpdEZpcnN0KHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdFJlc3QodmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMucmlnaHQpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBUdXBsZSBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuVHVwbGU7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLlR1cGxlOyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBUdXBsZSB7IHJldHVybiBuZXcgVHVwbGUocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgZ2V0IGxlZnQoKTogVG9rZW4uT3BlblN5bWJvbCB7IHJldHVybiBUb2tlbi5PcGVuU3ltYm9sLnJlYWQodGhpcy5fdiwgNTIpOyB9XG4gICAgICAgIGdldCBmaXJzdCgpOiBUcmVlIHwgdW5kZWZpbmVkIHsgcmV0dXJuIHJlYWRPcHRpb24odGhpcy5fdiwgMTI4LCBUcmVlLnJlYWQpOyB9XG4gICAgICAgIGdldCByZXN0KCk6IEl0ZXJhYmxlSXRlcmF0b3I8T3BlcmF0b3JEZWxpbWl0ZWRUcmVlPiB7IHJldHVybiByZWFkU2VxdWVuY2UodGhpcy5fdiwgMTMzLCA4MSwgT3BlcmF0b3JEZWxpbWl0ZWRUcmVlLnJlYWQpOyB9XG4gICAgICAgIGdldCByaWdodCgpOiBUb2tlbi5DbG9zZVN5bWJvbCB7IHJldHVybiBUb2tlbi5DbG9zZVN5bWJvbC5yZWFkKHRoaXMuX3YsIDEzNyk7IH1cbiAgICAgICAgdmlzaXRGaXJzdCh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCAxMjgsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKFRyZWUucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgICAgIHZpc2l0UmVzdCh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdFNlcXVlbmNlKHRoaXMuX3YsIDEzMywgODEsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKE9wZXJhdG9yRGVsaW1pdGVkVHJlZS5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLmxlZnQpIHx8ICEhdGhpcy52aXNpdEZpcnN0KHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdFJlc3QodmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMucmlnaHQpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBBbm5vdGF0ZWQgZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLkFubm90YXRlZDtcbiAgICAgICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IHRoaXMudHlwZSA9IFR5cGUuQW5ub3RhdGVkOyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBBbm5vdGF0ZWQgeyByZXR1cm4gbmV3IEFubm90YXRlZChyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgICAgICBnZXQgdG9rZW4oKTogVG9rZW4uT3BlcmF0b3IgeyByZXR1cm4gVG9rZW4uT3BlcmF0b3IucmVhZCh0aGlzLl92LCA1Mik7IH1cbiAgICAgICAgZ2V0IGFubm90YXRpb24oKTogVG9rZW4uSWRlbnQgeyByZXR1cm4gVG9rZW4uSWRlbnQucmVhZCh0aGlzLl92LCAxMjgpOyB9XG4gICAgICAgIGdldCBhcmd1bWVudCgpOiBUcmVlIHwgdW5kZWZpbmVkIHsgcmV0dXJuIHJlYWRPcHRpb24odGhpcy5fdiwgMjExLCBUcmVlLnJlYWQpOyB9XG4gICAgICAgIGdldCBuZXdsaW5lcygpOiBJdGVyYWJsZUl0ZXJhdG9yPFRva2VuLk5ld2xpbmU+IHsgcmV0dXJuIHJlYWRTZXF1ZW5jZSh0aGlzLl92LCAyMTYsIDc2LCBUb2tlbi5OZXdsaW5lLnJlYWQpOyB9XG4gICAgICAgIGdldCBleHByZXNzaW9uKCk6IFRyZWUgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCAyMjAsIFRyZWUucmVhZCk7IH1cbiAgICAgICAgdmlzaXRBcmd1bWVudCh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCAyMTEsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKFRyZWUucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgICAgIHZpc2l0TmV3bGluZXModmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRTZXF1ZW5jZSh0aGlzLl92LCAyMTYsIDc2LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUb2tlbi5OZXdsaW5lLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdEV4cHJlc3Npb24odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgMjIwLCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUcmVlLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMudG9rZW4pIHx8ICEhdmlzaXRvcih0aGlzLmFubm90YXRpb24pIHx8ICEhdGhpcy52aXNpdEFyZ3VtZW50KHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdE5ld2xpbmVzKHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdEV4cHJlc3Npb24odmlzaXRvcik7IH1cbiAgICB9XG4gICAgZXhwb3J0IGNsYXNzIEFubm90YXRlZEJ1aWx0aW4gZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLkFubm90YXRlZEJ1aWx0aW47XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLkFubm90YXRlZEJ1aWx0aW47IH1cbiAgICAgICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IEFubm90YXRlZEJ1aWx0aW4geyByZXR1cm4gbmV3IEFubm90YXRlZEJ1aWx0aW4ocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgZ2V0IHRva2VuKCk6IFRva2VuLk9wZXJhdG9yIHsgcmV0dXJuIFRva2VuLk9wZXJhdG9yLnJlYWQodGhpcy5fdiwgNTIpOyB9XG4gICAgICAgIGdldCBhbm5vdGF0aW9uKCk6IFRva2VuLklkZW50IHsgcmV0dXJuIFRva2VuLklkZW50LnJlYWQodGhpcy5fdiwgMTI4KTsgfVxuICAgICAgICBnZXQgbmV3bGluZXMoKTogSXRlcmFibGVJdGVyYXRvcjxUb2tlbi5OZXdsaW5lPiB7IHJldHVybiByZWFkU2VxdWVuY2UodGhpcy5fdiwgMjExLCA3NiwgVG9rZW4uTmV3bGluZS5yZWFkKTsgfVxuICAgICAgICBnZXQgZXhwcmVzc2lvbigpOiBUcmVlIHwgdW5kZWZpbmVkIHsgcmV0dXJuIHJlYWRPcHRpb24odGhpcy5fdiwgMjE1LCBUcmVlLnJlYWQpOyB9XG4gICAgICAgIHZpc2l0TmV3bGluZXModmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRTZXF1ZW5jZSh0aGlzLl92LCAyMTEsIDc2LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUb2tlbi5OZXdsaW5lLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdEV4cHJlc3Npb24odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgMjE1LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUcmVlLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMudG9rZW4pIHx8ICEhdmlzaXRvcih0aGlzLmFubm90YXRpb24pIHx8ICEhdGhpcy52aXNpdE5ld2xpbmVzKHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdEV4cHJlc3Npb24odmlzaXRvcik7IH1cbiAgICB9XG4gICAgZXhwb3J0IGNsYXNzIERvY3VtZW50ZWQgZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLkRvY3VtZW50ZWQ7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLkRvY3VtZW50ZWQ7IH1cbiAgICAgICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IERvY3VtZW50ZWQgeyByZXR1cm4gbmV3IERvY3VtZW50ZWQocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgZ2V0IGRvY3VtZW50YXRpb24oKTogRG9jQ29tbWVudCB7IHJldHVybiBEb2NDb21tZW50LnJlYWQodGhpcy5fdiwgNTIpOyB9XG4gICAgICAgIGdldCBleHByZXNzaW9uKCk6IFRyZWUgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCAxMzYsIFRyZWUucmVhZCk7IH1cbiAgICAgICAgdmlzaXRFeHByZXNzaW9uKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0T3B0aW9uKHRoaXMuX3YsIDEzNiwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoVHJlZS5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLmRvY3VtZW50YXRpb24pIHx8ICEhdGhpcy52aXNpdEV4cHJlc3Npb24odmlzaXRvcik7IH1cbiAgICB9XG4gICAgZXhwb3J0IGNsYXNzIENvbnN0cnVjdG9yRGVmaW5pdGlvbiBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuQ29uc3RydWN0b3JEZWZpbml0aW9uO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5Db25zdHJ1Y3RvckRlZmluaXRpb247IH1cbiAgICAgICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IENvbnN0cnVjdG9yRGVmaW5pdGlvbiB7IHJldHVybiBuZXcgQ29uc3RydWN0b3JEZWZpbml0aW9uKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIGdldCBpZGVudCgpOiBUb2tlbi5JZGVudCB7IHJldHVybiBUb2tlbi5JZGVudC5yZWFkKHRoaXMuX3YsIDUyKTsgfVxuICAgICAgICBnZXQgYXJndW1lbnRzKCk6IEl0ZXJhYmxlSXRlcmF0b3I8QXJndW1lbnREZWZpbml0aW9uPiB7IHJldHVybiByZWFkU2VxdWVuY2UodGhpcy5fdiwgMTM1LCAzOSwgQXJndW1lbnREZWZpbml0aW9uLnJlYWQpOyB9XG4gICAgICAgIGdldCBibG9jaygpOiBJdGVyYWJsZUl0ZXJhdG9yPEFyZ3VtZW50RGVmaW5pdGlvbkxpbmU+IHsgcmV0dXJuIHJlYWRTZXF1ZW5jZSh0aGlzLl92LCAxMzksIDgxLCBBcmd1bWVudERlZmluaXRpb25MaW5lLnJlYWQpOyB9XG4gICAgICAgIHZpc2l0QXJndW1lbnRzKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0U2VxdWVuY2UodGhpcy5fdiwgMTM1LCAzOSwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoQXJndW1lbnREZWZpbml0aW9uLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgICAgICB2aXNpdEJsb2NrKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0U2VxdWVuY2UodGhpcy5fdiwgMTM5LCA4MSwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoQXJndW1lbnREZWZpbml0aW9uTGluZS5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLmlkZW50KSB8fCAhIXRoaXMudmlzaXRBcmd1bWVudHModmlzaXRvcikgfHwgISF0aGlzLnZpc2l0QmxvY2sodmlzaXRvcik7IH1cbiAgICB9XG4gICAgZXhwb3J0IHR5cGUgVHJlZSA9IEludmFsaWQgfCBCb2R5QmxvY2sgfCBBcmd1bWVudEJsb2NrQXBwbGljYXRpb24gfCBPcGVyYXRvckJsb2NrQXBwbGljYXRpb24gfCBJZGVudCB8IFByaXZhdGUgfCBOdW1iZXIgfCBXaWxkY2FyZCB8IFN1c3BlbmRlZERlZmF1bHRBcmd1bWVudHMgfCBUZXh0TGl0ZXJhbCB8IEFwcCB8IE5hbWVkQXBwIHwgT3ByQXBwIHwgVW5hcnlPcHJBcHAgfCBBdXRvc2NvcGVkSWRlbnRpZmllciB8IE9wclNlY3Rpb25Cb3VuZGFyeSB8IFRlbXBsYXRlRnVuY3Rpb24gfCBNdWx0aVNlZ21lbnRBcHAgfCBUeXBlRGVmIHwgQXNzaWdubWVudCB8IEZ1bmN0aW9uIHwgRm9yZWlnbkZ1bmN0aW9uIHwgSW1wb3J0IHwgRXhwb3J0IHwgR3JvdXAgfCBUeXBlU2lnbmF0dXJlIHwgVHlwZUFubm90YXRlZCB8IENhc2VPZiB8IExhbWJkYSB8IEFycmF5IHwgVHVwbGUgfCBBbm5vdGF0ZWQgfCBBbm5vdGF0ZWRCdWlsdGluIHwgRG9jdW1lbnRlZCB8IENvbnN0cnVjdG9yRGVmaW5pdGlvbjtcbiAgICBjb25zdCBWQVJJQU5UX1JFQURFUlMgPSBbSW52YWxpZC5yZWFkLCBCb2R5QmxvY2sucmVhZCwgQXJndW1lbnRCbG9ja0FwcGxpY2F0aW9uLnJlYWQsIE9wZXJhdG9yQmxvY2tBcHBsaWNhdGlvbi5yZWFkLCBJZGVudC5yZWFkLCBQcml2YXRlLnJlYWQsIE51bWJlci5yZWFkLCBXaWxkY2FyZC5yZWFkLCBTdXNwZW5kZWREZWZhdWx0QXJndW1lbnRzLnJlYWQsIFRleHRMaXRlcmFsLnJlYWQsIEFwcC5yZWFkLCBOYW1lZEFwcC5yZWFkLCBPcHJBcHAucmVhZCwgVW5hcnlPcHJBcHAucmVhZCwgQXV0b3Njb3BlZElkZW50aWZpZXIucmVhZCwgT3ByU2VjdGlvbkJvdW5kYXJ5LnJlYWQsIFRlbXBsYXRlRnVuY3Rpb24ucmVhZCwgTXVsdGlTZWdtZW50QXBwLnJlYWQsIFR5cGVEZWYucmVhZCwgQXNzaWdubWVudC5yZWFkLCBGdW5jdGlvbi5yZWFkLCBGb3JlaWduRnVuY3Rpb24ucmVhZCwgSW1wb3J0LnJlYWQsIEV4cG9ydC5yZWFkLCBHcm91cC5yZWFkLCBUeXBlU2lnbmF0dXJlLnJlYWQsIFR5cGVBbm5vdGF0ZWQucmVhZCwgQ2FzZU9mLnJlYWQsIExhbWJkYS5yZWFkLCBBcnJheS5yZWFkLCBUdXBsZS5yZWFkLCBBbm5vdGF0ZWQucmVhZCwgQW5ub3RhdGVkQnVpbHRpbi5yZWFkLCBEb2N1bWVudGVkLnJlYWQsIENvbnN0cnVjdG9yRGVmaW5pdGlvbi5yZWFkXTtcbiAgICBleHBvcnQgZnVuY3Rpb24gcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogVHJlZSB7IHJldHVybiByZWFkRW51bTxUcmVlPihWQVJJQU5UX1JFQURFUlMsIHZpZXcsIGFkZHJlc3MpOyB9XG4gICAgZXhwb3J0IGZ1bmN0aW9uIGlzSW5zdGFuY2Uob2JqOiB1bmtub3duKTogb2JqIGlzIFRyZWUgeyByZXR1cm4gb2JqIGluc3RhbmNlb2YgQWJzdHJhY3RCYXNlOyB9XG59XG5leHBvcnQgdHlwZSBUcmVlID0gVHJlZS5UcmVlXG5leHBvcnQgY2xhc3MgTXVsdGlTZWdtZW50QXBwU2VnbWVudCBleHRlbmRzIExhenlPYmplY3Qge1xuICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB9XG4gICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IE11bHRpU2VnbWVudEFwcFNlZ21lbnQgeyByZXR1cm4gbmV3IE11bHRpU2VnbWVudEFwcFNlZ21lbnQocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICBnZXQgaGVhZGVyKCk6IFRva2VuIHsgcmV0dXJuIFRva2VuLnJlYWQodGhpcy5fdiwgMCk7IH1cbiAgICBnZXQgYm9keSgpOiBUcmVlIHwgdW5kZWZpbmVkIHsgcmV0dXJuIHJlYWRPcHRpb24odGhpcy5fdiwgNCwgVHJlZS5yZWFkKTsgfVxuICAgIHZpc2l0Qm9keSh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCA0LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUcmVlLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKSB8fCAhIXZpc2l0b3IodGhpcy5oZWFkZXIpIHx8ICEhdGhpcy52aXNpdEJvZHkodmlzaXRvcik7IH1cbn1cbmV4cG9ydCBjbGFzcyBDYXNlTGluZSBleHRlbmRzIExhenlPYmplY3Qge1xuICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB9XG4gICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IENhc2VMaW5lIHsgcmV0dXJuIG5ldyBDYXNlTGluZShyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgIGdldCBuZXdsaW5lKCk6IFRva2VuLk5ld2xpbmUgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCAwLCBUb2tlbi5OZXdsaW5lLnJlYWQpOyB9XG4gICAgZ2V0IGNhc2UoKTogQ2FzZSB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDUsIENhc2UucmVhZCk7IH1cbiAgICB2aXNpdE5ld2xpbmUodmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgMCwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoVG9rZW4uTmV3bGluZS5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICB2aXNpdENhc2UodmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgNSwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoQ2FzZS5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF0aGlzLnZpc2l0TmV3bGluZSh2aXNpdG9yKSB8fCAhIXRoaXMudmlzaXRDYXNlKHZpc2l0b3IpOyB9XG59XG5leHBvcnQgbW9kdWxlIEJhc2Uge1xuICAgIGV4cG9ydCBhYnN0cmFjdCBjbGFzcyBBYnN0cmFjdEJhc2UgZXh0ZW5kcyBMYXp5T2JqZWN0IHtcbiAgICAgICAgcHJvdGVjdGVkIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY29uc3QgZW51bSBUeXBlIHtcbiAgICAgICAgQmluYXJ5ID0gMCxcbiAgICAgICAgT2N0YWwgPSAxLFxuICAgICAgICBIZXhhZGVjaW1hbCA9IDJcbiAgICB9XG4gICAgZXhwb3J0IGNvbnN0IHR5cGVOYW1lcyA9IFtcIkJpbmFyeVwiLCBcIk9jdGFsXCIsIFwiSGV4YWRlY2ltYWxcIl0gYXMgY29uc3Q7XG4gICAgZXhwb3J0IGNsYXNzIEJpbmFyeSBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuQmluYXJ5O1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5CaW5hcnk7IH1cbiAgICAgICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IEJpbmFyeSB7IHJldHVybiBuZXcgQmluYXJ5KHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgT2N0YWwgZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLk9jdGFsO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5PY3RhbDsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogT2N0YWwgeyByZXR1cm4gbmV3IE9jdGFsKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgSGV4YWRlY2ltYWwgZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLkhleGFkZWNpbWFsO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5IZXhhZGVjaW1hbDsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogSGV4YWRlY2ltYWwgeyByZXR1cm4gbmV3IEhleGFkZWNpbWFsKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgdHlwZSBCYXNlID0gQmluYXJ5IHwgT2N0YWwgfCBIZXhhZGVjaW1hbDtcbiAgICBjb25zdCBWQVJJQU5UX1JFQURFUlMgPSBbQmluYXJ5LnJlYWQsIE9jdGFsLnJlYWQsIEhleGFkZWNpbWFsLnJlYWRdO1xuICAgIGV4cG9ydCBmdW5jdGlvbiByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBCYXNlIHsgcmV0dXJuIHJlYWRFbnVtPEJhc2U+KFZBUklBTlRfUkVBREVSUywgdmlldywgYWRkcmVzcyk7IH1cbiAgICBleHBvcnQgZnVuY3Rpb24gaXNJbnN0YW5jZShvYmo6IHVua25vd24pOiBvYmogaXMgQmFzZSB7IHJldHVybiBvYmogaW5zdGFuY2VvZiBBYnN0cmFjdEJhc2U7IH1cbn1cbmV4cG9ydCB0eXBlIEJhc2UgPSBCYXNlLkJhc2VcbmV4cG9ydCBjbGFzcyBBcmd1bWVudERlZmluaXRpb25MaW5lIGV4dGVuZHMgTGF6eU9iamVjdCB7XG4gICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IH1cbiAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogQXJndW1lbnREZWZpbml0aW9uTGluZSB7IHJldHVybiBuZXcgQXJndW1lbnREZWZpbml0aW9uTGluZShyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgIGdldCBuZXdsaW5lKCk6IFRva2VuLk5ld2xpbmUgeyByZXR1cm4gVG9rZW4uTmV3bGluZS5yZWFkKHRoaXMuX3YsIDApOyB9XG4gICAgZ2V0IGFyZ3VtZW50KCk6IEFyZ3VtZW50RGVmaW5pdGlvbiB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDc2LCBBcmd1bWVudERlZmluaXRpb24ucmVhZCk7IH1cbiAgICB2aXNpdEFyZ3VtZW50KHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0T3B0aW9uKHRoaXMuX3YsIDc2LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihBcmd1bWVudERlZmluaXRpb24ucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLm5ld2xpbmUpIHx8ICEhdGhpcy52aXNpdEFyZ3VtZW50KHZpc2l0b3IpOyB9XG59XG5leHBvcnQgY2xhc3MgQXJndW1lbnREZWZhdWx0IGV4dGVuZHMgTGF6eU9iamVjdCB7XG4gICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IH1cbiAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogQXJndW1lbnREZWZhdWx0IHsgcmV0dXJuIG5ldyBBcmd1bWVudERlZmF1bHQocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICBnZXQgZXF1YWxzKCk6IFRva2VuLk9wZXJhdG9yIHsgcmV0dXJuIFRva2VuLk9wZXJhdG9yLnJlYWQodGhpcy5fdiwgMCk7IH1cbiAgICBnZXQgZXhwcmVzc2lvbigpOiBUcmVlIHsgcmV0dXJuIFRyZWUucmVhZCh0aGlzLl92LCA3Nik7IH1cbiAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMuZXF1YWxzKSB8fCAhIXZpc2l0b3IodGhpcy5leHByZXNzaW9uKTsgfVxufVxuZXhwb3J0IGNsYXNzIE9wZXJhdG9yQmxvY2tFeHByZXNzaW9uIGV4dGVuZHMgTGF6eU9iamVjdCB7XG4gICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IH1cbiAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogT3BlcmF0b3JCbG9ja0V4cHJlc3Npb24geyByZXR1cm4gbmV3IE9wZXJhdG9yQmxvY2tFeHByZXNzaW9uKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgZ2V0IG9wZXJhdG9yKCk6IFJlc3VsdDxUb2tlbi5PcGVyYXRvciwgTXVsdGlwbGVPcGVyYXRvckVycm9yPiB7IHJldHVybiByZWFkUmVzdWx0KHRoaXMuX3YsIDAsIFRva2VuLk9wZXJhdG9yLnJlYWQsIE11bHRpcGxlT3BlcmF0b3JFcnJvci5yZWFkKTsgfVxuICAgIGdldCBleHByZXNzaW9uKCk6IFRyZWUgeyByZXR1cm4gVHJlZS5yZWFkKHRoaXMuX3YsIDQpOyB9XG4gICAgdmlzaXRPcGVyYXRvcih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdFJlc3VsdCh0aGlzLl92LCAwLCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUb2tlbi5PcGVyYXRvci5yZWFkKHZpZXcsIGFkZHJlc3MpKSwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoTXVsdGlwbGVPcGVyYXRvckVycm9yLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKSB8fCAhIXRoaXMudmlzaXRPcGVyYXRvcih2aXNpdG9yKSB8fCAhIXZpc2l0b3IodGhpcy5leHByZXNzaW9uKTsgfVxufVxuZXhwb3J0IGNsYXNzIFJldHVyblNwZWNpZmljYXRpb24gZXh0ZW5kcyBMYXp5T2JqZWN0IHtcbiAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgfVxuICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBSZXR1cm5TcGVjaWZpY2F0aW9uIHsgcmV0dXJuIG5ldyBSZXR1cm5TcGVjaWZpY2F0aW9uKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgZ2V0IGFycm93KCk6IFRva2VuLk9wZXJhdG9yIHsgcmV0dXJuIFRva2VuLk9wZXJhdG9yLnJlYWQodGhpcy5fdiwgMCk7IH1cbiAgICBnZXQgdHlwZU5vZGUoKTogVHJlZSB7IHJldHVybiBUcmVlLnJlYWQodGhpcy5fdiwgNzYpOyB9XG4gICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLmFycm93KSB8fCAhIXZpc2l0b3IodGhpcy50eXBlTm9kZSk7IH1cbn1cbmV4cG9ydCBjbGFzcyBMaW5lIGV4dGVuZHMgTGF6eU9iamVjdCB7XG4gICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IH1cbiAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogTGluZSB7IHJldHVybiBuZXcgTGluZShyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgIGdldCBuZXdsaW5lKCk6IFRva2VuLk5ld2xpbmUgeyByZXR1cm4gVG9rZW4uTmV3bGluZS5yZWFkKHRoaXMuX3YsIDApOyB9XG4gICAgZ2V0IGV4cHJlc3Npb24oKTogVHJlZSB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDc2LCBUcmVlLnJlYWQpOyB9XG4gICAgdmlzaXRFeHByZXNzaW9uKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0T3B0aW9uKHRoaXMuX3YsIDc2LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUcmVlLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKSB8fCAhIXZpc2l0b3IodGhpcy5uZXdsaW5lKSB8fCAhIXRoaXMudmlzaXRFeHByZXNzaW9uKHZpc2l0b3IpOyB9XG59XG5leHBvcnQgY2xhc3MgT3BlcmF0b3JMaW5lIGV4dGVuZHMgTGF6eU9iamVjdCB7XG4gICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IH1cbiAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogT3BlcmF0b3JMaW5lIHsgcmV0dXJuIG5ldyBPcGVyYXRvckxpbmUocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICBnZXQgbmV3bGluZSgpOiBUb2tlbi5OZXdsaW5lIHsgcmV0dXJuIFRva2VuLk5ld2xpbmUucmVhZCh0aGlzLl92LCAwKTsgfVxuICAgIGdldCBleHByZXNzaW9uKCk6IE9wZXJhdG9yQmxvY2tFeHByZXNzaW9uIHwgdW5kZWZpbmVkIHsgcmV0dXJuIHJlYWRPcHRpb24odGhpcy5fdiwgNzYsIE9wZXJhdG9yQmxvY2tFeHByZXNzaW9uLnJlYWQpOyB9XG4gICAgdmlzaXRFeHByZXNzaW9uKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0T3B0aW9uKHRoaXMuX3YsIDc2LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihPcGVyYXRvckJsb2NrRXhwcmVzc2lvbi5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMubmV3bGluZSkgfHwgISF0aGlzLnZpc2l0RXhwcmVzc2lvbih2aXNpdG9yKTsgfVxufVxuZXhwb3J0IGNsYXNzIEFyZ3VtZW50VHlwZSBleHRlbmRzIExhenlPYmplY3Qge1xuICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB9XG4gICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IEFyZ3VtZW50VHlwZSB7IHJldHVybiBuZXcgQXJndW1lbnRUeXBlKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgZ2V0IG9wZXJhdG9yKCk6IFRva2VuLk9wZXJhdG9yIHsgcmV0dXJuIFRva2VuLk9wZXJhdG9yLnJlYWQodGhpcy5fdiwgMCk7IH1cbiAgICBnZXQgdHlwZU5vZGUoKTogVHJlZSB7IHJldHVybiBUcmVlLnJlYWQodGhpcy5fdiwgNzYpOyB9XG4gICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLm9wZXJhdG9yKSB8fCAhIXZpc2l0b3IodGhpcy50eXBlTm9kZSk7IH1cbn1cbmV4cG9ydCBjbGFzcyBDYXNlIGV4dGVuZHMgTGF6eU9iamVjdCB7XG4gICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IH1cbiAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogQ2FzZSB7IHJldHVybiBuZXcgQ2FzZShyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgIGdldCBkb2N1bWVudGF0aW9uKCk6IERvY0NvbW1lbnQgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCAwLCBEb2NDb21tZW50LnJlYWQpOyB9XG4gICAgZ2V0IHBhdHRlcm4oKTogVHJlZSB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDUsIFRyZWUucmVhZCk7IH1cbiAgICBnZXQgYXJyb3coKTogVG9rZW4uT3BlcmF0b3IgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCAxMCwgVG9rZW4uT3BlcmF0b3IucmVhZCk7IH1cbiAgICBnZXQgZXhwcmVzc2lvbigpOiBUcmVlIHwgdW5kZWZpbmVkIHsgcmV0dXJuIHJlYWRPcHRpb24odGhpcy5fdiwgMTUsIFRyZWUucmVhZCk7IH1cbiAgICB2aXNpdERvY3VtZW50YXRpb24odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgMCwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoRG9jQ29tbWVudC5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICB2aXNpdFBhdHRlcm4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgNSwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoVHJlZS5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICB2aXNpdEFycm93KHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0T3B0aW9uKHRoaXMuX3YsIDEwLCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUb2tlbi5PcGVyYXRvci5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICB2aXNpdEV4cHJlc3Npb24odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgMTUsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKFRyZWUucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdERvY3VtZW50YXRpb24odmlzaXRvcikgfHwgISF0aGlzLnZpc2l0UGF0dGVybih2aXNpdG9yKSB8fCAhIXRoaXMudmlzaXRBcnJvdyh2aXNpdG9yKSB8fCAhIXRoaXMudmlzaXRFeHByZXNzaW9uKHZpc2l0b3IpOyB9XG59XG5leHBvcnQgbW9kdWxlIFRva2VuIHtcbiAgICBleHBvcnQgYWJzdHJhY3QgY2xhc3MgQWJzdHJhY3RCYXNlIGV4dGVuZHMgTGF6eU9iamVjdCB7XG4gICAgICAgIHByb3RlY3RlZCBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgfVxuICAgICAgICBnZXQgbGVmdE9mZnNldFZpc2libGUoKTogbnVtYmVyIHsgcmV0dXJuIHJlYWRVMzIodGhpcy5fdiwgMCk7IH1cbiAgICAgICAgZ2V0IGxlZnRPZmZzZXRDb2RlUmVwckJlZ2luKCk6IG51bWJlciB7IHJldHVybiByZWFkVTMyKHRoaXMuX3YsIDQpOyB9XG4gICAgICAgIGdldCBsZWZ0T2Zmc2V0Q29kZVJlcHJMZW4oKTogbnVtYmVyIHsgcmV0dXJuIHJlYWRVMzIodGhpcy5fdiwgOCk7IH1cbiAgICAgICAgZ2V0IGxlZnRPZmZzZXRDb2RlU3RhcnRVdGY4KCk6IG51bWJlciB7IHJldHVybiByZWFkVTMyKHRoaXMuX3YsIDEyKTsgfVxuICAgICAgICBnZXQgd2hpdGVzcGFjZVN0YXJ0SW5Db2RlQnVmZmVyKCk6IG51bWJlciB7IHJldHVybiByZWFkVTMyKHRoaXMuX3YsIDE2KTsgfVxuICAgICAgICBnZXQgbGVmdE9mZnNldENvZGVTdGFydExpbmUoKTogbnVtYmVyIHsgcmV0dXJuIHJlYWRVMzIodGhpcy5fdiwgMjApOyB9XG4gICAgICAgIGdldCBsZWZ0T2Zmc2V0Q29kZVN0YXJ0Q29sMTYoKTogbnVtYmVyIHsgcmV0dXJuIHJlYWRVMzIodGhpcy5fdiwgMjQpOyB9XG4gICAgICAgIGdldCB3aGl0ZXNwYWNlTGVuZ3RoSW5Db2RlQnVmZmVyKCk6IG51bWJlciB7IHJldHVybiByZWFkVTMyKHRoaXMuX3YsIDI4KTsgfVxuICAgICAgICBnZXQgbGVmdE9mZnNldENvZGVMZW5OZXdsaW5lcygpOiBudW1iZXIgeyByZXR1cm4gcmVhZFUzMih0aGlzLl92LCAzMik7IH1cbiAgICAgICAgZ2V0IGxlZnRPZmZzZXRDb2RlTGVuTGluZUNoYXJzMTYoKTogbnVtYmVyIHsgcmV0dXJuIHJlYWRVMzIodGhpcy5fdiwgMzYpOyB9XG4gICAgICAgIGdldCBjb2RlUmVwckJlZ2luKCk6IG51bWJlciB7IHJldHVybiByZWFkVTMyKHRoaXMuX3YsIDQwKTsgfVxuICAgICAgICBnZXQgY29kZVJlcHJMZW4oKTogbnVtYmVyIHsgcmV0dXJuIHJlYWRVMzIodGhpcy5fdiwgNDQpOyB9XG4gICAgICAgIGdldCBjb2RlU3RhcnRVdGY4KCk6IG51bWJlciB7IHJldHVybiByZWFkVTMyKHRoaXMuX3YsIDQ4KTsgfVxuICAgICAgICBnZXQgc3RhcnRJbkNvZGVCdWZmZXIoKTogbnVtYmVyIHsgcmV0dXJuIHJlYWRVMzIodGhpcy5fdiwgNTIpOyB9XG4gICAgICAgIGdldCBjb2RlU3RhcnRMaW5lKCk6IG51bWJlciB7IHJldHVybiByZWFkVTMyKHRoaXMuX3YsIDU2KTsgfVxuICAgICAgICBnZXQgY29kZVN0YXJ0Q29sMTYoKTogbnVtYmVyIHsgcmV0dXJuIHJlYWRVMzIodGhpcy5fdiwgNjApOyB9XG4gICAgICAgIGdldCBsZW5ndGhJbkNvZGVCdWZmZXIoKTogbnVtYmVyIHsgcmV0dXJuIHJlYWRVMzIodGhpcy5fdiwgNjQpOyB9XG4gICAgICAgIGdldCBjb2RlTGVuTmV3bGluZXMoKTogbnVtYmVyIHsgcmV0dXJuIHJlYWRVMzIodGhpcy5fdiwgNjgpOyB9XG4gICAgICAgIGdldCBjb2RlTGVuTGluZUNoYXJzMTYoKTogbnVtYmVyIHsgcmV0dXJuIHJlYWRVMzIodGhpcy5fdiwgNzIpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY29uc3QgZW51bSBUeXBlIHtcbiAgICAgICAgTmV3bGluZSA9IDAsXG4gICAgICAgIE9wZW5TeW1ib2wgPSAxLFxuICAgICAgICBDbG9zZVN5bWJvbCA9IDIsXG4gICAgICAgIEJsb2NrU3RhcnQgPSAzLFxuICAgICAgICBCbG9ja0VuZCA9IDQsXG4gICAgICAgIFdpbGRjYXJkID0gNSxcbiAgICAgICAgU3VzcGVuZGVkRGVmYXVsdEFyZ3VtZW50cyA9IDYsXG4gICAgICAgIElkZW50ID0gNyxcbiAgICAgICAgT3BlcmF0b3IgPSA4LFxuICAgICAgICBEaWdpdHMgPSA5LFxuICAgICAgICBOdW1iZXJCYXNlID0gMTAsXG4gICAgICAgIFByaXZhdGUgPSAxMSxcbiAgICAgICAgVGV4dFN0YXJ0ID0gMTIsXG4gICAgICAgIFRleHRFbmQgPSAxMyxcbiAgICAgICAgVGV4dFNlY3Rpb24gPSAxNCxcbiAgICAgICAgVGV4dEVzY2FwZSA9IDE1LFxuICAgICAgICBUZXh0SW5pdGlhbE5ld2xpbmUgPSAxNixcbiAgICAgICAgVGV4dE5ld2xpbmUgPSAxNyxcbiAgICAgICAgSW52YWxpZCA9IDE4XG4gICAgfVxuICAgIGV4cG9ydCBjb25zdCB0eXBlTmFtZXMgPSBbXCJOZXdsaW5lXCIsIFwiT3BlblN5bWJvbFwiLCBcIkNsb3NlU3ltYm9sXCIsIFwiQmxvY2tTdGFydFwiLCBcIkJsb2NrRW5kXCIsIFwiV2lsZGNhcmRcIiwgXCJTdXNwZW5kZWREZWZhdWx0QXJndW1lbnRzXCIsIFwiSWRlbnRcIiwgXCJPcGVyYXRvclwiLCBcIkRpZ2l0c1wiLCBcIk51bWJlckJhc2VcIiwgXCJQcml2YXRlXCIsIFwiVGV4dFN0YXJ0XCIsIFwiVGV4dEVuZFwiLCBcIlRleHRTZWN0aW9uXCIsIFwiVGV4dEVzY2FwZVwiLCBcIlRleHRJbml0aWFsTmV3bGluZVwiLCBcIlRleHROZXdsaW5lXCIsIFwiSW52YWxpZFwiXSBhcyBjb25zdDtcbiAgICBleHBvcnQgY2xhc3MgTmV3bGluZSBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuTmV3bGluZTtcbiAgICAgICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IHRoaXMudHlwZSA9IFR5cGUuTmV3bGluZTsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogTmV3bGluZSB7IHJldHVybiBuZXcgTmV3bGluZShyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgICAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcik7IH1cbiAgICB9XG4gICAgZXhwb3J0IGNsYXNzIE9wZW5TeW1ib2wgZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLk9wZW5TeW1ib2w7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLk9wZW5TeW1ib2w7IH1cbiAgICAgICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IE9wZW5TeW1ib2wgeyByZXR1cm4gbmV3IE9wZW5TeW1ib2wocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBDbG9zZVN5bWJvbCBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuQ2xvc2VTeW1ib2w7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLkNsb3NlU3ltYm9sOyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBDbG9zZVN5bWJvbCB7IHJldHVybiBuZXcgQ2xvc2VTeW1ib2wocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBCbG9ja1N0YXJ0IGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5CbG9ja1N0YXJ0O1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5CbG9ja1N0YXJ0OyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBCbG9ja1N0YXJ0IHsgcmV0dXJuIG5ldyBCbG9ja1N0YXJ0KHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgQmxvY2tFbmQgZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLkJsb2NrRW5kO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5CbG9ja0VuZDsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogQmxvY2tFbmQgeyByZXR1cm4gbmV3IEJsb2NrRW5kKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgV2lsZGNhcmQgZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLldpbGRjYXJkO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5XaWxkY2FyZDsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogV2lsZGNhcmQgeyByZXR1cm4gbmV3IFdpbGRjYXJkKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIGdldCBsaWZ0TGV2ZWwoKTogbnVtYmVyIHsgcmV0dXJuIHJlYWRVMzIodGhpcy5fdiwgNzYpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgU3VzcGVuZGVkRGVmYXVsdEFyZ3VtZW50cyBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuU3VzcGVuZGVkRGVmYXVsdEFyZ3VtZW50cztcbiAgICAgICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IHRoaXMudHlwZSA9IFR5cGUuU3VzcGVuZGVkRGVmYXVsdEFyZ3VtZW50czsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogU3VzcGVuZGVkRGVmYXVsdEFyZ3VtZW50cyB7IHJldHVybiBuZXcgU3VzcGVuZGVkRGVmYXVsdEFyZ3VtZW50cyhyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgICAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcik7IH1cbiAgICB9XG4gICAgZXhwb3J0IGNsYXNzIElkZW50IGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5JZGVudDtcbiAgICAgICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IHRoaXMudHlwZSA9IFR5cGUuSWRlbnQ7IH1cbiAgICAgICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IElkZW50IHsgcmV0dXJuIG5ldyBJZGVudChyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgICAgICBnZXQgaXNGcmVlKCk6IGJvb2xlYW4geyByZXR1cm4gcmVhZEJvb2wodGhpcy5fdiwgNzYpOyB9XG4gICAgICAgIGdldCBsaWZ0TGV2ZWwoKTogbnVtYmVyIHsgcmV0dXJuIHJlYWRVMzIodGhpcy5fdiwgNzcpOyB9XG4gICAgICAgIGdldCBpc1R5cGVPckNvbnN0cnVjdG9yKCk6IGJvb2xlYW4geyByZXR1cm4gcmVhZEJvb2wodGhpcy5fdiwgODEpOyB9XG4gICAgICAgIGdldCBpc09wZXJhdG9yTGV4aWNhbGx5KCk6IGJvb2xlYW4geyByZXR1cm4gcmVhZEJvb2wodGhpcy5fdiwgODIpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgT3BlcmF0b3IgZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLk9wZXJhdG9yO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5PcGVyYXRvcjsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogT3BlcmF0b3IgeyByZXR1cm4gbmV3IE9wZXJhdG9yKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgRGlnaXRzIGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5EaWdpdHM7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLkRpZ2l0czsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogRGlnaXRzIHsgcmV0dXJuIG5ldyBEaWdpdHMocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgZ2V0IGJhc2UoKTogQmFzZSB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDc2LCBCYXNlLnJlYWQpOyB9XG4gICAgICAgIHZpc2l0QmFzZSh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCA3NiwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoQmFzZS5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdEJhc2UodmlzaXRvcik7IH1cbiAgICB9XG4gICAgZXhwb3J0IGNsYXNzIE51bWJlckJhc2UgZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLk51bWJlckJhc2U7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLk51bWJlckJhc2U7IH1cbiAgICAgICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IE51bWJlckJhc2UgeyByZXR1cm4gbmV3IE51bWJlckJhc2UocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBQcml2YXRlIGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5Qcml2YXRlO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5Qcml2YXRlOyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBQcml2YXRlIHsgcmV0dXJuIG5ldyBQcml2YXRlKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgVGV4dFN0YXJ0IGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5UZXh0U3RhcnQ7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLlRleHRTdGFydDsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogVGV4dFN0YXJ0IHsgcmV0dXJuIG5ldyBUZXh0U3RhcnQocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBUZXh0RW5kIGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5UZXh0RW5kO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5UZXh0RW5kOyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBUZXh0RW5kIHsgcmV0dXJuIG5ldyBUZXh0RW5kKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgVGV4dFNlY3Rpb24gZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLlRleHRTZWN0aW9uO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5UZXh0U2VjdGlvbjsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogVGV4dFNlY3Rpb24geyByZXR1cm4gbmV3IFRleHRTZWN0aW9uKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgVGV4dEVzY2FwZSBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuVGV4dEVzY2FwZTtcbiAgICAgICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IHRoaXMudHlwZSA9IFR5cGUuVGV4dEVzY2FwZTsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogVGV4dEVzY2FwZSB7IHJldHVybiBuZXcgVGV4dEVzY2FwZShyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgICAgICBnZXQgdmFsdWUoKTogbnVtYmVyIHsgcmV0dXJuIHJlYWRVMzIodGhpcy5fdiwgNzYpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgVGV4dEluaXRpYWxOZXdsaW5lIGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5UZXh0SW5pdGlhbE5ld2xpbmU7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLlRleHRJbml0aWFsTmV3bGluZTsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogVGV4dEluaXRpYWxOZXdsaW5lIHsgcmV0dXJuIG5ldyBUZXh0SW5pdGlhbE5ld2xpbmUocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBUZXh0TmV3bGluZSBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuVGV4dE5ld2xpbmU7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLlRleHROZXdsaW5lOyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBUZXh0TmV3bGluZSB7IHJldHVybiBuZXcgVGV4dE5ld2xpbmUocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBJbnZhbGlkIGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5JbnZhbGlkO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5JbnZhbGlkOyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBJbnZhbGlkIHsgcmV0dXJuIG5ldyBJbnZhbGlkKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKTsgfVxuICAgIH1cbiAgICBleHBvcnQgdHlwZSBUb2tlbiA9IE5ld2xpbmUgfCBPcGVuU3ltYm9sIHwgQ2xvc2VTeW1ib2wgfCBCbG9ja1N0YXJ0IHwgQmxvY2tFbmQgfCBXaWxkY2FyZCB8IFN1c3BlbmRlZERlZmF1bHRBcmd1bWVudHMgfCBJZGVudCB8IE9wZXJhdG9yIHwgRGlnaXRzIHwgTnVtYmVyQmFzZSB8IFByaXZhdGUgfCBUZXh0U3RhcnQgfCBUZXh0RW5kIHwgVGV4dFNlY3Rpb24gfCBUZXh0RXNjYXBlIHwgVGV4dEluaXRpYWxOZXdsaW5lIHwgVGV4dE5ld2xpbmUgfCBJbnZhbGlkO1xuICAgIGNvbnN0IFZBUklBTlRfUkVBREVSUyA9IFtOZXdsaW5lLnJlYWQsIE9wZW5TeW1ib2wucmVhZCwgQ2xvc2VTeW1ib2wucmVhZCwgQmxvY2tTdGFydC5yZWFkLCBCbG9ja0VuZC5yZWFkLCBXaWxkY2FyZC5yZWFkLCBTdXNwZW5kZWREZWZhdWx0QXJndW1lbnRzLnJlYWQsIElkZW50LnJlYWQsIE9wZXJhdG9yLnJlYWQsIERpZ2l0cy5yZWFkLCBOdW1iZXJCYXNlLnJlYWQsIFByaXZhdGUucmVhZCwgVGV4dFN0YXJ0LnJlYWQsIFRleHRFbmQucmVhZCwgVGV4dFNlY3Rpb24ucmVhZCwgVGV4dEVzY2FwZS5yZWFkLCBUZXh0SW5pdGlhbE5ld2xpbmUucmVhZCwgVGV4dE5ld2xpbmUucmVhZCwgSW52YWxpZC5yZWFkXTtcbiAgICBleHBvcnQgZnVuY3Rpb24gcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogVG9rZW4geyByZXR1cm4gcmVhZEVudW08VG9rZW4+KFZBUklBTlRfUkVBREVSUywgdmlldywgYWRkcmVzcyk7IH1cbiAgICBleHBvcnQgZnVuY3Rpb24gaXNJbnN0YW5jZShvYmo6IHVua25vd24pOiBvYmogaXMgVG9rZW4geyByZXR1cm4gb2JqIGluc3RhbmNlb2YgQWJzdHJhY3RCYXNlOyB9XG59XG5leHBvcnQgdHlwZSBUb2tlbiA9IFRva2VuLlRva2VuXG5leHBvcnQgbW9kdWxlIFRleHRFbGVtZW50IHtcbiAgICBleHBvcnQgYWJzdHJhY3QgY2xhc3MgQWJzdHJhY3RCYXNlIGV4dGVuZHMgTGF6eU9iamVjdCB7XG4gICAgICAgIHByb3RlY3RlZCBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgfVxuICAgICAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcik7IH1cbiAgICB9XG4gICAgZXhwb3J0IGNvbnN0IGVudW0gVHlwZSB7XG4gICAgICAgIFNlY3Rpb24gPSAwLFxuICAgICAgICBFc2NhcGUgPSAxLFxuICAgICAgICBOZXdsaW5lID0gMixcbiAgICAgICAgU3BsaWNlID0gM1xuICAgIH1cbiAgICBleHBvcnQgY29uc3QgdHlwZU5hbWVzID0gW1wiU2VjdGlvblwiLCBcIkVzY2FwZVwiLCBcIk5ld2xpbmVcIiwgXCJTcGxpY2VcIl0gYXMgY29uc3Q7XG4gICAgZXhwb3J0IGNsYXNzIFNlY3Rpb24gZXh0ZW5kcyBBYnN0cmFjdEJhc2Uge1xuICAgICAgICByZWFkb25seSB0eXBlOiBUeXBlLlNlY3Rpb247XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLlNlY3Rpb247IH1cbiAgICAgICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IFNlY3Rpb24geyByZXR1cm4gbmV3IFNlY3Rpb24ocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgZ2V0IHRleHQoKTogVG9rZW4uVGV4dFNlY3Rpb24geyByZXR1cm4gVG9rZW4uVGV4dFNlY3Rpb24ucmVhZCh0aGlzLl92LCAwKTsgfVxuICAgICAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMudGV4dCk7IH1cbiAgICB9XG4gICAgZXhwb3J0IGNsYXNzIEVzY2FwZSBleHRlbmRzIEFic3RyYWN0QmFzZSB7XG4gICAgICAgIHJlYWRvbmx5IHR5cGU6IFR5cGUuRXNjYXBlO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5Fc2NhcGU7IH1cbiAgICAgICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IEVzY2FwZSB7IHJldHVybiBuZXcgRXNjYXBlKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIGdldCB0b2tlbigpOiBUb2tlbi5UZXh0RXNjYXBlIHsgcmV0dXJuIFRva2VuLlRleHRFc2NhcGUucmVhZCh0aGlzLl92LCAwKTsgfVxuICAgICAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMudG9rZW4pOyB9XG4gICAgfVxuICAgIGV4cG9ydCBjbGFzcyBOZXdsaW5lIGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5OZXdsaW5lO1xuICAgICAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgdGhpcy50eXBlID0gVHlwZS5OZXdsaW5lOyB9XG4gICAgICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBOZXdsaW5lIHsgcmV0dXJuIG5ldyBOZXdsaW5lKHJlYWRPZmZzZXQodmlldywgYWRkcmVzcykpOyB9XG4gICAgICAgIGdldCBuZXdsaW5lKCk6IFRva2VuLk5ld2xpbmUgeyByZXR1cm4gVG9rZW4uTmV3bGluZS5yZWFkKHRoaXMuX3YsIDApOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKSB8fCAhIXZpc2l0b3IodGhpcy5uZXdsaW5lKTsgfVxuICAgIH1cbiAgICBleHBvcnQgY2xhc3MgU3BsaWNlIGV4dGVuZHMgQWJzdHJhY3RCYXNlIHtcbiAgICAgICAgcmVhZG9ubHkgdHlwZTogVHlwZS5TcGxpY2U7XG4gICAgICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB0aGlzLnR5cGUgPSBUeXBlLlNwbGljZTsgfVxuICAgICAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogU3BsaWNlIHsgcmV0dXJuIG5ldyBTcGxpY2UocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICAgICAgZ2V0IG9wZW4oKTogVG9rZW4uT3BlblN5bWJvbCB7IHJldHVybiBUb2tlbi5PcGVuU3ltYm9sLnJlYWQodGhpcy5fdiwgMCk7IH1cbiAgICAgICAgZ2V0IGV4cHJlc3Npb24oKTogVHJlZSB8IHVuZGVmaW5lZCB7IHJldHVybiByZWFkT3B0aW9uKHRoaXMuX3YsIDc2LCBUcmVlLnJlYWQpOyB9XG4gICAgICAgIGdldCBjbG9zZSgpOiBUb2tlbi5DbG9zZVN5bWJvbCB7IHJldHVybiBUb2tlbi5DbG9zZVN5bWJvbC5yZWFkKHRoaXMuX3YsIDgxKTsgfVxuICAgICAgICB2aXNpdEV4cHJlc3Npb24odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgNzYsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKFRyZWUucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgICAgIHZpc2l0Q2hpbGRyZW4odmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gc3VwZXIudmlzaXRDaGlsZHJlbih2aXNpdG9yKSB8fCAhIXZpc2l0b3IodGhpcy5vcGVuKSB8fCAhIXRoaXMudmlzaXRFeHByZXNzaW9uKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLmNsb3NlKTsgfVxuICAgIH1cbiAgICBleHBvcnQgdHlwZSBUZXh0RWxlbWVudCA9IFNlY3Rpb24gfCBFc2NhcGUgfCBOZXdsaW5lIHwgU3BsaWNlO1xuICAgIGNvbnN0IFZBUklBTlRfUkVBREVSUyA9IFtTZWN0aW9uLnJlYWQsIEVzY2FwZS5yZWFkLCBOZXdsaW5lLnJlYWQsIFNwbGljZS5yZWFkXTtcbiAgICBleHBvcnQgZnVuY3Rpb24gcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogVGV4dEVsZW1lbnQgeyByZXR1cm4gcmVhZEVudW08VGV4dEVsZW1lbnQ+KFZBUklBTlRfUkVBREVSUywgdmlldywgYWRkcmVzcyk7IH1cbiAgICBleHBvcnQgZnVuY3Rpb24gaXNJbnN0YW5jZShvYmo6IHVua25vd24pOiBvYmogaXMgVGV4dEVsZW1lbnQgeyByZXR1cm4gb2JqIGluc3RhbmNlb2YgQWJzdHJhY3RCYXNlOyB9XG59XG5leHBvcnQgdHlwZSBUZXh0RWxlbWVudCA9IFRleHRFbGVtZW50LlRleHRFbGVtZW50XG5leHBvcnQgY2xhc3MgRnJhY3Rpb25hbERpZ2l0cyBleHRlbmRzIExhenlPYmplY3Qge1xuICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB9XG4gICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IEZyYWN0aW9uYWxEaWdpdHMgeyByZXR1cm4gbmV3IEZyYWN0aW9uYWxEaWdpdHMocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICBnZXQgZG90KCk6IFRva2VuLk9wZXJhdG9yIHsgcmV0dXJuIFRva2VuLk9wZXJhdG9yLnJlYWQodGhpcy5fdiwgMCk7IH1cbiAgICBnZXQgZGlnaXRzKCk6IFRva2VuLkRpZ2l0cyB7IHJldHVybiBUb2tlbi5EaWdpdHMucmVhZCh0aGlzLl92LCA3Nik7IH1cbiAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMuZG90KSB8fCAhIXZpc2l0b3IodGhpcy5kaWdpdHMpOyB9XG59XG5leHBvcnQgY2xhc3MgTXVsdGlwbGVPcGVyYXRvckVycm9yIGV4dGVuZHMgTGF6eU9iamVjdCB7XG4gICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IH1cbiAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogTXVsdGlwbGVPcGVyYXRvckVycm9yIHsgcmV0dXJuIG5ldyBNdWx0aXBsZU9wZXJhdG9yRXJyb3IocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICBnZXQgb3BlcmF0b3JzKCk6IEl0ZXJhYmxlSXRlcmF0b3I8VG9rZW4uT3BlcmF0b3I+IHsgcmV0dXJuIHJlYWRTZXF1ZW5jZSh0aGlzLl92LCAwLCA3NiwgVG9rZW4uT3BlcmF0b3IucmVhZCk7IH1cbiAgICB2aXNpdE9wZXJhdG9ycyh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdFNlcXVlbmNlKHRoaXMuX3YsIDAsIDc2LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUb2tlbi5PcGVyYXRvci5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF0aGlzLnZpc2l0T3BlcmF0b3JzKHZpc2l0b3IpOyB9XG59XG5leHBvcnQgY2xhc3MgT3BlcmF0b3JEZWxpbWl0ZWRUcmVlIGV4dGVuZHMgTGF6eU9iamVjdCB7XG4gICAgY29uc3RydWN0b3IodmlldzogRGF0YVZpZXcpIHsgc3VwZXIodmlldyk7IH1cbiAgICBzdGF0aWMgcmVhZCh2aWV3OiBEYXRhVmlldywgYWRkcmVzczogbnVtYmVyKTogT3BlcmF0b3JEZWxpbWl0ZWRUcmVlIHsgcmV0dXJuIG5ldyBPcGVyYXRvckRlbGltaXRlZFRyZWUocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICBnZXQgb3BlcmF0b3IoKTogVG9rZW4uT3BlcmF0b3IgeyByZXR1cm4gVG9rZW4uT3BlcmF0b3IucmVhZCh0aGlzLl92LCAwKTsgfVxuICAgIGdldCBib2R5KCk6IFRyZWUgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCA3NiwgVHJlZS5yZWFkKTsgfVxuICAgIHZpc2l0Qm9keSh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCA3NiwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoVHJlZS5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF2aXNpdG9yKHRoaXMub3BlcmF0b3IpIHx8ICEhdGhpcy52aXNpdEJvZHkodmlzaXRvcik7IH1cbn1cbmV4cG9ydCBjbGFzcyBBcmd1bWVudERlZmluaXRpb24gZXh0ZW5kcyBMYXp5T2JqZWN0IHtcbiAgICBjb25zdHJ1Y3Rvcih2aWV3OiBEYXRhVmlldykgeyBzdXBlcih2aWV3KTsgfVxuICAgIHN0YXRpYyByZWFkKHZpZXc6IERhdGFWaWV3LCBhZGRyZXNzOiBudW1iZXIpOiBBcmd1bWVudERlZmluaXRpb24geyByZXR1cm4gbmV3IEFyZ3VtZW50RGVmaW5pdGlvbihyZWFkT2Zmc2V0KHZpZXcsIGFkZHJlc3MpKTsgfVxuICAgIGdldCBvcGVuKCk6IFRva2VuLk9wZW5TeW1ib2wgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCAwLCBUb2tlbi5PcGVuU3ltYm9sLnJlYWQpOyB9XG4gICAgZ2V0IG9wZW4yKCk6IFRva2VuLk9wZW5TeW1ib2wgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCA1LCBUb2tlbi5PcGVuU3ltYm9sLnJlYWQpOyB9XG4gICAgZ2V0IHN1c3BlbnNpb24oKTogVG9rZW4uT3BlcmF0b3IgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCAxMCwgVG9rZW4uT3BlcmF0b3IucmVhZCk7IH1cbiAgICBnZXQgcGF0dGVybigpOiBUcmVlIHsgcmV0dXJuIFRyZWUucmVhZCh0aGlzLl92LCAxNSk7IH1cbiAgICBnZXQgdHlwZU5vZGUoKTogQXJndW1lbnRUeXBlIHwgdW5kZWZpbmVkIHsgcmV0dXJuIHJlYWRPcHRpb24odGhpcy5fdiwgMTksIEFyZ3VtZW50VHlwZS5yZWFkKTsgfVxuICAgIGdldCBjbG9zZTIoKTogVG9rZW4uQ2xvc2VTeW1ib2wgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCAyNCwgVG9rZW4uQ2xvc2VTeW1ib2wucmVhZCk7IH1cbiAgICBnZXQgZGVmYXVsdCgpOiBBcmd1bWVudERlZmF1bHQgfCB1bmRlZmluZWQgeyByZXR1cm4gcmVhZE9wdGlvbih0aGlzLl92LCAyOSwgQXJndW1lbnREZWZhdWx0LnJlYWQpOyB9XG4gICAgZ2V0IGNsb3NlKCk6IFRva2VuLkNsb3NlU3ltYm9sIHwgdW5kZWZpbmVkIHsgcmV0dXJuIHJlYWRPcHRpb24odGhpcy5fdiwgMzQsIFRva2VuLkNsb3NlU3ltYm9sLnJlYWQpOyB9XG4gICAgdmlzaXRPcGVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0T3B0aW9uKHRoaXMuX3YsIDAsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKFRva2VuLk9wZW5TeW1ib2wucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgdmlzaXRPcGVuMih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCA1LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUb2tlbi5PcGVuU3ltYm9sLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgIHZpc2l0U3VzcGVuc2lvbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCAxMCwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoVG9rZW4uT3BlcmF0b3IucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgdmlzaXRUeXBlTm9kZSh2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiB2aXNpdE9wdGlvbih0aGlzLl92LCAxOSwgKHZpZXcsIGFkZHJlc3MpID0+IHZpc2l0b3IoQXJndW1lbnRUeXBlLnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgIHZpc2l0Q2xvc2UyKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0T3B0aW9uKHRoaXMuX3YsIDI0LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUb2tlbi5DbG9zZVN5bWJvbC5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICB2aXNpdERlZmF1bHQodmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRPcHRpb24odGhpcy5fdiwgMjksICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKEFyZ3VtZW50RGVmYXVsdC5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICB2aXNpdENsb3NlKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0T3B0aW9uKHRoaXMuX3YsIDM0LCAodmlldywgYWRkcmVzcykgPT4gdmlzaXRvcihUb2tlbi5DbG9zZVN5bWJvbC5yZWFkKHZpZXcsIGFkZHJlc3MpKSk7IH1cbiAgICB2aXNpdENoaWxkcmVuKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHN1cGVyLnZpc2l0Q2hpbGRyZW4odmlzaXRvcikgfHwgISF0aGlzLnZpc2l0T3Blbih2aXNpdG9yKSB8fCAhIXRoaXMudmlzaXRPcGVuMih2aXNpdG9yKSB8fCAhIXRoaXMudmlzaXRTdXNwZW5zaW9uKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLnBhdHRlcm4pIHx8ICEhdGhpcy52aXNpdFR5cGVOb2RlKHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdENsb3NlMih2aXNpdG9yKSB8fCAhIXRoaXMudmlzaXREZWZhdWx0KHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdENsb3NlKHZpc2l0b3IpOyB9XG59XG5leHBvcnQgY2xhc3MgRG9jQ29tbWVudCBleHRlbmRzIExhenlPYmplY3Qge1xuICAgIGNvbnN0cnVjdG9yKHZpZXc6IERhdGFWaWV3KSB7IHN1cGVyKHZpZXcpOyB9XG4gICAgc3RhdGljIHJlYWQodmlldzogRGF0YVZpZXcsIGFkZHJlc3M6IG51bWJlcik6IERvY0NvbW1lbnQgeyByZXR1cm4gbmV3IERvY0NvbW1lbnQocmVhZE9mZnNldCh2aWV3LCBhZGRyZXNzKSk7IH1cbiAgICBnZXQgb3BlbigpOiBUb2tlbi5UZXh0U3RhcnQgeyByZXR1cm4gVG9rZW4uVGV4dFN0YXJ0LnJlYWQodGhpcy5fdiwgMCk7IH1cbiAgICBnZXQgZWxlbWVudHMoKTogSXRlcmFibGVJdGVyYXRvcjxUZXh0RWxlbWVudD4geyByZXR1cm4gcmVhZFNlcXVlbmNlKHRoaXMuX3YsIDc2LCA0LCBUZXh0RWxlbWVudC5yZWFkKTsgfVxuICAgIGdldCBuZXdsaW5lcygpOiBJdGVyYWJsZUl0ZXJhdG9yPFRva2VuLk5ld2xpbmU+IHsgcmV0dXJuIHJlYWRTZXF1ZW5jZSh0aGlzLl92LCA4MCwgNzYsIFRva2VuLk5ld2xpbmUucmVhZCk7IH1cbiAgICB2aXNpdEVsZW1lbnRzKHZpc2l0b3I6IE9iamVjdFZpc2l0b3IpOiBib29sZWFuIHsgcmV0dXJuIHZpc2l0U2VxdWVuY2UodGhpcy5fdiwgNzYsIDQsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKFRleHRFbGVtZW50LnJlYWQodmlldywgYWRkcmVzcykpKTsgfVxuICAgIHZpc2l0TmV3bGluZXModmlzaXRvcjogT2JqZWN0VmlzaXRvcik6IGJvb2xlYW4geyByZXR1cm4gdmlzaXRTZXF1ZW5jZSh0aGlzLl92LCA4MCwgNzYsICh2aWV3LCBhZGRyZXNzKSA9PiB2aXNpdG9yKFRva2VuLk5ld2xpbmUucmVhZCh2aWV3LCBhZGRyZXNzKSkpOyB9XG4gICAgdmlzaXRDaGlsZHJlbih2aXNpdG9yOiBPYmplY3RWaXNpdG9yKTogYm9vbGVhbiB7IHJldHVybiBzdXBlci52aXNpdENoaWxkcmVuKHZpc2l0b3IpIHx8ICEhdmlzaXRvcih0aGlzLm9wZW4pIHx8ICEhdGhpcy52aXNpdEVsZW1lbnRzKHZpc2l0b3IpIHx8ICEhdGhpcy52aXNpdE5ld2xpbmVzKHZpc2l0b3IpOyB9XG59XG4iLCAiY29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2Rpcm5hbWUgPSBcIkM6XFxcXFByb2plY3RzXFxcXGVuc29cXFxcZW5zb1xcXFxhcHBcXFxcZ3VpMlxcXFxzaGFyZWRcXFxcYXN0XCI7Y29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2ZpbGVuYW1lID0gXCJDOlxcXFxQcm9qZWN0c1xcXFxlbnNvXFxcXGVuc29cXFxcYXBwXFxcXGd1aTJcXFxcc2hhcmVkXFxcXGFzdFxcXFxwYXJzZS50c1wiO2NvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9pbXBvcnRfbWV0YV91cmwgPSBcImZpbGU6Ly8vQzovUHJvamVjdHMvZW5zby9lbnNvL2FwcC9ndWkyL3NoYXJlZC9hc3QvcGFyc2UudHNcIjtpbXBvcnQgKiBhcyBtYXAgZnJvbSAnbGliMC9tYXAnXG5pbXBvcnQgdHlwZSB7IEFzdElkLCBNb2R1bGUsIE5vZGVDaGlsZCwgT3duZWQsIE93bmVkUmVmcywgVGV4dEVsZW1lbnQsIFRleHRUb2tlbiB9IGZyb20gJy4nXG5pbXBvcnQge1xuICBUb2tlbixcbiAgYXNPd25lZCxcbiAgaXNUb2tlbklkLFxuICBuZXdFeHRlcm5hbElkLFxuICBwYXJlbnRJZCxcbiAgcmV3cml0ZVJlZnMsXG4gIHN1YnRyZWVSb290cyxcbiAgc3luY0ZpZWxkcyxcbiAgc3luY05vZGVNZXRhZGF0YSxcbn0gZnJvbSAnLidcbmltcG9ydCB7IGFzc2VydCwgYXNzZXJ0RGVmaW5lZCwgYXNzZXJ0RXF1YWwgfSBmcm9tICcuLi91dGlsL2Fzc2VydCdcbmltcG9ydCB7IHRyeUdldFNvbGVWYWx1ZSwgemlwIH0gZnJvbSAnLi4vdXRpbC9kYXRhL2l0ZXJhYmxlJ1xuaW1wb3J0IHR5cGUgeyBTb3VyY2VSYW5nZUVkaXQsIFNwYW5UcmVlIH0gZnJvbSAnLi4vdXRpbC9kYXRhL3RleHQnXG5pbXBvcnQge1xuICBhcHBseVRleHRFZGl0cyxcbiAgYXBwbHlUZXh0RWRpdHNUb1NwYW5zLFxuICBlbmNsb3NpbmdTcGFucyxcbiAgdGV4dENoYW5nZVRvRWRpdHMsXG4gIHRyaW1FbmQsXG59IGZyb20gJy4uL3V0aWwvZGF0YS90ZXh0J1xuaW1wb3J0IHtcbiAgSWRNYXAsXG4gIGlzVXVpZCxcbiAgcmFuZ2VMZW5ndGgsXG4gIHNvdXJjZVJhbmdlRnJvbUtleSxcbiAgc291cmNlUmFuZ2VLZXksXG4gIHR5cGUgU291cmNlUmFuZ2UsXG4gIHR5cGUgU291cmNlUmFuZ2VLZXksXG59IGZyb20gJy4uL3lqc01vZGVsJ1xuaW1wb3J0IHsgZ3JhcGhQYXJlbnRQb2ludGVycyB9IGZyb20gJy4vZGVidWcnXG5pbXBvcnQgeyBwYXJzZV90cmVlLCB4eEhhc2gxMjggfSBmcm9tICcuL2ZmaSdcbmltcG9ydCAqIGFzIFJhd0FzdCBmcm9tICcuL2dlbmVyYXRlZC9hc3QnXG5pbXBvcnQgeyBNdXRhYmxlTW9kdWxlIH0gZnJvbSAnLi9tdXRhYmxlTW9kdWxlJ1xuaW1wb3J0IHR5cGUgeyBMYXp5T2JqZWN0IH0gZnJvbSAnLi9wYXJzZXJTdXBwb3J0J1xuaW1wb3J0IHtcbiAgQXBwLFxuICBBc3NpZ25tZW50LFxuICBBc3QsXG4gIEJvZHlCbG9jayxcbiAgRG9jdW1lbnRlZCxcbiAgRnVuY3Rpb24sXG4gIEdlbmVyaWMsXG4gIEdyb3VwLFxuICBJZGVudCxcbiAgSW1wb3J0LFxuICBJbnZhbGlkLFxuICBNdXRhYmxlQXNzaWdubWVudCxcbiAgTXV0YWJsZUFzdCxcbiAgTXV0YWJsZUJvZHlCbG9jayxcbiAgTXV0YWJsZUlkZW50LFxuICBOZWdhdGlvbkFwcCxcbiAgTnVtZXJpY0xpdGVyYWwsXG4gIE9wckFwcCxcbiAgUHJvcGVydHlBY2Nlc3MsXG4gIFRleHRMaXRlcmFsLFxuICBVbmFyeU9wckFwcCxcbiAgVmVjdG9yLFxuICBXaWxkY2FyZCxcbn0gZnJvbSAnLi90cmVlJ1xuXG4vKiogUmV0dXJuIHRoZSByYXcgcGFyc2VyIG91dHB1dCBmb3IgdGhlIGdpdmVuIGNvZGUuICovXG5leHBvcnQgZnVuY3Rpb24gcGFyc2VFbnNvKGNvZGU6IHN0cmluZyk6IFJhd0FzdC5UcmVlLkJvZHlCbG9jayB7XG4gIGNvbnN0IGJsb2IgPSBwYXJzZV90cmVlKGNvZGUpXG4gIGNvbnN0IHRyZWUgPSBSYXdBc3QuVHJlZS5yZWFkKG5ldyBEYXRhVmlldyhibG9iLmJ1ZmZlciksIGJsb2IuYnl0ZUxlbmd0aCAtIDQpXG4gIC8vIFRoZSByb290IG9mIHRoZSBwYXJzZXIgb3V0cHV0IGlzIGFsd2F5cyBhIGJvZHkgYmxvY2suXG4gIGFzc2VydCh0cmVlLnR5cGUgPT09IFJhd0FzdC5UcmVlLlR5cGUuQm9keUJsb2NrKVxuICByZXR1cm4gdHJlZVxufVxuXG4vKiogUHJpbnQgdGhlIEFTVCBhbmQgcmUtcGFyc2UgaXQsIGNvcHlpbmcgYGV4dGVybmFsSWRgcyAoYnV0IG5vdCBvdGhlciBtZXRhZGF0YSkgZnJvbSB0aGUgb3JpZ2luYWwuICovXG5leHBvcnQgZnVuY3Rpb24gbm9ybWFsaXplKHJvb3RJbjogQXN0KTogQXN0IHtcbiAgY29uc3QgcHJpbnRlZCA9IHByaW50KHJvb3RJbilcbiAgY29uc3QgaWRNYXAgPSBzcGFuTWFwVG9JZE1hcChwcmludGVkLmluZm8pXG4gIGNvbnN0IG1vZHVsZSA9IE11dGFibGVNb2R1bGUuVHJhbnNpZW50KClcbiAgY29uc3QgdHJlZSA9IHBhcnNlRW5zbyhwcmludGVkLmNvZGUpXG4gIGNvbnN0IHsgcm9vdDogcGFyc2VkLCBzcGFucyB9ID0gYWJzdHJhY3QobW9kdWxlLCB0cmVlLCBwcmludGVkLmNvZGUpXG4gIG1vZHVsZS5yZXBsYWNlUm9vdChwYXJzZWQpXG4gIHNldEV4dGVybmFsSWRzKG1vZHVsZSwgc3BhbnMsIGlkTWFwKVxuICByZXR1cm4gcGFyc2VkXG59XG5cbi8qKiBQcm9kdWNlIGBBc3RgIHR5cGVzIGZyb20gYFJhd0FzdGAgcGFyc2VyIG91dHB1dC4gKi9cbmV4cG9ydCBmdW5jdGlvbiBhYnN0cmFjdChcbiAgbW9kdWxlOiBNdXRhYmxlTW9kdWxlLFxuICB0cmVlOiBSYXdBc3QuVHJlZS5Cb2R5QmxvY2ssXG4gIGNvZGU6IHN0cmluZyxcbiAgc3Vic3RpdHV0b3I/OiAoa2V5OiBOb2RlS2V5KSA9PiBPd25lZCB8IHVuZGVmaW5lZCxcbik6IHsgcm9vdDogT3duZWQ8TXV0YWJsZUJvZHlCbG9jaz47IHNwYW5zOiBTcGFuTWFwOyB0b1JhdzogTWFwPEFzdElkLCBSYXdBc3QuVHJlZT4gfVxuZXhwb3J0IGZ1bmN0aW9uIGFic3RyYWN0KFxuICBtb2R1bGU6IE11dGFibGVNb2R1bGUsXG4gIHRyZWU6IFJhd0FzdC5UcmVlLFxuICBjb2RlOiBzdHJpbmcsXG4gIHN1YnN0aXR1dG9yPzogKGtleTogTm9kZUtleSkgPT4gT3duZWQgfCB1bmRlZmluZWQsXG4pOiB7IHJvb3Q6IE93bmVkOyBzcGFuczogU3Bhbk1hcDsgdG9SYXc6IE1hcDxBc3RJZCwgUmF3QXN0LlRyZWU+IH1cbmV4cG9ydCBmdW5jdGlvbiBhYnN0cmFjdChcbiAgbW9kdWxlOiBNdXRhYmxlTW9kdWxlLFxuICB0cmVlOiBSYXdBc3QuVHJlZSxcbiAgY29kZTogc3RyaW5nLFxuICBzdWJzdGl0dXRvcj86IChrZXk6IE5vZGVLZXkpID0+IE93bmVkIHwgdW5kZWZpbmVkLFxuKTogeyByb290OiBPd25lZDsgc3BhbnM6IFNwYW5NYXA7IHRvUmF3OiBNYXA8QXN0SWQsIFJhd0FzdC5UcmVlPiB9IHtcbiAgY29uc3QgYWJzdHJhY3RvciA9IG5ldyBBYnN0cmFjdG9yKG1vZHVsZSwgY29kZSwgc3Vic3RpdHV0b3IpXG4gIGNvbnN0IHJvb3QgPSBhYnN0cmFjdG9yLmFic3RyYWN0VHJlZSh0cmVlKS5ub2RlXG4gIGNvbnN0IHNwYW5zID0geyB0b2tlbnM6IGFic3RyYWN0b3IudG9rZW5zLCBub2RlczogYWJzdHJhY3Rvci5ub2RlcyB9XG4gIHJldHVybiB7IHJvb3Q6IHJvb3QgYXMgT3duZWQ8TXV0YWJsZUJvZHlCbG9jaz4sIHNwYW5zLCB0b1JhdzogYWJzdHJhY3Rvci50b1JhdyB9XG59XG5cbi8qKiBQcm9kdWNlcyBgQXN0YCB0eXBlcyBmcm9tIGBSYXdBc3RgIHBhcnNlciBvdXRwdXQuICovXG5jbGFzcyBBYnN0cmFjdG9yIHtcbiAgcHJpdmF0ZSByZWFkb25seSBtb2R1bGU6IE11dGFibGVNb2R1bGVcbiAgcHJpdmF0ZSByZWFkb25seSBjb2RlOiBzdHJpbmdcbiAgcHJpdmF0ZSByZWFkb25seSBzdWJzdGl0dXRvcjogKChrZXk6IE5vZGVLZXkpID0+IE93bmVkIHwgdW5kZWZpbmVkKSB8IHVuZGVmaW5lZFxuICByZWFkb25seSBub2RlczogTm9kZVNwYW5NYXBcbiAgcmVhZG9ubHkgdG9rZW5zOiBUb2tlblNwYW5NYXBcbiAgcmVhZG9ubHkgdG9SYXc6IE1hcDxBc3RJZCwgUmF3QXN0LlRyZWU+XG5cbiAgLyoqXG4gICAqICBAcGFyYW0gbW9kdWxlIC0gV2hlcmUgdG8gYWxsb2NhdGUgdGhlIG5ldyBub2Rlcy5cbiAgICogIEBwYXJhbSBjb2RlIC0gU291cmNlIGNvZGUgdGhhdCB3aWxsIGJlIHVzZWQgdG8gcmVzb2x2ZSByZWZlcmVuY2VzIGluIGFueSBwYXNzZWQgYFJhd0FzdGAgb2JqZWN0cy5cbiAgICogIEBwYXJhbSBzdWJzdGl0dXRvciAtIEEgZnVuY3Rpb24gdGhhdCBjYW4gaW5qZWN0IHN1YnRyZWVzIGZvciBzb21lIHNwYW5zLCBpbnN0ZWFkIG9mIHRoZSBhYnN0cmFjdG9yIHByb2R1Y2luZyB0aGVtLlxuICAgKiAgICBUaGlzIGNhbiBiZSB1c2VkIGZvciBpbmNyZW1lbnRhbCBhYnN0cmFjdGlvbi5cbiAgICovXG4gIGNvbnN0cnVjdG9yKFxuICAgIG1vZHVsZTogTXV0YWJsZU1vZHVsZSxcbiAgICBjb2RlOiBzdHJpbmcsXG4gICAgc3Vic3RpdHV0b3I/OiAoa2V5OiBOb2RlS2V5KSA9PiBPd25lZCB8IHVuZGVmaW5lZCxcbiAgKSB7XG4gICAgdGhpcy5tb2R1bGUgPSBtb2R1bGVcbiAgICB0aGlzLmNvZGUgPSBjb2RlXG4gICAgdGhpcy5zdWJzdGl0dXRvciA9IHN1YnN0aXR1dG9yXG4gICAgdGhpcy5ub2RlcyA9IG5ldyBNYXAoKVxuICAgIHRoaXMudG9rZW5zID0gbmV3IE1hcCgpXG4gICAgdGhpcy50b1JhdyA9IG5ldyBNYXAoKVxuICB9XG5cbiAgYWJzdHJhY3RUcmVlKHRyZWU6IFJhd0FzdC5UcmVlKTogeyB3aGl0ZXNwYWNlOiBzdHJpbmcgfCB1bmRlZmluZWQ7IG5vZGU6IE93bmVkIH0ge1xuICAgIGNvbnN0IHdoaXRlc3BhY2VTdGFydCA9IHRyZWUud2hpdGVzcGFjZVN0YXJ0SW5Db2RlUGFyc2VkXG4gICAgY29uc3Qgd2hpdGVzcGFjZUVuZCA9IHdoaXRlc3BhY2VTdGFydCArIHRyZWUud2hpdGVzcGFjZUxlbmd0aEluQ29kZVBhcnNlZFxuICAgIGNvbnN0IHdoaXRlc3BhY2UgPSB0aGlzLmNvZGUuc3Vic3RyaW5nKHdoaXRlc3BhY2VTdGFydCwgd2hpdGVzcGFjZUVuZClcbiAgICBjb25zdCBjb2RlU3RhcnQgPSB3aGl0ZXNwYWNlRW5kXG4gICAgY29uc3QgY29kZUVuZCA9IGNvZGVTdGFydCArIHRyZWUuY2hpbGRyZW5MZW5ndGhJbkNvZGVQYXJzZWRcbiAgICBjb25zdCBzcGFuS2V5ID0gbm9kZUtleShjb2RlU3RhcnQsIGNvZGVFbmQgLSBjb2RlU3RhcnQpXG4gICAgY29uc3Qgc3Vic3RpdHV0ZSA9IHRoaXMuc3Vic3RpdHV0b3I/LihzcGFuS2V5KVxuICAgIGlmIChzdWJzdGl0dXRlKSByZXR1cm4geyBub2RlOiBzdWJzdGl0dXRlLCB3aGl0ZXNwYWNlIH1cbiAgICBsZXQgbm9kZTogT3duZWRcbiAgICBzd2l0Y2ggKHRyZWUudHlwZSkge1xuICAgICAgY2FzZSBSYXdBc3QuVHJlZS5UeXBlLkJvZHlCbG9jazoge1xuICAgICAgICBjb25zdCBsaW5lcyA9IEFycmF5LmZyb20odHJlZS5zdGF0ZW1lbnRzLCAobGluZSkgPT4ge1xuICAgICAgICAgIGNvbnN0IG5ld2xpbmUgPSB0aGlzLmFic3RyYWN0VG9rZW4obGluZS5uZXdsaW5lKVxuICAgICAgICAgIGNvbnN0IGV4cHJlc3Npb24gPSBsaW5lLmV4cHJlc3Npb24gPyB0aGlzLmFic3RyYWN0VHJlZShsaW5lLmV4cHJlc3Npb24pIDogdW5kZWZpbmVkXG4gICAgICAgICAgcmV0dXJuIHsgbmV3bGluZSwgZXhwcmVzc2lvbiB9XG4gICAgICAgIH0pXG4gICAgICAgIG5vZGUgPSBCb2R5QmxvY2suY29uY3JldGUodGhpcy5tb2R1bGUsIGxpbmVzKVxuICAgICAgICBicmVha1xuICAgICAgfVxuICAgICAgY2FzZSBSYXdBc3QuVHJlZS5UeXBlLkZ1bmN0aW9uOiB7XG4gICAgICAgIGNvbnN0IG5hbWUgPSB0aGlzLmFic3RyYWN0VHJlZSh0cmVlLm5hbWUpXG4gICAgICAgIGNvbnN0IGFyZ3VtZW50RGVmaW5pdGlvbnMgPSBBcnJheS5mcm9tKHRyZWUuYXJncywgKGFyZykgPT4gdGhpcy5hYnN0cmFjdENoaWxkcmVuKGFyZykpXG4gICAgICAgIGNvbnN0IGVxdWFscyA9IHRoaXMuYWJzdHJhY3RUb2tlbih0cmVlLmVxdWFscylcbiAgICAgICAgY29uc3QgYm9keSA9IHRyZWUuYm9keSAhPT0gdW5kZWZpbmVkID8gdGhpcy5hYnN0cmFjdFRyZWUodHJlZS5ib2R5KSA6IHVuZGVmaW5lZFxuICAgICAgICBub2RlID0gRnVuY3Rpb24uY29uY3JldGUodGhpcy5tb2R1bGUsIG5hbWUsIGFyZ3VtZW50RGVmaW5pdGlvbnMsIGVxdWFscywgYm9keSlcbiAgICAgICAgYnJlYWtcbiAgICAgIH1cbiAgICAgIGNhc2UgUmF3QXN0LlRyZWUuVHlwZS5JZGVudDoge1xuICAgICAgICBjb25zdCB0b2tlbiA9IHRoaXMuYWJzdHJhY3RUb2tlbih0cmVlLnRva2VuKVxuICAgICAgICBub2RlID0gSWRlbnQuY29uY3JldGUodGhpcy5tb2R1bGUsIHRva2VuKVxuICAgICAgICBicmVha1xuICAgICAgfVxuICAgICAgY2FzZSBSYXdBc3QuVHJlZS5UeXBlLkFzc2lnbm1lbnQ6IHtcbiAgICAgICAgY29uc3QgcGF0dGVybiA9IHRoaXMuYWJzdHJhY3RUcmVlKHRyZWUucGF0dGVybilcbiAgICAgICAgY29uc3QgZXF1YWxzID0gdGhpcy5hYnN0cmFjdFRva2VuKHRyZWUuZXF1YWxzKVxuICAgICAgICBjb25zdCB2YWx1ZSA9IHRoaXMuYWJzdHJhY3RUcmVlKHRyZWUuZXhwcilcbiAgICAgICAgbm9kZSA9IEFzc2lnbm1lbnQuY29uY3JldGUodGhpcy5tb2R1bGUsIHBhdHRlcm4sIGVxdWFscywgdmFsdWUpXG4gICAgICAgIGJyZWFrXG4gICAgICB9XG4gICAgICBjYXNlIFJhd0FzdC5UcmVlLlR5cGUuQXBwOiB7XG4gICAgICAgIGNvbnN0IGZ1bmMgPSB0aGlzLmFic3RyYWN0VHJlZSh0cmVlLmZ1bmMpXG4gICAgICAgIGNvbnN0IGFyZyA9IHRoaXMuYWJzdHJhY3RUcmVlKHRyZWUuYXJnKVxuICAgICAgICBub2RlID0gQXBwLmNvbmNyZXRlKHRoaXMubW9kdWxlLCBmdW5jLCB1bmRlZmluZWQsIHVuZGVmaW5lZCwgYXJnKVxuICAgICAgICBicmVha1xuICAgICAgfVxuICAgICAgY2FzZSBSYXdBc3QuVHJlZS5UeXBlLk5hbWVkQXBwOiB7XG4gICAgICAgIGNvbnN0IGZ1bmMgPSB0aGlzLmFic3RyYWN0VHJlZSh0cmVlLmZ1bmMpXG4gICAgICAgIGNvbnN0IG9wZW4gPSB0cmVlLm9wZW4gPyB0aGlzLmFic3RyYWN0VG9rZW4odHJlZS5vcGVuKSA6IHVuZGVmaW5lZFxuICAgICAgICBjb25zdCBuYW1lID0gdGhpcy5hYnN0cmFjdFRva2VuKHRyZWUubmFtZSlcbiAgICAgICAgY29uc3QgZXF1YWxzID0gdGhpcy5hYnN0cmFjdFRva2VuKHRyZWUuZXF1YWxzKVxuICAgICAgICBjb25zdCBhcmcgPSB0aGlzLmFic3RyYWN0VHJlZSh0cmVlLmFyZylcbiAgICAgICAgY29uc3QgY2xvc2UgPSB0cmVlLmNsb3NlID8gdGhpcy5hYnN0cmFjdFRva2VuKHRyZWUuY2xvc2UpIDogdW5kZWZpbmVkXG4gICAgICAgIGNvbnN0IHBhcmVucyA9IG9wZW4gJiYgY2xvc2UgPyB7IG9wZW4sIGNsb3NlIH0gOiB1bmRlZmluZWRcbiAgICAgICAgY29uc3QgbmFtZVNwZWNpZmljYXRpb24gPSB7IG5hbWUsIGVxdWFscyB9XG4gICAgICAgIG5vZGUgPSBBcHAuY29uY3JldGUodGhpcy5tb2R1bGUsIGZ1bmMsIHBhcmVucywgbmFtZVNwZWNpZmljYXRpb24sIGFyZylcbiAgICAgICAgYnJlYWtcbiAgICAgIH1cbiAgICAgIGNhc2UgUmF3QXN0LlRyZWUuVHlwZS5VbmFyeU9wckFwcDoge1xuICAgICAgICBjb25zdCBvcHIgPSB0aGlzLmFic3RyYWN0VG9rZW4odHJlZS5vcHIpXG4gICAgICAgIGNvbnN0IGFyZyA9IHRyZWUucmhzID8gdGhpcy5hYnN0cmFjdFRyZWUodHJlZS5yaHMpIDogdW5kZWZpbmVkXG4gICAgICAgIGlmIChhcmcgJiYgb3ByLm5vZGUuY29kZSgpID09PSAnLScpIHtcbiAgICAgICAgICBub2RlID0gTmVnYXRpb25BcHAuY29uY3JldGUodGhpcy5tb2