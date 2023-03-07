/** @file Tool for extracting sources of dynamic assets from compiled WASM binaries. */

import path from 'path'
import * as args from 'asset-extractor/args'
import * as fs from 'asset-extractor/fs'
import * as log from 'runner/log'
import * as name from 'runner/name'
import * as runner from 'runner/index'

// ===========
// === App ===
// ===========

/** The main application. It loads the WASM file from disk, runs before main entry points, extract
 * asset sources and saves them to files. */
class App extends runner.App {
    override async loadWasm() {
        const mainJsUrl = path.join(__dirname, this.config.groups.loader.options.jsUrl.value)
        const mainWasmUrl = path.join(__dirname, this.config.groups.loader.options.wasmUrl.value)
        const mainJs = await fs.readFile(mainJsUrl, 'utf8')
        const mainWasm = await fs.readFile(mainWasmUrl)
        this.wasm = await this.compileAndRunWasm(mainJs, mainWasm)
    }

    async extractAssets(outDir: string) {
        await log.Task.asyncRun('Extracting dynamic assets source code.', async () => {
            // Clear the extracted-sources directory before getting new sources.
            // If getting sources fails we leave the directory empty, not outdated.
            await fs.rm(outDir, { recursive: true, force: true })
            await fs.mkdir(outDir)
            const assetsMap = this.getAssetSources()
            if (assetsMap) {
                await log.Task.asyncRun(`Writing assets to '${outDir}'.`, async () => {
                    for (const [builder, asset] of assetsMap) {
                        for (const [key, files] of asset) {
                            const dirPath = path.join(outDir, builder, key)
                            await fs.mkdir(dirPath, { recursive: true })
                            for (const [name, data] of files) {
                                const filePath = path.join(dirPath, name)
                                await fs.writeFile(`${filePath}`, Buffer.from(data))
                            }
                        }
                    }
                })
            }
        })
    }

    override async run(): Promise<void> {
        const parser = args.parse()
        const outDir = parser.args.outDir.value
        if (outDir) {
            await log.Task.asyncRun('Running the program.', async () => {
                await app.loadAndInitWasm()
                const r = app.runBeforeMainEntryPoints().then(() => {
                    return app.extractAssets(outDir)
                })
                await r
            })
        } else {
            parser.printHelpAndExit(1)
        }
    }
}

// ============
// === Main ===
// ============

const app = new App()
void app.run()
