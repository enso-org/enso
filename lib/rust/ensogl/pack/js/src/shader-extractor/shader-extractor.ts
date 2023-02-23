/** @file Tool for extracting shaders of EnsoGL shapes from compiled WASM binaries. */

import path from 'path'
import * as args from 'shader-extractor/args'
import * as fs from 'shader-extractor/fs'
import * as log from 'runner/log'
import * as name from 'runner/name'
import * as runner from 'runner/index'

// ===========
// === App ===
// ===========

/** The main application. It loads the WASM file from disk, runs before main entry points, extract
 * not optimized shaders and saves them to files. */
class App extends runner.App {
    override async loadWasm() {
        const mainJsUrl = path.join(__dirname, this.config.params.pkgJsUrl.value)
        const mainWasmUrl = path.join(__dirname, this.config.params.pkgWasmUrl.value)
        const mainJs = await fs.readFile(mainJsUrl, 'utf8')
        const mainWasm = await fs.readFile(mainWasmUrl)
        this.wasm = await this.compileAndRunWasm(mainJs, mainWasm)
    }

    async extractShaders(outDir: string) {
        await log.Task.asyncRun('Extracting shaders code.', async () => {
            const shadersMap = this.getShaders()
            if (shadersMap) {
                await log.Task.asyncRun(`Writing shaders to '${outDir}'.`, async () => {
                    await fs.rm(outDir, { recursive: true, force: true })
                    await fs.mkdir(outDir)
                    const fileNames = []
                    for (const [codePath, code] of shadersMap) {
                        const fileName = name.mangle(codePath)
                        const filePath = path.join(outDir, fileName)
                        await fs.writeFile(`${filePath}.vertex.glsl`, code.vertex)
                        await fs.writeFile(`${filePath}.fragment.glsl`, code.fragment)
                        fileNames.push(fileName)
                    }
                    const fileListPath = path.join(outDir, 'list.txt')
                    await fs.writeFile(fileListPath, fileNames.join('\n'))
                })
            }
        })
    }

    override async run(): Promise<void> {
        const parser = args.parse()
        const outDir = parser.args.outDir.value
        if (outDir) {
            await log.Task.asyncRun('Running the program.', async () => {
                app.config.print()
                await app.loadAndInitWasm()
                const r = app.runBeforeMainEntryPoints().then(() => {
                    return app.extractShaders(outDir)
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
