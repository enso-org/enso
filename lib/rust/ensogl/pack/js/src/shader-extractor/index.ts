import host from 'runner/host'

import { App as BaseApp } from 'runner/index'

import { Args, parseArgs } from 'shader-extractor/args'
import { Task } from 'runner/log/task'
import path from 'path'
import * as fs from 'shader-extractor/fs'
import * as name from 'runner/name'

class App extends BaseApp {
    override async loadWasm() {
        console.log('loadWasm in node!')
        if (host.node) {
            const mainJsUrl = path.join(__dirname, this.config.mainJsUrl.value)
            const mainWasmUrl = path.join(__dirname, this.config.mainWasmUrl.value)
            const mainJs = await fs.readFile(mainJsUrl, 'utf8')
            const mainWasm = await fs.readFile(mainWasmUrl)
            this.wasm = await this.compileAndRunWasm(mainJs, mainWasm)
        }
    }

    async extractShaders(target: string) {
        await Task.asyncWith('Extracting shaders code.', async () => {
            const shadersMap = this.getShaders()
            if (shadersMap) {
                await Task.asyncWith(`Writing shaders to '${target}'.`, async () => {
                    await fs.rm(target, { recursive: true, force: true })
                    await fs.mkdir(target)
                    const fileNames = []
                    for (const [codePath, code] of shadersMap) {
                        const fileName = name.mangle(codePath)
                        const filePath = path.join(target, fileName)
                        await fs.writeFile(`${filePath}.vertex.glsl`, code.vertex)
                        await fs.writeFile(`${filePath}.fragment.glsl`, code.fragment)
                        fileNames.push(fileName)
                    }
                    const fileListPath = path.join(target, 'list.txt')
                    await fs.writeFile(fileListPath, fileNames.join('\n'))
                })
            }
        })
    }
}

async function main() {
    const args = parseArgs()
    console.log('hello node2!')
    if (host.node) {
        const app = new App()
        const extractShadersPath = args.extractShaders.value
        if (extractShadersPath) {
            await Task.asyncWith('Running the program.', async () => {
                app.printResolvedConfig()
                await app.loadAndInitWasm()
                app.runBeforeMainEntryPoints()
                await app.extractShaders(extractShadersPath)
            })
        } else {
            args.printHelpAndExit(1)
        }
    }
}

main()
