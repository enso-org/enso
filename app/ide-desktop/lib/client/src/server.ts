// @ts-expect-error
import createServer from 'create-servers'
import * as fs from 'fs'
// @ts-expect-error
import * as mime from 'mime-types'
import * as path from 'path'
import * as portfinder from 'portfinder'

// ==============
// === Config ===
// ==============

export class Config {
    dir: string
    port: number
    fallback: string
    constructor(cfg: { dir: string; port: number; fallback: string }) {
        this.dir = cfg.dir
        this.port = cfg.port
        this.fallback = cfg.fallback
    }
}

// ============
// === Port ===
// ============

async function findPort(port: number): Promise<number> {
    return await portfinder.getPortPromise({ port, startPort: port })
}

// ==============
// === Server ===
// ==============

/// A simple server implementation.
///
/// Initially it was based on `union`, but later we migrated to `create-servers`. Read this topic to
/// learn why: https://github.com/http-party/http-server/issues/483
export class Server {
    config: Config
    server: any
    constructor(config: Config) {
        this.config = config
    }

    static async create(config: Config): Promise<Server> {
        let local_config = Object.assign({}, config)
        local_config.port = await findPort(local_config.port)
        const server = new Server(local_config)
        await server.run()
        return server
    }

    run(): Promise<void> {
        return new Promise((resolve, reject) => {
            this.server = createServer(
                {
                    http: this.config.port,
                    handler: (request: any, response: any) => {
                        this.process(request.url, response)
                    },
                },
                (err: any) => {
                    if (err) {
                        console.error(`Error creating server:`, err.http)
                        reject(err)
                    }
                    console.log(`Server started on port ${this.config.port}.`)
                    console.log(`Serving files from '${process.cwd()}/${this.config.dir}'.`)
                    resolve()
                }
            )
        })
    }

    process(resource: any, response: any) {
        let resource_file = `${this.config.dir}${resource}`
        fs.readFile(resource_file, (err: any, data: any) => {
            if (err) {
                let fallback = this.config.fallback
                if (resource === fallback) {
                    console.error(`Fallback resource '${resource}' not found.`)
                } else {
                    this.process(fallback, response)
                }
            } else {
                let contentType = mime.contentType(path.extname(resource_file))
                let contentLength = data.length
                response.setHeader('Content-Type', contentType)
                response.setHeader('Content-Length', contentLength)
                response.writeHead(200)
                response.end(data)
            }
        })
    }
}
