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
class Server {
    config: Config
    server: any
    constructor(config: Config) {
        this.config = config
        let self = this
        this.server = createServer(
            {
                http: this.config.port,
                handler: function (request: any, response: any) {
                    self.process(request.url, response)
                },
            },
            (err: any) => {
                if (err) {
                    return console.log(err.http)
                }
                console.log(`Server started on port ${this.config.port}.`)
                console.log(`Serving files from '${process.cwd()}/${this.config.dir}'.`)
            }
        )
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

export async function create(cfg: Config) {
    let local_cfg = Object.assign({}, cfg)
    local_cfg.port = await findPort(local_cfg.port)
    return new Server(local_cfg)
}
