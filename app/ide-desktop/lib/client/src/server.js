import createServer from 'create-servers'
import * as fs from 'fs'
import * as mime from 'mime-types'
import * as path from 'path'
import * as portfinder from 'portfinder'

// ============
// === Port ===
// ============

export const DEFAULT_PORT = 8080

async function findPort(cfg) {
    if (!cfg.port) {
        cfg.port = await portfinder.getPortPromise({ port: DEFAULT_PORT, startPort: DEFAULT_PORT })
    }
}

// ==============
// === Server ===
// ==============

/// A simple server implementation.
///
/// Initially it was based on `union`, but later we migrated to `create-servers`. Read this topic to
/// learn why: https://github.com/http-party/http-server/issues/483
class Server {
    constructor(cfg) {
        let self = this
        this.dir = cfg.dir
        this.port = cfg.port
        this.fallback = cfg.fallback
        console.log('f', createServer)
        this.server = createServer(
            {
                http: this.port,
                handler: function (request, response) {
                    self.process(request.url, response)
                },
            },
            function (errs) {
                if (errs) {
                    return console.log(errs.http)
                }
                console.log(`Server started on port ${this.port}.`)
                console.log(`Serving files from '${process.cwd()}/${this.dir}'.`)
            }.bind(this)
        )
    }

    process(resource, response) {
        let resource_file = `${this.dir}${resource}`
        fs.readFile(
            resource_file,
            function (err, data) {
                if (err) {
                    let fallback = this.fallback
                    if (fallback) {
                        if (resource === fallback) {
                            console.error(`Fallback resource '${resource}' not found.`)
                        } else {
                            this.process(fallback, response)
                        }
                    } else {
                        console.error(`Resource '${resource}' not found.`)
                    }
                } else {
                    let contentType = mime.contentType(path.extname(resource_file))
                    let contentLength = data.length
                    response.setHeader('Content-Type', contentType)
                    response.setHeader('Content-Length', contentLength)
                    response.writeHead(200)
                    response.end(data)
                }
            }.bind(this)
        )
    }
}

export async function create(cfg) {
    let local_cfg = Object.assign({}, cfg)
    await findPort(local_cfg)
    return new Server(local_cfg)
}
