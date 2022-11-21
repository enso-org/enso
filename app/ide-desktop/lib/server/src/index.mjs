import path from 'node:path'
import portfinder from 'portfinder'
import connect from 'connect'
import serveStatic from 'serve-static'
import logger from 'morgan'

export const DEFAULT_PORT = 8080

export async function start({ root, assets, port }) {
    assets = assets ?? path.join(root, 'assets')

    const freePort = await portfinder.getPortPromise({ port: port ?? DEFAULT_PORT});
    const reload_requests = []

    let app = connect()
        .use(logger('dev', { skip: (req, res) => res.statusCode < 400 }))
        .use(serveStatic(root))
        .use('/assets', serveStatic(assets))
        .use('/live-reload', (req, res) => {
            reload_requests.push(
                res.writeHead(200, {
                  "Content-Type": "text/event-stream",
                  "Cache-Control": "no-cache",
                  Connection: "keep-alive",
                })
            );
        });


    await app.listen(freePort);
    var serverUrl = `http://127.0.0.1:${freePort}`;
    console.log(("Serving %s"), serverUrl);

    return {
        port: freePort,
        reload() {
            reload_requests.forEach((res) => res.write("data: reload\n\n"));
            reload_requests.length = 0;
        }
    }
}
