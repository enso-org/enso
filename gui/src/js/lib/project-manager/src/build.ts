/// Build script that downloads and extracts the project manager from CI.
/// The project manager will be available at `paths.dist.bin` after running this script.

// @ts-ignore
import { http } from 'follow-redirects'
import * as os from 'os'
import * as fss from 'fs'
import * as path from 'path'
// @ts-ignore
import * as tar from 'tar'
// @ts-ignore
import * as unzipper from 'unzipper'
import * as url from 'url'
// @ts-ignore
import * as paths from './../../../../../build/paths'
import { IncomingMessage } from 'http'
const fs = fss.promises

const distPath = paths.dist.bin
const buildInfoPath = paths.dist.buildInfo

// =============
// === Utils ===
// =============

async function get_build_config() {
    let exists = fss.existsSync(buildInfoPath)
    if (exists) {
        let configFile = await fs.readFile(buildInfoPath)
        return JSON.parse(configFile.toString())
    } else {
        throw `Could not find build file at "${buildInfoPath}".`
    }
}

// =======================
// === Project Manager ===
// =======================

async function get_project_manager_url(): Promise<string> {
    const config = await get_build_config()
    const target_platform = config.target
    console.log('webpack target ' + target_platform)
    // This constant MUST be synchronized with `ENGINE` constant in src/js/lib/client/tasks/signArchives.js.
    // Also it is usually a good idea to synchronize it with `ENGINE_VERSION_FOR_NEW_PROJECTS` in
    // src/rust/ide/src/controller/project.rs. See also https://github.com/enso-org/ide/issues/1359
    const version = '0.2.27'
    let base_url: string = 'https://github.com/enso-org/'
    base_url += 'enso/releases/download/'
    base_url += `enso-${version}/enso-project-manager-${version}`
    let postfix
    if (target_platform === 'linux') {
        postfix = `linux-amd64.tar.gz`
    } else if (target_platform === 'macos') {
        postfix = `macos-amd64.tar.gz`
    } else if (target_platform === 'win') {
        postfix = `windows-amd64.zip`
    } else {
        throw 'UnsupportedPlatform: ' + target_platform
    }
    return `${base_url}-${postfix}`
}

function project_manager_path() {
    return paths.get_project_manager_path(distPath)
}

function make_project_manager_binary_executable() {
    const bin_path = project_manager_path()
    if (os.platform() != 'win32') {
        fss.chmodSync(bin_path, '744')
    }
}

function decompress_project_manager(source_file_path: fss.PathLike, target_folder: string) {
    let decompressor
    if (source_file_path.toString().endsWith('.zip')) {
        decompressor = unzipper.Extract({ path: target_folder })
    } else {
        decompressor = tar.x({
            C: target_folder,
        })
    }
    fss.createReadStream(source_file_path)
        .pipe(decompressor)
        .on('finish', make_project_manager_binary_executable)
}

class DownloadProgressIndicator {
    private progress_bytes: number
    private progress_truncated: string

    constructor() {
        this.progress_bytes = 0
        this.progress_truncated = '0.0'
    }

    /**
     * Indicate that progress has been made. Will emit a console log for each tenth of an MB that
     * has been progressed.
     */
    add_progress_bytes(count: number): void {
        this.progress_bytes += count
        /// Update truncated progress
        const progress_new = `${Math.trunc(this.progress_bytes / 10e4) / 10}`
        if (progress_new != this.progress_truncated) {
            this.progress_truncated = progress_new
            console.log(this.formatted_message())
        }
    }

    formatted_message(): string {
        return `Download progress: ${this.progress_truncated}MB.`
    }
}

// TODO: Consider adding to common library for re-use in other parts of the build system.
async function download_project_manager(file_url: string, overwrite: boolean): Promise<void> {
    const parse_result = url.parse(file_url).pathname
    if (parse_result === undefined || parse_result === null) {
        throw `File URL does not contain valid path name: ` + file_url
    }
    const file_name = parse_result.split('/').pop()
    if (file_name === undefined || file_name === null) {
        throw `File URL does not contain path separator: ` + file_url
    }
    const file_path = path.resolve(distPath, file_name)

    if (fss.existsSync(file_path) && !overwrite) {
        console.log(
            `The ${file_path} file exists. Project manager executable will not be regenerated.`
        )
        return
    }

    await fs.mkdir(distPath, { recursive: true })

    const parsed = url.parse(file_url)
    const options = {
        host: parsed.host,
        port: 80,
        path: parsed.pathname,
    }

    const target_file = fss.createWriteStream(file_path)
    const progress_indicator = new DownloadProgressIndicator()
    http.get(options, (res: IncomingMessage) => {
        res.on('data', (data: string) => {
            target_file.write(data)
            progress_indicator.add_progress_bytes(data.length)
        }).on('end', () => {
            target_file.end()
            console.log(`${file_url} downloaded to "${file_path}".`)
            decompress_project_manager(file_path, distPath)
        })
    })
}

// ============
// === Main ===
// ============

async function main() {
    let file_url = await get_project_manager_url()
    await download_project_manager(file_url, false)
}

main()
