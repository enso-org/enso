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
// @ts-ignore
import { ENGINE_VERSION } from './../../../../../build/release'
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

interface BuildInfo {
    version: string
    target: string
}

function get_project_manager_url({ version, target }: BuildInfo): string {
    console.log('webpack target ' + target)
    let base_url: string = 'https://github.com/enso-org/'
    base_url += 'enso/releases/download/'
    base_url += `enso-${version}/enso-project-manager-${version}`
    let postfix
    if (target === 'linux') {
        postfix = `linux-amd64.tar.gz`
    } else if (target === 'macos') {
        postfix = `macos-amd64.tar.gz`
    } else if (target === 'win') {
        postfix = `windows-amd64.zip`
    } else {
        throw 'UnsupportedPlatform: ' + target
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

async function decompress_project_manager(source_file_path: fss.PathLike, target_folder: string) {
    await fs.mkdir(target_folder, { recursive: true })
    await fs.rm(path.join(target_folder, 'enso'), { recursive: true, force: true })
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
    const download_dir = path.resolve(paths.dist.tmp, 'project-manager/')
    const file_path = path.resolve(download_dir, file_name)

    if (fss.existsSync(file_path) && !overwrite) {
        console.log(
            `The file ${file_path} exists. ` +
                'Project manager executable does not need to be downloaded.'
        )
    } else {
        await fs.mkdir(download_dir, { recursive: true })

        const parsed = url.parse(file_url)
        const options = {
            host: parsed.host,
            port: 80,
            path: parsed.pathname,
        }

        const target_file = fss.createWriteStream(file_path)
        const progress_indicator = new DownloadProgressIndicator()
        await new Promise((resolve, reject) =>
            http
                .get(options, (res: IncomingMessage) => {
                    res.on('data', (data: string) => {
                        target_file.write(data)
                        progress_indicator.add_progress_bytes(data.length)
                    }).on('end', () => {
                        target_file.end()
                        console.log(`${file_url} downloaded to "${file_path}".`)
                        resolve(undefined)
                    })
                })
                .on('error', async (e: http.ClientRequest) => {
                    target_file.end()
                    await fs.rm(file_path)
                    reject('Error: The download of the project manager was interrupted:\n' + e)
                })
        )
    }

    await decompress_project_manager(file_path, distPath)
}

// ============
// === Main ===
// ============

async function main() {
    const buildInfo: BuildInfo = {
        version: ENGINE_VERSION,
        target: (await get_build_config()).target,
    }

    // The file at path `buildInfoFile` should always contain the build info of the project manager
    // that is currently installed in the dist directory. We read the file if it exists and compare
    // it with the version and target platform that we need. If they already agree then the right
    // project manager is already installed and there is nothing to do.
    const build_info_file = path.join(distPath, 'installed-enso-version')
    let existing_build_info: BuildInfo | null
    try {
        const build_info_file_content = await fs.readFile(build_info_file)
        existing_build_info = JSON.parse(build_info_file_content.toString())
    } catch (error) {
        const file_does_not_exist = error.code === 'ENOENT' // Standing for "Error NO ENTry"
        if (file_does_not_exist) {
            existing_build_info = null
        } else {
            console.error(error)
            process.exit(1)
        }
    }
    if (
        buildInfo.version !== existing_build_info?.version ||
        buildInfo.target !== existing_build_info?.target
    ) {
        // We remove the build info file to avoid misinformation if the build is interrupted during
        // the call to `download_project_manager`.
        // We use `force: true` because the file might not exist.
        await fs.rm(build_info_file, { force: true })
        let project_manager_url = get_project_manager_url(buildInfo)
        try {
            await download_project_manager(project_manager_url, false)
        } catch (error) {
            console.error(error)
            process.exit(1)
        }
        await fs.writeFile(build_info_file, JSON.stringify(buildInfo))
    } else {
        console.log(
            `The right version of the project manager is already installed, ` +
                `according to ${build_info_file}.`
        )
    }
}

main()
