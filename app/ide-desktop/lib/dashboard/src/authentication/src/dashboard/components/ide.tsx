/** @file Container that launches the IDE. */
import * as react from 'react'

import * as cloudService from '../service'
import * as projectManagerService from '../projectManagerService'

import * as app from '../../../../../../../../../target/ensogl-pack/linked-dist/index'

// =================
// === Constants ===
// =================

const IDE_CDN_URL = 'https://ensocdn.s3.us-west-1.amazonaws.com/ide'
const FALLBACK_VERSION = '2023.1.1-nightly.2023.4.13'

// =================
// === Component ===
// =================

interface Props {
    project: cloudService.Project
    backendService: cloudService.Backend | projectManagerService.Backend
}

/** Container that launches the IDE. */
function Ide({ project, backendService }: Props) {
    react.useEffect(() => {
        void (async () => {
            const ideVersion = (
                'listVersions' in backendService
                    ? await backendService.listVersions({
                          versionType: cloudService.VersionType.ide,
                          default: true,
                      })
                    : null
            )?.[0].number.value
            const backendVersion = (
                'listVersions' in backendService
                    ? await backendService.listVersions({
                          versionType: cloudService.VersionType.backend,
                          default: true,
                      })
                    : null
            )?.[0].number.value
            const projectIdeVersion = project.ideVersion?.value ?? ideVersion ?? FALLBACK_VERSION
            const projectEngineVersion =
                project.engineVersion?.value ?? backendVersion ?? FALLBACK_VERSION
            const appInstance = new app.App({
                config: {
                    loader: {
                        assetsUrl: `${IDE_CDN_URL}/${projectIdeVersion}/dynamic-assets`,
                        wasmUrl: `${IDE_CDN_URL}/${projectIdeVersion}/pkg-opt.wasm`,
                        jsUrl: `${IDE_CDN_URL}/${projectIdeVersion}/pkg.js.gz`,
                    },
                    // engine: {
                    //     rpcUrl: `${project.address!}json`,
                    //     dataUrl: `${project.address!}binary`,
                    //     preferredVersion: projectEngineVersion,
                    // },
                    // startup: {
                    //     project: project.packageName,
                    // },
                },
            })
            appInstance.showEntryPointSelector()
            void appInstance.run()
        })()
    }, [project])

    return (
        <>
            <div id="root" />
            <link rel="stylesheet" href={`${IDE_CDN_URL}/${FALLBACK_VERSION}/style.css`} />
        </>
    )
}

export default Ide
