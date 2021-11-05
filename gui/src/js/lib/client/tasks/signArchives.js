/**
 This script signs the content of all archives that we have for macOS. For this to work this needs
 to run on macOS with `codesign`, and a JDK installed. `codesign` is needed to sign the files,
 while the JDK is needed for correct packing and unpacking of java archives.

 We require this extra step as our dependencies contain files that require us to re-sign jar
 contents that cannot be opened as pure zip archives, but require a java toolchain to extract
 and re-assemble to preserve manifest information. This functionality is not provided by
 `electron-osx-sign` out of the box.

 This code is based on https://github.com/electron/electron-osx-sign/pull/231 but our use-case
 is unlikely to be supported by electron-osx-sign as it adds a java toolchain as additional
 dependency.
 This script should be removed once the engine is signed.
**/
const fs = require('fs')
const path = require('path')
const child_process = require('child_process')
const { dist } = require('../../../../../build/paths')
const { ENGINE_VERSION } = require('../../../../../build/release')

const contentRoot = path.join(dist.root, 'client', 'mac', 'Enso.app', 'Contents')
const resRoot = path.join(contentRoot, 'Resources')

const ID = '"Developer ID Application: New Byte Order Sp. z o. o. (NM77WTZJFQ)"'
// Placeholder name for temporary archives.
const tmpArchive = 'temporary_archive.zip'

const GRAALVM = 'graalvm-ce-java11-21.1.0'

// Helper to execute a command in a given directory and return the output.
const run = (cmd, cwd) => child_process.execSync(cmd, { shell: true, cwd }).toString()

// Run the signing command.
function sign(targetPath, cwd) {
    console.log(`Signing ${targetPath} in ${cwd}`)
    const entitlements_path = path.resolve('./', 'entitlements.mac.plist')
    return run(
        `codesign -vvv --entitlements ${entitlements_path} --force --options=runtime ` +
            `--sign ${ID} ${targetPath}`,
        cwd
    )
}

// Create and return an empty directory in the current folder. The directory will be named `.temp`.
// If it already exists all content will be deleted.
function getTmpDir() {
    const workingDir = '.temp'
    run(`rm -rf ${workingDir}`)
    run(`mkdir ${workingDir}`)
    return path.resolve(workingDir)
}

/**
 * Sign content of an archive. This function extracts the archive, signs the required files,
 * re-packages the archive and replaces the original.
 *
 * @param {string} archivePath - folder the archive is located in.
 * @param {string} archiveName - file name of the archive
 * @param {string[]} binPaths - paths of files to be signed. Must be relative to archive root.
 */
function signArchive(archivePath, archiveName, binPaths) {
    const sourceArchive = path.join(archivePath, archiveName)
    const workingDir = getTmpDir()
    try {
        const isJar = archiveName.endsWith(`jar`)

        if (isJar) {
            run(`jar xf ${sourceArchive}`, workingDir)
        } else {
            run(`unzip -d${workingDir} ${sourceArchive}`)
        }

        for (let binary of binPaths) {
            sign(binary, workingDir)
        }

        if (isJar) {
            if (archiveName.includes(`runner`)) {
                run(`jar -cfm ${tmpArchive} META-INF/MANIFEST.MF . `, workingDir)
            } else {
                run(`jar -cf ${tmpArchive} . `, workingDir)
            }
        } else {
            run(`zip -rm ${tmpArchive} . `, workingDir)
        }

        console.log(run(`/bin/mv ${workingDir}/${tmpArchive} ${sourceArchive}`))
        run(`rm -R ${workingDir}`)
        console.log(
            `Successfully repacked ${sourceArchive} to handle signing inner native dependency.`
        )
    } catch (error) {
        run(`rm -R ${workingDir}`)
        console.error(
            `Could not repackage ${archiveName}.  Please check the "signArchives.js" task in ` +
                `client/tasks to ensure that it's working. This jar has to be treated specially` +
                ` because it has a native library and apple's codesign does not sign inner ` +
                `native libraries correctly for jar files`
        )
        throw error
    }
}

// Archives, and their content that need to be signed in an extra step. If a new archive is added
// to the engine dependencies this also needs to be added here. If an archive is not added here, it
// will show up as a failure to notarise the IDE. The offending archive will be named in the error
// message provided by Apple and can then be added here.
const toSign = [
    {
        jarDir: `enso/dist/${ENGINE_VERSION}/lib/Standard/Database/${ENGINE_VERSION}/polyglot/java`,
        jarName: 'sqlite-jdbc-3.34.0.jar',
        jarContent: [
            'org/sqlite/native/Mac/aarch64/libsqlitejdbc.jnilib',
            'org/sqlite/native/Mac/x86_64/libsqlitejdbc.jnilib',
        ],
    },
    {
        jarDir: `enso/dist/${ENGINE_VERSION}/component`,
        jarName: 'runner.jar',
        jarContent: [
            'org/sqlite/native/Mac/x86_64/libsqlitejdbc.jnilib',
            'com/sun/jna/darwin/libjnidispatch.jnilib',
        ],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'jdk.jartool.jmod',
        jarContent: ['bin/jarsigner', 'bin/jar'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'jdk.jdeps.jmod',
        jarContent: ['bin/javap', 'bin/jdeprscan', 'bin/jdeps'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'jdk.jstatd.jmod',
        jarContent: ['bin/jstatd'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'jdk.pack.jmod',
        jarContent: ['bin/unpack200', 'bin/pack200'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'jdk.hotspot.agent.jmod',
        jarContent: ['bin/jhsdb'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'jdk.jfr.jmod',
        jarContent: ['bin/jfr'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'jdk.rmic.jmod',
        jarContent: ['bin/rmic'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'java.rmi.jmod',
        jarContent: ['bin/rmid', 'bin/rmiregistry'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'java.base.jmod',
        jarContent: ['bin/java', 'bin/keytool', 'lib/jspawnhelper'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'jdk.jlink.jmod',
        jarContent: ['bin/jmod', 'bin/jlink', 'bin/jimage'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'jdk.scripting.nashorn.shell.jmod',
        jarContent: ['bin/jjs'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'jdk.jcmd.jmod',
        jarContent: ['bin/jstack', 'bin/jcmd', 'bin/jps', 'bin/jmap', 'bin/jstat', 'bin/jinfo'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'jdk.jshell.jmod',
        jarContent: ['bin/jshell'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'jdk.compiler.jmod',
        jarContent: ['bin/javac', 'bin/serialver'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'java.scripting.jmod',
        jarContent: ['bin/jrunscript'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'jdk.jdi.jmod',
        jarContent: ['bin/jdb'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'jdk.javadoc.jmod',
        jarContent: ['bin/javadoc'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'jdk.jconsole.jmod',
        jarContent: ['bin/jconsole'],
    },
    {
        jarDir: `enso/runtime/${GRAALVM}/Contents/Home/jmods`,
        jarName: 'jdk.javadoc.jmod',
        jarContent: ['bin/javadoc'],
    },
]

// Extra files that need to be signed.
const extra = [
    `enso/runtime/${GRAALVM}/Contents/MacOS/libjli.dylib`,
    `enso/runtime/${GRAALVM}/Contents/Home/languages/llvm/native/bin/ld.lld`,
    `enso/runtime/${GRAALVM}/Contents/Home/languages/R/library/MASS/libs/MASS.so`,
    `enso/runtime/${GRAALVM}/Contents/Home/languages/R/library/cluster/libs/cluster.so`,
    `enso/runtime/${GRAALVM}/Contents/Home/languages/R/library/nnet/libs/nnet.so`,
    `enso/runtime/${GRAALVM}/Contents/Home/languages/R/library/rpart/libs/rpart.so`,
    `enso/runtime/${GRAALVM}/Contents/Home/languages/R/library/lattice/libs/lattice.so`,
    `enso/runtime/${GRAALVM}/Contents/Home/languages/R/library/nlme/libs/nlme.so`,
    `enso/runtime/${GRAALVM}/Contents/Home/languages/R/library/class/libs/class.so`,
    `enso/runtime/${GRAALVM}/Contents/Home/languages/R/library/spatial/libs/spatial.so`,
    `enso/runtime/${GRAALVM}/Contents/Home/languages/R/library/foreign/libs/foreign.so`,
    `enso/runtime/${GRAALVM}/Contents/Home/languages/R/library/Matrix/libs/Matrix.so`,
    `enso/runtime/${GRAALVM}/Contents/Home/languages/R/library/KernSmooth/libs/KernSmooth.so`,
    `enso/runtime/${GRAALVM}/Contents/Home/languages/R/library/survival/libs/survival.so`,
]

// The list of readonly files in the GraalVM distribution.
const readonly = [`enso/runtime/${GRAALVM}/Contents/Home/lib/server/classes.jsa`]

function beforeSign() {
    for (let file of readonly) {
        const target = path.join(resRoot, file)
        fs.chmodSync(target, 0o644)
    }
}

exports.default = async function () {
    // Sign archives.
    for (let toSignData of toSign) {
        const jarDir = path.join(resRoot, toSignData.jarDir)
        const jarName = toSignData.jarName
        const jarContent = toSignData.jarContent
        console.log({ jarDir, jarName, jarContent })
        signArchive(jarDir, jarName, jarContent)
    }
    // Sign single binaries.
    for (let toSign of extra) {
        const target = path.join(resRoot, toSign)
        sign(target)
    }
    // Finally re-sign the top-level enso.
    sign(path.join(contentRoot, 'MacOs/Enso'))
}

module.exports = { beforeSign }
