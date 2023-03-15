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

import fs from 'node:fs/promises'
import os from 'node:os'
import path from 'node:path'
import child_process from 'node:child_process'
import glob from 'fast-glob'

// ===============================================
// === Patterns of entities that need signing. ===
// ===============================================

/** Parts of the GraalVM distribution that need to be signed by us in an extra step. */
async function graalSignables(resourcesDir: string): Promise<Signable[]> {
    const archivePatterns: ArchivePattern[] = [
        [`Contents/Home/jmods/jdk.jartool.jmod`, ['bin/jarsigner', 'bin/jar']],
        [`Contents/Home/jmods/jdk.jdeps.jmod`, ['bin/javap', 'bin/jdeprscan', 'bin/jdeps']],
        [`Contents/Home/jmods/jdk.jstatd.jmod`, ['bin/jstatd']],
        [`Contents/Home/jmods/jdk.pack.jmod`, ['bin/unpack200', 'bin/pack200']],
        [`Contents/Home/jmods/jdk.hotspot.agent.jmod`, ['bin/jhsdb']],
        [`Contents/Home/jmods/jdk.jfr.jmod`, ['bin/jfr']],
        [`Contents/Home/jmods/jdk.rmic.jmod`, ['bin/rmic']],
        [`Contents/Home/jmods/java.rmi.jmod`, ['bin/rmid', 'bin/rmiregistry']],
        [`Contents/Home/jmods/java.base.jmod`, ['bin/java', 'bin/keytool', 'lib/jspawnhelper']],
        [`Contents/Home/jmods/jdk.jlink.jmod`, ['bin/jmod', 'bin/jlink', 'bin/jimage']],
        [`Contents/Home/jmods/jdk.scripting.nashorn.shell.jmod`, ['bin/jjs']],
        [
            `Contents/Home/jmods/jdk.jcmd.jmod`,
            ['bin/jstack', 'bin/jcmd', 'bin/jps', 'bin/jmap', 'bin/jstat', 'bin/jinfo'],
        ],
        [`Contents/Home/jmods/jdk.jshell.jmod`, ['bin/jshell']],
        [`Contents/Home/jmods/jdk.compiler.jmod`, ['bin/javac', 'bin/serialver']],
        [`Contents/Home/jmods/java.scripting.jmod`, ['bin/jrunscript']],
        [`Contents/Home/jmods/jdk.jdi.jmod`, ['bin/jdb']],
        [`Contents/Home/jmods/jdk.javadoc.jmod`, ['bin/javadoc']],
        [`Contents/Home/jmods/jdk.jconsole.jmod`, ['bin/jconsole']],
        [`Contents/Home/jmods/jdk.javadoc.jmod`, ['bin/javadoc']],
    ]

    const binariesPatterns = [
        `Contents/Home/languages/llvm/native/bin/graalvm-native-ld`,
        `Contents/Home/languages/llvm/native/bin/ld.lld`,
        `Contents/Home/languages/R/library/class/libs/class.so`,
        `Contents/Home/languages/R/library/cluster/libs/cluster.so`,
        `Contents/Home/languages/R/library/foreign/libs/foreign.so`,
        `Contents/Home/languages/R/library/KernSmooth/libs/KernSmooth.so`,
        `Contents/Home/languages/R/library/lattice/libs/lattice.so`,
        `Contents/Home/languages/R/library/MASS/libs/MASS.so`,
        `Contents/Home/languages/R/library/Matrix/libs/Matrix.so`,
        `Contents/Home/languages/R/library/nlme/libs/nlme.so`,
        `Contents/Home/languages/R/library/nnet/libs/nnet.so`,
        `Contents/Home/languages/R/library/rpart/libs/rpart.so`,
        `Contents/Home/languages/R/library/spatial/libs/spatial.so`,
        `Contents/Home/languages/R/library/survival/libs/survival.so`,
        `Contents/MacOS/libjli.dylib`,
    ]

    // We use `*` for Graal versioned directory to not have to update this script on every GraalVM update.
    // Updates might still be needed when the list of binaries to sign changes.
    const graalDir = path.join(resourcesDir, 'enso', 'runtime', '*')
    const archives = await ArchiveToSign.lookupMany(graalDir, archivePatterns)
    const binaries = await BinaryToSign.lookupMany(graalDir, binariesPatterns)
    return [...archives, ...binaries]
}

/** Parts of the Enso Engine distribution that need to be signed by us in an extra step. */
async function ensoPackageSignables(resourcesDir: string): Promise<Signable[]> {
    /// Archives, and their content that need to be signed in an extra step. If a new archive is added
    /// to the engine dependencies this also needs to be added here. If an archive is not added here, it
    /// will show up as a failure to notarise the IDE. The offending archive will be named in the error
    /// message provided by Apple and can then be added here.
    const engineDir = `${resourcesDir}/enso/dist/*`
    const archivePatterns: ArchivePattern[] = [
        [
            `lib/Standard/Database/*/polyglot/java/sqlite-jdbc-*.jar`,
            [
                'org/sqlite/native/Mac/aarch64/libsqlitejdbc.jnilib',
                'org/sqlite/native/Mac/x86_64/libsqlitejdbc.jnilib',
            ],
        ],
        [
            `/component/runner.jar`,
            [
                'org/sqlite/native/Mac/x86_64/libsqlitejdbc.jnilib',
                'com/sun/jna/darwin-aarch64/libjnidispatch.jnilib',
                'com/sun/jna/darwin-x86-64/libjnidispatch.jnilib',
            ],
        ],
    ]
    return ArchiveToSign.lookupMany(engineDir, archivePatterns)
}

// ================
// === Signing. ===
// ================

/** Information we need to sign a given binary. */
interface SigningContext {
    /** A digital identity that is stored in a keychain that is on the calling user's keychain
     * search list. We rely on this already being set up by the Electron Builder.
     */
    identity: string
    /** Path to the entitlements file. */
    entitlements: string
}

/** An entity that we want to sign. */
interface Signable {
    /** Sign this entity. */
    sign(context: SigningContext): Promise<void>
}

/** Placeholder name for temporary archives. */
const tmpArchive = 'temporary_archive.zip'

/** Helper to execute a program in a given directory and return the output. */
const run = (cmd: string, args: string[], cwd?: string) => {
    console.log('Running', cmd, args, cwd)
    return child_process.execFileSync(cmd, args, { cwd }).toString()
}

/** Archive with some binaries that we want to sign.
 *
 * Can be either a zip or a jar file.
 */
class ArchiveToSign implements Signable {
    /** An absolute path to the archive. */
    path: string

    /** A list of patterns for files to sign inside the archive.
     *
     *  Relative to the root of the archive.
     */
    binaries: glob.Pattern[]

    /** Create a new instance. */
    constructor(path: string, binaries: glob.Pattern[]) {
        this.path = path
        this.binaries = binaries
    }

    /**
     * Sign content of an archive. This function extracts the archive, signs the required files,
     * re-packages the archive and replaces the original.
     */
    async sign(context: SigningContext) {
        console.log(`Signing archive ${this.path}`)
        const archiveName = path.basename(this.path)
        const workingDir = await getTmpDir()
        try {
            const isJar = archiveName.endsWith(`jar`)

            if (isJar) {
                run(`jar`, ['xf', this.path], workingDir)
            } else {
                // We cannot use `unzip` here because of the following issue:
                // https://unix.stackexchange.com/questions/115825/extra-bytes-error-when-unzipping-a-file
                // This started to be an issue with GraalVM 22.3.0 release.
                run(`7za`, ['X', `-o${workingDir}`, this.path])
            }

            const binariesToSign = await BinaryToSign.lookupMany(workingDir, this.binaries)
            for (const binaryToSign of binariesToSign) {
                binaryToSign.sign(context)
            }

            if (isJar) {
                if (archiveName.includes(`runner`)) {
                    run(`jar`, ['-cfm', tmpArchive, 'META-INF/MANIFEST.MF', '.'], workingDir)
                } else {
                    run(`jar`, ['-cf', tmpArchive, '.'], workingDir)
                }
            } else {
                run(`zip`, ['-rm', tmpArchive, '.'], workingDir)
            }

            // We cannot use fs.rename because temp and target might be on different volumes.
            console.log(run(`/bin/mv`, [path.join(workingDir, tmpArchive), this.path]))
            console.log(
                `Successfully repacked ${this.path} to handle signing inner native dependency.`
            )
        } catch (error) {
            console.error(
                `Could not repackage ${archiveName}. Please check the ${import.meta.url} task to ` +
                    `ensure that it's working. This jar has to be treated specially` +
                    ` because it has a native library and Apple's codesign does not sign inner ` +
                    `native libraries correctly for jar files.`
            )
            throw error
        } finally {
            await rmRf(workingDir)
        }
    }

    /** Looks up for archives to sign using the given path pattern. */
    static async lookup(base: string, [pattern, binaries]: ArchivePattern) {
        return lookupHelper(path => new ArchiveToSign(path, binaries))(base, pattern)
    }

    /** Looks up for archives to sign using the given path patterns. */
    static lookupMany = lookupManyHelper(ArchiveToSign.lookup)
}

/** A single code binary file to be signed. */
class BinaryToSign implements Signable {
    /** An absolute path to the binary. */
    path: string

    /** Create a new instance. */
    constructor(path: string) {
        this.path = path
    }

    /** Sign this binary. */
    async sign({ entitlements, identity }: SigningContext) {
        console.log(`Signing ${this.path}`)
        run(`codesign`, [
            '-vvv',
            '--entitlements',
            entitlements,
            '--force',
            '--options=runtime',
            '--sign',
            identity,
            this.path,
        ])
    }

    /** Looks up for binaries to sign using the given path pattern. */
    static lookup = lookupHelper(path => new BinaryToSign(path))

    /** Looks up for binaries to sign using the given path patterns. */
    static lookupMany = lookupManyHelper(BinaryToSign.lookup)
}

// ==============================
// === Discovering Signables. ===
// ==============================

/** Helper used to concisely define patterns for an archive to sign.
 *
 * Consists of pattern of the archive path and set of patterns for files to sign inside the archive.
 */
type ArchivePattern = [glob.Pattern, glob.Pattern[]]

/** Like `glob` but returns absolute paths by default. */
async function globAbs(pattern: glob.Pattern, options?: glob.Options): Promise<string[]> {
    const paths = await glob(pattern, { absolute: true, ...options })
    return paths
}

/** Glob patterns relative to a given base directory. Base directory is allowed to be a pattern as
 * well.
 **/
async function globAbsIn(
    base: glob.Pattern,
    pattern: glob.Pattern,
    options?: glob.Options
): Promise<string[]> {
    return globAbs(path.join(base, pattern), options)
}

/** Generate a lookup function for a given Signable type. */
function lookupHelper<R extends Signable>(mapper: (path: string) => R) {
    return async (base: string, pattern: glob.Pattern) => {
        const paths = await globAbsIn(base, pattern)
        return paths.map(mapper)
    }
}

/** Generate a lookup function for a given Signable type. */
function lookupManyHelper<T, R extends Signable>(
    lookup: (base: string, pattern: T) => Promise<R[]>
) {
    return async function (base: string, patterns: T[]) {
        const results = await Promise.all(patterns.map(pattern => lookup(base, pattern)))
        return results.flat()
    }
}

// ==================
// === Utilities. ===
// ==================

/** Remove file recursively. */
async function rmRf(path: string) {
    await fs.rm(path, { recursive: true, force: true })
}

/**
 * Get a new temporary directory. Caller is responsible for cleaning up the directory.
 */
async function getTmpDir(prefix?: string) {
    const ret = await fs.mkdtemp(path.join(os.tmpdir(), prefix ?? 'enso-signing-'))
    return ret
}

// ====================
// === Entry point. ===
// ====================

/** Input for this script. */
interface Input extends SigningContext {
    appOutDir: string
    productFilename: string
}

/** Entry point, meant to be used from an afterSign Electron Builder's hook. */
export default async function (context: Input) {
    console.log('Environment: ', process.env)
    const { appOutDir, productFilename, entitlements } = context
    const appDir = path.join(appOutDir, `${productFilename}.app`)
    const contentsDir = path.join(appDir, 'Contents')
    const resourcesDir = path.join(contentsDir, 'Resources')

    // Sign archives.
    console.log('Signing GraalVM elemenets...')
    for (const signable of await graalSignables(resourcesDir)) await signable.sign(context)

    console.log('Signing Engine elements...')
    for (const signable of await ensoPackageSignables(resourcesDir)) await signable.sign(context)

    // Finally re-sign the top-level enso.
    const topLevelExecutable = new BinaryToSign(path.join(contentsDir, 'MacOS', productFilename))
    await topLevelExecutable.sign(context)
}
