import crypto from 'node:crypto'
import os from 'node:os'
import path from 'node:path'

import { run } from './lib/exec.mjs'
import { ensurePathExists, getTmpDir, rmRf } from './lib/fs.mjs'
import { globAbsoluteIn } from './lib/glob.mjs'
import { createLogger } from './lib/logger.mjs'
import { buildCodesignArgs } from './lib/signing.mjs'

class BinaryToSign {
  constructor(filePath) {
    this.path = filePath
  }

  async shouldSign({ verbose, logger }) {
    try {
      const output = run('file', ['-b', this.path], { verbose, stdio: 'pipe' })
      return output.includes('Mach-O')
    } catch (error) {
      logger.warn(`Unable to determine file type for ${this.path}: ${error}`)
      return false
    }
  }

  static async lookup(base, pattern) {
    const paths = await globAbsoluteIn(base, pattern)
    return paths.map((filePath) => new BinaryToSign(filePath))
  }

  static async lookupMany(base, patterns, { logger }) {
    const results = await Promise.all(
      patterns.map(async (pattern) => {
        const found = await BinaryToSign.lookup(base, pattern)
        if (found.length === 0) {
          logger.debug(`No files found for pattern ${String(pattern)} in ${base}`)
        }
        return found
      }),
    )
    return results.flat()
  }

  async sign({ entitlements, identity, keychainPath, verbose, logger }) {
    if (!(await this.shouldSign({ verbose, logger }))) {
      return false
    }

    const args = buildCodesignArgs({
      target: this.path,
      identity,
      entitlements,
      keychainPath,
      verbose,
    })
    run('codesign', args, { verbose })
    return true
  }
}

class ArchiveToSign {
  constructor(filePath, binaries) {
    this.path = filePath
    this.binaries = binaries
  }

  static async lookup(base, [pattern, binaries]) {
    const paths = await globAbsoluteIn(base, pattern)
    return paths.map((filePath) => new ArchiveToSign(filePath, binaries))
  }

  static async lookupMany(base, patterns, { logger }) {
    const results = await Promise.all(
      patterns.map(async (pattern) => {
        const found = await ArchiveToSign.lookup(base, pattern)
        if (found.length === 0) {
          logger.debug(`No files found for pattern ${String(pattern)} in ${base}`)
        }
        return found
      }),
    )
    return results.flat()
  }

  async sign(context) {
    const archiveName = path.basename(this.path)
    const workingDir = await getTmpDir()

    try {
      const archiveType = getArchiveType(archiveName)
      extractArchive({ archiveType, archivePath: this.path, workingDir, verbose: context.verbose })

      let anySigned = false

      const nestedArchives = await lookupArchivesInDirectory(workingDir, this.binaries)
      for (const nestedArchive of nestedArchives) {
        await nestedArchive.sign(context)
        anySigned = true
      }

      const binariesToSign = await BinaryToSign.lookupMany(workingDir, this.binaries, {
        logger: context.logger,
      })
      for (const binaryToSign of binariesToSign) {
        const signed = await binaryToSign.sign(context)
        anySigned ||= signed
      }

      if (!anySigned) {
        return
      }

      const repackedPath = repackArchive({
        archiveType,
        archiveName,
        workingDir,
        verbose: context.verbose,
      })
      run('/bin/mv', [repackedPath, this.path], { verbose: context.verbose })
    } catch (error) {
      context.logger.error(
        `Could not repackage ${archiveName}. This archive has a native library and must be handled specially before notarization.`,
      )
      throw error
    } finally {
      await rmRf(workingDir)
    }
  }
}

function getArchiveType(archiveName) {
  if (archiveName.endsWith('.tar.gz') || archiveName.endsWith('.tgz')) {
    return 'tar.gz'
  }
  if (archiveName.endsWith('.tar')) {
    return 'tar'
  }
  if (
    archiveName.endsWith('.jar') ||
    archiveName.endsWith('.jmod') ||
    archiveName.endsWith('.zip')
  ) {
    return 'zip'
  }
  throw new Error(`Unsupported archive type for ${archiveName}`)
}

function extractArchive({ archiveType, archivePath, workingDir, verbose }) {
  if (archiveType === 'zip') {
    run('ditto', ['-x', '-k', archivePath, workingDir], { verbose })
    return
  }

  if (archiveType === 'tar') {
    run('tar', ['-xf', archivePath], { cwd: workingDir, verbose })
    return
  }

  if (archiveType === 'tar.gz') {
    run('tar', ['-xzf', archivePath], { cwd: workingDir, verbose })
    return
  }

  throw new Error(`Unsupported archive type ${archiveType}`)
}

function repackArchive({ archiveType, archiveName, workingDir, verbose }) {
  const outputPath = path.join(os.tmpdir(), `enso-repack-${crypto.randomUUID()}-${archiveName}`)

  if (archiveType === 'zip') {
    run('ditto', ['-c', '-k', '--sequesterRsrc', '.', outputPath], {
      cwd: workingDir,
      verbose,
    })
    return outputPath
  }

  if (archiveType === 'tar') {
    run('tar', ['-cf', outputPath, '.'], { cwd: workingDir, verbose })
    return outputPath
  }

  if (archiveType === 'tar.gz') {
    run('tar', ['-czf', outputPath, '.'], { cwd: workingDir, verbose })
    return outputPath
  }

  throw new Error(`Unsupported archive type ${archiveType}`)
}

async function lookupArchivesInDirectory(base, binariesPatterns) {
  const nestedArchivePatterns = [
    '**/*.jar',
    '**/*.jmod',
    '**/*.zip',
    '**/*.tar',
    '**/*.tgz',
    '**/*.tar.gz',
  ]
  const nestedArchivePaths = await globAbsoluteIn(base, nestedArchivePatterns)
  return nestedArchivePaths.map((archivePath) => new ArchiveToSign(archivePath, binariesPatterns))
}

async function graalSignables(resourcesDir, { logger }) {
  const archivePatterns = [
    ['Contents/Home/jmods/java.base.jmod', ['bin/java', 'bin/keytool', 'lib/jspawnhelper']],
    ['Contents/Home/jmods/java.rmi.jmod', ['bin/rmiregistry']],
    ['Contents/Home/jmods/java.scripting.jmod', ['bin/jrunscript']],
    ['Contents/Home/jmods/jdk.compiler.jmod', ['bin/javac', 'bin/serialver']],
    ['Contents/Home/jmods/jdk.hotspot.agent.jmod', ['bin/jhsdb']],
    ['Contents/Home/jmods/jdk.httpserver.jmod', ['bin/jwebserver']],
    ['Contents/Home/jmods/jdk.jartool.jmod', ['bin/jarsigner', 'bin/jar']],
    ['Contents/Home/jmods/jdk.javadoc.jmod', ['bin/javadoc']],
    ['Contents/Home/jmods/jdk.jconsole.jmod', ['bin/jconsole']],
    ['Contents/Home/jmods/jdk.jdeps.jmod', ['bin/javap', 'bin/jdeprscan', 'bin/jdeps']],
    ['Contents/Home/jmods/jdk.jdi.jmod', ['bin/jdb']],
    ['Contents/Home/jmods/jdk.jfr.jmod', ['bin/jfr']],
    ['Contents/Home/jmods/jdk.jlink.jmod', ['bin/jmod', 'bin/jlink', 'bin/jimage']],
    ['Contents/Home/jmods/jdk.jshell.jmod', ['bin/jshell']],
    [
      'Contents/Home/jmods/jdk.jpackage.jmod',
      ['bin/jpackage', 'classes/jdk/jpackage/internal/resources/jpackageapplauncher'],
    ],
    ['Contents/Home/jmods/jdk.jstatd.jmod', ['bin/jstatd']],
    [
      'Contents/Home/jmods/jdk.jcmd.jmod',
      ['bin/jstack', 'bin/jcmd', 'bin/jps', 'bin/jmap', 'bin/jstat', 'bin/jinfo'],
    ],
  ]

  const binariesPatterns = ['Contents/MacOS/libjli.dylib']
  const graalDir = path.join(resourcesDir, 'enso', 'runtime', '*')

  const archives = await ArchiveToSign.lookupMany(graalDir, archivePatterns, { logger })
  const binaries = await BinaryToSign.lookupMany(graalDir, binariesPatterns, { logger })
  return [...archives, ...binaries]
}

async function ensoPackageSignables(resourcesDir, { verbose = false, logger } = {}) {
  const engineDir = `${resourcesDir}/enso/dist/*`
  const archiveContentPatterns = ['**/*.{dylib,jnilib,so,node}', '**/bin/*']

  async function jarHasNativeEntries(jarPath, verbose) {
    try {
      const listing = run('unzip', ['-l', jarPath], { verbose, stdio: 'pipe' })
      if (/\.(dylib|jnilib|node)$/im.test(listing)) {
        return true
      }
      return /\.so$/im.test(listing)
    } catch (error) {
      logger.debug(`Could not inspect archive ${jarPath}, will process conservatively: ${error}`)
      return true
    }
  }

  const engineJars = await globAbsoluteIn(engineDir, [
    '**/*.jar',
    '**/*.jmod',
    '**/*.zip',
    '**/*.tar',
    '**/*.tgz',
    '**/*.tar.gz',
  ])
  const jarsToProcess = []
  for (const jarPath of engineJars) {
    if (jarPath.endsWith('.jar') && !(await jarHasNativeEntries(jarPath, verbose))) {
      continue
    }
    jarsToProcess.push(jarPath)
  }

  const archives = jarsToProcess.map(
    (jarPath) => new ArchiveToSign(jarPath, archiveContentPatterns),
  )

  const binaries = await BinaryToSign.lookupMany(engineDir, archiveContentPatterns, { logger })
  return [...archives, ...binaries]
}

export async function signArchivesWithIdentity({
  app,
  entitlements,
  identity,
  keychainPath,
  verbose = false,
  logger,
}) {
  await ensurePathExists(app, 'App bundle')
  await ensurePathExists(entitlements, 'Entitlements file')

  const resolvedLogger = logger ?? createLogger({ verbose })

  const appDir = path.resolve(app)
  const contentsDir = path.join(appDir, 'Contents')
  const resourcesDir = path.join(contentsDir, 'Resources')
  const ensoDir = path.join(resourcesDir, 'enso')

  await ensurePathExists(ensoDir, 'Enso resources directory')
  run('chmod', ['-R', 'u+w', ensoDir], { verbose })

  resolvedLogger.info('Signing GraalVM elements...')
  for (const signable of await graalSignables(resourcesDir, { logger: resolvedLogger })) {
    await signable.sign({
      identity,
      entitlements,
      keychainPath,
      verbose,
      logger: resolvedLogger,
    })
  }

  resolvedLogger.info('Signing Engine elements...')
  for (const signable of await ensoPackageSignables(resourcesDir, {
    verbose,
    logger: resolvedLogger,
  })) {
    await signable.sign({
      identity,
      entitlements,
      keychainPath,
      verbose,
      logger: resolvedLogger,
    })
  }

  resolvedLogger.info('Engine signing completed successfully.')
}
