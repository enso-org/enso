import { EnsoPath, FileId } from 'enso-common/src/services/Backend'
import { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { spawn } from 'node:child_process'
import process from 'node:process'
import { arrayBuffer } from 'node:stream/consumers'
import { expect, test } from 'vitest'
import { tarGzWriteStream } from '../../src/archive'
import { createRemoteBackend } from '../../src/backend'
import { electronExecutablePath } from '../electronTest'

const remoteBackend = await createRemoteBackend()

function runAppExecutable(args: readonly string[]): Promise<{
  readonly stdout: string
  readonly stderr: string
  readonly code: number
}> {
  // `spawnSync` is fine, use async here in case blocking will be slower in the future.
  const appProcess = spawn(electronExecutablePath, args, {
    env: { ...process.env, NODE_ENV: 'development' },
  })
  return new Promise<{
    readonly stdout: string
    readonly stderr: string
    readonly code: number
  }>((resolve, reject) => {
    appProcess.on('error', reject)
    let stdout = ''
    let stderr = ''
    appProcess.stdout.on('data', (data) => {
      stdout += data.toString()
    })
    appProcess.stderr.on('data', (data) => {
      stderr += data.toString()
    })
    appProcess.on('exit', (code) => {
      resolve({ stdout, stderr, code: code ?? 0 })
    })
  })
}

async function uploadFile(remoteBackend: RemoteBackend, file: File): Promise<void> {
  const { presignedUrls, sourcePath, uploadId } = await remoteBackend.uploadFileStart(
    { fileId: null, fileName: file.name, parentDirectoryId: null },
    file,
  )
  const parts = await Promise.all(
    presignedUrls.map((url, index) =>
      remoteBackend.uploadFileChunk(url, file, index).then((result) => result.part),
    ),
  )
  await remoteBackend.uploadFileEnd({
    sourcePath,
    uploadId,
    parts,
    fileName: file.name,
    assetId: null,
    parentDirectoryId: null,
  })
}

async function createProject(mainEnso: string, projectName: string) {
  const fileName = `${projectName}.enso-project`
  const fileStream = tarGzWriteStream()
  const fileBlob = arrayBuffer(fileStream.stream)
  await fileStream.addFile(
    Buffer.from(`name: ${projectName}
namespace: local
version: 0.0.1
edition: 2025.2.1
prefer-local-libraries: 'true'`),
    { name: 'package.yaml' },
  )
  await fileStream.addFolder({ name: 'src' })
  await fileStream.addFile(Buffer.from(mainEnso), { name: 'src/Main.enso' })
  fileStream.finalize()
  return new File([await fileBlob], fileName)
}

async function uploadProject(
  remoteBackend: RemoteBackend,
  username: string,
  projectName: string,
  mainEnso: string,
  { force = false, log = (_message: unknown) => {} } = {},
) {
  if (force) {
    log('Fetching existing project...')
    const project = await remoteBackend
      .resolveEnsoPath(EnsoPath(`enso://Users/${username}/headless file write.project`))
      .catch(() => null)
    if (project) {
      log('Existing project found, deleting...')
      await remoteBackend.deleteAsset(project.id, { force: true }, project.title)
    } else {
      log('No existing project found, skipping deletion.')
    }
  }
  log('Uploading new project...')
  const projectFile = await createProject(mainEnso, projectName)
  await uploadFile(remoteBackend, projectFile)
}

// FIXME: This test is skipped as it currently has issues.
test.skip('writing to cloud file', async () => {
  const valueToWrite = Math.floor(Math.random() * 1_000_000)
  console.info('Fetching user info...')
  const user = await remoteBackend.usersMe()
  if (!user) {
    throw new Error('No user logged in')
  }
  const fileContentToWrite = `\
from Standard.Base import all
from Standard.Table import all
from Standard.Database import all
from Standard.AWS import all
from Standard.Geo import all
from Standard.Google import all
from Standard.Microsoft import all
from Standard.Snowflake import all
from Standard.Tableau import all
import Standard.Examples
import Standard.Visualization

main =
    file1 = "${valueToWrite}".write '~/headless written file.txt'`
  await uploadProject(remoteBackend, user.name, 'headless file write', fileContentToWrite, {
    force: true,
    log: (message) => console.info(message),
  })
  const project = await remoteBackend
    .resolveEnsoPath(EnsoPath(`enso://Users/${user.name}/headless file write.project`))
    .catch(() => null)
  expect(project?.type === 'project', 'Project was uploaded correctly').toBe(true)
  if (project?.type !== 'project') {
    throw new Error('Project not found after upload')
  }
  const actualFileContent = await remoteBackend.getMainFileContent(project.id)
  expect(actualFileContent, 'Project content was written correctly').toBe(fileContentToWrite)
  console.info('Running app executable...')
  const { stdout, stderr, code } = await runAppExecutable([
    '--headless',
    '--startup.project',
    `enso://Users/${user.name}/headless file write.enso-project`,
  ])
  if (code !== 0) {
    process.stdout.write(stdout)
    process.stderr.write(stderr)
  }
  expect(code, 'Process should exit with code 0').toBe(0)
  console.info('Resolving written file...')
  const siblings = await remoteBackend.searchDirectory({
    title: 'headless',
    description: null,
    extension: null,
    query: null,
    type: null,
    from: null,
    labels: null,
    pageSize: null,
    parentId: null,
    sortDirection: null,
    sortExpression: null,
  })
  console.log(siblings)
  const fileAsset = await remoteBackend.resolveEnsoPath(
    EnsoPath(`enso://Users/${user.name}/headless written file.txt`),
  )
  console.info('Fetching file details...')
  const fileInfo = await remoteBackend.getFileDetails(
    fileAsset.id as FileId,
    'headless written file.txt',
    true,
  )
  const fileResponse = await fetch(fileInfo.url!)
  const fileContent = await fileResponse.text()
  expect(fileContent, 'File content should match the written value').toBe(String(valueToWrite))
})
