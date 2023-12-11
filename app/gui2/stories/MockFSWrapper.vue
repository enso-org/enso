<script setup lang="ts">
import { useProjectStore } from '@/stores/project'
import { mockFsDirectoryHandle } from '@/util/convert/fsAccess'
import { MockWebSocket, type WebSocketHandler } from '@/util/net'
import { mockDataWSHandler } from 'shared/dataServer/mock'
import { type Path as LSPath } from 'shared/languageServerTypes'
import { watchEffect } from 'vue'

const projectStore = useProjectStore()

interface FileTree {
  [name: string]: FileTree | string | ArrayBuffer
}

const props = defineProps<{
  files: FileTree | undefined
  directory: FileSystemDirectoryHandle | undefined
  /** The path of the root directory. */
  prefix?: string[] | undefined
}>()

let resolveDataWsHandler: ((handler: WebSocketHandler) => void) | undefined
let dataWsHandler: Promise<WebSocketHandler> = new Promise((resolve) => {
  resolveDataWsHandler = resolve
})
function setDataWsHandler(handler: WebSocketHandler) {
  if (resolveDataWsHandler) {
    resolveDataWsHandler(handler)
    resolveDataWsHandler = undefined
  } else {
    dataWsHandler = Promise.resolve(handler)
  }
}

MockWebSocket.addMock('data', async (data, send) => {
  ;(await dataWsHandler)(data, send)
})

watchEffect(async (onCleanup) => {
  let maybeDirectory = props.files ? mockFsDirectoryHandle(props.files, '(root)') : props.directory
  if (!maybeDirectory) return
  const prefixLength = props.prefix?.length ?? 0
  const directory = maybeDirectory
  const ls = await projectStore.lsRpcConnection
  const maybeProjectRoot = (await projectStore.contentRoots).find((root) => root.type === 'Project')
    ?.id
  if (!maybeProjectRoot) return
  const projectRoot = maybeProjectRoot
  async function walkFiles(
    dir: FileSystemDirectoryHandle,
    segments: string[],
    cb: (path: LSPath) => void,
  ) {
    for await (const [name, dirOrFile] of dir.entries()) {
      const newSegments = [...segments, name]
      if (dirOrFile.kind === 'directory') walkFiles(dirOrFile, newSegments, cb)
      else {
        cb({
          rootId: projectRoot,
          segments: newSegments.slice(prefixLength),
        })
      }
    }
  }
  walkFiles(directory, props.prefix ?? [], (path) =>
    ls.emit('file/event', [{ kind: 'Added', path }]),
  )
  onCleanup(() => {
    walkFiles(directory, props.prefix ?? [], (path) =>
      ls.emit('file/event', [{ kind: 'Removed', path }]),
    )
  })
  setDataWsHandler(
    mockDataWSHandler(async (segments) => {
      segments = segments.slice(prefixLength)
      if (!segments.length) return
      let file
      try {
        let dir = directory
        for (const segment of segments.slice(0, -1)) {
          dir = await dir.getDirectoryHandle(segment)
        }
        const fileHandle = await dir.getFileHandle(segments.at(-1)!)
        file = await fileHandle.getFile()
      } catch {
        return
      }
      return await file?.arrayBuffer()
    }),
  )
})
</script>

<template>
  <slot></slot>
</template>
