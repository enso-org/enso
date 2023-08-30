import {
  ref,
  watchEffect,
  type Ref,
  type ShallowRef,
  watch,
  shallowRef,
  type WatchSource,
  computed,
} from 'vue'
import { defineStore } from 'pinia'
import * as Y from 'yjs'
import { WebsocketProvider } from 'y-websocket'
import { Awareness } from 'y-protocols/awareness'
import { evalWatchSource } from '@/util/reactivity'

watch

/**
 * The project store synchronizes and holds the open project-related data. The synchronization is
 * performed using a CRDT data types from Yjs. Once the data is synchronized with a "LS bridge"
 * client, it is submitted to the language server as a document update.
 */
export const useProjectStore = defineStore('project', () => {
  const rootDocument = new Y.Doc()
  const awareness = new Awareness(rootDocument)

  // inputs
  const projectName = ref<string>()
  const observedFileName = ref<string>()

  // For now, let's assume that the websocket server is running on the same host as the web server.
  // Eventually, we can make this configurable, or even runtime variable.
  let socketUrl = location.origin.replace(/^http/, 'ws')

  watchEffect((onCleanup) => {
    const provider = new WebsocketProvider(socketUrl, 'enso-projects', rootDocument, { awareness })
    onCleanup(() => {
      provider.disconnect()
    })
  })

  const projectsMap = rootDocument.getMap<Y.Doc>('projects')
  const projectDoc = reactiveMapValue(() => projectsMap, projectName)

  watchEffect((onCleanup) => {
    const doc = projectDoc.value
    if (doc == null) return
    doc.load()
    onCleanup(() => {
      doc.destroy()
    })
  })

  const projectFiles = computed(() => projectDoc.value?.getMap<Y.Text>('files'))
  const observedFileText = reactiveMapValue(projectFiles, observedFileName)

  return {
    setProjectName(name: string) {
      projectName.value = name
    },
    setObservedFileName(name: string) {
      projectName.value = name
    },
    projectFileNames: computed(() => projectFiles.value?.keys() ?? []),
    observedFileText,
  }
})

function reactiveMapValue<T>(
  mapSource: WatchSource<Y.Map<T> | undefined>,
  keySource: Ref<string | undefined>,
): ShallowRef<T | undefined> {
  const mapValue = shallowRef<T>()
  watchEffect((onCleanup) => {
    const theMap = evalWatchSource(mapSource)
    if (theMap == null) return
    const theKey = evalWatchSource(keySource)
    if (theKey == null) return

    mapValue.value = theMap.get(theKey)
    const observeKeyChange = (change: Y.YMapEvent<T>) => {
      if (change.keysChanged.has(theKey)) {
        mapValue.value = theMap.get(theKey)
      }
    }

    theMap.observe(observeKeyChange)
    onCleanup(() => {
      theMap.unobserve(observeKeyChange)
    })
  })

  return mapValue
}
