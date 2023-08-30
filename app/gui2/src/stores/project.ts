import { ref, computed } from 'vue'
import { defineStore } from 'pinia'
import * as yjs from 'yjs'

/**
 * The project store synchronizes and holds the open project-related data. The synchronization is
 * performed using a CRDT data types from Yjs.
 */
export const useProjectStore = defineStore('project', () => {
  //   const doc = new yjs.Doc()
  //   const files = doc.getMap('files')

  //   files.get('test.txt')

  const files = ref<ProjectFile[]>([])
})

interface ProjectFile {
  path: yjs.Text
  content: yjs.Text
}
