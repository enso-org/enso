<script setup lang="ts">
import MarkdownEditorImpl from '@/components/MarkdownEditor/MarkdownEditorImpl.vue'
import type { Text } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { TreeCursor, type SyntaxNode } from '@lezer/common'
import { computed, watch, type WatchSource } from 'vue'

const { source, parsed } = defineProps<{
  source: Text
  parsed: SyntaxNode
}>()

function parseRow(cursor: TreeCursor, output: string[]) {
  if (!cursor.firstChild()) return
  do {
    if (cursor.name === 'TableCell') {
      output.push(source.sliceString(cursor.from, cursor.to))
    } else if (cursor.name !== 'TableDelimiter') {
      console.warn('Unexpected in table row:', cursor.name)
    }
  } while (cursor.nextSibling())
  cursor.parent()
}

const content = computed(() => {
  const headers: string[] = []
  const rows: string[][] = []
  const cursor = parsed.cursor()
  if (cursor.firstChild()) {
    do {
      if (cursor.name === 'TableRow') {
        const newRow: string[] = []
        parseRow(cursor, newRow)
        rows.push(newRow)
      } else if (cursor.name === 'TableHeader') {
        parseRow(cursor, headers)
      } else if (cursor.name !== 'TableDelimiter') {
        console.warn('Unexpected at top level of table:', cursor.name)
      }
    } while (cursor.nextSibling())
  }
  return { headers, rows }
})

const sync = (text: WatchSource<string | undefined>) => (view: EditorView) => {
  watch(
    text,
    (newText) =>
      view.dispatch({
        changes: { from: 0, to: view.state.doc.length, insert: newText ?? '' },
      }),
    { immediate: true },
  )
}
</script>

<template>
  <table>
    <thead>
      <tr>
        <th v-for="(_cell, c) in content.headers" :key="c" class="cell">
          <MarkdownEditorImpl
            :toolbar="false"
            readonly
            :editorReadyCallback="sync(() => content.headers[c])"
          />
        </th>
      </tr>
    </thead>
    <tbody class="tableBody">
      <tr v-for="(row, r) in content.rows" :key="r" class="row">
        <td v-for="(_cell, c) in row" :key="c" class="cell">
          <MarkdownEditorImpl
            :toolbar="false"
            readonly
            :editorReadyCallback="sync(() => content.rows[r]?.[c])"
          />
        </td>
      </tr>
    </tbody>
  </table>
</template>

<style scoped>
.cell {
  border: 1px solid #dddddd;
}
.tableBody .row:nth-of-type(even) {
  background-color: #f3f3f3;
}
:deep(.cm-line) {
  padding-right: 6px;
}
</style>
