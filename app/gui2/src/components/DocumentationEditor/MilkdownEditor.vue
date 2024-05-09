<script setup lang="ts">
import {
  Editor,
  editorViewCtx,
  parserCtx,
  prosePluginsCtx,
  rootCtx,
  serializerCtx,
  SerializerReady,
} from '@milkdown/core'
import type { Ctx, MilkdownPlugin } from '@milkdown/ctx'
import { commonmark } from '@milkdown/preset-commonmark'
import { Slice } from '@milkdown/prose/model'
import { Plugin, PluginKey } from '@milkdown/prose/state'
import { nord } from '@milkdown/theme-nord'
import '@milkdown/theme-nord/style.css'
import { insert } from '@milkdown/utils'
import { Milkdown, useEditor } from '@milkdown/vue'
import { createDebouncer } from 'lib0/eventloop'
import { shallowRef, watch, watchEffect } from 'vue'

const syncedContent = defineModel<string>({ required: true })
const editing = defineModel<boolean>('editing', { default: false })
const props = defineProps<{
  preferSingleLine?: boolean | undefined
}>()

// === Set up editor ===

function buildEditor() {
  const editor = shallowRef<Editor>()
  const { get, loading } = useEditor((root) =>
    Editor.make()
      .config((ctx) => {
        ctx.set(rootCtx, root)
      })
      .config(nord)
      .use(commonmark)
      .use(ensoSync),
  )
  watch(loading, (loading) => {
    if (!loading) editor.value = get()!
  })
  return editor
}

// === Update editor from model ===

type MilkdownAction = (ctx: Ctx) => void

function syncToContent(newContent: string): MilkdownAction {
  return (ctx: Ctx) => {
    const view = ctx.get(editorViewCtx)
    const parser = ctx.get(parserCtx)
    const { state } = view
    const doc = parser(newContent)
    if (!doc) return
    const serializer = ctx.get(serializerCtx)
    if (serializer(state.doc).trimEnd() === newContent.trimEnd()) return
    view.dispatch(
      state.tr
        .replace(0, state.doc.content.size, new Slice(doc.content, 0, 0))
        .setMeta(ensoSyncKey, true),
    )
  }
}

// === Update model from editor ===

const ensoSyncKey = new PluginKey('ENSO_SYNC')

const ensoSync: MilkdownPlugin = (ctx) => {
  const debouncer = createDebouncer(0)
  return async () => {
    await ctx.wait(SerializerReady)
    const serializer = ctx.get(serializerCtx)
    const plugin = new Plugin({
      ensoSyncKey,
      state: {
        init: () => {},
        apply: (tr) => {
          if (!tr.docChanged || tr.getMeta('addToHistory') === false) return
          if (tr.getMeta(ensoSyncKey) === true) return
          const markdown = serializer(tr.doc)
          // Trim the content (and compare trimmed) because Milkdown adds a trailing newline.
          const trimmed = markdown.trimEnd()
          if (trimmed !== syncedContent.value.trimEnd())
            debouncer(() => (syncedContent.value = trimmed))
        },
      },
    })
    ctx.update(prosePluginsCtx, (x) => x.concat(plugin))
  }
}

// === Start editor ===

const editor = buildEditor()

watchEffect(() => editor.value?.action(syncToContent(syncedContent.value)))
watchEffect(() => editor.value?.action((ctx) => (ctx.get(editorViewCtx).dom.spellcheck = false)))
watchEffect(() => {
  editor.value?.action(
    editing.value ?
      (ctx) => ctx.get(editorViewCtx).dom.focus()
    : (ctx) => ctx.get(editorViewCtx).dom.blur(),
  )
})

function onEnter(e: KeyboardEvent) {
  if (props.preferSingleLine) {
    if (e.shiftKey) {
      editor.value?.action(insert('\n'))
    } else {
      editing.value = false
    }
    e.stopImmediatePropagation()
    e.preventDefault()
  }
}
</script>

<template>
  <Milkdown
    class="milkdown"
    @wheel.stop.passive
    @contextmenu.stop
    @click.stop="editing = true"
    @focusin="editing = true"
    @focusout="editing = false"
    @keydown.enter.capture="onEnter"
  />
</template>

<style scoped>
.milkdown {
  user-select: auto;
  height: 100%;
  white-space: pre-wrap;
}
</style>

<style>
h1 {
  font-weight: 700;
  font-size: 16px;
  line-height: 1.75;
}

h2,
h3,
h4,
h5,
h6 {
  font-size: 14px;
  line-height: 2;
}

p + p {
  margin-bottom: 4px;
}

.ProseMirror {
  outline: none;
}
</style>
