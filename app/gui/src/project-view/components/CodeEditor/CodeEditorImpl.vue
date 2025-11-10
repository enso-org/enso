<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { useEnsoDiagnostics } from '@/components/CodeEditor/diagnostics'
import { ensoSyntax } from '@/components/CodeEditor/ensoSyntax'
import { useEnsoSourceSync } from '@/components/CodeEditor/sync'
import { ensoHoverTooltip } from '@/components/CodeEditor/tooltips'
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import VueHostRender, { VueHostInstance } from '@/components/VueHostRender.vue'
import { useAutoBlur } from '@/util/autoBlur'
import { useCodeMirror } from '@/util/codemirror'
import { highlightStyle } from '@/util/codemirror/highlight'
import { testSupport } from '@/util/codemirror/testSupport'
import { indentWithTab, insertNewlineKeepIndent } from '@codemirror/commands'
import {
  bracketMatching,
  defaultHighlightStyle,
  foldGutter,
  syntaxHighlighting,
} from '@codemirror/language'
import { lintGutter } from '@codemirror/lint'
import { highlightSelectionMatches } from '@codemirror/search'
import { drawSelection, keymap } from '@codemirror/view'
import { onMounted, toRef, useTemplateRef, type ComponentInstance } from 'vue'

const { store: project, module, graph } = useCurrentProject()

const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorRoot>>('editorRoot')

const autoindentOnEnter = {
  key: 'Enter',
  run: insertNewlineKeepIndent,
}

const vueHost = new VueHostInstance()
const { editorView, setExtraExtensions } = useCodeMirror(editorRoot, {
  extensions: [
    keymap.of([indentWithTab, autoindentOnEnter]),
    drawSelection(),
    syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
    bracketMatching(),
    foldGutter(),
    lintGutter(),
    highlightSelectionMatches(),
    () => (module.value.root ? ensoSyntax(toRef(module.value, 'root')) : []),
    ensoHoverTooltip(graph, vueHost),
    () => (editorRoot.value ? highlightStyle(editorRoot.value.highlightClasses) : []),
  ],
  vueHost: () => vueHost,
  lineMode: 'multi',
})
;(window as any).__codeEditorApi = testSupport(editorView)
useAutoBlur(editorView.dom)
const { updateListener, connectModuleListener } = useEnsoSourceSync(project, module, editorView)
const ensoDiagnostics = useEnsoDiagnostics(project, module, graph, editorView)
setExtraExtensions([updateListener, ensoDiagnostics])
connectModuleListener()

onMounted(() => {
  editorView.focus()
})
</script>

<template>
  <CodeMirrorRoot ref="editorRoot" class="CodeEditor" @keydown.tab.stop.prevent>
    <VueHostRender :host="vueHost" />
  </CodeMirrorRoot>
</template>

<!--suppress CssUnusedSymbol -->
<style scoped>
.CodeEditor {
  height: 100%;
}

:deep(.cm-scroller) {
  /*noinspection CssNoGenericFontName*/
  font-family: var(--font-mono);
}

:deep(.cm-editor) {
  backdrop-filter: var(--blur-app-bg);
  background-color: rgba(255, 255, 255, 0.9);
  box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);
  border: 1px solid rgba(255, 255, 255, 0.4);

  opacity: 1;
  color: black;
  text-shadow: 0 0 2px rgba(255, 255, 255, 0.4);
  font-size: 12px;
  outline: 1px solid transparent;
  transition: outline 0.1s ease-in-out;
  &:deep(.cm-focused) {
    outline: 1px solid rgba(0, 0, 0, 0.5);
  }
}

:deep(.cm-tooltip-hover) {
  padding: 4px;
  border-radius: 4px;
  border: 1px solid rgba(0, 0, 0, 0.4);
  text-shadow: 0 0 2px rgba(255, 255, 255, 0.4);

  &::before {
    content: '';
    background-color: rgba(255, 255, 255, 0.9);
    backdrop-filter: blur(64px);
    border-radius: 4px;
  }
}

:deep(.cm-gutters) {
  border-radius: 3px 0 0 3px;
  min-width: 32px;
}
</style>
