<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import type { QualifiedImport } from '$/providers/openedProjects/module/imports'
import SvgButton from '@/components/SvgButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import type { Icon } from '@/util/iconMetadata/iconName'
import { ProjectPath } from '@/util/projectPath'
import { Ok } from 'enso-common/src/utilities/data/result'

const { projectNames: names, module } = useCurrentProject()

const props = defineProps<{
  message: string
  type: MessageType
}>()

function containsLibraryName(): ProjectPath | null {
  const prefix = 'Compile error: Fully qualified name references a library '
  if (props.message.startsWith(prefix)) {
    const rest = props.message.substring(prefix.length)
    const libName = rest.split(' ')
    if (!libName[0]) return null
    const path = names.value.parseProjectPathRaw(libName[0])
    if (!path.ok) return null
    return path.value
  } else {
    return null
  }
}
function copyText() {
  window.navigator.clipboard.writeText(props.message)
}
function fixImport() {
  const libName = containsLibraryName()
  if (libName) {
    const theImport = {
      kind: 'Qualified',
      module: libName,
    } satisfies QualifiedImport
    module.value.edit((edit) => {
      module.value.addMissingImports(edit, [theImport])
      return Ok()
    })
  }
}
</script>

<script lang="ts">
/** The type of a message. */
export type MessageType = 'error' | 'warning' | 'missing' | 'panic'
export const iconForMessageType: Record<MessageType, Icon> = {
  error: 'error',
  warning: 'warning',
  missing: 'metadata',
  panic: 'panic',
}

export const colorForMessageType: Record<MessageType, string> = {
  error: 'var(--color-error)',
  warning: 'var(--color-warning)',
  missing: 'var(--color-missing-value)',
  panic: 'var(--color-error)',
}
</script>

<template>
  <div class="GraphNodeMessage" :style="{ '--background-color': colorForMessageType[props.type] }">
    <SvgIcon class="icon" :name="iconForMessageType[props.type]" />
    <div class="message" v-text="props.message"></div>
    <div class="toolbar">
      <SvgButton
        v-if="containsLibraryName()"
        name="edit"
        class="fixImportButton"
        title="Fix Import"
        @activate="fixImport"
      />
      <SvgButton
        v-if="!containsLibraryName()"
        name="copy2"
        class="copyButton"
        title="Copy message text"
        @activate="copyText"
      />
    </div>
  </div>
</template>

<style scoped>
.GraphNodeMessage {
  --horizontal-padding: 8px;
  display: flex;
  height: 24px;
  padding: 0 var(--horizontal-padding);
  align-items: flex-start;
  gap: 6px;
  font-weight: 800;
  white-space: nowrap;
  border-radius: var(--radius-full);
  color: var(--color-text-inversed);
  background-color: var(--background-color);
  line-height: 20px;
  z-index: -1;
  pointer-events: none;
  opacity: 1;
  transition: opacity 0.2s ease;
}

.icon {
  margin: auto 0;
}

.message {
  margin-top: 1px;
}

.toolbar {
  padding: 4px;
  margin-right: calc(0px - var(--horizontal-padding));
  border-radius: var(--radius-full);
  position: relative;
  z-index: 1;
  pointer-events: auto;

  & > .SvgButton:hover {
    background-color: color-mix(in oklab, black, transparent 90%);
    color: color-mix(in oklab, var(--color-text-inversed), transparent 20%);
  }

  & > .SvgButton:active {
    background-color: color-mix(in oklab, black, transparent 70%);
    color: var(--color-text-inversed);
  }
}
</style>
