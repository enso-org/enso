<script setup lang="ts">
import { textEditorsBindings } from '@/bindings'
import { autoUpdate, flip, useFloating } from '@floating-ui/vue'
import { computed, toRef, useTemplateRef } from 'vue'

const props = defineProps<{
  referenceElement: HTMLElement
  href: string
  popOut: boolean
}>()

const floatingElement = useTemplateRef<HTMLElement>('floating')

const { floatingStyles } = useFloating(toRef(props, 'referenceElement'), floatingElement, {
  placement: 'top-start',
  strategy: () => (props.popOut ? 'fixed' : 'absolute'),
  middleware: [flip()],
  whileElementsMounted: autoUpdate,
})

function showOpenProjectModal() {
}

</script>

<template>
  <teleport to="#floatingLayer">
    <div ref="floating" class="LinkEditPopup" :style="floatingStyles" @pointerdown.stop.prevent>
      <a
        v-if="href.startsWith('enso:')"
        class="link"
        :href="href"
        target="_blank"
        rel="noopener,noreferrer"
        >Follow link</a
      >
      <a
        v-else
        class="link"
        @click="showOpenProjectModal"
        >Follow link</a
      >
      ({{ textEditorsBindings.bindings.openLink.humanReadable }})
    </div>
    </div>
  </teleport>
</template>

<style scoped>
.LinkEditPopup {
  font-family: var(--font-sans);
  color: gray;
  border-radius: var(--radius-default);
  backdrop-filter: var(--blur-app-bg);
  background-color: rgba(255, 255, 255, 0.9);
  box-shadow: 0 4px 30px rgba(0, 0, 0, 0.2);
  padding: 8px;
  width: max-content;
}

.link {
  cursor: pointer;
  color: blue;
  &:hover {
    text-decoration: underline;
  }
}
</style>
