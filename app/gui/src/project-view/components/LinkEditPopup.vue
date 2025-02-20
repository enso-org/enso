<script setup lang="ts">
import { textEditorsBindings } from '@/bindings'
import { autoUpdate, flip, useFloating } from '@floating-ui/vue'
import { toRef, useTemplateRef } from 'vue'

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
</script>

<template>
  <div ref="floating" class="LinkEditPopup" :style="floatingStyles" @pointerdown.stop.prevent>
    <a class="link" :href="href" target="_blank" rel="noopener,noreferrer">Follow link</a> ({{
      textEditorsBindings.bindings.openLink.humanReadable
    }})
  </div>
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
