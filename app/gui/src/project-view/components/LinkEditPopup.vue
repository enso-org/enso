<script setup lang="ts">
import { useNavigateLink } from '$/utils/links'
import { textEditorsBindings } from '@/bindings'
import { autoUpdate, flip, useFloating } from '@floating-ui/vue'
import { ref, toRef, useTemplateRef } from 'vue'

const props = defineProps<{
  referenceElement: HTMLElement
  href: string
  popOut: boolean
}>()

const navigateLink = useNavigateLink()
const navigating = ref(false)

function onClick() {
  navigating.value = true
  navigateLink(props.href).finally(() => (navigating.value = false))
}

const floatingElement = useTemplateRef<HTMLElement>('floating')

const { floatingStyles } = useFloating(toRef(props, 'referenceElement'), floatingElement, {
  placement: 'top-start',
  strategy: () => (props.popOut ? 'fixed' : 'absolute'),
  middleware: [flip()],
  whileElementsMounted: autoUpdate,
})
</script>

<template>
  <teleport to="#floatingLayer">
    <div ref="floating" class="LinkEditPopup" :style="floatingStyles" @pointerdown.stop.prevent>
      <a v-if="!navigating" class="link" @click="onClick">Follow link</a>
      ({{ textEditorsBindings.bindings.openLink.humanReadable }})
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

.modal {
  position: absolute;
  top: 0;
  left: 0;
  width: 100vw;
  height: 100vh;
  cursor: pointer;
  max-height: 100vh;

  &::before {
    content: '';
    position: absolute;
    inset: 0;
    top: -100px;
    height: calc(100% + 200px);
    background: var(--color-dim);
  }

  > .modal-container {
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    background-color: var(--color-app-bg);
    border-radius: var(--radius-default);
    backdrop-filter: blur(8px);
    cursor: default;
    padding: 1em;
    display: flex;
    flex-flow: column nowrap;
    gap: 1em;
    max-width: 32em;
  }
}

.button-bar {
  display: flex;
  gap: 1em;
  justify-content: flex-end;
}
</style>
