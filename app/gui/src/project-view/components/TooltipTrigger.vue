<script setup lang="ts">
import { useTooltipRegistry } from '@/providers/tooltipRegistry'
import { usePropagateScopesToAllRoots } from '@/util/patching'
import type { Placement } from '@floating-ui/vue'
import type { VueInstance } from '@vueuse/core'
import { toRef } from 'vue'
import type { Opt } from '@/util/data/opt'

const {
  placement = 'top',
  whenOverflow = undefined,
  showOnClick = false,
} = defineProps<{
  placement?: Placement
  /**
   * If set, the tooltip is inhibited unless the reference element is overflowing. If `true`, the
   * element in the default slot is the reference element; if an element is explicitly provided, it
   * will be used as the reference element.
   */
  whenOverflow?: Opt<true | HTMLElement | VueInstance>
  showOnClick?: boolean
}>()

usePropagateScopesToAllRoots()

const registry = useTooltipRegistry()
const slots = defineSlots<{
  default(props: any): any
  tooltip(): any
}>()

const tooltipSlot = toRef(slots, 'tooltip')
const registered = registry.registerTooltip(tooltipSlot)
function onEnter(e: PointerEvent) {
  if (e.target instanceof HTMLElement && tooltipSlot.value != null) {
    registered.onTargetEnter(e.target, { placement: () => placement, whenOverflow: () => whenOverflow })
  }
}

function onLeave(e: PointerEvent) {
  if (e.target instanceof HTMLElement && tooltipSlot.value != null) {
    registered.onTargetLeave(e.target)
  }
}

function onClick(e: MouseEvent) {
  if (showOnClick && e.target instanceof HTMLElement && tooltipSlot.value != null) {
    registered.forceShow(e.target)
  }
}

defineExpose({
  hideTooltip() {
    registered.forceHide()
  },
})
</script>

<template>
  <slot v-bind="{ ...$attrs }" @pointerenter="onEnter" @pointerleave="onLeave" @click="onClick" />
</template>
