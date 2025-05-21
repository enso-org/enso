<script setup lang="ts">
import { useTooltipRegistry, type TooltipDisplayStrategy } from '@/providers/tooltipRegistry'
import { usePropagateScopesToAllRoots } from '@/util/patching'
import { toRef } from 'vue'

const { when = 'always' } = defineProps<{ when?: TooltipDisplayStrategy }>()

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
    registered.onTargetEnter(e.target, () => when)
  }
}

function onLeave(e: PointerEvent) {
  if (e.target instanceof HTMLElement && tooltipSlot.value != null) {
    registered.onTargetLeave(e.target)
  }
}

defineExpose({
  hideTooltip() {
    registered.forceHide()
  },
})
</script>

<template>
  <slot v-bind="{ ...$attrs }" @pointerenter="onEnter" @pointerleave="onLeave" />
</template>
