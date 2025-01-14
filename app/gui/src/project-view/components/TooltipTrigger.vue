<script setup lang="ts">
import { useTooltipRegistry } from '@/providers/tooltipRegistry'
import { usePropagateScopesToAllRoots } from '@/util/patching'
import { toRef } from 'vue'

usePropagateScopesToAllRoots()

const registry = useTooltipRegistry()
const slots = defineSlots<{
  default(props: any): any
  tooltip(): any
}>()

const tooltipSlot = toRef(slots, 'tooltip')
const registered = registry.registerTooltip(tooltipSlot)
function onEnter(e: PointerEvent) {
  if (e.target instanceof HTMLElement) {
    registered.onTargetEnter(e.target)
  }
}

function onLeave(e: PointerEvent) {
  if (e.target instanceof HTMLElement) {
    registered.onTargetLeave(e.target)
  }
}
</script>

<template>
  <slot v-bind="{ ...$attrs }" @pointerenter="onEnter" @pointerleave="onLeave" />
</template>
