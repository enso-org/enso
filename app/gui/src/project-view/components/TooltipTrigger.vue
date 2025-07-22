<script setup lang="ts">
import { useTooltipRegistry, type TooltipDisplayStrategy } from '@/providers/tooltipRegistry'
import { usePropagateScopesToAllRoots } from '@/util/patching'
import { Placement } from '@floating-ui/vue'
import { toRef } from 'vue'

const {
  placement = 'top',
  when = 'always',
  showOnClick = false,
} = defineProps<{
  placement?: Placement
  when?: TooltipDisplayStrategy
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
    registered.onTargetEnter(e.target, { placement: () => placement, when: () => when })
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
