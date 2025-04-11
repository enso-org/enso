<script setup lang="ts" generic="P">
import { PropsWithChildren } from 'react'
import { applyPureReactInVue } from 'veaury'
import { computed } from 'vue'

const { reactComponent, layoutProps } = defineProps<{
  reactComponent: (props: PropsWithChildren<P>) => JSX.Element | undefined
  layoutProps?: P
}>()

const vueLayout = computed(() => applyPureReactInVue(reactComponent))
</script>

<template>
  <component :is="vueLayout" v-bind="layoutProps">
    <RouterView />
  </component>
</template>
