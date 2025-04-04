<script setup lang="ts" generic="P">
import { PropsWithChildren } from 'react'
import { applyPureReactInVue } from 'veaury'

const { reactComponent, layoutProps } = defineProps<{
  reactComponent: (props: PropsWithChildren<P>) => JSX.Element | undefined
  layoutProps?: P
}>()
const vueLayout = applyPureReactInVue(reactComponent)
</script>

<template>
  <component :is="vueLayout" v-bind="layoutProps">
    <RouterView />
  </component>
</template>
