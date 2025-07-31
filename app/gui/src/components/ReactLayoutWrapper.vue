<script setup lang="ts" generic="P">
import { reactComponent as vueReactComponent } from '@/util/react'
import type { PropsWithChildren } from 'react'
import { computed } from 'vue'

const { reactComponent, layoutProps } = defineProps<{
  reactComponent: (props: PropsWithChildren<P>) => JSX.Element | undefined
  layoutProps?: P
}>()

const vueLayout = computed(() => vueReactComponent(reactComponent) as any)
</script>

<template>
  <component :is="vueLayout" v-bind="layoutProps">
    <RouterView />
  </component>
</template>
