<script lang="ts" setup>
/**
 * @file A Project view in Tab react component
 *
 * Currently the ProjectView component is displayed in tab component written in react; when the tab
 * switches, we want to keep the PV instance while detaching it from DOM. In pure Vue environment it
 * would be as simple as adding `KeepAlive` in tab panel, but here we must "simulate" it by reading
 * `hidden` property.
 */
import ProjectView from '@/ProjectView.vue'
import { ComponentProps } from 'vue-component-type-helpers'

const _props = defineProps<{
  hidden: boolean
  projectViewProps: ComponentProps<typeof ProjectView>
}>()
</script>

<template>
  <KeepAlive>
    <ProjectView v-if="!hidden" v-bind="projectViewProps" />
  </KeepAlive>
</template>
