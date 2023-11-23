<script setup lang="ts">
import { logEvent } from 'histoire/client'
import { ref } from 'vue'

import TopBar from '@/components/TopBar.vue'

const title = ref('Mock Project')
const modes = ref(['design', 'live'])
const mode = ref('live')
const breadcrumbs = ref(['main', 'ad_analytics'])
</script>

<template>
  <Story title="Top Bar" group="graph" :layout="{ type: 'grid', width: 500 }" autoPropsDisabled>
    <div style="height: 48px">
      <TopBar
        :title="title"
        :mode="mode"
        :modes="modes"
        :breadcrumbs="breadcrumbs"
        @back="logEvent('back', [])"
        @forward="logEvent('forward', [])"
        @execute="logEvent('execute', [])"
        @breadcrumbClick="logEvent('breadcrumbClick', [$event])"
        @update:mode="logEvent('update:mode', [$event]), (mode = $event)"
      />
    </div>

    <template #controls>
      <HstText v-model="title" title="title" />
      <HstSelect v-model="mode" title="mode" :options="modes" />
      <HstJson v-model="modes" title="modes" />
      <HstJson v-model="breadcrumbs" title="breadcrumbs" />
    </template>
  </Story>
</template>
