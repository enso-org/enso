<script setup lang="ts">
import { logEvent } from 'histoire/client'
import { ref } from 'vue'

import TopBar from '@/components/TopBar.vue'

const title = ref('Mock Project')
const recordMode = ref(true)
const breadcrumbs = ref(['main', 'ad_analytics'])
</script>

<template>
  <Story title="Top Bar" group="graph" :layout="{ type: 'grid', width: 500 }" autoPropsDisabled>
    <div style="height: 48px">
      <TopBar
        :title="title"
        :recordMode="recordMode"
        :breadcrumbs="breadcrumbs"
        @back="logEvent('back', [])"
        @forward="logEvent('forward', [])"
        @recordOnce="logEvent('recordOnce', [])"
        @breadcrumbClick="logEvent('breadcrumbClick', [$event])"
        @update:recordMode="logEvent('update:recordMode', [$event]), (recordMode = $event)"
      />
    </div>

    <template #controls>
      <HstText v-model="title" title="title" />
      <HstJson v-model="recordMode" title="recordMode" />
      <HstJson v-model="breadcrumbs" title="breadcrumbs" />
    </template>
  </Story>
</template>
