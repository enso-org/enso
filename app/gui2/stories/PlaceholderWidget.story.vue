<script setup lang="ts">
/// <reference types="@histoire/plugin-vue/components" />

import PlaceholderWidget from '@/components/widgets/PlaceholderWidget.vue'
</script>

<template>
  <Story title="Placeholder" group="widgets" :layout="{ type: 'grid', width: 200 }">
    <PlaceholderWidget />
  </Story>
</template>
