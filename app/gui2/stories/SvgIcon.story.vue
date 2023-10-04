<script setup lang="ts">
import { logEvent } from 'histoire/client'
import { ref } from 'vue'

import SvgIcon from '@/components/SvgIcon.vue'

import iconList from '@/util/iconList.json'

const name = ref('enso_logo')
</script>

<template>
  <Story title="SVG Icon" group="misc" :layout="{ type: 'grid', width: 500 }" auto-props-disabled>
    <SvgIcon :name="name" @click="logEvent('click', [])" />

    <template #controls>
      <HstSelect v-model="name" :options="iconList" />
    </template>
  </Story>
</template>
