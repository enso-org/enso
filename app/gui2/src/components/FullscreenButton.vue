<script setup lang="ts">
/** @file A button for toggling the fullscreen state of an element. */

import SvgButton from '@/components/SvgButton.vue'

const fullscreen = defineModel<boolean>({ required: true })
</script>

<template>
  <SvgButton
    :name="fullscreen ? 'exit_fullscreen' : 'fullscreen'"
    :title="fullscreen ? 'Exit Fullscreen' : 'Fullscreen'"
    @click.stop="fullscreen = !fullscreen"
  />
</template>
