<script setup lang="ts">
import { injectActionContext } from '@/providers/actionContext'

const actionContext = injectActionContext()

defineExpose({
  close() {
    actionContext.openPosition = null
  },
})
</script>

<template>
  <slot />
</template>
