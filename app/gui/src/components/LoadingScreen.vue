<script setup lang="ts">
import { useText } from '$/providers/text'
import GrowingSpinner from '@/components/shared/GrowingSpinner.vue'
import { computed } from 'vue'

const SPINNER_SIZE_PX = 64

const text = useText()
const message = computed(() => text.getText('loadingAppMessage'))
</script>

<template>
  <div class="grid h-screen w-screen place-items-center text-primary" data-testid="loading-screen">
    <div className="flex flex-col items-center gap-8 text-center">
      <GrowingSpinner phase="loading-fast" :size="SPINNER_SIZE_PX" />
      <h1 class="message" v-text="message" />
    </div>
  </div>
</template>

<style scoped>
.message {
  line-height: 29px;
  font-weight: 700;
  font-size: 19px;
}
</style>
