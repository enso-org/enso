<script setup lang="ts">
import { ref } from 'vue'

import { useKeyboard } from '@/composables/keyboard'
import { useNavigator } from '@/composables/navigator'

const viewportNode = ref<HTMLElement>()

const keyboard = useKeyboard()
const navigator = useNavigator(viewportNode, keyboard)
</script>

<template>
  <div ref="viewportNode" class="NavigatorWrapper">
    <slot :navigator="navigator"></slot>
  </div>
</template>
