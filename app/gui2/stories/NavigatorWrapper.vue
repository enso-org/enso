<script setup lang="ts">
/// <reference types="@histoire/plugin-vue/components" />
import { ref } from 'vue'

import { useNavigator } from '@/util/navigator'

const viewportNode = ref<HTMLElement>()

const navigator = useNavigator(viewportNode)
</script>

<template>
  <div ref="viewportNode" class="NavigatorWrapper">
    <slot :navigator="navigator"></slot>
  </div>
</template>
