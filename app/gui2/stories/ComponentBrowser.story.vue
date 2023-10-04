<script setup lang="ts">
import { logEvent } from 'histoire/client'

import ComponentBrowser from '@/components/ComponentBrowser.vue'
import { Vec2 } from '@/util/vec2'
import NavigatorWrapper from './NavigatorWrapper.vue'

const position = new Vec2(-329.5, 0)
</script>

<template>
  <Story
    title="Component Browser"
    group="graph"
    :layout="{ type: 'single', iframe: false }"
    responsive-disabled
    auto-props-disabled
  >
    <NavigatorWrapper style="height: 100%">
      <template #default="{ navigator }">
        <ComponentBrowser
          :position="position"
          :navigator="navigator"
          @finished="logEvent('finished', [])"
        />
      </template>
    </NavigatorWrapper>
  </Story>
</template>
