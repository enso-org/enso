<script setup lang="ts">
/// <reference types="@histoire/plugin-vue/components" />
import { logEvent } from 'histoire/client'

import ComponentBrowser from '@/components/ComponentBrowser.vue'
import { Vec2 } from '@/util/vec2'
import NavigatorWrapper from './NavigatorWrapper.vue'

const position = new Vec2(-329.5, 0)
</script>

<template>
  <Story title="Component Browser" group="graph" responsive-disabled auto-props-disabled>
    <NavigatorWrapper>
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

<style>
.NavigatorWrapper {
  height: 100vh;
}
</style>
