<script setup lang="ts">
import { logEvent } from 'histoire/client'

import ComponentBrowser from '@/components/ComponentBrowser.vue'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { Vec2 } from '@/util/vec2'
import GroupColorsWrapper from './GroupColorsWrapper.vue'
import NavigatorWrapper from './NavigatorWrapper.vue'

const position = new Vec2(-329.5, 0)
</script>

<template>
  <Story
    title="Component Browser"
    group="graph"
    :layout="{ type: 'single', iframe: false }"
    :setupApp="
      () => {
        useSuggestionDbStore()
      }
    "
    responsiveDisabled
    autoPropsDisabled
  >
    <NavigatorWrapper style="height: 100%">
      <template #default="{ navigator }">
        <GroupColorsWrapper>
          <ComponentBrowser
            :position="position"
            :navigator="navigator"
            @finished="logEvent('finished', [])"
          />
        </GroupColorsWrapper>
      </template>
    </NavigatorWrapper>
  </Story>
</template>
