<script setup lang="ts">
import { logEvent } from 'histoire/client'

import ComponentBrowser from '@/components/ComponentBrowser.vue'
import { Usage } from '@/components/ComponentBrowser/input'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { Vec2 } from '@/util/data/vec2'
import GroupColorsWrapper from './GroupColorsWrapper.vue'
import NavigatorWrapper from './NavigatorWrapper.vue'

const position = new Vec2(-329.5, 0)
const usage: Usage = { type: 'newNode' }
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
            :nodePosition="position"
            :navigator="navigator"
            :usage="usage"
            :sourcePort="null"
            :initialCaretPosition="[0, 0]"
            :closeIfClicked="() => false"
            @finished="logEvent('finished', [])"
          />
        </GroupColorsWrapper>
      </template>
    </NavigatorWrapper>
  </Story>
</template>
