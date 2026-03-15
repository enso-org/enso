<script setup lang="ts">
import { useContainerData } from '$/providers/container'
import { useResizeHandles } from '@/components/resizeHandles'
import ResizeHandles from '@/components/ResizeHandles.vue'
import SizeTransition from '@/components/SizeTransition.vue'
import SvgButton from '@/components/SvgButton.vue'
import { useResizeObserver } from '@/composables/events'
import { computed, ref, toRef, useTemplateRef } from 'vue'
import { Drive } from './reactTabs'

const DEFAULT_WIDTH_PX = 600

const props = defineProps<{ middlePanelShown: boolean }>()

const containerData = useContainerData()
const width = toRef(containerData, 'leftPanelWidth')
const visible = ref(true)

const root = useTemplateRef('content')
const cssClass = computed(() => ({
  focusedPanel: containerData.focusedPanel.type === 'drive',
}))
const widthStyle = computed(() =>
  visible.value && props.middlePanelShown ?
    { width: `${width.value ?? DEFAULT_WIDTH_PX}px` }
  : { width: '100%' },
)

const resizeHandles = useResizeHandles({
  size: useResizeObserver(root),
})
resizeHandles.onResizeWidth((value) => (width.value = value))
</script>

<template>
  <div class="LeftPanel">
    <SizeTransition width :duration="250">
      <div
        v-if="visible || !middlePanelShown"
        ref="content"
        class="panel"
        :class="cssClass"
        :style="widthStyle"
      >
        <Drive />
        <ResizeHandles v-if="middlePanelShown" right v-on="resizeHandles.events" />
      </div>
    </SizeTransition>
    <div class="shadow" />
    <SvgButton
      v-model="visible"
      class="toggleVisibilityButton"
      name="right_side_panel"
      title="Toggle Drive Panel"
      :disabled="!middlePanelShown"
    />
  </div>
</template>

<style scoped>
.LeftPanel {
  position: relative;
  flex-shrink: 1;
  flex-grow: 1;
  min-width: 48px;
  height: 100%;
  z-index: 1;
}

.panel {
  position: relative;
  height: 100%;
  width: 100%;
  min-width: 200px;
}

.shadow {
  position: absolute;
  top: 0;
  right: 0;
  width: 100%;
  height: 100%;
  box-shadow:
    0.5px 2.2px 0px rgb(0 0 0 / 0.84%),
    0 1.2px 5.65px 0px rgb(0 0 0 / 1.21%),
    0 2.25px 10.64px 0 rgb(0 0 0 / 1.5%),
    0 4px 19px 0 rgb(0 0 0 / 1.79%),
    0 7.5px 35.5px 0 rgb(0 0 0 / 2.16%),
    0 18px 85px 0 rgb(0 0 0 / 3%);
  z-index: -1;
}

.toggleVisibilityButton {
  position: absolute;
  left: 16px;
  top: 20px;
}
</style>
