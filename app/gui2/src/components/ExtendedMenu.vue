<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { ref } from 'vue'

const isDropdownOpen = ref(false)

const props = defineProps<{
  zoomLevel: number
}>()
const emit = defineEmits<{ zoomIn: []; zoomOut: []; fitToAllClicked: [] }>()
</script>

<template>
  <div class="ExtendedMenu">
    <div class="moreIcon" @pointerdown="isDropdownOpen = !isDropdownOpen"></div>
    <Transition name="dropdown">
      <div v-show="isDropdownOpen" class="ExtendedMenuPane">
        <div class="row">
          <div class="zoomBar row">
            <div class="button">Zoom</div>
            <div class="zoomControl last">
              <div class="zoomButton minus" @pointerdown="emit('zoomOut')" />
              <span
                class="zoomScaleLabel"
                v-text="props.zoomLevel ? props.zoomLevel.toFixed(0) + '%' : '?'"
              ></span>
              <div class="zoomButton plus" @pointerdown="emit('zoomIn')" />
            </div>
          </div>
          <div class="divider"></div>
          <SvgIcon
            name="show_all"
            class="last showAllIcon"
            @pointerdown="emit('fitToAllClicked')"
          />
        </div>
      </div>
    </Transition>
  </div>
</template>

<style scoped>
.ExtendedMenu {
  display: flex;
  border-radius: var(--radius-full);
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
  place-items: center;
  gap: 12px;
  padding: 4px 10px 4px 8px;
  width: 32px;
  height: 32px;
  margin-left: auto;
  margin-right: 125px;
}

.ExtendedMenuPane {
  position: absolute;
  display: flex;
  left: -268px;
  width: 300px;
  top: 100%;
  margin-top: 6px;
  padding: 4px;
  border-radius: var(--radius-full);
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
}

.row {
  width: 100%;
  display: flex;
  gap: 4px;
  padding-left: 4px;
}

.last {
  margin-left: auto;
}

.divider {
  border-left: 1px solid var(--color-text);
  border-right: 1px solid var(--color-text);
  height: 17px;
  margin-top: 2px;
  margin-left: 10px;
  margin-right: 10px;
}

.zoomControl {
  display: flex;
  gap: 4px;
  align-items: center;
}

.showAllIcon {
  margin-right: 10px;
  margin-top: 2px;
}

.zoomScaleLabel {
  width: 4em;
  text-align: center;
}

.moreIcon {
  width: 32px;
  height: 32px;
  text-align: center;
  font-size: 24px;
  font-family: var(--font-code);
  position: absolute;
  right: -4px;
  top: 8px;
}

.moreIcon:before {
  content: '\2807';
}

.zoomButton {
  border: 2px solid;
  width: 15px;
  height: 15px;
  border-radius: var(--radius-full);
  position: relative;
  margin: 4px;
  display: inline-block;
  vertical-align: middle;
}

.zoomButton.plus:before,
.zoomButton.plus:after {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: var(--color-text);
}

.zoomButton.plus:before {
  width: 2px;
  margin: 2px auto;
}

.zoomButton.plus:after {
  margin: auto 2px;
  height: 2px;
}

.zoomButton.minus:before {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: var(--color-text);
  margin: auto 2px;
  height: 2px;
}
</style>
