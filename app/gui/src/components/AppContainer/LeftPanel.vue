<script setup lang="ts">
import HelpBar from '$/components/AppContainer/HelpBar.vue'
import { Drive } from '$/components/AppContainer/reactTabs'
import { useAuth } from '$/providers/auth'
import { useBackends } from '$/providers/backends'
import { categoryKey, useCategories } from '$/providers/category'
import { useContainerData } from '$/providers/container'
import { useDriveLocation } from '$/providers/drive'
import { useIsOnline } from '$/providers/online'
import { useText } from '$/providers/text'
import { optPx } from '$/utils/dom'
import ActionMenu from '@/components/ActionMenu.vue'
import DropdownMenu from '@/components/DropdownMenu.vue'
import { useResizeHandles } from '@/components/resizeHandles'
import ResizeHandles from '@/components/ResizeHandles.vue'
import SizeTransition from '@/components/SizeTransition.vue'
import SvgButton from '@/components/SvgButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { useResizeObserver } from '@/composables/events'
import type { DisplayableActionName } from '@/providers/action'
import { Path } from 'enso-common/src/services/Backend'
import { computed, ref, toRef, toRefs, useTemplateRef } from 'vue'
import CategoryButton from './CategoryButton.vue'

const LEFT_BAR_EXTENSION_TIME_MS = 550
const ENSO_ICON_MENU_ACTIONS: DisplayableActionName[] = [
  'help.whatsNew',
  'help.community',
  'help.gettingStarted',
  'help.askAQuestion',
  'help.componentExamples',
  'help.exampleWorkflows',
  'help.docs',
  'help.contactUs',
]

const props = defineProps<{ middlePanelShown: boolean }>()

const categories = useCategories()
const containerData = useContainerData()
const auth = useAuth()
const { getText } = useText()
const isOnline = useIsOnline()
const { localBackend } = useBackends()
const { leftPanelWidth: width, leftPanelToggledOn: toggledOn } = toRefs(containerData)
const visible = computed(() => toggledOn.value || !props.middlePanelShown)
const currentCategory = toRef(useDriveLocation(), 'currentCategory')

const cloudDisabledReason = computed(() => {
  if (!isOnline.value) {
    return getText('unavailableOffline')
  } else if (!auth.session?.user.isEnabled) {
    return getText('notEnabledSubtitle')
  } else {
    return false
  }
})

const localDisabledReason = computed(() => {
  if (localBackend == null) {
    return getText('localBackendNotDetectedError')
  } else {
    return false
  }
})

const root = useTemplateRef('content')
const cssClass = computed(() => ({
  focusedPanel: containerData.focusedPanel.type === 'drive',
  middlePanelShown: props.middlePanelShown,
}))
const widthStyle = computed(() => (props.middlePanelShown ? { width: optPx(width.value) } : {}))
const canAddLocalDirectories = window.api != null

const resizeHandles = useResizeHandles({
  size: useResizeObserver(root),
})
resizeHandles.onResizeWidth((value) => (width.value = value))

const leftBarExtended = ref(false)
let leftBarExtensionTimeout: ReturnType<typeof setTimeout> | undefined

function onEnter() {
  if (!leftBarExtended.value) {
    leftBarExtensionTimeout = setTimeout(
      () => (leftBarExtended.value = true),
      LEFT_BAR_EXTENSION_TIME_MS,
    )
  }
}

function onLeave() {
  leftBarExtended.value = false
  clearTimeout(leftBarExtensionTimeout)
}

async function onAddDirectoryClick() {
  const [newDirectory] = (await window.api?.fileBrowser.openFileBrowser('directory')) ?? []

  if (newDirectory != null) {
    const path = Path(newDirectory)
    categories.addLocalDirectory(path)
    currentCategory.value = { type: 'localDirectory', path }
  }
}
</script>

<template>
  <div class="LeftPanel">
    <div
      class="leftBar"
      :class="{ expanded: leftBarExtended }"
      @mouseenter="onEnter"
      @mouseleave="onLeave"
    >
      <SvgButton
        v-model="toggledOn"
        class="leftBarIcon"
        name="right_side_panel"
        title="Toggle Drive Panel"
        :disabled="!middlePanelShown"
      />
      <div class="categories">
        <CategoryButton
          v-for="category of categories.cloudCategoriesList"
          :key="categoryKey(category)"
          :category="category"
          :extended="leftBarExtended"
          :disabled="cloudDisabledReason"
        />
      </div>
      <div class="categories">
        <CategoryButton
          v-for="category of categories.localCategoriesList"
          :key="categoryKey(category)"
          :category="category"
          :extended="leftBarExtended"
          :disabled="localDisabledReason"
        />
        <SvgButton
          v-if="canAddLocalDirectories"
          class="leftBarIcon"
          name="folder_add_small"
          :label="leftBarExtended ? 'Add Directory' : undefined"
          @click="onAddDirectoryClick"
        />
      </div>
      <div class="shadow" />
    </div>
    <SizeTransition width :duration="250">
      <div v-if="visible" class="sizeWrapper">
        <div ref="content" class="panel" :class="cssClass" :style="widthStyle">
          <HelpBar />
          <Drive />
          <ResizeHandles v-if="middlePanelShown" right v-on="resizeHandles.events" />
        </div>
      </div>
    </SizeTransition>
    <DropdownMenu class="ensoIcon">
      <template #button><SvgIcon name="enso_logo" /></template>
      <template #menu><ActionMenu :actions="ENSO_ICON_MENU_ACTIONS" /></template>
    </DropdownMenu>
    <div class="shadow" />
  </div>
</template>

<style scoped>
.LeftPanel {
  position: relative;
  min-width: 48px;
  z-index: 1;

  & > .shadow {
    top: var(--top-bar-height);
  }
}

.leftBar {
  --button-height: 26px;
  --color-menu-entry-selected-bg: rgb(255 255 255);
  --color-menu-entry-hover-bg: rgba(255 255 255 / 0.8);
  position: absolute;
  width: 48px;
  max-width: 48px;
  height: 100%;
  margin-top: var(--top-bar-height);
  padding: 16px 0;
  background-color: var(--color-dashboard-background);
  display: flex;
  flex-direction: column;
  align-items: start;
  gap: 8px;
  z-index: 2;

  transition: max-width 200ms ease-in-out;

  &.expanded {
    width: fit-content;
    max-width: 400px;
  }

  & > .shadow {
    top: 0px;
  }
}

.categories {
  width: 100%;
  display: flex;
  flex-direction: column;
  align-items: start;
  gap: 8px;
  overflow: hidden;

  &:before {
    content: '';
    width: calc(100% - 32px);
    height: 1px;
    margin: 0 16px;
    background-color: rgba(0 0 0 / 0.1);
  }
}

.leftBarIcon {
  margin: 0 12px;
}

.CategoryButton {
  margin: 4px 16px;
  width: calc(100% - 32px);
}

/* This element's visible width will be overwritten by the size transition, but the inner content's
 * will not, preventing content reflow. Content reflow is disruptive to the appearance of the transition, and can affect
 * the framerate drastically.
 */
.sizeWrapper {
  height: 100%;
  min-width: 0;
  width: 100%;
  background-color: var(--panel-background);
  padding-left: 48px;
}

.panel {
  position: relative;
  height: 100%;
  width: 100%;
  min-width: 200px;
  display: flex;
  flex-direction: column;
  gap: 0;

  &.middlePanelShown {
    /* Default width, overriden in style tag. */
    width: 600px;
    max-width: 60vw;
  }
}

.shadow {
  position: absolute;
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
  clip-path: polygon(0 0, 100vw 0, 100vw 100%, 0 100%);
  z-index: -1;
}

.ensoIcon {
  --icon-size: 32px;
  --arrow-offset: 12px;
  position: absolute;
  top: 8px;
  left: 8px;
  padding: 0;
  z-index: 1;
}
</style>
