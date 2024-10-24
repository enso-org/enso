<script setup lang="ts">
import { graphBindings } from '@/bindings'
import ColorRing from '@/components/ColorRing.vue'
import DropdownMenu from '@/components/DropdownMenu.vue'
import MenuButton from '@/components/MenuButton.vue'
import SvgButton from '@/components/SvgButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { ref, watch } from 'vue'

const nodeColor = defineModel<string | undefined>('nodeColor')
const props = defineProps<{
  isRecordingEnabledGlobally: boolean
  isRecordingOverridden: boolean
  isVisualizationEnabled: boolean
  isFullMenuVisible: boolean
  isRemovable: boolean
  matchableNodeColors: Set<string>
  documentationUrl: string | undefined
}>()
const emit = defineEmits<{
  'update:isRecordingOverridden': [isRecordingOverridden: boolean]
  'update:isVisualizationEnabled': [isVisualizationEnabled: boolean]
  startEditing: []
  startEditingComment: []
  openFullMenu: []
  delete: []
  createNewNode: []
}>()

const isDropdownOpened = ref(false)
const showColorPicker = ref(false)
const isVisualizationEnabled = ref(props.isVisualizationEnabled)
watch(isVisualizationEnabled, (v) => emit('update:isVisualizationEnabled', v))

function closeDropdown() {
  isDropdownOpened.value = false
}

function openDocs(url: string) {
  window.open(url, '_blank')
}

function readableBinding(binding: keyof (typeof graphBindings)['bindings']) {
  return graphBindings.bindings[binding].humanReadable
}
</script>

<template>
  <div
    class="CircularMenu"
    :class="{
      menu: !showColorPicker,
      openedDropdown: isDropdownOpened,
    }"
  >
    <template v-if="!showColorPicker">
      <SvgButton
        name="eye"
        class="slot1"
        title="Visualization"
        @click.stop="isVisualizationEnabled = !isVisualizationEnabled"
      />
      <SvgButton
        name="edit"
        class="slot2"
        title="Code Edit"
        data-testid="edit-button"
        @click.stop="emit('startEditing')"
      />
      <DropdownMenu
        v-model:open="isDropdownOpened"
        placement="bottom-start"
        title="More"
        class="slot3 More"
      >
        <template #button><SvgIcon name="3_dot_menu" class="moreIcon" /></template>
        <template #entries>
          <MenuButton
            :modelValue="props.isVisualizationEnabled"
            @update:modelValue="emit('update:isVisualizationEnabled', $event)"
            @click.stop="closeDropdown"
          >
            <SvgIcon name="eye" class="rowIcon" />
            <span v-text="`${props.isVisualizationEnabled ? 'Hide' : 'Show'} Visualization`"></span>
          </MenuButton>
          <MenuButton
            v-if="documentationUrl"
            @click.stop="closeDropdown(), openDocs(documentationUrl)"
          >
            <SvgIcon name="help" class="rowIcon" />
            <span>Help</span>
            <span class="shortcutHint" v-text="`${readableBinding('openDocumentation')}`"></span>
          </MenuButton>
          <MenuButton @click.stop="closeDropdown(), emit('createNewNode')">
            <SvgIcon name="add" class="rowIcon" />
            <span>Add New Component</span>
          </MenuButton>
          <MenuButton @click.stop="closeDropdown(), emit('startEditingComment')">
            <SvgIcon name="comment" class="rowIcon" />
            <span>Add Comment</span>
          </MenuButton>
          <MenuButton @click.stop="closeDropdown(), (showColorPicker = true)">
            <SvgIcon name="paint_palette" class="rowIcon" />
            <span>Color Component</span>
          </MenuButton>
          <MenuButton @click.stop="closeDropdown(), emit('startEditing')">
            <SvgIcon name="edit" class="rowIcon" />
            <span>Code Edit</span>
          </MenuButton>
          <MenuButton
            data-testid="removeNode"
            :disabled="!isRemovable"
            @click.stop="closeDropdown(), emit('delete')"
          >
            <SvgIcon name="trash2" class="rowIcon" />
            <span>Remove Component</span>
            <span class="shortcutHint" v-text="`${readableBinding('deleteSelected')}`"></span>
          </MenuButton>
        </template>
      </DropdownMenu>
    </template>
    <ColorRing
      v-else
      v-model="nodeColor"
      :matchableColors="matchableNodeColors"
      :initialColorAngle="90"
      @close="showColorPicker = false"
    />
  </div>
</template>

<style scoped>
.CircularMenu {
  position: absolute;
  left: -36px;
  bottom: -36px;
  width: var(--outer-diameter);
  height: var(--outer-diameter);
  user-select: none;
  pointer-events: none;
  /* This is a variable so that it can be referenced in computations,
     but currently it can't be changed due to many hard-coded values below. */
  --outer-diameter: 104px;
  /* It would be preferred to use var(--color-app-bg) and var(--blur-app-bg) here, 
     but for some reason the dropdown is ignoring backdrop-filter, 
     and does not match circular menu in color.*/
  --dropdown-opened-background: white;
  --dropdown-opened-backdrop-filter: none;
}

:deep(.DropdownMenuContent) {
  width: 210px;
  margin-top: 2px;
  padding: 4px;
  background: var(--dropdown-opened-background);
  backdrop-filter: var(--dropdown-opened-backdrop-filter);

  > * {
    display: flex;
    align-items: center;
    justify-content: left;
    padding-left: 8px;
    padding-right: 8px;
  }
}

.rowIcon {
  display: inline-block;
  margin-right: 8px;
}

.shortcutHint {
  margin-left: auto;
  opacity: 0.8;
}

.menu {
  > * {
    pointer-events: all;
  }

  &:before {
    content: '';
    position: absolute;
    backdrop-filter: var(--blur-app-bg);
    background: var(--color-app-bg);
    width: 100%;
    height: 100%;
    pointer-events: all;
    top: 36px;
    transition: all ease 0.1s;
    clip-path: path(
      'M0,16 V16 A52,52,0,0,0,52,68 A16,16,0,0,0,52,36 A20,20,0,0,1,32,16 A16,16,0,0,0,0,16'
    );
  }
  &.openedDropdown:before {
    background: var(--dropdown-opened-background);
    backdrop-filter: var(--dropdown-opened-backdrop-filter);
    clip-path: path(
      'M0,16 V68 A52,52,0,0,0,52,68 A16,16,0,0,0,52,36 A20,20,0,0,1,32,16 A16,16,0,0,0,0,16'
    );
  }
}

:deep(.ColorRing .gradient) {
  clip-path: path(
    evenodd,
    'M0,52 A52,52 0,1,1 104,52 A52,52 0,1,1 0, 52 z m52,20 A20,20 0,1,1 52,32 20,20 0,1,1 52,72 z'
  );
}

/**
  * The following styles are used to position the icons in a circular pattern. The slots are named slot1 to slot3 and
  * are positioned using absolute positioning. The slots are positioned in a circle with slot1 at the bottom and the rest
  * of the slots are positioned in a clockwise direction along one quarter of the circle.
  * ```
  * slot3
  *      slot2
  *           slot1
  * ```
 */
.slot1 {
  position: absolute;
  left: 44px;
  top: 80px;
}

.slot2 {
  position: absolute;
  top: 69.46px;
  left: 18.54px;
}

.slot3 {
  position: absolute;
  top: 44px;
  left: 8px;
}
</style>
