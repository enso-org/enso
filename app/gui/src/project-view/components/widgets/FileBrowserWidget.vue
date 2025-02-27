<script lang="ts">
export default {
  name: 'FileBrowserWidget',
}
</script>

<script setup lang="ts">
import ContextMenuTrigger from '@/components/ContextMenuTrigger.vue'
import LoadingSpinner from '@/components/shared/LoadingSpinner.vue'
import SvgButton from '@/components/SvgButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import FileBrowserEntry from '@/components/widgets/FileBrowserWidget/FileBrowserEntry.vue'
import { Directory, useFileBrowserStack } from '@/components/widgets/FileBrowserWidget/paths'
import { useBackend } from '@/composables/backend'
import { Action } from '@/providers/action'
import { injectBackend } from '@/providers/backend'
import { assert } from '@/util/assert'
import type { ToValue } from '@/util/reactivity'
import { useToast } from '@/util/toast'
import type {
  DatalinkAsset,
  DirectoryAsset,
  DirectoryId,
  FileAsset,
} from 'enso-common/src/services/Backend'
import Backend, {
  assetIsDatalink,
  assetIsDirectory,
  assetIsFile,
} from 'enso-common/src/services/Backend'
import { computed, onMounted, reactive, ref, toRef, toValue, watch } from 'vue'

const props = withDefaults(
  defineProps<{
    writeMode?: boolean
    choosenPath?: string
  }>(),
  { writeMode: false, choosenPath: '' },
)

const emit = defineEmits<{
  pathAccepted: [path: string]
}>()

const { query, fetch, ensureQueryData, mutation } = useBackend('remote')
const { remote: backend } = injectBackend()

const errorToast = useToast.error()
const newDirPlaceholder = Symbol()

let nextKeyForNewDir = 0
/**
 * Override for `:key` attribute in content entries.
 *
 * When new directory is added, it receives new entry.id, but we want animations to treat them
 * as same element. Therefore we assign a number as a key to every new directory placeholder,
 * and keep them once the placeholder turns into actual entry.
 */
const keyOverride: Map<DirectoryId | symbol, number> = reactive(new Map())

const currentUser = query('usersMe', [])
const currentOrganization = query('getOrganization', [])

const {
  filenameInputContents,
  directoryStack,
  currentDirectory,
  currentFilePath,
  highlightedName,
  initializeStack,
  isDirectoryStackInitializing,
} = useFileBrowserStack(
  backend,
  toRef(props, 'choosenPath'),
  currentUser.data,
  toRef(props, 'writeMode'),
  (dir) => fetch('listDirectory', listDirectoryArgs(dir)),
)

// === Directory Contents ===

function listDirectoryArgs(params: ToValue<Directory | undefined>) {
  return computed<Parameters<Backend['listDirectory']> | undefined>(() => {
    const paramsValue = toValue(params)
    return paramsValue ?
        [
          {
            parentId: paramsValue.id,
            filterBy: null,
            labels: null,
            recentProjects: false,
          },
          paramsValue.title,
        ]
      : undefined
  })
}

const { isPending, isError, data, error } = query(
  'listDirectory',
  listDirectoryArgs(currentDirectory),
)
const compareTitle = (a: { title: string }, b: { title: string }) => a.title.localeCompare(b.title)
const directories = computed(
  () => data.value && data.value.filter((asset) => assetIsDirectory(asset)).sort(compareTitle),
)
const files = computed(
  () =>
    data.value &&
    data.value.filter((asset) => assetIsFile(asset) || assetIsDatalink(asset)).sort(compareTitle),
)
const isEmpty = computed(
  () => directories.value?.length === 0 && files.value?.length === 0 && editedAsset.value == null,
)

// === Prefetching ===

watch(directories, (directories) => {
  // Prefetch directories to avoid lag when the user navigates, but only if we don't already have stale data.
  // When the user opens a directory with stale data, it will refresh and the animation will show what files have
  // changed since they last viewed.
  for (const directory of directories ?? [])
    ensureQueryData('listDirectory', listDirectoryArgs(directory))
})

// === Interactivity ===

function enterDir(dir: DirectoryAsset) {
  directoryStack.value.push(dir)
}

function popTo(index: number) {
  directoryStack.value.splice(index + 1)
}

function chooseFile(file: FileAsset | DatalinkAsset) {
  filenameInputContents.value = file.title
  if (!props.writeMode) {
    acceptCurrentFile()
  }
}

function acceptCurrentFile() {
  if (currentFilePath.value) {
    emit('pathAccepted', currentFilePath.value)
  } else {
    return false
  }
}

const isBusy = computed(() => isDirectoryStackInitializing.value || isPending.value)

const anyError = computed(() =>
  isError.value ? error
  : currentUser.isError.value ? currentUser.error
  : currentOrganization.isError.value ? currentOrganization.error
  : undefined,
)

// === Creating and Renaming Directories ===

const editedAsset = ref<{
  asset?: Directory
  name: string
  state: 'editing' | 'pending' | 'just created'
  createdId?: DirectoryId
}>()

// Don't await invalidates, because we want `createDirectory` to return first, to fill
// `keyOverride` property before getting update from backend.
const createDir = mutation('createDirectory', { meta: { awaitInvalidates: false } })
const updateDir = mutation('updateDirectory')

function addNewDirectory() {
  assert(editedAsset.value == null)
  keyOverride.set(newDirPlaceholder, nextKeyForNewDir++)
  editedAsset.value = { name: 'New Folder', state: 'editing' }
}

function renameDirectory(dir: DirectoryAsset) {
  assert(editedAsset.value == null)
  editedAsset.value = { asset: dir, name: dir.title, state: 'editing' }
}

async function acceptName(name: string) {
  if (editedAsset.value?.state !== 'editing') {
    console.error('Accepting edited name without editing')
    return
  }
  const edited = editedAsset.value
  edited.name = name
  edited.state = 'pending'
  const parentId = currentDirectory.value?.id
  if (parentId == null) {
    console.error('Cannot rename directory without parentId')
    return
  }
  const action =
    edited.asset == null ? createDir.mutateAsync([{ title: edited.name, parentId }, false])
    : edited.asset.title != edited.name ?
      updateDir.mutateAsync([edited.asset.id, { title: edited.name }, edited.asset.title])
    : Promise.resolve(undefined)
  action.then(
    (result) => {
      assert(edited === editedAsset.value)
      // Editing existing asset does not require 'just created' state, because we await
      // invalidates there
      if (edited.asset == null && result != null) {
        edited.createdId = result.id
        edited.state = 'just created'
        const key = keyOverride.get(newDirPlaceholder)
        if (key != null) {
          keyOverride.set(result.id, key)
        }
      } else {
        editedAsset.value = undefined
      }
    },
    (error) => {
      const actionDescription = edited.asset == null ? 'create folder' : 'rename folder'
      errorToast.show(`Failed to ${actionDescription}: ${error}`)
      editedAsset.value = undefined
    },
  )
}

watch(
  directories,
  (dirs) => {
    // Finish editing once received an updated directory.
    if (dirs?.find((dir) => dir.id === editedAsset.value?.createdId)) {
      editedAsset.value = undefined
    }
  },
  { flush: 'sync' },
)
// Currently, the only way to "focus" on an element is by context menu.
const focusedDirectory = ref<DirectoryAsset>()
const renameAction: Action = {
  icon: 'edit',
  description: 'Rename directory',
  disabled: computed(() => focusedDirectory.value == null || editedAsset.value != null),
  action: () => focusedDirectory.value && renameDirectory(focusedDirectory.value),
}

// === Initialization ===

onMounted(() => {
  Promise.all([currentUser.promise.value, currentOrganization.promise.value]).then(
    ([user, organizaton]) => {
      initializeStack(user, organizaton)
    },
  )
})
</script>

<template>
  <div class="FileBrowserWidget">
    <div class="topBar">
      <div class="directoryStack">
        <TransitionGroup>
          <template v-for="(directory, index) in directoryStack" :key="directory.id ?? 'root'">
            <SvgIcon v-if="index > 0" name="arrow_right_head_only" />
            <div
              class="clickable"
              :class="{ nonInteractive: index === directoryStack.length - 1 }"
              @click.stop="popTo(index)"
              v-text="directory.title"
            ></div>
          </template>
        </TransitionGroup>
      </div>
      <SvgButton
        name="folder_add"
        title="Add New Folder"
        :disabled="editedAsset != null"
        @click.stop="addNewDirectory"
      />
    </div>

    <div v-if="anyError" class="centerContent contents">Error: {{ anyError }}</div>
    <div v-else-if="isBusy" class="centerContent contents"><LoadingSpinner /></div>
    <div v-else-if="isEmpty" class="centerContent contents">Directory is empty</div>
    <div v-else :key="currentDirectory?.id ?? 'root'" class="listing contents">
      <ContextMenuTrigger :actions="[renameAction]" @hidden="focusedDirectory = undefined">
        <TransitionGroup>
          <FileBrowserEntry
            v-if="editedAsset && editedAsset.asset == null"
            :key="keyOverride.get(newDirPlaceholder) ?? newDirPlaceholder"
            icon="folder"
            :title="editedAsset.name"
            :editingState="editedAsset.state"
            @nameAccepted="acceptName($event)"
          />
          <FileBrowserEntry
            v-for="entry in directories"
            :key="keyOverride.get(entry.id) ?? entry.id"
            icon="folder"
            :title="editedAsset?.asset?.id === entry.id ? editedAsset.name : entry.title"
            :editingState="editedAsset?.asset?.id === entry.id ? editedAsset.state : undefined"
            @click="enterDir(entry)"
            @nameAccepted="acceptName($event)"
            @contextmenu="focusedDirectory = entry"
          />
          <FileBrowserEntry
            v-for="entry in files"
            :key="entry.id"
            icon="text2"
            :title="entry.title"
            :highlighted="entry.title === highlightedName"
            @click="chooseFile(entry)"
          />
        </TransitionGroup>
      </ContextMenuTrigger>
    </div>
    <div v-if="writeMode" class="fileNameBar">
      <input
        v-model="filenameInputContents"
        class="fileNameInput"
        @pointerdown.stop
        @click.stop
        @contextmenu.stop
        @keydown.backspace.stop
        @keydown.delete.stop
        @keydown.arrow-left.stop
        @keydown.arrow-right.stop
        @keydown.enter.stop="acceptCurrentFile()"
      />
      <SvgButton
        class="fileNameAcceptButton"
        label="Ok"
        :disabled="!filenameInputContents"
        @click.stop="acceptCurrentFile"
      />
    </div>
  </div>
</template>

<style scoped>
.FileBrowserWidget {
  --border-width: 2px;
  --border-radius-inner: calc(var(--radius-default) - var(--border-width));
  background-color: var(--background-color);
  padding: var(--border-width);
  border-radius: 0 0 var(--radius-default) var(--radius-default);
  min-width: 400px;
  min-height: 200px;
  max-height: 600px;
  overflow-y: auto;
  overflow-x: hidden;
  display: flex;
  flex-direction: column;
}

.topBar {
  color: white;
  background-color: var(--background-color);
  display: flex;
  flex-direction: row;
  padding: 2px 8px;
}

.directoryStack {
  --transition-duration: 0.1s;
  color: white;
  gap: 2px;
  display: flex;
  align-items: center;
  flex-grow: 1;
}

.contents {
  flex: 1;
  width: 100%;
  background-color: var(--color-frame-selected-bg);
  border-radius: 0 0 var(--border-radius-inner) var(--border-radius-inner);
}

.listing {
  --transition-duration: 0.5s;
  padding: 8px;
  display: flex;
  height: 100%;
  flex-direction: column;
  align-items: start;
  justify-content: start;
  gap: 8px;
}

.centerContent {
  display: flex;
  align-items: center;
  justify-content: center;
}

.nonInteractive {
  pointer-events: none;
}

.v-move,
.v-enter-active,
.v-leave-active {
  transition: all var(--transition-duration) ease;
}
.v-enter-from,
.v-leave-to {
  opacity: 0;
  transform: translateX(30px);
}
.list-leave-active {
  position: absolute;
}

.fileNameBar {
  width: 100%;
  display: flex;
  flex-direction: row;
  padding: var(--border-width) 0 0 0;
  gap: var(--border-width);
}

.fileNameInput {
  border-radius: var(--border-radius-inner);
  height: calc(var(--border-radius-inner) * 2);
  padding: 0 8px;
  background-color: var(--color-frame-selected-bg);
  flex-grow: 1;
  appearance: textfield;
  -moz-appearance: textfield;
  user-select: all;
}

.fileNameAcceptButton {
  --color-menu-entry-hover-bg: color-mix(in oklab, var(--color-frame-selected-bg), black 10%);
  border-radius: var(--border-radius-inner);
  height: calc(var(--border-radius-inner) * 2);
  margin: 0px;
  padding: 4px 12px;
  background-color: var(--color-frame-selected-bg);
}
</style>
