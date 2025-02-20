<script lang="ts">
export default {
  name: 'FileBrowserWidget',
}
</script>

<script setup lang="ts">
import LoadingSpinner from '@/components/shared/LoadingSpinner.vue'
import SvgButton from '@/components/SvgButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { useBackend } from '@/composables/backend'
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
import { computed, onMounted, reactive, ref, toValue, watch } from 'vue'
import { Err, Ok, Result } from 'ydoc-shared/util/data/result'
import FileBrowserEntry from './FileBrowserWidget/FileBrowserEntry.vue'

const { writeMode = false } = defineProps<{ writeMode?: boolean }>()

const emit = defineEmits<{
  pathAccepted: [path: string]
}>()

const { query, fetch, ensureQueryData, mutation } = useBackend('remote')
const { remote: backend } = injectBackend()

const errorToast = useToast.error()
const fileName = ref<string>('')
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

// === Current Directory ===

interface Directory {
  id: DirectoryId
  title: string
}

const currentUser = query('usersMe', [])
const currentOrganization = query('getOrganization', [])
const directoryStack = ref<Directory[]>([])
const isDirectoryStackInitializing = computed(() => directoryStack.value.length === 0)
const currentDirectory = computed(() => directoryStack.value[directoryStack.value.length - 1])

const currentPath = computed(() => {
  if (!currentUser.data.value) return
  let root = backend?.rootPath(currentUser.data.value) ?? 'enso://'
  if (!root.endsWith('/')) root += '/'
  return `${root}${directoryStack.value
    .slice(1)
    .map((dir) => `${dir.title}/`)
    .join('')}`
})

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

class DirNotFoundError {
  constructor(public dirName: string) {}

  toString() {
    return `Directory "${this.dirName}" not found`
  }
}

function popTo(index: number) {
  directoryStack.value.splice(index + 1)
}

function chooseFile(file: FileAsset | DatalinkAsset) {
  fileName.value = file.title
  if (!writeMode) {
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
  : undefined,
)

const currentFilePath = computed(
  () => fileName.value && currentPath.value && `${currentPath.value}${fileName.value}`,
)

// === Creating and Renaming Directories ===

const editedAsset = ref<{
  asset: Directory | typeof newDirPlaceholder
  name: string
  state: 'editing' | 'pending' | 'just created'
  createdId?: DirectoryId
}>()
const createDir = mutation('createDirectory', { meta: { awaitInvalidates: false } })
const updateDir = mutation('updateDirectory')

function addNewDirectory() {
  assert(editedAsset.value == null)
  keyOverride.set(newDirPlaceholder, nextKeyForNewDir++)
  editedAsset.value = { asset: newDirPlaceholder, name: 'New Folder', state: 'editing' }
}

function acceptName(name: string, actionDescription: string) {
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
  const requestBody = { title: edited.name, parentId }
  const action =
    edited.asset === newDirPlaceholder ?
      createDir.mutateAsync([requestBody, false])
    : updateDir.mutateAsync([edited.asset.id, requestBody, edited.asset.title])
  action
    .then((result) => {
      assert(edited === editedAsset.value)
      if (result?.id) {
        editedAsset.value.createdId = result.id
        editedAsset.value.state = 'just created'
        const key = keyOverride.get(newDirPlaceholder)
        if (key != null) {
          keyOverride.set(result.id, key)
        }
      }
    })
    .catch((error) => {
      errorToast.show(`Failed to ${actionDescription}: ${error}`)
      editedAsset.value = undefined
    })
}

watch(
  directories,
  (dirs) => {
    // Remove placeholder once received an actual directory.
    if (dirs?.find((dir) => dir.id === editedAsset.value?.createdId)) editedAsset.value = undefined
  },
  { flush: 'sync' },
)

// === Initialization ===

async function enterDirByName(name: string, stack: Directory[]): Promise<Result> {
  const currentDir = stack[stack.length - 1]
  if (currentDir == null) return Err('Stack is empty')
  const content = await fetch('listDirectory', listDirectoryArgs(currentDir))
  const nextDir = content.find(
    (asset): asset is DirectoryAsset => assetIsDirectory(asset) && asset.title === name,
  )
  if (!nextDir) return Err(new DirNotFoundError(name))
  stack.push(nextDir)
  return Ok()
}

onMounted(() => {
  Promise.all([currentUser.promise.value, currentOrganization.promise.value]).then(
    async ([user, organization]) => {
      if (!user) {
        errorToast.show('Cannot load file list: not logged in.')
        return
      }
      const rootDirectoryId =
        backend?.rootDirectoryId(user, organization, null) ?? user.rootDirectoryId
      const stack = [{ id: rootDirectoryId, title: 'Cloud' }]
      if (rootDirectoryId != user.rootDirectoryId) {
        let result = await enterDirByName('Users', stack)
        result = result.ok ? await enterDirByName(user.name, stack) : result
        if (!result.ok) errorToast.reportError(result.error, 'Cannot enter home directory')
      }
      directoryStack.value = stack
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

    <div v-if="isBusy" class="centerContent contents"><LoadingSpinner /></div>
    <div v-else-if="anyError" class="centerContent contents">Error: {{ anyError }}</div>
    <div v-else-if="isEmpty" class="centerContent contents">Directory is empty</div>
    <div v-else :key="currentDirectory?.id ?? 'root'" class="listing contents">
      <TransitionGroup>
        <FileBrowserEntry
          v-if="editedAsset?.asset === newDirPlaceholder"
          :key="keyOverride.get(newDirPlaceholder) ?? newDirPlaceholder"
          icon="folder"
          :title="editedAsset.name"
          :editingState="editedAsset.state"
          @nameAccepted="acceptName($event, 'create folder')"
        />
        <FileBrowserEntry
          v-for="entry in directories"
          :key="keyOverride.get(entry.id) ?? entry.id"
          icon="folder"
          :title="editedAsset?.asset === entry ? editedAsset.name : entry.title"
          :editingState="editedAsset?.asset === entry ? editedAsset.state : undefined"
          @click="enterDir(entry)"
          @nameAccepted="acceptName($event, 'rename folder')"
        />
        <FileBrowserEntry
          v-for="entry in files"
          :key="entry.id"
          icon="text2"
          :title="entry.title"
          @click="chooseFile(entry)"
        />
      </TransitionGroup>
    </div>
    <div v-if="writeMode" class="fileNameBar">
      <input
        v-model="fileName"
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
        :disabled="!fileName"
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
