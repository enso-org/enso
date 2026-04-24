<script setup lang="ts">
import ContextMenuTrigger from '@/components/ContextMenuTrigger.vue'
import FileBrowserEntry from '@/components/widgets/FileBrowserWidget/FileBrowserEntry.vue'
import type { Directory } from '@/components/widgets/FileBrowserWidget/pathBrowsing'
import { useToast } from '@/util/toast'
import {
  type AnyAsset,
  type AssetId,
  assetIsDatalink,
  assetIsDirectory,
  assetIsFile,
  assetIsSecret,
  type CreatedDirectory,
  type DirectoryId,
} from 'enso-common/src/services/Backend'
import { computed, reactive, ref, watch } from 'vue'
import { assert } from 'ydoc-shared/util/assert'

const { assets, chosenFilename, targetType, matchesFilter } = defineProps<{
  assets: readonly AnyAsset[]
  chosenFilename: string | null
  targetType: 'file' | 'secret' | 'directory'
  matchesFilter: (asset: AnyAsset) => boolean
}>()

const emit = defineEmits<{
  renameDirectory: [
    Directory | undefined,
    string,
    (action: Promise<CreatedDirectory | null>) => void,
  ]
  enterDirectory: [Directory]
  choose: [AnyAsset, boolean]
  'update:editingAsset': [boolean]
}>()

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
const keyOverride: Map<AssetId | symbol, number> = reactive(new Map())

const compareTitle = (a: { title: string }, b: { title: string }) => a.title.localeCompare(b.title)
const entries = computed(() => {
  const directories = []
  const files = []
  for (const asset of assets) {
    if (assetIsDirectory(asset)) directories.push(asset)
    else if (assetIsTargetType(asset) && matchesFilter(asset)) files.push(asset)
  }
  directories.sort(compareTitle)
  files.sort(compareTitle)
  return [...directories, ...files]
})
declare const brandTargetType: unique symbol
type TargetType = AnyAsset & { [brandTargetType]: never }
function assetIsTargetType(asset: AnyAsset): asset is TargetType {
  switch (targetType) {
    case 'file':
      return assetIsFile(asset) || assetIsDatalink(asset)
    case 'secret':
      return assetIsSecret(asset)
    case 'directory':
      return assetIsDirectory(asset)
    default:
      return false
  }
}

function findWithTitle(entries: AnyAsset[], title: string) {
  return entries.find(({ title: entryTitle }) => entryTitle === title)
}

const focusedAsset = computed(() =>
  focused.value?.type === 'asset' ? focused.value.asset
  : focused.value?.type === 'title' ? findWithTitle(entries.value, focused.value.title)
  : undefined,
)
const focusedDirectory = computed(() =>
  focusedAsset.value && assetIsDirectory(focusedAsset.value) ? focusedAsset.value : undefined,
)

const editedAsset = ref<{
  asset?: Directory
  name: string
  state: 'editing' | 'pending' | 'just created'
  createdId?: DirectoryId
}>()

const renameDirectoryEnabled = computed(
  () => focusedDirectory.value != null && editedAsset.value == null,
)

function entryIcon(entry: AnyAsset) {
  return assetIsDirectory(entry) ? 'folder' : 'text2'
}

const focused = ref<{ type: 'asset'; asset: AnyAsset } | { type: 'title'; title: string } | null>()

function focusAsset(asset: AnyAsset) {
  focused.value = { type: 'asset', asset }
}

const highlightedName = computed(() =>
  focused.value?.type === 'asset' ? focused.value.asset.title : focused.value?.title,
)

watch(
  () => chosenFilename,
  (title) => (focused.value = title == null ? null : { type: 'title', title }),
  { immediate: true },
)

function onClick(entry: AnyAsset, isDouble: boolean) {
  if (isDouble && assetIsDirectory(entry)) {
    emit('enterDirectory', entry)
    return
  }
  focusAsset(entry)
  if (assetIsTargetType(entry)) emit('choose', entry, isDouble)
}

function addDirectoryAction() {
  if (editedAsset.value != null) {
    console.warn('Ignoring attempt to begin creating directory while already editing asset')
    return
  }
  keyOverride.set(newDirPlaceholder, nextKeyForNewDir++)
  editedAsset.value = { name: 'New Folder', state: 'editing' }
}

function renameDirectoryAction() {
  if (editedAsset.value != null) {
    console.warn('Ignoring attempt to begin renaming directory while already editing asset')
    return
  }
  if (focusedDirectory.value == null) {
    errorToast.show('Unable to rename directory: No directory selected')
    return
  }
  editedAsset.value = {
    asset: focusedDirectory.value,
    name: focusedDirectory.value.title,
    state: 'editing',
  }
}

async function acceptName(name: string) {
  if (editedAsset.value?.state !== 'editing') {
    console.error('Ignoring attempt to accept edited name when not in editing state')
    return
  }
  const edited = editedAsset.value
  edited.name = name
  edited.state = 'pending'
  emit('renameDirectory', edited.asset, name, (action) =>
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
    ),
  )
}

watch(
  entries,
  (entries) => {
    // Finish editing once received an updated directory.
    if (entries?.some(({ id }) => id === editedAsset.value?.createdId)) {
      editedAsset.value = undefined
    }
  },
  { flush: 'sync' },
)
const editingAsset = computed(() => editedAsset.value != null)
watch(editingAsset, (editingAsset) => emit('update:editingAsset', editingAsset))

const isEmpty = computed(() => entries.value.length === 0 && editedAsset.value == null)

defineExpose({
  newDirectory: {
    action: addDirectoryAction,
  },
  renameDirectory: {
    enabled: renameDirectoryEnabled,
    action: renameDirectoryAction,
  },
})
</script>

<template>
  <div
    class="FileBrowserContent"
    :class="{ centerContent: isEmpty }"
    @click.stop="focused = undefined"
    @wheel.stop.passive
  >
    <div v-if="isEmpty" class="centerContent">Directory is empty</div>
    <ContextMenuTrigger v-show="!isEmpty" :actions="['fileBrowser.renameDirectory']">
      <TransitionGroup>
        <FileBrowserEntry
          v-if="editedAsset && editedAsset.asset == null"
          :key="keyOverride.get(newDirPlaceholder) ?? newDirPlaceholder"
          icon="folder"
          :title="editedAsset.name"
          :editingState="editedAsset.state"
          @nameAccepted="acceptName($event)"
          @contextmenu.capture.stop
        />
        <FileBrowserEntry
          v-for="entry in entries"
          :key="keyOverride.get(entry.id) ?? entry.id"
          :icon="entryIcon(entry)"
          :title="editedAsset?.asset?.id === entry.id ? editedAsset.name : entry.title"
          :highlighted="entry.title === highlightedName"
          :editingState="editedAsset?.asset?.id === entry.id ? editedAsset.state : undefined"
          @click.stop="onClick(entry, false)"
          @dblclick.stop="onClick(entry, true)"
          @nameAccepted="acceptName($event)"
          @contextmenu.capture="focusAsset(entry)"
        />
      </TransitionGroup>
    </ContextMenuTrigger>
  </div>
</template>

<style scoped>
.FileBrowserContent {
  --transition-duration: 0.5s;
  padding: 8px;
  display: flex;
  height: 100%;
  flex-direction: column;
  align-items: start;
  justify-content: start;
  gap: 8px;
  overflow-y: auto;
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
</style>
