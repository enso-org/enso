<script setup lang="ts">
import LoadingSpinner from '@/components/shared/LoadingSpinner.vue'
import SvgButton from '@/components/SvgButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { useBackend } from '@/composables/backend'
import { injectBackend } from '@/providers/backend'
import type { ToValue } from '@/util/reactivity'
import { useToast } from '@/util/toast'
import type {
  DatalinkAsset,
  DatalinkId,
  DirectoryAsset,
  DirectoryId,
  FileAsset,
  FileId,
} from 'enso-common/src/services/Backend'
import Backend, {
  assetIsDatalink,
  assetIsDirectory,
  assetIsFile,
} from 'enso-common/src/services/Backend'
import { computed, ref, toValue, watch } from 'vue'
import { Err, Ok, Result } from 'ydoc-shared/util/data/result'

const emit = defineEmits<{
  pathSelected: [path: string]
}>()

const { query, fetch, ensureQueryData } = useBackend('remote')
const { remote: backend } = injectBackend()

const errorToast = useToast.error()

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
const isEmpty = computed(() => directories.value?.length === 0 && files.value?.length === 0)

// === Selected File ===

interface File {
  id: FileId | DatalinkId
  title: string
}

const selectedFile = ref<File>()

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
  selectedFile.value = file
}

const isBusy = computed(
  () =>
    isDirectoryStackInitializing.value ||
    isPending.value ||
    (selectedFile.value && currentUser.isPending.value),
)

const anyError = computed(() =>
  isError.value ? error
  : currentUser.isError.value ? currentUser.error
  : undefined,
)

const selectedFilePath = computed(
  () =>
    selectedFile.value && currentPath.value && `${currentPath.value}${selectedFile.value.title}`,
)

watch(selectedFilePath, (path) => {
  if (path) emit('pathSelected', path)
})

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
</script>

<template>
  <div class="FileBrowserWidget">
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
    <div v-if="isBusy" class="centerContent contents"><LoadingSpinner /></div>
    <div v-else-if="anyError" class="centerContent contents">Error: {{ anyError }}</div>
    <div v-else-if="isEmpty" class="centerContent contents">Directory is empty</div>
    <div v-else :key="currentDirectory?.id ?? 'root'" class="listing contents">
      <TransitionGroup>
        <div v-for="entry in directories" :key="entry.id">
          <SvgButton :label="entry.title" name="folder" class="entry" @click="enterDir(entry)" />
        </div>
        <div v-for="entry in files" :key="entry.id">
          <SvgButton :label="entry.title" name="text2" class="entry" @click="chooseFile(entry)" />
        </div>
      </TransitionGroup>
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
  display: flex;
  flex-direction: column;
}

.directoryStack {
  --transition-duration: 0.1s;
  color: white;
  padding: 2px;
  gap: 2px;
  background-color: var(--background-color);
  display: flex;
  align-items: center;
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

.entry {
  width: 100%;
  justify-content: start;
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
</style>
