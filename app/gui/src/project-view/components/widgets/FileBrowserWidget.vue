<script lang="ts">
export default {
  name: 'FileBrowserWidget',
}
</script>

<script setup lang="ts">
import LoadingSpinner from '@/components/shared/LoadingSpinner.vue'
import UpsertSecretPanel from '@/components/UpsertSecretPanel.vue'
import { mapPath, useEnsoPaths } from '@/components/widgets/FileBrowserWidget/ensoPath'
import {
  listDirectoryArgs,
  useCurrentPath,
  useSecretCreation,
  useUpsertDirectory,
} from '@/components/widgets/FileBrowserWidget/fileBrowser'
import FileBrowserContent from '@/components/widgets/FileBrowserWidget/FileBrowserContent.vue'
import FileBrowserModals from '@/components/widgets/FileBrowserWidget/FileBrowserModals.vue'
import FileBrowserNameBar from '@/components/widgets/FileBrowserWidget/FileBrowserNameBar.vue'
import FileBrowserTopBar from '@/components/widgets/FileBrowserWidget/FileBrowserTopBar.vue'
import {
  usePathBrowsing,
  type Directory,
} from '@/components/widgets/FileBrowserWidget/pathBrowsing'
import { useUserFiles } from '@/components/widgets/FileBrowserWidget/userFiles'
import { useBackend } from '@/composables/backend'
import { injectProjectBackend } from '@/providers/backend'
import type { AnyAsset } from 'enso-common/src/services/Backend'
import { assetIsDirectory, AssetType } from 'enso-common/src/services/Backend'
import { computed, ref, toValue, useTemplateRef, watch, watchEffect } from 'vue'

const props = withDefaults(
  defineProps<{
    writeMode?: boolean
    choosenPath?: string
    type?: 'file' | 'secret' | 'directory'
  }>(),
  { writeMode: false, choosenPath: '', type: 'file' },
)

const emit = defineEmits<{
  pathAccepted: [path: string]
  close: []
}>()

const browserContent = useTemplateRef('browserContent')

// === Cloud file APIs ===

const { query, fetch, ensureQueryData, mutation } = useBackend('remote')
const { remote: backend } = injectProjectBackend()
const { userFiles, userFilesError } = useUserFiles({
  backend,
  user: query('usersMe', []),
  organization: query('getOrganization', []),
})
const { parseEnsoPath, ensoPath, printEnsoPath } = useEnsoPaths(userFiles)
const listDirectory = (dir: Directory | undefined) => fetch('listDirectory', listDirectoryArgs(dir))
const {
  setBrowsingPath,
  enteredPath,
  unenteredPathSuffix,
  currentDirectory,
  isPending: isBrowsingPending,
} = usePathBrowsing({
  listDirectory,
})
const { isPending, data, error } = query('listDirectory', listDirectoryArgs(currentDirectory))
const { acceptName } = useUpsertDirectory({ currentDirectory, mutation })

type AssetExists = { exists: true; type: AssetType } | { exists: false }
async function assetExists(name: string): Promise<AssetExists> {
  const currentDir = currentDirectory.value
  if (currentDir == null) return { exists: false }
  const content = await listDirectory(currentDir)
  const asset = content.find((asset) => asset.title === name)
  if (!asset) return { exists: false }
  return { exists: true, type: asset.type }
}

// Prefetch directories to avoid lag when the user navigates, but only if we don't already have
// stale data. When the user opens a directory with stale data, it will refresh and the animation
// will show what files have changed since they last viewed.
watch(data, (assets) => {
  for (const asset of assets ?? [])
    if (assetIsDirectory(asset)) ensureQueryData('listDirectory', listDirectoryArgs(asset))
})

// === Current Path ===

const { currentDirPath, chosenFilename, setPath, enterDir, popTo, append } = useCurrentPath({
  home: () => ensoPath(toValue(userFiles.value?.home ?? [])),
  enteredPath,
})
watchEffect(() => setPath(parseEnsoPath(props.choosenPath)))
watchEffect(() => currentDirPath.value && setBrowsingPath(currentDirPath.value))
watchEffect(() => {
  if (props.writeMode && unenteredPathSuffix.value)
    filenameInputContents.value = unenteredPathSuffix.value
})

const filenameInputContents = ref('')
watchEffect(() => {
  if (chosenFilename.value) filenameInputContents.value = chosenFilename.value
})
const highlightedFilename = computed(
  () => (props.writeMode && filenameInputContents.value) || chosenFilename.value,
)

// === Status ===

const overwriteFilename = ref<string | null>(null)
const warningText = ref<string | null>(null)
const isBusy = computed(
  () => isBrowsingPending.value || isPending.value || commitSecretPending.value,
)
const anyError = computed(() => error.value ?? userFilesError.value)
const creatingSecret = ref(false)
const editingAsset = ref(false)
const disableTopBarButtons = computed(() => creatingSecret.value || editingAsset.value)

// === Secret creation ===

const { commitSecret, commitSecretPending } = useSecretCreation({ currentDirectory, mutation })

async function createSecret(value: string, name: string) {
  creatingSecret.value = false
  return commitSecret(value, name, () => acceptFile(name))
}

// === Accepting Chosen File ===

async function tryAcceptCurrentFile() {
  if (!enteredPath.value) {
    // We can only reach this if there was a root previously, but there isn't now. This might be
    // possible if the session is lost.
    warningText.value = 'Unable to access files'
    return
  }
  const path = mapPath(enteredPath.value, append(...filenameInputContents.value.split('/')))
  const enteringResult = await setBrowsingPath(path)
  currentDirPath.value = path
  if (!enteringResult.ok) {
    warningText.value = `${enteringResult.error.payload.toString()}`
    return
  }
  filenameInputContents.value = unenteredPathSuffix.value
  const assetInfo = await assetExists(filenameInputContents.value)
  if (assetInfo.exists && assetInfo.type === AssetType.file && props.writeMode) {
    overwriteFilename.value = filenameInputContents.value
  } else if (assetInfo.exists && assetInfo.type === AssetType.directory) {
    warningText.value = `'${filenameInputContents.value}' is a directory, not a file`
  } else {
    acceptCurrentFile()
    return
  }
}

function acceptCurrentFile() {
  acceptFile(filenameInputContents.value)
}

function acceptFile(name: string) {
  if (!enteredPath.value) return
  const currentFilePath = printEnsoPath(mapPath(enteredPath.value, append(...name.split('/'))))
  emit('pathAccepted', currentFilePath)
}

function chooseEntry(asset: AnyAsset, close: boolean) {
  if (props.writeMode) {
    filenameInputContents.value = asset.title
  } else {
    acceptFile(asset.title)
    if (close) emit('close')
  }
}
</script>

<template>
  <div class="FileBrowserWidget">
    <FileBrowserModals
      v-model:overwriteFilename="overwriteFilename"
      v-model:warningText="warningText"
      @overwriteConfirmed="acceptCurrentFile"
    />
    <FileBrowserTopBar
      :directoryStack="enteredPath?.segments ?? []"
      :disabled="disableTopBarButtons"
      :enableSecretCreation="type === 'secret'"
      @popTo="popTo"
      @newDirectory="browserContent?.addNewDirectory"
      @newSecret="creatingSecret = true"
    />
    <div v-if="anyError" class="centerContent browserContents">Error: {{ anyError }}</div>
    <div v-else-if="isBusy" class="centerContent browserContents">
      <LoadingSpinner phase="loading-medium" />
    </div>
    <div v-else-if="creatingSecret" class="browserContents">
      <UpsertSecretPanel @accepted="createSecret" @canceled="creatingSecret = false" />
    </div>
    <FileBrowserContent
      v-show="!(anyError || isBusy || creatingSecret)"
      ref="browserContent"
      :key="currentDirectory?.id ?? 'root'"
      class="browserContents"
      :assets="data ?? []"
      :chosenFilename="highlightedFilename"
      :targetType="type ?? 'file'"
      @renameDirectory="acceptName"
      @enterDirectory="enterDir"
      @choose="chooseEntry"
      @update:editingAsset="editingAsset = $event"
    />
    <FileBrowserNameBar
      v-if="writeMode"
      v-model="filenameInputContents"
      @accept="tryAcceptCurrentFile"
    />
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
  contain: layout;
}

.browserContents {
  flex: 1;
  width: 100%;
  background-color: var(--color-frame-selected-bg);
  border-radius: 0 0 var(--border-radius-inner) var(--border-radius-inner);
}

.centerContent {
  display: flex;
  align-items: center;
  justify-content: center;
}
</style>
