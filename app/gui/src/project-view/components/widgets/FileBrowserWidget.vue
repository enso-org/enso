<script lang="ts">
export default {
  name: 'FileBrowserWidget',
}
</script>

<script setup lang="ts">
import { useBackends } from '$/providers/backends'
import type { FileType } from '$/providers/openedProjects/widgetRegistry/configuration'
import ActionButton from '@/components/ActionButton.vue'
import LoadingSpinner from '@/components/shared/LoadingSpinner.vue'
import UpsertSecretPanel from '@/components/UpsertSecretPanel.vue'
import { useEnsoPaths } from '@/components/widgets/FileBrowserWidget/ensoPath'
import {
  listDirectoryArgs,
  useCurrentPath,
  useSecretCreation,
  useUpsertDirectory,
} from '@/components/widgets/FileBrowserWidget/fileBrowser'
import FileBrowserBreadcrumbs from '@/components/widgets/FileBrowserWidget/FileBrowserBreadcrumbs.vue'
import FileBrowserContent from '@/components/widgets/FileBrowserWidget/FileBrowserContent.vue'
import FileBrowserModals from '@/components/widgets/FileBrowserWidget/FileBrowserModals.vue'
import FileBrowserNameBar from '@/components/widgets/FileBrowserWidget/FileBrowserNameBar.vue'
import FileBrowserTopBar from '@/components/widgets/FileBrowserWidget/FileBrowserTopBar.vue'
import { useNameBar } from '@/components/widgets/FileBrowserWidget/nameBar'
import {
  usePathBrowsing,
  type Directory,
} from '@/components/widgets/FileBrowserWidget/pathBrowsing'
import { useAcceptCurrentFile } from '@/components/widgets/FileBrowserWidget/useAcceptCurrentFile'
import { useFileBrowserSync } from '@/components/widgets/FileBrowserWidget/useFileBrowserSync'
import { useUserFiles } from '@/components/widgets/FileBrowserWidget/userFiles'
import { useBackend } from '@/composables/backend'
import { registerHandlers } from '@/providers/action'
import { providePopoverRoot } from '@/providers/popoverRoot'
import type { AnyAsset } from 'enso-common/src/services/Backend'
import { assetIsDirectory, AssetType } from 'enso-common/src/services/Backend'
import { computed, ref, toValue, useTemplateRef, watch } from 'vue'

const props = withDefaults(
  defineProps<{
    writeMode?: boolean
    choosenPath?: string
    type?: 'file' | 'secret' | 'directory'
    fileTypes?: FileType[] | undefined
    allowOverride?: boolean
  }>(),
  {
    writeMode: false,
    choosenPath: '',
    type: 'file',
    fileTypes: () => [{ label: 'All files', extensions: ['*'] }],
    allowOverride: false,
  },
)

const emit = defineEmits<{
  pathAccepted: [path: string]
  close: []
}>()

const browserContent = useTemplateRef('browserContent')

// === Cloud file APIs ===

const { query, fetch, ensureQueryData, mutation } = useBackend('remote')
const { remoteBackend: backend } = useBackends()
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
  const content = (await listDirectory(currentDir))?.assets
  const asset = content?.find((asset) => asset.title === name)
  if (!asset) return { exists: false }
  return { exists: true, type: asset.type }
}

// Prefetch directories to avoid lag when the user navigates, but only if we don't already have
// stale data. When the user opens a directory with stale data, it will refresh and the animation
// will show what files have changed since they last viewed.
watch(data, (response) => {
  for (const asset of response?.assets ?? [])
    if (assetIsDirectory(asset)) ensureQueryData('listDirectory', listDirectoryArgs(asset))
})

// === Current Path ===

const { filenameInput, extensionInput, fullFilePath, setFilename, fileExtensionFilter } =
  useNameBar()

const { currentDirPath, chosenFilename, setPath, enterDir, popTo, append } = useCurrentPath({
  home: () => ensoPath(toValue(userFiles.value?.home ?? [])),
  enteredPath,
})
useFileBrowserSync({
  writeMode: () => props.writeMode,
  choosenPath: () => props.choosenPath,
  parseEnsoPath,
  currentDirPath,
  chosenFilename,
  setPath,
  setBrowsingPath,
  append,
  setFilename,
  unenteredPathSuffix,
})

const highlightedFilename = computed(
  () => (props.writeMode && fullFilePath.value) || chosenFilename.value,
)

// === Status ===

const isBusy = computed(
  () => isBrowsingPending.value || isPending.value || commitSecretPending.value,
)
const anyError = computed(() => error.value ?? userFilesError.value)
const creatingSecret = ref(false)
const editingAsset = ref(false)
const enableTopBarButtons = computed(() => !creatingSecret.value && !editingAsset.value)

// === Secret creation ===

const { commitSecret, commitSecretPending } = useSecretCreation({ currentDirectory, mutation })

async function createSecret(value: string, name: string) {
  creatingSecret.value = false
  return commitSecret(value, name, () => acceptFile(name))
}

// === Accepting Chosen File ===

const { overwriteFilename, warningText, tryAcceptCurrentFile, acceptCurrentFile, acceptFile } =
  useAcceptCurrentFile({
    enteredPath,
    fullFilePath,
    currentDirPath,
    setBrowsingPath,
    append,
    setFilename,
    unenteredPathSuffix,
    assetExists,
    writeMode: () => props.writeMode,
    allowOverride: () => props.allowOverride,
    printEnsoPath,
    pathAcceptedCallback: (p) => emit('pathAccepted', p),
  })

function chooseEntry(asset: AnyAsset, close: boolean) {
  const name = asset.type === AssetType.datalink ? `${asset.title}.datalink` : asset.title
  if (props.writeMode) {
    setFilename(name)
  } else {
    acceptFile(name)
    if (close) emit('close')
  }
}

const root = useTemplateRef<HTMLDivElement>('root')
providePopoverRoot(root)

registerHandlers({
  'fileBrowser.newDirectory': {
    enabled: enableTopBarButtons,
    action: () => browserContent.value?.newDirectory.action(),
  },
  'fileBrowser.renameDirectory': {
    enabled: () =>
      enableTopBarButtons.value && (browserContent.value?.renameDirectory.enabled.value ?? false),
    action: () => browserContent.value?.renameDirectory.action(),
  },
  'fileBrowser.newSecret': {
    available: computed(() => props.type === 'secret'),
    enabled: enableTopBarButtons,
    action: () => (creatingSecret.value = true),
  },
  'fileBrowser.navigateUp': {
    enabled: () => enableTopBarButtons.value && !!enteredPath.value?.segments.length,
    action: () => popTo(enteredPath.value!.segments.length - 1),
  },
})
</script>

<template>
  <div ref="root" class="FileBrowserWidgetWrapper">
    <div class="FileBrowserWidget">
      <FileBrowserModals
        v-model:overwriteFilename="overwriteFilename"
        v-model:warningText="warningText"
        @overwriteConfirmed="acceptCurrentFile"
      />
      <FileBrowserTopBar>
        <FileBrowserBreadcrumbs
          :directoryStack="enteredPath?.segments ?? []"
          :enabled="enableTopBarButtons"
          @popTo="popTo"
        />
        <ActionButton action="fileBrowser.newDirectory" />
        <ActionButton action="fileBrowser.newSecret" />
      </FileBrowserTopBar>
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
        :assets="data?.assets ?? []"
        :chosenFilename="highlightedFilename"
        :targetType="type ?? 'file'"
        :matchesFilter="fileExtensionFilter.matches"
        @renameDirectory="acceptName"
        @enterDirectory="enterDir"
        @choose="chooseEntry"
        @update:editingAsset="editingAsset = $event"
      />
      <FileBrowserNameBar
        v-model:filenameInput="filenameInput"
        v-model:extensionInput="extensionInput"
        :writeMode="writeMode ?? false"
        :root="root"
        :fileExtensionFilter="fileExtensionFilter.filter.value"
        :displayedExtension="fileExtensionFilter.displayedExtension.value"
        :fileTypes="props.fileTypes"
        @accept="tryAcceptCurrentFile"
        @setFilter="fileExtensionFilter.filter.value = $event"
      />
    </div>
  </div>
</template>

<style scoped>
.FileBrowserWidgetWrapper {
  --z-index: var(--z-index-file-browser, 0);
  --z-index-selection-submenu: calc(var(--z-index) - 1);
  --background-color: var(--file-browser-background-color, var(--color-panel-accent));
  --dropdown-bg: var(--background-color);
  --dropdown-fg: var(--file-browser-text-color, white);
  --dropdown-item-hover-bg: color-mix(in oklab, var(--dropdown-bg) 70%, white 30%);
  --dropdown-item-selected-bg: color-mix(
    in oklab,
    var(--dropdown-bg) 80%,
    var(--color-node-background) 20%
  );
}

.FileBrowserWidget {
  --border-width: 2px;
  --border-radius-inner: calc(var(--radius-default) - var(--border-width));
  --corner-radius: var(--file-browser-corner-radius, var(--radius-default));
  background-color: var(--background-color);
  padding: var(--border-width);
  border-radius: var(--corner-radius);
  min-width: var(--file-browser-min-width, 400px);
  min-height: 200px;
  max-height: 412px;
  overflow: hidden;
  display: flex;
  flex-direction: column;
  contain: layout;
  position: relative;
  z-index: var(--z-index);
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

:deep(.FileBrowserButton) {
  --color-menu-entry-hover-bg: color-mix(in oklab, var(--color-frame-selected-bg), black 10%);
  border-radius: var(--border-radius-inner);
  height: calc(var(--border-radius-inner) * 2);
  margin: 0;
  padding: 4px 12px;
  background-color: var(--color-frame-selected-bg);
}
</style>
