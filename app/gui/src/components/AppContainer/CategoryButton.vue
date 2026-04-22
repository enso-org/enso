<script setup lang="ts">
import { ASSETS_MIME_TYPE } from '#/data/mimeTypes'
import { ASSETS_DATA_TRANSFER_PAYLOAD } from '#/layouts/Drive/Categories'
import { SEARCH_PARAMS_PREFIX } from '$/appUtils'
import {
  canTransferBetweenCategories,
  categoryEq,
  categoryFromKey,
  categoryIcon,
  useCategories,
  type Category,
  type LocalDirectory,
} from '$/providers/category'
import { useContainerData } from '$/providers/container'
import { useDriveLocation } from '$/providers/drive'
import { useReactApi } from '$/providers/reactApi'
import { useText } from '$/providers/text'
import { debouncedGetter } from '$/utils/reactivity'
import LoadingSpinner from '@/components/shared/LoadingSpinner.vue'
import SvgButton from '@/components/SvgButton.vue'
import TooltipTrigger from '@/components/TooltipTrigger.vue'
import { computed, ref, toRefs } from 'vue'
import { useRouter } from 'vue-router'

const {
  category,
  extended = false,
  disabled = false,
} = defineProps<{
  category: Category
  extended: boolean
  disabled?: string | false
}>()

const LOADING_INDICATOR_DELAY_MS = 100

const { categoryLabel, removeLocalDirectory } = useCategories()
const { currentCategory } = toRefs(useDriveLocation())
const { leftPanelShown, leftPanelToggledOn } = toRefs(useContainerData())
const reactApi = useReactApi()
const { getText } = useText()
const router = useRouter()

const label = computed(() => categoryLabel(category))
const selected = computed(() => categoryEq(category, currentCategory.value))
const isLoading = computed(() => selected.value && reactApi.isTransitioning)
const delayedIsLoading = debouncedGetter(() => isLoading.value, LOADING_INDICATOR_DELAY_MS)
const showLoading = computed(() => isLoading.value && delayedIsLoading.value)

const isDropTarget = computed(
  () =>
    !categoryEq(currentCategory.value, category) &&
    canTransferBetweenCategories(currentCategory.value, category),
)

const acceptedDragTypes = computed(() => (isDropTarget.value ? [ASSETS_MIME_TYPE] : []))
const dropHover = ref(false)

function onClick() {
  if (!leftPanelShown.value) {
    leftPanelToggledOn.value = true
  }
  currentCategory.value = category
}

function onDragover(event: DragEvent) {
  for (const item of event.dataTransfer?.items ?? []) {
    if (acceptedDragTypes.value.includes(item.type)) {
      dropHover.value = true
      event.preventDefault()
      return
    }
  }
}

async function onDrop(event: DragEvent) {
  const payloads = await Promise.all(
    Array.from(event.dataTransfer?.items ?? [])
      .filter((item) => item.kind === 'string' && item.type === ASSETS_MIME_TYPE)
      .map(async (item) => {
        const text = await new Promise<string>((resolve) => item.getAsString(resolve))
        const parsedPayload = ASSETS_DATA_TRANSFER_PAYLOAD.safeParse(JSON.parse(text))
        return parsedPayload.success ? parsedPayload.data : null
      }),
  ).then((items) => items.filter((payload) => payload != null))
  const firstItem = payloads[0]?.items[0]
  if (firstItem == null) return
  event.preventDefault()

  const transfer = async () => {
    await Promise.all(
      payloads.map((payload) => {
        const fromCategory = categoryFromKey(payload.category)
        return (
          fromCategory && reactApi.transferBetweenCategories(fromCategory, category, payload.items)
        )
      }),
    )
  }

  if (category.type === 'trash') {
    const itemsCount = payloads.flatMap(({ items }) => items).length
    reactApi.confirmDelete({
      defaultOpen: true,
      actionText:
        itemsCount === 1 ?
          getText('deleteSelectedAssetActionText', firstItem.title)
        : getText('deleteSelectedAssetsActionText', itemsCount),
      onConfirm: transfer,
    })
  } else {
    await transfer()
  }
}

function onRemoveLocalDirClick(directory: LocalDirectory) {
  reactApi.confirmDelete({
    actionText: getText('removeTheLocalDirectoryXFromFavorites', categoryLabel(directory)),
    actionButtonLabel: getText('remove'),
    onConfirm: () => {
      removeLocalDirectory(directory.path)
    },
  })
}
</script>

<template>
  <div class="CategoryButton">
    <TooltipTrigger placement="right" :enabled="disabled !== false">
      <template #default="triggerProps">
        <SvgButton
          :class="{ dropHover }"
          :name="!showLoading ? categoryIcon(category.type) : undefined"
          :label="extended ? label : undefined"
          :aria-label="label"
          :modelValue="leftPanelShown && selected"
          :disabled="disabled !== false"
          v-bind="triggerProps"
          @update:modelValue="onClick"
          @dragover="onDragover"
          @dragleave="dropHover = false"
          @drop="onDrop"
        >
          <LoadingSpinner v-if="showLoading" phase="loading-medium" :size="16" />
        </SvgButton>
      </template>
      <template #tooltip>
        {{ disabled }}
      </template>
    </TooltipTrigger>

    <SvgButton
      v-if="extended && category.type === 'local'"
      name="settings"
      @activate="
        () =>
          router.push({
            path: '/settings',
            query: { [`${SEARCH_PARAMS_PREFIX}SettingsTab`]: JSON.stringify('local') },
          })
      "
    />
    <SvgButton
      v-else-if="extended && category.type === 'localDirectory'"
      name="minus"
      @activate="onRemoveLocalDirClick(category)"
    />
  </div>
</template>

<style scoped>
.CategoryButton {
  display: flex;
  flex-direction: row;
  justify-content: space-between;
}

.dropHover {
  background-color: rgba(255 255 255);
}
</style>
