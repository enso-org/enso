<script setup lang="ts">
import { ASSETS_MIME_TYPE } from '#/data/mimeTypes'
import { ASSETS_DATA_TRANSFER_PAYLOAD } from '#/layouts/Drive/Categories'
import {
  canTransferBetweenCategories,
  categoryEq,
  categoryFromKey,
  categoryIcon,
  useCategories,
  type Category,
} from '$/providers/categories'
import { useDriveLocation } from '$/providers/drive'
import { useReactApi } from '$/providers/reactApi'
import { useText } from '$/providers/text'
import SvgButton from '@/components/SvgButton.vue'
import { computed, toRefs } from 'vue'

const { category, extended = false } = defineProps<{
  category: Category
  extended: boolean
}>()

const { categoryLabel } = useCategories()
const { currentCategory } = toRefs(useDriveLocation())
const { transferBetweenCategories, confirmDelete } = useReactApi()
const { getText } = useText()

const isDropTarget = computed(
  () =>
    !categoryEq(currentCategory.value, category) &&
    canTransferBetweenCategories(currentCategory.value, category),
)

const acceptedDragTypes = computed(() => (isDropTarget.value ? [ASSETS_MIME_TYPE] : []))

function onDragover(event: DragEvent) {
  for (const item of event.dataTransfer?.items ?? []) {
    if (acceptedDragTypes.value.find((type) => type === item.type)) {
      event.preventDefault()
      return
    }
  }
}

async function onDrop(event: DragEvent) {
  // unsetModal()
  // if (event.dropOperation === 'cancel') return
  console.debug(event.dataTransfer?.items.length)
  console.debug(event.dataTransfer?.items[0]?.kind, event.dataTransfer?.items[0]?.type)
  console.debug(event.dataTransfer?.items[1]?.kind, event.dataTransfer?.items[1]?.type)
  event.dataTransfer?.items[0]?.getAsString(console.debug)

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
        return fromCategory && transferBetweenCategories(fromCategory, category, payload.items)
      }),
    )
  }

  if (category.type === 'trash') {
    confirmDelete({
      defaultOpen: true,
      actionText:
        payloads[0]?.items.length === 1 && firstItem != null ?
          getText('deleteSelectedAssetActionText', firstItem.title)
        : getText('deleteSelectedAssetsActionText', payloads.flatMap(({ items }) => items).length),
      onConfirm: transfer,
    })
  } else {
    await transfer()
  }
}
</script>

<template>
  <SvgButton
    class="leftBarIcon"
    :name="categoryIcon(category.type)"
    :label="extended ? categoryLabel(category) : undefined"
    :modelValue="categoryEq(category, currentCategory)"
    @update:modelValue="currentCategory = category"
    @dragover="onDragover"
    @drop="onDrop"
  />
</template>
