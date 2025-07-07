<script setup lang="ts">
import TooltipTrigger from '@/components/TooltipTrigger.vue'
import { TypeInfo } from '@/stores/project/computedValueRegistry'
import { qnLastSegment } from '@/util/qualifiedName'
import { computed } from 'vue'

const props = defineProps<{
  typeInfo?: TypeInfo | undefined
  unknownLabel?: string | undefined
  testId: string
}>()

const additionalTypes = computed<string[]>(() => {
  if (props.typeInfo == null) {
    return []
  }

  const typeInfo = props.typeInfo
  const combinedTypes = [...(typeInfo?.visibleTypes ?? []), ...(typeInfo?.hiddenTypes ?? [])]
  const additionalTypes = combinedTypes.flatMap((type) =>
    type.path ? qnLastSegment(type.path) : [],
  )
  return additionalTypes.length === 1 ? [] : additionalTypes
})

const label = computed(() => {
  if (props.typeInfo == null) return props.unknownLabel
  if (props.typeInfo != null && props.typeInfo?.primaryType.path) {
    return qnLastSegment(props.typeInfo.primaryType.path)
  }

  return undefined
})
</script>

<template>
  <div v-if="label" :data-testid="props.testId" class="componentEditorLabel">
    <TooltipTrigger v-if="additionalTypes.length > 0">
      <template #default="triggerProps">
        <span
          class="additionalTypesPlaceholder"
          v-bind="triggerProps"
          v-text="`${label} & + ${additionalTypes.length - 1}`"
        />
      </template>
      <template #tooltip>
        <div class="flex flex-col">
          <span v-for="type in additionalTypes" :key="type" v-text="type" />
        </div>
      </template>
    </TooltipTrigger>
    <span v-else v-text="label" />
  </div>
</template>

<style scoped>
.additionalTypesPlaceholder {
  background-color: rgba(0, 0, 0, 0.1);
  padding: 1px 3px;
  border-radius: 2px;
}

.componentEditorLabel {
  white-space: nowrap;
  opacity: 0.7;
}
</style>
