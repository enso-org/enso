<script setup lang="ts">
import TooltipTrigger from '@/components/TooltipTrigger.vue'
import { TypeInfo } from '@/stores/project/computedValueRegistry'
import { qnLastSegment } from '@/util/qualifiedName'
import { computed } from 'vue'

const props = defineProps<{ typeInfo?: TypeInfo | undefined; unknownLabel?: string }>()

type DisplayedAdditionalTypes = null | { kind: 'multiple'; types: string[] }

const additionalTypes = computed<DisplayedAdditionalTypes>(() => {
  if (props.typeInfo != null) {
    const typeInfo = props.typeInfo
    const combinedTypes = [...(typeInfo?.visibleTypes ?? []), ...(typeInfo?.hiddenTypes ?? [])]
    const additionalTypes = combinedTypes
      .flatMap((type) => (type.path ? qnLastSegment(type.path) : []))
    if (additionalTypes.length === 1) return null
    return { kind: 'multiple', types: additionalTypes }
  }
  return null
})

const label = computed(() => {
  if (props.typeInfo == null) return props.unknownLabel ?? 'Input'
  if (props.typeInfo != null && props.typeInfo?.primaryType.path) {
    return qnLastSegment(props.typeInfo.primaryType.path)
  }

  return undefined
})
</script>

<template>
  <div v-if="label" data-testid="component-editor-label" class="no-wrap">
    <template v-if="additionalTypes?.kind === 'multiple'">
      <TooltipTrigger>
        <template #default="triggerProps">
          <span
            class="additionalTypesPlaceholder"
            v-bind="triggerProps"
            v-text="`${label} & ${additionalTypes.types.length - 1} more`"
          />
        </template>
        <template #tooltip>
          <div class="flex flex-col">
            <span v-for="type in additionalTypes.types" :key="type" v-text="type" />
          </div>
        </template>
      </TooltipTrigger>
    </template>
    <span v-else v-text="label" />
  </div>
</template>

<style scoped>
.additionalTypesPlaceholder {
  background-color: rgba(0, 0, 0, 0.1);
  padding: 1px 3px;
  border-radius: 2px;
}

.no-wrap {
  white-space: nowrap;
  opacity: 0.7;
}
</style>
