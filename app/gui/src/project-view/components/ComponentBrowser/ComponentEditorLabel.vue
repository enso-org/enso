<script setup lang="ts">
import { SelfArg } from '@/components/ComponentBrowser/filtering'
import TooltipTrigger from '@/components/TooltipTrigger.vue'
import { qnLastSegment } from '@/util/qualifiedName'
import { computed } from 'vue'

const props = defineProps<{ selfArg?: SelfArg | undefined }>()

type DisplayedAdditionalTypes =
  | null
  | { kind: 'single'; type: string }
  | { kind: 'multiple'; types: string[] }

const additionalTypes = computed<DisplayedAdditionalTypes>(() => {
  if (props.selfArg?.type === 'known') {
    const additionalTypes = props.selfArg.typeInfo?.hiddenTypes.flatMap((type) =>
      type.path ? qnLastSegment(type.path) : [],
    )
    if (additionalTypes.length === 0) return null
    if (additionalTypes.length === 1 && additionalTypes[0]) {
      return { kind: 'single', type: additionalTypes[0] }
    } else {
      return { kind: 'multiple', types: additionalTypes }
    }
  }
  return null
})

const label = computed(() => {
  if (props.selfArg == null) return 'Input'
  if (props.selfArg.type === 'known' && props.selfArg.typeInfo?.primaryType.path) {
    return qnLastSegment(props.selfArg.typeInfo.primaryType.path)
  }

  return undefined
})
</script>

<template>
  <div v-if="label" data-testid="component-editor-label">
    <span v-if="additionalTypes?.kind === 'single'" v-text="`${label} & ${additionalTypes.type}`" />
    <template v-else-if="additionalTypes?.kind === 'multiple'">
      <span v-text="`${label} & `" />
      <TooltipTrigger>
        <template #default="triggerProps">
          <span
            class="additionalTypesPlaceholder"
            v-bind="triggerProps"
            v-text="`${additionalTypes.types.length} more`"
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
    <span> Components</span>
  </div>
</template>

<style scoped>
.additionalTypesPlaceholder {
  background-color: rgba(0, 0, 0, 0.1);
  padding: 1px 2px;
  border-radius: 2px;
}
</style>
