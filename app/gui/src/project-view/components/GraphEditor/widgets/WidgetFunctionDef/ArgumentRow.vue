<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { WidgetInput } from '@/providers/widgetRegistry'
import { computed } from 'vue'
import { ArgumentDefinition, ConcreteRefs } from 'ydoc-shared/ast'
import { isSome, mapOrUndefined } from 'ydoc-shared/util/data/opt'

const { definition } = defineProps<{
  definition: ArgumentDefinition<ConcreteRefs>
}>()

const allWidgets = computed(() =>
  [
    mapOrUndefined(definition.open?.node, WidgetInput.FromAst),
    mapOrUndefined(definition.open2?.node, WidgetInput.FromAst),
    mapOrUndefined(definition.suspension?.node, WidgetInput.FromAst),
    mapOrUndefined(definition.pattern?.node, WidgetInput.FromAst),
    mapOrUndefined(definition.type?.operator.node, WidgetInput.FromAst),
    mapOrUndefined(definition.type?.type.node, WidgetInput.FromAst),
    mapOrUndefined(definition.close2?.node, WidgetInput.FromAst),
    mapOrUndefined(definition.defaultValue?.equals.node, WidgetInput.FromAst),
    mapOrUndefined(definition.defaultValue?.expression.node, WidgetInput.FromAst),
    mapOrUndefined(definition.close?.node, WidgetInput.FromAst),
  ].flatMap((v, key) => (isSome(v) ? ([[key, v]] as const) : [])),
)
</script>

<template>
  <div class="ArgumentRow widgetResetPadding widgetRounded">
    <SvgIcon name="grab" />
    <NodeWidget v-for="[key, widget] of allWidgets" :key="key" :input="widget" />
  </div>
</template>

<style scoped>
.ArgumentRow {
  display: flex;
  flex-direction: row;
  place-items: center;
  overflow-x: clip;
  margin-left: 24px;

  .SvgIcon {
    color: color-mix(in srgb, currentColor, transparent 50%);
    margin-right: 4px;
  }
}
</style>
