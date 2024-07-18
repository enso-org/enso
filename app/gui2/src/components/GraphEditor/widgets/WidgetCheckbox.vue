<script setup lang="ts">
import { ArgumentNameShownKey } from '@/components/GraphEditor/widgets/WidgetArgumentName.vue'
import CheckboxWidget from '@/components/widgets/CheckboxWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { requiredImportsByFQN } from '@/stores/graph/imports'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { assert } from '@/util/assert'
import { Ast } from '@/util/ast'
import { ArgumentInfoKey } from '@/util/callTree'
import { type Identifier, type QualifiedName } from '@/util/qualifiedName'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const suggestionDb = useSuggestionDbStore()

const trueImport = computed(() =>
  requiredImportsByFQN(
    suggestionDb.entries,
    'Standard.Base.Data.Boolean.Boolean.True' as QualifiedName,
    true,
  ),
)
const falseImport = computed(() =>
  requiredImportsByFQN(
    suggestionDb.entries,
    'Standard.Base.Data.Boolean.Boolean.False' as QualifiedName,
    true,
  ),
)
const value = computed({
  get() {
    return WidgetInput.valueRepr(props.input)?.endsWith('True') ?? false
  },
  set(value) {
    const edit = graph.startEdit()
    const theImport = value ? trueImport.value : falseImport.value
    if (props.input.value instanceof Ast.Ast) {
      const { requiresImport } = setBoolNode(
        edit.getVersion(props.input.value),
        value ? ('True' as Identifier) : ('False' as Identifier),
      )
      if (requiresImport) graph.addMissingImports(edit, theImport)
      props.onUpdate({ edit })
    } else {
      graph.addMissingImports(edit, theImport)
      props.onUpdate({
        edit,
        portUpdate: {
          value: value ? 'True' : 'False',
          origin: props.input.portId,
        },
      })
    }
  },
})

const primary = computed(() => props.nesting < 2)
const argumentName = computed(() => {
  if (ArgumentNameShownKey in props.input) return
  return props.input[ArgumentInfoKey]?.info?.name
})
</script>

<script lang="ts">
function isBoolNode(ast: Ast.Ast) {
  const candidate =
    ast instanceof Ast.PropertyAccess && ast.lhs?.code() === 'Boolean' ? ast.rhs
    : ast instanceof Ast.Ident ? ast.token
    : undefined
  return candidate && ['True', 'False'].includes(candidate.code())
}
function setBoolNode(ast: Ast.Mutable, value: Identifier): { requiresImport: boolean } {
  if (ast instanceof Ast.MutablePropertyAccess) {
    ast.setRhs(value)
    return { requiresImport: false }
  } else {
    assert(ast instanceof Ast.MutableIdent)
    ast.setToken(value)
    return { requiresImport: true }
  }
}

export const widgetDefinition = defineWidget(
  WidgetInput.isAstOrPlaceholder,
  {
    priority: 500,
    score: (props) => {
      if (props.input.value instanceof Ast.Ast && isBoolNode(props.input.value))
        return Score.Perfect
      return props.input.expectedType === 'Standard.Base.Data.Boolean.Boolean' ?
          Score.Good
        : Score.Mismatch
    },
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetCheckbox" :class="{ primary }">
    <span v-if="argumentName" class="name widgetApplyPadding" v-text="argumentName" />
    <CheckboxWidget v-model="value" class="widgetRounded" contenteditable="false" />
  </div>
</template>

<style scoped>
.WidgetCheckbox {
  display: flex;
  flex-direction: row;
  align-items: center;
}

.name {
  color: rgb(255 255 255 / 0.5);
  margin-right: var(--widget-token-pad-unit);
}
</style>
