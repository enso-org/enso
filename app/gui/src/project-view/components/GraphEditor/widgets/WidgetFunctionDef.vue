<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import ArgumentRow from '@/components/GraphEditor/widgets/WidgetFunctionDef/ArgumentRow.vue'
import { FunctionName } from '@/components/GraphEditor/widgets/WidgetFunctionName.vue'
import { DisplayIcon } from '@/components/GraphEditor/widgets/WidgetIcon.vue'
import DraggableList from '@/components/widgets/DraggableList.vue'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { DocumentationData } from '@/stores/suggestionDatabase/documentation'
import { Ast } from '@/util/ast'
import { type MethodPointer } from '@/util/methodPointer'
import { computed, Ref } from 'vue'
import { BodyBlock, identifier, MutableModule } from 'ydoc-shared/ast'

const { input, onUpdate } = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()

const funcIcon = computed(() => {
  return input[FunctionInfoKey]?.docsData.value?.iconName ?? 'enso_logo'
})

function addArgument(): Ast.ArgumentDefinition<Ast.ConcreteRefs> {
  throw 'unimplemented'
}

const argumentsList = computed({
  get() {
    return input.value.argumentDefinitions
  },
  set(value) {
    const edit = graph.startEdit()
    const ast = edit.getVersion(input.value)
    ast.setArgumentDefinitionsCopy(value)
    console.log(value)
    onUpdate({ edit, directInteraction: true })
  },
})

const serializedFuncIdentifier = identifier('serialized')!

function serializeArgument(arg: Ast.ArgumentDefinition<Ast.ConcreteRefs>): string {
  const edit = MutableModule.Transient()
  const tempFuncDef = Ast.FunctionDef.new(
    serializedFuncIdentifier,
    // Can be treated as "owned" here, because we serialize it to code and discard the edit anyway.
    [],
    BodyBlock.new([], edit),
    { edit },
  )
  tempFuncDef.setArgumentDefinitionsCopy([arg])
  return tempFuncDef.code()
}
function deserializeArgument(
  payload: string,
): Ast.ArgumentDefinition<Ast.ConcreteRefs> | undefined {
  return Ast.FunctionDef.tryParse(payload)?.argumentDefinitions[0]
}

const funcNameInput = computed(() => {
  const nameAst = input.value.name
  const widgetInput = WidgetInput.FromAst(nameAst)
  widgetInput[DisplayIcon] = {
    icon: funcIcon.value,
    allowChoice: true,
    showContents: true,
  }

  const methodPointer = input[FunctionInfoKey]?.methodPointer
  if (methodPointer) {
    widgetInput[FunctionName] = {
      editableNameExpression: nameAst.externalId,
      methodPointer,
    }
  }
  return widgetInput
})
</script>

<template>
  <div class="WidgetFunctionDef">
    <NodeWidget :input="funcNameInput" />
    <div class="FunctionDefArguments">
      <DraggableList
        axis="y"
        showHandles
        :modelValue="argumentsList"
        :newItem="addArgument"
        :toDragPayload="serializeArgument"
        :fromDragPayload="deserializeArgument"
      >
        <template #default="{ item }">
          <ArgumentRow :definition="item" />
        </template>
      </DraggableList>
    </div>
  </div>
</template>

<script lang="ts">
export const FunctionInfoKey: unique symbol = Symbol.for('WidgetInput:FunctionInfoKey')
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [FunctionInfoKey]?: {
      methodPointer: MethodPointer
      docsData: Ref<DocumentationData | undefined>
    }
  }
}
export const widgetDefinition = defineWidget(
  WidgetInput.astMatcher(Ast.FunctionDef),
  {
    priority: 999,
    score: Score.Perfect,
  },
  import.meta.hot,
)
</script>

<style scoped>
.WidgetFunctionDef {
  display: flex;
  flex-direction: column;
  align-items: flex-start;
}

.FunctionDefArguments {
  margin-left: 24px;
}
</style>
