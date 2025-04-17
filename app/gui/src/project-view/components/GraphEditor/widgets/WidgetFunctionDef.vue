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
import { isDef } from '@vueuse/core'
import { computed, Ref } from 'vue'
import { newArgumentDefinition } from 'ydoc-shared/ast'
import { assertUnreachable } from 'ydoc-shared/util/assert'

const { input, onUpdate } = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()

const funcIcon = computed(() => {
  return input[FunctionInfoKey]?.docsData.value?.iconName ?? 'enso_logo'
})

function doEdit(editFn: (ast: Ast.MutableFunctionDef) => void) {
  const edit = graph.startEdit()
  editFn(edit.getVersion(input.value))
  onUpdate({ edit, directInteraction: true })
}

function handleAddItem() {
  if (input.editHandler?.addItem()) return
  doEdit((ast) =>
    ast.pushArgumentDefinitions(
      newArgumentDefinition(
        nextArgName(ast.argumentDefinitions.map((a) => a.pattern.node.code()).filter(isDef)),
      ),
    ),
  )
}

function nextArgName(existingNames: string[]): string {
  for (let i = 1; ; i++) {
    const proposedName = `arg${i}`
    if (!existingNames.includes(proposedName)) return proposedName
  }
  assertUnreachable()
}

function handleRemove(index: number) {
  doEdit((ast) => ast.spliceArgumentDefinitions(index, 1))
}

function handleReorder(oldIndex: number, newIndex: number) {
  doEdit((ast) => ast.moveArgumentDefinitions(oldIndex, newIndex))
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
        :items="input.value.argumentDefinitions"
        @addItem="handleAddItem"
        @remove="handleRemove"
        @reorder="handleReorder"
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
