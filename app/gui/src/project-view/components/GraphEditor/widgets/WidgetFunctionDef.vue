<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import ArgumentRow from '@/components/GraphEditor/widgets/WidgetFunctionDef/ArgumentRow.vue'
import { FunctionName } from '@/components/GraphEditor/widgets/WidgetFunctionName.vue'
import { DisplayIcon } from '@/components/GraphEditor/widgets/WidgetIcon.vue'
import DraggableList from '@/components/widgets/DraggableList.vue'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { DocumentationData } from '@/stores/suggestionDatabase/documentation'
import { Ast } from '@/util/ast'
import { type MethodPointer } from '@/util/methodPointer'
import { computed, Ref } from 'vue'

const { input, onUpdate } = defineProps(widgetProps(widgetDefinition))

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
    // This doesn't preserve AST identities, because the values are not `Ast.Owned`.
    // Getting/setting an Array is incompatible with ideal synchronization anyway;
    // `DraggableList` needs to operate on the `Ast.Vector` for edits to be merged as `Y.Array` operations.
    input.value.printSubtree
    const newAst = Ast.Vector.build({ value }, (element, tempModule) => tempModule.copy(element))
    onUpdate({
      portUpdate: { value: newAst, origin: input.portId },
      directInteraction: true,
    })
  },
})

function serializeArgument(arg: Ast.ArgumentDefinition<Ast.ConcreteRefs>): string {
  // return arg.code()
  throw 'unimplemented'
}
function deserializeArgument(
  payload: string,
): Ast.ArgumentDefinition<Ast.ConcreteRefs> | undefined {
  throw 'unimplemented'
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
      <template v-for="(definition, i) in input.value.argumentDefinitions" :key="i">
        <DraggableList
          axis="y"
          showHandles
          :modelValue="input.value.argumentDefinitions"
          :newItem="addArgument"
          :toDragPayload="serializeArgument"
          :fromDragPayload="deserializeArgument"
        >
          <template #default="{ item }">
            <ArgumentRow :definition="item" />
          </template>
        </DraggableList>
      </template>
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
