<script setup lang="ts">
import AutoSizedInput from '@/components/widgets/AutoSizedInput.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useProjectStore } from '@/stores/project'
import { Ast } from '@/util/ast'
import { Err, Ok, type Result } from '@/util/data/result'
import { useToast } from '@/util/toast'
import { computed, ref, watchEffect } from 'vue'
import { PropertyAccess } from 'ydoc-shared/ast'
import type { ExpressionId } from 'ydoc-shared/languageServerTypes'
import NodeWidget from '../NodeWidget.vue'

const props = defineProps(widgetProps(widgetDefinition))
const displayedName = ref(props.input.value.code())

const project = useProjectStore()
const renameError = useToast.error()

const thisArg = computed(() =>
  props.input.value instanceof PropertyAccess ? props.input.value.lhs : undefined,
)
const operator = computed(() =>
  props.input.value instanceof PropertyAccess ? props.input.value.operator : undefined,
)
const name = computed(() =>
  props.input.value instanceof PropertyAccess ? props.input.value.rhs : props.input.value,
)
watchEffect(() => (displayedName.value = name.value.code()))

function newNameAccepted(newName: string | undefined) {
  if (!newName) {
    displayedName.value = name.value.code()
  } else {
    renameFunction(newName).then((result) => result.ok || renameError.reportError(result.error))
  }
}

async function renameFunction(newName: string): Promise<Result> {
  if (!project.modulePath?.ok) return project.modulePath ?? Err('Unknown module Path')
  const refactorResult = await project.lsRpcConnection.renameSymbol(
    project.modulePath.value,
    props.input[FunctionName].editableName,
    newName,
  )
  if (!refactorResult.ok) return refactorResult
  return Ok()
}
</script>

<script lang="ts">
export const FunctionName: unique symbol = Symbol.for('WidgetInput:FunctionName')
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [FunctionName]?: {
      /**
       * Id of expression which is accepted by Language Server's
       * [`refactoring/renameSymbol` method](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#refactoringrenamesymbol)
       */
      editableName: ExpressionId
    }
  }
}

function isFunctionName(
  input: WidgetInput,
): input is WidgetInput & { value: Ast.Ast; [FunctionName]: { editableName: ExpressionId } } {
  return WidgetInput.isAst(input) && FunctionName in input
}

export const widgetDefinition = defineWidget(
  isFunctionName,
  {
    priority: -20,
    score: Score.Perfect,
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetFunctionName">
    <NodeWidget v-if="thisArg" :input="WidgetInput.FromAst(thisArg)" />
    <NodeWidget v-if="operator" :input="WidgetInput.FromAst(operator)" />
    <AutoSizedInput
      v-model="displayedName"
      class="FunctionName widgetApplyPadding"
      @change="newNameAccepted"
      @pointerdown.stop
      @click.stop
      @keydown.enter.stop
      @keydown.arrow-left.stop
      @keydown.arrow-right.stop
    />
  </div>
</template>

<style scoped>
.WidgetFunctionName {
  display: flex;
  flex-direction: row;
  align-items: center;
}
</style>
