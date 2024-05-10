<script setup lang="ts">
import AutoSizedInput from '@/components/widgets/AutoSizedInput.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useProjectStore } from '@/stores/project'
import type { SuggestionEntry } from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import { Err, Ok, type Result } from '@/util/data/result'
import { useToast } from '@/util/toast'
import type { ExpressionId } from 'shared/languageServerTypes'
import { ref, watchEffect } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const name = ref(props.input.value.code())

const project = useProjectStore()
const renameError = useToast.error()

watchEffect(() => (name.value = props.input.value.code()))

function newNameAccepted(name: string | undefined) {
  if (!name) return
  renameFunction(name).then((result) => {
    if (!result.ok) {
      // TODO[ao]: method for this:
      const msg = result.error.message('Cannot rename function')
      renameError.show(msg)
      console.error(msg)
    }
  })
}

async function renameFunction(newName: string): Promise<Result> {
  if (!project.modulePath?.ok) return project.modulePath ?? Err('Unknown module Path')
  const refactorResult = await project.lsRpcConnection.renameSymbol(
    project.modulePath.value,
    props.input[FunctionName].editableName,
    newName,
  )
  if (!refactorResult.ok) return refactorResult
  name.value = refactorResult.value.newName
  return Ok()
}
</script>

<script lang="ts">
// TODO[ao]: Rename?
export const FunctionName: unique symbol = Symbol('FunctionName')
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [FunctionName]?: {
      calledFunction: SuggestionEntry
      editableName: ExpressionId
    }
  }
}

function isFunctionName(
  input: WidgetInput,
): input is WidgetInput & { value: Ast.Ast; [FunctionName]: SuggestionEntry } {
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
  <AutoSizedInput v-model="name" class="FunctionName" @change="newNameAccepted" />
</template>

<style scoped>
.FunctionName {
  display: inline-block;
  vertical-align: middle;
  white-space: pre;
  color: rgb(255 255 255 / 0.33);

  &.Ident,
  &.TextSection,
  &.Digits {
    color: white;
  }

  &.TextSection,
  &.Digits {
    font-weight: bold;
  }
}
</style>
