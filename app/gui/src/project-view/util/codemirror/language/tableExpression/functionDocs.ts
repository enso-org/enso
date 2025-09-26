import TableExpressionFunctionDocs from '@/components/TableExpressionFunctionDocs.vue'
import type { MethodCompletionInfo } from '@/util/codemirror/language/tableExpression/completionData'
import { singleCursorTooltipExtension } from '@/util/codemirror/tooltips/showTooltip'
import { vueTooltipView } from '@/util/codemirror/tooltips/tooltipView'
import { getVueHost, vueHostExt } from '@/util/codemirror/vueHostExt'
import type { EditorState, Extension } from '@codemirror/state'
import type { Tooltip } from '@codemirror/view'
import { completionTypeAt } from 'lezer-enso-table-expr'
import { computed } from 'vue'

function memoizeIf<Value, Args extends unknown[]>(
  getValue: (...args: Args) => Value,
  isEquivalent: (a: Value, b: Value) => boolean,
) {
  let prev: { value: Value } | undefined = undefined
  return (...args: Args) => {
    const newValue = getValue(...args)
    if (prev != null && isEquivalent(newValue, prev.value)) return prev.value
    prev = { value: newValue }
    return newValue
  }
}

/**
 * Wraps a tooltip function to return the same object if the `pos` hasn't changed; this prevents flickering when moving
 * the cursor.
 */
function memoizeTooltip<Args extends unknown[]>(getTooltip: (...args: Args) => Tooltip | null) {
  return memoizeIf(getTooltip, (a, b) => a?.pos === b?.pos)
}

function docsTooltip(
  state: EditorState,
  getMethod: (name: string) => MethodCompletionInfo | undefined,
) {
  const pos = state.selection.ranges[0]!.anchor
  const completion = completionTypeAt(pos, state)
  if (completion?.type !== 'functionInfo') return null
  const vueHost = state.field(getVueHost)
  if (vueHost == null) return null
  const method = getMethod(completion.functionName)
  if (method == null) return null
  const documentation = method.documentation
  return {
    pos: completion.pos,
    create: () => vueTooltipView(vueHost, TableExpressionFunctionDocs, { documentation }),
    clip: false,
  }
}

/**
 * CodeMirror extension adding a tooltip showing the documentation of a function in the table expression language,
 * complementing the autocomplete feature.
 */
export function functionDocs(methods: (() => MethodCompletionInfo[]) | undefined): Extension {
  const methodByName = computed(
    () => new Map<string, MethodCompletionInfo>((methods?.() ?? []).map((m) => [m.name, m])),
  )
  const getMethod = (name: string) => methodByName.value.get(name)
  const getTooltip = memoizeTooltip(docsTooltip)
  const tooltipExt = singleCursorTooltipExtension((state) => getTooltip(state, getMethod))
  return [tooltipExt, vueHostExt]
}
