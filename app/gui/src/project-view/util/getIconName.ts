import { type NodeId } from '$/providers/openedProjects/graph'
import { type GraphDb } from '$/providers/openedProjects/graph/graphDatabase'
import { evaluationProgress } from '$/providers/openedProjects/project/computedValueRegistry'
import {
  SuggestionKind,
  type SuggestionEntry,
} from '$/providers/openedProjects/suggestionDatabase/entry'
import { type Icon } from '@/util/iconMetadata/iconName'
import { type MethodPointer } from '@/util/methodPointer'
import { type ProjectPath } from '@/util/projectPath'
import { type QualifiedName } from '@/util/qualifiedName'
import { type ToValue } from '@/util/reactivity'
import { computed, toValue, type ComputedRef } from 'vue'
import type { ExternalId } from 'ydoc-shared/yjsModel'
import type { AnyIcon, AnyWidgetIcon } from './icons'

const typeNameToIconLookup: Record<string, Icon> = {
  'Data.Text.Text': 'text_input',
  'Data.Numbers.Integer': 'input_number',
  'Data.Numbers.Float': 'input_number',
  'Data.Array.Array': 'array_new',
  'Data.Vector.Vector': 'array_new',
  'Data.Time.Date.Date': 'calendar',
  'Data.Time.Date_Time.Date_Time': 'calendar',
  'Data.Time.Time_Of_Day.Time_Of_Day': 'time',
}

export const DEFAULT_ICON = 'enso_logo'

/** Returns an icon override for certain standard library types. */
export function typeNameToIcon(typePath: ProjectPath): Icon {
  if (typePath.project === ('Standard.Base' as QualifiedName) && typePath.path != null) {
    return typeNameToIconLookup[typePath.path] ?? DEFAULT_ICON
  } else {
    return DEFAULT_ICON
  }
}

/** Returns an icon override for a suggestion entry kind. */
export function suggestionEntryToIcon(entry: SuggestionEntry) {
  if (entry.iconName) return entry.iconName
  if (entry.kind === SuggestionKind.Local) return 'local_scope2'
  if (entry.kind === SuggestionKind.Module) return 'collection'
  return DEFAULT_ICON
}

/** Returns an icon for a suggestion entry or method call. */
export function displayedIconOf(
  entry?: SuggestionEntry,
  methodCall?: MethodPointer,
  actualType?: ProjectPath,
): Icon {
  if (entry) {
    return suggestionEntryToIcon(entry)
  } else if (!methodCall?.name && actualType) {
    return typeNameToIcon(actualType)
  } else {
    return DEFAULT_ICON
  }
}

/** Returns the icon to show on a component. */
export function iconOfNode(node: NodeId, graphDb: GraphDb) {
  const expressionInfo = graphDb.getExpressionInfo(node)
  const suggestionEntry = graphDb.getNodeMainSuggestion(node)
  const nodeType = graphDb.nodeIdToNode.get(node)?.type
  switch (nodeType) {
    default:
    case 'component':
      return displayedIconOf(
        suggestionEntry,
        expressionInfo?.methodCall?.methodPointer,
        expressionInfo?.typeInfo?.primaryType,
      )
    case 'output':
      return 'data_output'
    case 'input':
      return 'data_input'
  }
}

/**
 * Returns the icon to show on a component, using either the provided base icon or an icon
 * representing its current status.
 */
export function useDisplayedIcon(
  graphDb: ToValue<GraphDb>,
  externalId: ToValue<ExternalId | undefined>,
  baseIcon: ToValue<AnyIcon>,
): {
  displayedIcon: ComputedRef<AnyWidgetIcon>
} {
  return {
    displayedIcon: computed(() =>
      evaluationProgress(toValue(graphDb).getExpressionInfo(toValue(externalId))) == null ?
        toValue(baseIcon)
      : '$evaluating',
    ),
  }
}
