import { useGraphStore, useProjectStore } from '$/components/WithCurrentProject.vue'
import { type NodeId } from '$/providers/openedProjects/graph/graphDatabase'
import { TypeInfo } from '$/providers/openedProjects/project/computedValueRegistry'
import type { NodeVisualizationConfiguration } from '$/providers/openedProjects/project/executionContext'
import LoadingErrorVisualization from '@/components/visualizations/LoadingErrorVisualization.vue'
import LoadingVisualization from '@/components/visualizations/LoadingVisualization.vue'
import type { ToolbarItem } from '@/components/visualizations/toolbar'
import {
  DEFAULT_VISUALIZATION_CONFIGURATION,
  DEFAULT_VISUALIZATION_IDENTIFIER,
  useVisualizationStore,
  type VisualizationDataSource,
} from '@/stores/visualization'
import type { Visualization } from '@/stores/visualization/runtimeTypes'
import { Ast } from '@/util/ast'
import { toError } from '@/util/data/error'
import { ProjectPath } from '@/util/projectPath'
import type { ToValue } from '@/util/reactivity'
import { computedAsync } from '@vueuse/core'
import type { Opt } from 'enso-common/src/utilities/data/opt'
import type { Result } from 'enso-common/src/utilities/data/result'
import {
  computed,
  onErrorCaptured,
  ref,
  shallowRef,
  toValue,
  watch,
  watchEffect,
  type ShallowRef,
} from 'vue'
import { isIdentifier } from 'ydoc-shared/ast'
import type { VisualizationIdentifier } from 'ydoc-shared/yjsModel'

/** Used for testing. */
export type RawDataSource = { type: 'raw'; data: any }

export interface UseVisualizationDataOptions {
  selectedVis: ToValue<Opt<VisualizationIdentifier>>
  /** @deprecated use typeInfo instead */
  typename: ToValue<ProjectPath | undefined>
  typeinfo: ToValue<TypeInfo | undefined>
  dataSource: ToValue<VisualizationDataSource | RawDataSource | undefined>
}

/**
 * Visualization data composable for Visualization component.
 *
 * This composable manages picking the proper visualization component, attaching engine's
 * visualization to get input data, and updating the preprocessor if requested.
 *
 * TODO[ao]: Docs about returned refs and functions.
 */
export function useVisualizationData({
  selectedVis,
  dataSource,
  typename,
  typeinfo,
}: UseVisualizationDataOptions) {
  const visPreprocessor = ref(DEFAULT_VISUALIZATION_CONFIGURATION)
  const vueError = ref<Error>()

  const projectStore = useProjectStore()
  const visualizationStore = useVisualizationStore()
  const graph = useGraphStore()

  // Flag used to prevent rendering the visualization with a stale preprocessor while the new preprocessor is being
  // prepared asynchronously.
  const preprocessorLoading = ref(false)

  const configForGettingDefaultVisualization = computed<NodeVisualizationConfiguration | undefined>(
    () => {
      if (toValue(selectedVis)) return
      const dataSourceValue = toValue(dataSource)
      if (dataSourceValue?.type !== 'node') return
      return {
        visualizationModule: 'Standard.Visualization.Helpers',
        expression: 'a -> a.default_visualization.to_js_object.to_json',
        expressionId: dataSourceValue.nodeId,
      }
    },
  )

  const defaultVisualizationRaw = projectStore.useVisualizationData(
    configForGettingDefaultVisualization,
  ) as ShallowRef<Result<{ library: { name: string } | null; name: string } | undefined>>

  const defaultVisualizationForCurrentNodeSource = computed<VisualizationIdentifier | undefined>(
    () => {
      const raw = defaultVisualizationRaw.value
      if (!raw?.ok || !raw.value || !raw.value.name) return
      return {
        name: raw.value.name,
        module:
          raw.value.library == null ?
            { kind: 'Builtin' }
          : { kind: 'Library', name: raw.value.library.name },
      }
    },
  )

  const executeExpression = async (
    expressionFunction: (nodeIdentifier: string) => Ast.Owned<Ast.Expression>,
    timeoutMs?: number,
  ) => {
    const dataSourceValue = toValue(dataSource)
    if (dataSourceValue?.type !== 'node') return

    const graphDb = graph.db
    const nodeFirstOutputPort = graphDb.getNodeFirstOutputPort(dataSourceValue.nodeId as NodeId)
    const identifier = graphDb.getOutputPortIdentifier(nodeFirstOutputPort)
    if (identifier === undefined) return

    const contextId =
      dataSourceValue.nodeId &&
      graphDb.nodeIdToNode.get(dataSourceValue.nodeId as NodeId)?.outerAst.externalId
    if (contextId === undefined) return

    const expression = expressionFunction(identifier)
    return timeoutMs ?
        await projectStore.queuedExecuteExpression(contextId, expression.code(), timeoutMs)
      : await projectStore.queuedExecuteExpression(contextId, expression.code())
  }

  const currentVisualization = computed(() => {
    const selectedTypeValue = toValue(selectedVis)
    if (selectedTypeValue) return selectedTypeValue
    if (defaultVisualizationForCurrentNodeSource.value)
      return defaultVisualizationForCurrentNodeSource.value
    const [id] = visualizationStore.byType(toValue(typeinfo), toValue(typename))
    return id ?? DEFAULT_VISUALIZATION_IDENTIFIER
  })

  const visualization = shallowRef<Visualization>()

  onErrorCaptured((error) => {
    vueError.value = error
    return false
  })

  const nodeVisualizationData = projectStore.useVisualizationData(() => {
    const dataSourceValue = toValue(dataSource)
    if (dataSourceValue?.type !== 'node') return
    return {
      ...visPreprocessor.value,
      expressionId: dataSourceValue.nodeId,
    }
  })

  const expressionVisualizationData = computedAsync(
    () => {
      try {
        const dataSourceValue = toValue(dataSource)
        if (dataSourceValue?.type !== 'expression') return
        if (preprocessorLoading.value) return
        const preprocessor = visPreprocessor.value
        const args = preprocessor.positionalArgumentsExpressions
        const tempModule = Ast.MutableModule.Transient()
        const preprocessorModule = Ast.parseExpression(
          preprocessor.visualizationModule,
          tempModule,
        )!
        // TODO[ao]: it work with builtin visualization, but does not work in general case.
        // Tracked in https://github.com/orgs/enso-org/discussions/6832#discussioncomment-7754474.
        if (!isIdentifier(preprocessor.expression)) {
          console.error(`Unsupported visualization preprocessor definition`, preprocessor)
          return
        }
        const preprocessorQn = Ast.PropertyAccess.new(
          tempModule,
          preprocessorModule,
          preprocessor.expression,
        )
        const preprocessorInvocation = Ast.App.PositionalSequence(preprocessorQn, [
          Ast.Wildcard.new(tempModule),
          ...args.map((arg) => Ast.Group.new(tempModule, Ast.parseExpression(arg, tempModule)!)),
        ])
        const rhs = Ast.parseExpression(dataSourceValue.expression, tempModule)!
        const expression = Ast.OprApp.new(tempModule, preprocessorInvocation, '<|', rhs)
        return projectStore.executeExpression(dataSourceValue.contextId, expression.code())
      } catch (e) {
        console.error(e)
        throw e
      }
    },
    undefined,
    { onError: console.error },
  )

  const effectiveVisualizationData = computed(() => {
    const dataSourceValue = toValue(dataSource)
    const name = currentVisualization.value?.name
    if (dataSourceValue?.type === 'raw') return dataSourceValue.data
    if (vueError.value) return { name, error: vueError.value }
    const visualizationData = nodeVisualizationData.value ?? expressionVisualizationData.value
    if (!visualizationData) return
    if (visualizationData.ok) return visualizationData.value
    else return { name, error: new Error(`${visualizationData.error.payload}`) }
  })

  function updatePreprocessor(
    visualizationModule: string,
    expression: string,
    ...positionalArgumentsExpressions: string[]
  ) {
    visPreprocessor.value = { visualizationModule, expression, positionalArgumentsExpressions }
  }

  function switchToDefaultPreprocessor() {
    visPreprocessor.value = DEFAULT_VISUALIZATION_CONFIGURATION
  }

  watch(
    () => [currentVisualization.value, visualization.value],
    () => (vueError.value = undefined),
  )

  watchEffect(async () => {
    preprocessorLoading.value = true
    if (currentVisualization.value == null) return
    visualization.value = undefined
    try {
      const module = await visualizationStore.get(currentVisualization.value).value
      if (module) {
        if (module.defaultPreprocessor != null) {
          updatePreprocessor(...module.defaultPreprocessor)
        } else {
          switchToDefaultPreprocessor()
        }
        visualization.value = module.default
      } else {
        switch (currentVisualization.value.module.kind) {
          case 'Builtin': {
            vueError.value = new Error(
              `The builtin visualization '${currentVisualization.value.name}' was not found.`,
            )
            break
          }
          case 'CurrentProject': {
            vueError.value = new Error(
              `The visualization '${currentVisualization.value.name}' was not found in the current project.`,
            )
            break
          }
          case 'Library': {
            vueError.value = new Error(
              `The visualization '${currentVisualization.value.name}' was not found in the library '${currentVisualization.value.module.name}'.`,
            )
            break
          }
        }
      }
    } catch (caughtError) {
      vueError.value = toError(caughtError)
    }
    preprocessorLoading.value = false
  })

  const allVisualizations = computed(() =>
    Array.from(visualizationStore.byType(toValue(typeinfo), toValue(typename))),
  )

  const effectiveVisualization = computed(() => {
    if (
      vueError.value ||
      (nodeVisualizationData.value && !nodeVisualizationData.value.ok) ||
      (expressionVisualizationData.value && !expressionVisualizationData.value.ok)
    ) {
      return LoadingErrorVisualization
    }
    if (!visualization.value || effectiveVisualizationData.value == null) {
      return LoadingVisualization
    }
    return visualization.value
  })

  // Visualization-provided configuration
  const toolbarOverlay = ref(false)
  const toolbarDefinition = shallowRef<ToValue<Readonly<ToolbarItem[]>>>()
  watch(effectiveVisualization, () => {
    toolbarOverlay.value = false
    toolbarDefinition.value = undefined
  })

  return {
    effectiveVisualization,
    effectiveVisualizationData,
    updatePreprocessor,
    allVisualizations,
    currentVisualization,
    setToolbarDefinition: (definition: ToValue<Readonly<ToolbarItem[]>>) =>
      (toolbarDefinition.value = definition),
    visualizationDefinedToolbar: computed(() => toValue(toolbarDefinition.value)),
    toolbarOverlay,
    executeExpression,
  }
}
