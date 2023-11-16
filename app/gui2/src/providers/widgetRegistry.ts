import type { GraphDb } from '@/stores/graph/graphDatabase'
import { AstExtended } from '@/util/ast'
import type { SuggestionId } from 'shared/languageServerTypes/suggestions'
import type { ExprId } from 'shared/yjsModel'
import { computed, shallowReactive, type Component } from 'vue'
import { z } from 'zod'
import { createContextStore } from '.'

export type WidgetComponent = Component<{ input: WidgetInput }>

/**
 * Information about an argument that doesn't have an assigned value yet, therefore are not
 * represented in the AST.
 *
 * TODO: Generate placeholders from suggestions and add support for them in various widgets. [#8257]
 */
export class PlaceholderArgument {
  /** The call expression tow which the placeholder is attached. */
  callExpression: ExprId
  /** The suggestion ID pointing to a method with a list of expected arguments. */
  methodId: SuggestionId
  /** The index of relevant argument in the suggestion entry. */
  index: number

  constructor(callExpression: ExprId, methodId: SuggestionId, index: number) {
    this.callExpression = callExpression
    this.methodId = methodId
    this.index = index
  }
}

/**
 * A type representing any kind of input that can have a widget attached to it. This can be either
 * an AST node, or a placeholder argument.
 */
export type WidgetInput = AstExtended | PlaceholderArgument

export function widgetAst(input: WidgetInput): AstExtended | undefined {
  return input instanceof AstExtended ? input : undefined
}

export function widgetArg(input: WidgetInput): PlaceholderArgument | undefined {
  return input instanceof PlaceholderArgument ? input : undefined
}

/**
 * Description of how well a widget matches given input. Used to determine which widget should be
 * used, or whether the applied widget override is valid in given context.
 */
export enum Score {
  /**
   * This widget kind cannot accept the node. It will never be used, even if it was explicitly
   * requested using an override.
   */
  Mismatch,
  /**
   * A bad, but syntactically valid match. Matching widget kind will only be used if it was
   * explicitly requested using an override. Should be the default choice for cases where
   * the node is syntactically valid in this widget's context, but no sensible defaults can
   * be inferred from context.
   */
  OnlyOverride,
  /**
   * A good match, but there might be a better one. one. This widget will be used if there is no
   * better option.
   */
  Good,
  /** Widget matches perfectly and can be used outright, without checking other kinds. */
  Perfect,
}

export interface WidgetProps {
  input: WidgetInput
  config: WidgetConfiguration | undefined
  nesting: number
}

export interface WidgetDefinition {
  /** The priority number determining the order in which the widgets are matched. Smaller numbers
   * have higher priority, and are matched first.
   */
  priority: number
  /**
   * Score how well this widget type matches current {@link WidgetProps}, e.g. checking if AST node
   * or declaration type matches specific patterns. When this method returns
   * {@link Score::Mismatch}, this widget component will not be used, even if its type was requested
   * by an override. The override will be ignored and another best scoring widget will be used.
   */
  match(info: WidgetProps, db: GraphDb): Score
}

/**
 * An external configuration for a widget retreived from the language server.
 *
 * TODO: Actually implement reading dynamic widget configuration. [#8260]
 * The expected configuration type is defined as Enso type `Widget` in the following file:
 * distribution/lib/Standard/Base/0.0.0-dev/src/Metadata.enso
 */
export type WidgetConfiguration = z.infer<typeof widgetConfigurationSchema>
export const widgetConfigurationSchema = z.object({})

export interface WidgetModule {
  default: WidgetComponent
  widgetDefinition: WidgetDefinition
}

function isWidgetModule(module: unknown): module is WidgetModule {
  return (
    typeof module === 'object' &&
    module !== null &&
    'default' in module &&
    'widgetDefinition' in module &&
    isWidgetComponent(module.default) &&
    isWidgetDefinition(module.widgetDefinition)
  )
}

function isWidgetComponent(component: unknown): component is WidgetComponent {
  return typeof component === 'object' && component !== null && 'render' in component
}

function isWidgetDefinition(config: unknown): config is WidgetDefinition {
  return typeof config === 'object' && config !== null && 'priority' in config && 'match' in config
}

export function defineWidget(definition: WidgetDefinition): WidgetDefinition {
  return definition
}

export { injectFn as injectWidgetRegistry, provideFn as provideWidgetRegistry }
const { provideFn, injectFn } = createContextStore(
  'Widget registry',
  (db: GraphDb) => new WidgetRegistry(db),
)

export class WidgetRegistry {
  loadedModules: WidgetModule[] = shallowReactive([])
  sortedModules = computed(() => {
    return [...this.loadedModules].sort(
      (a, b) => a.widgetDefinition.priority - b.widgetDefinition.priority,
    )
  })
  constructor(private db: GraphDb) {}

  loadBuiltins() {
    const bulitinWidgets = import.meta.glob('@/components/GraphEditor/widgets/*.vue')
    for (const [path, asyncModule] of Object.entries(bulitinWidgets)) {
      this.loadAndCheckWidgetModule(asyncModule(), path)
    }
  }

  async loadAndCheckWidgetModule(asyncModule: Promise<unknown>, path: string) {
    const m = await asyncModule
    if (isWidgetModule(m)) this.registerWidgetModule(m)
    else console.error('Invalid widget module:', path, m)
  }

  /**
   * Register a known loaded widget module. Once registered, the widget will take part in widget
   * selection process. Caution: registering a new widget module will trigger re-matching of all
   * widgets on all nodes in the scene, which can be expensive.
   */
  registerWidgetModule(module: WidgetModule) {
    this.loadedModules.push(module)
  }

  /**
   * Select a type of widget to use for given widget input (e.g. AST node). The selection is based
   * on the widget's `match` function, which returns a score indicating how well the widget matches
   * the input. The widget with the highest score and priority is selected. Widget kinds that are
   * present in `alreadyUsed` set are always skipped.
   */
  select(props: WidgetProps, alreadyUsed?: Set<WidgetComponent>): WidgetComponent | undefined {
    // The type and score of the best widget found so far.
    let best: WidgetComponent | undefined = undefined
    let bestScore = Score.Mismatch

    // Iterate over all loaded widget kinds in order of decreasing priority.
    for (const module of this.sortedModules.value) {
      // Skip matching widgets that are declared as already used.
      if (alreadyUsed && alreadyUsed.has(module.default)) continue

      // Perform a match and update the best widget if the match is better than the previous one.
      const score = module.widgetDefinition.match(props, this.db)
      // If we found a perfect match, we can return immediately, as there can be no better match.
      if (score === Score.Perfect) return module.default
      if (score > bestScore) {
        bestScore = score
        best = module.default
      }
    }
    // Once we've checked all widgets, return the best match found, if any.
    return best
  }
}

// TODO: add tests for select
