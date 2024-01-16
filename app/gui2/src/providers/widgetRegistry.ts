import { createContextStore } from '@/providers'
import type { PortId } from '@/providers/portInfo'
import type { WidgetConfiguration } from '@/providers/widgetRegistry/configuration'
import type { GraphDb } from '@/stores/graph/graphDatabase'
import type { RequiredImport } from '@/stores/graph/imports.ts'
import type { Typename } from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import { MutableModule, type Owned } from '@/util/ast/abstract.ts'
import { computed, shallowReactive, type Component, type PropType } from 'vue'

export type WidgetComponent<T extends WidgetInput> = Component<WidgetProps<T>>

export namespace WidgetInput {
  export function FromAst(ast: Ast.Ast | Ast.Token): WidgetInput {
    return {
      portId: ast.exprId,
      value: ast,
    }
  }

  export function valueRepr(input: WidgetInput): string | undefined {
    if (typeof input.value === 'string') return input.value
    else return input.value?.code()
  }

  /** Check if input is placeholder, i.e. it does not represent any Ast node. */
  export function isPlaceholder(
    input: WidgetInput,
  ): input is WidgetInput & { value: string | undefined } {
    return input.value == null || typeof input.value === 'string'
  }

  /** Match input against a specific AST node type. */
  export function astMatcher<T extends Ast.Ast>(nodeType: new (...args: any[]) => T) {
    return (input: WidgetInput): input is WidgetInput & { value: T } =>
      input.value instanceof nodeType
  }

  export function isAst(input: WidgetInput): input is WidgetInput & { value: Ast.Ast } {
    return input.value instanceof Ast.Ast
  }

  /** Rule out token inputs. */
  export function isAstOrPlaceholder(
    input: WidgetInput,
  ): input is WidgetInput & { value: Ast.Ast | string | undefined } {
    return isPlaceholder(input) || isAst(input)
  }

  export function isToken(input: WidgetInput): input is WidgetInput & { value: Ast.Token } {
    return input.value instanceof Ast.Token
  }

  export function isFunctionCall(
    input: WidgetInput,
  ): input is WidgetInput & { value: Ast.App | Ast.Ident | Ast.OprApp } {
    return (
      input.value instanceof Ast.App ||
      input.value instanceof Ast.Ident ||
      input.value instanceof Ast.OprApp
    )
  }
}

/**
 * Widget instance input.
 *
 * This input is first used to decide which widget should be instantiated, then by the widget
 * instance to show proper value.
 *
 * This interface can be extended by specific widgets to add their data to be propagated to
 * subwidgets - to avoid breaking other widgets, additional property should be optional indexed
 * by symbols, for example:
 *
 * ```ts
 * export const ArgumentApplicationKey: unique symbol = Symbol('ArgumentApplicationKey')
 * export const ArgumentInfoKey: unique symbol = Symbol('ArgumentInfoKey')
 * declare module '@/providers/widgetRegistry' {
 *   export interface WidgetInput {
 *     [ArgumentApplicationKey]?: ArgumentApplication
 *     [ArgumentInfoKey]?: {
 *       appKind: ApplicationKind
 *       info: SuggestionEntryArgument | undefined
 *     }
 *   }
 * }
 * ```
 */
export interface WidgetInput {
  /**
   * Port identification. See {@link PortId}
   *
   * Also, used as usage key (see {@link usageKeyForInput})
   */
  portId: PortId
  /**
   * An expected widget value. If Ast.Ast or Ast.Token, the widget represents an existing part of
   * code. If string, it may be e.g. a default value of an argument.
   */
  value: Ast.Ast | Ast.Token | string | undefined
  /** An expected type which widget should set. */
  expectedType?: Typename | undefined
  /** Configuration provided by engine. */
  dynamicConfig?: WidgetConfiguration | undefined
  /** Force the widget to be a connectible port. */
  forcePort?: boolean
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
   * A good match, but there might be a better one. This widget will be used if there is no better
   * option.
   */
  Good,
  /** Widget matches perfectly and can be used outright, without checking other kinds. */
  Perfect,
}

export interface WidgetProps<T> {
  input: T
  nesting: number
}

/**
 * Information about widget update.
 *
 * When widget want's to change its value, it should emit this with `portUpdate` set (as their
 * port may not represent any existing AST node) with `edit` containing any additional modifications
 * (like inserting necessary imports).
 *
 * The handlers interested in a specific port update should apply it using received edit. The edit
 * is committed in {@link NodeWidgetTree}.
 */
export interface WidgetUpdate {
  edit: MutableModule
  portUpdate?: {
    value: Owned<Ast.Ast> | string | undefined
    origin: PortId
  }
}

/**
 * Create Vue props definition for a widget component. This cannot be done automatically by using
 * typed `defineProps`, because vue compiler is not able to resolve conditional types. As a
 * workaround, the runtime prop information is specified manually, and the inferred `T: WidgetInput`
 * type is provided through `PropType`.
 */
export function widgetProps<T extends WidgetInput>(_def: WidgetDefinition<T>) {
  return {
    input: {
      type: Object as PropType<T>,
      required: true,
    },
    nesting: { type: Number, required: true },
    onUpdate: {
      type: Function as PropType<(update: WidgetUpdate) => void>,
      required: true,
    },
  } as const
}

type InputMatcherFn<T extends WidgetInput> = (input: WidgetInput) => input is T
type InputMatcherSymbol<T extends WidgetInput> = symbol & keyof T
type InputMatcher<T extends WidgetInput> = InputMatcherSymbol<T> | InputMatcherFn<T>

type InputTy<M> = M extends (infer T)[]
  ? InputTy<T>
  : M extends InputMatcherFn<infer T>
  ? T
  : M extends symbol & keyof WidgetInput
  ? WidgetInput & { [S in M]: Required<WidgetInput>[S] }
  : never

export interface WidgetOptions<T extends WidgetInput> {
  /** The priority number determining the order in which the widgets are matched. Smaller numbers
   * have higher priority, and are matched first.
   */
  priority: number
  /**
   * Score how well this widget type matches current {@link WidgetProps}, e.g. checking if AST node
   * or declaration type matches specific patterns. When this method returns
   * {@link Score::Mismatch}, this widget component will not be used.
   *
   * When a static score value is provided, it will be used for all inputs that pass the input
   * filter. When not provided, the widget will be considered a perfect match for all inputs that
   * pass the input filter.
   */
  score?: ((info: WidgetProps<T>, db: GraphDb) => Score) | Score
  // A list of widget kinds that will be prevented from being used on the same node as this widget,
  // once this widget is used.
  prevent?: WidgetComponent<any>[]
}

export interface WidgetDefinition<T extends WidgetInput> {
  /** The priority number determining the order in which the widgets are matched. Smaller numbers
   * have higher priority, and are matched first.
   */
  priority: number
  /**
   * Filter the widget input type to only accept specific types of input. The declared widget props
   * will depend on the type of input accepted by this filter. Only widgets that pass this filter
   * will be scored and potentially used.
   */
  match: (input: WidgetInput) => input is T
  /**
   * Score how well this widget type matches current {@link WidgetProps}, e.g. checking if AST node
   * or declaration type matches specific patterns. When this method returns
   * {@link Score::Mismatch}, this widget component will not be used.
   *
   * When a static score value is provided, it will be used for all inputs that pass the input
   * filter. When not provided, the widget will be considered a perfect match for all inputs that
   */
  score: (props: WidgetProps<T>, db: GraphDb) => Score
  prevent: WidgetComponent<any>[] | undefined
}

export interface WidgetModule<T extends WidgetInput> {
  default: WidgetComponent<T>
  widgetDefinition: WidgetDefinition<T>
}

function isWidgetModule(module: unknown): module is WidgetModule<any> {
  return (
    typeof module === 'object' &&
    module !== null &&
    'default' in module &&
    'widgetDefinition' in module &&
    isWidgetComponent(module.default) &&
    isWidgetDefinition(module.widgetDefinition)
  )
}

function isWidgetComponent(component: unknown): component is WidgetComponent<any> {
  return typeof component === 'object' && component !== null
}

function isWidgetDefinition(config: unknown): config is WidgetDefinition<any> {
  return typeof config === 'object' && config !== null && 'priority' in config && 'match' in config
}

/**
 *
 * @param matchInputs Filter the widget input to only accept specific types of input. The
 * declared widget props will depend on the type of input accepted by this filter. Only widgets that
 * pass this filter will be scored and potentially used.
 *
 * The filter can be a type guard function `(input: WidgetInput) => input is MyType`, a symbol representing
 * required property `MySymbol`, or an array combining any of the above. When an array is provided, the
 * widget will match if any of the filters in the array match.
 */
export function defineWidget<M extends InputMatcher<any> | InputMatcher<any>[]>(
  matchInputs: M,
  definition: WidgetOptions<InputTy<M>>,
): WidgetDefinition<InputTy<M>> {
  let score: WidgetDefinition<InputTy<M>>['score']
  if (typeof definition.score === 'function') {
    score = definition.score
  } else {
    const staticScore = definition.score ?? Score.Perfect
    score = () => staticScore
  }

  return {
    priority: definition.priority,
    match: makeInputMatcher<InputTy<M>>(matchInputs),
    score,
    prevent: definition.prevent,
  }
}

function makeInputMatcher<T extends WidgetInput>(
  matcher: InputMatcher<T> | InputMatcher<T>[],
): (input: WidgetInput) => input is T {
  if (Array.isArray(matcher)) {
    const matchers = matcher.map(makeInputMatcher)
    return (input: WidgetInput): input is T => matchers.some((f) => f(input))
  } else if (typeof matcher === 'function') {
    // When matcher is a function with assignable prototype, it must be a type guard.
    return matcher as (input: WidgetInput) => input is T
  } else if (typeof matcher === 'symbol') {
    return (input: WidgetInput): input is T => matcher in input
  } else {
    throw new Error('Invalid widget input matcher definiton: ' + matcher)
  }
}

export { injectFn as injectWidgetRegistry, provideFn as provideWidgetRegistry }
const { provideFn, injectFn } = createContextStore(
  'Widget registry',
  (db: GraphDb) => new WidgetRegistry(db),
)

export class WidgetRegistry {
  loadedModules: WidgetModule<any>[] = shallowReactive([])
  sortedModules = computed(() => {
    return [...this.loadedModules].sort(
      (a, b) => a.widgetDefinition.priority - b.widgetDefinition.priority,
    )
  })
  constructor(private db: GraphDb) {}

  loadBuiltins() {
    const bulitinWidgets = import.meta.glob('@/components/GraphEditor/widgets/*.vue')
    this.loadAndCheckWidgetModules(Object.entries(bulitinWidgets))
  }

  async loadAndCheckWidgetModules(
    asyncModules: [path: string, asyncModule: () => Promise<unknown>][],
  ) {
    const modules = await Promise.allSettled(
      asyncModules.map(([path, mod]) => mod().then((m) => [path, m] as const)),
    )
    for (const result of modules) {
      if (result.status === 'fulfilled') {
        const [path, mod] = result.value
        if (isWidgetModule(mod)) this.registerWidgetModule(mod)
        else console.error('Invalid widget module:', path, mod)
      }
    }
  }

  /**
   * Register a known loaded widget module. Once registered, the widget will take part in widget
   * selection process. Caution: registering a new widget module will trigger re-matching of all
   * widgets on all nodes in the scene, which can be expensive.
   */
  registerWidgetModule(module: WidgetModule<any>) {
    this.loadedModules.push(module)
  }

  /**
   * Select a type of widget to use for given widget input (e.g. AST node). The selection is based
   * on the widget's `match` function, which returns a score indicating how well the widget matches
   * the input. The widget with the highest score and priority is selected. Widget kinds that are
   * present in `alreadyUsed` set are always skipped.
   */
  select<T extends WidgetInput>(
    props: WidgetProps<T>,
    alreadyUsed?: Set<WidgetComponent<any>>,
  ): WidgetModule<T> | undefined {
    // The type and score of the best widget found so far.
    let best: WidgetModule<T> | undefined = undefined
    let bestScore = Score.Mismatch

    // Iterate over all loaded widget kinds in order of decreasing priority.
    for (const widgetModule of this.sortedModules.value) {
      // Skip matching widgets that are declared as already used.
      if (alreadyUsed && alreadyUsed.has(widgetModule.default)) continue

      // Skip widgets that don't match the input type.
      if (!widgetModule.widgetDefinition.match(props.input)) continue

      // Perform a match and update the best widget if the match is better than the previous one.
      const score = widgetModule.widgetDefinition.score(props, this.db)
      // If we found a perfect match, we can return immediately, as there can be no better match.
      if (score === Score.Perfect) return widgetModule
      if (score > bestScore) {
        bestScore = score
        best = widgetModule
      }
    }
    return best
  }
}

// TODO: add tests for select
