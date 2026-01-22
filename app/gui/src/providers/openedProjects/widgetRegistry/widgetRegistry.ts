import type { GraphDb } from '$/providers/openedProjects/graph/graphDatabase'
import type { ModuleStore } from '$/providers/openedProjects/module'
import type { Typename } from '$/providers/openedProjects/suggestionDatabase/entry'
import type { WidgetConfiguration } from '$/providers/openedProjects/widgetRegistry/configuration'
import {
  devtoolsAddWidgetScore,
  devtoolsEndSelection,
  devtoolsStartSelection,
} from '$/providers/openedProjects/widgetRegistry/devtools'
import type { WidgetEditHandlerParent } from '$/providers/openedProjects/widgetRegistry/editHandler'
import type { PortId } from '@/providers/portInfo'
import { Ast } from '@/util/ast'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import type { ViteHotContext } from 'vite/types/hot.js'
import { computed, shallowReactive, type Component, type PropType } from 'vue'
import type { Class } from 'ydoc-shared/util/types'

export type WidgetComponent<T extends WidgetInput> = Component<WidgetProps<T>>

declare const brandWidgetId: unique symbol
/** Uniquely identifies a widget type. */
export type WidgetTypeId = string & { [brandWidgetId]: never }

export namespace WidgetInput {
  /** Returns widget-input data for the given AST tree or token. */
  export function FromAst<A extends Ast.Ast | Ast.Token>(ast: A) {
    return FromAstWithPortId(ast, ast.id)
  }

  /** Returns widget-input data for the given AST tree or token with a specific port ID. */
  export function FromAstWithPortId<A extends Ast.Ast | Ast.Token>(
    ast: A,
    portId: PortId,
  ): WidgetInput & { value: A } {
    return {
      portId,
      value: ast,
    }
  }

  /** Returns widget-input data for the given AST tree or a placeholder in case a value is missing. */
  export function FromAstOrPlaceholder<A extends Ast.Ast | Ast.Token | string | undefined>(
    ast: A,
    portIdFallback: () => PortId,
  ): WidgetInput & { value: A } {
    return ast instanceof Ast.Ast || ast instanceof Ast.Token ?
        WidgetInput.FromAst(ast)
      : {
          portId: portIdFallback(),
          value: ast,
        }
  }

  /** Returns the input marked to be a port. */
  export function WithPort<T extends WidgetInput>(input: T): T {
    return {
      ...input,
      forcePort: true,
    }
  }

  /** A string representation of widget's value - the code in case of AST value. */
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
  export function astMatcher<T extends Ast.Ast>(nodeType: Class<T>) {
    return (input: WidgetInput): input is WidgetInput & { value: T } =>
      input.value instanceof nodeType
  }

  /** Match input against a placeholder or specific AST node type. */
  export function placeholderOrAstMatcher<T extends Ast.Ast>(nodeType: Class<T>) {
    return (input: WidgetInput): input is WidgetInput & { value: T | string | undefined } =>
      isPlaceholder(input) || input.value instanceof nodeType
  }

  /** Check if input's value is existing AST node (not placeholder or token). */
  export function isAst(input: WidgetInput): input is WidgetInput & { value: Ast.Expression } {
    return input.value instanceof Ast.Ast && input.value.isExpression()
  }

  /** Check if input's value is existing AST node or placeholder. Rule out token inputs. */
  export function isAstOrPlaceholder(
    input: WidgetInput,
  ): input is WidgetInput & { value: Ast.Expression | string | undefined } {
    return isPlaceholder(input) || isAst(input)
  }

  /** Check if input's value is an AST token. */
  export function isToken(input: WidgetInput): input is WidgetInput & { value: Ast.Token } {
    return input.value instanceof Ast.Token
  }

  /** Check if input's value is an AST which potentially may be a function call. */
  export function isFunctionCall(input: WidgetInput): input is WidgetInput & {
    value: Ast.App | Ast.Ident | Ast.PropertyAccess | Ast.OprApp | Ast.AutoscopedIdentifier
  } {
    return (
      input.value instanceof Ast.App ||
      input.value instanceof Ast.Ident ||
      input.value instanceof Ast.PropertyAccess ||
      input.value instanceof Ast.OprApp ||
      input.value instanceof Ast.AutoscopedIdentifier
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
 * declare module '$/providers/openedProjects/widgetRegistry' {
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
  editHandler?: WidgetEditHandlerParent | undefined
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
   * A last resort match. This widget will be used only if there is no other good option present.
   */
  Weak,
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
 * When widget wants to change its value, it should emit this with `portUpdate` set (as their
 * port may not represent any existing AST node) with `edit` containing any additional modifications
 * (like inserting necessary imports).
 *
 * The same way widgets may set their metadata (as this is also technically an AST modification).
 * Every widget type should set it's name as `metadataKey`.
 *
 * The handlers interested in a specific port update should apply it using received edit. The edit
 * is committed in {@link ComponentWidgetTree}.
 */
export interface WidgetUpdate {
  edit?: Ast.MutableModule | undefined
  portUpdate?:
    | { origin: PortId; value: Ast.Owned<Ast.MutableExpression> | string | undefined }
    | { origin: PortId; metadataKey: string; metadata: unknown }

  /**
   * Set to true if the updated is caused by direct interaction with the origin widget - a usual case.
   * An example if _nondirect_ interaction is an update of a port connected to a removed node).
   */
  directInteraction: boolean
}

/**
 * Handle a direct child widget port value update in a special way, while letting any direct edit updates
 * through unaffected.
 */
export async function rewritePortValueUpdate(
  update: WidgetUpdate,
  parentOnUpdate: UpdateHandler,
  originPredicate: PortId | ((origin: PortId) => boolean) | undefined,
  valueHandler: (
    value: Ast.Owned<Ast.MutableExpression> | string | undefined,
  ) => UpdateResult | Promise<UpdateResult>,
) {
  if (
    update.portUpdate &&
    'value' in update.portUpdate &&
    originMatches(update.portUpdate.origin, originPredicate)
  ) {
    const { portUpdate, ...remainingUpdate } = update
    let result = valueHandler(portUpdate.value)
    if (result instanceof Promise) result = await result
    if (!result.ok) return result
    return parentOnUpdate(remainingUpdate)
  } else {
    return parentOnUpdate(update)
  }
}

function originMatches(
  origin: PortId,
  predicate: PortId | ((origin: PortId) => boolean) | undefined,
) {
  return (
    predicate === undefined ||
    origin === predicate ||
    (typeof predicate === 'function' && predicate(origin))
  )
}

/**
 * Apply graph edits described by a `WidgetUpdate` struct.
 */
export function applyWidgetUpdates(update: WidgetUpdate, module: ModuleStore) {
  function invalidOriginErr(origin: PortId) {
    return Err(`[UPDATE ${origin}] Invalid top-level origin. Expected expression ID.`)
  }

  if (!update.edit && update.portUpdate && !('value' in update.portUpdate)) {
    // A fast-track for metadata-only updates. Edit is quite a heavy operation,
    // and we don't need it in this case.
    const { origin, metadata, metadataKey } = update.portUpdate
    if (Ast.isAstId(origin)) {
      module.setWidgetMetadata(origin, metadataKey, metadata)
      return Ok()
    } else {
      return invalidOriginErr(origin)
    }
  } else {
    const applyInEdit = (edit: Ast.MutableModule) => {
      if (update.portUpdate) {
        const { origin } = update.portUpdate
        if (Ast.isAstId(origin)) {
          if ('value' in update.portUpdate) {
            const value = update.portUpdate.value
            const ast =
              value instanceof Ast.Ast ? value
              : value == null ? Ast.Wildcard.new(edit)
              : undefined
            if (ast) {
              edit.replaceValue(origin, ast)
            } else if (typeof value === 'string') {
              edit.tryGet(origin)?.syncToCode(value)
            }
          }
          if ('metadata' in update.portUpdate) {
            const { metadataKey, metadata } = update.portUpdate
            edit.tryGet(origin)?.setWidgetMetadata(metadataKey, metadata)
          }
          return Ok()
        } else {
          return invalidOriginErr(origin)
        }
      } else return Ok()
    }

    if (update.edit) return applyInEdit(update.edit)
    else return module.edit(applyInEdit)
  }
}

export type UpdateResult = Result<void, string>
export type HandledUpdate = UpdateResult | Promise<UpdateResult>
export type UpdateHandler = (update: WidgetUpdate) => UpdateResult | Promise<UpdateResult>

/**
 * Create Vue props definition for a widget component. This cannot be done automatically by using
 * typed `defineProps`, because vue compiler is not able to resolve conditional types. As a
 * workaround, the runtime prop information is specified manually, and the inferred `T: WidgetInput`
 * type is provided through `PropType`.
 */
export function widgetProps<T extends WidgetInput>(def: WidgetDefinition<T>) {
  return {
    input: { type: Object as PropType<T>, required: true },
    nesting: { type: Number, required: true },
    widgetTypeId: {
      type: String as unknown as PropType<WidgetTypeId>,
      default: def.widgetTypeId,
    },
    updateCallback: { type: Function as PropType<UpdateHandler>, required: true },
  } as const
}

type InputMatcherFn<T extends WidgetInput> = (input: WidgetInput) => input is T
type InputMatcher<T extends WidgetInput> = keyof WidgetInput | InputMatcherFn<T>

type InputTy<M> =
  M extends (infer T)[] ? InputTy<T>
  : M extends InputMatcherFn<infer T> ? T
  : M extends keyof WidgetInput ? WidgetInput & Required<Pick<WidgetInput, M>>
  : never

export interface WidgetOptions<T extends WidgetInput> {
  /**
   * The priority number determining the order in which the widgets are matched. Smaller numbers
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
  /**
   * If false, this widget will be matched only when at least one another widget is guaranteed to
   * be matched for the same input. `true` by default.
   *
   * The widget marked with `false` *must* have a child with the same input,
   * otherwise it can be selected as leaf.
   *
   * It can be useful for widgets that wrap other widgets, but shouldn’t be selected when
   * no child widget is ever selected. One particular example is `WidgetSelectionArrow`.
   */
  allowAsLeaf?: boolean
}

export interface WidgetDefinition<T extends WidgetInput> {
  /** See {@link WidgetOptions.priority} */
  priority: number
  /**
   * Filter the widget input type to only accept specific types of input. The declared widget props
   * will depend on the type of input accepted by this filter. Only widgets that pass this filter
   * will be scored and potentially used.
   */
  match: (input: WidgetInput) => input is T
  /** See {@link WidgetOptions.score} */
  score: (props: WidgetProps<T>, db: GraphDb) => Score
  /** See {@link WidgetOptions.prevent} */
  prevent: WidgetComponent<any>[] | undefined
  /** See {@link WidgetOptions.allowAsLeaf}. */
  allowAsLeaf: boolean
  widgetTypeId: WidgetTypeId
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
  hmr?: ViteHotContext,
): WidgetDefinition<InputTy<M>> {
  let score: WidgetDefinition<InputTy<M>>['score']
  if (typeof definition.score === 'function') {
    score = definition.score
  } else {
    const staticScore = definition.score ?? Score.Good
    score = () => staticScore
  }

  const resolved: WidgetDefinition<InputTy<M>> = {
    priority: definition.priority,
    match: makeInputMatcher<InputTy<M>>(matchInputs),
    score,
    prevent: definition.prevent,
    allowAsLeaf: definition.allowAsLeaf ?? true,
    widgetTypeId: crypto.randomUUID() as WidgetTypeId,
  }

  // Checking hmr.data, as it is undefined in unit test enviroment
  if (import.meta.hot && hmr && hmr.data) {
    if (hmr.data.widgetDefinition) {
      const widgetTypeId = hmr.data.widgetDefinition.widgetTypeId
      Object.assign(hmr.data.widgetDefinition, resolved, { widgetTypeId })
    } else {
      hmr.data.widgetDefinition = shallowReactive(resolved)
    }
    return hmr.data.widgetDefinition
  }
  return resolved
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

/** TODO: Add docs */
export class WidgetRegistry {
  loadedModules: WidgetModule<any>[] = shallowReactive([])
  sortedModules = computed(() => {
    return [...this.loadedModules].sort(
      (a, b) => a.widgetDefinition.priority - b.widgetDefinition.priority,
    )
  })
  /** TODO: Add docs */
  constructor(private db: GraphDb) {}

  /** TODO: Add docs */
  loadWidgets(modules: [path: string, module: unknown][]) {
    for (const [path, mod] of modules) {
      if (isWidgetModule(mod)) this.registerWidgetModule(mod)
      else console.error('Invalid widget module:', path, mod)
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
    devtoolsStartSelection()

    // The type and score of the best widget found so far.
    let best: WidgetModule<T> | undefined = undefined
    let bestScore = Score.Mismatch
    let foundLeafMatch = false

    // Iterate over all loaded widget kinds in order of decreasing priority.
    for (const widgetModule of this.sortedModules.value) {
      // Skip matching widgets that are declared as already used.
      if (alreadyUsed && alreadyUsed.has(widgetModule.default)) {
        devtoolsAddWidgetScore(widgetModule, 'alreadyUsed')
        continue
      }

      // Skip widgets that don't match the input type.
      if (!widgetModule.widgetDefinition.match(props.input)) {
        devtoolsAddWidgetScore(widgetModule, 'inputMismatch')
        continue
      }

      // Perform a match and update the best widget if the match is better than the previous one.
      const score = widgetModule.widgetDefinition.score(props, this.db)

      devtoolsAddWidgetScore(widgetModule, 'scored', score)

      if (score > Score.Mismatch) {
        foundLeafMatch ||= widgetModule.widgetDefinition.allowAsLeaf
      }
      if (score > bestScore) {
        bestScore = score
        best = widgetModule
      }

      // If we found a perfect match, we can return immediately, as there can be no better match.
      // We don’t care if this match allows being a leaf or not – we already know
      // there are other matched widgets without this restriction.
      if (bestScore === Score.Perfect && foundLeafMatch) {
        devtoolsEndSelection(best)
        return best
      }
    }

    // We didn’t find any widget that supports being a leaf, we can’t select any.
    if (!foundLeafMatch) {
      devtoolsEndSelection(undefined)
      return undefined
    }

    devtoolsEndSelection(best)
    return best
  }
}
