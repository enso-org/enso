import type { GraphDb } from '@/stores/graph/graphDatabase'
import { AstExtended } from '@/util/ast'
import { computed, shallowReactive, type Component, type PropType } from 'vue'
import { z } from 'zod'
import { createContextStore } from '.'

export type WidgetComponent<T extends WidgetInput> = Component<WidgetProps<T>>

// class WidgetInputAccess<K extends WidgetInputKeys> {
//   private constructor(private key: K) {}

//   private wrapped = new WeakMap<ValidWidgetInputTypes[K], WidgetInput>()
//   private static instances = new Map<WidgetInputKeys, WidgetInputAccess<any>>()
//   static For<K extends WidgetInputKeys>(key: K): WidgetInputAccess<K> {
//     return map.setIfUndefined(
//       WidgetInputAccess.instances,
//       key as WidgetInputKeys,
//       () => new WidgetInputAccess(key),
//     )
//   }

//   wrap(input: ValidWidgetInputTypes[K]): WidgetInput {
//     let wrapped = this.wrapped.get(input)
//     if (wrapped == null) {
//       wrapped = { [kindKey]: this.key, [kindVal]: input }
//       this.wrapped.set(input, wrapped)
//     }
//     return wrapped
//   }

//   get(union: WidgetInput): ValidWidgetInputTypes[K] | undefined {
//     return union[kindKey] === this.key ? union[kindVal] : undefined
//   }
// }

/**
 * A type representing any kind of input that can have a widget attached to it. It is defined as an
 * interface to allow for extension by widgets themselves. The actual input received by the widget
 * will always be one of the types defined in this interface. The key of each property is used as a
 * key for the discriminated union, so it should be unique.
 *
 * Declare new widget input types by declaring a new unique symbol key, then extending this
 * interface with a new property with that key and a value of the new input type.
 *
 * ```ts
 * declare const MyCustomInputTypeKey: unique symbol;
 * declare module '@/providers/widgetRegistry' {
 *   export interface WidgetInputTypes {
 *     [MyCustomInputTypeKey]: MyCustomInputType
 *   }
 * }
 * ```
 *
 * All declared widget input types must have unique symbols, and all values must be objects.
 * Declarations that do not follow these rules will be ignored or will cause type errors.
 */
export interface WidgetInputTypes {}

type ValidInputKey<K> = K extends keyof WidgetInputTypes & symbol
  ? WidgetInputTypes[K] extends object
    ? K
    : never
  : never

/**
 * Filtered version of {@link WidgetInputTypes} that only contains valid declared input types.
 */
export type ValidWidgetInputTypes = {
  [P in ValidInputKey<keyof WidgetInputTypes>]: WidgetInputTypes[P]
}

export type WidgetInputKeys = keyof ValidWidgetInputTypes

/**
 * A discriminated union of all possible widget input types.
 */
export type WidgetInput = {
  [K in keyof ValidWidgetInputTypes]: ValidWidgetInputTypes[K]
}[keyof ValidWidgetInputTypes]

export function widgetAst(input: WidgetInput): AstExtended | undefined {
  return input instanceof AstExtended ? input : undefined
  // return inputAst.get(input)
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

export interface WidgetProps<T> {
  input: T
  config: WidgetConfiguration | undefined
  nesting: number
}

/**
 * Create vue props definition for a widget component. This cannot be done automatically by using
 * typed `defineProps`, because vue compiler is not able to resolve conditional types. As a
 * workaround, the runtime prop information is specified manually, and the complex conditional type
 * is still provided through `PropType`.
 *
 */
export function widgetProps<Def extends WidgetDefinition<any>>(_def: Def) {
  return {
    input: {
      type: Object as PropType<Def extends WidgetDefinition<infer T> ? T : WidgetInput>,
      required: true,
    },
    config: { type: Object as PropType<WidgetConfiguration | undefined>, required: false },
    nesting: { type: Number, required: true },
  } as const
}

type InputMatchFn<T extends WidgetInput> = (input: WidgetInput) => input is T

type InputTy<M> = M extends InputMatchFn<infer T>
  ? T
  : M extends InputMatchFn<infer T>[]
  ? T
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
  score: (info: WidgetProps<T>, db: GraphDb) => Score
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
  return typeof component === 'object' && component !== null && 'render' in component
}

function isWidgetDefinition(config: unknown): config is WidgetDefinition<any> {
  return typeof config === 'object' && config !== null && 'priority' in config && 'match' in config
}

/**
 *
 * @param match Filter the widget input type to only accept specific types of input. The declared
 * widget props will depend on the type of input accepted by this filter. Only widgets that pass
 * this filter will be scored and potentially used.
 */
export function defineWidget<M extends InputMatchFn<any> | InputMatchFn<any>[]>(
  match: M,
  definition: WidgetOptions<InputTy<M>>,
): WidgetDefinition<InputTy<M>> {
  let score: WidgetDefinition<InputTy<M>>['score']
  if (typeof definition.score === 'function') {
    score = definition.score
  } else {
    const staticScore = definition.score ?? Score.Perfect
    score = () => staticScore
  }
  let matchFn: InputMatchFn<InputTy<M>>
  if (Array.isArray(match)) {
    matchFn = (input: WidgetInput): input is InputTy<M> => match.some((f) => f(input))
  } else if (typeof match === 'function') {
    matchFn = match
  } else {
    throw new Error('Invalid widget match definiton')
  }

  return {
    priority: definition.priority,
    match: matchFn,
    score,
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
  ): WidgetComponent<T> | undefined {
    // The type and score of the best widget found so far.
    let best: WidgetComponent<T> | undefined = undefined
    let bestScore = Score.Mismatch

    // Iterate over all loaded widget kinds in order of decreasing priority.
    for (const module of this.sortedModules.value) {
      // Skip matching widgets that are declared as already used.
      if (alreadyUsed && alreadyUsed.has(module.default)) continue

      // Skip widgets that don't match the input type.
      if (!module.widgetDefinition.match(props.input)) continue

      // Perform a match and update the best widget if the match is better than the previous one.
      const score = module.widgetDefinition.score(props, this.db)
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
