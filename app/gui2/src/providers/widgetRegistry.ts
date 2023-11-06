import type { GraphDb } from '@/stores/graph/graphDatabase'
import type { AstExtended } from '@/util/ast'
import { computed, markRaw, reactive, type Component } from 'vue'
import { createContextStore } from '.'

export type WidgetComponent = Component<{ ast: AstExtended }>

/**
 * Description of how well a widget matches given node. Used to determine which widget should be
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

interface NodeInfo {
  ast: AstExtended
  db: GraphDb
}

export interface WidgetConfig {
  priority: number
  beforeOverride: boolean
  match(info: NodeInfo): Score
}

interface WidgetModule {
  default: WidgetComponent
  widgetConfig: WidgetConfig
}

export function defineWidget(config: WidgetConfig): WidgetConfig {
  return config
}

export { injectFn as injectWidgetRegistry, provideFn as provideWidgetRegistry }
const { provideFn, injectFn } = createContextStore(
  'Widget registry',
  (db: GraphDb) => new WidgetRegistry(db),
)

class WidgetRegistry {
  loadedModules: WidgetModule[] = reactive([])
  sortedModules = computed(() => {
    return [...this.loadedModules].sort((a, b) => b.widgetConfig.priority - a.widgetConfig.priority)
  })
  constructor(private db: GraphDb) {
    const bulitinWidgets = import.meta.glob('@/components/GraphEditor/widgets/*.vue')
    for (const [path, asyncModule] of Object.entries(bulitinWidgets)) {
      asyncModule().then((m) => {
        if (typeof m === 'object' && m !== null && 'default' in m && 'widgetConfig' in m) {
          this.loadedModules.push({
            default: markRaw(m.default as WidgetComponent),
            widgetConfig: m.widgetConfig as WidgetConfig,
          })
        } else {
          console.error('Invalid widget module:', path, m)
        }
      })
    }
  }

  select(
    ast: AstExtended,
    alreadyUsed: Set<WidgetComponent> | undefined,
  ): WidgetComponent | undefined {
    const info = { ast, db: this.db }
    let best: WidgetComponent | undefined = undefined
    let bestScore = Score.Mismatch
    for (const module of this.sortedModules.value) {
      if (alreadyUsed && alreadyUsed.has(module.default)) continue
      const score = module.widgetConfig.match(info)
      if (score === Score.Perfect) return module.default
      if (score > bestScore) {
        bestScore = score
        best = module.default
      }
    }
    return best
  }
}
