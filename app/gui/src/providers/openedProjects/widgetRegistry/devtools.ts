import { Score, type WidgetModule } from '$/providers/openedProjects/widgetRegistry'
import { setupDevtoolsPlugin } from '@vue/devtools-api'
import { getCurrentInstance, type App, type ComponentInternalInstance } from 'vue'

/**************
 * PUBLIC API *
 **************/

/** Vue Devtools plugin for widget tree. Only registers itself in dev mode.  */
export const widgetDevtools = {
  install(app: App) {
    DEV: addWidgetDevtools(app)
  },
}

/** Notify devtools that widget selection has started. */
export function devtoolsStartSelection() {
  DEV: doStartSelection()
}

/** Notify devtools that widget selection has ended. */
export function devtoolsEndSelection(widgetModule: WidgetModule<any> | undefined) {
  DEV: doEndSelection(widgetModule)
}

/** Notify devtools about evaluated widget score. */
export function devtoolsAddWidgetScore(widgetModule: WidgetModule<any>, ...result: ScoreResult) {
  DEV: doAddWidgetScoreEntry(widgetModule, result)
}

/***********
 * PRIVATE *
 ***********/

type ScoreResult = ['scored', Score] | ['inputMismatch'] | ['alreadyUsed']

interface ScoreEntry {
  widgetModule: WidgetModule<any>
  result: ScoreResult
}

interface WidgetDevtoolsContext {
  scoreEntries: ScoreEntry[]
  selected: WidgetModule<any> | undefined
}

function printScoreResult(result: ScoreResult, isSelected: boolean): string {
  switch (result[0]) {
    case 'alreadyUsed':
      return 'skipped (already used)'
    case 'inputMismatch':
      return 'input mismatch'
    case 'scored':
      return `Score.${Score[result[1]]}${isSelected ? ' ★' : ''}`
  }
}

const contexts = new WeakMap<ComponentInternalInstance, WidgetDevtoolsContext>()

let notifyComponentUpdate = (_vm: ComponentInternalInstance) => {}

function getCtx(
  componentInstance?: ComponentInternalInstance | undefined,
): WidgetDevtoolsContext | undefined {
  const instance = componentInstance ?? getCurrentInstance()
  return instance ? contexts.get(instance) : undefined
}

function doStartSelection() {
  const instance = getCurrentInstance()
  if (!instance) return
  contexts.set(instance, {
    scoreEntries: [],
    selected: undefined,
  })
}

function getComponentName(Component: any, includeInferred = true) {
  return typeof Component == 'function' ?
      Component.displayName || Component.name
    : Component.name || (includeInferred && Component.__name)
}

function getModuleLabel(widgetModule: WidgetModule<any> | undefined): string {
  if (widgetModule == null) return '<NONE>'
  const componentName = getComponentName(widgetModule.default)
  return typeof componentName === 'string' ? componentName : (
      widgetModule.widgetDefinition.widgetTypeId
    )
}

function doEndSelection(widgetModule: WidgetModule<any> | undefined) {
  const ctx = getCtx()
  if (!ctx) return
  ctx.selected = widgetModule
  notifyComponentUpdate(getCurrentInstance()!)
}

function doAddWidgetScoreEntry(widgetModule: WidgetModule<any>, result: ScoreResult) {
  const ctx = getCtx()
  if (ctx) ctx.scoreEntries.push({ widgetModule, result })
}

function addWidgetDevtools(app: App) {
  setupDevtoolsPlugin(
    {
      app,
      id: 'enso-widget-registry-devtools',
      label: 'Widget Registry',
      enableEarlyProxy: true,
    },
    (api) => {
      notifyComponentUpdate = (componentInstance) => api.notifyComponentUpdate(componentInstance)
      api.on.visitComponentTree(({ treeNode, componentInstance }) => {
        const ctx = getCtx(componentInstance)
        if (ctx == null) return
        treeNode.tags.push({
          label: getModuleLabel(ctx.selected),
          backgroundColor: 0x5bcffb,
          textColor: 0,
        })
      })

      api.on.inspectComponent(({ componentInstance, instanceData }) => {
        const ctx = getCtx(componentInstance)
        if (ctx == null) return
        for (const entry of ctx.scoreEntries) {
          instanceData.state.push({
            type: 'Widget Selection',
            key: getModuleLabel(entry.widgetModule),
            editable: false,
            value: printScoreResult(entry.result, ctx.selected === entry.widgetModule),
          })
        }
      })
    },
  )
}
