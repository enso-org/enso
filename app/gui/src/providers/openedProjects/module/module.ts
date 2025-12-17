import { type ProjectStore } from '$/providers/openedProjects/project'
import { proxyRefs } from '$/utils/reactivity'
import { assert } from '@/util/assert'
import { Ast } from '@/util/ast'
import { type AstId, MutableModule } from '@/util/ast/abstract'
import { reactiveModule } from '@/util/ast/reactive'
import { type Events, stringUnionToArray } from '@/util/data/observable'
import { type MethodPointer } from '@/util/methodPointer'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { computed, effectScope, onScopeDispose, reactive, ref } from 'vue'
import { SourceDocument } from 'ydoc-shared/ast/sourceDocument'
import {
  defaultLocalOrigin,
  DistributedModule,
  localUserActionOrigins,
  type Origin,
} from 'ydoc-shared/yjsModel'
import * as Y from 'yjs'
import { type ProjectNameStore } from '../projectNames'
import { type SuggestionDbStore } from '../suggestionDatabase'
import {
  type AbstractImport,
  addImports,
  analyzeImports,
  type DetectedConflict,
  detectImportConflicts,
  filterOutRedundantImports,
  type RequiredImport,
} from './imports'

export type X = Awaited<ReturnType<typeof createModuleStore>>
export type XX = X extends Result<infer T> ? T : never
export type XXX<T extends Result<any, any>> = T extends Result<infer V> ? V : never
export type XY = XXX<X>
/**
 * Module Store
 *
 * This store keeps the module AST and provides methods for analyzing and modifying it from
 * "code" perspective. It does not manage graph's nodes and connections, although it exposes
 * API for node and widgets metadata, as it is defined at AST level.
 */
export type ModuleStore = X extends Result<infer T> ? T : never

export interface EditOptions {
  skipTreeRepair?: boolean
  origin?: Origin
  logLevel?: 'none' | 'info' | 'warn' | 'error'
  logPreamble?: string
}

/** Constructor of {@link ModuleStore} */
export async function createModuleStore(
  proj: ProjectStore,
  projectNames: ProjectNameStore,
  suggestionDb: SuggestionDbStore,
) {
  const FILE_NAME = 'Main.enso'
  proj.setObservedFileName(FILE_NAME)

  const scope = effectScope()
  //TODO[ao] refactor & timeout
  const modPromise = new Promise<DistributedModule>(async (resolve) => {
    const mod = await proj.projectModel.openModule(FILE_NAME)
    if (mod != null) resolve(mod)
    else {
      const cb = () => proj.projectModel.openModule
      proj.projectModel.modules.observe(() => {
        proj.projectModel.openModule(FILE_NAME).then((mod) => {
          if (mod != null) {
            proj.projectModel.modules.unobserve(cb)
            resolve(mod)
          }
        })
      })
    }
  })
  const mod = await modPromise //proj.projectModel.openModule(FILE_NAME)
  // if (mod == null) return Err(`Cannot load module ${FILE_NAME}`)
  const moduleModel = mod
  for (const origin of localUserActionOrigins) mod.undoManager.addTrackedOrigin(origin)
  const module = reactiveModule(moduleModel.doc.ydoc, onScopeDispose)

  return scope.run(() => {
    const source = SourceDocument.Empty(reactive)
    const root = ref<Ast.BodyBlock>()
    const ast = module as Ast.Module
    const observers: ((update: Ast.ModuleUpdate) => void)[] = []

    const handle = module.observe((update) => {
      const rootAst = module.root()
      console.debug('>', rootAst)
      if (rootAst instanceof Ast.BodyBlock) {
        console.debug('>>', rootAst.lines.length)
        root.value = rootAst
        if (
          update.nodesAdded.size != 0 ||
          update.nodesDeleted.size != 0 ||
          update.nodesUpdated.size != 0 ||
          update.updateRoots.size != 0
        ) {
          source.applyUpdate(module, update)
        }
        for (const observer of observers) observer(update)
      } else {
        root.value = undefined
      }
    })
    onScopeDispose(() => {
      module.unobserve(handle)
      source.clear()
    })

    const undoManagerStatus = reactive({
      canUndo: false,
      canRedo: false,
      update(m: Y.UndoManager) {
        this.canUndo = m.canUndo()
        this.canRedo = m.canRedo()
      },
    })
    const update = () => undoManagerStatus.update(moduleModel.undoManager)
    const events = stringUnionToArray<keyof Events<Y.UndoManager>>()(
      'stack-item-added',
      'stack-item-popped',
      'stack-cleared',
      'stack-item-updated',
    )
    events.forEach((event) => moduleModel.undoManager.on(event, update))

    const undoManager = proxyRefs({
      undo() {
        moduleModel.undoManager.undo()
      },
      redo() {
        moduleModel.undoManager.redo()
      },
      undoStackBoundary() {
        moduleModel.undoManager.stopCapturing()
      },
      canUndo: computed(() => undoManagerStatus.canUndo),
      canRedo: computed(() => undoManagerStatus.canRedo),
    })

    function stopCapturingUndo() {
      moduleModel.undoManager.stopCapturing()
    }

    function observe(f: (update: Ast.ModuleUpdate) => void) {
      observers.push(f)
      return () => {
        const index = observers.indexOf(f)
        if (index !== -1) observers.splice(index, 1)
      }
    }

    function edit<R extends Result<any>>(f: (edit: MutableModule) => R, options?: EditOptions): R
    function edit<R extends Result<any>>(
      f: (edit: MutableModule) => Promise<R>,
      options?: EditOptions,
    ): Promise<R>
    function edit<R extends Result<any>>(
      f: (edit: MutableModule) => R | Promise<R>,
      options?: EditOptions,
    ): R | Promise<R>
    /**
     * Edit the AST module.
     *
     * Optimization options: These are safe to use for metadata-only edits; otherwise, they require extreme caution.
     * @param options.skipTreeRepair - If the edit is certain not to produce incorrect or non-canonical syntax, this may be set
     * to `true` for better performance.
     */
    function edit(
      f: (edit: MutableModule) => Promise<Result> | Result,
      options: EditOptions = {},
    ): Promise<Result> | Result {
      const edit = module.edit()
      const logLevel = options.logLevel ?? 'error'

      const treeRepair = (result: Result) => {
        if (result.ok) {
          const root = edit.root()
          assert(root instanceof Ast.BodyBlock)
          edit.transact(() => Ast.repair(root, edit))
        }
        return result
      }

      const applyEdit = (result: Result) => {
        if (result.ok) module.applyEdit(edit, options.origin)
        else if (logLevel !== 'none')
          console[logLevel](result.error.message(options.logPreamble ?? 'Cannot commit AST edit.'))
        return result
      }

      const result = edit.transact(() => {
        const result = f(edit)
        if (options.skipTreeRepair === true) return result
        return result instanceof Promise ? result.then(treeRepair) : treeRepair(result)
      })
      return result instanceof Promise ? result.then(applyEdit) : applyEdit(result)
    }

    function batchEdits(f: () => void, origin: Origin = defaultLocalOrigin) {
      module.transact(f, origin)
    }

    function hasMethod(name: string): boolean {
      const root = ast.root()
      return root != null && Ast.findModuleMethod(root, name) != null
    }

    function getMethodAst(ptr: MethodPointer, edit?: Ast.Module): Result<Ast.FunctionDef> {
      const topLevel = (edit ?? ast).root()
      if (!topLevel) return Err('Module unavailable')
      assert(topLevel instanceof Ast.BodyBlock)
      if (!proj.moduleProjectPath?.ok)
        return proj.moduleProjectPath ?? Err('Unknown module project path')
      if (!ptr.module.equals(proj.moduleProjectPath.value))
        return Err('Cannot read method from different module')
      if (!ptr.module.equals(ptr.definedOnType)) return Err('Method pointer is not a module method')
      const method = Ast.findModuleMethod(topLevel, ptr.name)
      if (!method) {
        const modulePath = projectNames.printProjectPath(proj.moduleProjectPath.value)
        return Err(`No method with name ${ptr.name} in ${modulePath}`)
      }
      return Ok(method.statement)
    }

    function mutableNodeMetadata(node: AstId | undefined, edit?: Ast.MutableModule) {
      edit ??= module
      return edit.tryGet(node)?.mutableNodeMetadata()
    }

    function setWidgetMetadata(widget: AstId, widgetKey: string, md: unknown) {
      const ast = module.tryGet(widget)
      if (!ast) return
      ast.setWidgetMetadata(widgetKey, md)
    }

    /**
     * Try adding imports. Do not add those conflicting with existing imports - return
     * {@link DetectedConflict} in such case.
     */
    function addMissingImports(
      edit: MutableModule,
      newImports: RequiredImport[],
    ): DetectedConflict[] | undefined {
      if (!root.value) {
        console.error(`BUG: Cannot add required imports: No BodyBlock module root.`)
        return
      }
      const topLevel = edit.getVersion(root.value)
      const existingImports = [...analyzeImports(topLevel, projectNames)]

      const conflicts = []
      const nonConflictingImports = []
      for (const newImport of newImports) {
        const conflictInfo = detectImportConflicts(suggestionDb.entries, existingImports, newImport)
        if (conflictInfo?.detected) {
          conflicts.push(conflictInfo)
        } else {
          nonConflictingImports.push(newImport)
        }
      }
      addMissingImportsDisregardConflicts(edit, nonConflictingImports, existingImports)

      if (conflicts.length > 0) return conflicts
    }

    /* Adds imports, ignores any possible conflicts.
     * `existingImports` are optional and will be used instead of `readImports(topLevel)` if provided. */
    function addMissingImportsDisregardConflicts(
      edit: MutableModule,
      imports: RequiredImport[],
      existingImports?: AbstractImport[] | undefined,
    ) {
      if (!imports.length) return
      if (!root.value) {
        console.error(`BUG: Cannot add required imports: No BodyBlock module root.`)
        return
      }
      const topLevel = edit.getVersion(root.value)
      const existingImports_ = existingImports ?? [...analyzeImports(topLevel, projectNames)]

      const importsToAdd = filterOutRedundantImports(existingImports_, imports)
      if (!importsToAdd.length) return
      addImports(topLevel, importsToAdd, projectNames)
    }

    function onBeforeEdit(f: (transaction: Y.Transaction) => void): { unregister: () => void } {
      moduleModel.doc.ydoc.on('beforeTransaction', f)
      return { unregister: () => moduleModel.doc.ydoc.off('beforeTransaction', f) }
    }

    return Ok(
      proxyRefs({
        source,
        ast,
        root,
        observe,
        edit,
        batchEdits,
        onBeforeEdit,
        hasMethod,
        getMethodAst,
        mutableNodeMetadata,
        setWidgetMetadata,
        addMissingImports,
        undoManager,
        stopCapturingUndo,
      }),
    )
  })!
}
