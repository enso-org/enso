import { assertDefined } from '../util/assert'
import {
  rangeEquals,
  sourceRangeFromKey,
  textChangeToEdits,
  translateRange,
  type SourceRange,
  type SourceRangeEdit,
} from '../util/data/text'
import { type Origin } from '../yjsModel'
import { type Module, type ModuleUpdate } from './mutableModule'
import { printWithSpans } from './print'
import { type AstId } from './tree'

/**
 * Provides a view of the text representation of a module,
 *  and information about the correspondence between the text and the ASTs,
 *  that can be kept up-to-date by applying AST changes.
 */
export class SourceDocument {
  private readonly observers: SourceDocumentObserver[] = []
  private constructor(
    private readonly state: SourceDocumentState,
    private readonly rawState: SourceDocumentState,
  ) {}

  /**
   * Create an empty {@link SourceDocument}.
   * @param bless - A function adding reactivity to the document state
   */
  static Empty(bless?: (state: SourceDocumentState) => SourceDocumentState) {
    const state = { text: '', spans: new Map() }
    return new this(bless ? bless(state) : state, state)
  }

  /** Reset this {@link SourceDocument} to an empty state. */
  clear() {
    if (this.state.spans.size !== 0) this.state.spans.clear()
    if (this.state.text !== '') {
      const textEdit = { from: 0, to: this.state.text.length, insert: '' }
      this.state.text = ''
      this.notifyObservers([textEdit], undefined)
    }
  }

  /** Apply a {@link ModuleUpdate} and notify observers of the edits. */
  applyUpdate(module: Module, update: ModuleUpdate) {
    for (const id of update.nodesDeleted) this.state.spans.delete(id)
    const root = module.root()
    if (!root) return
    const subtreeTextEdits = new Array<SourceRangeEdit>()
    const printed = printWithSpans(root)
    for (const [key, nodes] of printed.info.nodes) {
      const range = sourceRangeFromKey(key)
      for (const node of nodes) {
        const oldSpan = this.rawState.spans.get(node.id)
        if (!oldSpan || !rangeEquals(range, oldSpan)) this.state.spans.set(node.id, range)
        if (update.updateRoots.has(node.id) && node.id !== root.id) {
          assertDefined(oldSpan)
          const oldCode = this.rawState.text.slice(oldSpan.from, oldSpan.to)
          const newCode = printed.code.slice(range.from, range.to)
          const subedits = textChangeToEdits(oldCode, newCode).map((textEdit) =>
            translateRange(textEdit, oldSpan.from),
          )
          subtreeTextEdits.push(...subedits)
        }
      }
    }
    if (printed.code !== this.rawState.text) {
      const textEdits =
        update.updateRoots.has(root.id) ?
          [{ from: 0, to: this.rawState.text.length, insert: printed.code }]
        : subtreeTextEdits
      this.state.text = printed.code
      this.notifyObservers(textEdits, update.origin)
    }
  }

  /** Get the entire text representation of this module. */
  get text(): string {
    return this.state.text
  }

  /** Get a span in this document by its {@link AstId}. */
  getSpan(id: AstId): SourceRange | undefined {
    return this.state.spans.get(id)
  }

  /** Add a callback to be called with a list of edits on every update. */
  observe(observer: SourceDocumentObserver) {
    this.observers.push(observer)
    if (this.rawState.text.length)
      observer([{ from: 0, to: 0, insert: this.rawState.text }], undefined)
  }

  /** Remove a callback to no longer be called with a list of edits on every update. */
  unobserve(observer: SourceDocumentObserver) {
    const index = this.observers.indexOf(observer)
    if (index !== undefined) this.observers.splice(index, 1)
  }

  private notifyObservers(
    textEdits: ReadonlyArray<Readonly<SourceRangeEdit>>,
    origin: Origin | undefined,
  ) {
    for (const o of this.observers) o(textEdits, origin)
  }
}

export interface SourceDocumentState {
  text: string
  readonly spans: Map<AstId, SourceRange>
}

export type SourceDocumentObserver = (
  textEdits: ReadonlyArray<Readonly<SourceRangeEdit>>,
  origin: Origin | undefined,
) => void
