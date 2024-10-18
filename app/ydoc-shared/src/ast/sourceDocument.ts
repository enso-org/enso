import { print, type AstId, type Module, type ModuleUpdate } from '.'
import { assertDefined } from '../util/assert'
import type { SourceRangeEdit } from '../util/data/text'
import { offsetEdit, textChangeToEdits } from '../util/data/text'
import type { Origin, SourceRange } from '../yjsModel'
import { rangeEquals, sourceRangeFromKey } from '../yjsModel'

/**
 * Provides a view of the text representation of a module,
 *  and information about the correspondence between the text and the ASTs,
 *  that can be kept up-to-date by applying AST changes.
 */
export class SourceDocument {
  private text_: string
  private readonly spans: Map<AstId, SourceRange>
  private readonly observers: SourceDocumentObserver[]

  private constructor(text: string, spans: Map<AstId, SourceRange>) {
    this.text_ = text
    this.spans = spans
    this.observers = []
  }

  /** Create an empty {@link SourceDocument}. */
  static Empty() {
    return new this('', new Map())
  }

  /** Reset this {@link SourceDocument} to an empty state. */
  clear() {
    if (this.spans.size !== 0) this.spans.clear()
    if (this.text_ !== '') {
      const range: SourceRange = [0, this.text_.length]
      this.text_ = ''
      this.notifyObservers([{ range, insert: '' }], undefined)
    }
  }

  /** Apply a {@link ModuleUpdate} and notify observers of the edits. */
  applyUpdate(module: Module, update: ModuleUpdate) {
    for (const id of update.nodesDeleted) this.spans.delete(id)
    const root = module.root()
    if (!root) return
    const subtreeTextEdits = new Array<SourceRangeEdit>()
    const printed = print(root)
    for (const [key, nodes] of printed.info.nodes) {
      const range = sourceRangeFromKey(key)
      for (const node of nodes) {
        const oldSpan = this.spans.get(node.id)
        if (!oldSpan || !rangeEquals(range, oldSpan)) this.spans.set(node.id, range)
        if (update.updateRoots.has(node.id) && node.id !== root.id) {
          assertDefined(oldSpan)
          const oldCode = this.text_.slice(oldSpan[0], oldSpan[1])
          const newCode = printed.code.slice(range[0], range[1])
          const subedits = textChangeToEdits(oldCode, newCode).map(textEdit =>
            offsetEdit(textEdit, oldSpan[0]),
          )
          subtreeTextEdits.push(...subedits)
        }
      }
    }
    if (printed.code !== this.text_) {
      const textEdits =
        update.updateRoots.has(root.id) ?
          [{ range: [0, this.text_.length] satisfies SourceRange, insert: printed.code }]
        : subtreeTextEdits
      this.text_ = printed.code
      this.notifyObservers(textEdits, update.origin)
    }
  }

  /** Get the entire text representation of this module. */
  get text(): string {
    return this.text_
  }

  /** Get a span in this document by its {@link AstId}. */
  getSpan(id: AstId): SourceRange | undefined {
    return this.spans.get(id)
  }

  /** Add a callback to be called with a list of edits on every update. */
  observe(observer: SourceDocumentObserver) {
    this.observers.push(observer)
    if (this.text_.length) observer([{ range: [0, 0], insert: this.text_ }], undefined)
  }

  /** Remove a callback to no longer be called with a list of edits on every update. */
  unobserve(observer: SourceDocumentObserver) {
    const index = this.observers.indexOf(observer)
    if (index !== undefined) this.observers.splice(index, 1)
  }

  private notifyObservers(textEdits: readonly SourceRangeEdit[], origin: Origin | undefined) {
    for (const o of this.observers) o(textEdits, origin)
  }
}

export type SourceDocumentObserver = (
  textEdits: readonly SourceRangeEdit[],
  origin: Origin | undefined,
) => void
