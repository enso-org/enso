import { print, type AstId, type Module, type ModuleUpdate } from '.'
import { rangeEquals, sourceRangeFromKey, type SourceRange } from '../yjsModel'

export class SourceDocument {
  private text_: string
  private readonly spans: Map<AstId, SourceRange>

  private constructor(text: string, spans: Map<AstId, SourceRange>) {
    this.text_ = text
    this.spans = spans
  }

  static Empty() {
    return new this('', new Map())
  }

  clear() {
    if (this.text_ !== '') this.text_ = ''
    if (this.spans.size !== 0) this.spans.clear()
  }

  applyUpdate(module: Module, update: ModuleUpdate) {
    for (const id of update.nodesDeleted) this.spans.delete(id)
    const root = module.root()
    if (!root) return
    const printed = print(root)
    for (const [key, nodes] of printed.info.nodes) {
      const range = sourceRangeFromKey(key)
      for (const node of nodes) {
        const oldSpan = this.spans.get(node.id)
        if (!oldSpan || !rangeEquals(range, oldSpan)) this.spans.set(node.id, range)
      }
    }
    if (printed.code !== this.text_) this.text_ = printed.code
  }

  get text(): string {
    return this.text_
  }

  getSpan(id: AstId): SourceRange | undefined {
    return this.spans.get(id)
  }
}
