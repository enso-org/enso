import { syntaxTree } from '@codemirror/language'
import { Prec, RangeSetBuilder, type Text } from '@codemirror/state'
import {
  Decoration,
  type DecorationSet,
  type EditorView,
  type PluginValue,
  ViewPlugin,
  type ViewUpdate,
} from '@codemirror/view'
import type { SyntaxNodeRef, Tree } from '@lezer/common'

/** Extension that uses decorations to render lists. */
export function listDecoratorExt() {
  return Prec.lowest(
    ViewPlugin.define((view) => new ListDecorator(view), {
      decorations: (v) => v.decorations,
    }),
  )
}

type ListStackEntry = { type: 'BulletList' } | { type: 'OrderedList'; nextValue?: number }

class ListHierarchyVisitor {
  private readonly listStack: ListStackEntry[] = []
  private oddBulletDepth: boolean = true

  constructor(
    private readonly doc: Text,
    private readonly emit: (from: number, to: number, value: Decoration) => void,
  ) {}

  private updateDepth(node: SyntaxNodeRef, enterOrLeave: 'enter' | 'leave') {
    if (node.name === 'BulletList' || node.name === 'OrderedList') {
      if (enterOrLeave === 'enter') {
        this.listStack.push({ type: node.name })
      } else {
        this.listStack.pop()
      }
      if (node.name === 'BulletList') this.oddBulletDepth = !this.oddBulletDepth
    }
  }

  enter(node: SyntaxNodeRef) {
    this.updateDepth(node, 'enter')
    const parentList = this.listStack[this.listStack.length - 1]
    if (parentList) {
      if (
        parentList.type === 'OrderedList' &&
        parentList.nextValue == null &&
        node.name === 'ListMark'
      )
        parentList.nextValue = Number.parseInt(
          this.doc.sliceString(node.from, node.to).match(/[0-9]+/)![0],
        )
      if (node.name === 'Paragraph') {
        const listType = parentList.type
        const classes = [`cm-${listType}-item`]
        if (listType === 'BulletList' && this.oddBulletDepth) classes.push('cm-BulletList-item-odd')
        const styles = [`--cm-list-depth: ${this.listStack.length - 1}`]
        if (parentList.type === 'OrderedList' && parentList.nextValue) {
          styles.push(`counter-set: list-item ${parentList.nextValue}`)
          parentList.nextValue += 1
        }
        this.emit(
          node.from,
          node.from + this.doc.slice(node.from, node.to).line(1).to,
          Decoration.mark({ class: classes.join(' '), attributes: { style: styles.join(';') } }),
        )
      }
    }
  }

  leave(node: SyntaxNodeRef) {
    this.updateDepth(node, 'leave')
  }
}

class ListDecorator implements PluginValue {
  decorations: DecorationSet

  /** Constructor. */
  constructor(view: EditorView) {
    this.decorations = this.buildDeco(syntaxTree(view.state), view)
  }

  private isInvalidatedBy(update: ViewUpdate) {
    return update.docChanged || update.viewportChanged
  }

  /** Applies the view update to the decoration set. */
  update(update: ViewUpdate) {
    if (!this.isInvalidatedBy(update)) return
    this.decorations = this.buildDeco(syntaxTree(update.state), update.view)
  }

  private buildDeco(tree: Tree, view: EditorView) {
    if (!tree.length) return Decoration.none
    const builder = new RangeSetBuilder<Decoration>()
    const visitor = new ListHierarchyVisitor(view.state.doc, builder.add.bind(builder))
    for (const { from, to } of view.visibleRanges) {
      tree.iterate({
        from,
        to,
        enter: visitor.enter.bind(visitor),
        leave: visitor.leave.bind(visitor),
      })
    }
    return builder.finish()
  }
}
