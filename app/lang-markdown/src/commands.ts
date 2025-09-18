import {StateCommand, Text, EditorState, EditorSelection, ChangeSpec, countColumn, Line} from "@codemirror/state"
import {syntaxTree, indentUnit} from "@codemirror/language"
import {SyntaxNode, Tree} from "@lezer/common"
import {markdownLanguage} from "./markdown"

class Context {
  constructor(
    readonly node: SyntaxNode,
    readonly from: number,
    readonly to: number,
    readonly spaceBefore: string,
    readonly spaceAfter: string,
    readonly type: string,
    readonly item: SyntaxNode | null
  ) {}

  blank(maxWidth: number | null, trailing = true) {
    let result = this.spaceBefore + (this.node.name == "Blockquote" ? ">" : "")
    if (maxWidth != null) {
      while (result.length < maxWidth) result += " "
      return result
    } else {
      for (let i = this.to - this.from - result.length - this.spaceAfter.length; i > 0; i--) result += " "
      return result + (trailing ? this.spaceAfter : "")
    }
  }

  marker(doc: Text, add: number) {
    let number = this.node.name == "OrderedList" ? String((+itemNumber(this.item!, doc)[2] + add)) : ""
    return this.spaceBefore + number + this.type + this.spaceAfter
  }
}

function getContext(node: SyntaxNode, doc: Text) {
  let nodes: SyntaxNode[] = [], context: Context[] = []
  for (let cur: SyntaxNode | null = node; cur; cur = cur.parent) {
    if (cur.name == "FencedCode") return context
    if (cur.name == "ListItem" || cur.name == "Blockquote") nodes.push(cur)
  }
  for (let i = nodes.length - 1; i >= 0; i--) {
    let node = nodes[i], match
    let line = doc.lineAt(node.from), startPos = node.from - line.from
    if (node.name == "Blockquote" && (match = /^ *>( ?)/.exec(line.text.slice(startPos)))) {
      context.push(new Context(node, startPos, startPos + match[0].length, "", match[1], ">", null))
    } else if (node.name == "ListItem" && node.parent!.name == "OrderedList" &&
               (match = /^( *)\d+([.)])( *)/.exec(line.text.slice(startPos)))) {
      let after = match[3], len = match[0].length
      if (after.length >= 4) { after = after.slice(0, after.length - 4); len -= 4 }
      context.push(new Context(node.parent!, startPos, startPos + len, match[1], after, match[2], node))
    } else if (node.name == "ListItem" && node.parent!.name == "BulletList" &&
               (match = /^( *)([-+*])( {1,4}\[[ xX]\])?( +)/.exec(line.text.slice(startPos)))) {
      let after = match[4], len = match[0].length
      if (after.length > 4) { after = after.slice(0, after.length - 4); len -= 4 }
      let type = match[2]
      if (match[3]) type += match[3].replace(/[xX]/, ' ')
      context.push(new Context(node.parent!, startPos, startPos + len, match[1], after, type, node))
    }
  }
  return context
}

function itemNumber(item: SyntaxNode, doc: Text) {
  return /^(\s*)(\d+)(?=[.)])/.exec(doc.sliceString(item.from, item.from + 10))!
}

function renumberList(after: SyntaxNode, doc: Text, changes: ChangeSpec[], offset = 0) {
  for (let prev = -1, node = after;;) {
    if (node.name == "ListItem") {
      let m = itemNumber(node, doc)
      let number = +m[2]
      if (prev >= 0) {
        if (number != prev + 1) return
        changes.push({from: node.from + m[1].length, to: node.from + m[0].length, insert: String(prev + 2 + offset)})
      }
      prev = number
    }
    let next = node.nextSibling
    if (!next) break
    node = next
  }
}

function normalizeIndent(content: string, state: EditorState) {
  let blank = /^[ \t]*/.exec(content)![0].length
  if (!blank || state.facet(indentUnit) != "\t") return content
  let col = countColumn(content, 4, blank)
  let space = ""
  for (let i = col; i > 0;) {
    if (i >= 4) { space += "\t"; i -= 4 }
    else { space += " "; i-- }
  }
  return space + content.slice(blank)
}

/// This command, when invoked in Markdown context with cursor
/// selection(s), will create a new line with the markup for
/// blockquotes and lists that were active on the old line. If the
/// cursor was directly after the end of the markup for the old line,
/// trailing whitespace and list markers are removed from that line.
///
/// The command does nothing in non-Markdown context, so it should
/// not be used as the only binding for Enter (even in a Markdown
/// document, HTML and code regions might use a different language).
export const insertNewlineContinueMarkup: StateCommand = ({state, dispatch}) => {
  let tree = syntaxTree(state), {doc} = state
  let dont = null, changes = state.changeByRange(range => {
    if (!range.empty || !markdownLanguage.isActiveAt(state, range.from, 0)) return dont = {range}
    let pos = range.from, line = doc.lineAt(pos)
    let context = getContext(tree.resolveInner(pos, -1), doc)
    while (context.length && context[context.length - 1].from > pos - line.from) context.pop()
    if (!context.length) return dont = {range}
    let inner = context[context.length - 1]
    if (inner.to - inner.spaceAfter.length > pos - line.from) return dont = {range}

    let emptyLine = pos >= (inner.to - inner.spaceAfter.length) && !/\S/.test(line.text.slice(inner.to))
    // Empty line in list
    if (inner.item && emptyLine) {
      let first = inner.node.firstChild!, second = inner.node.getChild("ListItem", "ListItem")
      // Not second item or blank line before: delete a level of markup
      if (first.to >= pos || second && second.to < pos ||
          line.from > 0 && !/[^\s>]/.test(doc.lineAt(line.from - 1).text)) {
        let next = context.length > 1 ? context[context.length - 2] : null
        let delTo, insert = ""
        if (next && next.item) { // Re-add marker for the list at the next level
          delTo = line.from + next.from
          insert = next.marker(doc, 1)
        } else {
          delTo = line.from + (next ? next.to : 0)
        }
        let changes: ChangeSpec[] = [{from: delTo, to: pos, insert}]
        if (inner.node.name == "OrderedList") renumberList(inner.item!, doc, changes, -2)
        if (next && next.node.name == "OrderedList") renumberList(next.item!, doc, changes)
        return {range: EditorSelection.cursor(delTo + insert.length), changes}
      } else { // Move second item down, making tight two-item list non-tight
        let insert = blankLine(context, state, line)
        return {range: EditorSelection.cursor(pos + insert.length + 1),
                changes: {from: line.from, insert: insert + state.lineBreak}}
      }
    }

    if (inner.node.name == "Blockquote" && emptyLine && line.from) {
      let prevLine = doc.lineAt(line.from - 1), quoted = />\s*$/.exec(prevLine.text)
      // Two aligned empty quoted lines in a row
      if (quoted && quoted.index == inner.from) {
        let changes = state.changes([{from: prevLine.from + quoted.index, to: prevLine.to},
                                     {from: line.from + inner.from, to: line.to}])
        return {range: range.map(changes), changes}
      }
    }

    let changes: ChangeSpec[] = []
    if (inner.node.name == "OrderedList") renumberList(inner.item!, doc, changes)
    let continued = inner.item && inner.item.from < line.from
    let insert = ""
    // If not dedented
    if (!continued || /^[\s\d.)\-+*>]*/.exec(line.text)![0].length >= inner.to) {
      for (let i = 0, e = context.length - 1; i <= e; i++) {
        insert += i == e && !continued ? context[i].marker(doc, 1)
          : context[i].blank(i < e ? countColumn(line.text, 4, context[i + 1].from) - insert.length : null)
      }
    }
    let from = pos
    while (from > line.from && /\s/.test(line.text.charAt(from - line.from - 1))) from--
    insert = normalizeIndent(insert, state)
    if (nonTightList(inner.node, state.doc)) insert = blankLine(context, state, line) + state.lineBreak + insert
    changes.push({from, to: pos, insert: state.lineBreak + insert})
    return {range: EditorSelection.cursor(from + insert.length + 1), changes}
  })
  if (dont) return false
  dispatch(state.update(changes, {scrollIntoView: true, userEvent: "input"}))
  return true
}

function isMark(node: SyntaxNode) {
  return node.name == "QuoteMark" || node.name == "ListMark"
}

function nonTightList(node: SyntaxNode, doc: Text) {
  if (node.name != "OrderedList" && node.name != "BulletList") return false
  let first = node.firstChild!, second = node.getChild("ListItem", "ListItem")
  if (!second) return false
  let line1 = doc.lineAt(first.to), line2 = doc.lineAt(second.from)
  let empty = /^[\s>]*$/.test(line1.text)
  return line1.number + (empty ? 0 : 1) < line2.number
}

function blankLine(context: Context[], state: EditorState, line: Line) {
  let insert = ""
  for (let i = 0, e = context.length - 2; i <= e; i++) {
    insert += context[i].blank(i < e
      ? countColumn(line.text, 4, context[i + 1].from) - insert.length
      : null, i < e)
  }
  return normalizeIndent(insert, state)
}

function contextNodeForDelete(tree: Tree, pos: number) {
  let node = tree.resolveInner(pos, -1), scan = pos
  if (isMark(node)) {
    scan = node.from
    node = node.parent!
  }
  for (let prev; prev = node.childBefore(scan);) {
    if (isMark(prev)) {
      scan = prev.from
    } else if (prev.name == "OrderedList" || prev.name == "BulletList") {
      node = prev.lastChild!
      scan = node.to
    } else {
      break
    }
  }
  return node
}

/// This command will, when invoked in a Markdown context with the
/// cursor directly after list or blockquote markup, delete one level
/// of markup. When the markup is for a list, it will be replaced by
/// spaces on the first invocation (a further invocation will delete
/// the spaces), to make it easy to continue a list.
///
/// When not after Markdown block markup, this command will return
/// false, so it is intended to be bound alongside other deletion
/// commands, with a higher precedence than the more generic commands.
export const deleteMarkupBackward: StateCommand = ({state, dispatch}) => {
  let tree = syntaxTree(state)
  let dont = null, changes = state.changeByRange(range => {
    let pos = range.from, {doc} = state
    if (range.empty && markdownLanguage.isActiveAt(state, range.from)) {
      let line = doc.lineAt(pos)
      let context = getContext(contextNodeForDelete(tree, pos), doc)
      if (context.length) {
        let inner = context[context.length - 1]
        let spaceEnd = inner.to - inner.spaceAfter.length + (inner.spaceAfter ? 1 : 0)
        // Delete extra trailing space after markup
        if (pos - line.from > spaceEnd && !/\S/.test(line.text.slice(spaceEnd, pos - line.from)))
          return {range: EditorSelection.cursor(line.from + spaceEnd),
                  changes: {from: line.from + spaceEnd, to: pos}}
        if (pos - line.from == spaceEnd &&
            // Only apply this if we're on the line that has the
            // construct's syntax, or there's only indentation in the
            // target range
            (!inner.item || line.from <= inner.item.from || !/\S/.test(line.text.slice(0, inner.to)))) {
          let start = line.from + inner.from
          // Replace a list item marker with blank space
          if (inner.item && inner.node.from < inner.item.from && /\S/.test(line.text.slice(inner.from, inner.to))) {
            let insert = inner.blank(countColumn(line.text, 4, inner.to) - countColumn(line.text, 4, inner.from))
            if (start == line.from) insert = normalizeIndent(insert, state)
            return {range: EditorSelection.cursor(start + insert.length),
                    changes: {from: start, to: line.from + inner.to, insert}}
          }
          // Delete one level of indentation
          if (start < pos)
            return {range: EditorSelection.cursor(start), changes: {from: start, to: pos}}
        }
      }
    }
    return dont = {range}
  })
  if (dont) return false
  dispatch(state.update(changes, {scrollIntoView: true, userEvent: "delete"}))
  return true
}
