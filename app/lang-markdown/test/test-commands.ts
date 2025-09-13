import {EditorState, EditorSelection, StateCommand, Extension} from "@codemirror/state"
import {markdown, deleteMarkupBackward, insertNewlineContinueMarkup} from "@codemirror/lang-markdown"
import {indentUnit} from "@codemirror/language"
import ist from "ist"

function mkState(doc: string, extension?: Extension) {
  let cursors = []
  for (let pos = 0;;) {
    pos = doc.indexOf("|", pos)
    if (pos < 0) break
    cursors.push(EditorSelection.cursor(pos))
    doc = doc.slice(0, pos) + doc.slice(pos + 1)
  }
  return EditorState.create({
    doc,
    selection: cursors.length ? EditorSelection.create(cursors) : undefined,
    extensions: [markdown().language, EditorState.allowMultipleSelections.of(true), extension || []]
  })
}

function stateStr(state: EditorState) {
  let doc = state.doc.toString()
  for (let i = state.selection.ranges.length - 1; i >= 0; i--) {
    let range = state.selection.ranges[i]
    doc = doc.slice(0, range.from) + "|" + doc.slice(range.to)
  }
  return doc
}

const tabs: Extension = [EditorState.tabSize.of(4), indentUnit.of("\t")]

function cmd(state: EditorState, command: StateCommand) {
  command({state, dispatch(tr) { state = tr.state }})
  return state
}

describe("insertNewlineContinueMarkup", () => {
  function test(from: string, to: string, ext?: Extension) {
    ist(stateStr(cmd(mkState(from, ext), insertNewlineContinueMarkup)), to)
  }

  it("doesn't continue anything at the top level", () =>
    test("one|", "one|"))

  it("doesn't do anything in non-Markdown content", () =>
    test("- <div|>", "- <div|>"))

  it("can continue blockquotes", () =>
    test("> one|", "> one\n> |"))

  it("will end blockquotes after two empty lines", () =>
    test("> one\n>\n> |", "> one\n\n|"))

  it("will end nested blockquotes after two empty lines", () =>
    test("> > one\n> >\n> > |", "> > one\n> \n> |"))

  it("can continue nested blockquotes", () =>
    test("> > one|", "> > one\n> > |"))

  it("preserves the absence of a blockquote space", () =>
    test(">>one|", ">>one\n>>|"))

  it("can continue bullet lists with dashes", () =>
    test(" - one|", " - one\n - |"))

  it("can continue bullet lists with asterisks", () =>
    test(" *  one|", " *  one\n *  |"))

  it("can continue bullet lists with plus signs", () =>
    test("+ one|", "+ one\n+ |"))

  it("can continue ordered lists with dots", () =>
    test(" 1. one|", " 1. one\n 2. |"))

  it("can continue ordered lists with parens", () =>
    test("2)  one|", "2)  one\n3)  |"))

  it("can continue lists inside blockquotes", () =>
    test("> - one|", "> - one\n> - |"))

  it("can continue markup for multiple cursors", () =>
    test("> one|\n\n- two|", "> one\n> |\n\n- two\n- |"))

  it("can continue nested lists", () =>
    test(" - one\n    1. two|", " - one\n    1. two\n    2. |"))

  it("will leave space before nested blockquotes", () =>
    test(" - one\n   > quoted|", " - one\n   > quoted\n   > |"))

  it("can drop trailing space when pressing enter in a blockquote", () =>
    test(">  |", ">\n> |"))

  it("can move list markup when pressing enter directly after it", () =>
    test(" - one\n - |", " - one\n\n - |"))

  it("will preserve non-tight list after second item", () =>
    test(" - one\n\n - two|", " - one\n\n - two\n\n - |"))

  it("will preserve tight list after second item", () =>
    test(" - one\n - two|", " - one\n - two\n - |"))

  it("exits tight list after second item", () =>
    test(" - one\n - two\n - |", " - one\n - two\n|"))

  it("can drop list markup after an empty line", () =>
    test(" - one\n\n - |", " - one\n\n|"))

  it("deletes the first list marker", () =>
    test(" - |", "|"))

  it("will keep the current ordered list number when moving a marker", () =>
    test(" 1. one\n 2. |", " 1. one\n\n 2. |"))

  it("can move list markup inside a blockquote", () =>
    test("> 1. one\n> 2. |", "> 1. one\n>\n> 2. |"))

  it("can preserve list tightness inside a blockquote", () =>
    test("> 1. one\n>\n> 2. two|", "> 1. one\n>\n> 2. two\n>\n> 3. |"))

  it("renumbers following ordered list items", () =>
    test("1. one|\n2. two", "1. one\n2. |\n3. two"))

  it("renumbers after removed markers", () =>
    test("1. one\n\n2. |\n\n3. three", "1. one\n\n|\n\n2. three"))

  it("stops renumbering on discontinuities", () =>
    test("1. one|\n2. two\n3. three\n1. four", "1. one\n2. |\n3. two\n4. three\n1. four"))

  it("doesn't fire when the cursor is before the markup depth", () =>
    test("- a\n|bc", "- a\n|bc"))

  it("continues list items", () =>
    test("- a\n  b|", "- a\n  b\n  |"))

  it("continues dedented list items", () =>
    test("- hello\nhello|", "- hello\nhello\n|"))

  it("can lift out of one list level", () =>
    test("1. a\n\n   1. b\n\n   2. |", "1. a\n\n   1. b\n\n2. |"))

  it("can lift out of one list level and renumber", () =>
    test("1. a\n\n   1. b\n\n   2. |\n\n2. d", "1. a\n\n   1. b\n\n2. |\n\n3. d"))

  it("doesn't treat lines with content after the cursor as empty", () => {
    test("1. |x\n2. y", "1.\n2. |x\n3. y")
    test("1. x\n2. |y", "1. x\n2.\n3. |y")
  })

  it("doesn't take effect in fenced code", () => {
    test("- ```foo|", "- ```foo|")
    test("> - ```foo|", "> - ```foo|")
  })

  it("continues nested task lists at the right level", () => {
    test("- [ ] item 1\n  - [ ] item 1.1|",
         "- [ ] item 1\n  - [ ] item 1.1\n  - [ ] |")
  })

  it("properly indents task lists nested 2 deep", () => {
    test("- [ ] item 1\n  - [ ] item 1.1\n    - [ ] item 1.1.1|",
         "- [ ] item 1\n  - [ ] item 1.1\n    - [ ] item 1.1.1\n    - [ ] |")
  })

  it("handles tab-indentation", () => {
    test(" - one\n\t- two|", " - one\n\t- two\n\t- |", tabs)
  })
})

describe("deleteMarkupBackward", () => {
  function test(from: string, to: string, ext?: Extension) {
    ist(stateStr(cmd(mkState(from, ext), deleteMarkupBackward)), to)
  }

  it("does nothing in regular text", () =>
     test("one|", "one|"))

  it("does nothing at the top level", () =>
     test("one\n|", "one\n|"))

  it("can delete blockquote markers", () =>
     test("> |", "|"))

  it("only deletes one marker at a time", () =>
     test("> > |", "> |"))

  it("deletes trailing whitespace", () =>
     test(">   |", "> |"))

  it("clears list markers", () =>
     test(" - one\n - |", " - one\n   |"))

  it("removes extra indentation", () =>
    test("> - one\n> -    |", "> - one\n> - |"))

  it("clears triple-space indentation", () =>
    test(" - one\n   |", " - one\n|"))

  it("clears one level of indentation", () =>
    test("- one\n    - two\n      |", "- one\n    - two\n  |"))

  it("deletes the first list marker immediately", () =>
    test(" - |", "|"))

  it("clears number markers in one go", () =>
    test("1. one\n2. |", "1. one\n   |"))

  it("deletes nested list markers", () =>
    test(" > - |", " > |"))

  it("can delete for multiple cursors", () =>
    test("> |\n> |\n> |", "|\n|\n|"))

  it("does noting in a continued list item", () =>
    test("- Foo\n-\n  |Welcome", "- Foo\n-\n  |Welcome"))

  it("doesn't delete normal text in continued list items", () =>
    test("- \na |b", "- \na |b"))

  it("normalizes whitespace on deleting", () =>
    test("  - one\n  - |", "  - one\n\t|", tabs))
})
