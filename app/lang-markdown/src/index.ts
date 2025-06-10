import {Prec, EditorState} from "@codemirror/state"
import {KeyBinding, keymap} from "@codemirror/view"
import {Language, LanguageSupport, LanguageDescription, syntaxTree} from "@codemirror/language"
import {Completion, CompletionContext} from "@codemirror/autocomplete"
import {MarkdownExtension, MarkdownParser, parseCode} from "@lezer/markdown"
import {html, htmlCompletionSource} from "@codemirror/lang-html"
import {commonmarkLanguage, markdownLanguage, mkLang, getCodeParser} from "./markdown"
import {insertNewlineContinueMarkup, deleteMarkupBackward} from "./commands"
export {commonmarkLanguage, markdownLanguage, insertNewlineContinueMarkup, deleteMarkupBackward}

/// A small keymap with Markdown-specific bindings. Binds Enter to
/// [`insertNewlineContinueMarkup`](#lang-markdown.insertNewlineContinueMarkup)
/// and Backspace to
/// [`deleteMarkupBackward`](#lang-markdown.deleteMarkupBackward).
export const markdownKeymap: readonly KeyBinding[] = [
  {key: "Enter", run: insertNewlineContinueMarkup},
  {key: "Backspace", run: deleteMarkupBackward}
]

const htmlNoMatch = html({matchClosingTags: false})

/// Markdown language support.
export function markdown(config: {
  /// When given, this language will be used by default to parse code
  /// blocks.
  defaultCodeLanguage?: Language | LanguageSupport,
  /// A source of language support for highlighting fenced code
  /// blocks. When it is an array, the parser will use
  /// [`LanguageDescription.matchLanguageName`](#language.LanguageDescription^matchLanguageName)
  /// with the fenced code info to find a matching language. When it
  /// is a function, will be called with the info string and may
  /// return a language or `LanguageDescription` object.
  codeLanguages?: readonly LanguageDescription[] | ((info: string) => Language | LanguageDescription | null),
  /// Set this to false to disable installation of the Markdown
  /// [keymap](#lang-markdown.markdownKeymap).
  addKeymap?: boolean,
  /// Markdown parser
  /// [extensions](https://github.com/lezer-parser/markdown#user-content-markdownextension)
  /// to add to the parser.
  extensions?: MarkdownExtension,
  /// The base language to use. Defaults to
  /// [`commonmarkLanguage`](#lang-markdown.commonmarkLanguage).
  base?: Language,
  /// By default, the extension installs an autocompletion source that
  /// completes HTML tags when a `<` is typed. Set this to false to
  /// disable this.
  completeHTMLTags?: boolean
  /// By default, HTML tags in the document are handled by the [HTML
  /// language](https://github.com/codemirror/lang-html) package with
  /// tag matching turned off. You can pass in an alternative language
  /// configuration here if you want.
  htmlTagLanguage?: LanguageSupport
} = {}) {
  let {codeLanguages,
       defaultCodeLanguage,
       addKeymap = true,
       base: {parser} = commonmarkLanguage,
       completeHTMLTags = true,
       htmlTagLanguage = htmlNoMatch} = config
  if (!(parser instanceof MarkdownParser)) throw new RangeError("Base parser provided to `markdown` should be a Markdown parser")
  let extensions = config.extensions ? [config.extensions] : []
  let support = [htmlTagLanguage.support], defaultCode
  if (defaultCodeLanguage instanceof LanguageSupport) {
    support.push(defaultCodeLanguage.support)
    defaultCode = defaultCodeLanguage.language
  } else if (defaultCodeLanguage) {
    defaultCode = defaultCodeLanguage
  }
  let codeParser = codeLanguages || defaultCode ? getCodeParser(codeLanguages, defaultCode) : undefined
  extensions.push(parseCode({codeParser, htmlParser: htmlTagLanguage.language.parser}))
  if (addKeymap) support.push(Prec.high(keymap.of(markdownKeymap)))
  let lang = mkLang(parser.configure(extensions))
  if (completeHTMLTags) support.push(lang.data.of({autocomplete: htmlTagCompletion}))
  return new LanguageSupport(lang, support)
}

function htmlTagCompletion(context: CompletionContext) {
  let {state, pos} = context, m = /<[:\-\.\w\u00b7-\uffff]*$/.exec(state.sliceDoc(pos - 25, pos))
  if (!m) return null
  let tree = syntaxTree(state).resolveInner(pos, -1)
  while (tree && !tree.type.isTop) {
    if (tree.name == "CodeBlock" || tree.name == "FencedCode" || tree.name == "ProcessingInstructionBlock" ||
        tree.name == "CommentBlock" || tree.name == "Link" || tree.name == "Image") return null
    tree = tree.parent!
  }

  return {
    from: pos - m[0].length, to: pos,
    options: htmlTagCompletions(),
    validFor: /^<[:\-\.\w\u00b7-\uffff]*$/
  }
}

let _tagCompletions: readonly Completion[] | null = null
function htmlTagCompletions() {
  if (_tagCompletions) return _tagCompletions
  let result = htmlCompletionSource(new CompletionContext(EditorState.create({extensions: htmlNoMatch}), 0, true))
  return _tagCompletions = result ? result.options : []
}
