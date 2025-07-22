<!-- NOTE: README.md is generated from src/README.md -->

# @codemirror/lang-markdown [![NPM version](https://img.shields.io/npm/v/@codemirror/lang-markdown.svg)](https://www.npmjs.org/package/@codemirror/lang-markdown)

[ [**WEBSITE**](https://codemirror.net/) | [**ISSUES**](https://github.com/codemirror/dev/issues) | [**FORUM**](https://discuss.codemirror.net/c/next/) | [**CHANGELOG**](https://github.com/codemirror/lang-markdown/blob/main/CHANGELOG.md) ]

This package implements Markdown language support for the
[CodeMirror](https://codemirror.net/) code editor.

The [project page](https://codemirror.net/) has more information, a
number of [examples](https://codemirror.net/examples/) and the
[documentation](https://codemirror.net/docs/).

This code is released under an
[MIT license](https://github.com/codemirror/lang-markdown/tree/main/LICENSE).

We aim to be an inclusive, welcoming community. To make that explicit,
we have a [code of
conduct](http://contributor-covenant.org/version/1/1/0/) that applies
to communication around the project.

## Usage

```javascript
import {EditorView, basicSetup} from "codemirror"
import {markdown} from "@codemirror/lang-markdown"

const view = new EditorView({
  parent: document.body,
  doc: `*CodeMirror* Markdown \`mode\``,
  extensions: [basicSetup, markdown()]
})
```

## API Reference

@markdown

@markdownLanguage

@commonmarkLanguage

@insertNewlineContinueMarkup

@deleteMarkupBackward

@markdownKeymap
