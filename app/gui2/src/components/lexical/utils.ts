import { $createParagraphNode, $createTextNode, $getRoot, $setSelection } from 'lexical'

export function $setRootText(text: string) {
  if (text === $getRoot().getTextContent()) return
  const root = $getRoot()
  root.clear()
  const paragraph = $createParagraphNode()
  paragraph.append($createTextNode(text))
  root.append(paragraph)
  $setSelection(null)
}
