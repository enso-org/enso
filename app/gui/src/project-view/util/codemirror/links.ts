import { textEditorsBindings } from '@/bindings'
import { injectKeyboard } from '@/providers/keyboard'
import { useStateEffect } from '@/util/codemirror/reactivity'
import { valueExt } from '@/util/codemirror/stateEffect'
import { type ToValue } from '@/util/reactivity'
import { type EditorView } from '@codemirror/view'
import { toValue } from 'vue'

/** Sets hover text for links in the editor. */
export function useLinkTitles(
  editorView: EditorView,
  { readonly }: { readonly: ToValue<boolean> },
) {
  const keyboard = injectKeyboard()
  useStateEffect(editorView, setLinkAttributesFactory, () =>
    linkAttributeFactory(
      toValue(readonly) ? 'Click to open link in a new window.'
      : keyboard.mod ? `${textEditorsBindings.bindings.openLink.humanReadable} to open link.`
      : `Click to edit; ${textEditorsBindings.bindings.openLink.humanReadable} to open link.`,
    ),
  )
}

export type LinkAttributesFactory = (url: string) => Record<string, string>
export const {
  set: setLinkAttributesFactory,
  get: linkAttributesFactory,
  changed: linkAttributesFactoryChanged,
  extension: linkDecoratorStateExt,
} = valueExt<LinkAttributesFactory>((href) => ({ href }))

function linkAttributeFactory(title: string) {
  return (href: string) => ({
    href,
    title,
    target: '_blank',
  })
}
