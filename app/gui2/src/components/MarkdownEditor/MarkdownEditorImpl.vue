<script setup lang="ts">
import FloatingSelectionMenu from '@/components/MarkdownEditor/FloatingSelectionMenu.vue'
import FormattingToolbar from '@/components/MarkdownEditor/FormattingToolbar.vue'
import { imagePlugin } from '@/components/MarkdownEditor/ImagePlugin'
import SelectionFormattingToolbar from '@/components/MarkdownEditor/SelectionFormattingToolbar.vue'
import { lexicalRichTextTheme, useFormatting } from '@/components/MarkdownEditor/formatting'
import {
  provideLexicalImageUrlTransformer,
  type UrlTransformer,
} from '@/components/MarkdownEditor/imageUrlTransformer'
import { listPlugin } from '@/components/MarkdownEditor/listPlugin'
import { markdownPlugin } from '@/components/MarkdownEditor/markdown'
import { useLexical } from '@/components/lexical'
import LexicalContent from '@/components/lexical/LexicalContent.vue'
import LexicalDecorators from '@/components/lexical/LexicalDecorators.vue'
import { shallowRef, toRef, useCssModule, type ComponentInstance } from 'vue'

const markdown = defineModel<string>({ required: true })
const props = defineProps<{
  transformImageUrl?: UrlTransformer | undefined
}>()

const contentElement = shallowRef<ComponentInstance<typeof LexicalContent>>()

provideLexicalImageUrlTransformer(toRef(props, 'transformImageUrl'))

const theme = lexicalRichTextTheme(useCssModule('lexicalTheme'))
const { editor } = useLexical(
  contentElement,
  'MarkdownEditor',
  theme,
  markdownPlugin(markdown, [listPlugin, imagePlugin]),
)
const formatting = useFormatting(editor)
</script>

<template>
  <div class="MarkdownEditor fullHeight">
    <FormattingToolbar :formatting="formatting" @pointerdown.prevent />
    <LexicalContent ref="contentElement" class="fullHeight" @wheel.stop @contextmenu.stop />
    <FloatingSelectionMenu :selectionElement="contentElement">
      <SelectionFormattingToolbar :formatting="formatting" />
    </FloatingSelectionMenu>
    <LexicalDecorators :editor="editor" />
  </div>
</template>

<style scoped>
.fullHeight {
  height: 100%;
}

:deep(.toggledOn) {
  color: black;
  opacity: 0.6;
}
:deep(.toggledOff) {
  color: black;
  opacity: 0.3;
}
:deep(.DropdownMenuButton) {
  color: inherit;
  opacity: inherit;
}
:deep(.DropdownMenuContent .MenuButton) {
  justify-content: unset;
}
</style>

<style module="lexicalTheme" src="@/components/MarkdownEditor/theme.css" />
