/** @file The Vue MarkdownEditor wrapped for use from React. */
import { vueComponent } from '#/utilities/vue'
// eslint-disable-next-line no-restricted-syntax
import MarkdownEditorVue from '@/components/MarkdownEditor.vue'

// eslint-disable-next-line no-restricted-syntax
export const MarkdownEditor = vueComponent(MarkdownEditorVue)
