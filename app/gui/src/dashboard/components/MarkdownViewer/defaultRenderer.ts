/** @file The Vue MarkdownEditor wrapped for use from React. */
import { lazyVueComponent } from '#/utilities/vue'

export const MarkdownEditor = lazyVueComponent(() => import('@/components/MarkdownEditor.vue'))
