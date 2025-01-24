import { type VueHost } from '@/components/VueHostRender.vue'
import { valueExt } from '@/util/codemirror/stateEffect'

/** A CodeMirror extension for getting and setting a {@link VueHost} for Vue-based decorations. */
export const {
  set: setVueHost,
  get: getVueHost,
  changed: vueHostChanged,
  extension: vueHostExt,
} = valueExt<VueHost | undefined>(undefined)
