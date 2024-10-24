import { isUrlString } from '@/util/data/urlString'
import { isIconName } from '@/util/iconMetadata/iconName'
import type { DefineComponent, PropType } from 'vue'
import * as z from 'zod'

/* eslint-disable @typescript-eslint/no-empty-object-type */
export type Visualization =
  | DefineComponent<
      // Props
      { data: { type: PropType<any>; required: true } },
      {},
      unknown,
      {},
      {},
      {},
      {},
      // Emits
      {
        'update:preprocessor'(module: string, method: string, ...args: string[]): void
      }
    >
  | DefineComponent<
      // Props
      { data: { type: PropType<any>; required: true } },
      {},
      unknown,
      {},
      {},
      {},
      {},
      // Emits
      {}
    >
/* eslint-enable @typescript-eslint/no-empty-object-type */

export const VisualizationModule = z.object({
  // This is UNSAFE, but unavoidable as the type of `Visualization` is impossible to statically
  // check, since it is a Vue component. Instead any type errors will be caught by Vue
  // when trying to mount the visualization, and replaced with a 'Loading Error' visualization.
  default: z.custom<Visualization>(() => true),
  name: z.string(),
  // The name of an icon, or a URL or data URL. If it contains `:`, it is assumed to be a URL.
  icon: z
    .string()
    .transform((s) => {
      if (isIconName(s) || isUrlString(s)) return s
      console.warn(`Invalid icon name '${s}'`)
      return undefined
    })
    .optional(),
  inputType: z.string().optional(),
  defaultPreprocessor: (
    z.string().array().min(2) as unknown as z.ZodType<
      [module: string, method: string, ...args: string[]]
    >
  )
    .readonly()
    .optional(),
  scripts: z.string().array().optional(),
  styles: z.string().array().optional(),
})
export type VisualizationModule = z.infer<typeof VisualizationModule>
