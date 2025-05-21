/** @file Constants for `UserBar`. */
import type { TextId } from 'enso-common/src/text'
import * as z from 'zod'

/** The type of topbar link data. */
export interface UserBarLinks extends z.infer<typeof TOPBAR_LINKS_SCHEMA> {}

export const TOPBAR_LINKS_SCHEMA = z.object({
  items: z.array(
    z
      .object({
        name: z.custom<TextId>(),
        url: z.string().url(),
        menu: z.array(
          z.object({
            name: z.custom<TextId>().and(z.string()),
            url: z.string().url(),
          }),
        ),
      })
      .or(
        z.object({
          name: z.custom<TextId>(),
          menu: z.array(
            z.object({
              name: z.custom<TextId>().and(z.string()),
              url: z.string().url(),
            }),
          ),
        }),
      )
      .or(
        z.object({
          name: z.custom<TextId>().and(z.string()),
          url: z.string().url(),
        }),
      ),
  ),
})
