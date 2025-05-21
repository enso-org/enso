import { Err, Ok, Result } from '@/util/data/result'
import * as z from 'zod'

interface Macro {
  description: string
  value: string
}

export const documentationMetadataSchema = z.object({
  advanced: z.boolean().optional(),
  aliases: z.array(z.string()).optional(),
  deprecated: z.boolean().optional(),
  icon: z.string().optional(),
  group: z.string().optional(),
  private: z.boolean().optional(),
  unstable: z.boolean().optional(),
  suggested: z.number().optional(),
  macros: z
    .array(
      z
        .record(z.string(), z.string())
        .refine((obj) => Object.keys(obj).length === 1, {
          message: 'Only a single macro per `macros` array element is allowed',
        })
        .transform((obj) => {
          const [key, value] = Object.entries(obj)[0]!
          return { description: key, value } satisfies Macro
        }),
    )
    .optional(),
  added: z.string().optional(),
  modified: z.string().optional(),
  removed: z.string().optional(),
  upcoming: z.string().optional(),
})

export type DocumentationMetadata = z.infer<typeof documentationMetadataSchema>

/** Validate parsed metadata object against the schema. */
export function validateMetadata(metadata: object): Result<DocumentationMetadata> {
  const result = documentationMetadataSchema.safeParse(metadata)
  if (!result.success) {
    return Err(result.error)
  }
  return Ok(result.data)
}
