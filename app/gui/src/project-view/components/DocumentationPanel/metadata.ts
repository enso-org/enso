/** Documentation metadata included in the front-matter section of the Markdown doc comment. */

import { assert } from '@/util/assert'
import { Opt } from '@/util/data/opt'
import { Err, Ok, Result } from '@/util/data/result'
import { SyntaxNode, SyntaxNodeRef } from '@lezer/common'
import { parse } from 'yaml'
import * as z from 'zod'

/** A macro definition in the `macros` array. */
export interface Macro {
  /** The macro name. */
  description: string
  /** The macro expansion. */
  value: string
}

/** The documentation metadata schema. It is based on the representation of the parsed YAML object. */
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

/** Extract metadata front-matter section from Markdown document. */
export function extractMetadata(
  source: string,
  documentation: SyntaxNodeRef,
): Result<Opt<DocumentationMetadata>> {
  const frontMatter = documentation.node.getChild('YAMLFrontMatter')
  const content = frontMatter?.node.getChild('YAMLContent')
  if (!frontMatter || !content) {
    return Ok(undefined)
  }
  return parseMetadata(source.slice.bind(source), content.node)
}

/** Parse metadata front-matter section from 'YAMLContent' markdown node. Will throw if `frontMatterContent` is not a 'YAMLContent' node. */
export function parseMetadata(
  source: (from: number, to: number) => string,
  frontMatterContent: SyntaxNode,
): Result<DocumentationMetadata> {
  assert(frontMatterContent.node.name === 'YAMLContent')
  return validateMetadata(parse(source(frontMatterContent.from, frontMatterContent.to)))
}
