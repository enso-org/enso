/** Documentation metadata included in the front-matter section of the Markdown doc comment. */
import { assert } from '@/util/assert'
import type { Opt } from '@/util/data/opt'
import type { SyntaxNode, SyntaxNodeRef } from '@lezer/common'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { parse as yamlParse, stringify as yamlStringify } from 'yaml'
import * as z from 'zod'

/** A macro definition in the `macros` array. */
export interface Macro {
  /** The macro name. */
  description: string
  /** The macro expansion. */
  value: string
}

function orUndefined(ctx: { error: z.ZodError }) {
  console.error('Parsing of documentation metadata field failed: ', ctx.error)
  return undefined
}

/** The documentation metadata schema. It is based on the representation of the parsed YAML object. */
export const documentationMetadataSchema = z.object({
  advanced: z.boolean().optional().catch(orUndefined),
  aliases: z.array(z.string()).optional().catch(orUndefined),
  deprecated: z.boolean().optional().catch(orUndefined),
  icon: z.string().optional().catch(orUndefined),
  group: z.string().optional().catch(orUndefined),
  private: z.boolean().optional().catch(orUndefined),
  unstable: z.boolean().optional().catch(orUndefined),
  suggested: z.number().optional().catch(orUndefined),
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
    .optional()
    .catch(orUndefined),
  added: z.string().optional().catch(orUndefined),
  modified: z.string().optional().catch(orUndefined),
  removed: z.string().optional().catch(orUndefined),
  upcoming: z.string().optional().catch(orUndefined),
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
  return validateMetadata(yamlParse(source(frontMatterContent.from, frontMatterContent.to)))
}

/** Serialize a frontmatter section for test purposes. */
export function frontmatter(content: DocumentationMetadata): string {
  return `---\n${yamlStringify(content)}---\n`
}
