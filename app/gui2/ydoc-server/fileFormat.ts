import * as json from 'lib0/json'
import z from 'zod'

export type Vector = z.infer<typeof vector>
export const vector = z.tuple([z.number(), z.number()])

const visualizationProject = z.discriminatedUnion('project', [
  z.object({ project: z.literal('Builtin') }),
  z.object({ project: z.literal('CurrentProject') }),
  z.object({ project: z.literal('Library'), contents: z.string() }),
])

export type VisualizationMetadata = z.infer<typeof visualizationMetadata>
const visualizationMetadata = z
  .object({
    show: z.boolean().default(true),
    project: visualizationProject.optional(),
    name: z.string().optional(),
  })
  .passthrough()

export type NodeMetadata = z.infer<typeof nodeMetadata>
export const nodeMetadata = z
  .object({
    position: z.object({ vector }).catch((ctx) => {
      printError(ctx)
      return { vector: [0, 0] satisfies Vector }
    }),
    visualization: visualizationMetadata.optional().catch(() => undefined),
  })
  .passthrough()

export type ImportMetadata = z.infer<typeof importMetadata>
export const importMetadata = z.object({}).passthrough()

export type IdeMetadata = z.infer<typeof ideMetadata>
export const ideMetadata = z
  .object({
    node: z.record(z.string().uuid(), nodeMetadata),
    import: z.record(z.string(), importMetadata),
  })
  .passthrough()
  .default(() => defaultMetadata().ide)
  .catch((ctx) => {
    printError(ctx)
    return defaultMetadata().ide
  })

export type Metadata = z.infer<typeof metadata>
export const metadata = z
  .object({
    ide: ideMetadata,
  })
  .passthrough()
  .catch((ctx) => {
    printError(ctx)
    return defaultMetadata()
  })

export type IdMapValue = z.infer<typeof idMapValue>
export const idMapValue = z.object({
  value: z.number(),
})

export type IdMapRange = z.infer<typeof idMapRange>
export const idMapRange = z.object({
  index: idMapValue,
  size: idMapValue,
})

export type IdMapEntry = z.infer<typeof idMapEntry>
export const idMapEntry = z.tuple([idMapRange, z.string().uuid()])

export type IdMap = z.infer<typeof idMap>
export const idMap = z.array(idMapEntry).catch((ctx) => {
  printError(ctx)
  return []
})

function defaultMetadata() {
  return {
    ide: {
      node: {},
      import: {},
    },
  }
}

function printError(ctx: { error: z.ZodError; input: any }) {
  console.error('=== METADATA PARSE ERROR ===')
  console.error('Error:', ctx.error.issues)
  console.error('Input:', ctx.input)
  console.error('============================')
}

/**
 * Parses the metadata JSON string if provided. If parts of the metadata are missing or invalid,
 * they are filled in with default values.
 *
 * Failure to parse the metadata JSON string is logged to the console.
 */
export function tryParseMetadataOrFallback(metadataJson: string | undefined | null): Metadata {
  if (metadataJson == null) return defaultMetadata()
  const parsedMeta = tryParseJson(metadataJson)
  return metadata.parse(parsedMeta)
}

export function tryParseIdMapOrFallback(idMapJson: string | undefined | null): IdMap {
  if (idMapJson == null) return []
  const parsedIdMap = tryParseJson(idMapJson)
  return idMap.parse(parsedIdMap)
}

function tryParseJson(jsonString: string) {
  try {
    return json.parse(jsonString)
  } catch (e) {
    console.error('Failed to parse metadata JSON:')
    console.error(e)
    return null
  }
}
