import { z } from 'zod'

/** Intermediate step in the parsing process, when we rename `constructor` field to `kind`.
 *
 * It helps to avoid issues with TypeScript, which considers `constructor` as a reserved keyword in many contexts.
 */
const inputSchema = z
  .object({
    constructor: z.string(),
  })
  .passthrough()
type Input = z.infer<typeof inputSchema>
type WithoutConstructor = Omit<Input, 'constructor'>
type WithKind = WithoutConstructor & { kind: string }
const withKindSchema: z.ZodType<WithKind, z.ZodTypeDef, Input> = inputSchema.transform((value) => {
  const kind = value['constructor'] as string
  return { kind, ...structuredClone(value) }
})

/** Widget display mode. Determines when the widget should be expanded. */
export enum DisplayMode {
  /** The widget should always be in its expanded mode. */
  Always,
  /** The widget should only be in its expanded mode when it has non-default value. */
  WhenModified,
  /** The widget should only be in its expanded mode whe the whole node is expanded. */
  ExpandedOnly,
}

const displaySchema = withKindSchema.pipe(
  z.union([
    z.object({ kind: z.literal('Always') }).transform((_) => DisplayMode.Always),
    z.object({ kind: z.literal('When_Modified') }).transform((_) => DisplayMode.WhenModified),
    z.object({ kind: z.literal('Expanded_Only') }).transform((_) => DisplayMode.ExpandedOnly),
  ]),
)

const withDisplay = z.object({ display: displaySchema })
export type WithDisplay = z.infer<typeof withDisplay>

/** A choosable item in SingleChoice widget. */
const choiceSchema = z.object({
  value: z.string(),
  label: z.string().nullable(),
  parameters: z.lazy(() => z.array(argumentSchema)),
})
export type Choice = z.infer<typeof choiceSchema>

/**
 * An external configuration for a widget retreived from the language server.
 *
 * The expected configuration type is defined as Enso type `Widget` in the following file:
 * distribution/lib/Standard/Base/0.0.0-dev/src/Metadata.enso
 *
 * To avoid ruining forward compatibility, only fields that are used by the IDE are defined here.
 */
// Defining widget definition type explicitly is necessary because of recursive structure
// of some variants, like VectorEditor. Zod can’t handle type inference by itself.
export type WidgetConfiguration =
  | SingleChoice
  | VectorEditor
  | MultiChoice
  | CodeInput
  | BooleanInput
  | NumericInput
  | TextInput
  | FolderBrowse
  | FileBrowse
  | FunctionCall

export interface VectorEditor {
  kind: 'Vector_Editor'
  item_editor: WidgetConfiguration
  item_default: string
}

export interface MultiChoice {
  kind: 'Multi_Choice'
}

export interface CodeInput {
  kind: 'Code_Input'
}

export interface BooleanInput {
  kind: 'Boolean_Input'
}

export interface NumericInput {
  kind: 'Numeric_Input'
  maximum?: number | undefined
  minimum?: number | undefined
}

export interface TextInput {
  kind: 'Text_Input'
}

export interface FolderBrowse {
  kind: 'Folder_Browse'
}

export interface FileBrowse {
  kind: 'File_Browse'
}

export interface SingleChoice {
  kind: 'Single_Choice'
  label: string | null
  values: Choice[]
}

export interface FunctionCall {
  kind: 'FunctionCall'
  parameters: Map<string, (WidgetConfiguration & WithDisplay) | null>
}

export const widgetConfigurationSchema: z.ZodType<
  WidgetConfiguration & WithDisplay,
  z.ZodTypeDef,
  any
> = withKindSchema.pipe(
  z.discriminatedUnion('kind', [
    z
      .object({
        kind: z.literal('Single_Choice'),
        label: z.string().nullable(),
        values: z.array(choiceSchema),
      })
      .merge(withDisplay),
    z
      .object({
        kind: z.literal('Vector_Editor'),
        /* eslint-disable camelcase */
        item_editor: z.lazy(() => widgetConfigurationSchema),
        item_default: z.string(),
        /* eslint-enable camelcase */
      })
      .merge(withDisplay),
    z.object({ kind: z.literal('Multi_Choice') }).merge(withDisplay),
    z.object({ kind: z.literal('Code_Input') }).merge(withDisplay),
    z.object({ kind: z.literal('Boolean_Input') }).merge(withDisplay),
    z
      .object({
        kind: z.literal('Numeric_Input'),
        maximum: z.number().optional(),
        minimum: z.number().optional(),
      })
      .merge(withDisplay),
    z.object({ kind: z.literal('Text_Input') }).merge(withDisplay),
    z.object({ kind: z.literal('Folder_Browse') }).merge(withDisplay),
    z.object({ kind: z.literal('File_Browse') }).merge(withDisplay),
  ]),
)

const argNameSchema = z.string()
const argumentSchema = z.tuple([argNameSchema, widgetConfigurationSchema.nullable()])
export type ArgumentWidgetConfiguration = z.infer<typeof argumentSchema>

export const argsWidgetConfigurationSchema = z.array(argumentSchema)
export type ArgsWidgetConfiguration = z.infer<typeof argsWidgetConfigurationSchema>

export function functionCallConfiguration(parameters: ArgumentWidgetConfiguration[]): FunctionCall {
  return {
    kind: 'FunctionCall',
    parameters: new Map(parameters),
  }
}
