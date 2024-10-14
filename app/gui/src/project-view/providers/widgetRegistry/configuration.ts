import { z } from 'zod'

/**
 * Intermediate step in the parsing process, when we rename `constructor` field to `kind`.
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
  | MultipleChoice
  | CodeInput
  | BooleanInput
  | NumericInput
  | TextInput
  | FolderBrowse
  | FileBrowse
  | FunctionCall
  | OneOfFunctionCalls
  | SomeOfFunctionCalls

export interface VectorEditor {
  kind: 'Vector_Editor'
  item_editor: WidgetConfiguration
  item_default: string
}

export interface MultipleChoice {
  kind: 'Multiple_Choice'
  label: string | null
  values: Choice[]
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
  existing_only?: boolean | undefined
}

export interface SingleChoice {
  kind: 'Single_Choice'
  label: string | null
  values: Choice[]
}

/**
 * Dynamic configuration for a function call with a list of arguments with known dynamic configuration.
 * This kind of config is not provided by the engine directly, but is derived from other config types by widgets.
 */
export interface FunctionCall {
  kind: 'FunctionCall'
  parameters: Map<string, (WidgetConfiguration & WithDisplay) | null>
}

/**
 * Dynamic configuration for one of the possible function calls. It is typically the case for dropdown widget.
 * One of function calls will be chosen by WidgetFunction basing on the actual AST at the call site,
 * and the configuration will be used in child widgets.
 * This kind of config is not provided by the engine directly, but is derived from other config types by widgets.
 */
export interface OneOfFunctionCalls {
  kind: 'OneOfFunctionCalls'
  /**
   * A list of possible function calls and their corresponding configuration.
   * The key is typically a fully qualified or autoscoped name of the function, but in general it can be anything,
   * depending on the widget implementation.
   */
  possibleFunctions: Map<string, FunctionCall>
}

export interface SomeOfFunctionCalls {
  kind: 'SomeOfFunctionCalls'
  possibleFunctions: Map<string, FunctionCall>
}

export const widgetConfigurationSchema: z.ZodType<
  WidgetConfiguration & WithDisplay,
  z.ZodTypeDef,
  any
> = withKindSchema.pipe(
  /* eslint-disable camelcase */
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
        item_editor: z.lazy(() => widgetConfigurationSchema),
        item_default: z.string(),
      })
      .merge(withDisplay),
    z
      .object({
        kind: z.literal('Multiple_Choice'),
        label: z.string().nullable(),
        values: z.array(choiceSchema),
      })
      .merge(withDisplay),
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
    z
      .object({ kind: z.literal('File_Browse'), existing_only: z.boolean().optional() })
      .merge(withDisplay),
    /* eslint-enable camelcase */
  ]),
)

const argNameSchema = z.string()
const argumentSchema = z.tuple([argNameSchema, widgetConfigurationSchema.nullable()])
export type ArgumentWidgetConfiguration = z.infer<typeof argumentSchema>

export const argsWidgetConfigurationSchema = z.array(argumentSchema)
export type ArgsWidgetConfiguration = z.infer<typeof argsWidgetConfigurationSchema>

/**
 * Create {@link WidgetConfiguration} object from parameters received from the engine, possibly
 * applying those to an inherited config received from parent widget.
 */
export function functionCallConfiguration(
  parameters: ArgumentWidgetConfiguration[],
  inherited?: FunctionCall,
): FunctionCall {
  const parametersMap = new Map(inherited?.parameters)
  for (const [name, param] of parameters) {
    parametersMap.set(name, parametersMap.get(name) ?? param)
  }
  return {
    kind: 'FunctionCall',
    parameters: parametersMap,
  }
}

/** A configuration for the inner widget of a single-choice selection widget. */
export function singleChoiceConfiguration(config: SingleChoice): OneOfFunctionCalls {
  return {
    kind: 'OneOfFunctionCalls',
    possibleFunctions: new Map(
      config.values.map((value) => [value.value, functionCallConfiguration(value.parameters)]),
    ),
  }
}

/** A configuration for the inner widget of a multiple-choice selection widget. */
export function multipleChoiceConfiguration(config: MultipleChoice): SomeOfFunctionCalls {
  return {
    kind: 'SomeOfFunctionCalls',
    possibleFunctions: new Map(
      config.values.map((value) => [value.value, functionCallConfiguration(value.parameters)]),
    ),
  }
}
