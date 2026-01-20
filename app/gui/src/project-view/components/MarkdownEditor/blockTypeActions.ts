import type { BlockType } from '@/components/MarkdownEditor/codemirror/formatting'
import type { DisplayableActionName } from '@/providers/action'
import * as objects from 'enso-common/src/utilities/data/object'

export const blockTypeAction = {
  Paragraph: 'documentationEditor.paragraph',
  ATXHeading1: 'documentationEditor.header1',
  ATXHeading2: 'documentationEditor.header2',
  ATXHeading3: 'documentationEditor.header3',
  BulletList: 'documentationEditor.list',
  OrderedList: 'documentationEditor.numberedList',
  Blockquote: 'documentationEditor.quote',
  FencedCode: 'documentationEditor.code',
} satisfies Record<BlockType, DisplayableActionName>

export type BlockTypeAction = (typeof blockTypeAction)[keyof typeof blockTypeAction]

export const actionBlockType: Record<BlockTypeAction, BlockType> = objects.unsafeFromEntries(
  objects.unsafeEntries(blockTypeAction).map(([key, value]) => [value, key]),
)
