/**
 * @file A module responsible for translating file edits between the Yjs document updates and the
 * Language server protocol structures.
 */

import diff from 'fast-diff'
import * as json from 'lib0/json'
import { inspect } from 'node:util'
import * as Y from 'yjs'
import { Position, TextEdit } from '../shared/languageServerTypes'
import { ModuleDoc, NodeMetadata, decodeRange } from '../shared/yjsModel'
import * as fileFormat from './fileFormat'

interface AppliedUpdates {
  edits: TextEdit[]
  newContent: string
  newMetadata: fileFormat.Metadata
}

const META_TAG = '#### METADATA ####'

export function applyDocumentUpdates(
  doc: ModuleDoc,
  syncedMeta: fileFormat.Metadata,
  syncedContent: string,
  contentDelta: Y.YTextEvent['delta'] | null,
  idMapKeys: Y.YMapEvent<Uint8Array>['keys'] | null,
  metadataKeys: Y.YMapEvent<NodeMetadata>['keys'] | null,
): AppliedUpdates {
  const synced = preParseContent(syncedContent)
  let newContent = ''

  const allEdits: TextEdit[] = []
  if (contentDelta && contentDelta.length > 0) {
    const { code, edits } = convertDeltaToTextEdits(synced.code, contentDelta)
    newContent += code
    allEdits.push(...edits)
  } else {
    newContent += synced.code
  }

  const metaStartLine = (newContent.match(/\n/g) ?? []).length
  let metaContent = META_TAG + '\n'

  if (idMapKeys != null || synced.idMapJson == null || (contentDelta && contentDelta.length > 0)) {
    const idMapJson = json.stringify(idMapToArray(doc.idMap))
    metaContent += idMapJson + '\n'
  } else {
    metaContent += (synced.idMapJson ?? '[]') + '\n'
  }

  let newMetadata = syncedMeta
  if (metadataKeys != null) {
    const nodeMetadata = { ...syncedMeta.ide.node }
    for (const [key, op] of metadataKeys) {
      switch (op.action) {
        case 'delete':
          delete nodeMetadata[key]
          break
        case 'add':
        case 'update': {
          const updatedMeta = doc.metadata.get(key)
          const oldMeta = nodeMetadata[key] ?? {}
          if (updatedMeta == null) continue
          nodeMetadata[key] = {
            ...oldMeta,
            position: {
              vector: [updatedMeta.x, updatedMeta.y],
            },
          }
          break
        }
      }
    }
    // Update the metadata object without changing the original order of keys.
    newMetadata = { ...syncedMeta }
    newMetadata.ide = { ...syncedMeta.ide }
    newMetadata.ide.node = nodeMetadata
    const metadataJson = json.stringify(newMetadata)
    metaContent += metadataJson
  } else {
    metaContent += synced.metadataJson ?? '{}'
  }

  const oldMetaContent = syncedContent.slice(synced.code.length)
  allEdits.push(...applyDiffAsTextEdits(metaStartLine, oldMetaContent, metaContent))
  newContent += metaContent

  return {
    edits: allEdits,
    newContent,
    newMetadata: newMetadata,
  }
}

export function convertDeltaToTextEdits(
  prevText: string,
  contentDelta: Y.YTextEvent['delta'],
): { code: string; edits: TextEdit[] } {
  const edits = []
  let index = 0
  let newIndex = 0
  let lineNum = 0
  let lineStartIdx = 0
  let code = ''
  for (const op of contentDelta) {
    if (op.insert != null && typeof op.insert === 'string') {
      const pos = {
        character: newIndex - lineStartIdx,
        line: lineNum,
      }
      // if the last edit was a delete on the same position, we can merge the insert into it
      const lastEdit = edits[edits.length - 1]
      if (
        lastEdit &&
        lastEdit.text.length === 0 &&
        lastEdit.range.start.line === pos.line &&
        lastEdit.range.start.character === pos.character
      ) {
        lastEdit.text = op.insert
      } else {
        edits.push({ range: { start: pos, end: pos }, text: op.insert })
      }
      const numLineBreaks = (op.insert.match(/\n/g) ?? []).length
      if (numLineBreaks > 0) {
        lineNum += numLineBreaks
        lineStartIdx = newIndex + op.insert.lastIndexOf('\n') + 1
      }
      code += op.insert
      newIndex += op.insert.length
    } else if (op.delete != null) {
      const start = {
        character: newIndex - lineStartIdx,
        line: lineNum,
      }
      const deleted = prevText.slice(index, index + op.delete)
      const numLineBreaks = (deleted.match(/\n/g) ?? []).length
      const character =
        numLineBreaks > 0
          ? deleted.length - (deleted.lastIndexOf('\n') + 1)
          : newIndex - lineStartIdx + op.delete
      const end = {
        character,
        line: lineNum + numLineBreaks,
      }
      edits.push({ range: { start, end }, text: '' })
      index += op.delete
    } else if (op.retain != null) {
      const retained = prevText.slice(index, index + op.retain)
      const numLineBreaks = (retained.match(/\n/g) ?? []).length
      lineNum += numLineBreaks
      if (numLineBreaks > 0) {
        lineStartIdx = newIndex + retained.lastIndexOf('\n') + 1
      }
      code += retained
      index += op.retain
      newIndex += op.retain
    }
  }
  code += prevText.slice(index)
  return { code, edits }
}

interface PreParsedContent {
  code: string
  idMapJson: string | null
  metadataJson: string | null
}

export function preParseContent(content: string): PreParsedContent {
  const splitPoint = content.lastIndexOf(META_TAG)
  if (splitPoint < 0) {
    return {
      code: content,
      idMapJson: null,
      metadataJson: null,
    }
  }
  const code = content.slice(0, splitPoint)
  const metadataString = content.slice(splitPoint + META_TAG.length)
  const metaLines = metadataString.trim().split('\n')
  const idMapJson = metaLines[0] ?? null
  const metadataJson = metaLines[1] ?? null
  return { code, idMapJson, metadataJson }
}

function idMapToArray(map: Y.Map<Uint8Array>): fileFormat.IdMapEntry[] {
  const entries: fileFormat.IdMapEntry[] = []
  const doc = map.doc!
  map.forEach((rangeBuffer, id) => {
    const decoded = decodeRange(rangeBuffer)
    const index = Y.createAbsolutePositionFromRelativePosition(decoded[0], doc)?.index
    const endIndex = Y.createAbsolutePositionFromRelativePosition(decoded[1], doc)?.index
    if (index == null || endIndex == null) return
    const size = endIndex - index
    entries.push([{ index: { value: index }, size: { value: size } }, id])
  })
  entries.sort(idMapCmp)
  return entries
}

function idMapCmp(a: fileFormat.IdMapEntry, b: fileFormat.IdMapEntry) {
  const val1 = a[0]?.index?.value ?? 0
  const val2 = b[0]?.index?.value ?? 0
  if (val1 === val2) {
    const size1 = a[0]?.size.value ?? 0
    const size2 = b[0]?.size.value ?? 0
    return size1 - size2
  }
  return val1 - val2
}

export function applyDiffAsTextEdits(
  lineOffset: number,
  oldString: string,
  newString: string,
): TextEdit[] {
  const changes = diff(oldString, newString)
  let newIndex = 0
  let lineNum = lineOffset
  let lineStartIdx = 0
  const edits = []
  for (const [op, text] of changes) {
    if (op === 1) {
      const pos = {
        character: newIndex - lineStartIdx,
        line: lineNum,
      }
      edits.push({ range: { start: pos, end: pos }, text })
      const numLineBreaks = (text.match(/\n/g) ?? []).length
      if (numLineBreaks > 0) {
        lineNum += numLineBreaks
        lineStartIdx = newIndex + text.lastIndexOf('\n') + 1
      }
      newIndex += text.length
    } else if (op === -1) {
      const start = {
        character: newIndex - lineStartIdx,
        line: lineNum,
      }
      const numLineBreaks = (text.match(/\n/g) ?? []).length
      const character =
        numLineBreaks > 0
          ? text.length - (text.lastIndexOf('\n') + 1)
          : newIndex - lineStartIdx + text.length
      const end = {
        character,
        line: lineNum + numLineBreaks,
      }
      edits.push({ range: { start, end }, text: '' })
    } else if (op === 0) {
      const numLineBreaks = (text.match(/\n/g) ?? []).length
      lineNum += numLineBreaks
      if (numLineBreaks > 0) {
        lineStartIdx = newIndex + text.lastIndexOf('\n') + 1
      }
      newIndex += text.length
    }
  }
  return edits
}

export function prettyPrintDiff(from: string, to: string): string {
  const colReset = '\x1b[0m'
  const colRed = '\x1b[31m'
  const colGreen = '\x1b[32m'

  const diffs = diff(from, to)
  if (diffs.length === 1 && diffs[0]![0] === 0) return 'No changes'
  let content = ''
  for (let i = 0; i < diffs.length; i++) {
    const [op, text] = diffs[i]!
    if (op === 1) {
      content += colGreen + text
    } else if (op === -1) {
      content += colRed + text
    } else if (op === 0) {
      content += colReset
      const numNewlines = (text.match(/\n/g) ?? []).length
      if (numNewlines < 2) {
        content += text
      } else {
        const firstNewline = text.indexOf('\n')
        const lastNewline = text.lastIndexOf('\n')
        const firstLine = text.slice(0, firstNewline + 1)
        const lastLine = text.slice(lastNewline + 1)
        const isFirst = i === 0
        const isLast = i === diffs.length - 1
        if (!isFirst) content += firstLine
        if (!isFirst && !isLast) content += '...\n'
        if (!isLast) content += lastLine
      }
    }
  }
  content += colReset
  return content
}

/** Apply text edits intended for language server to a given starting text. Used for verification
 * during testing if generated edits were correct. This is intentionally a very simple, not
 * performant implementation.
 */
export function applyTextEdits(content: string, edits: TextEdit[]): string {
  let newContent = content
  for (const edit of edits) {
    try {
      newContent = applySingleEdit(newContent, edit)
    } catch (e) {
      throw new ApplyEditError(edit, newContent, e)
    }
  }
  return newContent
}

class ApplyEditError extends Error {
  edit: TextEdit
  content: string
  inner: unknown
  constructor(edit: TextEdit, content: string, inner: unknown) {
    const message =
      `Failed to apply edit ${inspect(edit)} to content:\n${JSON.stringify(content)}\n` +
      String(inner)
    super(message)
    this.edit = edit
    this.content = content
    this.inner = inner
  }
}

function applySingleEdit(content: string, edit: TextEdit): string {
  const startOffset = lsPositionToTextOffset(content, edit.range.start)
  const endOffset = lsPositionToTextOffset(content, edit.range.end)
  return content.slice(0, startOffset) + edit.text + content.slice(endOffset)
}

function lsPositionToTextOffset(content: string, pos: Position): number {
  const lineData = getNthLineLengthAndOffset(content, pos.line)
  if (pos.character > lineData.length) {
    throw new Error(
      `Character ${pos.character} is out of bounds for line ${pos.line}. Line length ${lineData.length}.`,
    )
  }
  return lineData.offset + pos.character
}

function getNthLineLengthAndOffset(
  content: string,
  line: number,
): { offset: number; length: number } {
  let offset = 0
  for (let i = 0; i < line; i++) {
    offset = content.indexOf('\n', offset) + 1
    if (offset <= 0) {
      throw new Error(`Line ${line} not found in content:\n${content}`)
    }
  }
  const nextNewline = content.indexOf('\n', offset)
  const length = nextNewline < 0 ? content.length - offset : nextNewline - offset
  return { offset, length }
}
