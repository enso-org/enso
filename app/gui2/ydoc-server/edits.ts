/**
 * @file A module responsible for translating file edits between the Yjs document updates and the
 * Language server protocol structures.
 */

import diff from 'fast-diff'
import * as json from 'lib0/json'
import * as Y from 'yjs'
import { combineFileParts, splitFileContents } from '../shared/ensoFile'
import { TextEdit } from '../shared/languageServerTypes'
import { ModuleDoc, type NodeMetadata, type VisualizationMetadata } from '../shared/yjsModel'
import * as fileFormat from './fileFormat'
import { serializeIdMap } from './serialization'

interface AppliedUpdates {
  edits: TextEdit[]
  newContent: string
  newMetadata: fileFormat.Metadata
}

export function applyDocumentUpdates(
  doc: ModuleDoc,
  syncedMeta: fileFormat.Metadata,
  syncedContent: string,
  dataKeys: Y.YMapEvent<Uint8Array>['keys'] | null,
  metadataKeys: Y.YMapEvent<NodeMetadata>['keys'] | null,
): AppliedUpdates {
  const synced = splitFileContents(syncedContent)

  let codeUpdated = false
  let idMapUpdated = false
  if (dataKeys != null) {
    for (const [key, op] of dataKeys) {
      switch (op.action) {
        case 'add':
        case 'update': {
          if (key === 'code') {
            codeUpdated = true
          } else if (key === 'idmap') {
            idMapUpdated = true
          }
          break
        }
      }
    }
  }

  let newCode: string
  let idMapJson: string
  let metadataJson: string

  const allEdits: TextEdit[] = []
  if (codeUpdated) {
    const text = doc.getCode()
    allEdits.push(...applyDiffAsTextEdits(0, synced.code, text))
    newCode = text
  } else {
    newCode = synced.code
  }

  if (idMapUpdated || synced.idMapJson == null) {
    idMapJson = serializeIdMap(doc.getIdMap())
  } else {
    idMapJson = synced.idMapJson
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
            visualization: updatedMeta.vis
              ? translateVisualizationToFile(updatedMeta.vis)
              : undefined,
          }
          break
        }
      }
    }
    // Update the metadata object without changing the original order of keys.
    newMetadata = { ...syncedMeta }
    newMetadata.ide = { ...syncedMeta.ide }
    newMetadata.ide.node = nodeMetadata
    metadataJson = json.stringify(newMetadata)
  } else {
    metadataJson = synced.metadataJson ?? '{}'
  }

  const newContent = combineFileParts({
    code: newCode,
    idMapJson,
    metadataJson,
  })

  const oldMetaContent = syncedContent.slice(synced.code.length)
  const metaContent = newContent.slice(newCode.length)
  const metaStartLine = (newCode.match(/\n/g) ?? []).length
  allEdits.push(...applyDiffAsTextEdits(metaStartLine, oldMetaContent, metaContent))

  return {
    edits: allEdits,
    newContent,
    newMetadata,
  }
}

function translateVisualizationToFile(
  vis: VisualizationMetadata,
): fileFormat.VisualizationMetadata | undefined {
  let project = undefined
  switch (vis.identifier?.module.kind) {
    case 'Builtin':
      project = { project: 'Builtin' } as const
      break
    case 'CurrentProject':
      project = { project: 'CurrentProject' } as const
      break
    case 'Library':
      project = { project: 'Library', contents: vis.identifier.module.name } as const
      break
    default:
      return { show: vis.visible }
  }
  return {
    name: vis.identifier.name,
    show: vis.visible,
    project,
  }
}

export function translateVisualizationFromFile(
  vis: fileFormat.VisualizationMetadata,
): VisualizationMetadata | undefined {
  let module
  switch (vis.project?.project) {
    case 'Builtin':
      module = { kind: 'Builtin' } as const
      break
    case 'CurrentProject':
      module = { kind: 'CurrentProject' } as const
      break
    case 'Library':
      module = { kind: 'Library', name: vis.project.contents } as const
      break
    default:
      module = null
  }
  return {
    identifier: module && vis.name ? { name: vis.name, module } : null,
    visible: vis.show,
  }
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
