/**
 * @file A module responsible for translating file edits between the Yjs document updates and the
 * Language server protocol structures.
 */

import diff from 'fast-diff'
import type { ModuleUpdate } from '../shared/ast'
import { MutableModule, print, spanMapToIdMap } from '../shared/ast'
import { EnsoFileParts } from '../shared/ensoFile'
import { TextEdit } from '../shared/languageServerTypes'
import { assert } from '../shared/util/assert'
import { IdMap, ModuleDoc, type VisualizationMetadata } from '../shared/yjsModel'
import * as fileFormat from './fileFormat'

interface AppliedUpdates {
  newCode: string | undefined
  newIdMap: IdMap | undefined
  newMetadata: fileFormat.IdeMetadata['node'] | undefined
}

export function applyDocumentUpdates(
  doc: ModuleDoc,
  synced: EnsoFileParts,
  update: ModuleUpdate,
): AppliedUpdates {
  const codeChanged = update.fieldsUpdated.length !== 0
  let idsChanged = false
  let metadataChanged = false
  for (const { changes } of update.metadataUpdated) {
    for (const [key] of changes) {
      if (key === 'externalId') {
        idsChanged = true
      } else {
        metadataChanged = true
      }
    }
    if (idsChanged && metadataChanged) break
  }

  let newIdMap = undefined
  let newCode = undefined
  let newMetadata = undefined

  const syncModule = new MutableModule(doc.ydoc)
  const root = syncModule.root()
  assert(root != null)
  if (codeChanged || idsChanged || synced.idMapJson == null) {
    const { code, info } = print(root)
    if (codeChanged) newCode = code
    newIdMap = spanMapToIdMap(info)
  }
  if (codeChanged || idsChanged || metadataChanged) {
    // Update the metadata object.
    // Depth-first key order keeps diffs small.
    newMetadata = {} satisfies fileFormat.IdeMetadata['node']
    root.visitRecursiveAst((ast) => {
      let pos = ast.nodeMetadata.get('position')
      const vis = ast.nodeMetadata.get('visualization')
      if (vis && !pos) pos = { x: 0, y: 0 }
      if (pos) {
        newMetadata![ast.externalId] = {
          position: { vector: [Math.round(pos.x), Math.round(-pos.y)] },
          visualization: vis && translateVisualizationToFile(vis),
        }
      }
    })
  }

  return { newCode, newIdMap, newMetadata }
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
