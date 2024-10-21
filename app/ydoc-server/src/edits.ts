/**
 * @file A module responsible for translating file edits between the Yjs document updates and the
 * Language server protocol structures.
 */

import diff from 'fast-diff'
import type { ModuleUpdate } from 'ydoc-shared/ast'
import { MutableModule, print, spanMapToIdMap } from 'ydoc-shared/ast'
import { EnsoFileParts } from 'ydoc-shared/ensoFile'
import { TextEdit } from 'ydoc-shared/languageServerTypes'
import { assert } from 'ydoc-shared/util/assert'
import { IdMap, ModuleDoc, type VisualizationMetadata } from 'ydoc-shared/yjsModel'
import * as fileFormat from './fileFormat'

/**
 * The simulated metadata of this size takes c.a. 1 second on my machine. It should be quite
 * bearable, even on slower machines.
 *
 * Full benchmark results (from edits.bench.ts):
 *   name                hz        min        max       mean        p75        p99       p995       p999      rme  samples
 * · Diffing 10000   8.7370     108.66     132.93     114.46     111.73     132.93     132.93     132.93  ±11.28%        5
 * · Diffing 15000   4.0483     239.82     257.99     247.02     257.99     257.99     257.99     257.99   ±9.71%        3
 * · Diffing 20000   2.1577     462.40     464.52     463.46     464.52     464.52     464.52     464.52   ±2.90%        2
 * · Diffing 25000   1.3744     727.61     727.61     727.61     727.61     727.61     727.61     727.61   ±0.00%        1
 * · Diffing 30000   0.9850   1,015.25   1,015.25   1,015.25   1,015.25   1,015.25   1,015.25   1,015.25   ±0.00%        1
 * · Diffing 35000   0.6934   1,442.27   1,442.27   1,442.27   1,442.27   1,442.27   1,442.27   1,442.27   ±0.00%        1
 * · Diffing 40000   0.5141   1,945.24   1,945.24   1,945.24   1,945.24   1,945.24   1,945.24   1,945.24   ±0.00%        1
 * · Diffing 50000   0.3315   3,016.59   3,016.59   3,016.59   3,016.59   3,016.59   3,016.59   3,016.59   ±0.00%        1
 * · Diffing 60000   0.2270   4,405.46   4,405.46   4,405.46   4,405.46   4,405.46   4,405.46   4,405.46   ±0.00%        1
 * · Diffing 70000   0.1602   6,240.52   6,240.52   6,240.52   6,240.52   6,240.52   6,240.52   6,240.52   ±0.00%        1
 * · Diffing 80000   0.1233   8,110.54   8,110.54   8,110.54   8,110.54   8,110.54   8,110.54   8,110.54   ±0.00%        1
 * · Diffing 90000   0.0954  10,481.47  10,481.47  10,481.47  10,481.47  10,481.47  10,481.47  10,481.47   ±0.00%        1
 * · Diffing 100000  0.0788  12,683.46  12,683.46  12,683.46  12,683.46  12,683.46  12,683.46  12,683.46   ±0.00%        1
 * · Diffing 250000  0.0107  93,253.97  93,253.97  93,253.97  93,253.97  93,253.97  93,253.97  93,253.97   ±0.00%        1
 */
const MAX_SIZE_FOR_NORMAL_DIFF = 30000

interface AppliedUpdates {
  newCode: string | undefined
  newIdMap: IdMap | undefined
  newMetadata: fileFormat.IdeMetadata['node'] | undefined
}

/** Return an object containing updated versions of relevant fields, given an update payload. */
export function applyDocumentUpdates(
  doc: ModuleDoc,
  synced: EnsoFileParts,
  update: ModuleUpdate,
): AppliedUpdates {
  const codeChanged = update.nodesUpdated.size || update.nodesAdded.size || update.nodesDeleted.size
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
    root.visitRecursiveAst(ast => {
      let pos = ast.nodeMetadata.get('position')
      const vis = ast.nodeMetadata.get('visualization')
      const colorOverride = ast.nodeMetadata.get('colorOverride')
      if (vis && !pos) pos = { x: 0, y: 0 }
      if (pos) {
        newMetadata![ast.externalId] = {
          position: { vector: [Math.round(pos.x), Math.round(-pos.y)] },
          visualization: vis && translateVisualizationToFile(vis),
          colorOverride,
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
  }
  return {
    show: vis.visible,
    width: vis.width ?? undefined,
    height: vis.height ?? undefined,
    ...(project == null || vis.identifier == null ?
      {}
    : {
        project: project,
        name: vis.identifier.name,
      }),
  }
}

/**
 * Convert from the serialized file representation of visualization metadata
 * to the internal representation.
 */
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
    width: vis.width ?? null,
    height: vis.height ?? null,
  }
}

/**
 * A simplified diff algorithm.
 *
 * The `fast-diff` package uses Myers' https://neil.fraser.name/writing/diff/myers.pdf with some
 * optimizations to generate minimal diff. Unfortunately, event this algorithm is still too slow
 * for our metadata. Therefore we need to use faster algorithm which will not produce theoretically
 * minimal diff.
 *
 * This is quick implementation making diff which just replaces entire string except common prefix
 * and suffix.
 */
export function stupidFastDiff(oldString: string, newString: string): diff.Diff[] {
  const minLength = Math.min(oldString.length, newString.length)
  let commonPrefixLen, commonSuffixLen
  for (commonPrefixLen = 0; commonPrefixLen < minLength; ++commonPrefixLen)
    if (oldString[commonPrefixLen] !== newString[commonPrefixLen]) break
  if (oldString.length === newString.length && oldString.length === commonPrefixLen)
    return [[0, oldString]]
  for (commonSuffixLen = 0; commonSuffixLen < minLength - commonPrefixLen; ++commonSuffixLen)
    if (oldString.at(-1 - commonSuffixLen) !== newString.at(-1 - commonSuffixLen)) break
  const commonPrefix = oldString.substring(0, commonPrefixLen)
  const removed = oldString.substring(commonPrefixLen, oldString.length - commonSuffixLen)
  const added = newString.substring(commonPrefixLen, newString.length - commonSuffixLen)
  const commonSuffix = oldString.substring(oldString.length - commonSuffixLen, oldString.length)
  return (commonPrefix ? ([[0, commonPrefix]] as diff.Diff[]) : [])
    .concat(removed ? [[-1, removed]] : [])
    .concat(added ? [[1, added]] : [])
    .concat(commonSuffix ? [[0, commonSuffix]] : [])
}

/** Return a list of text edits describing how to turn one string into another. */
export function applyDiffAsTextEdits(
  lineOffset: number,
  oldString: string,
  newString: string,
): TextEdit[] {
  const changes =
    oldString.length + newString.length > MAX_SIZE_FOR_NORMAL_DIFF ?
      stupidFastDiff(oldString, newString)
    : diff(oldString, newString)
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
        numLineBreaks > 0 ?
          text.length - (text.lastIndexOf('\n') + 1)
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

/** Pretty print a code diff for display in the terminal using ANSI escapes to control text colors. */
export function prettyPrintDiff(from: string, to: string): string {
  const colReset = '\x1b[0m'
  const colRed = '\x1b[31m'
  const colGreen = '\x1b[32m'

  const diffs =
    from.length + to.length > MAX_SIZE_FOR_NORMAL_DIFF ? stupidFastDiff(from, to) : diff(from, to)
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
