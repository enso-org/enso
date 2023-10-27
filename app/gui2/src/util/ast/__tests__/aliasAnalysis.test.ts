/**
 * The testing process employs a unique DSL-like syntax to denote the anticipated results.
 *
 * Take this Enso program as an example:
 * ```
 * main =
 *     x = 1
 *     y = 2
 *     z = x -> x + y
 * ```
 *
 * It would be annotated in the following manner:
 * ```
 * main =
 *     «1,x» = 1
 *     «2,y» = 2
 *     z = «3,x» -> «3,x» + »2,y«
 *     u = »1,x« + »2,y«
 * ```
 *
 * In general: `»1,x«` indicates that the identifier links to the `x` variable as per the `«1,x»` pattern.
 * Numerical prefixes are used to differentiate variables of the same name, which is permitted by the language via
 * shadowing.
 *
 * All unannotated identifiers are assumed to preexist in the environment (captured from an external scope or imports).
 */

import { assert, expect, test } from 'vitest'
import * as aliasAnalysis from '../aliasAnalysis'
import { AliasAnalyzer, ObjectKeyedMap, ObjectKeyedSet } from '../aliasAnalysis'
import {ContentRange} from "../../../../shared/yjsModel";
import {assertDefined, assertEqual, assertNotEqual} from "../../assert";

/** The type of annotation. */
enum AnnotationType {
  /** An identifier binding. */
  Binding = 'Binding',
  /** An identifier usage. */
  Usage = 'Usage',
}

/** Information about an annotated identifier. */
class Annotation {
  /**
   * @param kind Whether this is an identifier binding or usage.
   * @param id The numerical prefix of the identifier.
   */
  constructor(
      public kind: AnnotationType,
      public id: number,
  ) {}
}

function parseAnnotations(annotatedCode: string): {
  unannotatedCode: string
  annotations: ObjectKeyedMap<ContentRange, Annotation>
} {
  const annotations = new ObjectKeyedMap()

  // Iterate over all annotations (either bindings or usages).
  // I.e. we want to cover both `«1,x»` and `»1,x«` cases, while keeping the track of the annotation type.
  const annotationRegex = /«(\d+),([^»]+)»|»(\d+),([^»]+)«/g

  // As the annotations are removed from the code, we need to keep track of the offset between the annotated and
  // unannotated code. This is necessary to correctly calculate the start and length of the annotations.
  let accumulatedOffset = 0

  const unannotatedCode = annotatedCode.replace(
      annotationRegex,
      (match, bindingPrefix, bindingName, usagePrefix, usageName, offset) => {
        // Sanity check: either both binding prefix and name are present, or both usage prefix and name are present.
        // Otherwise, we have an internal error in the regex.
        assertEqual(bindingPrefix != null, bindingName != null)
        assertEqual(usagePrefix != null, usageName != null)
        assertNotEqual(bindingPrefix != null, usagePrefix != null)

        const id = parseInt(bindingPrefix ?? usagePrefix, 10)
        const name = bindingName ?? usageName
        const kind = bindingPrefix != null ? AnnotationType.Binding : AnnotationType.Usage

        const start = offset - accumulatedOffset
        const end = start + name.length
        const range = [start, end]

        const annotation = new Annotation(kind, id)
        accumulatedOffset += match.length - name.length
        annotations.set(range, annotation)
        return name
      },
  )
  return { unannotatedCode, annotations }
}

/** Alias analysis test case, typically parsed from an annotated code. */
class TestCase {
  /** The expected aliases. */
  expectedAliases: Map<ContentRange, ContentRange[]> = new Map()

  /** The expected unresolved symbols. */
  unresolvedSymbols: Set<ContentRange> = new Set()

  /**
   * @param code The code of the program to be tested, without annotations.
   */
  constructor(public readonly code: string) {}

  /** Parse from the annotated code. */
  static parse(annotatedCode: string): TestCase {
    const { unannotatedCode, annotations } = parseAnnotations(annotatedCode)
    const testCase = new TestCase(unannotatedCode)

    const prefixBindings = new Map<number, ContentRange>()

    for (const [range, annotation] of annotations) {
      if (annotation.kind === AnnotationType.Binding) {
        prefixBindings.set(annotation.id, range)
        testCase.expectedAliases.set(range, [])
      }
    }
    for (const [range, annotation] of annotations) {
      if (annotation.kind === AnnotationType.Usage) {
        const bindingRange = prefixBindings.get(annotation.id)
        if (bindingRange == null) {
          testCase.unresolvedSymbols.add(range)
        } else {
          const usages = testCase.expectedAliases.get(bindingRange)
          assertDefined(usages, `No usages list found for binding with id ${annotation.id}.`)
          usages.push(range)
        }
      }
    }

    return testCase
  }

  run() {
    const analyzer = new AliasAnalyzer(this.code)
    analyzer.process()

    // Check that each expected connection is present.
    for (const [source, targets] of this.expectedAliases) {
      const foundTargets = analyzer.aliases.get(source)
      assertDefined(foundTargets, `Expected binding [${source}] not found.`)
      console.log(`Found expected binding [${source}]`)

      for (const target of targets) {
        const foundConnection = foundTargets.has(target)
        assert(foundConnection, `Expected connection [${source}] -> [${target}] not found.`)
        console.log(`Found expected connection [${source}] -> [${target}]`)
      }
    }

    // Unresolved symbols
    for (const unresolvedSymbol of this.unresolvedSymbols) {
      assert(
          analyzer.unresolvedSymbols.has(unresolvedSymbol),
          `Expected unresolved symbol usage at [${unresolvedSymbol}] not observed.`,
      )
      console.log(`Found expected unresolved symbol usage at [${unresolvedSymbol}]`)
    }
  }

  static parseAndRun(annotatedCode: string) {
    const testCase = TestCase.parse(annotatedCode)
    testCase.run()
  }
}

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest

  test('Annotations parsing', (ctx) => {
    const annotatedCode = `main =
    «1,x» = 1
    «2,y» = 2
    z = «3,x» -> »3,x« + »2,y«
    u = »1,x« + »2,y«`

    const expectedUnannotatedCode = `main =
    x = 1
    y = 2
    z = x -> x + y
    u = x + y`

    const { unannotatedCode, annotations } = parseAnnotations(annotatedCode)
    assert(unannotatedCode === expectedUnannotatedCode)
    assert(annotations.size === 7)
    const validateAnnotation = (
        range: ContentRange,
        kind: AnnotationType,
        prefix: number,
        identifier: string,
    ) => {
      try {
        const a = annotations.get(range)
        assertDefined(a, `Annotation is not defined.`)
        assertEqual(a.kind, kind, 'Invalid annotation kind.')
        assertEqual(a.id, prefix, 'Invalid annotation prefix.')
        assertEqual(
            unannotatedCode.substring(range[0], range[1]),
            identifier,
            'Invalid annotation identifier.',
        )
      } catch (e) {
        const message = `Invalid annotation at [${range}]: ${e}`
        throw new Error(message)
      }
    }

    validateAnnotation([11, 12], AnnotationType.Binding, 1, 'x')
    validateAnnotation([21, 22], AnnotationType.Binding, 2, 'y')
    validateAnnotation([35, 36], AnnotationType.Binding, 3, 'x')
    validateAnnotation([40, 41], AnnotationType.Usage, 3, 'x')
    validateAnnotation([44, 45], AnnotationType.Usage, 2, 'y')
    validateAnnotation([54, 55], AnnotationType.Usage, 1, 'x')
    validateAnnotation([58, 59], AnnotationType.Usage, 2, 'y')
  })

  test('Plain dependency', (ctx) => {
    const code = 'main =\n    «1,x» = 1\n    »1,x«'
    TestCase.parseAndRun(code)
  })

  test('Unresolved dependency', (ctx) => {
    const code = 'main =\n    «1,x» = 1\n    »2,y«'
    TestCase.parseAndRun(code)
  })

  test('Shadowing', (ctx) => {
    const code = `main =
    «1,x» = 1
    «2,y» = 2
    z = «3,x» -> »3,x« + »2,y«
    u = »1,x« + »2,y«`

    TestCase.parseAndRun(code)
  })

  test('Plain symbol usage', () => {
    const code = '»1,x«'
    TestCase.parseAndRun(code)
  })

  test('Accessors', () => {
    const code = `main =
    «1,x» = 1
    »1,x«.x.y »2,arg«`
    TestCase.parseAndRun(code)
  })
}

test('MyMap with number[] keys', () => {
  const map = new ObjectKeyedMap<number[], string>()

  const key1 = [1, 2, 3]
  const key2 = [4, 5, 6]
  const key3 = [1, 2, 3]

  expect(map.size).toBe(0)

  expect(map.has(key1)).toBe(false)
  map.set(key1, 'value1')
  expect(map.size).toBe(1)
  expect(map.get(key1)).toBe('value1')
  expect(map.has(key1)).toBe(true)

  map.set(key2, 'value2')
  expect(map.size).toBe(2)
  expect(map.get(key2)).toBe('value2')
  expect(map.has(key2)).toBe(true)

  expect(map.has(key3)).toBe(true)
  expect(map.get(key3)).toBe('value1')
  map.set(key3, 'value3')
  expect(map.size).toBe(2)
  expect(map.get(key1)).toBe('value3')
  expect(map.get(key3)).toBe('value3')

  map.delete(key1)
  expect(map.size).toBe(1)
  expect(map.has(key1)).toBe(false)
  expect(map.get(key1)).toBeUndefined()
  expect(map.get(key2)).toBe('value2')
  expect(map.get(key3)).toBeUndefined()

  map.clear()
  expect(map.size).toBe(0)
  expect(map.has(key2)).toBe(false)
  expect(map.get(key2)).toBe(undefined)
})

test('ObjectKeyedSet', () => {
  const set = new ObjectKeyedSet<{ a: number }>()

  const key1 = { a: 1 }
  const key2 = { a: 2 }
  const key3 = { a: 1 }

  expect(set.size).toBe(0)

  expect(set.has(key1)).toBe(false)
  set.add(key1)
  expect(set.size).toBe(1)
  expect(set.has(key1)).toBe(true)

  set.add(key2)
  expect(set.size).toBe(2)
  expect(set.has(key2)).toBe(true)

  expect(set.has(key3)).toBe(true)
  set.add(key3)
  expect(set.size).toBe(2)

  const asArray = Array.from(set)
  expect(asArray).toEqual([key1, key2])

  set.delete(key1)
  expect(set.size).toBe(1)
  expect(set.has(key1)).toBe(false)
  expect(set.has(key2)).toBe(true)

  set.clear()
  expect(set.size).toBe(0)
  expect(set.has(key2)).toBe(false)
})
