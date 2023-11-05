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

import type { ContentRange } from '@/../../../../shared/yjsModel'
import {assert, assertDefined, assertEmpty, assertEqual, assertLength, assertNotEqual} from '@/util/assert'
import { MappedKeyMap, MappedSet } from '@/util/containers'
import { expect, test } from 'vitest'
import { IdMap } from '../../../../shared/yjsModel'
import { AliasAnalyzer } from '../aliasAnalysis'

/** The type of annotation. */
enum AnnotationType {
  /** An identifier binding (introducing variable). */
  Binding = 'Binding',
  /** An identifier usage. */
  Usage = 'Usage',
}

/** Information about an annotated identifier. */
class Annotation {
  /**
   * @param kind Whether this is an identifier binding or usage.
   * @param id The special user-defined id to disambiguate identifiers with the same name.
   */
  constructor(
    public kind: AnnotationType,
    public id: number,
  ) {}
}

/** Parse annotations from the annotated code. See the file-top comment for the syntax. */
function parseAnnotations(annotatedCode: string): {
  unannotatedCode: string
  annotations: MappedKeyMap<ContentRange, Annotation>
} {
  const annotations = new MappedKeyMap(IdMap.keyForRange)

  // Iterate over all annotations (either bindings or usages).
  // I.e. we want to cover both `«1,x»` and `»1,x«` cases, while keeping the track of the annotation type.
  const annotationRegex = /«(\d+),([^«»]+)»|»(\d+),([^«»]+)«/g

  // As the annotations are removed from the code, we need to keep track of the offset between the annotated and
  // unannotated code. This is necessary to correctly calculate the start and length of the annotations.
  let accumulatedOffset = 0

  const unannotatedCode = annotatedCode.replace(
    annotationRegex,
    (match, bindingPrefix, bindingName, usagePrefix, usageName, offset) => {
      console.log(`Processing annotated identifier ${match}.`)

      // Sanity check: either both binding prefix and name are present, or both usage prefix and name are present.
      // Otherwise, we have an internal error in the regex.

      expect(bindingPrefix != null).toBe(bindingName != null)

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
  // Make sure that all annotations were removed. i.e. make sure that there are no unmatched `«` or `»` characters.
  assert(!unannotatedCode.includes('«'), `Unmatched « character. Full code: ${unannotatedCode}`)
  assert(!unannotatedCode.includes('»'), `Unmatched » character. Full code: ${unannotatedCode}`)
  return { unannotatedCode, annotations }
}

/** Alias analysis test case, typically parsed from an annotated code. */
class TestCase {
  /** The expected aliases. */
  readonly expectedAliases = new MappedKeyMap<ContentRange, ContentRange[]>(IdMap.keyForRange)

  /** The expected unresolved symbols. */
  readonly expectedUnresolvedSymbols = new MappedSet<ContentRange>(IdMap.keyForRange)

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
        assert(
          !prefixBindings.has(annotation.id),
          `Duplicate binding with id ${annotation.id} at [${range}].`,
        )
        prefixBindings.set(annotation.id, range)
        console.debug(`Binding ${annotation.id}@[${range}]`)
        testCase.expectedAliases.set(range, [])
      }
    }
    for (const [range, annotation] of annotations) {
      if (annotation.kind === AnnotationType.Usage) {
        const bindingRange = prefixBindings.get(annotation.id)
        if (bindingRange == null) {
          testCase.expectedUnresolvedSymbols.add(range)
        } else {
          const usages = testCase.expectedAliases.get(bindingRange)
          assertDefined(usages, `No usages list found for binding with id ${annotation.id}.`)
          usages.push(range)
        }
      }
    }

    return testCase
  }

  repr(range: ContentRange): string {
    return this.code.substring(range[0], range[1])
  }

  prettyPrint(range: ContentRange): string {
    return `${this.repr(range)}@[${range}]`
  }

  run(): AliasAnalyzer {
    const analyzer = new AliasAnalyzer(this.code)
    analyzer.process()

    // Check that each expected connection is present.
    for (const [source, targets] of this.expectedAliases) {
      const foundTargets = analyzer.aliases.get(source)
      assertDefined(
        foundTargets,
        `Expected binding ${this.prettyPrint(source)} not found.`,
      )
      console.log(`Found expected binding ${this.prettyPrint(source)}`)

      for (const target of targets) {
        const foundConnection = foundTargets.has(target)
        assert(foundConnection, `Expected connection [${source}] -> [${target}] not found.`)
        console.log(`Found expected connection [${source}] -> [${target}]`)
      }
    }

    for (const unresolvedSymbol of this.expectedUnresolvedSymbols) {
      assert(
        analyzer.unresolvedSymbols.has(unresolvedSymbol),
        `Expected unresolved symbol usage ${this.prettyPrint(unresolvedSymbol)} not observed.`,
      )
      console.log(`Found expected unresolved symbol usage at ${this.prettyPrint(unresolvedSymbol)}.`)
    }
    return analyzer
  }

  static parseAndRun(annotatedCode: string) {
    const testCase = TestCase.parse(annotatedCode)
    return testCase.run()
  }
}

test('Annotations parsing', () => {
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

test('Plain dependency', () => {
  const analyzer = TestCase.parseAndRun('main =\n    «1,x» = 1\n    »1,x«')
  assertEmpty(analyzer.unresolvedSymbols)

})

test('Unresolved dependency', () => {
  const analyzer = TestCase.parseAndRun('main =\n    «1,x» = 1\n    »2,y«')
  assertLength(analyzer.unresolvedSymbols, 1) // `y` is not in scope.
})

test('Local variable shadowed by the lambda argument', () => {
  const analyzer =  TestCase.parseAndRun(`main =
    «1,x» = 1
    «2,y» = 2
    z = «3,x» -> »3,x« + »2,y«
    u = »1,x« + »2,y«`)
  assertEmpty(analyzer.unresolvedSymbols)
})

test('Plain symbol usage', () => {
  const analyzer = TestCase.parseAndRun('»1,x«')
  assertLength(analyzer.unresolvedSymbols, 1) // `x` is not in scope.
})

test('Accessors', () => {
  const analyzer = TestCase.parseAndRun(`main =
    «1,x» = 1
    »1,x«.x.y »2,arg«`)
  assertLength(analyzer.unresolvedSymbols, 1) // `arg` is not in scope.
})

test('Function argument', () => {
  const analyzer = TestCase.parseAndRun(`main =
    «1,func» «2,arg» = »2,arg« + 1
    »1,func« »3,arg«`)
  assertLength(analyzer.unresolvedSymbols, 1) // `arg` is not in scope.
})

test('Multi-segment application', () => {
  const analyzer = TestCase.parseAndRun(`main =
    «1,x» = True
    if »1,x« then »2,y« else »3,z«`)
})

test('Complex?', () => {
  const code = `## PRIVATE
   Given a positive index and a list, returns the node.
find_node_from_start «3,list» «4,index» =
    «1,loop» «2,current» «5,idx» = case »2,current« of
        Nil -> if »5,idx« == 0 then »2,current« else Error.throw (Index_Out_Of_Bounds.Error »4,index« »4,index«-»5,idx«+1)
        Cons _ «7,xs» -> if »5,idx« == 0 then »2,current« else @Tail_Call »1,loop« »7,xs« (»5,idx« - 1)
    »1,loop« »3,list« »4,index«
    »6,idx«`

  const analyzer = TestCase.parseAndRun(code)
  assertLength(analyzer.unresolvedSymbols, 1) // The last line's `idx` - as it is now out of scope.
})

test('Named argument application', () => {
  const code = `«1,main» =
    «2,hundred» = 100
    »3,summarize_transaction« (price = »2,hundred«)`
  const analyzer = TestCase.parseAndRun(code)
  assertLength(analyzer.unresolvedSymbols, 1) // `summarize_transaction`
  // Note: the `price` argument is not a variable usage and should be ignored by the alias analysis.
})

test('Default argument application', () => {
  const code = `«1,main» =
    »3,summarize_transaction« default`
  const analyzer = TestCase.parseAndRun(code)
  assertLength(analyzer.unresolvedSymbols, 1) // `summarize_transaction`
  // Note: the `default` keyword is not a variable usage and should be ignored by the alias analysis.
})

test('Text literals', () => {
  const code = `«1,main» =
    «2,fmt_string» = 'Hello, my age is \`»3,time«.now.year - »4,birthday«\`'`
  const analyzer = TestCase.parseAndRun(code)
  assertLength(analyzer.unresolvedSymbols, 2) // `time` and `birthday`
})
