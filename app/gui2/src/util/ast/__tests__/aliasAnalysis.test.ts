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

import { assertDefined } from '@/util/assert'
import { AliasAnalyzer } from '@/util/ast/aliasAnalysis'
import { MappedKeyMap, MappedSet } from '@/util/containers'
import { IdMap, type ContentRange } from 'shared/yjsModel'
import { expect, test } from 'vitest'

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
  const annotations = new MappedKeyMap<ContentRange, Annotation>(IdMap.keyForRange)

  // Iterate over all annotations (either bindings or usages).
  // I.e. we want to cover both `«1,x»` and `»1,x«` cases, while keeping the track of the annotation type.
  const annotationRegex = /«(\d+),([^«»]+)»|»(\d+),([^«»]+)«/g

  // As the annotations are removed from the code, we need to keep track of the offset between the annotated and
  // unannotated code. This is necessary to correctly calculate the start and length of the annotations.
  let accumulatedOffset = 0

  const unannotatedCode = annotatedCode.replace(
    annotationRegex,
    (
      match,
      bindingPrefix: string | undefined,
      bindingName: string | undefined,
      usagePrefix: string | undefined,
      usageName: string | undefined,
      offset: number,
    ) => {
      console.log(`Processing annotated identifier ${match}.`)

      // Sanity check: either both binding prefix and name are present, or both usage prefix and name are present.
      // Otherwise, we have an internal error in the regex.
      expect(bindingPrefix != null).toBe(bindingName != null)
      expect(usagePrefix != null).toBe(usageName != null)
      expect(bindingPrefix != null).not.toBe(usagePrefix != null)

      const id = parseInt(bindingPrefix ?? usagePrefix ?? '0', 10)
      const name = bindingName ?? usageName ?? ''
      const kind = bindingPrefix != null ? AnnotationType.Binding : AnnotationType.Usage

      const start = offset - accumulatedOffset
      const end = start + name.length
      const range: ContentRange = [start, end]

      const annotation = new Annotation(kind, id)
      accumulatedOffset += match.length - name.length
      annotations.set(range, annotation)
      return name
    },
  )
  // Make sure that all annotations were removed. i.e. make sure that there are no unmatched `«` or `»` characters.
  expect(
    unannotatedCode.includes('«'),
    `Unmatched « character. Full code: ${unannotatedCode}`,
  ).toBe(false)
  expect(
    unannotatedCode.includes('»'),
    `Unmatched » character. Full code: ${unannotatedCode}`,
  ).toBe(false)
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
        expect(
          prefixBindings.has(annotation.id),
          `Duplicate binding with id ${annotation.id} at [${range}].`,
        ).toBe(false)
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
      assertDefined(foundTargets, `Expected binding ${this.prettyPrint(source)} not found.`)
      console.log(`Found expected binding ${this.prettyPrint(source)}`)

      for (const target of targets) {
        const foundConnection = foundTargets.has(target)
        // assert(foundConnection, `Expected connection [${source}] -> [${target}] not found.`)
        expect(foundConnection, `Expected connection [${source}] -> [${target}] not found.`).toBe(
          true,
        )
        console.log(`Found expected connection [${source}] -> [${target}]`)
      }
    }

    for (const unresolvedSymbol of this.expectedUnresolvedSymbols) {
      expect(
        analyzer.unresolvedSymbols.has(unresolvedSymbol),
        `Expected unresolved symbol usage ${this.prettyPrint(unresolvedSymbol)} not observed.`,
      ).toBe(true)
      console.log(
        `Found expected unresolved symbol usage at ${this.prettyPrint(unresolvedSymbol)}.`,
      )
    }

    expect(this.expectedUnresolvedSymbols.size, 'Unresolved symbols count mismatch.').toBe(
      analyzer.unresolvedSymbols.size,
    )
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
  expect(unannotatedCode).toBe(expectedUnannotatedCode)
  expect(annotations.size).toBe(7)
  const validateAnnotation = (
    range: ContentRange,
    kind: AnnotationType,
    prefix: number,
    identifier: string,
  ) => {
    try {
      const a = annotations.get(range)
      assertDefined(a, `No annotation found at [${range}].`)
      expect(a.kind, 'Invalid annotation kind.').toBe(kind)
      expect(a.id, 'Invalid annotation prefix.').toBe(prefix)
      expect(unannotatedCode.substring(range[0], range[1]), 'Invalid annotation identifier.').toBe(
        identifier,
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

function runTestCase(code: string) {
  return () => TestCase.parseAndRun(code)
}

test(
  'Plain dependency',
  runTestCase(`main =
    «1,x» = 1
    »1,x«`),
)

test(
  'Unresolved dependency',
  runTestCase(`main =
    «1,x» = 1
    »2,y«`),
)

test(
  'Local variable shadowed by the lambda argument',
  runTestCase(`main =
    «1,x» = 1
    «2,y» = 2
    z = «3,x» -> »3,x« + »2,y«
    u = »1,x« + »2,y«`),
)

test('Plain symbol usage', runTestCase('»1,x«'))

test(
  'Accessors',
  runTestCase(`main =
    «1,x» = 1
    »1,x«.x.y »2,arg«`),
)

test(
  'Function argument',
  runTestCase(`main =
    «1,func» «2,arg» = »2,arg« + 1
    »1,func« »3,arg«`),
)

test(
  'Multi-segment application',
  runTestCase(`main =
    «1,x» = True
    if »1,x« then »2,y« else »3,z«`),
)

test(
  'Function and case of',
  runTestCase(`## PRIVATE
   Given a positive index and a list, returns the node.
find_node_from_start «3,list» «4,index» =
    «1,loop» «2,current» «5,idx» = case »2,current« of
        Nil -> if »5,idx« == 0 then »2,current« else Error.throw (Index_Out_Of_Bounds.Error »4,index« »4,index«-»5,idx«+1)
        Cons _ «7,xs» -> if »5,idx« == 0 then »2,current« else @Tail_Call »1,loop« »7,xs« (»5,idx« - 1)
    »1,loop« »3,list« »4,index«
    »6,idx«`),
)

test(
  'Named argument application',
  runTestCase(`«1,main» =
    «2,hundred» = 100
    »3,summarize_transaction« (price = »2,hundred«)`),
)

test(
  'Default argument application',
  runTestCase(`«1,main» =
    »3,summarize_transaction« »2,default«`),
)

test(
  'Text literals',
  runTestCase(`«1,main» =
    «2,fmt_string» = 'Hello, my age is \`»3,time«.now.year - »4,birthday«\`'`),
)

test(
  'Before and after binding',
  runTestCase(`«1,main» =
    »2,x«
    «3,x» = 1
    »3,x«`),
)

test(
  'Identifier called default',
  runTestCase(`«1,main» =
    »2,default«
    «3,default» = 1
    »3,default«`),
)
