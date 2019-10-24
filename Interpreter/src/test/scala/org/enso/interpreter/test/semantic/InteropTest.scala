package org.enso.interpreter.test.semantic

class InteropTest extends LanguageTest {
  "Interop library" should "support tail recursive functions" in {
    val code =
      """
        |@{
        |  recurFun = { |i| ifZero: [i, 0, @recurFun [i - 1]] };
        |  recurFun
        |}
        |""".stripMargin
    val recurFun = eval(code)
    recurFun.call(15) shouldEqual 0
  }

  "Interop library" should "support calling curried functions" in {
    val code =
      """
        |@{
        |  fun = { |x, y, z| (x + y) + z };
        |  @fun [y = 1]
        |}
        |""".stripMargin
    val curriedFun = eval(code)
    curriedFun.call(2, 3) shouldEqual 6
  }

  "Interop library" should "support creating curried calls" in {
    val code =
      """
        |{ |x, y, z| (x + y) + z }
        |""".stripMargin
    val fun = eval(code)
    fun.call(1).call(2).call(3) shouldEqual 6
  }

  "Interop library" should "work with oversaturated calls on unresolved methods returned from functions" in {
    val code =
      """
        |Any.method = this
        |
        |{ |x| method }
        |""".stripMargin
    val fun = eval(code)
    fun.call(1, 2) shouldEqual 2
  }
}
