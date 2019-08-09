package org.enso.interpreter;

class CurryingTest extends LanguageTest {

  "Functions" should "allow defaulted values to be removed" in {
    pending
    val code =
      """
        |addNum = { |a, num = 10| a + num }
        |
        |add = @addNum [num = !!]
        |
        |0
      """.stripMargin

    noException should be thrownBy parse(code)
    eval(code) shouldEqual 0
  }

}
