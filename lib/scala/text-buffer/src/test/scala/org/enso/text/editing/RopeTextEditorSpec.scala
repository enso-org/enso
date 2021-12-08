package org.enso.text.editing

import TestData._
import org.enso.text.buffer.Rope
import org.enso.text.editing.model.{Position, Range, TextEdit}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class RopeTextEditorSpec extends AnyFlatSpec with Matchers {

  "A rope text editor" should "insert text before beginning of a line" in {
    //given
    val beforeMain          = Range(Position(1, 0), Position(1, 0))
    val insertionBeforeMain = TextEdit(beforeMain, "ultra_")
    //when
    val result = RopeTextEditor.edit(testSnippet, insertionBeforeMain)
    //then
    result.toString mustBe """
                             |ultra_main =
                             |    apply = v f -> f v
                             |    adder = a b -> a + b
                             |    plusOne = apply (f = adder 1)
                             |    result = plusOne 10
                             |    result""".stripMargin
  }

  it should "replace a substring" in {
    //given
    val mainPosition    = Range(Position(1, 0), Position(1, 4))
    val mainReplacement = TextEdit(mainPosition, "run")
    //when
    val result = RopeTextEditor.edit(testSnippet, mainReplacement)
    //then
    result.toString mustBe """
                             |run =
                             |    apply = v f -> f v
                             |    adder = a b -> a + b
                             |    plusOne = apply (f = adder 1)
                             |    result = plusOne 10
                             |    result""".stripMargin
  }

  it should "replace a multiline substring" in {
    //given
    val resultPosition = Range(Position(5, 4), Position(6, 10))
    val change =
      """sum = plusOne 5
        |    sum""".stripMargin
    val resultReplacement = TextEdit(resultPosition, change)
    //when
    val result = RopeTextEditor.edit(testSnippet, resultReplacement)
    //then
    result.toString mustBe """
                             |main =
                             |    apply = v f -> f v
                             |    adder = a b -> a + b
                             |    plusOne = apply (f = adder 1)
                             |    sum = plusOne 5
                             |    sum""".stripMargin
  }

  it should "be able to insert change at the end of file" in {
    //given
    val eof          = Range(Position(6, 10), Position(6, 10))
    val insertedText = """
                         |    return result""".stripMargin
    val insertion    = TextEdit(eof, insertedText)
    //when
    val result = RopeTextEditor.edit(testSnippet, insertion)
    //then
    result.toString mustBe """
                             |main =
                             |    apply = v f -> f v
                             |    adder = a b -> a + b
                             |    plusOne = apply (f = adder 1)
                             |    result = plusOne 10
                             |    result
                             |    return result""".stripMargin
  }

  it should "support code points above 0xFFFF" in {
    //given
    //0x0001F4AF
    val utf32Text           = Rope("unicode: \ud83d\udcaf end")
    val positionInCodeUnits = Range(Position(0, 9), Position(0, 10))
    //0x0001F449
    val diff = TextEdit(positionInCodeUnits, "\ud83d\udc49")
    //when
    val result = RopeTextEditor.edit(utf32Text, diff)
    //then
    result.toString mustBe "unicode: \ud83d\udc49 end"
  }

  it should "append metadata to new file, not adding extra characters (#3094)" in {
    //given
    val buffer = Rope(
"""from Standard.Base import all

main =
    operator1 = 0.up_to 100 . to_vector . map .noise
    operator1.sort



#### METADATA ####
[[{"index":{"value":5},"size":{"value":8}},"ea731558-2d92-48e4-896f-02a49b337978"],[{"index":{"value":13},"size":{"value":1}},"c21bdcde-a83d-40e2-a135-85a975b4c3f1"],[{"index":{"value":14},"size":{"value":4}},"f4578770-d850-404c-8a91-141976955d4d"],[{"index":{"value":26},"size":{"value":3}},"e8d747ad-80e6-4da7-9629-2fca72a2b6dd"],[{"index":{"value":0},"size":{"value":29}},"5e70abd3-d7f8-41d9-9301-0feb624d8c17"],[{"index":{"value":31},"size":{"value":4}},"7967cd4b-f16d-4bde-9a42-d0535e2ba892"],[{"index":{"value":36},"size":{"value":1}},"c2e9eeca-ce26-43e5-b633-488026c7577c"],[{"index":{"value":42},"size":{"value":9}},"e1abf4c7-cf01-443e-9afc-c6f598a3ce2c"],[{"index":{"value":52},"size":{"value":1}},"434a0690-7dc8-4916-abc6-949dc6b94693"],[{"index":{"value":54},"size":{"value":1}},"de2e3c13-30d8-42a0-a47f-72760ba01769"],[{"index":{"value":55},"size":{"value":1}},"74b16b49-9f57-4ad6-ac7a-e6e5b53d7ee4"],[{"index":{"value":56},"size":{"value":5}},"dde505e0-e782-490d-9a2c-203204c724cf"],[{"index":{"value":54},"size":{"value":7}},"bcdad27f-99f5-4557-9bec-e2f64cab8331"],[{"index":{"value":62},"size":{"value":3}},"a76bbf95-6126-4e62-87db-c94f4899d03a"],[{"index":{"value":54},"size":{"value":11}},"ebb9453f-0fc9-49eb-af69-e5c3ce53b550"],[{"index":{"value":66},"size":{"value":1}},"f154b0fd-40e4-4240-9b30-3d26b37d57f8"],[{"index":{"value":68},"size":{"value":9}},"03b3c795-e93c-42df-ac94-46cdfc0330c9"],[{"index":{"value":54},"size":{"value":23}},"c400471a-d4dc-4d8a-97c2-2b19267ed3b0"],[{"index":{"value":78},"size":{"value":1}},"5ec8a317-9766-4a0d-bb4d-30f732bd39bb"],[{"index":{"value":80},"size":{"value":3}},"544cafac-ec7e-478b-9ffb-b73e9aed49d4"],[{"index":{"value":85},"size":{"value":5}},"ef957e91-90b7-4195-82be-ea0808aa7763"],[{"index":{"value":84},"size":{"value":6}},"8c55acdb-e147-41c0-9136-177490ba49a1"],[{"index":{"value":80},"size":{"value":10}},"5025740e-5941-4a31-a69d-d8aeeeea6acc"],[{"index":{"value":54},"size":{"value":36}},"f7f9a3e0-1fa9-4c22-a888-dada5f3039f5"],[{"index":{"value":42},"size":{"value":48}},"c7a12b6b-58a1-4724-be03-6a0378c4512e"],[{"index":{"value":95},"size":{"value":9}},"eb1f426d-afbd-434b-95de-1c542839296d"],[{"index":{"value":104},"size":{"value":1}},"1e3f5d37-763c-4bd1-87d8-98c05619bbed"],[{"index":{"value":105},"size":{"value":4}},"e2fcbf3b-9a5a-408e-838f-84991dded75a"],[{"index":{"value":95},"size":{"value":14}},"d34a9f68-a190-4efa-9d47-ed576d011a4a"],[{"index":{"value":37},"size":{"value":73}},"fa02c898-e116-467d-9f4d-693d9b784fac"],[{"index":{"value":31},"size":{"value":79}},"df2d39de-cf56-4c0a-897d-cd7d28718b9c"]]
{"ide":{"node":{},"project":null}}""")
    val editRange = Range(Position(10, 0), Position(10, 34))
    val editText  = """{"ide":{"node":{"f7f9a3e0-1fa9-4c22-a888-dada5f3039f5":{"position":{"vector":[-100.0,140.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null}},"project":null}}"""
    val edit      = TextEdit(editRange, editText)
    //when
    val result = RopeTextEditor.edit(buffer, edit)
    //then
    result.toString mustBe
"""from Standard.Base import all

main =
    operator1 = 0.up_to 100 . to_vector . map .noise
    operator1.sort



#### METADATA ####
[[{"index":{"value":5},"size":{"value":8}},"ea731558-2d92-48e4-896f-02a49b337978"],[{"index":{"value":13},"size":{"value":1}},"c21bdcde-a83d-40e2-a135-85a975b4c3f1"],[{"index":{"value":14},"size":{"value":4}},"f4578770-d850-404c-8a91-141976955d4d"],[{"index":{"value":26},"size":{"value":3}},"e8d747ad-80e6-4da7-9629-2fca72a2b6dd"],[{"index":{"value":0},"size":{"value":29}},"5e70abd3-d7f8-41d9-9301-0feb624d8c17"],[{"index":{"value":31},"size":{"value":4}},"7967cd4b-f16d-4bde-9a42-d0535e2ba892"],[{"index":{"value":36},"size":{"value":1}},"c2e9eeca-ce26-43e5-b633-488026c7577c"],[{"index":{"value":42},"size":{"value":9}},"e1abf4c7-cf01-443e-9afc-c6f598a3ce2c"],[{"index":{"value":52},"size":{"value":1}},"434a0690-7dc8-4916-abc6-949dc6b94693"],[{"index":{"value":54},"size":{"value":1}},"de2e3c13-30d8-42a0-a47f-72760ba01769"],[{"index":{"value":55},"size":{"value":1}},"74b16b49-9f57-4ad6-ac7a-e6e5b53d7ee4"],[{"index":{"value":56},"size":{"value":5}},"dde505e0-e782-490d-9a2c-203204c724cf"],[{"index":{"value":54},"size":{"value":7}},"bcdad27f-99f5-4557-9bec-e2f64cab8331"],[{"index":{"value":62},"size":{"value":3}},"a76bbf95-6126-4e62-87db-c94f4899d03a"],[{"index":{"value":54},"size":{"value":11}},"ebb9453f-0fc9-49eb-af69-e5c3ce53b550"],[{"index":{"value":66},"size":{"value":1}},"f154b0fd-40e4-4240-9b30-3d26b37d57f8"],[{"index":{"value":68},"size":{"value":9}},"03b3c795-e93c-42df-ac94-46cdfc0330c9"],[{"index":{"value":54},"size":{"value":23}},"c400471a-d4dc-4d8a-97c2-2b19267ed3b0"],[{"index":{"value":78},"size":{"value":1}},"5ec8a317-9766-4a0d-bb4d-30f732bd39bb"],[{"index":{"value":80},"size":{"value":3}},"544cafac-ec7e-478b-9ffb-b73e9aed49d4"],[{"index":{"value":85},"size":{"value":5}},"ef957e91-90b7-4195-82be-ea0808aa7763"],[{"index":{"value":84},"size":{"value":6}},"8c55acdb-e147-41c0-9136-177490ba49a1"],[{"index":{"value":80},"size":{"value":10}},"5025740e-5941-4a31-a69d-d8aeeeea6acc"],[{"index":{"value":54},"size":{"value":36}},"f7f9a3e0-1fa9-4c22-a888-dada5f3039f5"],[{"index":{"value":42},"size":{"value":48}},"c7a12b6b-58a1-4724-be03-6a0378c4512e"],[{"index":{"value":95},"size":{"value":9}},"eb1f426d-afbd-434b-95de-1c542839296d"],[{"index":{"value":104},"size":{"value":1}},"1e3f5d37-763c-4bd1-87d8-98c05619bbed"],[{"index":{"value":105},"size":{"value":4}},"e2fcbf3b-9a5a-408e-838f-84991dded75a"],[{"index":{"value":95},"size":{"value":14}},"d34a9f68-a190-4efa-9d47-ed576d011a4a"],[{"index":{"value":37},"size":{"value":73}},"fa02c898-e116-467d-9f4d-693d9b784fac"],[{"index":{"value":31},"size":{"value":79}},"df2d39de-cf56-4c0a-897d-cd7d28718b9c"]]
{"ide":{"node":{"f7f9a3e0-1fa9-4c22-a888-dada5f3039f5":{"position":{"vector":[-100.0,140.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null}},"project":null}}"""
  }

}
