package parser

import org.scalatest.Inside.inside
import parser.SimpleParser._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.flatspec.AnyFlatSpec

import scala.None

class SimpleParserSpec extends AnyFlatSpec {
    // sucess test
    "A SimpleParser" should "parse valid json values" in {
        jsonNull.runParser("nullable123").contains(("able123", JsonNull()))
        jsonValue.runParser("nullable123") should contain(("able123", JsonNull()))

        jsonBool.runParser("true123") should contain(("123", JsonBool(true)))
        jsonBool.runParser("false123") should contain(("123", JsonBool(false)))
        jsonValue.runParser("true123") should contain(("123", JsonBool(true)))

        jsonNumber.runParser("12345a") should contain(("a", JsonNumber(12345)))
        jsonNumber.runParser("0a") should contain(("a", JsonNumber(0)))
        jsonValue.runParser("123asdf") should contain(("asdf", JsonNumber(123)))

        jsonArray.runParser("[]") should contain(("", JsonArray(List())))
        jsonArray.runParser("[902]") should contain(("", JsonArray(List(JsonNumber(902)))))
        jsonArray.runParser("[true,902,null]") should contain(("", JsonArray(List(JsonBool(true), JsonNumber(902), JsonNull()))))
        jsonValue.runParser("[null, false, 123, \"str\"]") should contain(("", JsonArray(List(JsonNull(), JsonBool(false), JsonNumber(123), JsonString("str")))))
    }

    "A SimpleParser.jsonObject" should "parse a valid json object" in {
        val json = jsonObject.runParser("""{
              "name" : "John Doe",
              "country" : "USA",
              "age" : 25,
              "verified" : true,
              "address" : null,
              "data" : ["pizza", 90, false, null]
              }""".replaceAll("\\s+","")
        )
        inside (json) {
            case Some(("", jsonValue)) =>
                jsonValue should be
                JsonObject(List(
                    ("name", JsonString("John Doe")),
                    ("country", JsonString("USA")),
                    ("age", JsonNumber(25)),
                    ("verified", JsonBool(true)),
                    ("address", JsonNull()),
                    ("data", JsonArray(List(JsonString("pizza"), JsonNumber(90), JsonBool(false), JsonNull())))
                ))
        }
    }

    "A SimpleParser.jsonObject" should "parse a valid nested json object" in {
        val json = jsonObject.runParser("""{
              "company" : "Alphabet",
              "age" : 23,
              "founders": ["Larry", "Sergey"],
              "subsidiaries" : [
                {
                    "company": "Youtube",
                    "age" : 17,
                    "founders": ["Steve", "Chad", "Jawed"]
                },
                {
                    "company": "DoubleClick",
                    "age" : 26
                },
                {
                    "company": "Nest",
                    "age" : 12
                }
              ]
             }""".replaceAll("\\s+","")
        )
        inside (json) {
            case Some(("", jsonValue)) =>
                jsonValue should be
                    JsonObject(List(
                        ("company", JsonString("Alphabet")),
                        ("age", JsonNumber(23)),
                        ("founders", JsonArray(List(
                            JsonString("Larry"), JsonString("Sergey")
                        ))),
                        ("subsidiaries", JsonArray(List(
                            JsonObject(List(
                                ("company", JsonString("Youtube")),
                                ("age", JsonNumber(17)),
                                ("founders", JsonArray(List(
                                    JsonString("Steve"), JsonString("Chad"), JsonString("Jawed")
                                )))
                            )),
                            JsonObject(List(
                                ("company", JsonString("DoubleClick")),
                                ("age", JsonNumber(26))
                            )),
                            JsonObject(List(
                                ("company", JsonString("Nest")),
                                ("age", JsonNumber(12))
                            ))
                        )))
                    ))
        }
    }

    // fail test
    "A SimpleParser.jsonObject" should "not parse a invalid json values" in {
        jsonObject.runParser("""{ city: Paris}""") shouldBe None
        jsonObject.runParser("""{"company"; "Googe"}""") shouldBe None
        jsonObject.runParser("""{ "city": "Paris", "data": []]}""") shouldBe None
    }
}
