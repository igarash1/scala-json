package parser

import org.scalatest.Inside.inside
import parser.SimpleParser._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.flatspec.AnyFlatSpec

import scala.None

class SimpleParserSpec extends AnyFlatSpec {
    // success test
    "A SimpleParser" should "parse valid json values" in {
        jsonNull.run("nullable123").contains(("able123", JsonNull()))
        jsonValue.run("nullable123") should contain(("able123", JsonNull()))

        jsonBool.run("true123") should contain(("123", JsonBool(true)))
        jsonBool.run("false123") should contain(("123", JsonBool(false)))
        jsonValue.run("true123") should contain(("123", JsonBool(true)))

        jsonNumber.run("12345a") should contain(("a", JsonNumber(12345)))
        jsonNumber.run("0a") should contain(("a", JsonNumber(0)))
        jsonNumber.run("0.013") should contain(("", JsonNumber(0.013)))
        jsonNumber.run("1e10") should contain(("", JsonNumber(1e10)))
        jsonValue.run("-.013") should contain(("", JsonNumber(-.013)))
        jsonValue.run("123asdf") should contain(("asdf", JsonNumber(123)))

        jsonArray.run("[]") should contain(("", JsonArray(List())))
        jsonArray.run("[-.0e4]") should contain(("", JsonArray(List(JsonNumber(-.0e4)))))
        jsonArray.run("[true,902,null]") should contain(("", JsonArray(List(JsonBool(true), JsonNumber(902), JsonNull()))))
        jsonValue.run("[null, false, -12.3, \"str\"]") should contain(("", JsonArray(List(JsonNull(), JsonBool(false), JsonNumber(-12.3), JsonString("str")))))
    }

    "A SimpleParser.jsonObject" should "parse a valid json object" in {
        val json = jsonObject.run("""{
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
        val json = jsonObject.run("""{
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
        jsonObject.run("""{ city: Paris}""") shouldBe None
        jsonObject.run("""{"company"; "Googe"}""") shouldBe None
        jsonObject.run("""{ "city": "Paris", "data": []]}""") shouldBe None
    }
}
