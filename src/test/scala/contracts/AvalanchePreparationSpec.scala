package contracts

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import rosen.bridge.AvalanchePreparation

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.UUID

/** Offline compilation and isolated input refusals for the contributor contract overlay. */
class AvalanchePreparationSpec extends AnyFunSuite {
  /** Use distinct synthetic identities; none denotes a minted deployment token. */
  private def id(value: Int): Json = Json.fromString(f"$value%02x" * 32)

  /** Supply a complete mainnet fixture without obtaining keys or node data. */
  private def fixture: Json = Json.obj(
    "schema" -> Json.fromString("rosen-avalanche-contract-preparation/v1"),
    "chainId" -> Json.fromInt(43114), "cleanupConfirm" -> Json.fromInt(21600),
    "globalTokens" -> Json.obj("RWTRepoNFT" -> id(1), "GuardNFT" -> id(2), "RSN" -> id(3), "MinFeeNFT" -> id(4), "EmissionNFT" -> id(5), "ERSN" -> id(6)),
    "chainTokens" -> Json.obj("RWTId" -> id(7), "CleanupNFT" -> id(8), "AwcNFT" -> id(9), "RepoConfigNFT" -> id(10)),
    "custody" -> Json.obj("lock" -> Json.fromString("0x" + "11" * 20), "cold" -> Json.fromString("0x" + "22" * 20)),
    "counterparts" -> Json.obj(
      "avax" -> Json.obj("tokenId" -> id(11), "decimals" -> Json.fromInt(9)),
      "joe" -> Json.obj("tokenId" -> id(12), "decimals" -> Json.fromInt(9))
    )
  )

  /** Replace one field while retaining every other valid input. */
  private def replace(input: Json, path: List[String], value: Json): Json = path match {
    case head :: Nil => input.mapObject(_.add(head, value))
    case head :: rest => input.mapObject(fields => fields.add(head, replace(fields(head).get, rest, value)))
    case Nil => value
  }

  test("offline mainnet preparation compiles all upstream contract addresses and AVAX/JOE counterpart overlays") {
    val (contracts, tokens) = AvalanchePreparation.prepare(fixture)
    val chain = contracts.hcursor.downField("avalanche")
    val addresses = chain.downField("addresses").focus.get.asObject.get
    assert(addresses.size == 11)
    assert(addresses("lock").get.asString.contains("0x" + "11" * 20))
    assert(addresses("cold").get.asString.contains("0x" + "22" * 20))
    assert(addresses.values.forall(_.asString.exists(_.nonEmpty)))
    assert(chain.downField("tokens").downField("RWTId").as[String].right.get == ("07" * 32))
    val sets = tokens.hcursor.downField("tokens").as[Vector[Json]].right.get
    assert(sets.size == 2)
    assert(sets(0).hcursor.downField("avalanche").downField("type").as[String].right.get == "native")
    assert(sets(1).hcursor.downField("avalanche").downField("tokenId").as[String].right.get == AvalanchePreparation.JoeAddress)
    assert(sets(1).hcursor.downField("avalanche").downField("type").as[String].right.get == "ERC-20")
    assert(sets.forall(_.hcursor.downField("ergo").downField("residency").as[String].right.get == "wrapped"))
    assert(AvalanchePreparation.prepare(fixture) == ((contracts, tokens)))
  }

  test("changing GuardNFT changes bound contract addresses without changing TSS custody or source asset") {
    val initial = AvalanchePreparation.prepare(fixture)
    val changed = AvalanchePreparation.prepare(replace(fixture, List("globalTokens", "GuardNFT"), id(13)))
    val before = initial._1.hcursor.downField("avalanche").downField("addresses")
    val after = changed._1.hcursor.downField("avalanche").downField("addresses")
    assert(before.downField("RepoConfig").focus != after.downField("RepoConfig").focus)
    assert(before.downField("Emission").focus != after.downField("Emission").focus)
    assert(before.downField("lock").focus == after.downField("lock").focus)
    assert(initial._2 == changed._2)
  }

  val faults = Seq(
    ("wrong network", List("chainId"), Json.fromInt(43113)),
    ("unassigned chain NFT", List("chainTokens", "AwcNFT"), Json.Null),
    ("malformed chain NFT", List("chainTokens", "AwcNFT"), Json.fromString("ff" * 31)),
    ("zero global identity", List("globalTokens", "GuardNFT"), Json.fromString("00" * 32)),
    ("colliding global identity", List("globalTokens", "RSN"), id(2)),
    ("zero custody", List("custody", "lock"), Json.fromString("0x" + "00" * 20)),
    ("same lock and cold", List("custody", "cold"), Json.fromString("0x" + "11" * 20)),
    ("unassigned counterpart", List("counterparts", "joe", "tokenId"), Json.Null),
    ("counterpart collision", List("counterparts", "joe", "tokenId"), id(11)),
    ("invalid counterpart decimals", List("counterparts", "joe", "decimals"), Json.fromInt(19)),
    ("fractional cleanup", List("cleanupConfirm"), Json.fromDoubleOrNull(1.5)),
    ("zero cleanup", List("cleanupConfirm"), Json.fromInt(0)),
    ("unknown schema", List("schema"), Json.fromString("another-schema"))
  )
  faults.foreach { case (label, path, value) =>
    test(s"preparation refuses $label before contract compilation") {
      intercept[IllegalArgumentException](AvalanchePreparation.prepare(replace(fixture, path, value)))
    }
  }

  test("preparation refuses an unexpected field instead of silently accepting a different deployment input") {
    intercept[IllegalArgumentException](AvalanchePreparation.prepare(fixture.mapObject(_.add("network", Json.fromString("fuji")))))
  }

  test("strict parsing preserves distinct objects that use the same field name") {
    assert(AvalanchePreparation.parseInput(fixture.noSpaces) == fixture)
  }

  val duplicateFaults = Seq(
    ("root network", "\"chainId\":43114", "\"chainId\":43113,\"chainId\":43114"),
    ("nested custody", "\"lock\":\"0x" + "11" * 20 + "\"", "\"lock\":\"0x" + "33" * 20 + "\",\"lock\":\"0x" + "11" * 20 + "\""),
    ("nested counterpart", "\"tokenId\":\"" + "0c" * 32 + "\"", "\"tokenId\":\"" + "0d" * 32 + "\",\"tokenId\":\"" + "0c" * 32 + "\""),
    ("escaped decoded name", "\"chainId\":43114", "\"chainId\":43113,\"chain" + "\\" + "u0049d\":43114"),
    ("null followed by assigned value", "\"chainId\":43114", "\"chainId\":null,\"chainId\":43114")
  )
  duplicateFaults.foreach { case (label, original, duplicate) =>
    test(s"CLI refuses duplicate $label without creating deployment outputs") {
      val text = fixture.noSpaces.replace(original, duplicate)
      assert(text != fixture.noSpaces)
      val directory = Paths.get("target", "avalanche-input-tests", UUID.randomUUID().toString)
      Files.createDirectories(directory)
      val input = directory.resolve("input.json")
      val output = directory.resolve("output")
      Files.write(input, text.getBytes(StandardCharsets.UTF_8))
      val error = intercept[io.circe.ParsingFailure](AvalanchePreparation.main(Array("--input", input.toString, "--output", output.toString)))
      assert(error.getMessage.contains("duplicate key name"))
      assert(!Files.exists(output))
    }
  }
}
