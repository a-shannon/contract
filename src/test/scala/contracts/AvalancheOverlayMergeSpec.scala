package contracts

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import rosen.bridge.{AvalancheOverlayMerge, AvalanchePreparation}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.UUID

/** Preserve the real configuration consumer shape and isolate every merge refusal. */
class AvalancheOverlayMergeSpec extends AnyFunSuite {
  private val example = Paths.get("src/test/resources/avalanche/assigned-values.example.json")
  private val input = AvalanchePreparation.parseInput(new String(Files.readAllBytes(example), StandardCharsets.UTF_8))
  private lazy val overlays = AvalanchePreparation.prepare(input)
  private def contracts = Json.obj(
    "version" -> Json.fromString("existing-contract-release"), "tokens" -> input.hcursor.downField("globalTokens").focus.get,
    "ergo" -> Json.obj("addresses" -> Json.obj("lock" -> Json.fromString("existing-ergo-lock"))),
    "ethereum" -> Json.obj("addresses" -> Json.obj("lock" -> Json.fromString("existing-ethereum-lock")))
  )
  private def member(id: String) = Json.obj("tokenId" -> Json.fromString(id), "type" -> Json.fromString("EIP-004"), "name" -> Json.fromString("existing"), "decimals" -> Json.fromInt(9), "residency" -> Json.fromString("native"), "extra" -> Json.obj())
  private def tokens = Json.obj("version" -> Json.fromString("existing-token-release"), "tokens" -> Json.arr(Json.obj("ergo" -> member("77" * 32))))
  private def replaceTokenSet(set: Json) = tokens.mapObject(_.add("tokens", Json.arr(set)))
  private def withContractTokens(chain: String, value: Json) = contracts.mapObject(_.add(chain,
    contracts.hcursor.downField(chain).focus.get.mapObject(_.add("tokens", value))))

  test("merge appends Avalanche and both counterparts while preserving all existing fields, assets and versions") {
    val merged = AvalancheOverlayMerge.merge(contracts, tokens, overlays._1, overlays._2)
    assert(merged._1.mapObject(_.remove("avalanche")) == contracts)
    assert(merged._1.hcursor.downField("avalanche").focus == overlays._1.hcursor.downField("avalanche").focus)
    val mergedSets = merged._2.hcursor.downField("tokens").as[Vector[Json]].right.get
    assert(mergedSets.take(1) == tokens.hcursor.downField("tokens").as[Vector[Json]].right.get)
    assert(mergedSets.drop(1) == overlays._2.hcursor.downField("tokens").as[Vector[Json]].right.get)
    assert(merged._2.hcursor.downField("version").focus == tokens.hcursor.downField("version").focus)
  }

  input.hcursor.downField("chainTokens").focus.get.asObject.get.toVector.foreach { case (key, id) =>
    test(s"merge refuses new $key colliding with an existing chain contract token") {
      val base = withContractTokens("ethereum", Json.obj("CleanupNFT" -> id))
      intercept[IllegalArgumentException](AvalancheOverlayMerge.merge(base, tokens, overlays._1, overlays._2))
    }
    test(s"merge refuses new $key colliding with an existing Ergo asset counterpart") {
      val base = replaceTokenSet(Json.obj("ergo" -> member(id.asString.get)))
      intercept[IllegalArgumentException](AvalancheOverlayMerge.merge(contracts, base, overlays._1, overlays._2))
    }
  }

  Seq("avax", "joe").foreach { asset =>
    test(s"merge refuses new $asset counterpart colliding with an existing chain contract token") {
      val id = input.hcursor.downField("counterparts").downField(asset).downField("tokenId").focus.get
      val base = withContractTokens("ethereum", Json.obj("CleanupNFT" -> id))
      intercept[IllegalArgumentException](AvalancheOverlayMerge.merge(base, tokens, overlays._1, overlays._2))
    }
  }

  test("merge preserves existing chain tokens, shared global identities and unrelated nested fields") {
    val shared = input.hcursor.downField("globalTokens").downField("RSN").focus.get
    val base = withContractTokens("ergo", Json.obj("ExistingNFT" -> Json.fromString("66" * 32), "SharedRSN" -> shared))
      .mapObject(_.add("metadata", Json.obj("tokensElsewhere" -> Json.obj("unchanged" -> shared))))
    val baseTokens = replaceTokenSet(Json.obj("ergo" -> member(shared.asString.get)))
    val merged = AvalancheOverlayMerge.merge(base, baseTokens, overlays._1, overlays._2)
    assert(merged._1.mapObject(_.remove("avalanche")) == base)
    assert(merged._2.hcursor.downField("tokens").as[Vector[Json]].right.get.head == baseTokens.hcursor.downField("tokens").as[Vector[Json]].right.get.head)
  }

  val existingContractTokenFaults = Seq(
    "nonobject token map" -> Json.arr(),
    "nested token identity" -> Json.obj("CleanupNFT" -> Json.obj("tokenId" -> Json.fromString("66" * 32))),
    "array token identity" -> Json.obj("CleanupNFT" -> Json.arr(Json.fromString("66" * 32))),
    "numeric token identity" -> Json.obj("CleanupNFT" -> Json.fromInt(1)),
    "null token identity" -> Json.obj("CleanupNFT" -> Json.Null),
    "empty token identity" -> Json.obj("CleanupNFT" -> Json.fromString("")),
    "blank token identity" -> Json.obj("CleanupNFT" -> Json.fromString(" "))
  )
  existingContractTokenFaults.foreach { case (label, faulty) =>
    test(s"merge refuses existing chain $label") {
      intercept[IllegalArgumentException](AvalancheOverlayMerge.merge(withContractTokens("ethereum", faulty), tokens, overlays._1, overlays._2))
    }
  }

  test("merge refuses a case alias of JOE in existing contract tokens") {
    val base = withContractTokens("ethereum", Json.obj("ExistingToken" -> Json.fromString("0x" + AvalanchePreparation.JoeAddress.drop(2).toUpperCase)))
    intercept[IllegalArgumentException](AvalancheOverlayMerge.merge(base, tokens, overlays._1, overlays._2))
  }

  test("merge refuses an uppercase Ergo alias of a newly allocated chain NFT") {
    val id = input.hcursor.downField("chainTokens").downField("RepoConfigNFT").as[String].right.get
    val base = withContractTokens("ethereum", Json.obj("CleanupNFT" -> Json.fromString(id.toUpperCase)))
    intercept[IllegalArgumentException](AvalancheOverlayMerge.merge(base, tokens, overlays._1, overlays._2))
  }

  test("merge refuses an uppercase Ergo alias of a newly allocated asset counterpart") {
    val id = input.hcursor.downField("counterparts").downField("joe").downField("tokenId").as[String].right.get
    val base = replaceTokenSet(Json.obj("ergo" -> member(id.toUpperCase)))
    intercept[IllegalArgumentException](AvalancheOverlayMerge.merge(contracts, base, overlays._1, overlays._2))
  }

  val contractFaults = Seq(
    "nonobject base" -> Json.arr(),
    "missing version" -> contracts.mapObject(_.remove("version")),
    "empty version" -> contracts.mapObject(_.add("version", Json.fromString(" "))),
    "existing Avalanche" -> contracts.mapObject(_.add("avalanche", Json.obj())),
    "missing Ergo" -> contracts.mapObject(_.remove("ergo")),
    "different global deployment tokens" -> contracts.mapObject(_.add("tokens", Json.obj()))
  )
  contractFaults.foreach { case (label, faulty) =>
    test(s"merge refuses $label") {
      intercept[IllegalArgumentException](AvalancheOverlayMerge.merge(faulty, tokens, overlays._1, overlays._2))
    }
  }
  val tokenFaults = Seq(
    "nonobject token base" -> Json.arr(),
    "missing token version" -> tokens.mapObject(_.remove("version")),
    "extra token root field" -> tokens.mapObject(_.add("network", Json.fromString("other"))),
    "nonarray tokens" -> tokens.mapObject(_.add("tokens", Json.obj())),
    "nonobject token set" -> tokens.mapObject(_.add("tokens", Json.arr(Json.fromString("other")))),
    "member without identity" -> replaceTokenSet(Json.obj("ergo" -> Json.obj())),
    "empty member identity" -> replaceTokenSet(Json.obj("ergo" -> member(""))),
    "existing Avalanche asset" -> replaceTokenSet(Json.obj("avalanche" -> member("other"))),
    "existing AVAX identity" -> replaceTokenSet(Json.obj("ergo" -> member("avax"))),
    "existing counterpart identity" -> replaceTokenSet(Json.obj("ergo" -> member("0b" * 32))),
    "case alias of existing JOE address" -> replaceTokenSet(Json.obj("ethereum" -> member("0x" + AvalanchePreparation.JoeAddress.drop(2).toUpperCase)))
  )
  tokenFaults.foreach { case (label, faulty) =>
    test(s"merge refuses $label") {
      intercept[IllegalArgumentException](AvalancheOverlayMerge.merge(contracts, faulty, overlays._1, overlays._2))
    }
  }

  test("CLI regenerates the overlays, seals merged files and leaves all three input files untouched") {
    val directory = Paths.get("target", "avalanche-merge-tests", UUID.randomUUID().toString)
    Files.createDirectories(directory)
    val baseContracts = directory.resolve("base-contracts.json")
    val baseTokens = directory.resolve("base-tokens.json")
    val contractsBytes = contracts.noSpaces.getBytes(StandardCharsets.UTF_8)
    val tokensBytes = tokens.noSpaces.getBytes(StandardCharsets.UTF_8)
    val assignedBytes = Files.readAllBytes(example)
    Files.write(baseContracts, contractsBytes)
    Files.write(baseTokens, tokensBytes)
    val output = directory.resolve("output")
    AvalancheOverlayMerge.main(Array("--input", example.toString, "--contracts-base", baseContracts.toString, "--tokens-base", baseTokens.toString, "--output", output.toString))
    val resultContracts = AvalanchePreparation.parseInput(new String(Files.readAllBytes(output.resolve("contracts.json")), StandardCharsets.UTF_8))
    val resultTokens = AvalanchePreparation.parseInput(new String(Files.readAllBytes(output.resolve("tokens.json")), StandardCharsets.UTF_8))
    val receipt = AvalanchePreparation.parseInput(new String(Files.readAllBytes(output.resolve("manifest.json")), StandardCharsets.UTF_8))
    assert((resultContracts, resultTokens) == AvalancheOverlayMerge.merge(contracts, tokens, overlays._1, overlays._2))
    assert(receipt.hcursor.downField("chainId").as[Int].right.get == 43114)
    assert(receipt.hcursor.downField("activated").as[Boolean].right.get == false)
    assert(receipt.hcursor.downField("inputs").as[Vector[Json]].right.get.size == 3)
    assert(receipt.hcursor.downField("files").as[Vector[Json]].right.get.size == 2)
    assert(java.util.Arrays.equals(Files.readAllBytes(example), assignedBytes))
    assert(java.util.Arrays.equals(Files.readAllBytes(baseContracts), contractsBytes))
    assert(java.util.Arrays.equals(Files.readAllBytes(baseTokens), tokensBytes))
    intercept[IllegalArgumentException](AvalancheOverlayMerge.main(Array("--input", example.toString, "--contracts-base", baseContracts.toString, "--tokens-base", baseTokens.toString, "--output", output.toString)))
    assert(Files.readAllBytes(output.resolve("contracts.json")).sameElements((resultContracts.spaces2 + "\n").getBytes(StandardCharsets.UTF_8)))
  }
}
