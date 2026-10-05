package rosen.bridge

import io.circe.Json
import info.BuildInfo

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.security.MessageDigest

/** Add freshly compiled Avalanche configuration without replacing existing Rosen chains or assets. */
object AvalancheOverlayMerge {
  /** Require an object while retaining its unrelated fields. */
  private def fields(value: Json, label: String) =
    value.asObject.getOrElse(throw new IllegalArgumentException(s"$label must be an object"))

  /** Require an explicit configuration version without assigning a release number. */
  private def version(value: Json, label: String): Unit =
    require(value.hcursor.downField("version").as[String].toOption.exists(_.trim.nonEmpty), s"$label requires a version")

  /** Normalize EVM addresses and Ergo token bytes for cross-deployment collision checks. */
  private def identity(value: String): String =
    if (value.matches("0x[0-9a-fA-F]{40}") || value.matches("[0-9a-fA-F]{64}")) value.toLowerCase else value

  /** Read the flat token identities emitted by the existing contract generator. */
  private def contractTokenIds(value: Json, label: String): Vector[String] =
    fields(value, label).toVector.map { case (key, token) =>
      token.asString.filter(_.trim.nonEmpty).map(identity)
        .getOrElse(throw new IllegalArgumentException(s"$label.$key requires an identity"))
    }

  /** Merge only a newly prepared chain; existing configuration and release versions stay intact. */
  def merge(contractsBase: Json, tokensBase: Json, contractsOverlay: Json, tokensOverlay: Json): (Json, Json) = {
    val contracts = fields(contractsBase, "contracts base")
    val tokens = fields(tokensBase, "tokens base")
    version(contractsBase, "contracts base")
    version(tokensBase, "tokens base")
    require(!contracts.contains("avalanche"), "Contracts base already contains Avalanche")
    require(contracts("ergo").exists(_.isObject), "Contracts base requires its existing Ergo configuration")
    require(contracts("tokens") == contractsOverlay.hcursor.downField("tokens").focus, "Assigned global tokens differ from the existing Rosen deployment")
    require(tokens.keys.toSet == Set("version", "tokens"), "Tokens base requires version and tokens")
    val existing = tokens("tokens").flatMap(_.asArray).getOrElse(throw new IllegalArgumentException("Tokens base requires an array"))
    val added = tokensOverlay.hcursor.downField("tokens").as[Vector[Json]].toOption.getOrElse(throw new IllegalArgumentException("Prepared tokens require an array"))
    require(added.size == 2, "Preparation requires AVAX and JOE counterparts")
    val existingContractIds = contractTokenIds(contracts("tokens").get, "existing global tokens") ++
      contracts.toVector.filterNot { case (key, _) => key == "version" || key == "tokens" }.flatMap { case (key, profile) =>
        profile.asObject.flatMap(_("tokens")).toVector.flatMap(value => contractTokenIds(value, s"existing $key tokens"))
      }
    val existingIds = existing.flatMap { set =>
      val members = fields(set, "existing token set")
      require(!members.contains("avalanche"), "Tokens base already contains Avalanche")
      members.values.map { member =>
        member.hcursor.downField("tokenId").as[String].toOption.filter(_.nonEmpty)
          .map(identity).getOrElse(throw new IllegalArgumentException("Existing token member requires an identity"))
      }
    }
    val addedIds = added.flatMap(set => fields(set, "prepared token set").values.map { member =>
      identity(member.hcursor.downField("tokenId").as[String].toOption.getOrElse(throw new IllegalArgumentException("Prepared token requires an identity")))
    })
    val chain = contractsOverlay.hcursor.downField("avalanche").focus.getOrElse(throw new IllegalArgumentException("Prepared contracts require Avalanche"))
    val chainTokens = fields(chain, "prepared Avalanche contracts")("tokens")
      .getOrElse(throw new IllegalArgumentException("Prepared Avalanche contracts require tokens"))
    // Global deployment tokens are intentionally reused; only chain and asset IDs are newly allocated.
    val allocatedIds = contractTokenIds(chainTokens, "prepared Avalanche tokens") ++ addedIds
    require(allocatedIds.distinct.size == allocatedIds.size, "Prepared chain and asset identities collide")
    require(!allocatedIds.exists((existingContractIds ++ existingIds).contains), "Prepared identity collides with an existing Rosen contract or asset")
    val mergedContracts = Json.fromJsonObject(contracts.add("avalanche", chain))
    val mergedTokens = Json.fromJsonObject(tokens.add("tokens", Json.arr((existing ++ added): _*)))
    (mergedContracts, mergedTokens)
  }

  /** Pin exact source/configuration bytes for the generated deployment candidate. */
  private def digest(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString

  /** Collect compilation inputs without recording machine paths. */
  private def sourcePins(): Vector[Json] = {
    import scala.collection.JavaConverters._
    val stream = Files.walk(Paths.get("src/main/scala"))
    try {
      val source = stream.iterator().asScala.filter(path => Files.isRegularFile(path) && (path.toString.endsWith(".scala") || path.toString.endsWith(".es"))).toVector
      (source ++ Vector(Paths.get("build.sbt"), Paths.get("project/plugins.sbt"), Paths.get("project/build.properties")))
        .sortBy(_.toString).map(path => Json.obj("path" -> Json.fromString(path.toString.replace('\\', '/')), "sha256" -> Json.fromString(digest(Files.readAllBytes(path)))))
    } finally stream.close()
  }

  /** Regenerate from assigned values and write a fresh, disabled deployment candidate without network access. */
  def main(args: Array[String]): Unit = {
    require(args.length == 8 && args(0) == "--input" && args(2) == "--contracts-base" && args(4) == "--tokens-base" && args(6) == "--output",
      "Usage: --input <assigned-values.json> --contracts-base <contracts.json> --tokens-base <tokens.json> --output <new-directory>")
    val inputFiles = Vector("assigned-values" -> Paths.get(args(1)), "contracts-base" -> Paths.get(args(3)), "tokens-base" -> Paths.get(args(5)))
    val bytes = inputFiles.map { case (_, path) => Files.readAllBytes(path) }
    val inputs = bytes.map(value => AvalanchePreparation.parseInput(new String(value, StandardCharsets.UTF_8)))
    val sources = sourcePins()
    val prepared = AvalanchePreparation.prepare(inputs(0))
    val merged = merge(inputs(1), inputs(2), prepared._1, prepared._2)
    require(sourcePins() == sources, "Contract compilation sources changed during preparation")
    inputFiles.zip(bytes).foreach { case ((_, path), captured) =>
      require(java.util.Arrays.equals(captured, Files.readAllBytes(path)), "Deployment input changed during preparation")
    }
    val output = Paths.get(args(7))
    require(!Files.exists(output) && Files.isDirectory(output.toAbsolutePath.getParent), "Output must be a new directory under an existing parent")
    Files.createDirectory(output)
    val payloads = Vector("contracts.json" -> merged._1, "tokens.json" -> merged._2)
    val members = payloads.map { case (name, value) =>
      val payload = (value.spaces2 + "\n").getBytes(StandardCharsets.UTF_8)
      Files.write(output.resolve(name), payload)
      Json.obj("path" -> Json.fromString(name), "bytes" -> Json.fromInt(payload.length), "sha256" -> Json.fromString(digest(payload)))
    }
    val inputPins = inputFiles.zip(bytes).map { case ((role, _), captured) => Json.obj("role" -> Json.fromString(role), "sha256" -> Json.fromString(digest(captured))) }
    val receipt = Json.obj(
      "schema" -> Json.fromString("rosen-avalanche-deployment-candidate/v1"), "chainId" -> Json.fromInt(43114),
      "activated" -> Json.False, "minted" -> Json.False, "broadcast" -> Json.False,
      "generatorVersion" -> Json.fromString(BuildInfo.version), "javaVersion" -> Json.fromString(System.getProperty("java.version")),
      "scalaVersion" -> Json.fromString(scala.util.Properties.versionNumberString),
      "inputs" -> Json.arr(inputPins: _*), "sources" -> Json.arr(sources: _*), "files" -> Json.arr(members: _*)
    )
    Files.write(output.resolve("manifest.json"), (receipt.spaces2 + "\n").getBytes(StandardCharsets.UTF_8))
    println("Avalanche deployment configuration candidate prepared: contracts.json, tokens.json")
  }
}
