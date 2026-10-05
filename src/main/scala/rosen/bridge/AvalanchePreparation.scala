package rosen.bridge

import helpers.{ErgoNetwork, MainTokens, Network, NetworkGeneral, Tokens}
import info.BuildInfo
import io.circe.Json
import io.circe.jawn.JawnParser
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit.{ColdErgoClient, NetworkType}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.security.MessageDigest

/** Compile the existing Rosen scripts offline and prepare AVAX/JOE token-map overlays. */
object AvalanchePreparation {
  val JoeAddress = "0x6e84a6216ea6dacc71ee8e6b0a5b7322eebc0fdd"
  private val globalKeys = Set("RWTRepoNFT", "GuardNFT", "RSN", "MinFeeNFT", "EmissionNFT", "ERSN")
  private val chainKeys = Set("RWTId", "CleanupNFT", "AwcNFT", "RepoConfigNFT")

  /** Reject duplicate decoded field names at every object depth before assigning deployment values. */
  def parseInput(text: String): Json = JawnParser(false).parse(text).fold(error => throw error, identity)

  /** Reject missing and unexpected fields before compiling any script or writing outputs. */
  private def objectFields(json: Json, expected: Set[String], label: String): Map[String, Json] = {
    val fields = json.asObject.getOrElse(throw new IllegalArgumentException(s"$label must be an object"))
    require(fields.keys.toSet == expected, s"$label requires exactly ${expected.toSeq.sorted.mkString(", ")}")
    fields.toMap
  }

  /** Require an assigned nonzero canonical Ergo token identity. */
  private def tokenId(json: Json, label: String): String = {
    val value = json.asString.getOrElse(throw new IllegalArgumentException(s"$label requires an assigned token ID"))
    require(value.matches("[0-9a-f]{64}") && value.exists(_ != '0'), s"Invalid $label token ID")
    value
  }

  /** Require a canonical nonzero EVM custody address without network inference. */
  private def custody(json: Json, label: String): String = {
    val value = json.asString.getOrElse(throw new IllegalArgumentException(s"$label requires an assigned address"))
    require(value.matches("0x[0-9a-f]{40}") && value.drop(2).exists(_ != '0'), s"Invalid $label address")
    value
  }

  /** Read a bounded integer without accepting numeric strings, fractions or overflow. */
  private def integer(json: Json, minimum: Int, maximum: Int, label: String): Int = {
    val value = json.asNumber.flatMap(_.toInt).getOrElse(throw new IllegalArgumentException(s"Invalid $label integer"))
    require(value >= minimum && value <= maximum, s"$label must be in [$minimum, $maximum]")
    value
  }

  /** Preserve Rosen's source token types and require every bridge asset's Ergo counterpart. */
  private def tokenSet(id: String, name: String, native: Boolean, counterpart: (String, Int)): Json = {
    val source = Json.obj(
      "tokenId" -> Json.fromString(id), "name" -> Json.fromString(name),
      "decimals" -> Json.fromInt(18), "type" -> Json.fromString(if (native) "native" else "ERC-20"),
      "residency" -> Json.fromString("native"), "extra" -> Json.obj()
    )
    val ergo = Json.obj(
      "tokenId" -> Json.fromString(counterpart._1), "name" -> Json.fromString(s"rs$name"),
      "decimals" -> Json.fromInt(counterpart._2), "type" -> Json.fromString("EIP-004"),
      "residency" -> Json.fromString("wrapped"), "extra" -> Json.obj()
    )
    Json.obj("avalanche" -> source, "ergo" -> ergo)
  }

  /** Bind supplied identities to the upstream contract generator using a network-free context. */
  def prepare(input: Json): (Json, Json) = {
    val root = objectFields(input, Set("schema", "chainId", "cleanupConfirm", "globalTokens", "chainTokens", "custody", "counterparts"), "input")
    require(root("schema").asString.contains("rosen-avalanche-contract-preparation/v1"), "Unknown preparation schema")
    require(integer(root("chainId"), 43114, 43114, "chainId") == 43114, "Preparation targets mainnet")
    val cleanup = integer(root("cleanupConfirm"), 1, Int.MaxValue, "cleanupConfirm")
    val globals = objectFields(root("globalTokens"), globalKeys, "globalTokens").map { case (key, value) => key -> tokenId(value, key) }
    val chain = objectFields(root("chainTokens"), chainKeys, "chainTokens").map { case (key, value) => key -> tokenId(value, key) }
    val addresses = objectFields(root("custody"), Set("lock", "cold"), "custody")
    val lock = custody(addresses("lock"), "lock")
    val cold = custody(addresses("cold"), "cold")
    require(lock != cold, "Lock and cold custody must be distinct")
    val counterparts = objectFields(root("counterparts"), Set("avax", "joe"), "counterparts").map { case (asset, value) =>
      val fields = objectFields(value, Set("tokenId", "decimals"), s"counterparts.$asset")
      asset -> (tokenId(fields("tokenId"), s"counterparts.$asset"), integer(fields("decimals"), 0, 18, s"counterparts.$asset.decimals"))
    }
    val ids = globals.values.toSeq ++ chain.values.toSeq ++ counterparts.values.map(_._1)
    require(ids.distinct.size == ids.size, "Token identities must not collide")
    val sources = scriptPins()

    // ColdErgoClient has no node transport; only the unchanged Rosen scripts are compiled.
    val client = new ColdErgoClient(NetworkType.MAINNET, 1000000, 2.toByte)
    val network = ErgoNetwork(client, new ErgoAddressEncoder(NetworkType.MAINNET.networkPrefix))
    val main = MainTokens(globals("RWTRepoNFT"), globals("GuardNFT"), globals("RSN"), globals("MinFeeNFT"), globals("EmissionNFT"), globals("ERSN"))
    val tokens = Tokens(chain("CleanupNFT"), chain("RWTId"), chain("AwcNFT"), chain("RepoConfigNFT"))
    val config = Network(tokens, lock, cold, cleanup)
    val contracts = new Contracts(NetworkGeneral(network, main), config)
    val contractOverlay = Json.obj(
      "version" -> Json.fromString(BuildInfo.version), "tokens" -> main.toJson(),
      "avalanche" -> contracts.buildContractsJson("avalanche")
    )
    // The generic Lock also compiles for the Ergo counterparty; Avalanche uses TSS custody.
    require(contracts.Lock._2.nonEmpty, "Ergo lock compilation failed")
    require(scriptPins() == sources, "Contract scripts changed during compilation")
    val tokenOverlay = Json.obj(
      "version" -> Json.fromString(BuildInfo.version),
      "tokens" -> Json.arr(tokenSet("avax", "AVAX", true, counterparts("avax")), tokenSet(JoeAddress, "JOE", false, counterparts("joe")))
    )
    (contractOverlay, tokenOverlay)
  }

  /** Hash the exact input, compiled-script sources and generated bytes for a deployment handoff. */
  private def digest(bytes: Array[Byte]): String = MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString

  /** Capture the exact upstream script sources without persisting host paths. */
  private def scriptPins(): Seq[Json] = {
    val stream = Files.list(Paths.get("src/main/scala/rosen/bridge/scripts"))
    try {
      import scala.collection.JavaConverters._
      stream.iterator().asScala.filter(_.toString.endsWith(".es")).toSeq.sortBy(_.getFileName.toString).map { path =>
        Json.obj("path" -> Json.fromString(path.toString.replace('\\', '/')), "sha256" -> Json.fromString(digest(Files.readAllBytes(path))))
      }
    } finally stream.close()
  }

  /** Write a fresh preparation directory; this entry point has no minting or broadcast operation. */
  def main(args: Array[String]): Unit = {
    require(args.length == 4 && args(0) == "--input" && args(2) == "--output", "Usage: --input <assigned-values.json> --output <new-directory>")
    val inputBytes = Files.readAllBytes(Paths.get(args(1)))
    val input = parseInput(new String(inputBytes, StandardCharsets.UTF_8))
    val sourcePins = scriptPins()
    val (contracts, tokens) = prepare(input)
    require(scriptPins() == sourcePins, "Contract scripts changed before output preparation")
    val output = Paths.get(args(3))
    require(!Files.exists(output), "Output directory must be new")
    val parent = output.toAbsolutePath.getParent
    require(Files.isDirectory(parent), "Output parent must exist")
    val payloads = Seq("contracts-overlay.json" -> contracts, "tokens-overlay.json" -> tokens)
    Files.createDirectory(output)
    val members = payloads.map { case (name, value) =>
      val bytes = (value.spaces2 + "\n").getBytes(StandardCharsets.UTF_8)
      Files.write(output.resolve(name), bytes)
      Json.obj("path" -> Json.fromString(name), "bytes" -> Json.fromInt(bytes.length), "sha256" -> Json.fromString(digest(bytes)))
    }
    val manifest = Json.obj(
      "schema" -> Json.fromString("rosen-avalanche-contract-preparation-receipt/v1"),
      "chainId" -> Json.fromInt(43114), "mode" -> Json.fromString("offline-preparation"),
      "minted" -> Json.False, "broadcast" -> Json.False, "inputSha256" -> Json.fromString(digest(inputBytes)),
      "scripts" -> Json.arr(sourcePins: _*), "files" -> Json.arr(members: _*)
    )
    Files.write(output.resolve("manifest.json"), (manifest.spaces2 + "\n").getBytes(StandardCharsets.UTF_8))
    println(s"Avalanche contract and token overlays prepared: ${members.size} files")
  }
}
