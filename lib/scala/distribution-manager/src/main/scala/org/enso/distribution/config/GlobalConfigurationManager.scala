package org.enso.distribution.config

import com.typesafe.scalalogging.Logger
import org.enso.distribution.DistributionManager
import org.enso.distribution.FileSystem.PathSyntax
import org.enso.scala.yaml.{YamlDecoder, YamlEncoder}
import org.yaml.snakeyaml.{DumperOptions, Yaml}
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.{MappingNode, Node, NodeTuple, ScalarNode, Tag}

import java.io.{BufferedWriter, StringReader}
import java.nio.file.{Files, NoSuchFileException, Path}
import scala.jdk.CollectionConverters.{CollectionHasAsScala, SeqHasAsJava}
import scala.util.{Failure, Success, Try, Using}

/** Manages the global configuration of the distribution. */
class GlobalConfigurationManager(distributionManager: DistributionManager) {
  private val logger = Logger[GlobalConfigurationManager]

  /** Location of the global configuration file. */
  def configLocation: Path =
    distributionManager.paths.config / GlobalConfigurationManager.globalConfigName

  /** Loads the current global configuration.
    *
    * If the configuration file does not exist, the default config is returned.
    * If it exists but cannot be loaded, an exception is thrown.
    */
  def getConfig: GlobalConfig =
    GlobalConfigurationManager
      .readConfig(configLocation)
      .recoverWith { case _: NoSuchFileException =>
        logger.debug(
          "Global config [{}] not found, falling back to defaults.",
          configLocation
        )
        Success(GlobalConfig.Default)
      }
      .get

  /** Applies `update` to the current config and saves the returned value. */
  def updateConfig(update: GlobalConfig => GlobalConfig): Unit = {
    val updated = update(getConfig)
    GlobalConfigurationManager.writeConfig(configLocation, updated).get
  }

  /** Sets `key` to the raw JSON `value` in the config.
    *
    * If changing that setting would result in the config becoming unreadable
    * (because an invalid value has been set for a known field), the config is
    * not saved and an exception is thrown.
    */
  def updateConfigRaw(key: String, value: String): Unit = {
    stringToYamlNode(value)
      .flatMap(newValueNode =>
        updateYamlNode(key.split("\\.").toList, getConfig, newValueNode)
          .flatMap { updatedNode =>
            GlobalConfigurationManager
              .writeConfigRaw(configLocation, updatedNode)
              .recoverWith { case e: InvalidConfigError =>
                Failure(
                  InvalidConfigError(
                    s"Invalid value for key `$key`. Config changes were not saved",
                    e
                  )
                )
              }
          }
      )
      .get
  }

  private def stringToYamlNode(value: String): Try[Node] = {
    if (value == null) {
      Success(null)
    } else {
      val snakeYaml = new org.yaml.snakeyaml.Yaml()
      Try(snakeYaml.compose(new StringReader(value)))
    }
  }

  /** Updates GlobalConfig's YAML representation at the provided key-path */
  private def updateYamlNode(
    keys: List[String],
    config: GlobalConfig,
    yamlNode: Node
  ): Try[Node] = {
    val encoder   = implicitly[YamlEncoder[GlobalConfig]]
    val snakeYaml = new org.yaml.snakeyaml.Yaml()
    updateYamlNode(keys, snakeYaml.represent(encoder.encode(config)), yamlNode)
  }

  /** Updates the given YAML node at the provided key-path.
    * If the new `yamlNode` is null, the node at the provided key-path should be removed.
    *
    * @param keys list of keys representing the parent-child relation in YAML nodes
    * @param original the currently traversed YAML node
    * @param yamlNode the node to be placed at the end of the key-path
    * @return the updated YAML node
    */
  private def updateYamlNode(
    keys: List[String],
    original: Node,
    yamlNode: Node
  ): Try[Node] = {
    keys match {
      case Nil =>
        Success(yamlNode)
      case head :: rest =>
        original match {
          case mappingNode: MappingNode =>
            val tuples = mappingNode.getValue.asScala
            val (failures, mappings) = tuples
              .map { tuple =>
                tuple.getKeyNode match {
                  case s: ScalarNode =>
                    Right((s.getValue, (tuple.getValueNode, s)))
                  case _ =>
                    Left(
                      new YAMLException(
                        "Internal error: Unexpected key in the mapping node"
                      )
                    )
                }
              }
              .span(_.isLeft)
            if (failures.isEmpty) {
              val m = mappings.map(_.toOption.get).toMap
              m.get(head) match {
                case Some((entryNode, scalarKeyNode)) =>
                  val others = m.removed(head)
                  updateYamlNode(rest, entryNode, yamlNode) map { node =>
                    createMappingNode(
                      others.toList.map { case (_, (valueNode, keyNode)) =>
                        (keyNode, valueNode)
                      },
                      scalarKeyNode,
                      node,
                      mappingNode
                    )
                  }
                case None if yamlNode == null =>
                  // cannot remove node that is not present
                  Success(mappingNode)
                case None =>
                  val scalarKeyNode = new ScalarNode(
                    Tag.YAML,
                    keys.mkString("."),
                    null,
                    null,
                    DumperOptions.ScalarStyle.PLAIN
                  )
                  Success(
                    createMappingNode(
                      m.toList.map { case (_, (valueNode, keyNode)) =>
                        (keyNode, valueNode)
                      },
                      scalarKeyNode,
                      yamlNode,
                      mappingNode
                    )
                  )
              }
            } else {
              Failure(failures.head.left.toOption.get)
            }
          case _ =>
            Failure(
              new YAMLException(s"Cannot replace `$head` in the non-map field")
            )
        }
    }
  }

  private def createMappingNode(
    existingTuples: List[(Node, Node)],
    newKeyNode: Node,
    newValueNode: Node,
    existingMappingNode: MappingNode
  ): Node = {
    val allTuples =
      if (newValueNode != null)
        existingTuples ++ List((newKeyNode, newValueNode))
      else existingTuples
    new MappingNode(
      existingMappingNode.getTag,
      true,
      allTuples.map(v => new NodeTuple(v._1, v._2)).asJava,
      existingMappingNode.getStartMark,
      existingMappingNode.getEndMark,
      existingMappingNode.getFlowStyle
    )
  }

  /** Removes the `key` from the config.
    *
    * If removing that setting would result in the config becoming unreadable,
    * the config is not saved and an exception is thrown.
    */
  def removeFromConfig(key: String): Unit = {
    updateYamlNode(key.split("\\.").toList, getConfig, null).map(updatedNode =>
      GlobalConfigurationManager.writeConfigRaw(
        configLocation,
        updatedNode
      )
    )
  }
}

object GlobalConfigurationManager {

  /** Name of the main global configuration file. */
  val globalConfigName: String = "global-config.yaml"

  /** Tries to read the global config from the given `path`. */
  private def readConfig(path: Path): Try[GlobalConfig] =
    Using(Files.newBufferedReader(path)) { reader =>
      val snakeYaml = new Yaml()
      Try(snakeYaml.compose(reader)).toEither
        .flatMap(implicitly[YamlDecoder[GlobalConfig]].decode(_))
        .toTry
    }.flatten

  /** Tries to write the provided `config` to the given `path`. */
  private def writeConfig(path: Path, config: GlobalConfig): Try[Unit] = {
    val snakeYaml = new org.yaml.snakeyaml.Yaml()
    writeConfigRaw(
      path,
      snakeYaml.represent(
        implicitly[YamlEncoder[GlobalConfig]].encode(config)
      )
    )
  }

  /** Tries to write the config from a raw JSON value to the given `path`.
    *
    * The config will not be saved if it is invalid, instead an exception is
    * thrown.
    */
  private def writeConfigRaw(path: Path, rawNode: Node): Try[Unit] = {
    def verifyConfig: Try[Unit] = {
      implicitly[YamlDecoder[GlobalConfig]].decode(rawNode) match {
        case Left(failure) =>
          Failure(
            InvalidConfigError(
              s"Cannot parse modified config. Config changes were not saved.",
              failure
            )
          )
        case Right(_) => Success(())
      }
    }

    def bufferedWriter: BufferedWriter = {
      Files.createDirectories(path.getParent)
      Files.newBufferedWriter(path)
    }
    def writeConfig: Try[Unit] =
      Using(bufferedWriter) { writer =>
        val dumperOptions = new DumperOptions()
        dumperOptions.setIndent(2)
        dumperOptions.setPrettyFlow(true)
        val yaml = new Yaml(dumperOptions)
        yaml.serialize(rawNode, writer)
        writer.newLine()
      }

    for {
      _ <- verifyConfig
      _ <- writeConfig
    } yield ()
  }
}
