package org.enso.yaml

import org.enso.scala.yaml.{YamlDecoder, YamlEncoder}
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.{DumperOptions, Yaml}

import java.io.{FileReader, StringReader}
import java.nio.file.Path
import scala.util.{Try, Using}

/** A helper for parsing YAML configs. */
object YamlHelper {

  /** Parses a string representation of a YAML configuration of type `R`. */
  def parseString[R](
    yamlString: String
  )(implicit decoder: YamlDecoder[R]): Either[ParseError, R] = {
    val snakeYaml = new org.yaml.snakeyaml.Yaml()
    Try(snakeYaml.compose(new StringReader(yamlString))).toEither
      .flatMap(decoder.decode(_))
      .left
      .map(ParseError(_))
  }

  /** Tries to load and parse a YAML file at the provided path. */
  def load[R](path: Path)(implicit decoder: YamlDecoder[R]): Try[R] =
    Using(new FileReader(path.toFile)) { reader =>
      val snakeYaml = new org.yaml.snakeyaml.Yaml()
      Try(snakeYaml.compose(reader))
        .flatMap(decoder.decode(_).toTry)
    }.flatten

  /** Saves a YAML representation of an object into a string. */
  def toYaml[A](obj: A)(implicit encoder: YamlEncoder[A]): String = {
    val node          = encoder.encode(obj)
    val dumperOptions = new DumperOptions()
    dumperOptions.setIndent(2)
    dumperOptions.setPrettyFlow(true)
    val yaml = new Yaml(dumperOptions)
    yaml.dumpAs(node, Tag.MAP, DumperOptions.FlowStyle.BLOCK)
  }
}
