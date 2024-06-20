package org.enso.yaml

import org.yaml.snakeyaml.nodes.Node

trait SnakeYamlEncoder[T] {
  def encode(value: T): Node
}
