package org.enso.polyglot.data
import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}

/** An either-or-both data type. */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(
      value = classOf[These.Here[_]],
      name  = "theseHere"
    ),
    new JsonSubTypes.Type(
      value = classOf[These.There[_]],
      name  = "theseThere"
    ),
    new JsonSubTypes.Type(
      value = classOf[These.Both[_, _]],
      name  = "theseBoth"
    )
  )
)
sealed trait These[+A, +B]

object These {

  case class Here[+A](
    @JsonTypeInfo(
      use     = JsonTypeInfo.Id.CLASS,
      include = JsonTypeInfo.As.PROPERTY
    )
    here: A
  ) extends These[A, Nothing]

  case class There[+B](
    @JsonTypeInfo(
      use     = JsonTypeInfo.Id.CLASS,
      include = JsonTypeInfo.As.PROPERTY
    )
    there: B
  ) extends These[Nothing, B]

  case class Both[+A, +B](
    @JsonTypeInfo(
      use     = JsonTypeInfo.Id.CLASS,
      include = JsonTypeInfo.As.PROPERTY
    )
    here: A,
    @JsonTypeInfo(
      use     = JsonTypeInfo.Id.CLASS,
      include = JsonTypeInfo.As.PROPERTY
    )
    there: B
  ) extends These[A, B]
}
