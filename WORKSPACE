## Until bzlmod support is added to rules_scala
## https://github.com/bazelbuild/rules_scala/pull/1541

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "bazel_skylib",
    sha256 = "b8a1527901774180afc798aeb28c4634bdccf19c4d98e7bdd1ce79d1fe9aaad7",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.4.1/bazel-skylib-1.4.1.tar.gz",
        "https://github.com/bazelbuild/bazel-skylib/releases/download/1.4.1/bazel-skylib-1.4.1.tar.gz",
    ],
)

load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")

RULES_SCALA_VERSION = "6.4.0"

RULES_SCALA_SHA = "9a23058a36183a556a9ba7229b4f204d3e68c8c6eb7b28260521016b38ef4e00"


http_archive(
    name = "io_bazel_rules_scala",
    sha256 = RULES_SCALA_SHA,
    strip_prefix = "rules_scala-%s" % RULES_SCALA_VERSION,
    url = "https://github.com/bazelbuild/rules_scala/releases/download/v%s/rules_scala-v%s.tar.gz" % (RULES_SCALA_VERSION, RULES_SCALA_VERSION),
)

load("@io_bazel_rules_scala//:scala_config.bzl", "scala_config")

scala_config(scala_version = "2.13.12") ## TODO: impossible to match 2.13.11


load("@io_bazel_rules_scala//scala:scala.bzl", "rules_scala_setup", "rules_scala_toolchain_deps_repositories")

# loads other rules Rules Scala depends on
rules_scala_setup()

# Loads Maven deps like Scala compiler and standard libs. On production projects you should consider
# defining a custom deps toolchains to use your project libs instead
rules_scala_toolchain_deps_repositories(fetch_sources = True)

load("@rules_proto//proto:repositories.bzl", "rules_proto_dependencies", "rules_proto_toolchains")

rules_proto_dependencies()

rules_proto_toolchains()

load("@io_bazel_rules_scala//scala:toolchains.bzl", "scala_register_toolchains")

scala_register_toolchains()

load("@io_bazel_rules_scala//testing:scalatest.bzl", "scalatest_repositories", "scalatest_toolchain")

scalatest_repositories()

scalatest_toolchain()
