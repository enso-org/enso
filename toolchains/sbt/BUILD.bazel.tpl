load("@//toolchains/sbt:toolchain.bzl", "sbt_toolchain")

sbt_toolchain(
    name = "toolchain_impl",
    sbt_binary = "{sbt_binary}",
)

filegroup(
    name = "sbt_launch_jar",
    srcs = ["sbt/bin/sbt-launch.jar"],
    visibility = ["//visibility:public"],
)

toolchain(
    name = "toolchain",
    toolchain_type = "@//toolchains/sbt:toolchain_type",
    toolchain = ":toolchain_impl",
    visibility = ["//visibility:public"],
)