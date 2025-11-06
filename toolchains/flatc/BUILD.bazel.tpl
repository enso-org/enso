load("@//toolchains/flatc:toolchain.bzl", "flatc_toolchain")
load("@platforms//host:constraints.bzl", "HOST_CONSTRAINTS")

flatc_toolchain(
    name = "toolchain_impl",
    flatc_binary = "{flatc_binary}",
    flatc_path = "{flatc_path}",
)

toolchain(
    name = "toolchain",
    toolchain_type = "@//toolchains/flatc:toolchain_type",
    toolchain = ":toolchain_impl",
    visibility = ["//visibility:public"],
    exec_compatible_with = HOST_CONSTRAINTS,
    target_compatible_with = HOST_CONSTRAINTS,
)