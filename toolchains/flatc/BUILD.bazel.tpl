load("@//toolchains/flatc:toolchain.bzl", "flatc_toolchain")

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
    exec_compatible_with = [
        {exec_constraints},
    ],
    target_compatible_with = [
        {target_constraints},
    ],
)