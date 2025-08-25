"""
Repository definitions for FlatBuffers toolchains. Custom repository rule to download and extract the flatc binary for specific platforms.
"""

def _flatc_toolchain_repo_impl(rctx):
    rctx.download_and_extract(
        url = rctx.attr.url,
        sha256 = rctx.attr.sha256,
    )
    flatc_binary = "flatc"
    os_constraint = None
    arch_constraint = None
    if rctx.attr.os == "windows":
        flatc_binary = "flatc.exe"

    if rctx.attr.os == "macos":
        os_constraint = '"@platforms//os:macos"'
    elif rctx.attr.os == "linux":
        os_constraint = '"@platforms//os:linux"'
    elif rctx.attr.os == "windows":
        os_constraint = '"@platforms//os:windows"'

    if rctx.attr.arch == "aarch64":
        arch_constraint = '"@platforms//cpu:aarch64"'
    elif rctx.attr.arch == "x86_64":
        arch_constraint = '"@platforms//cpu:x86_64"'

    constraints = [os_constraint, arch_constraint]
    constraints_str = ", ".join(constraints)
    substitutions = {
        "{flatc_binary}": str(rctx.path(flatc_binary)),
        "{flatc_path}": str(rctx.path(flatc_binary).dirname),
        "{exec_constraints}": constraints_str,
        "{target_constraints}": constraints_str,
    }
    rctx.template(
        "BUILD.bazel",
        rctx.attr._build_tpl,
        substitutions = substitutions,
    )

flatc_toolchain_repo = repository_rule(
    implementation = _flatc_toolchain_repo_impl,
    attrs = {
        "url": attr.string(mandatory = True),
        "sha256": attr.string(mandatory = True),
        "os": attr.string(mandatory = True, values = ["macos", "linux", "windows"]),
        "arch": attr.string(mandatory = False, values = ["x86_64", "aarch64"], default = "x86_64"),
        "_build_tpl": attr.label(
            default = "@//toolchains/flatc:BUILD.bazel.tpl",
        ),
    },
)

def _ext_impl(_ctx):
    # Macos binaries are universal.
    for arch in ["aarch64", "x86_64"]:
        flatc_toolchain_repo(
            name = "flatc_toolchain_macos_{}".format(arch),
            url = "https://github.com/google/flatbuffers/releases/download/v24.3.25/Mac.flatc.binary.zip",
            sha256 = "277274f4e1037dbb57b1b95719721fe3d58c86983d634103284ad8c1d9cf19dd",
            os = "macos",
            arch = arch,
        )
    flatc_toolchain_repo(
        name = "flatc_toolchain_linux_x86_64",
        url = "https://github.com/google/flatbuffers/releases/download/v24.3.25/Linux.flatc.binary.clang++-15.zip",
        sha256 = "e7ac9c277adbad6a80321108eacf264046d1fba47300a060a6d7d686e7e4d7be",
        os = "linux",
        arch = "x86_64",
    )
    flatc_toolchain_repo(
        name = "flatc_toolchain_windows_x86_64",
        url = "https://github.com/google/flatbuffers/releases/download/v24.3.25/Windows.flatc.binary.zip",
        sha256 = "6455f5b6272b908dad073721e21b11720a9fddbae06e28b5c75f8ec458e7fe30",
        os = "windows",
        arch = "x86_64",
    )

flatc_toolchain = module_extension(
    implementation = _ext_impl,
)
