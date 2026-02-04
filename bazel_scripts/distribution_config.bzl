"""Configuration for building Enso Engine distribution."""

load("//toolchains/sbt:run_sbt.bzl", "run_sbt")

def _sbt_path_props(base, sep):
    """Generates SBT path properties for a given base variable and separator."""
    return [
        "-Dsbt.repositories.local={}{}.sbt{}repositories".format(base, sep, sep),
        "-Dsbt.global.base={}".format(base),
        "-Dsbt.boot.directory={}{}.sbt{}boot".format(base, sep, sep),
        "-Dsbt.ivy.home={}{}.ivy2".format(base, sep),
        "-Dsbt.coursier.home={}{}.coursier".format(base, sep),
    ]

_SBT_PATH_PROPS = select({
    "@platforms//os:windows": _sbt_path_props("%TEMP%", "\\"),
    "//conditions:default": _sbt_path_props("$$TMPDIR", "/"),
})

_COMMON_JVM_OPTS = [
    "-Xss16M",
    "-Xmx4G",
    "-XX:+UseCompressedOops",
    "--add-exports=jdk.compiler/com.sun.tools.javac.api=ALL-UNNAMED",
    "--add-exports=jdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED",
    "--add-exports=jdk.compiler/com.sun.tools.javac.file=ALL-UNNAMED",
    "--add-exports=jdk.compiler/com.sun.tools.javac.parser=ALL-UNNAMED",
    "--add-exports=jdk.compiler/com.sun.tools.javac.tree=ALL-UNNAMED",
    "--add-exports=jdk.compiler/com.sun.tools.javac.util=ALL-UNNAMED",
    "--add-exports=jdk.graal.compiler/jdk.graal.compiler.graphio=ALL-UNNAMED",
    "--enable-native-access=ALL-UNNAMED",
    "--sun-misc-unsafe-memory-access=allow",
]

SBT_SYSTEM_PROPS = _SBT_PATH_PROPS + _COMMON_JVM_OPTS

_COMMON_ENV = {
    "JAVA_HOME": "$(JAVABASE)",
    "LC_ALL": "C.UTF-8",
    "SBT_SERVER_FORCESTART": "1",
}

def get_enso_env(native_image = False):
    """Get environment variables needed by SBT build.

    Args:
        native_image: Whether to build for native image (sets specific env vars).

    Returns:
        Environment dict with platform-specific PATH and other environment variables.
    """
    path_windows = "$(FLATC_PATH);$(JAVABASE)/bin;%PATH%"
    path_unix = "$(FLATC_PATH):$(JAVABASE)/bin:$$PATH"

    windows_env = dict(_COMMON_ENV, LocalAppData = "%TEMP%\\LocalAppData", PATH = path_windows)
    unix_env = dict(_COMMON_ENV, PATH = path_unix)

    if native_image:
        windows_env["ENSO_LAUNCHER"] = "native"
        unix_env["ENSO_LAUNCHER"] = "native"

    return select({
        "@platforms//os:windows": windows_env,
        "//conditions:default": unix_env,
    })

def engine_distribution(name, out_dir, srcs, extra_system_props = [], native_image = False, **kwargs):
    """Macro to build engine distribution using sbt.

    Args:
        name: The name of the rule.
        out_dir: Output directory.
        srcs: Source files.
        extra_system_props: Additional system properties.
        native_image: Whether to build for native image (sets specific env vars).
        **kwargs: Additional arguments for run_sbt.
    """
    run_sbt(
        name = name,
        srcs = srcs,
        args = [
            "buildEngineDistribution",
        ],
        env = get_enso_env(native_image),
        out_dir = out_dir,
        system_props = SBT_SYSTEM_PROPS + extra_system_props,
        **kwargs
    )
