"""

"""

def _find_usable_system_tar(rctx, tar_name):
    tar = rctx.which(tar_name)
    if not tar:
        fail("tar not found on PATH, and we don't handle this case yet")

    # Run tar --version and see if we are satisfied to use it
    tar_version = rctx.execute([tar, "--version"]).stdout.strip()
    if tar_version.find("bsdtar") >= 0:
        return tar

    fail("tar isn't a BSD tar")

def _tar_toolchain_repo_impl(rctx):
    tar_name = "tar.exe"
    tar = _find_usable_system_tar(rctx, tar_name)
    output = rctx.path(tar_name)
    rctx.symlink(tar, output)
    rctx.file("BUILD.bazel", """
load("@aspect_bazel_lib//lib/private:tar_toolchain.bzl", "tar_toolchain")

tar_toolchain(
    name = "windows_builtin_tar_toolchain",
    binary = "tar.exe",
)
""")

tar_toolchain_repo_rule = repository_rule(
    implementation = _tar_toolchain_repo_impl,
)

def _tar_toolchain_impl(_ctx):
    tar_toolchain_repo_rule(
        name = "tar_toolchain",
    )

tar_toolchain = module_extension(
    implementation = _tar_toolchain_impl,
)
