"""Cc toolchain definitions for use on wasm platforms"""

def _dummy_cc_toolchain_impl(_ctx):
    # The `all_files` attribute is referenced by rustc_compile_action().
    return [platform_common.ToolchainInfo(all_files = depset([]))]

dummy_cc_toolchain = rule(
    implementation = _dummy_cc_toolchain_impl,
    attrs = {},
)

# dummy values from https://bazel.build/tutorials/ccp-toolchain-config#configuring_the_c_toolchain
def _config_impl(ctx):
    return cc_common.create_cc_toolchain_config_info(
        ctx = ctx,
        toolchain_identifier = "dummy-wasm32-cc-toolchain",
        host_system_name = "unknown",
        target_system_name = "unknown",
        target_cpu = "unknown",
        target_libc = "unknown",
        compiler = "unknown",
        abi_version = "unknown",
        abi_libc_version = "unknown",
    )

dummy_cc_config = rule(
    implementation = _config_impl,
    attrs = {},
    provides = [CcToolchainConfigInfo],
)
