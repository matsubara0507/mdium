# Give your project a name. :)
workspace(name = "mdium")

# Load the repository rule to download an http archive.
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

# For Docker:
#  rules_docker is depends on rules_python 0.1.0.
#  But, rules_haskell_dependencies in rules_haskell install rules_python 0.0.1.
#  So, install rules_python 0.1.0 before exec rules_haskell_dependencies.
http_archive(
    name = "rules_python",
    url = "https://github.com/bazelbuild/rules_python/releases/download/0.3.0/rules_python-0.3.0.tar.gz",
    sha256 = "934c9ceb552e84577b0faf1e5a2f0450314985b4d8712b2b70717dc679fdc01b",
)

# Download rules_haskell and make it accessible as "@rules_haskell".
http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-8c24bb72afaad76d0980d9b0c94b69d6409c80e5",
    urls = ["https://github.com/matsubara0507/rules_haskell/archive/8c24bb72afaad76d0980d9b0c94b69d6409c80e5.tar.gz"],
    sha256 = "f6adc84934874de28e0135d9f6ec67f0bbe30e5005afed374c4ce10c23d46ac4",
)

load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

# Setup all Bazel dependencies required by rules_haskell.
rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot",
)

stack_snapshot(
    name = "stackage",
    packages = [
        "aeson",
        "base",
        "dotenv",
        "extensible",
        "fallible",
        "github",
        "mix",
        "mix-json-logger",
        "mix-plugin-github",
        "pandoc",
        "pandoc-types",
        "rio",
        "wreq",
    ],
    setup_deps = {
        "xml-conduit": ["cabal-doctest"],
    },
    local_snapshot = "//:stack-snapshot.yaml",
)

# Download a GHC binary distribution from haskell.org and register it as a toolchain.
rules_haskell_toolchains(version = "8.10.7")

# Docker
http_archive(
    name = "io_bazel_rules_docker",
    sha256 = "95d39fd84ff4474babaf190450ee034d958202043e366b9fc38f438c9e6c3334",
    strip_prefix = "rules_docker-0.16.0",
    urls = ["https://github.com/bazelbuild/rules_docker/releases/download/v0.16.0/rules_docker-v0.16.0.tar.gz"],
    patches = ["//patch:fix-rules-docker.patch"],
    patch_args = ["-p1"],
)

load(
    "@io_bazel_rules_docker//repositories:repositories.bzl",
    container_repositories = "repositories",
)
container_repositories()

load(
    "@io_bazel_rules_docker//repositories:deps.bzl",
    container_deps = "deps",
)
container_deps()

load(
    "@io_bazel_rules_docker//container:container.bzl",
    "container_pull",
)

container_pull(
    name = "haskell_base",
    registry = "ghcr.io",
    repository = "matsubara0507/ubuntu-for-haskell",
    digest = "sha256:6a4c2444a7644907e0b523baf9d4516d0fe8c573d0165ce52ea9e38e4d096909",
)
