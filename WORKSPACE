# Give your project a name. :)
workspace(name = "mdium")

# Load the repository rule to download an http archive.
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

# Download rules_haskell and make it accessible as "@rules_haskell".
http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-df5aeb7169021d88ef7bdf2a1d7655838cd1b375",
    urls = ["https://github.com/tweag/rules_haskell/archive/df5aeb7169021d88ef7bdf2a1d7655838cd1b375.tar.gz"],
    sha256 = "b9bb4f015cb5e9dcf61541cf2dcead33279fd26a2f09f1617468a0ccea95be98",
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
rules_haskell_toolchains(version = "9.0.1")

# Docker
http_archive(
    name = "io_bazel_rules_docker",
    sha256 = "59536e6ae64359b716ba9c46c39183403b01eabfbd57578e84398b4829ca499a",
    strip_prefix = "rules_docker-0.22.0",
    urls = ["https://github.com/bazelbuild/rules_docker/releases/download/v0.22.0/rules_docker-v0.22.0.tar.gz"],
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
