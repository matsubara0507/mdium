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
    sha256 = "b6d46438523a3ec0f3cead544190ee13223a52f6a6765a29eae7b7cc24cc83a0",
    urls = ["https://github.com/bazelbuild/rules_python/releases/download/0.1.0/rules_python-0.1.0.tar.gz"],
)

# Download rules_haskell and make it accessible as "@rules_haskell".
http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-3b8182ca5287cf93687fff1cefd98910f683b679",
    urls = ["https://github.com/tweag/rules_haskell/archive/3b8182ca5287cf93687fff1cefd98910f683b679.tar.gz"],
    sha256 = "85f269adfecfc5760fae6017608f7efebfccb719c22c7e71af03c4887f54b08e",
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
    local_snapshot = "//:stack-snapshot.yaml",
)

# Download a GHC binary distribution from haskell.org and register it as a toolchain.
rules_haskell_toolchains(version = "8.8.4")

# Docker
http_archive(
    name = "io_bazel_rules_docker",
    sha256 = "1698624e878b0607052ae6131aa216d45ebb63871ec497f26c67455b34119c80",
    strip_prefix = "rules_docker-0.15.0",
    urls = ["https://github.com/bazelbuild/rules_docker/releases/download/v0.15.0/rules_docker-v0.15.0.tar.gz"],
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
    registry = "registry.hub.docker.com",
    repository = "matsubara0507/ubuntu-for-haskell",
    digest = "sha256:5967c5908a6c79dc4f4253badfe90326aaf4584a3eaa42d9c9ecc5ae8ba4d133",
)
