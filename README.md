# mdium

## Requirement

- git
- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
- Medium integration token
    - you can generate in [medium settigns](https://medium.com/me/settings).

## Install

Clone this repository:

```
$ git clone https://github.com/matsubara0507/mdium.git
$ cd mdium
```

Install with stack tool:

```
$ stack install mdium
```

## Usage

```
mdium [options] [input-file]
      --version  Show version
  -v  --verbose  Enable verbose mode: verbosity level "debug"
      --me       Call Medium `me` API
```

Set Medium integration token to `MEDIUM_TOKEN` environment (use `./.env` or `~/.env`).
Can check to validate your token by executing `medium --me` command:

```
$ mdium --me
Hi MATSUBARA Nobutada!!
```

If post [`examle/example.md`](example/example.md) to Medium:

```
$ mdium ./example/examle.md
post success, browse to: https://medium.com/@nobutada/e31f70013b36
```

then generate story like below in Medium:

![](example/example.png)
