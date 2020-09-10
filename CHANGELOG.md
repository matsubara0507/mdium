# Changelog for mdium

## Unreleased changes

- Misc: update LTS to 16.3 (GHC 8.8.4)
- CI: replace to GitHub Actions
- Misc: use GitHub Container Registry
- Misc: add ENTRYPOINT to Docker image

### 1.0.0

- Feat: `--publications` option (show your publication groups)
- Feat: `--orgs` option (post story to publication)
- Refactor: update all for lts-14.6 and stack v2
    - update deps package extensible to 0.6.1
    - use mix.hs and fallible
    - use githash instead of gitrev
    - update docker integration

## 0.2.0.0

- Feat: `--title` option (explicit specify post title)
- Reafactor: `API.defaultPostStroyParams`
- Feat: docker image

## 0.1.0.0

- Feat: Post Markdown story to Medium
- Feat: `--me` option (verify Medium token)
