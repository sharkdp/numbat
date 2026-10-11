# Release checklist

This file can be used as-is, or copied into the GitHub PR description which includes
necessary changes for the upcoming release.

## Version bump

- [ ] Get the latest version by running `git tag | sort -V | tail -n1`
      or `grep ^version numbat/Cargo.toml`.
- [ ] Check if there are any changes in `numbat-exchange-rates` since the
      last release by running `git diff vX.Y.Z.. numbat-exchange-rates`. If that
      is the case, bump the version in `numbat-exchange-rates/Cargo.toml`.
- [ ] Update versions and dependencies in the following files:
      `numbat/Cargo.toml`, `numbat-cli/Cargo.toml`. Run `cargo build` to update `Cargo.lock`.
      Make sure to `git add` the `Cargo.lock` changes as well.
- [ ] Update the `numbat` version in `book/src/cli/installation.md`.
- [ ] Run `bash build.sh` in `numbat-wasm/` to update its `Cargo.lock` file.
- [ ] Update the version and date in `CITATION.cff`.

## Pre-release checks and updates

- [ ] Install the latest version (`cargo install -f --path numbat-cli`) and make
      sure that it is available on the `PATH` (`numbat --version` should show the
      new version).
- [ ] Push all changes and wait for CI to succeed (before continuing with the
      next section).
- [ ] Run `cargo publish --dry-run --workspace` to make sure that it will succeed later
      (after creating the GitHub release).

## Release

- [ ] Create a tag and push it: `git tag vX.Y.Z; git push origin tag vX.Y.Z`.
      This will trigger binary packaging and release uploads via GitHub Actions.
      REMINDER: If your `origin` is a fork, don't forget to push to e.g. `upstream`
      instead.
- [ ] Go to https://github.com/sharkdp/numbat/releases/new to create the new
      release. Select the new tag and also use it as the release title. Autogenerate
      the release notes, edit, and then publish the release.
- [ ] Check if the binary deployment works (archives and Debian packages should
      appear when the CI run *for the Git tag* has finished).
- [ ] Publish to crates.io by running the following in a *clean* repository.
      One way to do this is to clone a fresh copy.
      ```
      cargo publish --workspace
      ```
- [ ] Publish the web calculator and documentation through
      [pink-nixos](https://github.com/sharkdp/pink-nixos). Ensure the intended
      Numbat changes, including generated documentation and the WASM lock file,
      are committed and pushed to `main`. In the `pink-nixos` checkout:
      ```sh
      nix flake update numbat
      git diff -- flake.lock
      git add flake.lock
      git commit -m "Update Numbat website revision"
      ./deploy.sh
      ```
      The deployment requires a clean checkout and builds both the calculator
      and documentation from the pinned revision. Verify
      [numbat.dev](https://numbat.dev/) and its
      [documentation](https://numbat.dev/docs/), then push the `pink-nixos` commit.
