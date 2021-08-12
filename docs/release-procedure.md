# Release Procedure

1. Checkout to `master` and ensure there are no pending changes.

    ```
    git checkout master && git status
    ```

2. Ensure all tests are passed.

    ```
    cargo test
    ```

3. Bump version in [Cargo.toml](../Cargo.toml).

    ```toml
    version = "0.2.0"
    ```

4. Update the changelog entries in [CHANGELOG.md](../CHANGELOG.md).

    ```
    ## [0.2.0](https://github.com/kafji/tpyo/tree/v0.2.0) - 2021-12-31

    - Tpyo can reload its configuration file without killing itself, similar to Nginx.
    ```

5. Commit changes.

    ```
    git commit -am "Prepare release 0.2.0"
    ```

6. Tag the commit.

    ```
    git tag v0.2.0
    ```

7. Publish the commit and tag.

    ```
    git push --tags origin master
    ```

8. Build binary.

    ```
    cargo build --release
    ```

9. Publish binary on GitHub.

    https://github.com/kafji/tpyo/releases/new?tag=v0.2.0&title=0.2.0
