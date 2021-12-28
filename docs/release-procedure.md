# Release Procedure

1. Stage environment.

    1. Ensure current local revision is up to date with `origin/master` and there are no pending changes.

        ```
        git checkout master && git fetch origin master && git status
        ```

2. Verify current revision build is green on [https://github.com/kafji/wmd](https://github.com/kafji/wmd).

3. Update documents.

    1. Bump version in [Cargo.toml](../Cargo.toml).

        ```toml
        version = "0.2.0"
        ```

    2. Update the changelog entries in [CHANGELOG.md](../CHANGELOG.md).

        ```
        ## [0.2.0](https://github.com/kafji/wmd/tree/v0.2.0) - 2021-12-31

        - Add a feature.
        ```

4. Commit changes and create version tag.

    ```
    git commit -am "Prepare release 0.2.0" && git tag v0.2.0
    ```

5. Publish the commit and the tag.

    ```
    git push origin master && git push --tags origin
    ```

6. Create release on GitHub.

    https://github.com/kafji/wmd/releases/new?tag=v0.2.0&body=```%0Adocker pull ghcr.io/kafji/wmd:0.2%0A```
