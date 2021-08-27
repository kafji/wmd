# wmd Manual

## Distribution & Installation

The binary can be found in the project release page in [https://github.com/kafji/wmd/releases](https://github.com/kafji/wmd/releases).

## Configuration File

wmd requires a configuration file.

An example can be found at [/wmd.example.toml](../wmd.example.toml).

Follow these steps to create your configuration file:

1. Copy [the example file](../wmd.example.toml) as `wmd.toml`.

    ```bash
    cp ./wmd.example.toml wmd.toml
    ```

2. Set the `url` key with URL to access your instance of wmd.

   If you're deploying it locally without specifiying custom port, it will be `http://127.0.0.1:39496/`.

    ```toml
    url = "http://127.0.0.1:39496/"
    ```

3. Define your search targets.

   The example file already have few search targets for example purposes.

   To modify your search targets see [Search Targets](#search-targets) section.

## Start Server

To start serving search requests, type `wmd serve -c ./wmd.toml` in your terminal.

By default, wmd will listen at port `39496` this can be overriden. See `wmd serve --help` on how to do so.

When you see `wmd: start listening` message in your terminal, your wmd instance is up and running.

```
Aug 15 00:00:00.000  INFO wmd: start listening addr=127.0.0.1:39496
```

For how to setup your web browser to send search request to wmd instance, see [end-user-manual.md](end-user-manual.md).

## Search Targets

### Terms

The primary endpoint of wmd is `/search` with `q` as its query parameter. Where query will be split into prefix and keywords delimited by a space.

```
https://xmpl.netonique.net/search?q=rs partialeq
                                    ^^ ^^^^^^^^^
                                prefix keywords
                                    ^^^^^^^^^^^^
                                    query

```

### Configuration

Search targets are defined in the configuration file.

For example, following snippet is a search target for query that starts with `rs`.

```toml
[[targets]]
prefix = "rs"
name = "The Rust Standard Library"
url_template = "https://doc.rust-lang.org/std/index.html?search={keywords}"
```

`{keywords}` in the `url_template` value will be replaced by the actual keywords, so `rs arc` will become `https://doc.rust-lang.org/std/index.html?search=arc`.

In the case where wmd fails to find target with matching prefix, the search request will be redirected to the default target, which is the first target found in the configuration file, and all of the query will be treated as keywords.
