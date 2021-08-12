# Tpyo Manual

## Distribution & Installation

The binary can be found in the project release page in [https://github.com/kafji/tpyo/releases](https://github.com/kafji/tpyo/releases).

## Configuration File

Tpyo requires a configuration file.

An example can be found at [/tpyo.example.toml](../tpyo.example.toml).

Follow these steps to create your configuration file:

1. Copy [the example file](../tpyo.example.toml) as `tpyo.toml`.

    ```bash
    cp ./tpyo.example.toml tpyo.toml
    ```

2. Set the `url` key with URL to access your instance of Tpyo.

   If you're deploying it locally without specifiying custom port, it will be `http://127.0.0.1:39496/`.

    ```toml
    url = "http://127.0.0.1:39496/"
    ```

3. Define your search targets.

   The example file already have few search targets for example purposes.

   To modify your search targets see [Search Targets](#search-targets) section.

## Start Server

To start serving search requests, type `tpyo serve` in your terminal.

By default, Tpyo will listen at port `39496` and expecting the configuration file at `./tpyo.toml`. Both of these defaults can be overriden. See `tpyo serve --help` on how to do so.

When you see `tpyo: start listening` message in your terminal, your Tpyo instance is up and running.

```
Aug 15 00:00:00.000  INFO tpyo: start listening addr=127.0.0.1:39496
```

For how to setup your web browser to send search request to Tpyo instance, see [end-user-manual.md](end-user-manual.md).

## Search Targets

### Terms

The primary endpoint of Tpyo is `/search` with `q` as its query parameter. Where query will be split into prefix and keywords delimited by a space.

```
https://tpyo.netonique.net/search?q=rs partialeq
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

In the case where Tpyo fails to find target with matching prefix, the search request will be redirected to the default target, which is the first target found in the configuration file, and all of the query will be treated as keywords.
