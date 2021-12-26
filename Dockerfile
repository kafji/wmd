# syntax=docker/dockerfile:1
FROM debian:11
ENV RUST_LOG=info
WORKDIR ~
COPY ./wmd.example.toml /etc/wmd/config.toml
COPY ./target/release/wmd /usr/local/bin
EXPOSE 8000
ENTRYPOINT ["wmd"]
CMD ["serve", "--port", "8000", "--config", "/etc/wmd/config.toml"]
