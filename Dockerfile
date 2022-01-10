# syntax=docker/dockerfile:1
FROM debian:11
ENV RUST_LOG=info
COPY wmd.example.toml /etc/wmd/config.toml
COPY target/release/wmd /usr/local/bin
EXPOSE 80
ENTRYPOINT ["wmd"]
CMD ["serve", "--port", "80", "--config", "/etc/wmd/config.toml"]
