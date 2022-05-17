# syntax=docker/dockerfile:1
FROM debian:11
COPY target/release/wmd /usr/local/bin
EXPOSE 3000
ENTRYPOINT ["wmd"]
CMD []
