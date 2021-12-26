cargo build --release
WMD_VERSION=$(./target/release/wmd --version)
docker build . --tag wmd:$WMD_VERSION
