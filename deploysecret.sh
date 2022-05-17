#!/bin/bash

base64 -w0 < ./wmd.toml | xargs -I % flyctl secrets set WMD_CONFIG=%
