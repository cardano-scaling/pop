#!/bin/sh

# Set the name and tag of the Docker image to scan
NAME="alpine"
VERSION="latest"

# Run Syft to scan the Docker image and output the result in JSON format
syft scan ${NAME}:${VERSION} -o json > sbom.json

