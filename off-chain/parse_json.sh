#!/bin/sh

# Use jq to extract and print the keys (i.e., the package names) from the SBOM
echo "SBOM Keys:"
jq 'keys' sbom.json

# Ask the user to select a key to display its contents
echo "Enter a key to display its contents (or 'q' to quit):"
read KEY

if [ "$KEY" = "q" ]; then
  exit 0
fi

# Use jq to extract and print the selected key's contents from the SBOM
echo "Contents of '$KEY':"
jq ".[\"$KEY\"]" sbom.json
script
