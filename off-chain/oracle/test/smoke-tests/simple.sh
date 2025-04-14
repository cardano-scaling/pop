#!/usr/bin/env bash

set -euo pipefail
# set -x

function jsonEquality() {
    local json1="$1"
    local json2="$2"

    # Use jq to compare the two JSON strings
    if [[ "$(echo "$json1" | jq -r -c -S .)" == "$(echo "$json2" | jq -c -r -S .)" ]]; then
        return  # JSONs are equal
    else
        echo "JSONs are not equal:"
        echo "Expected: $json2"
        echo "Actual:   $json1"
        exit 1  # JSONs are not equal
    fi
}

function keyValJSON() {
    local key="$1"
    local value="$2"
    # Create a JSON object with the key and value
    echo "{\"key\": \"$key\", \"value\": $value}"
}

function keyJSON() {
    local key="$1"
    # Create a JSON object with the key
    echo "{\"key\": \"$key\"}"
}

function stringToBufferJSON() {
    local inputString="$1"
    local -a buffer=()
    for ((i=0; i<${#inputString}; i++)); do
        buffer+=("$(printf "%d" "'${inputString:i:1}")")
    done
    jq -n --argjson buf "$(printf '%s\n' "${buffer[@]}" | jq -s '.')" \
        '{"type":"Buffer","data":$buf}'
}

PORT=${1:-3000}
SERVER=${2:-"localhost"}

BASE_URL="http://$SERVER:$PORT"

function callCurl() {
    local method="$1"
    local url="$2"
    local data="$3"

    curl -s -X "$method" "$BASE_URL/$url" \
        -H "Content-Type: application/json" \
        -d "$data"
}

# gross cleanup
for key in "apple" "banana" "orange"; do
    callCurl DELETE "value" "{\"key\":\"$key\"}" >> /dev/null || true
done

########### scenarios ###########

# root end-point
result=$(callCurl GET "root" "" | jq -r '.root')
jsonEquality "$result" "null"

# insert a retrieve an apple at key apple
_=$(callCurl PUT "value" "$(keyValJSON "apple" "\"apple\"")")
response=$(callCurl GET "value" "$(keyJSON "apple")" | jq -r '.value')
jsonEquality "$response" "$(stringToBufferJSON "apple")"

# delete the apple
_=$(callCurl DELETE "value" "$(keyJSON "apple")")
response=$(callCurl GET "value" "$(keyJSON "apple")" | jq -r '.value')
jsonEquality "$response" "null"

# insert and get a proof for apple
_=$(callCurl PUT "value" "$(keyValJSON "apple" "\"apple\"")")
response=$(callCurl GET "proof" "$(keyJSON "apple")" | jq -r '.proof.json')
jsonEquality "$response" "[]"

# insert and get a proof for banana
_=$(callCurl PUT "value" "$(keyValJSON "banana" "\"banana\"")")
response=$(callCurl GET "proof" "$(keyJSON "banana")" | jq -r '.proof.json')
expected='[
  {
    "type": "leaf",
    "skip": 0,
    "neighbor": {
      "key": "09ad7de5023dec71b2b4d5dc28d296327c6bbd6d47f199cbb9afafc8967d19d9",
      "value": "09ad7de5023dec71b2b4d5dc28d296327c6bbd6d47f199cbb9afafc967d19d9"
    }
  }
]'

jsonEquality "$response" "$expected"