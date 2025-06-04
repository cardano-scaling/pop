#!/bin/sh

# proof of concept for releasing software using PoP;
# A scheduler that monitors radicle peer and sends transactions using cardano-cli
# 
# Run rad node logs and process each line
rad node logs |
  while IFS= read -r line; do
    # Check if the line contains "Received command QueryState"
    if grep -q "Received command QueryState" <<< "$line"; then
      # Run the command and capture the output
      tip_info=$(nix run ~/workshop/cardano-node/.#cardano-cli -- query tip --testnet-magic 1 --socket-path "$HOME/workshop/preprod/db/node.socket")
      # Print the message with the tip information
      printf "QueryState command received. Current node tip:\n%s\n" "$tip_info"
    fi
  done
