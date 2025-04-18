# Play around

## Yaci or Blockfrost
You can use either Yaci or Blockfrost to run the contract.

### Yaci
To use Yaci

In a separate shell (use nix develop for a dirty setup)

```
yaci-cli

> create-cluster
> start
```

In the primary shell

```
export YACI=1
```


### Blockfrost

To use Blockfrost

```
export BLOCKFROST_PROJECT_ID=your_project_id
```

## credentials

```
just credentials
```

Create `ms.sk` and `me.addr` files in the current directory.
The address is a `preview` network address.

## funding

### Blockfrost

If you use blockfrost and the real preview network, you can use the `me.addr` address to fund it.

> https://docs.cardano.org/cardano-testnets/tools/faucet

### Yaci

If you use yaci-cli, you can use the `me.addr` address to fund it via the `topup` command.

```
> topup <address> <amount>
```

## build

All the commands are run in the shell opened via `nix develop`.

```
just build
```

Build the aiken code.

## check


```
just check
```

Check the aiken code.

## demo


Start the cli interface with

```
npx tsx off_chain/demo
```

Use `info` to inspect the state.

Top up the `cage` address with the Yaci CLI or the Blockfrost faucet.

Use `boot` to add a new token to the cage.

Use `insert` or `delete` to add facts to current token.

```
insert a a
update
info
```

```
delete a a
update
info
```

Both `insert` and `delete` commands take a key and a value.


Keys in both `insert` and `delete` are list of comma separated strings (no space)
Values are strings.

If you create un-includable facts, you can use `retract` (as their owner) to remove them.

```