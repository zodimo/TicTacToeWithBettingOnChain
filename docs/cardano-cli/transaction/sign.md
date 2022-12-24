```text
Usage: cardano-cli transaction sign (--tx-body-file FILE | --tx-file FILE)
            (--signing-key-file FILE [--address STRING])
            [--mainnet | --testnet-magic NATURAL]
            --out-file FILE

  Sign a transaction

Available options:
  --tx-body-file FILE      Input filepath of the JSON TxBody.
  --tx-file FILE           Input filepath of the JSON Tx.
  --signing-key-file FILE  Input filepath of the signing key (one or more).
  --address STRING         Byron address (Base58-encoded).
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Specify a testnet magic id.
  --out-file FILE          Output filepath of the JSON Tx.
  -h,--help                Show this help text
```