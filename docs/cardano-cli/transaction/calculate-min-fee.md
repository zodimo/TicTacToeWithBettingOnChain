```text
Usage: cardano-cli transaction calculate-min-fee --tx-body-file FILE
            [--mainnet | --testnet-magic NATURAL]
            (--genesis FILE | --protocol-params-file FILE)
            --tx-in-count NATURAL
            --tx-out-count NATURAL
            --witness-count NATURAL
            [--byron-witness-count NATURAL]

  Calculate the minimum fee for a transaction.

Available options:
  --tx-body-file FILE      Input filepath of the JSON TxBody.
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Specify a testnet magic id.
  --genesis FILE           [TESTING] The genesis file to take initial protocol
                           parameters from. For test clusters only, since the
                           parameters are going to be obsolete for production
                           clusters.
  --protocol-params-file FILE
                           Filepath of the JSON-encoded protocol parameters file
  --tx-in-count NATURAL    The number of transaction inputs.
  --tx-out-count NATURAL   The number of transaction outputs.
  --witness-count NATURAL  The number of Shelley key witnesses.
  --byron-witness-count NATURAL
                           The number of Byron key witnesses (default is 0).
  -h,--help                Show this help text
```