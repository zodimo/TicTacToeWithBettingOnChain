```text
Usage: cardano-cli transaction submit 
            [ --shelley-mode
            | --byron-mode [--epoch-slots NATURAL]
            | --cardano-mode [--epoch-slots NATURAL]
            ]
            (--mainnet | --testnet-magic NATURAL)
            --tx-file FILE

  Submit a transaction to the local node whose Unix domain socket is obtained
  from the CARDANO_NODE_SOCKET_PATH environment variable.

Available options:
  --shelley-mode           For talking to a node running in Shelley-only mode.
  --byron-mode             For talking to a node running in Byron-only mode.
  --epoch-slots NATURAL    The number of slots per epoch for the Byron era.
                           (default: 21600)
  --cardano-mode           For talking to a node running in full Cardano mode
                           (default).
  --epoch-slots NATURAL    The number of slots per epoch for the Byron era.
                           (default: 21600)
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Specify a testnet magic id.
  --tx-file FILE           Filepath of the transaction you intend to submit.
  -h,--help                Show this help text
```