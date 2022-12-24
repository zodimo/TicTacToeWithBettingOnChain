```text
Usage: cardano-cli address build 
            ( --payment-verification-key STRING
            | --payment-verification-key-file FILE
            | --payment-script-file FILE
            )
            [ --stake-verification-key STRING
            | --stake-verification-key-file FILE
            | --stake-script-file FILE
            ]
            (--mainnet | --testnet-magic NATURAL)
            [--out-file FILE]

  Build a Shelley payment address, with optional delegation to a stake address.

Available options:
  --payment-verification-key STRING
                           Payment verification key (Bech32-encoded)
  --payment-verification-key-file FILE
                           Filepath of the payment verification key.
  --payment-script-file FILE
                           Filepath of the payment script.
  --stake-verification-key STRING
                           Stake verification key (Bech32 or hex-encoded).
  --stake-verification-key-file FILE
                           Filepath of the staking verification key.
  --stake-script-file FILE Filepath of the staking script.
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Specify a testnet magic id.
  --out-file FILE          Optional output file. Default is to write to stdout.
  -h,--help                Show this help text
```