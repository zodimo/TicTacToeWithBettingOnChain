# TX-IN

```text
Available options:
  --tx-in TX-IN            TxId#TxIx
  --tx-in-script-file FILE The file containing the script to witness the
                           spending of the transaction input.
  --tx-in-datum-cbor-file CBOR FILE
                           The script datum, in the given JSON file. The file
                           must follow the special JSON schema for script data.
  --tx-in-datum-file JSON FILE
                           The script datum, in the given JSON file. The file
                           must follow the special JSON schema for script data.
  --tx-in-datum-value JSON VALUE
                           The script datum, in JSON syntax. There is no schema:
                           (almost) any JSON value is supported, including
                           top-level strings and numbers.
  --tx-in-inline-datum-present
                           Inline datum present at transaction input.
  --tx-in-redeemer-cbor-file CBOR FILE
                           The script redeemer, in the given JSON file. The file
                           must follow the special JSON schema for script data.
  --tx-in-redeemer-file JSON FILE
                           The script redeemer, in the given JSON file. The file
                           must follow the special JSON schema for script data.
  --tx-in-redeemer-value JSON VALUE
                           The script redeemer, in JSON syntax. There is no
                           schema: (almost) any JSON value is supported,
                           including top-level strings and numbers.
  --tx-in-execution-units (INT, INT)
                           The time and space units needed by the script.
  --read-only-tx-in-reference TX-IN
                           Specify a read only reference input. This reference
                           input is not witnessing anything it is simply
                           provided in the plutus script context.
  --tx-in-collateral TX-IN TxId#TxIx
```

extracted text from cardano-cli transaction build-raw -h
```text
            (
              --tx-in TX-IN
              [                   
                --spending-tx-in-reference TX-IN
                --spending-plutus-script-v2
                (   
                  --spending-reference-tx-in-datum-cbor-file CBOR FILE
                  | --spending-reference-tx-in-datum-file JSON FILE
                  | --spending-reference-tx-in-datum-value JSON VALUE
                  | --spending-reference-tx-in-inline-datum-present
                )
                (   
                  --spending-reference-tx-in-redeemer-cbor-file CBOR FILE
                  | --spending-reference-tx-in-redeemer-file JSON FILE
                  | --spending-reference-tx-in-redeemer-value JSON VALUE
                )
                --spending-reference-tx-in-execution-units (INT, INT)
                | --simple-script-tx-in-reference TX-IN
                | --tx-in-script-file FILE
                [
                  (   
                    --tx-in-datum-cbor-file CBOR FILE
                    | --tx-in-datum-file JSON FILE
                    | --tx-in-datum-value JSON VALUE
                    | --tx-in-inline-datum-present
                  )
                  (   
                    --tx-in-redeemer-cbor-file CBOR FILE
                    | --tx-in-redeemer-file JSON FILE
                    | --tx-in-redeemer-value JSON VALUE
                  )
                  --tx-in-execution-units (INT, INT)
                ]
              ]
            )

```