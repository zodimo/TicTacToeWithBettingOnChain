import { SigningKeyFile } from "../command/transaction/sign/signing-key-file.js";
import { TxToSign } from "../command/transaction/sign/tx-to-sign.js";

export class TransactionSignOptions {
  constructor(
    public readonly txToSign: TxToSign,
    public readonly signingKeyFiles: SigningKeyFile[]
  ) {}
}
