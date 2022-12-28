import { config } from "../../../cardano-cli-config.js";
import { Command } from "../command.js";
import { Network } from "../network.js";
import { OutFile } from "../shared/out-file.js";
import { SigningKeyFile } from "./sign/signing-key-file.js";
import { TxToSign } from "./sign/tx-to-sign.js";

/*
Usage: cardano-cli transaction sign (--tx-body-file FILE | --tx-file FILE)
            (--signing-key-file FILE [--address STRING])
            [--mainnet | --testnet-magic NATURAL]
            --out-file FILE

*/

export class Sign extends Command {
  private network?: Network;
  private outFile?: OutFile;
  private signingKeyFiles: SigningKeyFile[];
  private txToSign?: TxToSign;

  constructor(private commandPrefix: string) {
    super();
    this.network = config.getNetwork();
    this.signingKeyFiles = [];
  }

  withTxToSign(txToSign: TxToSign): Sign {
    this.txToSign = txToSign;
    return this;
  }

  withSigningKeyFile(signingKeyFile: SigningKeyFile): Sign {
    this.signingKeyFiles.push(signingKeyFile);
    return this;
  }

  withNetwork(network: Network): Sign {
    this.network = network;
    return this;
  }

  withOutFile(outFile: OutFile): Sign {
    this.outFile = outFile;
    return this;
  }

  getCommand(): string {
    let output: string[] = [this.commandPrefix, "sign"];
    if (this.txToSign) {
      output.push(this.txToSign.asParameter());
    }
    this.signingKeyFiles.map((signingKeyFile) => {
      output.push(signingKeyFile.asParameter());
    });

    if (this.network) {
      output.push(this.network.asParameter());
    }
    if (this.outFile) {
      output.push(this.outFile.asParameter());
    }
    return output.join(" ");
  }
}
