import { Command } from "../command.js";

/*
Usage: cardano-cli stake-address key-gen --verification-key-file FILE
            --signing-key-file FILE
*/

export class KeyGen extends Command {
  private verificationKeyFile?: string;
  private signingKeyFile?: string;
  constructor(private commandPrefix: string) {
    super();
  }

  withVerificationKeyFile(verificationKeyFile: string): KeyGen {
    this.verificationKeyFile = verificationKeyFile;
    return this;
  }
  withSigningKeyFile(signingKeyFile: string): KeyGen {
    this.signingKeyFile = signingKeyFile;
    return this;
  }
  getCommand(): string {
    let ouput: string[] = [this.commandPrefix, "key-gen"];

    if (this.verificationKeyFile) {
      ouput.push(`--verification-key-file ${this.verificationKeyFile}`);
    }
    if (this.signingKeyFile) {
      ouput.push(`--signing-key-file ${this.signingKeyFile}`);
    }

    return ouput.join(" ");
  }
}
