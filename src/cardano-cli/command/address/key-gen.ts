/*
Usage: cardano-cli address key-gen [--normal-key | --extended-key | --byron-key]
            --verification-key-file FILE
            --signing-key-file FILE
*/

import { Command } from "../command.js";
import { CommandParameter } from "../command-parameter.js";

class KeyType extends CommandParameter {
  private constructor(private paramKey: string) {
    super();
  }

  static normal(value: string): KeyType {
    // --normal-key
    const param = "normal-key";
    return new KeyType(param);
  }
  static extended(value: string): KeyType {
    // --extended-key
    const param = "extended-key";
    return new KeyType(param);
  }
  static byron(value: string): KeyType {
    // --byron-key
    const param = "byron-key";
    return new KeyType(param);
  }

  asParameter(): string {
    return `--${this.paramKey}`;
  }
}

export class KeyGen extends Command {
  private keyType?: KeyType;
  private verificationKeyFile?: string;
  private signingKeyFile?: string;
  constructor(private commandPrefix: string) {
    super();
  }

  withKeyType(keyType: KeyType): KeyGen {
    this.keyType = keyType;
    return this;
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
    if (this.keyType) {
      ouput.push(this.keyType.asParameter());
    }
    if (this.verificationKeyFile) {
      ouput.push(`--verification-key-file ${this.verificationKeyFile}`);
    }
    if (this.signingKeyFile) {
      ouput.push(`--signing-key-file ${this.signingKeyFile}`);
    }

    return ouput.join(" ");
  }
}
