import { CommandParameter } from "../../command-parameter.js";

export class SigningKeyFile extends CommandParameter {
    constructor(private filename: string, private address?: string) {
      super();
    }
    asParameter(): string {
      const output: string[] = [`--signing-key-file ${this.filename}`];
      if (this.address) {
        output.push(`--address ${this.address}`);
      }
      return output.join(" ");
    }
  }
  