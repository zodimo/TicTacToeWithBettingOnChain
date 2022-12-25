import { execSync } from "child_process";
import { Address } from "./address.js";
import { CardanoCliBin } from "./cardano-cli-bin.js";

interface AddressBuilder {
  (address: Address): Address;
}

export interface BuilderInterface {
  toString(): string;
}

export class Builder {
  private command?: string;
  private commandParams?: BuilderInterface;

  constructor(private bin: CardanoCliBin) {}

  address: (address: AddressBuilder) => void = (callback) => {
    this.ensureCommandNotYetSet();
    let address = new Address();
    this.command = "address";
    this.commandParams = callback(address);
  };

  ensureCommandNotYetSet() {
    if (this.command) {
      const className = this.constructor.name;
      throw new Error(`${className}: Command is aleady set!`);
    }
  }

  getCommand(): string {
    if (!this.command || !this.commandParams) {
      const className = this.constructor.name;
      throw new Error(`${className}: Command not yet set!`);
    }

    return `${this.bin.toString()} ${this.command} ${this.commandParams.toString()}`;
  }

  public runCommand(): string {
    const formattedCommand = this.getCommand();
    return execSync(formattedCommand).toString().trim();
  }
}
