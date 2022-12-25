import { Info } from "./address/info.js";
import { BuilderInterface } from "./builder.js";

interface KeyGenBuilder {
  (address: Address): string;
}
interface KeyGenBuilder {
  (address: Address): string;
}
interface InfoBuilder {
  (info: Info): Info;
}

export class Address {
  private command?: BuilderInterface;

  constructor() {}
  keyGen(): string {
    throw new Error("Not Implemented!");
  }
  keyHash(): string {
    throw new Error("Not Implemented!");
  }
  build(): string {
    throw new Error("Not Implemented!");
  }
  info: (callback: InfoBuilder) => void = (callback): void => {
    const info = new Info();
    this.command = callback(info);
  };

  toString(): string {
    if (!this.command) {
      const className = this.constructor.name;
      throw new Error(`${className}: Command not yet set!`);
    }

    return this.command.toString();
  }
}
