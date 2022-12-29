import { CommandParameter } from "./command-parameter.js";

export class NodeMode extends CommandParameter {
  private constructor(private mode: string, private epochSlots?: number) {
    super();
  }

  static shelley(): NodeMode {
    return new NodeMode("shelley");
  }

  static byron(epochSlots?: number): NodeMode {
    return new NodeMode("byron", epochSlots);
  }
  static cardano(epochSlots?: number): NodeMode {
    return new NodeMode("cardano", epochSlots);
  }
  toString(): string {
    return this.mode;
  }

  asParameter(): string {
    const output: string[] = [`--${this.mode}-mode`];
    if (this.epochSlots) {
      output.push(`--epoch-slots ${this.epochSlots}`);
    }
    return output.join(" ");
  }
}
