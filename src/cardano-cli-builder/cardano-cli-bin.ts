import fs from "fs";

export class CardanoCliBin {
  constructor(private bin: string) {}
  ensureBinExists(): void {
    if (fs.existsSync(this.bin)) {
      const className = this.constructor.name;
      throw new Error(`${className}: Bin file do not exist: ${this.bin}`);
    }
  }

  toString(): string {
    return this.bin;
  }
}
