import { BuilderInterface } from "../builder.js";

/*
Available options:
  --address ADDRESS        A Cardano address
  --out-file FILE          Optional output file. Default is to write to stdout.
  -h,--help                Show this help text
*/

export class Info implements BuilderInterface {
  private _address?: string;
  private _outFile?: string;

  constructor() {}

  address(address: string): void {
    this._address = address;
  }

  outFile(outFile: string): void {
    this._outFile = outFile;
  }

  toString(): string {
    const className = this.constructor.name;

    if (!this._address) {
      throw new Error(`${className}: address is not yet set!`);
    }
    let output = `--address ${this._address}`;
    if (this._outFile) {
      output += ` --out-file ${this._outFile}`;
    }

    return `info ${output}`;
  }
}
