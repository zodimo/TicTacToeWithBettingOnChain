import { CommandParameter } from "../../command-parameter.js";

export class RequiredSigner extends CommandParameter {
    private constructor(private paramKey: string, private paramValue: string) {
      super();
    }
  
    static file(value: string): RequiredSigner {
      //  --required-signer FILE
      const param = "required-signer";
      return new RequiredSigner(param, value);
    }
  
    static hash(value: string): RequiredSigner {
      //  --required-signer-hash HASH
      const param = "required-signer-hash";
      return new RequiredSigner(param, value);
    }
  
    asParameter(): string {
      return `--${this.paramKey} ${this.paramValue}`;
    }
  }