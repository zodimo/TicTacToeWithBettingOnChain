import { CommandParameter } from "../../command-parameter.js";

export class Assertion extends CommandParameter {
  private constructor(private paramKey: string) {
    super();
  }

  scriptValid(): Assertion {
    return new Assertion("script-valid");
  }

  scriptInvalid(): Assertion {
    return new Assertion("script-invalid");
  }

  asParameter(): string {
    return `--${this.paramKey}`;
  }
}
