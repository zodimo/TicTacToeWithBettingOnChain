import { CommandParameter } from "./command-parameter.js";

export class Era extends CommandParameter {
  private constructor(private era: string) {
    super();
  }

  static byron(): Era {
    return new Era("byron");
  }

  static shelly(): Era {
    return new Era("shelly");
  }

  static allegra(): Era {
    return new Era("allegra");
  }

  static mary(): Era {
    return new Era("mary");
  }

  static alonzo(): Era {
    return new Era("mary");
  }
  static babbage(): Era {
    return new Era("babbage");
  }

  toString(): string {
    return this.era;
  }

  asParameter(): string {
    return `--${this.era}-era`;
  }
}
