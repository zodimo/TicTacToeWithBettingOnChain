export class Era {
  private constructor(private era: string) {}

  byron(): Era {
    return new Era("byron");
  }

  shelly(): Era {
    return new Era("shelly");
  }

  allegra(): Era {
    return new Era("allegra");
  }

  mary(): Era {
    return new Era("mary");
  }

  alonzo(): Era {
    return new Era("mary");
  }
  babbage(): Era {
    return new Era("babbage");
  }

  toString(): string {
    return this.era;
  }

  asParameter(): string {
    return `--${this.era}-era`;
  }
}
