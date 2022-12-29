import { Command } from "./command.js";
import { CommandBuilder } from "./command-builder.js";
import { KeyGen } from "./stake-address/key-gen.js";
import { Build } from "./stake-address/build.js";
import { KeyHash } from "./stake-address/key-hash.js";
import { RegistrationCertificate } from "./stake-address/registration-certificate.js";
import { DeregistrationCertificate } from "./stake-address/deregistration-certificate.js";
import { DelegationCertificate } from "./stake-address/delegation-certificate.js";

/*
Usage: cardano-cli stake-address 
            ( key-gen
            | build
            | key-hash
            | registration-certificate
            | deregistration-certificate
            | delegation-certificate
            )
*/
export class StakeAddress extends Command {
  constructor(private commandPrefix: string) {
    super();
  }
  getCommand(): string {
    return `${this.commandPrefix} query`;
  }

  keyGen(builder?: CommandBuilder<KeyGen>): KeyGen {
    let command = new KeyGen(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }

  build(builder?: CommandBuilder<Build>): Build {
    let command = new Build(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }

  keyHash(
    builder?: CommandBuilder<KeyHash>
  ): KeyHash {
    let command = new KeyHash(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }

  registrationCertificate(
    builder?: CommandBuilder<RegistrationCertificate>
  ): RegistrationCertificate {
    let command = new RegistrationCertificate(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }
  deregistrationCertificate(
    builder?: CommandBuilder<DeregistrationCertificate>
  ): DeregistrationCertificate {
    let command = new DeregistrationCertificate(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }

  delegationCertificate(
    builder?: CommandBuilder<DelegationCertificate>
  ): DelegationCertificate {
    let command = new DelegationCertificate(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }

}
