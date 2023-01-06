import { runCommand } from "../run-command.js";

export abstract class Command {
  abstract getCommand(): string;

  runCommand(): string {
    return runCommand(this.getCommand());
  }

  isset(value: any): boolean {
    return value !== null && value !== undefined;
  }
}
