export abstract class Options {
  isset(value: any): boolean {
    return value !== null && value !== undefined;
  }
}
