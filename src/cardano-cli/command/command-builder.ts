export interface CommandBuilder<T> {
    (tip: T): T;
  }