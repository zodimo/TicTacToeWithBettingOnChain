import assert from "assert";
import {
  Data,
  DataArray,
  DataBytes,
  DataConstr,
  DataNumber,
  FromScriptDataFactory,
  ToScriptDataSerialise,
} from "../cardano-cli/script-data.js";

export enum Row {
  ROW_A = "A", //constuctor 0
  ROW_B = "B", //constuctor 1
  ROW_C = "C", //constuctor 2
}

export enum Column {
  Col_1 = 1, //constuctor 0
  Col_2 = 2, //constuctor 1
  Col_3 = 3, //constuctor 2
}

export class Move implements ToScriptDataSerialise {
  constructor(public readonly row: Row, public readonly column: Column) {}
  equals(move: Move): boolean {
    return this.column == move.column && this.row == move.row;
  }
  toScriptData(): Data {
    const rowToData: (row: Row) => Data = (row) => {
      switch (row) {
        case Row.ROW_A:
          return DataConstr.from(0, []);
        case Row.ROW_B:
          return DataConstr.from(1, []);
        case Row.ROW_C:
          return DataConstr.from(2, []);
        default:
          throw new Error(`Unsupported Row: ${row} `);
      }
    };

    const columnToData: (column: Column) => Data = (column) => {
      switch (column) {
        case Column.Col_1:
          return DataConstr.from(0, []);
        case Column.Col_2:
          return DataConstr.from(1, []);
        case Column.Col_3:
          return DataConstr.from(2, []);
        default:
          throw new Error(`Unsuppord Row: ${column} `);
      }
    };

    return DataConstr.from(0, [rowToData(this.row), columnToData(this.column)]);
  }
}

export class Moves implements ToScriptDataSerialise {
  constructor(public readonly movesMade: MoveMade[]) {}

  static initialise(): Moves {
    return new Moves([]);
  }

  makeMove(playerAddress: string, move: Move): Moves {
    if (this.hasMoveBeenMade(move)) {
      throw new Error("Illegal move: position is occupied.");
    }
    const moves = this.movesMade;
    moves.push(new MoveMade(playerAddress, move));
    return new Moves(moves);
  }

  hasMoveBeenMade(move: Move): boolean {
    return !!this.movesMade.find((moveMade) => {
      return moveMade.move.equals(move);
    });
  }

  getMoveMadeAtPosition(row: Row, column: Column): MoveMade | null {
    const move = new Move(row, column);
    const moveMade = this.movesMade.find((moveMade) => {
      if (moveMade.move.equals(move)) {
        return moveMade;
      }
    });
    return moveMade ?? null;
  }

  toScriptData(): Data {
    const movesMadeToData: (movesMade: MoveMade[]) => Data = (movesMade) => {
      return DataArray.fromArray(movesMade.map((moveMade) => moveMade.toScriptData()));
    };
    return DataConstr.from(0, [movesMadeToData(this.movesMade)]);
  }
}

export class MoveMade implements ToScriptDataSerialise {
  constructor(public readonly playerAddress: string, public readonly move: Move) {}
  toScriptData(): Data {
    return DataConstr.from(0, [DataBytes.fromString(this.playerAddress), this.move.toScriptData()]);
  }
}

enum GameStateConstuctors {
  GameInitiated = 0,
  GameStarted = 1,
  GameInProgress = 2,
  GameIsWon = 3,
  GameIsTied = 4,
}

export class GameInitiated implements ToScriptDataSerialise {
  constructor(
    public readonly playerOneAddress: string,
    public readonly betInAda: number,
    public readonly gameMaxIntervalInMins: number
  ) {}
  toScriptData(): Data {
    return DataConstr.from(GameStateConstuctors.GameInitiated, [
      DataBytes.fromString(this.playerOneAddress),
      DataNumber.fromNumber(this.betInAda),
      DataNumber.fromNumber(this.gameMaxIntervalInMins),
    ]);
  }
}

export class GameStarted implements ToScriptDataSerialise {
  constructor(
    public readonly playerOneAddress: string,
    public readonly playerTwoAddress: string,
    public readonly betInAda: number,
    public readonly gameMaxIntervalInMins: number,
    public readonly playerAddressToMakeMove: string
  ) {}
  toScriptData(): Data {
    return DataConstr.from(GameStateConstuctors.GameStarted, [
      DataBytes.fromString(this.playerOneAddress),
      DataBytes.fromString(this.playerTwoAddress),
      DataNumber.fromNumber(this.betInAda),
      DataNumber.fromNumber(this.gameMaxIntervalInMins),
      DataBytes.fromString(this.playerAddressToMakeMove),
    ]);
  }
}

export class GameInProgress implements ToScriptDataSerialise {
  constructor(
    public readonly playerOneAddress: string,
    public readonly playerTwoAddress: string,
    public readonly betInAda: number,
    public readonly gameMaxIntervalInMins: number,
    public readonly playerAddressToMakeMove: string,
    public readonly moves: Moves
  ) {}

  toScriptData(): Data {
    return DataConstr.from(GameStateConstuctors.GameInProgress, [
      DataBytes.fromString(this.playerOneAddress),
      DataBytes.fromString(this.playerTwoAddress),
      DataNumber.fromNumber(this.betInAda),
      DataNumber.fromNumber(this.gameMaxIntervalInMins),
      DataBytes.fromString(this.playerAddressToMakeMove),
      this.moves.toScriptData(),
    ]);
  }
}

export class GameIsWon implements ToScriptDataSerialise {
  constructor(
    public readonly playerOneAddress: string,
    public readonly playerTwoAddress: string,
    public readonly betInAda: number,
    public readonly gameMaxIntervalInMins: number,
    public readonly winningPlayerAddress: string,
    public readonly moves: Moves
  ) {}

  toScriptData(): Data {
    return DataConstr.from(GameStateConstuctors.GameIsWon, [
      DataBytes.fromString(this.playerOneAddress),
      DataBytes.fromString(this.playerTwoAddress),
      DataNumber.fromNumber(this.betInAda),
      DataNumber.fromNumber(this.gameMaxIntervalInMins),
      DataBytes.fromString(this.winningPlayerAddress),
      this.moves.toScriptData(),
    ]);
  }
}

export class GameIsTied implements ToScriptDataSerialise {
  constructor(
    public readonly playerOneAddress: string,
    public readonly playerTwoAddress: string,
    public readonly betInAda: number,
    public readonly moves: Moves
  ) {}

  toScriptData(): Data {
    return DataConstr.from(GameStateConstuctors.GameIsTied, [
      DataBytes.fromString(this.playerOneAddress),
      DataBytes.fromString(this.playerTwoAddress),
      DataNumber.fromNumber(this.betInAda),
      this.moves.toScriptData(),
    ]);
  }
}

export type GameState = GameInitiated | GameStarted | GameInProgress | GameIsWon | GameIsTied;

///////////////////////////////////////
//
//
//     Game Data Factories
//
//
///////////////////////////////////////

export class RowFactory extends FromScriptDataFactory<Row> {
  fromScriptData(data: DataConstr): Row {
    assert.equal(data.getFields().length, 0, `Row: Extects 0 fields got ${data.getFields().length}`);
    switch (data.getIndex()) {
      case 0:
        return Row.ROW_A;
      case 1:
        return Row.ROW_B;
      case 2:
        return Row.ROW_C;

      default:
        throw new Error(`Unknown data constructor: ${data.getIndex()}`);
    }
  }
}

export class ColumnFactory extends FromScriptDataFactory<Column> {
  fromScriptData(data: DataConstr): Column {
    assert.equal(data.getFields().length, 0, `Column: Extects 0 fields got ${data.getFields().length}`);
    switch (data.getIndex()) {
      case 0:
        return Column.Col_1;
      case 1:
        return Column.Col_2;
      case 2:
        return Column.Col_3;

      default:
        throw new Error(`Unknown data constructor: ${data.getIndex()}`);
    }
  }
}

export class MoveFactory extends FromScriptDataFactory<Move> {
  fromScriptData(data: DataConstr): Move {
    assert.equal(data.getIndex(), 0, `Move: Extects constructor 0 got ${data.getIndex()}`);
    assert.equal(data.getFields().length, 2, `Move: Extects 2 fields got ${data.getFields().length}`);

    assert.equal(data.getFields()[0] instanceof DataConstr, true);
    const rowData: DataConstr = data.getFields()[0] as DataConstr;

    assert.equal(data.getFields()[0] instanceof DataConstr, true);
    const columnData: DataConstr = data.getFields()[1] as DataConstr;

    return new Move(new RowFactory().fromScriptData(rowData), new ColumnFactory().fromScriptData(columnData));
  }
}

export class MoveMadeFactory extends FromScriptDataFactory<MoveMade> {
  fromScriptData(data: Data): MoveMade {
    assert.equal(
      data instanceof DataConstr,
      true,
      `Expects Data to be of type DataConstr got ${data.constructor.name}`
    );
    const validData: DataConstr = data as DataConstr;

    assert.equal(validData.getIndex(), 0, `MoveMade: Extects constructor 0 got ${validData.getIndex()}`);
    assert.equal(validData.getFields().length, 2, `MoveMade: Extects 2 fields got ${validData.getFields().length}`);
    assert.equal(validData.getFields()[0] instanceof DataBytes, true);
    const playerAddressData: DataBytes = validData.getFields()[0] as DataBytes;

    assert.equal(validData.getFields()[1] instanceof DataConstr, true);
    const moveData: DataConstr = validData.getFields()[1] as DataConstr;

    return new MoveMade(playerAddressData.toString(), new MoveFactory().fromScriptData(moveData));
  }
}

export class MovesFactory extends FromScriptDataFactory<Moves> {
  fromScriptData(data: DataConstr): Moves {
    assert.equal(data.getFields().length, 1, `Moves: Extects 1 field got ${data.getFields().length}`);
    assert.equal(data.getFields()[0] instanceof DataArray, true);
    const movesMadeData: DataArray = data.getFields()[0] as DataArray;
    const movesMade: MoveMade[] = [];
    movesMadeData.getArray().forEach((moveMadeData) => {
      const moveMadeFactory = new MoveMadeFactory();
      movesMade.push(moveMadeFactory.fromScriptData(moveMadeData));
    });
    return new Moves(movesMade);
  }
}

export class GameStateFactory extends FromScriptDataFactory<GameState> {
  fromScriptData(data: Data): GameState {
    assert.equal(
      data instanceof DataConstr,
      true,
      `Expects Data to be of type DataConstr got ${data.constructor.name}`
    );
    const validData: DataConstr = data as DataConstr;
    switch (validData.getIndex()) {
      case GameStateConstuctors.GameInitiated:
        return this.createGameInitiated(validData);

      case GameStateConstuctors.GameStarted:
        return this.createGameStarted(validData);

      case GameStateConstuctors.GameInProgress:
        return this.createGameInProgress(validData);

      case GameStateConstuctors.GameIsWon:
        return this.createGameIsWon(validData);
      case GameStateConstuctors.GameIsTied:
        return this.createGameIsTied(validData);

      default:
        throw new Error(`Unknown data constructor: ${validData.getIndex()}`);
    }
  }

  createGameInitiated(data: DataConstr): GameInitiated {
    assert.equal(data.getFields().length, 3, `GameInitiated: Extects 3 fields got ${data.getFields().length}`);
    assert.equal(data.getFields()[0] instanceof DataBytes, true);
    const playerOneAddressData: DataBytes = data.getFields()[0] as DataBytes;
    assert.equal(data.getFields()[1] instanceof DataNumber, true);
    const betInAdaData: DataNumber = data.getFields()[1] as DataNumber;
    assert.equal(data.getFields()[2] instanceof DataNumber, true);
    const gameMaxIntervalInMinsData: DataNumber = data.getFields()[2] as DataNumber;

    return new GameInitiated(
      playerOneAddressData.toString(),
      betInAdaData.getValue(),
      gameMaxIntervalInMinsData.getValue()
    );
  }

  createGameStarted(data: DataConstr): GameStarted {
    assert.equal(data.getFields().length, 5, `GameStarted: Extects 5 fields got ${data.getFields().length}`);
    assert.equal(data.getFields()[0] instanceof DataBytes, true);
    const playerOneAddressData: DataBytes = data.getFields()[0] as DataBytes;
    assert.equal(data.getFields()[1] instanceof DataBytes, true);
    const playerTwoAddressData: DataBytes = data.getFields()[1] as DataBytes;
    assert.equal(data.getFields()[2] instanceof DataNumber, true);
    const betInAdaData: DataNumber = data.getFields()[2] as DataNumber;
    assert.equal(data.getFields()[3] instanceof DataNumber, true);
    const gameMaxIntervalInMinsData: DataNumber = data.getFields()[3] as DataNumber;
    assert.equal(data.getFields()[4] instanceof DataBytes, true);
    const playerAddressToMakeMoveData: DataBytes = data.getFields()[4] as DataBytes;

    return new GameStarted(
      playerOneAddressData.toString(),
      playerTwoAddressData.toString(),
      betInAdaData.getValue(),
      gameMaxIntervalInMinsData.getValue(),
      playerAddressToMakeMoveData.toString()
    );
  }

  createGameInProgress(data: DataConstr): GameInProgress {
    assert.equal(data.getFields().length, 6, `GameInProgress: Extects 6 fields got ${data.getFields().length}`);
    assert.equal(data.getFields()[0] instanceof DataBytes, true);
    const playerOneAddressData: DataBytes = data.getFields()[0] as DataBytes;
    assert.equal(data.getFields()[1] instanceof DataBytes, true);
    const playerTwoAddressData: DataBytes = data.getFields()[1] as DataBytes;
    assert.equal(data.getFields()[2] instanceof DataNumber, true);
    const betInAdaData: DataNumber = data.getFields()[2] as DataNumber;
    assert.equal(data.getFields()[3] instanceof DataNumber, true);
    const gameMaxIntervalInMinsData: DataNumber = data.getFields()[3] as DataNumber;
    assert.equal(data.getFields()[4] instanceof DataBytes, true);
    const playerAddressToMakeMoveData: DataBytes = data.getFields()[4] as DataBytes;

    assert.equal(
      data.getFields()[5] instanceof DataConstr,
      true,
      `Expected DataConstr, got ${data.getFields()[5].constructor.name}`
    );
    const movesData: DataConstr = data.getFields()[5] as DataConstr;

    return new GameInProgress(
      playerOneAddressData.toString(),
      playerTwoAddressData.toString(),
      betInAdaData.getValue(),
      gameMaxIntervalInMinsData.getValue(),
      playerAddressToMakeMoveData.toString(),
      new MovesFactory().fromScriptData(movesData)
    );
  }

  createGameIsWon(data: DataConstr): GameState {
    assert.equal(data.getFields().length, 6, `GameInProgress: Extects 6 fields got ${data.getFields().length}`);
    assert.equal(data.getFields()[0] instanceof DataBytes, true);
    const playerOneAddressData: DataBytes = data.getFields()[0] as DataBytes;
    assert.equal(data.getFields()[1] instanceof DataBytes, true);
    const playerTwoAddressData: DataBytes = data.getFields()[1] as DataBytes;
    assert.equal(data.getFields()[2] instanceof DataNumber, true);
    const betInAdaData: DataNumber = data.getFields()[2] as DataNumber;
    assert.equal(data.getFields()[3] instanceof DataNumber, true);
    const gameMaxIntervalInMinsData: DataNumber = data.getFields()[3] as DataNumber;
    assert.equal(data.getFields()[4] instanceof DataBytes, true);
    const winningPlayerAddressData: DataBytes = data.getFields()[4] as DataBytes;

    assert.equal(
      data.getFields()[5] instanceof DataConstr,
      true,
      `Expected DataConstr, got ${data.getFields()[5].constructor.name}`
    );
    const movesData: DataConstr = data.getFields()[5] as DataConstr;

    return new GameIsWon(
      playerOneAddressData.toString(),
      playerTwoAddressData.toString(),
      betInAdaData.getValue(),
      gameMaxIntervalInMinsData.getValue(),
      winningPlayerAddressData.toString(),
      new MovesFactory().fromScriptData(movesData)
    );
  }

  createGameIsTied(data: DataConstr): GameIsTied {
    assert.equal(data.getFields().length, 4, `GameInProgress: Extects 4 fields got ${data.getFields().length}`);
    assert.equal(data.getFields()[0] instanceof DataBytes, true);
    const playerOneAddressData: DataBytes = data.getFields()[0] as DataBytes;
    assert.equal(data.getFields()[1] instanceof DataBytes, true);
    const playerTwoAddressData: DataBytes = data.getFields()[1] as DataBytes;
    assert.equal(data.getFields()[2] instanceof DataNumber, true);
    const betInAdaData: DataNumber = data.getFields()[2] as DataNumber;

    assert.equal(
      data.getFields()[3] instanceof DataConstr,
      true,
      `Expected DataConstr, got ${data.getFields()[3].constructor.name}`
    );
    const movesData: DataConstr = data.getFields()[3] as DataConstr;

    return new GameIsTied(
      playerOneAddressData.toString(),
      playerTwoAddressData.toString(),
      betInAdaData.getValue(),
      new MovesFactory().fromScriptData(movesData)
    );
  }
}