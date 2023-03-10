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

/**
 * REDEEMERS
 * 
PlutusTx.makeIsDataIndexed ''GameActionDatum [ ('JoinGameCommand, 0)
                                             , ('MakeMoveCommand, 1)
                                             , ('ClaimWinCommand, 2)
                                             , ('ClaimTieCommand, 3)
                                             , ('CancelInitiatedGameCommand, 4)
                                             , ('CancelInProgressGameCommand, 5)
                                             ]
 */

// Commands

export enum GameAction {
  START_GAME = "start-game",
  JOIN_GAME = "join-game",
  MAKE_MOVE = "make-move",
}

export enum GameEndAction {
  CANCEL_INITIATED_GAME = "cancel-initiated-game",
  CANCEL_IN_PROGRESS_GAME = "cancel-in-progress-game",
  CLAIM_WIN = "claim-win",
  CLAIM_TIE = "claim-tie",
}

export class PubKeyHash implements ToScriptDataSerialise {
  private constructor(private bytes: Uint8Array) {}

  static fromHexString(value: string): PubKeyHash {
    if (!(value.length % 2 === 0 && /^[0-9A-F]*$/i.test(value))) {
      throw new Error(`Not a valid hex value: ${value}`);
    }
    const bytes = Buffer.from(value, "hex");
    return new PubKeyHash(bytes);
  }

  toHexString(): string {
    return Buffer.from(this.bytes).toString("hex");
  }

  toScriptData(): Data {
    return DataBytes.fromBytes(this.bytes);
  }

  equals(pubkeyHash: PubKeyHash): boolean {
    return this.toHexString() == pubkeyHash.toHexString();
  }
}

interface GameActionCommandInterface<T, U> {
  getAction(): T;
  getParameters(): U;
}

export interface ToRedeemerScriptData {
  toRedeemerScriptData(): Data;
}

export class StartGameCommand implements GameActionCommandInterface<GameAction, StartGameParams> {
  constructor(private params: StartGameParams) {}
  getAction(): GameAction {
    return GameAction.START_GAME;
  }
  getParameters(): StartGameParams {
    return this.params;
  }
}

export class JoinGameCommand implements GameActionCommandInterface<GameAction, JoinGameParams>, ToRedeemerScriptData {
  constructor(private gameState: GameState, private params: JoinGameParams) {}

  getGameState(): GameState {
    return this.gameState;
  }
  getAction(): GameAction {
    return GameAction.JOIN_GAME;
  }
  getParameters(): JoinGameParams {
    return this.params;
  }
  toRedeemerScriptData(): Data {
    return DataConstr.from(0, [this.params.playerTwoPubKeyHash.toScriptData()]);
  }

  get betInAda(): number {
    return this.gameState.betInAda;
  }
}

export class MakeMoveCommand implements GameActionCommandInterface<GameAction, MakeMoveParams>, ToRedeemerScriptData {
  constructor(private gameState: GameState, private params: MakeMoveParams) {}
  getGameState(): GameState {
    return this.gameState;
  }
  getAction(): GameAction {
    return GameAction.MAKE_MOVE;
  }
  getParameters(): MakeMoveParams {
    return this.params;
  }
  toRedeemerScriptData(): Data {
    return DataConstr.from(1, [this.params.playerPubKeyHash.toScriptData(), this.params.move.toScriptData()]);
  }
}

export class ClaimWinCommand implements GameActionCommandInterface<GameEndAction, void>, ToRedeemerScriptData {
  constructor(private gameState: GameState) {}
  getGameState(): GameState {
    return this.gameState;
  }
  getAction(): GameEndAction {
    return GameEndAction.CLAIM_WIN;
  }
  getParameters(): void {}

  toRedeemerScriptData(): Data {
    return DataConstr.from(2, []);
  }
}

export class ClaimTieCommand implements GameActionCommandInterface<GameEndAction, void>, ToRedeemerScriptData {
  constructor(private gameState: GameState) {}
  getGameState(): GameState {
    return this.gameState;
  }
  getAction(): GameEndAction {
    return GameEndAction.CLAIM_TIE;
  }
  getParameters(): void {}

  toRedeemerScriptData(): Data {
    return DataConstr.from(3, []);
  }
}

export class CancelInitiatedGameCommand
  implements GameActionCommandInterface<GameEndAction, void>, ToRedeemerScriptData
{
  constructor(private gameState: GameState) {}
  getGameState(): GameState {
    return this.gameState;
  }
  getAction(): GameEndAction {
    return GameEndAction.CANCEL_INITIATED_GAME;
  }
  getParameters(): void {}

  toRedeemerScriptData(): Data {
    return DataConstr.from(4, []);
  }
}

export class CancelInProgressGameCommand
  implements GameActionCommandInterface<GameEndAction, void>, ToRedeemerScriptData
{
  constructor(private gameState: GameState) {}
  getGameState(): GameState {
    return this.gameState;
  }
  getAction(): GameEndAction {
    return GameEndAction.CANCEL_IN_PROGRESS_GAME;
  }
  getParameters(): void {}

  toRedeemerScriptData(): Data {
    return DataConstr.from(5, []);
  }
}

export type GameActionCommand = StartGameCommand | JoinGameCommand | MakeMoveCommand;

export type EndGameActionCommand =
  | ClaimWinCommand
  | ClaimTieCommand
  | CancelInitiatedGameCommand
  | CancelInProgressGameCommand;

export class StartGameParams {
  constructor(
    public readonly playerOnePubKeyHash: PubKeyHash,
    public readonly betInAda: number,
    public readonly gameMaxIntervalInSeconds: number
  ) {}
}

export class JoinGameParams {
  constructor(public readonly playerTwoPubKeyHash: PubKeyHash) {}
}

export class MakeMoveParams {
  constructor(public readonly playerPubKeyHash: PubKeyHash, public readonly move: Move) {}
}

/**
 * DATUMS
 */

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

export class MovesMade implements ToScriptDataSerialise {
  constructor(public readonly movesMade: MoveMade[]) {}

  static initialise(): MovesMade {
    return new MovesMade([]);
  }

  makeMove(playerPubKeyHash: PubKeyHash, move: Move): MovesMade {
    if (this.hasMoveBeenMade(move)) {
      throw new Error("Illegal move: position is occupied.");
    }
    const listOfmovesMade = this.movesMade;
    listOfmovesMade.push(new MoveMade(playerPubKeyHash, move));
    return new MovesMade(listOfmovesMade);
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
  constructor(public readonly playerPubKeyHash: PubKeyHash, public readonly move: Move) {}
  toScriptData(): Data {
    return DataConstr.from(0, [this.playerPubKeyHash.toScriptData(), this.move.toScriptData()]);
  }
}

enum GameStateConstuctors {
  GameInitiated = 0,
  GameInProgress = 1,
  GameIsWon = 2,
  GameIsTied = 3,
}

export class GameInitiated implements ToScriptDataSerialise {
  constructor(
    public readonly playerOnePubKeyHash: PubKeyHash,
    public readonly betInAda: number,
    public readonly gameMaxIntervalInSeconds: number,
    public readonly occurredAtPosixTime: number
  ) {}

  toScriptData(): Data {
    return DataConstr.from(GameStateConstuctors.GameInitiated, [
      this.playerOnePubKeyHash.toScriptData(),
      DataNumber.fromNumber(this.betInAda),
      DataNumber.fromNumber(this.gameMaxIntervalInSeconds),
      DataNumber.fromNumber(this.occurredAtPosixTime),
    ]);
  }

  expiresAtPosixTime(): number {
    const gameMaxIntervalInMillis = this.gameMaxIntervalInSeconds * 1000;
    return this.occurredAtPosixTime + gameMaxIntervalInMillis;
  }
}

export class GameInProgress implements ToScriptDataSerialise {
  constructor(
    public readonly playerOnePubKeyHash: PubKeyHash,
    public readonly playerTwoPubKeyHash: PubKeyHash,
    public readonly betInAda: number,
    public readonly gameMaxIntervalInSeconds: number,
    public readonly occurredAtPosixTime: number,
    public readonly playerAddressToMakeMove: PubKeyHash,
    public readonly movesMade: MovesMade
  ) {}

  toScriptData(): Data {
    return DataConstr.from(GameStateConstuctors.GameInProgress, [
      this.playerOnePubKeyHash.toScriptData(),
      this.playerTwoPubKeyHash.toScriptData(),
      DataNumber.fromNumber(this.betInAda),
      DataNumber.fromNumber(this.gameMaxIntervalInSeconds),
      DataNumber.fromNumber(this.occurredAtPosixTime),
      this.playerAddressToMakeMove.toScriptData(),
      this.movesMade.toScriptData(),
    ]);
  }
  expiresAtPosixTime(): number {
    const gameMaxIntervalInMillis = this.gameMaxIntervalInSeconds * 1000;
    return this.occurredAtPosixTime + gameMaxIntervalInMillis;
  }
}

export class GameIsWon implements ToScriptDataSerialise {
  constructor(
    public readonly playerOnePubKeyHash: PubKeyHash,
    public readonly playerTwoPubKeyHash: PubKeyHash,
    public readonly betInAda: number,
    public readonly gameMaxIntervalInSeconds: number,
    public readonly occurredAtPosixTime: number,
    public readonly winningPlayerPubKeyHash: PubKeyHash,
    public readonly movesMade: MovesMade
  ) {}

  toScriptData(): Data {
    return DataConstr.from(GameStateConstuctors.GameIsWon, [
      this.playerOnePubKeyHash.toScriptData(),
      this.playerTwoPubKeyHash.toScriptData(),
      DataNumber.fromNumber(this.betInAda),
      DataNumber.fromNumber(this.gameMaxIntervalInSeconds),
      DataNumber.fromNumber(this.occurredAtPosixTime),
      this.winningPlayerPubKeyHash.toScriptData(),
      this.movesMade.toScriptData(),
    ]);
  }
}

export class GameIsTied implements ToScriptDataSerialise {
  constructor(
    public readonly playerOnePubKeyHash: PubKeyHash,
    public readonly playerTwoPubKeyHash: PubKeyHash,
    public readonly betInAda: number,
    public readonly occurredAtPosixTime: number,
    public readonly movesMade: MovesMade
  ) {}

  toScriptData(): Data {
    return DataConstr.from(GameStateConstuctors.GameIsTied, [
      this.playerOnePubKeyHash.toScriptData(),
      this.playerTwoPubKeyHash.toScriptData(),
      DataNumber.fromNumber(this.betInAda),
      DataNumber.fromNumber(this.occurredAtPosixTime),
      this.movesMade.toScriptData(),
    ]);
  }
}

export type GameState = GameInitiated | GameInProgress | GameIsWon | GameIsTied;

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
    const playerPubKeyHashData: DataBytes = validData.getFields()[0] as DataBytes;

    assert.equal(validData.getFields()[1] instanceof DataConstr, true);
    const moveData: DataConstr = validData.getFields()[1] as DataConstr;

    return new MoveMade(
      PubKeyHash.fromHexString(playerPubKeyHashData.toHex()),
      new MoveFactory().fromScriptData(moveData)
    );
  }
}

export class MovesMadeFactory extends FromScriptDataFactory<MovesMade> {
  fromScriptData(data: DataConstr): MovesMade {
    assert.equal(data.getFields().length, 1, `Moves: Extects 1 field got ${data.getFields().length}`);
    assert.equal(data.getFields()[0] instanceof DataArray, true);
    const movesMadeData: DataArray = data.getFields()[0] as DataArray;
    const movesMade: MoveMade[] = [];
    movesMadeData.getArray().forEach((moveMadeData) => {
      const moveMadeFactory = new MoveMadeFactory();
      movesMade.push(moveMadeFactory.fromScriptData(moveMadeData));
    });
    return new MovesMade(movesMade);
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
    assert.equal(data.getFields().length, 4, `GameInitiated: Extects 4 fields got ${data.getFields().length}`);
    assert.equal(data.getFields()[0] instanceof DataBytes, true);
    const playerOnePubKeyHashData: DataBytes = data.getFields()[0] as DataBytes;
    assert.equal(data.getFields()[1] instanceof DataNumber, true);
    const betInAdaData: DataNumber = data.getFields()[1] as DataNumber;
    assert.equal(data.getFields()[2] instanceof DataNumber, true);
    const gameMaxIntervalInSecondsData: DataNumber = data.getFields()[2] as DataNumber;
    assert.equal(data.getFields()[3] instanceof DataNumber, true);
    const occurredAtPosixTimeData: DataNumber = data.getFields()[3] as DataNumber;

    return new GameInitiated(
      PubKeyHash.fromHexString(playerOnePubKeyHashData.toHex()),
      betInAdaData.getValue(),
      gameMaxIntervalInSecondsData.getValue(),
      occurredAtPosixTimeData.getValue()
    );
  }

  createGameInProgress(data: DataConstr): GameInProgress {
    assert.equal(data.getFields().length, 7, `GameInProgress: Extects 7 fields got ${data.getFields().length}`);
    assert.equal(data.getFields()[0] instanceof DataBytes, true);
    const playerOnePubKeyHashData: DataBytes = data.getFields()[0] as DataBytes;
    assert.equal(data.getFields()[1] instanceof DataBytes, true);
    const playerTwoPubKeyHashData: DataBytes = data.getFields()[1] as DataBytes;
    assert.equal(data.getFields()[2] instanceof DataNumber, true);
    const betInAdaData: DataNumber = data.getFields()[2] as DataNumber;
    assert.equal(data.getFields()[3] instanceof DataNumber, true);
    const gameMaxIntervalInSecondsData: DataNumber = data.getFields()[3] as DataNumber;
    assert.equal(data.getFields()[4] instanceof DataNumber, true);
    const occurredAtPosixTimeData: DataNumber = data.getFields()[4] as DataNumber;
    assert.equal(data.getFields()[5] instanceof DataBytes, true);
    const playerAddressToMakeMoveData: DataBytes = data.getFields()[5] as DataBytes;

    assert.equal(
      data.getFields()[6] instanceof DataConstr,
      true,
      `Expected DataConstr, got ${data.getFields()[6].constructor.name}`
    );
    const movesData: DataConstr = data.getFields()[6] as DataConstr;

    return new GameInProgress(
      PubKeyHash.fromHexString(playerOnePubKeyHashData.toHex()),
      PubKeyHash.fromHexString(playerTwoPubKeyHashData.toHex()),
      betInAdaData.getValue(),
      gameMaxIntervalInSecondsData.getValue(),
      occurredAtPosixTimeData.getValue(),
      PubKeyHash.fromHexString(playerAddressToMakeMoveData.toHex()),
      new MovesMadeFactory().fromScriptData(movesData)
    );
  }

  createGameIsWon(data: DataConstr): GameIsWon {
    assert.equal(data.getFields().length, 7, `GameIsWon: Extects 7 fields got ${data.getFields().length}`);
    assert.equal(data.getFields()[0] instanceof DataBytes, true);
    const playerOnePubKeyHashData: DataBytes = data.getFields()[0] as DataBytes;
    assert.equal(data.getFields()[1] instanceof DataBytes, true);
    const playerTwoPubKeyHashData: DataBytes = data.getFields()[1] as DataBytes;
    assert.equal(data.getFields()[2] instanceof DataNumber, true);
    const betInAdaData: DataNumber = data.getFields()[2] as DataNumber;
    assert.equal(data.getFields()[3] instanceof DataNumber, true);
    const gameMaxIntervalInSecondsData: DataNumber = data.getFields()[3] as DataNumber;
    assert.equal(data.getFields()[4] instanceof DataNumber, true);
    const occurredAtPosixTimeData: DataNumber = data.getFields()[4] as DataNumber;
    assert.equal(data.getFields()[5] instanceof DataBytes, true);
    const winningPlayerPubKeyHashData: DataBytes = data.getFields()[5] as DataBytes;

    assert.equal(
      data.getFields()[6] instanceof DataConstr,
      true,
      `Expected DataConstr, got ${data.getFields()[6].constructor.name}`
    );
    const movesData: DataConstr = data.getFields()[6] as DataConstr;

    return new GameIsWon(
      PubKeyHash.fromHexString(playerOnePubKeyHashData.toHex()),
      PubKeyHash.fromHexString(playerTwoPubKeyHashData.toHex()),
      betInAdaData.getValue(),
      gameMaxIntervalInSecondsData.getValue(),
      occurredAtPosixTimeData.getValue(),
      PubKeyHash.fromHexString(winningPlayerPubKeyHashData.toHex()),
      new MovesMadeFactory().fromScriptData(movesData)
    );
  }

  createGameIsTied(data: DataConstr): GameIsTied {
    assert.equal(data.getFields().length, 5, `GameIsTied: Extects 5 fields got ${data.getFields().length}`);
    assert.equal(data.getFields()[0] instanceof DataBytes, true);
    const playerOnePubKeyHashData: DataBytes = data.getFields()[0] as DataBytes;
    assert.equal(data.getFields()[1] instanceof DataBytes, true);
    const playerTwoPubKeyHashData: DataBytes = data.getFields()[1] as DataBytes;
    assert.equal(data.getFields()[2] instanceof DataNumber, true);
    const betInAdaData: DataNumber = data.getFields()[2] as DataNumber;
    assert.equal(data.getFields()[3] instanceof DataNumber, true);
    const occurredAtPosixTimeData: DataNumber = data.getFields()[3] as DataNumber;

    assert.equal(
      data.getFields()[4] instanceof DataConstr,
      true,
      `Expected DataConstr, got ${data.getFields()[4].constructor.name}`
    );
    const movesData: DataConstr = data.getFields()[4] as DataConstr;

    return new GameIsTied(
      PubKeyHash.fromHexString(playerOnePubKeyHashData.toHex()),
      PubKeyHash.fromHexString(playerTwoPubKeyHashData.toHex()),
      betInAdaData.getValue(),
      occurredAtPosixTimeData.getValue(),
      new MovesMadeFactory().fromScriptData(movesData)
    );
  }
}
