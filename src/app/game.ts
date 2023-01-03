/**
 * This is where the magic will happen
 *
 * Command Driven and Immutable
 *
 * write the game first, then make the transitional validation onchainable :)
 */

import assert from "assert";
import {
  Column,
  GameInitiated,
  GameInProgress,
  GameIsTied,
  GameIsWon,
  GameState,
  JoinGameParams,
  MakeMoveParams,
  Moves,
  Row,
  StartGameParams,
} from "./game-data.js";

// Commands

enum GameAction {
  START_GAME = "start-game",
  JOIN_GAME = "join-game",
  MAKE_MOVE = "make-move",
  CLAIM_WIN = "claim-win",
  CLAIM_TIE = "claim-tie",
  CANCEL_INITIATED_GAME = "cancel-initiated-game",
  CANCEL_IN_PROGRESS_GAME = "cancel-in-progress-game",
}

interface GameActionCommandInterface<T> {
  getAction(): GameAction;
  getParameters(): T;
}

export class StartGameCommand implements GameActionCommandInterface<StartGameParams> {
  constructor(private params: StartGameParams) {}
  getAction(): GameAction {
    return GameAction.START_GAME;
  }
  getParameters(): StartGameParams {
    return this.params;
  }
}

export class JoinGameCommand implements GameActionCommandInterface<JoinGameParams> {
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
}

export class MakeMoveCommand implements GameActionCommandInterface<MakeMoveParams> {
  constructor(private gameState: GameState, private params: MakeMoveParams) {}
  getGameState(): GameState {
    return this.gameState;
  }
  getAction(): GameAction {
    return GameAction.JOIN_GAME;
  }
  getParameters(): MakeMoveParams {
    return this.params;
  }
}

export class ClaimWinCommand implements GameActionCommandInterface<void> {
  constructor(private gameState: GameState) {}
  getGameState(): GameState {
    return this.gameState;
  }
  getAction(): GameAction {
    return GameAction.CLAIM_WIN;
  }
  getParameters(): void {}
}

export class ClaimTieCommand implements GameActionCommandInterface<void> {
  constructor(private gameState: GameState) {}
  getGameState(): GameState {
    return this.gameState;
  }
  getAction(): GameAction {
    return GameAction.CLAIM_TIE;
  }
  getParameters(): void {}
}

export class CancelInitiatedGameCommand implements GameActionCommandInterface<void> {
  constructor(private gameState: GameState) {}
  getGameState(): GameState {
    return this.gameState;
  }
  getAction(): GameAction {
    return GameAction.CANCEL_INITIATED_GAME;
  }
  getParameters(): void {}
}

export class CancelInProgressGameCommand implements GameActionCommandInterface<void> {
  constructor(private gameState: GameState) {}
  getGameState(): GameState {
    return this.gameState;
  }
  getAction(): GameAction {
    return GameAction.CANCEL_IN_PROGRESS_GAME;
  }
  getParameters(): void {}
}

export type GameActionCommand =
  | StartGameCommand
  | JoinGameCommand
  | MakeMoveCommand
  | ClaimWinCommand
  | ClaimTieCommand
  | CancelInitiatedGameCommand
  | CancelInProgressGameCommand;

export class Payout {
  constructor(public readonly pubKeyHash: string, public readonly amountInAda: number) {}
}

export class GameIsWonPayout {
  constructor(public readonly payout: Payout) {}
}

export class GameIsTiePayout {
  constructor(public readonly payouts: Payout[]) {}
}

export class InitiatedGameIsCancelledPayout {
  constructor(public readonly payout: Payout) {}
}

export class InprogressGameIsCancelledPayout {
  constructor(public readonly payout: Payout) {}
}

export type GamePayOut =
  | GameIsWonPayout
  | GameIsTiePayout
  | InitiatedGameIsCancelledPayout
  | InprogressGameIsCancelledPayout;

export class Game {
  private constructor(public readonly gameState: GameState) {}

  static handleActionCommand(actionCommand: GameActionCommand): GameState | GamePayOut {
    switch (actionCommand.getAction()) {
      case GameAction.START_GAME:
        return Game.startGame((actionCommand as StartGameCommand).getParameters()).gameState;

      case GameAction.JOIN_GAME:
        return Game.loadGame((actionCommand as JoinGameCommand).getGameState()).joinGame(
          (actionCommand as JoinGameCommand).getParameters()
        ).gameState;

      case GameAction.MAKE_MOVE:
        return Game.loadGame((actionCommand as MakeMoveCommand).getGameState()).makeMove(
          (actionCommand as MakeMoveCommand).getParameters()
        ).gameState;

      case GameAction.CLAIM_WIN:
        return Game.loadGame((actionCommand as ClaimWinCommand).getGameState()).claimWin();

      case GameAction.CLAIM_TIE:
        return Game.loadGame((actionCommand as ClaimTieCommand).getGameState()).claimTie();
      case GameAction.CANCEL_INITIATED_GAME:
        return Game.loadGame((actionCommand as CancelInitiatedGameCommand).getGameState()).cancelInitiatedGame();
      case GameAction.CANCEL_IN_PROGRESS_GAME:
        return Game.loadGame((actionCommand as CancelInProgressGameCommand).getGameState()).cancelInProgressGame();
      default:
        throw new Error(`Unknown action : ${actionCommand.getAction()}`);
    }
  }

  static startGame(params: StartGameParams): Game {
    //create game state from params
    const postixTimeNow = +Date.now().toString();

    const gameState = new GameInitiated(
      params.playerOnePubKeyHash,
      params.betInAda,
      params.gameMaxIntervalInSeconds,
      postixTimeNow
    );
    return new Game(gameState);
  }

  static loadGame(gameState: GameState): Game {
    return new Game(gameState);
  }

  joinGame(params: JoinGameParams): Game {
    assert.equal(this.gameState instanceof GameInitiated, true);
    const currentGamestate: GameInitiated = this.gameState as GameInitiated;
    const playerTwoPubKeyHash = params.playerTwoPubKeyHash;
    //randomize which player to start;
    const playerAddressToMakeMove = playerTwoPubKeyHash;
    const postixTimeNow = +Date.now().toString();

    const gameState = new GameInProgress(
      currentGamestate.playerOnePubKeyHash,
      params.playerTwoPubKeyHash,
      currentGamestate.betInAda,
      currentGamestate.gameMaxIntervalInSeconds,
      postixTimeNow,
      playerAddressToMakeMove,
      Moves.initialise()
    );

    return new Game(gameState);
  }

  makeMove(params: MakeMoveParams): Game {
    assert.equal(
      this.gameState instanceof GameInProgress,
      true,
      `Game must be InProgress!, current state: ${this.gameState.constructor.name}`
    );

    const currentGamestate: GameInProgress = this.gameState as GameInProgress;
    assert.equal(
      currentGamestate.playerAddressToMakeMove == params.playerPubKeyHash,
      true,
      "Wrong player playing now!"
    );

    // select other player to play next.
    let playerAddressToMakeMove = currentGamestate.playerOnePubKeyHash;
    if (currentGamestate.playerAddressToMakeMove == playerAddressToMakeMove) {
      playerAddressToMakeMove = currentGamestate.playerTwoPubKeyHash;
    }

    this.assertGameIsPlayable(currentGamestate.moves);

    const currentMoves = currentGamestate.moves;
    const newMoves = currentMoves.makeMove(params.playerPubKeyHash, params.move);
    const postixTimeNow = +Date.now().toString();

    let gameState: GameState;

    if (this.isGameWon(newMoves)) {
      gameState = new GameIsWon(
        currentGamestate.playerOnePubKeyHash,
        currentGamestate.playerTwoPubKeyHash,
        currentGamestate.betInAda,
        currentGamestate.gameMaxIntervalInSeconds,
        postixTimeNow,
        params.playerPubKeyHash, //winning player address
        newMoves
      );
    } else if (this.isGameTied(newMoves)) {
      gameState = new GameIsTied(
        currentGamestate.playerOnePubKeyHash,
        currentGamestate.playerTwoPubKeyHash,
        currentGamestate.betInAda,
        postixTimeNow,
        newMoves
      );
    } else {
      gameState = new GameInProgress(
        currentGamestate.playerOnePubKeyHash,
        currentGamestate.playerTwoPubKeyHash,
        currentGamestate.betInAda,
        currentGamestate.gameMaxIntervalInSeconds,
        postixTimeNow,
        playerAddressToMakeMove,
        newMoves
      );
    }

    return new Game(gameState);
  }

  cancelInitiatedGame(): GamePayOut {
    assert.equal(
      this.gameState instanceof GameInitiated,
      true,
      `GameState is expected to be GameInitiated, got ${this.gameState.constructor.name}`
    );
    const currentGamestate: GameInitiated = this.gameState as GameInitiated;
    const postixTimeNow = +Date.now().toString();
    assert.equal(
      currentGamestate.expiresAtPosixTime() < postixTimeNow,
      true,
      "Expected Game to have reached its timeout!"
    );

    console.log(`Nobody want to play! ,You can have your ${currentGamestate.betInAda} Ada back!`);

    return new InitiatedGameIsCancelledPayout(
      new Payout(currentGamestate.playerOnePubKeyHash, currentGamestate.betInAda)
    );
  }

  cancelInProgressGame(): GamePayOut {
    assert.equal(
      this.gameState instanceof GameInProgress,
      true,
      `GameState is expected to be GameInProgress, got ${this.gameState.constructor.name}`
    );
    const currentGamestate: GameInProgress = this.gameState as GameInProgress;
    const postixTimeNow = +Date.now().toString();
    assert.equal(
      currentGamestate.expiresAtPosixTime() < postixTimeNow,
      true,
      "Expected Game to have reached its timeout!"
    );

    let winnerByTimout = currentGamestate.playerOnePubKeyHash;
    if (currentGamestate.playerAddressToMakeMove == winnerByTimout) {
      winnerByTimout = currentGamestate.playerTwoPubKeyHash;
    }

    console.log(`Player ${currentGamestate.playerAddressToMakeMove} has failed to repond.`);
    console.log(`Winner by TIMEOUT is ${winnerByTimout}!`);
    console.log(
      `Congratulation player : ${winnerByTimout} you won ${currentGamestate.betInAda} Ada + Original ${currentGamestate.betInAda} Ada`
    );
    const payoutAmountInAda = currentGamestate.betInAda * 2;
    return new InprogressGameIsCancelledPayout(new Payout(winnerByTimout, payoutAmountInAda));
  }

  claimWin(): GamePayOut {
    //the wallet call this methods will pay the fees.
    assert.equal(
      this.gameState instanceof GameIsWon,
      true,
      `Game must be Won!, current state: ${this.gameState.constructor.name}`
    );
    const currentGamestate: GameIsWon = this.gameState as GameIsWon;

    console.log(
      `Congratulation player : ${currentGamestate.winningPlayerPubKeyHash} you won ${currentGamestate.betInAda} Ada + Original ${currentGamestate.betInAda} Ada`
    );
    const payoutAmountInAda = currentGamestate.betInAda * 2;
    return new GameIsWonPayout(new Payout(currentGamestate.winningPlayerPubKeyHash, payoutAmountInAda));
  }
  claimTie(): GamePayOut {
    //the wallet call this methods will pay the fees.
    assert.equal(
      this.gameState instanceof GameIsTied,
      true,
      `Game must be Tied!, current state: ${this.gameState.constructor.name}`
    );
    const currentGamestate: GameIsTied = this.gameState as GameIsTied;
    console.log("Game is Tied, return player bets.");
    console.log(`player : ${currentGamestate.playerOnePubKeyHash} ${currentGamestate.betInAda} Ada returned`);
    console.log(`player : ${currentGamestate.playerTwoPubKeyHash} ${currentGamestate.betInAda} Ada returned`);
    return new GameIsTiePayout([
      new Payout(currentGamestate.playerOnePubKeyHash, currentGamestate.betInAda),
      new Payout(currentGamestate.playerTwoPubKeyHash, currentGamestate.betInAda),
    ]);
  }

  assertGameIsPlayable(moves: Moves): void {
    //assert that game is not won or tied.
    if (this.isGameWon(moves)) {
      // @todo fill in player
      throw new Error("Game is not playable, winner is: [FILL in PLAYER HERE ]!");
    }
    if (this.isGameTied(moves)) {
      // @todo fill in player
      throw new Error("Game is not playable, it is a tie!");
    }
  }
  isGameTied(moves: Moves): boolean {
    const isTiedValidator: (moves: Moves) => boolean = (moves) => {
      // if top rows are played but not the same player

      const topLeftMove = moves.getMoveMadeAtPosition(Row.ROW_A, Column.Col_1);
      const topMiddleMove = moves.getMoveMadeAtPosition(Row.ROW_A, Column.Col_2);
      const topRightMove = moves.getMoveMadeAtPosition(Row.ROW_A, Column.Col_3);

      if (topLeftMove && topMiddleMove && topRightMove) {
        // top row played and nobody won !
        return !this.isGameWon(moves);
      }
      return false;
    };
    //can later be replaced by a custom strategy
    return isTiedValidator(moves);
  }
  isGameWon(moves: Moves): boolean {
    const isWonStrategyValidator: (moves: Moves) => boolean = (moves) => {
      //top row same player strategy

      const topLeftMove = moves.getMoveMadeAtPosition(Row.ROW_A, Column.Col_1);
      const topMiddleMove = moves.getMoveMadeAtPosition(Row.ROW_A, Column.Col_2);
      const topRightMove = moves.getMoveMadeAtPosition(Row.ROW_A, Column.Col_3);

      if (topLeftMove && topMiddleMove && topRightMove) {
        if (
          topLeftMove.playerAddress == topMiddleMove.playerAddress &&
          topLeftMove.playerAddress == topRightMove.playerAddress
        ) {
          return true;
        }
      }
      return false;
    };
    //can later be replaced by a custom strategy
    return isWonStrategyValidator(moves);
  }
}
