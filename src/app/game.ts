/**
 * This is where the magic will happen
 *
 * Command Driven and Immutable
 *
 * write the game first, then make the transitional validation onchainable :)
 */

import assert from "assert";
import {
  CancelInitiatedGameCommand,
  CancelInProgressGameCommand,
  ClaimTieCommand,
  ClaimWinCommand,
  Column,
  EndGameActionCommand,
  GameAction,
  GameActionCommand,
  GameEndAction,
  GameInitiated,
  GameInProgress,
  GameIsTied,
  GameIsWon,
  GameState,
  JoinGameCommand,
  JoinGameParams,
  MakeMoveCommand,
  MakeMoveParams,
  MovesMade,
  PubKeyHash,
  Row,
  StartGameCommand,
  StartGameParams,
} from "./game-data.js";


export class Payout {
  constructor(public readonly pubKeyHash: PubKeyHash, public readonly amountInAda: number) {}
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

  public static handleEndGameActionCommand(actionCommand: EndGameActionCommand): GamePayOut {
    switch (actionCommand.getAction()) {
      case GameEndAction.CLAIM_WIN:
        return Game.loadGame((actionCommand as ClaimWinCommand).getGameState()).claimWin();

      case GameEndAction.CLAIM_TIE:
        return Game.loadGame((actionCommand as ClaimTieCommand).getGameState()).claimTie();
      case GameEndAction.CANCEL_INITIATED_GAME:
        return Game.loadGame((actionCommand as CancelInitiatedGameCommand).getGameState()).cancelInitiatedGame();
      case GameEndAction.CANCEL_IN_PROGRESS_GAME:
        return Game.loadGame((actionCommand as CancelInProgressGameCommand).getGameState()).cancelInProgressGame();
      default:
        throw new Error(`Unknown action : ${actionCommand.getAction()}`);
    }
  }
  public static handleActionCommand(actionCommand: GameActionCommand): GameState {
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
      default:
        throw new Error(`Unknown action : ${actionCommand.getAction()}`);
    }
  }

  static startGame(params: StartGameParams): Game {
    //create game state from params
    // @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/now
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
      playerTwoPubKeyHash,
      currentGamestate.betInAda,
      currentGamestate.gameMaxIntervalInSeconds,
      postixTimeNow,
      playerAddressToMakeMove,
      MovesMade.initialise()
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
      currentGamestate.playerAddressToMakeMove.equals(params.playerPubKeyHash),
      true,
      "Wrong player playing now!"
    );

    // select other player to play next.
    let playerAddressToMakeMove = currentGamestate.playerOnePubKeyHash;
    if (currentGamestate.playerAddressToMakeMove.equals(playerAddressToMakeMove)) {
      playerAddressToMakeMove = currentGamestate.playerTwoPubKeyHash;
    }

    this.assertGameIsPlayable(currentGamestate.movesMade);

    const currentMovesMade = currentGamestate.movesMade;
    //this will throw and error if move cannot be made
    const updatedMovesMade = currentMovesMade.makeMove(params.playerPubKeyHash, params.move);
    const postixTimeNow = +Date.now().toString();

    let gameState: GameState;

    if (this.isGameWon(updatedMovesMade)) {
      gameState = new GameIsWon(
        currentGamestate.playerOnePubKeyHash,
        currentGamestate.playerTwoPubKeyHash,
        currentGamestate.betInAda,
        currentGamestate.gameMaxIntervalInSeconds,
        postixTimeNow,
        params.playerPubKeyHash, //winning player address
        updatedMovesMade
      );
    } else if (this.isGameTied(updatedMovesMade)) {
      gameState = new GameIsTied(
        currentGamestate.playerOnePubKeyHash,
        currentGamestate.playerTwoPubKeyHash,
        currentGamestate.betInAda,
        postixTimeNow,
        updatedMovesMade
      );
    } else {
      gameState = new GameInProgress(
        currentGamestate.playerOnePubKeyHash,
        currentGamestate.playerTwoPubKeyHash,
        currentGamestate.betInAda,
        currentGamestate.gameMaxIntervalInSeconds,
        postixTimeNow,
        playerAddressToMakeMove,
        updatedMovesMade
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
    if (currentGamestate.playerAddressToMakeMove.equals(winnerByTimout)) {
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

  assertGameIsPlayable(moves: MovesMade): void {
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
  isGameTied(moves: MovesMade): boolean {
    const isTiedValidator: (moves: MovesMade) => boolean = (moves) => {
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
  isGameWon(moves: MovesMade): boolean {
    const isWonStrategyValidator: (moves: MovesMade) => boolean = (moves) => {
      //top row same player strategy

      const topLeftMove = moves.getMoveMadeAtPosition(Row.ROW_A, Column.Col_1);
      const topMiddleMove = moves.getMoveMadeAtPosition(Row.ROW_A, Column.Col_2);
      const topRightMove = moves.getMoveMadeAtPosition(Row.ROW_A, Column.Col_3);

      if (topLeftMove && topMiddleMove && topRightMove) {
        if (
          topLeftMove.playerPubKeyHash.equals(topMiddleMove.playerPubKeyHash) &&
          topLeftMove.playerPubKeyHash.equals(topRightMove.playerPubKeyHash)
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
