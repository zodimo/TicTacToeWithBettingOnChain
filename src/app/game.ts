/**
 * This is where the magic will happen
 *
 * EventDriven and Immutable
 *
 * write the game first, them make the transitional validation onchainable :)
 */

import assert from "assert";
import {
  Column,
  GameInitiated,
  GameInProgress,
  GameIsTied,
  GameIsWon,
  GameStarted,
  GameState,
  Move,
  Moves,
  Row,
} from "./game-data.js";

export class StartGameParams {
  constructor(
    public readonly playerOneAddress: string,
    public readonly betInAda: number,
    public readonly gameMaxIntervalInMins: number
  ) {}
}

export class JoinGameParams {
  constructor(public readonly playerTwoAddress: string) {}
}

export class MakeMoveParams {
  constructor(public readonly playerAddress: string, public readonly move: Move) {}
}

export class Game {
  private constructor(public readonly gameState: GameState) {}

  static startGame(params: StartGameParams): Game {
    //create game state from params
    const gameState = new GameInitiated(params.playerOneAddress, params.betInAda, params.gameMaxIntervalInMins);
    return new Game(gameState);
  }

  static loadGame(gameState: GameState): Game {
    return new Game(gameState);
  }

  joinGame(params: JoinGameParams): Game {
    assert.equal(this.gameState instanceof GameInitiated, true);
    const currentGamestate: GameInitiated = this.gameState as GameInitiated;
    //randomize which player is x and o
    const playerXAddress = currentGamestate.playerOneAddress;
    const playerOAddress = params.playerTwoAddress;
    //randomize which player to start;
    const playerAddressToMakeMove = playerOAddress;

    const gameState = new GameStarted(
      currentGamestate.playerOneAddress,
      params.playerTwoAddress,
      currentGamestate.betInAda,
      currentGamestate.gameMaxIntervalInMins,
      playerXAddress,
      playerOAddress,
      playerAddressToMakeMove
    );

    return new Game(gameState);
  }

  makeFirstMove(params: MakeMoveParams): Game {
    assert.equal(
      this.gameState instanceof GameStarted,
      true,
      `Game must be Started!, current state: ${this.gameState.constructor.name}`
    );
    const currentGamestate: GameStarted = this.gameState as GameStarted;
    assert.equal(currentGamestate.playerAddressToMakeMove == params.playerAddress, true, "Wrong player playing now!");

    // select other player to play next.
    let playerAddressToMakeMove = currentGamestate.playerXAddress;
    if (currentGamestate.playerAddressToMakeMove == playerAddressToMakeMove) {
      playerAddressToMakeMove = currentGamestate.playerOAddress;
    }

    const move = Moves.initialise();

    const gameState = new GameInProgress(
      currentGamestate.playerOneAddress,
      currentGamestate.playerTwoAddress,
      currentGamestate.betInAda,
      currentGamestate.gameMaxIntervalInMins,
      currentGamestate.playerXAddress,
      currentGamestate.playerOAddress,
      playerAddressToMakeMove,
      move.makeMove(params.playerAddress, params.move)
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
    assert.equal(currentGamestate.playerAddressToMakeMove == params.playerAddress, true, "Wrong player playing now!");

    // select other player to play next.
    let playerAddressToMakeMove = currentGamestate.playerXAddress;
    if (currentGamestate.playerAddressToMakeMove == playerAddressToMakeMove) {
      playerAddressToMakeMove = currentGamestate.playerOAddress;
    }

    this.assertGameIsPlayable(currentGamestate.moves);

    const currentMoves = currentGamestate.moves;
    const newMoves = currentMoves.makeMove(params.playerAddress, params.move);

    let gameState: GameState;

    if (this.isGameWon(newMoves)) {
      gameState = new GameIsWon(
        currentGamestate.playerOneAddress,
        currentGamestate.playerTwoAddress,
        currentGamestate.betInAda,
        currentGamestate.gameMaxIntervalInMins,
        currentGamestate.playerXAddress,
        currentGamestate.playerOAddress,
        params.playerAddress, //winning player address
        newMoves
      );
    } else if (this.isGameTied(newMoves)) {
      gameState = new GameIsTied(
        currentGamestate.playerOneAddress,
        currentGamestate.playerTwoAddress,
        currentGamestate.betInAda,
        currentGamestate.playerXAddress,
        currentGamestate.playerOAddress,
        newMoves
      );
    } else {
      gameState = new GameInProgress(
        currentGamestate.playerOneAddress,
        currentGamestate.playerTwoAddress,
        currentGamestate.betInAda,
        currentGamestate.gameMaxIntervalInMins,
        currentGamestate.playerXAddress,
        currentGamestate.playerOAddress,
        playerAddressToMakeMove,
        newMoves
      );
    }

    return new Game(gameState);
  }

  claimWin() {
    //the wallet call this methods will pay the fees.
    assert.equal(
      this.gameState instanceof GameIsWon,
      true,
      `Game must be Won!, current state: ${this.gameState.constructor.name}`
    );
    const currentGamestate: GameIsWon = this.gameState as GameIsWon;

    console.log(
      `Congratulation player : ${currentGamestate.winningPlayerAddress} you won ${currentGamestate.betInAda} Ada + Original ${currentGamestate.betInAda} Ada`
    );
  }
  claimTie() {
    //the wallet call this methods will pay the fees.
    assert.equal(
      this.gameState instanceof GameIsTied,
      true,
      `Game must be Tied!, current state: ${this.gameState.constructor.name}`
    );
    const currentGamestate: GameIsTied = this.gameState as GameIsTied;
    console.log("Game is Tied, return player bets.");
    console.log(`player : ${currentGamestate.playerXAddress} ${currentGamestate.betInAda} Ada returned`);
    console.log(`player : ${currentGamestate.playerOAddress} ${currentGamestate.betInAda} Ada returned`);
  }
  claimTimeOut() {
    throw new Error("Not yet implemented!!");
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
