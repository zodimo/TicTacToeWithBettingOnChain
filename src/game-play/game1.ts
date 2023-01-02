import { Game, GameState, JoinGameParams, StartGameParams, Move, Row, Column, MakeMoveParams } from "../app/game.js";

// TX 1
const playerOneAddress = "Player1Address";
const startGameParams = new StartGameParams(playerOneAddress, 50, 30);

const tx1: (startGameParams: StartGameParams) => GameState = (startGameParams) => {
  return Game.startGame(startGameParams).gameState;
};

const tx1GameState = tx1(startGameParams);
console.log(`tx1 [${tx1GameState.constructor.name}]: ${JSON.stringify(tx1GameState, null, 2)}`);

// TX 2
const playerTwoAddress = "Player2Address";
const joinGameParams = new JoinGameParams(playerTwoAddress);

const tx2: (gameState: GameState, joinGameParams: JoinGameParams) => GameState = (gamestate, joinGameParams) => {
  return Game.loadGame(gamestate).joinGame(joinGameParams).gameState;
};

const tx2GameState = tx2(tx1GameState, joinGameParams);
console.log(`tx2 [${tx2GameState.constructor.name}]: ${JSON.stringify(tx2GameState, null, 2)}`);

// TX 3
// fake game start with player 2 as O and move A 1
const move1 = new Move(Row.ROW_A, Column.Col_1);
const makeMoveParams1 = new MakeMoveParams(playerTwoAddress, move1);
const tx3: (gameState: GameState, makeMoveParams: MakeMoveParams) => GameState = (gamestate, makeMoveParams) => {
  //Make first move
  return Game.loadGame(gamestate).makeFirstMove(makeMoveParams).gameState;
};

const tx3GameState = tx3(tx2GameState, makeMoveParams1);
console.log(`tx3 [${tx3GameState.constructor.name}]: ${JSON.stringify(tx3GameState, null, 2)}`);

// TX 4
// Player 1 Move B 2
const move2 = new Move(Row.ROW_B, Column.Col_2);
const makeMoveParams2 = new MakeMoveParams(playerOneAddress, move2);
const tx4: (gameState: GameState, makeMoveParams: MakeMoveParams) => GameState = (gamestate, makeMoveParams) => {
  return Game.loadGame(gamestate).makeMove(makeMoveParams).gameState;
};

const tx4GameState = tx4(tx3GameState, makeMoveParams2);
console.log(`tx4 [${tx4GameState.constructor.name}]: ${JSON.stringify(tx4GameState, null, 2)}`);

// TX 5
// Player 2 Move A 2
const move3 = new Move(Row.ROW_A, Column.Col_2);
const makeMoveParams3 = new MakeMoveParams(playerTwoAddress, move3);
const tx5: (gameState: GameState, makeMoveParams: MakeMoveParams) => GameState = (gamestate, makeMoveParams) => {
  return Game.loadGame(gamestate).makeMove(makeMoveParams).gameState;
};

const tx5GameState = tx5(tx4GameState, makeMoveParams3);
console.log(`tx5 [${tx5GameState.constructor.name}]: ${JSON.stringify(tx5GameState, null, 2)}`);

// TX 6
// Player 1 Move B 3
const move4 = new Move(Row.ROW_B, Column.Col_3);
const makeMoveParams4 = new MakeMoveParams(playerOneAddress, move4);
const tx6: (gameState: GameState, makeMoveParams: MakeMoveParams) => GameState = (gamestate, makeMoveParams) => {
  return Game.loadGame(gamestate).makeMove(makeMoveParams).gameState;
};

const tx6GameState = tx6(tx5GameState, makeMoveParams4);
console.log(`tx6 [${tx6GameState.constructor.name}]: ${JSON.stringify(tx6GameState, null, 2)}`);

// TX 7
// Player 2 Move A 3
const move5 = new Move(Row.ROW_A, Column.Col_3);
const makeMoveParams5 = new MakeMoveParams(playerTwoAddress, move5);
const tx7: (gameState: GameState, makeMoveParams: MakeMoveParams) => GameState = (gamestate, makeMoveParams) => {
  return Game.loadGame(gamestate).makeMove(makeMoveParams).gameState;
};

const tx7GameState = tx7(tx6GameState, makeMoveParams5);
console.log(`tx7 [${tx7GameState.constructor.name}]: ${JSON.stringify(tx7GameState, null, 2)}`);
