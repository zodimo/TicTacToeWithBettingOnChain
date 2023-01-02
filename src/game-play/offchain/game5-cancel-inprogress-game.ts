import { Column, GameState, GameStateFactory, Move, Row } from "../app/game-data.js";
import { Game, JoinGameParams, StartGameParams, MakeMoveParams } from "../app/game.js";
import { runCommand } from "../cardano-cli/run-command.js";
import { ScriptDataJsonSchema } from "../cardano-cli/script-data.js";

// TX 1
const playerOneAddress = "Player1Address";
const startGameParams = new StartGameParams(playerOneAddress, 50, 1);

const tx1: (startGameParams: StartGameParams) => GameState = (startGameParams) => {
  return Game.startGame(startGameParams).gameState;
};

const tx1GameState = tx1(startGameParams);
const tx1GameStateAsScriptData = tx1GameState.toScriptData();
console.log("##########################");
console.log(`tx1 [${tx1GameState.constructor.name}]: ${JSON.stringify(tx1GameState, null, 2)}`);
console.log(
  `tx1 scriptdata : ${tx1GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 2
const playerTwoAddress = "Player2Address";
const joinGameParams = new JoinGameParams(playerTwoAddress);

const tx2: (gameState: GameState, joinGameParams: JoinGameParams) => GameState = (gamestate, joinGameParams) => {
  return Game.loadGame(gamestate).joinGame(joinGameParams).gameState;
};
const tx1GameStateFromScriptData = new GameStateFactory().fromScriptData(tx1GameStateAsScriptData);
const tx2GameState = tx2(tx1GameStateFromScriptData, joinGameParams);
const tx2GameStateAsScriptData = tx2GameState.toScriptData();
console.log("##########################");
console.log(`tx2 [${tx2GameState.constructor.name}]: ${JSON.stringify(tx2GameState, null, 2)}`);
console.log(
  `tx2 scriptdata : ${tx2GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////
// TX 3
// fake game start with player 2 as O and move A 1

//    1   2   3
// A _O_|___|___
// B ___|___|___
// C    |   |

const move1 = new Move(Row.ROW_A, Column.Col_1);
const makeMoveParams1 = new MakeMoveParams(playerTwoAddress, move1);
const tx3: (gameState: GameState, makeMoveParams: MakeMoveParams) => GameState = (gamestate, makeMoveParams) => {
  //Make first move
  return Game.loadGame(gamestate).makeFirstMove(makeMoveParams).gameState;
};
const tx2GameStateFromScriptData = new GameStateFactory().fromScriptData(tx2GameStateAsScriptData);
const tx3GameState = tx3(tx2GameStateFromScriptData, makeMoveParams1);
const tx3GameStateAsScriptData = tx3GameState.toScriptData();
console.log("##########################");
console.log(`tx3 [${tx3GameState.constructor.name}]: ${JSON.stringify(tx3GameState, null, 2)}`);
console.log(
  `tx3 scriptdata : ${tx3GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

console.log("sleeping for 5 seconds");
runCommand(`sleep 5`);

const tx4: (gameState: GameState) => void = (gamestate) => {
  Game.loadGame(gamestate).cancelInProgressGame();
};

const tx3GameStateFromScriptData = new GameStateFactory().fromScriptData(tx3GameStateAsScriptData);
tx4(tx3GameStateFromScriptData);