import { GameState, GameStateFactory } from "../app/game-data.js";
import { Game, StartGameParams } from "../app/game.js";
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
console.log("sleeping for 5 seconds");
runCommand(`sleep 5`);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// Cancel game

const tx2: (gameState: GameState) => void = (gamestate) => {
  Game.loadGame(gamestate).cancelInitiatedGame();
};
const tx1GameStateFromScriptData = new GameStateFactory().fromScriptData(tx1GameStateAsScriptData);
tx2(tx1GameStateFromScriptData);
