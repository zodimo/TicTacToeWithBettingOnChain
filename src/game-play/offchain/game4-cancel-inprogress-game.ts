import {
  GameStateFactory,
  JoinGameParams,
  StartGameParams,
  CancelInProgressGameCommand,
  JoinGameCommand,
  StartGameCommand,
} from "../../app/game-data.js";
import { Game } from "../../app/game.js";
import { runCommand } from "../../cardano-cli/run-command.js";
import { ScriptDataJsonSchema } from "../../cardano-cli/script-data.js";

// TX 1
const playerOneAddress = "Player1Address";
const startGameParams = new StartGameParams(playerOneAddress, 50, 1);
const tx1Command = new StartGameCommand(startGameParams);
const tx1GameState = Game.handleActionCommand(tx1Command);
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
const tx1GameStateFromScriptData = new GameStateFactory().fromScriptData(tx1GameStateAsScriptData);
const tx2Command = new JoinGameCommand(tx1GameStateFromScriptData, joinGameParams);
const tx2GameState = Game.handleActionCommand(tx2Command);
const tx2GameStateAsScriptData = tx2GameState.toScriptData();
console.log("##########################");
console.log(`tx2 [${tx2GameState.constructor.name}]: ${JSON.stringify(tx2GameState, null, 2)}`);
console.log(
  `tx2 scriptdata : ${tx2GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

console.log("sleeping for 5 seconds");
runCommand(`sleep 5`);

const tx2GameStateFromScriptData = new GameStateFactory().fromScriptData(tx2GameStateAsScriptData);
const tx3Command = new CancelInProgressGameCommand(tx2GameStateFromScriptData);
const payout = Game.handleEndGameActionCommand(tx3Command);
console.log(payout);
