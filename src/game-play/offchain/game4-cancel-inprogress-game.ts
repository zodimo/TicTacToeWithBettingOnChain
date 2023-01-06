import {
  GameStateFactory,
  JoinGameParams,
  StartGameParams,
  CancelInProgressGameCommand,
  JoinGameCommand,
  StartGameCommand,
  PubKeyHash,
} from "../../app/game-data.js";
import { Game } from "../../app/game.js";
import { runCommand } from "../../cardano-cli/run-command.js";
import { ScriptDataJsonSchema } from "../../cardano-cli/script-data.js";

const playerOneAddress = "fb21b4500aa8740c8335fc75914e96b8d66c1afc57c03ad0f98ad928";
const playerTwoAddress = "91f60209b232cac65d34c0584fdc33d7024de208d78e4c696fef3a63";


// TX 1
const startGameParams = new StartGameParams(PubKeyHash.fromHexString(playerOneAddress), 50, 1);
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
const joinGameParams = new JoinGameParams(PubKeyHash.fromHexString(playerTwoAddress));
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
