import {
  GameStateFactory,
  StartGameParams,
  CancelInitiatedGameCommand,
  StartGameCommand,
  PubKeyHash,
} from "../../app/game-data.js";
import { Game } from "../../app/game.js";
import { runCommand } from "../../cardano-cli/run-command.js";
import { ScriptDataJsonSchema } from "../../cardano-cli/script-data.js";

const playerOneAddress = "fb21b4500aa8740c8335fc75914e96b8d66c1afc57c03ad0f98ad928";


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
console.log("sleeping for 5 seconds");
runCommand(`sleep 5`);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// Cancel game

const tx1GameStateFromScriptData = new GameStateFactory().fromScriptData(tx1GameStateAsScriptData);
const tx2Command = new CancelInitiatedGameCommand(tx1GameStateFromScriptData);
const payout = Game.handleEndGameActionCommand(tx2Command);
console.log(payout);
