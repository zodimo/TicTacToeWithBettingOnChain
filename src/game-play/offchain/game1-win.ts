import {
  Column,
  GameStateFactory,
  Move,
  Row,
  JoinGameParams,
  StartGameParams,
  MakeMoveParams,
  ClaimWinCommand,
  JoinGameCommand,
  MakeMoveCommand,
  StartGameCommand,
  PubKeyHash,
} from "../../app/game-data.js";
import { Game } from "../../app/game.js";
import { fromJson, ScriptDataJsonSchema } from "../../cardano-cli/script-data.js";


const playerOneAddress = "fb21b4500aa8740c8335fc75914e96b8d66c1afc57c03ad0f98ad928";
const playerTwoAddress = "91f60209b232cac65d34c0584fdc33d7024de208d78e4c696fef3a63";


// TX 1
const startGameParams = new StartGameParams(PubKeyHash.fromHexString(playerOneAddress), 50, 30);
const tx1Command = new StartGameCommand(startGameParams);
const tx1GameState = Game.handleActionCommand(tx1Command);
const tx1GameStateAsScriptDataJson = tx1GameState
  .toScriptData()
  .toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema);
console.log("##########################");
console.log(`tx1 [${tx1GameState.constructor.name}]: ${JSON.stringify(tx1GameState, null, 2)}`);
console.log(`tx1 scriptdata : ${tx1GameStateAsScriptDataJson}`);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 2

const joinGameParams = new JoinGameParams(PubKeyHash.fromHexString(playerTwoAddress));

//gamestate from scriptData
const tx1GameStateFromScriptData = new GameStateFactory().fromScriptData(fromJson(tx1GameStateAsScriptDataJson));
const tx2Command = new JoinGameCommand(tx1GameStateFromScriptData, joinGameParams);
const tx2GameState = Game.handleActionCommand(tx2Command);
const tx2GameStateAsScriptDataJson = tx2GameState
  .toScriptData()
  .toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema);
console.log("##########################");
console.log(`tx2 [${tx2GameState.constructor.name}]: ${JSON.stringify(tx2GameState, null, 2)}`);
console.log(`tx2 scriptdata : ${tx2GameStateAsScriptDataJson}`);

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
const makeMoveParams1 = new MakeMoveParams(PubKeyHash.fromHexString(playerTwoAddress), move1);
const tx2GameStateFromScriptData = new GameStateFactory().fromScriptData(fromJson(tx2GameStateAsScriptDataJson));
const tx3Command = new MakeMoveCommand(tx2GameStateFromScriptData, makeMoveParams1);
const tx3GameState = Game.handleActionCommand(tx3Command);
const tx3GameStateAsScriptDataJson = tx3GameState
  .toScriptData()
  .toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema);
console.log("##########################");
console.log(`tx3 [${tx3GameState.constructor.name}]: ${JSON.stringify(tx3GameState, null, 2)}`);
console.log(`tx3 scriptdata : ${tx3GameStateAsScriptDataJson}`);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 4
// Player 1 Move B 2
//    1   2   3

// A _O_|___|___
// B ___|_X_|___
// C    |   |

const move2 = new Move(Row.ROW_B, Column.Col_2);
const makeMoveParams2 = new MakeMoveParams(PubKeyHash.fromHexString(playerOneAddress), move2);
const tx3GameStateFromScriptData = new GameStateFactory().fromScriptData(fromJson(tx3GameStateAsScriptDataJson));
const tx4Command = new MakeMoveCommand(tx3GameStateFromScriptData, makeMoveParams2);
const tx4GameState = Game.handleActionCommand(tx4Command);
const tx4GameStateAsScriptDataJson = tx4GameState
  .toScriptData()
  .toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema);
console.log("##########################");
console.log(`tx4 [${tx4GameState.constructor.name}]: ${JSON.stringify(tx4GameState, null, 2)}`);
console.log(`tx4 scriptdata : ${tx4GameStateAsScriptDataJson}`);

// ///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 5
// Player 2 Move A 2

// A _O_|_O_|___
// B ___|_X_|___
// C    |   |

const move3 = new Move(Row.ROW_A, Column.Col_2);
const makeMoveParams3 = new MakeMoveParams(PubKeyHash.fromHexString(playerTwoAddress), move3);
const tx4GameStateFromScriptData = new GameStateFactory().fromScriptData(fromJson(tx4GameStateAsScriptDataJson));
const tx5Command = new MakeMoveCommand(tx4GameStateFromScriptData, makeMoveParams3);
const tx5GameState = Game.handleActionCommand(tx5Command);
const tx5GameStateAsScriptDataJson = tx5GameState
  .toScriptData()
  .toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema);
console.log("##########################");
console.log(`tx5 [${tx5GameState.constructor.name}]: ${JSON.stringify(tx5GameState, null, 2)}`);
console.log(`tx5 scriptdata : ${tx5GameStateAsScriptDataJson}`);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 6
// Player 1 Move B 3

// A _O_|_O_|___
// B ___|_X_|_X_
// C    |   |

const move4 = new Move(Row.ROW_B, Column.Col_3);
const makeMoveParams4 = new MakeMoveParams(PubKeyHash.fromHexString(playerOneAddress), move4);
const tx5GameStateFromScriptData = new GameStateFactory().fromScriptData(fromJson(tx5GameStateAsScriptDataJson));
const tx6Command = new MakeMoveCommand(tx5GameStateFromScriptData, makeMoveParams4);
const tx6GameState = Game.handleActionCommand(tx6Command);
const tx6GameStateAsScriptDataJson = tx6GameState.toScriptData().toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema);
console.log("##########################");
console.log(`tx6 [${tx6GameState.constructor.name}]: ${JSON.stringify(tx6GameState, null, 2)}`);
console.log(
  `tx6 scriptdata : ${tx6GameStateAsScriptDataJson}`
);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 7
// Player 2 Move A 3

// A _O_|_O_|_O_
// B ___|_X_|_X_
// C    |   |

const move5 = new Move(Row.ROW_A, Column.Col_3);
const makeMoveParams5 = new MakeMoveParams(PubKeyHash.fromHexString(playerTwoAddress), move5);
const tx6GameStateFromScriptData = new GameStateFactory().fromScriptData(fromJson(tx6GameStateAsScriptDataJson));
const tx7Command = new MakeMoveCommand(tx6GameStateFromScriptData, makeMoveParams5);
const tx7GameState = Game.handleActionCommand(tx7Command);
const tx7GameStateAsScriptDataJson = tx7GameState.toScriptData().toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema);
console.log("##########################");
console.log(`tx7 [${tx7GameState.constructor.name}]: ${JSON.stringify(tx7GameState, null, 2)}`);
console.log(
  `tx7 scriptdata : ${tx7GameStateAsScriptDataJson}`
);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

const tx7GameStateFromScriptData = new GameStateFactory().fromScriptData(fromJson(tx7GameStateAsScriptDataJson));
const tx8Command = new ClaimWinCommand(tx7GameStateFromScriptData);
const payout = Game.handleEndGameActionCommand(tx8Command);
// TX 8 Claim Win
console.log("##########################");
console.log(payout);
