import {
  Column,
  GameState,
  GameStateFactory,
  Move,
  Row,
  JoinGameParams,
  StartGameParams,
  MakeMoveParams,
} from "../../app/game-data.js";
import {
  ClaimTieCommand,
  Game,
  GamePayOut,
  JoinGameCommand,
  MakeMoveCommand,
  StartGameCommand,
} from "../../app/game.js";
import { ScriptDataJsonSchema } from "../../cardano-cli/script-data.js";

/**
 * The changes from the wins is from TX7
 */

// TX 1
const playerOneAddress = "Player1Address";
const startGameParams = new StartGameParams(playerOneAddress, 50, 30);
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

//gamestate from scriptData
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
// TX 3
// fake game start with player 2 as O and move A 1

//    1   2   3
// A _O_|___|___
// B ___|___|___
// C    |   |

const move1 = new Move(Row.ROW_A, Column.Col_1);
const makeMoveParams1 = new MakeMoveParams(playerTwoAddress, move1);
const tx2GameStateFromScriptData = new GameStateFactory().fromScriptData(tx2GameStateAsScriptData);
const tx3Command = new MakeMoveCommand(tx2GameStateFromScriptData, makeMoveParams1);
const tx3GameState = Game.handleActionCommand(tx3Command);
const tx3GameStateAsScriptData = tx3GameState.toScriptData();
console.log("##########################");
console.log(`tx3 [${tx3GameState.constructor.name}]: ${JSON.stringify(tx3GameState, null, 2)}`);
console.log(
  `tx3 scriptdata : ${tx3GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
);

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
const makeMoveParams2 = new MakeMoveParams(playerOneAddress, move2);
const tx3GameStateFromScriptData = new GameStateFactory().fromScriptData(tx3GameStateAsScriptData);
const tx4Command = new MakeMoveCommand(tx3GameStateFromScriptData, makeMoveParams2);
const tx4GameState = Game.handleActionCommand(tx4Command);
const tx4GameStateAsScriptData = tx4GameState.toScriptData();
console.log("##########################");
console.log(`tx4 [${tx4GameState.constructor.name}]: ${JSON.stringify(tx4GameState, null, 2)}`);
console.log(
  `tx4 scriptdata : ${tx4GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
);

// ///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 5
// Player 2 Move A 2

// A _O_|_O_|___
// B ___|_X_|___
// C    |   |

const move3 = new Move(Row.ROW_A, Column.Col_2);
const makeMoveParams3 = new MakeMoveParams(playerTwoAddress, move3);
const tx4GameStateFromScriptData = new GameStateFactory().fromScriptData(tx4GameStateAsScriptData);
const tx5Command = new MakeMoveCommand(tx4GameStateFromScriptData, makeMoveParams3);
const tx5GameState = Game.handleActionCommand(tx5Command);
const tx5GameStateAsScriptData = tx5GameState.toScriptData();
console.log("##########################");
console.log(`tx5 [${tx5GameState.constructor.name}]: ${JSON.stringify(tx5GameState, null, 2)}`);
console.log(
  `tx5 scriptdata : ${tx5GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 6
// Player 1 Move B 3

// A _O_|_O_|___
// B ___|_X_|_X_
// C    |   |

const move4 = new Move(Row.ROW_B, Column.Col_3);
const makeMoveParams4 = new MakeMoveParams(playerOneAddress, move4);
const tx5GameStateFromScriptData = new GameStateFactory().fromScriptData(tx5GameStateAsScriptData);
const tx6Command = new MakeMoveCommand(tx5GameStateFromScriptData, makeMoveParams4);
const tx6GameState = Game.handleActionCommand(tx6Command);
const tx6GameStateAsScriptData = tx6GameState.toScriptData();
console.log("##########################");
console.log(`tx6 [${tx6GameState.constructor.name}]: ${JSON.stringify(tx6GameState, null, 2)}`);
console.log(
  `tx6 scriptdata : ${tx6GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 7
// Player 2 Move B 1

// A _O_|_O_|___
// B _O_|_X_|_X_
// C    |   |

const move5 = new Move(Row.ROW_B, Column.Col_1);
const makeMoveParams5 = new MakeMoveParams(playerTwoAddress, move5);
const tx6GameStateFromScriptData = new GameStateFactory().fromScriptData(tx6GameStateAsScriptData);
const tx7Command = new MakeMoveCommand(tx6GameStateFromScriptData, makeMoveParams5);
const tx7GameState = Game.handleActionCommand(tx7Command);
const tx7GameStateAsScriptData = tx7GameState.toScriptData();
console.log("##########################");
console.log(`tx7 [${tx7GameState.constructor.name}]: ${JSON.stringify(tx7GameState, null, 2)}`);
console.log(
  `tx7 scriptdata : ${tx7GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 8
// Player 2 Move B 1

// A _O_|_O_|_X_
// B _O_|_X_|_X_
// C    |   |

const move6 = new Move(Row.ROW_A, Column.Col_3);
const makeMoveParams6 = new MakeMoveParams(playerOneAddress, move6);
const tx7GameStateFromScriptData = new GameStateFactory().fromScriptData(tx7GameStateAsScriptData);
const tx8Command = new MakeMoveCommand(tx7GameStateFromScriptData, makeMoveParams6);
const tx8GameState = Game.handleActionCommand(tx8Command);
const tx8GameStateAsScriptData = tx8GameState.toScriptData();
console.log("##########################");
console.log(`tx8 [${tx8GameState.constructor.name}]: ${JSON.stringify(tx8GameState, null, 2)}`);
console.log(
  `tx8 scriptdata : ${tx8GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

const tx8GameStateFromScriptData = new GameStateFactory().fromScriptData(tx8GameStateAsScriptData);
const tx9Command = new ClaimTieCommand(tx8GameStateFromScriptData);
const payout = Game.handleEndGameActionCommand(tx9Command);
// TX 9 Claim Tie
console.log("##########################");
console.log(payout);
