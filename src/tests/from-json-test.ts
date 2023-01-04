import { GameStateFactory } from "../app/game-data.js";
import { fromJson } from "../cardano-cli/script-data.js";

const gameStateJsonString = `{"constructor":2,"fields":[{"bytes":"506c617965723141646472657373"},{"bytes":"506c617965723241646472657373"},{"int":50},{"int":30},{"int":1672784251817},{"bytes":"506c617965723141646472657373"},{"constructor":0,"fields":[{"list":[{"constructor":0,"fields":[{"bytes":"506c617965723241646472657373"},{"constructor":0,"fields":[{"constructor":0,"fields":[]},{"constructor":0,"fields":[]}]}]}]}]}]}`;
const gameStateScriptData = fromJson(gameStateJsonString);
const tx3GameStateFromScriptData = new GameStateFactory().fromScriptData(gameStateScriptData);
