import { DataConstr, fromJson } from "../cardano-cli/script-data.js";
import {StartGameData} from "./emurgo-datum.js";
const scriptDataJson: string = `{"constructor":0,"fields":[{"bytes":"7370656369616c"},{"int":50},{"int":30}]}`;

const data = fromJson(scriptDataJson) as DataConstr;

const startGameData=StartGameData.fromScriptData(data);


console.log(startGameData);
