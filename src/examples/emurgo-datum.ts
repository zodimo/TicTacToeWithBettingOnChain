import assert from "assert";
import * as ScriptData from "../cardano-cli/script-data.js";

// @see https://docs.cardano.org/cardano-components/cardano-serialization-lib/
// @see https://github.com/Emurgo/cardano-serialization-lib/tree/master/doc/getting-started

// plutus types
// data Row = Row_A| Row_B | Row_C deriving Show
// data Column = Col_1 | Col_2 | Col_3 deriving Show
// data Move = Move Row Column deriving Show

// PlutusTx.makeIsDataIndexed ''Row [('Row_A,0),('Row_B,1),('Row_C,2)]
// PlutusTx.makeIsDataIndexed ''Column [('Col_1,0),('Col_2,1),('Col_3,2)]
// PlutusTx.makeIsDataIndexed ''Move [('Move,0)]

export enum Row {
  Row_A,
  Row_B,
  Row_C,
}
export enum Column {
  Col_1,
  Col_2,
  Col_3,
}
export class Move {
  constructor(private readonly row: Row, private readonly column: Column) {}

  toScriptData(): ScriptData.Data {
    let rowData = null;
    switch (this.row) {
      case Row.Row_A:
        rowData = ScriptData.DataConstr.from(0, []);
        break;
      case Row.Row_B:
        rowData = ScriptData.DataConstr.from(1, []);
        break;
      case Row.Row_C:
        rowData = ScriptData.DataConstr.from(2, []);
        break;
      default:
        throw new Error(`Non-existent row in switch: ${this.row}`);
    }

    let columnData = null;
    switch (this.column) {
      case Column.Col_1:
        columnData = ScriptData.DataConstr.from(0, []);
        break;
      case Column.Col_2:
        columnData = ScriptData.DataConstr.from(1, []);
        break;
      case Column.Col_3:
        columnData = ScriptData.DataConstr.from(2, []);
        break;

      default:
        throw new Error(`Non-existent row in switch: ${this.column}`);
    }

    return ScriptData.DataConstr.from(0, [rowData, columnData]);
  }

  toScriptDataJson(schema: number) {
    return this.toScriptData().toScriptDataJson(schema);
  }
}

export class UnitData {
  toScriptData(): ScriptData.Data {
    return ScriptData.DataConstr.unit();
  }
  toScriptDataJson(schema: number) {
    return this.toScriptData().toScriptDataJson(schema);
  }
}

export class StartGameData {
  constructor(
    public readonly gameName: string,
    public readonly gameBetInAda: number,
    public readonly deadlineInMins: number = 30
  ) {}

  static fromScriptData(data: ScriptData.Data): StartGameData {
    assert.equal(data instanceof ScriptData.DataConstr, true);
    let validData: ScriptData.DataConstr = data as ScriptData.DataConstr;
    assert.equal(validData.getIndex(), 0, "Extects constructor index of 0");
    assert.equal(validData.getFields().length, 3, "Extects 3 fields");
    assert.equal(validData.getFields()[0] instanceof ScriptData.DataBytes, true);
    const gameNameData: ScriptData.DataBytes = validData.getFields()[0] as ScriptData.DataBytes;
    assert.equal(validData.getFields()[1] instanceof ScriptData.DataNumber, true);
    const gameBetInAda: ScriptData.DataNumber = validData.getFields()[1] as ScriptData.DataNumber;
    assert.equal(validData.getFields()[2] instanceof ScriptData.DataNumber, true);
    const deadlineInMins: ScriptData.DataNumber = validData.getFields()[2] as ScriptData.DataNumber;

    return new StartGameData(gameNameData.toString(), gameBetInAda.getValue(), deadlineInMins.getValue());
  }

  toScriptData(): ScriptData.Data {
    return ScriptData.DataConstr.from(0, [
      ScriptData.DataBytes.fromString(this.gameName),
      ScriptData.DataNumber.fromNumber(this.gameBetInAda),
      ScriptData.DataNumber.fromNumber(this.deadlineInMins),
    ]);
  }

  toScriptDataJson(schema: number): string {
    // constuctor can only be details
    return this.toScriptData().toScriptDataJson(schema);
  }
}
