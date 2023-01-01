import * as a from "@emurgo/cardano-serialization-lib-nodejs";
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

  get data(): a.ConstrPlutusData {
    let rowData = null;
    switch (this.row) {
      case Row.Row_A:
        rowData = a.ConstrPlutusData.new(a.BigNum.from_str("0"), a.PlutusList.new());
        break;
      case Row.Row_B:
        rowData = a.ConstrPlutusData.new(a.BigNum.from_str("1"), a.PlutusList.new());
        break;
      case Row.Row_C:
        rowData = a.ConstrPlutusData.new(a.BigNum.from_str("2"), a.PlutusList.new());
        break;
      default:
        throw new Error(`Non-existent row in switch: ${this.row}`);
    }

    let columnData = null;
    switch (this.column) {
      case Column.Col_1:
        columnData = a.ConstrPlutusData.new(a.BigNum.from_str("0"), a.PlutusList.new());
        break;
      case Column.Col_2:
        columnData = a.ConstrPlutusData.new(a.BigNum.from_str("1"), a.PlutusList.new());
        break;
      case Column.Col_3:
        columnData = a.ConstrPlutusData.new(a.BigNum.from_str("2"), a.PlutusList.new());
        break;

      default:
        throw new Error(`Non-existent row in switch: ${this.column}`);
    }

    const plutusList = a.PlutusList.new();
    plutusList.add(a.PlutusData.new_constr_plutus_data(rowData));
    plutusList.add(a.PlutusData.new_constr_plutus_data(columnData));
    return a.ConstrPlutusData.new(a.BigNum.from_str("0"), plutusList);
  }

  toScriptDataJson(schema: number) {
    return a.PlutusData.from_hex(this.data.to_hex()).to_json(schema);
  }
}

export class UnitData {
  get data(): a.ConstrPlutusData {
    return a.ConstrPlutusData.new(a.BigNum.from_str("0"), a.PlutusList.new());
  }
  toScriptDataJson(schema: number) {
    return a.PlutusData.from_hex(this.data.to_hex()).to_json(schema);
  }
}

export class PlutusScriptDataJsonSchema {
  // @see node_modules/@emurgo/cardano-serialization-lib-nodejs/cardano_serialization_lib.js:815
  static get ScriptDataJsonNoSchema() {
    return 0;
  }
  static get ScriptDataJsonDetailedSchema() {
    return 1;
  }
}

// const move = new Move(Row.Row_A,Column.Col_3);
// console.log(a.PlutusData.from_hex(move.data.to_hex()).to_json(PlutusScriptDataJsonSchema.ScriptDataJsonNoSchema));

//  data StartGameData = StartGameData
//  { gameBetInAda:: Integer
//  , deadlineInMins:: Integer
//  }

// PlutusTx.makeIsDataIndexed ''StartGameData [('StartGameData,0)]

export class StartGameData {
  constructor(
    public readonly gameName: string,
    public readonly gameBetInAda: Number,
    public readonly deadlineInMins: Number = 30
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

  get data(): a.ConstrPlutusData {
    const plutusList = a.PlutusList.new();
    // string variable
    plutusList.add(a.PlutusData.new_bytes(Buffer.from(this.gameName, "utf-8")));
    plutusList.add(a.PlutusData.new_integer(a.BigInt.from_str(this.gameBetInAda.toString())));
    plutusList.add(a.PlutusData.new_integer(a.BigInt.from_str(this.deadlineInMins.toString())));

    return a.ConstrPlutusData.new(a.BigNum.from_str("0"), plutusList);
  }

  toScriptDataJson(schema: number): string {
    // constuctor can only be details
    return a.PlutusData.from_hex(this.data.to_hex()).to_json(schema);
  }
}

// const startGameData:StartGameData=new StartGameData(10,15);
// console.log(startGameData.data.to_hex());
// console.log(startGameData.toScriptDataJson());
