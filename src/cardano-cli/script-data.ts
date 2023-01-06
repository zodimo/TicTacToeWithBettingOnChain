import * as csl from "@emurgo/cardano-serialization-lib-nodejs";

/*
 * The schema specifies that ALL keys and ALL values must be contained in a JSON map with 2 cases:
 * 1. For ConstrPlutusData there must be two fields "constructor" contianing a number and "fields" containing its fields
 *    e.g. { "constructor": 2, "fields": [{"int": 2}, {"list": [{"bytes": "CAFEF00D"}]}]}
 * 2. For all other cases there must be only one field named "int", "bytes", "list" or "map"
 *    Integer's value is a JSON number e.g. {"int": 100}
 *    Bytes' value is a hex string representing the bytes WITHOUT any prefix e.g. {"bytes": "CAFEF00D"}
 *    Lists' value is a JSON list of its elements encoded via the same schema e.g. {"list": [{"bytes": "CAFEF00D"}]}
 *    Maps' value is a JSON list of objects, one for each key-value pair in the map, with keys "k" and "v"
 *          respectively with their values being the plutus datum encoded via this same schema
 *          e.g. {"map": [
 *              {"k": {"int": 2}, "v": {"int": 5}},
 *              {"k": {"map": [{"k": {"list": [{"int": 1}]}, "v": {"bytes": "FF03"}}]}, "v": {"list": []}}
 *          ]}
 */

export interface ToScriptDataSerialise {
  toScriptData(): Data;
}

export abstract class FromScriptDataFactory<T> {
  abstract fromScriptData(data: Data): T;
}

export enum ScriptDataJsonSchema {
  // @see node_modules/@emurgo/cardano-serialization-lib-nodejs/cardano_serialization_lib.js:815
  ScriptDataJsonNoSchema = 0,
  ScriptDataJsonDetailedSchema = 1,
}

abstract class ScriptData {
  abstract toPlutusData(): csl.PlutusData;

  toScriptDataJson(schema: number): string {
    // constuctor can only be details
    return this.toPlutusData().to_json(schema);
  }
}

export class DataBytes extends ScriptData {
  private constructor(private bytes: Uint8Array) {
    super();
  }

  static fromBytes(bytes: Uint8Array): DataBytes {
    return new DataBytes(bytes);
  }

  static fromString(value: string): DataBytes {
    const bytes = Buffer.from(value, "utf-8");
    return new DataBytes(bytes);
  }

  static fromHexString(value: string): DataBytes {
    if (!(value.length % 2 === 0 && /^[0-9A-F]*$/i.test(value))) {
      throw new Error(`Not a valid hex value: ${value}`);
    }
    const bytes = Buffer.from(value, "hex");
    return new DataBytes(bytes);
  }

  toString(): string {
    return this.bytes.toString();
  }

  toHex(): string {
    return Buffer.from(this.bytes).toString("hex");
  }

  toPlutusData(): csl.PlutusData {
    return csl.PlutusData.new_bytes(this.bytes);
  }
}

export class DataNumber extends ScriptData {
  constructor(private int: number) {
    super();
  }

  static fromNumber(value: number): DataNumber {
    return new DataNumber(value);
  }

  toPlutusData(): csl.PlutusData {
    return csl.PlutusData.new_integer(csl.BigInt.from_str(this.int.toString()));
  }

  getValue(): number {
    return this.int;
  }
}

export class DataArray extends ScriptData {
  constructor(private list: Array<Data>) {
    super();
  }

  static fromArray(value: Array<Data>): DataArray {
    return new DataArray(value);
  }

  toPlutusData(): csl.PlutusData {
    const plutusList = csl.PlutusList.new();
    this.list.forEach((data) => {
      plutusList.add(data.toPlutusData());
    });
    return csl.PlutusData.new_list(plutusList);
  }

  getArray(): Array<Data> {
    return this.list;
  }
}

class DataMap extends ScriptData {
  constructor(private map: Map<Data, Data>) {
    super();
  }

  static fromMap(value: Map<Data, Data>): DataMap {
    return new DataMap(value);
  }

  toPlutusData(): csl.PlutusData {
    const plutusMap = csl.PlutusMap.new();
    for (const [key, value] of this.map.entries()) {
      plutusMap.insert(key.toPlutusData(), value.toPlutusData());
    }
    return csl.PlutusData.new_map(plutusMap);
  }

  getMap(): Map<Data, Data> {
    return this.map;
  }
}

export class DataConstr extends ScriptData {
  constructor(private index: number, private fields: Data[]) {
    super();
  }

  static from(index: number, fields: Data[]): DataConstr {
    return new DataConstr(index, fields);
  }

  static unit(): DataConstr {
    return new DataConstr(0, []);
  }

  toPlutusData(): csl.PlutusData {
    const plutusList = csl.PlutusList.new();
    this.fields.forEach((data) => {
      plutusList.add(data.toPlutusData());
    });

    const constrPlutusData = csl.ConstrPlutusData.new(csl.BigNum.from_str(this.index.toString()), plutusList);
    return csl.PlutusData.new_constr_plutus_data(constrPlutusData);
  }

  getIndex(): number {
    return this.index;
  }

  getFields(): Data[] {
    return this.fields;
  }
}

export const fromJson: (json: string) => Data = (json) => {
  const fromJsonObject: (jsonObject: any) => Data = (jsonObject) => {
    //bytes
    if ("bytes" in jsonObject) {
      return DataBytes.fromHexString(jsonObject.bytes);
    }
    //int
    if ("int" in jsonObject) {
      return new DataNumber(jsonObject.int);
    }
    //list
    if ("list" in jsonObject) {
      const values = new Array();
      if (jsonObject.list) {
        jsonObject.list.forEach((data: any) => {
          values.push(fromJsonObject(data));
        });
      }
      return new DataArray(values);
    }
    //map
    if ("map" in jsonObject) {
      const map = new Map();
      for (const [key, value] of jsonObject.map.entries()) {
        map.set(fromJsonObject(key), fromJsonObject(value));
      }
      return new DataMap(map);
    }
    //constuctor
    if ("constructor" in jsonObject && "fields" in jsonObject) {
      const dataFields: Data[] = [];
      if (jsonObject.fields) {
        jsonObject.fields.forEach((data: any) => {
          dataFields.push(fromJsonObject(data));
        });
      }
      const constuctorIndex = +jsonObject["constructor"];

      return new DataConstr(constuctorIndex, dataFields);
    }

    throw new Error(`Could not create Data from this object: ${JSON.stringify(jsonObject)}`);
  };

  const scriptDataJson = JSON.parse(json);

  return fromJsonObject(scriptDataJson);
};

export type Data = DataBytes | DataNumber | DataArray | DataMap | DataConstr;
