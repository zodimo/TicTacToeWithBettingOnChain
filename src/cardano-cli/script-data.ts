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

abstract class ScriptData {
  abstract asPlutusData(): csl.PlutusData;
  toScriptDataJson(schema: number) {
    // constuctor can only be details
    return this.asPlutusData().to_json(schema);
  }
}

export class DataBytes extends ScriptData {
  private constructor(private bytes: Uint8Array) {
    super();
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

  asPlutusData(): csl.PlutusData {
    throw csl.PlutusData.new_bytes(this.bytes);
  }
}

export class DataNumber extends ScriptData {
  constructor(private value: number) {
    super();
  }
  static fromNumber(value: number): DataNumber {
    return new DataNumber(value);
  }
  asPlutusData(): csl.PlutusData {
    return csl.PlutusData.new_integer(csl.BigInt.from_str(this.value.toString()));
  }

  getValue(): number {
    return this.value;
  }
}

export class DataArray extends ScriptData {
  constructor(private values: Array<Data>) {
    super();
  }

  static fromArray(value: Array<Data>): DataArray {
    return new DataArray(value);
  }

  asPlutusData(): csl.PlutusData {
    const plutusList = csl.PlutusList.new();
    this.values.forEach((data) => {
      plutusList.add(data.asPlutusData());
    });
    return csl.PlutusData.new_list(plutusList);
  }
  getArray(): Array<Data> {
    return this.values;
  }
}

class DataMap extends ScriptData {
  constructor(private value: Map<Data, Data>) {
    super();
  }

  static fromMap(value: Map<Data, Data>): DataMap {
    return new DataMap(value);
  }

  asPlutusData(): csl.PlutusData {
    const plutusMap = csl.PlutusMap.new();
    for (const [key, value] of this.value.entries()) {
      plutusMap.insert(key.asPlutusData(), value.asPlutusData());
    }
    return csl.PlutusData.new_map(plutusMap);
  }
  getMap(): Map<Data, Data> {
    return this.value;
  }
}

export class DataConstr extends ScriptData {
  constructor(private index: number, private fields: Data[]) {
    super();
  }

  static from(index: number, fields: Data[]): DataConstr {
    return new DataConstr(index, fields);
  }

  asPlutusData(): csl.PlutusData {
    const plutusList = csl.PlutusList.new();
    this.fields.forEach((data) => {
      plutusList.add(data.asPlutusData());
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
    if (jsonObject.hasOwnProperty("bytes")) {
      return DataBytes.fromHexString(jsonObject.bytes);
    }
    //int
    if (jsonObject.hasOwnProperty("int")) {
      return new DataNumber(jsonObject.int);
    }
    //list
    if (jsonObject.hasOwnProperty("list")) {
      const values = new Array();
      if (jsonObject.list) {
        jsonObject.list.forEach((data: any) => {
          values.push(fromJsonObject(data));
        });
      }
      return new DataArray(values);
    }
    //map
    if (jsonObject.hasOwnProperty("map")) {
      const map = new Map();
      for (const [key, value] of jsonObject.map.entries()) {
        map.set(fromJsonObject(key), fromJsonObject(value));
      }
      return new DataMap(map);
    }
    //constuctor
    if (jsonObject.hasOwnProperty("constructor") && jsonObject.hasOwnProperty("fields")) {
      const dataFields: Data[] = [];
      if (jsonObject.fields) {
        jsonObject.fields.forEach((data: any) => {
          dataFields.push(fromJsonObject(data));
        });
      }
      const constuctorIndex = +scriptDataJson["constructor"];

      return new DataConstr(constuctorIndex, dataFields);
    }

    throw new Error(`Could not create Data from this object: ${JSON.stringify(jsonObject)}`);
  };

  const scriptDataJson: Object = JSON.parse(json);

  return fromJsonObject(scriptDataJson);
};

export type Data = DataBytes | DataNumber | DataArray | DataMap | DataConstr;
