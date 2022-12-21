import * as a from "@emurgo/cardano-serialization-lib-nodejs";

// plutus types
// data Row = Row_A| Row_B | Row_C deriving Show
// data Column = Col_1 | Col_2 | Col_3 deriving Show
// data Move = Move Row Column deriving Show

// PlutusTx.makeIsDataIndexed ''Row [('Row_A,0),('Row_B,1),('Row_C,2)]
// PlutusTx.makeIsDataIndexed ''Column [('Col_1,0),('Col_2,1),('Col_3,2)]
// PlutusTx.makeIsDataIndexed ''Move [('Move,0)]


// @see https://docs.cardano.org/cardano-components/cardano-serialization-lib/
// @see https://github.com/Emurgo/cardano-serialization-lib/tree/master/doc/getting-started

const rowA: a.ConstrPlutusData = a.ConstrPlutusData.new(
  a.BigNum.from_str("0"),
  a.PlutusList.new()
);

const rowB: a.ConstrPlutusData = a.ConstrPlutusData.new(
  a.BigNum.from_str("1"),
  a.PlutusList.new()
);

const rowC: a.ConstrPlutusData = a.ConstrPlutusData.new(
  a.BigNum.from_str("2"),
  a.PlutusList.new()
);

const col1: a.ConstrPlutusData = a.ConstrPlutusData.new(
  a.BigNum.from_str("0"),
  a.PlutusList.new()
);

const col2: a.ConstrPlutusData = a.ConstrPlutusData.new(
  a.BigNum.from_str("1"),
  a.PlutusList.new()
);

const col3: a.ConstrPlutusData = a.ConstrPlutusData.new(
  a.BigNum.from_str("2"),
  a.PlutusList.new()
);

enum Row {
  Row_A,
  Row_B,
  Row_C,
}
enum Column {
  Col_1,
  Col_2,
  Col_3,
}
class Move {
  constructor(private readonly row: Row, private readonly column: Column) {}

  get data(): a.ConstrPlutusData {
    let rowData = null;
    switch (this.row) {
      case Row.Row_A:
        rowData = rowA;
        break;
      case Row.Row_B:
        rowData = rowB;
        break;
      case Row.Row_C:
        rowData = rowC;
        break;
      default:
        throw new Error(`Non-existent row in switch: ${this.row}`);
    }

    let columnData = null;
    switch (this.column) {
      case Column.Col_1:
        columnData = col1;
        break;
      case Column.Col_2:
        columnData = col2;
        break;
      case Column.Col_3:
        columnData = col3;
        break;

      default:
        throw new Error(`Non-existent row in switch: ${this.column}`);
    }

    const plutusList=a.PlutusList.new()
    plutusList.add(a.PlutusData.new_constr_plutus_data(rowData));
    plutusList.add(a.PlutusData.new_constr_plutus_data(columnData));
    return a.ConstrPlutusData.new(
      a.BigNum.from_str("0"),
      plutusList
      )
  }
}

const move = new Move(Row.Row_A,Column.Col_3);
console.log(move.data.to_hex());

