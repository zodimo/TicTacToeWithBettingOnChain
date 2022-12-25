import { Data, Constr } from "lucid-cardano";

/**
 * Player 1 start game ..
 * datum betInAda 10 Ada
 * deadlineInMins = 30
 *
 * scriptAddress
 */

// plutus types
// data Row = Row_A| Row_B | Row_C deriving Show
// data Column = Col_1 | Col_2 | Col_3 deriving Show
// data Move = Move Row Column deriving Show

// PlutusTx.makeIsDataIndexed ''Row [('Row_A,0),('Row_B,1),('Row_C,2)]
// PlutusTx.makeIsDataIndexed ''Column [('Col_1,0),('Col_2,1),('Col_3,2)]
// PlutusTx.makeIsDataIndexed ''Move [('Move,0)]

const rowA: Data = new Constr(0, []);

const rowB: Data = new Constr(1, []);

const rowC: Data = new Constr(2, []);

const col1: Data = new Constr(0, []);

const col2: Data = new Constr(1, []);

const col3: Data = new Constr(0, []);

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

  get data(): Data {
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
    return new Constr(0, [rowData, columnData]);
  }
}

const move = new Move(Row.Row_A, Column.Col_3);

// @see https://docs.cardano.org/cardano-components/cardano-serialization-lib/
// @see https://github.com/Emurgo/cardano-serialization-lib/tree/master/doc/getting-started
console.log(new Move(Row.Row_A, Column.Col_3).data);

const t = Data.to(move.data);
console.log(t);
