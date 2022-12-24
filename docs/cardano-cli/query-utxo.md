```javascript
  /**
   * @param {paymentAddr} address
   * @returns {object}
   */
  queryUtxo(address) {
    const UID = Math.random().toString(36).substr(2, 9);
    const utxosPath= `${this.dir}/tmp/utxo_${UID}.json`;  
    execSync(`${this.cliPath} query utxo \
            --${this.network} \
            --address ${address} \
            --cardano-mode \
            --out-file ${utxosPath}
            `);

    const utxos=JSON.parse(execSync(`cat ${utxosPath}`));
    fs.rmSync(utxosPath);
    return utxos;
  }
```