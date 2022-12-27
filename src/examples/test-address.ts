const value =
  "265bc95a2b2fe28bd75c12eb172b8fa89923e465f50aecc963ae56f374b94711#1";
const regex = new RegExp(
  /^(?<transactionHash>[a-f0-9]+)#(?<outputIndex>\d+)/,
  "g"
);
const matches = regex.exec(value);

console.log(matches);

// const paragraph = "The quick brown fox jumps over the lazy dog. It barked.";

// const capturingRegex = /(?<animal>fox|cat) jumps over/;
// const found = paragraph.match(capturingRegex);
// console.log(found.groups); // {animal: "fox"}
