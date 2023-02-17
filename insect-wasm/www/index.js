import * as insect from "insect-wasm";

const runButton = document.getElementById("run");
const codeTextarea = document.getElementById("code");

runButton.addEventListener("click", event => {
  const output = insect.interpret(codeTextarea.value);
  document.getElementById("output").innerHTML = output;
});
