import * as numbat from "numbat-wasm";

const runButton = document.getElementById("run");
const codeTextarea = document.getElementById("code");

runButton.addEventListener("click", event => {
  const output = numbat.interpret(codeTextarea.value);
  document.getElementById("output").innerHTML = output;
});
