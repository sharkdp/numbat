import { setup_panic_hook, Numbat } from "numbat-wasm";

setup_panic_hook();

var numbat = Numbat.new();

const runButton = document.getElementById("run");
const codeTextarea = document.getElementById("code");

/*runButton.addEventListener("click", event => {
  const output = numbat.interpret(codeTextarea.value);
  document.getElementById("output").innerHTML = output;
});*/

// Load KeyboardEvent polyfill for old browsers
keyboardeventKeyPolyfill.polyfill();
  
var clearCommands = ["clear", "cls", "quit", "exit"];

function updateUrlQuery(query) {
  /*url = new URL(window.location);
  if (query == null) {
    url.searchParams.delete('q');
  } else {
    url.searchParams.set('q', query);
  }

  history.replaceState(null, null, url);*/
}

function interpret(line) {
  // Skip empty lines or line comments
  var lineTrimmed = line.trim();
  if (lineTrimmed === "" || lineTrimmed[0] === "#") {
    return;
  }

  // Run Numbat
  const output = numbat.interpret(line);

  // Handle shell commands
  /*if (clearCommands.indexOf(res.msgType) >= 0) {
    // Clear screen:
    this.clear();
    return;
  } else if (res.msgType === "quit") {
    // Treat as reset:
    this.clear();
    insectEnv = Insect.initialEnvironment;
    return;
  } else if (res.msgType === "copy") {
    // Copy result to clipboard:
    if (res.msg === "") {
      res.msg = "\nNo result to copy.\n";
    } else {
      navigator.clipboard.writeText(res.msg);
      res.msg = "\nCopied result '" + res.msg + "' to clipboard.\n";
    }
  }*/
  updateUrlQuery(line);

  return output;
}

function colored(col, str) {
  return "[[;#" + col + ";]" + str + "]";
}


$(document).ready(function() {
  var term = $('#terminal').terminal(interpret, {
    greetings: false,
    name: "terminal",
    height: 550,
    prompt: "[[;;;prompt]>>> ]",
    // clear: false, // do not include 'clear' command
    // exit: false, // do not include 'exit' command
    checkArity: false,
    historySize: 200,
    historyFilter(line) {
      return line.trim() !== "";
    },
    completion(inp, cb) {
      /*var identifiers = Insect.identifiers(insectEnv);

      var keywords =
        identifiers.concat(Insect.functions(insectEnv), Insect.supportedUnits, Insect.commands);

      cb(keywords.sort());*/
      cb([]);
    },
    onClear() {
      updateUrlQuery(null);
    }
  });

  // evaluate expression in query string if supplied (via opensearch)
  if (location.search) {
    var queryParams = new URLSearchParams(location.search);
    if (queryParams.has("q")) {
      term.exec(queryParams.get("q"));
    }
  }
});
