import init, {
  setup_panic_hook,
  Numbat,
  FormatType,
} from "./pkg/numbat_wasm.js";

async function main() {
  await init();

  setup_panic_hook();

  for (var i = 1; i <= 4; i++) {
    setupEditor(i);
  }
}

main();

function interpret(input) {
  var numbat = Numbat.new(false, FormatType.Html);
  numbat.interpret("use extra::astronomy");

  var result = numbat.interpret(input);

  return result.output;
}

ace.config.set("basePath", "https://cdnjs.cloudflare.com/ajax/libs/ace/1.9.6/");

ace.define(
  "ace/mode/numbat_highlight_rules",
  function (require, exports, module) {
    var oop = require("ace/lib/oop");
    var TextHighlightRules =
      require("ace/mode/text_highlight_rules").TextHighlightRules;

    var NumbatHighlightRules = function () {
      this.$rules = {
        start: [
          {
            token: "comment",
            regex: "#.*$",
          },
          {
            token: "keyword",
            regex:
              "\\b(?:per|to|let|fn|dimension|unit|use|long|short|both|none|print|assert|assert_eq|type|if|then|else|true|false)\\b",
          },
          {
            token: "constant.numeric",
            regex:
              "\\b(?:[0-9]+(?:[._][0-9]+)*" +
              "|0x[0-9A-Fa-f]+|0o[0-7]+|0b[01]+(?:_[01]+)*" +
              "|\\.[0-9]+(?:[eE][-+]?[0-9]+)?" +
              "|[0-9]+[eE][-+]?[0-9]+)\\b",
          },
          // {
          //     token: "variable",
          //     regex: "\\b[a-z\u00B0\u0394\u03C0\u00B5][A-Za-z0-9_\u00B0\u0394\u03C0\u00B5]*\\b"
          // },
          {
            token: "support.type",
            regex: "\\b[A-Z][A-Za-z]*\\b",
          },
          {
            token: "string",
            regex: '"[^"]*"',
          },
          {
            token: "keyword.operator",
            regex: "\\+|-|\\*|/|\\^|➞|→|:|÷|×|≤|≥|≠|<|>|²|³|\\(|\\)",
          },
          {
            token: "meta.decorator",
            regex: "@[\\w_]+",
          },
        ],
      };
    };

    oop.inherits(NumbatHighlightRules, TextHighlightRules);

    exports.NumbatHighlightRules = NumbatHighlightRules;
  },
);

ace.define("ace/mode/numbat", function (require, exports, module) {
  var oop = require("ace/lib/oop");
  var TextMode = require("ace/mode/text").Mode;
  var NumbatHighlightRules =
    require("ace/mode/numbat_highlight_rules").NumbatHighlightRules;

  var Mode = function () {
    this.HighlightRules = NumbatHighlightRules;
  };
  oop.inherits(Mode, TextMode);

  (function () {
    this.lineCommentStart = "#";

    this.$id = "ace/mode/numbat";
  }).call(Mode.prototype);

  exports.Mode = Mode;
});

function numbatEvaluate(editor, outputId) {
  const code = editor.getValue();

  const result = interpret(code).replace(/\s*$/, "");

  document.getElementById(outputId).innerHTML = result;
}

function debounce(func, wait, immediate) {
  var timeout;
  return function () {
    var context = this,
      args = arguments;
    var later = function () {
      timeout = null;
      if (!immediate) func.apply(context, args);
    };
    var callNow = immediate && !timeout;
    clearTimeout(timeout);
    timeout = setTimeout(later, wait);
    if (callNow) func.apply(context, args);
  };
}

function setupEditor(id) {
  const output_id = "output" + id.toString();
  var editor = ace.edit("editor" + id.toString(), {
    mode: "ace/mode/numbat",
    showPrintMargin: false,
    showGutter: true,
    highlightActiveLine: false,
    highlightGutterLine: false,
    maxLines: 20,
  });

  editor.renderer.setPadding(10);

  // Move cursor to the end of the input
  const count = editor.session.getLength();
  const lastLine = editor.session.getLine(count - 1);
  editor.session.selection.moveCursorTo(count - 1, lastLine.length);

  function evaluate() {
    numbatEvaluate(editor, output_id);
  }

  var debouncedEvaluate = debounce(evaluate, 500);

  editor.getSession().on("change", debouncedEvaluate);

  editor.commands.addCommand({
    name: "evaluateCode",
    bindKey: { win: "Ctrl-Enter", mac: "Command-Enter" },
    exec: function (editor) {
      numbatEvaluate(editor, output_id);
    },
    readOnly: true,
  });

  evaluate();
}
