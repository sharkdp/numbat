import init, {
  setup_panic_hook,
  Numbat,
  FormatType,
} from "./pkg/numbat_wasm.js";

async function main() {
  await init();

  setup_panic_hook();

  const num_editors = 7;
  for (var i = 1; i <= num_editors; i++) {
    setupEditor(i);
  }

  // Evaluate all editors once. Do this after initialization
  // such that we can call ace.edit(…) internally without
  // initializing an empty editor.
  for (var i = 1; i <= num_editors; i++) {
    numbatEvaluate(i);
  }
}

main();

function interpret(input, with_prelude) {
  var numbat = Numbat.new(with_prelude, false, FormatType.Html);
  if (with_prelude) {
    numbat.interpret("use extra::astronomy");
  }

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
              "\\b(?:per|to|let|fn|where|and|dimension|unit|use|long|short|both|none|print|assert|assert_eq|type|if|then|else|true|false)\\b",
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

function getOutputId(id) {
  return (id == 4 || id == 5) ? "output4and5" : "output" + id.toString();
}

function getCode(id) {
  if (id == 4 || id == 5) {
    return ace.edit("editor4").getValue() + "\n" + ace.edit("editor5").getValue();
  } else {
    return ace.edit("editor" + id.toString()).getValue();
  }
}

function numbatEvaluate(id) {
  const code = getCode(id);
  const output_id = getOutputId(id);
  const with_prelude = (id == 4 || id == 5) ? false : true;

  const result = interpret(code, with_prelude).replace(/\s*$/, "");

  document.getElementById(output_id).innerHTML = result;
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
    numbatEvaluate(id);
  }

  var debouncedEvaluate = debounce(evaluate, 500);

  editor.getSession().on("change", debouncedEvaluate);

  editor.commands.addCommand({
    name: "evaluateCode",
    bindKey: { win: "Ctrl-Enter", mac: "Command-Enter" },
    exec: function (editor) {
      numbatEvaluate(id);
    },
    readOnly: true,
  });
}
