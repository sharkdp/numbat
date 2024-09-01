import init, {
    setup_panic_hook,
    Numbat,
    FormatType,
} from "./pkg/numbat_wasm.js";

async function main() {
    await init();

    setup_panic_hook();

    initializeEditor();
    initializeSplit();
    initializeThemeToggle();
}

main();

function interpret(input) {
    var numbat = Numbat.new(true, false, FormatType.Html);

    let parts = input.split("\n\n");

    let results = parts.map((part) => {
        let res = part.match(/\n/g);
        let num_newlines = res ? res.length : 0;

        let brs = "<br>".repeat(num_newlines);
        let output = part.trim().length > 0 ? numbat.interpret(part).output : "";
        let result = "";

        if (output.trim().length === 0) {
            result = brs + "<br>";
        } else {
            result = brs + "<div>" + output  + "</div>";
        }

        return result;
    });

    return results.join("<br>");
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

function initializeEditor() {
    const editor = ace.edit("editor", {
        mode: "ace/mode/numbat",
        showPrintMargin: false,
        showGutter: true,
        highlightActiveLine: false,
        highlightGutterLine: false,
        scrollPastEnd: 0,
    });

    function evaluate() {
        let code = editor.getValue();

        let output = interpret(code);

        document.getElementById("results").innerHTML = output;
    }

    var debouncedEvaluate = debounce(evaluate, 500);

    editor.getSession().on("change", debouncedEvaluate);

    evaluate();

    editor.focus();
}

function initializeSplit() {
    Split(['#editor', '#results'], {
        gutterSize: 15,
    });
}

function initializeThemeToggle() { 
    const colorSchemeQueryList = window.matchMedia('(prefers-color-scheme: dark)');
    
    let toggleImg = document.querySelector("#theme-toggle img");
    toggleImg.src = colorSchemeQueryList.matches ? "assets/sun.svg" : "assets/moon.svg";

    function toggleTheme() {
        let classList = document.querySelector(":root").classList;
        let isToggledOn = classList.toggle("alt-theme");

        if (colorSchemeQueryList.matches) {
            toggleImg.src = isToggledOn ? "assets/moon.svg" : "assets/sun.svg";
        } else {
            toggleImg.src = isToggledOn ? "assets/sun.svg" : "assets/moon.svg";
        }
    } 

    document.getElementById("theme-toggle").addEventListener("click", toggleTheme);
}