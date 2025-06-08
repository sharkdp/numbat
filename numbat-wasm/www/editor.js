import init, {
    setup_panic_hook,
    Numbat,
    FormatType,
} from "./pkg/numbat_wasm.js";

let editor;

async function main() {
    await init();
    setup_panic_hook();
    
    // Configure Monaco Editor loader
    require.config({ 
        paths: { 
            'vs': 'https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.45.0/min/vs' 
        } 
    });
    
    require(['vs/editor/editor.main'], function() {
        initializeMonacoLanguage();
        initializeEditor();
        initializeSplit();
    });
}

main();

function interpret(input) {
    var numbat = Numbat.new(true, false, FormatType.Html);

    let parts = input.split("\n\n");

    let results = parts.map((part) => {
        let res = part.match(/\n/g);
        let num_newlines = res ? res.length : 0;

        let brs = "<br>".repeat(num_newlines);
        let interpretOutput = numbat.interpret(part);
        
        let output = part.trim().length > 0 ? interpretOutput.output.trim() : "";
        
        if (interpretOutput.is_error) {
            output = output.replace(/<(input:\d+)>/gm, "&lt;$1&gt;")
        }
        let result = "";

        if (output.trim().length === 0) {
            result = brs + "<br>";
        } else {
            result = brs + "<div>" + output  + "</div>";
        }

        interpretOutput.free();
        return result;
    });

    return results.join("<br>");
}

function initializeMonacoLanguage() {
    // Register the Numbat language
    monaco.languages.register({ id: 'numbat' });

    // Define tokens for syntax highlighting
    monaco.languages.setMonarchTokensProvider('numbat', {
        tokenizer: {
            root: [
                // Comments - must come first to take priority
                [/#.*$/, 'comment'],
                
                // Keywords
                [/\b(?:per|to|let|fn|where|and|dimension|unit|use|long|short|both|none|print|assert|assert_eq|type|if|then|else|true|false)\b/, 'keyword'],
                
                // Function names (identifiers followed by parentheses) - must come before numbers
                [/\b[a-zA-Z_][a-zA-Z0-9_]*(?=\s*\()/, 'entity.name.function'],
                
                // General identifiers (must come before types to prevent partial matching)
                [/\b[a-zA-Z_][a-zA-Z0-9_]*\b/, 'identifier'],
                
                // Numbers (standalone numbers, not part of identifiers)
                [/\b(?:[0-9]+(?:[._][0-9]+)*|0x[0-9A-Fa-f]+|0o[0-7]+|0b[01]+(?:_[01]+)*|\.[0-9]+(?:[eE][-+]?[0-9]+)?|[0-9]+[eE][-+]?[0-9]+)(?![a-zA-Z_])/, 'number'],
                
                // Types (identifiers that start with uppercase letter, but this won't match now due to general identifier rule above)
                [/\b[A-Z][A-Za-z0-9_]*\b/, 'type'],
                
                // Strings
                [/"[^"]*"/, 'string'],
                
                // Decorators
                [/@[\w_]+/, 'annotation'],
                
                // Operators and special characters (including parentheses, but excluding #)
                [/[+\-*/^➞→:÷×≤≥≠<>²³()]/, 'operator'],
            ]
        }
    });

    // Configure language features
    monaco.languages.setLanguageConfiguration('numbat', {
        comments: {
            lineComment: '#'
        },
        brackets: [
            ['(', ')'],
        ],
        autoClosingPairs: [
            { open: '(', close: ')' },
            { open: '"', close: '"' },
        ],
    });

    // Define custom theme for Numbat
    monaco.editor.defineTheme('numbat-light', {
        base: 'vs',
        inherit: true,
        rules: [
            { token: 'comment', foreground: '8c8c8c' },
            { token: 'keyword', foreground: 'c802ff' },
            { token: 'entity.name.function', foreground: '000000' },
            { token: 'identifier', foreground: '000000' },
            { token: 'number', foreground: '0040ff' },
            { token: 'type', foreground: 'ca3b63' },
            { token: 'string', foreground: '27a85f' },
            { token: 'operator', foreground: 'db2828' },
            { token: 'annotation', foreground: '27a85f' },
        ],
        colors: {
            'editor.background': '#FFFFFF',
            'editor.foreground': '#000000',
            'editorLineNumber.foreground': '#000000',
            'editorGutter.background': '#eee',
        }
    });

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

function initializeEditor() {
    editor = monaco.editor.create(document.getElementById('editor'), {
        value: `8 km / (1 h + 25 min)

atan2(30 cm, 1 m) -> deg

let ω = 2π c / 660 nm
ℏ ω -> eV


fn braking_distance(v) = v t_reaction + v² / 2 µ g0
  where t_reaction = 1 s # driver reaction time
    and µ = 0.7          # coefficient of friction

braking_distance(50 km/h) -> m`,
        language: 'numbat',
        theme: 'numbat-light',
        fontFamily: 'Fira Mono, monospace',
        fontSize: 16,
        minimap: { enabled: false },
        lineNumbers: 'on',
        glyphMargin: false,
        folding: false,
        lineDecorationsWidth: 0,
        lineNumbersMinChars: 4,
        renderLineHighlight: 'none',
        scrollBeyondLastLine: false,
        automaticLayout: true,
    });

    function evaluate() {
        let code = editor.getValue();
        let output = interpret(code);
        document.getElementById("results").innerHTML = output;
    }

    var debouncedEvaluate = debounce(evaluate, 500);

    editor.onDidChangeModelContent(debouncedEvaluate);

    evaluate();
    editor.focus();
}

function initializeSplit() {
    Split(['#editor', '#results'], {
        gutterSize: 15,
    });
}

// Theme toggle functionality removed