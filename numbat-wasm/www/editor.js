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
    });
}

main();

function interpretAndGetResults(input) {
    var numbat = Numbat.new(true, false, FormatType.Html);
    
    let lines = input.split('\n');
    let results = [];
    let currentBlock = '';
    let blockStartLine = 0;
    
    for (let i = 0; i < lines.length; i++) {
        let line = lines[i].trim();
        
        if (line === '') {
            // Empty line - process current block if it exists
            if (currentBlock.trim()) {
                let interpretOutput = numbat.interpret(currentBlock);
                let output = interpretOutput.output.trim();
                
                if (interpretOutput.is_error) {
                    output = output.replace(/<(input:\d+)>/gm, "&lt;$1&gt;");
                }
                
                if (output) {
                    // Add result to the last non-empty line of the block
                    for (let j = i - 1; j >= blockStartLine; j--) {
                        if (lines[j].trim()) {
                            results.push({
                                lineNumber: j + 1,
                                output: output,
                                isError: interpretOutput.is_error
                            });
                            break;
                        }
                    }
                }
                
                interpretOutput.free();
                currentBlock = '';
            }
            blockStartLine = i + 1;
        } else {
            // Non-empty line - add to current block
            if (currentBlock) currentBlock += '\n';
            currentBlock += lines[i];
        }
    }
    
    // Process final block if it exists
    if (currentBlock.trim()) {
        let interpretOutput = numbat.interpret(currentBlock);
        let output = interpretOutput.output.trim();
        
        if (interpretOutput.is_error) {
            output = output.replace(/<(input:\d+)>/gm, "&lt;$1&gt;");
        }
        
        if (output) {
            // Add result to the last line
            results.push({
                lineNumber: lines.length,
                output: output,
                isError: interpretOutput.is_error
            });
        }
        
        interpretOutput.free();
    }
    
    return results;
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
        fontSize: 19,
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

    let currentResultElements = [];

    function evaluate() {
        let code = editor.getValue();
        let results = interpretAndGetResults(code);
        
        // Clear previous result elements
        currentResultElements.forEach(element => {
            if (element.parentNode) {
                element.parentNode.removeChild(element);
            }
        });
        currentResultElements = [];
        
        // Clear previous errors in side panel
        const sidePanel = document.getElementById('side-panel');
        sidePanel.innerHTML = '';
        
        // Separate errors and successful results
        const errors = results.filter(result => result.isError);
        const successes = results.filter(result => !result.isError);
        
        // Display errors in side panel
        if (errors.length > 0) {
            const errorHeader = document.createElement('div');
            errorHeader.textContent = 'Errors';
            errorHeader.style.cssText = `
                color: #cc3b0a;
                font-family: 'Fira Mono', monospace;
                font-size: 19px;
                font-weight: bold;
                margin: 0 0 15px 0;
                border-bottom: 2px solid #cc3b0a;
                padding-bottom: 5px;
            `;
            sidePanel.appendChild(errorHeader);
            
            errors.forEach(error => {
                const errorElement = document.createElement('pre');
                errorElement.innerHTML = error.output; // Keep HTML for error formatting
                errorElement.style.cssText = `
                    background-color: rgba(204, 59, 10, 0.1);
                    border-left: 4px solid #cc3b0a;
                    padding: 10px;
                    margin-bottom: 10px;
                    font-family: 'Fira Mono', monospace;
                    font-size: 17px;
                    border-radius: 4px;
                    white-space: pre-wrap;
                    overflow-x: auto;
                    margin-top: 0;
                `;
                
                const lineInfo = document.createElement('div');
                lineInfo.textContent = `Line ${error.lineNumber}`;
                lineInfo.style.cssText = `
                    font-family: 'Fira Mono', monospace;
                    font-size: 16px;
                    color: #cc3b0a;
                    font-weight: bold;
                    margin-bottom: 5px;
                `;
                
                const errorContainer = document.createElement('div');
                errorContainer.appendChild(lineInfo);
                errorContainer.appendChild(errorElement);
                sidePanel.appendChild(errorContainer);
            });
        }
        
        // Display successful results inline in editor
        successes.forEach(result => {
            const cleanOutput = result.output.replace(/<[^>]*>/g, '');
            
            setTimeout(() => {
                const editorContainer = editor.getDomNode();
                const model = editor.getModel();
                const lineContent = model.getLineContent(result.lineNumber);
                const lineEndColumn = lineContent.length + 1;
                
                const position = editor.getScrolledVisiblePosition({
                    lineNumber: result.lineNumber,
                    column: lineEndColumn
                });
                
                if (position) {
                    const resultElement = document.createElement('div');
                    resultElement.textContent = cleanOutput;
                    resultElement.style.cssText = `
                        position: absolute;
                        left: ${position.left + 30}px;
                        top: ${position.top}px;
                        color: #0066cc;
                        background-color: rgba(0, 102, 204, 0.1);
                        font-family: 'Fira Mono', monospace;
                        font-size: 19px;
                        font-style: italic;
                        opacity: 0.9;
                        pointer-events: none;
                        z-index: 1000;
                        white-space: nowrap;
                        padding: 2px 6px;
                        border-radius: 3px;
                    `;
                    
                    editorContainer.appendChild(resultElement);
                    currentResultElements.push(resultElement);
                }
            }, 100);
        });
    }

    var debouncedEvaluate = debounce(evaluate, 1000);

    editor.onDidChangeModelContent(debouncedEvaluate);

    editor.onDidChangeModel(() => {
        currentResultElements.forEach(element => {
            if (element.parentNode) {
                element.parentNode.removeChild(element);
            }
        });
        currentResultElements = [];
    });

    evaluate();
    editor.focus();
}

// Split functionality removed - now using inline output

// Theme toggle functionality removed