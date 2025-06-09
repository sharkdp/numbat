// Simple LSP client implementation for Monaco Editor
// This version communicates with the Rust LSP server

class NumbatLSPClient {
    constructor(editor) {
        this.editor = editor;
        this.isConnected = false;
        this.messageId = 0;
        this.pendingRequests = new Map();
        
        // For now, we'll use the WASM-based validation directly
        // In a full implementation, this would connect to the Rust LSP server via WebSocket
        this.initializeWasmValidation();
        this.setupEventHandlers();
    }
    
    initializeWasmValidation() {
        // Check if Numbat WASM is available
        if (window.Numbat && window.FormatType) {
            this.isConnected = true;
            console.log('Numbat LSP Client initialized with WASM validation');
        } else {
            console.warn('Numbat WASM not available for LSP client');
        }
    }
    
    setupEventHandlers() {
        // Listen for content changes
        this.editor.onDidChangeModelContent((e) => {
            this.debounceValidation();
        });
    }
    
    debounceValidation() {
        if (this.validationTimeout) {
            clearTimeout(this.validationTimeout);
        }
        
        this.validationTimeout = setTimeout(() => {
            this.validateDocument();
        }, 500); // 500ms debounce
    }
    
    async validateDocument() {
        if (!this.isConnected) return;
        
        const model = this.editor.getModel();
        const text = model.getValue();
        
        try {
            // Create a Numbat context similar to the Rust LSP server
            const numbat = window.Numbat.new(true, false, window.FormatType.Html);
            if (!numbat) {
                console.warn('Failed to create Numbat instance');
                return;
            }
            
            const diagnostics = this.getNumbatDiagnostics(text, numbat);
            this.updateDiagnostics(diagnostics);
            
            numbat.free();
        } catch (error) {
            console.error('Error validating Numbat code:', error);
        }
    }
    
    getNumbatDiagnostics(text, numbat) {
        const diagnostics = [];
        
        // Try to interpret the text and collect errors
        const result = numbat.interpret(text);
        
        if (result.is_error) {
            // Create a diagnostic from the error
            const diagnostic = this.createDiagnosticFromError(result.output, text);
            if (diagnostic) {
                diagnostics.push(diagnostic);
            }
        }
        
        return diagnostics;
    }
    
    createDiagnosticFromError(errorOutput, sourceText) {
        // Parse error message to extract line and column info if available
        const errorText = errorOutput.replace(/<[^>]*>/g, ''); // Strip HTML
        
        console.log('Debug: Error text:', errorText); // Debug logging
        
        // Try to extract line information from error message
        // Look for codespan patterns like "┌─ :3:1" and line markers like "3 │"
        const codespanMatch = errorText.match(/┌─ :(\d+):(\d+)/);
        const lineMarkerMatch = errorText.match(/(\d+) │/);
        const inputMatch = errorText.match(/input:(\d+):(\d+)/);
        
        let lineNumber = 1; // Default to line 1
        let columnNumber = 1; // Default to column 1
        
        if (codespanMatch) {
            lineNumber = parseInt(codespanMatch[1]);
            columnNumber = parseInt(codespanMatch[2]);
            console.log('Debug: Found codespan match:', lineNumber, columnNumber);
        } else if (lineMarkerMatch) {
            lineNumber = parseInt(lineMarkerMatch[1]);
            columnNumber = 1; // Default column when only line is available
            console.log('Debug: Found line marker match:', lineNumber);
        } else if (inputMatch) {
            lineNumber = parseInt(inputMatch[1]);
            columnNumber = parseInt(inputMatch[2]);
            console.log('Debug: Found input match:', lineNumber, columnNumber);
        } else {
            console.log('Debug: No line/column match found, using defaults');
        }
        
        // Get the actual line content to determine a better end position
        const lines = sourceText.split('\\n');
        const targetLine = lines[lineNumber - 1]; // Convert to 0-based for array access
        const lineLength = targetLine ? targetLine.length : 10;
        
        return {
            severity: monaco.MarkerSeverity.Error,
            startLineNumber: lineNumber,
            startColumn: columnNumber,
            endLineNumber: lineNumber,
            endColumn: Math.min(columnNumber + 10, lineLength + 1), // Don't exceed line length
            message: errorText.trim() || 'Numbat error', // Full error message
            source: 'numbat'
        };
    }
    
    updateDiagnostics(diagnostics) {
        const model = this.editor.getModel();
        if (!model) return;
        
        // Update Monaco editor markers
        monaco.editor.setModelMarkers(model, 'numbat', diagnostics);
        
        console.log(`Updated ${diagnostics.length} diagnostics`);
    }
    
    dispose() {
        if (this.validationTimeout) {
            clearTimeout(this.validationTimeout);
        }
    }
}

// Export for use in editor.js
window.NumbatLSPClient = NumbatLSPClient;