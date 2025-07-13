// Simple LSP client implementation for Monaco Editor
// Uses WASM Numbat with structured diagnostics for accurate error positioning

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
            console.log('Numbat LSP Client initialized with structured diagnostics');
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
        
        if (result.is_error && result.diagnostics) {
            // Use structured diagnostics from the WASM interface
            for (const diagInfo of result.diagnostics) {
                for (const range of diagInfo.ranges) {
                    diagnostics.push({
                        severity: monaco.MarkerSeverity.Error,
                        startLineNumber: range.start_line,
                        startColumn: range.start_column,
                        endLineNumber: range.end_line,
                        endColumn: range.end_column,
                        message: diagInfo.message,
                        source: 'numbat'
                    });
                }
            }
        }
        
        return diagnostics;
    }
    
    updateDiagnostics(diagnostics) {
        const model = this.editor.getModel();
        if (!model) return;
        
        // Update Monaco editor markers
        monaco.editor.setModelMarkers(model, 'numbat', diagnostics);
    }
    
    dispose() {
        if (this.validationTimeout) {
            clearTimeout(this.validationTimeout);
        }
    }
}

// Export for use in editor.js
window.NumbatLSPClient = NumbatLSPClient;