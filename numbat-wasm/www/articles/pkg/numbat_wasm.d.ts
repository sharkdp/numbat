/* tslint:disable */
/* eslint-disable */
/**
*/
export function setup_panic_hook(): void;
/**
*/
export enum FormatType {
  JqueryTerminal = 0,
  Html = 1,
}
/**
*/
export class InterpreterOutput {
  free(): void;
/**
*/
  is_error: boolean;
/**
*/
  readonly output: string;
}
/**
*/
export class Numbat {
  free(): void;
/**
* @param {boolean} load_prelude
* @param {boolean} enable_pretty_printing
* @param {FormatType} format_type
* @returns {Numbat}
*/
  static new(load_prelude: boolean, enable_pretty_printing: boolean, format_type: FormatType): Numbat;
/**
* @param {string} xml_content
*/
  set_exchange_rates(xml_content: string): void;
/**
* @param {string} code
* @returns {InterpreterOutput}
*/
  interpret(code: string): InterpreterOutput;
/**
* @returns {any}
*/
  print_environment(): any;
/**
* @returns {any}
*/
  print_functions(): any;
/**
* @returns {any}
*/
  print_dimensions(): any;
/**
* @returns {any}
*/
  print_variables(): any;
/**
* @returns {any}
*/
  print_units(): any;
/**
* @returns {any}
*/
  help(): any;
/**
* @param {string} keyword
* @returns {any}
*/
  print_info(keyword: string): any;
/**
* @param {string} input
* @returns {any[]}
*/
  get_completions_for(input: string): any[];
}

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly setup_panic_hook: () => void;
  readonly __wbg_numbat_free: (a: number) => void;
  readonly __wbg_interpreteroutput_free: (a: number) => void;
  readonly __wbg_get_interpreteroutput_is_error: (a: number) => number;
  readonly __wbg_set_interpreteroutput_is_error: (a: number, b: number) => void;
  readonly interpreteroutput_output: (a: number, b: number) => void;
  readonly numbat_new: (a: number, b: number, c: number) => number;
  readonly numbat_set_exchange_rates: (a: number, b: number, c: number) => void;
  readonly numbat_interpret: (a: number, b: number, c: number) => number;
  readonly numbat_print_environment: (a: number) => number;
  readonly numbat_print_functions: (a: number) => number;
  readonly numbat_print_dimensions: (a: number) => number;
  readonly numbat_print_variables: (a: number) => number;
  readonly numbat_print_units: (a: number) => number;
  readonly numbat_help: (a: number) => number;
  readonly numbat_print_info: (a: number, b: number, c: number) => number;
  readonly numbat_get_completions_for: (a: number, b: number, c: number, d: number) => void;
  readonly __wbindgen_add_to_stack_pointer: (a: number) => number;
  readonly __wbindgen_free: (a: number, b: number, c: number) => void;
  readonly __wbindgen_malloc: (a: number, b: number) => number;
  readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
  readonly __wbindgen_exn_store: (a: number) => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;
/**
* Instantiates the given `module`, which can either be bytes or
* a precompiled `WebAssembly.Module`.
*
* @param {SyncInitInput} module
*
* @returns {InitOutput}
*/
export function initSync(module: SyncInitInput): InitOutput;

/**
* If `module_or_path` is {RequestInfo} or {URL}, makes a request and
* for everything else, calls `WebAssembly.instantiate` directly.
*
* @param {InitInput | Promise<InitInput>} module_or_path
*
* @returns {Promise<InitOutput>}
*/
export default function __wbg_init (module_or_path?: InitInput | Promise<InitInput>): Promise<InitOutput>;
