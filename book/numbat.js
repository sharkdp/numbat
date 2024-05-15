hljs.registerLanguage('numbat', function(hljs) {
  return {
    name: 'Numbat',
    aliases: ['nbt'],
    case_insensitive: false,
    keywords: {
      keyword: 'per to let fn dimension unit use struct long short both none if then else true false print assert assert_eq type',
    },
    contains: [
      hljs.HASH_COMMENT_MODE,
      hljs.BINARY_NUMBER_MODE,
      hljs.QUOTE_STRING_MODE,
      {
        className: 'number',
        begin: /\b0o[0-7]+\b/
      },
      {
        className: 'number',
        begin: '(-?)(\\b0[xX][a-fA-F0-9]+|(\\b[0-9_]+(\\.[0-9_]*)?|\\.[0-9_]+)([eE][-+]?[0-9_]+)?|NaN|inf)'
      },
      {
        className: 'meta',
        begin: /@[a-z_]*/
      },
      {
        className: 'title',
        begin: /(?<=(\)\s*(->|[→➞])|[:=<\/\*×·])\s*)\b[A-Z][a-zA-Z0-9_]*\b/,
      },
      {
        className: 'operator',
        variants: [
          { begin: '\\+' },
          { begin: '-' },
          { begin: '\\*' },
          { begin: '/' },
          { begin: '\\^' },
          { begin: '÷' },
          { begin: '×' },
          { begin: '=' },
          { begin: '->' },
          { begin: '→' },
          { begin: '➞' }
        ]
      },
    ]
  };
});
