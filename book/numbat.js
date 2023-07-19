hljs.registerLanguage('numbat', function(hljs) {
  return {
    name: 'Numbat',
    aliases: ['nbt'],
    case_insensitive: false,
    keywords: {
      keyword: 'per to let fn dimension unit use long short both none print assert_eq',
    },
    contains: [
      hljs.HASH_COMMENT_MODE,
      hljs.BINARY_NUMBER_MODE,
      {
        className: 'number',
        begin: /\b0o[0-7]+\b/
      },
      {
        className: 'number',
        begin: '(-?)(\\b0[xX][a-fA-F0-9]+|(\\b[0-9_]+(\\.[0-9_]*)?|\\.[0-9_]+)([eE][-+]?[0-9_]+)?)'
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
          { begin: '%' },
          { begin: '=' },
          { begin: '->' },
          { begin: '→' },
          { begin: '➞' }
        ]
      },
    ]
  };
});
