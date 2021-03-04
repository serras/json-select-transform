# `json-select-transform`

A Haskell library for matching/selection and transformation of JSON document.
Inspired by [ST.js](https://selecttransform.github.io/site/).

### `select`
- template: `{ "number" : "{{x}}" }`
- document: `{ "number" : 1 }`
- result: `{ a -> 1 }`

### `transform`
- template: `{ "number" : "{{x}}" }`
- assignment: `{ a -> 1 }`
- result: `{ "number" : 1 }`

### `transform` with array
- template: `{ "{{#each n}}" : { "number" : "{{n}}" } }`
- assignment: `{ n -> [1, 2] }`
- result: `[ { "number" : 1 }, { "number" : 2 } ]`