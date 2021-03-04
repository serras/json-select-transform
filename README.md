# `json-select-transform`

A Haskell library for matching/selection and transformation of JSON document.
Inspired by [ST.js](https://selecttransform.github.io/site/).

### `select`
- template: `{ "number" : "{{x}}" }`
- document: `{ "number" : 1 }`
- result: `{ x -> 1 }`

### `transform`
- template: `{ "number" : "{{x}}" }`
- assignment: `{ x -> 1 }`
- result: `{ "number" : 1 }`

### `transform` with interpolation
- template: `{ "message" : "the result is {{x}}" }`
- assignment: `{ x -> 1 }`
- result: `{ "message" : "the result is 1" }`

### `transform` with array
- template: `{ "{{#each n}}" : { "number" : "{{n}}" } }`
- assignment: `{ n -> [1, 2] }`
- result: `[ { "number" : 1 }, { "number" : 2 } ]`