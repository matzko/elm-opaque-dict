# Changelog

## [2.0.0] - 2024-04-09

Breaking changes: Make `update` match the behavior of `Dict.update`:

- Previously, if the mapping function passed to `update` returned `Nothing`, it would not change the value of the matching key. Now, it deletes it.
- Previously, if there was no matching value but the mapping function returned something anyways, nothing would change. Now, it adds the key-value pair to the dictionary.
