# v0.1.1

* Fix bugs with `Vec` ranges with negative step values
* Improve inline documentation, and error-checking of input data
* Improve macro utils:
  * Add `SplitType`, to examine type declarations (e.x. `A{B, C<:D} <: E`), and `combinetype()` to put them back together.
  * Handle type parameters within `is_scopable_name()`, for example `A.B{C}`.
  * `SplitArg` can parse escaped argument declarations (e.x. `esc(i::Int)`), and `combinearg()` can emit them to assist macros in code generation.
  * Add new constructor to all the "split" data types (`SplitDef`, `SplitArg`, etc) for manually providing their fields.
  * Add support for lambdas (`->` expressions) to `SplitDef`

# v0.1.0

* Initial release