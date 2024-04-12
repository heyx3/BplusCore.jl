# v0.1.2

* Add `TrueOrdering`, and `true_order` for `vsize()`, so vectors can index arrays more intuitively if desired
* Fix bug in output of `@make_toggleable_asserts`
* Clarify the field-of-view in `m4_projection()`

# v0.1.1

* Fix bugs with `Vec` ranges with negative step values
* Improve inline documentation, and error-checking of input data
* Huge upgrades to macro utilities:
  * Add `visit_exprs` for debugging expressions
  * Adding `SplitType` for type declarations, such as `A{B<:Integer} <: C`
  * Unifying all the `Split[X]` types under `AbstractSplitExpr` and the interface `combine_expr()`
  * Handle escaping of most `Split[X]` expressions
  * Optional support for type parameters in `is_scopable_name()`, for example `A.B{C}`.
  * Add new constructor to all the `Split[X]` types to manually provide their fields.
  * Add `SplitDef` support for lambdas (`->` expressions).
  * All `SplitX` types inherit from `AbstractSplitExpr`, and their various combine functions can be accessed through the more generalized `combine_expr()`.
  * The `where_params` for `SplitDef` are explicitly typed as `Vector{SplitType}`. Previously they weren't being split
* Add `@ano_value` to go with `@ano_enum`
* Add `@bp_check_throws` for testing

# v0.1.0

* Initial release