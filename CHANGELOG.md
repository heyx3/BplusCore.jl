# v0.1.1

* Fix bugs with `Vec` ranges with negative step values
* Improve inline documentation
* Improve user-error-checking
* Improve macro utils:
  * Add `SplitType` to examine type declarations (e.x. `A{B, C<:D} <: E`), and `combinetype()` to put them back together
  * `is_scopable_name` can now optionally allow type-parameters (e.x. `A.B{C}`)

# v0.1.0

* Initial release