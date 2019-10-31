#' Deprecated Functions
#'
#' Deprecated `shinywqg` functions.
#'
#' @keywords internal
#' @name shinywqg_deprecated
NULL

#' @describeIn shinywqg_deprecated Check Function Template
#'
#' \lifecycle{soft-deprecated}
#'
#' @export
#'
#' @examples
#'
#' check_function_template("1")
check_function_template <- function(x, x_name = NULL) {
  deprecate_soft("0.0.0.9000",
    what = "check_function_template()",
    with = "chk_function_template()"
  )
  if (is.null(x_name)) x_name <- deparse_backtick(substitute(x))
  chk_function_template(x, x_name = x_name)
}
