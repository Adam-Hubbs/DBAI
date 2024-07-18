#' Print Method for llm_completion objects
#'
#' @param x The `llm_completion` object to be printed.
#' @param ... optional. Other arguments to be passed to the print method.
#'
#' @return invisible
#' @export
print.llm_completion_DBAI <- function(x, ...) {
  xchar <- as.character(x)
  print(xchar)
}
