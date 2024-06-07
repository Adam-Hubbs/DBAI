#' Title: Print Method for llm_completion
#'
#' @param x
#'
#' @return Invisible
#' @export print.llm_completion
#' @export
print.llm_completion <- function(x) {
  cat("llm object\n\n")
  cat("Result:\n")
  print(x$Result)
  cat("\n")
  cat("Prompt:\n")
  print(x$Prompt)
  cat("\n")
  cat(paste("Model:", x$Model, "\n\n"))
  cat(paste("Provider:", x$Model_Provider, "\n\n"))
  cat(paste("Date:", x$Date, "\n\n"))
  cat("Raw Metadata:\n")
  print(x$Raw)
}



