#' S3 Method for llm_completion objects
#'
#' @param x llm_completion object
#' @export
print.llm_completion <- function(x) {
  cat("llm object\n\n")
  cat("Result:\n")
  print(x$Result)
  cat("\n")
  cat("Prompt:\n")
  print(x$Prompt)
  cat(paste("Model:", x$Model, "\n"))
  cat(paste("Provider:", x$Model_Provider, "\n"))
  cat(paste("Date:", x$Date, "\n"))
  cat("Raw Metadata:\n")
  cat(x$Raw)
}

.S3method("print", "llm_completion")


