#' is_valid_model_provider
#' @title Checks if a string is a valid model provider or not
#' @param provider required; The string to check if it is a valid model provider or not.
#' @return boolean of if it is a model provider or not.
is_valid_model_provider <- function(provider) {
  valid_names <- c("OpenAI", "Google", "Anthropic")
  if (all(provider %in% valid_names)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
