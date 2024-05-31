#' list_models
#' @title List models available in the OpenAI API (Anthropic has no API for this.)
#' @param openai_api_key required; defaults to `Sys.getenv("OPENAI_API_KEY")` (i.e., the value is retrieved from the `.Renviron` file); a length one character vector. Specifies OpenAI API key.
#' @return a list of models available in the OpenAI API.
#' @export
list_models <- function(openai_api_key) {

  if(is.null(openai_api_key) || openai_api_key == "") {
    stop("API Key not found. Please set the OPENAI_API_KEY environment variable.")
  }

  if (!is.character(openai_api_key)) {
    stop("Error: 'openai_api_key' must be a character string.")
  }

  if (length(openai_api_key) != 1) {
    stop("Error: 'openai_api_key' must be a single value.")
  }

  base_url <- "https://api.openai.com/v1/models"

  headers <- c(
    "Authorization" = paste("Bearer", openai_api_key)
  )

  response <- httr::GET(
    url = base_url,
    httr::add_headers(.headers = headers),
    encode = "json"
  )

  parsed <- response |>
    httr::content(as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON(flatten = TRUE)

  if (httr::http_error(response)) {
    paste0(
      "OpenAI API request failed [",
      httr::status_code(response),
      "]:\n\n",
      parsed$error$message
    ) |>
      stop(call. = FALSE)
  }
  cat("Models available in the OpenAI API:\n\n")
  return(parsed$data$id)
}

