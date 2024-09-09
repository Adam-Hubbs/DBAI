#' llm_generate
#' @param source required; A source dataframe or llm-completion object.
#' @param input required; A column name in the source dataframe
#' @param output optional; A string of a column name (Or a vector of strings) to be created in the source dataframe storing the output of the models. Defaults to `output`.
#' @param prompt required; A string (Or vector of Strings for handling multiple operations at the same time) of prompts to be sent to the AI model.
#' @param model required; a character vector.
#' @param return_invisible optional; A boolean to return just the output (`TRUE`) or an llm-completion object containing model metadata (`FALSE`). Defaults to `FALSE`.
#' @param iterations optional; An integer. Number of completions to generate for each prompt Defaults to `1`.
#' @param repair optional; A boolean to repair NA's in the output column and keep values already present in the output column if the output column has already been created. False overrides the data already in an output column if it exists. Useful to continue a computation if you have been rate limited. Defaults to `FALSE`.
#' @param progress optional; a length one logical vector. Defaults to `TRUE`. Determines whether to show a progress bar in the console. Not available when using repair mode.
#' @param temperature optional; defaults to `1`; a length one numeric vector with the value between `0` (More analytical) and `2` (More creative). `0-2` for OpenAI and Google models, `0-1` for Anthropic models.
#' @param top_p optional; defaults to `1`; a numeric vector with the value between `0` and `1`.
#' @param top_k optional; a numeric vector with the integer value greater than `0`. Only sample from the top_k options for each subsequent token. Not recommended, for most use cases use temperature instead.
#' @param anthropic_version optional; defaults to `2023-06-01`; a character vector. Specifies the version of the Anthropic's models.
#' @param n optional; defaults to `1`; a numeric vector with the integer value greater than `0`.
#' @param presence_penalty optional; defaults to `0`; a numeric vector with a value between `-2` and `2`.
#' @param frequency_penalty optional; defaults to `0`; a numeric vector with a value between `-2` and `2`.
#' @param max_tokens optional; defaults to `(4096 - prompt tokens)`; a numeric vector with the integer value greater than `0`.
#' @param openai_api_key optional; defaults to `Sys.getenv("OPENAI_API_KEY")` (i.e., the value is retrieved from the `.Renviron` file); a length one character vector. Specifies OpenAI API key. Must obtain API Key from OpenAI.
#' @param openai_organization optional; defaults to `NULL`; a length one character vector. Specifies OpenAI organization.
#' @param anthropic_api_key optional; defaults to `Sys.getenv("ANTHROPIC_API_KEY")` (i.e., the value is retrieved from the `.Renviron` file); a length one character vector. Specifies Anthropic API key.
#' @param google_api_key optional; defaults to `Sys.getenv("GOOGLE_API_KEY")` (i.e., the value is retrieved from the `.Renviron` file); a length one character vector. Specifies Google API key. Must obtain API Key from Google.
#' @return A dataframe with the output column(s) created
#' @export
llm_generate <- function(
  source,
  input,
  output = "output",
  prompt,
  model = "gpt-3.5-turbo",
  model_provider = NA,
  return_invisible = FALSE,
  iterations = 1,
  repair = FALSE,
  progress = TRUE,
  temperature = 1,
  top_p = 1,
  top_k = NULL,
  anthropic_version = "2023-06-01",
  n = 1,
  presence_penalty = 0,
  frequency_penalty = 0,
  max_tokens = 4096,
  openai_api_key = Sys.getenv("OPENAI_API_KEY"),
  openai_organization = NULL,
  anthropic_api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  google_api_key = Sys.getenv("GOOGLE_API_KEY")) {

  # Handle Automatic model_provider
  if (!is_valid_model_provider(model_provider)) {
    lookup_table <- c(
      "gpt-3.5-turbo-16k" = "OpenAI",
      "gpt-3.5-turbo-instruct" = "OpenAI",
      "gpt-4-turbo-2024-04-09" = "OpenAI",
      "gpt-4-turbo" = "OpenAI",
      "gpt-4-1106-preview" = "OpenAI",
      "gpt-3.5-turbo-1106" = "OpenAI",
      "gpt-4-0125-preview" = "OpenAI",
      "gpt-3.5-turbo-0125" = "OpenAI",
      "gpt-3.5-turbo" = "OpenAI",
      "gpt-3.5-turbo-0301" = "OpenAI",
      "gpt-4-turbo-preview" = "OpenAI",
      "gpt-4o-2024-05-13" = "OpenAI",
      "gpt-3.5-turbo-instruct-0914" = "OpenAI",
      "gpt-3.5-turbo-16k-0613" = "OpenAI",
      "gpt-4" = "OpenAI",
      "gpt-4-1106-vision-preview" = "OpenAI",
      "gpt-4-0613" = "OpenAI",
      "gpt-4o" = "OpenAI",
      "gpt-3.5" = "OpenAI",
      "claude-3-opus-20240229" = "Anthropic",
      "claude-3-sonnet-20240229" = "Anthropic",
      "claude-3-haiku-20240307" = "Anthropic",
      "gemini-1.5-pro" = "Google",
      "gemini-1.5-flash" = "Google",
      "gemini-1.0-pro" = "Google"
    )
    providers <- lookup_table[model]
    if (length(providers) != length(model)) {
      cli::cli_abort(c(
        "Unable to find model provider.",
        i = "Please provide a model provider for every model you supply. i.e. `OpenAI`, `Google`, or `Anthropic`.",
        x = "The following model{?s} could not be automatically matched to a model provider: {.val missings}"
      ))
    } else {
      model_providers <- providers
    }
  }

  #Dispatch to model_provider functions
  if (model_provider == "OpenAI") {
    rtn <- gpt.character()
  } else if (model_provider == "Google") {
    rtn <- gemini.character()
  } else if (model_provider == "Anthropic") {
    rtn <- claude.character()
  } else {
    cli::cli_abort(c(
      "Unable to find model provider.",
      i = "Please provide a model provider for every model you supply. i.e. `OpenAI`, `Google`, or `Anthropic`.",
      x = "The following model{?s} could not be automatically matched to a model provider: {.val model_provider}"
    ))
  }
return(rtn)
}




