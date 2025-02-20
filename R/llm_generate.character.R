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
#' @param top_p optional; defaults to `NULL`; a numeric vector with the value between `0` and `1`.
#' @param top_k optional; a numeric vector with the integer value greater than `0`. Only sample from the top_k options for each subsequent token. Not recommended, for most use cases use temperature instead.
#' @param anthropic_version optional; defaults to `2023-06-01`; a character vector. Specifies the version of the Anthropic's models.
#' @param presence_penalty optional; defaults to `0`; a numeric vector with a value between `-2` and `2`.
#' @param frequency_penalty optional; defaults to `0`; a numeric vector with a value between `-2` and `2`.
#' @param max_tokens optional; defaults to `(4096 - prompt tokens)`; a numeric vector with the integer value greater than `0`.
#' @param max_completion_tokens optional; defaults to `NULL`; used in OpenAI reasoning models; a numeric vector with the integer value greater than `0`. Specifies the maximum number of tokens to generate for each completion. This includes tokens generated as part of the reasoning process and are not returned to the user.
#' @param logit_bias optional; defaults to `NULL`; a JSON object that maps tokens (as specified by their toekn ID in the tokenizer) to an associated bias value. -100 to 100.
#' @param logprobs optional; defaults to `FALSE`. If `TRUE`, the API will return log probabilities for each token.
#' @param top_logprobs optional; An integer between `0` and `20`. Specifies the number of most likely tokens to return at each token position. The API will return log probabilities for the top `top_logprobs` tokens at each position. The `logprobs` must be set to `TRUE` to use this parameter.
#' @param seed optional; defaults to `NULL`. An integer that allows for reproducible results when using the same seed. (BETA)
#' @param stop optional; Defaults to `NULL`. A vector of strings (Up to length 4) of sequences where the API will stop generating further tokens.
#' @param user optional; defaults to `NULL`. A string that specifies the user ID to associate with the completion.
#' @param is_reasoning_model optional; defaults to `NULL`. A vector of booleans that specifies if the model is a reasoning model. If `TRUE`, the model will be treated as a reasoning model. Reasoning models use different model parameters and are optimized for reasoning tasks. OpenAI only.
#' @param reasoning_effort optional; defauls to `medium`. A vector of strings that specifies the effort level for the reasoning model. OpenAI only. Must be one of c(`low`, `medium`, `high`).
#' @param openai_api_key optional; defaults to `Sys.getenv("OPENAI_API_KEY")` (i.e., the value is retrieved from the `.Renviron` file); a length one character vector. Specifies OpenAI API key. Must obtain API Key from OpenAI.
#' @param openai_organization optional; defaults to `NULL`; a length one character vector. Specifies OpenAI organization.
#' @param anthropic_version required; defaults to `2023-06-01`; a length one character vector. Specifies the version of the Anthropic's models.
#' @param anthropic_api_key optional; defaults to `Sys.getenv("ANTHROPIC_API_KEY")` (i.e., the value is retrieved from the `.Renviron` file); a length one character vector. Specifies Anthropic API key.
#' @param google_api_key optional; defaults to `Sys.getenv("GOOGLE_API_KEY")` (i.e., the value is retrieved from the `.Renviron` file); a length one character vector. Specifies Google API key. Must obtain API Key from Google.
#' @return A dataframe with the output column(s) created
#' @export
llm_generate.character <- function(
  source,
  prompt = "",
  model = "gpt-3.5-turbo",
  model_provider = NA,
  repair = FALSE,
  progress = TRUE,
  temperature = 1,
  top_p = NULL,
  top_k = NULL,
  anthropic_version = "2023-06-01",
  presence_penalty = 0,
  frequency_penalty = 0,
  max_tokens = 4096,
  max_completion_tokens = NULL,
  logit_bias = NULL,
  logprobs = FALSE,
  top_logprobs = NULL,
  seed = NULL,
  stop = NULL,
  user = NULL,
  is_reasoning_model = NULL,
  reasoning_effort = NULL,
  openai_api_key = Sys.getenv("OPENAI_API_KEY"),
  openai_organization = NULL,
  anthropic_api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  google_api_key = Sys.getenv("GOOGLE_API_KEY"),
  parentInfo = NULL) {

  if (is.na(model)) {
    cli::cli_abort(c("Model not specified.", i = "Please provide a model to use."))
  }
  # Handle Automatic model_provider
  if (!DBAI:::is_valid_model_provider(model_provider)) {
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
    missings <- model[is.na(providers)]
    if (length(missings) >= 1) {
      cli::cli_abort(c(
        "Unable to find model provider.",
        i = "Please provide a model provider for every model you supply. i.e. `OpenAI`, `Google`, or `Anthropic`.",
        x = "The following model{?s} could not be automatically matched to a model provider: {.val {missings}}"
      ))
    } else {
      model_provider <- providers
    }
  }

  ### Initialize Dummy Environment for Pass by reference system --
  if(is.null(parentInfo)) {
    parentInfo <- new.env()
    parentInfo$df <- FALSE
    parentInfo$call <- match.call.defaults()
  }

  parentInfo$NACount <- 0L
  parentInfo$EmptyCount <- 0L
  parentInfo$http_error <- 0L
  parentInfo$firstLineError <- 0L
  parentInfo$safteyRestriction <- 0L

  #Dispatch to model_provider functions
  if (model_provider == "OpenAI") {
    rtn <- gpt.character(source = source, prompt = prompt, model = model, progress = progress, temperature = temperature, top_p = top_p, max_tokens = max_tokens, frequency_penalty = frequency_penalty, presence_penalty = presence_penalty, openai_api_key = openai_api_key, openai_organization = openai_organization, is_reasoning_model = is_reasoning_model, reasoning_effort = reasoning_effort, max_completion_tokens = max_completion_tokens, parentInfo = parentInfo)
  } else if (model_provider == "Google") {
    rtn <- gemini.character(source = source, prompt = prompt, model = model, progress = progress, temperature = temperature, top_p = top_p, top_k = top_k, max_tokens = max_tokens, google_api_key = google_api_key, parentInfo = parentInfo)
  } else if (model_provider == "Anthropic") {
    rtn <- DBAI:::claude.character(source = source, prompt = prompt, model = model, progress = progress, temperature = temperature, top_p = top_p, top_k = top_k, max_tokens = max_tokens, anthropic_api_key = anthropic_api_key, anthropic_version = anthropic_version, parentInfo = parentInfo)
  } else {
    cli::cli_abort(c(
      "Unable to find model provider.",
      i = "Please provide a model provider for every model you supply. i.e. `OpenAI`, `Google`, or `Anthropic`.",
      x = "The following model{?s} could not be automatically matched to a model provider: {.val {model_provider}}"
    ))
  }
return(rtn)
}



