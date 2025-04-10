#' llm_generate
#' @param source required; A source dataframe or llm-completion object.
#' @param input required; A column name in the source dataframe
#' @param output optional; A string of a column name (Or a vector of strings) to be created in the source dataframe storing the output of the models. Defaults to `output`.
#' @param prompt required; A string (Or vector of Strings for handling multiple operations at the same time) of prompts to be sent to the AI model.
#' @param model required; a character vector.
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
#' @param anthropic_api_key optional; defaults to `Sys.getenv("ANTHROPIC_API_KEY")` (i.e., the value is retrieved from the `.Renviron` file); a length one character vector. Specifies Anthropic API key.
#' @param google_api_key optional; defaults to `Sys.getenv("GOOGLE_API_KEY")` (i.e., the value is retrieved from the `.Renviron` file); a length one character vector. Specifies Google API key. Must obtain API Key from Google.
#' @return A dataframe with the output column(s) created
#' @export
llm_generate.data.frame <- function(
    source,
    input,
    output = "output",
    prompt = "",
    model = "gpt-3.5-turbo",
    model_provider = NA,
    iterations = 1,
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

  ### Validate Statements ----------------------------------

  if (length(model) != length(model_provider)) {
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
   

  # Handle vectorization mapping and explicit vector recycling here rather than in the child functions
  var_list <- c("output", "prompt", "model", "model_provider", "temperature", "top_p", "top_k", "presence_penalty", "frequency_penalty", "max_tokens", "max_completion_tokens", "is_reasoning_model", "reasoning_effort")


  length_list <- sapply(mget(var_list), length)
  max_len <- max(length_list)
  vector_vars <- length_list == max_len
  invalid_vars <- var_list[!length_list %in% c(0, 1, max_len)]
  iterate_vars <- var_list[vector_vars]

  if (length(invalid_vars) > 0) {
    cli::cli_abort(c(
      "Some variables have invalid lengths.",
      "i" = "All variables should have length {.code 1} or {.code {max_len}} (to match the longest variable).",
      "x" = "The following variable{?s} ha{?s/ve} {? /an /}invalid length{?s}: {.val {invalid_vars}}"
    ))
  }

  ### Explicitly recycle vectors
  for (var_name in var_list) {
    assign(var_name, vctrs::vec_recycle(get(var_name), max_len))
  }


  ### Initialize Dummy Environment for Pass by reference system --
  parentInfo <- new.env()
  parentInfo$NACount <- 0L
  parentInfo$EmptyCount <- 0L
  parentInfo$http_error <- 0
  parentInfo$firstLineError <- 0L
  parentInfo$df <- TRUE
  parentInfo$baseCall <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  parentInfo$call <- match.call.defaults()
  parentInfo$llm_generate <- TRUE
  parentInfo$prompt <- FALSE

  ### Initialize Progress Bar -----------------------------
  if(repair == TRUE && progress == TRUE) {
    message("Progress bars are not supported in Repair mode")
    progress <- FALSE
  }

  if (progress == TRUE) {
    parentInfo$pb <- progress::progress_bar$new(
      total = nrow(source) * length(prompt) * iterations,
      format = "[:bar] :current/:total | :percent | :eta remaining"
    )
  }

  if('prompt' %in% colnames(source)){
    parentInfo$prompt <- TRUE
    source <- source %>%
    rename(prompts = prompt)
    }

  ### Add all variables into these calls, not just OpenAI ones.
  ### Main Loop ----------------------------------
  if(repair == TRUE) {
    for (iter in 1:iterations) {
      for (h in 1:max_len) {
        if (h == 1) {
          outputcol <- output[1]
        } else if("output" %in% iterate_vars) {
          outputcol <- output[h]
        } else {
          outputcol <- paste0(output[1], "_", h)
        }
        if (iter > 1) {
          outputcol <- paste0(outputcol, "_", iter)
        }
        na_index <- apply(source[, outputcol], 1, function(row) {
          any(is.na(row) | row == "" | row == " " | row == "NA")
        })
        na_input <- source[[input]][na_index]
        source[[outputcol]][na_index] <- llm_generate(source = na_input, prompt = prompt[h], progress = progress, model = model[h], temperature = temperature[h], top_p = top_p[h], presence_penalty = presence_penalty[h], frequency_penalty = frequency_penalty[h], max_tokens = max_tokens[h], openai_organization = openai_organization, anthropic_version = anthropic_version, parentInfo = parentInfo, model_provider = model_provider[h], is_reasoning_model = is_reasoning_model[h], reasoning_effort = reasoning_effort[h], max_completion_tokens = max_completion_tokens[h])
      }
    }
  } else {
    source <- source |>
      dplyr::ungroup()
    for (iter in 1:iterations) {
      for (h in 1:max_len) {
        if (h == 1) {
          outputcol <- output[1]
        } else if("output" %in% iterate_vars) {
          outputcol <- output[h]
        } else {
          outputcol <- paste0(output[1], "_", h)
        }
        if (iter > 1) {
          outputcol <- paste0(outputcol, "_I", iter)
        }
        source <- source |>
          dplyr::mutate(!!outputcol := llm_generate(source = !!sym(input), prompt = prompt[h], progress = progress, model = model[h], temperature = temperature[h], top_p = top_p[h], presence_penalty = presence_penalty[h], frequency_penalty = frequency_penalty[h], max_tokens = max_tokens[h], openai_organization = openai_organization, anthropic_version = anthropic_version, parentInfo = parentInfo, model_provider = model_provider[h], is_reasoning_model = is_reasoning_model[h], reasoning_effort = reasoning_effort[h], max_completion_tokens = max_completion_tokens[h]))
      }
    }
  }

  ### Clean up dataframe -----------------------------
if(parentInfo$prompt == TRUE){
  source <- source |>
    dplyr::ungroup() |>
    rename(prompt = prompts)
} else {
  source <- source |>
    dplyr::ungroup()
}
  ### Warnings and Messages ---------------------------
  if(parentInfo$firstLineError > 0) {
    cli::cli_alert_warning("First Error Located in Row: {parentInfo$firstLineError}")
    cli::cli_bullets(c(i = "This is could be a rate limit error. Check the website of the model provider for specific rate limits for your usage tier. Rerun this function again with {.code repair=TRUE} to continue processing once you are no longer rate-limited."))
  }

  if(parentInfo$http_error > 0) {
    cli::cli_alert_warning("There are {parentInfo$http_error} {?error/errors} from OpenAI servers.")
  }

  if(parentInfo$NACount > 0) {
    cli::cli_alert_warning("There are {parentInfo$NACount} missing {? value/values} in the input column.")
    cli::cli_bullets(c(i = "{parentInfo$NACount} {? NA/NAs} introduced in the output."))
  }

  if(parentInfo$EmptyCount > 0) {
    cli::cli_alert_warning("There are {parentInfo$EmptyCount} empty {? string/strings} in the input column.")
    cli::cli_bullets(c(i = "{parentInfo$EmptyCount} empty {? string/strings} introduced in the output."))
  }
  ### Return Object --------------------------
  return(source)
}

