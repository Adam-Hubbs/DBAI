#' gpt
#' @param source required; A source dataframe or llm-completion object.
#' @param input required; A column name in the source dataframe.
#' @param output optional; A string of a column name (Or a vector of strings) to be created in the source dataframe storing the output of the models. Defaults to `output`.
#' @param prompt required; A string (Or vector of Strings for handling multiple operations at the same time) of prompts to be sent to the AI model.
#' @param model required; a length one character vector.
#' @param iterations optional; An integer. Number of completions to generate for each prompt Defaults to `1`.
#' @param repair optional; A boolean to repair NA's in the output column and keep values already present in the output column if the output column has already been created. False overrides the data already in an output column if it exists. Useful to continue a computation if you have been rate limited. Defaults to `FALSE`.
#' @param progress optional; a length one logical vector. Defaults to `TRUE`. Determines whether to show a progress bar in the console. Not available when using repair mode.
#' @param temperature optional; defaults to `1`; a length one numeric vector with the value between `0` (More analytical) and `2` (More creative).
#' @param top_p optional; defaults to `1`; a length one numeric vector with the value between `0` and `1`.
#' @param presence_penalty optional; defaults to `0`; a length one numeric vector with a value between `-2` and `2`.
#' @param frequency_penalty optional; defaults to `0`; a length one numeric vector with a value between `-2` and `2`.
#' @param max_tokens optional; defaults to `(4096 - prompt tokens)`; a length one numeric vector with the integer value greater than `0`.
#' @param openai_api_key required; defaults to `Sys.getenv("OPENAI_API_KEY")` (i.e., the value is retrieved from the `.Renviron` file); a length one character vector. Specifies OpenAI API key. Must obtain API Key from OpenAI.
#' @param openai_organization optional; defaults to `NULL`; a length one character vector. Specifies OpenAI organization.
#' @param parentInfo Used internally. Do not supply.
#' @param logit_bias optional; defaults to `NULL`; a JSON object that maps tokens (as specified by their toekn ID in the tokenizer) to an associated bias value. -100 to 100.
#' @param logprobs optional; defaults to `FALSE`. If `TRUE`, the API will return log probabilities for each token.
#' @param top_logprobs optional; An integer between `0` and `20`. Specifies the number of most likely tokens to return at each token position. The API will return log probabilities for the top `top_logprobs` tokens at each position. The `logprobs` must be set to `TRUE` to use this parameter.
#' @param seed optional; defaults to `NULL`. An integer that allows for reproducible results when using the same seed. (BETA)
#' @param stop optional; Defaults to `NULL`. A vector of strings (Up to length 4) of sequences where the API will stop generating further tokens.
#' @param user optional; defaults to `NULL`. A string that specifies the user ID to associate with the completion.
#' @return A dataframe with the output column(s) created
#' @export
gpt.data.frame <- function(source,
                input,
                output = "output",
                prompt = "",
                model = "gpt-3.5-turbo",
                iterations = 1,
                repair = FALSE,
                progress = TRUE,
                temperature = NULL,
                top_p = NULL,
                presence_penalty = NULL,
                frequency_penalty = NULL,
                max_tokens = 4096,
                logit_bias = NULL,
                logprobs = FALSE,
                top_logprobs = NULL,
                seed = NULL,
                stop = NULL,
                user = NULL,
                openai_api_key = Sys.getenv("OPENAI_API_KEY"),
                openai_organization = NULL,
                call = rlang::caller_env(),
                parentInfo = NULL) {

  # TODO
  # Deprecate `n` in favor of `iterations`
  # Note about max_tokens and how it is now max_completion_tokens



  ### Validate Statements ----------------------------------
  ### API KEY
  if(is.null(openai_api_key) || openai_api_key == "" || is.na(openai_api_key)) {
    cli::cli_abort(c("API Key not found.", i = "Please set the {.envvar OPENAI_API_KEY} environment variable.", i = "Did you forget to set {.envvar OPENAI_API_KEY} with {.code Sys.env(OPENAI_API_KEY='XXXXX')}?"), call = call)
  }

  if (!is.character(openai_api_key) || length(openai_api_key) != 1) {
    cli::cli_abort(c("{.envvar OPENAI_API_KEY} must be a length one character vector.", i = "Did you forget to set {.envvar OPENAI_API_KEY} with {.code Sys.env(OPENAI_API_KEY='XXXXX')}?"), call = call)
  }

  ### SOURCE DATAFRAME
  if (length(source) == 0) {
    cli::cli_abort(c("The source dataframe is empty.", i = "Please provide a non-empty dataframe."), call = call)
  }

  ### INPUT COLUMN
  if(is.null(input)) {
    cli::cli_abort(c("The input column is missing.", i = "Please provide a column name in the source dataframe."), call = call)
  } else if (!input %in% colnames(source)) {
    cli::cli_abort(c("The input column does not exist in the source dataframe.", x = "{.code {input}} not found in source", i = "Please provide a valid column name in the source dataframe."), call = call)
  }

  ### PROMPT
  if (!is.null(prompt)) {
    if (!is.character(prompt)) {
      cli::cli_abort(c("{.var prompt} (if supplied) must be a character vector.", x = "You supplied a {.cls {class(prompt)}} vector."), call = call)
    }
  }
  ### OPENAI ORGANIZATION
  if (!is.null(openai_organization) && (!is.character(openai_organization))) {
    cli::cli_abort(c("{.var openai_organization} must be a length one character vector."), call = call)
  }

  ### PROGRESS
  if (!is.logical(progress) || length(progress) != 1 || is.na(progress)) {
    cli::cli_abort(c("{.var progress} must either be {.var TRUE} or {.var FALSE}."), call = call)
  }

  ### MODEL
  if (any(is.null(model)) || !is.character(model) || any(is.na(model))) {
    cli::cli_abort(c("{.var model} must be a non-NA character vector."), call = call)
  }

  ### TEMPERATURE
  if(!is.null(temperature)) {
    if (!is.numeric(temperature) || any(temperature < 0) || any(temperature > 2)) {
      cli::cli_abort(c("{.var temperature} must be a vector of numbers between {.code 0} and {.code 2}.", x = "You supplied {.var {temperature}}."), call = call)
    }
  }

  ### TOP P
  if (!is.null(top_p)) {
    if (!is.numeric(top_p) || any(top_p < 0) || any(top_p > 1)) {
      cli::cli_abort(c("{.var top_p} must be a vector of numbers between {.code 0} and {.code 1}.", x = "You supplied {.var {top_p}}."), call = call)
    }
  }

  ### TEMPERATURE & TOP P
  if (!is.null(temperature) && !is.null(top_p)) {
      cli::cli_alert_warning(c("It is not recommended to specify both {.var temperature} and {.var top_p} at the same time. Some models may refuse to generate if both values are supplied."))
  }

  ### MAX TOKENS
  if (!is.null(max_tokens)) {
    if (!is.numeric(max_tokens) || any(is.na(max_tokens)) || any(max_tokens %% 1 != 0)) {
      cli::cli_abort(c("{.var max_tokens} must be a integer vector of values between {.code 0} and {.code 4096}.", x = "You supplied a length {length(max_tokens)} {.cls {typeof(max_tokens)}} vector."), call = call)
    }
    if (any(max_tokens <= 0) || any(max_tokens > 4096)) {
      cli::cli_abort(c("{.var max_tokens} must be an integer between {.code 0} and {.code 4096}.", x = "You supplied {.var {max_tokens}}."), call = call)
    }
  }

  ### PRESENCE PENALTY
  if(!is.null(presence_penalty)) {
    if (!is.numeric(presence_penalty) || any(is.na(presence_penalty))) {
      cli::cli_abort(c("{.var presence_penalty} must be a number between {.code -2} and {.code 2}.", x = "You supplied a length {length(presence_penalty)} {.cls {typeof(presence_penalty)}} vector."), call = call)
    }
    if (any(presence_penalty < -2) || any(presence_penalty > 2)) {
      cli::cli_abort(c("{.var presence_penalty} must be a number between {.code -2} and {.code 2}.", x = "You supplied {.var {presence_penalty}}."), call = call)
    }
  }

  ### FREQUENCY PENALTY
  if(!is.null(frequency_penalty)) {
    if (!is.numeric(frequency_penalty) || any(is.na(frequency_penalty))) {
      cli::cli_abort(c("{.var frequency_penalty} must be a number between {.code -2} and {.code 2}.", x = "You supplied a length {length(frequency_penalty)} {.cls {typeof(frequency_penalty)}}."), call = call)
    }
    if (any(frequency_penalty < -2) || any(frequency_penalty > 2)) {
      cli::cli_abort(c("{.var frequency_penalty} must be a number between {.code -2} and {.code 2}.", x = "You supplied {.var {frequency_penalty}}."), call = call)
    }
  }

  ### REPAIR
  if(!is.logical(repair) || length(repair) != 1 || is.na(repair)) {
    cli::cli_abort(c("{.var repair} must either be {.code TRUE} or {.code FALSE}."), call = call)
  }

  if(repair == TRUE) {
    missing_cols <- setdiff(output, colnames(source))
    if (length(missing_cols) > 0) {
      cli::cli_abort(c("Output not found in source.", i = "When using {.code repair=TRUE}, all elements of {.var output} must be present in {.var source}.", x = "{.var {missing_cols}} {?does/do} not appear in {.code source}."), call = call)
    }
  }

  ### ITERATIONS
  if (is.null(iterations) || !is.numeric(iterations) || length(iterations) != 1 || is.na(iterations) || iterations <= 0 || iterations %% 1 != 0) {
    cli::cli_abort(c("{.code iterations} must be a positive integer."), call = call)
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

  ### Prepare the dataframe -----------------------------
  source <- source |>
    dplyr::rowwise()

  ### Vectorization Mapping -----------------------------
  #Double check that this is all the vars. Also think about how/if we want to implement input, and output.

  var_list <- c("output", "prompt", "model", "temperature", "top_p", "presence_penalty", "frequency_penalty", "max_tokens")


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
        source[[outputcol]][na_index] <- gpt(source = na_input, prompt = prompt[h], progress = progress, model = model[h], temperature = temperature[h], top_p = top_p[h], presence_penalty = presence_penalty[h], frequency_penalty = frequency_penalty[h], max_tokens = max_tokens[h], openai_organization = openai_organization, parentInfo = parentInfo)
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
          dplyr::mutate(!!outputcol := gpt(source = !!sym(input), prompt = prompt[h], progress = progress, model = model[h], temperature = temperature[h], top_p = top_p[h], presence_penalty = presence_penalty[h], frequency_penalty = frequency_penalty[h], max_tokens = max_tokens[h], openai_organization = openai_organization, parentInfo = parentInfo))
      }
    }
  }

  ### Clean up dataframe -----------------------------
  source <- source |>
    dplyr::ungroup()

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
