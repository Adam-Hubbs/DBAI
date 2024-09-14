#' claude
#' @param source required; A source dataframe or llm-completion object.
#' @param input required; A column name in the source dataframe
#' @param output optional; A string of a column name (Or a vector of strings) to be created in the source dataframe storing the output of the models. Defaults to `output`.
#' @param prompt required; A string (Or vector of Strings for handling multiple operations at the same time) of a system message to be sent to the AI model.
#' @param model required; a length one character vector.
#' @param return_invisible optional; A boolean to return just the output (`TRUE`) or an llm object containing model metadata (`FALSE`). Defaults to `FALSE`.
#' @param iterations optional; An integer. Number of completions to generate for each row. Defaults to `1`.
#' @param repair optional; A boolean to repair NA's in the output column and keep values already present in the output column if the output column has already been created. False overrides the data already in an output column if it exists. Useful to continue a computation if you have been rate limited. Defaults to `FALSE`.
#' @param progress optional; a length one logical vector. Defaults to `TRUE`. Determines whether to show a progress bar in the console.
#' @param temperature optional; defaults to `1`; a length one numeric vector with the value between `0` (More analytical) and `1` (More creative).
#' @param top_p optional; a length one numeric vector with the value between `0` and `1`. Only specify a temperature or a top_p, never both. Not recommended, for most use cases use temperature instead.
#' @param top_k optional; a length one numeric vector with the integer value greater than `0`. Only sample from the top_k options for each subsequent token. Not recommended, for most use cases use temperature instead.
#' @param anthropic_version required; defaults to `2023-06-01`; a length one character vector. Specifies the version of the Anthropic's models.
#' @param max_tokens optional; defaults to `(4096 - prompt tokens)`; a length one numeric vector with the integer value greater than `0`.
#' @param anthropic_api_key required; defaults to `Sys.getenv("ANTHROPIC_API_KEY")` (i.e., the value is retrieved from the `.Renviron` file); a length one character vector. Specifies Anthropic API key.
#' @return A dataframe with the output column(s) created
#' @export
claude.character <- function(source,
                   output = "output",
                   prompt = "",
                   model = "claude-3-haiku-20240307",
                   iterations = 1,
                   repair = FALSE,
                   progress = TRUE,
                   temperature = 1,
                   top_p = NULL,
                   top_k = NULL,
                   anthropic_version = "2023-06-01",
                   max_tokens = 4096,
                   parentInfo = NULL,
                   call = rlang::caller_env(),
                   anthropic_api_key = Sys.getenv("ANTHROPIC_API_KEY")) {

    ### Initialize Dummy Environment for Pass by reference system --
    if(is.null(parentInfo)) {
      parentInfo <- new.env()
      parentInfo$df <- FALSE
      Call <- DBAI:::match.call.defaults()
    } else {
      Call <- parentInfo$call
    }
    parentInfo$NACount <- 0L
    parentInfo$EmptyCount <- 0L
    parentInfo$http_error <- 0L
    parentInfo$firstLineError <- 0L


  ### Validate Statements ---------------------------------
  if (parentInfo$df != TRUE) {
    ### API KEY
    if(is.null(anthropic_api_key) || anthropic_api_key == "" || is.na(anthropic_api_key)) {
      cli::cli_abort(c("API Key not found.", i = "Please set the {.envvar ANTHROPIC_API_KEY} environment variable.", i = "Did you forget to set {.envvar ANTHROPIC_API_KEY} with {.code Sys.env(ANTHROPIC_API_KEY='XXXXX')}?"), call = call)
    }

    if (!is.character(anthropic_api_key) || length(anthropic_api_key) != 1) {
      cli::cli_abort(c("{.envvar ANTHROPIC_API_KEY} must be a length one character vector.", i = "Did you forget to set {.envvar ANTHROPIC_API_KEY} with {.code Sys.env(ANTHROPIC_API_KEY='XXXXX')}?"), call = call)
    }

    ### SOURCE COLUMN
    if(is.null(source)) {
      cli::cli_abort(c("{.var source} is missing.", i = "Did you forget to pass a dataframe or character vector to the function?"), call = call)
    }

    ### PROMPT
    if (!is.null(prompt)) {
      if (!is.character(prompt)) {
        cli::cli_abort(c("{.var prompt} (if supplied) must be a character vector.", x = "You supplied a {.cls {class(prompt)}} vector."), call = call)
      }
      if (length(prompt) != 1) {
        cli::cli_abort(c("{.var prompt} must be a length one character vector."), call = call)
      }
    }

    ### Other Function Parameters
    if (is.null(anthropic_version)) {
      cli::cli_abort(c(x = "{.var anthropic_version} must be supplied.", i = "For more information see {.url https://docs.anthropic.com/en/api/versioning}."))
    }

    if (!is.null(anthropic_version) && (!is.character(anthropic_version) || length(anthropic_version) != 1)) {
      cli::cli_abort(c("{.var anthropic_version} must be a length one character vector."), call = call)
    }

    ### Progress
    if (!is.logical(progress) || length(progress) != 1 || is.na(progress)) {
      cli::cli_abort(c("{.var progress} must either be {.var TRUE} or {.var FALSE}."), call = call)
    }

    ### Other Model Parameters
    if (is.null(model) || !is.character(model) || length(model) != 1 || is.na(model)) {
      cli::cli_abort(c("{.var model} must be a length one non-NA character vector."), call = call)
    }

    ### TEMPERATURE
    if(!is.null(temperature)) {
      if (!is.numeric(temperature) || length(temperature) != 1 || temperature < 0 || temperature > 1) {
        cli::cli_abort(c("{.var temperature} must be a number between {.code 0} and {.code 1}.", x = "You supplied {.var {temperature}}."), call = call)
      }
    }

    ### TOP P
    if (!is.null(top_p)) {
      if (!is.numeric(top_p) || length(top_p) != 1 || top_p < 0 || top_p > 1) {
        cli::cli_abort(c("{.var top_p} must be a number between {.code 0} and {.code 1}.", x = "You supplied {.var {top_p}}."), call = call)
      }
    }

    ### TEMPERATURE & TOP P
    if (!is.null(temperature) && !is.null(top_p)) {
      cli::cli_alert_warning(c("It is not recommended to specify both {.var temperature} and {.var top_p} at the same time. Some models may refuse to generate if both values are supplied."))
    }

    ### TOP K
    if (!is.null(top_k)) {
      if (!is.numeric(top_k) || any(top_k < 0)) {
        cli::cli_abort(c("{.var top_k} must be a length 1 numeric vector greater than {.code 0}.", x = "You supplied {.var {top_k}}."), call = call)
      }
    }

    ###
    ### TOP_K and Temperature Warning but allow it
    ###

    ### MAX TOKENS
    if (!is.null(max_tokens)) {
      if (!is.numeric(max_tokens) || length(max_tokens) != 1 || is.na(max_tokens) || max_tokens %% 1 != 0) {
        cli::cli_abort(c("{.var max_tokens} must be an integer between {.code 0} and {.code 4096}.", x = "You supplied a length {length(max_tokens)} {.cls {typeof(max_tokens)}} vector."), call = call)
      }
      if (max_tokens <= 0 || max_tokens > 4096) {
        cli::cli_abort(c("{.var max_tokens} must be an integer between {.code 0} and {.code 4096}.", x = "You supplied {.var {max_tokens}}."), call = call)
      }
    }
  }

  ### End Validation ----------------------------------

  if(length(source) < 3) {
    progress <- FALSE
  }

  ### Initialize Progress Bar -----------------------------
  if (progress == TRUE && parentInfo$df == FALSE) {
   parentInfo$pb <- progress::progress_bar$new(
     total = length(source) * length(prompt),
     format = "[:bar] :current/:total | :percent | :eta remaining"
   )
  }


  ### Build skeleton ------------------------------------
  base_url <- "https://api.anthropic.com/v1/messages"

  headers <- c(
    "x-api-key" = anthropic_api_key,
    "content-type" = "application/json",
    "anthropic-version" = anthropic_version
  )


  body <- list()
  body[["model"]] <- model
  body[["temperature"]] <- temperature
  if(!is.null(top_p)) body[["top_p"]] <- top_p
  if(!is.null(top_k)) body[["top_k"]] <- top_k
  body[["max_tokens"]] <- max_tokens




  ### Completion Function for Anthropic -------------------------
  completion <- function(input, prompt) {
    body[["system"]] <- prompt
    body[["messages"]] <- list(
      list(
        "role" = "user",
        "content" = input
      )
    )

    response <- httr::POST(
      url = base_url,
      httr::add_headers(.headers = headers),
      body = body,
      encode = "json"
    )

    parsed <- response |>
      httr::content(as = "text", encoding = "UTF-8") |>
      jsonlite::fromJSON(flatten = TRUE)

    if (httr::http_error(response)) {
      parentInfo$http_error <- parentInfo$http_error + 1
      if(httr::status_code(response) == 404) {
        cli::cli_abort(c("Anthropic API request failed [404]."), footer = {parsed$error$message}, call = call, class = "404")
      } else {
        cli::cli_abort(c("Anthropic API request failed [{httr::status_code(response)}]", x = "{parsed$error$message}"), call = call)
      }
    }
    parsed
  }


  ### Main Call. Checks if input is valid and calls the completion function -----------------------
  process_element <- function(input) {
    working_vec <- NA
    for (i in seq_along(input)) {
      if(progress == TRUE) parentInfo$pb$tick()
      if(is.na(input[i])) {
        parentInfo$NACount <- parentInfo$NACount + 1
        working_vec[i] <- NA
      } else if (input[i] == "" || input[i] == " ") {
        parentInfo$EmptyCount <- parentInfo$EmptyCount + 1
        working_vec[i] <- ""
      } else {
        working_vec[i] <- tryCatch({completion(input[i], prompt)$content$text}, error = function(e) {
          if (parentInfo$df == FALSE) {
            if(parentInfo$firstLineError == 0) {
              parentInfo$firstLineError <- i
            }
          }
          if ("404" %in% class(e)) {
            cli::cli_abort(c("{e$message}", x = {e$footer}), call = call)
          }
          return(NA)
        })
      }
    }
    return(working_vec)
  }

  # Apply the function to each element of the source vector
  if (length(source) > 1) {
    output_vector <- process_element(source)
  } else {
    a <- completion(input = source, prompt = prompt)
    output_vector <- a$content$text
  }

  ### Warnings and Errors -----------------------------
  if(parentInfo$df != TRUE) {
    if(parentInfo$firstLineError > 0) {
      cli::cli_alert_warning("First Error Located in Row: {parentInfo$firstLineError}")
      cli::cli_bullets(c(i = "This is could be a rate limit error. Check the website of the model provider for specific rate limits for your usage tier. Rerun this function again with {.code repair=TRUE} to continue processing once you are no longer rate-limited."))
    }

    if(parentInfo$http_error > 0) {
      cli::cli_alert_warning("There are {parentInfo$http_error} {?error/errors} from Google servers.")
    }

    if(parentInfo$NACount > 0) {
      cli::cli_alert_warning("There are {parentInfo$NACount} missing {? value/values} in the input column.")
      cli::cli_bullets(c(i = "{parentInfo$NACount} {? NA/NAs} introduced in the output."))
    }

    if(parentInfo$EmptyCount > 0) {
      cli::cli_alert_warning("There are {parentInfo$EmptyCount} empty {? string/strings} in the input column.")
      cli::cli_bullets(c(i = "{parentInfo$EmptyCount} empty {? string/strings} introduced in the output."))
    }
  }


  ### Return Object --------------------------
  if(!exists("a", envir = environment())) {
    a <- completion(input = source[1], prompt = prompt)
  }
  if (prompt == "") {
    prompt <- source
  }
  output_vector <- new_llm_completion(output_vector, Call = Call, Prompt = prompt, Model = model, Model_Provider = "Anthropic", Date = Sys.Date(), Temperature = temperature, Raw = a)

  return(output_vector)
}
