#' gemini
#' @export
gemini <- function(
    source,
    input,
    output = "output",
    prompt,
    model = "gemini-1.5-flash",
    iterations = 1,
    repair = FALSE,
    progress = TRUE,
    temperature = 1,
    top_p = NULL,
    top_k = NULL,
    max_tokens = 4096,
    google_api_key = Sys.getenv("GOOGLE_API_KEY"),
    call = rlang::caller_env(),
    parentInfo = NULL) {
  
  ### Initialize Dummy Environment for Pass by reference system --
  if(is.null(parentInfo)) {
    parentInfo <- new.env()
    parentInfo$df <- FALSE
    Call <- match.call.defaults()
  } else {
    Call <- parentInfo$call
  }
  parentInfo$NACount <- 0L
  parentInfo$EmptyCount <- 0L
  parentInfo$http_error <- 0L
  parentInfo$firstLineError <- 0L

### Validate Statements ----------------------------------

if (parentInfo$df != TRUE) {
  ### API KEY
  if(is.null(google_api_key) || google_api_key == "" || is.na(google_api_key)) {
    cli::cli_abort(c("API Key not found.", i = "Please set the {.envvar GOOGLE_API_KEY} environment variable.", i = "Did you forget to set {.envvar GOOGLE_API_KEY} with {.code Sys.env(GOOGLE_API_KEY='XXXXX')}?"), call = call)
  }

  if (!is.character(google_api_key) || length(google_api_key) != 1) {
    cli::cli_abort(c("{.envvar GOOGLE_API_KEY} must be a length one character vector.", i = "Did you forget to set {.envvar GOOGLE_API_KEY} with {.code Sys.env(GOOGLE_API_KEY='XXXXX')}?"), call = call)
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
  if (!is.null(google_organization) && (!is.character(google_organization) || length(google_organization) != 1)) {
    cli::cli_abort(c("{.var google_organization} must be a length one character vector."), call = call)
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
    if (!is.numeric(temperature) || length(temperature) != 1 || temperature < 0 || temperature > 2) {
      cli::cli_abort(c("{.var temperature} must be a number between {.code 0} and {.code 2}.", x = "You supplied {.var {temperature}}."), call = call)
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
      cli::cli_abort(c("{.var top_k} must be a vector of numbers greater than {.code 0}.", x = "You supplied {.var {top_k}}."), call = call)
    }
  }

  ### TOP_P AND TOP_K
  if (!is.null(top_p) && !is.null(top_k)) {
      cli::cli_alert_warning(c("It is not recommended to specify both {.var top_p} and {.var top_k} at the same time. Some models may refuse to generate if both values are supplied."))
  }

  ### Is TOP_K and Temperature at the same time a problem?

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



  ### Build skeleton --------------------------------------
  base_url <- "https://generativelanguage.googleapis.com/v1/models/"
  url_model <- paste0(base_url, model, ":generateContent?key=", google_api_key)

  headers <- c(
    "Content-Type" = "application/json"
  )

  generationConfig <- list()
  generationConfig[["temperature"]] <- temperature
  generationConfig[["topP"]] <- top_p
  if (!is.null(top_k)) generationConfig[["topK"]] <- top_k
  generationConfig[["maxOutputTokens"]] <- max_tokens

  body <- list()
  body[["generationConfig"]] <- generationConfig



  ### Completion Function for Google -----------------------
  completion <- function(input, prompt) {
    body[["contents"]] <- list(
      list(
        "parts" = list(
          list(
            "text" = paste(prompt, input)
          )
        )
      )
    )

    response <- httr::POST(
      url = url_model,
      httr::add_headers(.headers = headers),
      body = body,
      encode = "json"
    )

    parsed <- response |>
      httr::content(as = "text", encoding = "UTF-8") |>
      jsonlite::fromJSON(flatten = TRUE)

    if (httr::http_error(response)) {
      parentInfo$http_error <- parentInfo$http_error + 1
      cli::cli_abort(c("Google API request failed[{httr::status_code(response)}]", x = "{parsed$error$message}"), call = call)
    }
    if (parsed$candidates$finishReason != "STOP") {
      parentInfo$safteyRestriction <- parentInfo$safteyRestriction + 1
      cli::cli_alert_warning(c("Gemini API refused to return completion for reason {parsed$candidates$finishReason}"), call = call)
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
        working_vec[i] <- tryCatch({completion(input[i], prompt)$candidates$`content.parts`[[1]]$text}, error = function(e) {
          #Suppress_line_messages = TRUE if this function is called from gpt.data.frame using repair mode and the indecies are wrong.
          if (suppress_line_messages == FALSE) {
            if(parentInfo$firstLineError == 0) {
              parentInfo$firstLineError <- i
            }
            cli::cli_alert_warning(c("Error: Returning NA in row {i}", i = "Message: {e$message}"))
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
    output_vector <- a$candidates$`content.parts`[[1]]$text
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

    if(parentInfo$safteyRestriction > 0) {
      cli::cli_alert_warning("There {? is/are} {parentInfo$safteyRestriction} saftey restriction{?s} reported from Google servers.")
      cli::cli_bullets(c(i = "{parentInfo$safteyRestriction} {? NA/NAs} introduced in the output."))
    }
  }


  ### Return Object --------------------------
  if(!exists("a", envir = environment())) {
    a <- completion(input = source[1], prompt = prompt)
  }
  if (prompt == "") {
    prompt <- source
  }
  output_vector <- new_llm_completion(output_vector, Call = Call, Prompt = prompt, Model = model, Model_Provider = "OpenAI", Date = Sys.Date(), Temperature = temperature, Raw = a)

  return(output_vector)
}