#' gpt
#' @export
gpt.character <- function(source,
                prompt = "",
                progress = TRUE,
                repair = FALSE,
                iterations = 1,
                model = "gpt-3.5-turbo",
                temperature = 1,
                top_p = 1,
                n = 1,
                presence_penalty = 0,
                frequency_penalty = 0,
                max_tokens = 4096,
                openai_api_key = Sys.getenv("OPENAI_API_KEY"),
                openai_organization = NULL,
                call = rlang::caller_env(),
                output = "output",
                parentInfo = NULL) {

  ### Validate Statements ----------------------------------

  ### API Key
  if(is.null(openai_api_key) || openai_api_key == "") {
    cli::cli_abort(c("API Key not found.", i = "Please set the {.envvar OPENAI_API_KEY} environment variable.", i = "Did you forget to set {.envvar OPENAI_API_KEY} with {.code Sys.env(OPENAI_API_KEY='XXXXX')}?"), call = call)
  }

  if (!is.character(openai_api_key) || length(openai_api_key) != 1) {
    cli::cli_abort(c("{.envvar OPENAI_API_KEY} must be a length one character vector.", i = "Did you forget to set {.envvar OPENAI_API_KEY} with {.code Sys.env(OPENAI_API_KEY='XXXXX')}?"), call = call)
  }

  if(is.null(source)) {
    cli::cli_abort(c("{.var source} is missing.", i = "Did you forget to pass a dataframe, vector, or string to the function?"), call = call)
  }

  ### Prompt
  if (!is.null(prompt)) {
    if (!is.character(prompt)) {
      cli::cli_abort(c("{.var prompt} (if supplied) must be a character vector.", x = "You supplied a {.cls {class(prompt)}} vector."), call = call)
    }
  }

  ### Other Function Parameters
  if (!is.null(openai_organization) && (!is.character(openai_organization) || length(openai_organization) != 1)) {
    cli::cli_abort(c("{.var openai_organization} must be a length one character vector."), call = call)
  }

  ### Progress
  if (!is.logical(progress) || length(progress) != 1 || is.na(progress)) {
    cli::cli_abort(c("{.var progress} must either be {.var TRUE} or {.var FALSE}."), call = call)
  }

  ### Other Model Parameters
  if (is.null(model) || !is.character(model) || length(model) != 1 || is.na(model)) {
    cli::cli_abort(c("{.var model} must be a non-NA character vector."), call = call)
  }

  if (!is.numeric(temperature) || length(temperature) != 1 || is.na(temperature) || temperature < 0 || temperature > 2) {
    cli::cli_abort(c("{.var temperature} must be a number between {.code 0} and {.code 2}.", x = "You supplied {.var {temperature}}."), call = call)
  }

  if (!is.numeric(top_p) || length(top_p) != 1 || is.na(top_p) || top_p < 0 || top_p > 1) {
    cli::cli_abort(c("{.var top_p} must be a number between {.code 0} and {.code 1}.", x = "You supplied {.var {top_p}}."), call = call)
  }

  if (!is.null(temperature) && !is.null(top_p)) {
    if(temperature != 1 || top_p != 1) {
      cli::cli_alert_warning(c("It is not recommended to specify both {.var temperature} and {.var top_p} at the same time. Some models may refuse to generate if both values are supplied."))
    }
  }

  if (!is.null(max_tokens)) {
    if (!is.numeric(max_tokens) || length(max_tokens) != 1 || is.na(max_tokens) || max_tokens %% 1 != 0) {
      cli::cli_abort(c("{.var max_tokens} must be an integer between {.code 0} and {.code 4096}.", x = "You supplied a length {length(max_tokens)} {.cls {typeof(max_tokens)}} vector."), call = call)
    }
    if (max_tokens <= 0 || max_tokens > 4096) {
    cli::cli_abort(c("{.var max_tokens} must be an integer between {.code 0} and {.code 4096}.", x = "You supplied {.var {max_tokens}}."), call = call)
    }
  }

  if(!is.null(presence_penalty)) {
    if (!is.numeric(presence_penalty) || length(presence_penalty) != 1 || is.na(presence_penalty)) {
      cli::cli_abort(c("{.var presence_penalty} must be a number between {.code -2} and {.code 2}.", x = "You supplied a length {length(presence_penalty)} {.cls {typeof(presence_penalty)}} vector."), call = call)
    }
    if (presence_penalty < -2 || presence_penalty > 2) {
      cli::cli_abort(c("{.var presence_penalty} must be a number between {.code -2} and {.code 2}.", x = "You supplied {.var {presence_penalty}}."), call = call)
    }
  }


  if(!is.null(frequency_penalty)) {
    if (!is.numeric(frequency_penalty) || length(frequency_penalty) != 1 || is.na(frequency_penalty)) {
      cli::cli_abort(c("{.var frequency_penalty} must be a number between {.code -2} and {.code 2}.", x = "You supplied a length {length(frequency_penalty)} {.cls {typeof(frequency_penalty)}}."), call = call)
    }
    if (frequency_penalty < -2 || frequency_penalty > 2) {
      cli::cli_abort(c("{.var frequency_penalty} must be a number between {.code -2} and {.code 2}.", x = "You supplied {.var {frequency_penalty}}."), call = call)
    }
  }

  ### End Validation ----------------------------------
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



  ### Build skeleton ---------------------------------
  base_url <- "https://api.openai.com/v1/chat/completions"

  headers <- c(
    "Authorization" = paste("Bearer", openai_api_key),
    "Content-Type" = "application/json"
  )

  if (!is.null(openai_organization)) {
    headers["OpenAI-Organization"] <- openai_organization
  }

  body <- list()
  body[["model"]] <- model
  body[["temperature"]] <- temperature
  body[["top_p"]] <- top_p
  body[["n"]] <- n
  body[["max_tokens"]] <- max_tokens
  body[["presence_penalty"]] <- presence_penalty
  body[["frequency_penalty"]] <- frequency_penalty



  ### Completion Function for OpenAI -----------------------
  completion <- function(input, prompt) {
    body[["messages"]] <- list(
      list(
        "role" = "system",
        "content" = prompt
      ),
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
    cli::cli_abort(c("OpenAI API request failed[{httr::status_code(response)}]", x = "{parsed$error$message}"), call = call)
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
        working_vec[i] <- tryCatch({completion(input[i], prompt)$choices$message.content}, error = function(e) {
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
    output_vector <- a$choices$message.content
  }




  ### Warnings and Errors -----------------------------
  if(parentInfo$firstLineError > 0) {
    cli::cli_alert_warning("First Error Located in Row: {parentInfo$firstLineError}")
    cli::cli_bullets(c(i = "This is could be a rate limit error. Check the website of the model provider for specific rate limits for your usage tier. Rerun this function again with {.code repair=TRUE} to continue processing once you are no longer rate-limited."))
  }

  if(parentInfo$http_error > 0) {
    cli::cli_alert_warning("There are {parentInfo$http_error} {?error/errors} from OpenAI servers.")
  }

  if(parentInfo$NACount > 0) {
    cli::cli_alert_warning("There are {parentInfo$NACount} missing {? value/values} in the input column.")
    cli::cli_bullets(c(i = "{parentInfo$NACount} {? NA/NA's} introduced in the output."))
  }

  if(parentInfo$EmptyCount > 0) {
    cli::cli_alert_warning("There are {parentInfo$EmptyCount} empty {? string/strings} in the input column.")
    cli::cli_bullets(c(i = "{parentInfo$EmptyCount} empty {? string/string's} introduced in the output."))
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
