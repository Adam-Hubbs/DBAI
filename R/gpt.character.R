#' gpt
#' @export
gpt.character <- function(source,
                prompt = "",
                progress = TRUE,
                repair = FALSE,
                iterations = 1,
                model = "gpt-3.5-turbo",
                temperature = NULL,
                top_p = NULL,
                presence_penalty = NULL,
                frequency_penalty = NULL,
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
                call = rlang::caller_env(),
                output = "output",
                parentInfo = NULL) {


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

  ### Validate Statements ----------------------------------

  if (parentInfo$df != TRUE) {
    ### API KEY
    if(is.null(openai_api_key) || openai_api_key == "" || is.na(openai_api_key)) {
      cli::cli_abort(c("API Key not found.", i = "Please set the {.envvar OPENAI_API_KEY} environment variable.", i = "Did you forget to set {.envvar OPENAI_API_KEY} with {.code Sys.env(OPENAI_API_KEY='XXXXX')}?"), call = call)
    }

    if (!is.character(openai_api_key) || length(openai_api_key) != 1) {
      cli::cli_abort(c("{.envvar OPENAI_API_KEY} must be a length one character vector.", i = "Did you forget to set {.envvar OPENAI_API_KEY} with {.code Sys.env(OPENAI_API_KEY='XXXXX')}?"), call = call)
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
    if (!is.null(openai_organization) && (!is.character(openai_organization) || length(openai_organization) != 1)) {
      cli::cli_abort(c("{.var openai_organization} must be a length one character vector."), call = call)
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

    ### MAX TOKENS
    if (!is.null(max_tokens)) {
      if (!is.numeric(max_tokens) || length(max_tokens) != 1 || is.na(max_tokens) || max_tokens %% 1 != 0) {
        cli::cli_abort(c("{.var max_tokens} must be an integer between {.code 0} and {.code 4096}.", x = "You supplied a length {length(max_tokens)} {.cls {typeof(max_tokens)}} vector."), call = call)
      }
      if (max_tokens <= 0 || max_tokens > 4096) {
        cli::cli_abort(c("{.var max_tokens} must be an integer between {.code 0} and {.code 4096}.", x = "You supplied {.var {max_tokens}}."), call = call)
      }
    }

    ### PRESENCE PENALTY
    if(!is.null(presence_penalty)) {
      if (!is.numeric(presence_penalty) || length(presence_penalty) != 1 || is.na(presence_penalty)) {
        cli::cli_abort(c("{.var presence_penalty} must be a number between {.code -2} and {.code 2}.", x = "You supplied a length {length(presence_penalty)} {.cls {typeof(presence_penalty)}} vector."), call = call)
      }
      if (presence_penalty < -2 || presence_penalty > 2) {
        cli::cli_abort(c("{.var presence_penalty} must be a number between {.code -2} and {.code 2}.", x = "You supplied {.var {presence_penalty}}."), call = call)
      }
    }

    ### FREQUENCY PENALTY
    if(!is.null(frequency_penalty)) {
      if (!is.numeric(frequency_penalty) || length(frequency_penalty) != 1 || is.na(frequency_penalty)) {
        cli::cli_abort(c("{.var frequency_penalty} must be a number between {.code -2} and {.code 2}.", x = "You supplied a length {length(frequency_penalty)} {.cls {typeof(frequency_penalty)}}."), call = call)
      }
      if (frequency_penalty < -2 || frequency_penalty > 2) {
        cli::cli_abort(c("{.var frequency_penalty} must be a number between {.code -2} and {.code 2}.", x = "You supplied {.var {frequency_penalty}}."), call = call)
      }
    }

    ### Logbrobs and top_logprobs
    if (!is.null(logprobs)) {
      if (!is.logical(logprobs) || length(logprobs) != 1 || is.na(logprobs)) {
        cli::cli_abort(c("{.var logprobs}, if supplied, must either be {.code TRUE} or {.code FALSE}."), call = call)
      }
      if (logprobs == TRUE) {
        if (!is.null(top_logprobs)) {
          if (!is.numeric(top_logprobs) || length(top_logprobs) != 1 || is.na(top_logprobs) || top_logprobs < 0 || top_logprobs > 20) {
            cli::cli_abort(c("{.var top_logprobs}, if supplied, must be a number between {.code 0} and {.code 20}.", x = "You supplied {.var {top_logprobs}}."), call = call)
          }
        }
      }
    } else {
      if (!is.null(top_logprobs)) {
        cli::cli_abort(c("{.var top_logprobs} cannot be supplied without {.var logprobs} being {.code TRUE}.", call = call))
      }
    }


    ### Seed
    if (!is.null(seed)) {
      if (!is.numeric(seed) || length(seed) != 1 || is.na(seed) || seed < 0) {
        cli::cli_abort(c("{.var seed}, if supplied, must be a number greater than {.code 0}.", x = "You supplied {.var {seed}}."), call = call)
      }
    }

    ### Stop
    if (!is.null(stop)) {
      if (!is.character(stop) || length(stop) > 4 || is.na(stop)) {
        cli::cli_abort(c("{.var stop}, if supplied, must be a length 1 to 4 character vector."), call = call)
      }
    }

    ### User
    if (!is.null(user)) {
      if (!is.character(user) || length(user) != 1 || is.na(user)) {
        cli::cli_abort(c("{.var user}, if supplied, must be a length one character vector."), call = call)
      }
    }
  }

    ### Is Reasoning Model
    if(is.null(is_reasoning_model)) {
      if (!is.null(reasoning_effort) || !is.null(max_completion_tokens)) {
        is_reasoning_model <- TRUE
        cli::cli_alert_warning(c("Inferred {.var is_reasoning_model} to be {.code TRUE} based on the presence of reasoning parameters."))
      } else {
        is_reasoning_model <- FALSE
      }

    }

      if (!is.logical(is_reasoning_model) || length(is_reasoning_model) != 1 || is.na(is_reasoning_model)) {
        cli::cli_abort(c("{.var is_reasoning_model} must either be {.var TRUE} or {.var FALSE}."), call = call)
      }

      if (is_reasoning_model == TRUE) {
        ### If using outdated model parameters, error
        if (max_tokens != 4096) {
          cli::cli_alert_warning(c("{.var max_tokens} is being ignored because {.var is_reasoning_model} is {.code TRUE}. Using {.code max_completion_tokens} instead."))
        }


        ### Reasoning Effort
        if (!is.null(reasoning_effort)) {
          if (!is.character(reasoning_effort) || length(reasoning_effort) != 1 || is.na(reasoning_effort)) {
            cli::cli_abort(c("{.var reasoning_effort} must be a length one character vector."), call = call)
          }
          if (!reasoning_effort %in% c("low", "medium", "high")) {
            cli::cli_abort(c("{.var reasoning_effort} must be one of c(`low`, `medium`, `high`).", x = "You supplied {.var {reasoning_effort}}."), call = call)
          }
        }



        ### Max Completion Tokens
        if (!is.null(max_completion_tokens)) {
          if (!is.numeric(max_completion_tokens) || length(max_completion_tokens) != 1 || is.na(max_completion_tokens) || max_completion_tokens %% 1 != 0) {
            cli::cli_abort(c("{.var max_completion_tokens} must be an integer greater than {.code 0}.", x = "You supplied a length {length(max_completion_tokens)} {.cls {typeof(max_completion_tokens)}} vector."), call = call)
          }
          if (max_completion_tokens <= 0) {
            cli::cli_abort(c("{.var max_completion_tokens} must be an integer greater than {.code 0}.", x = "You supplied {.var {max_completion_tokens}}."), call = call)
          }
        }
      } else { # is_reasoning_model == FALSE


        ### If using reasoning model parameters, error
        if (!is.null(max_completion_tokens)) {
          cli::cli_alert_warning(c("{.var max_completion_tokens} is being ignored because {.var is_reasoning_model} is {.code FALSE}. Using {.code max_tokens} instead."))
        }



        ### If reasoning specific parameters exist, error
        if (!is.null(reasoning_effort)) {
          cli::cli_alert_warning(c("{.var reasoning_effort}is being ignored because {.var is_reasoning_model} is {.code FALSE}."))
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
  body[["presence_penalty"]] <- presence_penalty
  body[["frequency_penalty"]] <- frequency_penalty

  if (is_reasoning_model == TRUE) {
    body[["reasoning_effort"]] <- reasoning_effort
    body[["max_completion_tokens"]] <- max_completion_tokens
  } else {
    body[["max_tokens"]] <- max_tokens
  }




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
      if(httr::status_code(response) == 404) {
        cli::cli_abort(c("OpenAI API request failed [404]."), footer = {parsed$error$message}, call = call, class = "404")
      } else {
        #If the error message contains the string 'Use 'max_completion_tokens' instead' then cli abort with class of 'is_reasoning_model'
        if (grepl("Use 'max_completion_tokens' instead", parsed$error$message)) {
          cli::cli_abort(c("OpenAI API request failed [{httr::status_code(response)}]", i = "You are probably attempting to use a reasoning model with parameters meant for non-reasoning models.", i = "Set {.var is_reasoning_model} = {.code TRUE}.", x = "{parsed$error$message}"), call = call, class = "is_reasoning_model")
        } else {
          cli::cli_abort(c("OpenAI API request failed [{httr::status_code(response)}]", x = "{parsed$error$message}"), call = call)
        }
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
        working_vec[i] <- tryCatch({completion(input[i], prompt)$choices$message.content}, error = function(e) {
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
    output_vector <- a$choices$message.content
  }




  ### Warnings and Errors -----------------------------
  if(parentInfo$df != TRUE) {
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
