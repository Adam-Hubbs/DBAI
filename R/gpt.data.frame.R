#' gpt
#' @param source required; A source dataframe or llm-completion object.
#' @param input required; A column name in the source dataframe
#' @param output optional; A string of a column name (Or a vector of strings) to be created in the source dataframe storing the output of the models. Defaults to `output`.
#' @param prompt required; A string (Or vector of Strings for handling multiple operations at the same time) of prompts to be sent to the AI model.
#' @param model required; a length one character vector.
#' @param return_invisible optional; A boolean to return just the output (`TRUE`) or an llm-completion object containing model metadata (`FALSE`). Defaults to `FALSE`.
#' @param iterations optional; An integer. Number of completions to generate for each prompt Defaults to `1`.
#' @param repair optional; A boolean to repair NA's in the output column and keep values already present in the output column if the output column has already been created. False overrides the data already in an output column if it exists. Useful to continue a computation if you have been rate limited. Defaults to `FALSE`.
#' @param progress optional; a length one logical vector. Defaults to `TRUE`. Determines whether to show a progress bar in the console. Not available when using repair mode.
#' @param temperature optional; defaults to `1`; a length one numeric vector with the value between `0` (More analytical) and `2` (More creative).
#' @param top_p optional; defaults to `1`; a length one numeric vector with the value between `0` and `1`.
#' @param n optional; defaults to `1`; a length one numeric vector with the integer value greater than `0`.
#' @param presence_penalty optional; defaults to `0`; a length one numeric vector with a value between `-2` and `2`.
#' @param frequency_penalty optional; defaults to `0`; a length one numeric vector with a value between `-2` and `2`.
#' @param max_tokens optional; defaults to `(4096 - prompt tokens)`; a length one numeric vector with the integer value greater than `0`.
#' @param openai_api_key required; defaults to `Sys.getenv("OPENAI_API_KEY")` (i.e., the value is retrieved from the `.Renviron` file); a length one character vector. Specifies OpenAI API key. Must obtain API Key from OpenAI.
#' @param openai_organization optional; defaults to `NULL`; a length one character vector. Specifies OpenAI organization.
#' @return A dataframe with the output column(s) created
#' @export
gpt.data.frame <- function(source,
                input,
                output = "output",
                prompt,
                model = "gpt-3.5-turbo",
                return_invisible = FALSE,
                iterations = 1,
                repair = FALSE,
                progress = TRUE,
                temperature = 1,
                top_p = 1,
                n = 1,
                presence_penalty = 0,
                frequency_penalty = 0,
                max_tokens = 4096,
                openai_api_key = Sys.getenv("OPENAI_API_KEY"),
                openai_organization = NULL) {


  ### Repair mode support llm-completion object as well as Dataframe
  ### Add support for Matricies (And Arrays?)


  ### Validate Statements ----------------------------------



  ### API Key
  if(is.null(openai_api_key) || openai_api_key == "") {
    stop("API Key not found. Please set the OPENAI_API_KEY environment variable.")
  }

  if (!is.character(openai_api_key)) {
    stop("Error: 'openai_api_key' must be a character string.")
  }

  if (length(openai_api_key) != 1) {
    stop("Error: 'openai_api_key' must be a single value.")
  }



  ### Source
  if (nrow(source) == 0) {
    stop("Dataframe is empty. Please provide a valid dataframe.")
  }



  ### Input
  if(is.null(input)) {
    stop("Input column is null. Please provide a column name you would like to use for the inputs to the model.")
  } else if (!is.character(input)) {
    stop("Input column must be a string.")
  } else if (!input %in% colnames(source)) {
    stop("Input column does not exist in the dataframe.")
  }



  ### Prompt
  if(is.null(prompt)) {
    stop("Prompt is null. Please provide a prompt.")
  } else if (!is.character(prompt)) {
    stop("Prompt must be a string or vector of strings.")
  }



  ### Other Function Parameters
  if(!is.logical(repair) || length(repair) != 1 || is.na(repair)) {
    stop("Return Invisible must be a length one boolean.")
  }

  if(repair == TRUE) {
    if (!all(output %in% colnames(source))) {
      stop("All elements of 'output' must be be present in 'source' dataframe when using repair mode. Please provide output columns that already exist in the dataframe or turn repair mode off.")
    }
  }

  if (!is.null(openai_organization) && (!is.character(openai_organization) || length(openai_organization) != 1 || is.na(openai_organization))) {
    stop("Openai_organization must be a non-NA string.")
  }

  if (is.null(iterations) || !is.numeric(iterations) || length(iterations) != 1 || is.na(iterations) || iterations <= 0 || iterations %% 1 != 0) {
    stop("Iterations must be a positive integer.")
  }

  if (!is.logical(progress) || length(progress) != 1 || is.na(progress)) {
    stop("Progress must be a boolean.")
  }



  ### Other Model Parameters
  if (is.null(model) || !is.character(model) || length(model) != 1 || is.na(model)) {
    stop("Model must be a non-NA string.")
  }

  if (!is.numeric(temperature) || length(temperature) != 1 || is.na(temperature) || temperature < 0 || temperature > 2) {
    stop("Temperature must be a number between 0 and 2.")
  }

  if (!is.numeric(top_p) || length(top_p) != 1 || is.na(top_p) || top_p < 0 || top_p > 1) {
    stop("Top_p must be a number between 0 and 1.")
  }

  if (!is.null(temperature) && !is.null(top_p)) {
    if(temperature != 1 || top_p != 1) {
      warning("It is recommended NOT to specify temperature and top_p at the same time.")
    }
  }

  if (!is.null(max_tokens) && (!is.numeric(max_tokens) || length(max_tokens) != 1 || is.na(max_tokens) || max_tokens <= 0 || max_tokens %% 1 != 0 || max_tokens > 4096)) {
    stop("Max_tokens must be a positive integer.")
  }

  if (!is.numeric(presence_penalty) || length(presence_penalty) != 1 || is.na(presence_penalty) || presence_penalty < -2 || presence_penalty > 2) {
    stop("Presence_penalty must be a number between -2 and 2.")
  }

  if (!is.numeric(frequency_penalty) || length(frequency_penalty) != 1 || is.na(frequency_penalty) || frequency_penalty < -2 || frequency_penalty > 2) {
    stop("Frequency_penalty must be a number between -2 and 2.")
  }

  ### End Validation ----------------------------------


  ### Initialize Dummy Environment for Pass by reference system --
  parentInfo <- new.env()
  parentInfo$NACount <- 0L
  parentInfo$EmptyCount <- 0L
  parentInfo$http_error <- 0
  parentInfo$firstLineError <- 0L

  llmObj <- NULL



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
      paste0(
        "OpenAI API request failed [",
        httr::status_code(response),
        "]:\n\n",
        parsed$error$message
      ) |>
        stop(call. = FALSE)
    }
    parsed
  }


  ### Main Call. Checks if input is valid and calls the completion function -----------------------
  CallGPT <- function(parentInfo, input, prompt) {
    if(progress == TRUE) parentInfo$pb$tick()
    ### Do not quit if there are NA's, just return NA for those rows
    if(is.na(input)) {
      parentInfo$NACount <- parentInfo$NACount + 1
      return(NA)
    } else if (input == "" || input == " ") {
      parentInfo$EmptyCount <- parentInfo$EmptyCount + 1
      return("")
    } else {
      ### Call to OpenAI Endpoint
      completion(input, prompt)$choices$message.content
    }
  }


  ### Prepare the dataframe -----------------------------
  source <- source |>
    dplyr::mutate(DBAI_Index_Row_Number = dplyr::row_number()) |>
    dplyr::rowwise()



  ### Main Loop ----------------------------------
  if(repair == TRUE) {
    for (iter in 1:iterations) {
      for (h in seq_along(prompt)) {
        if (iter == 1) {
          outputcol <- output[h]
        } else {
          outputcol <- paste0(output[h], "_", iter)
        }
        source <- source |>
          dplyr::mutate(!!sym(outputcol) := if_else(is.na(!!sym(outputcol)),
                                                    tryCatch(CallGPT(parentInfo, !!sym(input), prompt = prompt[h]), error = function(e) {
                                                      return(NA)
                                                    }), !!sym(outputcol))
          )
      }
    }
  } else {
    for (iter in 1:iterations) {
      for (h in seq_along(prompt)) {
        if (iter == 1) {
          outputcol <- output[h]
        } else {
          outputcol <- paste0(output[h], "_", iter)
        }
        source <- source |>
          dplyr::mutate(!!outputcol := tryCatch(CallGPT(parentInfo, !!sym(input), prompt = prompt[h]), error = function(e) {
            if(parentInfo$firstLineError == 0) {
              parentInfo$firstLineError <- DBAI_Index_Row_Number
            }
            message(paste("Error: Returning NA in row", DBAI_Index_Row_Number, "Message:", e$message))
            return(NA)
          }))
      }
    }
  }



  ### Clean up dataframe -----------------------------
  source <- source |>
    dplyr::ungroup() |>
    dplyr::select(-DBAI_Index_Row_Number)



  ### Warnings and Errors -----------------------------
  if(parentInfo$firstLineError > 0) {
    warning(paste("First Error located in row", parentInfo$firstLineError, ". This is probably a rate limit error. Check the website of the model provider for specific rate limits for your usage tier. Rerun this function again with repair=TRUE to continue processing once you are no longer rate-limited."))
  }

  if(parentInfo$http_error > 0) {
    warning(paste("There are", parentInfo$http_error, "error(s) returned from OpenAI servers."))
  }

  if(parentInfo$NACount > 0) {
    warning(paste("There are", parentInfo$NACount, "missing values in the input column.", parentInfo$NACount, "NA's introduced in the output."))
  }

  if(parentInfo$EmptyCount > 0) {
    warning(paste("There are", parentInfo$EmptyCount, "empty strings in the input column.", parentInfo$EmptyCount, "empty strings introduced in the output."))
  }



  ### Return Object --------------------------
  attr(source, "Model") <- model
  attr(source, "Model_Provider") <- "OpenAI"
  attr(source, "Date") <- Sys.Date()
  attr(source, "Raw") <- completion(input = source[[input]][1], prompt = prompt[1])
  attr(source, "Prompt") <- prompt
  class(source) <- c("llm_completion", class(source))
  return(source)
}
