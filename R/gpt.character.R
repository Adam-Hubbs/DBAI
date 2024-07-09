#' gpt
#' @export
gpt.character <- function(source,
                prompt = "",
                progress = TRUE,
                model = "gpt-3.5-turbo",
                temperature = 1,
                top_p = 1,
                n = 1,
                presence_penalty = 0,
                frequency_penalty = 0,
                max_tokens = 4096,
                openai_api_key = Sys.getenv("OPENAI_API_KEY"),
                openai_organization = NULL) {

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



  ### Prompt
  if (!is.character(prompt)) {
    stop("Prompt must be a string or vector of strings.")
  }



  ### Other Function Parameters
  if (!is.null(openai_organization) && (!is.character(openai_organization) || length(openai_organization) != 1 || is.na(openai_organization))) {
    stop("Openai_organization must be a non-NA string.")
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

  Call <- match.call.defaults()

  ### Initialize Dummy Environment for Pass by reference system --
  parentInfo <- new.env()
  parentInfo$NACount <- 0L
  parentInfo$EmptyCount <- 0L
  parentInfo$http_error <- 0
  parentInfo$firstLineError <- 0L

 if(length(source) < 3) {
   progress <- FALSE
 }


  ### Initialize Progress Bar -----------------------------
  if (progress == TRUE) {
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




  ### Main Loop ----------------------------------
  process_element <- function(input, index) {
    tryCatch({
      result <- CallGPT(parentInfo, input, prompt)
      return(result)
    }, error = function(e) {
      if(parentInfo$firstLineError == 0) {
        parentInfo$firstLineError <- index
      }
      message(paste("Error: Returning NA in row", index, "Message:", e$message))
      return(NA)
    })
  }

  # Apply the function to each element of the source vector
  if (length(source) > 1) {
    output_vector <- unname(mapply(process_element, source, 1:length(source)))
  } else {
    a <- completion(input = source, prompt = prompt)
    output_vector <- a$choices$message.content
  }




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
  attr(output_vector, "Model") <- model
  attr(output_vector, "Model_Provider") <- "OpenAI"
  attr(output_vector, "Date") <- Sys.Date()
  if(!exists("a", envir = environment())) {
    attr(output_vector, "Raw") <- completion(input = source[1], prompt = prompt)
  } else {
    attr(output_vector, "Raw") <- a
  }
  attr(output_vector, "Prompt") <- prompt
  class(output_vector) <- c("llm_completion", class(output_vector))
  return(output_vector)
}
