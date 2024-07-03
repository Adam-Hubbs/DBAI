print.llm_completion_DBAI <- function(x, ..., all = TRUE) {
  stopifnot(is.logical(all))
  cli::cli_text("{.bold `llm_completion`} object:\n")
  if (all == TRUE) {
    cli::cli_div(theme = list(span.dbai = list(color = "blue", "font-weight" = "bold")))
    attrs <- setdiff(names(attributes(x)), c("class", "names"))

    cli::cli_h2("Attributes:")
    #Figure out better printing for Raw list attribute
    #use cli::cli_ul()?
    for (attr in attrs) {
        cli::cli_text(paste0("{.dbai ", attr, ": } ", attr(x, attr)))
    }
    cli::cli_h2("Values:")
  } 
  #xchar <- as.character(x)
  xchar <- x |> attributes() <- NULL
  NextMethod(xchar)
}
print(aa)

validate_old <- function() {
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
  if(is.null(source)) {
    stop("Dataframe is null. Please provide a dataframe.")
  }

  if (any(class(source) == "llm_completion")) {
    source <- source$Result
  }

  if (!is.data.frame(source)) {
    stop("Input 'source' must be a dataframe.")
  } else if (nrow(source) == 0) {
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
  if(!is.logical(return_invisible) || length(return_invisible) != 1 || is.na(return_invisible)) {
    stop("Return Invisible must be a length one boolean.")
  }

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
}







#library(cli)
#call = caller_env() MUST be in the function arguments for the big guys in order for these errors traceback to work


cli::cli_abort(c("API Key not found.", i = "Please set the {.envvar OPENAI_API_KEY} environment variable."))

validate_new <- function() {
  ### Validate Statements ----------------------------------



  ### API Key
  if(is.null(openai_api_key) || openai_api_key == "") {
    cli::cli_abort(c("API Key not found.", i = "Please set the {.envvar OPENAI_API_KEY} environment variable.", i = "Did you forget to set {.envvar OPENAI_API_KEY} with {.code Sys.env(OPENAI_API_KEY='XXXXX')}?"), call = call)
    #stop("API Key not found. Please set the OPENAI_API_KEY environment variable.")
  }

  if (!is.character(openai_api_key) || length(openai_api_key) != 1) {
    cli::cli_abort(c("{.envvar OPENAI_API_KEY} must be a length one character vector.", i = "Did you forget to set {.envvar OPENAI_API_KEY} with {.code Sys.env(OPENAI_API_KEY='XXXXX')}?"), call = call)
    #stop("Error: 'openai_api_key' must be a character string.")
  }

  # if (length(openai_api_key) != 1) {
  #   stop("Error: 'openai_api_key' must be a single value.")
  # }



  ### Source
  if(is.null(source)) {
    cli::cli_abort(c("{.var source} is missing.", i = "Did you forget to pass a dataframe, vector, or string to the function?"), call = call)
    #stop("Dataframe is null. Please provide a dataframe.")
  }

  if (any(class(source) == "llm_completion")) {
    source <- source$Result
  }

  if (!is.data.frame(source)) {
    stop("Input 'source' must be a dataframe.")
  } else if (nrow(source) == 0) {
    stop("Dataframe is empty. Please provide a valid dataframe.")
  }



  ### Input
  if(is.null(input)) {
    #cols <- colnames(source)
    cols <- cli::cli_vec(c("Column1", "Betty", "Jake", "Harry", "Dominuque", "Jenny"), style = list("vec-trunc" = 4))
    cli::cli_abort(c("{.var input} is missing.", x = "When passing a dataframe to the {.code source} argument you must supply a column that exists in that dataframe.", i = "Column Names: {.var {cols}}."), call = call)
    
    #stop("Input column is null. Please provide a column name you would like to use for the inputs to the model.")
  } else if (!is.character(input)) {
    stop("Input column must be a string.")
  } else if (!input %in% colnames(source)) {
    stop("Input column does not exist in the dataframe.")
  }



  ### Prompt
  if(is.null(prompt)) {
    stop("Prompt is null. Please provide a prompt.")
  } else if (!is.character(prompt)) {
    prompt <- 1L
    cli::cli_abort(c("{.var prompt} (if supplied) must be a character vector.", x = "You supplied a {.cls {class(prompt)}} vector."), call = call)
    
    #stop("Prompt must be a string or vector of strings.")
  }




  ### Other Function Parameters
  if(!is.logical(return_invisible) || length(return_invisible) != 1 || is.na(return_invisible)) {
    stop("Return Invisible must be a length one boolean.")
  }

  if(!is.logical(repair) || length(repair) != 1 || is.na(repair)) {
    cli::cli_abort(c("{.var repair} must either be {.code TRUE} or {.code FALSE}."), call = call)
    #stop("Return Invisible must be a length one boolean.")
  }

  if(repair == TRUE) {
    output <- c("HEEEY", "a", "DOG")
    missing_cols <- setdiff(output, colnames(source))
    if (length(missing_cols) > 0) {
      cli::cli_abort(c("Output not found in source", i = "When using {.code repair=TRUE}, all elements of {.var output} must be present in {.var source}.", x = "{.var {missing_cols}} {?does/do} not appear in {.code source}."), call = call)
    }
      #stop("All elements of 'output' must be be present in 'source' dataframe when using repair mode. Please provide output columns that already exist in the dataframe or turn repair mode off.")
    }
  }

  if (!is.null(openai_organization) && (!is.character(openai_organization) || length(openai_organization) != 1 || is.na(openai_organization))) {
    cli::cli_abort(c("{.var openai_organization}, if supplied, must be a non-NA length one character vector."), call = call)
    #stop("Openai_organization must be a non-NA string.")
  }


  if (is.null(iterations) || !is.numeric(iterations) || length(iterations) != 1 || is.na(iterations) || iterations <= 0 || iterations %% 1 != 0) {
    cli::cli_abort(c("{.code iterations} must be a positive integer."), call = call)
    #stop("Iterations must be a positive integer.")
  }

  if (!is.logical(progress) || length(progress) != 1 || is.na(progress)) {
    cli::cli_abort(c("{.var progress} must either be {.var TRUE} or {.var FALSE}."), call = call)
    #stop("Progress must be a boolean.")
  }



   ### Other Model Parameters
  if (is.null(model) || !is.character(model) || length(model) != 1 || is.na(model)) {
    cli::cli_abort(c("{.var model}, must be a non-NA character vector."), call = call)
    
    #stop("Model must be a non-NA string.")
  }

  if (!is.numeric(temperature) || length(temperature) != 1 || is.na(temperature) || temperature < 0 || temperature > 2) {
    cli::cli_abort(c("{.var temperature}, must be a number between {.code 0} and {.code 2}."), call = call)
    #stop("Temperature must be a number between 0 and 2.")
  }

  if (!is.numeric(top_p) || length(top_p) != 1 || is.na(top_p) || top_p < 0 || top_p > 1) {
    cli::cli_abort(c("{.var top_p}, must be a number between {.code 0} and {.code 1}."), call = call)
    #stop("Top_p must be a number between 0 and 1.")
  }

  if (!is.null(temperature) && !is.null(top_p)) {
    if(temperature != 1 || top_p != 1) {
      cli::cli_alert_warning(c("It is not recommended to specify both {.var temperature} and {.var top_p} at the same time. Some models may refuse to generate if both values are supplied."), call = call)
      #warning("It is recommended NOT to specify temperature and top_p at the same time.")
    }
  }
  max_tokens <- 5994.1
  if (!is.null(max_tokens)) {
    if (!is.numeric(max_tokens) || length(max_tokens) != 1 || is.na(max_tokens) || max_tokens %% 1 != 0) {
      cli::cli_abort(c("{.var max_tokens}, must be an integer between {.code 0} and {.code 4096}.", x = "You supplied a length {length(max_tokens)} {.cls {typeof(max_tokens)}}"), call = call)
    }
    if (max_tokens <= 0 || max_tokens > 4096) {
    cli::cli_abort(c("{.var max_tokens}, must be an integer between {.code 0} and {.code 4096}.", x = "You supplied {.var {max_tokens}}"), call = call)
    }
  }

  if(!is.null(presence_penalty)) {
    if (!is.numeric(presence_penalty) || length(presence_penalty) != 1 || is.na(presence_penalty)) {
      cli::cli_abort(c("{.var presence_penalty}, must be a number between {.code -2} and {.code 2}.", x = "You supplied a length {length(presence_penalty)} {.cls {typeof(presence_penalty)}}"), call = call)
    }
    if (presence_penalty < -2 || presence_penalty > 2) {
      cli::cli_abort(c("{.var presence_penalty}, must be a number between {.code -2} and {.code 2}.", x = "You supplied {.var {presence_penalty}}"), call = call)
    }
  }

  if(!is.null(frequency_penalty)) {
    if (!is.numeric(frequency_penalty) || length(frequency_penalty) != 1 || is.na(frequency_penalty)) {
      cli::cli_abort(c("{.var frequency_penalty}, must be a number between {.code -2} and {.code 2}.", x = "You supplied a length {length(frequency_penalty)} {.cls {typeof(frequency_penalty)}}"), call = call)
    }
    if (frequency_penalty < -2 || frequency_penalty > 2) {
      cli::cli_abort(c("{.var frequency_penalty}, must be a number between {.code -2} and {.code 2}.", x = "You supplied {.var {frequency_penalty}}"), call = call)
    }
  }

  if (httr::http_error(response)) {
    parentInfo$http_error <- parentInfo$http_error + 1
    cli::cli_abort(c("OpenAI API request failed [{httr::status_code(response)}]", x = "{parsed$error$message}"), call = call, )
  }


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

  #How should we clean this up?
  ### Warnings and Errors -----------------------------
  if(parentInfo$firstLineError > 0) {
    cli_alert_warning("First Error Located in Row: {parentInfo$firstLineError}")
    cli_bullets(c(i = "This is probably a rate limit error. Check the website of the model provider for specific rate limits for your usage tier. Rerun this function again with repair=TRUE to continue processing once you are no longer rate-limited."))
    #warning(paste("First Error located in row", parentInfo$firstLineError, ". This is probably a rate limit error. Check the website of the model provider for specific rate limits for your usage tier. Rerun this function again with repair=TRUE to continue processing once you are no longer rate-limited."))
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
