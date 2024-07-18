#' gpt
#' @param source required; A source dataframe or llm-completion object.
#' @param input required; A column name in the source dataframe.
#' @param output optional; A string of a column name (Or a vector of strings) to be created in the source dataframe storing the output of the models. Defaults to `output`.
#' @param prompt required; A string (Or vector of Strings for handling multiple operations at the same time) of prompts to be sent to the AI model.
#' @param model required; a length one character vector.
#' @param iterate_over optional; The variable that you want to iterate over. Defaults to `prompt`.
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
                iterate_over = "prompt",
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










  #iterate_over


  ### Initialize Dummy Environment for Pass by reference system --
  parentInfo <- new.env()
  parentInfo$NACount <- 0L
  parentInfo$EmptyCount <- 0L
  parentInfo$http_error <- 0
  parentInfo$firstLineError <- 0L


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
  var_list <- c("input", "output", "prompt", "model", "temperature", "top_p", "n", "presence_penalty", "frequency_penalty", "max_tokens")


  length_list <- sapply(mget(var_list), length)
  max_len <- max(length_list)
  vector_vars <- length_list == max_len
  invalid_vars <- var_list[!length_list %in% c(0, 1, max_len)]
  invalid_vars
  if (length(invalid_vars) > 0) {
    cli::cli_abort(c(
      "Some variables have invalid lengths.",
      "i" = "All variables should have length {.code 1} or {.code {max_len}} (to match the longest variable).",
      "x" = "The following variable{?s} ha{?s/ve} {? /an /}invalid length{?s}: {.val {invalid_vars}}"
    ))
  }

  ### Repair mode
  if (repair == TRUE) {
    na_index <- c()
    for (out in output) {
      na_index[1] <- which(is.na(source[[out]]))
    }
  }


  ### Main Loop ----------------------------------
  if(repair == TRUE) {
    for (iter in 1:iterations) {
      for (h in seq_along(prompt)) {
        if (iter == 1) {
          outputcol <- output[h]
        } else {
          outputcol <- paste0(output[h], "_", iter)
        }
        na_output <- which(is.na(source[[outputcol]]))
        na_input <- source[[input]][na_output]
        source[[outputcol]][na_output] <- gpt(source = na_input, prompt = prompt[h], progress = progress, model = model, temperature = temperature, top_p = top_p, n = n, presence_penalty = presence_penalty, frequency_penalty = frequency_penalty, max_tokens = max_tokens, openai_organization = openai_organization)
      }
    }
  } else {
    source <- source |>
      dplyr::ungroup()
    for (iter in 1:iterations) {
      for (h in seq_along(prompt)) {
        if (iter == 1) {
          outputcol <- output[h]
        } else {
          outputcol <- paste0(output[h], "_", iter)
        }
        source <- source |>
          dplyr::mutate(!!outputcol := gpt(source = !!sym(input), prompt = prompt[h], progress = progress, model = model, temperature = temperature, top_p = top_p, n = n, presence_penalty = presence_penalty, frequency_penalty = frequency_penalty, max_tokens = max_tokens, openai_organization = openai_organization))
      }
    }
  }



  ### Clean up dataframe -----------------------------
  source <- source |>
    dplyr::ungroup()



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
  return(source)
}
