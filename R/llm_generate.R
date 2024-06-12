#' llm_generate
#' @param source required; A source dataframe or llm-completion object.
#' @param input required; A column name in the source dataframe
#' @param output optional; A string of a column name (Or a vector of strings) to be created in the source dataframe storing the output of the models. Defaults to `output`.
#' @param prompt required; A string (Or vector of Strings for handling multiple operations at the same time) of prompts to be sent to the AI model.
#' @param model required; a length one character vector.
#' @param return_invisible optional; A boolean to return just the output (`TRUE`) or an llm-completion object containing model metadata (`FALSE`). Defaults to `FALSE`.
#' @param iterations optional; An integer. Number of completions to generate for each prompt Defaults to `1`.
#' @param repair optional; A boolean to repair NA's in the output column and keep values already present in the output column if the output column has already been created. False overrides the data already in an output column if it exists. Useful to continue a computation if you have been rate limited. Defaults to `FALSE`.
#' @param progress optional; a length one logical vector. Defaults to `TRUE`. Determines whether to show a progress bar in the console. Not available when using repair mode.
#' @param temperature optional; defaults to `1`; a length one numeric vector with the value between `0` (More analytical) and `2` (More creative). `0-2` for OpenAI and Google models, `0-1` for Anthropic models.
#' @param top_p optional; defaults to `1`; a length one numeric vector with the value between `0` and `1`.
#' @param top_k optional; a length one numeric vector with the integer value greater than `0`. Only sample from the top_k options for each subsequent token. Not recommended, for most use cases use temperature instead.
#' @param anthropic_version optional; defaults to `2023-06-01`; a length one character vector. Specifies the version of the Anthropic's models.
#' @param n optional; defaults to `1`; a length one numeric vector with the integer value greater than `0`.
#' @param presence_penalty optional; defaults to `0`; a length one numeric vector with a value between `-2` and `2`.
#' @param frequency_penalty optional; defaults to `0`; a length one numeric vector with a value between `-2` and `2`.
#' @param max_tokens optional; defaults to `(4096 - prompt tokens)`; a length one numeric vector with the integer value greater than `0`.
#' @param openai_api_key optional; defaults to `Sys.getenv("OPENAI_API_KEY")` (i.e., the value is retrieved from the `.Renviron` file); a length one character vector. Specifies OpenAI API key. Must obtain API Key from OpenAI.
#' @param openai_organization optional; defaults to `NULL`; a length one character vector. Specifies OpenAI organization.
#' @param anthropic_api_key optional; defaults to `Sys.getenv("ANTHROPIC_API_KEY")` (i.e., the value is retrieved from the `.Renviron` file); a length one character vector. Specifies Anthropic API key.
#' @param google_api_key optional; defaults to `Sys.getenv("GOOGLE_API_KEY")` (i.e., the value is retrieved from the `.Renviron` file); a length one character vector. Specifies Google API key. Must obtain API Key from Google.
#' @return A dataframe with the output column(s) created
#' @export
llm_generate <- function(source,
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
                top_k = NULL,
                anthropic_version = "2023-06-01",
                n = 1,
                presence_penalty = 0,
                frequency_penalty = 0,
                max_tokens = 4096,
                openai_api_key = Sys.getenv("OPENAI_API_KEY"),
                openai_organization = NULL,
                anthropic_api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                google_api_key = Sys.getenv("GOOGLE_API_KEY"))
{



  #Everything is up to date and working! Gemini and Clause and now stable. I've added a function called llm_completion that can handle all model providers. I think this function should be the default moving forward because it abstracts the model providers and one function is easier to remember than four. I've updated documentation to reflect that. This is open to discussion though.I have added a repair mode. If you are running a large dataset through this function and there are errors on the model providers end, or you are rate limited, the function automatically saves what has been done so far. With repair mode, you can pass it a dataframe or llm_completion object and it will keep what has already been done and pick up right where it left off, saving time and money and making it so you don't have to manua;ly subset the dataframe, run the function again, then recomine it later. The documentation has also been updated and uploaded to Box (the .qmd file). I think everything in the package is in a pretty good shape right now. Testing by your other RA's would be appritiated. I think this is ready to use in a presentation now. Just as another reminder, I will be getting married this week, and I will be available for questions and bug fixes until this thursday, then I will be unavailable unitl June  24th.

  ### Repair mode support llm-completion object as well as Dataframe
  ### Add support for Matricies (And Arrays?)


  ### Validate Statements ----------------------------------


  lookup_table <- list(
    "gpt-3.5-turbo-16k" = "openai",
    "gpt-3.5-turbo-instruct" = "openai",
    "gpt-4-turbo-2024-04-09" = "openai",
    "gpt-4-turbo" = "openai",
    "gpt-4-1106-preview" = "openai",
    "gpt-3.5-turbo-1106" = "openai",
    "gpt-4-0125-preview" = "openai",
    "gpt-3.5-turbo-0125" = "openai",
    "gpt-3.5-turbo" = "openai",
    "gpt-3.5-turbo-0301" = "openai",
    "gpt-4-turbo-preview" = "openai",
    "gpt-4o-2024-05-13" = "openai",
    "gpt-3.5-turbo-instruct-0914" = "openai",
    "gpt-3.5-turbo-16k-0613" = "openai",
    "gpt-4" = "openai",
    "gpt-4-1106-vision-preview" = "openai",
    "gpt-4-0613" = "openai",
    "gpt-4o" = "openai",
    "gpt-3.5" = "openai",
    "claude-3-opus-20240229" = "anthropic",
    "claude-3-sonnet-20240229" = "anthropic",
    "claude-3-haiku-20240307" = "anthropic",
    "gemini-1.5-pro" = "google",
    "gemini-1.5-flash" = "google",
    "gemini-1.0-pro" = "google",
    "gemini-pro-vision" = "google")

    providers <- lookup_tabe[[model]]
  ### Figure out which model provider we are going to use
  ###
  ###
  ### Figure out how to attatch meta-data
  ### How to store tmp instead of override. Use arrays?
  ### Possible Error in getting Sys.getenv for api keys not being used.
  for(i in c(1:length(providers))) {
    if(any(grepl(paste0("^", output[1]), colnames(source)))) {
      output2 <- paste0(output, "_", model[i])
    }
    if(providers[i] == "openai") {
      tmp <- gpt(source,
          input,
          output = output2,
          prompt,
          model,
          return_invisible,
          iterations,
          repair,
          progress,
          temperature,
          top_p,
          n,
          presence_penalty,
          frequency_penalty,
          max_tokens,
          openai_api_key,
          openai_organization)
    } else if(i == "anthropic"){
      tmp <- claude(
        source,
        input,
        output = output2,
        prompt,
        model,
        return_invisible,
        iterations,
        repair,
        progress,
        temperature,
        top_p,
        top_k,
        anthropic_version,
        max_tokens,
        anthropic_api_key)
    } else if(i == "google"){
      tmp <- gemini(
        source,
        input,
        output = output2,
        prompt,
        model,
        return_invisible,
        iterations,
        repair,
        progress,
        temperature,
        top_p,
        top_k,
        max_tokens,
        google_api_key)
    } else {
      stop("Model not found. Please check the model name.")
    }
  }




}
