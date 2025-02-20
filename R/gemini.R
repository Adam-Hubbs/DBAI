#' gemini
#' @param source required; A source dataframe or llm-completion object.
#' @param input required; A column name in the source dataframe
#' @param output optional; A string of a column name (Or a vector of strings) to be created in the source dataframe storing the output of the models. Defaults to `output`.
#' @param prompt required; A string (Or vector of Strings for handling multiple operations at the same time) of prompts to be sent to the AI model.
#' @param model required; a length one character vector.
#' @param return_invisible optional; A boolean to return just the output (`TRUE`) or an llm-completion object containing model metadata (`FALSE`). Defaults to `FALSE`.
#' @param iterations optional; An integer. Number of completions to generate for each prompt. Defaults to `1`.
#' @param repair optional; A boolean to repair NA's in the output column and keep values already present in the output column if the output column has already been created. False overrides the data already in an output column if it exists. Useful to continue a computation if you have been rate limited. Defaults to `FALSE`.
#' @param progress optional; a length one logical vector. Defaults to `TRUE`. Determines whether to show a progress bar in the console. Not available when using repair mode.
#' @param temperature optional; defaults to `1`; a length one numeric vector with the value between `0` (More analytical) and `2` (More creative).
#' @param top_p optional; defaults to `0.95`; a length one numeric vector with the value between `0` and `1`.
#' @param top_k optional; a length one numeric vector with the integer value greater than `0`. Only sample from the top_k options for each subsequent token. Not recommended, for most use cases use temperature instead. If no value is provided, it does not use nucleus sampling.
#' @param max_tokens optional; a length one numeric vector with the integer value greater than `0`. For Gemini this only includes output tokens. Defaults to `4096`.
#' @param google_api_key required; defaults to `Sys.getenv("GOOGLE_API_KEY")` (i.e., the value is retrieved from the `.Renviron` file); a length one character vector. Specifies Google API key. Must obtain API Key from Google.
#' @return A dataframe with the output column(s) created
#' @export
gemini <- function(source,
                   input,
                   output = "output",
                   prompt,
                   model = "gemini-1.5-flash",
                   return_invisible = FALSE,
                   iterations = 1,
                   repair = FALSE,
                   progress = TRUE,
                   temperature = 1,
                   top_p = NULL,
                   top_k = NULL,
                   max_tokens = 4096,
                   google_api_key = Sys.getenv("GOOGLE_API_KEY"),
                   parentInfo = NULL) {
  UseMethod("gemini")
}
