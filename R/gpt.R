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
#' @param max_completion_tokens optional; defaults to `NULL`; used in OpenAI reasoning models; a numeric vector with the integer value greater than `0`. Specifies the maximum number of tokens to generate for each completion. This includes tokens generated as part of the reasoning process and are not returned to the user.
#' @param is_reasoning_model optional; defaults to `NULL`. A vector of booleans that specifies if the model is a reasoning model. If `TRUE`, the model will be treated as a reasoning model. Reasoning models use different model parameters and are optimized for reasoning tasks. OpenAI only.
#' @param reasoning_effort optional; defauls to `medium`. A vector of strings that specifies the effort level for the reasoning model. OpenAI only. Must be one of c(`low`, `medium`, `high`).
#' @param openai_api_key required; defaults to `Sys.getenv("OPENAI_API_KEY")` (i.e., the value is retrieved from the `.Renviron` file); a length one character vector. Specifies OpenAI API key. Must obtain API Key from OpenAI.
#' @param openai_organization optional; defaults to `NULL`; a length one character vector. Specifies OpenAI organization.
#' @param parentInfo Used internally. Do not supply.
#' @return A dataframe with the output column(s) created
#' @export
gpt <- function(source,
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
                max_completion_tokens = NULL,
                is_reasoning_model = NULL,
                reasoning_effort = NULL,
                openai_api_key = Sys.getenv("OPENAI_API_KEY"),
                openai_organization = NULL,
                call = rlang::caller_env(),
                parentInfo = NULL) {
  UseMethod("gpt")
}
