#' llm_generate
#' @param source required; A source dataframe or llm-completion object.
#' @param input required; A column name in the source dataframe
#' @param output optional; A string of a column name (Or a vector of strings) to be created in the source dataframe storing the output of the models. Defaults to `output`.
#' @param prompt required; A string (Or vector of Strings for handling multiple operations at the same time) of prompts to be sent to the AI model.
#' @param model required; a character vector.
#' @param return_invisible optional; A boolean to return just the output (`TRUE`) or an llm-completion object containing model metadata (`FALSE`). Defaults to `FALSE`.
#' @param iterations optional; An integer. Number of completions to generate for each prompt Defaults to `1`.
#' @param repair optional; A boolean to repair NA's in the output column and keep values already present in the output column if the output column has already been created. False overrides the data already in an output column if it exists. Useful to continue a computation if you have been rate limited. Defaults to `FALSE`.
#' @param progress optional; a length one logical vector. Defaults to `TRUE`. Determines whether to show a progress bar in the console. Not available when using repair mode.
#' @param temperature optional; defaults to `1`; a length one numeric vector with the value between `0` (More analytical) and `2` (More creative). `0-2` for OpenAI and Google models, `0-1` for Anthropic models.
#' @param top_p optional; defaults to `1`; a numeric vector with the value between `0` and `1`.
#' @param top_k optional; a numeric vector with the integer value greater than `0`. Only sample from the top_k options for each subsequent token. Not recommended, for most use cases use temperature instead.
#' @param anthropic_version optional; defaults to `2023-06-01`; a character vector. Specifies the version of the Anthropic's models.
#' @param n optional; defaults to `1`; a numeric vector with the integer value greater than `0`.
#' @param presence_penalty optional; defaults to `0`; a numeric vector with a value between `-2` and `2`.
#' @param frequency_penalty optional; defaults to `0`; a numeric vector with a value between `-2` and `2`.
#' @param max_tokens optional; defaults to `(4096 - prompt tokens)`; a numeric vector with the integer value greater than `0`.
#' @param logit_bias optional; defaults to `NULL`; a JSON object that maps tokens (as specified by their toekn ID in the tokenizer) to an associated bias value. -100 to 100.
#' @param logprobs optional; defaults to `FALSE`. If `TRUE`, the API will return log probabilities for each token.
#' @param top_logprobs optional; An integer between `0` and `20`. Specifies the number of most likely tokens to return at each token position. The API will return log probabilities for the top `top_logprobs` tokens at each position. The `logprobs` must be set to `TRUE` to use this parameter.
#' @param seed optional; defaults to `NULL`. An integer that allows for reproducible results when using the same seed. (BETA)
#' @param stop optional; Defaults to `NULL`. A vector of strings (Up to length 4) of sequences where the API will stop generating further tokens.
#' @param user optional; defaults to `NULL`. A string that specifies the user ID to associate with the completion.
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
                top_p = NULL,
                top_k = NULL,
                anthropic_version = "2023-06-01",
                n = 1,
                presence_penalty = 0,
                frequency_penalty = 0,
                max_tokens = 4096,
                openai_api_key = Sys.getenv("OPENAI_API_KEY"),
                openai_organization = NULL,
                anthropic_api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                google_api_key = Sys.getenv("GOOGLE_API_KEY"),
                parentInfo = NULL)
{
  UseMethod("llm_generate")
}
