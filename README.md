---
title: "DBAI Intro"
author: "Adam Hubbs"
format: html
editor: visual
execute: 
  warning: false
  error: false
---

# DBAI

DBAI is a package for using Large Language Models with R datasets. You can use it to call AI models straight from R without any other knowledge of API's or Python. This can be useful for sentiment analysis, imputing missing data, creating synthetic data, making predictions and more.

DBAI was created with these two key principles in mind: *Transparency* and *Simplicity*

Every call to DBAI includes invisible meta-data information that you can access.

DBAI was made for social scientists, not AI/CS researchers. It uses an intuitive interface to return text generation from data. It was made to abstract technical details. For advanced AI research, we recommend you use packages like TensorFlow in Python.

DBAI supports the following model providers: - Open AI - Anthropic - Google

These models can all be accessed with the function `llm_generate`, or you can access functions specific to each model provider.

All of these models can be called using the same function with the same syntax. The main difference is each of these three companies require their own API Key. An API Key is similar to an ID or Credit Card number for these companies. They use it to authenticate that it is really you making this request, and to track your usage to bill you. API Keys are obtained from the model provider's website. For more details about obtaining an API Key and the Costs associated, see *API Keys*.

-   <https://platform.openai.com/playground>

-   <https://www.anthropic.com/api>

-   <https://ai.google.dev/gemini-api>

## Costs

All of these companies provide reasonable access to their models. Their cost changes frequently, so the most accurate place to go to gauge price is the website for these companies directly. They typically provide cost per 1 Million tokens (roughly equivalent to a syllable or word). For most datasets up to 10,000 rows the cost should be in pennies.

## Rate Limits

Rate limits are restrictions on how many API calls you can make in a given period of time. Usually, this is measured in Requests Per Minute. This can be thought of as how many observations/rows you can use the function on in a minute before it stops working. For the companies we work with, rate limits start at a low limit of 3-15 per minute for the free/cheapest options. Rate limits increase the more money you put on file with that provider. Usually between \$50 and \$100 will get you rate limits large enough for most datasets. If you get rate limited in the middle of a function call it will save everything that has been done up to that point and tell you when it stopped. You can then rerun the function with `repair=TRUE` to have it keep going where it left off.

## Loading the Package

DBAI is hosted on github. You can install packages from github by using the `pak::pak()` function. This is the equivalent to installing a package from CRAN using `install.packages()`, and only needs to be done once.

``` r
devtools::install_github("Adam-Hubbs/DBAI")
```

To load the package into memory, use the `library()` function as you would a package from CRAN.

``` r
library(DBAI)
```

There is one more step we must do before using this package. We need to set the API Key. We will need to set this information in an environmental variable so the R package can recognize it. This needs to be done once per R session. You only need the API Key for the model provider you want to use. For example, if you only care about access to Claude models then you only need the Anthropic API Key. If you only want to run OpenAI's models you only need the OpenAI API Key, etc.

``` r
Sys.setenv(
  OPENAI_API_KEY = 'XXXXXXXXXXX'
)

Sys.setenv(
  ANTHROPIC_API_KEY = 'XXXXXXXXXXXX'
)

Sys.setenv(
  GOOGLE_API_KEY = 'XXXXXXXXXXXX'
)
```

## Example Data

Let's start with a simple example. Generating a basic completion.

``` r
constitution <- llm_generate("Tell me about the Constitution of the United States of America.")
```

If we call `print()` on constitution, we can see the result. If we want all the meta-data, call `summary()`.

DBAI works with a variety of data types. You can pass it character vectors, or dataframe objects. Lets do a character vector example.

``` r
demographics_vector <- c("60 year old white man from Kansas. Is an evangelical protestant and a farmer.",
                         "26 year old white female investment banker from New York. Is a Catholic.",
                         "45 year old black male lawyer from Phoenix. Is an atheist.",
                         "43 year old black female. Works as a social worker in Baltimore and is a practicing muslim.")




prompt <- "I will give you demographic information. I want you to predict who they voted for in the 2020 Presidential election. Make your best guess if you are unsure. Say 'Trump' or 'Biden' only. Do not say anything else."



vote_choice <- llm_generate(demographics_vector, prompt = prompt, max_tokens = 5)
```

If we pass it a character vector then it will return a character vector (With all the meta-data invisibly attached).

We can also use data frames!

``` r
survey_data <- tibble(id = c(1, 2, 3),
                      message = c("Guns are great",
                                  "i think guns are bad",
                                  "They protect my family and keep the king of england out of my face"))


gun_prompt <- "I will give you a statement. I want you to tell me if the overall sentiment is 'Pro-gun' or 'Anti-gun'. Say 'Pro-gun' or 'Anti-gun' only."


survey_data1 <- llm_generate(survey_data, input = "message", output = "GunStance", prompt = gun_prompt, model = "gpt-3.5-turbo", max_tokens = 5)
```

The first argument is the dataframe. Next we have the input column and the output column, and then any model parameters.

`DBAI` functions are vectorized and can take multiple prompts or model parameters at the same time and return multiple results. Let's extend our first example. Perhaps we want to run this with both gpt-3.5 and gpt-4 to compare the results. That's as easy as passing a c() with the models you want in the model parameter.

``` r

# You can easily compare with different models or model parameters.
# Lets look at two different models
survey_data2 <- llm_generate(survey_data, input = "message", output = "GunStance", prompt = gun_prompt, model = c("gpt-3.5-turbo", "gpt-4"), max_tokens = 5)




# Lets change the temperature
survey_data3 <- llm_generate(survey_data, input = "message", output = c("GunStanceMidTemp", "GunStanceLowTemp"), prompt = gun_prompt, model = "gpt-3.5-turbo", max_tokens = 5, temperature = c(1, 0.1))

```

Now lets look at using two prompts at the same time. Here, we ask it to not only predict the Vote choice, but also predict the Party affiliation of the respondents.

``` r

df <- data.frame(
  year = c(60, 26, 45, 43),
  demographics = demographics_vector
)

prompt <- "I will give you demographic information. I want you to predict who they voted for in the 2020 Presidential election. Make your best guess if you are unsure. Say 'Trump' or 'Biden' only. Do not say anything else."





prompt2 <- "I will give you demographic information. I want you to predict what political party they identify with or lean torwards. Make your best guess if you are unsure. Say 'Republican' or 'Democratic' only. Do not say anything else."





prompts <- c(prompt, prompt2)




outputVec <- c("Vote", "Party")




df <- llm_generate(df, input = "demographics", prompt = prompts, max_tokens = 10, output = outputVec)
```

We only made a few minor changes to the function call. This time we pass a vector of outputs and a vector of prompts. When we run this chunk we will now get two columns of data.

## Conditional Prompts

Let's try a bit more complicated example. Here we will conditionally use one prompt or another based on the data in the data frame.

Imagine we asked a question about whether education spending is necessary or not. We code the respondent as `For` if they agree that more education spending is necessary and `Against` if they disagree. Here we will use an LLM to attempt to use motivational interviewing to persuade the respondent in a way that contradicts their views.

``` r
library(tidyverse)


prompt1 <- "I will give you some demographic information about a person. I want you to try to presuade them that more education spending is nesessary using motivational interviewing techniques."



prompt2 <- "I will give you some demographic information about a person. I want you to try to presuade them that more education spending is NOT nesessary using motivational interviewing techniques."




education_df <- tibble(stance = c("For", "Against", "Against", "For"), demographics = c(
  "28 year old white man from Seattle who works as an architect.",
  "67 year old black woman from Atlanta who is retired.",
  "43 year old white woman from New York who works as a banker.",
  "36 year old hispanic man from San Diego who works as a software engineer."
))




education_df <- education_df |>
  mutate(persuasion = case_when(
    stance == "Against" ~ llm_generate(demographics, prompt = prompt1),
    stance == "For" ~ llm_generate(demographics, prompt = prompt2),
    TRUE ~ NA))
```

DBAI functions work within mutate and control flow statements. That means you can conditionally use prompts on certain observations.

## Model Arguments

Lets take a look at some of the other argument options. We will discuss four common parameters for Large Language Models, `temperature`, `top_p`, `top_k`, and `max_tokens`. The first three of these parameters are ways to change how deterministic or random the response will be., and the last deals with how long the response is.

`Temperature` specifies how repetitive and analytical , or creative and random the responses are. For our models, temperature runs between 0 (Analytical) and 2 (Creative). Setting the temperature too low can lead to the exact same results for many calls, regardless of data. It can also be hyper-sensitive to examples in the prompt. Setting the temperature too high can lead the LLM to hallucinate and make up things that aren't real. By default, the temperature is 1, a happy medium. You may want to play around with temperature to suit your specific needs.

`Top_p` and `top_k` both have to do with sampling methods that the LLM uses. In order to understand this, let's take a brief refresher on how Large Language Models work in the first place.

LLM's work by predicting the next token (roughly analogous to a syllable or word) in a sequence. They are trained on up to virtually all written history. Let's take the sentence: `The man sat next to his`. A LLM would then try to predict the next word. There are many possibilities for what could come next, and the LLM knows this, so it produces probabilities for each word that could come next. Let's say this is what it predicts, along with it's respective probability. *Note, this is purely hypothetical.*

| Word      | Probability |
|-----------|-------------|
| Wife      | .35         |
| Dog       | .2          |
| Son       | .12         |
| Daughter  | .1          |
| Friend    | .08         |
| Lawyer    | .06         |
| Iguana    | .05         |
| Sculpture | .04         |

Here we see that the most likely next word is 'Wife' but there are many other options. `Top_k` restricts the sample to the top K number of options. For example, if `top_k` were set to 3 in this example, the LLM would choose between 'Wife', 'Dog', and 'Son'. `Top_p` similarly restricts the sample but instead of giving a fixed number of options, it gives a fixed probability. If out `top_p` were set to .5, then 'Wife' and 'Dog' would be considered because they represent the least number of options needed to reach at least a .5 probability.

Lets say the LLM ended up choosing 'Wife'. Lets look at the word after wife for another example of the difference between top_k and top_p. Maybe the next word after 'Wife' is a little harder to determine and there were 12 words that combined together reached .5 probability. In this case using the `top_p` method those 12 words would be options for the LLM. Using the `top_k` method still only the top 3 would be considered.

It is not advised to use temperature, top_p, or top_k at the same time. Some models will let you and some won't. Unless you really have to, we recommend only changing temperature.

`Max_tokens` determines how long the maximum response will be. Some models count both the input and output tokens as one, and some only count the output tokens. Setting the max_tokens to a low number like 5 will restrict the response to only a couple words while leaving the max_tokens blank or setting it to a large number like 4,000 will enable the LLM to respond with paragraphs of analysis.

## Advanced Features

Let's look at some of the other features we haven't talked about yet.

Sometimes you just want to run the same thing a couple times just to make sure. Perhaps you are asking Claude is there is personally identifiable information in a certain field, or maybe you want to have Gemini deduce a topic from a long comment. For some tasks doing it multiple times is a good indication that your results are internally valid. This is where the `iterations` argument comes in. You can pass iterations any integer value and the function will rerun the call that many times. It will add a \_X at the end of the output column for each iteration \_2 for the second, \_3 for the third, etc. This can stack with having multiple prompts, so thinking back to the Vote and Party example if we also set iterations to equal 2, then it would run both Vote and Party twice.

You may have noticed that there is a handy progress bar that pops up as you run the functions. It shows you helpful information like the number of completed calls out of the total number of calls, and the estimated time remaining. If like to live in suspense however, it is possible to disable this. Just set `progress = FALSE`.

One of the most useful arguments is `repair`. This special repair mode seeks to solve common problems that you will most likely encounter. Perhaps you get rate-limited for trying to run a huge dataset all at once, or maybe OpenAI's servers are busy and they deny your request. For whatever reason you have an error popping up when the function finishes. Do Not Fret! Repair mode is here to save the day! Simply rerun the function with `repair = TRUE`, and the function will pick up right where it left off! One caveat thought, the progress bar isn't yet compatible with repair mode, so you won't get to see the pretty bar inch across the screen.

## Documentation

This is a fully built R Package and as such it has documentation available. If you ever want to view the documentation, just call the `?` function. This will give you an overview of the function and detailed information on each argument to the function. When switching model providers be sure to check the defaults and ranges on various model parameters as some differ slightly between model providers.

``` r
?gpt
?claude
?gemini
?llm_generate
?list_models
```

## 
