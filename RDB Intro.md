---
title: "DBAI Intro"
author: "Adam Hubbs"
format: html
editor: visual
execute: 
  warning: false
  error: false
---

## DBAI

DBAI is a package for using Large Language Models with R datasets. You can use it to call AI models straight from R without any other knowledge of API's or Python. This can be useful for sentiment analysis, imputing missing data, creating synthetic data, making predictions and more.

DBAI was created with the following principles in mind: - Transparency - Simplicity

Every call to DBAI includes invisible meta-data information that you can access. With the proliferation of Artificial Intelligence tools the specific parameters being fed to the models are becoming super important.

DBAI was made for social scientists, not AI/CS researchers. It uses an intuitive interface to return text generation from data. It was made to abstract technical details. For advanced AI research, we reccomend you use packages like TensorFlow in Python.

DBAI supports the following model providers: - Open AI - Anthropic - Google

These models can all be accessed with the function `llm_generate`, or you can access functions specific to each model provider.

All of these models can be called using the same function with the same syntax. The main difference is each of these three companies require their own API Key. An API Key is similar to an ID or Credit Card number for these companies. They use it to authenticate that it is really you making this request, and to track your usage to bill you. API Keys are obtained from the model provider's website. For more details about obtaining an API Key and the Costs associated, see *API Keys*.

-   <https://platform.openai.com/playground>

-   <https://www.anthropic.com/api>

-   <https://ai.google.dev/gemini-api>

### Costs

All of these companies provide reasonable access to their models. Their cost changes frequently, so the most accurate place to go to gauge price is the website for these companies directly. They typically provide cost per 1 Million tokens (roughly equivalent to a syllable or word). For most datasets up to 10,000 rows the cost should be in pennies.

## Rate Limits

Rate limits are restrictions on how many API calls you can make in a given period of time. Usually, this is measured in Requests Per Minute. This can be thought of as how many observations/rows you can use the function on in a minute before it stops working. For the companies we work with, rate limits start at a low limit of 3-15 per minute for the free/cheapest options. Rate limits increase the more money you put on file with that provider. Usually between \$50 and \$100 will get you rate limits large enough for most datasets. If you get rate limited in the middle of a function call it will save everything that has been done up to that point and tell you when it stopped. You can then rerun the function with `repair=TRUE` to have it keep going where it left off.

## Loading the Package

DBAI is hosted on github. You can install packages from github by using the `remotes::install_github()` function. This is the equivalent to installing a package from CRAN using `install.packages()`, and only needs to be done once.

```{r}
remotes::install_github("Adam-Hubbs/DBAI")
```

To load the package into memory, use the `library()` function as you would a package from CRAN.

```{r}
library(DBAI)
```

There is one more step we must do before using this package. We need to set the API Key. We will need to set this information in an environmental variable so the R package can recognize it. This needs to be done once per R session. You only need the API Key for the model provider you want to use. For example, if you only care about access to Claude models then you only need the Anthropic API Key. If you only want to run OpenAI's models you only need the OpenAI API Key, etc.

```{r}
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

Let's input some example data and use this function. We'll start with a dataset containing demographic information. We have age, gender, occupation, location, race, and religion information on individuals. For this example, we have taken this information and condensed it into text form in a column called 'demo'. Let's take this information and try to predict who they voted for in the 2020 presidential election.

```{r}
sample_df <- data.frame(
  year = c(1964, 1998, 1979, 1981),
  gender = c("Male", "Female", "Male", "Female"),
  occupation = c("Farmer", "Investment Banker", "Lawyer", "Social Worker"),
  location = c("Kansas", "New York", "Phoenix", "Baltimore"),
  race = c("White", "White", "Hispanic", "Black"),
  religion = c("Evangelical Protestant", "Catholic", "Catholic", "Muslim"),
  demo = c(
    "60 year old white man from Kansas. Is an evangelical protestant and a farmer.",
    "26 year old white female investment banker from New York. Is a Catholic.",
    "45 year old male lawyer from Phoenix. Is a hispanic catholic.",
    "43 year old black female. Works as a social worker in Baltimore and is a practicing muslim."))
    
  
  
prompt <- "I will give you demographic information. I want you to predict who they voted for in the 2020 Presidential election. Make your best guess if you are unsure. Say 'Trump' or 'Biden' only. Do not say anything else."
```

With our dataset and our prompt, lets call our function.

```{r}
return_obj <- llm_generate(source = sample_df, input = "demo", output = "Vote", prompt = prompt, model = c("gpt-3.5-turbo", "gemini-1.5-flash"), max_tokens = 10)

print(return_obj)
```

Here we call out function `gpt()` and tell it the source of our data is "sample_df", column of data we want processed is called "demo". We want it to spit out the results in a column called "Vote", the prompt we are using is "prompt", the model is "gpt-3.5-turbo".

Go ahead and run this and examine the results.

DBAI functions return the text of the completion with meta-data invisibly attached. To view the meta-data attached with a return vector, call `summary()` on the output.

```{r}
result_df <- gpt(source = sample_df, input = "demo", output = "Vote", prompt = prompt, model = "gpt-3.5-turbo")

summary(result_df$Vote)
```

`DBAI` functions are vectorized and can take multiple prompts at the same time and return multiple results. Let's extend our first example.

```{r}
sample_df <- data.frame(
  year = c(1964, 1998, 1979, 1981),
  gender = c("Male", "Female", "Male", "Female"),
  occupation = c("Farmer", "Investment Banker", "Lawyer", "Social Worker"),
  location = c("Kansas", "New York", "Phoenix", "Baltimore"),
  race = c("White", "White", "Hispanic", "Black"),
  religion = c("Evangelical Protestant", "Catholic", "Catholic", "Muslim"),
  demo = c(
    "60 year old white man from Kansas. Is an evangelical protestant and a farmer.",
    "26 year old white female investment banker from New York. Is a Catholic.",
    "45 year old male lawyer from Phoenix. Is a hispanic catholic.",
    "43 year old black female. Works as a social worker in Baltimore and is a practicing muslim."))
    
prompt <- "I will give you demographic information. I want you to predict who they voted for in the 2020 Presidential election. Make your best guess if you are unsure. Say 'Trump' or 'Biden' only. Do not say anything else."

prompt2 <- "I will give you demographic information. I want you to predict what political party they identify with or lean torwards. Make your best guess if you are unsure. Say 'Republican' or 'Democratic' only. Do not say anything else."

prompts <- c(prompt, prompt2)
outputVec <- c("Vote", "Party")
```

Here, we ask it to not only predict the Vote choice, but also predict the Party affiliation of the respondents.

```{r}
sample_df <- gpt(sample_df, input = "demo", output = outputVec, prompt = prompts, model = "gpt-3.5-turbo", return_invisible = TRUE)

print(sample_df)
```

We only made a few minor changes to the function call. This time we pass a vector of outputs and a vector of prompts. When we run this chunk we will now get two columns of data.

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

Lets say the LLM ended up choosing 'Wife'. Lets look at the wrod after wife for another example of the difference between top_k and top_p. Maybe the next word after 'Wife' is a little harder to determine and there were 12 words that combined together reached .5 probability. In this case using the `top_p` method those 12 words would be options for the LLM. Using the `top_k` method still only the top 3 would be considered.

It is not advised to use temperature, top_p, or top_k at the same time. Some models will let you and some won't. Unless you really have to, we recommend only changing temperature.

`Max_tokens` determines how long the maximum response will be. Some models count both the input and output tokens as one, and some only count the output tokens. Setting the max_tokens to a low number like 5 will restrict the response to only a couple words while leaving the max_tokens blank or setting it to a large number like 4,000 will enable the LLM to respond with paragraphs of analysis.

## Another Example

Let's look at another example. One where AI tools can really shine - Textual Analysis!

```{r}
messages <- data.frame(id = c(1, 2, 1), 
                       message = c("Guns are great", 
                                   "i think guns are bad", 
                                   "They protect my family and keep the king of england out of my face"))

good <- "I will give you a statement. I want you to tell me if the overall sentiment is 'Pro-gun' or 'Anti-gun'. Say 'Pro-gun' or 'Anti-gun' only."

messages <- gpt(messages, input = "message", output = "GunStance", prompt = good, model = "gpt-3.5-turbo", max_tokens = 5, return_invisible = TRUE)

View(messages)
```

Here we ask it to categorize statements into either Pro-gun or Anti-gun stances.

## Advanced Features

Let's look at some of the other features we haven't talked about yet.

Sometimes you just want to run the same thing a couple times just to make sure. Perhaps you are asking Claude is there is personally identifiable information in a certain field, or maybe you want to have Gemini deduce a topic from a long comment. For some tasks doing it multiple times is a good indication that your results are internally valid. This is where the `iterations` argument comes in. You can pass iterations any integer value and the function will rerun the call that many times. It will add a \_X at the end of the output column for each iteration \_2 for the second, \_3 for the third, etc. This can stack with having multiple prompts, so thinking back to the Vote and Party example if we also set iterations to equal 2, then it would run both Vote and Party twice.

You may have noticed that there is a handy progress bar that pops up as you run the functions. It shows you helpful information like the number of completed calls out of the total number of calls, and the estimated time remaining. If like to live in suspense however, it is possible to disable this. Just set `progress = FALSE`.

One of the most useful arguments is `repair`. This special repair mode seeks to solve common problems that you will most likely encounter. Perhaps you get rate-limited for trying to run a huge dataset all at once, or maybe OpenAI's servers are busy and they deny your request. For whatever reason you have an error popping up when the function finishes. Do Not Fret! Repair mode is here to save the day! Simply rerun the function with `repair = TRUE`, and the function will pick up right where it left off. Repair mode works regardless of if `return_invisible` was set to True or False (i.e. you can pass it either a dataframe or a llm_completion object) and it will figure it out for you! One caveat thought, the progress bar isn't yet compatible with repair mode, so you won't get to see the pretty bar inch across the screen.

## Documentation

This is a fully built R Package and as such it has documentation available. If you ever want to view the documentation, just call the `?` function. This will give you an overview of the function and detailed information on each argument to the function. When switching model providers be sure to check the defaults and ranges on various model parameters as some differ slightly between model providers.

```{r}
?gpt
?claude
?gemini
?llm_generate
?list_models
```

## 
