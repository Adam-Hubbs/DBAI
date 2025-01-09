### SPSA DBAI Workshop
### More inforamtion at https://adam-hubbs.github.io/DBAI/


###############################################
### Installing the Package --------------------
###############################################

# The install.packages() functions only need to be run once. The library() functions need to be run every time you start a new R session.
install.packages("devtools")
library(devtools)
install_github("Adam-Hubbs/DBAI")
library(DBAI)





constitution <- llm_generate("Tell me about the Constitution of the United States of America.")
# This doesn't work! Notice how the error tells us how to fix the problem.





# Let's set the API Key. This is a TEST Key and will NOT WORK AFTER TODAY!!!!! You will need to obtain your own API Key from the website of the model provider you wish to use (OpenAI, Anthropic, or Google).
# API Key Broken
Sys.setenv(
  OPENAI_API_KEY = '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'
)






#######################################
### Simple Request --------------------
#######################################

constitution <- llm_generate("Tell me about the Constitution of the United States of America.")




print(constitution)
# Responses come to you in a character format.






######################################
### Using Vectors --------------------
######################################

demographics_vector <- c("60 year old white man from Kansas. Is an evangelical protestant and a farmer.",
                         "26 year old white female investment banker from New York. Is a Catholic.",
                         "45 year old black male lawyer from Phoenix. Is an atheist.",
                         "43 year old black female. Works as a social worker in Baltimore and is a practicing muslim.")




prompt <- "I will give you demographic information. I want you to predict who they voted for in the 2020 Presidential election. Make your best guess if you are unsure. Say 'Trump' or 'Biden' only. Do not say anything else."




# We can use Vectors as well to iterate over many different demographics and get the model to predict who they voted for in the 2020 Presidential election.
vote_choice <- llm_generate(demographics_vector, prompt = prompt, max_tokens = 5)




print(vote_choice)





# Modify the model parameters
# These will not work, but will error and tell you how to fix the problem.

vote_choice2 <- llm_generate(demographics_vector, prompt = 4, max_tokens = 5)




vote_choice2 <- llm_generate(demographics_vector, prompt = prompt, max_tokens = 5, temperature = 800)




vote_choice2 <- llm_generate(demographics_vector, prompt = prompt, max_tokens = 5, temperature = 2)





print(vote_choice2)




# While it looks like DBAI only returns the responses, it actually records all sorts of metadata and stores it invisibly. It will never get in the way, but you can always access it easily with summary().
summary(vote_choice2)





##########################################
### Using Data Frames --------------------
##########################################

# We can also use data frames (And tibbles). With data frames, you can run more than one prompt at the same time.
survey_data <- data.frame(id = c(1, 2, 3),
                          message = c("Guns are great",
                                      "i think guns are bad",
                                      "They protect my family and keep the king of england out of my face"))






gun_prompt <- "I will give you a statement. I want you to tell me if the overall sentiment is 'Pro-gun' or 'Anti-gun'. Say 'Pro-gun' or 'Anti-gun' only."






survey_data <- llm_generate(survey_data, input = "message", output = "GunStance", prompt = gun_prompt, model = "gpt-3.5-turbo", max_tokens = 5)






View(survey_data)








# Let's try another example, this time with returning more than one column.
df <- data.frame(
  year = c(60, 26, 45, 43),
  demographics = demographics_vector
)




View(df)




prompt <- "I will give you demographic information. I want you to predict who they voted for in the 2020 Presidential election. Make your best guess if you are unsure. Say 'Trump' or 'Biden' only. Do not say anything else."





prompt2 <- "I will give you demographic information. I want you to predict what political party they identify with or lean torwards. Make your best guess if you are unsure. Say 'Republican' or 'Democratic' only. Do not say anything else."





prompts <- c(prompt, prompt2)




outputVec <- c("Vote", "Party")




df <- llm_generate(df, input = "demographics", prompt = prompts, max_tokens = 10, output = outputVec)




View(df)




print(df$Party)




summary(df$Party)



#########################################
### Another Data frame example ----------
#########################################

# Let's try a bit more complicated example. Here we will conditionally use one prompt or another based on the data in the data frame.
# Imagine we asked a question about whether education spending is necessary or not. We code the respondent as `For` if they agree that more education spending is necessary and `Against` if they disagree. Here we will use an LLM to attempt to use motivational interviewing to persuade the respondent in a way that contradicts their views.

#install.packages("tidyverse")
library(tidyverse)


prompt1 <- "I will give you some demographic information about a person. I want you to try to presuade them that more education spending is nesessary using motivational interviewing techniques."



prompt2 <- "I will give you some demographic information about a person. I want you to try to presuade them that more education spending is NOT nesessary using motivational interviewing techniques."




education_df <- tibble(stance = c("For", "Against", "Against", "For"), demographics = c(
  "28 year old white man from Seattle who works as an architect.",
  "67 year old black woman from Atlanta who is retired.",
  "43 year old white woman from New York who works as a banker.",
  "36 year old hispanic man from San Diego who works as a software engineer."
))




View(education_df)




education_df <- education_df |>
  mutate(persuasion = case_when(
    stance == "For" ~ llm_generate(demographics, prompt = prompt1),
    stance == "Against" ~ llm_generate(demographics, prompt = prompt2),
    TRUE ~ NA))




View(education_df)




summary(education_df$persuasion)
# DBAI automatically preserves all meta data and puts it together for you. Here we called the function two different times, but that was collapsed into one column. DBAI takes the meta-data from both times into one place.
