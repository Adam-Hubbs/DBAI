### CSED DBAI Workshop
### More inforamtion at https://adam-hubbs.github.io/DBAI/


###############################################
### Installing the Package --------------------
###############################################

# The install.packages() functions only need to be run once. The library() functions need to be run every time you start a new R session.
install.packages("remotes")
library(remotes)
install_github("Adam-Hubbs/DBAI")
library(DBAI)





constitution <- llm_generate("Tell me about the Constitution of the United States of America.")
# This doesn't work! Notice how the error tells us how to fix the problem.





# Let's set the API Key. This is a TEST Key and will NOT WORK AFTER TODAY!!!!! You will need to obtain your own API Key from the website of the model provider you wish to use (OpenAI, Anthropic, or Google).
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
df <- data.frame(
  year = c(60, 26, 45, 43),
  demographics = demographics_vector
)




prompt <- "I will give you demographic information. I want you to predict who they voted for in the 2020 Presidential election. Make your best guess if you are unsure. Say 'Trump' or 'Biden' only. Do not say anything else."





prompt2 <- "I will give you demographic information. I want you to predict what political party they identify with or lean torwards. Make your best guess if you are unsure. Say 'Republican' or 'Democratic' only. Do not say anything else."





prompts <- c(prompt, prompt2)




outputVec <- c("Vote", "Party")




df <- llm_generate(df, input = "demographics", prompt = prompts, max_tokens = 10, output = outputVec)





print(df$Party)




summary(df$Party)



#########################################
### Another Data frame example ----------
#########################################

# Let's try a bit more complicated example. Here we will conditionally use one prompt or another based on the data in the data frame.

#install.packages("tidyverse")
library(tidyverse)




prompt1 <- "Respond with another sentance continuing the story."



prompt2 <- "Respond with STORY ENDS HERE. Do not respond with anything else. This is very important."




story_df <- tibble(continue_story = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE), so_far = c(
  "Once upon a time, there was a little princess.",
  "On the edge of the Andromeda galaxy, Captain Zara Ryker received a distress signal that defied all known physics.",
  "With the dragon defeated and the kingdom restored, Princess Aria took her place on the throne, ready to lead her people into an era of peace.",
  "Detective Harris had seen his share of strange cases, but the sight of the abandoned mansion's library filled with fresh rose petals left him puzzled.",
  "As the last notes of the orchestra faded away, Eleanor knew that the echoes of their love would linger in the halls of the old theater forever.",
  "Hand in hand, they walked along the beach, knowing that no matter where life took them, they would always have each other.",
  "In the summer of 1924, as the jazz bands played and the Charleston swept through New York, young Annabelle Monroe was about to uncover a family secret.",
  "Under the glow of the Parisian moonlight, Emma stumbled upon a quaint bookshop, never expecting to meet the man who would capture her heart.",
  "As the first rays of dawn broke over the Sahara Desert, Alex tightened his backpack straps, ready to embark on the treasure hunt of a lifetime.",
  "Beneath the towering spires of the crystal city, young mage Lira was chosen to defend the realm against an ancient darkness.",
  "The first day of senior year was supposed to be ordinary, but when Jordan found a mysterious key in his locker, everything changed.",
  "In the aftermath of the revolution, as the city began to rebuild, hope flickered in the hearts of the people, promising a brighter future.",
  "In a world where emotions were outlawed, Mia's accidental smile set off a chain of events that threatened to topple the regime."))




View(story_df)




story_df <- story_df |>
  mutate(next_sentance = case_when(
    continue_story == TRUE ~ llm_generate(so_far, prompt = prompt1),
    continue_story == FALSE ~ llm_generate(so_far, prompt = prompt2),
    TRUE ~ NA))




View(story_df)




summary(story_df$next_sentance)
# DBAI automatically preserves all meta data and puts it together for you. Here we called the function two different times, but that was collapsed into one column. DBAI takes the meta-data from both times into one place.
