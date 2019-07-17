#install the labb-cat R package
install.packages("nzilbb.labbcat")

#make sure it is loaded
require("nzilbb.labbcat")
require("tidyverse")

#set the url for use later
labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"

#generate a list of unique wordforms from your data
#(make sure df = your data frame and word = your orthographic wordform)
wordforms <- df %>%
  select(word) %>%
  distinct()

#####LOG IN DETAILS#####
#username: demo
#password: demo
########################

#query the R labb-cat package to get the stressed phonology from CELEX
stress <- getDictionaryEntries(labbcat.url, "CELEX-EN", "Phonology (wordform)", wordforms) %>%
  filter(key %in% wordforms$word) %>%
  mutate(stressed_phonology = ifelse(V1 != "", as.character(V1), NA)) %>%
  arrange(key)

#combine the stress coded values to your data
df_test3 <- df %>%
  mutate(number_test = 1:n()) %>%
  inner_join(.,
             stress[, c("key", "stressed_phonology")],
             by = c("word" = "key")) %>%
  group_by(number_test) %>%
  filter(n()<2) %>%
  ungroup() %>%
  mutate(number_test = NULL,
         vowelDISC = fct_recode(factor(vowel),
                                  `i` = "FLEECE",
                                  `I` = "KIT",
                                  `E` = "DRESS",
                                  `\\{` = "TRAP",
                                  `\\@` = "SCHWA",
                                  `#` = "START",
                                  `Q` = "LOT",
                                  `\\$` = "THOUGHT",
                                  `3` = "NURSE",
                                  `V` = "STRUT",
                                  `U` = "FOOT",
                                  `u` = "GOOSE"))

