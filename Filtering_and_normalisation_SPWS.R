####################
#Outlier removal and Lobanov-adapted normalisation for SPWS
####################

#this script will:
#filter out tokens that are +/- 3 SDs from a speaker's mean F1/F2 productions (calculated per speaker, per vowel)
#normalise the speaker's F1/F2 values using an adpated version of Lobanov's method

#you will need a data set that follows Simón's initial specification (i.e it must contain variables for speaker, vowel, midpoint F1, midpoint F2, grammatical word category, other social information)
#you will also need the stress variable for the vowel (coded as 'stressed' or 'unstressed')

#-------------
#MAKE SURE YOUR DATA HAS NOT ALREADY BEEN FILTERED USING SIMÓN'S SCRIPT
#-------------

#i.e. ensure that no filtering has already been done using the wrangledataSPWS.r script
#only use that script to get the grammatical category classification, not the filtering or normalisation

###################
#load in the data and packages
###################

library(data.table) # https://github.com/Rdatatable/data.table/wiki/Installation
library(sylcount) # https://github.com/wrathematics/sylcount/tree/master/R
library(tidyverse) # https://www.tidyverse.org/packages/

df <- read.csv("data_file.csv", header = TRUE, stringsAsFactors = F)

###################
#Simón wrangling initial procedure
#i.e. get grammatical category and syllable count variables
###################

#adds a column to specify which words are in the data
#this is used later when merging with grammatical words
df$wordInData <- 'yes'

### Imports the Grammatical List file
grammaticalList <- read.csv('gram_list.csv', stringsAsFactors = F)

#renames columns in list
names(grammaticalList) <- c('word', 'grammatical')

### Mergers the data with the grammtical word list
#add grammatical word information
df <- merge(df, grammaticalList, by = "word", all=TRUE)

#deletes entries from grammaticalList that do not appear in the data
df <- df[complete.cases(df$wordInData),]
#assigns no grammatical words
df$grammatical[is.na(df$grammatical)] <- 'no'

#deletes entries with empty word labels
df <- df[!(df$word %in% c('', ' ')),]

### Adding syllable count
sylcnt <- sapply(sylcount(as.character(df$word)), `[[`, 1)

#adds Syllable count information
df$sylcnt <- sylcnt

###################
#sd filtering
###################

#we can create a 'stressgram' variable to define which vowels are stressed/unstressed and grammatical/non-grammatical
df$vowelstressgram = paste(df$vowel, df$stress, df$grammatical)

#if you decide to remove outliers based on the 'vowelstressgram' variable this will remove tokens that are +/-3 SDs from a speaker's mean for vowels that are stressed/unstressed and grammatical/ungrammatical
#e.g. for KIT the SD of tokens will be calculated on the basis of whether the token is:
#KIT and stressed and grammatical
#KIT and stressed and ungrammatical
#KIT and unstressed and grammatical
#KIT and unstressed and ungrammatical

#------------
#NOTE
#If you want to just calculate the SD based on per speaker per vowel, change where it says 'vowelstressgram' to just your 'vowel' variable name
#e.g. in the main filtering code below it should be:
# for (vwl in levels(factor(df$vowel))) {
#------------

#set the SD threshold for outliers to 3
sdThreshold=3

#create a variable to store the outlier categories
df$Outlier3 <- FALSE

#this is the outlier analysis
#it calculates whether a token is >/< 3 SD from the mean
#
#TRUE if it is an outlier, FALSE if it is not

#3 SD threshold was chosen on the basis of some more extreme values may be quantitatively interesting

#the analysis calculates SD per vowel per speaker, so it loops through each vowel for each speaker and determines the mean and SD of those rows

for (vwl in levels(factor(df$vowel))) {
  for (spkr in levels(factor(df$speaker))) {
    rows <- which(df$vowel==vwl & df$speaker==spkr)
    if (length(rows) > 1) {
      subDF <- df[rows,]
      means <- apply(subDF[,c("F1","F2")], 2, mean, na.rm=TRUE)
      sds <- apply(subDF[,c("F1","F2")], 2, sd, na.rm=TRUE)
      df$Outlier3[rows] <- subDF$F1 < means[1]-sdThreshold*sds[1] | subDF$F1 > means[1]+sdThreshold*sds[1] |
        subDF$F2 < means[2]-sdThreshold*sds[2] | subDF$F2 > means[2]+sdThreshold*sds[2]
    }
  }
  print(vwl)
}

#keep cases that are not outliers
outliersremoved <- df[df$Outlier3 == FALSE, ]

################
#lobanov-style normalisation
#based on means and SDs from stressed ungrammatical tokens only
################

#this function requires a df containing columns for:
#speaker
#vowel
#F1
#F2
#stress (this should be categorical either "stressed" or "unstressed")
#grammatical (this should be categorial either "yes" or "no")

lobanov.SPWS <- function(outliersremoved, speaker, vowel, F1, F2, stress, grammatical){
  
  #first filter the data so there is only stressed and non-grammatical tokens
  stressed_df <- df %>%
    filter(stress == "stressed",
           grammatical == "no")
  
  #get summary means and sd
  summary_vowels_all <- stressed_df %>%
    group_by(speaker, vowel) %>%
    summarise(mean_F1 = mean(F1),
              mean_F2 = mean(F2),
              sd_F1 = sd(F1),
              sd_F2 = sd(F2),
              token_count_vowel = n())
  
  #get the mean_of_means and sd_of_means from the the speaker_summaries
  #this will give each speaker a mean caculated from the means across all vowels
  #as well as the standard deviation of the means
  summary_mean_of_means <- summary_vowels_all %>%
    group_by(speaker) %>%
    summarise(mean_of_means_F1 = mean(mean_F1),
              mean_of_means_F2 = mean(mean_F2),
              sd_of_means_F1 = sd(mean_F1),
              sd_of_means_F2 = sd(mean_F2)
    )
  
  #combine these values with the full raw dataset
  #then use these values to normalise the data in a new Lobanov style
  df <- df %>%
    inner_join(., summary_mean_of_means) %>%
    mutate(F1_lobanov_SPWS = (F1 - mean_of_means_F1)/sd_of_means_F1,
           F2_lobanov_SPWS = (F2 - mean_of_means_F2)/sd_of_means_F2,
           mean_of_means_F1 = NULL,
           mean_of_means_F2 = NULL,
           sd_of_means_F1 = NULL,
           sd_of_means_F2 = NULL)
  
  return(df)
}

#now run the function on your dataset, this will add 2 new variables (F1_lobanov_SPWS and F2_lobanov_SPWS)
#if you have different variable names to those inside the brackets, make you sure you change them
#e.g. if your stress variable is called 'stresscode', change it to that instead of 'stress'
normalised_df <- lobanov.SPWS(outliersremoved, speaker, vowel, F1, F2, stress, grammatical)

write.csv(normalised_df, 'normalised_df.csv', row.names = F)
