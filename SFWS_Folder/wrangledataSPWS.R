#"Sociophonetics workshop, ANU, 16-19 July 2019"
#Main script

### R libraries used
library(data.table) # https://github.com/Rdatatable/data.table/wiki/Installation
library(sylcount) # https://github.com/wrathematics/sylcount/tree/master/R
library(tidyverse) # https://www.tidyverse.org/packages/
library(vowels) # command within R: install.packages("vowels")

### Sourcing functions
#...............................................................................
#...............................................................................
#...............................................................................
source('./functions/sd_calculation_function.R')

### Useful functions
#To change the label of a specific column
#colnames(dataframe)[which(names(dataframe) == "columnName")] <- "newColumnName"
#example
#colnames(df)[which(names(df) == "target.orthography")] <- "word"

### Imports data
#Required headers which are used in the script
#Should you need to change column headers, use the above function
#word = vowel carrying words
#F1 = Formant values for F1
#F2 = Formant values for F2
#vowel = vowel labels
#speaker = speaker labels

#...............................................................................
#...............................................................................
#...............................................................................
df <- read.csv('file/path', stringsAsFactors = F)

#adds a column to specify which words are in the data
#this is used later when merging with grammatical words
df$wordInData <- 'yes'

### Imports the Grammatical List file
#...............................................................................
#...............................................................................
#...............................................................................
grammaticalList <- read.csv('gram_list.csv', stringsAsFactors = F)
#renames columns in list
names(grammaticalList) <- c('word', 'grammatical')

### Mergers the data with the grammtical word list
#add grammatical word information
df <- merge(df, grammaticalList, by = 'word', all=TRUE)
#deletes entries from grammaticalList that do not appear in the data
df <- df[complete.cases(df$wordInData),]
#assigns no grammatical words
df$grammatical[is.na(df$grammatical)] <- 'no'
#deletes entries with empty word labels
df <- df[!(df$word %in% c('', ' ')),]

### Adding syllable count
#...............................................................................
#...............................................................................
#...............................................................................
#Adds syllable count
sylcnt <- sapply(sylcount(as.character(df$word)), `[[`, 1)

#adds Syllable count information
df$sylcnt <- sylcnt

### Standard Deviation Filtering
#...............................................................................
#...............................................................................
#...............................................................................

#Creates a new dataframe with SD filtering
#
#USe this line of code ONLY if you have not 
#
#IMPORTANT = here, we set sd_value to 3 sd
#this line generally takes a while, depending on the size of the data
df <- sd_classify(data = df, sd_value = 3, f1_label = 'F1', f2_label = 'F2', vowel_column = 'vowel', sex_column = 'speaker', plot_vowels = T, compare = T)

### Normalisation
#...............................................................................
#...............................................................................
#...............................................................................
#creates a context for each token
df$context <- 1:nrow(df)

#creates a data with the required columns to be input into the normalisation functions
dnorm <- df[,c('speaker', 'vowel', 'context', 'F1', 'F2', 'F1', 'F1', 'F2', 'F1'),]
names(dnorm) <- c('speaker_id', 'vowel_id', 'context', 'F1', 'F2', 'F3', 'F1_glide', 'F2_glide', 'F3_glide')
#Deletes the unnecessary columns
dnorm$F3 <- NA
dnorm$F1_glide <- NA
dnorm$F2_glide <- NA
dnorm$F3_glide <- NA

#Delete NA values
dnorm <- dnorm[complete.cases(dnorm$F1, dnorm$F2),]

#Normalises formants using the Lobanov method
normed.vowels <- norm.lobanov(dnorm)
#Rescales the values to Hz
rescaled <- scalevowels(normed.vowels)

#Renames columns to be compatible with the input data
names(rescaled) <- c('speaker', 'segment',       'context',    'F1_norm'    ,'F2_norm', 'F1b', 'F2b')
#Substes to the relevant columns
rescaled <- rescaled[,c('speaker', 'segment',       'context',    'F1_norm'    ,'F2_norm')]

#Merges the normalised values to the main data frame
#...............................................................................
#...............................................................................
#...............................................................................
df <- merge(df, rescaled[,c('context',    'F1_norm'    ,'F2_norm')], by = 'context')

#saves the wrangled data
#...............................................................................
#...............................................................................
#...............................................................................
write.csv(df, 'data_Wrangled.csv', row.names = F, quote = F)

