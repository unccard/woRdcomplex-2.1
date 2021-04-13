# woRdcomplex version 1.2 (12 April 2021)--an R software script for
#automated phonetic transcription analysis v 1.1 by Kevin T Cunningham. Copyright (C)
#2020. Kevin T Cunningham
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. AMDG. 
#Script calculates the edit distance ratio for intelligible words in a sample. Requires CSV file "mrc2.csv"

batch<-{}
ppa<-{}

library(tidyr)
library(tidytext)
library(stringr)

# list for each important category 
# unsure if i need to do list(c()) or if i can just do list(), and what is the difference
engl_voiceless_cons <- list(c("C","f","h","k","p","s","S","t","T")) #should h be included here? 
engl_voiced_cons <- list(c("b","d","D","g","J","m","n","G","v","z","Z")) #should liquids and semivowels be included or no?
engl_fricatives <- list(c("D","f","s","S","T","v","z","Z"))
engl_affricates <- list(c("C","J"))
engl_velars <- list(c("k","g","G"))
engl_liquids <- list(c("l","r"))
# syllabic liquids? 
engl_rhotic_vowels <- list(c("X-R")) #i think this is correct but unsure  

# function to determine if value is in a list 
list_search <- function(char, list_name) {
  result <- FALSE
  for(el in list_name) {
    if(el == char) result <- TRUE
  }
  result
}

phonetic<-tibble()
stress<-tibble()
mrc<-read.csv('mrc2.csv', na.strings=c("", "NA"))
fileNames = dir(pattern = ".txt")
for (fileName in fileNames){
  phonetic<-tibble()
  stress<-tibble()
  data<-{}
  sample <- readChar(fileName, file.info(fileName)$size)
  sample<-as.character(sample) # returns sample as text representation
  sample<-str_to_upper(sample, locale="en") #converts to uppercase
  text_df<-tibble(text=sample)  #tibble is a simple data frame
  text_df <-text_df%>%
    unnest_tokens(word, text) #this breaks the column into words, one token(word) per row
  tibbletest <-tibble(mrc$word, mrc$phon) #looks like setting up a variable that includes only the word and phon columns from MRC
  tibbletest <- na.omit(tibbletest) #na.omit gets rid of cases where the value is na
  concrete <-na.omit(tibble(mrc$word, mrc$conc)) # this creates a variable of concreteness which does not produce any output
  
  tibbletest <- na.omit(tibbletest) # i don't understand how this is different from two lines above.
  for (i in 1:nrow(text_df)){
    r<-which(toupper(text_df$word[i])==tibbletest$`mrc$word`, arr.ind = TRUE) # toupper goes to uppercase. why are we doing this?
    # i guess that line defines r as which words have a value that can be found in the MRC file.
    
    phonetic <- paste(phonetic, tibbletest$`mrc$phon`[r[1]]) # not really sure what this does. from a test, it looks like it is one variable, which is the phonetic transcription of each word
    phonetic<-gsub("NA", "", phonetic) #substitute NA with blank
    phonetic<-str_split(string=phonetic, pattern=" ") #i think this splits the string whenever there is a space
    phonetic<-as.data.frame(phonetic, stringsAsFactors=FALSE) #makes this into a dataframe
    
    tibbletest$`mrc$phon`[30407]
    points<-0  # QUESTION are we calculating points for individual word or whole transcript? 
    for (word in 1:nrow(phonetic)){
      
      # BEGIN PSEUDOCODE of new solution 
      
      len<-str_length(word) #length of word, used to iterate through its chars
      if (polysyll == 1) points=points+1  #word patterns (1)
      if (nonInitialPrimaryStress == 1) points=points+1  #word patterns (2)
      points=points+str_count(phonetic[j,], "X-R")  #sound classes (2), rhotic vowels
      
      # for loop to find consonant clusters and sound classes 
      for (index in 0:len-1) {
        if (index == len-1) {
          if (list_search(word_index, engl_voiced_cons) | list_search(word[index], engl_voiceless_cons)) {
            points=points+1  #syllable structures (1)
          }
        }
        if (list_search(word[index], engl_voiced_cons) | list_search(word[index], engl_voiceless_cons)) {
          j <- index
          is_cluster <- FALSE 
          while (j < len-1) {
            if (list_search(word[j+1], engl_voiced_cons) | list_search(word[j+1], engl_voiceless_cons)) {
              j=j+1
              is_cluster <- TRUE
            } 
            else break
          }
          if (is_cluster) points=points+1  #syllable structures (2)
        }
        if (list_search(word[index], engl_velars)) points=points+1  #sound classes (1)
        if (list_search(word[index], engl_liquids)) points=points+1  #sound classes (2)
        #if word[i] in syllabic_liquid then points=points+1  #sound classes (2)
        if (list_search(word[index], engl_fricatives) | list_search(word[index], engl_affricates)) {
          points=points+1  #sound classes (3)
          if (list_search(word[index], engl_voiced_cons)) {
            points=points+1  #sound classes (4)
          }
        }
      }
      
      # END PSEUDOCODE of new solution 
      
    }
    phonetic[!apply(phonetic == "", 1, all),]
    score=points/nrow(phonetic)
  }
}
data<-cbind(fileName, score, points, nrow(phonetic))
write.table(data, file="WCD_data.csv", append=TRUE, sep = ",", row.names = FALSE, col.names = FALSE)


