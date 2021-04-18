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

# phoneme categories 
engl_voiceless_cons <- c("C","f","h","k","p","s","S","t","T") # h is probably okay but may want to omit
engl_voiced_cons <- c("b","d","D","g","J","l","m","n","G","r","v","w","y","z","Z") 
engl_syll_cons <- c("L", "M", "N", "R")  
engl_fricatives <- c("D","f","h","s","S","T","v","z","Z")
engl_affricates <- c("C","J")
engl_velars <- c("k","g","G")
engl_liquids <- c("l","L","r","R","X") 

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
    points<-0  
    for (word in 1:nrow(phonetic)){
      
      # BEGIN new solution 
      
      len<-str_length(word)  # number of characters in the word 
      if (polysyll == 1) points=points+1  #word patterns (1)
      if (nonInitialPrimaryStress == 1) points=points+1  #word patterns (2)
     
      # for loop to find consonant clusters and sound classes 
      for (index in 0:len-1) {
        phoneme<-substr(word, index, index)
        if (index == len-1) {
          if (phoneme %in% engl_voiced_cons | phoneme %in% engl_voiceless_cons | phoneme %in% engl_syll_cons) { 
            points=points+1  #syllable structures (1)
          }
        }
        if (phoneme %in% engl_voiced_cons | phoneme %in% engl_voiceless_cons) {
          j <- index
          is_cluster <- FALSE 
          while (j < len-1) {
            next_phon <- substr(word, j+1, j+1)
            if (next_phon %in% engl_voiced_cons | next_phon %in% engl_voiceless_cons) {
              j=j+1
              is_cluster <- TRUE
            } 
            else break
          }
          if (is_cluster) points=points+1  #syllable structures (2)
        }
        if (phoneme %in% engl_velars) points=points+1  #sound classes (1)
        if (phoneme %in% engl_liquids) points=points+1  #sound classes (2)
        if (phoneme %in% engl_fricatives | phoneme %in% engl_affricates) {
          points=points+1  #sound classes (3)
          if (phoneme %in% engl_voiced_cons) {
            points=points+1  #sound classes (4)
          }
        }
      }
      
      # END new solution 
      
    }
    phonetic[!apply(phonetic == "", 1, all),]
    score=points/nrow(phonetic)
  }
}
data<-cbind(fileName, score, points, nrow(phonetic))
write.table(data, file="WCD_data.csv", append=TRUE, sep = ",", row.names = FALSE, col.names = FALSE)


