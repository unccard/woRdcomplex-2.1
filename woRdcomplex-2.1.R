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
#library(tibble)
library(dplyr)

# phoneme categories 
engl_voiceless_cons <- c("C","f","h","k","p","s","S","t","T") # h is probably okay but may want to omit
engl_voiced_cons <- c("b","d","D","g","J","l","m","n","G","r","v","w","y","z","Z") 
engl_syll_cons <- c("L", "M", "N", "R")  
engl_fricatives <- c("D","f","h","s","S","T","v","z","Z")
engl_affricates <- c("C","J")
engl_velars <- c("k","g","G")
engl_liquids <- c("l","L","r","R","X") 

phon_score <- 0
wf_score <- 0

#phonetic<-tibble()
stress<-tibble()
word_db<-read.csv('UNCCombWordDB.csv', na.strings=c("", "NA"))
fileNames = dir(pattern = ".txt")
for (fileName in fileNames){
  #phonetic<-tibble()
  phonetic <- c()
  stress<-tibble()
  data<-{} 
  sample <- readChar(fileName, file.info(fileName)$size)
  sample<-as.character(sample) # returns sample as text representation
  sample<-str_to_lower(sample, locale="en") #converts to lowercase to match DB file 
  text_df<-tibble(text=sample)  #tibble is a simple data frame
  text_df <-text_df%>%
  unnest_tokens(word, text) #this breaks the column into words, one token(word) per row
  tibbletest <-tibble(word_db$word, word_db$phon_klattese, word_db$polysyll, word_db$nonInitialPrimaryStress, word_db$SUBTLWF0to10) #variable that isolates what we need from db
  tibbletest <- na.omit(tibbletest) #na.omit gets rid of cases where the value is na
  #concrete <-na.omit(tibble(data$word, data$conc)) # this creates a variable of concreteness which does not produce any output
  

  for(i in 1:nrow(text_df)) {
  #for(i in 1:5) {
    word <- toString(text_df[i,1])
    row <- which(tibbletest[,1] == word)
    col <- 2
    # need to omit character(0)
    phonetic<-append(phonetic, toString(tibbletest[row, col]))
  }
  
  phonetic<-as.data.frame(phonetic)
  phonetic<-unique(phonetic)
  
  print(phonetic)
  #tibbletest <- na.omit(tibbletest) # i don't understand how this is different from two lines above.
  for (i in 1:nrow(phonetic)){
    #r<-which(text_df$word[i]==tibbletest$`word_db$word`, arr.ind = TRUE) 
    # r is index of current word in the large database.
    
    #append(phonetic, r[1])
    
    #phonetic = paste(phonetic, tibbletest$`word_db$phon_klattese`[r[1]]) # looking for phonetic transcription in column 1 of row r 
    #phonetic <- gsub("NA", "", phonetic) #substitute NA with blank
    #phonetic <- strsplit(phonetic, "") #i think this splits the string whenever there is a space 
    #phonetic <- separate_rows(as.character(phonetic), 1, sep = " ")
    #cSplit(phonetic, direction = "long")
    #phonetic = as.data.frame(phonetic, stringsAsFactors=FALSE) #makes this into a dataframe
    
    #as.data.frame(phonetic)
    
    #tibbletest$`word_db$phon_klattese`[30407] # useful for testing? 
    phon_points<-0  
    wf_points<-0
    for (word in 1:nrow(phonetic)){
      
      # BEGIN new solution 
      
      len <- str_length(word)  # number of characters in the word 
      polysyll <- tibbletest$`word_db$polysyll`[word]  # is polysyllabic y/n 
      nonInitPrimStress <- word_db$nonInitialPrimaryStress[word]  # has non-initial stress y/n
      word_freq <- word_db$SUBTLWF0to10[word]  # normalized dist. word frequency score 
      wf_points = wf_points + word_freq  # totaling wf for each word 
      #if (polysyll == 1) phon_points=phon_points+1  #word patterns (1)
      #if (nonInitPrimStress == 1) phon_points=phon_points+1  #word patterns (2)
     
      # for loop to find consonant clusters and sound classes 
      for (index in 1:len) {
        phoneme<-substr(word, index, index)
        if (index == len) {
          if (phoneme %in% engl_voiced_cons | phoneme %in% engl_voiceless_cons | phoneme %in% engl_syll_cons) { 
            phon_points=phon_points+1  #syllable structures (1)
          }
        }
        if (phoneme %in% engl_voiced_cons | phoneme %in% engl_voiceless_cons) {
          j <- index
          is_cluster <- FALSE 
          while (j < len) {
            next_phon <- substr(word, j+1, j+1)
            if (next_phon %in% engl_voiced_cons | next_phon %in% engl_voiceless_cons) {
              j=j+1
              is_cluster <- TRUE
            } 
            else break
          }
          if (is_cluster) phon_points=phon_points+1  #syllable structures (2)
        }
        if (phoneme %in% engl_velars) phon_points=phon_points+1  #sound classes (1)
        if (phoneme %in% engl_liquids) phon_points=phon_points+1  #sound classes (2)
        if (phoneme %in% engl_fricatives | phoneme %in% engl_affricates) {
          phon_points=phon_points+1  #sound classes (3)
          if (phoneme %in% engl_voiced_cons) {
            phon_points=phon_points+1  #sound classes (4)
          }
        }
      }
      
      # END new solution 
      
    }
    phonetic[!apply(phonetic == "", 1, all),]
    wf_score =  wf_points/nrow(phonetic)  
    phon_score = phon_points/nrow(phonetic)
  }
}
data<-cbind(fileName, phon_score, phon_points, nrow(phonetic))
write.table(data, file="WCD_data.csv", append=TRUE, sep = ",", row.names = FALSE, col.names = FALSE) #unsure what this is doing


