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
library(dplyr)

# phoneme categories 
engl_voiceless_cons <- c("C","f","h","k","p","s","S","t","T") # h is probably okay but may want to omit
engl_voiced_cons <- c("b","d","D","g","J","l","m","n","G","r","v","w","y","z","Z") 
engl_syll_cons <- c("L", "M", "N", "R")  
engl_fricatives <- c("D","f","h","s","S","T","v","z","Z")
engl_affricates <- c("C","J")
engl_velars <- c("k","g","G")
engl_liquids <- c("l","L","r","R","X") 

word_db<-read.csv('UNCCombWordDB.csv', na.strings=c("", "NA"))
fileNames = dir(pattern = ".txt")

for (fileName in fileNames){
  
  phonetic_tscript <- c()
  polysyll_tscript <- c()
  nonInitPrimStress_tscript <- c()
  wf_tscript <- c()
  
  phon_total <- 0
  wf_total <- 0
  
  data<-{} 
  sample <- readChar(fileName, file.info(fileName)$size)
  sample<-as.character(sample) # returns sample as text representation
  sample<-str_to_lower(sample, locale="en") #converts to lowercase to match DB file 
  text_df<-tibble(text=sample)  #tibble is a simple data frame
  text_df <-text_df%>%
  unnest_tokens(word, text) #this breaks the column into words, one token(word) per row
  tibbletest <-tibble(word_db$word, word_db$phon_klattese, word_db$polysyll, word_db$nonInitialPrimaryStress, word_db$SUBTLWF0to10) #variable that isolates what we need from db
  #tibbletest <- na.omit(tibbletest) #na.omit gets rid of cases where the value is na
  #concrete <-na.omit(tibble(data$word, data$conc)) # this creates a variable of concreteness which does not produce any output
  
  # creating vectors for each variable for each word in the transcript 
  for(i in 1:nrow(text_df)) {
    word <- toString(text_df[i,1])
    row <- which(tibbletest[,1] == word)
    if(identical(toString(tibbletest[row, 2]),"character(0)")){}
    else {
      phonetic_tscript <- append(phonetic_tscript, toString(tibbletest[row, 2]))
      polysyll_tscript <- append(polysyll_tscript, toString(tibbletest[row, 3]))
      nonInitPrimStress_tscript <- append(nonInitPrimStress_tscript, toString(tibbletest[row, 4]))
      wf_tscript <- append(wf_tscript, toString(tibbletest[row, 5]))
    }
  }
  
  # transforming the vectors into dataframes 
  phonetic_tscript<-as.data.frame(phonetic_tscript)
  polysyll_tscript<-as.data.frame(polysyll_tscript)
  nonInitPrimStress_tscript<-as.data.frame(nonInitPrimStress_tscript)
  wf_tscript<-as.data.frame(wf_tscript)
  
  # for loop for each word in the phonetic transcript to calculate its score 
  for (word in 1:nrow(phonetic_tscript)){
    print(word)
    
    # cumulative points for each word 
    phon_points<-0  
    wf_points<-0
    
    # isolating the data we need to calculate each word's score   
    len <- str_length(word)  # number of characters in the word 
    polysyll <- polysyll_tscript[word,1]  # if polysyllabic 
    nonInitPrimStress <- nonInitPrimStress_tscript[word,1]  # if non-initial stress 
    
    wf_points = wf_points + wf_tscript[word,1]  # adding up normalized wf score  
    
    if (polysyll == 1) phon_points=phon_points+1  #word patterns (1)
    if (nonInitPrimStress == 1) phon_points=phon_points+1  #word patterns (2)
     
    # for loop to find consonant clusters and sound classes 
    for (index in 1:len) {
      phoneme<-substr(word, index, index)
      if (index == len) {
        if (phoneme %in% engl_voiced_cons | phoneme %in% engl_voiceless_cons | phoneme %in% engl_syll_cons) { 
          phon_points=phon_points+1  #syllable structures (1)
        }
      }
      if (phoneme %in% engl_voiced_cons | phoneme %in% engl_voiceless_cons) {
        i <- index
        is_cluster <- FALSE 
        while (i < len) {
          next_phon <- substr(word, i+1, i+1)
          if (next_phon %in% engl_voiced_cons | next_phon %in% engl_voiceless_cons) {
            i=i+1
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
    
    phon_total = phon_total + phon_points # adding phonetic points for this word to our total 
    wf_total = wf_total + wf_points # adding wf points for this word to our total  
  }
  #phonetic[!apply(phonetic == "", 1, all),]
  
  # calculate averages for each transcript from total points 
  avg_phon <- phon_total/nrow
  avg_wf <- wf_total/nrow(phonetic_tscript) 
}

#data<-cbind(fileName, phon_total, phon_points, nrow(phonetic))
#write.table(data, file="WCD_data.csv", append=TRUE, sep = ",", row.names = FALSE, col.names = FALSE) #unsure what this is doing


