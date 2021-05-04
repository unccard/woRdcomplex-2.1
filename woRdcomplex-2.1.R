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

total_phon_score <- 0
total_wf_score <- 0

word_db<-read.csv('UNCCombWordDB.csv', na.strings=c("", "NA"))
fileNames = dir(pattern = ".txt")

for (fileName in fileNames){
  
  phonetic_tscript <- c()
  polysyll_tscript <- c()
  nonInitPrimStress_tscript <- c()
  wf_tscript <- c()
  
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
  #phonetic_tscript<-unique(phonetic_tscript) 
  
  polysyll_tscript<-as.data.frame(polysyll_tscript)
  
  nonInitPrimStress_tscript<-as.data.frame(nonInitPrimStress_tscript)
  
  wf_tscript<-as.data.frame(wf_tscript)
  
  for (i in 1:nrow(phonetic_tscript)){
    phon_points<-0  
    wf_points<-0
      
    len <- str_length(word)  # number of characters in the word 
    polysyll <- tibbletest[word,3]  # is polysyllabic y/n 
    nonInitPrimStress <- tibbletest[]  # has non-initial stress y/n
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
  }
  phonetic[!apply(phonetic == "", 1, all),]
  wf_score =  wf_points/nrow(phonetic)  
  phon_score = phon_points/nrow(phonetic)
}

print(phon_score)
print(wf_score)
data<-cbind(fileName, phon_score, phon_points, nrow(phonetic))
write.table(data, file="WCD_data.csv", append=TRUE, sep = ",", row.names = FALSE, col.names = FALSE) #unsure what this is doing


