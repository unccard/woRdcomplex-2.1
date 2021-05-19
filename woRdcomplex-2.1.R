# woRdcomplex version 2.1 (13 May 2021)--an R software script for
# automated phonetic transcription analysis by Lindsay Greene, adapted from v1.1 by Kevin T. Cunningham. 
# Copyright (C) 2021. Lindsay Greene
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. AMDG. 
# Script calculates the edit distance ratio for intelligible words in a sample. 
# Requires CSV file "UNCCombWordDB.csv" and that the user specify a file path on line 26.  

library(tidyr)
library(tidytext)
library(stringr)
library(dplyr)

# phoneme categories 
engl_voiceless_cons <- c("C","f","h","k","p","s","S","t","T")
engl_voiced_cons <- c("b","d","D","g","J","l","m","n","G","r","v","w","y","z","Z")
engl_syll_cons <- c("L", "M", "N", "R")
engl_fricatives <- c("D","f","h","s","S","T","v","z","Z")
engl_affricates <- c("C","J")
engl_velars <- c("k","g","G")
engl_liquids <- c("l","L","r","R","X")

word_db <- read.csv('UNCCombWordDB.csv', na.strings=c("", "NA"))

# TO DO: fill in arguments of data.path with path to directory containing .txt files, leaving first argument blank 
# for example: /Users/folder1/folder2 -> data_path("", "Users", "folder1", "folder2")
data_path <- file.path("", "Users", "lindsaygreene", "Desktop")


# TO DO: add avg imag/conc/fam headers and change ncol  
# set up data frame to store results 
data <- data.frame(matrix(vector(), ncol=4, nrow=length(files)))  # data frame to store output  
files <- list.files(path=data_path, pattern="*.txt")
header_names <- list("Total_Words_in_Tscript", "Total_Words_Found_in_DB","Avg_Phon_Score", 
                    "Avg_WF_Score")  # column headers for data frame
colnames(data) <- header_names
rownames(data) <- files

# set up word by word analysis 
word_by_word <- data.frame(matrix(vector(), ncol=2))
names <- list("klattese", "wcm_score")
rownames(word_by_word) <- names

for (file in 1:length(files)){
  
  fileName <- files[file]
  
  # update file name to absolute path 
  fileName <- paste(data_path, "/", fileName, sep="")
  
  # read and store text from file 
  sample <- readChar(fileName, file.info(fileName)$size)
  sample<-as.character(sample)  # returns sample as text representation
  sample<-str_to_lower(sample, locale="en")  # convert sample to lowercase to match DB file 
  text_df<-tibble(text=sample)  # convert sample to tibble (a simple data frame) 
  text_df <-text_df%>%  # way of filtering the data 
  unnest_tokens(word, text)  # break the column into one word per row 
  tibbletest <-tibble(word_db$word, word_db$phon_klattese, word_db$polysyll, word_db$nonInitialPrimaryStress, 
                      word_db$SUBTLWF0to10, word_db$fam, word_db$conc, word_db$imag)  # isolates the categories we need from word_db 
  
  # initialize vectors that will be populated with data for each word in sample 
  phonetic_tscript <- c()
  polysyll_tscript <- c()
  nonInitPrimStress_tscript <- c()
  wf_tscript <- c()
  #fam_tscript <- c()
  #conc_tscript <- c()
  #imag_tscript <- c()
  
  # initialize cumulative points for each file 
  phon_total <- wf_total <- fam_total <- conc_total <- imag_total <- 0 
  
  # populate vectors with data for each word in the transcript 
  for(i in 1:nrow(text_df)) {
    word <- toString(text_df[i,1])
    row <- which(tibbletest[,1] == word)
    if(!identical(toString(tibbletest[row, 2]),"character(0)")){  # omit words not found in word_db
      phonetic_tscript <- append(phonetic_tscript, toString(tibbletest[row, 2]))
      polysyll_tscript <- append(polysyll_tscript, toString(tibbletest[row, 3]))
      nonInitPrimStress_tscript <- append(nonInitPrimStress_tscript, toString(tibbletest[row, 4]))
      wf_tscript <- append(wf_tscript, toString(tibbletest[row, 5]))
    }
    #fam_tscript <- append(fam_tscript, toString(tibbletest[row, 6]))
    #conc_tscript <- append(conc_tscript, toString(tibbletest[row, 7]))
    #imag_tscript <- append(conc_tscript, toString(tibbletest[row, 8]))
  }
  
  # transform the vectors into data frames 
  phonetic_tscript<-as.data.frame(phonetic_tscript)
  polysyll_tscript<-as.data.frame(polysyll_tscript)
  nonInitPrimStress_tscript<-as.data.frame(nonInitPrimStress_tscript)
  wf_tscript<-as.data.frame(wf_tscript)
  #fam_tscript<-as.data.frame(fam_tscript)
  #conc_tscript<-as.data.frame(conc_tscript)
  #imag_tscript<-as.data.frame(imag_tscript)
  
  # for loop going through each word in the phonetic transcript to calculate its scores 
  for (word in 1:nrow(phonetic_tscript)){
    
    klattese <- phonetic_tscript[word,1]
    
    # initialize cumulative points for each word in file 
    phon_points <- 0 
    
    # isolate data specific to current word    
    len <- str_length(klattese)  # number of characters in the word 
    polysyll <- polysyll_tscript[word,1]  # if polysyllabic 
    nonInitPrimStress <- nonInitPrimStress_tscript[word,1]  # if non-initial stress
    wf <- as.double(wf_tscript[word,1])
    
    # TO DO: conc, fam, imag vars
    
    # BEGIN algorithm to calculate points for the word 
    
    if (polysyll == 1) phon_points=phon_points+1  # word patterns (1)
    if (nonInitPrimStress == 1) phon_points=phon_points+1  # word patterns (2)
     
    # for loop to find consonant clusters and sound classes 
    for (index in 1:len) {
      phoneme<-substr(klattese, index, index)
      if (index == len) {
        if (phoneme %in% engl_voiced_cons | phoneme %in% engl_voiceless_cons | phoneme %in% engl_syll_cons) { 
          phon_points=phon_points+1  # syllable structures (1)
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
        if (is_cluster) phon_points=phon_points+1  # syllable structures (2)
      }
      if (phoneme %in% engl_velars) phon_points=phon_points+1  # sound classes (1)
      if (phoneme %in% engl_liquids) phon_points=phon_points+1  # sound classes (2)
      if (phoneme %in% engl_fricatives | phoneme %in% engl_affricates) {
        phon_points=phon_points+1  # sound classes (3)
        if (phoneme %in% engl_voiced_cons) {
          phon_points=phon_points+1  # sound classes (4)
        }
      }
    }
    
    # END algorithm to calculate points for the word 
    
    # store info in word by word output 
    word_by_word[word, 1] = klattese
    word_by_word[word, 2] = phon_points
    
    # adding points for current word to cumulative total 
    phon_total = phon_total + phon_points 
    wf_total = wf_total + wf  
    #fam_total = fam_total + fam
    #conc_total = conc_total + conc
    #imag_total = imag_total + imag 
  }
  
  # calculate averages for file from total points 
  avg_phon <- phon_total/nrow(phonetic_tscript)
  avg_wf <- wf_total/nrow(wf_tscript) 
  #avg_fam <- avg_conc <- avg_imag <- 0 
  
  #if(nrow(avg_fam)==0) {  # believe they should all have same num rows 
    #avg_fam = fam_total/nrow(fam_tscript)
    #avg_conc = conc_total/nrow(conc_tscript)
    #avg_imag = imag_total/nrow(imag_tscript)
  #}
  
  
  # write output and file name to data frame  
  data[file,1] = nrow(text_df)
  data[file,2] = nrow(phonetic_tscript)
  data[file,3] = avg_phon
  data[file,4] = avg_wf 
  #TO DO assign values to imag/conc etc 
}

# write output to file and save to same location as .txt files 
write.csv(data, file=paste(data_path, "/", "wcm_ouput.csv", sep=""))
#write.csv(word_by_word, file=paste(data_path, "/", "word_by_word.csv", sep=""))
