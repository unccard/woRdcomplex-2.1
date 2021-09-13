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
#library(tokenizers)

# phoneme categories 
engl_voiceless_cons <- c("C","f","h","k","p","s","S","t","T")
engl_voiced_cons <- c("b","d","D", "F","g","J","l","m","n","G","r","v","w","y","z","Z")
engl_syll_cons <- c("L", "M", "N", "R")
engl_fricatives <- c("D","f","h","s","S","T","v","z","Z")
engl_affricates <- c("C","J")
engl_velars <- c("k","g","G")
engl_liquids <- c("l","L","r","R","X")

word_db <- read.csv('UNCCombWordDB.csv', na.strings=c("", "NA"))

# TO DO: fill in arguments of data.path with path to directory containing .txt files, leaving first argument blank 
# for example: /Users/folder1/folder2 -> data_path("", "Users", "folder1", "folder2")
data_path <- file.path("", "Users", "lindsaygreene", "Desktop")
# data_path <- file.path("", "Users", "lindsaygreene", "Desktop", "programming", "woRdcomplexity", "woRdcomplex-2.1")

# set up data frame to store results 
data <- data.frame(matrix(vector(), ncol=7, nrow=length(files)))  # data frame to store avg output  
files <- list.files(path=data_path, pattern="*.txt")
header_names <- list("Total_Words_in_Tscript", "Total_Words_Found_in_DB","Avg_Phon_Score", 
                    "Avg_WF_Score", "Avg_Fam_Score", "Avg_Conc_Score", "Avg_Imag_Score")  # column headers for avg output df 
colnames(data) <- header_names
rownames(data) <- files

# set up word by word analysis 
word_by_word <- data.frame(matrix(vector(), ncol=5))  # data frame to store info ab individual words from each transcript
#names <- list("File_Name", "Target_Word", "Target_Klattese", "Actual_Klattese","Target_WCM_Score", 
              #"Actual_WCM_Score", "WCM_Ratio","Word_Frequency")  # column headers for word by word df 
names <- list("File_Name", "Target_Word", "Target_Klattese", "Target_WCM_Score", "Word_Frequency")
colnames(word_by_word) <- names
wbw_row = 1  # count number of rows in word by word db 

for (file in 1:length(files)){
  
  fileName <- files[file]
  filePath <- paste(data_path, "/", fileName, sep="")  # update file name to absolute path 
  
  # read and store text from file 
  sample <- readChar(filePath, file.info(filePath)$size)
  #tokenized_sample <- tokenize_words(sample)
  #sample <- sample.rstrip('\r\n')
  #sample <- as.character(sample)  # returns sample as text representation
  #tokenized_sample <- str_to_lower(sample, locale="en")  # convert sample to lowercase to match DB file 
  
  
  text_df<-tibble(text=sample)  # convert sample to tibble (a simple data frame) 
  text_df <-text_df%>%  # way of filtering the data 
  unnest_tokens(word, text)  # break the column into one word per row 
  tibbletest <-tibble(word_db$word, word_db$phon_klattese, word_db$polysyll, word_db$nonInitialPrimaryStress, 
                      word_db$SUBTLWF0to10, word_db$fam, word_db$conc, word_db$imag)  # isolates the categories we need from word_db 
  
  # initialize vectors that will be populated with data for each word in sample 
  foundInDB_tscript <- c()  # word in english orthography (if found in the database)
  phonetic_tscript <- c()  # word in klattese
  polysyll_tscript <- c()  # whether the word is polysyllabic
  nonInitPrimStress_tscript <- c()  # whether the word has non-initial primary stress 
  wf_tscript <- c()  # frequency of the word 
  
  stripped_words <- c()
  #fam_tscript <- c()
  #conc_tscript <- c()
  #imag_tscript <- c()
  
  # initialize cumulative points for each file 
  target_wcm_total <- wf_total <- fam_total <- conc_total <- imag_total <- 0 
  
  # populate vectors with data for each word in the transcript 
  for(i in 1:nrow(text_df)) {
    word <- str_trim(toString(text_df[i,1]), "right")
    stripped_words <- append(word, stripped_words[i, 1])
    row <- which(tibbletest[,1] == word)
    if(!identical(toString(tibbletest[row, 2]),"character(0)")){  # omit words not found in word_db
      foundInDB_tscript <- append(foundInDB_tscript, toString(tibbletest[row, 1]))
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
  foundInDB_tscript<-as.data.frame(foundInDB_tscript)
  phonetic_tscript<-as.data.frame(phonetic_tscript)
  polysyll_tscript<-as.data.frame(polysyll_tscript)
  nonInitPrimStress_tscript<-as.data.frame(nonInitPrimStress_tscript)
  wf_tscript<-as.data.frame(wf_tscript)
  # fam_tscript<-as.data.frame(fam_tscript)
  # conc_tscript<-as.data.frame(conc_tscript)
  # imag_tscript<-as.data.frame(imag_tscript)
  
  # replace empty values in conc, imag, and fam with 0 
  # fam_null <- conc_null <- imag_null <- 0
  # for(i in 1:nrow(fam_tscript)) if(fam_tscript[i,1]=="NA" || fam_tscript[i,1]=="character(0)") {
  #   fam_tscript[i,1] = 0
  #   fam_null = fam_null + 1
  # }
  # for(i in 1:nrow(conc_tscript)) if(conc_tscript[i,1]=="NA" || conc_tscript[i,1]=="character(0)") {
  #   conc_tscript[i,1] = 0
  #   conc_null = conc_null + 1
  # }
  # for(i in 1:nrow(imag_tscript)) if(imag_tscript[i,1]=="NA" || imag_tscript[i,1]=="character(0)") {
  #   imag_tscript[i,1] = 0
  #   imag_null = imag_null + 1
  # }
  
  # for loop going through each word in the phonetic transcript to calculate its scores 
  for (word in 1:nrow(phonetic_tscript)){
    
    klattese <- phonetic_tscript[word,1]
    target_klattese_readable <- ""  # klattese word minus stress and syllable marker 
    
    # initialize cumulative points for each word in file 
    target_points <- 0 
    
    # isolate data specific to current word    
    len <- str_length(klattese)  # number of characters in the word 
    polysyll <- polysyll_tscript[word,1]  # if polysyllabic 
    nonInitPrimStress <- nonInitPrimStress_tscript[word,1]  # if non-initial stress
    wf <- as.double(wf_tscript[word,1])
    
    # BEGIN algorithm to calculate points for the word 
    
    if (polysyll == 1) target_points=target_points+1  # word patterns (1)
    if (nonInitPrimStress == 1) target_points=target_points+1  # word patterns (2)
     
    # for loop to find consonant clusters and sound classes 
    for (index in 1:len) {
      phoneme <- substr(klattese, index, index)
      
      # isolate phonemes and remove stress marker 
      # this makes klattese more readable in csv file format 
      if((phoneme >= 41 && phoneme >= 90) || (phoneme >= 61 && phoneme >= 122)) {
        target_klattese_readable = paste(target_klattese_readable, phoneme, sep = "")
      } else if(phoneme == '@' || phoneme == '^' || phoneme == '|') {
        target_klattese_readable = paste(target_klattese_readable, phoneme, sep = "")
      }
      
      if (index == len) {
        if (phoneme %in% engl_voiced_cons | phoneme %in% engl_voiceless_cons | phoneme %in% engl_syll_cons) { 
          target_points=target_points+1  # syllable structures (1)
        } 
      }
      if (phoneme %in% engl_voiced_cons | phoneme %in% engl_voiceless_cons) {
        i <- index
        is_cluster <- FALSE 
        while (i < len) {
          next_phon <- substr(klattese, i+1, i+1)
          if (next_phon %in% engl_voiced_cons | next_phon %in% engl_voiceless_cons) {
            i=i+1
            is_cluster <- TRUE
          } 
          else break
        }
        if (is_cluster) target_points=target_points+1  # syllable structures (2)
      }
      if (phoneme %in% engl_velars) target_points=target_points+1  # sound classes (1)
      if (phoneme %in% engl_liquids) target_points=target_points+1  # sound classes (2)
      if (phoneme %in% engl_fricatives | phoneme %in% engl_affricates) {
        target_points=target_points+1  # sound classes (3)
        if (phoneme %in% engl_voiced_cons) {
          target_points=target_points+1  # sound classes (4)
        }
      }
    }
    
    # END algorithm to calculate points for the word 
    
    # store info in word by word output 
    word_by_word[wbw_row, 1] = fileName
    word_by_word[wbw_row, 2] = foundInDB_tscript[word, 1]
    word_by_word[wbw_row, 3] = target_klattese_readable
    # word_by_word[wbw_row, 4] = actual_klattese_readable
    word_by_word[wbw_row, 4] = target_points
    # word_by_word[wbw_row, 6] = actual_points
    # word_by_word[wbw_row, 7] = wcm_ratio
    word_by_word[wbw_row, 5] = wf
    
    # adding points for current word to cumulative total 
    target_wcm_total = target_wcm_total + target_points 
    # actual_wcm_total = actual_wcm_total + actual_points
    wf_total = wf_total + wf  
    
    wbw_row = wbw_row + 1  # move to next row in database
  }
  
  # loop through fam, conc, imag and add up non-zero values 
  # for(i in 1:nrow(fam_tscript)) if(as.integer(fam_tscript[i,1])>0) fam_total = fam_total + as.integer(fam_tscript[i,1])
  # for(i in 1:nrow(conc_tscript)) if(as.integer(conc_tscript[i,1])>0) conc_total = conc_total + as.integer(conc_tscript[i,1])
  # for(i in 1:nrow(imag_tscript)) if(as.integer(imag_tscript[i,1])>0) imag_total = imag_total + as.integer(imag_tscript[i,1])

  # calculate averages for file from total points 
  avg_target_wcm <- target_wcm_total/nrow(phonetic_tscript)
  avg_wf <- wf_total/nrow(wf_tscript)
  # avg_fam <- fam_total/(nrow(fam_tscript)-fam_null) 
  # avg_conc <- conc_total/(nrow(conc_tscript)-conc_null)
  # avg_imag <- imag_total/(nrow(imag_tscript)-imag_null) 
  
  # write output and file name to avg output data frame  
  data[file,1] = nrow(text_df)
  data[file,2] = nrow(phonetic_tscript)
  data[file,3] = avg_target_wcm
  data[file,4] = avg_wf 
  # data[file,5] = avg_fam
  # data[file,6] = avg_conc
  # data[file,7] = avg_imag
}

# write output to file and save to same location to be opened in excel 
write.csv(data, file=paste(data_path, "/", "wcm_output.csv", sep=""))
write.csv(word_by_word, file=paste(data_path, "/", "word_by_word.csv", sep=""))
