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
engl_voiced_cons <- c("b","d","D","F","g","J","l","M","m","N","n","G","r","v","w","y","z","Z")  # word final M and N? 
# engl_syll_cons <- c("L", "M", "N", "R") 
engl_fricatives <- c("D","f","h","s","S","T","v","z","Z")
engl_affricates <- c("C","J")
engl_velars <- c("k","g","G")
engl_liquids <- c("l","L","r","R","X")

word_db <- read.csv('/Users/lindsaygreene/Desktop/programming/woRdcomplexity/woRdcomplex-2.1/UNCCombWordDB.csv', na.strings=c("", "NA"))

# TO DO: fill in arguments of data.path with path to directory containing .txt files, leaving first argument blank 
# for example: /Users/folder1/folder2 -> data_path("", "Users", "folder1", "folder2")
data_path <- file.path("", "Users", "lindsaygreene", "Desktop")

# set up data frame to store average results  
data <- data.frame(matrix(vector(), ncol=4, nrow=length(files)))  # data frame to store avg output  
files <- list.files(path=data_path, pattern="*.txt")
header_names <- list("Total_Words_in_Tscript", "Total_Words_Found_in_DB","Avg_Phon_Score","Avg_WF_Score")  # column headers for avg output df 
colnames(data) <- header_names
rownames(data) <- files

# set up data frame to store word by word results 
word_by_word <- data.frame(matrix(vector(), ncol=5))  # data frame to store info ab individual words from each transcript
names <- list("File_Name", "Word", "Phonetic_Word", "WCM_Score", "Word_Frequency")  # column headers for word by word df 
colnames(word_by_word) <- names
wbw_row = 1  # count number of rows in word by word db 

for (file in 1:length(files)){
  
  fileName <- files[file]
  filePath <- paste(data_path, "/", fileName, sep="")  # update file name to absolute path 
  
  # read and store text from file 
  sample <- readChar(filePath, file.info(filePath)$size)
  sample<-as.character(sample)  # returns sample as text representation
  sample<-str_to_lower(sample, locale="en")  # convert sample to lowercase to match DB file 
  text_df<-tibble(text=sample)  # convert sample to tibble (a simple data frame) 
  text_df <-text_df%>%  # way of filtering the data in dplyr 
  unnest_tokens(word, text)  # break the column into one word per row 
  tibbletest <-tibble(word_db$word, word_db$phon_klattese, word_db$polysyll, word_db$nonInitialPrimaryStress, 
                      word_db$SUBTLWF0to10, word_db$fam, word_db$conc, word_db$imag)  # isolates the categories we need from word_db 
  
  # initialize vectors that will be populated with data for each word in sample 
  foundInDB_tscript <- c()  # each word in English orthography (if found in the database)
  phonetic_tscript <- c()  # each word in Klattese
  polysyll_tscript <- c()  # whether each word is polysyllabic
  nonInitPrimStress_tscript <- c()  # whether each word has non-initial primary stress 
  wf_tscript <- c()  # frequency of each word 
  
  # initialize cumulative points for each file 
  phon_total <- wf_total <- 0 
  # populate vectors with data for each word in the transcript 
  for(i in 1:nrow(text_df)) {
    word <- toString(text_df[i,1])
    row <- which(tibbletest[,1] == word)
    if(!identical(toString(tibbletest[row, 2]),"character(0)")){  # omit words not found in word_db
      foundInDB_tscript <- append(foundInDB_tscript, toString(tibbletest[row, 1]))
      phonetic_tscript <- append(phonetic_tscript, toString(tibbletest[row, 2]))
      polysyll_tscript <- append(polysyll_tscript, toString(tibbletest[row, 3]))
      nonInitPrimStress_tscript <- append(nonInitPrimStress_tscript, toString(tibbletest[row, 4]))
      wf_tscript <- append(wf_tscript, toString(tibbletest[row, 5]))
    }
  }
  
  # transform the vectors into data frames 
  foundInDB_tscript<-as.data.frame(foundInDB_tscript)
  phonetic_tscript<-as.data.frame(phonetic_tscript)
  polysyll_tscript<-as.data.frame(polysyll_tscript)
  nonInitPrimStress_tscript<-as.data.frame(nonInitPrimStress_tscript)
  wf_tscript<-as.data.frame(wf_tscript)
  
  # for loop going through each word in the phonetic transcript to calculate its scores 
  for (word in 1:nrow(phonetic_tscript)){
    
    klattese <- phonetic_tscript[word,1]
    klattese_without_tilde <- ""  # Klattese word minus stress & syllable markers for readability  
    
    # initialize cumulative points for each word in file 
    phon_points <- 0 
    
    # isolate data specific to current word    
    len <- str_length(klattese)  # number of characters in the word 
    polysyll <- polysyll_tscript[word,1]  # if the word is polysyllabic 
    nonInitPrimStress <- nonInitPrimStress_tscript[word,1]  # if the word has non-initial stress
    wf <- as.double(wf_tscript[word,1])
    
    # BEGIN algorithm to calculate points for the word 
    
    if (polysyll == 1) phon_points=phon_points+1  # word patterns (1)
    if (nonInitPrimStress == 1) phon_points=phon_points+1  # word patterns (2)
    
    # if the word ends in a consonant 
    final_phoneme <- substr(klattese, len, len)
    if (final_phoneme %in% engl_voiced_cons | final_phoneme %in% engl_voiceless_cons) { 
      phon_points=phon_points+1  # syllable structures (1)
    } 
    
    # if the word has consonant clusters 
    split <- strsplit(klattese, "([iIEe@aWY^cOoUuRx|X\\ˈ]+|-+)+")  # regular expression to isolate consonants 
    for(i in 1:length(split[[1]])) {
      if(str_length(split[[1]][i]) > 1) { 
        phon_points = phon_points + 1  # syllable structures (2)
      }
    }
     
    # for loop to assign points for sound classes 
    for (index in 1:len) {
      phoneme <- substr(klattese, index, index)
      
      # isolate phonemes and remove stress marker to make Klattese more readable in csv file format 
      if((phoneme >= 41 && phoneme >= 90) || (phoneme >= 61 && phoneme >= 122)) {
        klattese_without_tilde = paste(klattese_without_tilde, phoneme, sep = "")
      } else if(phoneme == '@' || phoneme == '^' || phoneme == '|') {
        klattese_without_tilde = paste(klattese_without_tilde, phoneme, sep = "")
      }
      
      # WCM rules for sound classes 
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
    word_by_word[wbw_row, 1] = fileName
    word_by_word[wbw_row, 2] = foundInDB_tscript[word, 1]
    word_by_word[wbw_row, 3] = klattese_without_tilde
    word_by_word[wbw_row, 4] = phon_points
    word_by_word[wbw_row, 5] = wf
    
    wbw_row = wbw_row + 1  # move to next row in the word by word df 
    
    # adding points for current word to cumulative total 
    phon_total = phon_total + phon_points 
    wf_total = wf_total + wf  
  }
  
  # calculate averages for file from total points 
  avg_phon <- phon_total/nrow(phonetic_tscript)
  avg_wf <- wf_total/nrow(wf_tscript)
  
  # write output and file name to avg output data frame  
  data[file,1] = nrow(text_df)
  data[file,2] = nrow(phonetic_tscript)
  data[file,3] = avg_phon
  data[file,4] = avg_wf 
}

# write output to file and save to same location to be opened in excel 
write.csv(data, file=paste(data_path, "/", "wcm_output.csv", sep=""))
write.csv(word_by_word, file=paste(data_path, "/", "word_by_word.csv", sep=""))
