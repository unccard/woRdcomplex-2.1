# woRdcomplex version 3.1 (30 January 2023)--an R software script for text analysis, including
# automated phonetic transcription analysis in v2.1 by Lindsay Greene, adapted from v1.1 by Kevin T. Cunningham. 
# Copyright (C) 2021. Lindsay Greene
# Modifications 30 January 2023 integrating code from Cunningham and Haley "WIM" which includes Shannon entropy 
# and MATTR-5 metrics from connected speech input

# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. AMDG. 
# Script calculates the edit distance ratio for intelligible words in a sample. 
# Requires that the user specify a file path on line 19.  

library(tidyr)
library(tidytext)
library(stringr)
library(dplyr)
library(rJava)
library(qdap, include.only=c("diversity", "automated_readability_index", "coleman_liau", "flesch_kincaid"))  # readability functions 
source("functions.R")

# adding for the purpose of MATTR calculations

#Simple script to calculate the moving-average type-token ratio for all text files in a directory. Outputs a single CSV file with filename 
#  Michalke, M. (2018). koRpus: An R Package for Text Analysis (Version 0.11-5). Available from https://reaktanz.de/?c=hacking&s=koRpus
library(koRpus)
#Set language to English for koRpus. Please see package documentation for further information (?koRpus)
set.kRp.env(lang="en")
koRpus.lang.en::lang.support.en()

word_db <- read.csv('UNCWordDB-2022-02-07.csv', na.strings=c("", "NA"))
tibbletest <-tibble(word_db$Word, word_db$KlatteseSyll, word_db$KlatteseBare, word_db$Zipf.value)  # isolate categories from word_db 

# TO DO: fill in arguments of data.path with path to directory containing .txt files, leaving first argument blank 
# for example: /Users/folder1/folder2 -> data_path("", "Users", "folder1", "folder2")
#data_path <- file.path("", "Users", "lindsaygreene", "Desktop", "temp")

#data_path <- "/Users/jacksa/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Documents - CARD/Cinderella/Transcripts/ABControlTranscripts"
#data_path <- "/Users/jacksa/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Documents - CARD/Cinderella/Transcripts/StrokeTranscripts/ReviewedUpdatedPreTxCinderellas/test"
#data_path <- "/Users/jacksa/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Documents - CARD/Cinderella/Transcripts/PPA cinderella transcripts UNM"
#data_path <- "/Users/jacksa/Documents/github/woRdcomplex-2.1"

data_path <- "C:/Users/jacksa/OneDrive - University of North Carolina at Chapel Hill/Documents - CARD/General/Dementia/PittSelected/PittSelCont"

files <- list.files(path=data_path, pattern="*.txt")

# create data frames to store results 
data <- createAverageDF()
word_by_word <- createWordByWordDF()
readability <- createReadabilityDF()
wbw_row = 1  # track row of wbw output, important for when we have multiple files 

for (file in 1:length(files)){
  
  fileName <- files[file]
  filePath <- paste(data_path, "/", fileName, sep="")  # update file name to absolute path 
  
  # read and store text from file
  sample <- readInSample(filePath)
  
  # perform readability analysis 
  auto_read_index <- automated_readability_index(sample)
  coleman_liau <- coleman_liau(sample)
  flesch_kincaid <- flesch_kincaid(sample)
  
  # store readability calculations 
  readability[file, 1] = fileName
  readability[file, 2] = auto_read_index$Readability$Automated_Readability_Index
  readability[file, 3] = coleman_liau$Readability$Coleman_Liau
  readability[file, 4] = flesch_kincaid$Readability$FK_grd.lvl
  readability[file, 5] = flesch_kincaid$Readability$FK_read.ease
  
  # convert sample format to analyze phonological complexity
  text_df<- convertToDF(sample)
  is_contraction <- is_nt_contraction <- 0
  contraction <- ""
  
  # initialize vectors that will be populated with data for each word in sample
  foundInDB_tscript <- phonetic_tscript <- phonetic_plain_tscript <- wf_tscript <- c()

  # populate vectors with data for each word in the transcript
  for(i in 1:nrow(text_df)) {
    word <- toString(text_df[i,1])
    
    # if word contains apostrophe, then it is a contraction 
    if(grepl("'", word, fixed=TRUE)) {
      is_contraction = 1
      parts = strsplit(word, "'")
      #TODO: don't is gonna come out weird bc it looks like do so might have to make a special case for the Klattese - will not affect wcm but just looks weird
      nt_contractions <- c("couldn", "shouldn", "wouldn", "didn", "don", "doesn", "wasn", "aren")
 
      # I think when Lindsay created the 'nt contraction fix, there is not a definition of "word" for regular contractions, so AJ added this
      word <- parts[[1]][1]
      contraction <- parts[[1]][2]
      
        if(parts[[1]][1] %in% nt_contractions) {
        is_nt_contraction = 1
        word <- substr(parts[[1]][1], 1, nchar(parts[[1]][1])-1)
      }
 
    }
    row <- which(tibbletest[,1] == word)
    if(!identical(toString(tibbletest[row, 2]),"character(0)")){  # omit words not found in word_db
      foundInDB_tscript <- append(foundInDB_tscript, toString(tibbletest[row, 1]))
      klatt = toString(tibbletest[row, 2])
      bare_klatt = toString(tibbletest[row,3])
      if(is_nt_contraction == 1) {
        klatt <- paste(klatt, "N", sep="") # Replace the N in nt contractions 
        bare_klatt <- paste(bare_klatt, "N", sep="")
        word <- paste(word,"n",sep="")
        is_nt_contraction = 0  # reset the flag
      }
      phonetic_tscript <- append(phonetic_tscript, klatt)
      phonetic_plain_tscript <- append(phonetic_plain_tscript, bare_klatt)
      wf_tscript <- append(wf_tscript, toString(tibbletest[row, 4]))
    }
    
    # If we have any contraction stem 
    if(is_contraction == 1) {  # if there is a next element 
      print(foundInDB_tscript)
      print(contraction)
      rescueContraction(contraction, foundInDB_tscript, phonetic_tscript, phonetic_plain_tscript)

            is_contraction = 0  # reset the flag 
    }
    
  }

  # transform the vectors into data frames
  foundInDB_tscript<-as.data.frame(foundInDB_tscript)
  print(foundInDB_tscript)
  phonetic_tscript<-as.data.frame(phonetic_tscript)
  phonetic_plain_tscript<-as.data.frame(phonetic_plain_tscript)
  wf_tscript<-as.data.frame(wf_tscript)

  # initialize cumulative points for each file
  phon_total <- wf_total <- 0

  # for loop going through each word in the phonetic transcript to calculate its scores
  for (word in 1:nrow(foundInDB_tscript)){

    # isolate data specific to current word
    klattese <- phonetic_tscript[word,1]
    klattese_plain <- phonetic_plain_tscript[word,1]
    wf <- as.double(wf_tscript[word,1])

    phon_points <- calculateWCM(klattese)

    # store info in word by word output
    word_by_word[wbw_row, 1] = fileName
    word_by_word[wbw_row, 2] = foundInDB_tscript[word, 1]
    word_by_word[wbw_row, 3] = klattese_plain
    word_by_word[wbw_row, 4] = phon_points
    word_by_word[wbw_row, 5] = wf

    wbw_row = wbw_row + 1

    # adding points for current word to cumulative total
    phon_total = phon_total + phon_points
    wf_total = wf_total + wf
  }

  # calculate averages for file from total points
  avg_phon <- phon_total/nrow(phonetic_tscript)
  avg_wf <- wf_total/nrow(wf_tscript)

  # obtain Word Information Measure (WIM) from Shannon Entropy of qdap diversity function

  divDF <- diversity(sample)
  wim <- divDF$shannon

  # get MATTR-5 using korpus package functions
  
  tokenized.obj <- tokenize(filePath, lang = "en") #using the tokenize function in koRpus
  m <- MATTR(tokenized.obj, window = 5) #this is the analysis window, currently set to 5 words
  m <- m@MATTR
  m <-m$MATTR
  
  # write output and file name to avg output data frame
  data[file,1] = nrow(text_df)
  data[file,2] = nrow(phonetic_tscript)
  data[file,3] = avg_phon
  data[file,4] = avg_wf
  data[file,5] = wim
  data[file,6] = m
}

# write output to file and save to same location to be opened in excel 
write.csv(readability, file=paste(data_path, "/", "readability_output.csv", sep=""))
write.csv(data, file=paste(data_path, "/", "wcm_output.csv", sep=""))
write.csv(word_by_word, file=paste(data_path, "/", "word_by_word_output.csv", sep=""))
