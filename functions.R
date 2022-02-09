createAverageDF <- function() {
  data <- data.frame(matrix(vector(), ncol=4, nrow=length(files)))  # data frame to store avg output  
  header_names <- list("Total_Words_in_Tscript", "Total_Words_Found_in_DB","Avg_WCM_Score","Avg_WF_Score")  # column headers for avg output df 
  colnames(data) <- header_names
  rownames(data) <- files
  return(data)
}

createWordByWordDF <- function() {
  word_by_word <- data.frame(matrix(vector(), ncol=5))  # data frame to store info ab individual words from each transcript
  names <- list("File_Name", "Word", "Phonetic_Word", "WCM_Score", "Word_Frequency")  # column headers for word by word df 
  colnames(word_by_word) <- names
  return(word_by_word)
}

createReadabilityDF <- function() {
  readability <- data.frame(matrix(vector(), ncol=5))
  names <- list("File_Name", "ARI", "Coleman_Liau", "FK_Grade_Level", "FK_Read_Ease")
  colnames(readability) <- names
  return(readability)
}

readInSample <- function(filePath) {
  sample <- readChar(filePath, file.info(filePath)$size)
  sample<-as.character(sample)  # returns sample as text representation
  sample<-str_to_lower(sample, locale="en")  # convert sample to lowercase to match DB file
  return(sample)
}

convertToDF <- function(sample){
  text_df<-tibble(text=sample)  # convert sample to tibble (a simple data frame)
  text_df <-text_df%>%  # way of filtering the data in dplyr
  unnest_tokens(word, text)  # break the column into one word per row
  return(text_df)
}

rescueContraction <- function(contraction, foundInDB_tscript, phonetic_tscript, phonetic_plain_tscript) {  # format contractions for display in output file
  isVoiced <- 1
  engl_voiceless_cons <- c("C","f","h","k","p","s","S","t","T")
  
  word <- foundInDB_tscript[nrow(foundInDB_tscript), 1]  # the contraction stem 
  base <- phonetic_tscript[nrow(phonetic_tscript), 1]  # base in klattese
  bare_base <- phonetic_plain_tscript[nrow(phonetic_plain_tscript), 1]  # bare base in klattese 
  final_phoneme <- substr(base, str_length(base), str_length(base))  # last sound in base
  if(final_phoneme %in% engl_voiceless_cons) isVoiced <- 0
  
  # add the correct pronunciation of the contraction to the klattese, and format english 
  if(contraction == "s") {
    word <- paste(word, "'s", sep="")
    if(isVoiced == 1) {
      base <- paste(base, "z", sep="")
      bare_base <- paste(bare_base, "z", sep="")
    } else {
      base <- paste(base, "s", sep = "")
      bare_base <- paste(bare_base, "s", sep="")
    }
  } else if(contraction == "d") {
    word <- paste(word, "'d", sep="")
    base <- paste(base, "d", sep="")
    bare_base <- paste(bare_base, "d", sep="")
  } else if(contraction == "t") {
    word <- paste(word, "'t", sep="")
    base <- paste(base, "t", sep="")
    bare_base <- paste(bare_base, "t", sep="")
  } else if(contraction == "ve") {
    word <- paste(word, "'ve", sep="")
    base <- paste(base, "v", sep="")
    bare_base <- paste(bare_base, "v", sep="")
  } 
  else {  # contraction is "ll"
    word <- paste(word, "'ll", sep="")
    base <- paste(base, "L", sep="")  
    bare_base <- paste(bare_base, "L", sep="")
  }
  
  # Replace the values in transcripts with rescued contractions
  foundInDB_tscript[nrow(foundInDB_tscript), 1] <- word
  phonetic_tscript[nrow(phonetic_tscript), 1] <- base
  phonetic_plain_tscript[nrow(phonetic_plain_tscript), 1] <- bare_base
}

calculateWCM <- function(klattese) {  # calculate WCM score for the word 
  # phoneme categories 
  engl_voiceless_cons <- c("C","f","h","k","p","s","S","t","T")
  engl_voiced_cons <- c("b","d","D","F","g","J","l","M","m","N","n","G","r","v","w","y","z","Z")  # word final M and N? 
  engl_syll_cons <- c("L", "M", "N", "R") 
  engl_fricatives <- c("D","f","h","s","S","T","v","z","Z")
  engl_affricates <- c("C","J")
  engl_velars <- c("k","g","G")
  engl_liquids <- c("l","L","r","R","X")
  
  phon_points <- 0 
  syllables <- 1
  nonInitPrimStress <- 0
  
  # if the word ends in a consonant 
  len <- str_length(klattese)
  final_phoneme <- substr(klattese, len, len)
  if (final_phoneme %in% engl_voiced_cons | final_phoneme %in% engl_voiceless_cons | final_phoneme %in% engl_syll_cons) { 
    phon_points=phon_points+1  # syllable structures (1)
  } 
  
  # if the word has consonant clusters 
  split <- strsplit(klattese, "([iIEe@aWY^cOoUuRx|XLMNR\\ˈ]+|-+)+")  # regular expression to isolate consonants 
  for(i in 1:length(split[[1]])) {
    if(str_length(split[[1]][i]) > 1) { 
      phon_points = phon_points + 1  # syllable structures (2)
    }
  }
  
  # for loop to assign points for sound classes, and find stress and syllables 
  for (i in 1:str_length(klattese)) {
    phoneme <- substr(klattese, i, i)
    if(phoneme == '-') syllables=syllables+1
    if(phoneme == 'ˈ' && syllables >= 2) nonInitPrimStress = 1
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
  # WCM rules for word patterns 
  if (syllables > 2) phon_points=phon_points+1  # word patterns (1)
  if (nonInitPrimStress == 1) phon_points=phon_points+1  # word patterns (2)
  
  return(phon_points) 
}
