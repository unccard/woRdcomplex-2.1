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

rescueContraction <- function(contraction, word, klatt, bare_klatt) {  # format contractions for display in output file

  engl_voiceless_cons <- c("C","f","h","k","p","s","S","t","T")
  final_phoneme <- substr(klatt, str_length(klatt), str_length(klatt))  # last sound in klatt before contraction
  
  # add the correct pronunciation of the contraction to the klattese, and format english 
  if(contraction == "s") {
    word <- paste(word, "'s", sep="")
    if(!(final_phoneme %in% engl_voiceless_cons)) {  # If prior sound voiced, the 's contraction becomes voiced
      klatt <- paste(klatt, "z", sep="")
      bare_klatt <- paste(bare_klatt, "z", sep="")
    } else {
      klatt <- paste(klatt, "s", sep = "")
      bare_klatt <- paste(bare_klatt, "s", sep="")
    }
  } else if(contraction == "d") {
    word <- paste(word, "'d", sep="")
    klatt <- paste(klatt, "d", sep="")
    bare_klatt <- paste(bare_klatt, "d", sep="")
  } else if(contraction == "t") {
    word <- paste(word, "'t", sep="")
    klatt <- paste(klatt, "t", sep="")
    bare_klatt <- paste(bare_klatt, "t", sep="")
  } else if(contraction == "m") {
    word <- paste(word, "'m", sep="")
    klatt <- paste(klatt, "m", sep="")
    bare_klatt <- paste(bare_klatt, "m", sep="")
  } else if(contraction == "ve") {
    word <- paste(word, "'ve", sep="")
    klatt <- paste(klatt, "v", sep="")
    bare_klatt <- paste(bare_klatt, "v", sep="")
  } else if(contraction == "re") {
    word <- paste(word, "'re", sep="")
    klatt <- paste(klatt, "X", sep="")
    bare_klatt <- paste(bare_klatt, "X", sep="")
  }
  else {  # contraction is "ll"
    word <- paste(word, "'ll", sep="")
    klatt <- paste(klatt, "L", sep="")  
    bare_klatt <- paste(bare_klatt, "L", sep="")
  }
  
  return(c(word, klatt, bare_klatt))
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
