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

# list for each important category 
engl_voiceless_cons <- list(c("C","f","h","k","p","s","S","t","T")) #should h be included here? 
engl_voiced_cons <- list(c("b","d","D","g","J","m","n","G","v","z","Z")) #should liquids and semivowels be included or no?
engl_fricatives <- list(c("D","f","s","S","T","v","z","Z"))
engl_affricates <- list(c("C","J"))
engl_velars <- list(c("k","g","G"))
engl_liquids <- list(c("l","r"))
engl_rhotic_vowels <- list(c("X-R")) #i think this is correct but unsure  

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
  #sample<-toupper(sample)
  text_df<-tibble(text=sample)  #tibble is a simple data frame
  text_df <-text_df%>%
    unnest_tokens(word, text) #this breaks the column into words, one token(word) per row
  ##text_df<- text_df %>%
    #anti_join(stop_words)
  tibbletest <-tibble(mrc$word, mrc$phon) #looks like setting up a variable that includes only the word and phon columns from MRC
  #stresstest<-tibble(mrc$word, mrc$stress)
  tibbletest <- na.omit(tibbletest) #na.omit gets rid of cases where the value is na
  concrete <-na.omit(tibble(mrc$word, mrc$conc)) # this creates a variable of concreteness which does not produce any output
  
  tibbletest <- na.omit(tibbletest) # i don't understand how this is different from two lines above.
  
  #which(toupper(text_df$word[100])==tibbletest$`mrc$word`, arr.ind = TRUE)
  #i=1
  for (i in 1:nrow(text_df)){
    r<-which(toupper(text_df$word[i])==tibbletest$`mrc$word`, arr.ind = TRUE) # toupper goes to uppercase. why are we doing this?
    # i guess that line defines r as which words have a value that can be found in the MRC file.
    
    phonetic <- paste(phonetic, tibbletest$`mrc$phon`[r[1]]) # not really sure what this does. from a test, it looks like it is one variable, which is the phonetic transcription of each word
    
    
   ## b<-which(toupper(text_df$word[i])==tibbletest$`mrc$stress`, arr.ind = TRUE)
    #stress <- paste(stress, stresstest$`mrc$stress`[r[1]])
    
    #stess<-paste(stress, tibbletest$`mrc$phon`[r[1]]))
    #i=i+1
    
  ##    it looks to me as if version 1.1 does not count points for non-initial word stress, or for words with > 2 syllables. I don't see that it would be hard to do. In the mrc word database, there is a column for "stress". The coding is incomplete (i.e. not all items have an entry for stress). When present, the coding looks like a series of digits, each representing a syllable. If the digit is 1, then that syllable has primary stress, if 2, secondary stress, if 0 it is unstressed. For example incomprehensible would be 10200. Actually, in my opinion, that word could also be 20100, but the database says otherwise
    
    # Stoel-Gammon defines non-initial word stress as "Productions with stress on any syllable but the first receive 1 point." I think this means if any syllable from 2 through N has anything but 0, it receives a point.
    
  }
  phonetic<-gsub("NA", "", phonetic) #substitute NA with blank
  phonetic<-str_split(string=phonetic, pattern=" ") #i think this splits the string whenever there is a space
  phonetic<-as.data.frame(phonetic, stringsAsFactors=FALSE) #makes this into a dataframe
  
  tibbletest$`mrc$phon`[30407]
  points<-0
  #phonetic<-phonetic$c......wel....De.....Iz....eI....m.n....De.....wID...........
  for (j in 1:nrow(phonetic)){
    
    
    # BEGIN PSEUDOCODE of new solution 
    
    if(polysyll == 1) points=points+1  #word patterns (1)
    if(nonInitialPrimaryStress == 1) points=points+1  #word patterns (2)
    if(word[len-1] in engl_voiced_cons, engl_voiceless_cons) points=points+1  #syllable structures (1)
    
    # for loop to find consonant clusters and sound classes 
    int len = length of word 
    for(int i = 0; i < len; i++) {
      if(word[i] in engl_voiced_cons or in engl_voiceless_cons) {
        int j = i
        boolean is_cluster = false 
        while j < len {
          if (word[j+1] in engl_voiced_cons or engl_voiceless_cons) j++, is_cluster = true
          else break
        }
        if(is_cluster) then points=points+1  #syllable structures (2)
      }
      if word[i] in velar then points=points+1  #sound classes (1)
      if word[i] in liquid then points=points+1  #sound classes (2)
      if word[i] in syllabic_liquid then points=points+1  #sound classes (2)
      if word[i] in rhotic_vowel then points=points+1  #sound classes (2)
      if word[i] in engl_fricatives or engl_affricates then points=points+1  #sound classes (3)
      if word[i] in (engl_fricatives or engl_affricates) and (engl_voiced_cons) then points=points+1  #sound classes (4)
    }
    
    # END PSEUDOCODE of new solution 
    
    
    
    # BELOW is the points system implemented in version 1.1
    
    #if more than 1 syllalbe, add 1 point- 
    
    #AJ: actually we should be adding a point if more than (> 2 syllables)- Lindsey how do we modify? I think this code is saying if there is one slash, that means we have two syllables. I think N slashes corresponds to N-1 syllables. so we need to say > 1, so only words with 3 or more syllables get this. 
    
    if(str_count(phonetic[j,], "/")>0){
      points=points+1
    }
    
    # what about word final consonant? That should be a point. Lindsey, any ideas?
    
    #add a point for each velar consonant
    points=points+str_count(phonetic[j,], "k")
    points=points+str_count(phonetic[j,], "g")
    
    #add a point for each liquid
    points=points+str_count(phonetic[j,], "l")
    points=points+str_count(phonetic[j,], "r")
    
    #Need to add fricative or affricate
    points=points+str_count(phonetic[j,], "v")
    points=points+str_count(phonetic[j,], "z")
    #points=points+str_count(phonetic[j,], "Q") #zh
    points=points+str_count(phonetic[j,], "Z") #zh
    points=points+str_count(phonetic[j,], "f")
    points=points+str_count(phonetic[j,], "s")
    points=points+str_count(phonetic[j,], "S")
    
    #Need to add voiced fricatve or affricate
    points=points+str_count(phonetic[j,], "v")
    points=points+str_count(phonetic[j,], "z")
    #points=points+str_count(phonetic[j,], "Q") #zh In the mrc2.csv database, it looks like Q is added in at the end of a transcription as a marker of non-initial stress
    points=points+str_count(phonetic[j,], "Z") #zh
    

    # AJ: I think there should be an easier/more elegant way to count consonant clusters. 
    
    # Consonant clusters approximation
    #b-blends
    points=points+str_count(phonetic[j,], "bd")+str_count(phonetic[j,], "bl")+str_count(phonetic[j,], "br")+str_count(phonetic[j,], "bz")
    #d blends
    points=points+str_count(phonetic[j,], "dr")+str_count(phonetic[j,], "dw")+str_count(phonetic[j,], "dz")
    points=points+str_count(phonetic[j,], "dr")+str_count(phonetic[j,], "dw")+str_count(phonetic[j,], "dz")
    # f blends
    points=points+str_count(phonetic[j,], "fl")+str_count(phonetic[j,], "fr")+str_count(phonetic[j,], "fs")+str_count(phonetic[j,], "ft")
    #g blends
    points=points+str_count(phonetic[j,], "gd")+str_count(phonetic[j,], "gl")+str_count(phonetic[j,], "gr")+str_count(phonetic[j,], "gw")+str_count(phonetic[j,], "gz")
    # Z blend
    points=points+str_count(phonetic[j,], "Zd")+str_count(phonetic[j,], "dZd")
    # k blends
    points=points+str_count(phonetic[j,], "kl")+str_count(phonetic[j,], "kr")+str_count(phonetic[j,], "ks")+str_count(phonetic[j,], "kt")+str_count(phonetic[j,], "kw")
    
    #l blends
    points=points+str_count(phonetic[j,], "lb")+str_count(phonetic[j,], "ltS")+str_count(phonetic[j,], "ltSt")+str_count(phonetic[j,], "ld")+str_count(phonetic[j,], "lf")+str_count(phonetic[j,], "ldZ")+str_count(phonetic[j,], "ldZd")+str_count(phonetic[j,], "lk")+str_count(phonetic[j,], "lkt")+str_count(phonetic[j,], "lm")+str_count(phonetic[j,], "lp")+str_count(phonetic[j,], "lpt")+str_count(phonetic[j,], "ls")+str_count(phonetic[j,], "ltf")+str_count(phonetic[j,], "lv")+str_count(phonetic[j,], "lz")+str_count(phonetic[j,], "lvd")
    
    # m blends
    points=points+str_count(phonetic[j,], "md")+str_count(phonetic[j,],"mf")+str_count(phonetic[j,], "mft")+str_count(phonetic[j,], "mp")+str_count(phonetic[j,], "mpt")+str_count(phonetic[j,], "mz")
    
    # n blends
    points=points+str_count(phonetic[j,], "ntS")+str_count(phonetic[j,],"ntSt")+str_count(phonetic[j,], "ns")+str_count(phonetic[j,], "nd")+str_count(phonetic[j,], "ndZ")+str_count(phonetic[j,], "ndZd")+str_count(phonetic[j,], "nt")+str_count(phonetic[j,], "nz")
    
    #velar nasal blends
  #  points=points+str_count(phonetic[j,], "9d")+str_count(phonetic[j,], "9k")+str_count(phonetic[j,], "9z")
    points=points+str_count(phonetic[j,], "Nd")+str_count(phonetic[j,], "Nk")+str_count(phonetic[j,], "Nz")
    
    #p blends
    points=points+str_count(phonetic[j,], "ps")+str_count(phonetic[j,], "pt")
    
    #s blends
    points=points+str_count(phonetic[j,], "sf")+str_count(phonetic[j,], "sk")+str_count(phonetic[j,], "skr")+str_count(phonetic[j,], "skw")+str_count(phonetic[j,], "sl")+str_count(phonetic[j,], "sm")+str_count(phonetic[j,], "sn")+str_count(phonetic[j,], "sp")+str_count(phonetic[j,], "spl")+str_count(phonetic[j,], "spr")+str_count(phonetic[j,], "st")+str_count(phonetic[j,], "str")+str_count(phonetic[j,], "sw")
    
    #T blends
    points=points+str_count(phonetic[j,], "Tr")+str_count(phonetic[j,], "Ts")+str_count(phonetic[j,], "Tw")
    
    #S blends
    points=points+str_count(phonetic[j,], "Sr")+str_count(phonetic[j,], "St")
    
    #D blends
    points=points+str_count(phonetic[j,], "Dd")+str_count(phonetic[j,], "Dz")
    
    #p blends
    points=points+str_count(phonetic[j,], "pl")+str_count(phonetic[j,], "pr")+str_count(phonetic[j,], "ps")+str_count(phonetic[j,], "pt")
    
    #v blends
    points=points+str_count(phonetic[j,], "vd")+str_count(phonetic[j,], "vz")
    
    #z blends
    points=points+str_count(phonetic[j,], "zd")
    #syllable stress
    
  }
  #phonetic$c..........n.U....D.t....It....ne.v.....keIm....b.k....aI....maIt...!=""
  #enominator <- sum(phonetic$c..........n.U....D.t....It....ne.v.....keIm....b.k....aI....maIt...!="")
  phonetic[!apply(phonetic == "", 1, all),]
  
  score=points/nrow(phonetic)
  #phonetic<-as.data.frame(phonetic)
  #phonetic[!is.na(phonetic$c......wel....De.....Iz....eI....m.n....De.....wID....NA....NA...),]
 #for (i in text_df){
    #print(which(i==mrcword, arr.ind = TRUE))}
 # for (i in nrow(text_df)){
  #  print((which(text_df$word[i]==tibbletest$`mrc$word`, arr.ind = TRUE)))
  #}
  data<-cbind(fileName, score, points, nrow(phonetic))
  write.table(data, file="WCD_data.csv", append=TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
  
}
#fiat mihi secundum verbum tuum

