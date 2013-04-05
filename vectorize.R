vectorize <- function(inputString){
  #lower case
  inputString <- tolower(inputString)
  arf <- sapply(inputString, tokenize, language = "en")
  arfo <- 'a'
  for (i in 1:length(arf)){
    arfo <- rbind(arfo,sapply(arf[[i]], SnowballStemmer))
    end    
  }  
  
}