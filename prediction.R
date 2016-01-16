
#predict next word based on input



# function to clean input. Returns a character vector with individual tokens.
# Requires the presence of 

regex <- read.csv("data/clean/regex.csv", sep = ";", stringsAsFactors = FALSE)
abbrev <- read.csv("data/clean/abbreviations.csv", stringsAsFactors = FALSE)
twitter_ab <- read.csv("data/clean/twitter_abbreviations.csv", stringsAsFactors = FALSE)

profanity <- readLines("data/clean/profanity.txt")
replace <- readLines("data/clean/replace.txt")

load("data/lookup/dict.RData")
load("data/lookup/newdict.RData")
load("data/lookup/satzanfang.RData")
load("data/lookup/n2_matrix.RData")
load("data/lookup/n3_matrix.RData")
load("data/lookup/n4_matrix.RData")
load("data/lookup/n2.RData")
load("data/lookup/n3.RData")
load("data/lookup/n4.RData")



regex <- read.csv("data/clean/regex.csv", sep = ";", stringsAsFactors = FALSE)
abbrev <- read.csv("data/clean/abbreviations.csv", stringsAsFactors = FALSE)
twitter_ab <- read.csv("data/clean/twitter_abbreviations.csv", stringsAsFactors = FALSE)



clean_input <- function(x){
        
        
        x <- toLower(x)
        
        # replace numbers, ranges etc. 
        x <- stri_replace_all_regex(x,
                                    regex$pattern,
                                    regex$replacement,
                                    vectorize_all = FALSE)
        
        # replace common abbreviations
        x <- stri_replace_all_fixed(x,
                                    abbrev$pattern,
                                    abbrev$replacement,
                                    vectorize_all = FALSE)
        
        # replace twitter abbreviations
        x <- stri_replace_all_fixed(x,
                                    twitter_ab$pattern,
                                    twitter_ab$replacement,
                                    vectorize_all = FALSE)
        
        # tokenize input
        token <- tokenize(x, what = "word",
                          removeNumbers = FALSE,
                          removeSeparators = TRUE,
                          removeTwitter = TRUE,
                          removePunct = TRUE)[[1]]
        
        # replace tokens not in dictionary
        token[!(token %in% dict$match)] <- "<unk>"
        
        token
}


# function that returns n-gram from tokenized phrase

ngrammer <- function(x, n) {
        
        # collapse to ngram
        phrase <- paste(tail(x, n), collapse = " ")
        
        phrase
        
}



# function to calculate stupid backoff score
StupidBackoff <- function(alpha = 0.4, n4, n3, n2) {
        
        score = 0
        
        if (!is.na(n4)){
                score <- n4
        }
        
        if (!is.na(n3) & (alpha * n3) > score){
                score <- alpha * n3
                
        }
        
        if (!is.na(n2) & (alpha * alpha * n2) > score){
                score <- alpha * alpha * n2
                
        }
        
        score
        
}

# function that returns next word and frequency
nextword.stupid <- function(x, n) {
        
        # return ID of matched phrase
        n4_ID <- n4[n4$match == ngrammer(x, 3)]$ID_match
        
        n3_ID <- n3[n3$match == ngrammer(x, 2)]$ID_match
        
        n2_ID <- n2[n2$match == ngrammer(x, 1)]$ID_match
        
        # extract predictions from sparse matrix
        if (length(n4_ID != 0)) {
                
                n4_match <- data.table(summary(n4_matrix)) %>%
                            filter(i == n4_ID) %>%
                            select(j, x) %>%
                            arrange(desc(x))
                
        } else n4_match <- data.table(j = integer(),
                                      x = integer())
                
        if (length(n3_ID != 0)){
                
                n3_match <- data.table(summary(n3_matrix)) %>%
                            filter(i == n3_ID) %>%
                            select(j, x) %>%
                            arrange(desc(x))
                
        } else { n3_match <- data.table(j = integer(),
                                      x = integer())
        }
                
        if (length(n2_ID != 0)) {
                
                n2_match <- data.table(summary(n2_matrix)) %>%
                            filter(i == n2_ID) %>%
                            select(j, x) %>%
                            arrange(desc(x))
        } else n2_match <- data.table(j = integer(),
                                      x = integer())
        
        
        # create data frame with all predictions
        df <- merge(n4_match, n3_match, by = "j", all = TRUE)
        colnames(df) <- c("j", "n4_freq", "n3_freq")
        df <- merge(df, n2_match, by = "j", all = TRUE)
        colnames(df) <- c("pred", "n4_freq", "n3_freq", "n2_freq")
        
#         n4_match <- arrange(n4[n4$match == ngrammer(x, 3)], desc(freq)) %>%
#                 select(pred, freq) 
#         
#         n3_match <- arrange(n3[n3$match == ngrammer(x, 2)], desc(freq)) %>%
#                 select(pred, freq)
#         
#         n2_match <- arrange(n2[n2$match == ngrammer(x, 1)], desc(freq)) %>%
#                 select(pred, freq)
        
#         df <- merge(n4_match, n3_match, by = "pred", all = TRUE)
#         colnames(df) <- c("pred", "n4_freq", "n3_freq")
#         df <- merge(df, n2_match, by = "pred", all = TRUE)
#         colnames(df) <- c("pred", "n4_freq", "n3_freq", "n2_freq")
        
        # if no matches found, return df based on unigrams
        if (nrow(df) == 0 ) { df ## here comes code to return top unigrams
                
        # apply stupid backoff algorithm to all prediction frequencies to calculate score
        } else {
                df$score <- mapply(StupidBackoff, n4=df$n4_freq, n3=df$n3_freq,
                                   n2=df$n2_freq)
                df <- df %>% arrange(desc(score))
                
                head(df, n) # return specified number of predictions
                
        }       
        
}

nextword <- function(x, n) {
        
        # return ID of matched phrase
        n4_ID <- n4[n4$match == ngrammer(x, 3)]$ID_match
        
        n3_ID <- n3[n3$match == ngrammer(x, 2)]$ID_match
        
        n2_ID <- n2[n2$match == ngrammer(x, 1)]$ID_match
        
        # extract predictions from sparse matrix
        if (length(n4_ID != 0)) {
                
                n4_match <- data.table(summary(n4_matrix)) %>%
                        filter(i == n4_ID) %>%
                        select(j, x) %>%
                        arrange(desc(x))
                
        } else n4_match <- data.table(j = integer(),
                                      x = integer())
        
        if (length(n3_ID != 0)){
                
                n3_match <- data.table(summary(n3_matrix)) %>%
                        filter(i == n3_ID) %>%
                        select(j, x) %>%
                        arrange(desc(x))
                
        } else { n3_match <- data.table(j = integer(),
                                        x = integer())
        }
        
        if (length(n2_ID != 0)) {
                
                n2_match <- data.table(summary(n2_matrix)) %>%
                        filter(i == n2_ID) %>%
                        select(j, x) %>%
                        arrange(desc(x))
        } else n2_match <- data.table(j = integer(),
                                      x = integer())
        
        
        # create data frame with all predictions
        df <- merge(n4_match, n3_match, by = "j", all = TRUE)
        colnames(df) <- c("j", "n4_freq", "n3_freq")
        df <- merge(df, n2_match, by = "j", all = TRUE)
        colnames(df) <- c("pred", "n4_freq", "n3_freq", "n2_freq")
        
        #         n4_match <- arrange(n4[n4$match == ngrammer(x, 3)], desc(freq)) %>%
        #                 select(pred, freq) 
        #         
        #         n3_match <- arrange(n3[n3$match == ngrammer(x, 2)], desc(freq)) %>%
        #                 select(pred, freq)
        #         
        #         n2_match <- arrange(n2[n2$match == ngrammer(x, 1)], desc(freq)) %>%
        #                 select(pred, freq)
        
        #         df <- merge(n4_match, n3_match, by = "pred", all = TRUE)
        #         colnames(df) <- c("pred", "n4_freq", "n3_freq")
        #         df <- merge(df, n2_match, by = "pred", all = TRUE)
        #         colnames(df) <- c("pred", "n4_freq", "n3_freq", "n2_freq")
        
        # if no matches found, return df based on unigrams
        if (nrow(df) == 0 ) { df ## here comes code to return top unigrams
                
                # apply stupid backoff algorithm to all prediction frequencies to calculate score
        } else {df <- df %>% arrange(desc(n4_freq), desc(n3_freq), desc(n2_freq))
                
                head(df, n) # return specified number of predictions
                
        }       
        
}

# function that returns next word

wortwahl <- function(input, resultlist = 30, show = 10, alpha = 0.4) {
        
        require(dplyr)
        
        phrase <- clean_input(input)
        
        df <- nextword(phrase, n = resultlist)
        
        if (nrow(df) == 0) {
                
                head(satzanfang, show)       
                
        } else {

        # filter out profanity and replacements
        df <- cbind(newdict[newdict$ID %in% df$pred], n4_freq = df$n4_freq,
                    n3_freq = df$n3_freq, n2_freq = df$n2_freq)
        df <- df %>%
                filter(!(pred %in% profanity)) %>%
                filter(!(pred %in% replace))
        
        
                if (nrow(df) == 0) {
                
                        head(satzanfang, show)
                
                }
        
                else head(df, show)
        }
        
}

wortwahl.stupid <- function(input, resultlist = 30, show = 10, alpha = 0.4) {
        
        require(dplyr)
        
        phrase <- clean_input(input)
        
        df <- nextword.stupid(phrase, n = resultlist)
        
        if (nrow(df) == 0) {
                
                head(satzanfang, show)       
                
        } else {
                
                # filter out profanity and replacements
                df <- cbind(newdict[newdict$ID %in% df$pred], score= df$score)
                df <- df %>%
                        filter(!(pred %in% profanity)) %>%
                        filter(!(pred %in% replace)) %>%
                        select(pred, score)
                
                
                if (nrow(df) == 0) {
                        
                        head(satzanfang, show)
                        
                }
                
                else head(df, show)
        }
        
}

