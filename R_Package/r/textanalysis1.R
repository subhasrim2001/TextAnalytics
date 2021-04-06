#working directory - change to file location
getwd()

f_name <- "data/MA.txt"
#read text
readLines(f_name)

#check structure to parse
str(readLines(f_name))

#make into one single element separated by spaces 
#instead of lines 
text <- paste(readLines(f_name), collapse=" ")

#look at contents of file
text

#text cleaning
#remove punctuation
text2 <- gsub(pattern = "\\W", replace=" ", text)

#remove numbers
text3 <- gsub(pattern="\\d", replace=" ", text2)

#make all to lowercase - case insensitive
text4 <- tolower(text3)


#install text mining package
install.packages("tm")

#load package
library(tm)

#uninteresting words
stopwords(kind = "en")

#remove uninteresting words
text5 <- removeWords(text4, stopwords())


#remove single letter words
text6 <- gsub(pattern="\\b[A-z]\\b{1}", replace=" ", text5)

#remove white spaces
text7 <- stripWhitespace(text6)

#sentiment analysis packages
install.packages("stringr")
install.packages("wordcloud")

#load them
library(stringr)
library(wordcloud)

#split the element into words using whitespace
bag_of_words <- str_split(text7, pattern="\\s+")

class(bag_of_words)
#change from list to character vector
bag_of_words <- unlist(bag_of_words)
class(bag_of_words)

#checking
bag_of_words
str(bag_of_words)

#match positive and negative words and find count
getwd()
#scan the words
poswords <- scan('data/positive.txt', what='character', comment.char=';')
negwords <- scan('data/negative.txt', what='character', comment.char=';')

#display
poswords
negwords
bag_of_words

#match the words
match(bag_of_words, poswords)
!is.na(match(bag_of_words, poswords))
p_sum = sum(!is.na(match(bag_of_words, poswords)))

match(bag_of_words, negwords)
!is.na(match(bag_of_words, negwords))
n_sum = sum(!is.na(match(bag_of_words, negwords)))

#get sentiment score
sentiment_score <- p_sum - n_sum

wordcloud(bag_of_words)
wordcloud(bag_of_words, min.freq=2)
wordcloud(bag_of_words, min.freq=2, random.order=FALSE)
wordcloud(bag_of_words, min.freq=2, random.order=FALSE, color=rainbow(7), scale=c(3, 0.2))

#doing the same for a corpus

file.choose()
folder <- "C:\\Users\\venka\\Documents\\GitHub\\TextAnalytics\\R_Package\\data\\Corpus"
#pick only text documents
file_list <- list.files(path = folder, pattern = "*.txt")
file_list <- paste(folder, "\\", file_list, sep="")

#process the corpus together
a <- lapply(file_list, FUN=readLines)
corpus <- lapply(a, FUN=paste, collapse=" ")

#clean up the corpus - whitespaces, digits
corpus2 <- gsub(pattern="\\W", replace=" ", corpus)
corpus3 <- gsub(pattern="\\d", replace=" ", corpus2)
corpus4 <- tolower(corpus3)
corpus5 <- removeWords(corpus4, stopwords())
corpus6 <- gsub(pattern="\\b[A-z]\\b{1}", replace=" ", corpus5)
corpus7 <- stripWhitespace(corpus6)

wordcloud(corpus7, random.order = FALSE, col=rainbow(7))

#seperated wordclouds to compare corpuses
corpus8 <- Corpus(VectorSource(corpus7))
corpus8

#open a new box
x11()
#matrix with rows - words, columns - documents, values - frequency
tdm <- TermDocumentMatrix(corpus8)
#to increase printing limit
options(max.print=999999)
m <- as.matrix(tdm)
colnames(m) <- c("speech1", "speech2", "speech3", "speech4", "speech5")
comparison.cloud(m)

#sentiment analysis of corpus
#split wherever there is a space
bags_of_words <- str_split(corpus7, pattern="\\s+")
#x is a document
p_score <- lapply(bags_of_words, function(x){sum(!is.na(match(x, poswords)))})
n_score <- lapply(bags_of_words, function(x){sum(!is.na(match(x, negwords)))})
sentiment_score <- lapply(bags_of_words, function(x){sum(!is.na(match(x, poswords))) - sum(!is.na(match(x, negwords)))})
sentiment_score <- unlist(sentiment_score)

#compare between the scores and show distribution
mean(sentiment_score)
sd(sentiment_score)
x11()
hist(sentiment_score)
