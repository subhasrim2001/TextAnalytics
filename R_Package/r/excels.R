# Text analytics using Excel data set - SMS text messages (spam.csv)
# Extracting meaningful words from a huge data set

install.packages("lattice")
install.packages("caret")
install.packages("quanteda")

# Load up the .CSV data and view 
ham_or_spam <- read.csv("C:\\Users\\venka\\Documents\\GitHub\\TextAnalytics\\R_Package\\data\\spam.csv")
str(ham_or_spam)
View(ham_or_spam)

# Extract the required columns from the data set and rename them
ham_or_spam <- ham_or_spam[, 1:2]
names(ham_or_spam) <- c("Label", "Text")
View(ham_or_spam)

# Convert class label into a factor - categorise data and store it as levels
ham_or_spam$Label <- as.factor(ham_or_spam$Label)

# Distribution of class labels (i.e., ham and spam)
prop.table(table(ham_or_spam$Label))

# Distribution of text lengths of the SMS 
# messages by adding a new feature for the length of each message.
ham_or_spam$TextLength <- nchar(ham_or_spam$Text)

library(lattice)

# Plot a graph (histogram) with TextLength as x axis and the Percent of total as y axis
histogram(~TextLength, data = ham_or_spam)

# Classification and Regression
library(caret)

# Use caret to create a 70%/30% stratified split. 
# Set the random seed.
# Stratified split: train/test split that ensures the correct ham/spam class label proportions 
set.seed(50000)
indexes <- createDataPartition(ham_or_spam$Label, times = 1,
                               p = 0.7, list = FALSE)

train <- ham_or_spam[indexes,]
test <- ham_or_spam[-indexes,]

# Verify proportions - must be same
prop.table(table(train$Label))
prop.table(table(test$Label))

library(quanteda)

# Row - document and Column - Token
# Extracting words
# Tokenize SMS text messages
# Criteria: Tokenize into words by removing digits, punctuation characters, symbols and hyphens
train.tokens <- tokens(train$Text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, split_hyphens = TRUE)

# Check by displaying
train.tokens[[250]]

# Lower case the tokens
train.tokens <- tokens_tolower(train.tokens)
train.tokens[[250]]

# quanteda's built-in stopword list: Remove words like "a," "and," "but," "how," "or," and "what"
train.tokens <- tokens_select(train.tokens, stopwords(), 
                              selection = "remove")
train.tokens[[250]]

# Representation of textual data - Document Frequency(Feature) Matrix
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)

train.tokens.matrix <- as.matrix(train.tokens.dfm)
View(train.tokens.matrix[1:20, 1:100])

# Combine by rows and cols
train.tokens.df <- cbind(Label = train$Label, convert(train.tokens.dfm, to = "data.frame")) 

# Make syntactically valid names out of character vectors
names(train.tokens.df) <- make.names(names(train.tokens.df))