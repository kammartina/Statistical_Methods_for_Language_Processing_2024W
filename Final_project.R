########################################
# 1. GUTENBERG LIBRARY
########################################

library(gutenbergr) # for loading the book library
library(tidytext) # for data manipulation
library(dplyr) # for data manipulation

my_mirror<- "http://mirrors.xmission.com/gutenberg"

df<-gutenberg_metadata

# finding a book nearest to my FIRST name
unique(df$author)[startsWith(unique(df$author), "Tw")]
gutenberg_works(author == "Twain, Mark")

# saving the particular book
AdvHuc <- gutenberg_download(79, mirror = my_mirror)

# Total words and unique words
total_words <- nrow(words_AdvHuc)
unique_words <- n_distinct(words_AdvHuc$words)
lexical_diversity <- unique_words / total_words
print(c("Total Words" = total_words, "Unique Words" = unique_words, "Lexical Diversity" = lexical_diversity))


########################################
# 2. WORD AND BIGRAM COUNTS
########################################

unnest_tokens(AdvHuc, words, text)

# creating data frame with the list of all words
words_AdvHuc<-unnest_tokens(AdvHuc, words, text)

# counting which words occur how many times
countwords<-count(words_AdvHuc,words, sort=TRUE)

# counting bigrams
bigram_AdvHuc<-unnest_tokens(AdvHuc, words, text, token = "ngrams", n=2)
countbigrams<-count(bigram_AdvHuc,words, sort=TRUE)

# erasing the empty row
countbigrams<-countbigrams[-1,]

# top 20 most frequent words (excluding stop words, if applicable)
top_words <- countwords %>%
  slice_max(n, n = 20)

# plotting the most frequent words
library(ggplot2)
ggplot(top_words, aes(x = reorder(words, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 20 Most Frequent Words", x = "Words", y = "Frequency")

# top 20 most frequent bi-grams
top_bigrams <- countbigrams %>%
  slice_max(n, n = 20)

# plotting the most frequent bi-grams
ggplot(top_bigrams, aes(x = reorder(words, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 20 Most Frequent Bi-Grams", x = "Bi-Grams", y = "Frequency")


########################################
# 3. DEPENDENCE OF WORDS
########################################

# testing for dependence of bi-grams/phrases
countbigrams<-count(bigram_AdvHuc,words,sort=TRUE)

# erasing the empty row
countbigrams<-countbigrams[-1,]

# chosen phrase: "looking for"

# frequency distribution:
countbigrams[startsWith(countbigrams$words,"looking for"),] # how many times "looking for"
countbigrams[startsWith(countbigrams$words,"looking "),] # how many times "looking"and something else
countbigrams[endsWith(countbigrams$words," for"),] # how many times something else and "for"

# 1. counting "looking for"
looking_for_count <- sum(countbigrams[startsWith(countbigrams$words, "looking for"), ]$n)
# 2. "looking [something else]"
looking_other_count <- sum(countbigrams[startsWith(countbigrams$words, "looking "), ]$n) - looking_for_count
# 3. "[something else] for"
other_for_count <- sum(countbigrams[endsWith(countbigrams$words, " for"), ]$n) - looking_for_count
# 4. all other bi-grams
neither_count <- sum(countbigrams$n) - looking_for_count - looking_other_count - other_for_count

# testing the type:
subset_data <- countbigrams[startsWith(countbigrams$words, "looking for"), ]
print(subset_data)  # inspecting the rows being selected

# creating contingency table
contingency_table <- matrix(c(looking_for_count, looking_other_count,
                              other_for_count, neither_count),
                            nrow = 2, byrow = TRUE,
                            dimnames = list(c("looking", "not looking"),
                                            c("for", "not for")))
print(contingency_table)

# visualizing with a simple mosaic plot
mosaicplot(contingency_table)

# performing chi-square test
chisq_result <- chisq.test(contingency_table)

## Steps repeated for another bi-gram: "fuzzy cloud"
# frequency distributions
fuzzy_cloud_count <- sum(countbigrams[startsWith(countbigrams$words, "fuzzy cloud"), ]$n)
fuzzy_other_count <- sum(countbigrams[startsWith(countbigrams$words, "fuzzy "), ]$n) - fuzzy_cloud_count
other_cloud_count <- sum(countbigrams[endsWith(countbigrams$words, " cloud"), ]$n) - fuzzy_cloud_count
neither_count <- sum(countbigrams$n) - fuzzy_cloud_count - fuzzy_other_count - other_cloud_count

# contingency table
contingency_table_fuzzy_cloud <- matrix(c(fuzzy_cloud_count, fuzzy_other_count,
                                          other_cloud_count, neither_count),
                                        nrow = 2, byrow = TRUE,
                                        dimnames = list(c("fuzzy", "not fuzzy"),
                                                        c("cloud", "not cloud")))
print(contingency_table_fuzzy_cloud)

# chi-square test
chisq_result_fuzzy_cloud <- chisq.test(contingency_table_fuzzy_cloud)
print(chisq_result_fuzzy_cloud)

# mosaic plot
mosaicplot(contingency_table_fuzzy_cloud)


########################################
# 4. HYPOTHESIS TEST AND CHI-SQUARE EXPLAINED
########################################

# for explanation see the PDF file

########################################
# 5. COMPUTING ENTROPY
########################################

# computing the overall entropy for each thousands words in the text --> applying for-loop
entropy<-c() # all entropies will be stored in one big vector
for(i in 1:214)
{
  entr<-words_AdvHuc[(i*1000+1):(i*1000-1000),2]
  char<-unnest_tokens(entr,token,words, token="characters")
  df.char<-as.data.frame(count(char,token,sort=TRUE))
  df.char$relfreq<-df.char$n/sum(df.char$n)
  df.char$ent<-df.char$relfreq*log2(df.char$relfreq)
  entropy<- c(entropy, - sum(df.char$ent)) # (empty) vector concatenated with the current entropy
}

entropy

plot(entropy)


########################################
# 6. ENTROPY OF A RANDOM VARIABLE EXPLAINED
########################################

# for explanation see the PDF file


########################################
# 7. COMPUTING CONFIDENCE INTERVAL
########################################

# entropy scores are already in a vector named 'entropy'
# computing the mean, standard deviation, and sample size
mean_entropy <- mean(entropy)
std_dev_entropy <- sd(entropy)
sample_size <- length(entropy)

# computing the standard error
standard_error <- std_dev_entropy / sqrt(sample_size)

# finding the critical t-value for a 95% confidence level
t_value <- qt(0.975, df = sample_size - 1)

# computing the confidence interval
lower_bound <- mean_entropy - t_value * standard_error
upper_bound <- mean_entropy + t_value * standard_error

cat("95% Confidence Interval for Entropy:\n")
cat("Mean Entropy:", mean_entropy, "\n")
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")

# plotting a histogram of entropy values
hist(entropy, breaks = 20, col = "lightblue", main = "Entropy Values with Confidence Interval", xlab = "Entropy")

# adding confidence interval as a line
abline(v = mean_entropy, col = "blue", lwd = 2)  # Mean entropy
abline(v = lower_bound, col = "red", lty = 2)  # Lower bound
abline(v = upper_bound, col = "red", lty = 2)  # Upper bound


########################################
# 8. CONFIDENCE INTERVAL EXPLAINED
########################################

# for explanation see the PDF file


########################################
# 9. NAIVE BAYES CLASSIFIER
########################################

# dividing the text AdvHuc (30958) into 4 classes
30958/4
AdvHuc1<-AdvHuc[1:7739,]
AdvHuc2<-AdvHuc[(7739+1):(2*7739),]
AdvHuc3<-AdvHuc[(2*7739+1):(3*7739),]
AdvHuc4<-AdvHuc[(3*7739+1):30958,]

# combining all sections into a list
sections <- list(AdvHuc1, AdvHuc2, AdvHuc3, AdvHuc4)

# defining the sentence to classify (from AdvHuc3)
sentence <- c("Dobbs shuffled over to the chair and fell into it")

# preprocessing and tokenizing the text
library(tm)  # library for text preprocessing

preprocess_text <- function(text) {
  # converting to lowercase and splitting into words
  words <- tolower(unlist(strsplit(paste(text, collapse = " "), "\\W+")))
  return(words[words != ""])  # removing empty strings
}

# preprocessing all sections
tokens_sections <- lapply(sections, preprocess_text)

# preprocessing the sentence
sentence_tokens <- preprocess_text(sentence)

# computing word frequencies for each section
compute_word_freq <- function(tokens) {
  table(tokens) / length(tokens)  # relative frequencies
}

freq_sections <- lapply(tokens_sections, compute_word_freq)

# calculating Naive Bayes probabilities for the sentence
compute_sentence_prob <- function(freq_section, sentence_tokens) {
  # for each word in the sentence, calculate its frequency in the section
  probs <- sapply(sentence_tokens, function(word) {
    if (word %in% names(freq_section)) {
      return(freq_section[word])
    } else {
      return(1e-6)  # adding smoothing for unseen words
    }
  })
  return(prod(probs))  # multiplying probabilities for all words
}

# computing probabilities for each section
probs <- sapply(freq_sections, compute_sentence_prob, sentence_tokens = sentence_tokens)

# normalizing the probabilities and finding the most likely section
probs_normalized <- probs / sum(probs)
predicted_section <- which.max(probs_normalized)

# results:
cat("Sentence belongs to section:", predicted_section, "\n")
cat("Probabilities for each section:\n")
print(probs_normalized)

# plotting the results:
# defining sections and probabilities
sections <- c("Section 1", "Section 2", "Section 3", "Section 4")
probabilities <- probs_normalized

# creating the dot plot
plot(probabilities, 
     type = "o", 
     pch = 16, 
     col = "blue", 
     xaxt = "n", 
     main = "Normalized Probabilities for Each Section",
     xlab = "Sections", 
     ylab = "Probability", 
     ylim = c(0, 1))

# adding section labels to the x-axis
axis(1, at = 1:4, labels = sections)

# highlighting the section with the highest probability
points(which.max(probabilities), max(probabilities), col = "red", pch = 16, cex = 1.5)
text(which.max(probabilities), max(probabilities), labels = "Highest", pos = 3, col = "red")