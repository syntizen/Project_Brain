# Remove Everything from the R directory
rm(list=ls(all=TRUE))

# Set Seed
set.seed(123)
library(pdftools)
library(readxl)
library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2) #creative visualizations
library(lubridate) # Char to Date
library(tidyr) # Kernal to the wordcloud
library(widyr) # Bigrams
library(igraph) # Create lexcicon gragh
library(ggraph) # Create Histogram
library (SnowballC) #word stemming algorithm


myFile <- file.choose()  # choose that file in csv format

txt <- pdf_text(myFile)
txt <- data_frame(txt)

my_stopwords <- data_frame(word = c(as.character(1:10),
                                    "1", "2", "3", "ee","4","5","6","7","8","9","10",
                                    "fa", "pse", "4", "6410",
                                    "145", "600", "t3", "l1", "50", "18", "17", "16", "15", "11", "00", "12", "60"))

txt %>%
  unnest_tokens(word, txt)%>%
  anti_join(stop_words)%>%
  anti_join(my_stopwords)%>%
  count(word, sort = TRUE)%>%
  mutate(word = reorder(word, n))%>%
  filter(n>=10)%>%
  top_n(20)%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()


txt[1]
#txt <- as_tibble(txt)

#pairwise_count(word1,word2 sort = TRUE, upper = FALSE)
#txt
word_pairs <- txt %>%
  pairwise_count(word, txt,n)
word_pairs
