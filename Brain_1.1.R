library(pdftools)# read pdf need poppler install
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
#txt <- data_frame(txt)

txt <- str_split(txt, '\n')

txt[1]

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

txt %>%
pairwise_count(word, sort = TRUE, upper = FALSE)
