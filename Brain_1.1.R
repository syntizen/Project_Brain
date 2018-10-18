library(pdftools)
library(tidytext)
library(dplyr)

myFile <- file.choose()  # choose that file in csv format

txt <- pdf_text(myFile)
txt <- data_frame(txt)
txt %>%
  unnest_tokens(word, txt)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)
