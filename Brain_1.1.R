library(pdftools)
library(tidytext)
library(dplyr)
txt <- pdf_text("filpath")
txt <- data_frame(txt)
txt %>%
  unnest_tokens(word, txt)