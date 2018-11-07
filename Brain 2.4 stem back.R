# Remove Everything from the R directory
rm(list=ls(all=TRUE))

# Set Seed
set.seed(123)

#install.packages("wordcloud")
## Load Library
library(readxl) #read exls
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
library(stringr) # remove number


#######################################################################




## Load Data
# (Place Your Code for loading the data) 
myFile <- file.choose()  # choose that file in csv format
metadata_a <- read_xlsx(myFile) 
#View(Data)
names(metadata_a)



########################################################################

#metadata_a <- as.character(metadata_a)
#class(metadata_a)
#class(metadata_a$Department)

#title
#metadata_a$Title <- as.character(metadata_a$Title)
#class(metadata_a$Title)

metadata_a$id <- as.character(metadata_a$id)

class(metadata_a$id)

#andeavor_title <- data_frame(id = metadata_a$id, title = metadata_a$Title)
#andeavor_title

#department

#andeavor_dept <- data_frame(id = metadata_a$id, dept = metadata_a$Department) %>%
#  unnest(dept)
#andeavor_dept

#site
#andeavor_site <- data_frame(id = metadata_a$id, site = metadata_a$Site)
#andeavor_site %>%
#  select(site) %>%
#  sample_n(5)

#description

#metadata_a$Event.Description <- as.character(metadata_a$Description)
class(metadata_a$Description)
andeavor_desc <- data_frame(id = metadata_a$id, desc = metadata_a$Description) %>%
  unnest(desc)
andeavor_desc

#tokenize
#title

#andeavor_title <- andeavor_title %>%
#  unnest_tokens(word, title) %>%
#  anti_join(stop_words)


#andeavor_title
  
#Word count
#andeavor_title %>%
#  count(word, sort = TRUE)
andeavor_desc %>%
  count(word, sort = TRUE)

#My stop words
#undesirable_words = read.csv("~/Desktop/Andeavor/2. Raw_Dataset/2. raw_dataset-1/Mike_Syed_Text_Mining_Sub_Section/Data4.csv",stringsAsFactors = FALSE,header=FALSE)$V1
#undesirable_words
#undesirable_words <- as.list(undesirable_words)
#class(undesirable_words)

my_stopwords <- data_frame(word = c(as.character(1:10),
                                    "fa", "pse"))
#class(my_stopwords)


#word stem

#library (SnowballC)
#andeavor_title <- andeavor_title %>%
#  anti_join(my_stopwords)%>%
#  #mutate stem words with snowballC library
#  mutate(word = wordStem(word))
#  #filter(!word %in% undesirable_words) 
#  


andeavor_desc <- andeavor_desc %>%
  unnest_tokens(word, desc) %>%
  anti_join(stop_words)%>%
  filter(!str_detect(word, "^[0-9]*$")) %>% # remove numbers
  anti_join(my_stopwords)%>%
#  anti_join(url_words) %>%
  mutate(word = SnowballC::wordStem(word))  # word stem



#%>%
#  mutate(word = wordStem(word))

#  filter(!word %in% undesirable_words) 
  
andeavor_desc
#andeavor_title

#most comon departments
#andeavor_dept %>%
#  group_by(dept) %>%
#  count(sort = TRUE)

#lower upper case
#mutate() = toupper())

#Pairwise
#title_word_pairs <- andeavor_title %>%
#  pairwise_count(word, id, sort = TRUE, upper = FALSE)
#title_word_pairs

desc_word_pairs <- andeavor_desc %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE)
desc_word_pairs




#Plot of title
#set.seed(1234)
#title_word_pairs %>%
#  filter(n >= 20) %>%
#  graph_from_data_frame()%>%
#  ggraph(layout = "fr") +
#  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
#  geom_node_point(size = 2) +
#  geom_node_text(aes(label = name), repel = TRUE,
#                 point.padding = unit(0.1, "lines")) +
#  theme_void()



#####################################################
##################################

#plot the Bars
desc_word_united <- desc_word_pairs %>%
  unite(word, item1, item2, sep = " ")
#desc_word_united
desc_word_united %>%
  mutate(word = reorder(word, n))%>%
  filter(n>=10)%>%
  top_n(10)%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

##plot the Bars
#desc_word_united <- desc_word_pairs %>%
#  unite(word, item1, item2, sep = " ")
##desc_word_united
#desc_word_united %>%
#  mutate(word = reorder(word, n))%>%
#  filter(n>=10)%>%
#  top_n(20)%>%
#  ggplot(aes(word, n))+
#  geom_col()+
#  xlab(NULL)+
#  coord_flip()



#plot of description
set.seed(234)
desc_word_pairs %>%
  top_n(50)%>%
  filter(n >=5) %>%
  graph_from_data_frame()%>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.1, "lines")) +
  theme_void()


#library(wordcloud)
#wordcloud(words = desc_word_united$word, freq = desc_word_united$n,
#          max.words=100, random.order=FALSE, rot.per=0, 
#          colors=brewer.pal(8, "Dark2"))

################################
###########################################################







##Post processing

andeavor_desc_post <- andeavor_desc %>%
  unnest_tokens(word, desc) %>%
  anti_join(stop_words)%>%
  filter(!str_detect(word, "^[0-9]*$")) %>% # remove numbers
  anti_join(my_stopwords)
#%>%
#  anti_join(url_words) %>%
#  mutate(word = SnowballC::wordStem(word))  # word stem


desc_word_pairs_post <- andeavor_desc_post %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE)
desc_word_pairs_post


#plot of description
set.seed(234)
desc_word_pairs_post %>%
  top_n(50)%>%
  filter(n >=5) %>%
  graph_from_data_frame()%>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.1, "lines")) +
  theme_void()


write.csv(desc_word_pairs, "desc_word_pairs.csv", row.names = FALSE)
desc_word_pairs <- read.csv(file="desc_word_pairs.csv", header=TRUE, sep=",")
desc_word_pairs


###########################################################
##################################

#plot the Bars
desc_word_united <- desc_word_pairs %>%
  unite(word, item1, item2, sep = " ")
#desc_word_united
desc_word_united %>%
  mutate(word = reorder(word, n))%>%
  filter(n>=10)%>%
  top_n(10)%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

#plot the Bars
desc_word_united <- desc_word_pairs %>%
  unite(word, item1, item2, sep = " ")
#desc_word_united
desc_word_united %>%
  mutate(word = reorder(word, n))%>%
  filter(n>=10)%>%
  top_n(20)%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()



#plot of description
set.seed(234)
desc_word_pairs %>%
  top_n(50)%>%
  filter(n >=5) %>%
  graph_from_data_frame()%>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.1, "lines")) +
  theme_void()


#library(wordcloud)
#wordcloud(words = desc_word_united$word, freq = desc_word_united$n,
#          max.words=100, random.order=FALSE, rot.per=0, 
#          colors=brewer.pal(8, "Dark2"))

################################
###########################################################

#output matrix
write.csv(desc_word_united, "desc_word_united.csv", row.names = FALSE)



