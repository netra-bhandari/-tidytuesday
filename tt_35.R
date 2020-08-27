library(dpyr)
library(tidyverse)
library(ggplot2)

# #tidy tuesday wk 35 "chopped" -------------------------------------------

# accessing the data ------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2020-08-25')
tuesdata <- tidytuesdayR::tt_load(2020, week = 35)

chopped <- tuesdata$chopped

# arrange data based on imdb ratings --------------------------------------

chopped_imdb <- chopped %>% arrange(episode_rating, desc())
chopped_imdb$entree <- tolower(chopped_imdb$entree)

# dtm_text ----------------------------------------------------------------

library(tidytext)
library(tm)

season_entre <- data.frame(season = chopped$season,entree = chopped$entree)

dtm_season <- season_entre %>% 
  unnest_tokens(input= entree,
                output=word) %>% 
  count(season, word) %>%
  cast_dtm(document= season,
           term=word, value=n) 

dtm <- as.matrix(dtm_season)
dtm <- data.frame(dtm)
dtm_colsums <- data.frame(entree = colnames(dtm), n = dtm %>% colSums()) 

remove <- c("in","of","mix","mixed","to","the")

dtm_colsums <- filter(dtm_colsums, entree!= remove)


# filter out entree based on protein  -------------------------------------------------------


beef_chopped <- chopped_imdb %>% filter(str_detect(entree, "beef")) %>% select(episode_rating)
beef_chopped$entree <- "Beef"

chicken_chopped <- chopped_imdb %>% filter(str_detect(entree, "chicken"))%>% select(episode_rating)
chicken_chopped$entree <- "Chicken"

pork_chopped <- chopped_imdb %>% filter(str_detect(entree, "pork"))%>% select(episode_rating)
pork_chopped$entree <- "Pork"

lamb_chopped <- chopped_imdb %>% filter(str_detect(entree, "lamb"))%>% select(episode_rating)
lamb_chopped$entree <- "Lamb"

beans_chopped <- chopped_imdb %>% filter(str_detect(entree, "beans"))%>% select(episode_rating)
beans_chopped$entree <- "Beans"

steak_chopped <- chopped_imdb %>% filter(str_detect(entree, "steak")) %>% select(episode_rating)
steak_chopped$entree <- "Steak"

protein <- rbind(beef_chopped,chicken_chopped,pork_chopped,
                 lamb_chopped, beans_chopped,steak_chopped)

# ggridges plot for highest protein sources ----------------------------------------------------------------

protein <- drop_na(protein)

library(ggridges)
png('protein_plot_wk35.png')

theme <- theme_minimal()+
  theme(plot.background = element_rect(fill = "white"),
        text = element_text(color = "black"),
        axis.title.x = element_text(color = "black", size = 15),
        axis.title.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "black", size = 30),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12))



protein_plot <- ggplot(protein, aes(x = episode_rating, y = entree, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Episode ratings") +  theme+
  labs(x = "Entree",
       y = "Episode ratings",
       title = "IMDB episode ratings for popular proteins in entree", 
       caption = "#tidytuesday 
               Data source:Kaggle & IMDB
               By:Netra Bhandari")
  

protein_plot
 
ggsave(filename =  "tt_35.png", protein_plot, width =10,height = 10 ,dpi = 300)
# make plot
dev.off()



# word cloud --------------------------------------------------------------
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("htmlwidgets")
library("webshot")

# word graph for entree ---------------------------------------------------

set.seed(1234)
word_cloud_plot <- wordcloud(words = dtm_colsums$entree, freq = dtm_colsums$n, min.freq = 10,
          max.words=200, random.order= FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# save plots
saveWidget(word_cloud_plot, file="mywordcloud.html")


