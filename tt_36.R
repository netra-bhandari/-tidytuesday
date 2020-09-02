
# tidy tuesday week 36  ---------------------------------------------------

#data on Global Crops yield from Our world in data


# Getting the data --------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2020-09-01')
tuesdata <- tidytuesdayR::tt_load(2020, week = 36)


# loading the libraries ---------------------------------------------------
library(tidyverse, dplyr)
library(ggplot2)


# Exploring the data set --------------------------------------------------

head(tuesdata) 
#separating the data into different dataframes 

arable <- tuesdata$arable_land_pin %>% filter( Entity == "India")

crop_vs_fert <- tuesdata$cereal_crop_yield_vs_fertilizer_application%>% 
                filter(Entity == "India")

crop_vs_tractor <- tuesdata$cereal_yields_vs_tractor_inputs_in_agriculture %>% 
                    filter(Entity == "India") %>% 
                    remove_missing()

lulc <- tuesdata$land_use_vs_yield_change_in_cereal_production %>% 
          filter(Entity == "India") %>% 
          remove_missing()

keycrop_yields <- tuesdata$key_crop_yields %>% 
                  filter(Entity == "India")



# exploring trends for India  ---------------------------------

library(hrbrthemes)
library(viridis)

crops_gather <- gather(keycrop_yields, "Crop", "Yield", -c("Entity","Code","Year"))

plot_1 <- crops_gather %>%
  ggplot(aes(x= Year, y= Yield, size = Yield, fill= Crop)) +
  geom_point(alpha=0.8, shape=21, color="aquamarine3") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Yield (tonnes per hectare)") +
  xlab("Year") +
  theme(legend.position = "right",#plot.background = element_rect(fill = "white"),
        text = element_text(color = "aquamarine3"),
        axis.title.x = element_text(color = "aquamarine3", size = 17),
        axis.title.y = element_text(color = "aquamarine3", size = 17),
        axis.text.x = element_text(color = "aquamarine3", size = 15),
        axis.text.y = element_text(color = "aquamarine3", size = 15),
        plot.title = element_text(size = 19, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "aquamarine3", size = 7),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        plot.background = element_rect(fill = "black"))+
  labs(title = "Crop yields in India between 1960 and 2018", subtitle = "India is an agroeconomy and the second most populous country. The following plot shows how some key crop production in India has increased over years ",
               caption = "#tidytuesday 
               Data source: Our world in data
               By:Netra Bhandari")

plot_1

#making the plot interactive
#library(plotly)
#plot_1 <- ggplotly(plot_1, tooltip="text")
#plot_1

#saving plot
ggsave(plot_1, filename = "tt_36_plot1.png", width = 15, height = 7,unit = "in" , dpi = 300)



