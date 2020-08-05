
# #tidytuesday ------------------------------------------------------------

#European Energy 
#data source - EUROSTAT
#part of tidy tuesday week 32

# libraries needed --------------------------------------------------------
library(tidytuesdayR)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(wesanderson)

# Getting the data from github --------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2020-08-04')
tuesdata <- tidytuesdayR::tt_load(2020, week = 32)

energy_types <- tuesdata$energy_types

colnames(energy_types)
# "country"      "country_name" "type"         "level"        "2016"         "2017"         "2018" 

unique(energy_types$type)

# "Conventional thermal" "Nuclear"              "Hydro"                "Pumped hydro power"   "Wind"                
#"Solar"                "Geothermal"           "Other"       

# Cleaning data -----------------------------------------------------------

energy_types <- energy_types %>% filter(level == "Level 1")
#removed level 2 since it is already included in level 1

energy_types <- gather(energy_types, "year", "n", 5:7)

energy_types <- spread(energy_types, type, n) 

energy_types$Total_energy <- rowSums(energy_types[,5:11])


energy_types <-  dplyr::mutate(energy_types,
                               Renewable_Energy = rowSums(energy_types[,c(6:7,9:11)])*100/energy_types$Total_energy)


energy_types <-  dplyr::mutate(energy_types,
                               Conventional_Energy = (energy_types$`Conventional thermal`)*100/energy_types$Total_energy,
                               Nuclear_Energy = (energy_types$Nuclear)*100/energy_types$Total_energy)
                              

energy_types <- energy_types %>% select("country","country_name","year","Total_energy",
                                        "Renewable_Energy","Conventional_Energy","Nuclear_Energy")

energy_types <- gather(energy_types,"type","percent", 5:7 )

energy_types$country_name <-energy_types$country_name %>% replace_na("United Kingdom")
 
# Data plotting -----------------------------------------------------------

library(wesanderson)

#choosing color pallete 
color_pal = wes_palette("Moonrise3")


#choosing appropriate theme
theme <- theme_minimal()+
  theme(plot.background = element_rect(fill = "white"),
        text = element_text(color = "black"),
        strip.text = element_text(color = "black"),
        strip.text.x = element_text(size = 22),
        panel.grid = element_blank(),
        axis.title.x = element_text(color = "black", size = 30),
        axis.title.y = element_text(color = "black", size = 30),
        axis.text.x = element_text(color = "black", size = 17),
        axis.text.y = element_text(color = "black", size = 17),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "black", size = 30),
        legend.position = c(0.90, .025),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 27))


plot <- energy_types %>%
  ggplot(aes(x= year, y=percent, fill = type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5)+
  scale_fill_manual(values = color_pal)+
  scale_y_continuous(breaks = seq(0,100,20), labels = paste0(seq(0,100,20)*1,"%"))+
  facet_wrap(~country_name, scales = "free")+
  coord_flip()+ theme+
  #write essential info with minimum words
  labs(x = "Year", y = "Percent Energy Produced",
    fill = "Energy type",
    title = "Percent energy production by European countries",
    caption = "#tidytuesday 
               Data source:Eurostat
               By:Netra Bhandari")
  

# saving the plot ---------------------------------------------------------

ggsave(filename =  "tt_32.png",plot, units = c("in"), width = 30,height = 23 ,dpi = 300)
  

