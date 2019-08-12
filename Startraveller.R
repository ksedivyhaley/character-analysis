library(tidyverse)
library(RColorBrewer)

getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

stat_levels <- c("Health", "Attack", "Defense", "Initiative", "Skills", "Will", "Ref", "Fort")

plot_ByStat <- function(data, title, fileName, colors){
  
  plot <- data %>%
    ggplot(aes(x=Name, y=Value, fill = Name)) + 
    geom_bar(stat="identity", position = "dodge") + 
    facet_wrap(~Stat) +
    theme_bw() +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
    scale_fill_manual(values = getPalette(colors)) +
    ggtitle(title)
  
  ggsave(paste0("StartravellerImages/", fileName, ".png"), plot)
  
}

plot_ByName <- function(data, title, fileName){
  
  plot <- data %>%
    ggplot(aes(x=Stat, y=Value, fill = Stat)) + 
    geom_bar(stat="identity", position = "dodge") + 
    facet_wrap(~Name) +
    theme_bw() +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
    scale_fill_manual(values = getPalette(8)) +
    ggtitle(title)
  
  ggsave(paste0("StartravellerImages/ByName", fileName, ".png"), 
         plot)
  
}


# Races ---------------------------------------------------------------


races <- read_csv("StartravellerRaces.csv")

races <- races %>%
  gather(key = "Stat", value = "Value", `Health`:`Fort`) %>%
  mutate(Bonus = as.numeric(Value),
         Name = factor(Race),
         Stat = factor(Stat, levels = stat_levels))

plot_ByStat(races, "Races", "Races", 12)
plot_ByName(races, "Races", "Races")


# Classes ---------------------------------------------------------------


classes <- read_csv("StartravellerClasses.csv")

classes <- classes %>%
  gather(key = "Stat", value = "Value", `Health`:`Fort`) %>%
  mutate(Bonus = as.numeric(Value),
         Name = factor(Class),
         Stat = factor(Stat, levels = stat_levels))

plot_ByStat(classes, "Classes", "Classes", 36)
ggsave("StartravellerImages/Classes.png", width=12)

plot_ByName(classes, "Classes", "Classes")
