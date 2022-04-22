#Final Project
#install.packages("terra")
library("terra")
library("tidyverse")
library("ggplot2")
tree.pecan <- read.csv("Z:/students/egshaw/Data/Final Project Data/individual_phenometrics_data.csv",
                       header = T)
#trying to split Phenophase Description column into three columns so it will be 
#easy to distinguish leaf, bud, and fruit phenophases and compare them to each other.
#tree.pecan <- tree.pecan %>% separate(col = Phenophase_Description, 
  #                                    into = c("Phenophase_Leaves", "Phenophase_Flowers", "Phenophase_Fruits"))

#Figure out min and max lat and long for temperature data search
print(min(tree.pecan$Latitude))
print(max(tree.pecan$Latitude))
print(min(tree.pecan$Longitude))
print(max(tree.pecan$Longitude))

# color = State[Phenophase_Description == "Breaking leaf buds"]
fig1 <- ggplot(subset(tree.pecan, Phenophase_Description == "Breaking leaf buds"), 
               aes(x = First_Yes_DOY, y = Longitude, color = State)) +
        geom_point() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        labs(title = "Variation in Breaking Leaf Buds") +
        xlab("First Sighting of Breaking Leaf Buds (Day of Year)") +
        ylab("Longitude")

