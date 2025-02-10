####Install the necessary R packages####
##You will only need to do this once to install the package in your local library
# install.packages("tidyverse") #A collection of packages for data wrangling
# install.packages("ggplot2") #For our data visualisation
# install.packages("scales") #A helpful package for transforming plot scales
# install.packages("RColorBrewer") #Contains pre-designed colour palettes, easily integrated with ggplot2

####Load the necessary R packages####
##You will need to do this every time you start R
library(tidyverse)
library(ggplot2)
library(scales)
library(RColorBrewer)
# setwd("...") #Change this to the folder where you have saved your file

####Let's Get Visualising!####

###Step 1: Load the Data###
data <- read.csv(file = "TestData_ggplot_workshop.csv", header = TRUE)
#Check number of rows and columns
nrow(data)
ncol(data)
glimpse(data)


###Step 2: Convert the Data into Long Format
long_data <- data %>%
  pivot_longer(cols = starts_with("Area_"), 
               names_to = "Algorithm", names_prefix  = "Area_", 
               values_to = "Area")

#Check number of rows and columns
nrow(long_data)
ncol(long_data)
#Area is the response variable

###Step 3: Create a Basic Ggplot Object
ggplot() #blank figure
ggplot(long_data) #blank figure again
ggplot(long_data, aes(x=TimeOrder, y = Area)) #adds the axis but nothing else

###Step 4: Add a Geometry
ggplot(long_data, aes(x=TimeOrder, y = Area)) +
  geom_point()

###Step 5: Adjust a Scale
ggplot(long_data, aes(x=TimeOrder, y = Area, colour = Algorithm)) +
  geom_point()+
  scale_colour_manual(values = c("#2E0014", "pink", "#809848", "blue"))
  #scale_x_discrete() # is for categorical



###Step 6: Adjust the Theme
library(cowplot)

ggplot(long_data, aes(x=TimeOrder, y = Area, colour = Algorithm)) +
  geom_point()+
  scale_colour_manual(values = c("#2E0014", "pink", "#809848", "blue"))+
  theme_cowplot()

###Step 7: Add a Facet
ggplot(long_data, aes(x=TimeOrder, y = Area)) +
  geom_point()+
  #scale_colour_manual(values = c("#2E0014", "pink", "#809848", "blue"))+
  theme_cowplot()+
  facet_wrap(.~Algorithm)
