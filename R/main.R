

# Environment setting

install.packages("tidyverse")
library(readr)
library(tidyverse)
finalProject <- list()

# Import Data

finalProject$data$filenames <- list.files("./Data")


for(i in finalProject$data$filenames)
{
  finalProject$data[[i]]
  <- read.csv(paste0("./Data/",i))
}

# Indenpendent Director on the board

glimpse(finalProject$data$Board.csv)








