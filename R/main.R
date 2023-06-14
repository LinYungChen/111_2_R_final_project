

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

# Indenpendent Director

## get the columns I need and change its name

glimpse(finalProject$data$Board.csv)

finalProject$board_analysis$indenpendent_director <-
  finalProject$data$Board.csv[,c("公司代號",
                                  "公司名稱",
                                  "董事席次.含獨立董事..席.",
                                  "獨立董事席次.席.",
                                  "董事出席董事會出席率")]

colnames(finalProject$board_analysis$indenpendent_director) <-
  c("number","name","all","independent","attend")

view(finalProject$board_analysis$indenpendent_director)


## Percentage of Indenpendent

finalProject$board_analysis$indenpendent_director$percentage <-
  finalProject$board_analysis$indenpendent_director$independent /
  finalProject$board_analysis$indenpendent_director$all

## Overall situation

finalProject$board_analysis$independentPercentage_category <-
  cut(finalProject$board_analysis$indenpendent_director$percentage,
      breaks = c(-Inf, 0.333, 0.499, Inf),
      labels = c("小於1/3", "未過半但過1/3", "過半數"))

finalProject$board_analysis$independentPercentage_count <-
  table(finalProject$board_analysis$independentPercentage_category)

finalProject$board_analysis$indenpendentOverall <-
  data.frame(
    Category = as.character(names(finalProject$board_analysis$independentPercentage_count)),
    Count = as.numeric(finalProject$board_analysis$independentPercentage_count)
    )

finalProject$board_analysis$independentPercentage_sum <-
  sum(as.numeric(finalProject$board_analysis$independentPercentage_count))

finalProject$board_analysis$indenpendentOverall$Percentage <-
  sprintf(
    "%.1f %%", round(finalProject$board_analysis$independentPercentage_count /
  finalProject$board_analysis$independentPercentage_sum, digits = 2) *100
  )

View(finalProject$board_analysis$indenpendentOverall)

## The company with "independent" of 50%

finalProject$board_analysis$indenpendentHalf <-
  finalProject$board_analysis$indenpendent_director[
    finalProject$board_analysis$indenpendent_director$percentage > 0.499,
  ]

View(finalProject$board_analysis$indenpendentHalf)

# Female Director

## get the columns I need, parsing,and change columns name

glimpse(finalProject$data$Board.csv)

finalProject$board_analysis$female_director <-
  finalProject$data$Board.csv[,c("公司代號",
                                 "公司名稱",
                                 "董事席次.含獨立董事..席.",
                                 "女性董事席次及比率.席")]

colnames(finalProject$board_analysis$female_director) <-
  c("number","name","all","female")

finalProject$board_analysis$female_director$female <-
  as.numeric(gsub("[^0-9.]", "",
                  finalProject$board_analysis$female_director$female))

## Percentage of Female

finalProject$board_analysis$female_director$percentage <-
  finalProject$board_analysis$female_director$female /
  finalProject$board_analysis$female_director$all

finalProject$board_analysis$female_director$percentage <-
  round(finalProject$board_analysis$female_director$percentage, digits = 2)

View(finalProject$board_analysis$female_director)

## Overall situation

finalProject$board_analysis$femalePercentage_category <-
  cut(finalProject$board_analysis$female_director$percentage,
      breaks = c(-Inf, 0 , 0.333, Inf),
      labels = c("未設置女性董事", "有設置但未達1/3", "達1/3"))

finalProject$board_analysis$femalePercentage_count <-
  table(finalProject$board_analysis$femalePercentage_category)

finalProject$board_analysis$femaleOverall <-
  data.frame(
    Category = as.character(names(finalProject$board_analysis$femalePercentage_count)),
    Count = as.numeric(finalProject$board_analysis$femalePercentage_count)
  )

finalProject$board_analysis$femalePercentage_sum <-
  sum(as.numeric(finalProject$board_analysis$femalePercentage_count))

finalProject$board_analysis$femaleOverall$Percentage <-
  sprintf(
    "%.1f %%", round(finalProject$board_analysis$femalePercentage_count /
                       finalProject$board_analysis$femalePercentage_sum, digits = 2) *100
  )

View(finalProject$board_analysis$femaleOverall)

## The company with "female" more than 1/3

finalProject$board_analysis$femaleReachStandard <-
  finalProject$board_analysis$female_director[
    finalProject$board_analysis$female_director$percentage > 0.333,
  ]

View(finalProject$board_analysis$femaleReachStandard)

## The company with "independent>1/2" and "female>1/3"

finalProject$board_analysis$BothIndependentAndFemaleReachStandard <-
  finalProject$board_analysis$female_director %>%
  inner_join(finalProject$board_analysis$indenpendent_director,
             by = "number") %>%
  filter(finalProject$board_analysis$female_director$percentage > 0.333,
         finalProject$board_analysis$indenpendent_director$percentage > 0.5)

View(finalProject$board_analysis$BothIndependentAndFemaleReachStandard)


## 直接轉移到「.Rmd」撰寫，省去複製貼上的過程。 2023.06.14



