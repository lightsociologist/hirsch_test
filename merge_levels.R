library(readr)
library(dplyr)

scholar_data_2018 <- read_csv("data/scholar_data_2018.csv")

schous <- scholar_data_2018 %>% filter(cntry=="usa")
                                             
schous <- schous %>% filter(lastyr>2015)

check <- as.data.frame(table(schous$name1))