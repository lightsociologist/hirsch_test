#This program incorporates data from Ioannidis et al. to build an analysis of
#factors related to variation in individual academics' Hirsch scores. 
# (see https://www.nature.com/articles/d41586-019-02479-7)
#Data: https://elsevier.digitalcommonsdata.com/datasets/btchxktzyw/2

library(readr)
library(dplyr)
library(fastDummies)
library(jtools)
library(data.table)
library(GGally)

#Load the 2019 data

Table_S6_career_2019 <- read_excel("data/Table-S6-career-2019.xlsx", 
                                        sheet = "Career")

#Limit to the US due to international variation, but may change

schous <- Table_S6_career_2019 %>% filter(cntry=="usa")
                                
#Some of the data is a little suspicious, likely due to faulty name disambiguation
#We can protect perhaps agaisnt some of that, perhaps, by limiting to recently active
#scholars and make sure that there is a plausible career here by setting first year to 
#1960.
             
schous <- schous %>% filter(lastyr>2016)

schous <- schous %>% filter(firstyr>1959)

#Pulling out the median for quick and dirty analysis

schous <- schous %>% rename(discipline = sm_subfield_1)

mh19 <- schous %>% group_by(discipline) %>% summarize(m = median(h19))

median(mh19$m)

#Select a discipline based on median scores for fixed effects if you'd like

schd <- dummy_cols(schous, select_columns = "discipline")

#Build an age control

schd$age <- schd$lastyr-schd$firstyr

#Build an institution control

uni_count <- as.data.frame(table(schd$inst_name))

uni_count <- uni_count %>% rename(inst_name=Var1, unicount=Freq)

schd <- left_join(schd, uni_count)

#Build a gender variable

library(stringr)
library(gender)

schd$firstname <- (str_split_fixed(schd$authfull, ",", 2)[,2])

schd <- schd %>% mutate_at(vars(firstname), function(x){gsub('[^ -~]', '', x)})

schd$firstname <- str_trim(schd$firstname, side = "left")

schd$firstname <- word(schd$firstname)

schd$firstname <- str_trim(schd$firstname, side = "both")

genguess <- gender(schd$firstname, years=c(1940, 1990), method="ssa")

genguess <-genguess %>% distinct(name, .keep_all = TRUE)

#merge gender and save

hind.df <- left_join(schd, genguess, by=c("firstname"="name"))

saveRDS(hind.df, file="data/hind19.df.rdata")

