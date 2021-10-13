#This program incorporates data from Ioannidis et al. to build an analysis of
#factors related to variation in individual academics' Hirsch scores. 
# (see https://www.nature.com/articles/d41586-019-02479-7)

library(readr)
library(dplyr)
library(fastDummies)
library(jtools)
library(data.table)
library(GGally)

#Load the 2018 data

scholar_data_2018 <- read_csv("data/scholar_data_2018.csv")

#Limit to the US due to international variation, but may change

schous <- scholar_data_2018 %>% filter(cntry=="usa")
                                
#Some of the data is a little suspicious, likely due to faulty name disambiguation
#We can protect perhaps agaisnt some of that, perhaps, by limiting to recently active
#scholars and make sure that there is a plausible career here by setting first year to 
#1960.
             
schous <- schous %>% filter(lastyr>2015)

schous <- schous %>% filter(firstyr>1960)

#Let's pull out fields or disciplines to remove very rare ones.

dis_count <- as.data.frame(table(schous$name1))

dis_count <- mutate(dis_count, delvar = ifelse(Freq<50, 1, 0))

dis_count <- dis_count %>% rename(name1 = Var1)

#We merge back the discipline counts and remove the ones that appear less than
#50 times.

schous <- left_join(schous, dis_count)

schous <- schous %>% filter(delvar==0)

#Pulling out the median for quick and dirty analysis

mh18 <- schous %>% group_by(name1) %>% summarize(m = median(h18))

median(mh18$m)

#Select Physiology based on median scores (for non-MLM)

schd <- dummy_cols(schous, select_columns = "name1")

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

schd$gender <- gender(schd$firstname, years=c(1940, 1990), method="ssa")


screg <- schd %>% select(starts_with("name1_"), age, h18, nps, nc9618, unicount, frac1)

screg2 <- screg

screg2$h18 <- log(screg2$h18)

logbase <- lm(h18 ~ age + nc9618 + nps + unicount +frac1, data=screg2) 

summary(logbase)

logmodel1 <- lm(h18 ~ .-name1_Physiology, data=screg2)

summary(logmodel1)

AIC(logbase, logmodel1)

t <- ggcoef(tail(broom::tidy(logmodel1, conf.int = TRUE), 125), sort = "ascending")

cfs <- t$data

cfs <- cfs %>% filter(term != "(Intercept)")

topcfs <- cfs %>% top_n(estimate, n=10)

bottomcfs <- cfs %>% top_n(estimate, n=-10)

topbottom <- bind_rows(topcfs, bottomcfs)  

ggcoef(topbottom, sort = "ascending")  

p <- ggplot(screg, aes(x=name1_Sociology, y=h18, color=name1_Sociology)) + 
  geom_point()  +
  geom_point(size=4, alpha=0.7) +
  stat_summary(fun=mean, geom="point", shape=23, color="black", aes(fill=name1_Sociology), size=4) +         
  stat_summary(fun.min=function(x)(mean(x)-sd(x)), 
               fun.max=function(x)(mean(x)+sd(x)),
               geom="errorbar", width=0.1) +
  theme_bw()  

  p + theme(legend.position = "none")  + xlab("Sociology = 1") + ylab("Hirsch Index")
  
#Practice below here
    
hmodel1 <- glm(h18 ~ .-name1_Paleontology, family="poisson", data=screg)

summary(hmodel1)



effect_plot(hmodel1, pred = name1_Sociology, interval = TRUE, plot.points = TRUE)

plot_summs(hmodel1, scale = TRUE)

confint(hmodel1)

str(summary(hmodel1))


fdlca <- schd %>% select(starts_with("name1_"))



