#https://www.nature.com/articles/d41586-019-02479-7


library(readr)
library(dplyr)
library(fastDummies)
library(jtools)
library(data.table)
library(GGally)

scholar_data_2018 <- read_csv("data/scholar_data_2018.csv")

schous <- scholar_data_2018 %>% filter(cntry=="usa")
                                             
schous <- schous %>% filter(lastyr>2015)

dis_count <- as.data.frame(table(schous$name1))

dis_count <- mutate(dis_count, delvar = ifelse(Freq<50, 1, 0))

dis_count <- dis_count %>% rename(name1 = Var1)

schous <- left_join(schous, dis_count)

schous <- schous %>% filter(delvar==0)

schd <- dummy_cols(schous, select_columns = "name1")

schd$age <- schd$lastyr-schd$firstyr

uni_count <- as.data.frame(table(schd$inst_name))

uni_count <- uni_count %>% rename(inst_name=Var1, unicount=Freq)

schd <- left_join(schd, uni_count)

screg <- schd %>% select(starts_with("name1_"), age, h18, nps, nc9618, unicount)



screg2 <- screg

screg2$h18 <- log(screg2$h18)

logmodel1 <- lm(h18 ~ .-name1_Paleontology, data=screg2)

summary(logmodel1)

ggcoef(tail(broom::tidy(hmodel1, conf.int = TRUE), 20), sort = "ascending")

t <- ggcoef(tail(broom::tidy(logmodel1, conf.int = TRUE), 125), sort = "ascending")

cfs <- t$data

cfs <- cfs %>% filter(term != "(Intercept)")

topcfs <- cfs %>% top_n(estimate, n=10)

bottomcfs <- cfs %>% top_n(estimate, n=-10)

topbottom <- bind_rows(topcfs, bottomcfs)  

ggcoef(topbottom, sort = "ascending")  
  
  
  
  
#Practice below here
    
hmodel1 <- glm(h18 ~ .-name1_Paleontology, family="poisson", data=screg)

summary(hmodel1)



effect_plot(hmodel1, pred = name1_Sociology, interval = TRUE, plot.points = TRUE)

plot_summs(hmodel1, scale = TRUE)

confint(hmodel1)

str(summary(hmodel1))

