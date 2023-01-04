#https://www.nature.com/articles/d41586-019-02479-7


library(readr)
library(dplyr)
library(fastDummies)
library(jtools)
library(data.table)
library(GGally)
library(readxl)

scholar_data_2018 <- read_csv("data/scholar_data_2018.csv")

#keep only usa based scholars for the discipline data to make sense

schous <- scholar_data_2018 %>% filter(cntry=="usa")

#Keep relatively currently active scholars

schous <- schous %>% filter(lastyr>2015)

#let's grab discipline data

dis_count <- as.data.frame(table(schous$name1))

#let's remove rare disciplines (those less than 50 scholars)

#dis_count <- mutate(dis_count, delvar = ifelse(Freq<50, 1, 0))

dis_count <- dis_count %>% rename(field = Var1)

dis_cnt2 <- dis_count 

CIPCode2010 <- read_excel("data/CIPCode2010.xlsx")

CIPCode2010$CIPTitle2 <- substr(CIPCode2010$CIPTitle,1,nchar(CIPCode2010$CIPTitle)-1)

discip <- left_join(dis_cnt2, CIPCode2010, by=c("field"="CIPTitle2"))

discip$cip4 <- ifelse(nchar(discip$CIPCode)>5, substr(discip$CIPCode,1,nchar(discip$CIPCode)-2), discip$CIPCode)

discip2 <- discip %>% select(field, cip4)

discip2 <- left_join(discip2, CIPCode2010, by=c("cip4"="CIPCode"))

discip2 <- distinct(discip2, field, .keep_all= TRUE)

discip2 <- discip2 %>% select(field, cip4, CIPFamily)

#let's save discipline out to merge with graduates survey

library(openxlsx)

write.xlsx(discip2, "~/Documents/GitHub/hirsch_test/data/dis_names_cip.xlsx")

#we can bring in the merge file (merged by looking up the discipline names here:
#https://nces.ed.gov/ipeds/cipcode/browse.aspx?y=55

dis_complete <- read_excel("data/dis_names_cip_complete.xlsx")

discomp <- left_join(dis_complete, CIPCode2010, by=c("cip4"="CIPCode"))

discomp <- discomp %>% select(field, cip4, CIPFamily, CIPTitle2)

discomp <- left_join(discomp, CIPCode2010, by=c("CIPFamily"="CIPCode"))

discomp <- discomp %>% select(field, cip4, CIPFamily, CIPTitle2.x, CIPTitle)

discomp <- discomp %>% rename(cip2=CIPFamily, cip4name=CIPTitle2.x, cip2name=CIPTitle)

#and the ipeds data

iped2018 <- read_csv("data/ipeds_2018.csv")

iped2017 <- read_csv("data/ipeds_2017.csv")

iped2016 <- read_csv("data/ipeds_2016.csv")

ipeds <- bind_rows(iped2018, iped2017)

ipeds <- bind_rows(ipeds, iped2016)

ipeds <- ipeds %>% filter(AWLEVEL>16)

ipeds$cip2 <- substr(ipeds$CIPCODE,1,nchar(ipeds$CIPCODE)-5)

ipeds$cip4 <- substr(ipeds$CIPCODE,1,nchar(ipeds$CIPCODE)-2)

ipeds <- filter(ipeds, CIPCODE!=99)

ipeds <- filter(ipeds, CIPCODE!=22)

ipeds <- filter(ipeds, CIPCODE!=51)

#just keep the major code (cip2) and the black, hispanic, and sex variables for now

ipedshort <- ipeds %>% select(cip4,  CTOTALT, CTOTALW, CBKAAT, CHISPT, CWHITT, CTOTALM)

#sum across all remaining variables by code

iped_sum <- ipedshort %>% group_by(cip4) %>% summarise_each(list(sum))

iped_sum <- iped_sum %>% filter(CTOTALT>50)

iped_sum <- iped_sum %>% filter(cip4!="")

#tabulate % white

iped_sum$perwhite <- iped_sum$CWHITT/iped_sum$CTOTALT

#tabulate % black

iped_sum$perblack <- iped_sum$CBKAAT/iped_sum$CTOTALT

#tabulate % latinx

iped_sum$perltx <- iped_sum$CHISPT/iped_sum$CTOTALT


#tabulate % women

iped_sum$perwomen <- iped_sum$CTOTALW/iped_sum$CTOTALT

#tabulate % men

iped_sum$permen <- iped_sum$CTOTALM/iped_sum$CTOTALT

#merge back with discomp

iped_mer <- left_join(discomp, iped_sum)


#merge with scholar data 

schous <- schous %>% rename(field=name1)

schous <- left_join(schous, iped_mer)

schous <- schous %>% filter(!is.na(field))

schous2 <- schous %>% filter(!is.na(perwomen))

schous$age <- schous$lastyr-schous$firstyr

schous <- schous %>% filter(age < 50)




uni_count <- as.data.frame(table(schous$inst_name))

uni_count <- uni_count %>% rename(inst_name=Var1, unicount=Freq)

schd <- left_join(schous, uni_count)

havg <- schd %>% group_by(cip4) %>% summarise(meanh = mean(h18))

ipeds_avg <- left_join(iped_sum, havg)


screg <- schd %>% select(field, age, h18, nps, nc9618, unicount, cip4, perltx, perwomen, perblack, perwhite, permen)

library(lmerTest)

model0_fit <- lmer(formula = h18 ~ 1 + (1|cip4), 
                data=screg)

summary(model0_fit)

VarCorr(model0_fit)

RandomEffects <- as.data.frame(VarCorr(model0_fit))
RandomEffects

ICC_between <- RandomEffects[1,4]/(RandomEffects[1,4]+RandomEffects[2,4]) 
ICC_between


model1 <- lmer(formula = h18 ~ age + (1|cip4), 
                   data=screg)

summary(model1)

model2 <- lmer(formula = h18 ~ age + nps + unicount + (1|field), 
                   data=screg, REML=FALSE)

summary(model2)

screg2 <- screg

screg2$dep <- log(screg2$h18)


model0_fit <- lmer(formula = dep ~ 1 + (1|cip4), 
                   data=screg2)

summary(model0_fit)

VarCorr(model0_fit)

RandomEffects <- as.data.frame(VarCorr(model0_fit))
RandomEffects

ICC_between <- RandomEffects[1,4]/(RandomEffects[1,4]+RandomEffects[2,4]) 
ICC_between


model1 <- lmer(formula = dep ~ age + (1|cip4), 
               data=screg2)

summary(model1)

model2 <- lmer(formula = dep ~ age + nps + unicount + perwhite + permen + (1|cip4), 
               data=screg2, REML=FALSE)

summary(model2)

                
mlm1 <- lmer(h18 ~ age + nps + unicount + (1|gss_code), data=screg2)

summary(mlm1)

mlm2 <- lmer(h18 ~ perwomen + perblack + perwhite + perasian + perpacific + permulti + perind + perunk +  (1|gss_code), data=screg2)

summary(mlm2)

