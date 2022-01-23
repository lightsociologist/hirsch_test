#https://elsevier.digitalcommonsdata.com/datasets/btchxktzyw/1/files/51ba39a5-4de9-4c79-b180-f0ca4d5747ed


library(brms)
library(dplyr)
library(lme4)
library(ggplot2)


hind.df <- readRDS("~/Documents/GitHub/hirsch_test/data/hind.df.rdata")

#Let's select variables for the mlm model

scmlm <- hind.df %>% select(name1, age, h18, nps, nc9618, unicount, frac1, proportion_female)

#Unconditional means model that shows the amount of total variance that is explained
#by in-discipline versus between disciplines.

model0_fit <- lmer(formula = h18 ~ 1 + (1|name1), 
                   data=scmlm)

#This computes the intra-class correlation 

RandomEffects <- as.data.frame(VarCorr(model0_fit))
RandomEffects

ICC_between <- RandomEffects[1,4]/(RandomEffects[1,4]+RandomEffects[2,4]) 
ICC_between

#Between group variation contributes to 18.3% of total variation.

scmlm$frac1_scale <- scmlm$frac1*100
scmlm$propfem_scale <- scmlm$proportion_female*100


model2 <- lmer(formula = h18 ~ age + nc9618 + nps + unicount + frac1_scale + propfem_scale + (1|name1), 
               data=scmlm, REML=FALSE)

ranef(model2)

centmlm <- scmlm %>%
group_by(name1) %>% 
  mutate(age.cm = mean(age),
         age.cwc = age-age.cm,
         nc9618.cm = mean(nc9618),
         nc9618.cwc = nc9618-nc9618.cm,
         nps.cm = mean(nps),
         nps.cwc = nps-nps.cm,
         unicount.cm = mean(unicount),
         unicount.cwc = unicount-unicount.cm,
         frac1_scale.cm = mean(frac1_scale),
         frac1_scale.cwc = frac1_scale-frac1_scale.cm,
         propfem_scale.cm = mean(propfem_scale, na.rm=TRUE),
         propfem_scale.cwc = propfem_scale-propfem_scale.cm)

#rescaled because of scale warning

centmlm$nc9618.cwc2 <- centmlm$nc9618.cwc/100

centmlm <- centmlm %>% select(h18, nc9618.cwc2, nps.cwc, unicount.cwc, frac1_scale.cwc,
                              propfem_scale.cwc, name1)

centmlm <- centmlm %>% na.omit()

#variables (H18 is the Hirsch Index for 2018, age is last year - first year in dataset,
#nc9618 is the total cites from 1996-2018/100, nps, is numver of single-authored pubs
#unicount is the nubmer of times the univsity appears, frac1 is the fraction of papers
#	fraction of AR+RE+CP in Science-Metrix category for author, propfem is the 
# the proportion female via the author first name)

modelnull <- lmer(formula = h18 ~ 1 + (1|name1), 
                  data=centmlm, REML=FALSE)

RandomEffects <- as.data.frame(VarCorr(modelnull))
RandomEffects

ICC_between <- RandomEffects[1,4]/(RandomEffects[1,4]+RandomEffects[2,4]) 
ICC_between


model3 <- lmer(formula = h18 ~ age.cwc + nc9618.cwc2 + nps.cwc + unicount.cwc + frac1_scale.cwc + propfem_scale.cwc + (1|name1), 
               data=centmlm, REML=FALSE)




summary(model3)

summary(modelnull)



#https://biologyforfun.wordpress.com/2017/04/03/interpreting-random-effects-in-linear-mixed-effect-models/

rando1 <- ranef(model3)$name1

rando2 <- ranef(modelnull)$name1

rando1$estmean <- rando1$`(Intercept)`+42.84

rando1$name <- row.names(rando1)

rando1 <- rando1[order(rando1$estmean),]

rando1 <- mutate(rando1, nid=row_number())

nseq <- c(1,2,3,4,5,47,48,49,50,51,91,92,93,94,95)

rando2 <- filter(rando1, rando1$nid%in%nseq)

#Not really satisfactory, but here is a plot

ggplot(rando2, aes(x= reorder(name, -estmean), y=estmean)) +
  geom_segment( aes(x=reorder(name, -estmean), xend=name, y=0, yend=estmean), color="skyblue") +
  geom_point( color="blue", size=3, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )