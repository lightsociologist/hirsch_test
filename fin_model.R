#https://elsevier.digitalcommonsdata.com/datasets/btchxktzyw/1/files/51ba39a5-4de9-4c79-b180-f0ca4d5747ed


library(brms)
library(dplyr)
library(lme4)
library(ggplot2)
library(stargazer)

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

#Between group variation contributes to XXX of total variation.

scmlm$frac1_scale <- scmlm$frac1*100
scmlm$propfem_scale <- scmlm$proportion_female*100

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

centmlm <- centmlm %>%  
  mutate(propfem.gmc = scale(propfem_scale, center = T, scale = F),
    nps.gmc = scale(nps, center = T, scale = F))

centmlm <- centmlm %>% select(h18, nc9618.cwc2, age.cwc, nps.cwc, unicount.cwc, frac1_scale.cwc,
                              propfem_scale.cwc, name1, propfem.gmc, nps.gmc, propfem_scale.cm)

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


model2 <- lmer(formula = h18 ~ propfem_scale.cwc + nps.cwc + nc9618.cwc2 + unicount.cwc + frac1_scale.cwc  + age.cwc + (1|name1), 
               data=centmlm, REML=FALSE)

summary(modelnull)

summary(model2)



#https://biologyforfun.wordpress.com/2017/04/03/interpreting-random-effects-in-linear-mixed-effect-models/

rando1 <- ranef(model2)$name1

rando1$estmean <- rando1$`(Intercept)`+42.84

rando1$name <- row.names(rando1)

rando1 <- rando1[order(rando1$estmean),]

rando1 <- mutate(rando1, nid=row_number())

nseq <- c(1,2,3,4,5,47,48,49,50,51,91,92,93,94,95)

rando2 <- filter(rando1, rando1$nid%in%nseq)

rando2$clrs <- c("blue", "blue", "blue", "blue", "blue", "green", "green", "green", "green", "green", 
                 "red", "red", "red", "red", "red")
          

#Lollipops

ggplot(rando2, aes(x= reorder(name, -estmean), y=estmean, group=clrs, color=clrs)) +
  geom_segment( aes(x=reorder(name, -estmean), xend=name, y=0, yend=estmean), color="skyblue") +
  geom_point(size=3, alpha=0.6) +
  geom_hline(yintercept = 42.84, color="orange") +
  theme_light() +
  coord_flip() +
  ylab("Estimated Mean Hirsch Index") + xlab("Discipline") +
  scale_color_discrete(name = "Rank", labels = c("Bottom 5", "Middle 5", "Top 5")) +
    theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

class(modelnull) <- "lmerMod"

class(model2) <- "lmerMod"

stargazer(modelnull, model2, type="html", out="table2.html",
          title = "Table 2: A Multilevel Model of the Hirsch Index",
          covariate.labels = c("Proportion Female Name","# of Sole Author Publications",
                               "# of Citations (1996-2018)", "University Count (# of Citation Allstars)", 
                               "Disciplinarity Score", "Age (Years Publishing)"),
          notes="<small>Data: Ioannidis et al. (2019)</small>",
          dep.var.labels="Hirsch Index",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001),
          model.names = FALSE,
          column.labels = c("Model 1: Null Model", "Model 2"))




#let's think about group level effects

dis.df <- as.data.frame(table(centmlm$name1))

colnames(dis.df)[1] <- "name1"

mhindex <- centmlm %>% group_by(name1) %>% summarize(mean(h18))

femmean <- centmlm %>% group_by(name1) %>% summarize(mean(propfem_scale.cm))

dis.df <- left_join(dis.df, mhindex)

dis.df <- left_join(dis.df, femmean)

colnames(dis.df)[3] <- "mhindex"

colnames(dis.df)[4] <- "mpropfem"

ggplot(dis.df, aes(x=mpropfem, y=mhindex)) +
  geom_point(alpha=0.5) +
  geom_smooth()



centmlm$nps.gmc2 <- centmlm$nps.gmc/100


centmlm$propfem.gmc2 <- centmlm$propfem.gmc/100


model3 <- lmer(formula = h18 ~ propfem_scale.cwc + nps.cwc + nc9618.cwc2 + unicount.cwc + frac1_scale.cwc  + age.cwc 
               + (1 + propfem.gmc2|name1), 
               data=centmlm, REML=FALSE)


class(model3) <- "lmerMod"


stargazer(modelnull, model3, type="html", out="tablex.html")
