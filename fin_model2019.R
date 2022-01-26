#https://elsevier.digitalcommonsdata.com/datasets/btchxktzyw/1/files/51ba39a5-4de9-4c79-b180-f0ca4d5747ed

library(dplyr)
library(lme4)
library(ggplot2)
library(stargazer)

hind.df <- readRDS("~/Documents/GitHub/hirsch_test/data/hind19.df.rdata")

#Let's select variables for the mlm model

scmlm <- hind.df %>% select(discipline, age, h19, nps, np6019, nc9619, unicount, sm_field_frac, proportion_female, sm_field)


scmlm$field_frac_scale <- scmlm$sm_field_frac*100
scmlm$propfem_scale <- scmlm$proportion_female*100
scmlm$cites_scale <- scmlm$nc9619/100
scmlm$prop_sole_scale <- (scmlm$nps/scmlm$np6019)*100

#Cluster means centering

centmlm <- scmlm %>%
group_by(discipline) %>% 
  mutate(age.cm = mean(age),
         age.cwc = age-age.cm,
         cites_scale.cm = mean(cites_scale),
         cites_scale.cwc = cites_scale-cites_scale.cm,
         prop_sole_scale.cm = mean(prop_sole_scale),
         prop_sole_scale.cwc = prop_sole_scale-prop_sole_scale.cm,
         unicount.cm = mean(unicount),
         unicount.cwc = unicount-unicount.cm,
         field_frac_scale.cm = mean(field_frac_scale),
         field_frac_scale.cwc = field_frac_scale-field_frac_scale.cm,
         propfem_scale.cm = mean(propfem_scale, na.rm=TRUE),
         propfem_scale.cwc = propfem_scale-propfem_scale.cm)


#For testing level 2 effects
#centmlm <- centmlm %>%  
#  mutate(propfem.gmc = scale(propfem_scale, center = T, scale = F),
#   nps.gmc = scale(nps, center = T, scale = F))

#Remove rare disciplines

#Let's pull out fields or disciplines to remove very rare ones.

dis_count <- as.data.frame(table(centmlm$discipline))

dis_count <- mutate(dis_count, delvar = ifelse(Freq<101, 1, 0))

dis_count <- dis_count %>% rename(discipline = Var1)



fld_count <- as.data.frame(table(centmlm$sm_field))

fld_count <- mutate(fld_count, delvar2 = ifelse(Freq<401, 1, 0))

fld_count <- fld_count %>% rename(sm_field = Var1)

fld_count <- fld_count %>% rename(Freq2 = Freq)

#We merge back the discipline counts and remove the ones that appear less than
#50 times.

centmlm <- left_join(centmlm, dis_count)

centmlm <- centmlm %>% filter(delvar==0)

centmlm <- left_join(centmlm, fld_count)

centmlm <- centmlm %>% filter(delvar2==0)


#Listwise delete
centmlm <- centmlm %>% na.omit()

#variables (H18 is the Hirsch Index for 2018, age is last year - first year in dataset,
#nc9618 is the total cites from 1996-2018/100, nps, is numver of single-authored pubs
#unicount is the nubmer of times the univsity appears, frac1 is the fraction of papers
#	fraction of AR+RE+CP in Science-Metrix category for author, propfem is the 
# the proportion female via the author first name)

#Unconditional means model that shows the amount of total variance that is explained
#by in-discipline versus between disciplines.

modelnull <- lmer(formula = h19 ~ 1 + (1|discipline), 
                  data=centmlm, REML=FALSE)

RandomEffects <- as.data.frame(VarCorr(modelnull))
RandomEffects

ICC_between <- RandomEffects[1,4]/(RandomEffects[1,4]+RandomEffects[2,4]) 
ICC_between


model2 <- lmer(formula = h19 ~ propfem_scale.cwc + prop_sole_scale.cwc + cites_scale.cwc + 
                 unicount.cwc + field_frac_scale.cwc  + age.cwc + (1|discipline), 
               data=centmlm, REML=FALSE)

summary(modelnull)

summary(model2)



#https://biologyforfun.wordpress.com/2017/04/03/interpreting-random-effects-in-linear-mixed-effect-models/

rando1 <- ranef(model2)$discipline

rando1$estmean <- rando1$`(Intercept)`+40.94

rando1$discipline <- row.names(rando1)

rando1 <- rando1[order(rando1$estmean),]

rando1 <- mutate(rando1, nid=row_number())

nseq <- c(1,2,3,4,5,62,63,64,65,66,116,117,118,119,120)

rando2 <- filter(rando1, rando1$nid%in%nseq)

rando2$clrs <- c("blue", "blue", "blue", "blue", "blue", "green", "green", "green", "green", "green", 
                 "red", "red", "red", "red", "red")
          

#Lollipops

ggplot(rando2, aes(x= reorder(discipline, -estmean), y=estmean, group=clrs, color=clrs)) +
  geom_segment( aes(x=reorder(discipline, -estmean), xend=discipline, y=0, yend=estmean), color="skyblue") +
  geom_point(size=3, alpha=0.6) +
  geom_hline(yintercept = 38.62, color="orange") +
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

class(climodelnull) <- "lmerMod"

class(climodel2) <- "lmerMod"


stargazer(modelnull, model2, climodelnull, climodel2, style="asr", type="html", out="19table2_all.html",
          align=TRUE, title = "Table 2: A Multilevel Model of the Hirsch Index",
          covariate.labels = c("Percentage Female Name","Percentage Sole Author Publications",
                               "# of Citations (1996-2019)", "University Count (# of Citation Allstars)", 
                               "Disciplinarity Score", "Age (Years Publishing)"),
          notes="<small>Data: Ioannidis et al. (2019)</small>",
          dep.var.labels="Hirsch Index",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001),
          model.names = FALSE,
          column.labels = c("Null Model (All Disciplines)", "Full Model (All Disciplines)", 
                            "Null Model (Clinical Medicine)",
                            "Full Model (Clinical Medicine)"))




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
