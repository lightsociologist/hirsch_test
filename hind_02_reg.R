#This program uses data constructed in hind_01_merge.R to build an analysis of the 
#Hirsch Index

load("~/Documents/GitHub/hirsch_test/data/hind.df.rdata")

screg <- hind.df %>% select(starts_with("name1_"), age, h18, nps, nc9618, unicount, frac1, proportion_female)

screg2 <- screg

screg2$h18 <- log(screg2$h18)

logbase <- lm(h18 ~ age + nc9618 + nps + unicount + frac1 + proportion_female, data=screg2) 

summary(logbase)

logmodel1 <- lm(h18 ~ .-name1_Physiology, data=screg2)

summary(logmodel1)

AIC(logbase, logmodel1)

anova(logbase, logmodel1)

#We can also run mlm

scmlm <- hind.df %>% select(name1, age, h18, nps, nc9618, unicount, frac1, proportion_female)

scmlm2 <- scmlm

scmlm2$lh18 <- log(screg2$h18)

library(lmerTest)

model0_fit <- lmer(formula = lh18 ~ 1 + (1|name1), 
                   data=scmlm2)

summary(model0_fit)

VarCorr(model0_fit)

RandomEffects <- as.data.frame(VarCorr(model0_fit))
RandomEffects

ICC_between <- RandomEffects[1,4]/(RandomEffects[1,4]+RandomEffects[2,4]) 
ICC_between

scmlm2$frac1_scale <- scmlm2$frac1*100
scmlm2$propfem_scale <- scmlm2$proportion_female*100


model2 <- lmer(formula = lh18 ~ age + nc9618 + nps + unicount + frac1_scale + propfem_scale + (1|name1), 
               data=scmlm2, REML=FALSE)

summary(model2)



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



