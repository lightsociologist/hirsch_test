
climlm <- centmlm %>% filter(sm_field=="Clinical Medicine")


climodelnull <- lmer(formula = h19 ~ 1 + (1|discipline), 
                  data=climlm, REML=FALSE)

icc_specs(climodelnull) %>%
  mutate_if(is.numeric, round, 2)

climodel2 <- lmer(formula = h19 ~ propfem_scale + prop_sole_scale +  
                 unicount_scale + field_frac_scale  + age_scale + (1|discipline), 
               data=climlm, REML=FALSE)

climodel3 <- lmer(formula = h19 ~ propfem_scale + prop_sole_scale +  
                 unicount_scale + field_frac_scale  + age_scale + (1 + prop_sole_scale + propfem_scale|discipline), 
               data=climlm, REML=FALSE)

summary(climodelnull)

summary(climodel2)

summary(climodel3)

ranova(climodel3)

anova(climodel2, climodel3)

confint(climodel3)

clidesc <- climlm %>% ungroup() %>% select(h19, propfem_scale, prop_sole_scale, nc9619, unicount, field_frac_scale, age)

clidesc <- as.data.frame(clidesc)

stargazer(clidesc, type = "html", digits=1, summary.stat = c("n", "mean", "sd"), covariate.labels=c("Hirsch Index", "% Female Name", 
                                                                                                 "% Sole Author Publications", "# of Citations", "University Count", "% Disciplinarity", "Age"), 
          title="Table 1b: Descriptive Statistics (Clinical Data)", out="table1b.html")



#https://biologyforfun.wordpress.com/2017/04/03/interpreting-random-effects-in-linear-mixed-effect-models/

clirando1 <- ranef(climodel2)$discipline

clirando1$estmean <- clirando1$`(Intercept)`+49.3

clirando1$discipline <- row.names(clirando1)

clirando1 <- clirando1[order(clirando1$estmean),]

clirando1 <- mutate(clirando1, nid=row_number())

nseq <- c(1,2,3,4,5,22,23,24,25,26, 40, 41, 42,43,44)

clirando2 <- filter(clirando1, clirando1$nid%in%nseq)

clirando2$clrs <- c("blue", "blue", "blue", "blue", "blue", "green", "green", "green", "green", "green", 
                 "red", "red", "red", "red", "red")


#Lollipops

ggplot(clirando2, aes(x= reorder(discipline, -estmean), y=estmean, group=clrs, color=clrs)) +
  geom_segment( aes(x=reorder(discipline, -estmean), xend=discipline, y=0, yend=estmean), color="skyblue") +
  geom_point(size=3, alpha=0.6) +
  geom_hline(yintercept = 49.3, color="orange") +
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

stargazer(modelnull, model2, type="html", style="asr", out="19table2.html",
          title = "Table 2: A Multilevel Model of the Hirsch Index",
          covariate.labels = c("Percentage Female Name","Percentage Sole Author Publications",
                               "# of Citations (1996-2019)", "University Count (# of Citation Allstars)", 
                               "Disciplinarity Score", "Age (Years Publishing)"),
          notes="<small>Data: Ioannidis et al. (2019)</small>",
          dep.var.labels="Hirsch Index",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001),
          model.names = FALSE,
          column.labels = c("Model 1: Null Model", "Model 2"))
