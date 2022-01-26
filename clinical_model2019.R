climlm <- centmlm %>% filter(sm_field=="Clinical Medicine")

dis_count <- as.data.frame(table(climlm$discipline))

dis_count <- mutate(dis_count, clidelvar = ifelse(Freq<25, 1, 0))

dis_count <- dis_count %>% rename(discipline = Var1)

dis_count <- dis_count %>% rename(clifreq = Freq)


climlm <- left_join(climlm, dis_count)

climlm <- climlm %>% filter(clidelvar==0)


climodelnull <- lmer(formula = h19 ~ 1 + (1|discipline), 
                  data=climlm, REML=FALSE)

RandomEffects <- as.data.frame(VarCorr(modelnull))
RandomEffects

ICC_between <- RandomEffects[1,4]/(RandomEffects[1,4]+RandomEffects[2,4]) 
ICC_between


climodel2 <- lmer(formula = h19 ~ propfem_scale.cwc + prop_sole_scale.cwc + cites_scale.cwc + 
                 unicount.cwc + field_frac_scale.cwc  + age.cwc + (1|discipline), 
               data=climlm, REML=FALSE)

summary(climodelnull)

summary(climodel2)



#https://biologyforfun.wordpress.com/2017/04/03/interpreting-random-effects-in-linear-mixed-effect-models/

rando1 <- ranef(climodel2)$discipline

rando1$estmean <- rando1$`(Intercept)`+49.3

rando1$discipline <- row.names(rando1)

rando1 <- rando1[order(rando1$estmean),]

rando1 <- mutate(rando1, nid=row_number())

nseq <- c(1,2,3,4,5,22,23,24,25,26, 40, 41, 42,43,44)

rando2 <- filter(rando1, rando1$nid%in%nseq)

rando2$clrs <- c("blue", "blue", "blue", "blue", "blue", "green", "green", "green", "green", "green", 
                 "red", "red", "red", "red", "red")


#Lollipops

ggplot(rando2, aes(x= reorder(discipline, -estmean), y=estmean, group=clrs, color=clrs)) +
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
