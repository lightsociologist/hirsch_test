dis_count2 <- as.data.frame(table(schous$name2))

dis_count2 <- mutate(dis_count2, delvar2 = ifelse(Freq<50, 1, 0))

dis_count2 <- dis_count2 %>% rename(name2 = Var1)


schous <- left_join(schous, dis_count2)

schous <- schous %>% filter(delvar2==0)


# select variables
fdlca <- fdlca+1

names(fdlca)<-make.names(names(fdlca),unique = TRUE)

# define function
f<-as.formula(paste("cbind(", paste0(names(fdlca), collapse = ","),")~ 1"))

library(poLCA)

#------ run a sequence of models with 1-10 classes and print out the model with the lowest BIC
max_II <- -100000
min_bic <- 100000
for(i in 2:10){
  lc <- poLCA(f, fdlca, nclass=i, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model<-lc
  }
}    	
LCA_best_model

lc <- poLCA(f, fdlca, nclass=5, maxiter=3000, 
            tol=1e-5, na.rm=FALSE,  
            nrep=10, verbose=TRUE, calc.se=TRUE)

check <- dummy_cols(schous, select_columns = "name2")
