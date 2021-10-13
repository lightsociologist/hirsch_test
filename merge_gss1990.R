#https://www.nature.com/articles/d41586-019-02479-7


library(readr)
library(dplyr)
library(fastDummies)
library(jtools)
library(data.table)
library(GGally)

scholar_data_2018 <- read_csv("data/scholar_data_2018.csv")

#keep only usa based scholars for the discipline data to make sense

schous <- scholar_data_2018 %>% filter(cntry=="usa")

#Keep relatively currently active scholars

schous <- schous %>% filter(lastyr>2015)

schous <- schous %>% filter(firstyr>1970)


#let's grab discipline data

dis_count <- as.data.frame(table(schous$name1))

#let's remove rare disciplines (those less than 50 scholars)

dis_count <- mutate(dis_count, delvar = ifelse(Freq<50, 1, 0))

dis_count <- dis_count %>% rename(discipline = Var1)

dis_cnt2 <- dis_count %>% filter(delvar==0)

#let's save discipline out to merge with graduates survey

write_csv(dis_cnt2, "~/Documents/GitHub/hirsch_test/data/dis_names.csv")

#we can bring in the merge file (merged by looking up the discipline names here:
#https://ncsesdata.nsf.gov/gradpostdoc/2018/html/gss18-dt-taba016.html)

gsscross <- read_csv("data/disnames_gss_crosswalk.csv")

#and the graduates data

grad.df <- read_csv("data/gss1990.csv")

#just keep the gss code and the race and sex variables for now

#grad.df <- grad.df[-c(1:18, 20)]

#sum across all remaining variables by code

grad_sum <- grad.df %>% group_by(gss_code) %>% summarise_each(list(sum))

#tabulate us students

grad_sum$total_us <- grad_sum$dr_ft_tot_all_races_v - grad_sum$dr_ft_tot_forgn_v

#tabulate race

grad_sum$perblack <- grad_sum$dr_ft_tot_black_v/grad_sum$total_us

grad_sum$perind <- grad_sum$dr_ft_tot_indian_v/grad_sum$total_us

grad_sum$perasian <- grad_sum$dr_ft_tot_asian_v/grad_sum$total_us

grad_sum$perpacific <- grad_sum$dr_ft_tot_pacific_v/grad_sum$total_us

grad_sum$perwhite <- grad_sum$dr_ft_tot_white_v/grad_sum$total_us

grad_sum$perhisp <- grad_sum$dr_ft_tot_hisp_v/grad_sum$total_us

grad_sum$permulti <- grad_sum$dr_ft_tot_multi_v/grad_sum$total_us

grad_sum$perunk <- grad_sum$dr_ft_tot_unk_v/grad_sum$total_us

#tabulate gender

grad_sum$num_uswomen <- grad_sum$dr_ft_wmen_all_races_v - grad_sum$dr_ft_wmen_forgn_v

grad_sum$perwomen <- grad_sum$num_uswomen/grad_sum$total_us

#merge with crosswalk

grad_sum <- left_join(grad_sum, gsscross)


#merge with scholar data 

schous <- left_join(schous, grad_sum)

schous <- schous %>% filter(!is.na(name1))

schous <- schous %>% filter(!is.na(perwomen))





schous$age <- schous$lastyr-schous$firstyr

uni_count <- as.data.frame(table(schous$inst_name))

uni_count <- uni_count %>% rename(inst_name=Var1, unicount=Freq)

schd <- left_join(schous, uni_count)

screg <- schd %>% select(age, name1, h18, nps, nc9618, unicount, gss_code, perwomen, perblack, perind,
                        perasian , perpacific, perwhite, perhisp, perunk, permulti, perwomen, total_us)



