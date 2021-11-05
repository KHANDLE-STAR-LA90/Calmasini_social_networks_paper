#**************************
#* Code for figure 2 in ms
#**************************

library(ggplot2)
library(dplyr)
library(viridis)
library(forcats)

final_dat <- read.csv("social_nets_dat_both_waves.csv")

# making a plot of proportions of people for each race who received a score of 1 
# for each support measure 

# drop NAs
dat_plots <- final_dat %>%
  drop_na(W1_EDU_new, income_num, ADL_IADL, tot_drinks_week, retirement_stat)

# make tables of counts for each support measure
mar <- dat_plots %>%
  group_by(W1_D_RACE_SUMMARY, "support" = marital_bin) %>%
  tally() %>%
  mutate(proportion = n/sum(n)) %>%
  filter(support == 1) %>%
  mutate(support = "marital status")

vol <- dat_plots %>%
  group_by(W1_D_RACE_SUMMARY, support = volunteer_bin) %>%
  tally() %>%
  mutate(proportion = n/sum(n)) %>%
  filter(support == 1) %>%
  mutate(support = "volunteer")

rel <- dat_plots %>%
  group_by(W1_D_RACE_SUMMARY, support = relatives_bin) %>%
  tally() %>%
  mutate(proportion = n/sum(n)) %>%
  filter(support == 1) %>%
  mutate(support = "relatives")

child <- dat_plots %>%
  group_by(W1_D_RACE_SUMMARY, support = children_bin) %>%
  tally() %>%
  mutate(proportion = n/sum(n)) %>%
  filter(support == 1) %>%
  mutate(support = "children")

friends <- dat_plots %>%
  group_by(W1_D_RACE_SUMMARY, support = friends_bin) %>%
  tally() %>%
  mutate(proportion = n/sum(n)) %>%
  filter(support == 1) %>%
  mutate(support = "friends")

conf <- dat_plots %>%
  group_by(W1_D_RACE_SUMMARY, support = confidante_bin) %>%
  tally() %>%
  mutate(proportion = n/sum(n)) %>%
  filter(support == 1) %>%
  mutate(support = "confidante")

# combined all dfs made above
dat_plots2 <- rbind(mar, vol, rel, child, friends, conf)

# relevel so order looks correct in plot
dat_plots2$support <- factor(dat_plots2$support, 
                             levels=c("marital status", "volunteer", "relatives", "children", 
                                      "friends", "confidante"))

dat_plots2$W1_D_RACE_SUMMARY <- as.factor(dat_plots2$W1_D_RACE_SUMMARY)
dat_plots2$W1_D_RACE_SUMMARY <-  fct_recode(dat_plots2$W1_D_RACE_SUMMARY,
                                            "Latino" = "LatinX")

# figure 2
png("/Users/ccalmasini/Desktop/Camilla KHANDLE/social_networks_paper/social_nets_plot_v5.png", units="in", width=10, height=6, res = 300)

ggplot(dat_plots2, aes(x = support, y = proportion, fill = W1_D_RACE_SUMMARY))+
  geom_col(width = 0.7, position = position_dodge(width = 0.7))+
  theme_minimal()+
  xlab("Measure of social integration")+
  ylab("Proportion of participants")+
  labs(fill='Race')+
  scale_fill_viridis(discrete = TRUE, option = "D")  +
  #scale_fill_brewer(palette = "RdBu")+
  scale_x_discrete(labels = c("Married/Partnered", "Volunteered\nin the past\n12 months",
                              "At least monthly\ncontact wih\nrelatives",
                              "At least monthly\ncontact with\nchildren",
                              "At least monthly\ncontact with\n friends",
                              "At least daily \ninteraction \nwith a confidante"))
dev.off()

# AAIC poster plot (just changing sizing)
png("/Users/ccalmasini/Desktop/Camilla KHANDLE/AAIC/poster/social_nets_plot_v5.png", units="in", width=8, height=6, res = 300)
ggplot(dat_plots2, aes(x = support, y = proportion, fill = W1_D_RACE_SUMMARY))+
  geom_col(width = 0.7, position = position_dodge(width = 0.7))+
  theme_minimal()+
  xlab("Measure of social integration")+
  ylab("Proportion of participants")+
  labs(fill='Race')+
  scale_fill_viridis(discrete = TRUE, option = "D")  +
  #scale_fill_brewer(palette = "RdBu")+
  scale_x_discrete(labels = c("Married/Partnered", "Volunteered\nin the past\n12 months",
                              "At least monthly\ncontact wih\nrelatives",
                              "At least monthly\ncontact with\nchildren",
                              "At least monthly\ncontact with\n friends",
                              "At least daily \ninteraction \nwith a confidante"))+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=12)) 
dev.off()