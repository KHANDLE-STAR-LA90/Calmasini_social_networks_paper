#***********************************
#* This code makes table 2 in the MS
#* (using 1343 complete cases)
#************************************

library(tableone)
library(kableExtra)

final_dat <- read.csv("/Users/ccalmasini/Desktop/Camilla KHANDLE/social_networks_paper/social_nets_dat.csv")

#centering education at 12 and age at 65
#turning variables into factors
#dropping nas for complete case analysis
khandle1_tab <- final_dat %>%
  mutate(W1_EDU_new_c = W1_EDU_new - 12,
         W1_D_GENDER = factor(case_when(W1_D_GENDER == 1 ~ "Male",
                                        W1_D_GENDER == 2 ~ "Female")),
         W1_SMK = ifelse(W1_SMK == 0, "No", "Yes"),
         years_over_65 = W1_INTERVIEW_AGE - 65, 
         int_score = ifelse(int_score == 0, 1, int_score),
         income_num = income_num/1000) %>%
  drop_na(W1_EDU_new_c, income_num, ADL_IADL, tot_drinks_week, retirement_stat)

## Vector of variables to summarize
dput(names(khandle1_tab))
myVars <- c("W1_INTERVIEW_AGE", "W1_D_GENDER", "W1_D_RACE_SUMMARY", "W1_EDU_new", "income_num", "tot_drinks_week", "int_score", "ADL_IADL", "retirement_stat")
## Vector of categorical variables that need transformation
catVars <- c("W1_D_GENDER", "W1_D_RACE_SUMMARY", "retirement_stat")
## Create a TableOne object
table1 <- CreateTableOne(vars = myVars, data = khandle1_tab, 
                                factorVars = catVars, strata = "int_score", 
                                test = FALSE,
                                addOverall = TRUE)

kableone(table1, format = "html", booktabs=T) %>%
  kable_styling(latex_options=c("striped", "HOLD_position"), font_size=10) %>%
  add_header_above(c(" ", "Integration Score" = 7)) %>%
  readr::write_file("/Users/ccalmasini/Desktop/Camilla KHANDLE/social_networks_paper/Tables/table1.html")
