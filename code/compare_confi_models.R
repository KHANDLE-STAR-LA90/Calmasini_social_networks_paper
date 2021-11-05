#*******************************************
#* Comparing models with varying thresholds 
#* for contact with a confidant
#*******************************************

khandle1 <- readRDS('datasets/khandle1.RDS')
khandle1_long <- readRDS('datasets/khandle1_long.RDS')

khandle1$confi_presence <- ifelse(khandle1$W1_CONFIDANTE == 1, 1, 0)
khandle1_long$confi_presence <- ifelse(khandle1_long$W1_CONFIDANTE == 1, 1, 0)

# function to extract coefs

make_table <- function(model){
  coef <- summary(model)$coef[2]
  confint <- data.frame(confint(model))
  
  colnames(confint) = c("conf.low", "conf.high")
  
  conf.low <- confint[4, "conf.low"] %>% round(3)
  conf.high  <- confint[4, "conf.high"] %>% round(3)
  coef <- coef %>% round(3)
  coef_ci <- paste0 (coef, " (", conf.low, ", ", conf.high, ")")
}

# overall models
m_presence <- lmerTest::lmer(
  cognitive_score ~ confi_presence + r_d + confi_presence:asian_vrmemz + 
    confi_presence:latin_vrmemz + poly(years_over_65, 2, raw = TRUE) + 
    W1_D_GENDER + W1_EDU_new_c + (1|STUDYID), 
  REML = FALSE, data = khandle1_long)

m_day <- lmerTest::lmer(
  cognitive_score ~ confidante_bin + r_d + confidante_bin:asian_vrmemz + 
    confidante_bin:latin_vrmemz + poly(years_over_65, 2, raw = TRUE) + 
    W1_D_GENDER + W1_EDU_new_c + (1|STUDYID), 
  REML = FALSE, data = khandle1_long)

m_week <- lmerTest::lmer(
  cognitive_score ~ confi_week_al + r_d + confi_week:asian_vrmemz + 
    confi_week:latin_vrmemz + poly(years_over_65, 2, raw = TRUE) + 
    W1_D_GENDER + W1_EDU_new_c + (1|STUDYID), 
  REML = FALSE, data = khandle1_long)

m_monthly <- lmerTest::lmer(
  cognitive_score ~ confi_month_al + r_d + confi_month:asian_vrmemz + 
    confi_month:latin_vrmemz + poly(years_over_65, 2, raw = TRUE) + 
    W1_D_GENDER + W1_EDU_new_c + (1|STUDYID), 
  REML = FALSE, data = khandle1_long)

m_year_mult <- lmerTest::lmer(
  cognitive_score ~ confi_year_mult_al + r_d + confi_year_mult:asian_vrmemz + 
    confi_year_mult:latin_vrmemz + poly(years_over_65, 2, raw = TRUE) + 
    W1_D_GENDER + W1_EDU_new_c + (1|STUDYID), 
  REML = FALSE, data = khandle1_long)

m_year <- lmerTest::lmer(
  cognitive_score ~ confi_year_al + r_d + confi_year:asian_vrmemz + 
    confi_year:latin_vrmemz + poly(years_over_65, 2, raw = TRUE) + 
    W1_D_GENDER + W1_EDU_new_c + (1|STUDYID), 
  REML = FALSE, data = khandle1_long)

# models by race
tables_confi <- list()
for(i in c("White", "Black", "LatinX", "Asian")){
  for(j in c("confi_presence", "confidante_bin", 
             "confi_week_al", "confi_month_al", "confi_year_mult_al")){
    if(i == "Asian"){
      formula <- as.formula(
        paste("cognitive_score ~", j, "+", j, 
        ":asian_vrmemz + r_d + poly(years_over_65, 2, raw = TRUE) + W1_D_GENDER + W1_EDU_new_c + (1|STUDYID)"))
      
      mod_main_confi <- lmerTest::lmer(
        formula, 
        REML = FALSE, 
        data = khandle1_long, 
        subset = (W1_D_RACE_SUMMARY == i))
      
    } else if (i == "LatinX") {
      formula <- as.formula(
        paste("cognitive_score ~", j, "+", j, 
              ":latin_vrmemz + r_d + poly(years_over_65, 2, raw = TRUE) + W1_D_GENDER + W1_EDU_new_c + (1|STUDYID)"))
      
      mod_main_confi <- lmerTest::lmer(
        formula, 
        REML = FALSE, 
        data = khandle1_long, 
        subset = (W1_D_RACE_SUMMARY == i))
    } else {
      formula <- as.formula(
        paste("cognitive_score ~", j,
              "+ r_d + poly(years_over_65, 2, raw = TRUE) + W1_D_GENDER + W1_EDU_new_c + (1|STUDYID)"))
      
      mod_main_confi <- lmerTest::lmer(
        formula,
        REML = FALSE, 
        data = khandle1_long, 
        subset = (W1_D_RACE_SUMMARY == i))
    }
    tables_confi[[i]][j] <- make_table(mod_main_confi)
  }
}

m_presence_tab <- make_table(m_presence)
m_day_tab <- make_table(m_day)
m_week_tab <- make_table(m_week)
m_monthly_tab <- make_table(m_monthly)
m_year_mult_tab <- make_table(m_year_mult)

tab_supp <- tribble(
  ~mod, ~presence, ~daily, ~weekly, ~monthly, ~mult_years,
  "Overall", m_presence_tab, m_day_tab, m_week_tab, m_monthly_tab, m_year_mult_tab,
  "Asian", tables_confi$Asian[[1]], tables_confi$Asian[[2]], tables_confi$Asian[[3]], tables_confi$Asian[[4]], tables_confi$Asian[[5]],
  "Black", tables_confi$Black[[1]], tables_confi$Black[[2]], tables_confi$Black[[3]], tables_confi$Black[[4]], tables_confi$Black[[5]],
  "Latino", tables_confi$LatinX[[1]], tables_confi$LatinX[[2]], tables_confi$LatinX[[3]], tables_confi$LatinX[[4]], tables_confi$LatinX[[5]],
  "White", tables_confi$White[[1]], tables_confi$White[[2]], tables_confi$White[[3]], tables_confi$White[[4]], tables_confi$White[[5]]
)

#---- binary thresholds -----

# overall models
m_presence <- lmerTest::lmer(
  cognitive_score ~ confi_presence + r_d + confi_presence:asian_vrmemz + 
    confi_presence:latin_vrmemz + poly(years_over_65, 2, raw = TRUE) + 
    W1_D_GENDER + W1_EDU_new_c + (1|STUDYID), 
  REML = FALSE, data = khandle1_long)

m_day <- lmerTest::lmer(
  cognitive_score ~ confidante_bin + r_d + confidante_bin:asian_vrmemz + 
    confidante_bin:latin_vrmemz + poly(years_over_65, 2, raw = TRUE) + 
    W1_D_GENDER + W1_EDU_new_c + (1|STUDYID), 
  REML = FALSE, data = khandle1_long)

m_week <- lmerTest::lmer(
  cognitive_score ~ confi_week + r_d + confi_week:asian_vrmemz + 
    confi_week:latin_vrmemz + poly(years_over_65, 2, raw = TRUE) + 
    W1_D_GENDER + W1_EDU_new_c + (1|STUDYID), 
  REML = FALSE, data = khandle1_long)

m_monthly <- lmerTest::lmer(
  cognitive_score ~ confi_month + r_d + confi_month:asian_vrmemz + 
    confi_month:latin_vrmemz + poly(years_over_65, 2, raw = TRUE) + 
    W1_D_GENDER + W1_EDU_new_c + (1|STUDYID), 
  REML = FALSE, data = khandle1_long)

m_year_mult <- lmerTest::lmer(
  cognitive_score ~ confi_year_mult + r_d + confi_year_mult:asian_vrmemz + 
    confi_year_mult:latin_vrmemz + poly(years_over_65, 2, raw = TRUE) + 
    W1_D_GENDER + W1_EDU_new_c + (1|STUDYID), 
  REML = FALSE, data = khandle1_long)

m_year <- lmerTest::lmer(
  cognitive_score ~ confi_year + r_d + confi_year:asian_vrmemz + 
    confi_year:latin_vrmemz + poly(years_over_65, 2, raw = TRUE) + 
    W1_D_GENDER + W1_EDU_new_c + (1|STUDYID), 
  REML = FALSE, data = khandle1_long)

# models by race
tables_confi <- list()
for(i in c("White", "Black", "LatinX", "Asian")){
  for(j in c("confi_presence", "confidante_bin", 
             "confi_week", "confi_month", "confi_year_mult", "confi_year")){
    if(i == "Asian"){
      formula <- as.formula(
        paste("cognitive_score ~", j, "+", j, 
              ":asian_vrmemz + r_d + poly(years_over_65, 2, raw = TRUE) + W1_D_GENDER + W1_EDU_new_c + (1|STUDYID)"))
      
      mod_main_confi <- lmerTest::lmer(
        formula, 
        REML = FALSE, 
        data = khandle1_long, 
        subset = (W1_D_RACE_SUMMARY == i))
      
    } else if (i == "LatinX") {
      formula <- as.formula(
        paste("cognitive_score ~", j, "+", j, 
              ":latin_vrmemz + r_d + poly(years_over_65, 2, raw = TRUE) + W1_D_GENDER + W1_EDU_new_c + (1|STUDYID)"))
      
      mod_main_confi <- lmerTest::lmer(
        formula, 
        REML = FALSE, 
        data = khandle1_long, 
        subset = (W1_D_RACE_SUMMARY == i))
    } else {
      formula <- as.formula(
        paste("cognitive_score ~", j,
              "+ r_d + poly(years_over_65, 2, raw = TRUE) + W1_D_GENDER + W1_EDU_new_c + (1|STUDYID)"))
      
      mod_main_confi <- lmerTest::lmer(
        formula,
        REML = FALSE, 
        data = khandle1_long, 
        subset = (W1_D_RACE_SUMMARY == i))
    }
    a <- summary(mod_main_confi)
    print(a$devcomp$dims)
    tables_confi[[i]][j] <- make_table(mod_main_confi)
  }
}

m_presence_tab <- make_table(m_presence)
m_day_tab <- make_table(m_day)
m_week_tab <- make_table(m_week)
m_monthly_tab <- make_table(m_monthly)
m_year_mult_tab <- make_table(m_year_mult)
m_year_tab <- make_table(m_year)

tab_supp <- tribble(
  ~mod, ~presence, ~daily, ~weekly, ~monthly, ~mult_years, ~once_year, 
  "Overall", m_presence_tab, m_day_tab, m_week_tab, m_monthly_tab, m_year_mult_tab, m_year_tab,
  "Asian", tables_confi$Asian[[1]], tables_confi$Asian[[2]], tables_confi$Asian[[3]], tables_confi$Asian[[4]], tables_confi$Asian[[5]], tables_confi$Asian[[6]],
  "Black", tables_confi$Black[[1]], tables_confi$Black[[2]], tables_confi$Black[[3]], tables_confi$Black[[4]], tables_confi$Black[[5]], tables_confi$Black[[6]],
  "Latino", tables_confi$LatinX[[1]], tables_confi$LatinX[[2]], tables_confi$LatinX[[3]], tables_confi$LatinX[[4]], tables_confi$LatinX[[5]], tables_confi$LatinX[[6]],
  "White", tables_confi$White[[1]], tables_confi$White[[2]], tables_confi$White[[3]], tables_confi$White[[4]], tables_confi$White[[5]], tables_confi$White[[6]],
)

































