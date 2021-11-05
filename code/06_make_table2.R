#***********************************
#* This code makes table 2 in the MS
#************************************

#---- integration score models ----
# complete case
table1 <- readRDS('code/output/tablem1.RDS')
tables_main_covars <- readRDS('code/output/tables_main_covars.RDS')
table2 <- readRDS('code/output/tablem2.RDS')
tables_all_covars <- readRDS('code/output/tables_all_covars.RDS')

# imputed
imputed_coef_main <- readRDS('code/output/imputed_coef_main.RDS')
imputed_coef_CI <- readRDS('code/output/imputed_coef_CI.RDS')

# make the table
int_score_main_tab <- rbind(table1,
                            tables_main_covars$Asian,
                            tables_main_covars$Black,
                            tables_main_covars$LatinX,
                            tables_main_covars$White)

int_score_all_tab <- rbind(table2,
                           tables_all_covars$Asian, 
                           tables_all_covars$Black,
                           tables_all_covars$LatinX,
                           tables_all_covars$White)

int_score_main_impute <- t(data.frame(imputed_coef_main))
int_score_impute <- t(data.frame(imputed_coef_CI))

table_int_score <- cbind(int_score_main_tab, int_score_main_impute, int_score_all_tab, int_score_impute)
rownames(table_int_score) <- c("Overall", "Asian", "Black", "LatinX", "White")

kable(table_int_score, format = "html", booktabs = FALSE, align = c("l", "c", "c"),
      col.names = c("Core covariates beta (95% CI)", "Core covariates - imputed", 
                    "Enhanced covariates beta (95% CI)", "Enhanced covariates imputed beta (95% CI)"), escape = FALSE) %>%
  kable_styling() %>%
  column_spec(1, width = "4cm") %>%
  column_spec(2, width = "3.5cm")%>%
  pack_rows(" ", 1, 1) %>%
  pack_rows(" ", 2, 2) %>%
  pack_rows(" ", 3, 3) %>%
  pack_rows(" ", 4, 4) %>%
  pack_rows(" ", 5, 5) %>%
  readr::write_file("Tables/w1_regressions_int_score.html")

#---- confidante models ----

# complete case
table1_confi <- readRDS('code/output/table1_confi.RDS')
tables_main_covars_confi <- readRDS('code/output/tables_main_covars_confi.RDS')
table2_confi <- readRDS('code/output/table2_confi.RDS')
tables_all_covars_confi <- readRDS('code/output/tables_all_covars_confi.RDS')

# imputed
imputed_coef_confi_main <- readRDS('code/output/imputed_coef_confi_main.RDS')
imputed_coef_CI_confi <- readRDS('code/output/imputed_coef_CI_confi.RDS')

# make the table
confi_main_tab <- rbind(table1_confi,
                        tables_main_covars_confi$Asian,
                        tables_main_covars_confi$Black,
                        tables_main_covars_confi$LatinX,
                        tables_main_covars_confi$White)

confi_all_tab <- rbind(table2_confi,
                       tables_all_covars_confi$Asian, 
                       tables_all_covars_confi$Black,
                       tables_all_covars_confi$LatinX,
                       tables_all_covars_confi$White)

confi_impute_main <- t(data.frame(imputed_coef_confi_main))
confi_impute <- t(data.frame(imputed_coef_CI_confi))

table_confi <- cbind(confi_main_tab, confi_impute_main, confi_all_tab, confi_impute)

rownames(table_confi) <- c("Overall", "Asian", "Black", "LatinX", "White")

#write.csv(table_confi, "/Users/ccalmasini/Desktop/Camilla KHANDLE/social_networks_paper/Tables/w1_regressions_confi.csv")

kable(table_confi, format = "html", booktabs = FALSE, align = c("l", "c", "c"),
      col.names = c("Core covariates beta (95% CI)", "Core - imputed", "Enhanced covariates beta (95% CI)", "Enhanced - imputed"), escape = FALSE) %>%
  kable_styling() %>%
  column_spec(1, width = "4cm") %>%
  column_spec(2, width = "3.5cm")%>%
  pack_rows(" ", 1, 1) %>%
  pack_rows(" ", 2, 2) %>%
  pack_rows(" ", 3, 3) %>%
  pack_rows(" ", 4, 4) %>%
  pack_rows(" ", 5, 5) %>%
  readr::write_file("Tables/w1_regressions_confi.html")





