#***********************************
#* This code makes table 3 in the MS
#************************************

#---- integration score models ----
# complete case
table1_long <- readRDS('code/output/tablem1_long.RDS')
tables_main_covars <- readRDS('code/output/tables_main_covars_long.RDS')
table2_long <- readRDS('code/output/tablem2_long.RDS')
tables_all_covars <- readRDS('code/output/tables_all_covars_long.RDS')

# imputed
imputed_int_score_dt <- readRDS('code/output/imputed_int_score_dt.RDS')
imputed_int_score_dt_main <- readRDS('code/output/imputed_int_score_dt_main.RDS')

# make the table
int_score_main_tab <- rbind(table1_long,
                            tables_main_covars$Asian,
                            tables_main_covars$Black,
                            tables_main_covars$LatinX,
                            tables_main_covars$White)

int_score_all_tab <- rbind(table2_long,
                           tables_all_covars$Asian, 
                           tables_all_covars$Black,
                           tables_all_covars$LatinX,
                           tables_all_covars$White)

impute_tab <- rbind(imputed_int_score_dt$full_sample,
                    imputed_int_score_dt$asian,
                    imputed_int_score_dt$black, 
                    imputed_int_score_dt$latinx, 
                    imputed_int_score_dt$white)

impute_tab_main_covars <- rbind(imputed_int_score_dt_main$full_sample,
                                imputed_int_score_dt_main$asian,
                                imputed_int_score_dt_main$black,
                                imputed_int_score_dt_main$latinx,
                                imputed_int_score_dt_main$white)

table_int_score_long <- cbind(int_score_main_tab, impute_tab_main_covars[,-2], int_score_all_tab[,-1], impute_tab[,-2])

kable(table_int_score_long, format = "html", booktabs = TRUE, align = c("l", "c", "c", "c", "c"),
      col.names = c(" ", "Core covariates", "Core - imputed", "Enhanced covariates", "Enhanced imputated"), escape = FALSE) %>%
  kable_styling() %>%
  column_spec(1, width = "4cm") %>%
  column_spec(2, width = "3.5cm")%>%
  column_spec(3, width = "3.5cm")%>%
  #column_spec(2, width = "4cm") %>%
  pack_rows("Overall", 1, 3, bold = TRUE) %>%
  pack_rows("Asian", 4, 6, bold = TRUE) %>%
  pack_rows("Black", 7, 9, bold = TRUE) %>%
  pack_rows("Latinx", 10, 12, bold = TRUE)%>%
  pack_rows("White", 13, 15, bold = TRUE)%>%
  readr::write_file("/Users/ccalmasini/Desktop/Camilla KHANDLE/social_networks_paper/Tables/w1w2_regressions_int_score.html")

#---- confidante models -----
# complete case
table1_confi <- readRDS('code/output/table1_confi_long.RDS')
tables_main_confi <- readRDS('code/output/tables_main_confi_long.RDS')
table2_confi <- readRDS('code/output/table2_confi_long.RDS')
tables_all_confi <- readRDS('code/output/tables_all_confi_long.RDS')

# imputed
imputed_coef_dt <- readRDS('code/output/imputed_coef_dt.RDS')
imputed_coef_dt_main <- readRDS('code/output/imputed_coef_dt_main.RDS')

#this makes the final table for all confidante models
table_confi_main <- rbind(table1_confi,
                          tables_main_confi$Asian,
                          tables_main_confi$Black,
                          tables_main_confi$LatinX,
                          tables_main_confi$White)

table_confi_all <- rbind(table2_confi,
                         tables_all_confi$Asian,
                         tables_all_confi$Black,
                         tables_all_confi$LatinX,
                         tables_all_confi$White)

impute_tab_confi <- rbind(imputed_coef_dt$full_sample,
                          imputed_coef_dt$asian,
                          imputed_coef_dt$black, 
                          imputed_coef_dt$latinx, 
                          imputed_coef_dt$white)

impute_tab_confi_main <- rbind(imputed_coef_dt_main$full_sample,
                               imputed_coef_dt_main$asian,
                               imputed_coef_dt_main$black,
                               imputed_coef_dt_main$latinx,
                               imputed_coef_dt_main$white)

table_confi_long <- cbind(table_confi_main, impute_tab_confi_main[,-2], 
                          table_confi_all[,-1], impute_tab_confi[, -2])

kable(table_confi_long, 
      format = "html", 
      booktabs = TRUE, 
      align = c("l", "c", "c"),
      col.names = linebreak(c(" ", "Core covariates", 
                              "Core covariates - imputed", 
                              "Enhanced covariates", "Enhanced - Imputed")), 
      escape = FALSE) %>%
  kable_styling() %>%
  column_spec(1, width = "4cm") %>%
  column_spec(2, width = "3.5cm")%>%
  column_spec(3, width = "3.5cm")%>%
  #column_spec(2, width = "4cm") %>%
  pack_rows("Overall", 1, 3, bold = TRUE) %>%
  pack_rows("Asian", 4, 6, bold = TRUE) %>%
  pack_rows("Black", 7, 9, bold = TRUE) %>%
  pack_rows("Latinx", 10, 12, bold = TRUE)%>%
  pack_rows("White", 13, 15, bold = TRUE)%>%
  readr::write_file("/Users/ccalmasini/Desktop/Camilla KHANDLE/social_networks_paper/Tables/w1w2_regressions_confi.html")




