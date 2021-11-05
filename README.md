# social_networks_paper

To check:

* recoding covariates longitudinal.Rmd
	* This should be the same as the one you already checked, I only added the wave 2 SENAS measures (which should also be scaled) and wave 2 age, and also coded retirement status for a covariate.

* mixed models.Rmd
	* This does the cross-sectional analysis
	* This should also be similar to before, I only added retirement status and the imputation using the core covariates

* mixed models longitudinal.Rmd
	* This does the longitudinal analysis

* make_table1.R
	* This just makes table1 that I put in the paper

General things to make sure:
* For int_score:
	* All full-sample models and models stratified by race = "Asian" should include an interaction between int_score and asian_vrmemz

* For confidante:
	* Full sample models should include interaction between confidante_bin and asian_vrmemz and confidante_bin and latin_vrmemz
	* Models stratified by race = "Asian" should include interaction between confidante_bin and asian_vrmemz
	* Models stratified by race = "LatinX" should include interaction between confidante_bin and latino_vrmemz

* For longitudinal analyses: same as above, but all models should also have an interaction between exposure and years_over_65


