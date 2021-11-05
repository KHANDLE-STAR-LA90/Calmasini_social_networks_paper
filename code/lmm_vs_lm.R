library(lmerTest)
library(ggplot2)

ggplot(khandle1_long, aes(x = r_d, y = cognitive_score, fill = W1_D_GENDER)) +
  geom_boxplot()+
  facet_grid(~confidante_bin) + 
  theme(axis.text = element_text(angle = 90))

ggplot(khandle1_long, aes(x = as.factor(confidante_bin), y = cognitive_score, fill = W1_D_GENDER)) +
  geom_boxplot()+
  #facet_grid(~confidante_bin) + 
  theme(axis.text = element_text(angle = 90))

ggplot(khandle1_long, aes(x = W1_D_GENDER, y = cognitive_score, fill = as.factor(confidante_bin))) +
  geom_boxplot()+
  facet_wrap(~r_d) + 
  theme(axis.text = element_text(angle = 90))

ggplot(khandle1_long, aes(x = as.factor(int_score), y = cognitive_score, fill = as.factor(W1_D_GENDER))) +
  geom_boxplot()+
  facet_wrap(~r_d) + 
  theme(axis.text = element_text(angle = 90))

# mixed model for confidante (coef 0.137)
m1 <- lmer(cognitive_score ~ confidante_bin + r_d + confidante_bin:asian_vrmemz + 
             confidante_bin:latin_vrmemz + poly(years_over_65, 2, raw = TRUE) + 
             W1_D_GENDER + W1_EDU_new_c + (1|STUDYID), REML = FALSE, data = khandle1_long
)

stargazer(summary(m1)$coef, type = "text", 
          title = "Orginal mixed effects model w/ Asian exec as ref group")


## using asian vrmem as ref  ( coef = -0.143 for asian vrmem)
m1.2 <- lmer(cognitive_score ~ confidante_bin + r_d + confidante_bin:relevel(asian_vrmemz, ref = 2) + 
             confidante_bin:latin_vrmemz + poly(years_over_65, 2, raw = TRUE) + 
             W1_D_GENDER + W1_EDU_new_c + (1|STUDYID), REML = FALSE, data = khandle1_long
)

stargazer(summary(m1.2)$coef, type = "text", 
          title = "Orginal mixed effects model w/ Asian exec as ref group")

# CI = (-0.314647393, 0.003) so ci includes coef from regression (-0.009)
ci1 <- confint(m1.2)
ci1

# linear model for asian vrmem (-0.009)
t <- khandle1_long %>%
  filter(W1_D_RACE_SUMMARY == "Asian" & cognitive_test == "vrmemz")

m2 <- lm(cognitive_score~confidante_bin + W1_D_GENDER + W1_EDU_new_c + 
           poly(years_over_65, 2, raw = TRUE), data = t)

stargazer(summary(m2)$coef, type = "text", title="Linear regression for effect of confidante on asian vrmem")

t2 <- khandle1_long %>%
  filter(W1_D_RACE_SUMMARY == "LatinX" & cognitive_test == "vrmemz")

m2.1 <- lm(cognitive_score~confidante_bin + W1_D_GENDER + W1_EDU_new_c + 
           poly(years_over_65, 2, raw = TRUE), data = t2)

stargazer(summary(m2.1)$coef, type = "text", 
          title="Linear regression for effect of confidante on asian vrmem")


# mixed model with all interactions -> same coef as linear model
m3 <- lmer(
     cognitive_score ~ (confidante_bin + W1_D_GENDER + poly(years_over_65, 2, raw = TRUE) + W1_EDU_new_c)*relevel(r_d, ref = "Asian_vrmemz") + (1|STUDYID), 
     REML = FALSE, data = khandle1_long
   )

stargazer(summary(m3)$coef, type = "text", 
          title = 'Mixed model with all interactions')

# removing age interaction -> coef stays the same

m4 <- lmerTest::lmer(
  cognitive_score ~ (confidante_bin + W1_D_GENDER + W1_EDU_new_c)*relevel(r_d, ref = "Asian_vrmemz") + poly(years_over_65, 2, raw = TRUE) + (1|STUDYID), 
  REML = FALSE, data = khandle1_long
)

stargazer(summary(m4)$coef, type = "text", title = 'Mixed model removing age interaction')

# removing education interaction -> coef changes a bit (-0.015 vs -0.009)
m5 <- lmerTest::lmer(
  cognitive_score ~ (confidante_bin + W1_D_GENDER)*relevel(r_d, ref = "Asian_vrmemz") + poly(years_over_65, 2, raw = TRUE) + W1_EDU_new_c + (1|STUDYID), 
  REML = FALSE, data = khandle1_long
)

stargazer(summary(m5)$coef, type = "text", title = 'Mixed model with confidante and race domain interaction and gender and race domain interaction')

# keeping only gender interaction (coef = -0.05)

m6 <- lmerTest::lmer(
  cognitive_score ~ confidante_bin + confidante_bin:asian_vrmemz + confidante_bin:latin_vrmemz + r_d*W1_D_GENDER + poly(years_over_65, 2, raw = TRUE) + W1_EDU_new_c + (1|STUDYID), 
  REML = FALSE, data = khandle1_long
)

stargazer(summary(m6)$coef, type = "text", title = 'Mixed model with gender interaction only')

#*********************
#* integration score
#*********************

# mixed model for integration score
m7 <- lmer(cognitive_score ~ int_score + r_d + int_score:asian_vrmemz + poly(years_over_65, 2, raw = TRUE) + 
             W1_D_GENDER + W1_EDU_new_c + (1|STUDYID), 
           REML = FALSE, data = khandle1_long)
stargazer(summary(m7)$coef, type = "text", title = 'Original mixed effects model')

# with asian vrmem as ref group (coef = -0.04 compared to -0.017)
m7.1 <- lmer(cognitive_score ~ int_score + r_d + int_score:relevel(asian_vrmemz, ref = 2) + poly(years_over_65, 2, raw = TRUE) + 
               W1_D_GENDER + W1_EDU_new_c + (1|STUDYID), 
             REML = FALSE, data = khandle1_long)

#CI (-0.10641785  0.03) #includes linear regression coef
cim7.1 <- confint(m7.1) 
cim7.1 

stargazer(summary(m7.1)$coef, type = "text", title = 'Original mixed effects model with asian vrmem as ref')

# mixed models with all interactions (coef = -0.017)
m8 <- lmer(cognitive_score ~ (int_score + poly(years_over_65, 2, raw = TRUE) + 
             W1_D_GENDER + W1_EDU_new_c) * relevel(r_d, ref = "Asian_vrmemz") + (1|STUDYID), 
           REML = FALSE, data = khandle1_long)
stargazer(summary(m8)$coef, type = "text", title = 'Mixed effects model with all interaction')

# linear regression for integration score (coef = -0.017)
m9 <- lm(cognitive_score ~ int_score + W1_D_GENDER + W1_EDU_new_c + poly(years_over_65, 2, raw = TRUE), data = t)
stargazer(summary(m9)$coef, type = "text", title = 'Linear regression')

# mixed model with race gender interaction (coef = -0.02)
m10 <- lmer(cognitive_score ~ int_score + r_d*W1_D_GENDER + int_score:asian_vrmemz + poly(years_over_65, 2, raw = TRUE) +
              W1_EDU_new_c + (1|STUDYID), 
            REML = FALSE, data = khandle1_long)
stargazer(summary(m10)$coef, type = "text", title = 'Linear regression with race domain x gender interaction')

anova(m10)
