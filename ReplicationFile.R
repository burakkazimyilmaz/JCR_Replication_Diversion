# Replication R File for "Pave the Way to Diversion: Increased Salience of Foreign Policy at the Times of Economic Hardships"
library(dplyr)
library(ggplot2)
library(patchwork)
library(stargazer)
library(mediation)
library(jtools)
library(lmtest)
library(AER)
library(interactions)

df <- read.csv("replication_data.csv")

# Table 2: Inflation, Foreign Policy Salience and the Use of Force ####
#### M ~ X ####
m1_inf <- lm(fine_tune_temp0 ~ ch_inflation_lag_1mth, data = df)
summary(m1_inf)

m2_inf <- lm(fine_tune_temp0 ~  ch_inflation_lag_1mth + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df)
summary(m2_inf)

#### Y ~ X + M ####
m3_inf <- glm(mipall_count ~ fine_tune_temp0 + ch_inflation_lag_1mth, data = df, family = poisson)
summary(m3_inf)

m4_inf <- glm(mipall_count ~ fine_tune_temp0 + ch_inflation_lag_1mth + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df, family = poisson)
summary(m4_inf)

#### Y ~ X ####
m5_inf <- glm(mipall_count ~ ch_inflation_lag_1mth, data = df, family = poisson)
summary(m5_inf)

m6_inf <- glm(mipall_count ~ ch_inflation_lag_1mth + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df, family = poisson)
summary(m6_inf)

#### Regression Table ####

stargazer(m1_inf, m2_inf, m5_inf, m6_inf, m3_inf, m4_inf,
          type = "text",
          ci.level = 0.95,
          star.cutoffs = c(0.1, 0.05, 0.01),
          notes.align = "l",
          notes.append = FALSE,
          notes.label = "Notes",
          notes = "$^{***}$p $<$ .01; $^{**}$p $<$ .05; $^{*}$p $<$ .1. Standard errors are in parentheses.")

# Dispersion test for count models ####

# Dispersion estimates across the Poisson specifications are close to 1
# (approximately 1.08–1.10), indicating only modest overdispersion.
# The dispersion test does not reject equidispersion at the 5% level for
# models m3_inf, m4_inf, and m6_inf. For m5_inf, the test is marginally
# significant (p = 0.044), although the estimated dispersion parameter
# is only 1.10. Overall, the diagnostics do not indicate substantial
# overdispersion.
#
# These diagnostics correspond to the Poisson specifications reported in the manuscript.

dispersiontest(m3_inf) # 1.085245
dispersiontest(m4_inf) # 1.079916
dispersiontest(m5_inf) # 1.099371
dispersiontest(m6_inf) # 1.095055

# Appendix D: Checking for autocorrelation in the initial OLS specifications ####

# The Durbin-Watson tests are conducted on the initial OLS
# specifications without the lagged dependent variable. These
# diagnostics indicate positive serial correlation.

# We then include a one-month lag of the dependent variable and verify
# that the substantive results for inflation are unchanged.

# Baseline model without lagged dependent variable
m1_inf <- lm(fine_tune_temp0 ~ ch_inflation_lag_1mth, data = df)
lmtest::dwtest(m1_inf)
# Full model without lagged dependent variable
m2_inf <- lm(fine_tune_temp0 ~ ch_inflation_lag_1mth + monthly_approval + election + pres_sec_term +
               unified + democrat + coldwar, data = df)
lmtest::dwtest(m2_inf)

# Models including one-month lag of the dependent variable ####

m1_dw <- lm(fine_tune_temp0 ~ fine_tune_lag_1mth + ch_inflation_lag_1mth, data = df)
m2_dw <- lm(fine_tune_temp0 ~ fine_tune_lag_1mth + ch_inflation_lag_1mth + monthly_approval + election + pres_sec_term +
              unified + democrat + coldwar, data = df)
summary(m1_dw)
summary(m2_dw)
stargazer(m1_dw, m2_dw,
          type = "latex",
          ci.level = 0.95,
          star.cutoffs = c(0.1, 0.05, 0.01),
          notes.align = "l",
          notes.append = FALSE,
          notes.label = "Notes",
          notes = "$^{***}$p $<$ .01; $^{**}$p $<$ .05; $^{*}$p $<$ .1. Standard errors are in parentheses.")

# Nested ANOVA test ####
# The restricted and fully specified models are estimated on the
# same set of complete observations to ensure a valid nested F-test.
# The test evaluates whether the additional controls in the fully
# specified model jointly improve model fit.
common_data <- na.omit(df[, c("fine_tune_temp0", "ch_inflation_lag_1mth", 
                              "monthly_approval", "election", "pres_sec_term", 
                              "unified", "democrat", "coldwar")])
model1_common <- lm(fine_tune_temp0 ~ ch_inflation_lag_1mth, data = common_data)
model2_common <- lm(fine_tune_temp0 ~ ch_inflation_lag_1mth + monthly_approval + 
                      election + pres_sec_term + unified + democrat + coldwar, 
                    data = common_data)
anova(model1_common, model2_common)

# Table 3: Causal Mediation Analysis Results ####
set.seed(1905)

mediator_model1 <- lm(fine_tune_temp0 ~ ch_inflation_lag_1mth + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df)
outcome_model1 <- glm(mipall_count ~ ch_inflation_lag_1mth + fine_tune_temp0 + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df, family = poisson)
mediation_model1 <- mediate(mediator_model1, outcome_model1, treat = "ch_inflation_lag_1mth", mediator = "fine_tune_temp0", robustSE = TRUE, sims = 1000)

summary(mediation_model1)

# Extracting the summary for reporting
mediation_summary <- summary(mediation_model1)

# Create a data frame for easier manipulation
mediation_results <- data.frame(
  Estimate = c(mediation_summary$d0, mediation_summary$d1, mediation_summary$z0, mediation_summary$z1, 
               mediation_summary$tau.coef, mediation_summary$n0, mediation_summary$n1, 
               mediation_summary$d.avg, mediation_summary$z.avg, mediation_summary$n.avg),
  `95% CI Lower` = c(mediation_summary$d0.ci[1], mediation_summary$d1.ci[1], mediation_summary$z0.ci[1], 
                     mediation_summary$z1.ci[1], mediation_summary$tau.ci[1], mediation_summary$n0.ci[1], 
                     mediation_summary$n1.ci[1], mediation_summary$d.avg.ci[1], mediation_summary$z.avg.ci[1], 
                     mediation_summary$n.avg.ci[1]),
  `95% CI Upper` = c(mediation_summary$d0.ci[2], mediation_summary$d1.ci[2], mediation_summary$z0.ci[2], 
                     mediation_summary$z1.ci[2], mediation_summary$tau.ci[2], mediation_summary$n0.ci[2], 
                     mediation_summary$n1.ci[2], mediation_summary$d.avg.ci[2], mediation_summary$z.avg.ci[2], 
                     mediation_summary$n.avg.ci[2]),
  `p-value` = c(mediation_summary$d0.p, mediation_summary$d1.p, mediation_summary$z0.p, mediation_summary$z1.p, 
                mediation_summary$tau.p, mediation_summary$n0.p, mediation_summary$n1.p, 
                mediation_summary$d.avg.p, mediation_summary$z.avg.p, mediation_summary$n.avg.p)
)

# Naming the rows for clarity
rownames(mediation_results) <- c(
  "ACME (control)", "ACME (treated)", "ADE (control)", "ADE (treated)",
  "Total Effect", "Prop. Mediated (control)", "Prop. Mediated (treated)",
  "ACME (average)", "ADE (average)", "Prop. Mediated (average)"
)

# Displaying the table using stargazer
stargazer(mediation_results, summary = FALSE, type = "latex", title = "Causal Mediation Analysis Results")


# Figure 5: Mediation Analysis Results ####
plot(mediation_model1)

# Appendix K: Sensitivity Analysis ####
sens_med1 <- medsens(mediation_model1, rho.by = 0.02, effect.type = "indirect", sims = 1000)
summary(sens_med1)
plot(sens_med1)

# Figure 3: Inflation and Salience of FP Issues in Presidential Rhetoric ####

# Calculate mean and SD of inflation
mean_inf <- mean(df$ch_inflation_lag_1mth, na.rm = TRUE)
sd_inf   <- sd(df$ch_inflation_lag_1mth, na.rm = TRUE)

p_m2_inf <- effect_plot(
  m2_inf,
  pred = ch_inflation_lag_1mth,
  interval = TRUE,
  x.label = "Inflation (Monthly)",
  y.label = "Predicted Foreign Policy Salience"
) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank()) +
  
  # Vertical lines: mean, +/- 1SD, +/- 2SD
  geom_vline(xintercept = mean_inf, linetype = "dotted", color = "gray40") +
  geom_vline(xintercept = mean_inf + sd_inf,  linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = mean_inf - sd_inf,  linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = mean_inf + 2*sd_inf, linetype = "dashed", color = "gray85") +
  geom_vline(xintercept = mean_inf - 2*sd_inf, linetype = "dashed", color = "gray85") +
  
  # Labels (placed at top of panel)
  annotate("text", x = mean_inf,           y = Inf, label = "Mean",
           vjust = 1.5, hjust = -0.1, color = "gray40") +
  annotate("text", x = mean_inf - sd_inf,  y = Inf, label = "-1SD",
           vjust = 1.5, hjust = -0.1, color = "gray50") +
  annotate("text", x = mean_inf + sd_inf,  y = Inf, label = "+1SD",
           vjust = 1.5, hjust = -0.1, color = "gray50") +
  annotate("text", x = mean_inf - 2*sd_inf, y = Inf, label = "-2SD",
           vjust = 1.5, hjust = -0.1, color = "gray60") +
  annotate("text", x = mean_inf + 2*sd_inf, y = Inf, label = "+2SD",
           vjust = 1.5, hjust = -0.1, color = "gray60")

p_m2_inf


# Figure 4: Salience of FP Issues in Presidential Rhetoric and Use of Force ####

# Calculate mean and SD of salience
mean_sal <- mean(df$fine_tune_temp0, na.rm = TRUE)
sd_sal <- sd(df$fine_tune_temp0, na.rm = TRUE)

xvals <- df$fine_tune_temp0
lo <- quantile(xvals, 0.01, na.rm = TRUE)
hi <- quantile(xvals, 0.99, na.rm = TRUE)

# Create the plot
p_m4 <- effect_plot(m4_inf, pred = fine_tune_temp0, interval = TRUE,
                    x.label = "Foreign Policy Salience",
                    y.label = "Predicted Count of Military Interventions") +
  theme_minimal() +
  theme(panel.grid.major = element_blank()) +
  
  # Add vertical lines for mean ± 1 and 2 SD
  geom_vline(xintercept = mean_sal, linetype = "dotted", color = "gray40") +
  geom_vline(xintercept = mean_sal + sd_sal, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = mean_sal - sd_sal, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = mean_sal + 2*sd_sal, linetype = "dashed", color = "gray85") +
  geom_vline(xintercept = mean_sal - 2*sd_sal, linetype = "dashed", color = "gray85") +
  
  # Optional: annotate the SD labels
  annotate("text", x = mean_sal, y = Inf, label = "Mean", vjust = 1.5, hjust = -0.1, color = "gray40") +
  annotate("text", x = mean_sal - sd_sal, y = Inf, label = "-1SD", vjust = 1.5, hjust = -0.1, color = "gray50") +
  annotate("text", x = mean_sal + sd_sal, y = Inf, label = "+1SD", vjust = 1.5, hjust = -0.1, color = "gray50") +
  annotate("text", x = mean_sal - 2*sd_sal, y = Inf, label = "-2SD", vjust = 1.5, hjust = -0.1, color = "gray60") +
  annotate("text", x = mean_sal + 2*sd_sal, y = Inf, label = "+2SD", vjust = 1.5, hjust = -0.1, color = "gray60") + coord_cartesian(xlim = c(lo, hi))


# Print it
print(p_m4)

# Appendix E: Inflation Lagged by Multiple Months ####

m1_lag2 <- lm(fine_tune_temp0 ~ ch_inflation_lag_2mth, data = df)

m2_lag2 <- lm(fine_tune_temp0 ~  ch_inflation_lag_2mth + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df)

m1_lag3 <- lm(fine_tune_temp0 ~ ch_inflation_lag_3mth, data = df)

m2_lag3 <- lm(fine_tune_temp0 ~  ch_inflation_lag_3mth + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df)

stargazer(m1_lag2, m2_lag2, m1_lag3, m2_lag3,
          type = "latex",
          ci.level = 0.95,
          star.cutoffs = c(0.1, 0.05, 0.01),
          notes.align = "l",
          notes.append = FALSE,
          notes.label = "Notes",
          notes = "$^{***}$p $<$ .01; $^{**}$p $<$ .05; $^{*}$p $<$ .1. Standard errors are in parentheses.")

# Appendix F: Post-9/11 As an Additional Control Variable ####

m1_inf <- lm(fine_tune_temp0 ~ ch_inflation_lag_1mth, data = df)
summary(m1_inf)

m2_inf <- lm(fine_tune_temp0 ~  ch_inflation_lag_1mth + monthly_approval + election + pres_sec_term + unified + democrat + coldwar + post_9_11, data = df)
summary(m2_inf)

m3_inf <- glm(mipall_count ~ fine_tune_temp0 + ch_inflation_lag_1mth, data = df, family = poisson)
summary(m3_inf)

m4_inf <- glm(mipall_count ~ fine_tune_temp0 + ch_inflation_lag_1mth + monthly_approval + election + pres_sec_term + unified + democrat + coldwar + post_9_11, data = df, family = poisson)
summary(m4_inf)

m5_inf <- glm(mipall_count ~ ch_inflation_lag_1mth, data = df, family = poisson)
summary(m5_inf)

m6_inf <- glm(mipall_count ~ ch_inflation_lag_1mth + monthly_approval + election + pres_sec_term + unified + democrat + coldwar + post_9_11, data = df, family = poisson)
summary(m6_inf)

# Regression Table 

stargazer(m1_inf, m2_inf, m5_inf, m6_inf, m3_inf, m4_inf,
          type = "latex",
          ci.level = 0.95,
          star.cutoffs = c(0.1, 0.05, 0.01),
          notes.align = "l",
          notes.append = FALSE,
          notes.label = "Notes",
          notes = "$^{***}$p $<$ .01; $^{**}$p $<$ .05; $^{*}$p $<$ .1. Standard errors are in parentheses.")

# Appendix G: Accounting for Prior Military Interventions ####

m1_inf_mip <- lm(fine_tune_temp0 ~ ch_inflation_lag_1mth + mipall_count_lag1, data = df)
summary(m1_inf_mip)

m2_inf_mip <- lm(fine_tune_temp0 ~  ch_inflation_lag_1mth + mipall_count_lag1 + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df)
summary(m2_inf_mip)

stargazer(m1_inf_mip, m2_inf_mip,
          type = "latex",
          ci.level = 0.95,
          star.cutoffs = c(0.1, 0.05, 0.01),
          notes.align = "l",
          notes.append = FALSE,
          notes.label = "Notes",
          notes = "$^{***}$p $<$ .01; $^{**}$p $<$ .05; $^{*}$p $<$ .1. Standard errors are in parentheses.")

# Appendix H: President Party Interaction with Inflation ####

m1_dem <- lm(fine_tune_temp0 ~  ch_inflation_lag_1mth*democrat, data = df)
summary(m1_dem)

m2_dem <- lm(fine_tune_temp0 ~  ch_inflation_lag_1mth*democrat + monthly_approval + election + pres_sec_term + unified + coldwar, data = df)
summary(m2_dem)

m3_dem <- glm(mipall_count ~ fine_tune_temp0 + ch_inflation_lag_1mth*democrat, data = df, family = poisson)
summary(m3_dem)

m4_dem <- glm(mipall_count ~ fine_tune_temp0 + ch_inflation_lag_1mth*democrat + monthly_approval + election + pres_sec_term + unified + coldwar, data = df, family = poisson)
summary(m4_dem)

m5_dem <- glm(mipall_count ~ ch_inflation_lag_1mth*democrat, data = df, family = poisson)
summary(m5_dem)

m6_dem <- glm(mipall_count ~ ch_inflation_lag_1mth*democrat + monthly_approval + election + pres_sec_term + unified + coldwar, data = df, family = poisson)
summary(m6_dem)

# Appendix Table 10

stargazer(m1_dem, m2_dem, m5_dem, m6_dem, m3_dem, m4_dem,
          type = "latex",
          ci.level = 0.95,
          star.cutoffs = c(0.1, 0.05, 0.01),
          notes.align = "l",
          notes.append = FALSE,
          notes.label = "Notes",
          notes = "$^{***}$p $<$ .01; $^{**}$p $<$ .05; $^{*}$p $<$ .1. Standard errors are in parentheses.")

#### Figure 6: Interaction of President’s Party with Inflation ####

df$democrat_f <- factor(
  df$democrat,
  levels = c(0, 1),
  labels = c("Republican", "Democratic")
)

m2_dem <- lm(fine_tune_temp0 ~  ch_inflation_lag_1mth*democrat_f + monthly_approval + election + pres_sec_term + unified + coldwar, data = df)

interact_plot(
  m2_dem,
  pred = ch_inflation_lag_1mth,
  modx = democrat_f,
  colors = c("darkred", "darkblue"),
  plot.points = FALSE,
  interval = TRUE,
  int.type = "confidence",
  x.label = "Change in Inflation (t–1)",
  y.label = "Foreign Policy Salience",
  legend.main = "President's Party"
)

#### Figure A4: Interaction of President’s Party with Unemployment ####
df$democrat_f <- factor(
  df$democrat,
  levels = c(0, 1),
  labels = c("Republican", "Democratic")
)

m2_dem_unemp <- lm(
  fine_tune_temp0 ~ employment_lag_1mth * democrat_f +
    monthly_approval + election + pres_sec_term + unified + coldwar,
  data = df
)

interact_plot(
  m2_dem_unemp,
  pred = employment_lag_1mth,
  modx = democrat_f,
  colors = c("darkred", "darkblue"),
  plot.points = FALSE,
  interval = TRUE,
  int.type = "confidence",
  x.label = "Unemployment",
  y.label = "Foreign Policy Salience",
  legend.main = "President's Party"
)

# Appendix I: President Party Interaction with Unemployment ####

m1_dem <- lm(fine_tune_temp0 ~  employment_lag_1mth*democrat, data = df)
summary(m1_dem)

m2_dem <- lm(fine_tune_temp0 ~  employment_lag_1mth*democrat + monthly_approval + election + pres_sec_term + unified + coldwar, data = df)
summary(m2_dem)

m3_dem <- glm(mipall_count ~ fine_tune_temp0 + employment_lag_1mth*democrat, data = df, family = poisson)
summary(m3_dem)

m4_dem <- glm(mipall_count ~ fine_tune_temp0 + employment_lag_1mth*democrat + monthly_approval + election + pres_sec_term + unified + coldwar, data = df, family = poisson)
summary(m4_dem)

m5_dem <- glm(mipall_count ~ employment_lag_1mth*democrat, data = df, family = poisson)
summary(m5_dem)

m6_dem <- glm(mipall_count ~ employment_lag_1mth*democrat + monthly_approval + election + pres_sec_term + unified + coldwar, data = df, family = poisson)
summary(m6_dem)

#### Table A11: Interaction of President’s Party with Unemployment ####

stargazer(m1_dem, m2_dem, m5_dem, m6_dem, m3_dem, m4_dem,
          type = "latex",
          ci.level = 0.95,
          star.cutoffs = c(0.1, 0.05, 0.01),
          notes.align = "l",
          notes.append = FALSE,
          notes.label = "Notes",
          notes = "$^{***}$p $<$ .01; $^{**}$p $<$ .05; $^{*}$p $<$ .1. Standard errors are in parentheses.")


# Appendix J: Robustness Check Using MID-Based Use of Force ####

m1_inf <- lm(fine_tune_temp0 ~ ch_inflation_lag_1mth, data = df)
summary(m1_inf)

m2_inf <- lm(fine_tune_temp0 ~  ch_inflation_lag_1mth + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df)
summary(m2_inf)

m3_inf <- glm(midall_count ~ fine_tune_temp0 + ch_inflation_lag_1mth, data = df, family = poisson)
summary(m3_inf)

m4_inf <- glm(midall_count ~ fine_tune_temp0 + ch_inflation_lag_1mth + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df, family = poisson)
summary(m4_inf)

m5_inf <- glm(midall_count ~ ch_inflation_lag_1mth, data = df, family = poisson)
summary(m5_inf)

m6_inf <- glm(midall_count ~ ch_inflation_lag_1mth + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df, family = poisson)
summary(m6_inf)

#### Table A12: Inflation, Foreign Policy Salience and MID-Based Use of Force ####

stargazer(m1_inf, m2_inf, m5_inf, m6_inf, m3_inf, m4_inf,
          type = "latex",
          ci.level = 0.95,
          star.cutoffs = c(0.1, 0.05, 0.01),
          notes.align = "l",
          notes.append = FALSE,
          notes.label = "Notes",
          notes = "$^{***}$p $<$ .01; $^{**}$p $<$ .05; $^{*}$p $<$ .1. Standard errors are in parentheses.")

# Appendix A: Different Measures for Economy (Unemployment Rate, Misery Index, GDP Growth (Quarterly)) ####

# Unemployment rate
m1_emp <- lm(fine_tune_temp0 ~ employment_lag_1mth, data = df)
summary(m1_emp)

m2_emp <- lm(fine_tune_temp0 ~  employment_lag_1mth  + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df)
summary(m2_emp)

m3_emp <- glm(mipall_count ~ fine_tune_temp0 + employment_lag_1mth, data = df, family = poisson)
summary(m3_emp)

m4_emp <- glm(mipall_count ~ fine_tune_temp0 + employment_lag_1mth + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df, family = poisson)
summary(m4_emp)

m5_emp <- glm(mipall_count ~ employment_lag_1mth, data = df, family = poisson)
summary(m5_emp)

m6_emp <- glm(mipall_count ~ employment_lag_1mth + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df, family = poisson)
summary(m6_emp)

#### Table A1: Unemployment, Foreign Policy Salience, and Use of Force ####

stargazer(m1_emp, m2_emp, m5_emp, m6_emp, m3_emp, m4_emp,
          type = "latex",
          ci.level = 0.95,
          star.cutoffs = c(0.1, 0.05, 0.01),
          notes.align = "l",
          notes.append = FALSE,
          notes.label = "Notes",
          notes = "$^{***}$p $<$ .01; $^{**}$p $<$ .05; $^{*}$p $<$ .1. Standard errors are in parantheses.")

#### Figure A1: Mediation Analysis Results ####

# Mediation with employment_lag_1mth
mediator_model2 <- lm(fine_tune_temp0 ~ employment_lag_1mth  + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df)
outcome_model2 <- glm(mipall_count ~ employment_lag_1mth + fine_tune_temp0 + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df, family = poisson)
mediation_model2 <- mediate(mediator_model2, outcome_model2, treat = "employment_lag_1mth", mediator = "fine_tune_temp0", robustSE = TRUE, sims = 1000)

summary(mediation_model2)
plot(mediation_model2)

# Misery Index

m1_mis <- lm(fine_tune_temp0 ~ misery_lag_1mth, data = df)
summary(m1_mis)

m2_mis <- lm(fine_tune_temp0 ~  misery_lag_1mth  + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df)
summary(m2_mis)

m3_mis <- glm(mipall_count ~ fine_tune_temp0 + misery_lag_1mth, data = df, family = poisson)
summary(m3_mis)

m4_mis <- glm(mipall_count ~ fine_tune_temp0 + misery_lag_1mth + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df, family = poisson)
summary(m4_mis)

m5_mis <- glm(mipall_count ~ misery_lag_1mth, data = df, family = poisson)
summary(m5_mis)

m6_mis <- glm(mipall_count ~ misery_lag_1mth + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df, family = poisson)
summary(m6_mis)

#### Table A2: Misery Index, Foreign Policy Salience, and Use of Force ####

stargazer(m1_mis, m2_mis, m5_mis, m6_mis, m3_mis, m4_mis,
          type = "latex",
          ci.level = 0.95,
          star.cutoffs = c(0.1, 0.05, 0.01),
          notes.align = "l",
          notes.append = FALSE,
          notes.label = "Notes",
          notes = "$^{***}$p $<$ .01; $^{**}$p $<$ .05; $^{*}$p $<$ .1. Standard errors are in parantheses.")

#### Figure A2: Mediation Analysis Results ####

# Mediation with employment_lag_1mth
mediator_model3 <- lm(fine_tune_temp0 ~  misery_lag_1mth + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df)
outcome_model3 <- glm(mipall_count ~ misery_lag_1mth + fine_tune_temp0 + monthly_approval + election + pres_sec_term + unified + democrat + coldwar, data = df, family = poisson)
mediation_model3 <- mediate(mediator_model3, outcome_model3, treat = "misery_lag_1mth", mediator = "fine_tune_temp0", robustSE = TRUE, sims = 1000)

summary(mediation_model3)
plot(mediation_model3)

# GDP Growth (Quarterly) 
library(dplyr)
library(lubridate)
library(haven)
library(zoo)
library(tidyr)
library(readxl)

gdp_growth <- read_excel("gdp_growth.xlsx")

df <- df %>%
  mutate(date = as.Date(paste(year, month, "1", sep = "-")))

df <- df %>%
  mutate(qdate = as.yearqtr(date))

quarterly <- df %>%
  group_by(qdate) %>%
  summarise(
    fine_tune_temp0 = mean(fine_tune_temp0, na.rm=TRUE),
    approval = mean(monthly_approval, na.rm=TRUE),
    mipall_count = sum(mipall_count, na.rm=TRUE),
    election = max(election),
    democrat = first(democrat),
    unified = first(unified),
    pres_sec_term = first(pres_sec_term),
    coldwar = max(coldwar)
  )

colnames(gdp_growth) <- c("year", "quarter", "gdp_growth")

gdp_growth <- gdp_growth %>%
  fill(year)

gdp_growth <- gdp_growth %>%
  mutate(
    qdate = as.yearqtr(
      paste(year, quarter),
      format = "%Y Q%q"
    )
  )

quarterly <- quarterly %>%
  left_join(
    gdp_growth %>% dplyr::select(qdate, gdp_growth),
    by = "qdate"
  )

m1_inf <- lm(fine_tune_temp0 ~ gdp_growth, data = quarterly)
summary(m1_inf)

m2_inf <- lm(fine_tune_temp0 ~  gdp_growth + approval + election + pres_sec_term + unified + democrat + coldwar, data = quarterly)
summary(m2_inf)

m3_inf <- glm(mipall_count ~ fine_tune_temp0 + gdp_growth, data = quarterly, family = poisson)
summary(m3_inf)

m4_inf <- glm(mipall_count ~ fine_tune_temp0 + gdp_growth + approval + election + pres_sec_term + unified + democrat + coldwar, data = quarterly, family = poisson)
summary(m4_inf)

m5_inf <- glm(mipall_count ~ gdp_growth, data = quarterly, family = poisson)
summary(m5_inf)

m6_inf <- glm(mipall_count ~ gdp_growth + approval + election + pres_sec_term + unified + democrat + coldwar, data = quarterly, family = poisson)
summary(m6_inf)

#### Table A3: GDP Growth (Quarterly), Foreign Policy Salience and the Use of Force ####

stargazer(m1_inf, m2_inf, m5_inf, m6_inf, m3_inf, m4_inf,
          type = "latex",
          ci.level = 0.95,
          star.cutoffs = c(0.1, 0.05, 0.01),
          notes.align = "l",
          notes.append = FALSE,
          notes.label = "Notes",
          notes = "$^{***}$p $<$ .01; $^{**}$p $<$ .05; $^{*}$p $<$ .1. Standard errors are in parentheses.")

# Figure 2: Monthly Foreign Policy Salience Scores and Inflation Rates, 1945–2019 ####

# Date variable
df <- df %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

# Common x-axis breaks
x_breaks <- seq(as.Date("1945-04-01"),
                as.Date("2019-12-31"),
                by = "24 months")

# Panel A1: Foreign Policy Salience

p1 <- ggplot(df, aes(x = date, y = fine_tune_temp0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.35) +
  geom_line(color = "black", linewidth = 0.25, alpha = 0.6) +
  geom_smooth(method = "loess", span = 0.15, se = FALSE, color = "black", linewidth = 1) +
  scale_x_date(breaks = x_breaks, date_labels = "%Y") +
  labs(title = "Foreign Policy Salience Score", x = NULL, y = NULL) +
  coord_cartesian(ylim = c(0, 5)) +
  theme_minimal(base_family = "Times New Roman") +
  theme(plot.title = element_text(face = "plain", size = 15, hjust = 0),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(color = "grey88", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
        axis.text.y = element_text(size = 11),
        plot.margin = margin(5, 10, 0, 10))

# Panel A2: Inflation

p2 <- ggplot(df, aes(x = date, y = inflation)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.35) +
  geom_line(color = "black", linewidth = 0.3, alpha = 0.6, linetype = "dashed") +
  geom_smooth(method = "loess", span = 0.15, se = FALSE, color = "black", linewidth = 1) +
  scale_x_date(breaks = x_breaks, date_labels = "%Y") +
  labs(title = "Monthly Inflation Rate (percent)", y = NULL, x = NULL,) +
  coord_cartesian(ylim = c(-2, 2)) +
  theme_minimal(base_family = "Times New Roman") +
  theme(plot.title = element_text(face = "plain", size = 15, hjust = 0),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
        axis.title.x = element_text(size = 14, margin = margin(t = 12)),
        panel.grid.minor = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(color = "grey88", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
        axis.text.y = element_text(size = 11),
        plot.margin = margin(0, 10, 5, 10))

# Combine panels

(p1 / p2) +
  plot_layout(heights = c(1, 1))
