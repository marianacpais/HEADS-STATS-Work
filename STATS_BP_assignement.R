library(haven)
library(readr)
library(ggplot2)
library(dplyr)
library(car)
library(knitr)

install.packages("kableExtra")
library(kableExtra)

install.packages("lm.beta")
library(lm.beta)

PA_DatasetD <- read_sav("PA-DatasetD.sav")

bp <- PA_DatasetD
bp <- data.frame(bp)

str(bp)
View(bp)

# Convert gender to a factor:
bp$sex<- factor(bp$sex, levels = c(1, 2), labels = c("Male", "Female"))

# check variables distribution:

hist(bp$sbp)

# ks.test(bp$sbp, "pnorm", mean=mean(bp$sbp), sd=sd(bp$sbp)), kolmogorv test

# shapiro-wilk test to check normality of the sbp:
shapiro.test(bp$sbp) ## p-value 0.045

# The Shapiro-Wilk test is highly sensitive to even small deviations from normality in large samples. This means that as the sample size increases, the test becomes more likely to reject the null hypothesis of normality, even if the deviations from normality are very small.

# Checking normality with Q-Q plot  
par(mfrow=c(1, 1)) # Reset graphics layout if necessary
qqnorm(bp$sbp, main="Q-Q Plot of Systolic Blood Pressure")
qqline(bp$sbp, col="red")


ggplot(bp, aes(x=sbp)) + geom_histogram(fill="lightblue", color="black", binwidth=10) + 
  ggtitle("Distribution of Systolic Blood Pressure") +
  xlab("Systolic Blood Pressure (mm Hg)") + ylab("Frequency") + theme_minimal(base_size = 15)

hist(bp$dbp)
qqnorm(bp$sbp, main="Q-Q Plot of Diastolic Blood Pressure")
qqline(bp$sbp, col="blue")

hist(bp$age)

# create variable IMC (peso(kg)/h^2 (m)) because the independent variables height and weight are strongly correlated and should not be used simultaneously:
bp <- bp %>%
  mutate(imc = weight / ((height / 100) ^ 2))

hist(bp$imc)

View(bp)
sum(is.na(bp)) ## no mising values


################ Scatter plots to check linearity:################

# systolic blood pressure and age:
ggplot(bp, aes(x=age, y=sbp)) + geom_point() + geom_smooth(method="lm") + 
  ggtitle("SBP vs AGE") + xlab("Age") + ylab("Systolic Blood Pressure (mm Hg)") + theme_minimal(base_size = 15)

cor_test_result <- cor.test(bp$sbp, bp$age, method = "pearson") # 0.49 (low to moderate correlation) - statistical significant p-value <0.05
print(cor_test_result)


#systolic blood pressure with the imc:
ggplot(bp, aes(x=imc, y=sbp)) + geom_point() + geom_smooth(method="lm") + 
  ggtitle("SBP vs IMC") + xlab("IMC") + ylab("Systolic Blood Pressure (mm Hg)") + theme_minimal(base_size = 15)

cor_test_result <- cor.test(bp$sbp, bp$imc, method = "pearson") # 0.43 (low to moderate correlation) - statistical significant p-value <0.05
print(cor_test_result)

#systolic blood pressure and wc:
ggplot(bp, aes(x=wc, y=sbp)) + geom_point() + geom_smooth(method="lm") + 
  ggtitle("SBP vs wc") + xlab("wc") + ylab("Systolic Blood Pressure (mm Hg)") + theme_minimal(base_size = 15)

cor_test_result <- cor.test(bp$sbp, bp$wc, method = "pearson") # 0.44 (low to moderate correlation) - statistical significant p-value <0.05
print(cor_test_result)


# systolic blood pressure and diastolic blood pressure:
ggplot(bp, aes(x=dbp, y=sbp)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  ggtitle("SBP vs DBP") + 
  xlab("Diastolic Blood Pressure (mm Hg)") + 
  ylab("Systolic Blood Pressure (mm Hg)") + 
  theme_minimal(base_size = 15)


# Correlation test between systolic and diastolic blood pressure:Pearson coefficient
cor_test_result <- cor.test(bp$sbp, bp$dbp, method = "pearson") # 0.82 (strong correlation) - statistical significant p-value <0.05
print(cor_test_result)

# Obvious - but we never measure systolic blood pressure without measuring the diastolic at the same event, makes no sense clinically!

######################################## boxplot of systolic blood pressure by Gender:
table(bp$sex) ## more women than men in the dataset
prop.table(table(bp$sex))

ggplot(bp, aes(x=sex, y=sbp, fill=sex)) +
  geom_boxplot() +
  ggtitle("Systolic Blood Pressure by Gender") +
  xlab("Gender") +
  ylab("Systolic Blood Pressure (mm Hg)") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("lightblue", "pink"))

## close medians in both genders - not very different from each other


####################################### descriptive statistics - create a table for the power point presentation

# Calculate summary statistics for sbp and dbp
summary_stats <- bp %>%
  summarise(
    count_sbp = n(),
    mean_sbp = mean(sbp, na.rm = TRUE),
    sd_sbp = sd(sbp, na.rm = TRUE),
    min_sbp = min(sbp, na.rm = TRUE),
    `25th percentile_sbp` = quantile(sbp, 0.25, na.rm = TRUE),
    median_sbp = median(sbp, na.rm = TRUE),
    `75th percentile_sbp` = quantile(sbp, 0.75, na.rm = TRUE),
    max_sbp = max(sbp, na.rm = TRUE),
    
    count_dbp = n(),
    mean_dbp = mean(dbp, na.rm = TRUE),
    sd_dbp = sd(dbp, na.rm = TRUE),
    min_dbp = min(dbp, na.rm = TRUE),
    `25th percentile_dbp` = quantile(dbp, 0.25, na.rm = TRUE),
    median_dbp = median(dbp, na.rm = TRUE),
    `75th percentile_dbp` = quantile(dbp, 0.75, na.rm = TRUE),
    max_dbp = max(dbp, na.rm = TRUE)
  )

# Create the summary statistics table
summary_table <- data.frame(
  Statistic = c("Count", "Mean", "Std Dev", "Min", "25th Percentile", "Median", "75th Percentile", "Max"),
  SBP = c(summary_stats$count_sbp, summary_stats$mean_sbp, summary_stats$sd_sbp, summary_stats$min_sbp, 
          summary_stats$`25th percentile_sbp`, summary_stats$median_sbp, summary_stats$`75th percentile_sbp`, summary_stats$max_sbp),
  DBP = c(summary_stats$count_dbp, summary_stats$mean_dbp, summary_stats$sd_dbp, summary_stats$min_dbp, 
          summary_stats$`25th percentile_dbp`, summary_stats$median_dbp, summary_stats$`75th percentile_dbp`, summary_stats$max_dbp)
)

# Display the table using kable
kable(summary_table, format = "html", caption = "Summary Statistics of Blood Pressure") %>%
  kable_styling(full_width = FALSE, position = "left")


##################### univariate analysis using linear regression:
# to understand the relationship between systolic blood pressure and each independent variable individually:

options(scipen=999)

# age
model_age <- lm(sbp ~ age, data = bp)  # p- value 0.000(..) age is statistical significant
summary(model_age)
confint(model_age)
anova(model_age)

#Plot the data and the age_model
ggplot(bp, aes(x = age, y = sbp)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", fill = "lightgray") +
  ggtitle("Relationship between Age and Systolic Blood Pressure") +
  xlab("Age") +
  ylab("Systolic Blood Pressure (mm Hg)") +
  theme_minimal(base_size = 15) 



# sex
model_sex <- lm(sbp ~ sex, data = bp) # p-value 0.505 gender is not statistical significant (as the boxplot already shown)
summary(model_sex)
anova(model_imc)

# imc
model_imc <- lm(sbp ~ imc, data = bp) # p-value 0.000(...) imc is statistical significant
summary(model_imc)
confint(model_imc)
anova(model_imc)

# Plot the data and the imc_model
ggplot(bp, aes(x = imc, y = sbp)) +
  geom_point(color = "blue", alpha = 0.5) + 
  geom_smooth(method = "lm", color = "red", fill = "lightgray") +
  ggtitle("Relationship between IMC and Systolic Blood Pressure") +
  xlab("IMC (BMI)") +
  ylab("Systolic Blood Pressure (mm Hg)") +
  theme_minimal(base_size = 15) 


# wc
model_wc <- lm(sbp ~ wc, data = bp)  # p-value 0.000(...) wc is statistical significant
summary(model_wc)
confint(model_wc)
anova(model_wc)

#Plot the data and the wc_model
ggplot(bp, aes(x = wc, y = sbp)) +
  geom_point(color = "blue", alpha = 0.5) + 
  geom_smooth(method = "lm", color = "red", fill = "lightgray") +
  ggtitle("Relationship between Waist Circumference and Systolic Blood Pressure") +
  xlab("Waist Circumference (cm)") +
  ylab("Systolic Blood Pressure (mm Hg)") +
  theme_minimal(base_size = 15) 

# dbp
model_dbp <- lm(sbp ~ dbp, data = bp) # p-value 0.000 (..) diastolic blood pressure is statistical significant
summary(model_dbp)
confint(model_dbp)
anova(model_dbp)

ggplot(bp, aes(x = dbp, y = sbp)) +
  geom_point(color = "blue", alpha = 0.5) + # Scatter plot of the data points
  geom_smooth(method = "lm", color = "red", fill = "lightgray") +
  ggtitle("Relationship between Diastolic and Systolic Blood Pressure") +
  xlab("Diastolic Blood Pressure (mm Hg)") +
  ylab("Systolic Blood Pressure (mm Hg)") +
  theme_minimal(base_size = 15) 
 


################################### multivariate analysis (with the statistically significant variables:sex excluded!!!)

multivariate_model_1 <- lm(sbp ~ age  + wc + imc + dbp, data = bp)
summary(multivariate_model_1)

#r^2 - 0.707/ adjusted 0.70 - 70% of the variance in the dependent variable is explained by the independent variables in model 1
# age, imc, and dbp are statistically significant predictors of SBP.
# wc is no longer a significant predictor in the presence of other variables.

##checking for multicollinearity: when independent variables in a model might be correlated:

vif(multivariate_model_1)

# Interpretation of VIF Values:
#   VIF = 1: No multicollinearity.
# 1 < VIF < 5: Moderate multicollinearity, usually not problematic.
# VIF > 5: High multicollinearity, which may be problematic.
# VIF > 10: Very high multicollinearity, often considered problematic and should be addressed.

# as suspected:
# wc: With a VIF of 4.490936, waist circumference shows moderate multicollinearity.
# imc: With a VIF of 4.095716, IMC shows moderate multicollinearity.

# checking for homecedasticity - of residuals
qqnorm(residuals(multivariate_model_1))
qqline(residuals(multivariate_model_1), col = "red")
par(mfrow = c(2, 2))  # Set up the plotting area
plot(multivariate_model_1)

# Reset plotting area
par(mfrow = c(1, 1))

# Normality of residuals
qqnorm(residuals(multivariate_model_1))
qqline(residuals(multivariate_model_1), col = "red")
# 

# Multivariate model 2: without wc (to avoid multicollinearity)
multivariate_model_2 <- lm(sbp ~ age  + imc + dbp, data = bp) 
summary(multivariate_model_2)

# R^2 - 0.703, adjusted 0.69 -  means that 70% of the variance in diastolic blood pressure is explained by the independent variables in this mode2.

# checking for homecedasticity - of residuals
qqnorm(residuals(multivariate_model_2))
qqline(residuals(multivariate_model_2), col = "red")
par(mfrow = c(2, 2))  # Set up the plotting area
plot(multivariate_model_1)

# Reset plotting area
par(mfrow = c(1, 1))

# Normality of residuals
qqnorm(residuals(multivariate_model_2))
qqline(residuals(multivariate_model_2), col = "red")


# check homocedasticity: plot Residuals vs. Fitted Values Plot
par(mfrow = c(2, 2))  # Set up the plotting area
plot(multivariate_model_1)


#################### MUlTIVARIATE MODELS WITHOUT DBP FOR OBVIOUS CLINICAL REASONS

# multivariate model 3: age, wc, imc
multivariate_model_3 <- lm(sbp ~ age + imc + wc , data = bp) ## r^2 0.34, adjusted  0.319
summary(multivariate_model_3)
anova(multivariate_model_3)

# multivariate model 4 - age + imc:                           ## preferable to use imc than wc!!
multivariate_model_4 <- lm(sbp ~ age + imc, data = bp)
summary(multivariate_model_4)  ## r^2 0.34, adjusted 0.325
anova(multivariate_model_4)

# multivariate model 5: age + wc
multivariate_model_5 <- lm(sbp ~ age + wc, data = bp) ## r^2 0.32, adjusted 0.305 preferable to use the model
summary(multivariate_model_5) 
anova(multivariate_model_5)


# evaluate performance of the models:

# compare model 3 and 4 with similar determination coefficient:
anova(multivariate_model_3,multivariate_model_4) 

# The ANOVA comparison between Model 4 and Model 3 shows that adding waist circumference (wc) to the model does not significantly improve the fit. This is evidenced by the p-value > 0.05


# #compare univariate model with diastolic blood pressure VS multivariate model_2:
# anova(model_dbp, multivariate_model_2) # multivariate_model_2 is preferable p-value>0.05
# 
# # compare multivariate model 4 with multivariate model 2:
# anova(multivariate_model_2, multivariate_model_4)

######### extract AIC (Akaike information criterion) and BIC (Baeysian information criterior) to compare models:

# Sample size
n <- nrow(bp)

# Extract AIC values
aic_mod1 <- extractAIC(multivariate_model_3, k = 2) # age, imc, wc
aic_mod2 <- extractAIC(multivariate_model_4, k = 2) # without wc
aic_mod3 <- extractAIC(multivariate_model_5, k = 2) # withou imc
aic_mod4 <- extractAIC(model_age, K = 2) # simple linear model just with age

# Extract BIC values
bic_mod1 <- extractAIC(multivariate_model_3, k = log(n))
bic_mod2 <- extractAIC(multivariate_model_4, k = log(n))
bic_mod3 <- extractAIC(multivariate_model_5, k = log(n))
bic_mod4 <- extractAIC(model_age, k = log(n))

# Create a data frame for comparison
comparison <- data.frame(
  Model = c("multivariate_model_3", "multivariate_model_4", "multivariate_model_5", "model_age"),
  AIC = c(aic_mod1[2], aic_mod2[2], aic_mod3[2], aic_mod4[2]),
  BIC = c(bic_mod1[2], bic_mod2[2], bic_mod3[2], bic_mod4[2])
)

# Print the comparison table
print(comparison)

# Determine the best model according to AIC
best_aic_model <- comparison[which.min(comparison$AIC), "Model"]
cat("According to AIC, the best model is:", best_aic_model, "\n")

# Determine the best model according to BIC
best_bic_model <- comparison[which.min(comparison$BIC), "Model"]
cat("According to BIC, the best model is:", best_bic_model, "\n")


## checking for multicollinearity: when independent variables in a model might be correlated:

vif(multivariate_model_4) # no multicollearity
vif(multivariate_model_3) ## imc (4.04); wc (4.04) moderate mullticolenearity -- check classification system

# Interpretation of VIF Values:
#   VIF = 1: No multicollinearity.
# 1 < VIF < 5: Moderate multicollinearity, usually not problematic.
# VIF > 5: High multicollinearity, which may be problematic.
# VIF > 10: Very high multicollinearity, often considered problematic and should be addressed.

# as suspected:
# wc: With a VIF of 4.490936, waist circumference shows moderate multicollinearity.
# imc: With a VIF of 4.095716, IMC shows moderate multicollinearity.

# checking for homecedasticity - of residuals
qqnorm(residuals(multivariate_model_1))
qqline(residuals(multivariate_model_1), col = "red")
par(mfrow = c(2, 2))  # Set up the plotting area
plot(multivariate_model_1)


# checking for homecedasticity - of residuals
qqnorm(residuals(multivariate_model_4))
qqline(residuals(multivariate_model_4), col = "red")
par(mfrow = c(2, 2))  # Set up the plotting area
plot(multivariate_model_4)




# perform cross validation:........


