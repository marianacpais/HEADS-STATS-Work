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

path <- "datasets/PA-DatasetD.sav"

PA_DatasetD <- read_sav(path)

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
  xlab("Systolic Blood Pressure (mmHg)") + ylab("Frequency") + theme_minimal(base_size = 15)

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

# Positive Linear Relationship: There is a clear positive linear relationship between SBP and DBP. As diastolic blood pressure increases, systolic blood pressure also tends to increase. This is indicated by the upward slope of the regression line.
# Strength of Relationship: The points are relatively close to the regression line, suggesting a fairly strong linear relationship. However, there is still some spread, indicating variability.
# Linearity: The linear relationship appears to be a good fit for the data, as indicated by the regression line fitting well through the points.
# Correlation: Given the strong linear relationship, you might find a high correlation coefficient between SBP and DBP.

# Correlation test between systolic and diastolic blood pressure:Pearson coefficient
cor_test_result <- cor.test(bp$sbp, bp$dbp, method = "pearson") # 0.82 (strong correlation) - statistical significant p-value <0.05
print(cor_test_result)

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
anova(model_sex)

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

multivariate_model_1 <- lm(sbp ~ age  + wc + imc, data = bp)
summary(multivariate_model_1)

#r^2 adjusted 0.3195 - 32% of the variance in the dependent variable is explained by the independent variables in model 1
# only age is a statistically significant predictor of SBP.
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
multivariate_model_2 <- lm(sbp ~ age  + imc, data = bp) 
summary(multivariate_model_2)

# R^2 adjusted - 0.325, means that 33% of the variance in diastolic blood pressure is explained by the independent variables in this model2.

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


#multivariate model 3 - without imc
multivariate_model_3 <- lm(sbp ~ age  + wc, data = bp) 
summary(multivariate_model_3)
anova(multivariate_model_3)


## evaluate performance of the models:

# compare model 2 and 1 with similar determination coefficient:
anova(multivariate_model_1,multivariate_model_2) # p-value <0.05

# The ANOVA comparison between Model 1 and Model 2 shows that adding waist circumference (wc) to the model does not significantly improve the fit. This is evidenced by the p-value of 0.778, which is not less than 0.05.


#compare univariate model with diastolic blood pressure VS multivariate model_2:
anova(model_dbp, multivariate_model_2) # multivariate_model_2 is preferable p-value>0.05

# extract AIC (Akaike information criterion) and BIC (Baeysian information criterior) to compare models:


# perform cross validation:


