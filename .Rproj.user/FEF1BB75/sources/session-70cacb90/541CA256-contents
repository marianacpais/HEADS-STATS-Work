library(haven)
library(ggplot2)
library(dplyr)
library(tableone)
library(officer)
library(flextable)
library(broom)

path <- "datasets/PA-DatasetD.sav"

bp <- read_sav(path)

bp$sex<- factor(bp$sex, levels = c(1, 2), labels = c("Male", "Female"))

###### OUTCOMES

mean_sbp <- mean(bp$sbp, na.rm = TRUE)
sd_sbp <- sd(bp$sbp, na.rm = TRUE)

shapiro.test(bp$sbp)

ggplot(bp, aes(x=sbp)) + 
  geom_histogram(fill="lightcyan3", color="black", binwidth=10)  + 
  # geom_vline(aes(xintercept=mean_sbp), color="darkgreen", linetype="dashed", size=1) +
  # geom_vline(aes(xintercept=mean_sbp + sd_sbp), color="brown", linetype="dotted", size=1) +
  # geom_vline(aes(xintercept=mean_sbp - sd_sbp), color="brown", linetype="dotted", size=1) +
  # ggtitle("Distribution of\nSystolic Blood Pressure") +
  xlab("Systolic Blood Pressure (mmHg)") + 
  ylab("Frequency") + 
  theme_minimal(base_size = 15) +
  annotate("text", x = mean_sbp, y = Inf, label = paste("Mean:", round(mean_sbp, 2)), color = "red", vjust = 1.5, angle = 90, size = 5) +
  annotate("text", x = mean_sbp + sd_sbp, y = Inf, label = paste("Mean + SD:\n", round(mean_sbp + sd_sbp, 2)), color = "blue", vjust = 1.5, angle = 90, size = 5) +
  annotate("text", x = mean_sbp - sd_sbp, y = Inf, label = paste("Mean - SD:\n", round(mean_sbp - sd_sbp, 2)), color = "blue", vjust = 1.5, angle = 90, size = 5)

qqnorm(bp$sbp, main="Q-Q Plot of Sistolic Blood Pressure")
qqline(bp$sbp, col="blue")

# create the alternate outcome variable PAM:
bp <- bp %>%
  mutate(pam = (2*dbp + sbp) / 3)


mean_pam <- mean(bp$pam, na.rm = TRUE)
sd_pam <- sd(bp$pam, na.rm = TRUE)

shapiro.test(bp$pam)

ggplot(bp, aes(x=pam)) + 
  geom_histogram(fill="bisque3", color="black", binwidth=10)  + 
  # geom_vline(aes(xintercept=mean_pam), color="darkgreen", linetype="dashed", size=1) +
  # geom_vline(aes(xintercept=mean_pam + sd_pam), color="brown", linetype="dotted", size=1) +
  # geom_vline(aes(xintercept=mean_pam - sd_pam), color="brown", linetype="dotted", size=1) +
  # ggtitle("Distribution of\nMean Blood Pressure") +
  xlab("Mean Blood Pressure (mmHg)") + 
  ylab("Frequency") + 
  theme_minimal(base_size = 15) +
  annotate("text", x = mean_pam, y = Inf, label = paste("Mean:", round(mean_pam, 2)), color = "red", vjust = 1.5, angle = 90, size = 5) +
  annotate("text", x = mean_pam + sd_pam, y = Inf, label = paste("Mean + SD:\n", round(mean_pam + sd_pam, 2)), color = "blue", vjust = 1.5, angle = 90, size = 5) +
  annotate("text", x = mean_pam - sd_pam, y = Inf, label = paste("Mean - SD:\n", round(mean_pam - sd_pam, 2)), color = "blue", vjust = 1.5, angle = 90, size = 5)

qqnorm(bp$pam, main="Q-Q Plot of Median Blood Pressure")
qqline(bp$pam, col="blue")


###################### PREDICTORS
# create variable IMC (peso(kg)/h^2 (m)) because the independent variables height and weight are strongly correlated and should not be used simultaneously:
bp <- bp %>%
  mutate(imc = weight / ((height / 100) ^ 2))

# TABLE 1
vars <- c("age", "imc", "wc")
factorVars <- c("sex")
table1 <- CreateTableOne(vars = vars, strata = "sex", data = bp, factorVars = factorVars)
table1_df <- as.data.frame(print(table1, printToggle = FALSE, quote = FALSE, noSpaces = TRUE, varLabels = TRUE))
table1_df <- cbind(Variable = rownames(table1_df), table1_df)
rownames(table1_df) <- NULL

table1_df$Variable <- gsub("age", "Age (years)", table1_df$Variable)
table1_df$Variable <- gsub("imc", "Body Mass Index (BMI, kg/m²)", table1_df$Variable)
table1_df$Variable <- gsub("wc", "Waist Circumference (cm)", table1_df$Variable)
ft <- flextable(table1_df)
ft <- set_header_labels(ft, Variable = "Variable", Overall = "Overall", p = "p-value")
ft <- theme_vanilla(ft)
ft <- autofit(ft)
ft <- bold(ft, part = "header")
ft <- add_footer_lines(ft, "Table 1: Summary of Variables by Sex")
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "Summary_Table.docx")

# DISTRIBUTION PLOTS
# Plot distribution of Age


ggplot(bp, aes(x=age)) +
  geom_histogram(fill="skyblue", color="black", binwidth=5) +
  # ggtitle("Distribution of Age") +
  xlab("Age (years)") +
  ylab("Frequency") +
  theme_minimal(base_size = 15)

qqnorm(bp$age, main="Q-Q Plot of Age")
qqline(bp$age, col="blue")

# Plot distribution of Body Mass Index (BMI)
ggplot(bp, aes(x=imc)) +
  geom_histogram(fill="skyblue", color="black", binwidth=2) +
  # ggtitle("Distribution of Body Mass Index (BMI)") +
  xlab("BMI (kg/m²)") +
  ylab("Frequency") +
  theme_minimal(base_size = 15)

qqnorm(bp$imc, main="Q-Q Plot of BMI")
qqline(bp$imc, col="blue")

# Plot distribution of Waist Circumference
ggplot(bp, aes(x=wc)) +
  geom_histogram(fill="skyblue", color="black", binwidth=5) +
  # ggtitle("Distribution of Waist Circumference") +
  xlab("Waist Circumference (cm)") +
  ylab("Frequency") +
  theme_minimal(base_size = 15)

qqnorm(bp$wc, main="Q-Q Plot of Waist Circunference")
qqline(bp$wc, col="blue")

######### UNIVARIATE

# Define the predictor variables and outcomes
predictors <- c("sex", "age", "imc", "wc")
outcomes <- c("sbp", "pam")

# Function to create and summarize the linear model
create_model_summary <- function(predictor, outcome) {
  formula <- as.formula(paste(outcome, "~", predictor))
  model <- lm(formula, data = bp)
  
  model_summary <- summary(model)
  model_confint <- confint(model)
  model_anova <- anova(model)
  
  list(model = model, summary = model_summary, confint = model_confint, anova = model_anova)
}

# Function to create a beautiful plot
create_model_plot <- function(predictor, outcome, model) {
  if (is.factor(bp[[predictor]])) {
    ggplot(bp, aes_string(x = predictor, y = outcome)) +
      geom_boxplot(fill = "bisque3", color = "black") +
      geom_jitter(width = 0.2) +
      ggtitle(paste("Linear Regression of", outcome, "on", predictor)) +
      xlab(predictor) +
      ylab(outcome) +
      theme_minimal(base_size = 15)
  } else {
    ggplot(bp, aes_string(x = predictor, y = outcome)) +
      geom_point() +
      geom_smooth(method = "lm", col = "bisque3") +
      ggtitle(paste("Linear Regression of", outcome, "on", predictor)) +
      xlab(predictor) +
      ylab(outcome) +
      theme_minimal(base_size = 15)
  }
}

# Create a list to store results
results <- list()

# Loop through predictors and outcomes
for (predictor in predictors) {
  for (outcome in outcomes) {
    result <- create_model_summary(predictor, outcome)
    plot <- create_model_plot(predictor, outcome, result$model)
    
    # Save the results and plot
    results[[paste(predictor, outcome, sep = "_")]] <- list(
      summary = result$summary,
      confint = result$confint,
      anova = result$anova,
      plot = plot
    )
  }
}

# Display results for each model
for (name in names(results)) {
  cat("\n\n### Model:", name, "\n")
  print(results[[name]]$summary)
  cat("\nConfidence Intervals:\n")
  print(results[[name]]$confint)
  cat("\nANOVA:\n")
  print(results[[name]]$anova)
  
  # Display the plot
  print(results[[name]]$plot)
}


########## MODEL BUILDING

# MODEL 1 - OUTCOME SBP
multivariate_model_1a <- lm(sbp ~ age + imc + wc , data = bp) ## r^2 adjusted  0.3195
summary(multivariate_model_1a)
anova(multivariate_model_1a)
confint(multivariate_model_1a)

tidy_model_1a <- tidy(multivariate_model_1a, conf.int = TRUE)
tidy_model_1a <- tidy_model_1a[tidy_model_1a$term != "(Intercept)", ]
ggplot(tidy_model_1a, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Coefficient Plot with 95% Confidence Intervals")


# MODEL 1 - OUTCOME MBP
multivariate_model_1b <- lm(pam ~ age + imc + wc , data = bp) ## r^2 adjusted  0.3458
summary(multivariate_model_1b)
anova(multivariate_model_1b)
confint(multivariate_model_1b)

tidy_model_1b <- tidy(multivariate_model_1b, conf.int = TRUE)
tidy_model_1b <- tidy_model_1b[tidy_model_1b$term != "(Intercept)", ]
ggplot(tidy_model_1b, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Coefficient Plot with 95% Confidence Intervals")


### Evaluate variable multicollinearity

vif(multivariate_model_1a)

# Interpretation of VIF Values:
#   VIF = 1: No multicollinearity.
# 1 < VIF < 5: Moderate multicollinearity, usually not problematic.
# VIF > 5: High multicollinearity, which may be problematic.
# VIF > 10: Very high multicollinearity, often considered problematic and should be addressed.

# as suspected:
# wc: With a VIF of 4.490936, waist circumference shows moderate multicollinearity.
# imc: With a VIF of 4.095716, IMC shows moderate multicollinearity.


########################

# MODEL 2 - OUTCOME SBP
multivariate_model_2a <- lm(sbp ~ age + imc, data = bp) ## r^2 adjusted  0.325
summary(multivariate_model_2a)
anova(multivariate_model_2a)
confint(multivariate_model_2a)

tidy_model_2a <- tidy(multivariate_model_2a, conf.int = TRUE)
tidy_model_2a <- tidy_model_2a[tidy_model_2a$term != "(Intercept)", ]
ggplot(tidy_model_2a, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Coefficient Plot with 95% Confidence Intervals")


# MODEL 2 - OUTCOME MBP
multivariate_model_2b <- lm(pam ~ age + imc, data = bp) ## r^2 adjusted  0.3448
summary(multivariate_model_2b)
anova(multivariate_model_2b)
confint(multivariate_model_2b)

tidy_model_2b <- tidy(multivariate_model_2b, conf.int = TRUE)
tidy_model_2b <- tidy_model_2b[tidy_model_2b$term != "(Intercept)", ]
ggplot(tidy_model_2b, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Coefficient Plot with 95% Confidence Intervals")


# MODEL 3 - OUTCOME SBP
multivariate_model_3a <- lm(sbp ~ age + wc, data = bp) ## r^2 adjusted  0.3052
summary(multivariate_model_3a)
anova(multivariate_model_3a)
confint(multivariate_model_3a)

tidy_model_3a <- tidy(multivariate_model_3a, conf.int = TRUE)
tidy_model_3a <- tidy_model_3a[tidy_model_3a$term != "(Intercept)", ]
ggplot(tidy_model_3a, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Coefficient Plot with 95% Confidence Intervals")


# MODEL 3 - OUTCOME MBP
multivariate_model_3b <- lm(pam ~ age + wc, data = bp) ## r^2 adjusted  0.3371
summary(multivariate_model_3b)
anova(multivariate_model_3b)
confint(multivariate_model_3b)

tidy_model_3b <- tidy(multivariate_model_3b, conf.int = TRUE)
tidy_model_3b <- tidy_model_3b[tidy_model_3b$term != "(Intercept)", ]
ggplot(tidy_model_3b, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Coefficient Plot with 95% Confidence Intervals")


######### MODEL EVALUATION

# compare model 3 and 4 with similar determination coefficient:
anova(multivariate_model_1a,multivariate_model_2a) 


n <- nrow(bp)

# Extract AIC values
aic_mod1a <- extractAIC(multivariate_model_1a, k = 2) # age, imc, wc
aic_mod2a <- extractAIC(multivariate_model_2a, k = 2) # without wc
aic_mod3a <- extractAIC(multivariate_model_3a, k = 2) # withou imc

aic_mod1b <- extractAIC(multivariate_model_1b, k = 2) # age, imc, wc
aic_mod2b <- extractAIC(multivariate_model_2b, k = 2) # without wc
aic_mod3b <- extractAIC(multivariate_model_3b, k = 2) # withou imc

# Extract BIC values
bic_mod1a <- extractAIC(multivariate_model_1a, k = log(n))
bic_mod2a <- extractAIC(multivariate_model_2a, k = log(n))
bic_mod3a <- extractAIC(multivariate_model_3a, k = log(n))

bic_mod1b <- extractAIC(multivariate_model_1b, k = log(n))
bic_mod2b <- extractAIC(multivariate_model_2b, k = log(n))
bic_mod3b <- extractAIC(multivariate_model_3b, k = log(n))

# Create a data frame for comparison
comparison_a <- data.frame(
  Model = c("multivariate_model_1a", "multivariate_model_2a", "multivariate_model_3a"),
  AIC = c(aic_mod1a[2], aic_mod2a[2], aic_mod3a[2]),
  BIC = c(bic_mod1a[2], bic_mod2a[2], bic_mod3a[2])
)

comparison_b <- data.frame(
  Model = c("multivariate_model_1b", "multivariate_model_2b", "multivariate_model_3b"),
  AIC = c(aic_mod1b[2], aic_mod2b[2], aic_mod3b[2]),
  BIC = c(bic_mod1b[2], bic_mod2b[2], bic_mod3b[2])
)



# Print the comparison table
print(comparison)

# Determine the best model according to AIC
best_aic_model_a <- comparison_a[which.min(comparison_a$AIC), "Model"]
cat("According to AIC, the best model is:", best_aic_model, "\n")

# Determine the best model according to BIC
best_bic_model_a <- comparison_a[which.min(comparison_a$BIC), "Model"]
cat("According to BIC, the best model is:", best_bic_model, "\n")



# Determine the best model according to AIC
best_aic_model_b <- comparison_b[which.min(comparison_b$AIC), "Model"]
cat("According to AIC, the best model is:", best_aic_model, "\n")

# Determine the best model according to BIC
best_bic_model_b <- comparison_b[which.min(comparison_b$BIC), "Model"]
cat("According to BIC, the best model is:", best_bic_model, "\n")


## checking for multicollinearity: when independent variables in a model might be correlated:

vif(multivariate_model_1a) 
vif(multivariate_model_2a) 
vif(multivariate_model_3a) 

# Interpretation of VIF Values:
#   VIF = 1: No multicollinearity.
# 1 < VIF < 5: Moderate multicollinearity, usually not problematic.
# VIF > 5: High multicollinearity, which may be problematic.
# VIF > 10: Very high multicollinearity, often considered problematic and should be addressed.


########## TEST ASSUMPTIONS

par(mfrow=c(2,2))
plot(multivariate_model_1a,cook.levels=c(0.25,0.5,1))

#Padronizar os coeficientes do modelo:
library(lm.beta)
lm.beta(multivariate_model_1a)


plot(multivariate_model_1b,cook.levels=c(0.25,0.5,1))

plot(multivariate_model_2a,cook.levels=c(0.25,0.5,1))

plot(multivariate_model_2b,cook.levels=c(0.25,0.5,1))

plot(multivariate_model_3a,cook.levels=c(0.25,0.5,1))

plot(multivariate_model_3b,cook.levels=c(0.25,0.5,1))
