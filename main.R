ds_path <- "datasets/Anesthesia-DatasetA.sav"
ds <- read_spss(ds_path)

hist(ds$EuroScore)
hist(ds$ICU_LoS, nclass = 20)


ggplot(ds, aes(x=ICU_LoS)) +
  geom_histogram(alpha=0.5, col=1, fill=13, bins=20)+
  ggtitle("Distribution of Length-of-Stay")

# Load necessary libraries
library(ggplot2)

# Fit a GLM with Gamma distribution
model <- glm(
  ICU_LoS ~ Age + Surgery, 
  family = Gamma(link = "log"), 
  data = ds
)

# Summary of the model
summary(model)

# Predict the fitted values
ds$fitted <- fitted(model)

# Plot the observed vs fitted values
ggplot(ds, aes(x = ICU_LoS, y = fitted)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'blue') +
  labs(title = 'Observed vs Fitted Values',
       x = 'Observed ICU Length of Stay',
       y = 'Fitted ICU Length of Stay') +
  theme_minimal()