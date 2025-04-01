###############first data set#############################################

#importation des donner du zip : gaze turbine co and nox emmission
gt_2011 <- read.table(file = file.choose(), header = TRUE, sep = ",", dec = ".")
gt_2012<- read.table(file = file.choose(), header = TRUE, sep = ",", dec = ".")
gt_2013<- read.table(file = file.choose(), header = TRUE, sep = ",", dec = ".")
gt_2014<- read.table(file = file.choose(), header = TRUE, sep = ",", dec = ".")
gt_2015<- read.table(file = file.choose(), header = TRUE, sep = ",", dec = ".")


# Concatenate the data frames data from 2011year to  2015
gt_all <- rbind(gt_2011, gt_2012, gt_2013, gt_2014, gt_2015)
View(gt_all)

summary(gt_all)
str(gt_all)

num_columns <- ncol(gt_all)
print(paste("Nombre de colonnes:", num_columns))

# Nbr  lignes
num_rows <- nrow(gt_all)
print(paste("Nombre de lignes:", num_rows))

# Dimensions (rows, columns)
dim_data <- dim(gt_all)
print(paste("Dimensions (rows, columns):", paste(dim_data, collapse=" x ")))



sapply(gt_all, class)



# graphiquement
library(naniar)

# Afficher un graphique des variables avec des valeurs manquantes
gg_miss_var(gt_all)

library(ggplot2)

#  boxplot
for (col in colnames(gt_all)) {
  boxplot(gt_all[[col]], main = paste("Boxplot de", col), col = "lightblue")
}




# Check for missing values
missing_values <- colSums(is.na(gt_all))
print(missing_values)


# Check for duplicated rows
duplicated_rows <- gt_all[duplicated(gt_all), ]
print(paste("Number of duplicated rows:", nrow(duplicated_rows)))
#==> "Number of duplicated rows: 7"

# Remove duplicated rows
gt_all <- gt_all[!duplicated(gt_all), ]
# Verify no duplicated rows
print(paste("Number of duplicated rows after removal:", sum(duplicated(gt_all))))


# Z-Score Standardization function
z_score_scale <- function(x) {
  (x - mean(x)) / sd(x)
}

# Apply standardization to each column
standardized_data <- as.data.frame(lapply(gt_all, z_score_scale))

# View standardized data
print(standardized_data)
View(standardized_data)

# Check the column names again to ensure CO is the dependent variable
print(names(standardized_data))  # Should print the column names including "CO"

# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Create a scatterplot for each independent variable, excluding "CO"
independent_vars <- setdiff(names(standardized_data), "CO")  # Exclude "CO"

# Create scatterplots for each independent variable against CO
plot_list <- lapply(independent_vars, function(var) {
  ggplot(standardized_data, aes_string(x = var, y = "CO")) +
    geom_point(color = "blue") +
    labs(
      title = paste("Scatterplot of CO vs", var),
      x = var ,
      y = "CO"
    ) +
    theme_minimal()
})

# Check the length of the plot list to ensure it's populated
print(length(plot_list))  # Should print the number of independent variables

# Combine all scatterplots into one grid layout, only if there are valid plots
if (length(plot_list) > 0) {
  do.call(grid.arrange, c(plot_list, ncol = 2))  # Adjust ncol to arrange plots in a grid
} else {
  print("No plots to display")
}

print(plot_list)

print(plot_list[[2]])  # Check the first plot

# Loop through each plot in plot_list and save them
for (i in 1:length(plot_list)) {
  ggsave(paste0("scatterplot_", i, ".png"), plot = plot_list[[i]], width = 8, height = 6)
}

getwd()  # This will show the current working directory



# Q-Q plot to check normality visually
qqnorm(standardized_data$AT)
qqline(standardized_data$AT, col = "red")



# Histogram to check the distribution
hist(standardized_data$AH, breaks = 50, col = "lightblue", main = "Histogram of AH", xlab = "AH")






##### ************partie correlation ****************


# Load necessary libraries
library(ggplot2)
library(corrplot)
install.packages("caret")
library(caret)

# Calculate Spearman correlation matrix
spearman_corr <- cor(standardized_data, method = "spearman")


print("Spearman Correlation Matrix:")
print(spearman_corr)

# Visualize Spearman Correlation Matrix using corrplot
corrplot(spearman_corr, method = "circle", type = "upper", tl.cex = 0.7, number.cex = 0.7)

# Spearman Correlation for CO (Carbon Monoxide) vs. other variables
co_spearman <- cor.test(standardized_data$CO, standardized_data$CDP, method = "spearman")  # Example with TIT (Turbine Inlet Temperature)
print(co_spearman)

# Find and remove highly correlated variables to avoid multicollinearity (for regression)
# Threshold for high correlation (e.g., 0.9 or -0.9)
high_corr_vars <- findCorrelation(spearman_corr, cutoff = 0.8)

# Print the variables to exclude due to multicollinearity
print("Variables to exclude due to high correlation:")
print(names(standardized_data)[high_corr_vars])

# Remove highly correlated variables
standardized_data <- standardized_data[, -high_corr_vars]

# Check the data after removal
print(head(standardized_data))




######### partie  regressin model first execution **********************

# Load necessary libraries
library(MASS)        # For stepwise regression
library(car)         # For VIF (Variance Inflation Factor)
library(lmtest)      # For Durbin-Watson test
library(ggplot2)     # For plotting

# Assume the data is in a data frame called `standardized_data`
# Fit the initial regression model with all predictors

full_model <- lm(CO ~ AT + AP +AH +AFDP +TAT + TEY  , data = standardized_data)
summary(full_model)
# Perform stepwise regression using both directions
#stepwise_model <- stepAIC(full_model, direction = "backward") ==> jarav=btha ama bic 3tat resultat 5ir 

# Summary of the final model after stepwise selection
#summary(stepwise_model)

stepwise_model_bic <- stepAIC(full_model, direction = "backward", k = log(nrow(standardized_data)))
summary(stepwise_model_bic)




# Adjusted R-squared value from the stepwise model
adjusted_r_squared <- summary(stepwise_model_bic)$adj.r.squared
cat("Adjusted R-squared: ", adjusted_r_squared, "\n")

# Durbin-Watson test for autocorrelation
dw_test <- dwtest(stepwise_model_bic)
cat("Durbin-Watson statistic: ", dw_test$statistic, "\n")

# Histogramme des résidus
hist(residuals(full_model), main = "Histogramme des résidus", xlab = "Résidus", breaks = 30)

# Q-Q plot
qqnorm(residuals(full_model))
qqline(residuals(full_model), col = "red")


# Coefficients of the final model
cat("Coefficients:\n")
print(summary(stepwise_model_bic)$coefficients)

# Check for multicollinearity using VIF
vif_values <- vif(stepwise_model_bic)
cat("Variance Inflation Factors (VIFs):\n")
print(vif_values)



residuals_model <- residuals(stepwise_model_bic)

# 1. **Normality Check**: Q-Q plot to check if residuals follow a normal distribution
qqnorm(residuals_model)
qqline(residuals_model, col = "red")
# Histogram to check the distribution of residuals
hist(residuals_model, breaks = 50, col = "lightblue", 
     main = "Histogram of Residuals", xlab = "Residuals")

# 2. **Homoscedasticity Check**: Plot of residuals vs fitted values
fitted_values <- fitted(stepwise_model_bic)
plot(fitted_values, residuals_model, 
     main = "Residuals vs Fitted Values", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")







######### partie  regressin model second execution **********************

# Load necessary libraries
library(MASS) # For calculating Mahalanobis distance
library(car)  # For checking VIF (Variance Inflation Factor)


# 3. Cook's Distance and Leverage to check for influential points
cooks_dist <- cooks.distance(stepwise_model_bic)
leverage <- hatvalues(stepwise_model_bic)

# Check if any Cook's distance > 1 and if leverage > 1/n

influential_points <- which(cooks_dist > 1 | leverage > (2 * length(residuals_model) / length(stepwise_model_bic$coefficients)))

cat("Influential points (Cook's distance > 1 or leverage > threshold):", influential_points, "\n")

# 4. Check for outliers based on standard residuals (greater than absolute value 3.29)
std_residuals <- rstandard(stepwise_model_bic)
outliers_std_residuals <- which(abs(std_residuals) > 3.29)

cat("Outliers based on standard residuals:", outliers_std_residuals, "\n")
# 5. Multivariate outliers using Mahalanobis distance
mahalanobis_dist <- mahalanobis(standardized_data[, c("AT", "AP", "AH", "AFDP", "TAT", "TEY", "CO", "NOX")], 
                                colMeans(standardized_data[, c("AT", "AP", "AH", "AFDP", "TAT", "TEY", "CO", "NOX")]), 
                                cov(standardized_data[, c("AT", "AP", "AH", "AFDP", "TAT", "TEY", "CO", "NOX")]))
p_values_mahalanobis <- 1 - pchisq(mahalanobis_dist, df = 4)

# Identify multivariate outliers with probability < 0.001
multivariate_outliers <- which(p_values_mahalanobis < 0.001)

cat("Multivariate outliers based on Mahalanobis distance:", multivariate_outliers, "\n")

# Combine all outliers (standard residuals and multivariate)
all_outliers <- unique(c(outliers_std_residuals, multivariate_outliers))
length(all_outliers)
# Calculate the percentage of outliers
percentage_outliers <- (length(all_outliers) / nrow(standardized_data)) * 100

# Print the result
cat("Percentage of outliers: ", percentage_outliers, "%\n")



# 6. Filter out the identified outliers and refit the model
filtered_data <- standardized_data[-all_outliers, ]
# Fit both models using the same dataset (filtered_data)
full_model2 <- lm(CO ~ AT + AP + AH + AFDP + TAT + TEY , data = filtered_data)
stepwise_model_bic2 <- stepAIC(full_model2, direction = "backward", k = log(nrow(filtered_data)))


dw_test2 <- dwtest(stepwise_model_bic2)
cat("Durbin-Watson statistic: ", dw_test2$statistic, "\n")

# Coefficients of the final model
cat("Coefficients:\n")
print(summary(stepwise_model_bic2)$coefficients)



# Check for multicollinearity using VIF
vif_values <- vif(stepwise_model_bic2)
cat("Variance Inflation Factors (VIFs) after filtering outliers:\n")
print(vif_values)

# 8. Durbin-Watson test for autocorrelation of residuals
library(lmtest)
dw_test <- dwtest(stepwise_model_bic2)
cat("Durbin-Watson test statistic:", dw_test$statistic, "\n")

# 10. Coefficients table
coefficients_table <- summary(stepwise_model_bic2)$coefficients
print(coefficients_table)

# 11. Model Summary (R-squared, Adjusted R-squared, etc.)
model_summary <- summary(stepwise_model_bic2)
cat("Model Summary:\n")
cat("R-squared:", model_summary$r.squared, "\n")
cat("Adjusted R-squared:", model_summary$adj.r.squared, "\n")
cat("Std. Error of Estimate:", model_summary$sigma, "\n")
cat("Durbin-Watson statistic:", dw_test$statistic, "\n")



residuals_model2 <- residuals(stepwise_model_bic2)
summary(stepwise_model_bic2)
# 1. **Normality Check**: Q-Q plot to check if residuals follow a normal distribution
qqnorm(residuals_model2)
qqline(residuals_model2, col = "red")
# Histogram to check the distribution of residuals
hist(residuals_model2, breaks = 50, col = "lightblue", 
     main = "Histogram of Residuals", xlab = "Residuals")

# 2. **Homoscedasticity Check**: Plot of residuals vs fitted values
fitted_values2 <- fitted(stepwise_model_bic2)
plot(fitted_values2, residuals_model2, 
     main = "Residuals vs Fitted Values", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")










###############second data set#############################################


ai4i2020 <- read.table(file = file.choose(), header = TRUE, sep = ",", dec = ".")

View(ai4i2020)
str(ai4i2020)
summary(ai4i2020)



# Check for missing values
missing_values <- colSums(is.na(ai4i2020))
print("Missing Values Per Column:")
print(missing_values)

# Check for duplicated rows
duplicated_rows <- ai4i2020[duplicated(ai4i2020), ]
print(paste("Number of duplicated rows:", nrow(duplicated_rows)))

# Install required packages if not already installed
if (!require("dplyr")) install.packages("dplyr")
if (!require("gmodels")) install.packages("gmodels")

library(dplyr)
library(gmodels)

# Select relevant columns
data <- ai4i2020 %>% select(Type, Machine.failure)

# Convert Machine.failure to a factor for analysis
data$Machine.failure <- factor(data$Machine.failure, labels = c("No", "Yes"))

# Crosstabulation
crosstab <- table(data$Type, data$Machine.failure)
print(crosstab)

# Add proportions
prop_table <- prop.table(crosstab, margin = 1) * 100  # Row percentages
print(round(prop_table, 1))  # Rounded percentages


library(ggplot2)
# Convert the proportions table into a data frame
prop_df <- as.data.frame(as.table(prop_table))
colnames(prop_df) <- c("Type", "MachineFailure", "Percentage")

# View the data frame
print(prop_df)


# Load ggplot2
library(ggplot2)

# Create the bar chart with enhancements
ggplot(prop_df, aes(x = Type, y = Percentage, fill = MachineFailure)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.7) +  # Stacked bars with adjusted width
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 5, color = "white") +  # Data labels with larger size
  scale_fill_manual(values = c("#1f77b4", "#e41a1c"), labels = c("No", "Yes")) +  # Custom colors (blue and red)
  labs(title = "Machine Failures by Product Type",
       x = "Product Type",
       y = "Percentage (%)",
       fill = "Machine Failure") +  # Custom title and labels
  theme_minimal(base_size = 14) +  # Increased base font size
  theme(
    text = element_text(size = 14),  # Larger text size for better readability
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Centered title with larger font
    axis.title = element_text(size = 14),  # Axis titles
    axis.text = element_text(size = 12),  # Axis labels
    legend.title = element_text(size = 14),  # Legend title
    legend.text = element_text(size = 12),  # Legend text
    panel.grid.major = element_line(color = "grey80", size = 0.5),  # Subtle gridlines for readability
    panel.grid.minor = element_blank()  # Remove minor gridlines for clarity
  )


# Chi-square test
chi_test <- chisq.test(crosstab)

# Print results
print(chi_test)


# Perform Fisher's Exact Test
fisher_test <- fisher.test(crosstab)

# Print Fisher's Exact Test results
cat("\nFisher's Exact Test Results:\n")
cat(sprintf("p-value = %.4f\n", fisher_test$p.value))


# Check Cramer's V for effect size (optional)
library(lsr)
cramersV(crosstab)

# Calculate Cramér's V for association strength
cramers_v <- cramersV(crosstab)




# Print Chi-squared test results
cat("\nChi-squared Test Results:\n")
cat(sprintf("X-squared = %.3f, df = %d, p-value = %.4f\n", chi_test$statistic, chi_test$parameter, chi_test$p.value))

# Print Fisher's Exact Test results
cat("\nFisher's Exact Test Results:\n")
cat(sprintf("p-value = %.4f\n", fisher_test$p.value))

# Print Cramér's V result
cat("\nCramér's V Index:\n")
cat(sprintf("Cramér's V = %.4f\n", cramers_v))

# Interpretation of the statistical tests
cat("\nInterpretation:\n")
if(chi_test$p.value < 0.05) {
  cat("The result is statistically significant, indicating that product type affects machine failure rates.\n")
} else {
  cat("The result is not statistically significant, suggesting no clear relationship between product type and machine failure.\n")
}

if(cramers_v < 0.1) {
  cat("The strength of the association is weak, meaning product type has a minimal effect on machine failure.\n")
} else if(cramers_v >= 0.1 & cramers_v < 0.3) {
  cat("The strength of the association is moderate.\n")
} else {
  cat("The strength of the association is strong.\n")
}




################################
library(dplyr)
library(coin)  # For Kruskal-Wallis Monte Carlo simulation

# Perform Normality Tests by Type
normality_results <- ai4i2020 %>%
  group_by(Type) %>%
  summarise(
    Kolmogorov_Statistic = ks.test(Rotational.speed..rpm., "pnorm", mean(Rotational.speed..rpm.), sd(Rotational.speed..rpm.))$statistic,
    Kolmogorov_p_value = ks.test(Rotational.speed..rpm., "pnorm", mean(Rotational.speed..rpm.), sd(Rotational.speed..rpm.))$p.value,
    Shapiro_Statistic = if (n() <= 5000) shapiro.test(Rotational.speed..rpm.)$statistic else NA,
    Shapiro_p_value = if (n() <= 5000) shapiro.test(Rotational.speed..rpm.)$p.value else NA
  )

# Print Normality Test Results
print("Normality Test Results:")
print(normality_results)

# Perform Kruskal-Wallis Test for Rotational Speed by Type
kruskal_test <- kruskal.test(Rotational.speed..rpm. ~ Type, data = ai4i2020)

# Print Kruskal-Wallis Test Results
cat("Kruskal-Wallis Test Results:\n")
print(kruskal_test)


######################################first t-test###################

# Normality test (Kolmogorov-Smirnov and Shapiro-Wilk) for Rotational Speed by Machine Failure
normality_results <- ai4i2020 %>%
  group_by(Machine.Failure) %>%
  summarise(
    Kolmogorov_Statistic = ks.test(Rotational.speed..rpm., "pnorm", mean(Rotational.speed..rpm., na.rm = TRUE), sd(Rotational.speed..rpm., na.rm = TRUE))$statistic,
    Kolmogorov_p_value = ks.test(Rotational.speed..rpm., "pnorm", mean(Rotational.speed..rpm., na.rm = TRUE), sd(Rotational.speed..rpm., na.rm = TRUE))$p.value,
    Shapiro_Statistic = shapiro.test(Rotational.speed..rpm.)$statistic,
    Shapiro_p_value = shapiro.test(Rotational.speed..rpm.)$p.value
  )

# Print Normality Test Results
print("Normality Test Results:")
print(normality_results)

# Levene's Test for Equality of Variances
library(car)
# Convert Machine.failure to a factor if it is not already
ai4i2020$Machine.failure <- as.factor(ai4i2020$Machine.failure)
levene_test <- leveneTest(Rotational.speed..rpm. ~ Machine.failure, data = ai4i2020)

# Print Levene's Test Results
cat("Levene's Test Results for Equality of Variances:\n")
print(levene_test)

# t-Test for Equality of Means (Welch's t-test if variances are unequal)
t_test_result <- t.test(Rotational.speed..rpm. ~ Machine.failure, data = ai4i2020, var.equal = FALSE)

# Print t-Test Results
cat("t-Test Results for Equality of Means (Welch's Test):\n")
print(t_test_result)




########################second t-test#######################
# Load necessary libraries
library(car)

# Convert 'Tool.wear.failure' to a factor if it isn't already
ai4i2020$TWF <- as.factor(ai4i2020$TWF)

# 1. Normality test (Kolmogorov-Smirnov and Shapiro-Wilk)
# Test for normality in each group
library(dplyr)

# Subset the data to a manageable size for the Shapiro-Wilk test (e.g., 5000 for each group)
normality_results <- ai4i2020 %>%
  group_by(TWF) %>%
  summarise(
    # Kolmogorov-Smirnov Test
    Kolmogorov_Statistic = ks.test(Rotational.speed..rpm., "pnorm", 
                                   mean(Rotational.speed..rpm., na.rm = TRUE), 
                                   sd(Rotational.speed..rpm., na.rm = TRUE))$statistic,
    Kolmogorov_p_value = ks.test(Rotational.speed..rpm., "pnorm", 
                                 mean(Rotational.speed..rpm., na.rm = TRUE), 
                                 sd(Rotational.speed..rpm., na.rm = TRUE))$p.value,
    
    # Shapiro-Wilk Test with subset if necessary
    Shapiro_Statistic = ifelse(n() > 5000, 
                               shapiro.test(sample(Rotational.speed..rpm., 5000))$statistic, 
                               shapiro.test(Rotational.speed..rpm.)$statistic),
    Shapiro_p_value = ifelse(n() > 5000, 
                             shapiro.test(sample(Rotational.speed..rpm., 5000))$p.value, 
                             shapiro.test(Rotational.speed..rpm.)$p.value)
  )

# Print the normality results
print(normality_results)



# 2. Levene's Test for Equality of Variances
levene_test <- leveneTest(Rotational.speed..rpm. ~ TWF, data = ai4i2020)

# Print Levene's test results
cat("\nLevene's Test for Equality of Variances:\n")
print(levene_test)

# 3. t-Test for Equality of Means
# Perform Welch's t-test (assuming unequal variances, which is more general)
t_test_result <- t.test(Rotational.speed..rpm. ~ TWF, data = ai4i2020, var.equal = FALSE)

# Print t-test results
cat("\nt-Test for Equality of Means (Welch's Test):\n")
print(t_test_result)






