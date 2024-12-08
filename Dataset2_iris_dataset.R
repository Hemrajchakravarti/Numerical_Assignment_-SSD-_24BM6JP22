# Installing necessary library
library(datasets)
library(ggplot2)
library(caret)
library(psych)
library(pheatmap)

# Load the mtcars dataset
data = iris
head(data, n = 6)
#UNIVARIATE ANALYSIS----
## Data overview and summary statistics-----
# 1. Data Overview
data_type = str(data)

### defining the numerical columns----
numerical_columns = c('Sepal.Length', 'Sepal.Width', 'Petal.Length','Petal.Width')
data_description_num = as.data.frame(describe(data)[numerical_columns,])
# 2. Summary Statistics for a numerical variable
View(data_description_num)
data(iris)

# Calculate summary statistics for a specific variable (e.g., Sepal.Length)
mean_value <- mean(iris$Sepal.Length)
median_value <- median(iris$Sepal.Length)
sd_value <- sd(iris$Sepal.Length)
min_value <- min(iris$Sepal.Length)
max_value <- max(iris$Sepal.Length)

# Print the statistics
cat("Mean:", mean_value, "\n")
cat("Median:", median_value, "\n")
cat("Standard Deviation:", sd_value, "\n")
cat("Minimum:", min_value, "\n")
cat("Maximum:", max_value, "\n")

# 3. Distribution Visualization
# Ploting and oulier detection
par(mfrow=c(1,2))
for (i in numerical_columns){
  hist(data[[i]],col='lightblue',main=paste("Histogram of",i,""),
       xlab=i, ylab="Frequency")
  boxplot(data[[i]],col='lightblue',outpch=19,outcol='red',
          main=paste("Histogram of",i,""), xlab=i, ylab="Frequency")
}
par(mfrow=c(1,1))

# 4. Categorical Variable Analysis
### defining the numerical columns----
categorical_columns = c('Species')
for (j in categorical_columns){
  col_counts = table(data[[j]])
  barplot(col_counts, xlab=j, ylab="frequency", main=j, col='lightblue')
}


#===================MULTIVARIATE ANALYSIS=================================================
# 5. Correlation Analysis
corr_data = data[,numerical_columns]
corr_matrix = cor(corr_data)

pheatmap(corr_matrix,display_numbers=TRUE,fontsize_number = 14,fontsize_col=14,  
         legend = TRUE, cluster_rows = FALSE,cluster_cols = FALSE,fontsize_row=14,
         main="Pearson's correlation cofficient")

Sepal.Length <- iris$Sepal.Length
Sepal.Width <- iris$Sepal.Width

# Calculate the Pearson correlation coefficient between mpg and hp
correlation <- cor(Sepal.Length, Sepal.Width)
cat("Pearson Correlation Coefficient between mpg and hp:", correlation, "\n")

# 6. Scatter Plot Visualization
##Scatter-Plot----
for (i in 1:(length(numerical_columns) - 1)) {
  for (j in (i + 1):length(numerical_columns)) {
    # Plot the pairwise scatter plot
    plot(data[[numerical_columns[i]]], data[[numerical_columns[j]]], 
         xlab = numerical_columns[i], ylab = numerical_columns[j], 
         main = paste("Scatter Plot of", numerical_columns[i], "vs", numerical_columns[j]),
         pch=19, col='lightblue')
    reg_line = lm(data[[numerical_columns[j]]] ~ data[[numerical_columns[i]]])
    
    # Add the regression line to the plot
    abline(reg_line, col = "red", lwd = 2)
    # Extract regression statistics
    intercept = coef(reg_line)[1]
    slope = coef(reg_line)[2]
    r_squared = summary(reg_line)$r.squared
    p_value = summary(reg_line)$coefficients[2, 4]
    
    # Format the equation and statistics text
    equation_text = paste("y = ", round(slope, 2), "x +", round(intercept, 2))
    stats_text = paste("R² = ", round(r_squared, 3), "\np-value = ", round(p_value, 10))
    
    # Add the equation and statistics to the plot
    text(x = max(data[[numerical_columns[i]]]) * 0., 
         y = max(data[[numerical_columns[j]]]) * 0.9, 
         labels = equation_text, col = "blue", cex = 0.8)
    text(x = max(data[[numerical_columns[i]]]) * 0.6, 
         y = max(data[[numerical_columns[j]]]) * 0.85, 
         labels = stats_text, col = "blue", cex = 0.8)
  }
}


# 7. Multiple Regression
par(mfrow=c(2,2))
mult_reg = c('Sepal.Length', 'Sepal.Width', 'Petal.Length','Petal.Width')
mult_reg_data = iris[,mult_reg]
fit_mult = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=mult_reg_data)
summary(fit_mult)
plot(fit_mult)

# 8.Model Diagnostics
shapiro.test(residuals(fit_mult))
par(mfrow=c(1,1))

# ADVANCED ANLYTICS----
#9. Principal Component Analysis (PCA)
complete_scaled = scale(mtcars)
pca = prcomp(complete_scaled, center = TRUE, scale. = TRUE)
summary(pca)
pca_scores = pca$x

# 10. PCA Interpretation (contribution of each variable to the principal components)
pca_loadings = pca$rotation

# Plot the variance explained by each principal component
plot(pca, main = "PCA - Variance Explained by Each Component")
screeplot(pca, main = "Scree Plot", type = "lines", col = "blue")

# 10. PCA Interpretation (contribution of each variable to the principal components)
# Create a biplot of the first two principal components
biplot(pca, scale = 0, cex = 0.7) 


