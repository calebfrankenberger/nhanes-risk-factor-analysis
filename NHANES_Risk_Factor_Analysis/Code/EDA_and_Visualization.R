#############################################################################################
# Author: Caleb Frankenberger                                                               # 
# Date: 11/13/2024                                                                          #
# Project: NHANES Diabetes Risk Factor Analysis                                             #
# Purpose: Exploratory data analysis and visualization                                      #
#############################################################################################


###################################################################
# Initial File and Data Setup
###################################################################

# Clean the environment and set up libraries
rm(list=ls())
library(car)

# Load the data and set path variables
data_path <- "C:/path/to/project/NHANES_Risk_Factor_Analysis/Data/Processed/clean_data.csv"
data <- read.csv(data_path)

visualizations_path <- "C:/path/to/project/NHANES_Risk_Factor_Analysis/Data/Visualizations"
setwd(visualizations_path)

# Define color combinations 
base_color <- "#377eb8"  
light_shade <- "#a6cee3"  
dark_shade <- "#08519c"  
cols <- c(base_color, light_shade, dark_shade)


par(bg="white", cex=1.1)

# Check data structure
str(data)

# Remove SEQN column, we won't be using it
data <- data[, !names(data) %in% "SEQN"]

# Separate binary variables (Diabetes_Status, Smoking_Status)
categorical_vars <- c("Diabetes_Status",  "Education_Level",  "Hypertension", "Race_Ethnicity")
continuous_vars <- c("Age", "BMI")
all_vars <- c(continuous_vars, categorical_vars)

###################################################################
# Plot Matrix Creation
###################################################################
# Set up the 6x6 grid
pdf("plot_matrix.pdf", width=10, height=10)
par(mfrow = c(6, 6), mar = c(2, 2, 2, 2), oma = c(4, 4, 4, 4), lwd = 1)

# Loop through rows and columns with indices
for (i in seq_along(all_vars)) {
  for (j in seq_along(all_vars)) {
    row_var <- all_vars[i]
    col_var <- all_vars[j]
    
    # Diagonal: Histograms (continuous) or barplots (categorical)
    if (row_var == col_var) {
      if (row_var %in% continuous_vars) {
        hist(data[[row_var]], prob = TRUE, 
             main = "", xlab = "", ylab = "", 
             col = light_shade, border = "black")
        lines(density(data[[row_var]], na.rm = TRUE), 
              col = dark_shade, lwd = 2)
      } else {
        barplot(table(data[[row_var]]), 
                main = "", xlab = "", ylab = "", 
                col = light_shade, border = "black")
      }
      
      # Continuous vs. Continuous: Scatterplots
    } else if (row_var %in% continuous_vars && col_var %in% continuous_vars) {
      plot(data[[col_var]], data[[row_var]], 
           main = "", xlab = "", ylab = "", 
           col = dark_shade, pch = 19)
      box(which = "plot", col = "black") 
      
      # Continuous vs. Categorical: Boxplots
    } else if ((row_var %in% continuous_vars && col_var %in% categorical_vars) || 
               (row_var %in% categorical_vars && col_var %in% continuous_vars)) {
      boxplot(data[[ifelse(row_var %in% continuous_vars, row_var, col_var)]] ~ 
                data[[ifelse(row_var %in% categorical_vars, row_var, col_var)]], 
              main = "", xlab = "", ylab = "", 
              col = cols, border = "black")
      
      # Categorical vs. Categorical: Mosaic plots
    } else if (row_var %in% categorical_vars && col_var %in% categorical_vars) {
      table_data <- table(data[[row_var]], data[[col_var]])
      mosaicplot(table_data, 
                 main = "", xlab = "", ylab = "", 
                 col = cols, border = "black")
    }
    
    # Column/Row labels
    if (i == 1) 
      mtext(col_var, side = 3, line = 3, cex = 0.9, font=2)
    if (j == 1) 
      mtext(row_var, side = 2, line = 3, cex = 0.9, font=2)
    
  }
}
dev.off()


###################################################################
# Computing Generalized VIFs
###################################################################
log_model <- glm(Diabetes_Status ~ Age + BMI + Education_Level + 
                   Hypertension + Race_Ethnicity, family=binomial(),
                 data=data)
vif(log_model)


###################################################################
# Checking relationship between predictors and log-odds of response
###################################################################
log_odds <- log(fitted(log_model) / (1 - fitted(log_model)))
par(mar = c(5, 5, 5, 2), oma=c(1, 1, 1, 1), cex=1.0)

# Age
plot(data$Age, log_odds,
     main="Log-Odds of Diabetes Status\nvs Age",
     xlab="Age",
     ylab="Log-Odds of Predicted Diabetes Status",
     xlim=c(min(data$Age), max(data$Age)),
     pch=21, bg=base_color, col=dark_shade)

# BMI
plot(data$BMI, log_odds,
     main="Log-Odds of Diabetes Status\nvs BMI",
     xlab="BMI",
     ylab="Log-Odds of Predicted Diabetes Status",
     xlim=c(min(data$BMI), max(data$BMI)),
     pch=21, bg=base_color, col=dark_shade)

# Function to create barplot using mean log-odds (to check against categorical predictors)
plot_categorical_vs_log_odds <- function(cat_var, data, log_odds, base_color, light_shade, dark_shade) {
  mean_log_odds <- aggregate(log_odds ~ data[[cat_var]], FUN = mean)
  colnames(mean_log_odds) <- c(cat_var, "Mean_Log_Odds")
  
  barplot(mean_log_odds$Mean_Log_Odds,
          names.arg = mean_log_odds[[cat_var]],
          main = paste("Mean Log-Odds of Diabetes Status\nby", cat_var),
          xlab = cat_var,
          ylab = "Mean Log-Odds of\nPredicted Diabetes Status",
          col = light_shade,
          border = dark_shade) 
}

# Education level
plot_categorical_vs_log_odds("Education_Level", data, log_odds, base_color, light_shade, dark_shade)

# Hypertension
plot_categorical_vs_log_odds("Hypertension", data, log_odds, base_color, light_shade, dark_shade)

# Race
plot_categorical_vs_log_odds("Race_Ethnicity", data, log_odds, base_color, light_shade, dark_shade)


## End of Script ##