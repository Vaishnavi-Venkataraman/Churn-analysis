#https://www.kaggle.com/datasets/blastchar/telco-customer-churn - dataset link

if (!require("corrplot")) install.packages("corrplot")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("ggthemes")) install.packages("ggthemes")
if (!require("randomForest")) install.packages("randomForest")
if (!require("party")) install.packages("party")
if(!require("knitr")) install.packages("knitr")
library(pROC)
library(plyr)
library(ggplot2)
library(caret)
library(MASS)

churn <- read.csv('Dataset.csv')
churn <- churn[complete.cases(churn), ]  # Remove rows with missing values
cols_recode1 <- c(10:15)
for (i in 1:ncol(churn[, cols_recode1])) {
  churn[, cols_recode1][, i] <- as.factor(mapvalues(churn[, cols_recode1][, i],from = c("No internet service"),to = c("No")))
}
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines,from = c("No phone service"),to = c("No")))

group_tenure <- function(tenure) {
  if (tenure >= 0 & tenure <= 12) {
    return('0-12 Month')
  } else if (tenure > 12 & tenure <= 24) {
    return('12-24 Month')
  } else if (tenure > 24 & tenure <= 48) {
    return('24-48 Month')
  } else if (tenure > 48 & tenure <= 60) {
    return('48-60 Month')
  } else if (tenure > 60) {
    return('> 60 Month')
  }
}
churn$tenure_group <- sapply(churn$tenure, group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)
churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,from = c("0", "1"),to = c("No", "Yes")))
churn$customerID <- NULL
churn$tenure <- NULL
churn$Churn <- as.factor(mapvalues(churn$Churn,from = c("No", "Yes"),to = c("0", "1")))

menu_options <- c(
  "1. Exploratory Data Analysis (EDA)",
  "2. Model Building and Evaluation",
  "3. ANOVA",
  "4. Hypothesis Testing",
  "0. Quit"
)

while (TRUE) {
  cat("\nChoose an option:\n")
  for (i in seq_along(menu_options)) {
    cat(menu_options[i], "\n")
  }
  choice <- as.numeric(readline("Enter your choice: "))
 
  if (choice == 1) {
    numeric.var <- sapply(churn, is.numeric)
    corr.matrix <- cor(churn[, numeric.var])
    corrplot(corr.matrix, main = "\n\nCorrelation Plot for Numerical Variables", method = "number")
   
    p1 <- ggplot(churn, aes(x = gender, fill = gender)) +ggtitle("Gender") +xlab("Gender") +geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5) +ylab("Percentage") +coord_flip() +theme_minimal()+scale_fill_manual(values = c("pink", "lightblue"), labels = c("Female", "Male"))
   
    p2 <- ggplot(churn, aes(x = SeniorCitizen, fill = SeniorCitizen)) +ggtitle("Senior Citizen") +
      xlab("Senior Citizen") +geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5) +ylab("Percentage") +coord_flip() +theme_minimal() +scale_fill_manual(values = c("coral1", "lightgreen"), labels = c("No", "Yes"))
   
    p3 <- ggplot(churn, aes(x = tenure_group, fill = Churn)) +geom_bar() + labs(x = "Tenure Group", y = "Count", title = "Churn by Tenure Group")
   
    p4 <- ggplot(churn, aes(x = tenure_group, fill = Churn)) + geom_bar() +facet_grid(gender ~ SeniorCitizen) +labs(x = "Tenure Group", y = "Count", title = "Churn by Tenure Group, Gender, and Senior Citizen")
   
    p5 <- ggplot(churn, aes(x = MonthlyCharges, fill = Churn)) +geom_density(alpha = 0.5) +labs(x = "Monthly Charges", y = "Density", title = "Monthly Charges Density by Churn")
   
    grid.arrange(p1, p2,ncol = 1)
    grid.arrange(p3,ncol=1)
    grid.arrange(p4,ncol=1)
    grid.arrange(p5,ncol=1)
   
    gender_distribution <- table(churn$gender)
    cat("Gender Distribution:")
    print(gender_distribution)
    senior_citizen_distribution <- table(churn$SeniorCitizen)
    cat("\nSenior Citizen Distribution:")
    print(senior_citizen_distribution)
    churn_by_tenure <- table(churn$tenure_group, churn$Churn)
    cat("\nChurn Counts by Tenure Group:\n")
    print(churn_by_tenure)
   
    # Density statistics for Monthly Charges by Churn
    monthly_charges_density_stats <- churn %>%group_by(Churn) %>%summarise(mean = mean(MonthlyCharges, na.rm = TRUE),sd = sd(MonthlyCharges, na.rm = TRUE),median = median(MonthlyCharges, na.rm = TRUE))
    cat("\nMonthly Charges Density Statistics by Churn:\n")
    print(monthly_charges_density_stats)
   
  } else if (choice == 2) {
    model_menu_options <- c(
      "1. Linear Regression",
      "2. Decision Tree",
      "4. Back to Main Menu"
    )
   
    while (TRUE) {
      cat("\nChoose a model to build and evaluate:\n")
      for (i in seq_along(model_menu_options)) {
        cat(model_menu_options[i], "\n")
      }
      model_choice <- as.numeric(readline("Enter your choice: "))
     
      if (model_choice == 1) {
        intrain <- createDataPartition(churn$MonthlyCharges, p = 0.7, list = FALSE)
        set.seed(2017)
        training <- churn[intrain, ]
        testing <- churn[-intrain, ]
       
        LinearModel <- lm(MonthlyCharges ~ ., data = training)
        coef_values <- coef(LinearModel)
        equation <- paste0("MonthlyCharges = ", round(coef_values[1], 3), " + ", paste0(paste(names(coef_values)[-1], "*", round(coef_values[-1], 3), collapse = " + ")))
        predictions <- predict(LinearModel, newdata = testing)
        mse <- mean((predictions - testing$MonthlyCharges)^2)
       
        b <- ggplot(training, aes_string(x = "TotalCharges", y = "MonthlyCharges")) +geom_point(color = 'lightblue') +geom_smooth(method = 'lm', color = 'red', se = FALSE) +labs(title = paste("Regression Line: Monthly Charges vs TotalCharges"), x = "TotalCharges", y = "Monthly Charges") +theme_minimal()
        print(b)
        a<-ggplot() + geom_point(aes(x = testing$MonthlyCharges, y = predictions), color = 'blue') + geom_smooth(aes(x = testing$MonthlyCharges, y = predictions), method = 'lm', color = 'red') + labs(title = "Actual vs Predicted Monthly Charges", x = "Actual Monthly Charges", y = "Predicted Monthly Charges") + theme_minimal()
        print(a)
        cat("\nLinear Regression Equation:\n", equation, "\n")
        cat("\nMean Squared Error (MSE):", mse, "\n")
 
      } else if (model_choice == 2)
      {
        intrain <- createDataPartition(churn$Churn, p = 0.7, list = FALSE)
        set.seed(2017)
        training <- churn[intrain, ]
        testing <- churn[-intrain, ]
       
        training$Contract <- factor(training$Contract)
        training$tenure_group <- factor(training$tenure_group)
        training$PaperlessBilling <- factor(training$PaperlessBilling)
       
        # testing data has the same factor levels as the training data
        testing$Contract <- factor(testing$Contract, levels = levels(training$Contract))
        testing$tenure_group <- factor(testing$tenure_group, levels = levels(training$tenure_group))
        testing$PaperlessBilling <- factor(testing$PaperlessBilling, levels = levels(training$PaperlessBilling))
       
        tree <- ctree(Churn ~ Contract + tenure_group + PaperlessBilling, data = training)
        plot(tree)
        pred_tree <- predict(tree, newdata = testing)
        print("Confusion Matrix for Decision Tree")
        print(table(Predicted = pred_tree, Actual = testing$Churn))
       
      }else if (model_choice == 4) {
        break  
      } else {
        cat("Invalid choice. Please enter a number between 1 and 4.\n")
      }
    }
   
  }else if(choice == 3){
    anova_model_full <- aov(MonthlyCharges ~ tenure_group * Contract * PaperlessBilling, data = churn)
    anova_summary <- summary(anova_model_full)[[1]]
   
    cat("========================================\n")
    cat("ANOVA Results (MonthlyCharges ~ tenure_group * Contract * PaperlessBilling)\n")
    cat("========================================\n")
    cat("\nGeneral Interpretation:\n")
    cat("If the p-value is < 0.05, the corresponding term significantly affects Monthly Charges.\n")
    cat("Otherwise, the effect is not statistically significant.\n\n")
   
    for (i in 1:nrow(anova_summary)) {
      term <- rownames(anova_summary)[i]
      df <- anova_summary[i, "Df"]
      f_value <- round(anova_summary[i, "F value"], 3)
      p_value <- anova_summary[i, "Pr(>F)"]
      if (!is.na(p_value)) {
        interpretation <- ifelse(p_value < 0.05,
                                 "Reject the null hypothesis: This term has a significant effect on Monthly Charges.",
                                 "Fail to reject the null hypothesis: This term does not have a significant effect on Monthly Charges.")
      } else {
        interpretation <- "No valid p-value (likely due to missing data)."
      }
     
      cat("Term:", term, "\n")
      cat("Degrees of Freedom (DF):", df, "\n")
      cat("F-Statistic:", f_value, "\n")
      cat("p-value:", format(p_value, scientific = TRUE), "\n")
      cat("Interpretation:", interpretation, "\n")
      cat("----------------------------------------\n")
    }
   
    boxplot_tenure <- ggplot(churn, aes(x = tenure_group, y = MonthlyCharges, fill = tenure_group)) +geom_boxplot() +theme_minimal() +labs(title = "Boxplot of Monthly Charges by Tenure Group", x = "Tenure Group", y = "Monthly Charges") +scale_fill_brewer(palette = "Set3")
    print(boxplot_tenure)
    diagnostic_plots <- function() {
      par(mfrow = c(2, 2))
      plot(anova_model_full)
    }
    diagnostic_plots()  
   
  } else if (choice == 4) {
    test_menu_options <- c(
      "1. T-Test (Two-sample t-test for Monthly Charges by Churn)",
      "2. Chi-Square Test (Churn vs Contract Type)",
      "3. F-Test (Variance Comparison)",
      "4. Back to Main Menu"
    )
   
    while (TRUE) {
      cat("\nChoose a hypothesis test:\n")
      for (i in seq_along(test_menu_options)) {
        cat(test_menu_options[i], "\n")
      }
      test_choice <- as.numeric(readline("Enter your choice: "))
     
      if (test_choice == 1) {
        t_test_result <- t.test(MonthlyCharges ~ Churn, data = churn)
        cat("========================================\n")
        cat("Two-Sample T-Test Results\n")
        cat("========================================\n")
        cat("t-statistic:", round(t_test_result$statistic, 3), "\n")
        cat("Degrees of Freedom:", t_test_result$parameter, "\n")
        cat("p-value:", format(t_test_result$p.value, scientific = TRUE), "\n")
        cat("Mean of Group 1 (Churn=No):", round(t_test_result$estimate[1], 2), "\n")
        cat("Mean of Group 2 (Churn=Yes):", round(t_test_result$estimate[2], 2), "\n")
        cat("Null Hypothesis: The mean Monthly Charges are the same for churned and non-churned customers.\n")
        if (t_test_result$p.value < 0.05) {
          cat("\nResult: Reject the null hypothesis.\n")
          cat("The mean Monthly Charges differ significantly between churned and non-churned customers.\n")
        } else {
          cat("\nResult: Fail to reject the null hypothesis.\n")
          cat("There is no significant difference in the mean Monthly Charges between the two groups.\n")
        }
       
      } else if (test_choice == 2) {
        chisq_test_result <- chisq.test(table(churn$Churn, churn$Contract))
        cat("========================================\n")
        cat("Chi-Square Test Results\n")
        cat("========================================\n")
        cat("Chi-Square Statistic:", round(chisq_test_result$statistic, 3), "\n")
        cat("Degrees of Freedom:", chisq_test_result$parameter, "\n")
        cat("p-value:", format(chisq_test_result$p.value, scientific = TRUE), "\n")
        cat("Null Hypothesis: Churn is independent of the Contract type.\n")
        if (chisq_test_result$p.value < 0.05) {
          cat("\nResult: Reject the null hypothesis.\n")
          cat("Churn is dependent on the type of contract.\n")
        } else {
          cat("\nResult: Fail to reject the null hypothesis.\n")
          cat("There is no significant association between churn and contract type.\n")
        }
       
      } else if (test_choice == 3) {
        var_test_result <- var.test(MonthlyCharges ~ Churn, data = churn)
        cat("F-Test for Monthly Charges by Churn Status\n")
        cat("========================================\n")
        cat("F-statistic:", round(var_test_result$statistic, 3), "\n")
        cat("Degrees of Freedom 1:", var_test_result$parameter[1], "\n")
        cat("Degrees of Freedom 2:", var_test_result$parameter[2], "\n")
        cat("p-value:", format(var_test_result$p.value, scientific = TRUE), "\n")
        cat("Null Hypothesis: The variances of Monthly Charges are the same for churned and non-churned customers.\n\n")
        if (var_test_result$p.value < 0.05) {
          cat("Result: Reject the null hypothesis.\n")
          cat("The variances are significantly different.\n")
        } else {
          cat("Result: Fail to reject the null hypothesis.\n")
          cat("There is no significant difference in variances.\n")
        }
      } else if (test_choice == 4) {
        break
      } else {
        cat("Invalid choice. Please enter a number between 1 and 4.\n")
      }
    }
  } else if (choice == 0) {
    break
  } else {
    cat("Invalid choice. Please enter a number between 1 and 4.\n")
  }
}
