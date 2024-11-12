library(haven) # Load the haven package into your R session
library(dplyr) # data manipulation
library(mediation) # mediation analysis
library(readxl) # data importation
library(ggplot2) # data visualization
library(corrplot) # for correlation visual
library(viridis) # Viridis color palllete
library(reshape2)


# Set base directory
data_dir <- "C:/Users/USER/Desktop/HAK" # here is the data location on your local computer

# set work directory
setwd("C:/Users/USER/Desktop/HAK") # set the work directory on your local computer

# dataset
dataset <- "wers11_seq_general_use_missings_coded_numeric_july2013.dta" # dataset from the work directory

# Read data
WPS <- read_dta(
  file.path(data_dir, dataset)) # the dataset was read using the parameter set above

# Data Cleaning:
# Selecting relevant columns and filtering
data_df <- WPS[, c("qa8h",
                   "qa8c", "qa7a","qa7b", "qa7c","qa7d", "qa7e","qa1",
                   "qe1","qe2", "qe13","qe11",
                   "qa8a", "qa8b","qa8d", "qa8e","qa8f", "qa8g",
                   "qb2a","qb2b", "qb3","qb4")] # the dataset contains large amount of data, the needed one was extracted

# Filter out rows with specific values
data_df1 <- data_df[!data_df$qb2a %in% c(-6, -9, -8, -1) & 
                      !data_df$qb2b %in% c(-6, -9, -8, -1) &
                      !data_df$qb3 %in% c(-6, -9, -8, -1) &
                      !data_df$qb4 %in% c(-6, -9, -8, -1) &
                      !data_df$qa8a %in% c(-6, -9, -8, -1) &
                      !data_df$qa8b %in% c(-6, -9, -8, -1) &
                      !data_df$qa8c %in% c(-6, -9, -8, -1) &
                      !data_df$qa8d %in% c(-6, -9, -8, -1) &
                      !data_df$qa8e %in% c(-6, -9, -8, -1) &
                      !data_df$qa8f %in% c(-6, -9, -8, -1) &
                      !data_df$qa8g %in% c(-6, -9, -8, -1) &
                      !data_df$qa8h %in% c(-6, -9, -8, -1) &
                      !data_df$qa7a %in% c(-6, -9, -8, -1) &
                      !data_df$qa7b %in% c(-6, -9, -8, -1) &
                      !data_df$qa7c %in% c(-6, -9, -8, -1) &
                      !data_df$qa7d %in% c(-6, -9, -8, -1) &
                      !data_df$qa7e %in% c(-6, -9, -8, -1) &
                      !data_df$qa1 %in% c(1,2,3,-6, -9, -8, -1) &
                      !data_df$qe1 %in% c(-6, -9, -8, -1) &
                      !data_df$qe2 %in% c(1,2,3,4,-6, -9, -8, -1) &
                      !data_df$qe13 %in% c(-6, -9, -8, -1) &
                      !data_df$qe11 %in% c(14,-6, -9, -8, -1) &
                      !data_df$qb3 %in% c(6, -6, -9, -8, -1),] # also, some values were filtered out to avooid outliers


# Variable Renaming
data_df2 <- data_df1 %>%
  rename(
    worklife_balance = qb2a,
    job_satisfaction = qa8h,
    job_autonomy = qa8c,
    influence_over_work_1 = qa7a,
    influence_over_work_2 = qa7b,
    influence_over_work_3 = qa7c,
    influence_over_work_4 = qa7d,
    influence_over_work_5 = qa7e,
    Satisfaction_1 = qa8a,
    Satisfaction_2 = qa8b,
    Satisfaction_3 = qa8e,
    Satisfaction_4 = qa8f,
    Satisfaction_5 = qa8g,
    WLB_1 = qb2b,
    WLB_2 = qb3,
    WLB_3 = qb4,
    length_of_employment = qa1,
    gender = qe1,
    age = qe2,
    race = qe13,
    pay_range = qe11
  ) # for easy computation, variable were rename for readability




# we keep a numerical copy of the dataset for further use
data_dt <- data_df2 %>%
  mutate_all(as.numeric) # the data type of the column variables were set aside for future use


# Grouping and Recoding Variables
# this is done to improve readability, codes were converted back to their respective meaning
data_df2 <- data_df2 %>%
  mutate(
    race = as.character(race),
    race = case_when(race %in% 1:3 ~ "British",race %in% 4:7 ~ "mixed",
                     race %in% 8:12 ~ "Asian",race %in% 13:15 ~ "black british",
                     race %in% 16:17 ~ "others",TRUE ~ race),
    
    gender = as.character(gender),
    gender = case_when(gender == 1 ~ "male",
                       gender == 2 ~ "female",
                       TRUE ~ gender),
    
    job_satisfaction = as.character(job_satisfaction),
    job_satisfaction = case_when(job_satisfaction == 1 ~ "very satisfied",
                                 job_satisfaction == 2 ~ "satisfied",job_satisfaction == 3 ~ "neutral",
                                 job_satisfaction == 4 ~ "dissatisfied",job_satisfaction == 5 ~ "very dissatisfied",
                                 TRUE ~ job_satisfaction),
    
    worklife_balance = as.character(worklife_balance),
    worklife_balance = case_when(worklife_balance == 1 ~ "s-agree",
                                 worklife_balance == 2 ~ "agree",worklife_balance == 3 ~ "indifferent",
                                 worklife_balance == 4 ~ "disagree",worklife_balance == 5 ~ "s-disagree",
                                 TRUE ~ worklife_balance),
    
    length_of_employment = as.character(length_of_employment),
    length_of_employment = case_when(length_of_employment == 1 ~ "below 1 year",
                                     length_of_employment == 2 ~ "1-2 years",length_of_employment == 3 ~ "2-5 years",
                                     length_of_employment == 4 ~ "5-10 years",length_of_employment == 5 ~ "over 10 years",
                                     TRUE ~ length_of_employment),
    
    pay_range = as.character(pay_range),
    pay_range = case_when(pay_range %in% 1:2 ~ "£3,120 or less",
                          pay_range %in% 3:4 ~ "£3,121 - £6,760",
                          pay_range %in% 5:6 ~ "£6,761 - £11,440",
                          pay_range %in% 7:9 ~ "£11,441 - £19,240",
                          pay_range %in% 10:11 ~ "£19,241 - £33,800",
                          pay_range %in% 12:13 ~ "£33,801 or more",
                          TRUE ~ pay_range),
    
    influence_over_work_1 = as.character(influence_over_work_1),
    influence_over_work_1 = case_when(influence_over_work_1 == 1 ~ "a lot",
                                      influence_over_work_1 == 2 ~ "some",
                                      influence_over_work_1 == 3 ~ "a little",
                                      influence_over_work_1 == 4 ~ "none",
                                      TRUE ~ influence_over_work_1),
    
    influence_over_work_2 = as.character(influence_over_work_2),
    influence_over_work_2 = case_when(influence_over_work_2 == 1 ~ "a lot",
                                      influence_over_work_2 == 2 ~ "some",
                                      influence_over_work_2 == 3 ~ "a little",
                                      influence_over_work_2 == 4 ~ "none",
                                      TRUE ~ influence_over_work_2),
    
    influence_over_work_3 = as.character(influence_over_work_3),
    influence_over_work_3 = case_when(influence_over_work_3 == 1 ~ "a lot",
                                      influence_over_work_3 == 2 ~ "some",
                                      influence_over_work_3 == 3 ~ "a little",
                                      influence_over_work_3 == 4 ~ "none",
                                      TRUE ~ influence_over_work_3),
    
    influence_over_work_4 = as.character(influence_over_work_4),
    influence_over_work_4 = case_when(influence_over_work_4 == 1 ~ "a lot",
                                      influence_over_work_4 == 2 ~ "some",
                                      influence_over_work_4 == 3 ~ "a little",
                                      influence_over_work_4 == 4 ~ "none",
                                      TRUE ~ influence_over_work_4),
    
    influence_over_work_5 = as.character(influence_over_work_5),
    influence_over_work_5 = case_when(influence_over_work_5 == 1 ~ "a lot",
                                      influence_over_work_5 == 2 ~ "some",
                                      influence_over_work_5 == 3 ~ "a little",
                                      influence_over_work_5 == 4 ~ "none",
                                      TRUE ~ influence_over_work_5),
    
    job_autonomy = as.character(job_autonomy),
    job_autonomy = case_when(job_autonomy == 1 ~ "very satisfied",
                             job_autonomy == 2 ~ "satisfied",
                             job_autonomy == 3 ~ "neither satisfied nor dissatisfied",
                             job_autonomy == 4 ~ "dissatisfied",
                             job_autonomy == 5 ~ "very dissatisfied",
                             TRUE ~ job_autonomy),
    
    age = as.character(age),
    age = case_when(age %in% 1:3 ~ "16-21",age == 2 ~ "18-19",age == 3 ~ "20-21",
                    age == 4 ~ "22-29", age == 5 ~ "30-39",age == 6 ~ "40-49",
                    age == 7 ~ "50-59", age %in% 8:9 ~ "60+",
                    TRUE ~ age),
    
    Satisfaction_1 = as.character(Satisfaction_1),
    Satisfaction_1 = case_when(
      Satisfaction_1 == "1" ~ "very dissatisfied",
      Satisfaction_1 == "2" ~ "satisfied",
      Satisfaction_1 == "3" ~ "neither satisfied nor dissatisfied",
      Satisfaction_1 == "4" ~ "dissatisfied",
      Satisfaction_1 == "5" ~ "very dissatisfied",
      TRUE ~ Satisfaction_1),
    
    Satisfaction_2 = as.character(Satisfaction_2),
    Satisfaction_2 = case_when(
      Satisfaction_2 == "1" ~ "very dissatisfied",
      Satisfaction_2 == "2" ~ "satisfied",
      Satisfaction_2 == "3" ~ "neither satisfied nor dissatisfied",
      Satisfaction_2 == "4" ~ "dissatisfied",
      Satisfaction_2 == "5" ~ "very dissatisfied",
      TRUE ~ Satisfaction_2),
    
    Satisfaction_3 = as.character(Satisfaction_3),
    Satisfaction_3 = case_when(
      Satisfaction_3 == "1" ~ "very dissatisfied",
      Satisfaction_3 == "2" ~ "satisfied",
      Satisfaction_3 == "3" ~ "neither satisfied nor dissatisfied",
      Satisfaction_3 == "4" ~ "dissatisfied",
      Satisfaction_3 == "5" ~ "very dissatisfied",
      TRUE ~ Satisfaction_3),
    
    Satisfaction_4 = as.character(Satisfaction_4),
    Satisfaction_4 = case_when(
      Satisfaction_4 == "1" ~ "very dissatisfied",
      Satisfaction_4 == "2" ~ "satisfied",
      Satisfaction_4 == "3" ~ "neither satisfied nor dissatisfied",
      Satisfaction_4 == "4" ~ "dissatisfied",
      Satisfaction_4 == "5" ~ "very dissatisfied",
      TRUE ~ Satisfaction_4),
    
    Satisfaction_5 = as.character(Satisfaction_5),
    Satisfaction_5 = case_when(
      Satisfaction_5 == "1" ~ "very dissatisfied",
      Satisfaction_5 == "2" ~ "satisfied",
      Satisfaction_5 == "3" ~ "neither satisfied nor dissatisfied",
      Satisfaction_5 == "4" ~ "dissatisfied",
      Satisfaction_5 == "5" ~ "very dissatisfied",
      TRUE ~ Satisfaction_5),
    
    WLB_1 = as.character(WLB_1),
    WLB_1 = case_when(
      WLB_1 == "1" ~ "strongly agree",
      WLB_1 == "2" ~ "agree",
      WLB_1 == "3" ~ "neither agree nor disagree",
      WLB_1 == "4" ~ "disagree",
      WLB_1 == "5" ~ "strongly disagree",
      TRUE ~ WLB_1),
    
    WLB_2 = as.character(WLB_2),
    WLB_2 = case_when(
      WLB_2 == "1" ~ "none",
      WLB_2 == "2" ~ "less than 1 day",
      WLB_2 == "3" ~ "1 to less than 2 days",
      WLB_2 == "4" ~ "2 to less than 5 days",
      WLB_2 == "5" ~ "5 to less than 10 days",
      TRUE ~ WLB_2),
    
    WLB_3 = as.character(WLB_3),
    WLB_3 = case_when(
      WLB_3 == "1" ~ "much higher",
      WLB_3 == "2" ~ "a bit higher",
      WLB_3 == "3" ~ "about the same",
      WLB_3 == "4" ~ "a bit lower",
      WLB_3 == "5" ~ "much lower",
      TRUE ~ WLB_3)
  )

# Add descriptive labels
attr(data_df2$influence_over_work_1, "label") <- "The tasks you do in your job"
attr(data_df2$influence_over_work_2, "label") <- "The pace at which you work"
attr(data_df2$influence_over_work_3, "label") <- "How you do your work"
attr(data_df2$influence_over_work_4, "label") <- "The order in which you carry out tasks"
attr(data_df2$influence_over_work_5, "label") <- "The time you start or finish your working day"
attr(data_df2$Satisfaction_1, "label") <- "How satisfied are you with the sense of achievement you get from your work"
attr(data_df2$Satisfaction_2, "label") <- "How satisfied are you with the scope for using your own initiative"
attr(data_df2$Satisfaction_3, "label") <- "How satisfied are you with the opportunity to develop your skills in your job"
attr(data_df2$Satisfaction_4, "label") <- "How satisfied are you with the amount of pay you receive"
attr(data_df2$Satisfaction_5, "label") <- "How satisfied are you with your Job Security"
attr(data_df2$WLB_1, "label") <- "I often find it difficult to do my job properly because of my commitments outside of work"
attr(data_df2$WLB_2, "label") <- "Apart from health and safety training, how much training have you had during the last 12 months, either paid for or organised by your employer?"
attr(data_df2$WLB_3, "label") <- "How well do the work skills you personally have match the skills you need to do your present job?"


# save the dataset (data_df2) to our work directory
write.csv(data_df2, "survey_data.csv", row.names = FALSE)

# Exploratory Data Analysis

# Create a list of column names
cols <- c("job_satisfaction",
          "job_autonomy",
          "influence_over_work_1",
          "influence_over_work_2",
          "influence_over_work_3",
          "influence_over_work_4",
          "influence_over_work_5",
          "length_of_employment",
          "gender",
          "age",
          "race",
          "pay_range",
          "Satisfaction_1",
          "Satisfaction_2",
          "Satisfaction_3",
          "Satisfaction_4",
          "Satisfaction_5",
          "worklife_balance",
          "WLB_1",
          "WLB_2",
          "WLB_3")

# Create a list to store frequency tables
freq_tables <- list()

# Loop through each column and calculate frequency tables
for(col in cols) {
  freq_table <- table(data_df2[[col]])
  freq_tables[[col]] <- data.frame(Value = names(freq_table), Frequency = as.numeric(freq_table))
  freq_tables[[col]]$Percentage <- round(freq_tables[[col]]$Frequency / sum(freq_tables[[col]]$Frequency) * 100, 2)
  
  # Add variable description if available
  label <- attr(data_df2[[col]], "label")
  if (!is.null(label)) {
    freq_tables[[col]]$Description <- label
  } else {
    freq_tables[[col]]$Description <- ""
  }
}

# Print the frequency table for each variables listed in 'cols'
for (col in cols) {
  cat("\nFrequency Table for:", col)
  if (freq_tables[[col]]$Description[1] != "") {
    cat("\nDescription:", freq_tables[[col]]$Description[1], "\n")
  }
  print(freq_tables[[col]][, c("Value", "Frequency", "Percentage")])
}

############
## plot for pay range

# Reorder levels of pay_range variable
data_df2$pay_range <- factor(data_df2$pay_range, levels = c("£3,120 or less", 
                                                            "£3,121 - £6,760", 
                                                            "£6,761 - £11,440", 
                                                            "£11,441 - £19,240", 
                                                            "£19,241 - £33,800", 
                                                            "£33,801 or more"))

pay_range_plot <- ggplot(data_df2, aes(x = pay_range)) +
  geom_bar(fill = 'skyblue', color = 'black') +  # Use skyblue color for all bars with black borders
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 5) +  # Add data labels with increased size
  labs(title = "Pay Range Distribution",
       x = "Pay Range",
       y = NULL) +  # No y-axis label
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove background color
        axis.line = element_line(color = 'black'),  # Add black color to axis lines
        plot.title = element_text(size = 20, face = "bold"),  # Increase title size
        axis.title.x = element_text(size = 16),  # Increase x-axis title size
        axis.text.x = element_text(size = 10),  # Increase x-axis text size
        axis.text.y = element_text(size = 10))  # Increase y-axis text size 
############

########## Age
# Plot bar chart of age without grid lines and background color
age_plot <- ggplot(data_df2, aes(x = age)) +
  geom_bar(fill = 'skyblue', color = 'black') +  # Use skyblue color for all bars with black borders
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 5) +  # Add data labels with increased size
  labs(title = "Age Distribution",
       x = "Age Group",
       y = NULL) +  # No y-axis label
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove background color
        axis.line = element_line(color = 'black'),  # Add black color to axis lines
        plot.title = element_text(size = 20, face = "bold"),  # Increase title size
        axis.title.x = element_text(size = 16),  # Increase x-axis title size
        axis.text.x = element_text(size = 14),  # Increase x-axis text size
        axis.text.y = element_text(size = 14))  # Increase y-axis text size (if needed)

###########



# Create a function to create pie chart for race and gender group
create_donut_chart <- function(data, variable) {
  chart <- ggplot(data, aes(x = "", fill = !!rlang::sym(variable))) +
    geom_bar(width = 1, color = "white") +
    geom_text(aes(label = paste0(round((..count..)/sum(..count..) * 100), "%")), 
              stat = "count", 
              position = position_stack(vjust = 0.5),  # Adjusted to center the text
              size = 5) +  # Increased text size
    coord_polar(theta = "y") +
    theme_void() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 16),  # Increased legend text size
          legend.title = element_text(size = 16, face = "bold"),  # Increased legend title size
          plot.title = element_text(size = 16, face = "bold")) +  # Increased plot title size
    labs(fill = variable, 
         title = paste("Distribution of", variable)) +
    scale_fill_viridis_d(option = "D", end = 1)  # Use viridis color palette
  return(chart)
}

# Create donut chart for gender
gender_donut <- create_donut_chart(data_df2, "gender")

# Create donut chart for race
race_donut <- create_donut_chart(data_df2, "race")

# Print the donut charts
print(gender_donut)
print(race_donut)




# Age distribution by pay range
age_pay_range_plot <- ggplot(data_df2, aes(x = age, fill = pay_range)) +
  geom_bar(position = "fill") +
  labs(title = "Age Distribution by Pay Range",
       x = "Age",
       y = "",
       fill = "Pay Range") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  # Use viridis color palette
  scale_fill_viridis(discrete = TRUE) +
  # Customize appearance
  theme(panel.grid = element_blank(), 
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.1, size = 16, face = "bold"),  # Increased title size
        axis.title.x = element_text(size = 14),  # Increased x-axis title size
        axis.text.x = element_text(size = 14),  # Increased x-axis text size
        legend.title = element_text(size = 16, face = "bold"),  # Increased legend title size
        legend.text = element_text(size = 14))  # Increased legend text size



# Pay_range by Gender

pay_range_gender_plot <- ggplot(data_df2, aes(x = pay_range, fill = gender)) +
  geom_bar(position = "fill") +
  labs(title = "Pay Range by Gender",
       x = "Pay Range",
       y = "",
       fill = "Gender") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  # Use viridis color palette
  scale_fill_viridis(discrete = TRUE) +
  # Customize appearance
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.1, size = 16, face = "bold"),  # Increased title size and centered
        axis.title.x = element_text(size = 14),  # Increased x-axis title size
        axis.text.x = element_text(size = 8),  # Increased x-axis text size
        legend.title = element_text(size = 14, face = "bold"),  # Increased legend title size and boldness
        legend.text = element_text(size = 10))  # Increased legend text size

########################
# Correlation matrix

# Select only the relevant columns
data_selected <- data_dt[, cols]

# Calculate correlation matrix for selected columns (excluding non-numeric columns)
numeric_cols <- sapply(data_selected, is.numeric)
cor_matrix <- round(cor(data_selected[, numeric_cols], use = "complete.obs"), 4)

# Convert the correlation matrix into a long format
cor_df <- melt(cor_matrix)

# Create correlation plot using ggplot2
correlation_plot <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Correlation", option = "H", end = 1, limits = c(-1, 1), 
                     breaks = seq(-1, 1, by = 0.20)) +
  geom_text(aes(label = round(value, 3)), color = "black", size = 1.7) +  # Increased text size for correlation values
  theme_minimal() +
  labs(title = "Correlation Matrix",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),  # Increased x-axis text size
        axis.text.y = element_text(size = 13),  # Increased y-axis text size
        plot.title = element_text(hjust = 0.1, size = 16, face = "bold"),  # Increased title size and centered
        legend.title = element_text(size = 12, face = "bold"),  # Increased legend title size and boldness
        legend.text = element_text(size = 8))  # Increased legend text size


# Function to save plots
save_plot <- function(plot, filename) {
  ggsave(filename = filename, plot = plot, width = 8, height = 6)
}

# Save each plot with a specific filename to the work directory
save_plot(pay_range_plot, "pay_range_plot.png")
save_plot(age_plot, "age_plot.png")
save_plot(gender_donut, "gender_donut.png")
save_plot(race_donut, "race_donut.png")
save_plot(age_pay_range_plot, "age_pay_range_plot.png")
save_plot(pay_range_gender_plot, "pay_range_gender_plot.png")
save_plot(correlation_plot, "correlation_plot.png")
#####################


# Mediation Analysis:
# we define variables
M <- data_dt$worklife_balance
X <- data_dt$job_autonomy
Y <- data_dt$job_satisfaction

# Remove rows with missing or infinite values from both X and Y
complete_cases <- complete.cases(X, M, Y)
X <- X[complete_cases]
M <- M[complete_cases]
Y <- Y[complete_cases]

# Create a new dataframe for mediation analysis
mediation_data <- data.frame(
  M = M,
  X = X,
  Y = Y)

# Check for missing or infinite values
missing_value <- sum(is.na(mediation_data))
print(missing_value)

# Define models using the new dataframe
model.m <- lm(M ~ X, data = mediation_data)  # Model for the mediator variable
model.y <- lm(Y ~ M + X, data = mediation_data)  # Model for the outcome variable
model.c <- lm(Y ~ X, data = mediation_data) # Model for total effect (path "c)

# print the summary
summary(model.m)
summary(model.y)
summary(model.c)

# Mediation Model
# Perform mediation analysis using the 'mediate' function
# model.m: Model for the mediator M
# model.y: Model for the outcome Y
# sims: Number of simulations to run for estimating the indirect effect
# treat: The treatment variable X
# mediator: The mediator variable M
# boot: If TRUE, bootstrap method is used for estimating confidence intervals

results = mediate(model.m, model.y, sims = 5000, treat ='X', mediator ='M', boot = T)
summary(results) 

# Plot mediation results
plot(results, main = "Mediation Plot", label = TRUE)


# calculate the indirect effect
# The indirect effect is the product of:
# 1. The effect of the treatment on the mediator (coefficient of 'X' in model.m)
# 2. The effect of the mediator on the outcome (coefficient of 'M' in model.y)
indirect_effect <- model.m$coefficients[2]*model.y$coefficients[2]
print(indirect_effect)

# Calculate the direct effect (path "c'"):
# The direct effect represents the effect of the treatment on the outcome,
# controlling for the mediator (coefficient of 'X' in model.c)
direct_effect <- coef(model.c)["X"]
print(direct_effect)
