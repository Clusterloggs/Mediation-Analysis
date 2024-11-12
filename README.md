

# Data Analysis Project: Pay, Gender, and Work Satisfaction

This project analyzes a dataset to explore the relationships between pay range, gender, age, and work satisfaction in a corporate environment. It includes visualizations, correlation analysis, and mediation analysis to investigate the factors influencing job satisfaction, particularly the role of work-life balance and job autonomy.

## Table of Contents

- [Project Overview](#project-overview)
- [Data Overview](#data-overview)
- [Analysis Workflow](#analysis-workflow)
  - [Data Preprocessing](#data-preprocessing)
  - [Visualizations](#visualizations)
  - [Correlation Analysis](#correlation-analysis)
  - [Mediation Analysis](#mediation-analysis)
- [Results and Findings](#results-and-findings)
- [Requirements](#requirements)
- [How to Run](#how-to-run)
- [File Structure](#file-structure)
- [Contributors](#contributors)

---

## Project Overview

This project aims to uncover insights about pay ranges, age distribution, gender representation, and their effects on work satisfaction. Additionally, mediation analysis investigates how work-life balance mediates the relationship between job autonomy and job satisfaction.

## Data Overview

The dataset consists of several columns, including:
- **Gender**: Gender of the employee.
- **Age**: Age group of the employee.
- **Pay Range**: Salary range categorized into different brackets.
- **Work-life Balance**: Self-reported work-life balance score.
- **Job Autonomy**: Level of autonomy at the job.
- **Job Satisfaction**: Overall satisfaction with the job.

## Analysis Workflow

### Data Preprocessing
- **Loading** and **Cleaning**: Loaded dataset, removed missing values, and handled categorical data appropriately.
- **Selection of Relevant Columns**: For correlation and mediation analysis, only relevant numeric and categorical columns were selected to focus on variables with the most impact.

### Visualizations
1. **Distribution of Pay Range**:
   - A bar plot shows the distribution of employees across different pay ranges, aiding in understanding salary allocation.
   
2. **Age and Gender Analysis**:
   - Donut charts display the distribution of age groups and gender proportions, providing demographic insights.

3. **Pay Range by Gender**:
   - A segmented bar plot depicts gender representation across pay ranges, showing potential pay disparities.

4. **Correlation Matrix**:
   - A heatmap visualizes correlations between numeric variables such as job autonomy, work-life balance, and job satisfaction.

### Correlation Analysis
- Created a **correlation matrix** to examine relationships between job satisfaction, job autonomy, work-life balance, and other numeric variables.
- Visualized the matrix with color-coded tiles to highlight positive, negative, and neutral correlations.

### Mediation Analysis
- Mediation analysis investigated **work-life balance** as a mediator in the relationship between **job autonomy** (independent variable) and **job satisfaction** (dependent variable).
- The `mediate` function calculated the indirect effect, using bootstrapping for robust confidence intervals. The model assessed both direct and indirect pathways to quantify their contributions to overall job satisfaction.

## Results and Findings

- **Pay Range and Gender**: Gender distribution across pay ranges reveals insights into pay equality within the organization.
- **Correlation Findings**: Strong correlations were found between job autonomy, work-life balance, and job satisfaction.
- **Mediation Analysis Results**:
  - **Indirect Effect**: Work-life balance was a significant mediator, suggesting that autonomy affects satisfaction partially through work-life balance.
  - **Direct Effect**: Job autonomy also directly impacts job satisfaction, but the presence of work-life balance enhances this relationship.

## Requirements

- **R (version 4.0 or higher)**
- **R Libraries**:
  - `ggplot2`
  - `dplyr`
  - `viridis`
  - `scales`
  - `reshape2`
  - `mediation`

## How to Run

1. **Install Required Packages** (if not already installed):
   ```R
   install.packages(c("ggplot2", "dplyr", "viridis", "scales", "reshape2", "mediation"))
   ```

2. **Run the Analysis Script**:
   - Load the script (`data_analysis.R`) in RStudio or run in an R environment:
   ```R
   source("data_analysis.R")
   ```

3. **View Results**:
   - Visualizations will display in the R environment.
   - Plots are saved as PNG files in the working directory, named according to the type of analysis (e.g., `pay_range_plot.png`, `correlation_plot.png`).
   - Mediation analysis results will be printed in the console, including direct and indirect effects.

## File Structure

```plaintext
Project/
├── README.md            # Project overview and documentation
├── data_analysis.R      # R script with analysis code
├── data/                # Folder for storing raw data files
├── output/              # Folder where plot images are saved
├── plots/               # Folder containing plot PNG files
└── results/             # Folder for storing analysis results (e.g., mediation output)
```

## Contributors

- **Azeez Akintonde** - Data Analysis, Visualization, Documentation
