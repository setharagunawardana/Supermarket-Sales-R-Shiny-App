# 🛒 Supermarket Sales Analysis Dashboard (Shiny App)

## 📌 Project Overview
This project is an **interactive Shiny dashboard** built in R to analyze the **Supermarket Sales Dataset (2019)**.  
The dataset contains historical sales transactions from **3 different branches** over a 3-month period.  

The dashboard provides:
- Exploratory data analysis (EDA) for categorical and continuous variables.
- Time series visualization of sales metrics.
- Univariate and bivariate analysis with interactive plots.
- Data quality checks (missing values, duplicates, outliers).


## 📊 Dataset
- **Source**: [Kaggle – Supermarket Sales Dataset](https://www.kaggle.com/code/aryantiwari123/supermarket-sales-prediction)  
- **Description**: Sales records including customer demographics, product lines, transaction details, and ratings.  
- **Attributes**:
  - Invoice ID, Branch, City, Customer type, Gender
  - Product line, Unit price, Quantity, Tax, Total
  - Date, Time, Payment method, COGS, Gross income, Rating  


## ⚡ Features of the Dashboard
- **Overview Tab**: Dataset description, source, and metadata.
- **Data Quality Tab**:
  - Detect missing values, duplicate rows, and outliers using the IQR method.  
- **Time Plots Tab**: Explore sales metrics over custom date ranges.  
- **Univariate Analysis Tab**:
  - Pie charts for categorical variables.
  - Box plots and summary statistics for continuous variables.  
- **Bivariate Analysis Tab**: Scatter plots to analyze relationships between variables.   

---

## 🚀 Installation & Setup
- Install required R packages:
  - install.packages(c("shiny", "dplyr", "ggplot2"))
- Run the app:
  - library(shiny)
  - runApp("app.R")
