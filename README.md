California Housing Prices Regression Analysis 🏡 📊

Analyzing housing prices in California to identify key factors like median income, housing age, and proximity to the ocean. This project uses advanced regression techniques to improve model accuracy and provide actionable insights.

Project Description 🔍
The project involves the following steps:

Data Cleaning: Addressed missing values and prepared categorical variables.
Model Development: Built multiple linear regression models to assess variable importance.
Diagnostics: Used Durbin-Watson test for autocorrelation and Cook's Distance for influential points.
Refinements: Incorporated a lagged variable and log-transformed the response variable for improved model assumptions.
Results: Improved Adjusted R² from 60% to 70%, with significant predictors like income and ocean proximity.
Technology Stack:
Programming Language: R
Libraries: lm, ggplot2, car
Dataset: California Housing Prices (Kaggle) : https://www.kaggle.com/datasets/camnugent/california-housing-prices
Environment Setup 🛠️
Clone the repository locally.
Install required R libraries:
install.packages(c("ggplot2", "car"))
Load the dataset and execute the R scripts for analysis.

How to RUN 🕹️

Load the dataset in RStudio.
Execute the script Project_Regression_Analysis_MATH564
View results and plots in the output folder.

Visualizations 📈

Residual vs. Fitted Plot: Diagnostic for heteroscedasticity.
Cook's Distance Plot: Highlights influential points.
Adjusted R² Improvements: Bar chart comparing initial and refined models.
Median Income vs. Housing Prices: Scatter plot with regression line.

Credits 🙌

This project was completed as part of the MATH 564: Regression Analysis course at Illinois Institute of Technology.
Guidance from Prof. Kia Ong was instrumental in understanding regression diagnostics and model refinement techniques.
The dataset was sourced from the California Housing Prices Dataset on Kaggle, which provided a comprehensive basis for my analysis.
