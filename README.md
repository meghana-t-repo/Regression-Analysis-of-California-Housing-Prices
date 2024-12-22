# Regression-Analysis-of-California-Housing-Prices
Analyzed California Housing Prices using regression techniques, addressing key model challenges to improve predictive accuracy and derive actionable insights.

library(readxl)
library(car)
# Load the dataset
data <- read_excel("C:/Users/megha/Sem2/Regression/Project/New Folder/housing.xlsx")

# View the structure and summary of the data
str(data)
## tibble [20,640 × 10] (S3: tbl_df/tbl/data.frame)
##  $ longitude         : num [1:20640] -122 -122 -122 -122 -122 ...
##  $ latitude          : num [1:20640] 37.9 37.9 37.9 37.9 37.9 ...
##  $ housing_median_age: num [1:20640] 41 21 52 52 52 52 52 52 42 52 ...
##  $ total_rooms       : num [1:20640] 880 7099 1467 1274 1627 ...
##  $ total_bedrooms    : num [1:20640] 129 1106 190 235 280 ...
##  $ population        : num [1:20640] 322 2401 496 558 565 ...
##  $ households        : num [1:20640] 126 1138 177 219 259 ...
##  $ median_income     : num [1:20640] 8.33 8.3 7.26 5.64 3.85 ...
##  $ median_house_value: num [1:20640] 452600 358500 352100 341300 342200 ...
##  $ ocean_proximity   : chr [1:20640] "NEAR BAY" "NEAR BAY" "NEAR BAY" "NEAR BAY" ...

summary(data)
##    longitude         latitude     housing_median_age  total_rooms   
##  Min.   :-124.3   Min.   :32.54   Min.   : 1.00      Min.   :    2  
##  1st Qu.:-121.8   1st Qu.:33.93   1st Qu.:18.00      1st Qu.: 1448  
##  Median :-118.5   Median :34.26   Median :29.00      Median : 2127  
##  Mean   :-119.6   Mean   :35.63   Mean   :28.64      Mean   : 2636  
##  3rd Qu.:-118.0   3rd Qu.:37.71   3rd Qu.:37.00      3rd Qu.: 3148  
##  Max.   :-114.3   Max.   :41.95   Max.   :52.00      Max.   :39320  
##                                                                     
##  total_bedrooms     population      households     median_income    
##  Min.   :   1.0   Min.   :    3   Min.   :   1.0   Min.   : 0.4999  
##  1st Qu.: 296.0   1st Qu.:  787   1st Qu.: 280.0   1st Qu.: 2.5634  
##  Median : 435.0   Median : 1166   Median : 409.0   Median : 3.5348  
##  Mean   : 537.9   Mean   : 1425   Mean   : 499.5   Mean   : 3.8707  
##  3rd Qu.: 647.0   3rd Qu.: 1725   3rd Qu.: 605.0   3rd Qu.: 4.7432  
##  Max.   :6445.0   Max.   :35682   Max.   :6082.0   Max.   :15.0001  
##  NA's   :207                                                        
##  median_house_value ocean_proximity   
##  Min.   : 14999     Length:20640      
##  1st Qu.:119600     Class :character  
##  Median :179700     Mode  :character  
##  Mean   :206856                       
##  3rd Qu.:264725                       
##  Max.   :500001                       
## 

##Step 1: Data Exploration and Cleaning
# Check for missing values
colSums(is.na(data))

##          longitude           latitude housing_median_age        total_rooms 
##                  0                  0                  0                  0 
##     total_bedrooms         population         households      median_income 
##                207                  0                  0                  0 
## median_house_value    ocean_proximity 
##                  0                  0


# Optionally, handle missing values if necessary
data <- na.omit(data)

# Convert the 'ocean_proximity' column to a factor
data$ocean_proximity <- as.factor(data$ocean_proximity)

# Summary statistics after cleaning
summary(data)

##    longitude         latitude     housing_median_age  total_rooms   
##  Min.   :-124.3   Min.   :32.54   Min.   : 1.00      Min.   :    2  
##  1st Qu.:-121.8   1st Qu.:33.93   1st Qu.:18.00      1st Qu.: 1450  
##  Median :-118.5   Median :34.26   Median :29.00      Median : 2127  
##  Mean   :-119.6   Mean   :35.63   Mean   :28.63      Mean   : 2636  
##  3rd Qu.:-118.0   3rd Qu.:37.72   3rd Qu.:37.00      3rd Qu.: 3143  
##  Max.   :-114.3   Max.   :41.95   Max.   :52.00      Max.   :39320  
##  total_bedrooms     population      households     median_income    
##  Min.   :   1.0   Min.   :    3   Min.   :   1.0   Min.   : 0.4999  
##  1st Qu.: 296.0   1st Qu.:  787   1st Qu.: 280.0   1st Qu.: 2.5637  
##  Median : 435.0   Median : 1166   Median : 409.0   Median : 3.5365  
##  Mean   : 537.9   Mean   : 1425   Mean   : 499.4   Mean   : 3.8712  
##  3rd Qu.: 647.0   3rd Qu.: 1722   3rd Qu.: 604.0   3rd Qu.: 4.7440  
##  Max.   :6445.0   Max.   :35682   Max.   :6082.0   Max.   :15.0001  
##  median_house_value   ocean_proximity
##  Min.   : 14999     <1H OCEAN :9034  
##  1st Qu.:119500     INLAND    :6496  
##  Median :179700     ISLAND    :   5  
##  Mean   :206864     NEAR BAY  :2270  
##  3rd Qu.:264700     NEAR OCEAN:2628  
##  Max.   :500001

##Step 2: Define Response and Predictor Variables Response Variable: median_house_value (assuming the goal is to predict house value). Predictors: Including median_income, housing_median_age, total_rooms, and ocean_proximity to assess their effects on the response.

# Define the response and predictor variables
response <- "median_house_value"
predictors <- c("median_income", "housing_median_age", "total_rooms", "ocean_proximity")

# Define the regression formula
formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))

##Step 3: Fit Initial Regression Model

# Fit the initial regression model
model <- lm(formula, data = data)
summary(model)

## 
## Call:
## lm(formula = formula, data = data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -531693  -46204  -12480   29828  481270 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                3.863e+04  2.260e+03  17.095  < 2e-16 ***
## median_income              3.761e+04  2.854e+02 131.775  < 2e-16 ***
## housing_median_age         1.145e+03  4.629e+01  24.728  < 2e-16 ***
## total_rooms                3.430e+00  2.545e-01  13.474  < 2e-16 ***
## ocean_proximityINLAND     -7.157e+04  1.250e+03 -57.245  < 2e-16 ***
## ocean_proximityISLAND      1.847e+05  3.265e+04   5.657 1.56e-08 ***
## ocean_proximityNEAR BAY    1.186e+04  1.755e+03   6.759 1.43e-11 ***
## ocean_proximityNEAR OCEAN  1.732e+04  1.618e+03  10.702  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 72960 on 20425 degrees of freedom
## Multiple R-squared:  0.6006, Adjusted R-squared:  0.6005 
## F-statistic:  4388 on 7 and 20425 DF,  p-value: < 2.2e-16

##Step 4: Durbin-Watson Test
# Load necessary library
library(lmtest)

# Perform the Durbin-Watson test
dw_test_result <- dwtest(model)
dw_test_result

## 
##  Durbin-Watson test
## 
## data:  model
## DW = 0.82531, p-value < 2.2e-16
## alternative hypothesis: true autocorrelation is greater than 0
Durbin-Watson Statistic (DW): 0.82531 (far below 2), which suggests positive autocorrelation in the residuals. p-value: < 2.2e-16, which is highly significant, indicating strong evidence against the null hypothesis (of no autocorrelation). Thus, we reject the null hypothesis and conclude that autocorrelation is present.

Since the Durbin-Watson test revealed positive autocorrelation, So we are addressing this issue to improve the reliability of the model. To address the autocorrelation detected in the Durbin-Watson test, a lagged variable lag_median_house_value is added to the model.

##Step 5: Add Lagged Variables to Address Autocorrelation
# Create a lagged version of median_house_value as a predictor
data$lag_median_house_value <- c(NA, head(data$median_house_value, -1))

# Re-fit the model, including the lagged variable
model_lag <- lm(median_house_value ~ median_income + housing_median_age + total_rooms + ocean_proximity + lag_median_house_value, data = data, na.action = na.exclude)
summary(model_lag)
## 
## Call:
## lm(formula = median_house_value ~ median_income + housing_median_age + 
##     total_rooms + ocean_proximity + lag_median_house_value, data = data, 
##     na.action = na.exclude)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -337910  -26645   -5906   19454  417635 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)               -2.525e+03  1.683e+03  -1.500 0.133683    
## median_income              1.923e+04  2.506e+02  76.736  < 2e-16 ***
## housing_median_age         5.018e+02  3.424e+01  14.656  < 2e-16 ***
## total_rooms                1.179e+00  1.871e-01   6.298 3.07e-10 ***
## ocean_proximityINLAND     -2.463e+04  9.811e+02 -25.108  < 2e-16 ***
## ocean_proximityISLAND      7.080e+04  2.392e+04   2.960 0.003077 ** 
## ocean_proximityNEAR BAY    4.760e+03  1.286e+03   3.701 0.000215 ***
## ocean_proximityNEAR OCEAN  8.177e+03  1.187e+03   6.889 5.78e-12 ***
## lag_median_house_value     5.980e-01  4.498e-03 132.944  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 53420 on 20423 degrees of freedom
##   (1 observation deleted due to missingness)
## Multiple R-squared:  0.7859, Adjusted R-squared:  0.7858 
## F-statistic:  9369 on 8 and 20423 DF,  p-value: < 2.2e-16


After modifying the model with the lagged variable,we rerun the Durbin-Watson test to check if autocorrelation has been reduced.

##Step 6: Conduct the Durbin-Watson Test Again

# Re-run the Durbin-Watson test on the new model
dw_test_result_lag <- dwtest(model_lag)
dw_test_result_lag

## 
##  Durbin-Watson test
## 
## data:  model_lag
## DW = 2.1791, p-value = 1
## alternative hypothesis: true autocorrelation is greater than 0

Interpretation: - The Durbin-Watson statistic of 2.1791 is close to 2, indicating that autocorrelation in the residuals has been reduced. - The high p-value (p = 1) suggests there is no significant autocorrelation remaining, confirming that the inclusion of the lagged variable successfully addressed this issue.

Final Model Selection
Based on this result, the modified model with the lagged variable is selected as the final model, as it improves model reliability by addressing autocorrelation.

##Step 7: Refit the Initial Model on the Same Data as model_lag

# Refit the initial model excluding the first row
model_refit <- lm(median_house_value ~ median_income + housing_median_age + total_rooms + ocean_proximity, 
                  data = data[-1, ])
##Step 8: Model Comparison and Selection To select the best-fitting model, we compare the AIC values of the initial and modified models (with the lagged variable). Lower AIC values indicate a better fit, considering model complexity and accuracy.
# Compare the AIC values of the models
AIC(model_refit, model_lag)

##             df      AIC
## model_refit  9 515577.1
## model_lag   10 502840.2

The model with the lagged variable (model_lag) has a lower AIC, indicating a better fit with a reasonable model complexity.

Interpretation of Predictors: - median_income and housing_median_age are significant positive predictors. - The categorical variable ocean_proximity shows different levels of impact on median_house_value. - The lagged term for median_house_value suggests that past values significantly influence current housing prices.

##Step 9: Diagnostic Checks on Final Model

To ensure the model assumptions are met (such as normality, homoscedasticity, and independence), we are performing diagnostic checks on model_lag.
# Residual vs Fitted plot
plot(model_lag, which = 1)  # Checks for linearity and homoscedasticity

![image](https://github.com/user-attachments/assets/d7b2d81f-1c39-4349-963f-bd2f150a1bbe)

# Normal Q-Q plot
plot(model_lag, which = 2)  # Checks for normality of residuals

![image](https://github.com/user-attachments/assets/6eda612d-a87f-4da3-b011-114fd73c0a0a)

# Scale-Location plot
plot(model_lag, which = 3)  # Checks for homoscedasticity

![image](https://github.com/user-attachments/assets/ff536849-97f3-43e8-a8ce-63c69da55e01)

# Residuals vs Leverage plot
plot(model_lag, which = 5)  # Identifies influential points

![image](https://github.com/user-attachments/assets/7b5bcdf2-93ab-416e-9073-9690e5fbbffc)

Diagnostic Analysis of Model Assumptions The following diagnostic plots were generated to assess the assumptions of the regression model and identifying any potential issues that may impact the reliability of the results: 1. Residuals vs. Fitted Plot: o This plot is used to check for non-linearity and heteroscedasticity in the residuals. A random scatter around the zero line indicates that the model’s assumptions are met. However, any systematic pattern (e.g., a funnel shape) suggests heteroscedasticity. o In our model, the plot reveals a noticeable pattern, suggesting potential issues with non-linearity and heteroscedasticity. This pattern indicates that the variability of the residuals changes across the range of fitted values, which may affect the accuracy of the p-values and standard errors. 2. Q-Q Plot of Residuals: o The Q-Q plot assesses the normality of residuals. If the residuals are normally distributed, they should follow a straight line on this plot. o Our Q-Q plot shows deviations from the straight line, especially at the tails, suggesting that the residuals are not perfectly normal. While minor deviations are generally acceptable, significant deviations indicate that normality assumptions may not hold, which could impact the reliability of hypothesis testing in our model. 3. Scale-Location Plot: o This plot checks for homoscedasticity (constant variance of residuals) across fitted values. Ideally, the points should be spread randomly without a discernible pattern. o In this plot, the residual spread appears to increase with fitted values, which indicates heteroscedasticity. This issue implies that the residuals do not have a constant variance, potentially affecting the model’s ability to provide accurate confidence intervals and p-values. 4. Residuals vs. Leverage Plot: o This plot helps identify influential observations that could disproportionately affect the model’s fit. Points with high leverage (high Cook’s distance) should be reviewed to determine if they unduly influence the model. o In our case, we observe a few data points with high leverage, suggesting that some observations may significantly impact the model’s predictions. Investigating these points further could improve model robustness.

Summary of Diagnostic Findings: The diagnostic plots indicate several potential issues: • Non-linearity: The residuals vs. fitted plot suggests a non-linear relationship that the current linear model may not fully capture. • Heteroscedasticity: Both the residuals vs. fitted plot and scale-location plot indicate heteroscedasticity, which affects the accuracy of standard errors and hypothesis tests. • Non-Normality of Residuals: The Q-Q plot suggests that residuals deviate from normality, particularly in the tails. • Influential Observations: The residuals vs. leverage plot highlights a few points with high leverage, which could skew model results. Based on these findings, further remediation steps will be taken to address these issues, including transformations to handle heteroscedasticity and adjustments for influential observations. These steps will improve the model’s reliability and ensure it meets regression assumptions for better interpretation and predictive performance.

##Step 10: Addressing Heteroscedasticity To address heteroscedasticity, we can start by applying a log transformation to the response variable, median_house_value.
# Apply a log transformation to the response variable
data$log_median_house_value <- log(data$median_house_value)

# Fit a new regression model with the transformed response variable
model_log <- lm(log_median_house_value ~ median_income + housing_median_age + total_rooms + ocean_proximity, data = data)
summary(model_log)

## 
## Call:
## lm(formula = log_median_house_value ~ median_income + housing_median_age + 
##     total_rooms + ocean_proximity, data = data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.3113 -0.2281 -0.0194  0.2059  1.9992 
## 
## Coefficients:
##                             Estimate Std. Error  t value Pr(>|t|)    
## (Intercept)                1.145e+01  1.085e-02 1055.565  < 2e-16 ***
## median_income              1.653e-01  1.370e-03  120.637  < 2e-16 ***
## housing_median_age         3.047e-03  2.223e-04   13.708  < 2e-16 ***
## total_rooms                2.088e-05  1.222e-06   17.084  < 2e-16 ***
## ocean_proximityINLAND     -5.036e-01  6.003e-03  -83.897  < 2e-16 ***
## ocean_proximityISLAND      7.603e-01  1.568e-01    4.850 1.24e-06 ***
## ocean_proximityNEAR BAY    3.418e-02  8.425e-03    4.057 5.00e-05 ***
## ocean_proximityNEAR OCEAN  3.776e-02  7.771e-03    4.860 1.19e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3503 on 20425 degrees of freedom
## Multiple R-squared:  0.6213, Adjusted R-squared:  0.6212 
## F-statistic:  4787 on 7 and 20425 DF,  p-value: < 2.2e-16

# Check residual plots again to see if heteroscedasticity has improved
par(mfrow = c(2, 2))
plot(model_log)

![image](https://github.com/user-attachments/assets/0acb7bf6-ae5c-467c-8735-a868e3f3ae58)

variable median_house_value with a logarithmic scale, we observe the following key relationships between predictors and housing prices:

median_income (Estimate = 0.1653): The coefficient for median_income suggests a strong positive association with housing prices. Specifically, for each unit increase in median_income, the expected log_median_house_value increases by 0.1653, which translates to an approximate 18% increase in median house price on the original scale. This finding highlights that higher-income areas tend to have substantially higher housing prices.

housing_median_age (Estimate = 0.0030): The positive relationship between housing_median_age and housing prices is weaker but still significant. A one-year increase in median housing age is associated with a 0.3% increase in median house price, indicating that older neighborhoods may have slightly higher prices.

total_rooms (Estimate = 2.088e-05): The small positive coefficient for total_rooms indicates a modest increase in housing price with an increase in the total number of rooms. However, its effect size is much smaller compared to median_income.

ocean_proximity (various levels): The categorical variable ocean_proximity shows that areas closer to the ocean have higher housing prices compared to inland areas. For example, properties “NEAR OCEAN” have an estimated 3.78% higher housing price than those inland, reflecting the added value of ocean proximity.

Residuals vs Fitted Plot: The spread of residuals is more uniform across fitted values compared to the original model. This indicates an improvement in addressing heteroscedasticity, as the residuals no longer exhibit a strong pattern or fan shape, suggesting that the variance of residuals is now more consistent.

Q-Q Plot: The Q-Q plot shows that residuals follow a roughly linear pattern along the theoretical quantiles, particularly in the middle range. This indicates that the residuals are approximately normally distributed, although there are some deviations at the tails. Overall, normality has improved, which supports the assumption of normally distributed errors.

Scale-Location Plot: In this plot, the residuals are more evenly distributed along the fitted values, and the red line is relatively flat, further supporting that heteroscedasticity has been mitigated. The transformation has helped to stabilize variance, as the residuals now show a more constant spread.

Residuals vs Leverage Plot: This plot identifies influential points that may disproportionately impact the model. While a few points show higher leverage, they do not appear to be unduly influencing the model, as indicated by their position relative to Cook’s distance lines. This suggests that the model’s fit is not overly reliant on these data points.

##Step 11: Checking for Multicollinearity

To assess multicollinearity, we can calculate the Variance Inflation Factor (VIF) for each predictor.

# Load car package if not already loaded
library(car)

# Calculate VIF for the predictors in the transformed model
vif_values <- vif(model_lag)
vif_values
##                            GVIF Df GVIF^(1/(2*Df))
## median_income          1.620798  1        1.273106
## housing_median_age     1.330604  1        1.153518
## total_rooms            1.197247  1        1.094188
## ocean_proximity        1.444101  4        1.047007
## lag_median_house_value 1.930104  1        1.389282

The VIF values for each predictor in the transformed model are as follows:

median_income: VIF = 1.27 housing_median_age: VIF = 1.15 total_rooms: VIF = 1.09 ocean_proximity: VIF = 1.05 lag_median_house_value: VIF = 1.39 Since all VIF values are below the commonly used threshold of 5 (and even well below a stricter threshold of 10), multicollinearity does not appear to be a concern in this model. Each predictor contributes independent information, and there is minimal redundancy among the predictors. This suggests that the model coefficients can be reliably interpreted without risk of inflated standard errors due to multicollinearity.

Conclusion: No further action is needed to address multicollinearity in this model. We can proceed with the interpretation of predictors and other diagnostic checks, confident that the predictor variables are not excessively correlated.

##Step 12: Identifying Influential Points After checking for multicollinearity, it’s essential to assess whether certain observations exert an outsized influence on the regression model. This is important because influential points, if not identified and addressed, can disproportionately affect the model’s coefficients and reduce its predictive accuracy.

Why Identifying Influential Points is Necessary Influential points are observations in the dataset that have a substantial impact on the estimation of regression coefficients. They can arise due to outliers or unique data points that differ significantly from the rest of the dataset. By identifying these points, we can determine if they are unduly affecting the model’s results. If influential points are present, we may need to investigate them further, potentially refining the model or addressing these points individually to improve overall model robustness.

Approach to Identifying Influential Points To detect influential points, we use Cook’s Distance, which measures the influence of each observation by quantifying the change in regression coefficients when that observation is removed. Observations with a high Cook’s Distance (commonly above the threshold (4/n),where 𝑛is the number of observations) are flagged as influential.

In the following code, we calculate and plot Cook’s Distance to visualize influential points and list the specific observations with Cook’s Distance values above the threshold.

# Calculate Cook's Distance for the final model (e.g., model_log)
cooks_distances <- cooks.distance(model_log)

# Plot Cook's Distance to visually identify influential points
plot(cooks_distances, type = "h", main = "Cook's Distance for Influential Points", ylab = "Cook's Distance")
abline(h = 4 / length(cooks_distances), col = "red", lty = 2)  # Threshold line

![image](https://github.com/user-attachments/assets/e401d80d-5579-488e-bd5a-dcc83b085ad1)

# Identify points with high Cook's Distance
influential_points <- which(cooks_distances > 4 / length(cooks_distances))
influential_points

##    27    56    59    60    61    62    64    65    66    68    69    71    73 
##    27    56    59    60    61    62    64    65    66    68    69    71    73 
##    74    90   104   132   193   198   199   200   201   252   253   254   301 
##    74    90   104   132   193   198   199   200   201   252   253   254   301 
##   307   310   314   315   316   323   324   325   327   328   329   330   334 
##   307   310   314   315   316   323   324   325   327   328   329   330   334 
##   337   338   339   341   342   343   345   348   352   376   379   394   408 
##   337   338   339   341   342   343   345   348   352   376   379   394   408 
##   450   454   456   457   458   488   491   509   510   511   513   550   567 
##   450   454   456   457   458   488   491   509   510   511   513   550   567 
##   572   671   798   859   917   949   969   971   980   982   986   988  1055 
##   572   671   798   859   917   949   969   971   980   982   986   988  1055 
##  1073  1076  1409  1413  1416  1536  1555  1556  1557  1592  1595  1597  1602 
##  1073  1076  1409  1413  1416  1536  1555  1556  1557  1592  1595  1597  1602 
##  1607  1623  1626  1637  1690  1708  1709  1710  1714  1717  1718  1721  1769 
##  1607  1623  1626  1637  1690  1708  1709  1710  1714  1717  1718  1721  1769 
##  1771  1773  1774  1775  1777  1778  1780  1781  1815  1842  1843  1844  1845 
##  1771  1773  1774  1775  1777  1778  1780  1781  1815  1842  1843  1844  1845 
##  1846  1848  1850  1855  1856  1857  1879  1903  1904  1986  1998  2012  2034 
##  1846  1848  1850  1855  1856  1857  1879  1903  1904  1986  1998  2012  2034 
##  2503  2520  2521  2522  2523  2524  2525  2526  2527  2528  2529  2530  2531 
##  2503  2520  2521  2522  2523  2524  2525  2526  2527  2528  2529  2530  2531 
##  2532  2533  2534  2535  2536  2537  2538  2539  2540  2541  2542  2543  2544 
##  2532  2533  2534  2535  2536  2537  2538  2539  2540  2541  2542  2543  2544 
##  2545  2546  2548  2549  2550  2551  2554  2557  2558  2559  2569  2570  2571 
##  2545  2546  2548  2549  2550  2551  2554  2557  2558  2559  2569  2570  2571 
##  2572  2573  2574  2577  2579  2580  2581  2584  2592  2595  2597  2598  2599 
##  2572  2573  2574  2577  2579  2580  2581  2584  2592  2595  2597  2598  2599 
##  2602  2609  2610  2611  2613  2614  2615  2616  2618  2619  2628  2630  2633 
##  2602  2609  2610  2611  2613  2614  2615  2616  2618  2619  2628  2630  2633 
##  2634  2635  2742  2753  2757  2758  2778  2779  2808  2874  2923  2953  2977 
##  2634  2635  2742  2753  2757  2758  2778  2779  2808  2874  2923  2953  2977 
##  2996  2999  3001  3004  3005  3102  3329  3339  3545  3759  4004  4008  4011 
##  2996  2999  3001  3004  3005  3102  3329  3339  3545  3759  4004  4008  4011 
##  4057  4080  4183  4192  4204  4205  4212  4215  4222  4226  4227  4228  4229 
##  4057  4080  4183  4192  4204  4205  4212  4215  4222  4226  4227  4228  4229 
##  4288  4308  4312  4313  4451  4506  4511  4514  4515  4517  4538  4546  4558 
##  4288  4308  4312  4313  4451  4506  4511  4514  4515  4517  4538  4546  4558 
##  4560  4561  4562  4568  4577  4578  4579  4582  4584  4585  4597  4599  4604 
##  4560  4561  4562  4568  4577  4578  4579  4582  4584  4585  4597  4599  4604 
##  4612  4625  4632  4634  4645  4650  4655  4666  4680  4772  4809  4821  4823 
##  4612  4625  4632  4634  4645  4650  4655  4666  4680  4772  4809  4821  4823 
##  5186  5187  5188  5191  5192  5194  5196  5197  5200  5201  5202  5203  5204 
##  5186  5187  5188  5191  5192  5194  5196  5197  5200  5201  5202  5203  5204 
##  5216  5219  5225  5235  5241  5242  5243  5244  5245  5248  5250  5273  5286 
##  5216  5219  5225  5235  5241  5242  5243  5244  5245  5248  5250  5273  5286 
##  5296  5332  5414  5450  5562  5580  5592  5620  5668  5826  5871  5911  5925 
##  5296  5332  5414  5450  5562  5580  5592  5620  5668  5826  5871  5911  5925 
##  5926  5994  6000  6002  6003  6161  6270  6296  6304  6307  6308  6309  6311 
##  5926  5994  6000  6002  6003  6161  6270  6296  6304  6307  6308  6309  6311 
##  6312  6313  6315  6317  6318  6320  6322  6323  6324  6325  6326  6327  6328 
##  6312  6313  6315  6317  6318  6320  6322  6323  6324  6325  6326  6327  6328 
##  6330  6331  6333  6337  6338  6339  6340  6342  6343  6344  6345  6369  6375 
##  6330  6331  6333  6337  6338  6339  6340  6342  6343  6344  6345  6369  6375 
##  6376  6377  6378  6379  6380  6381  6382  6383  6387  6482  6485  6490  6520 
##  6376  6377  6378  6379  6380  6381  6382  6383  6387  6482  6485  6490  6520 
##  6521  6540  6544  6550  6568  6580  6590  6596  6598  6599  6600  6605  6609 
##  6521  6540  6544  6550  6568  6580  6590  6596  6598  6599  6600  6605  6609 
##  6612  6613  6614  6617  6619  6620  6621  6623  6637  6641  6653  6655  6656 
##  6612  6613  6614  6617  6619  6620  6621  6623  6637  6641  6653  6655  6656 
##  6660  6664  6665  6666  6847  7183  7535  7983  8102  8103  8133  8136  8182 
##  6660  6664  6665  6666  6847  7183  7535  7983  8102  8103  8133  8136  8182 
##  8204  8205  8211  8212  8217  8220  8228  8229  8230  8231  8232  8579  8580 
##  8204  8205  8211  8212  8217  8220  8228  8229  8230  8231  8232  8579  8580 
##  8627  8643  8678  8703  8705  8706  8716  8727  8748  8757  8758  8759  8760 
##  8627  8643  8678  8703  8705  8706  8716  8727  8748  8757  8758  8759  8760 
##  8761  8762  8763  8764  8765  8769  8770  8779  8786  8787  8789  8820  8832 
##  8761  8762  8763  8764  8765  8769  8770  8779  8786  8787  8789  8820  8832 
##  8833  8834  8844  8858  8895  8924  8928  8929  8934  8950  8962  9049  9055 
##  8833  8834  8844  8858  8895  8924  8928  8929  8934  8950  8962  9049  9055 
##  9063  9068  9075  9077  9092  9097  9101  9198  9199  9225  9266  9275  9279 
##  9063  9068  9075  9077  9092  9097  9101  9198  9199  9225  9266  9275  9279 
##  9290  9297  9304  9311  9327  9330  9332  9365  9572  9650  9696  9697  9705 
##  9290  9297  9304  9311  9327  9330  9332  9365  9572  9650  9696  9697  9705 
##  9707  9708  9709  9710  9711  9717  9754  9759  9783  9802  9843  9853  9871 
##  9707  9708  9709  9710  9711  9717  9754  9759  9783  9802  9843  9853  9871 
##  9906 10055 10087 10207 10311 10314 10315 10343 10349 10353 10378 10399 10411 
##  9906 10055 10087 10207 10311 10314 10315 10343 10349 10353 10378 10399 10411 
## 10468 10483 10510 10511 10542 10548 10549 10567 10573 10621 10643 10654 10844 
## 10468 10483 10510 10511 10542 10548 10549 10567 10573 10621 10643 10654 10844 
## 10891 10928 11109 11245 11343 11379 11392 11395 11398 11405 11415 11417 11418 
## 10891 10928 11109 11245 11343 11379 11392 11395 11398 11405 11415 11417 11418 
## 11419 11638 11718 11796 11877 11953 11961 11987 12019 12021 12035 12044 12074 
## 11419 11638 11718 11796 11877 11953 11961 11987 12019 12021 12035 12044 12074 
## 12077 12084 12087 12090 12092 12097 12098 12100 12103 12196 12201 12224 12231 
## 12077 12084 12087 12090 12092 12097 12098 12100 12103 12196 12201 12224 12231 
## 12240 12257 12268 12272 12273 12311 12326 12340 12362 12363 12369 12376 12378 
## 12240 12257 12268 12272 12273 12311 12326 12340 12362 12363 12369 12376 12378 
## 12596 12599 12691 12695 12748 12914 12972 12976 13017 13032 13051 13054 13059 
## 12596 12599 12691 12695 12748 12914 12972 12976 13017 13032 13051 13054 13059 
## 13235 13236 13243 13249 13348 13638 13659 13660 13759 13761 13772 13849 13880 
## 13235 13236 13243 13249 13348 13638 13659 13660 13759 13761 13772 13849 13880 
## 13975 14060 14066 14075 14093 14098 14110 14134 14137 14140 14143 14144 14175 
## 13975 14060 14066 14075 14093 14098 14110 14134 14137 14140 14143 14144 14175 
## 14178 14191 14192 14204 14205 14213 14214 14234 14264 14265 14267 14268 14311 
## 14178 14191 14192 14204 14205 14213 14214 14234 14264 14265 14267 14268 14311 
## 14316 14318 14319 14320 14322 14323 14325 14348 14404 14406 14414 14478 14487 
## 14316 14318 14319 14320 14322 14323 14325 14348 14404 14406 14414 14478 14487 
## 14489 14530 14534 14535 14552 14554 14608 14622 14639 14664 14665 14666 14667 
## 14489 14530 14534 14535 14552 14554 14608 14622 14639 14664 14665 14666 14667 
## 14681 14682 14683 14727 14729 14730 14756 14773 14775 14790 14960 14969 15073 
## 14681 14682 14683 14727 14729 14730 14756 14773 14775 14790 14960 14969 15073 
## 15084 15090 15091 15094 15127 15131 15140 15141 15150 15156 15164 15189 15190 
## 15084 15090 15091 15094 15127 15131 15140 15141 15150 15156 15164 15189 15190 
## 15213 15265 15311 15312 15389 15439 15464 15465 15466 15468 15469 15470 15471 
## 15213 15265 15311 15312 15389 15439 15464 15465 15466 15468 15469 15470 15471 
## 15474 15475 15477 15480 15481 15483 15485 15488 15489 15490 15491 15492 15495 
## 15474 15475 15477 15480 15481 15483 15485 15488 15489 15490 15491 15492 15495 
## 15497 15498 15501 15502 15503 15504 15506 15510 15511 15512 15513 15514 15515 
## 15497 15498 15501 15502 15503 15504 15506 15510 15511 15512 15513 15514 15515 
## 15517 15518 15519 15520 15523 15524 15526 15527 15528 15529 15530 15533 15534 
## 15517 15518 15519 15520 15523 15524 15526 15527 15528 15529 15530 15533 15534 
## 15535 15536 15542 15545 15547 15549 15550 15552 15553 15554 15556 15557 15558 
## 15535 15536 15542 15545 15547 15549 15550 15552 15553 15554 15556 15557 15558 
## 15559 15560 15561 15562 15564 15566 15567 15569 15572 15576 15577 15580 15582 
## 15559 15560 15561 15562 15564 15566 15567 15569 15572 15576 15577 15580 15582 
## 15587 15593 15594 15595 15596 15597 15599 15600 15601 15602 15603 15604 15607 
## 15587 15593 15594 15595 15596 15597 15599 15600 15601 15602 15603 15604 15607 
## 15608 15610 15611 15612 15613 15615 15616 15620 15621 15622 15624 15625 15626 
## 15608 15610 15611 15612 15613 15615 15616 15620 15621 15622 15624 15625 15626 
## 15627 15631 15633 15641 15643 15648 15652 15658 15659 15660 15661 15664 15674 
## 15627 15631 15633 15641 15643 15648 15652 15658 15659 15660 15661 15664 15674 
## 15675 15705 15720 15823 15824 15826 15829 15830 15927 15930 15961 15964 15965 
## 15675 15705 15720 15823 15824 15826 15829 15830 15927 15930 15961 15964 15965 
## 15967 15968 15969 15970 15971 15973 15974 15982 15983 15984 15988 15989 16004 
## 15967 15968 15969 15970 15971 15973 15974 15982 15983 15984 15988 15989 16004 
## 16013 16014 16018 16029 16398 16406 16484 16485 16514 16520 16625 16669 16676 
## 16013 16014 16018 16029 16398 16406 16484 16485 16514 16520 16625 16669 16676 
## 16721 16730 16735 16736 16743 16745 16746 16748 16749 16750 16751 16753 16773 
## 16721 16730 16735 16736 16743 16745 16746 16748 16749 16750 16751 16753 16773 
## 16775 16777 16790 16825 16854 16862 16904 16908 16909 16913 16944 16945 16947 
## 16775 16777 16790 16825 16854 16862 16904 16908 16909 16913 16944 16945 16947 
## 16948 16950 16955 16976 16977 16978 16986 16995 16998 16999 17003 17007 17021 
## 16948 16950 16955 16976 16977 16978 16986 16995 16998 16999 17003 17007 17021 
## 17030 17033 17062 17083 17111 17134 17141 17145 17156 17157 17165 17244 17246 
## 17030 17033 17062 17083 17111 17134 17141 17145 17156 17157 17165 17244 17246 
## 17247 17248 17295 17312 17313 17314 17367 17632 17653 17658 17690 17714 17870 
## 17247 17248 17295 17312 17313 17314 17367 17632 17653 17658 17690 17714 17870 
## 17881 17909 18038 18039 18040 18054 18080 18090 18091 18097 18108 18115 18118 
## 17881 17909 18038 18039 18040 18054 18080 18090 18091 18097 18108 18115 18118 
## 18158 18162 18166 18170 18176 18178 18179 18185 18186 18187 18285 18288 18321 
## 18158 18162 18166 18170 18176 18178 18179 18185 18186 18187 18285 18288 18321 
## 18324 18325 18327 18363 18364 18370 18380 18472 18504 18682 18688 18692 18695 
## 18324 18325 18327 18363 18364 18370 18380 18472 18504 18682 18688 18692 18695 
## 18696 18697 18698 18706 18708 18710 18720 18729 18730 18732 18733 18746 18748 
## 18696 18697 18698 18706 18708 18710 18720 18729 18730 18732 18733 18746 18748 
## 18826 19021 19192 19216 19247 19268 19333 19335 19348 19353 19441 19442 19501 
## 18826 19021 19192 19216 19247 19268 19333 19335 19348 19353 19441 19442 19501 
## 19537 19608 19609 19870 19891 19962 19973 19979 19985 19991 20068 20070 20094 
## 19537 19608 19609 19870 19891 19962 19973 19979 19985 19991 20068 20070 20094 
## 20107 20119 20146 20149 20232 20239 20247 20321 20323 20324 20327 20337 20338 
## 20107 20119 20146 20149 20232 20239 20247 20321 20323 20324 20327 20337 20338 
## 20368 20372 
## 20368 20372


Summary of Influential Points: Cook’s Distance Plot: The plot shows the Cook’s Distance values for each observation, which helps identify influential data points that could have a disproportionate effect on the model’s parameters. A few points have relatively high Cook’s Distance values compared to the majority, suggesting they could be influential in altering the regression model’s results. The threshold for influence is generally considered to be observations with a Cook’s Distance greater than 4/n, where n is the number of observations. In this plot, certain observations exceed this threshold.

List of Influential Points: The identified influential points have been listed, indicating indices where the Cook’s Distance is significantly higher than others. These influential points may represent unusual cases, outliers, or data points that exert significant leverage on the model.

##Step 13: Remediation and Final Model Adjustments

After identifying influential points, it’s important to evaluate their impact on our regression model. Influential points, as identified by Cook’s Distance, may skew the regression results by having an outsized effect on the estimated coefficients. Here, we will examine the influence of these points and determine if any action, such as removing them, is warranted.

Why Remediation is Necessary: Influential points can disproportionately impact the model, potentially leading to biased estimates and affecting the reliability of predictions. By addressing these points, we aim to improve the model’s robustness and ensure that the estimates reflect general trends rather than being skewed by outliers or unique cases.

Approach to Remediation: Inspect Influential Points: Review the identified influential points to determine if they represent genuine data variations, unusual cases, or possible data entry errors. Decide on Adjustment: If the influential points appear to represent genuine observations with valuable information, they should remain in the model. If they are extreme outliers or data entry errors that may distort the overall model, consider excluding them. Refit the Model Without Influential Points: If certain influential points are removed, we can refit the model on the cleaned dataset and compare the results.

Code for Model Refit Without Influential Points In this case, we proceed by removing the influential points identified by Cook’s Distance and refitting the model to assess any changes in parameter estimates and model fit.

# Remove influential points and refit the model
data_cleaned <- data[-influential_points, ]

# Refit the model on the cleaned data
model_cleaned <- lm(log_median_house_value ~ median_income + housing_median_age + total_rooms + ocean_proximity, data = data_cleaned)
summary(model_cleaned)

## 
## Call:
## lm(formula = log_median_house_value ~ median_income + housing_median_age + 
##     total_rooms + ocean_proximity, data = data_cleaned)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.97734 -0.21655 -0.01939  0.19549  1.05941 
## 
## Coefficients:
##                             Estimate Std. Error  t value Pr(>|t|)    
## (Intercept)                1.136e+01  1.012e-02 1122.527  < 2e-16 ***
## median_income              1.828e-01  1.336e-03  136.829  < 2e-16 ***
## housing_median_age         3.466e-03  2.006e-04   17.279  < 2e-16 ***
## total_rooms                2.511e-05  1.222e-06   20.553  < 2e-16 ***
## ocean_proximityINLAND     -5.019e-01  5.269e-03  -95.270  < 2e-16 ***
## ocean_proximityNEAR BAY    1.073e-02  7.640e-03    1.404     0.16    
## ocean_proximityNEAR OCEAN  5.629e-02  6.958e-03    8.090 6.33e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3003 on 19436 degrees of freedom
## Multiple R-squared:  0.6976, Adjusted R-squared:  0.6975 
## F-statistic:  7473 on 6 and 19436 DF,  p-value: < 2.2e-16

# Compare AIC values for the original and cleaned models
AIC(model_log, model_cleaned)
## Warning in AIC.default(model_log, model_cleaned): models are not all fitted to
## the same number of observations
##               df       AIC
## model_log      9 15134.362
## model_cleaned  8  8406.129

Interpretation of Regression Model Results

Coefficients Interpretation:

Intercept: The intercept is estimated at 11.36, which represents the log-transformed baseline median house value when all predictors are zero. median_income: The coefficient for median_income is 1.828, indicating a strong positive association. This means that for each unit increase in median income, the log of the median house value increases by approximately 1.828, holding other variables constant. housing_median_age: The coefficient for housing_median_age is 0.346, suggesting that older housing age is associated with a higher median house value, albeit to a lesser extent compared to income. total_rooms: The coefficient for total_rooms is 0.000025, showing a small positive relationship between the number of rooms and median house value. ocean_proximity (categorical): The INLAND category has a large negative coefficient (-5.019), implying that homes located inland are associated with a significantly lower log-transformed house value compared to the baseline category. The NEAR OCEAN category has a positive coefficient (5.629), showing a significant increase in house value for properties near the ocean.

Model Fit and Significance:

R-squared: The model’s R-squared is 0.6976, meaning approximately 69.76% of the variance in the log-transformed median house value is explained by the predictors. This indicates a reasonably good fit. Adjusted R-squared: The adjusted R-squared of 0.6975 further validates the model’s strength in explaining the variability. F-statistic: The F-statistic is significant (p < 2.2e-16), showing that the overall model is statistically significant.

AIC Comparison:

AIC for model_log: 15134.362 AIC for model_cleaned: 8406.129 The model_cleaned has a significantly lower AIC value compared to model_log, indicating that the refined model provides a better fit to the data with a more parsimonious structure. A lower AIC suggests that model_cleaned is preferable as it achieves a good balance between model fit and complexity.

Conclusion: The regression model indicates that median_income and proximity to the ocean are significant positive predictors of median house value, while inland location negatively affects house values. The improvement in AIC for model_cleaned suggests that this model is a more optimal choice for prediction or inference in this dataset.

##Step 14: Final Diagnostic Checks and Conclusion After refining our model by removing influential points and comparing the AIC values, we have achieved a model with improved fit and stability. To ensure the final model meets all the assumptions of linear regression, we perform one last round of diagnostic checks.

Rationale for Final Diagnostic Checks The final diagnostic checks will help confirm that the refined model (model_cleaned) adheres to the assumptions of linear regression, such as linearity, homoscedasticity, normality of residuals, and lack of autocorrelation. If these assumptions are satisfied, we can be more confident in the validity and reliability of our model for inference or prediction.

Code for Final Diagnostic Checks

# Final diagnostic plots for the refined model
par(mfrow = c(2, 2))

# Residual vs Fitted plot for linearity and homoscedasticity
plot(model_cleaned, which = 1, main = "Residuals vs Fitted (model_cleaned)")

# Q-Q plot for normality of residuals
plot(model_cleaned, which = 2, main = "Normal Q-Q Plot (model_cleaned)")

# Scale-Location plot for homoscedasticity
plot(model_cleaned, which = 3, main = "Scale-Location Plot (model_cleaned)")

# Residuals vs Leverage plot to check for influential points
plot(model_cleaned, which = 5, main = "Residuals vs Leverage (model_cleaned)")


![image](https://github.com/user-attachments/assets/339a0c91-4b4e-40ea-8587-af5160b5488b)


Plots for model_cleaned The final diagnostic plots for model_cleaned provide insights into the model’s adherence to the assumptions of linear regression, including linearity, homoscedasticity, normality of residuals, and influence of specific points. Here’s a detailed interpretation of each plot:

Residuals vs Fitted Plot:

This plot checks for linearity and homoscedasticity. Ideally, the residuals should be randomly scattered around the horizontal line at zero, without any discernible pattern. In this case, the residuals show some funneling or spreading as fitted values increase, indicating potential heteroscedasticity (non-constant variance). Although the residuals generally hover around zero, the slight funneling suggests that while the log transformation improved the model, there might still be minor issues with heteroscedasticity.

Normal Q-Q Plot: This plot assesses the normality of residuals. If the residuals follow a normal distribution, they should align closely with the diagonal reference line. Here, the residuals deviate from the line, especially at the tails, indicating that the residuals are not perfectly normal. Although some deviation is acceptable, especially in large datasets, the tails suggest mild departures from normality, which might slightly impact statistical inference.

Scale-Location Plot: This plot provides another check for homoscedasticity. A flat red line and an even distribution of residuals along the range of fitted values would indicate constant variance. In our model, the red line is relatively flat, but there is a slight trend where residuals become more spread out as fitted values increase. This pattern supports the findings from the Residuals vs Fitted plot, hinting at minor heteroscedasticity. However, the deviation isn’t extreme, so it’s likely that the log transformation has largely addressed this issue.

Residuals vs Leverage Plot: This plot identifies influential points. Observations with high leverage or those outside the Cook’s distance lines may disproportionately impact the model. In this plot, a few points are close to the Cook’s distance line, indicating that these observations have some influence on the model. However, they don’t appear to be highly problematic as they are within acceptable ranges. Given the adjustments already made to remove influential points, the remaining data points seem to exert minimal undue influence on the model.

Summary of Diagnostic Analysis: The final diagnostic checks for model_cleaned indicate that the model is reasonably well-behaved:

Linearity and Homoscedasticity: While minor heteroscedasticity remains, the log transformation effectively reduced this issue. The patterns observed do not seem severe enough to significantly impact model validity. Normality: The Q-Q plot shows some deviation from normality in the tails, which may slightly affect the accuracy of p-values and confidence intervals. However, this deviation is not extreme. Influence: The leverage plot reveals that no points exert excessive influence, suggesting that the model is robust against individual outliers.

Overall, these diagnostics suggest that model_cleaned is a reliable model for predicting the log-transformed median house value, with only minor deviations from ideal assumptions.

##Step 15: Summary of Key Findings and Conclusions:

The analysis of the housing dataset using multiple linear regression has provided significant insights into the factors influencing median_house_value. After refining the model to address issues of autocorrelation, heteroscedasticity, and influential points, we have arrived at a more robust and interpretable model. Below is a summary of the key findings:

Significant Predictors Median Income: This is the strongest predictor of median_house_value. With a positive coefficient, median income shows a substantial impact on housing prices, indicating that as income levels rise, so does the median house value. Each unit increase in median_income corresponds to approximately an 18% increase in median house value on the original scale, underscoring the importance of income levels in housing valuation.

Housing Median Age: Housing age also has a positive association with house prices. For each additional year in housing age, there is a slight increase in median house value, suggesting that older neighborhoods may carry a premium, possibly due to established infrastructure or desirable locations.

Total Rooms: This variable shows a positive but smaller effect on housing prices. Each additional room corresponds to a modest increase in house value, supporting the idea that larger homes are valued higher.

Ocean Proximity: The categorical variable ocean_proximity has varying effects on house prices:

Inland properties: These properties have significantly lower house values compared to the baseline (which is proximity to the bay), with a large negative coefficient. Properties near the ocean: These homes show a significant positive impact on house prices, suggesting that proximity to the ocean is a valued feature. Model Performance and Fit The final model, model_cleaned, has an adjusted R-squared value of approximately 0.6975, indicating that around 69.75% of the variance in median_house_value is explained by the predictors in the model. This is a reasonably good fit for a regression model in real estate, where other unobserved factors (like neighborhood characteristics) can also play a role in housing prices.

The comparison of AIC values between the initial and final models showed a substantial decrease after removing influential points and refining the model. The final model’s lower AIC value suggests it provides a better fit without adding unnecessary complexity.

Addressing Model Assumptions Throughout the analysis, various diagnostic checks were performed to ensure that the assumptions of linear regression were reasonably met:

Linearity and Homoscedasticity: While slight heteroscedasticity remains, the log transformation on the response variable significantly reduced this issue. The Residuals vs Fitted plot shows a reasonably uniform spread around zero. Normality of Residuals: The Q-Q plot shows some deviations from normality at the tails, but these are not extreme. Given the dataset size, these deviations are considered minor and unlikely to affect the overall validity of the model. Influence of Outliers: After removing points with high Cook’s Distance, the model is less affected by extreme outliers, increasing the reliability of the estimated coefficients.

##Step 16: Practical Implications and Recommendations

Based on the analysis, we can draw practical conclusions that may benefit stakeholders in the housing market, such as real estate investors, developers, and policymakers.

Income-Driven Investment: Areas with higher median incomes are associated with significantly higher house values. Investors and developers might focus on such neighborhoods for high-value developments.

Desirability of Ocean Proximity: Proximity to the ocean is a major positive factor for house values. Properties near the ocean or bay are likely to attract higher prices. This finding could guide developers and investors when choosing locations for luxury or premium housing projects.

Older, Established Neighborhoods: The positive coefficient for housing age suggests that some buyers might value the established character of older neighborhoods. However, this effect is modest compared to income and proximity factors.

Recommendations for Pricing and Valuation: Real estate professionals can use these insights to better price properties. Inland properties, for instance, should be valued lower than those near the ocean or bay, even after accounting for room size and age.

Limitations and Future Research Residual Heteroscedasticity: While the log transformation helped address non-constant variance, slight heteroscedasticity remains. Future analyses could explore alternative transformations or more complex models like generalized least squares (GLS) that account for this issue. Non-Normal Residuals: Mild deviations from normality in the residuals might suggest that further refinements or alternative models (e.g., robust regression) could be explored. Unobserved Predictors: Factors like crime rates, school quality, and proximity to amenities were not included in this analysis. Future research could incorporate these variables for potentially improved predictions.

This analysis provided a comprehensive examination of the factors influencing housing prices in the dataset, focusing on identifying and mitigating model issues to improve reliability and interpretability. By addressing autocorrelation, influential points, and heteroscedasticity, we arrived at a robust model that reveals valuable insights into the housing market.

Key Conclusions:

Median income and proximity to the ocean are strong positive predictors of housing prices, highlighting the impact of economic and geographical factors on property values. Housing age and total rooms also contribute positively, albeit to a lesser extent, indicating that older and larger homes hold value in certain regions. The refined model, with a high adjusted R-squared and lower AIC, demonstrates an improved fit and balance between complexity and accuracy, making it suitable for predictive and inferential purposes.
