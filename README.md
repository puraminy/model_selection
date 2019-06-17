
Implementation of stepwise forward and backward model selection in R

Two methods were implemented

 - Forwards selection based on p-value
 - Backward elimination based on Adjusted R-squared

# Forwards Selection - p-values
 - Start with single predictor regressions of response vs. each explanatory variable
 - Pick the variable with the lowest significant p-value
 - Add the remaining variables one at a time to the existing model, and pick the variable with the lowest significant p-value
 - Repeat until any of the remaining variables do not have a significant p-value

# Backwards Elimination - Adjusted R-squared

 - Start with the full model
 - Drop one variable at a time and record adjusted R2 of each smaller model
 - Pick the model with the highest increase in adjusted R2
 - Repeat until none of the models yield an increase in adjusted R2

# How to use

```r
model1 = backward_adj_r2(dtframe = mtcars,response = "mpg", exclude = c("wt"))
summary(model1)

model2 = forward_p_value(dtframe = mtcars,response = "mpg", exclude = c("wt"), alpha=0.05)
summary(model2)
```
