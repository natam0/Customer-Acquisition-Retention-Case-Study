Customer Retention Case Study
================

``` r
library(SMCRM)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(randomForestSRC)
```

    ## 
    ##  randomForestSRC 3.3.3 
    ##  
    ##  Type rfsrc.news() to see new features, changes, and bug fixes. 
    ##  
    ## 
    ## 
    ## Attaching package: 'randomForestSRC'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     partial

``` r
library(rpart)
```

Read in the data

``` r
data(acquisitionRetention)
anyNA(acquisitionRetention)
```

    ## [1] FALSE

``` r
str(acquisitionRetention)
```

    ## 'data.frame':    500 obs. of  15 variables:
    ##  $ customer   : num  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ acquisition: num  1 1 1 0 1 1 1 1 0 0 ...
    ##  $ duration   : num  1635 1039 1288 0 1631 ...
    ##  $ profit     : num  6134 3524 4081 -638 5446 ...
    ##  $ acq_exp    : num  694 460 249 638 589 ...
    ##  $ ret_exp    : num  972 450 805 0 920 ...
    ##  $ acq_exp_sq : num  480998 211628 62016 407644 346897 ...
    ##  $ ret_exp_sq : num  943929 202077 648089 0 846106 ...
    ##  $ freq       : num  6 11 21 0 2 7 15 13 0 0 ...
    ##  $ freq_sq    : num  36 121 441 0 4 49 225 169 0 0 ...
    ##  $ crossbuy   : num  5 6 6 0 9 4 5 5 0 0 ...
    ##  $ sow        : num  95 22 90 0 80 48 51 23 0 0 ...
    ##  $ industry   : num  1 0 0 0 0 1 0 1 0 1 ...
    ##  $ revenue    : num  47.2 45.1 29.1 40.6 48.7 ...
    ##  $ employees  : num  898 686 1423 181 631 ...

``` r
cor(acquisitionRetention)
```

    ##                 customer  acquisition    duration      profit       acq_exp
    ## customer     1.000000000  0.054034172  0.03638573  0.04099600 -0.0344779052
    ## acquisition  0.054034172  1.000000000  0.94499140  0.96392988 -0.0017487494
    ## duration     0.036385733  0.944991403  1.00000000  0.98434462  0.0117152328
    ## profit       0.040996005  0.963929883  0.98434462  1.00000000  0.0395591912
    ## acq_exp     -0.034477905 -0.001748749  0.01171523  0.03955919  1.0000000000
    ## ret_exp      0.022855507  0.874115734  0.97852966  0.95164652  0.0118498772
    ## acq_exp_sq  -0.043878373 -0.077186844 -0.05987778 -0.03958462  0.9737781739
    ## ret_exp_sq  -0.007095947  0.631302293  0.82648533  0.77709045  0.0267900795
    ## freq         0.037846540  0.778910016  0.70998289  0.74689819  0.0007026176
    ## freq_sq      0.030427659  0.567605728  0.49768232  0.53904545 -0.0050416071
    ## crossbuy     0.064402299  0.866154614  0.83264401  0.85553188  0.0258650343
    ## sow          0.008009862  0.847150795  0.80815353  0.83170352  0.0308839247
    ## industry     0.095383073  0.244373420  0.20789458  0.22744229  0.0145649028
    ## revenue      0.004210533  0.248837329  0.22598952  0.24148754  0.0643407122
    ## employees    0.018449766  0.477019473  0.43320548  0.47130639 -0.0419314213
    ##                 ret_exp  acq_exp_sq   ret_exp_sq          freq      freq_sq
    ## customer     0.02285551 -0.04387837 -0.007095947  0.0378465396  0.030427659
    ## acquisition  0.87411573 -0.07718684  0.631302293  0.7789100155  0.567605728
    ## duration     0.97852966 -0.05987778  0.826485332  0.7099828902  0.497682317
    ## profit       0.95164652 -0.03958462  0.777090452  0.7468981898  0.539045453
    ## acq_exp      0.01184988  0.97377817  0.026790080  0.0007026176 -0.005041607
    ## ret_exp      1.00000000 -0.05562666  0.920692412  0.6940495656  0.513877389
    ## acq_exp_sq  -0.05562666  1.00000000 -0.023073486 -0.0604820431 -0.050203871
    ## ret_exp_sq   0.92069241 -0.02307349  1.000000000  0.5059909848  0.377358737
    ## freq         0.69404957 -0.06048204  0.505990985  1.0000000000  0.938395608
    ## freq_sq      0.51387739 -0.05020387  0.377358737  0.9383956081  1.000000000
    ## crossbuy     0.77688913 -0.04020979  0.578974344  0.6863182859  0.520562814
    ## sow          0.74092081 -0.02980165  0.532801273  0.6603665316  0.481760366
    ## industry     0.18104417  0.03250630  0.099428831  0.1604756340  0.102778207
    ## revenue      0.20188718  0.04431747  0.130012381  0.1545687918  0.095854901
    ## employees    0.40942256 -0.05879389  0.286288747  0.4292780382  0.355054413
    ##                crossbuy          sow     industry     revenue    employees
    ## customer     0.06440230  0.008009862  0.095383073 0.004210533  0.018449766
    ## acquisition  0.86615461  0.847150795  0.244373420 0.248837329  0.477019473
    ## duration     0.83264401  0.808153531  0.207894584 0.225989515  0.433205481
    ## profit       0.85553188  0.831703516  0.227442291 0.241487536  0.471306392
    ## acq_exp      0.02586503  0.030883925  0.014564903 0.064340712 -0.041931421
    ## ret_exp      0.77688913  0.740920812  0.181044165 0.201887177  0.409422565
    ## acq_exp_sq  -0.04020979 -0.029801655  0.032506302 0.044317467 -0.058793888
    ## ret_exp_sq   0.57897434  0.532801273  0.099428831 0.130012381  0.286288747
    ## freq         0.68631829  0.660366532  0.160475634 0.154568792  0.429278038
    ## freq_sq      0.52056281  0.481760366  0.102778207 0.095854901  0.355054413
    ## crossbuy     1.00000000  0.746630339  0.218109778 0.194752511  0.415964288
    ## sow          0.74663034  1.000000000  0.209726320 0.233400782  0.414154030
    ## industry     0.21810978  0.209726320  1.000000000 0.030086417 -0.002323206
    ## revenue      0.19475251  0.233400782  0.030086417 1.000000000  0.047489335
    ## employees    0.41596429  0.414154030 -0.002323206 0.047489335  1.000000000

Convert necessary variables to factors

``` r
acquisitionRetention$acquisition = as.factor(acquisitionRetention$acquisition)
acquisitionRetention$industry = as.factor(acquisitionRetention$industry)
```

Create a df for predicting acquisition

``` r
acquire_df = acquisitionRetention %>% 
  select(-duration, -profit, -ret_exp_sq, -ret_exp, -freq, -freq_sq, -sow, -crossbuy)
```

Split the data

``` r
set.seed(23)
training_index = sample(nrow(acquire_df), 0.7 * nrow(acquire_df))
train_acq = acquire_df[training_index, ]
test_acq = acquire_df[-training_index, ]
```

#### Decision Tree with whole data set

``` r
dt_model = rpart(acquisition ~ acq_exp + industry + revenue + employees, data = acquire_df)

rattle::fancyRpartPlot(dt_model, sub = "")
```

![](CustomerRetention_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
\#### Random Forest with whole acquisition data set

``` r
set.seed(23)
rf_model = rfsrc(acquisition ~ acq_exp + industry + revenue + employees, data = acquire_df, importance = TRUE, ntree = 1000)
rf_model
```

    ##                          Sample size: 500
    ##            Frequency of class labels: 162, 338
    ##                      Number of trees: 1000
    ##            Forest terminal node size: 1
    ##        Average no. of terminal nodes: 76.989
    ## No. of variables tried at each split: 2
    ##               Total no. of variables: 4
    ##        Resampling used to grow trees: swor
    ##     Resample size used to grow trees: 316
    ##                             Analysis: RF-C
    ##                               Family: class
    ##                       Splitting rule: gini *random*
    ##        Number of random split points: 10
    ##                     Imbalanced ratio: 2.0864
    ##                    (OOB) Brier score: 0.14413189
    ##         (OOB) Normalized Brier score: 0.57652757
    ##                            (OOB) AUC: 0.85746767
    ##                       (OOB) Log-loss: 0.43763632
    ##                         (OOB) PR-AUC: 0.73772284
    ##                         (OOB) G-mean: 0.71541321
    ##    (OOB) Requested performance error: 0.22, 0.41358025, 0.12721893
    ## 
    ## Confusion matrix:
    ## 
    ##           predicted
    ##   observed  0   1 class.error
    ##          0 95  67      0.4136
    ##          1 43 295      0.1272
    ## 
    ##       (OOB) Misclassification rate: 0.22

#### Forest inference

``` r
str(rf_model$importance)
```

    ##  num [1:4, 1:3] 0.2051 0.0621 0.1247 0.2866 -0.723 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : chr [1:4] "acq_exp" "industry" "revenue" "employees"
    ##   ..$ : chr [1:3] "all" "0" "1"

``` r
importance_df <- as.data.frame(rf_model$importance) %>%
  tibble::rownames_to_column(var = "variable")

importance_df %>%
  ggplot(aes(x = reorder(variable, all), y = all)) +
  geom_bar(stat = "identity", fill = "orange", color = "black", width = 0.5) +
  coord_flip() +
  labs(x = "Variables", y = "Variable importance") +
  theme_minimal()
```

![](CustomerRetention_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

#### Interactions

``` r
find.interaction(rf_model,
                      method = "vimp",
                      importance = "permute")
```

    ## Pairing employees with acq_exp 
    ## Pairing employees with revenue 
    ## Pairing employees with industry 
    ## Pairing acq_exp with revenue 
    ## Pairing acq_exp with industry 
    ## Pairing revenue with industry 
    ## 
    ##                               Method: vimp
    ##                     No. of variables: 4
    ##            Variables sorted by VIMP?: TRUE
    ##    No. of variables used for pairing: 4
    ##     Total no. of paired interactions: 6
    ##             Monte Carlo replications: 1
    ##     Type of noising up used for VIMP: permute
    ## 
    ##                     Var 1  Var 2 Paired Additive Difference
    ## employees:acq_exp  0.0782 0.0203 0.1010   0.0985     0.0026
    ## employees:revenue  0.0782 0.0153 0.0984   0.0935     0.0049
    ## employees:industry 0.0782 0.0187 0.0970   0.0969     0.0000
    ## acq_exp:revenue    0.0200 0.0144 0.0445   0.0344     0.0101
    ## acq_exp:industry   0.0200 0.0198 0.0341   0.0398    -0.0057
    ## revenue:industry   0.0157 0.0196 0.0358   0.0353     0.0005

No notably strong interactions.

#### Train Random Forest

``` r
set.seed(23)
rf_model1 = rfsrc(acquisition ~ acq_exp + industry + revenue + employees, data = train_acq, importance = TRUE, ntree = 1000)
rf_model1
```

    ##                          Sample size: 350
    ##            Frequency of class labels: 110, 240
    ##                      Number of trees: 1000
    ##            Forest terminal node size: 1
    ##        Average no. of terminal nodes: 53.905
    ## No. of variables tried at each split: 2
    ##               Total no. of variables: 4
    ##        Resampling used to grow trees: swor
    ##     Resample size used to grow trees: 221
    ##                             Analysis: RF-C
    ##                               Family: class
    ##                       Splitting rule: gini *random*
    ##        Number of random split points: 10
    ##                     Imbalanced ratio: 2.1818
    ##                    (OOB) Brier score: 0.13875148
    ##         (OOB) Normalized Brier score: 0.55500593
    ##                            (OOB) AUC: 0.86371212
    ##                       (OOB) Log-loss: 0.42305154
    ##                         (OOB) PR-AUC: 0.73654689
    ##                         (OOB) G-mean: 0.73350549
    ##    (OOB) Requested performance error: 0.20285714, 0.39090909, 0.11666667
    ## 
    ## Confusion matrix:
    ## 
    ##           predicted
    ##   observed  0   1 class.error
    ##          0 67  43      0.3909
    ##          1 28 212      0.1167
    ## 
    ##       (OOB) Misclassification rate: 0.2028571

#### Build a linear model and decision tree for later comparision

``` r
glm1 = glm(acquisition ~ acq_exp + acq_exp_sq + industry + revenue + employees, data = train_acq, family = binomial)
dt_model1 = rpart(acquisition ~ acq_exp + industry + revenue + employees, data = train_acq)
```

#### OOB error block rate

``` r
rf_model1$err.rate[length(rf_model1$err.rate)]
```

    ## [1] 0.1166667

#### Hyper-tuning

``` r
set.seed(23)
# Establish a list of possible values for hyper-parameters
mtry.values <- seq(4,6,1)
nodesize.values <- seq(4,8,2)
ntree.values <- seq(4e3,6e3,1e3)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry.values, nodesize = nodesize.values, ntree = ntree.values)

# Create an empty vector to store OOB error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {

    # Train a Random Forest model
   set.seed(23)
   model <- rfsrc(acquisition ~ acq_exp + industry + revenue + employees,
                            data = train_acq,
                            mtry = hyper_grid$mtry[i],
                            nodesize = hyper_grid$nodesize[i],
                            ntree = hyper_grid$ntree[i])  
  
                          
    # Store OOB error for the model                      
    oob_err[i] <- model$err.rate[length(model$err.rate)]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])
```

    ##   mtry nodesize ntree
    ## 4    4        6  4000

#### Rebuild Training Model with optimized hyper-parameters

``` r
set.seed(23)
rf_hyper_model <- rfsrc(acquisition ~ acq_exp + industry + revenue + employees,
            data = train_acq,
            mtry = 4,
            nodesize = 6,
            ntree = 4000) 
```

#### Predict on test set

``` r
preds_df <- 
  data.frame(pred1 = predict.rfsrc(rf_model1, newdata = test_acq)$predicted[,2], 
             pred2 = predict.rfsrc(rf_hyper_model, newdata = test_acq)$predicted[,2], 
             pred3 = predict.glm(glm1, newdata = test_acq, type = "response"), 
             pred4 = predict(dt_model1, newdata = test_acq)[,2],
             actual = test_acq$acquisition, 
             customer = test_acq$customer)
 
preds_df$thresh1 = ifelse(preds_df$pred1 >= 0.5, 1, 0)
preds_df$thresh2 = ifelse(preds_df$pred2 >= 0.5, 1, 0)
preds_df$thresh3 = ifelse(preds_df$pred3 >= 0.5, 1, 0)
preds_df$thresh4 = ifelse(preds_df$pred4 >= 0.5, 1, 0)

conf_mat1 = caret::confusionMatrix(as.factor(preds_df$thresh1), as.factor(preds_df$actual), positive = "1")
conf_mat2 = caret::confusionMatrix(as.factor(preds_df$thresh2), as.factor(preds_df$actual), positive = "1")
conf_mat3 = caret::confusionMatrix(as.factor(preds_df$thresh3), as.factor(preds_df$actual), positive = "1")
conf_mat4 = caret::confusionMatrix(as.factor(preds_df$thresh4), as.factor(preds_df$actual), positive = "1")


results <- tibble(
  Model = c("RF", "Tuned_RF", "GLM", "DT"),
  Accuracy = round(c(conf_mat1$overall[1], conf_mat2$overall[1], conf_mat3$overall[1], conf_mat4$overall[1]), 4
  ))

print(results)
```

    ## # A tibble: 4 × 2
    ##   Model    Accuracy
    ##   <chr>       <dbl>
    ## 1 RF          0.793
    ## 2 Tuned_RF    0.793
    ## 3 GLM         0.8  
    ## 4 DT          0.76

#### Predicted and Actual

Now we need to take our full data set and use our tuned Random Forest to
make predictions and from those predictions make a list of customers who
were predicted to be acquired and were actually acquired. We can then
use this data to build a duration model.

Create a list of customers with a pred over .5 and an actual of 1

``` r
preds_all <- 
  data.frame(pred2 = predict.rfsrc(rf_hyper_model, newdata = acquire_df)$predicted[,2], 
             actual = acquire_df$acquisition, 
             customer = acquire_df$customer)

preds_all$thresh = ifelse(preds_all$pred2 >= 0.5, 1, 0)

caret::confusionMatrix(as.factor(preds_all$thresh), as.factor(preds_all$actual), positive = "1")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 128  26
    ##          1  34 312
    ##                                           
    ##                Accuracy : 0.88            
    ##                  95% CI : (0.8482, 0.9072)
    ##     No Information Rate : 0.676           
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.7225          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.3662          
    ##                                           
    ##             Sensitivity : 0.9231          
    ##             Specificity : 0.7901          
    ##          Pos Pred Value : 0.9017          
    ##          Neg Pred Value : 0.8312          
    ##              Prevalence : 0.6760          
    ##          Detection Rate : 0.6240          
    ##    Detection Prevalence : 0.6920          
    ##       Balanced Accuracy : 0.8566          
    ##                                           
    ##        'Positive' Class : 1               
    ## 

``` r
high_value_customers = preds_all %>%
  filter(pred2 > 0.5 & actual == 1) %>%
  pull(customer)
```

Create duration df from `high_value_customer`

``` r
duration_df = acquisitionRetention[high_value_customers, ] %>% 
  select(-acquisition)
```

#### Linear model to test for perfect seperation

``` r
lm = lm(duration ~ . -ret_exp -profit -ret_exp_sq, data = duration_df)
summary(lm)
```

    ## 
    ## Call:
    ## lm(formula = duration ~ . - ret_exp - profit - ret_exp_sq, data = duration_df)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -454.9 -134.7  -17.2  127.9  578.2 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.017e+03  1.689e+02   6.020 5.07e-09 ***
    ## customer    -6.605e-02  8.640e-02  -0.764   0.4452    
    ## acq_exp      1.569e-01  5.001e-01   0.314   0.7540    
    ## acq_exp_sq  -8.968e-05  4.931e-04  -0.182   0.8558    
    ## freq         5.967e+00  1.032e+01   0.578   0.5637    
    ## freq_sq     -6.533e-01  5.131e-01  -1.273   0.2039    
    ## crossbuy     1.182e+01  6.263e+00   1.887   0.0601 .  
    ## sow          5.036e-01  6.003e-01   0.839   0.4021    
    ## industry1   -3.231e+01  2.537e+01  -1.274   0.2037    
    ## revenue     -4.028e-01  1.345e+00  -0.300   0.7647    
    ## employees   -1.816e-02  6.007e-02  -0.302   0.7626    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 214.1 on 301 degrees of freedom
    ## Multiple R-squared:  0.04498,    Adjusted R-squared:  0.01325 
    ## F-statistic: 1.418 on 10 and 301 DF,  p-value: 0.1714

#### Investigate RF

``` r
set.seed(23)
dur_rf <- rfsrc(duration ~  ret_exp + employees + industry + revenue, 
                            data = duration_df, 
                            importance = TRUE, 
                            ntree = 1000)

dur_rf
```

    ##                          Sample size: 312
    ##                      Number of trees: 1000
    ##            Forest terminal node size: 5
    ##        Average no. of terminal nodes: 39.108
    ## No. of variables tried at each split: 2
    ##               Total no. of variables: 4
    ##        Resampling used to grow trees: swor
    ##     Resample size used to grow trees: 197
    ##                             Analysis: RF-R
    ##                               Family: regr
    ##                       Splitting rule: mse *random*
    ##        Number of random split points: 10
    ##                      (OOB) R squared: 0.91852343
    ##    (OOB) Requested performance error: 3784.41297888

``` r
dur_rf$importance # values vary a lot
```

    ##      ret_exp    employees     industry      revenue 
    ## 293701.85039   8408.91170    -58.78518   8957.34968

``` r
data.frame(importance = dur_rf$importance) %>%
  tibble::rownames_to_column(var = "variable") %>%
  ggplot(aes(x = reorder(variable,importance), y = importance)) +
    geom_bar(stat = "identity", fill = "orange", color = "black")+
    coord_flip() +
     labs(x = "Variables", y = "Variable importance")+
     theme_minimal()         # where's industy? difficult to see
```

![](CustomerRetention_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
dur_rf$importance %>% log() # log transform
```

    ## Warning in log(.): NaNs produced

    ##   ret_exp employees  industry   revenue 
    ## 12.590320  9.037047       NaN  9.100230

``` r
data.frame(importance = dur_rf$importance + 100) %>% # add a large +ve constant
  log() %>%
  tibble::rownames_to_column(var = "variable") %>%
  ggplot(aes(x = reorder(variable,importance), y = importance)) +
    geom_bar(stat = "identity", fill = "orange", color = "black", width = 0.5)+
    coord_flip() +
    labs(x = "Variables", y = "Log-transformed variable importance") +
    theme_minimal()
```

![](CustomerRetention_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->

``` r
find.interaction(dur_rf,
                      method = "vimp",
                      importance = "permute")
```

    ## Pairing ret_exp with revenue 
    ## Pairing ret_exp with employees 
    ## Pairing ret_exp with industry 
    ## Pairing revenue with employees 
    ## Pairing revenue with industry 
    ## Pairing employees with industry 
    ## 
    ##                               Method: vimp
    ##                     No. of variables: 4
    ##            Variables sorted by VIMP?: TRUE
    ##    No. of variables used for pairing: 4
    ##     Total no. of paired interactions: 6
    ##             Monte Carlo replications: 1
    ##     Type of noising up used for VIMP: permute
    ## 
    ##                         Var 1    Var 2     Paired   Additive Difference
    ## ret_exp:revenue    54485.9853 -52.8875 54880.0268 54433.0978   446.9290
    ## ret_exp:employees  54485.9853  53.3341 54393.4237 54539.3194  -145.8957
    ## ret_exp:industry   54485.9853 -32.3031 54432.6482 54453.6822   -21.0340
    ## revenue:employees    -61.8076 104.5420     5.9976    42.7344   -36.7368
    ## revenue:industry     -61.8076 -64.3392   -90.4557  -126.1468    35.6912
    ## employees:industry   -30.8786 -45.8848   -17.3520   -76.7634    59.4114

#### Training Split

``` r
set.seed(23)
idx.train <- sample(1:nrow(duration_df), size = 0.6 * nrow(duration_df))
train_dur <- duration_df[idx.train,]
test_dur <- duration_df[-idx.train,]
```

``` r
set.seed(23)
dur_rf1 <- rfsrc(duration ~ ret_exp,
                            data = train_dur, 
                            importance = TRUE, 
                            ntree = 1000)

dur_rf1
```

    ##                          Sample size: 187
    ##                      Number of trees: 1000
    ##            Forest terminal node size: 5
    ##        Average no. of terminal nodes: 24.194
    ## No. of variables tried at each split: 1
    ##               Total no. of variables: 1
    ##        Resampling used to grow trees: swor
    ##     Resample size used to grow trees: 118
    ##                             Analysis: RF-R
    ##                               Family: regr
    ##                       Splitting rule: mse *random*
    ##        Number of random split points: 10
    ##                      (OOB) R squared: 0.92111916
    ##    (OOB) Requested performance error: 3669.34996776

``` r
# construct linear and non-linear parametric models on training set
lm1 <- lm(duration ~  ret_exp, data = train_dur)

lm2 <- lm(duration ~  ret_exp + ret_exp_sq, data = train_dur)

dt_dur1 <- rpart(duration ~ ret_exp, data = train_dur)
```

#### OOB error rates

``` r
dur_rf1$err.rate[length(dur_rf1$err.rate)]
```

    ## [1] 3669.35

``` r
# plot the OOB error rate
data.frame(err.rate = dur_rf1$err.rate) %>%
  na.omit() %>%
  tibble::rownames_to_column(var = "trees") %>%
  mutate(trees = as.numeric(trees)) %>%
  ggplot(aes(x = trees, y = err.rate, group = 1))+
  geom_line()+
  scale_x_continuous(breaks = seq(0,1050,100))+
  labs(x = "Number of trees", y = "OOB Error rate")+
  theme_minimal()
```

![](CustomerRetention_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
# compare OOB error rates of original forest and forest with interaction variables
set.seed(23)
dur_rf2 <- rfsrc(duration ~ ret_exp + profit,
                            data = train_dur, 
                            importance = TRUE, 
                            ntree = 1000)

dur_rf2$importance
```

    ##  ret_exp   profit 
    ## 93758.80 56483.52

``` r
data.frame(forest2 = dur_rf1$err.rate, forest3 = dur_rf2$err.rate) %>%
  na.omit() %>%
  tibble::rownames_to_column(var = "trees") %>%
  mutate(trees = as.numeric(trees)) %>%
  gather(key = forest_type, value = OOB.err, -trees) %>%
  ggplot(aes(x = trees, y = OOB.err, color = forest_type))+
  geom_line()+
  scale_color_brewer(palette = "Set1")+
  scale_x_continuous(breaks = seq(0,1050,100))+
  labs(x = "Number of trees", y = "OOB Error rate")+
  theme_minimal()  
```

![](CustomerRetention_files/figure-gfm/unnamed-chunk-24-2.png)<!-- -->

#### Hyper tune

``` r
set.seed(23)
# Establish a list of possible values for hyper-parameters
mtry.values <- seq(2,6,1)
nodesize.values <- seq(4,8,2)
ntree.values <- seq(4e3,6e3,1e3)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry.values, nodesize = nodesize.values, ntree = ntree.values)

# Create an empty vector to store OOB error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {

    # Train a Random Forest model
   set.seed(23)
   model <- rfsrc(duration ~ ret_exp, 
                            data = train_dur,
                            mtry = hyper_grid$mtry[i],
                            nodesize = hyper_grid$nodesize[i],
                            ntree = hyper_grid$ntree[i])  
  
                          
    # Store OOB error for the model                      
    oob_err[i] <- model$err.rate[length(model$err.rate)]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])
```

    ##    mtry nodesize ntree
    ## 36    2        6  6000

#### Build Tuned RF with training set

``` r
set.seed(23)
rf_hyper <- rfsrc(duration ~ ret_exp, 
                            data = train_dur,
                            mtry = 2,
                            nodesize = 6,
                            ntree = 6000)
```

#### Evaluate Error

``` r
error_df1 <- 
  data.frame(pred1a = predict.rfsrc(dur_rf1, newdata = test_dur)$predicted,
             pred1b = predict.rfsrc(dur_rf2, newdata = test_dur)$predicted,
             pred2a = predict.rfsrc(rf_hyper, newdata = test_dur)$predicted, 
             pred3a = predict(lm1, newdata = test_dur),
             pred4a = predict(lm2, newdata = test_dur),
             pred5a = predict(dt_dur1, newdata = test_dur),
             actual = test_dur$duration, 
             customer = test_dur$customer) %>%
  mutate(across(
    .cols = c(pred1a, pred1b, pred2a, pred3a, pred4a, pred5a),
    .fns = list(
      abs.error = ~ abs(actual - .),
      abs.percent.error = ~ abs(actual - .)/abs(actual)
    )
  ))

# MAE calculation
errors_sum = error_df1 %>%
  summarise(across(
    .cols = c(pred1a_abs.error, pred1b_abs.error, pred2a_abs.error, pred3a_abs.error, pred4a_abs.error, pred5a_abs.error),
    .fns = mean,
    .names = "{.col}_mae"
  ))
```

Results

``` r
results <- tibble(
  Model = c("RF1", "RF2", "Tuned_RF", "LM1", "LM2", "DT"),
  MSE = round(c(errors_sum$pred1a_abs.error_mae, errors_sum$pred1b_abs.error_mae, errors_sum$pred2a_abs.error_mae, errors_sum$pred3a_abs.error_mae, errors_sum$pred4a_abs.error_mae, errors_sum$pred5a_abs.error_mae), 4
  ))

print(results)
```

    ## # A tibble: 6 × 2
    ##   Model      MSE
    ##   <chr>    <dbl>
    ## 1 RF1       38.0
    ## 2 RF2       42.8
    ## 3 Tuned_RF  38.0
    ## 4 LM1       45.5
    ## 5 LM2       34.1
    ## 6 DT        49.8

### PDP

``` r
plot.variable(rf_hyper_model, partial = TRUE)
```

![](CustomerRetention_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
plot.variable(rf_hyper, partial = TRUE)
```

![](CustomerRetention_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->
