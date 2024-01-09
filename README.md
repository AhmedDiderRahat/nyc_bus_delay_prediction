---
Title: NYC Bus Delay Prediction
Author: Rahat, A.D., Sultana, T., & Mamun, R.K. 
Date: January 09th, 2024
---


### Introduction
In this report, we present our exploration into predicting bus delays in New York City using machine learning techniques. Our project began with a rich dataset from Kaggle, capturing the information of NYC's traffic. From this, we carefully selected a representative sample of 2000 rows. After preprocessing the data to refine and structure it, we conducted an exploratory data analysis to uncover underlying patterns and relationships. Our approach included splitting the data into an 80/20 train-test ratio and applying target encoding to the categorical features. The heart of our analysis involved developing and tuning two machine learning models: Lasso regression and Generalized Additive Models (GAM), aimed at accurately predicting bus delay times.


### Project Goal:
This project is the part of Machine Learning-II course during the winter semester of 2023/2024. The main goals of the project are:

    1. Prepeare the dataset.
    2. Do Exploratory Data Analysis(EDA) 
    3. Test-Train Split and Encoding.
    4. Implement two ML methods: Lasso and GAM.
    5. Do Hyperparameter tuning. 
    6. Result Analysis.


### Results of the model:

| Model Name                                 | Train RMSE | Test RMSE |
|--------------------------------------------|------------|-----------|
| Basic Lasso Model                          | 8.234974   | 8.1422    |
| Tuned Lasso Model (Lambda = 0.140666)      | 8.22499    | 8.131431  |
| Default GAM using spline smoothing K = 5   | 8.157889   | 8.106507  |
| Tuned GAM (K = 11)                         | 8.152894   | 8.129277  |


### Conclusion:
In conclusion, our NYC Bus Delay Time Prediction project applied machine learning techniques to develop models capable of forecasting bus delays. We implement two predictive models: Lasso regression and Generalized Additive Models (GAM). Through hyperparameter tuning, we refined our Lasso and Generalized Additive Models (GAM). The GAM, particularly with K set to 5 for spline smoothing, yielded the best performance on the test data. Notably, while a more complex GAM with K = 11 achieved the lowest training RMSE, it did not translate to the lowest test RMSE, highlighting the importance of model generalization overfitting to the training set.


### Project Report: 
The full report of this project is stored in this [link](https://github.com/AhmedDiderRahat/crypto_price_prediction/blob/main/documentation/report.pdf)
