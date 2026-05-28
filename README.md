# Loan Approval Prediction: A Predictive Modeling Approach in R

This repository contains a comprehensive statistical learning and predictive modeling project focused on automating eligibility screening for loan applicants. 

By evaluating demographic variables, financial histories, and credit properties, the project implements a robust machine learning pipeline in R to predict **Loan Approval Status** (`Y` / `N`) and optimize credit risk management.

This project was developed for the *Statistical Learning* course within the Master’s degree program in *Analytics and Data Science for Economics and Management* at the University of Brescia (A.Y. 2024/2025).

## Authors
* Isabella Cappiello
* Martina Pagan

---

## Project Architecture & Files
* **`SLproject.R`**: The complete production-ready R script detailing the entire workflow: exploratory data analysis (EDA), conditional missing data imputation, outlier detection, data balancing, feature selection, model training, and predictive performance validation.
* **`Poster.pptx`**: Academic presentation poster summarizing the empirical approach, research insights, model comparisons, and predictive metrics.

---

## Tech Stack & Key R Packages
The analysis was engineered entirely in **R**, utilizing specialized statistical learning and validation libraries:
* **Data Manipulation & Pre-processing:** `dplyr`, `readxl`, `reshape2`
* **Feature Selection & Regularization:** `leaps` (Best Subset Selection), `glmnet` (Ridge and Lasso Regression), `BeSS` (Best Subset Selection in High Dimensions)
* **Classification & Machine Learning:** `caret` (automated model training and hyperparameter tuning), `MASS` (LDA/QDA), `e1071` (Support Vector Machines - SVM)
* **Model Validation & Imbalance Handling:** `ROCR` (ROC curves and AUC evaluation), `ROSE` (Random Over-Sampling Examples), `biotools`
* **Data Visualization:** `ggplot2`, `ggrepel`, `kableExtra`

---

## Methodology & Analytical Pipeline

### 1. Data Cleaning & Advanced Imputation
* **Missing Data Detection:** Identified and systematically managed 75 missing values across critical predictors such as `Self_Employed`, `ApplicantIncome`, and specific loan parameters.
* **Conditional Imputation Strategy:** Missing entry substitutions were computed using conditional statistical metrics (e.g., matching income thresholds and subgroup distributions within specific categories) rather than generic global averages, preserving the underlying variance.

### 2. Feature Engineering & Selection
* Logarithmic transformations were evaluated and applied to stabilize variance and normalize heavily skewed financial distributions (e.g., applicant and co-applicant incomes).
* Implemented feature selection techniques including **Best Subset Selection**, **Lasso**, and **Ridge Regularization** to reduce multicollinearity, guarantee model parsimony, and isolate the optimal subset of predictors.

### 3. Resolving Target Class Imbalance
To mitigate classification bias stemming from skewed loan approval records (where approved loans naturally outweigh rejections), multiple class-balancing techniques were integrated and benchmarked:
* **ROSE** (Random Over-Sampling Examples) synthetic data generation.
* **Down-sampling** and **Up-sampling** optimization routines via the `caret` framework.

### 4. Classification Models & Comparative Framework
The predictive pipeline evaluates and compares several statistical learning binary classification algorithms to identify the most robust decision boundary:
* **Logistic Regression:** Implemented as the baseline probabilistic classifier.
* **Linear Discriminant Analysis (LDA) & Quadratic Discriminant Analysis (QDA):** Leveraged to evaluate multivariate normality boundaries and group separations.
* **Support Vector Machines (SVM):** Tuned with tailored kernels to handle potential non-linear decision boundaries in the feature space.

---

## Key Performance Metrics & Insights
Models were rigorously cross-validated and assessed based on a comprehensive confusion matrix framework evaluating **Sensitivity (Recall)**, **Specificity**, **Overall Accuracy**, and **Area Under the ROC Curve (AUC)**. 

Across all classification algorithms, the applicant's **`Credit_History`** stood out as the single most critical structural indicator for predicting loan approval status.
