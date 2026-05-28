# Loan Approval Prediction: A Predictive Modeling Approach in R

This repository contains a comprehensive statistical and predictive modeling project focused on automating eligibility screening for loan applicants. 

By evaluating demographic variables, financial histories, and credit properties, the project implements a robust machine learning pipeline in R to predict **Loan Approval Status** and optimize credit risk management.

This project was developed for the *Statistical Learning* course within the Master’s degree program in *Analytics and Data Science for Economics and Management* (A.Y. 2024/2025).

## Authors
* Isabella Cappiello
* Martina Pagan

---

## Project Architecture & Files
* **`SLproject.R`**: The complete production-ready R script detailing the entire workflow: exploratory data analysis (EDA), data cleaning, missing value imputation, resampling, variable selection, model training, and performance validation.
* **`Poster.pptx`**: Academic presentation poster summarizing the empirical approach, research insights, model comparisons, and predictive metrics.

---

## Tech Stack & Key R Packages
The analysis was engineered entirely in **R**, utilizing specialized statistical learning libraries:
* **Data Manipulation & Pre-processing:** `dplyr`, `readxl`, `reshape2`
* **Feature Selection & Regularization:** `leaps` (Best Subset Selection), `glmnet` (Ridge and Lasso Regression), `BeSS`
* **Classification & Machine Learning:** `caret` (model training and resampling tuning), `MASS` (LDA/QDA), `e1071` (Support Vector Machines - SVM)
* **Model Validation & Imbalance Handling:** `ROCR` (ROC curves and AUC evaluation), `ROSE` (Random Over-Sampling Examples)
* **Data Visualization:** `ggplot2`, `ggrepel`, `kableExtra`

---

## Methodology & Analytical Pipeline

### 1. Data Cleaning & Advanced Imputation
* **Missing Data Detection:** Identified and systematically managed 75 missing values across variables such as `Self_Employed`, `ApplicantIncome`, and loan parameters.
* **Imputation Strategy:** Missing entry substitutions were computed using conditional statistical metrics (e.g., matching income thresholds and subgroup distributions) rather than generic global averages.

### 2. Feature Engineering & Selection
* Logarithmic transformations were evaluated to stabilize variance and normalize heavily skewed distributions (e.g., applicant and co-applicant incomes).
* Implemented feature selection techniques including **Best Subset Selection** and **Lasso/Ridge Regularization** to reduce multicollinearity, guarantee model parsimony, and select optimal predictors (e.g., `Credit_History`).

### 3. Resolving Target Class Imbalance
To mitigate classification bias stemming from skewed loan approval records, multiple class-balancing techniques were integrated and benchmarked:
* **ROSE** (Random Over-Sampling Examples) synthetic generation
* **Down-sampling** and **Up-sampling** optimization routines via `caret`

### 4. Classification Models & Comparative Framework
The predictive pipeline evaluates and compares several statistical learning classification algorithms:
* **Logistic Regression:** Used as the baseline probabilistic interpreter.
* **Linear Discriminant Analysis (LDA) & Quadratic Discriminant Analysis (QDA):** To evaluate multivariate normality boundaries.
* **Support Vector Machines (SVM):** Implemented with tailored kernels to handle potential non-linear decision boundaries.

---

## Key Performance Metrics
Models were rigorously cross-validated and assessed based on **Sensitivity (Recall)**, **Specificity**, **Overall Accuracy**, and **Area Under the ROC Curve (AUC)**. The integration of `Credit_History` stood out as the single most critical structural indicator for predictive performance across all classification algorithms.
