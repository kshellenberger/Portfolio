# Healthcare Analytics Portfolio

A comprehensive collection of data science, statistical analysis, and business intelligence projects demonstrating expertise across Python, R, SQL, Power BI, and Excel.

---

## Table of Contents

1. [Machine Learning](#machine-learning)
2. [Statistical Analysis](#statistical-analysis)
3. [Time Series Analysis](#time-series-analysis)
4. [Data Engineering](#data-engineering)
5. [Business Intelligence](#business-intelligence)
6. [Project Management](#project-management)

---

## Machine Learning

### **Sentiment_Analysis_RNN.ipynb**
**Objective:** Predict sentiment of customer reviews using recurrent neural networks without overfitting.

- **Model:** Long Short-Term Memory (LSTM) with Adam optimizer
- **Languages/Tools:** Python, Jupyter Notebook, TensorFlow, Keras, NLTK
- **Dataset:** Combined Amazon, IMDB, and Yelp reviews (sentiment classification)
- **Key Techniques:** Text preprocessing, tokenization, sequence padding, NLP, regularization
- **Deliverables:** Trained LSTM model, sentiment predictions, performance metrics

---

### **Medical_Complication_Prediction_TreeBased.ipynb**
**Objective:** Compare Random Forest and XGBoost models for predicting patient complications with comprehensive model evaluation.

- **Models:** Random Forest and XGBoost Ensemble Methods
- **Languages/Tools:** Python, Jupyter Notebook, Scikit-learn, XGBoost, Pandas, Matplotlib
- **Dataset:** Imbalanced medical data (15% complications, 85% normal - realistic healthcare scenario)
- **Key Techniques:** 
  - Hyperparameter tuning with GridSearchCV
  - Class imbalance handling (scale_pos_weight, class_weight)
  - 5-fold stratified cross-validation
  - Feature importance analysis
  - ROC curve and confusion matrix analysis
  - Multiple evaluation metrics (Accuracy, Precision, Recall, F1-Score, ROC-AUC)
- **Key Findings:** XGBoost achieved superior ROC-AUC; both models show excellent recall for identifying complications
- **Deliverables:** Tuned models, comparative performance analysis, feature importance rankings, clinical recommendations

---

### **Medical_Feature_Engineering_Analysis.ipynb**
**Objective:** Demonstrate advanced feature engineering techniques and their impact on model performance.

- **Techniques:** Domain-driven feature creation, polynomial features, categorical binning, statistical feature selection, RFE, PCA
- **Languages/Tools:** Python, Jupyter Notebook, Scikit-learn, Pandas, Seaborn
- **Dataset:** Synthetic medical data with 15 clinical measurements
- **Engineering Methods:**
  - Cardiovascular indicators (Mean Arterial Pressure, Pulse Pressure)
  - Clinical categories (BP Stage, BMI Category, Glucose Status, Age Groups)
  - Health risk aggregation
  - Polynomial feature transformation
  - Cholesterol ratios and derived metrics
- **Feature Selection:**
  - F-Statistic selection
  - Mutual Information analysis
  - Recursive Feature Elimination (RFE)
  - Principal Component Analysis (PCA)
- **Key Findings:** 
  - 33% improvement in R² with engineered features vs. raw features
  - 10-feature model achieves 99% of full model performance with 60% fewer features
  - Domain knowledge features outperform automated polynomial features
  - PCA achieves 90% variance with [n] components (from 31 engineered features)
- **Deliverables:** Feature engineering pipeline, selection comparison, dimensionality reduction analysis, performance improvements

---

### **Wine_Classification_KMeans_Clustering.ipynb**
**Objective:** Cluster wine samples using KMeans after dimensionality reduction.

- **Model:** K-Means clustering with PCA preprocessing
- **Languages/Tools:** Python, Jupyter Notebook, Scikit-learn, Pandas, Seaborn
- **Dataset:** Wine dataset with 13 chemical features
- **Key Techniques:** PCA, standardization, silhouette analysis, cluster visualization
- **Deliverables:** Optimal cluster identification, 2D visualization, variance explained

---

## Statistical Analysis

### **Medical_ChiSquare_Analysis.R**
**Objective:** Test association between marital status and complication risk in medical data.

- **Test:** Chi-Square test of independence
- **Languages/Tools:** R, RStudio, ggplot2, Plotly
- **Dataset:** Medical records dataset
- **Key Findings:** Statistical relationship analysis, contingency tables, visualizations
- **Deliverables:** Test results, p-values, proportional bar charts

---

### **Anxiety_Prediction_LogisticRegression.R**
**Objective:** Predict patient anxiety using survey response items as predictors.

- **Model:** Binary logistic regression
- **Languages/Tools:** R, RStudio, Caret, Mosaic, ggplot2, Plotly
- **Dependent Variable:** Anxiety (Yes/No)
- **Key Techniques:** Binary classification, univariate analysis, mosaic plots, model diagnostics
- **Deliverables:** Logistic regression coefficients, classification metrics, visualizations

---

### **Income_Prediction_LinearRegression.R**
**Objective:** Predict patient income based on survey response items.

- **Model:** Multiple linear regression
- **Languages/Tools:** R, RStudio, Plotly, ggplot2, Tidyverse
- **Independent Variables:** 8 survey items
- **Key Techniques:** Multivariate regression, correlation analysis, residual diagnostics
- **Deliverables:** Regression coefficients, R-squared, residual plots, predictions

---

### **Medical_PCA_Analysis.R**
**Objective:** Reduce dimensionality of medical dataset to identify key patterns.

- **Technique:** Principal Component Analysis (PCA)
- **Languages/Tools:** R, RStudio, Boxplot analysis
- **Dataset:** Medical records with multiple numerical features
- **Key Findings:** Outlier identification, variance distribution, dimensionality reduction
- **Deliverables:** Boxplots for outlier detection, PCA components

---

## Time Series Analysis

### **Medical_Revenue_TimeSeries_ARIMA.R**
**Objective:** Forecast medical service revenue using ARIMA modeling.

- **Model:** AutoRegressive Integrated Moving Average (ARIMA)
- **Languages/Tools:** R, RStudio, ggplot2, Forecast, TSeries packages
- **Dataset:** Daily revenue time series (Medical records)
- **Key Techniques:** Stationarity testing (ADF), differencing, ACF/PACF, train-test split
- **Deliverables:** ARIMA model parameters, forecasts, test data validation

---

## Data Engineering

### **Medical_Services_ETL.sql**
**Objective:** Prepare and transform medical services data for analysis.

- **Operations:** Data cleaning, table creation, nested joins, ETL workflow
- **Languages/Tools:** SQL, VSCode, Database design
- **Dataset:** Medical services records with multiple health condition flags
- **Key Techniques:** Data type specification, foreign key relationships, COPY operations, incremental updates
- **Deliverables:** Clean tables, historical tracking, data quality checks

---

## Business Intelligence

### **Power_BI_Portfolio_Dashboard_KS.pdf**
**Objective:** Visualize logistic regression outcomes and supporting analytics.

- **Tool:** Power BI
- **Deliverables:** Interactive dashboard, risk metrics, drill-down capabilities
- **Applicable Use Case:** Risk department operational reporting and management dashboards

---

### **Excel_Medical_Economics_Trend_Analysis_Simulation_KS.xlsx**
**Objective:** Simulate medical economics analysis and trends across calendar year.

- **Tool:** Excel
- **Key Content:** Health economics metrics, calculations, trend analysis, financial projections
- **Applicable Use Case:** Financial planning and economic forecasting for healthcare organizations

---

## Project Management

### **CLARA_Product_Management.pptx**
**Objective:** Present innovative healthcare solution to competition judges.

- **Achievement:** 1st Place Winners - Hackathon
- **Role:** Project Manager
- **Responsibilities:**
  - Ideation and problem definition
  - Project scoping and requirements
  - Project plan development
  - Resource tracking and team coordination

---

## Technical Skills Summary

- **Languages:** Python, R, SQL
- **Data Science:** Machine Learning, Statistical Analysis, Time Series Forecasting, Advanced Feature Engineering
- **Python Libraries:** 
  - Deep Learning: TensorFlow, Keras, NLTK
  - Ensemble Methods: Scikit-learn, XGBoost
  - Feature Engineering: Scikit-learn, Pandas
  - Visualization: Matplotlib, Seaborn
  - Data Manipulation: Pandas, NumPy
- **R Packages:** Caret, ggplot2, Plotly, Tidyverse, Forecast, TSeries, Mosaic
- **Machine Learning Techniques:**
  - Supervised Learning: Linear/Logistic Regression, Decision Trees, Random Forest, XGBoost
  - Unsupervised Learning: K-Means, Hierarchical Clustering, PCA
  - Deep Learning: RNN, LSTM, Neural Networks
  - Feature Engineering: Domain-driven creation, polynomial features, categorical encoding
  - Feature Selection: F-Statistic, Mutual Information, RFE
  - Model Evaluation: Cross-validation, Grid Search, Hyperparameter Tuning, ROC Analysis
- **Databases:** SQL, Database Design, ETL
- **Business Intelligence:** Power BI, Excel, Tableau
- **Tools:** RStudio, Jupyter Notebook, VS Code, Git 
