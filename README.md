# Healthcare Cost Prediction - R Analytics Project

A comprehensive machine learning analysis to predict healthcare insurance costs using R, featuring advanced feature engineering, external benchmarking, and business insights.

## 🎯 Project Overview

This project develops predictive models for healthcare insurance costs using the Kaggle Medical Cost Personal Dataset, enhanced with external HCUP (Healthcare Cost and Utilization Project) benchmarking data. The analysis follows a systematic approach from data exploration through advanced feature engineering to model deployment.

### Key Features

- **External Data Integration**: HCUP hospital cost benchmarks for validation
- **Evidence-Based Feature Engineering**: ANOVA-guided feature selection
- **Advanced Statistical Analysis**: Comprehensive hypothesis testing
- **Business-Focused Insights**: ROI analysis and actionable recommendations
- **Tableau Integration**: Interactive dashboard for stakeholder presentation

## 📊 Dataset Information

### Primary Dataset
- **Source**: Kaggle Medical Cost Personal Dataset
- **Size**: 1,338 observations, 7 variables
- **Target**: Insurance charges (continuous)
- **Features**: Age, sex, BMI, children, smoker status, region

### External Benchmarking Data
- **Source**: HCUP (Healthcare Cost and Utilization Project)
- **Purpose**: National hospital cost benchmarks by age group
- **Integration**: Cost ratio features and validation metrics

## 🔬 Methodology

This project follows the CRISP-DM methodology adapted for healthcare analytics:

1. **Data Understanding & Exploration**
2. **Data Cleaning & Preparation**
3. **Feature Engineering & Selection**
4. **Model Development & Training**
5. **Model Evaluation & Validation**
6. **Business Insights & Deployment**

## 📁 Project Structure

```
healthcare-cost-prediction/
├── scripts/
│   ├── 00_housekeeping.R                   # Project organization and maintenance
│   ├── 01_data_exploration_cleaning.R      # Combined EDA and data cleaning
│   ├── 02_feature_engineering.R            # ANOVA-guided feature creation
│   ├── 03_modeling.R                       # Model training and tuning
│   ├── 04_model_evaluation.R               # Performance comparison
│   ├── 05_final_analysis.R                 # Business insights and reporting
│   └── utils.R                             # Helper functions
├── data/
│   ├── raw/                               # Original datasets
│   ├── processed/                         # Cleaned and engineered data
│   ├── external/                          # HCUP benchmarking data
│   └── data_dictionary.csv               # Variable documentation
├── outputs/
│   ├── tables/                            # Analysis results and summaries
│   ├── plots/                             # Visualizations and charts
│   └── models/                            # Trained model objects
├── tableau/
│   ├── data_extracts/                     # Data for dashboard
│   └── dashboards/                        # Tableau workbooks
├── docs/
│   ├── methodology.md                     # Detailed methodology
│   ├── results_summary.md                 # Executive summary
│   └── workflow_summary.csv               # Project progress tracking
└── README.md
```

## 🚀 Getting Started

### Prerequisites

```r
# Required R packages
install.packages(c(
  "tidyverse",      # Data manipulation and visualization
  "corrplot",       # Correlation analysis
  "VIM",           # Missing data visualization
  "skimr",         # Data summaries
  "caret",         # Machine learning
  "randomForest",  # Random forest modeling
  "xgboost",       # Gradient boosting
  "fastDummies",   # One-hot encoding
  "here"           # Path management
))
```

### Quick Start

```r
# 1. Clone the repository
git clone https:[//github.com/yourusername/healthcare-cost-prediction.git](https://github.com/S-Weatherby/Healthcare_Cost_Prediction_Model.git)]

# 2. Set working directory
setwd("healthcare-cost-prediction")

# 3. Run the analysis pipeline
source("scripts/00_housekeeping.R")
source("scripts/01_data_exploration_cleaning.R")
source("scripts/02_feature_engineering.R")
source("scripts/03_modeling.R")
source("scripts/04_model_evaluation.R")
source("scripts/05_final_analysis.R")
```

## 📈 Key Results & Insights

### Model Performance
- **Best Algorithm**: XGBoost (pending completion)
- **R-squared**: TBD (estimated >0.75)
- **RMSE**: TBD
- **Key Predictors**: Smoking status, age, BMI interactions

### Statistical Findings (ANOVA Analysis)

| Variable | F-Value | P-Value | Significance | Business Impact |
|----------|---------|---------|--------------|-----------------|
| Smoker | 587.46 | < 0.001 | *** | Very High |
| Age Group | 64.31 | < 0.001 | *** | High |
| BMI Category | 30.37 | < 0.001 | *** | High |
| Region | 3.58 | 0.013 | * | Medium |
| Sex | 1.47 | 0.225 | ns | Low |
| Children | 0.067 | 0.796 | ns | Low |

### Business Insights
- **Top Cost Driver**: Smoking status (400%+ cost increase)
- **Key Interactions**: Smoker × Age, Smoker × BMI
- **Benchmark Comparison**: Individual vs. national hospital averages
- **ROI Opportunity**: Early intervention programs for high-risk patients

## 🔧 Advanced Features

### Evidence-Based Feature Engineering
- **ANOVA-Guided Selection**: Statistical significance testing drives feature creation
- **Non-Linear Transformations**: Age², BMI² for capturing acceleration effects
- **Interaction Terms**: Smoker-age, smoker-BMI multiplicative effects
- **External Benchmarks**: Cost ratio vs. national averages
- **Risk Scoring**: Multi-factor risk categorization

### Key Engineered Features

```r
# Sample of high-priority engineered features
- cost_vs_national_ratio      # Individual vs. national benchmark
- smoker_age_interaction      # Multiplicative risk effect
- smoker_bmi_interaction      # Combined lifestyle impact
- age_squared                 # Non-linear age effects
- bmi_squared                 # Non-linear BMI effects
- high_risk_score            # Composite risk indicator
- cost_efficiency_index      # Value-based care metric
```

## 🔄 Project Workflow

### Current Status

| Script | Purpose | Status | Est. Time |
|--------|---------|--------|-----------|
| 00_housekeeping.R | Project organization | 🔄 Ongoing | Ongoing |
| 01_data_exploration_cleaning.R | Data loading and cleaning | ✅ Complete | 3-4 days |
| 02_feature_engineering.R | Feature creation and selection | 🔄 70% Complete | 2-3 days |
| 03_modeling.R | Model training and tuning | ⏳ Not Started | 3-4 days |
| 04_model_evaluation.R | Performance comparison | ⏳ Not Started | 2-3 days |
| 05_final_analysis.R | Business insights | ⏳ Not Started | 2-3 days |
| utils.R | Helper functions | 🔄 Ongoing | Ongoing |

### Next Steps Priority

1. **Complete Script 02**: Finish ANOVA analysis and feature engineering
2. **Begin Script 03**: Implement model training pipeline
3. **External Data**: Integrate HCUP benchmarking data
4. **Tableau Prep**: Create data extracts for dashboard
5. **Documentation**: Update methodology and results

## 📊 Tableau Dashboard

Interactive dashboard features:
- **Cost Prediction Tool**: Real-time predictions based on user inputs
- **Risk Factor Analysis**: Visual breakdown of cost drivers
- **Benchmark Comparisons**: Individual vs. population averages
- **ROI Calculator**: Intervention program cost-benefit analysis

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/new-analysis`)
3. Commit your changes (`git commit -am 'Add new analysis'`)
4. Push to the branch (`git push origin feature/new-analysis`)
5. Create a Pull Request

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- **Data Source**: Kaggle Medical Cost Personal Dataset
- **Benchmarking Data**: HCUP (Healthcare Cost and Utilization Project)
- **Statistical Methods**: Evidence-based feature engineering approach
- **Business Context**: Healthcare cost prediction and risk assessment

## 📧 Contact

For questions or collaboration opportunities, please reach out via:
- **Email**: shelita17smith@gmail.com
- **LinkedIn**: [LinkedIn Profile]([https://linkedin.com/in/yourprofile](https://www.linkedin.com/in/shelita-smith-b4092753/))

---

*This project demonstrates advanced R analytics capabilities for healthcare cost prediction, featuring statistical rigor, business insights, and practical deployment considerations.*
