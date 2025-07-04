<<<<<<< HEAD
=======
# Healthcare Cost Prediction Model

> **Predicting individual healthcare costs using machine learning to enable proactive care management and cost optimization**

[![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://www.r-project.org/)
[![RStudio](https://img.shields.io/badge/RStudio-75AADB?style=for-the-badge&logo=rstudio&logoColor=white)](https://rstudio.com/)
[![Tableau](https://img.shields.io/badge/Tableau-E97627?style=for-the-badge&logo=tableau&logoColor=white)](https://www.tableau.com/)

---

## 📊 Project Overview

This project develops machine learning models to predict individual healthcare costs using demographic, health, and behavioral factors. The analysis provides actionable insights for healthcare providers, insurance companies, and policymakers to optimize resource allocation and improve cost management strategies.

### 🎯 Business Impact
- **Predict healthcare costs** with high accuracy for budget planning and risk assessment
- **Identify high-cost patients** early for proactive care management interventions  
- **Quantify cost drivers** to guide policy decisions and preventive care programs
- **Enable data-driven pricing** strategies for insurance and healthcare providers

---

## 📈 Key Findings

*[Results will be updated upon completion of analysis]*

| Metric | Value | Business Impact |
|--------|-------|----------------|
| **Model Accuracy** | *[TBD]* | Reliable cost predictions for planning |
| **Top Cost Driver** | *[TBD]* | Primary factor for intervention targeting |
| **Prediction Error** | *[TBD]* | Average prediction accuracy |
| **High-Cost Detection** | *[TBD]* | Early identification capability |

### 💡 Strategic Insights
*[Key business insights will be documented here upon analysis completion]*
- Cost reduction opportunities through targeted interventions
- Regional variations and care program optimization potential
- Risk stratification capabilities for proactive care management
- ROI projections for preventive care investments

---

## 🛠️ Technical Implementation

### Machine Learning Pipeline
```
Raw Data → Feature Engineering → Model Training → Validation → Deployment
    ↓              ↓                   ↓             ↓           ↓
Insurance      Demographic         Multiple      Cross-    Interactive
Dataset     +  Health Factors   +  ML Models  + Validation  Dashboard
            +  Risk Categories     (XGBoost,   + Testing
                                  RF, Linear)
```

### Model Development Approach
| Stage | Algorithms Planned | Evaluation Metrics | Expected Outcome |
|-------|-------------------|-------------------|------------------|
| **Baseline** | Linear Regression, Random Forest | R², RMSE, MAE | Initial benchmarks |
| **Advanced** | XGBoost, Ensemble Methods | Cross-validation scores | Optimized performance |
| **Selection** | Best performing model | Business metrics | Production-ready model |

---

## 📁 Project Structure

```
healthcare-cost-prediction/
├── 📊 data/
│   ├── raw/                    # Original datasets
│   ├── processed/              # Cleaned data
│   └── data_dictionary.md      # Variable documentation
├── 📝 scripts/
│   ├── 01_data_exploration.R   # EDA and data understanding
│   ├── 02_data_cleaning.R      # Data preprocessing
│   ├── 03_feature_engineering.R # Feature creation
│   ├── 04_modeling.R           # Model development
│   ├── 05_model_evaluation.R   # Performance assessment
│   ├── 06_final_analysis.R     # Business insights
│   └── utils.R                 # Helper functions
├── 🤖 models/                  # Trained model objects
├── 📈 outputs/
│   ├── plots/                  # Visualizations
│   ├── tables/                 # Summary statistics
│   └── reports/                # Analysis documents
├── 📋 tableau/                 # Interactive dashboards
└── 📚 documentation/           # Project documentation
```

---

## 🚀 Getting Started

### Prerequisites
- **R** (≥ 4.0.0) and **RStudio**
- **Tableau Desktop** (for dashboard visualization)
- Required R packages (see installation below)

### Installation & Setup

1. **Clone the repository**
   ```bash
   git clone https://github.com/[YOUR-USERNAME]/healthcare-cost-prediction.git
   cd healthcare-cost-prediction
   ```

2. **Open in RStudio**
   ```r
   # Open the .Rproj file in RStudio
   # Install required packages
   install.packages(c("tidyverse", "caret", "randomForest", "xgboost", 
                      "corrplot", "VIM", "readxl", "here", "rmarkdown"))
   ```

3. **Run the analysis**
   ```r
   # Execute scripts in order
   source("scripts/01_data_exploration.R")
   source("scripts/02_data_cleaning.R")
   source("scripts/03_feature_engineering.R")
   source("scripts/04_modeling.R")
   source("scripts/05_model_evaluation.R")
   source("scripts/06_final_analysis.R")
   ```

4. **View dashboards**
   - Open `tableau/healthcare_cost_dashboard.twbx` in Tableau Desktop
   - Explore interactive visualizations and cost predictions

---

## 📊 Data Sources

### Primary Dataset
- **Medical Insurance Cost Dataset** (Kaggle)
  - 1,338 individual insurance records
  - Features: Age, BMI, smoking status, region, family size
  - Target: Annual healthcare charges

### Additional Data Sources (Planned)
- **HCUP Public Data**: National hospital cost benchmarks
- **CMS Medicare Data**: Regional healthcare spending patterns

### Features Used
| Feature | Type | Description | Business Relevance |
|---------|------|-------------|-------------------|
| `age` | Numerical | Patient age (18-64) | Age-related health risks |
| `bmi` | Numerical | Body Mass Index | Obesity-related costs |
| `smoker` | Categorical | Smoking status | Major cost driver |
| `region` | Categorical | Geographic region | Regional cost variations |
| `children` | Numerical | Number of dependents | Family size impact |
| `sex` | Categorical | Gender | Gender-specific health patterns |

---

## 🔍 Methodology

### Feature Engineering
- **Age Groups**: Categorical age bands for risk stratification
- **BMI Categories**: WHO standard weight classifications  
- **Risk Combinations**: Interaction features (smoking + obesity)
- **Family Complexity**: Derived family size and dependency metrics

### Model Development Plan
1. **Data Splitting**: 60% train / 20% validation / 20% test
2. **Cross-Validation**: 5-fold CV for model selection
3. **Hyperparameter Tuning**: Grid search optimization
4. **Ensemble Methods**: Multiple algorithm combination
5. **Feature Selection**: Recursive feature elimination

### Evaluation Metrics
- **R² Score**: Proportion of variance explained
- **RMSE**: Root Mean Square Error in dollars
- **MAE**: Mean Absolute Error for interpretability  
- **MAPE**: Mean Absolute Percentage Error for business context

---

## 📈 Results & Visualizations

*[Visualizations will be added upon completion of analysis]*

### Planned Outputs
- Model performance comparison charts
- Feature importance analysis
- Cost distribution analysis by demographics
- Interactive Tableau dashboard for cost prediction

### Dashboard Features (In Development)
- Interactive cost calculator
- Geographic cost variation maps
- Risk factor impact visualization
- Model performance metrics

---

## 💼 Business Applications

### For Healthcare Providers
- **Risk Stratification**: Identify patients likely to incur high costs
- **Resource Planning**: Predict capacity needs and staffing requirements
- **Preventive Care**: Target interventions for maximum cost reduction
- **Care Coordination**: Optimize care teams for high-risk patients

### For Insurance Companies  
- **Premium Pricing**: Data-driven actuarial modeling
- **Underwriting**: Enhanced risk assessment capabilities
- **Claims Prediction**: Forecast reserve requirements
- **Member Engagement**: Targeted wellness programs

### For Policymakers
- **Healthcare Planning**: Population health cost projections
- **Program Evaluation**: ROI analysis for public health initiatives  
- **Resource Allocation**: Evidence-based budget planning
- **Health Equity**: Identify cost disparities across populations

---

## 🚧 Current Status & Next Steps

### ✅ Completed
- [x] Project structure setup
- [x] Data source identification
- [x] Methodology planning
- [x] R environment configuration

### 🔄 In Progress
- [ ] Exploratory data analysis
- [ ] Data cleaning and preprocessing
- [ ] Feature engineering
- [ ] Model development and training
- [ ] Performance evaluation
- [ ] Business insights generation
- [ ] Tableau dashboard creation

### 🔮 Future Enhancements
- [ ] **Integrate Clinical Data**: Add diagnosis codes and treatment history
- [ ] **Time Series Modeling**: Predict cost trends over multiple years
- [ ] **Geographic Analysis**: Incorporate detailed location-based factors
- [ ] **Real-time Predictions**: Deploy model as web application
- [ ] **Advanced Algorithms**: Experiment with deep learning approaches

---

## 📚 Technical Documentation

### Key Files (To Be Created)
- **[Methodology Report](documentation/methodology.md)**: Detailed technical approach
- **[Data Dictionary](data/data_dictionary.md)**: Complete variable definitions  
- **[Business Case](outputs/reports/business_case.pdf)**: Executive summary and ROI analysis
- **[Model Documentation](documentation/model_specs.md)**: Algorithm specifications and parameters

### Code Quality Standards
- **Reproducible Research**: All analysis documented and version controlled
- **Modular Design**: Functions separated for reusability
- **Error Handling**: Robust data validation and error checking
- **Documentation**: Comprehensive inline comments and README files

---

## 🤝 Contributing

Interested in improving this project? Contributions are welcome!

1. **Fork** the repository
2. **Create** a feature branch (`git checkout -b feature/improvement`)
3. **Commit** your changes (`git commit -am 'Add new feature'`)
4. **Push** to the branch (`git push origin feature/improvement`)
5. **Create** a Pull Request

---

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

## 👤 Author

**[Shelita Smith]**
- **Email**: [shelita17smith@gmail.com]  
- **LinkedIn**: [(https://www.linkedin.com/in/shelita-smith-b4092753/)]

### About the Author
MPH in Health Policy & Organization with 3+ years experience in healthcare analytics and health disparities research.
---

## 🙏 Acknowledgments

- **Kaggle Community** for providing the medical insurance dataset
- **AHRQ HCUP** for healthcare cost research methodologies  
- **R Community** for excellent machine learning packages
- **Tableau** for powerful data visualization capabilities

---

## 📊 Project Stats

*[GitHub stats will be populated once repository is created]*

![GitHub repo size](https://img.shields.io/github/repo-size/[YOUR-USERNAME]/healthcare-cost-prediction)
![GitHub last commit](https://img.shields.io/github/last-commit/[YOUR-USERNAME]/healthcare-cost-prediction)
![GitHub languages](https://img.shields.io/github/languages/count/[YOUR-USERNAME]/healthcare-cost-prediction)
![GitHub top language](https://img.shields.io/github/languages/top/[YOUR-USERNAME]/healthcare-cost-prediction)

---

⭐ **Star this repository** if you found it helpful!

📧 **Questions?** Feel free to reach out or open an issue.

---

*Last updated: [DATE] | Project Status: In Development*
>>>>>>> 004f82de52e60283b35e4fbdf75d9ead211647f2
