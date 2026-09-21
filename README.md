# Assessing the Impact of Socioeconomic Variables on Life Expectancy

## 📌 Overview

This project explores the relationship between life expectancy and socioeconomic, health, education, mortality, and economic indicators across countries from 2000 to 2015.

An interactive dashboard was developed using **R Shiny and Plotly** to allow users to explore country-level patterns, compare developed and developing countries, analyze trends over time, and investigate relationships between key indicators and life expectancy.

The project focuses on understanding how factors such as **schooling, income, BMI, adult mortality, and HIV/AIDS** are associated with life expectancy.

---

## 🎯 Objectives

The project aims to:

- Explore global and country-level life expectancy patterns
- Compare health and socioeconomic indicators between developed and developing countries
- Analyze trends in mortality, schooling, BMI, income, GDP, and disease prevalence
- Investigate relationships between socioeconomic/health indicators and life expectancy
- Build an interactive analytical dashboard for data exploration
- Communicate findings through interactive visualizations and data storytelling

---

## 🛠️ Technologies

- **R**
- **R Shiny**
- **Shinydashboard**
- **Plotly**
- **ggplot2**
- **dplyr**
- **tidyr**
- **DT**
- **Maps**
- **Linear Regression**
- **Correlation Analysis**

---

## 📊 Dataset

The analysis uses the **Life Expectancy dataset**, containing country-level observations from **2000–2015**.

The dataset includes indicators related to:

- Life Expectancy
- Adult Mortality
- Infant Deaths
- Under-five Deaths
- Hepatitis B
- Measles
- Polio
- Diphtheria
- HIV/AIDS
- BMI
- Schooling
- Income Composition of Resources
- GDP
- Development Status

---

## 🔄 Data Preprocessing

The dataset was prepared before analysis by:

- Identifying numerical variables
- Handling missing numerical values using **median imputation**
- Standardizing column names
- Converting development status into a categorical variable

This preprocessing created a consistent dataset for subsequent exploratory and regression analysis. 

---

## 🔍 Exploratory Data Analysis

The dashboard provides interactive exploration of multiple dimensions of the dataset.

### Development Status

Users can compare indicators between:

- Developed countries
- Developing countries

Individual countries can also be selected for more detailed analysis.

### Health & Mortality

The dashboard explores:

- Adult mortality
- Infant deaths
- Under-five deaths
- Disease trends including HIV/AIDS, Hepatitis B, Measles, Polio, and Diphtheria

### Socioeconomic Indicators

The analysis investigates trends in:

- Schooling
- Income composition of resources
- GDP

### BMI

BMI values are categorized into:

- Underweight
- Normal Weight
- Overweight
- Obesity

Users can compare BMI distributions across selected countries.

---

## 🌍 Geographic Analysis

An interactive world map was developed to visualize **average life expectancy across countries**.

The dashboard also provides:

- Global life expectancy trends
- Indonesia's life expectancy trend
- Top and bottom countries based on average life expectancy
- Country-level trend comparisons

These visualizations allow users to identify geographic disparities in life expectancy.

---

## 📈 Correlation & Regression Analysis

A correlation matrix was implemented to examine relationships between numerical health and socioeconomic indicators. 

Five linear regression analyses were developed to investigate the relationship between **Life Expectancy** and:

1. Adult Mortality
2. Schooling
3. Income Composition of Resources
4. HIV/AIDS
5. BMI

Each model provides an interactive scatter plot with:

- Observed values
- Regression line
- Correlation coefficient
- Interactive tooltips

The dashboard also allows users to dynamically select the development status and predictor/response variables for regression analysis. 

---

## 💡 Key Findings

The analysis identified several notable patterns:

- Life expectancy generally showed an increasing trend over the analyzed period.
- Developed countries generally exhibited higher life expectancy than developing countries.
- Adult Mortality and HIV/AIDS showed negative associations with life expectancy.
- Schooling and Income Composition of Resources showed positive associations with life expectancy.
- BMI was also examined as a socioeconomic and health-related factor.
- Geographic disparities in life expectancy were observed across different regions.

---

## 📊 Dashboard Features

The interactive dashboard consists of several sections:

### Overview
- Life Expectancy Overview
- SDG 3: Good Health and Well-being
- Searchable Dataset
- Summary Statistics

### Exploration
- Developed vs Developing Countries
- Disease Trends
- Mortality Indicators
- Schooling Trends
- BMI Distribution
- Income Trends
- GDP Trends
- Country-level Insights

### Regression Analysis
- Interactive Correlation Matrix
- Dynamic Linear Regression
- Five Life Expectancy Regression Models

---

## 📁 Project Structure

```text
Life-Expectancy-Analysis/
│
├── LifeExpectancyFinalKelompok4.R
├── Life_Expectancy_Data.csv
├── README.md
│
└── www/
    ├── le.jpeg
    └── sdgs3.png
