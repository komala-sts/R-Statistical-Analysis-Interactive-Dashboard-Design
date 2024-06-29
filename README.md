# Statistical Analysis and Interactive Dashboard Design
# A Comprehensive Statistical Exploratory Analysis On Variations In The Employment Factors Influencing The GDP Per Worker$(More Precisely, GDP Per Employed Person) Between 3 Diverse Euro-African Countries During 2011-2020

## FILE SPECIFICATIONS

1) R File1: Komala_ASDV1.R for TASK1
2) R File2: Komala_ASDV2.R for TASK1
3) R File3:Komala_ASDV3.R for TASK1

4) Komala_ASDV_final.pbix is a Power BI file contains Two DahsBoards Implemented for TASK2 on 
the data imported from World Data Bank
5) Data-FileName: employment_on_gdp.csv Used for Both TASK1 and TASK2
**********************************************************************************************

As a Data Scientist involved in social and economic development at a non-governmental 
organization, given two Tasks in implementing the two main objectives of this project 
to conduct a
1) Comprehensive Exploratory Statistical analysis with detailed visualizations to unveil 
trends and patterns of “Variations in the Employment factors influencing the GDP per Worker 
GDP between 3 diverse income groups of Euro-African countries”, years between 2011 and 2020 
that ultimately provides an opportunity to identify patterns, variations, and potential 
drivers of economic performance. This research could contribute valuable insights for 
policymakers working on regional development initiatives. 

## Data Collected from 
World Data Bank(https://databank.worldbank.org/source/world-development-indicators). 
All indicators 11.
For 12 countries of 3 income groups(High, Mid and Low) for 10 years between 2011-2020
High Income Group  Countries	
Denmark France Germany  Italy 	

Mid Income Group  Countries
Egypt Kenya Namibia Nigeria	


Low Income Group  Countries
Ethiopia Malawi Uganda Tanzania

Data-FileName: employment_on_gdp.csv

Used for Both TASK1 and TASK2
**********************************************************************************************
### OBJECTIVES OF TASK1 AND TASK2

### Task1 Objectives: Exploratory and Statistical Analysis using Statistical Programming - R
### Task2 Objective: Comparative Explanatory Analysis using Business Analytics Solution using POWERBI

**********************************************************************************************

# Task1 Objectives: Exploratory and Statistical Analysis using Statistical Programming - R
Following 3 R files contains the Statistical Analyis code pertaining to the Main objective 
of this Assignment 

R File1: Komala_ASDV1.R
R File2: Komala_ASDV2.R
R File3:Komala_ASDV3.R

# MORE ABOUT TASK1
Robust R Programming is used to implement a Comprehensive Exploratory Statistical analysis 
with relevant assumptions and visualizations to theoretically prove the existence of major 
variations in the value of GDP per Person Employed between the countries of three major 
income groups: High, Mid and Low. By performing 

1) Data preparation/ data wrangling to perform Exploratory data analysis(EDA) and 
other following tests.

2) Correlation Analysis between GDP per Employed Person and other indicators of 3 
income groups in Euro-African Countries. 

3) Conducting test hypotheses to theoretically prove the existence of dissimilarity 
in GDP values between three income groups based on the Normality test of GDP data.

4) Assessing the relationships of indicators with GDP per Employed person and 
formulating equations using Regression

5) Analyzing GDP trends and Forecasting on the same for different Income groups 
using the Time series
**********************************************************************************************
**********************************************************************************************
 
## R File1: Komala_ASDV1.R: Contains the R code of Task 1

=> Data Pre-processing, mandatory Exploratory Data Analysis followed by 
  Implentation of following Two Statistical Analysis tasks
Data  preparation, outlier detection, and dealing with missing data. 
After data preparation and cleaning, reviewing of the dataset through Exploratory 
Data Analysis (EDA) and shown interesting facts about the data set by incorprating proper graphs
Such as Line graphs and histograms

# Descriptive Statistical Analysiscomputing the Means, Median, Standard deviation, Skewness and Kurtosis for all 6 indicators 
both 1) country-wise and 2) Income-group using the summary() method.

# Correlation Analysis for the Indicators
Subtask_A: Correlation Visualization on all indicators together
•	cor() method used to compute the correlation matrix for given numeric data. 
•	corrplot() method used to provide visualization on the correlation matrix.
•	“chart.Correlation()” methos is also used to show the Scatterplot view for the 
given correlation set of numeric variables. This gives better clarity. 

Subtask_B: Individual Correlation test and Correlation Visualization of all indicators with 
target variable GDP_PP_EMP using Cor.test() Pearson's product-moment correlation test 

**********************************************************************************************
**********************************************************************************************

## R File2: Komala_ASDV2.R: Contains the R code for Implementating the following Two Statistical Analysis tasks of Task 1

#  Hypothesis Test Analysis on The Employment Indicator
Two types of Hypothesis tests performed Section-A using Wilcoxon rank sum test and Section 
B,C,D & E  using the Kruskal-Wallis’s test with 5 objectives.
Objectives of 5 tests:

SECTION-A: Performing Hypothesis T-Test / Wilcoxon rank sum test (Mann-Whitney U test) 
(based on the results of Normality Assessments) on the GDP Per Worker$ values of 3 
income-groups to Hypothetically prove whether they are Identical or Not.

SECTION-B: ANOVA Hypothesis Test to statistically access & Hypothetically prove whether 
GDP Per Worker$ values are Identical or Not.

SECTION-C: ANOVA Hypothesis Test to statistically access & Hypothetically prove whether 
Agriculture Employment population % values are Identical or Not.

SECTION-D: ANOVA Hypothesis Test to statistically access & Hypothetically prove whether 
Employment population % values are Identical or Not.

SECTION-E: ANOVA Hypothesis Test to statistically access & Hypothetically prove whether 
Employment population % values are Identical or Not.

**********************************************************************************************
**********************************************************************************************

# Regression Analysis on Employment Indicators
3 Single Linear Regression(SLR) tests are conducted and one Multiple Linear Regression(MLR) 
test is conducted with following objectives.

SLR-Objective1: Finding SLR Formula/Equation between GDP-Per-Worker$(Y) and 
Service-Employment-population%(X) of High-Income Group countries.

SLR-Objective2: Finding SLR Formula/Equation between GDP-Per-Worker$(Y) and 
Agriculture-Employment-population%(X) of Mid-Income Group countries.

SLR-Objective3: Finding SLR Formula/Equation between GDP-Per-Worker$(Y) and 
Agriculture-Employment-population%(X) of Low-Income Group countries.

MLR-Objective: Finding a Formula/Equation using MLR for GDP-Per-Worker$ & 
EMP_Service added with more IVs of High-Income Group countries(extending the SLR1-Objective).


**********************************************************************************************
**********************************************************************************************
## R File3:Komala_ASDV3.R : Contains the R code for Implementating the following Statistical Analysis task of Task 1
# Forecasting GDP-Per-Worker$ Using Time Series Analysis Models

Aim to forecast GDP-per-worker$ for selected four Low-income countries using both Holt-Winters 
and ARIMA models and evaluate and assess the best Model out of them.  The consistent GDP growth
observed in low-income countries has captured the attention of researchers, making it a focal 
point for this experiment.

=====================================TASK 1 ENDS HERE=========================================

**********************************************************************************************
**********************************************************************************************
 
# Task2 Objective: Comparative Explanatory Analysis using POWER BI to provide Business Analytics Solution

Following PowerBi file was created and implemented two Dashboards to perform the task2 objective

Komala_ASDV_final.pbix is a Power BI file contains Two DahsBoards Implemented for Task2 on 
the data imported from World Data Bank
Data-FileName: employment_on_gdp.csv

**********************************************************************************************
**********************************************************************************************

# MORE ABOUT TASK2
Power Business Intelligence Tool is used to implement a working dashboard implemented for 
Comparative Explanatory analysis to discover the height of patterns, trends, and variations 
prevailing in 3 pivotal Employment factors(Service, Industry and Agriculture) influencing 
GDP per worker$ across 3 diverse income groups of Euro-African Countries, of  shedding 
light on the complexities of economic development. 

Moreover, another dashboard implemented to portray the variations prevailing in gender 
Employment especially in Service Sector which is the major factor contributes to the GDP 
per worker$ between the 3 diverse income-groups of Euro-African Countries.

The two Dashboards are focused to present the complex relationship between employment 
factors and GDP per person employed within the context of selected 12 Euro-African 
countries for 10 years(2011-2020 including covid years) the research aims to contribute 
valuable insights into the variations of employment factors across income groups in 3 
employment sectors and their impact on economic outcomes.


# Dashboards Objectives:

• Variations In Employment Factors Affecting GDP Per Worker$ Among 3 Diverse Euro-African Countries: 
This is an Explanatory Analysis on GDP-Per-Worker$  Variations.
• Variations In Service Sector Gender Employment Across Euro-African Countries: 
This provides the Gender Variations in population of Service Sector Employment 


The main table (employement_gdp) is the fact table and 3 dimension tables are created 
from the fact table specific column values that predominantly act as the support table for 
enhancing the accessibility of visualization that significantly helps to portray more complex 
valuable insights profoundly. 
The dim tables on Year, Income group and Country are created. 
3 New columns are created to find the contribution of different sectors on GDP value based on 
their population percentage indicators. 
13 DAX to support visualizations.
