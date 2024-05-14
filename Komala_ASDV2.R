#R file Number: 2 for Task1 ASDV Assignment
#Contains the R code for Implementation of 4.3 & 4.4 of Task 1 of ASDV Assignment 
#================================================================================
#4.3 Hypothesis Tests Analysis 
#4.4 Regression Test Analysis 
#On the Employment GDP dataset that contains 6 indicators downloaded from World Data Bank
#=================================================================================

#setting the working directory#
#setwd("C:/ASDV/ASDV_Activity")
# Installing Required statistical packages for Hypothesis testing and Regression#######

#Installing the Required Packages for Hypothesis
#For visualizations
install.packages("qqplotr") 
install.packages("ggplot2") 
#for shapiro test
install.packages("RVAideMemoire")

#Installing Required Packages for the Regression
#Companion to Applied Regression to perform Applied regression techniques
install.packages("car") 
#corrplot is for corrplot() function is used as a visual Exploratory tool on Correlation matrix
install.packages("corrplot") 
#Powerful Machine learning Library in R. Used in 4.4 Regression task
install.packages("caret") 

#Importing the packages for Hypothesis####
#library(datarium)
library(ggplot2) 
library(qqplotr)
library(RVAideMemoire)
#Importing the Packages for Regression#####
library(car)  
library(corrplot) 
library(caret)  
#stats: Is the available by default, ready to use base package.
options(scipen=999) 
#READING THE DATA####
emp_gdp_df = read.csv("employment_on_gdp_new.csv")
head(emp_gdp_df)
#Year,Country, GDP_PP_EMP, EMP_Agriculture, EMP_Service, EMP_Ser_F, EMP_Ser_M, EMP_Industry & Income_Grp
#Discarding  X column  by dropping it
emp_gdp_df1 <- subset(emp_gdp_df, select = -c(X))
head(emp_gdp_df1)
summary(emp_gdp_df1)
#Verification of Null Values
colSums(is.na(emp_gdp_df1))

# converting the Income_Grp into Numeric factor 
emp_gdp_df1$Income_Grp <- as.factor(emp_gdp_df1$Income_Grp)

#4.3: Hypothesis Tests Analysis : T-Test & ANOVA#####
# Two types of Hypothesis tests performed Section-A using Wilcoxon rank sum test and Section B,C,D & E  
#using the Kruskal-Wallis’s test with 5 objectives.
# Objectives of 5 tests (A, B, C D & E):
# SECTION-A: Performing Hypothesis T-Test / Wilcoxon rank sum test (Mann-Whitney U test) 
# (based on the results of Normality Assessments) on the GDP Per Worker$ values of 3 income-groups 
# to Hypothetically prove whether they are Identical or Not.
# SECTION-B: ANOVA Hypothesis Test to statistically access & Hypothetically prove whether 
# GDP Per Worker$ values are Identical or Not.
# SECTION-C: ANOVA Hypothesis Test to statistically access & Hypothetically prove whether
# Agriculture Employment population % values are Identical or Not.
# SECTION-D: ANOVA Hypothesis Test to statistically access & Hypothetically prove whether 
# Employment population % values are Identical or Not.
# SECTION-E: ANOVA Hypothesis Test to statistically access & Hypothetically prove whether 
# Employment population % values are Identical or Not.



#SECTION A: Hypothesis T-Test / Wilcoxon rank sum test (Mann-Whitney U test)##########
#Objective: Perform Hypothesis test in order to statistically access whether there is 
# significant difference between GDP-per-Worker(GDP_PP_EMP) and 3 income groups Countries 
# pairing in 3 groups through 3 number of T-Tests or Wilcoxon Test(based on the results of 
#  Normality Assessments).
# #Formula grouping factor for both test must have exactly 2 levels only.  Thus, it’s clear 
# that 3-income-groups are not significant in GDP. However, we could acces is there a 
# possible significane between either of income group combination such as 
# HighMid, HighLow and MidLow. Hence, we can create 3 DataFrames subsets in the combination of 
# HighMid, HighLow and MidLow
Income_highmid_GDP <-emp_gdp_df1[emp_gdp_df1$Income_Grp!="Low",] 
Income_highlow_GDP <-emp_gdp_df1[emp_gdp_df1$Income_Grp!="Mid",]
Income_midlow_GDP <-emp_gdp_df1[emp_gdp_df1$Income_Grp!="High",]
summary(Income_highmid_GDP)
summary(Income_highlow_GDP)
summary(Income_midlow_GDP)

#Normality Assessments for Hypothesis Test1####
#Assessing the Normality of Data before performing theT-Test using below methods
#A)Visual Method through Q-Q plot & Histogram Visually confirm if the plot appears to be on a normal curve 
#B) Statistical Test throughShapiro-Wilk Test
#A) Visual Method through Q-Q plot & Histogram####
#Income_highmid_GDP: Visually Assessing data Normality
#When points in the plot roughly fall along a straight diagonal line,then data is assumed to be normally distributed.
ggplot(mapping = aes(sample=Income_highmid_GDP$GDP_PP_EMP)) + 
  stat_qq_point(size = 2,color = "blue") + 
  stat_qq_line(color="orange") + 
  xlab("Theoretical") + 
  ylab("Income_highmid_GDP$GDP_PP_EMP")
hist(Income_highmid_GDP$GDP_PP_EMP)
#From the visualizaion it is evident that Data is not normally distributed

#Income_highlow_GDP: Visually Assessing data Normality
ggplot(mapping = aes(sample=Income_highlow_GDP$GDP_PP_EMP)) + 
  stat_qq_point(size = 2,color = "blue") + 
  stat_qq_line(color="orange") + 
  xlab("Theoretical") + 
  ylab("Income_highlow_GDP$GDP_PP_EMP")
hist(Income_highlow_GDP$GDP_PP_EMP)
#From the visualizaion it is evident that Data is not normally distributed

#Income_midlow_GDP: Visually Assessing data Normality
ggplot(mapping = aes(sample=Income_midlow_GDP$GDP_PP_EMP)) + 
  stat_qq_point(size = 2,color = "blue") + 
  stat_qq_line(color="orange") + 
  xlab("Theoretical") + 
  ylab("Income_midlow_GDP$GDP_PP_EMP")
hist(Income_midlow_GDP$GDP_PP_EMP)
#From the visualizaion it is evident that Data is not normally distributed

#Shapiro-Wilk test to perform Assessing the Normality of Dataset
#B) Statistical Test through Shapiro-Wilk Normality Test######
shapiro.test(Income_highmid_GDP$GDP_PP_EMP)
#The p-value of the above test is p-value = 0.000000001849
# lesser than .05, which evidently indicates that the data is Not normally distributed.

#Counter verifified with log transformation on the data
#Log transformation on data
shapiro.test(log(Income_highmid_GDP$GDP_PP_EMP))
#The p-value of the above shapiro.test with log transformation of data
# is p-value = 0.00000001187
#rexp generates n random numbers from the exponential distribution with
# specified rate for given n data
rexp_data1 = rexp(Income_highmid_GDP$GDP_PP_EMP, rate=3)
shapiro.test(rexp_data1)
#  p-value = 0.000000001325
#The p-value of the above shapiro.test with rexp transformation of data
# is  p-value = 0.000000001325
#So, should Reject Null Hypothesis and accept alternative hypothesis 
#that Income_highlow_GDP$GDP_PP_EMP is not Normal Regardless of any transformation.

#Shapiro Test for GDP_PP_EMP values of High-Low Income-group countries
shapiro.test(Income_highlow_GDP$GDP_PP_EMP)
#p-value = 0.000000000006569
shapiro.test(log(Income_highlow_GDP$GDP_PP_EMP))
#p-value = 0.00000000001931
rexp_data2 = rexp(Income_highlow_GDP$GDP_PP_EMP, rate=3)
shapiro.test(rexp_data2)
#p-value = 0.0000000159
#So, should Reject Null Hypothesis and accept alternative hypothesis 
#that Income_highlow_GDP$GDP_PP_EMP is not Normal Regardless of any transformation.


#Shapiro Test for GDP_PP_EMP values of Mid-Low Income-group countries
shapiro.test(Income_midlow_GDP$GDP_PP_EMP)
# p-value = 0.000000001165
shapiro.test(log(Income_midlow_GDP$GDP_PP_EMP))
# p-value = 0.000001501
rexp_data3 = rexp(Income_midlow_GDP$GDP_PP_EMP, rate=3)
shapiro.test(rexp_data3)
# p-value = 0.000000003557
#So, should Reject Null Hypothesis and accept alternative hypothesis 
#that Income_midlow_GDP$GDP_PP_EMP is not Normal Regardless of any transformation.

#Thus, regardless of any transformation the p-value is very low than the 0.05
#Hence the all three data 1)Income_highmid_GDP$GDP_PP_EMP,2)Income_highlow_GDP$GDP_PP_EMP, 
#3)Income_midlow_GDP$GDP_PP_EMP are "Not normally distributed"

# Because the datasets have not passed normality distribution the Assumtions, hence, T-test cannot be performed. 
#Therefore, alternatively performing the Non-Parametric Hypothesis Test using
#Wilcoxon rank sum test (also known as the Mann-Whitney U test)
#First, converting the categorical data into a factor
#Now, assessing the normality of data GDP_PP_EMP on subsets based on Income_Grp indicator 
#using a Q-Q plot and histograms. 

#Visually assessing the Normality for wilcoxon######
#Visually assessing the Normality using Q-Q plot and histograms on the 3 data subsets of 
#GDP_PP_EMP groupedby Income (high, mid and low)
#Now,let's plot the GDP_PP_EMP values of High Income Group, Mid Income Group and Low Income Group countries individually
#Creating data subsets on GDP_PP_EMP grouped by Income_Grp#########
Income_high_GDP <-emp_gdp_df1$GDP_PP_EMP[emp_gdp_df1$Income_Grp=="High"] 
Income_mid_GDP <-emp_gdp_df1$GDP_PP_EMP[emp_gdp_df1$Income_Grp=="Mid"]
Income_low_GDP <-emp_gdp_df1$GDP_PP_EMP[emp_gdp_df1$Income_Grp=="Low"]
ggplot(mapping = aes(sample = Income_high_GDP)) + 
  stat_qq_point(size = 2,color = "blue") + 
  stat_qq_line(color="red") + 
  xlab("Theoretical") +
  ylab("GDP_PP_EMP on High & Middle Income Group") +
  ggtitle("Normality Assessment on GDP_PP_EMP data  for \nHigh Income Group Countries")
hist(Income_high_GDP, 
     main="Histogram for GDP per Person Employed data \n of High Income Countries")
#Data Normality Exist in Income_high_GDP data

ggplot(mapping = aes(sample = Income_mid_GDP)) + 
  stat_qq_point(size = 2,color = "blue") + 
  stat_qq_line(color="red") + 
  xlab("Theoretical") + 
  ylab("GDP_PP_EMP on Mid Income Group") +
  ggtitle("Normality Assessment on GDP_PP_EMP data for\n Mid Income Group Countries")
hist(Income_mid_GDP, 
     main="Histogram for GDP per Person Employed data\n of Mid Income Countries")
# Small deviation of Normality Exist on Income_mid_GDP data

ggplot(mapping = aes(sample = Income_low_GDP)) + 
  stat_qq_point(size = 2,color = "blue") + 
  stat_qq_line(color="red") + 
  xlab("Theoretical") +
  ylab("GDP_PP_EMP on Low Income Group") +
  ggtitle("Normality Assessment on GDP_PP_EMP data for \nLow Income Group Countries")
hist(Income_low_GDP, 
     main="Histogram for GDP per Person Employed data \nof Low Income Countries")
#Data Normality Exist on Income_Low_GDP data

#Wilcoxon Hypothesis test1: GDP of High & Mid incomegroup ########
#Wilcoxon Hypothesis test between GDP_PP_EMP and High & Mid incomegroup countries
#Null and Alternative hypotheses are: 
# H0: The distribution of GDP Per Person Employed(GDP_PP_EMP)in the dataframe(Income_highmid_GDP)
#are identical for both High and mid Income group Countries 
# H1: The distribution of GDP Per Person Employed(GDP_PP_EMP) in the dataframe(Income_highmid_GDP)
#are NOT identical for  both High and mid Income group Countries 

# Performing the Non-Parametric Hypothesis Test on Income_highmid_GDP 
#Wilcoxon Hypothesis test (also known as the Mann-Whitney U test)
wilcox.test(GDP_PP_EMP ~ Income_Grp, data = Income_highmid_GDP )
#p-value < 0.00000000000000022
# From the output, we can see the p-value is 0.00000000000000022.
# Since, P value is less than the threshold value 0.05, thus
# we can reject the Null Hypothesis and accept the Alternative Hypothesis

#Wilcoxon Hypothesis test2: GDP High & Low incomegroup ########
#Wilcoxon Hypothesis test between GDP_PP_EMP and High & Low incomegroup countries
#Null and Alternative hypotheses, which are: 
# H0: The distribution of GDP Per Person Employed(GDP_PP_EMP) in the dataframe(Income_highlow_GDP)
#are identical for both High and Low Income group Countries 
# H1: The distribution of GDP Per Person Employed(GDP_PP_EMP)  in the dataframe(Income_highmid_GDP) 
#are NOT identical for  both High and Low Income group Countries 
wilcox.test(GDP_PP_EMP ~ Income_Grp, data = Income_highlow_GDP )
# p-value < 0.00000000000000022
# From the output, we can see the p-value is 0.00000000000000022.
# Since, P value is less than the threshold value 0.05, thus
# we can reject the Null Hypothesis and accept the Alternative Hypothesis

#Wilcoxon Hypothesis test3: GDP of Mid & Low incomegroup ########
#Wilcoxon Hypothesis test between GDP_PP_EMP and High & Low incomegroup countries
#Null and Alternative hypotheses, which are: 
# H0: The distribution of GDP Per Person Employed(GDP_PP_EMP) in the dataframe(Income_highlow_GDP)
#are identical for both High and Low Income group Countries 
# H1: The distribution of GDP Per Person Employed(GDP_PP_EMP)  in the dataframe(Income_highmid_GDP) 
#are NOT identical for  both High and Low Income group Countries 
wilcox.test(GDP_PP_EMP ~ Income_Grp, data = Income_midlow_GDP )
#p-value < 0.00000000000000022
# From the output, we can see the p-value is 0.00000000000000022.
# Since, P value is less than the threshold value 0.05, thus
# we can reject the Null Hypothesis and accept the Alternative Hypothesis

# Hypothesis Test result Discussion:
# However, all three final Wilcox.test() resulted very low p-value (<0.05 threshold) 
# hence we should Reject the Null Hypothesis and accept the Alternative Hypothesis. 
# Thus, it statistically proves that the GDP values of all 3 paired subsets of income groups
# (GDP of  High-Mid, High-Low & Mid-Low) are “Not Identical”, there is a significant 
# difference exist between them.

#SECTION B: ANOVA HYPOTHESIS TEST on GDP_PP_EMP######
#Objective of SECTION-B: ANOVA Hypothesis Test to statistically access & Hypothetically prove whether GDP Per Worker$ values 
#are Identical or Not between 3 income group Countries   
#In order to statistically access whether there is significant difference 
#between three income group countries on GDP_PP_EMP

#There are 6 Assumptions to perform ANOVA HYPOTHESIS TEST
#To perform ANOVA HYPOTHESIS TEST, the dataset needs to 
#"pass" six assumptions to give us a valid result
# Assumption 1: Dependent variable should be continuous.
# Assumption 2: Independent variables should be categorical with two or more categories.
# Assumption 3: Observations should be independent, which means that there is no relationship 
#between the observations in each group or between the groups themselves.
# Assumption 4: There should be no significant outliers.
# Assumption 5: Dependent variable should be approximately normally distributed for each category of the independent variable.
# Assumption 6: Variances of the dependent variable within each category should be homogeneous.

#ANOVA Assumptions Validations######
# Assumption 1 passed: Dependent variable(GDP_PP_EMP) is continuous.
# Assumption 2 passed: Independent variables(Income_Grp) is categorical 
#with 3 categories.
# Assumption 3 passed: Observations are independent in theDependent variable, 
#which means that there is no relationship between the 
#observations in each group or between the groups themselves.
# Assumption 4: Checking for significant outliers######
#Putting the Dependent variable(GDP_PP_EMP) ~ then Independent Variable
boxplot(GDP_PP_EMP ~ Income_Grp , data=emp_gdp_df1, names=c("High", "Low", "Mid"), 
        xlab="Income Group categories", ylab="GDP per Person Employed", 
        main="GDP per Person Employed for 3 types of Income groups")

# Assumption 5:Shapiro-Wilk- Assessing the Normal distribution of data ####
# of Dependent variable (GDP_PP_EMP)for each category of the 
# independent variable using Shapiro-Wilk test hypothesis
#Assessing data normality using Shapiro-Wilk test for hypothesis assumptions

# Now, recalling the Shapiro-Wilk test hypotheses:
# The Null and ALternative Hypothesis are
# H0: the variable follows a normal distribution 
# H1: the variable does NOT follow a normal distribution
#byf.shapiro() belongs to RVAideMemoire  
byf.shapiro(GDP_PP_EMP ~ Income_Grp, data=emp_gdp_df1)
#Analyisi: All the p-values 0.0264, 0.0407 are 0.0003 are below 
# the threshold 0.05. Hence we should Reject the NULL hypothesis and accept the alternative
#hypothesis that the Dependent variable date are not normally distributed

# Assumption 6: Assessing homogeneity of variances of GDP_PP_EMP 
#in three income levels of the Countries using Bartlett test
#Bartlett test is from the default package, stats and is used to perform this 6th assumption
#Assumption 6:Bartlett test of homogeneity of variances#####
# H0: variances of Dependent variable GDP_PP_EMP is homogeneous
# H1: variances of Dependent variable GDP_PP_EMP is NOT homogeneous
bartlett.test(GDP_PP_EMP ~ Income_Grp, data=emp_gdp_df1)
#p-value <0.00000000000000022
# From the above test output, the p-value is 0.00000000000000022 and 
#it is considerably very less than the threshold 0.05. 
#Hence, we should reject the Null Hypothesis and accept the alternative hypothesis
# This is a huge evidence to suggest that the variances of GDP_PP_Emp values 
#are not similar for the three Income group countries

#ANOVA TEST#########
#All 6 assumptions have be performed and the GDP_PP_EMP data has 
#Not passed 5th & 6th Assumptions
#Hence, conducting the Non-parametric Version of the One-way ANOVA Test using Kruskal-Wallis test
#Kruskal-Wallis test: Non-parametric ANOVA Test #########
# The Null and ALternative Hypothesis are
# 𝐻0: 𝜇High=𝜇Mid=𝜇Low
# 𝐻1: 𝜇High≠𝜇Mid≠𝜇Low
kruskal.test(GDP_PP_EMP ~ Income_Grp, data=emp_gdp_df1)

# From the above test output, the p-value <  0.00000000000000022 and 
#it is considerably very less than the threshold 0.05. 
#Hence, we should reject the Null Hypothesis and accept the alternative hypothesis
# This evidently suggest that there is significant dissimilarity effect
#on the GDP_PP_Emp values for three different Income group countries

# Nevertheless, the Kruskal-Wallis test will not give insights into 
#which pairs of groups are different. 
#Hence, we now need to run a post-hoc test to explore the specific 
#groups that differ 
#Here, Post hoc test is conducted using the Pairwise Wilcoxon Rank Sum Tests
#Post hoc test: pairwise.wilcox.test######
#between each other 
#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/pairwise.wilcox.test.html
# Perform pairwise Wilcoxon tests with Bonferroni correction
result_pairwise_GDP <- pairwise.wilcox.test(emp_gdp_df1$GDP_PP_EMP, emp_gdp_df1$Income_Grp,  p.adjust.method = "bonf")
#result_pairwise_GDP <- pairwise.wilcox.test(emp_gdp_df1$GDP_PP_EMP, emp_gdp_df1$Income_Grp,  p.adjust.method = "bonferroni")
# Print the pairwise comparison results
print(result_pairwise_GDP )
#1. Comparision between Low vs. High is Low <0.0000000000000002 - 
# The p-value is extremely low, indicating a statistically significant difference in GDP between the Low and High 
# income groups. Specifically, the GDP of the High income group is significantly different from that of the Low income group

#2. Comparision betwe. Mid vs. High is Mid <0.0000000000000002
# Similarly, the p-value is extremely low, suggesting a statistically significant difference in GDP between the 
# Mid and High income groups. The GDP of the High income group is significantly different from that of the Mid income group
#3. Comparision betwe. Mid vs. Low is Low <0.0000000000000002
# The p-value is extremely low, indicating a statistically significant difference in GDP between the 
# Mid and Low income groups. The GDP of the Low income group is significantly different from that of the Mid income group.

# In conclusion, the results suggest that there are significant 
# differences in GDP between all pairs of income groups (Low, Mid, and High). The p-values are extremely low, 
# well below conventional significance levels,indicating strong evidence to reject null hypothesis of equal medians. 
# Therefore, we could conclude that there are statistically significant differences in GDPper worker$ among the different income groups

#Section C: ANOVA HYPOTHESIS TEST on EMP_Agriculture######
#Objective of Section C: ANOVA Hypothesis Test to statistically access & Hypothetically prove whether 
#Agriculture Employment population % values are Identical or Not between 3 income group Countries   

#There are 6 Assumptions to perform ANOVA HYPOTHESIS TEST
#To perform ANOVA HYPOTHESIS TEST, the dataset needs to 
#"pass" six assumptions to give us a valid result
# Assumption 1: Dependent variable should be continuous.
# Assumption 2: Independent variables should be categorical with two or more categories.
# Assumption 3: Observations should be independent, which means that there is no relationship between the observations 
#in each group or between the groups themselves.
# Assumption 4: There should be no significant outliers.
# Assumption 5: Dependent variable should be approximately normally distributed for each category of the independent variable.
# Assumption 6: Variances of the dependent variable within each category should be homogeneous.
#emp_gdp_df1$EMP_Agriculture
#ANOVA Assumptions Validations######
# Assumption 1 passed: Dependent variable(EMP_Agriculture) is continuous.
# Assumption 2 passed: Independent variables(Income_Grp) is categorical 
#with 3 categories.
# Assumption 3 passed: Observations are independent in theDependent variable, 
#which means that there is no relationship between the 
#observations in each group or between the groups themselves.
# Assumption 4: Checking for significant outliers#####
#Putting the Dependent variable(EMP_Agriculture) ~ then Independent Variable
boxplot(EMP_Agriculture ~ Income_Grp , data=emp_gdp_df1, names=c("High", "Low", "Mid"), 
        xlab="Income Group categories", ylab="Agriculture Employment Percentage", 
        main="Agriculture Employment Percentage for\n 3 types of Income group Countries")
#Seems that High income group countries have less agriculture employment and 
#Low incomegroup countries have very high percentage of Agriculture employment
# Assumption 5: Assessing the Normal distribution of data#####
# of Dependent variable (EMP_Agriculture)for each category of the 
# independent variable using Shapiro-Wilk test hypothesis
#Assessing data normality using Shapiro-Wilk test for hypothesis assumptions
# Now, recalling the Shapiro-Wilk test hypotheses:
# The Null and ALternative Hypothesis are
# H0: the variable follows a normal distribution 
# H1: the variable does NOT follow a normal distribution
byf.shapiro(EMP_Agriculture ~ Income_Grp, data=emp_gdp_df1)
#Analysis: Except Low income group, whose the p-values= 0.201063, #remaining p-values are less than the 
# threshold 0.05 High=0.012909,  are Mid=0.004303 are below the threshold 0.05. Hence we we should Reject 
# the NULL hypothesis since majority of the  the Dependent variable data are not normally distributed

# Assumption 6: Assessing homogeneity of variances of 
#Agriculture employment percentage (EMP_Agricultureis a continous numeric value)
#in three income levels of the Countries using Bartlett test
#Bartlett test is from the default package, stats and is used to perform this 6th assumption
#Assumption 6:Bartlett test of homogeneity of variances#####
# H0: variances of Dependent variable EMP_Agriculture is homogeneous
# H1: variances of Dependent variable EMP_Agriculture is NOT homogeneous

bartlett.test(EMP_Agriculture ~ Income_Grp, data=emp_gdp_df1)
# From the above test output, the p-value is 0.00000000000000022 and 
#it is considerably very less than the threshold 0.05. 
#Hence, we should reject the Null Hypothesis and accept the alternative hypothesis
# This is a huge evidence to suggest that the 
#variances of GDP_PP_Emp values are not similar for the 
#three Income group countries

#ANOVA TEST#########
#All 6 assumptions have be performed and the EMP_Agriculture data has Not passed
# 5th & 6th Assumptions
#Hence, conducting the Non-parametric Version of the One-way ANOVA Test
#using Kruskal-Wallis test
#Kruskal-Wallis test: Non-parametric ANOVA Test #########
# The Null and ALternative Hypothesis are
# 𝐻0: 𝜇High=𝜇Mid=𝜇Low
# 𝐻1: 𝜇High≠𝜇Mid≠𝜇Low
kruskal.test(EMP_Agriculture ~ Income_Grp, data=emp_gdp_df1)

# From the above test output, the p-value <  0.00000000000000022 and 
#it is considerably very less than the threshold 0.05. 
#Hence, we should reject the Null Hypothesis and accept the alternative hypothesis
# This evidently suggest that there is significant dissimilarity effect
#on the EMP_Agriculture values for three different Income group countries

# Nevertheless, the Kruskal-Wallis test will not give insights into 
#which pairs of groups are different. 
#Hence, we now need to run a post-hoc test to explore the specific 
#groups that differ 
#Here, Post hoc test is conducted using the Pairwise Wilcoxon Rank Sum Tests
#Post hoc test: pairwise.wilcox.test######
#between each other 
#Ref:https://stat.ethz.ch/R-manual/R-devel/library/stats/html/pairwise.wilcox.test.html
# Perform pairwise Wilcoxon tests with Bonferroni correction
#Ref.:https://bookdown.org/thomas_pernet/Tuto/non-parametric-tests.html
result_pairwise_EmpAgri <- pairwise.wilcox.test(emp_gdp_df1$EMP_Agriculture, emp_gdp_df1$Income_Grp,  p.adjust.method = "bonf", exact = FALSE)
# Print the pairwise comparison results
print(result_pairwise_EmpAgri  )
#1. Comparision betwe. Low vs. High is Low <0.000000000000043 - 
# The p-value is extremely low, indicating a statistically significant difference in EMP_Agriculture 
# between the Low and High income groups. Specifically, the percentage Agriculture employement of the 
# High income group is significantly different from that of the Low income group. 
# 2. Comparision betwe. Mid vs. High is #Mid <0.000000000000043
# Similarly, the p-value is extremely low, suggesting a statistically difference in EMP_Agriculture between 
# the Mid and High income groups. The percentage Agriculture employement of the High income group is 
#significantly different from that of the Mid income group
# 3.Comparision betwe. Mid vs. Low is Low <0.000000000000043
# The p-value is extremely low, indicating a statistically significant difference in EMP_Agriculture between 
# the Mid and Low income groups. The percentage Agriculture employement ofLow income group is 
#significantly different from that of the Mid income group.

# In conclusion, the results suggest that there are high significant 
# differences exist in Agriculture Employment Ratio between all pairs of income groups (Low, Mid, and High). 
# The p-values are extremely low, well below conventional significance levels, indicating strong evidence to 
# reject the null hypothesis of equal medians. Therefore, we could conclude that there are statistically 
# significant differences in EMP_Agriculture among the different income groups

#Section D: ANOVA HYPOTHESIS TEST on EMP_Service######
#OObjective of Section D:ANOVA Hypothesis Test to statistically access & Hypothetically prove whether 
#Employment population % values are Identical or Not between 3 income group Countries 

#There are 6 Assumptions to perform ANOVA HYPOTHESIS TEST
#To perform ANOVA HYPOTHESIS TEST, the dataset needs to 
#"pass" six assumptions to give us a valid result
# Assumption 1: Dependent variable should be continuous.
# Assumption 2: Independent variables should be categorical with two or more categories.
# Assumption 3: Observations should be independent, which means that there is no relationship between the 
#observations in each group or between the groups themselves.
# Assumption 4: There should be no significant outliers.
# Assumption 5: Dependent variable should be approximately normally distributed for each category of the independent variable.
# Assumption 6: Variances of the dependent variable within each category should be homogeneous.
#emp_gdp_df1$EMP_Service
#ANOVA Assumptions Validations######
# Assumption 1 passed: Dependent variable(EMP_Service) is continuous.
# Assumption 2 passed: Independent variables(Income_Grp) is categorical 
#with 3 categories.
# Assumption 3 passed: Observations are independent in theDependent variable, 
#which means that there is no relationship between the observations in each group or between the groups 
# Assumption 4: Checking for significant outliers#####
#Putting the Dependent variable(EMP_Service) ~ then Independent Variable
boxplot(EMP_Service ~ Income_Grp , data=emp_gdp_df1, names=c("High", "Low", "Mid"), 
        xlab="Income Group categories", ylab="Service Employment Percentage", 
        main="Service Employment Percentage for\n 3 types of Income group Countries")
#Seems that High income group countries have less Service employement and 
#Low incomegroup countries have very high percentage of Service employment

# Assumption 5: Assessing the Normal distribution of data#####
# of Dependent variable (EMP_Service)for each category of the 
# independent variable using Shapiro-Wilk test hypothesis
#Assessing data normality using Shapiro-Wilk test for hypothesis assumptions

# Now, recalling the Shapiro-Wilk test hypotheses:
# The Null and ALternative Hypothesis are
# H0: the variable follows a normal distribution 
# H1: the variable does NOT follow a normal distribution
byf.shapiro(EMP_Service ~ Income_Grp, data=emp_gdp_df1)
#Analysis: Except Low income group, whose the p-values= 0.0733800,remaining p-values are less 
# than the threshold 0.05 High=0.0001131, and Mid= 0.00002027 are below the threshold 0.05. 
# Hence we we should Reject the NULL hypothesis since majority of the the Dependent variable data 
#are not normally distributed

# Assumption 6: Assessing homogeneity of variances of 
#Service employment percentage (EMP_Serviceis a continous numeric value)
#in three income levels of the Countries using Bartlett test
#Bartlett test is from the default package, stats and is used to perform this 6th assumption
#Assumption 6:Bartlett test of homogeneity of variances#####
# H0: variances of Dependent variable EMP_Service is homogeneous
# H1: variances of Dependent variable EMP_Service is NOT homogeneous

bartlett.test(EMP_Service ~ Income_Grp, data=emp_gdp_df1)

#From the above test output, the p-value is  0.004594 and 
#it is less than the threshold 0.05. 
#Hence, we should reject the Null Hypothesis and accept the alternative hypothesis
# This is a huge evidence to suggest that the 
#variances of  EMP_Service values are not similar for the 
#three Income group countries

#ANOVA TEST#########
#All 6 assumptions have be performed and the EMP_Service data has Not passed
# 5th & 6th Assumptions
#Hence, conducting the Non-parametric Version of the One-way ANOVA Test
#using Kruskal-Wallis test
#Kruskal-Wallis test: Non-parametric ANOVA Test #########
# The Null and ALternative Hypothesis are
# 𝐻0: 𝜇High=𝜇Mid=𝜇Low
# 𝐻1: 𝜇High≠𝜇Mid≠𝜇Low

kruskal.test(EMP_Service ~ Income_Grp, data=emp_gdp_df1)

# From the above test output, the p-value <  0.00000000000000022 and 
#it is considerably very less than the threshold 0.05. 
#Hence, we should reject the Null Hypothesis and accept the alternative hypothesis
# This evidently suggest that there is significant dissimilarity effect
#on the EMP_Service values for three different Income group countries

# Nevertheless, the Kruskal-Wallis test will not give insights into 
#which pairs of groups are different. 
#Hence, we now need to run a post-hoc test to explore the specific 
#groups that differ 
#Here, Post hoc test is conducted using the Pairwise Wilcoxon Rank Sum Tests
#Post hoc test: pairwise.wilcox.test######
#between each other 
#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/pairwise.wilcox.test.html
# Perform pairwise Wilcoxon tests with Bonferroni correction
#https://bookdown.org/thomas_pernet/Tuto/non-parametric-tests.html
result_pairwise_EmpSer <- pairwise.wilcox.test(emp_gdp_df1$EMP_Service, emp_gdp_df1$Income_Grp,  p.adjust.method = "bonf", exact = FALSE)
#,  exact = FALSE
#exact = FALSE  # or TRUE if you want to try an exact test
#result_pairwise_GDP <- pairwise.wilcox.test(emp_gdp_df1$GDP_PP_EMP, emp_gdp_df1$Income_Grp,  p.adjust.method = "bonferroni")
# Print the pairwise comparison results
print(result_pairwise_EmpSer  )
#1. Comparision betwe. Low vs. High is Low <0.000000000000043 - 
# The p-value is extremely low, indicating a statistically significant difference in EMP_Service between the 
# Low and High income groups. Specifically, the percentage Service employement of the High income group is 
#significantly different from that of the Low income group. 
#2.Comparision betwe. Mid vs. High is Mid <0.000000000000043
# Similarly, the p-value is extremely low, suggesting a statistically significant difference in 
# EMP_Service between the Mid and High income groups. The percentage Service employement of 
# the High income group is significantly different from that of the Mid income group
#3.Comparision betwe. Mid vs. Low is Low <0.000000000000043
# The p-value is extremely low, indicating a statistically significant difference in EMP_Service between the 
# Mid and Low income groups. The percentage Service employement ofLow income group is significantly different from 
# that of the Mid income group.

# In conclusion, the results suggest that there are high significant 
# differences exist in Service Employment Ratio between all pairs of 
#income groups (Low, Mid, and High). The p-values are extremely low, 
# well below conventional significance levels, indicating strong 
# evidence to reject the null hypothesis of equal medians. 
# Therefore, we could conclude that there are statistically 
# significant differences in EMP_Service among the different income groups

#Section E: ANOVA HYPOTHESIS TEST on EMP_Industry######
#Objective of Section E: ANOVA Hypothesis Test to statistically access & Hypothetically prove whether 
#Employment population % values are Identical or Not between 3 income group Countries  

#There are 6 Assumptions to perform ANOVA HYPOTHESIS TEST
#To perform ANOVA HYPOTHESIS TEST, the dataset needs to 
#"pass" six assumptions to give us a valid result
# Assumption 1: Dependent variable should be continuous.
# Assumption 2: Independent variables should be categorical with two or more categories.
# Assumption 3: Observations should be independent, which means that there is no relationship between the observations in each group or between the groups themselves.
# Assumption 4: There should be no significant outliers.
# Assumption 5: Dependent variable should be approximately normally distributed for each category of the independent variable.
# Assumption 6: Variances of the dependent variable within each category should be homogeneous.
#emp_gdp_df1$EMP_Industry
#ANOVA Assumptions Validations######
# Assumption 1 passed: Dependent variable(EMP_Industry) is continuous.
# Assumption 2 passed: Independent variables(Income_Grp) is categorical with 3 categories.
# Assumption 3 passed: Observations are independent in theDependent variable 
# Assumption 4: Checking for significant outliers#####
#Putting the Dependent variable(EMP_Industry) ~ then Independent Variable
boxplot(EMP_Industry ~ Income_Grp , data=emp_gdp_df1, names=c("High", "Low", "Mid"), 
        xlab="Income Group categories", ylab="Industry Employment Percentage", 
        main="Industry Employment Percentage for\n 3 types of Income group Countries")
#Seems that High income group countries have less Industry employement and Low incomegroup countries have very 
#high percentage of Industry employment

# Assumption 5: Assessing the Normal distribution of data#####
# of Dependent variable (EMP_Industry)for each category of the independent variable using Shapiro-Wilk test hypothesis
#Assessing data normality using Shapiro-Wilk test for hypothesis assumptions

# Now, recalling the Shapiro-Wilk test hypotheses:
# The Null and ALternative Hypothesis are
# H0: the variable follows a normal distribution 
# H1: the variable does NOT follow a normal distribution
byf.shapiro(EMP_Industry ~ Income_Grp, data=emp_gdp_df1)
#Analysis: Except Low income group, whose the p-values= 0.5295,
#remaining p-values are less than the threshold 0.05 High=0.00001371 ,  
#and Mid= 0.00002919 are below the threshold 0.05.Hence we we should Reject the NULL hypothesis since majority of the 
# the Dependent variable data are not normally distributed Data is Partially normally distributed

# Assumption 6: Assessing homogeneity of variances of 
#Industry employment percentage (EMP_Industryis a continous numeric value)
#in three income levels of the Countries using Bartlett test
#Bartlett test is from the default package, stats and is used to perform this 6th assumption
#Assumption 6:Bartlett test of homogeneity of variances#####
# H0: variances of Dependent variable EMP_Industry is homogeneous
# H1: variances of Dependent variable EMP_Industry is NOT homogeneous

bartlett.test(EMP_Industry ~ Income_Grp, data=emp_gdp_df1)

#From the above test output, the p-value is  p-value < 0.00000000000000022 and 
#it is less than the threshold 0.05. 
#Hence, we should reject the Null Hypothesis and accept the alternative hypothesis
# This is a huge evidence to suggest that the 
#variances of  EMP_Industry values are not similar for the 
#three Income group countries

#ANOVA TEST#########
#All 6 assumptions have be performed and the EMP_Industry data has Not passed
# 5th & 6th Assumptions
#Hence, conducting the Non-parametric Version of the One-way ANOVA Test using Kruskal-Wallis test
#Kruskal-Wallis test: Non-parametric ANOVA Test #########
# The Null and ALternative Hypothesis are
# 𝐻0: 𝜇High=𝜇Mid=𝜇Low
# 𝐻1: 𝜇High≠𝜇Mid≠𝜇Low
kruskal.test(EMP_Industry ~ Income_Grp, data=emp_gdp_df1)

# From the above test output, the p-value <  0.00000000000000022 and 
#it is considerably very less than the threshold 0.05. 
#Hence, we should reject the Null Hypothesis and accept the alternative hypothesis
# This evidently suggest that there is significant dissimilarity effect
#on the EMP_Industry values for three different Income group countries

# Nevertheless, the Kruskal-Wallis test will not give insights into 
#which pairs of groups are different. 
#Hence, we now need to run a post-hoc test to explore the specific 
#groups that differ 
#Here, Post hoc test is conducted using the Pairwise Wilcoxon Rank Sum Tests
#Post hoc test: pairwise.wilcox.test######
#between each other 
#ref.:https://stat.ethz.ch/R-manual/R-devel/library/stats/html/pairwise.wilcox.test.html
# Perform pairwise Wilcoxon tests with Bonferroni correction
#Ref.:https://bookdown.org/thomas_pernet/Tuto/non-parametric-tests.html
result_pairwise_EmpInd <- pairwise.wilcox.test(emp_gdp_df1$EMP_Industry, emp_gdp_df1$Income_Grp,  p.adjust.method = "bonf", exact = FALSE)
# Print the pairwise comparison results
print(result_pairwise_EmpInd  )
#1. Comparision betwe. Low vs. High is Low <0.000000000000043 - 
# The p-value is extremely low, indicating a statistically significant 
# difference in EMP_Industry between the Low and High income groups. Specifically, 
# the percentage Industry employement of the High income group is 
#significantly different from that of the Low income group. 
#2. Comparision betwe. Mid vs. High is Mid <0.000000508994686
# Similarly, the p-value is extremely low, suggesting a statistically 
# significant difference in EMP_Industry between the Mid and High income groups. 
# The percentage Industry employement of the High income group is 
#significantly different from that of the Mid income group
# 3. Comparision betwe. Mid vs. Low is Low <0.000000000000043
# The p-value is extremely low, indicating a statistically significant difference in EMP_Industry between the Mid and Low income groups.   
# The percentage Industry employement ofLow income group is significantly different from that of the Mid income group.

# In conclusion, the results suggest that there are high significant differences exist in 
# Industry Employment Ratio between all pairs of income groups (Low, Mid, and High). 
# The p-values are extremely low,  well below conventional significance levels, indicating strong 
# evidence to reject the null hypothesis of equal medians. Therefore, we could conclude that 
# there are statistically significant differences in EMP_Industry among the different income groups
#Hypothesis Analysis Ends Here ########

#4.4: Regression Analysis : Part one: SLR####
#3 SLR tests are conducted and one MLR test is conducted. Following are the objectives
# SLR objective1: Finding SLR Equation between (Y)GDP_PP_EMP and (X)EMP_Service of High Income GRP
# SLR objective2: Finding SLR Equation between (Y)GDP_PP_EMP & (X)EMP_Agriculture of Mid Income GRP
# SLR objective3: Finding SLR Equation between (Y)GDP_PP_EMP & (X)EMP_Agriculture of Low Income GRP
# MLR-Objective: Finding a linear equation using MLR for GDP_PP_EMP & EMP_Service added with more IVs 
#of High Income GRP 

#SLR.1 GDP_PP_EMP & EMP_Service of High Income GRP#####
#Steps to do SLR analysis are:
# 1. Load the data into R and briefly check the data
# 2. Define the objective of the regression analysis
# 3. Perform the linear regression analysis
# 4. Make sure the fitted model meets SLR assumptions
# 5. Report the resultseport the results

# SLR model, DV (Y, dependent variable, GDP_PP_EMP) is Numerical and
#  IV (X, independent variable, EMP_Industry)is Numerical.
#1.Loading the data####
EMP_GDP_HI <- emp_gdp_df1[emp_gdp_df1$Income_Grp=="High",c("EMP_Agriculture","EMP_Service","EMP_Industry","GDP_PP_EMP")]

#2.Defining the objective
#SLR objective1: Finding SLR Equation between (Y)GDP_PP_EMP and (X)EMP_Service of High Income GRP####
# Assessing Normality of both DV and IV
shapiro.test(EMP_GDP_HI$GDP_PP_EMP)
#p-value = 0.02645 #Reject Null hypothesis & accept Null hypothesis that data(EMP_GDP_HI$GDP_PP_EMP) is Not Normal
ggplot(mapping = aes(sample=EMP_GDP_HI$GDP_PP_EMP)) + 
  stat_qq_point(size = 2,color = "blue") + 
  stat_qq_line(color="orange") + 
  xlab("Theoretical") + 
  ylab("EMP_GDP_HI$GDP_PP_EMP")
hist(EMP_GDP_HI$GDP_PP_EMP)
#Visually also data distribution is Not Normal

shapiro.test(EMP_GDP_HI$EMP_Service)
#p-value = 0.0001131 #Reject Null hypothesis & accept Null hypothesis that data(EMP_GDP_HI$EMP_Service) is Not Normal
ggplot(mapping = aes(sample=EMP_GDP_HI$EMP_Service)) + 
  stat_qq_point(size = 2,color = "blue") + 
  stat_qq_line(color="orange") + 
  xlab("Theoretical") + 
  ylab("EMP_GDP_HI$EMP_Service")
hist(EMP_GDP_HI$EMP_Service)
#Visually also data distribution is Not Normal

#Checking the Relationship using the Correlation matrix between all variables(package: stats):
cor(EMP_GDP_HI)
#The result is a matrix of correlation values.
#Below is the Visualization of the a correlation matrix, using the corrplot package:
corrplot(cor(EMP_GDP_HI))  
#w.r.to GDP_PP_EMP (Y), 
#1)EMP_Agriculture has moderate positive correlation
#2)EMP_Service has moderate positive correlation 
#3)EMP_Industry has moderate negative correlation 

#3.Performing SLR(Single linear regression) analysis for High income countries using FS method######
#Applying Forward Stepwise(FS) in SLR:
#Intent to write the formula option for High income Countries
# Y = GDP_PP_EMP
# X = EMP_Service
#(Y ~ X)
#lm() function is used here to fit the linear model 
SLR_model1 <- lm(GDP_PP_EMP ~ EMP_Service, EMP_GDP_HI) 
summary.lm(SLR_model1)

#Pr(>|t|) p-value of Intercept is 0.000000288 and the Pr(>|t|) p-value of EMP_Service is  0.0111
# In both cases, the p-values are very small(<0.05), suggesting strong 
#evidence against the null hypothesis that the true coefficient is zero
#Thus, Both coefficients (Intercept & EMP_Service) are considered statistically significant and the 
#SLR1 equation will be#####
#GDP_PP_EMP = 74866.4  + 434.9 * EMP_Service
# Adjusted R-squared is 13.59% This means Employement Service ratio(EMP_Service) 
# with this regression equation can predict 13.59% of the entire variability 
# in the GDP_PP_EMP

#4. Assessing the fitted model meets SLR's 4 assumptions#####
#1. Linearity: Assessing the relationship between X and Y must be linear using a scatterplot of x and y. 
#Scatter plot to visualise the fitted regression line  
plot(GDP_PP_EMP ~ EMP_Service, EMP_GDP_HI, col = "blue", 
     main = "Regression: GDP per Person Employed & \nService Sector Employment population%", 
     xlab = "Service Employment population%", 
     ylab = "GDP per Person Employed")
#Then, adding the regression line to the plot:
abline(SLR_model1 , col="red")
#Linearity is approved. since data is spread around the regression line

#2. Residuals’ Independence
#Checking this assumption by examining a scatterplot of 
# “residuals versus fits”; the correlation should be approximately 0.
# In other words, there should not look like there is a relationship. 
# Ideally, this plot would not have a pattern where the red line
# is approximately horizontal at zero.
plot(SLR_model1, 1)
#Visualization shows partial aporoval of Residuals’ Independence 
#To confirm the Residuals’ Independence we can conduct 
# Durbin-Watson test called durbinWatsonTest on our model. 
#Ref:https://godatadrive.com/blog/basic-guide-to-test-assumptions-of-linear-regression-in-r
durbinWatsonTest(SLR_model1)
# p-value = 0.532 is more tha 0.05 hence, we should not reject the Null Hypothesis 
#The null hypothesis states that the errors are not auto-correlated with themselves(they are independent)

#3. Normality of residuals
plot(SLR_model1, 2)
#Residuals are approximately normally distributed means observations are distributed normally near the line.

#4. Equal variances of the residuals (Homoscedasticity)###
plot(SLR_model1, 3)
#Its is found that  residuals are randomly scattered around the red line with 
#roughly equal variability at all fitted values.

#5. Reporting the results of SLR-1#######
#All 4 assumptions were approved, and we can confirm that the fitted regression line is: 
#GDP_PP_EMP = 74866.4  + 434.9 * EMP_Service
#Now, we can use above equation to predict a GDP per Person Employed
#For a new sample with Service Employment population%  = 77.57063 of Denmark
#predicted GDP per Person Employed will be GDP_PP_EMP = =  74866.4  + 434.9 * 77.57063  = 1,08,601.866987
#predicted GDP per Person Employed will be: $1,08,601.87

#SLR.2 GDP_PP_EMP & EMP_Agriculture of Mid Income GRP#####
# SLR model, DV (Y, dependent variable, GDP_PP_EMP) is Numerical and
#  IV (X, independent variable, EMP_Agriculture)is Numerical.

#1.Loading the data####
EMP_GDP_MI <- emp_gdp_df1[emp_gdp_df1$Income_Grp=="Mid",c("EMP_Agriculture","EMP_Service","EMP_Industry","GDP_PP_EMP")]
EMP_GDP_MI

#2.Defining the objective
#SLR objective2: Finding SLR Equation between (Y)GDP_PP_EMP & (X)EMP_Agriculture of Mid Income GRP####
# Assessing Normality of both DV and IV
shapiro.test(EMP_GDP_MI$GDP_PP_EMP)
#p-value = 0.0003778 #Reject Null hypothesis & accept Null hypothesis that data(EMP_GDP_MI$GDP_PP_EMP) is Not Normal
hist(EMP_GDP_MI$GDP_PP_EMP)
#Visually also data distribution is Not Normal

shapiro.test(EMP_GDP_MI$EMP_Agriculture)
#p-value = 0.004303 #Reject Null hypothesis & accept Null hypothesis that data(EMP_GDP_MI$EMP_Agriculture) is Not Normal
#Not Normal
hist(EMP_GDP_MI$EMP_Agriculture)
#EMP_Agriculture is Visually also data distribution is Not Normal

#Checking the Relationship using the Correlation matrix between all variables(package: stats)
cor(EMP_GDP_MI)
#The result is a matrix of correlation values.

#Below is the Visualization of the a correlation matrix, using the corrplot package:
corrplot(cor(EMP_GDP_MI))  
#w.r.to GDP_PP_EMP (Y), 
#1)EMP_Agriculture has highest negative correlation  
#2)EMP_Service has moderate positive correlation
#3)EMP_Industry has high positive correlation 

#3.Performing SLR(Single linear regression)analysis for Mid income countries using FS method######
#Applying Forward Stepwise(FS) in SLR:
#Intent to write the formula option for Mid income Countries:
# Y = GDP_PP_EMP
# X = EMP_Agriculture
#(Y ~ X)
SLR_model2 <- lm(GDP_PP_EMP ~ EMP_Agriculture, EMP_GDP_MI) 
summary.lm(SLR_model2)

#Pr(>|t|) p-value of Intercept is 0.0000000000000002 and the Pr(>|t|) p-value of EMP_Service is  0.0000000000000192
#both are less then the threshold 0.05. In both cases, the p-values are very small(<0.05), suggesting strong 
# evidence against the null hypothesis that the true coefficient is zero

#Thus, Both coefficients (Intercept & EMP_Agriculture) are considered statistically significant and the  
#SLR2 equation will be#####
#GDP_PP_EMP = 78210.2  -1719.5 * EMP_Agriculture
# R-squared is 78.44% This means EMP_Agriculture ratio with this regression equation can 
#predict 78% of the entire variability in the GDP_PP_EMP

#4. Assessing the fitted model meets SLR's 4 assumptions#####
#1. Linearity: Assessing the relationship between X and Y must be linear using a scatterplot of x and y. 
#Scatter plot to visualise the fitted regression line  
plot(GDP_PP_EMP ~ EMP_Agriculture, EMP_GDP_MI, col = "blue", 
     main = "Regression: GDP per Person Employed & \n Agriculture Employment population%", 
     xlab = "Industry Employment population%", 
     ylab = "GDP per Person Employed")
#Then, adding the regression line to the plot:
abline(SLR_model2 , col="red")

#2. Residuals’ Independence
plot(SLR_model2, 1)
# Ideally, this plot does not have a pattern where the red line is approximately horizontal at zero.
#To confirm the Residuals’ Independence we can conduct 
# Durbin-Watson test called durbinWatsonTest on our model.  
durbinWatsonTest(SLR_model2)
# p-value = 0.438 is more tha 0.05 hence, we should not reject the Null Hypothesis 
#The null hypothesis states that the errors are not auto-correlated with themselves(they are independent)

#3. Normality of residuals
plot(SLR_model2, 2)
#Residuals are approximately normally distributed means observations are distributed normally near the line.

#4. Equal variances of the residuals (Homoscedasticity)###
plot(SLR_model2, 3)
#Its is found that  residuals are randomly scattered around the red line with 
#roughly equal variability at all fitted values.

#5. Reporting the resultseport the results#######
#All 4 assumptions were approved, and we can confirm that the fitted regression line is: 
#GDP_PP_EMP = 78210.2  -1719.5 * EMP_Agriculture
#Now, we can use above equation to predict a GDP per Person Employed
#For a new sample with Agriculture Employment population%  = 39.43818 of Kenya
#predicted GDP per Person Employed will be GDP_PP_EMP = 78210.2  -1719.5 * 39.43818 = 10,396.24949
#predicted GDP per Person Employed will be: $10,396.25


#SLR.3 GDP_PP_EMP & EMP_Agriculture of Low Income Group#####
# SLR model, DV (Y, dependent variable, GDP_PP_EMP) is Numerical and
# IV (X, independent variable, EMP_Agriculture)is Numerical.
#1.Loading the data####
EMP_GDP_LI <- emp_gdp_df1[emp_gdp_df1$Income_Grp=="Low",c("EMP_Agriculture","EMP_Service","EMP_Industry","GDP_PP_EMP")]

#2.Defining the objective
#SLR objective3: Finding SLR Equation between (Y)GDP_PP_EMP & (X)EMP_Agriculture of Low Income GRP####
# Assessing Normality of both DV and IV
shapiro.test(EMP_GDP_LI$GDP_PP_EMP)
#p-value = 0.04074 #Reject Null hypothesis & accept Null hypothesis that data(EMP_GDP_LI$GDP_PP_EMP) is Not Normal
hist(EMP_GDP_LI$GDP_PP_EMP)
#Visually also data distribution is Not Normal
#Analysis: DP_PP_EMP of Low income group Found Not Normal  

shapiro.test(EMP_GDP_LI$EMP_Agriculture)
# p-value = 0.2011 #Reject Null hypothesis & accept Null hypothesis that data(EMP_GDP_LI$EMP_Agriculture) is Not Normal
#Normal
hist(EMP_GDP_LI$EMP_Agriculture)
#EMP_Agriculture is Normal FOR LOW INCOMEGROUP COUNTRIES

#Checking the Relationship using the Correlation matrix 
#between all variables (used package: stats):
cor(EMP_GDP_LI)  
#The result is a matrix of correlation values.
#Below is the Visualization of the a correlation matrix, using the corrplot package:
corrplot(cor(EMP_GDP_LI))  
#w.r.to GDP_PP_EMP (Y), 
#1)There is moderate negative correlation with EMP_Agriculture
#2)There is moderate positive correlation with EMP_Service
#3)There is no correlation with EMP_Industry
# of Low income group countries

#3.Performing SLR(Single linear regression)analysis for Low income countries using FS method######
#Applying Forward Stepwise(FS) in SLR:
#Intent to write the formula option for Low income Countries:
# Y = GDP_PP_EMP
# X = EMP_Agriculture
#(Y ~ X)
SLR_model3 <- lm(GDP_PP_EMP ~ EMP_Agriculture, EMP_GDP_LI) 
summary.lm(SLR_model3)
#Pr(>|t|) p-value of Intercept is 0.00569 and the Pr(>|t|) p-value of EMP_Service is  0.11070
#here, Intercept is less then the threshold 0.05.
# In this case, p-value(0.00569) of Intercept  alone is very small(<0.05)
#But, p-value of EMP_Service is  (0.11070) is above threshold(>0.05)
# In conclusion, based on the p-value for EMP_Agriculture, the variable doesn't seem to be 
# statistically significant in predicting GDP_PP_EMP in this model. Therefore, caution is 
# needed in making strong interpretations or predictions based  on this specific regression model.

#SLR3 equation will be#####
#GDP_PP_EMP = 10700.15 -89.46 X EMP_Agriculture
#R-squared is 4% This means Employment Agriculture ratio(EMP_Agriculture) 
# with this regression equation can predict 4% of the entire variability in the GDP_PP_EMP

#4. Assessing the fitted model meets SLR's 4 assumptions#####
#1. Linearity: Assessing the relationship between X and Y must be linear using a scatterplot of x and y. 
#Scatter plot to visualize the fitted regression line  
plot(GDP_PP_EMP ~ EMP_Agriculture, EMP_GDP_LI, col = "blue", 
     main = "Regression: GDP per Person Employed & \n Agriculture Employment population%", 
     xlab = "Agriculture Employment population%", 
     ylab = "GDP per Person Employed")
#Then, adding the regression line to the plot:
abline(SLR_model3 , col="red")

#2. Residuals’ Independence
plot(SLR_model3, 1)
# Ideally, this plot does not have a pattern where the red line is approximately horizontal at zero.
#Visualization shows partial aporoval of Residuals’ Independence 
#To confirm the Residuals’ Independence we can conduct 
# Durbin-Watson test called durbinWatsonTest on our model. 
durbinWatsonTest(SLR_model3)
#p-value  0.85 is more tha 0.05 hence, we should not reject the Null Hypothesis 
#The null hypothesis states that the errors are not auto-correlated with themselves(they are independent)

#3. Normality of residuals
plot(SLR_model3, 2)
#Residuals are approximately normally distributed  means observations are distributed normally near the line.

#4. Equal variances of the residuals (Homoscedasticity)###
plot(SLR_model3, 3)
#Its is found that  residuals are randomly scattered around the red line with 
#roughly equal variability at all fitted values.

#5. Reporting the resultseport the results#######
#All 4 assumptions were approved, and we can confirm that the fitted regression line is: 
#GDP_PP_EMP = 10700.15 -89.46 * EMP_Agriculture
#Now, we can use above equation to predict a GDP per Person Employed
#For a new sample with Agriculture Employment population%  = 72.49886 of Ethiopia
#predicted GDP per Person Employed will be GDP_PP_EMP =  10700.15 -89.46 * 72.49886 = 4,214.4019844
#predicted GDP per Person Employed will be:$4,214.40

#4.4: Regression Analysis Test: Part Two: MLR####
#MLR-Objective: Finding a linear equation using MLR for GDP_PP_EMP & EMP_Service added with more IVs of High Income GRP #####
# MLR model, DV (Y, dependent variable, GDP_PP_EMP) is Numerical and
#  IVs (X, independent variables sucha as EMP_Agriculture EMP_Service and 
#EMP_Industry, EMP_Industry)are Numerical.

#1.Loading the data####
#Already loaded the data in the EMP_GDP_HI dataframe with required columns
#"EMP_Agriculture","EMP_Service","EMP_Industry","GDP_PP_EMP" of High income group Countries in SLR1-Objective.
#EMP_GDP_HI <- emp_gdp_df1[emp_gdp_df1$Income_Grp=="High",c("EMP_Agriculture","EMP_Service","EMP_Industry","GDP_PP_EMP")]

#2.Defining the objective
#Examining the possibility of linear relation between GDP per Person Employed and more than one 
#IVs of High Income GRP. Thereby extending the equation found in "SLR-Objective1"
#The independent variables are EMP_Agriculture EMP_Service and EMP_Industry. 
#Add each IV using forward selection to achieve the Linear equation for MLR
# checking normality of both DV and IVs is an additional check performed.
# Already checked the normality of DV(X= GDP_PP_EMP ) and one IV /(Y=EMP_Service) in SLR-Objective1
#using Shapiro.test() and visually using QQ plot histogram plot and it was Not Normal
#Normality check for other 2 IVs. EMP_Agriculture
shapiro.test(EMP_GDP_HI$EMP_Agriculture)
# p-value = 0.01291  #Reject Null hypothesis & accept Null hypothesis that 
#data(EMP_GDP_HI$EMP_Agriculture) is Not Normally distributed
hist(EMP_GDP_HI$EMP_Agriculture)
#Visually the data distribution of EMP_GDP_HI$EMP_Agriculture is Not Normal

#Normality check for EMP_Industry
shapiro.test(EMP_GDP_HI$EMP_Industry)
# p-value = 0.00001371 #Reject Null hypothesis & accept Null hypothesis that 
#data(EMP_GDP_HI$EMP_Industry) is Not Normally distributed
hist(EMP_GDP_HI$EMP_Industry)
#Visually the data distribution of EMP_GDP_HI$EMP_Agriculture is Not Normal

#Checking the Relationship using the Correlation matrix between all variables(package: stats):
cor(EMP_GDP_HI)
#The result is a matrix of correlation values.
#Visualization of the a correlation matrix, using the corrplot package:
corrplot(cor(EMP_GDP_HI), method = "number", type="lower",
         #col=brewer.pal(n=8, name="RdYlBu"),
         main = "Correlation Plot between Dependent & 3 Independent Variables")
#w.r.to GDP_PP_EMP (Y), 
#1)EMP_Agriculture has moderate positive correlation
#2)EMP_Service has moderate positive correlation 
#3)EMP_Industry has moderate negative correlation 

#3.Performing Multiple linear regression analysis using FS method######
# Existing SLR(SLR_model1) equation for the High income group Countries:
#GDP_PP_EMP = 74866.4  + 434.9 * EMP_Service
#Adding one more variable to the first SLR model(SLR_model1) to see the effect and built 
#the final MLR model step by step using forward stepwise
#Intent to write the formula option for High income Countries
# Y = GDP_PP_EMP,  X1 = EMP_Service,  X2 = EMP_Agriculture
#lm() function is used to fit the linear model including the multivariates
MLR_model1 <-lm(GDP_PP_EMP ~ EMP_Service + EMP_Agriculture, EMP_GDP_HI) 
summary.lm(MLR_model1)
# Interpretation of the above test result()
# All three coefficients (Intercept, EMP_Service, EMP_Agriculture) are statistically significant, 
# as indicated by the very small p-values (< 0.05). This suggests that the predictors 
# (EMP_Service and EMP_Agriculture) are contributing meaningfully to the model.
# The overall model is statistically significant, as indicated by the F-statistic with a very small p-value (0.0005359).
# The Adjusted R-squared value (0.2985) suggests that approximately 29.85% of the variability in 
#the response variable (GDP_PP_EMP) is explained by the predictors in the model.
# The final multiple linear regression equation based on the coefficients is:
#GDP_PP_EMP= 63021.7 + 525.2 * EMP_Service + 2025.2 * EMP_Agriculture with Adjusted R-squared is 29.85%

# By adding one more variable and fitting a Multiple Linear Regression  model we were able to get a 
# better model with higher prediction power and in the regression analysis has improved from  having
#Adjusted R-squared has improved from 13.59%  to R2 = 29.85% shows a good fit.
#Adding one more variable to the existing formula to build the final MLR model######
#Intent to write the formula option for High income Countries
# Y = GDP_PP_EMP,  X1 = EMP_Service,  X2 = EMP_Agriculture and X3 = EMP_Industry
MLR_model2 <-lm(GDP_PP_EMP ~ EMP_Service + EMP_Agriculture + EMP_Industry, EMP_GDP_HI) 
summary.lm(MLR_model2)
# All p-value are 0.167
#MLR Final equation will be#######
#GDP_PP_EMP = -5174915513  + 51750309 *EMP_Service + 51751973  * EMP_Agriculture +  51749770 * EMP_Industry 

# EMP_Agriculture  = 2.394497, EMP_Service = 77.57063, EMP_Industry = 20.03487
# GDP_PP_EMP = -5174915513  + 51750309 * 77.57063 + 51751973  * 2.394497 +  51749770 * 20.03487
# $1,08,417.397151
#Its because the 3 coefficients are the population percentage of the relevant sectors 
#such as agriculture, industry and service and hence the interception value is a huge negative to 
# leveraged with  the high values of the other 3 variables that returns a justifiable result

#4. Assessing the fitted model meets MLR's 5 assumptions#####
# 1. Linearity between Xs and Y
# 2. Residuals’ Independence
# 3. Normality of residuals
# 4. Equal variances of the residuals (Homoscedasticity)
# 5. No multicollinearity
#1. Linearity: The relationship between X and Y must be linear. Check 
#Linear relation is assessed using below scatterplot betw. DV(GDP_PP_EMP) and 3 IVs 
pairs(EMP_GDP_HI[,c(4,1,2,3)], lower.panel = NULL, pch = 15,cex = 0.7,
      main = "Scatterplot portraying Linear Relation \nbetween DV(GDP_PP_EMP) and 3 IVs " )

#2. Residuals’ Independence: Assessed using below plot
#Checking this assumption by examining a scatterplot of 
# “residuals versus fits”; the correlation should be approximately 0.
# In other words, there should not look like there is a relationship. 
# Ideally, this plot would not have a pattern where the red line
# is approximately horizontal at zero.
#Visualization suggests partial approval of Residuals’ Independence 
plot(MLR_model2, 1)
#Assumption is passed. 10years betw. 2011 to 2020. 
#Visualization suggests approval of Residuals’ Independence.
#counter verified by conducting R’s built-in function durbinWatsonTest() test using 
# output with a p-value, which will help you determine whether the assumption is met or not
durbinWatsonTest(MLR_model2)
#p-value  0.774 is more tha 0.05 hence, we should not reject the Null Hypothesis 
#The null hypothesis states that the errors are not auto-correlated with themselves(they are independent)

#3. Normality of residuals
plot(MLR_model2, 2)
#Residuals are approximately normally distributed means observations are distributed normally near the line

#4. Equal variances of the residuals (Homoscedasticity)###
plot(MLR_model2, 3)
#Residuals are randomly scattered around the red line with roughly equal variability at all fitted values

# 5. No multicollinearity variance inflation factor (VIF)
#Assessing the multicollinearity though VIF measures less than 5 by using the 
# vif() function on the model. vif() belongs to of Car package to see the VIF measures
vif(MLR_model2)
# Here, the VIF, measure is too high because the 
# A variance inflation factor (VIF) is a measure of the amount of multicollinearity in regression analysis. 
# Multicollinearity  exists when there is a correlation between multiple independent 
# variables in a multiple regression model. This can adversely affect the regression results.
# Because, these 3 IVs are the (EMP_Service, EMP_Agriculture and    EMP_Industry)
# makes the GDP per person employed hence it has very high multicollinearity

#6. Reporting the resultseport the results#######
#All 4 assumptions were approved, and we can confirm that the fitted regression line is: 
#Final MLR Equation is
#GDP_PP_EMP = -5174915513  + 51750309 *EMP_Service + 51751973  * EMP_Agriculture +  51749770 * EMP_Industry 
#Now, we can use above equation to predict a GDP per Person Employed. For a new sample
#predicted GDP per Person Employed will be
# EMP_Agriculture  = 2.394497
# EMP_Service = 77.57063
# EMP_Industry = 20.03487
# GDP_PP_EMP = -5174915513  + 51750309 * 77.57063 + 51751973  * 2.394497 +  51749770 * 20.03487
# $1,08,417.397151 against the actual $107,305.3198
#Regression Ends Here ########
