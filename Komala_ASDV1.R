#ASDV R file number:1 
#Contains the R code for Data Pre-processing followed by the implentation of Two tasks 4.1 and 4.2 
#on the Employment GDP data set that contains 6 indicators downloaded from World Data Bank
#=================================================================================================
#                  Main Objective
#===================================================================================================
#A Comparative Exploratory Analysis of the Employment factors influencing the GDP per person Employed
#between 3 income groups of Euro-African countries in 2011-2020
#===================================================================================================
#Objective of Task1: 1)A Comprehensive Statistical Exploratory Analysis on 
#variations of the Employment factors influencing the GDP per person Employed  
#between 3 diverse income groups of Euro-African countries in 2011-2020. 
#===================================================================================================

#Implementation of Data Preprocessing followed by Two tasks 4.1 and 4.2  
#Setting the working directory
#setwd("C:/ASDV/ASDV_Activity")
#Installing the Required Packages######
#The tidyverse is a collection of R packages including ggplot2, 
#dplyr, tidyr, and others. used for data manipulation, exploration, 
#and visualization. Especially, for using view() function
install.packages("tidyverse")

#ggplot2 for quickplot qplot()  quantile-quantile plots
install.packages("ggplot2") 

#gplots for heatmap 
install.packages("gplots")

#GGally extends the functionality of ggplot2 
#by adding several functions to reduce the complexity of 
#combining geoms with transformed data.
install.packages("GGally") 

# Mosaic for plotdist() function
install.packages('mosaic')

#dplyr package is for data manipulation operations 
#like select(), mutate(), filter(), summarise(), arrange()
install.packages("dplyr")

#moments is to calculate skewness, kurtosis
install.packages("moments") 

#corrplot is for corrplot() function
install.packages("corrplot") 

install.packages("rcompanion") 

#RColorBrewer is for brew.pal() function in corrplot 
install.packages(("RColorBrewer")) # 

#PerformanceAnalytics is for scatter plot for Correlation
install.packages("PerformanceAnalytics") 
#Ref.: https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/

#Loading the Required libraries using Library() into R session#####
library(tidyverse)
library(ggplot2) 
library(gplots) 
library(GGally)
library(mosaic)
library(dplyr) 
library(moments) 
library(corrplot) 
library(rcompanion)
library(RColorBrewer) 
library(PerformanceAnalytics)

#To prevent scientific notation in R programming
#By changing the global setting using options() method, by setting the scipen 
#argument, that is options(scipen = n)
options(scipen=999) 

#DATA PRE-PROCESSING#####
#READING THE DATA into Dataframe without header from employment_on_gdp.csv####
emp_gdp_df = read.csv("employment_on_gdp.csv", header=FALSE, skip=1)
head(emp_gdp_df)

#SETTING COLUMN NAMES#####
colnames(emp_gdp_df) = c("Year","Year_Code","Country","Country_Code","GDP_PP_EMP","EMP_Agriculture",
                         "EMP_Service",
                         "EMP_Ser_F","EMP_Ser_M","EMP_Industry")

#Shape of the Dataframe 
dim(emp_gdp_df)

#Overview of Dataframe 
#glimpse(emp_gdp_df)
#view(emp_gdp_df)

#CHECKING THE DATA TYPES OF COLUMNS IN THE DATASET####
str(emp_gdp_df)

#Dropping not required columns, Year_code and Country_code
emp_gdp_df <- subset(emp_gdp_df, select = -c(Year_Code,Country_Code))

head(emp_gdp_df)
dim(emp_gdp_df)
#[1] 120   8
#CHECKING THE MISSING VALUES#####
#Ref.: https://www.geeksforgeeks.org/data-preprocessing-in-r/
colSums(is.na(emp_gdp_df))
#There are totally 3 missing values. 
# each in EMP_Agriculture, EMP_Ser_F, EMP_Ser_M

#LISTING THE ROWS HAVING MISSING VALUES
emp_gdp_df[is.na(emp_gdp_df$EMP_Agriculture=="NA"),]  #Found at Kenya
emp_gdp_df[is.na(emp_gdp_df$EMP_Ser_F=="NA"),] #Found at Tanzania
emp_gdp_df[is.na(emp_gdp_df$EMP_Ser_M=="NA"),] #Found at France

#FIXING THE MISSING VALUE WITH MEDIAN OF RELEVANT Countries ###### 
#Finding the median for the 3 columns specific to relevant country
#Missing value of EMP_egriculture column appears in the Country="Kenya"
#finding median for Kenya Country on EMP_Agriculture column 
median_EMP_agri <- median(emp_gdp_df$EMP_Agriculture[emp_gdp_df$Country=="Kenya"], na.rm = TRUE) 
median_EMP_ser_f <- median(emp_gdp_df$EMP_Ser_F[emp_gdp_df$Country=="Tanzania"], na.rm = TRUE) 
median_EMP_ser_m <- median(emp_gdp_df$EMP_Ser_M[emp_gdp_df$Country=="France"], na.rm = TRUE) 

median_EMP_agri   #[1] 35.86827
median_EMP_ser_f   #[1] 27.27818
median_EMP_ser_m   #[1] 65.7376

#Replacing Missing values with medians of specific columns of respective countries
#Replacing Missing values with medians of EMP_Agriculture for Kenya
emp_gdp_df$EMP_Agriculture <- ifelse(is.na(emp_gdp_df$EMP_Agriculture) & emp_gdp_df$Country=="Kenya", 
                                     median_EMP_agri, emp_gdp_df$EMP_Agriculture)
#Replacing Missing values with medians of EMP_Ser_F for Tanzania
emp_gdp_df$EMP_Ser_F <- ifelse(is.na(emp_gdp_df$EMP_Ser_F) & emp_gdp_df$Country=="Tanzania", 
                               median_EMP_ser_f, emp_gdp_df$EMP_Ser_F)
#Replacing Missing values with medians of EMP_Ser_M for France
emp_gdp_df$EMP_Ser_M <- ifelse(is.na(emp_gdp_df$EMP_Ser_M) & emp_gdp_df$Country=="France", 
                               median_EMP_ser_m, emp_gdp_df$EMP_Ser_M)

#Verifying that there is no Missing Values
colSums(is.na(emp_gdp_df))

#DATA WRANGLING#########: Grouping the Countries on its Income basis
#group-by, filter, select, sort mutate
#Ref: https://bookdown.org/jgscott/DSGI/data-wrangling.html
#Creating a new column Income_group for Grouping Countries based
#on the Target value GDP_PP_EMP column
#Low < 7000
#Middle > 7000 < 100000
#High > 100000
emp_gdp_df$Income_Grp <- ifelse(emp_gdp_df$GDP_PP_EMP<7000,"Low", 
                                ifelse(emp_gdp_df$GDP_PP_EMP>7000 & emp_gdp_df$GDP_PP_EMP<100000, 
                                       "Mid","High"))
head(emp_gdp_df)

#To write a csv file for Task 4.3, 4.4 & 4.5
write.csv(emp_gdp_df, "employment_on_gdp_new.csv")
#CHECKING OUTLIERS##### 
#Over All Outlier observation of GDP Per Person Employed Data for all 10 countries
#Obtaining unique country names for Grouping the GDP values by Country for Boxplot 
unique_country = unique(emp_gdp_df$Country)
boxplot(GDP_PP_EMP~Country, data=emp_gdp_df,col=rainbow(8), 
        subset=Country %in% c(unique_country),
        cex.names = 0.7,cex.axis=0.7,las=2,
        main="Range of GDP per Worker$ for 12 Euro-African\n Countries across 10 years.")
#A slightly significant outlier exist on the Italy's GDP_PP_EMP Value

#Box plot display on GDP_PP_EMP for High income Countries
boxplot(GDP_PP_EMP~Country, data=emp_gdp_df,col=rainbow(8), 
        subset=Country %in% c(unique(emp_gdp_df$Country[emp_gdp_df$Income_Grp=="High"])),
        main="Range of GDP per Worker$ for 4 High Income \n Countries across 10 years.")

#Box plot display on GDP_PP_EMP for Mid income Countries
boxplot(GDP_PP_EMP~Country, data=emp_gdp_df,col=rainbow(8), 
        subset=Country %in% c(unique(emp_gdp_df$Country[emp_gdp_df$Income_Grp=="Mid"])),
        main="Range of GDP per Worker$ for 4 Mid Income \n Countries across 10 years.")

#Box plot display on GDP_PP_EMP for Low income Countries
boxplot(GDP_PP_EMP~Country, data=emp_gdp_df,col=rainbow(8), 
        subset=Country %in% c(unique(emp_gdp_df$Country[emp_gdp_df$Income_Grp=="Low"])),
        main="Range of GDP per Worker$ for 4 Low Income \n Countries across 10 years.")
 

#Outlier ObservAtion using Boxplot with qplot() function for All the Variable
qplot(Country,GDP_PP_EMP,data=emp_gdp_df, geom = "boxplot",fill=I("darkblue"),
      main = "Outlier Observations in GDP PErPerson on Specific \no12 Euro-African Countries")+
  theme(plot.title = element_text(size=14, face="bold"))

#Outliers ObservAtion on EMP_Agriculture using Boxplot with qplot() function 
qplot(Country,EMP_Agriculture,data=emp_gdp_df, geom = "boxplot",fill=I("green"),
      main = "Outliers in Agriculture Employment population % \non specific 12 Euro-African Countries")+
  theme(plot.title = element_text(size=14, face="bold"))
#For EMP_Agriculture: No outlier exist

#Outliers ObservAtion on EMP_Service using Boxplot with qplot() function 
qplot(Country,EMP_Service,data=emp_gdp_df, geom = "boxplot",fill=I("blue"),
      main = "Outlier Obs.in Service Employment population %  \non specific 12 Euro-African Countries")+
  theme(plot.title = element_text(size=14, face="bold"))
#For EMP_Service: Outliers exist only in Italy

#Outlier ObservAtion on EMP_Ser_M using Boxplot with qplot() function 
qplot(Country,EMP_Ser_M,data=emp_gdp_df, geom = "boxplot",fill=I("yellow"),
      main = "Outlier Obs.in Service Employment of Male population %  \n on specific 12 Euro-African Countries")+
  theme(plot.title = element_text(size=14, face="bold"))
#For EMP_Ser_M: Outliers exist in France, Italy and Uganda. As these countries got affected extemely in Covid

#Outlier ObservAtion on EMP_Ser_F using Boxplot with qplot() function 
qplot(Country,EMP_Ser_F,data=emp_gdp_df, geom = "boxplot",fill=I("purple"),
      main = "Outlier Obs. in Service Employment Female population % \n on specific 12 Euro-African Countries")+
  theme(plot.title = element_text(size=14, face="bold"))
#For EMP_Ser_F: Surprisingly No outliers unlike Male in Service(EMP_Ser_M)

#Outlier ObservAtion on EMP_Industry using Boxplot with qplot() function 
qplot(Country,EMP_Industry,data=emp_gdp_df, geom = "boxplot",fill=I("red"),
      main = "Outlier Obs. in Industry Employment population %  \n on specific 12 Euro-African Countries")+
  theme(plot.title = element_text(size=14, face="bold"))
#For EMP_Industry: Outliers exist only in Nigeria 


#DATA EXPLORATION ########
#Line Chart using qplot on GDP Indicator
#Ref:http://www.sthda.com/english/wiki/qplot-quick-plot-with-ggplot2-r-software-and-data-visualization
#LINE PLOT shows the trend of GDP Per Worker$ of all 12 Countries for 10 years
#unique_years = unique(emp_gdp_df$Year)
qplot(x= Year, y=GDP_PP_EMP, data=emp_gdp_df, geom="line",
      color = Country, 
      xlab="Year", ylab="GDP per Person Employed", 
      main = "Trend of GDP Per Worker$ for 12 Euro-African Countries betw. 2011-2020") + 
  geom_line(size = 1) +
  theme_minimal()

#Generating Line plots separately for better closer View on varying Trends on GDP per Worker$ for countries of varying Incomes across 10 yrs  
#Storing countriy names in 3 variables for 3 Income groups 
High_income_country <- emp_gdp_df[emp_gdp_df$Income_Grp=="High",]
Mid_income_country <- emp_gdp_df[emp_gdp_df$Income_Grp=="Mid",]
Low_income_country <- emp_gdp_df[emp_gdp_df$Income_Grp=="Low",]
#Line plot using qplot() on GDP Per Person Employed on High Income Country for better Closer View
qplot(x= Year, y=GDP_PP_EMP, data=High_income_country, geom="line",
      color = Country, 
      xlab="Year", ylab="GDP per Person Employed$", 
      main = "Varying Trends of GDP per Person Employed$ of \nHigh Income Countries:Denmark, France, Germany & Italy during 2011-2020") + 
  geom_line(size = 1) +
  theme_minimal()
#High Income group countries: faced severe down surge during covid-period 2019-2020  
#Especially, a sharp Nosedive for Italy and France

#Line plot  using qplot() on GDP Per Person Employed on Mid Income Country for better Closer View
qplot(x= Year, y=GDP_PP_EMP, data=Mid_income_country, geom="line",
      color = Country, 
      xlab="Year", ylab="GDP per Person Employed$", 
      main = "Varying Trends of GDP per Person Employed$ of \nMid Income Countries: Egypt, Kenya, Namibia & Nigeria during 2011-2020") + 
  geom_line(size = 1) +
  theme_minimal()
#Egypt has upward trend. namibia faces dowfall slightly more than Nigeria during pandemic
#Kenya's trend has no major change 

#Line plotusing qplot() on GDP Per PErson Employed on Low Income Country for better Closer View
qplot(x= Year, y=GDP_PP_EMP, data=Low_income_country, geom="line",
      color = Country, 
      xlab="Year", ylab="GDP per Person Employed$", 
      main = "Varying Trends of GDP per Person Employed$ of \nLow Income Countries: Ethiopia, Malawi, Tanzania & Uganda during 2011-2020") + 
  geom_line(size = 1) +
  theme_minimal()
#All have Consistant growth in GDP regardless of Covid pandemic period

#Trend of 5 (features)indicators
#Trend on Agriculture Employment population % for 12 countries (2011-2020)
qplot(x= Year, y=EMP_Agriculture, data=emp_gdp_df, geom="line",
      color = Country, 
      xlab="Year", ylab="Agriculture Employment population % ", 
      main = "Trend of Agriculture Employment population % \nfor 12 Euro-African Countries during 2011-2020") + 
  geom_line(size = 1) +
  theme_minimal()

#Trend on Service sector Employment population % for 12 countries (2011-2020)
qplot(x= Year, y=EMP_Service, data=emp_gdp_df, geom="line",
      color = Country, 
      xlab="Year", ylab="Service Sector Employment population % ", 
      main = "Trend of Service Sector Employment population % \nfor 12 Euro-African Countries during 2011-2020") + 
  geom_line(size = 1) +
  theme_minimal()

#Trend on Industrial sector Employment population % for 12 countries (2011-2020)
qplot(x= Year, y=EMP_Industry, data=emp_gdp_df, geom="line",
      color = Country, 
      xlab="Year", ylab="Industrial Sector Employment population %", 
      main = "Trend of Industrial Sector Employment population % \nfor 12 Euro-African Countries during 2011-2020") + 
  geom_line(size = 1) +
  theme_minimal()

#Trend on Male Employment population% in Service Sector for 12 countries (2011-2020)
qplot(x= Year, y=EMP_Ser_M, data=emp_gdp_df, geom="line",
      color = Country, 
      xlab="Year", ylab=" Male Employment Population% in Service Sector", 
      main = "Trend of Male Employment Population% in Service Sector\nfor 12 Euro-African Countries during 2011-2020") + 
  geom_line(size = 1) +
  theme_minimal()

#Trend on Female Employment population% in Service Sector for 12 countries (2011-2020)
qplot(x= Year, y=EMP_Ser_F, data=emp_gdp_df, geom="line",
      color = Country, 
      xlab="Year", ylab="Female Employment Population%", 
      main = "Trend of Female Employment Population% in Service Sector \nfor 12 Euro-African Countries during 2011-2020") + 
  geom_line(size = 1) +
  theme_minimal()

#EDA on 6 Indicators using Bar Chart for 2020 datasubset
emp_gdp_df_2020 <- emp_gdp_df[emp_gdp_df$Year==2020,]

#Barplot of GDP Per Person Employed for 12 countries in 2020 with Income grp. Specification
barplot(emp_gdp_df_2020$GDP_PP_EMP,names.arg=paste(emp_gdp_df_2020$Country,emp_gdp_df_2020$Income,sep="-"), 
        col=c(rainbow(length(unique(emp_gdp_df_2020$Country)))),
        xlab="Income range", ylab="GDP Per Person Employed$", 
        main="2020 GDP Per Person Employed$ of Europe and Africa\n(with Income grp. specification)",
        cex.names = 0.7,cex.axis=0.7,las=2, cex.main=0.8)

#Barplot of Agriculture Employment Population%  for 12 countries in 2020 
barplot(emp_gdp_df_2020$EMP_Agriculture,names.arg=paste(emp_gdp_df_2020$Country,emp_gdp_df_2020$Income,sep="-"), 
        col=c(rainbow(length(unique(emp_gdp_df_2020$Country)))),
        xlab="Income range", ylab="GDP Per Person Employed$", 
        main="Agriculture Employment Population% of Europe & Africa\nin 2020 (with Income grp. specification)",
        cex.names = 0.7,cex.axis=0.7,las=2, cex.main=0.8)

#Barplot of Service Sector Employment Population% for 12 countries in 2020 
barplot(emp_gdp_df_2020$EMP_Service,names.arg=paste(emp_gdp_df_2020$Country,emp_gdp_df_2020$Income,sep="-"), 
        col=c(rainbow(length(unique(emp_gdp_df_2020$Country)))),
        xlab="Income range", ylab="GDP Per Person Employed$", 
        main="Service Sector Employment Population% of Europe & Africa\nin 2020 (with Income grp. specification)",
        cex.names = 0.7,cex.axis=0.7,las=2, cex.main=0.8)

#Barplot of Industrial Sector Employment Population% for 12 countries in 2020 
barplot(emp_gdp_df_2020$EMP_Industry,names.arg=paste(emp_gdp_df_2020$Country,emp_gdp_df_2020$Income,sep="-"), 
        col=c(rainbow(length(unique(emp_gdp_df_2020$Country)))),
        xlab="Income range", ylab="GDP Per Person Employed$", 
        main="Industrial Sector Employment Population% of Europe & Africa\nin 2020 (with Income grp. specification)",
        cex.names = 0.7,cex.axis=0.7,las=2, cex.main=0.8)


# Setting up a 1x2 grid for side-by-side chart display
par(mfrow=c(1,2))  # Set up a 1x2 grid for side-by-side charts

#Barplot of Service Sector Male Employment Population% for 12 countries in 2020 
barplot(emp_gdp_df_2020$EMP_Ser_M,names.arg=paste(emp_gdp_df_2020$Country,emp_gdp_df_2020$Income,sep="-"), 
        col=c(rainbow(length(unique(emp_gdp_df_2020$Country)))),
        xlab="Income range", ylab="GDP Per Person Employed$", 
        main="Service Sector Male Employment Population% of Europe & Africa\nin 2020 (with Income grp. specification)",
        cex.names = 0.7,cex.axis=0.7,las=2, cex.main=0.8)

#Barplot of Service Sector Female Employment Population% for 12 countries in 2020 
barplot(emp_gdp_df_2020$EMP_Ser_F,names.arg=paste(emp_gdp_df_2020$Country,emp_gdp_df_2020$Income,sep="-"), 
        col=c(rainbow(length(unique(emp_gdp_df_2020$Country)))),
        xlab="Income range", ylab="GDP Per Person Employed$", 
        main="Service Sector Female Employment Population% of Europe & Africa\nin 2020 (with Income grp. specification)",
        cex.names = 0.7,cex.axis=0.7,las=2, cex.main=0.8)

#Implementing the comparision of GDP Per Worker$ between 2011 and 2020 using Piechart
pie(emp_gdp_df$GDP_PP_EMP[emp_gdp_df$Year==2011],labels =emp_gdp_df$Country,col=rainbow(10),
    main="GDP_PP_EMP 2011")
pie(emp_gdp_df$GDP_PP_EMP[emp_gdp_df$Year==2020],labels =emp_gdp_df$Country,col=rainbow(10),
    main="GDP_PP_EMP 2020")

#Resetting the grid to default (single chart at a time)
par(mfrow=c(1,1))

#Histograms showing the Data Frequency Distribution of all 6 indicators##########
hist(emp_gdp_df$GDP_PP_EMP,main="Data Frequency Distribution of\n GDP Per Worker% ",col="darkblue", border="black")
hist(emp_gdp_df$EMP_Agriculture,main="Data Frequency Distributionof \nAgriculture Employment population %",col="darkblue", border="black")
hist(emp_gdp_df$EMP_Service,main="Data Frequency Distributionof \nService Employment population %",col="darkblue", border="black")
hist(emp_gdp_df$EMP_Industry,main="Data Frequency Distribution of \nIndustry Employment population %",col="darkblue", border="black")
hist(emp_gdp_df$EMP_Ser_M,main="Data Frequency Distribution of \nMale Employment population % in Service sector",col="darkblue", border="black")
hist(emp_gdp_df$EMP_Ser_F,main="Data Frequency Distribution of \nFemale Employment population % in Service sector ",col="darkblue", border="black")

#4.1 Descriptive Statistical Analysis###### 
#Computing the Mean, Median, Mode, Standard deviation,Skewness and Kurtosis on the emp_gdp_df dataframe.
summary(emp_gdp_df)
#Country wise summarise on "Mean"value for all 6 Indicators  
countrywise_mean <- emp_gdp_df %>%
  group_by(Country) %>%
  summarise(across(c(GDP_PP_EMP, EMP_Agriculture, EMP_Service, EMP_Industry, EMP_Ser_F, EMP_Ser_M), list(mean = mean)))

#Income wise summarise on "Mean"value for all 6 Indicators
incomewise_mean <- emp_gdp_df %>%
  group_by(Income_Grp) %>%
  summarise(across(c(GDP_PP_EMP, EMP_Agriculture, EMP_Service, EMP_Industry, EMP_Ser_F, EMP_Ser_M), list(mean = mean)))

#Country wise summarise on "Median"value for all 6 Indicators
countrywise_median <- emp_gdp_df %>%
  group_by(Country) %>%
  summarise(across(c(GDP_PP_EMP, EMP_Agriculture, EMP_Service, EMP_Industry, EMP_Ser_F, EMP_Ser_M), list(median = median)))

#Income wise summarise on "Median"value for all 6 Indicators
incomewise_median <- emp_gdp_df %>%
  group_by(Income_Grp) %>%
  summarise(across(c(GDP_PP_EMP, EMP_Agriculture, EMP_Service, EMP_Industry, EMP_Ser_F, EMP_Ser_M), list(median = median)))

#Country wise summarise on "Standard Deviation"value for all 6 Indicators
countrywise_std <- emp_gdp_df %>%
  group_by(Country) %>%
  summarise(across(c(GDP_PP_EMP, EMP_Agriculture, EMP_Service, EMP_Industry, EMP_Ser_F, EMP_Ser_M), list(sd = sd )))

#Income wise summarise on  on "Standard Deviation"value for all 6 Indicators
incomewise_std <- emp_gdp_df %>%
  group_by(Income_Grp) %>%
  summarise(across(c(GDP_PP_EMP, EMP_Agriculture, EMP_Service, EMP_Industry, EMP_Ser_F, EMP_Ser_M), list(sd = sd )))

#Country wise summarise on "Skewness"value for all 6 Indicators
countrywise_skewness <- emp_gdp_df %>%
  group_by(Country) %>%
  summarise(across(c(GDP_PP_EMP, EMP_Agriculture, EMP_Service, EMP_Industry, EMP_Ser_F, EMP_Ser_M), list(skewness = skewness )))

#Income wise summarise on "Skewness"value for all 6 Indicators
incomewise_skewness <- emp_gdp_df %>%
  group_by(Income_Grp) %>%
  summarise(across(c(GDP_PP_EMP, EMP_Agriculture, EMP_Service, EMP_Industry, EMP_Ser_F, EMP_Ser_M), list(skewness = skewness )))

#Country wise summarise on "Kurtosis"value for all 6 Indicators
countrywise_kurtosis <- emp_gdp_df %>%
  group_by(Country) %>%
  summarise(across(c(GDP_PP_EMP, EMP_Agriculture, EMP_Service, EMP_Industry, EMP_Ser_F, EMP_Ser_M), list(kurtosis = kurtosis)))

#Income wise summarise on "Kurtosis"value for all 6 Indicators
incomewise_kurtosis <- emp_gdp_df %>%
  group_by(Income_Grp) %>%
  summarise(across(c(GDP_PP_EMP, EMP_Agriculture, EMP_Service, EMP_Industry, EMP_Ser_F, EMP_Ser_M), list(kurtosis = kurtosis)))

#Viewing the result of computations
view(countrywise_mean)
view(countrywise_median)
view(countrywise_std)
view(countrywise_skewness)
view( countrywise_kurtosis)

view(incomewise_mean )
view(incomewise_median )
view(incomewise_std )
view(incomewise_skewness)
view(incomewise_kurtosis)

#In genral, Summary() method summarizes the overall mean, median, min and max only, but not skewness & Kurtosis 
#Hence, Calculating the Mean, Median, Mode, Standard deviation, Skewness and Kurtosis using a for-loop
#Storing Unique country names in ascending order grouped by Income in the order of High, Mid & Low 
unique_Hcountry = sort( unique(emp_gdp_df$Country[emp_gdp_df$Income_Grp=="High"]))
unique_Mcountry =sort( unique(emp_gdp_df$Country[emp_gdp_df$Income_Grp=="Mid"]))
unique_Lcountry = sort(unique(emp_gdp_df$Country[emp_gdp_df$Income_Grp=="Low"]))
##Joining the Countries in the order of 1)High, then 2)Mid and finally 3)Low
unique_ALLcountry = union(unique_Hcountry,unique_Mcountry)
unique_ALLcountry = union(unique_ALLcountry, unique_Lcountry)
#Countries in the order of High, Mid , Low
unique_ALLcountry
#ref: https://rdrr.io/cran/fastGraph/man/plotDist.html#:~:text=See%20Also%20Examples-,Description,be%20specified%20by%20the%20user.
#For loop to compute countrywise mean, median, st.dv, skewness & Kurtosis#############
#Below for loop coded to compute the countrywise mean, median, mode, range, variance, 
#standard deviation, skewness and kurtosis  on all indicators.
#Also plots 1) histogram and #2)Normal QQ plot using qqnorm with a qqline 
#to display the theoritical data distribution across a 45 degree reference line.
print("Descriptive0 Statistical Analysis: ")
print("Mean, Standard Deviation,Skewness & Kurtosis for High, Medium & Low Income group Countries")
i <- 0
#num<- numeric(0)
for (country in unique_ALLcountry)
{
  i <- i + 1
  if (i == 1 )
  {
    print("Mean, Standard Deviation,Skewness & Kurtosis for High Income group Countries")
    
  } else if (i == 5 )
  {
    print("Mean, Standard Deviation,Skewness & Kurtosis for Medium Income group Countries")
    
  } else if (i == 9 )
  {
    print("Mean, Standard Deviation,Skewness & Kurtosis for Low Income group Countries")
    
  }
  #GDP_PP_EMP
  subset_gdpemp <- emp_gdp_df$GDP_PP_EMP[emp_gdp_df$Country== country]
  cat("Country: ", country," \n" )
  cat("GDP_PP_EMP: \n")
  #Mean
  mean_gdp <- mean(subset_gdpemp)
  median_gdp <- median(subset_gdpemp)
  mode_gdp <- mode(subset_gdpemp)
  range_gdp <- range(subset_gdpemp)
  var_gdp <- var(subset_gdpemp)
  sd_gdp <- sd(subset_gdpemp)
  
  #Skewness & Kurtosis
  skew_gdp <- skewness(emp_gdp_df$GDP_PP_EMP[emp_gdp_df$Country==country])
  kurt_gdp <- kurtosis(emp_gdp_df$GDP_PP_EMP[emp_gdp_df$Country==country])
  cat("Mean: ",mean_gdp,", Median: ",median_gdp,", Mode: ",mode_gdp,
      ", ValueRange: ",range_gdp,", Variance: ",var_gdp, "\n")
  cat("Standard Deviation: ",sd_gdp,", Skewness: ",skew_gdp,", Kurtosis: ",kurt_gdp," \n\n")
  
  #GDP_PP_EMP: Histogram for Assessing normality with Density plot & Kurtosis Line
  hist(subset_gdpemp, main = paste("Assessing Normality with Density plot & Kurtosis Line\nfor GDP_PP_EMP of ", country), 
       col = "lightblue", border = "black", probability = TRUE)
  # Here, probability = TRUE to see as density. We can exclude this to see frequency 
  # Overlaying a Density plot on the histogram
  lines(density(subset_gdpemp), col = "blue", lty = 2, lwd = 2)
  
  #Assessing the Theoritical Distribution that is normal Distribution
  qqnorm(subset_gdpemp, pch = 1, frame = FALSE, 
         main=paste("Assessing Normality using Q-Q Plot\nfor GDP_PP_EMP of ", country))
  qqline(subset_gdpemp, col = "steelblue", lwd = 2)
  
  #EMP_Agriculture
  subset_emp_agri <- emp_gdp_df$EMP_Agriculture[emp_gdp_df$Country== country]
  cat("EMP_Agriculture: \n")
  #Mean
  mean_agri <- mean(subset_emp_agri)
  median_agri <- median(subset_emp_agri)
  mode_agri <- mode(subset_emp_agri)
  range_agri <- range(subset_emp_agri)
  var_agri <- var(subset_emp_agri)
  sd_agri <- sd(subset_emp_agri)
  
  #Skewness & Kurtosis
  skew_agri <- skewness(subset_emp_agri)
  kurt_agri <- kurtosis(subset_emp_agri)
  cat("Mean: ",mean_agri,", Median: ",median_agri,", Mode: ",mode_agri,
      ", ValueRange: ",range_agri,", Variance: ",var_agri, "\n")
  cat("Standard Deviation: ",sd_agri,", Skewness: ",skew_agri,", Kurtosis: ",kurt_agri," \n\n")
  
  #Emp_Agriculture: Histogram for Assessing normality with Density plot & Kurtosis Line
  hist(subset_emp_agri, main = paste("Assessing Normality with Density plot & Kurtosis Line\nfor Emp_Agriculture of ", country), 
       col = "lightblue", border = "black", probability = TRUE)
  # Here, probability = TRUE to see as density. We can exclude this to see frequency 
  # Overlaying a Density plot on the histogram
  lines(density(subset_emp_agri), col = "blue", lty = 2, lwd = 2)
  
  #Assessing the Theoritical Distribution that is normal Distribution
  qqnorm(subset_emp_agri, pch = 1, frame = FALSE, 
         main=paste("Assessing Normality using Q-Q Plot\nfor EMP_Agriculture of ", country))
  qqline(subset_emp_agri, col = "steelblue", lwd = 2)
  
  #EMP_Service
  subset_emp_ser <- emp_gdp_df$EMP_Service[emp_gdp_df$Country== country]
  cat("EMP_Service: \n")
  #Mean
  mean_ser <- mean(subset_emp_ser)
  median_ser <- median(subset_emp_ser)
  mode_ser <- mode(subset_emp_ser)
  range_ser <- range(subset_emp_ser)
  var_ser <- var(subset_emp_ser)
  sd_ser <- sd(subset_emp_ser)
  #Skewness & Kurtosis
  skew_ser <- skewness(subset_emp_ser)
  kurt_ser <- kurtosis(subset_emp_ser)
  cat("Mean: ",mean_ser,", Median: ",median_ser,", Mode: ",mode_ser,
      ", ValueRange: ",range_ser,", Variance: ",var_ser, "\n")
  cat("Standard Deviation: ",sd_ser,", Skewness: ",skew_ser,", Kurtosis: ",kurt_ser," \n\n")
  
  #Emp_Service : Histogram for Assessing normality with Density plot & Kurtosis Line
  hist(subset_emp_ser, main = paste("Assessing Normality with Density plot & Kurtosis Line\nfor Emp_Service of ", country), 
       col = "lightblue", border = "black", probability = TRUE)
  # Here, probability = TRUE to see as density. We can exclude this to see frequency 
  # Overlaying a Density plot on the histogram
  lines(density(subset_emp_ser), col = "blue", lty = 2, lwd = 2)
  
  #Assessing the Theoritical Distribution that is normal Distribution
  qqnorm(subset_emp_ser, pch = 1, frame = FALSE, 
         main=paste("Assessing Normality using Q-Q Plot\nfor Emp_Service of ",country))
  qqline(subset_emp_ser, col = "steelblue", lwd = 2)
  
  #EMP_Industry
  subset_emp_ind <- emp_gdp_df$EMP_Industry[emp_gdp_df$Country== country]
  cat("EMP_Industry: \n")
  #Mean
  mean_ind <- mean(subset_emp_ind)
  median_ind  <- median(subset_emp_ind)
  mode_ind  <- mode(subset_emp_ind)
  range_ind <- range(subset_emp_ind)
  var_ind <- var(subset_emp_ind)
  sd_ind <- sd(subset_emp_ind)
  #Skewness & Kurtosis
  skew_ind <- skewness(subset_emp_ind)
  kurt_ind <- kurtosis(subset_emp_ind)
  cat("Mean: ",mean_ind,", Median: ",median_ind,", Mode: ",mode_ind,
      ", ValueRange: ",range_ind,", Variance: ",var_ind, "\n")
  cat("Standard Deviation: ",sd_ind,", Skewness: ",skew_ind,", Kurtosis: ",kurt_ind," \n\n")
  
  #Emp_Industry : Histogram for Assessing normality with Density plot & Kurtosis Line
  hist(subset_emp_ind, main = paste("Assessing Normality with Density plot & Kurtosis Line\nfor Emp_Industry of ", country), 
       col = "lightblue", border = "black", probability = TRUE)
  # Here, probability = TRUE to see as density. We can exclude this to see frequency 
  # Overlaying a Density plot on the histogram
  lines(density(subset_emp_ind), col = "blue", lty = 2, lwd = 2)
  
  #Assessing the Theoritical Distribution that is normal Distribution
  qqnorm(subset_emp_ind, pch = 1, frame = FALSE, 
         main=paste("Assessing Normality using Q-Q Plot\nfor Emp_Industry of ",country))
  qqline(subset_emp_ind, col = "steelblue", lwd = 2)
  
  #EMP_Ser_M
  subset_emp_ser_M <- emp_gdp_df$EMP_Ser_M[emp_gdp_df$Country== country]
  cat("EMP_Ser_M: \n")
  #Mean
  mean_ser_m <- mean(subset_emp_ser_M)
  median_ser_m  <- median(subset_emp_ser_M)
  mode_ser_m  <- mode(subset_emp_ser_M)
  range_ser_m <- range(subset_emp_ser_M)
  var_ser_m <- var(subset_emp_ser_M)
  sd_ser_m <- sd(subset_emp_ser_M)
  #Skewness & Kurtosis
  skew_ser_m <- skewness(subset_emp_ser_M)
  kurt_ser_m <- kurtosis(subset_emp_ser_M)
  cat("Mean: ",mean_ser_m,", Median: ",median_ser_m,", Mode: ",mode_ser_m,
      ", ValueRange: ",range_ser_m,", Variance: ",var_ser_m, "\n")
  cat("Standard Deviation: ",sd_ser_m,", Skewness: ",skew_ser_m,", Kurtosis: ",kurt_ser_m," \n\n")
  
  #Emp_Ser_M : Histogram for Assessing normality with Density plot & Kurtosis Line
  hist(subset_emp_ser_M, main = paste("Assessing Normality with Density plot & Kurtosis Line\nfor Emp_Ser_Male of ", country), 
       col = "lightblue", border = "black", probability = TRUE)
  # Here, probability = TRUE to see as density. We can exclude this to see frequency 
  # Overlaying a Density plot on the histogram
  lines(density(subset_emp_ser_M), col = "blue", lty = 2, lwd = 2)
  
  #Assessing the Theoritical Distribution that is normal Distribution
  qqnorm(subset_emp_ser_M, pch = 1, frame = FALSE, 
         main=paste("Assessing Normality using Q-Q Plot\nfor Emp_Ser_M of ",country))
  qqline(subset_emp_ser_M, col = "steelblue", lwd = 2)
  
  #EMP_Ser_F
  subset_emp_ser_F <- emp_gdp_df$EMP_Ser_F[emp_gdp_df$Country== country]
  cat("EMP_Ser_F: \n")
  #Mean
  mean_ser_f <- mean(subset_emp_ser_F)
  median_ser_f  <- median(subset_emp_ser_F)
  mode_ser_f  <- mode(subset_emp_ser_F)
  range_ser_f <- range(subset_emp_ser_F)
  var_ser_f <- var(subset_emp_ser_F)
  sd_ser_f <- sd(subset_emp_ser_F)
  #Skewness & Kurtosis
  skew_ser_f <- skewness(subset_emp_ser_F)
  kurt_ser_f <- kurtosis(subset_emp_ser_F)
  cat("Mean: ",mean_ser_f,", Median: ",median_ser_f,", Mode: ",mode_ser_f,
      ", ValueRange: ",range_ser_f,", Variance: ",var_ser_f, "\n")
  cat("Standard Deviation: ",sd_ser_f,", Skewness: ",skew_ser_f,", Kurtosis: ",kurt_ser_f," \n\n")
  
  #Emp_Ser_F : Histogram for Assessing normality with Density plot & Kurtosis Line
  hist(subset_emp_ser_F, main = paste("Assessing Normality with Density plot & Kurtosis Line\nfor Emp_Ser_Female of ", country), 
       col = "lightblue", border = "black", probability = TRUE)
  # Here, probability = TRUE to see as density. We can exclude this to see frequency 
  # Overlaying a Density plot on the histogram
  lines(density(subset_emp_ser_F), col = "blue", lty = 2, lwd = 2)
  
  #Assessing the Theoritical Distribution that is normal Distribution
  qqnorm(subset_emp_ser_F, pch = 1, frame = FALSE, 
         main=paste("Assessing Normality using Q-Q Plot\nfor Emp_Ser_F of ",country))
  qqline(subset_emp_ser_F, col = "steelblue", lwd = 2)
  
}
#For loop ends here##########
#Almost all plots shows the Bell-shapped curve except GDP of Italy, Industry population% of Nigeria
#Thus, Almost all indicators of all the countries are roughly normally distributed near to a bell-shaped curve


#4.2 Correlation Analysis for the Indicators#########
# SubtaskA: Correlation Visualization Between 7 indicators using chart.Correlation() function ####
#There are 6 Features:  EMP_Agriculture, EMP_Service, EMP_Ser_F,EMP_Ser_M, EMP_Industry and Income_Grp
#Except, Income_Grp rest are Numerical Features. Income_Grp is categorical
#The Target Label is GDP_PP_EMP

#Converting the Income Group (Categorical) Column into Numeric 
#inorder to Plot Correlation and to perform Correlation Test
emp_gdp_df$Income_Grp <- as.numeric(factor(emp_gdp_df$Income_Grp))

#Selecting only the 7 indicators by eliminating the Year and Country columns
continuous_vars <- emp_gdp_df %>%  
  select(-Year, -Country ) 
head(continuous_vars, 5)
#Following Computes Correlation Matrix gives an overview of the correlations for 
#all combinations of 6 continuous variables.
round(cor(continuous_vars), digits=2)

# Visualization of Correlation matrix 
corrplot(cor(continuous_vars), method = "number", type="lower",
         col=brewer.pal(n=8, name="RdYlBu"),
         main = "Correlation Plot between Target and 6 Features")

#Scatterplot view for better clarity. Using the PerformanceAnalytics library
chart.Correlation(continuous_vars)

str(emp_gdp_df)

#SubtaskB: Individual Correlation test & Scatterplot Visualization ######
#Between Target(GDP_PP_EMP) and 6 features(EMP_Agriculture, EMP_Service, EMP_Ser_F, EMP_Ser_M, EMP_Industry)
#Correlation Test Between GDP per Person Employed and Agriculture Employment population%
plot(emp_gdp_df$EMP_Agriculture, emp_gdp_df$GDP_PP_EMP,
     main="Scatterplot between GDP per Person Employed and \n Agriculture Employment population%",
     xlab="Agriculture Employment Population%",
     ylab="GDP per Person Employed")
cor.test(emp_gdp_df$GDP_PP_EMP, emp_gdp_df$EMP_Agriculture )

#Correlation Test Between GDP per Person Employed and Service Sector Employment population%
plot(emp_gdp_df$EMP_Service, emp_gdp_df$GDP_PP_EMP,
     main="Scatterplot between GDP per Person Employed and \n Service Sector Employment population%",
     xlab="Service Sector Employment population%",
     ylab="GDP per Person Employed")
cor.test(emp_gdp_df$GDP_PP_EMP, emp_gdp_df$EMP_Service )

#Correlation Test Between GDP per Person Employed and Industrial Sector Employment population%
plot(emp_gdp_df$EMP_Industry, emp_gdp_df$GDP_PP_EMP, 
     main="Scatterplot between GDP per Person Employed and \n Industrial Sector Employment population%",
     xlab="Industrial Employment population%",
     ylab="GDP per Person Employed")
cor.test(emp_gdp_df$GDP_PP_EMP, emp_gdp_df$EMP_Industry )
 
#Correlation Test Between GDP per Person Employed and Males Employment population% in Service Sector 
plot(emp_gdp_df$EMP_Ser_M,emp_gdp_df$GDP_PP_EMP, 
     main="Scatterplot between GDP per Person Employed and \n 
     Males Employment population% in Service Sector",
     xlab="Males Employment population% in Service",
     ylab="GDP per Person Employed")
cor.test(emp_gdp_df$GDP_PP_EMP, emp_gdp_df$EMP_Ser_M)


#Correlation Test Between GDP per Person Employed and Females Employment population% in Service Sector 
plot(emp_gdp_df$EMP_Ser_F,emp_gdp_df$GDP_PP_EMP, 
     main="Scatterplot between GDP per Person Employed and \n 
     Females Employment population% in Service Sector",
     xlab="Females Employment population% in Service",
     ylab="GDP per Person Employed")
cor.test(emp_gdp_df$GDP_PP_EMP, emp_gdp_df$EMP_Ser_F)

#Correlation Test Between GDP per Person Employed and  Income_Grp
plot(emp_gdp_df$Income_Grp, emp_gdp_df$GDP_PP_EMP, 
     main="Scatterplot between GDP per Person Employed and
      Income group(High, Mid, Low) of Countries",
     xlab="Income group of Countries",
     ylab="GDP per Person Employed")
cor.test(emp_gdp_df$GDP_PP_EMP, emp_gdp_df$Income_Grp )


#Correlation Tests Ends here######

