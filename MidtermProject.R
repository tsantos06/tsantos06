library(dplyr)
library(ggplot2)
hospitals = read.csv("hospitals.csv")

#how big is the dataset

dim(hospitals)

#What are the names of the columns

colnames(hospitals)

colDataTypes= sapply(hospitals, class)

cat(colDataTypes)

#Values missing?

missingvalues = any(is.na(hospitals))
print(missingvalues)
#False no values missing

# Hospital num 1064 and 1751 have lowest amount of beds, 3

LowNumBed=hospitals %>% filter(Total.Expense==min(Total.Expense))
print(LowNumBed)

#1,112 hospitals have babies being delivered

Babies = hospitals %>% filter(Births,or,Not==1)
count(Babies)

#scatter plot of beds and total expense

ggplot(hospitals, aes(x= beds, y=Total.Expense)) + geom_point()

#admissions and total expense?

ggplot(hospitals, aes(x= Admissions, y=Total.Expense)) + geom_point()

#beds and total expense for hospitals that have babies delivered

ggplot(Babies, aes(x=Beds, y=Total.Expense)) + geom_point()

#Relationship between amount of beds and total expense?

ggplot(hospitals, aes(x=Beds, y=Payroll.Expense)) + geom_point()

#2, descriptive analysis, pie chart

df = data.frame(
  AdTotExp = c("Admissions", "Total Expense"),
  Values = c(sum(hospitals$Admissions), sum(hospitals$"Total.Expense"))
)

df %>% ggplot(aes(x="", y=AdTotExp, fill=Values)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) +
  ggtitle("Pie Chart Admissions vs Total Expense")

#Bar Chart

AdCensus = sum(hospitals$Personnel)
print(AdCensus)
df2 =data.frame(
  category = c('Admissions', 'Personnel'),
  chart = c(sum(hospitals$Admissions), AdCensus)
)
ggplot(df2, aes(x=category, y=chart, fill=category)) + geom_bar(stat="identity")

#Line Chart

hospital.line = rank(hospitals$Total.Expense)
ggplot(hospitals, aes(x=hospital.line)) + geom_line(aes(y=Total.Expense))

#3, Simple Regression

SR = lm(Total.Expense ~ Beds, data=hospitals)
summary (SR)

#R^2 value = 0.6043

# R-squared value

rsquared <- summary(SR)$r.squared
print(paste("R-squared value:", rsquared))

# p-values
pvalues <- summary(SR)$coefficients[, 4]
print(paste("P-values:", pvalues))
#p-value = 0.0069

#4, Multivariate Regression

#multivariate linear regression
MR = lm(Total.Expense ~ Beds + Personnel, data = hospitals)

#summary of regression results
summary(MR)

#R-squared value
rsquared <- summary(MR)$r.squared
print(paste("R-squared value:", rsquared))

#p-values
pvalues <- summary(MR)$coefficients[, 4]
print(paste("P-values:", pvalues))

# Total expense vs admissions shows a Heteroscedastic trendline with many outliers 
#as the data points progress

#Payroll expense vs beds is aslo showing a heteroscedastic trendline with many
#outliers.

#The pie chart shows predominantly admissions over total expense meaning
#there needs to be more time spent on the total expense rather than admissions.
#also a larger hospital will be needed given that there is a high admission rate.

#The bar chart has admissions towering over personnel meaning the hospital
#needs more personnel to catch up to the amount of admission. Also because there
#is a higher admission rate, we can conclude a larger hospital will be needed.

#The simple regression output states the total expense of beds and the hospital
#size directly supports Option B because since the p-value is <0.05 we reject
#Option A.

#The multivariate regression output states the total expense of beds, personnel, and
# the hospital also directly supports Option B because since the p-value is <0.05,
#we reject Option A.
