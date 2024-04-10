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
#False, no values missing

LowNumBed <- hospitals %>% 
  filter(Total.Expense == min(Total.Expense))

print(LowNumBed)

Babies <- hospitals %>%
  filter(Births == 1 | Not == 1)

count(Babies)

#scatter plot of beds and total expense

ggplot(hospitals, aes(x= Beds, y=Total.Expense)) + geom_point()

#admissions and total expense?

ggplot(hospitals, aes(x= Admissions, y=Total.Expense)) + geom_point()

#beds and total expense for hospitals that have babies delivered

ggplot(Babies, aes(x=Beds, y=Total.Expense)) + geom_point()

#Relationship between amount of beds and total expense?

ggplot(hospitals, aes(x=Beds, y=Payroll.Expense)) + geom_point()

#2, descriptive analysis, pie chart

df = data.frame(
  AdTotExp = c("Admissions", "Personnel"),
  Values = c(sum(hospitals$Admissions), sum(hospitals$"Personnel"))
)

df %>% ggplot(aes(x="", y=AdTotExp, fill=Values)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) +
  ggtitle("Pie Chart Admissions vs Personnel")

#Bar Chart

AdCensus = sum(hospitals$Total.Expense)
print(AdCensus)
df2 =data.frame(
  category = c('Admissions', 'Total.Expense'),
  chart = c(sum(hospitals$Admissions), AdCensus)
)
ggplot(df2, aes(x=category, y=chart, fill=category)) + geom_bar(stat="identity")

#Line Chart

ggplot(hospitals, aes(x = Beds)) +
  geom_line(aes(y = Beds, color = "Beds")) +
  geom_line(aes(y = Personnel, color = "Personnel")) +  # Removed curly braces from "{Personnel}"
  labs(title = "Comparison of Beds and Personnel",
       x = "Beds",
       y = "Personnel") +
  scale_color_manual(values = c("Beds" = "blue", "Personnel" = "red")) +
  theme_minimal()

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

# Total expense vs beds shows a Heteroscedastic trendline with many outliers 
#as the data points progress

#Total expense vs admissions is also showing a heteroscedastic trendline with many
#outliers.

#Similarly again, payroll expense vs beds is showing a heteroscedastic trendline
#with many outliers

#The pie chart shows predominantly admissions over personnel meaning
#there needs to be more time spent on gathering teams together more so than 
#worrying about admissions. Also a larger hospital will be needed given that
#there is a high admission rate.

#The bar chart has total expense completely towering over admissions meaning the 
#hospital needs to lower the expenses because admissions cannot be controlled.

#The simple regression output states the total expense of beds and the hospital
#size directly supports Option B because since the p-value is <0.05 we reject
#Option A.

#The multivariate regression output states the total expense of beds, personnel, and
# the hospital also directly supports Option B because since the p-value is <0.05,
#we reject Option A.
