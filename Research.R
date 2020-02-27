# NYC Bicycle Ride Counts on Bridges from April 2016 to October 2016
# Remove global variables
# rm(list = ls())
# library(MASS)
# library(stats)
# library(jtools)
# library(ggplot2)
# library(AER)

# File is the path of the csv file
ds = read.csv(file = "C:/Users/alcir/OneDrive/Documents/R/Research-Project/nyc-east-river-bicycle-counts2016.csv")

# Clean Up Data and rearrange it in the format
ds$Date = as.Date(ds$Date, format("%m/%d/%Y"))

# columnsRemove = c("Date", "Day")
# indexOfColsRem = which(colnames(ds) %in% columnsRemove)
# ds = ds[, -indexOfColsRem]

#Change column names
colnames(ds)[3] = c("High Temp")
colnames(ds)[4] = c("Low Temp")
colnames(ds)[6:9] = c("Brooklyn_Bridge",
                      "Manhattan_Bridge",
                      "Williamsburg_Bridge",
                      "Queensboro_Bridge")
# T means traces of precipitation and S for snowing
ds = ds[ds$Precipitation != 'T',]
ds = ds[ds$Precipitation != '0.47 (S)',]
ds$Precipitation = factor(ds$Precipitation)
# Some reason treating values in columns as factors
# Change into numerical for convenience for now
ds$Precipitation = as.numeric(as.character(ds$Precipitation))
ds$Brooklyn_Bridge = gsub(",", "", ds$Brooklyn_Bridge)
Bridges = colnames(ds)[6:10]
ds[Bridges] = lapply(ds[Bridges], gsub, pattern = ",", replacement = "")

# Change factors to numerical, for some reason occurred and
# transformed them redundantly
ds$Brooklyn_Bridge = as.numeric(as.character(ds$Brooklyn_Bridge))
ds$Manhattan_Bridge = as.numeric(as.character(ds$Manhattan_Bridge))
ds$Williamsburg_Bridge = as.numeric(as.character(ds$Williamsburg_Bridge))
ds$Queensboro_Bridge = as.numeric(as.character(ds$Queensboro_Bridge))
ds$Total = as.numeric(as.character(ds$Total))
rownames(ds) = 1:nrow(ds)

# Plot the total amount of bicycles over the year 2016
ggplot(ds, aes(x = ds$Date, y = ds$Total),) + geom_bar(stat = "identity",fill = "royalblue") + xlab("Months") + ylab("Total Amount") + ggtitle("Total Amount of Bicycles Per Day Across NYC Bridges in 2016") + theme_minimal()

# Get independent variables
Low_Temp = ds$`Low Temp`
High_Temp = ds$`High Temp`
Precipitation = ds$Precipitation
# Bridges (dependent variables)
Brooklyn = ds$Brooklyn_Bridge
Manhattan = ds$Manhattan_Bridge
Williamsburg = ds$Williamsburg_Bridge
Queensboro = ds$Queensboro_Bridge

# Test for Brooklyn Bridge Analysis
Brk_Poisson_M = glm(
  formula = Brooklyn ~ High_Temp + Low_Temp + Precipitation,
  data = ds ,
  family = poisson
)
Brk_QuasiPoisson_M = glm(
  formula = Brooklyn ~ High_Temp + Low_Temp + Precipitation,
  data = ds ,
  family = quasipoisson
)
Brk_NegativeBinomial_M = glm.nb(formula = Brooklyn ~ High_Temp + Low_Temp + Precipitation,
                                data = ds)

dispersiontest(Brk_Poisson_M)
# mean and variance of the dependent variable aka count on x bridge
mean(Brooklyn)
var(Brooklyn)
summary(Brk_Poisson_M)
summary(Brk_QuasiPoisson_M)
summary(Brk_NegativeBinomial_M)

# Do other models since variance is greater than the mean
models_Poiss = lapply(6:10, function(x) glm(formula = ds[,x] ~ High_Temp + Low_Temp + Precipitation, data = ds, family = poisson) )
models_QuasiPoiss = lapply(6:10, function(x) glm(formula = ds[,x] ~ High_Temp + Low_Temp + Precipitation, data = ds, family = quasipoisson) )
models_NegBin = lapply(6:10, function(x) glm.nb(formula = ds[,x] ~ High_Temp + Low_Temp + Precipitation, data = ds) )

summaries = lapply(models_NegBin, summary)

# Get the AICs ot compare the models
AIC_Poiss = sapply(models_Poiss, AIC)
AIC_Quasi_Poiss = sapply(models_QuasiPoiss,AIC)
AIC_NegBin = sapply(models_NegBin, AIC)


AICs = c(AIC_Poiss,AIC_NegBin)
AICs = data.frame(AICs)
AICs$Bridge = rep(c("Brooklyn","Manhattan","Williamsburg","Queensboro","Total"),2)
AICs$Distribution = c(rep("Poisson",each=5),rep("Negative-Bin",each=5))
#Since quasi-poisson model can't be measured with AIC
# will only model with Negative Binomial Models

plot_Poiss = ggplot(AICs,aes(Bridge,AICs,fill=Distribution)) + geom_bar(stat = "identity",width = 0.75,position = position_dodge()) + theme_minimal() + theme(axis.text.x = element_text(angle = 90,hjust = 1))
plot_Poiss + labs(title = "Distribution of AIC for Poisson vs Negative Binomial Models",y="AIC")

