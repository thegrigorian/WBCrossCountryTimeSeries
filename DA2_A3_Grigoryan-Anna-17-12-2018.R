rm(list=ls())
##############################################################
# LOADING REQUIRED PACKAGES
library(wbstats)
library(data.table)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(lspline)
library(countrycode)
library(gridExtra)
library(stargazer)
library(scales)
library(lubridate)
library(DataCombine)
library(sandwich)
library(dyn) 
library(lmtest)


##############################################################
# CHANGE IT TO YOUR WORKING DIRECTORY
dir <-  "/Users/user/Desktop/DA2/"
data_out <- paste0(dir,"textbook_work/hw3/")
func <- paste0(dir, "textbook_work/ch00_tech_prep/")
output <- paste0(dir,"textbook_work/hw3/output/")

# Call theme function
source(paste0(func, "theme_bg.R"))

# Created a helper function with some useful stuff
source(paste0(func, "da_helper_functions.R")) 
options(digits = 3) 

##############################################################
# LOAD THE DATA FROM THE WEB

gdppc <- wb(indicator = "NY.GDP.PCAP.PP.KD", country = "NOR") # taking Norway as country
lifeexp <- wb(indicator = "SP.DYN.LE00.IN", country = "NOR") # there are more years for life expectancy

# Saving GDP per capita in gdppc and life expectancy in life_exp
gdppc <- as.data.table(gdppc)
lifeexp <- as.data.table(lifeexp)
lifeexp <- lifeexp[, life_exp := value, ]
lifeexp <- lifeexp[, .(date, life_exp), ]

# Merge the two in data
data <- merge(gdppc, lifeexp, by = 'date')
data <- data[, .(value, date, country, life_exp), ]

# Tidy the variables
colnames(data) <- c("gdp", "year", "country", "life_exp")
data$year <- paste0(data$year, "-12-31")
data$year <- ymd(data$year)
data[, ln_gdp := log(gdp), ]
data <- as.data.frame(data)

# Create the first differences for ln_gdp and life_exp
data <- change(data, Var = "ln_gdp", slideBy = -1,  type = "absolute", NewVar = "Diff_ln_gdp")
data <- change(data, Var = "life_exp", slideBy = -1,  type = "absolute", NewVar = "Diff_life_exp")

# Save the life expectancy and GDP per capita variables in ts format
data$life_exp <- ts(data$life_exp, start = 1990, frequency = 1)
data$Diff_life_exp <- ts(data$Diff_life_exp, start = 1990, frequency = 1)
data$ln_gdp <- ts(data$ln_gdp, start = 1990, frequency = 1)
data$Diff_ln_gdp <- ts(data$Diff_ln_gdp, start = 1990, frequency = 1)
str(data)

# Save the dataset 
write.csv(data, paste(data_out,"DA2_A3_Grigoryan-Anna_17-12-2018.csv", sep=""))

##############################################################
# DATA EXPLORATION

ggplot(data, aes(x = year, y = ln_gdp)) +
  geom_line(color = "blue")+
  labs(x = "Years", y = "GDP per capita in logs") +
  ggtitle("Graph 1: GDP per capita in logs over the years") +
  geom_point() 
ggsave(paste0(output, "lngdp.png"), width=12, height=7.5)

ggplot(data, aes(x = year, y = life_exp)) +
  geom_line(color = "blue")+
  labs(x = "Years", y = "Life expectancy in years") +
  ggtitle("Graph 2: Life expectancy over the years")+
  geom_point()
ggsave(paste0(output, "lfe_exp.png"), width=12, height=7.5)

ggplot(data, aes(x = ln_gdp, y = life_exp)) +
  labs(x = "GDP per capita in logs", y = "Life expectancy in years") +
  ggtitle("Graph 3: Loess regression of life expectancy over GDP per capita in logs") +
  geom_point() +
  geom_smooth()
ggsave(paste0(output, "norway.png"), width=12, height=7.5)


##############################################################
# DATA ANALYTICS

reg1 <- lm(life_exp ~ ln_gdp, data = data)

reg2 <- lm(Diff_life_exp ~ Diff_ln_gdp, data = data)

reg3 <- lm(Diff_life_exp ~ Diff_ln_gdp, data = data)
reg3nw <- coeftest(reg3, vcov. = NeweyWest(reg3, prewhite = F, lag = 18, verbose = T))

stargazer_r(list(reg1, reg2, reg3),  digits=3, out="model_123.html",sep="")

reg4 <- dyn$lm(Diff_life_exp ~ Diff_ln_gdp + lag(Diff_life_exp, -1), data = data)
reg4nw <- coeftest(reg4, vcov. = NeweyWest(reg4, prewhite = F, lag = 18, verbose = T))

reg5 <- dyn$lm(Diff_life_exp ~ Diff_ln_gdp + lag(Diff_life_exp, -2), data = data)
reg5nw <- coeftest(reg5, vcov. = NeweyWest(reg5, prewhite = F, lag = 18, verbose = T))

reg6 <- dyn$lm(Diff_life_exp ~ lag(Diff_ln_gdp, -2) + diff(lag(Diff_ln_gdp, -1)) + diff(Diff_ln_gdp) + lag(Diff_life_exp, -1), data = data)
reg6nw <- coeftest(reg6, vcov. = NeweyWest(reg6, prewhite = F, lag = 18, verbose = T))


stargazer_r(list(reg1, reg2, reg3, reg3nw),  digits=3, 
            out=paste0(output,"model_123.html",sep=""))
stargazer_r(list(reg4, reg4nw),  digits=3, 
            out=paste0(output,"model_4.html",sep=""))

stargazer_r(list(reg5, reg5nw),  digits=3, 
            out=paste0(output,"model_5.html",sep=""))

stargazer_r(list(reg6, reg6nw),  digits=3, 
            out=paste0(output,"model_6.html",sep=""))

