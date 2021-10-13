################################################################################
############################# Group Assignment #################################
################################################################################
remove(list = ls())
#------------------------------------------------------------------------------
# Set directory
#------------------------------------------------------------------------------
dir <- paste0('/Users/Nils/Documents/Ausbildung/Studium/RSM/Courses/Advanced',
' Statistics & Programming/Group Assignment/Code Folder/')
dirdata       <- paste0(dir, 'Data/')
dirprograms   <- paste0(dir, 'Programs/')

#------------------------------------------------------------------------------
# Import Libraries
#------------------------------------------------------------------------------
library(stargazer)
library(ggplot2)
library(AER)
library(tseries)
library(psych)


###############################################################################
# Load and clean Data
###############################################################################
#------------------------------------------------------------------------------
# Covid Data
#------------------------------------------------------------------------------
covidData <- read.csv(paste0(dirdata,'owid-covid-data.csv'), 
                      header = TRUE, sep=',')[ ,c('location', 'date', 
                                                  'new_cases', 'new_deaths', 
                                                  'new_cases_per_million',
                                                  'new_deaths_per_million',
                                                  'reproduction_rate',
                                                  'icu_patients_per_million',
                                                  'positive_rate',
                                                  'new_vaccinations',
                                                  'stringency_index')]
covidData$date <- as.Date(covidData$date)
colnames(covidData)[colnames(covidData) == "date"] <- "Date"

selectedCountry <- 'Netherlands'
#dateranges_country_1 <- as.Date (c("2020-07-06", "2021-01-06", "2021-01-07", "2021-07-07"))

# 27.02.2020: first covid case in the Netherlands 
#(see: https://www.dutchnews.nl/news/2020/05/coronavirus-a-timeline-of-the-pandemic-in-the-netherlands/)
dateranges_country_1 <- as.Date (c("2020-02-27", "2021-01-06",
                                   "2021-01-07", "2021-09-24"))

covid_country1 <- subset(covidData, (covidData$location == selectedCountry & 
                                  covidData$Date >= dateranges_country_1[1] &
                                  covidData$Date <= dateranges_country_1[4]))


#------------------------------------------------------------------------------
# Return Data
#------------------------------------------------------------------------------
# Read data from Investing.com
# Loop to read all data of all indices and stocks
# Indices: AEX, MSCI World
# Stocks: Eurocommercial, NSI N.V., Vastned, Wereldhave
# The stocks included are the companies from the AEX Real Estate which 
# were listed since Jan 2020 (CTP was not), focus on Real Estate in 
# the Netherlands and had complete price observations on Investing.com
# (not the case for Bever Holding)

file_names <- c('AEX', 'MSCI','ECM', 'NSI', 'VASN', 'WHA')

for (i in file_names) {
  # Read csv in temporary variable
  tmp <- read.csv(paste0(dirdata,i,'.csv'), header = TRUE, sep =',')
  # Format Dates and prices appropriately for return calulcation and 
  # Data merging
  tmp$Date <- as.Date(tmp$Date, format = "%b %d, %Y")
  tmp$Price <- as.numeric(sub(",","",tmp$Price))
  # Calculate returns
  n <- nrow(tmp)
  tmp$Disc_Return <- append(((tmp$Price[1:(n-1)] - tmp$Price[2:n]) / 
                               tmp$Price[2:n]), 0, after=n)*100
  # Name the column for data merging
  col_name <- paste0(i,'_disc_return')
  colnames(tmp)[colnames(tmp) == "Disc_Return"] <- col_name
  # Delete Last row (as no return information can be calculated)
  tmp <- head(tmp,-1)
  # Remove unnecessary columns before data merging
  tmp$Price <- NULL
  tmp$Open <- NULL
  tmp$High <- NULL
  tmp$Low <- NULL
  tmp$Vol. <- NULL
  tmp$Change.. <- NULL
  
  # Initialize df_returns in case it is the first data set loaded
  if (i == file_names[1]){
    df_returns = tmp
    }
  # Add the information to df_returns in case it is not the first data set
  else {
    df_returns <- merge(df_returns, tmp, by.x = 'Date', by.y = 'Date')
    }
}

#write.csv(df_returns, paste0(dirdata,"df_returns.csv"), row.names = FALSE)


# Calculate returns of an index composed of the selected stocks with
# equal-weighting 

df_returns$Portfolio_disc_return <- rowMeans(df_returns[,c('ECM_disc_return', 
                                                           'NSI_disc_return',
                                                           'VASN_disc_return',
                                                           'WHA_disc_return')],
                                             na.rm=TRUE)

# Merge return data with Covid-19 data
# Insert Column for Date: t+1
covid_country1$t_plus_1 <- covid_country1$Date + 1

df_country1 <- merge(df_returns, covid_country1, by.x = 'Date', 
                     by.y = 't_plus_1')

# insert Monday dummy
df_country1$dMonday <- ifelse(weekdays(df_country1$Date) == 'Monday', 1, 0)

# insert Before and After Vaccination dummy
df_country1$dPeriod <- ifelse(df_country1$Date <= dateranges_country_1[2],0,1)

df_summary <- merge(df_returns[,c('Date', 'AEX_disc_return',
                                  'Portfolio_disc_return', 'MSCI_disc_return')],
                    covid_country1[,c('Date','new_cases_per_million', 
                                      'reproduction_rate', 't_plus_1')],
                    by.x = 'Date', by.y = 't_plus_1')
df_summary$Date.y <- NULL
df_summary$Porfolio_cont_return <- 
  log(1+(df_summary$Portfolio_disc_return/100))*100
df_summary$AEX_cont_return <- log(1+(df_summary$AEX_disc_return/100))*100
df_summary$MSCI_cont_return <- log(1+(df_summary$MSCI_disc_return/100))*100
df_summary$dMonday <- ifelse(weekdays(df_summary$Date) == 'Monday', 1, 0)
df_summary$dPeriod <- ifelse(df_summary$Date <= dateranges_country_1[2],0,1)
df_summary[,c(1, 7, 2, 6, )]

stargazer(df_summary, type = 'text', order = c(1,2,8,3,7,5,6,4,9,10,11))


###############################################################################
# Data Analysis
###############################################################################

#------------------------------------------------------------------------------
#  Models with discrete returns
#------------------------------------------------------------------------------

# 1 AEX returns on new cases per million
mdl1 <- lm(AEX_disc_return ~ new_cases_per_million + MSCI_disc_return +
             dMonday + dPeriod + dPeriod:new_cases_per_million, 
           data = df_country1)

# 2 AEX returns on reproduction rate
mdl2 <- lm(AEX_disc_return ~ reproduction_rate + MSCI_disc_return +
             dMonday + dPeriod + dPeriod:reproduction_rate, 
           data = df_country1)
summary(mdl2)

# 3 Dutch Real Estate Portfolio on new cases per million
mdl3 <- lm(Portfolio_disc_return ~  new_cases_per_million + MSCI_disc_return + 
             dMonday + dPeriod + dPeriod:new_cases_per_million, 
           data = df_country1)
summary(mdl3)

# 4 Dutch Real Estate Portfolio on reproduction rate
mdl4 <- lm(Portfolio_disc_return ~  reproduction_rate  + MSCI_disc_return + 
             dMonday + dPeriod + dPeriod:reproduction_rate, data = df_country1)
summary(mdl4)

# Results table without robust SE
stargazer(mdl1, mdl2, mdl3, mdl4, type = 'text', intercept.bottom = FALSE)
# The first observation for a reproduction rate for the Netherlands was on
# 06.03.2020, therefore we have less observations in the regressions on
# the reproduction rate


#------------------------------------------------------------------------------
#  Models with continuous returns
#------------------------------------------------------------------------------
# transform discrete to continuous returns
df_country1$AEX_cont_return <- log(1+(df_country1$AEX_disc_return/100))*100
df_country1$Portfolio_cont_return <- 
  log(1+(df_country1$Portfolio_disc_return/100))*100
df_country1$MSCI_cont_return <- log(1+(df_country1$MSCI_disc_return/100))*100
write.csv(df_country1, paste0(dirdata,"df_country1.csv"), row.names = FALSE)





# 1 AEX returns on new cases per million
mdl5 <- lm(AEX_cont_return ~ new_cases_per_million + MSCI_cont_return +
             dMonday + dPeriod + dPeriod:new_cases_per_million, 
           data = df_country1)

# 2 AEX returns on reproduction rate
mdl6 <- lm(AEX_cont_return ~ reproduction_rate + MSCI_cont_return +
             dMonday + dPeriod + dPeriod:reproduction_rate, 
           data = df_country1)

# 3 Dutch Real Estate Portfolio on new cases per million
mdl7 <- lm(Portfolio_cont_return ~  new_cases_per_million  + MSCI_cont_return + 
             dMonday + dPeriod + dPeriod:new_cases_per_million, 
           data = df_country1)


# 4 Dutch Real Estate Portfolio on reproduction rate
mdl8 <- lm(Portfolio_cont_return ~  reproduction_rate  + MSCI_cont_return + 
             dMonday + dPeriod + dPeriod:reproduction_rate, data = df_country1)

# Results table without robust SE
stargazer(mdl5, mdl6, mdl7, mdl8, type = 'text', intercept.bottom = FALSE)


###############################################################################
# Robustness checks
###############################################################################

# Models with discrete returns
#------------------------------------------------------------------------------
# 1   Linearity and weak dependence (stationarity)
#------------------------------------------------------------------------------

# Stationarity 
# Visual inspection AEX daily returns
ggplot(df_country1, aes(x = Date, y = AEX_disc_return)) + geom_line() +
  xlab('Date') +
  ylab('AEX Daily Return')

ggsave(paste0(dirprograms,'AEX_stationarity.png'))

# Visual inspection Real Estate Portfolio daily returns
ggplot(df_country1, aes(x = Date, y = Portfolio_disc_return)) + geom_line() +
  xlab('Date') +
  ylab('Real Estate Portfolio Daily Return')
ggsave(paste0(dirprograms,'Real_Estate_stationarity.png'))

# Requirements for covariance stationarity (Wooldridge, 2013, p. 381):
# 1   E(x_t): constant over time
# 2   Var(x_t): constant over time
# 3   Cov(x_t, x_{t+h}): depends only on h and not on t

# Plots indicate that the first and third condition are fulfilled, but the 
# second condition remains to be questioned, as the variance seems to be 
# larger in the beginning of the observed period compared to the end


# Covariance stationarity is not required for the asymptotic properties of OLS
# (consistent estimator in large samples) in Time-Series. However, the 
# Time-Series needs to be weakly dependent (Wooldridge, 2013, p. 385)

# Requirements of weak dependence (Wooldridge, 2013, p.382):
# 1   Asymptotical uncorrelatedness: Corr(x_t, x_{t+h}) -> 0 as h -> ∞

# Test for unit root process - Dickey-Fuller Test
# If a process is unit root, it is not weakly dependent. Unit root processes 
# are highly persistent

adf.test(df_country1$AEX_disc_return)
adf.test(df_country1$AEX_cont_return)

adf.test(df_country1$Portfolio_disc_return)
adf.test(df_country1$Portfolio_cont_return)

# All of the test are significant, meaning that the null hypothesis of 
# a unit root process is rejected in favor of a weakly dependent (and 
# stationary) process


# Linearity
# Visual inspection for linearity
# AEX returns and new cases
ggplot(df_country1, aes(x = new_cases_per_million, y = AEX_disc_return)) +
  geom_point(size = 0.7) + 
  stat_smooth(method = "lm", formula = y ~ x, size = 1) + 
  xlab("New Cases per Million") +
  ylab("Real Estate Portfolio Returns")

# AEX returns and reproduction rate
ggplot(df_country1, aes(x = reproduction_rate, y = AEX_disc_return)) +
  geom_point(size = 0.7) + 
  stat_smooth(method = "lm", formula = y ~ x, size = 1) + 
  xlab("Reproduction Rate") +
  ylab("Real Estate Portfolio Returns")

# Real estate portfolio returns and new cases
ggplot(df_country1, aes(x = new_cases_per_million, y = Portfolio_disc_return)) +
  geom_point(size = 0.7) + 
  stat_smooth(method = "lm", formula = y ~ x, size = 1) + 
  xlab("New Cases per Million") +
  ylab("Real Estate Portfolio Returns")

# Real estate portfolio returns and reproduction rate
ggplot(df_country1, aes(x = reproduction_rate, y = Portfolio_disc_return)) +
  geom_point(size = 0.7) + 
  stat_smooth(method = "lm", formula = y ~ x, size = 1) + 
  xlab("Reproduction Rate") +
  ylab("Real Estate Portfolio Returns")


# Ramsey RESET Test (Greene, 2018, p.141)
resettest(mdl1, power=2)
resettest(mdl2, power=2)
resettest(mdl3, power=2)
resettest(mdl4, power=2)
resettest(mdl5, power=2)
resettest(mdl6, power=2)
resettest(mdl7, power=2)
resettest(mdl8, power=2)

# Model 3 and 7 indicate a possibly non-linear relationship

mdl3_new <- lm(Portfolio_disc_return ~  new_cases_per_million + 
                 I(new_cases_per_million^2) + MSCI_disc_return + 
                 I(MSCI_disc_return^2) + dMonday + dPeriod + 
                 dPeriod:new_cases_per_million, 
           data = df_country1)
summary(mdl3_new)
resettest(mdl3_new, power = 2)

mdl7_new <- lm(Portfolio_cont_return ~  new_cases_per_million + 
                 I(new_cases_per_million^2) + MSCI_cont_return + 
                 I(MSCI_cont_return^2) + dMonday + dPeriod + 
                 dPeriod:new_cases_per_million, 
               data = df_country1)
summary(mdl7_new)
resettest(mdl7_new, power = 2)

# Ramsey RESET test is no longer significant after inclusion of quadratic terms
# for 'new cases per million' and 'MSCI returns'

# The relationship between new cases per resident and the returns in the
# real estate portfolio seem to be indeed non-linear (quadratic). The 
# coefficients indicate that the linear component is positive, meaning that
# higher cases lead to higher returns, however this effect is reduced by the
# coefficient of the cubic term. The higher the case level the more negative
# this influence becomes. This means that very high case numbers actually have
# a negative effect 

# the net effect of cases is negative with more cases per million than:
-mdl3_new$coefficients['new_cases_per_million']/
  mdl3_new$coefficients['I(new_cases_per_million^2)']

# the net effect of cases is negative with more cases per million than:
-mdl7_new$coefficients['new_cases_per_million']/
  mdl7_new$coefficients['I(new_cases_per_million^2)']

#------------------------------------------------------------------------------
# 2   Multicollinearity (Full rank)
#------------------------------------------------------------------------------
# Variance Inflation Factors
vif(mdl1)
vif(mdl2)
vif(mdl3)
vif(mdl4)
vif(mdl5)
vif(mdl6)
vif(mdl7)
vif(mdl8)

# values are only higher for the Period dummy and its interaction term


#------------------------------------------------------------------------------
# 3   Mean independence of the variable of interest
#------------------------------------------------------------------------------
# strict exogeneity required in time-series for unbiasedness, contemporaneous
# exogeneity sufficient for consistent estimators in large samples (bias 
# converges to zero in large samples)
# Stock & Watson (2015, p.234) state that conditional mean independence for the  
# variable of interest is sufficient to fulfill this requirement and enables
# a causal interpretation for the variable of interest (not for other
# independent variables that might not be exogenous)

# New Covid cases and the reproduction rate of infections are very likely
# to be strictly exogenous as an influence of stock returns on these variables
# seems very unlikely (at any possible time lag). This would fulfill the 
# requirement of strict exogeneity that the errors are independent of the 
# independent variables from any time period. Also a lagged effect of the
# reproduction rate or new cases on stock returns seem unlikely, as this
# information could have been incorporated into stock prices already the day
# before. Only possibility for the conditional independence to not hold
# would be an omitted variable that correlates with the variables of interest
# and the stock returns

# create dataset without NAs in reproduction rate
df_reproduction_rate <- subset(df_country1, (!is.na(df_country1$reproduction_rate)))

cor(df_country1$new_cases_per_million, mdl1$residuals)
cor(df_reproduction_rate$reproduction_rate, mdl2$residuals)

cor(df_country1$new_cases_per_million, mdl3$residuals)
cor(df_reproduction_rate$reproduction_rate, mdl4$residuals)

# All correlations close to 0

mean(mdl1$residuals)
mean(mdl2$residuals)
mean(mdl3$residuals)
mean(mdl4$residuals)
mean(mdl5$residuals)
mean(mdl6$residuals)
mean(mdl7$residuals)
mean(mdl8$residuals)

# The mean of the residuals in all models very close to 0.
# If the residuals are independent of all the reproduction rate and the new
# cases of all periods and the expected value of the residuals is zero, the
# assumption of strict exogeneity holds. (Wooldridge, 2013, p.350)

#------------------------------------------------------------------------------
# 4   No serial correlation in the errors
#------------------------------------------------------------------------------
# For cross-sectional data the assumption is that data generation is fixed
# or random, this is not the case for time-series data as the observation from
# t_s is always related to the observation of t_{s+h} through its entity,
# which is the same for both observations. So in the cross-sectional case the
# random or fixed data generation leads to uncorrelated errors across the 
# observations. In time-series this is the case when there is no serial 
# correlation among the errors, thus the assumption is adjusted to no serial
# correlation from a random or fixed data generation (Wooldridge, 2013, p.353)


# Serial correlation is checked before homoskedasticity, as any serial 
# correlation will generally invalidate any test on heteroskedasticity
# (Wooldridge, 2013, p. 435)

# Breusch-Godfrey Test (Greene, 2018, p. 1000)
bgtest(mdl1)
bgtest(mdl2)
bgtest(mdl3)
bgtest(mdl4)
bgtest(mdl5)
bgtest(mdl6)
bgtest(mdl7)
bgtest(mdl8)

# Indicates serial correlation for mdl3, mdl6, mdl7, which are therfore
# reported with Newey-West SEs

# For robustness Newey-West SEs are reported for all models in the appendix
SE_sc_1 <- sqrt(diag(NeweyWest(mdl1)))
SE_sc_2 <- sqrt(diag(NeweyWest(mdl2)))
SE_sc_3 <- sqrt(diag(NeweyWest(mdl3)))
SE_sc_4 <- sqrt(diag(NeweyWest(mdl4)))
SE_sc_5 <- sqrt(diag(NeweyWest(mdl5)))
SE_sc_6 <- sqrt(diag(NeweyWest(mdl6)))
SE_sc_7 <- sqrt(diag(NeweyWest(mdl7)))
SE_sc_8 <- sqrt(diag(NeweyWest(mdl8)))


# Models with Newey_West SEs in the appendix of the Paper
stargazer(mdl1, mdl2, mdl3, mdl4, type='text', intercept.bottom = FALSE,
          se = list(SE_sc_1, SE_sc_2, SE_sc_3, SE_sc_4))

stargazer(mdl5, mdl6, mdl7, mdl8, type='text', intercept.bottom = FALSE,
          se = list(SE_sc_5, SE_sc_6, SE_sc_7, SE_sc_8))


#------------------------------------------------------------------------------
# 5   Homoskedasticity
#------------------------------------------------------------------------------
# Those models where the Breusch-Godfrey test did not indicate serially
# correlated errors are tested for heteroskedastic errors with the 
# Breusch-Pagan Test

lmtest ::bptest(mdl1)
lmtest ::bptest(mdl2)
# mdl3 has serially correlated errors
lmtest ::bptest(mdl4)
lmtest ::bptest(mdl5)
# mdl6 has serially correlated errors
# mdl7 has serially correlated errors
lmtest ::bptest(mdl8)

# the null hypothesis of homoskedasticity is rejected for all models

# White's robust standard errors are calculated
SE_het_1 <- sqrt(diag(vcovHC(mdl1, type = 'HC0')))
SE_het_2 <- sqrt(diag(vcovHC(mdl2, type = 'HC0')))
# mdl3 has serially correlated errors
SE_het_4 <- sqrt(diag(vcovHC(mdl4, type = 'HC0')))
SE_het_5 <- sqrt(diag(vcovHC(mdl5, type = 'HC0')))
# mdl6 has serially correlated errors
# mdl7 has serially correlated errors
SE_het_8 <- sqrt(diag(vcovHC(mdl8, type = 'HC0')))

# Mdl1, mdl2, mdl4, mdl5, and mdl8 are reported with White's robust standard 
# errors


#------------------------------------------------------------------------------
# 6   Normally distributed errors
#------------------------------------------------------------------------------
# Visual inspection
# Discrete returns
# Mdl1
ggplot(df_country1, aes(mdl1$residuals)) + geom_density() +
  stat_function(fun = dnorm, args = list(mean = mean(mdl1$residuals), 
                                         sd = sd(mdl1$residuals)), 
                color = 'red')

qplot(sample = mdl1$residuals, stat='qq') + 
  geom_abline(slope = 1,intercept = 0) +
  xlab('Theoretical Quantiles') +
  ylab('Sample Quantiles')

# Mdl2
ggplot(df_reproduction_rate, aes(mdl2$residuals)) + geom_density() +
  stat_function(fun = dnorm, args = list(mean = mean(mdl2$residuals), 
                                         sd = sd(mdl2$residuals)), 
                color = 'red')

qplot(sample = mdl2$residuals, stat='qq') + 
  geom_abline(slope = 1,intercept = 0) +
  xlab('Theoretical Quantiles') +
  ylab('Sample Quantiles')

# Mdl3
ggplot(df_country1, aes(mdl3$residuals)) + geom_density() +
  stat_function(fun = dnorm, args = list(mean = mean(mdl3$residuals), 
                                         sd = sd(mdl3$residuals)), 
                color = 'red')

qplot(sample = mdl3$residuals, stat='qq') + 
  geom_abline(slope = 1,intercept = 0) +
  xlab('Theoretical Quantiles') +
  ylab('Sample Quantiles')

# Mdl4
ggplot(df_reproduction_rate, aes(mdl4$residuals)) + geom_density() +
  stat_function(fun = dnorm, args = list(mean = mean(mdl4$residuals), 
                                         sd = sd(mdl4$residuals)), 
                color = 'red')

qplot(sample = mdl4$residuals, stat='qq') + 
  geom_abline(slope = 1,intercept = 0) +
  xlab('Theoretical Quantiles') +
  ylab('Sample Quantiles')




# Continuous returns
# Mdl5
ggplot(df_country1, aes(mdl5$residuals)) + geom_density() +
  stat_function(fun = dnorm, args = list(mean = mean(mdl5$residuals), 
                                         sd = sd(mdl5$residuals)), 
                color = 'red')

qplot(sample = mdl5$residuals, stat='qq') + 
  geom_abline(slope = 1,intercept = 0) +
  xlab('Theoretical Quantiles') +
  ylab('Sample Quantiles')


# Mdl6
ggplot(df_reproduction_rate, aes(mdl6$residuals)) + geom_density() +
  stat_function(fun = dnorm, args = list(mean = mean(mdl6$residuals), 
                                         sd = sd(mdl6$residuals)), 
                color = 'red')

qplot(sample = mdl6$residuals, stat='qq') + 
  geom_abline(slope = 1,intercept = 0) +
  xlab('Theoretical Quantiles') +
  ylab('Sample Quantiles')


# Mdl7
ggplot(df_country1, aes(mdl7$residuals)) + geom_density() +
  stat_function(fun = dnorm, args = list(mean = mean(mdl7$residuals), 
                                         sd = sd(mdl7$residuals)), 
                color = 'red')

qplot(sample = mdl7$residuals, stat='qq') + 
  geom_abline(slope = 1,intercept = 0) +
  xlab('Theoretical Quantiles') +
  ylab('Sample Quantiles')


# Mdl8
ggplot(df_reproduction_rate, aes(mdl8$residuals)) + geom_density() +
  stat_function(fun = dnorm, args = list(mean = mean(mdl8$residuals), 
                                         sd = sd(mdl8$residuals)), 
                color = 'red')

qplot(sample = mdl8$residuals, stat='qq') + 
  geom_abline(slope = 1,intercept = 0) +
  xlab('Theoretical Quantiles') +
  ylab('Sample Quantiles')



# The plots indicate that the residuals of all models possess heavy tails

# Shapiro Test to test normality
shapiro.test(mdl1$residuals)
shapiro.test(mdl2$residuals)
shapiro.test(mdl3$residuals)
shapiro.test(mdl4$residuals)
shapiro.test(mdl5$residuals)
shapiro.test(mdl6$residuals)
shapiro.test(mdl7$residuals)
shapiro.test(mdl8$residuals)

# The null hypothesis of normally distributed errors must be rejected
# for all models. This is most likely due to the heavy tails of the
# distribution. 
# The assumption of normally distributed errors is required for the sampling
# distribution of the estimators to be normally distributed too. Only when
# the distribution of the estimators is normal, inference about the
# significance is possible (Wooldridge, 2013, p. 120). However, the 
# distribution of the estimators is asymptotically normal in large samples
# (Wooldridge, 2013, p. 175) as long as the errors have a finite variance,
# regardless of their actual distribution.
# In the analysis at hand the sample is about 400 observations. Therefore,
# it can be assumed that the distribution of the estimators is asymptotically
# normal even though the errors are not, enabling statistical inferences.



#------------------------------------------------------------------------------
# Additional check: Outliers
#------------------------------------------------------------------------------
# Collect relevant information

# Select model - 1 to 8 is possible

mdl <- mdl1
explanatory_var <- df_country1[(variable.names(mdl)[2])]

yhat <- fitted(mdl)
ehat <- residuals(mdl)
zresid <- scale(ehat)
sresid  <- rstandard(mdl)
sdresid <- rstudent(mdl) 
hlev    <- hatvalues(mdl)

regByProducts <- 
  cbind("Predicted values"     = yhat,
        "Std Predicted value"  = (yhat-mean(yhat))/sd(yhat),
        "Residual"             = ehat,
        "Std Resid. (zresid)"  = as.numeric(zresid),
        "Stud Resid. (sresid)" = sresid,
        "Stud Deleted Resid."  = sdresid,
        "Cook's Distance"      = cooks.distance(mdl),
        "Centered Leverage"    = hlev
  )
tmp <- describe(regByProducts) # Store summary
class(tmp) <- "data.frame"     # Small repair
tmp[c(2,8,9,3,4)]

# Residuals versus fitted values
dfTmp <- data.frame(sdresid, yhat)

ggplot(dfTmp, aes(x=yhat, y=sdresid)) +
  geom_point(colour="blue4") +
  ylab("Studentized deleted residuals, sdresid") +
  xlab("Fitted values dependent, yhat") +
  geom_hline(yintercept = 0, colour="grey") +
  geom_hline(yintercept = qt(0.995,df=mdl$df.residual), 
             colour="grey", linetype=2) +
  geom_hline(yintercept = qt(0.005,df=mdl$df.residual), 
             colour="grey", linetype=2) +
  theme(axis.text.x = element_text(size=rel(1.25)),
        axis.text.y = element_text(size=rel(1.25)))

# Residuals versus explanatory variable
dfTmp <- data.frame(sdresid, Ex=explanatory_var[1])
colnames(dfTmp) = c('sdresid', 'Ex')

ggplot(dfTmp, aes(x=Ex, y=sdresid)) +
  geom_point(colour="blue4") +
  ylab("Studentized deleted residuals, sdresid") +
  xlab("Explanatory variable") +
  geom_hline(yintercept = 0, colour="grey") +
  geom_hline(yintercept = qt(0.995,df=mdl$df.residual), 
             colour="grey", linetype=2) +
  geom_hline(yintercept = qt(0.005,df=mdl$df.residual), 
             colour="grey", linetype=2) +
  theme(axis.text.x = element_text(size=rel(1.25)),
        axis.text.y = element_text(size=rel(1.25)))






###############################################################################
# Final Results
###############################################################################

# Discrete Returns
# mdl1, 2, and 4 with heteroskedastic robust errors
# mdl3 with Newey-West HAC robust errors
stargazer(mdl1, mdl2, mdl3, mdl4, type='text', intercept.bottom = FALSE,
          se = list(SE_het_1, SE_het_2, SE_sc_3, SE_het_4), df=F, 
          dep.var.labels = c('AEX disc. returns', 'RE disc. Returns'))

# Continuous Returns
# mdl5 and 8 with heteroskedastic robust errors
# mdl6 and 7 with Newey-West HAC robust errors
stargazer(mdl5, mdl6, mdl7, mdl8, type='text', intercept.bottom = FALSE,
          se = list(SE_het_5, SE_sc_6, SE_sc_7, SE_het_8), df=F, 
          dep.var.labels = c('AEX cont. returns', 'RE cont. Returns'))

