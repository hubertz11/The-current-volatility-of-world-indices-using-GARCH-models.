getwd()
setwd("C:/Users/HZ/Desktop/Studia/II semestr II rok/EFM/Project")

library(xts)
library(fBasics) # e.g. basicStats()
library(tseries)# e.g. jarque.bera.test()
library(car) # e.g. durbinWatsonTest()
library(FinTS) # e.g. ArchTest()
library(fGarch) # e.g. garchFit()
library(quantmod) # e.g. getSymbol()
library(rugarch) # e.g. ugarchfit()


# lets load additional function prepared to easily compare ICs for GARCH models

source("function_compareICs.GARCH.R")
source("function_compare.ICs.ugarchfit.R")

###########################
#DATA IMPORT======
###########################

# lets import the data with prices of 5 different world indices:
#SP500, DAX, FTSE 100, Nikkei 225 and CAC 40 
#directly from Yahoo finance from the 2015


#SP500
getSymbols("^GSPC",             # ticker
           from = "2015-01-01", # starting date
           to = "2021-05-18")   # end date


#DAX
getSymbols("^GDAXI",             # ticker
           from = "2015-01-01", # starting date
           to = "2021-05-18")   # end date


#FTSE 100(UK)
getSymbols("^FTSE",             # ticker
           from = "2015-01-01", # starting date
           to = "2021-05-18")   # end date

#Nikkei 225
getSymbols("^N225",             # ticker
           from = "2015-01-01", # starting date
           to = "2021-05-18")   # end date

#CAC 40(France)
getSymbols("^FCHI",             # ticker
           from = "2015-01-01", # starting date
           to = "2021-05-18")   # end date

#Keep only price and change the columns names
SP500 <- GSPC[, 6]
colnames(SP500) <- "SP500.Close"

DAX <- GDAXI[, 6]
colnames(DAX) <- "DAX.Close"

FTSE <- FTSE[, 6]
colnames(FTSE) <- "FTSE.Close"

Nikkei <- N225[, 6]
colnames(Nikkei) <- "Nikkei.Close"

CAC40 <- FCHI[, 6]
colnames(CAC40) <- "CAC40.Close"

###########################
#DATA CLEANING======
###########################

#Carry the last observation forward if NA occurs
na.locf(SP500)
sum(is.na(SP500))

na.locf.default(DAX)
sum(is.na(DAX))
which(is.na(DAX))
na.omit(DAX)
sum(is.na(DAX))
DAX <- DAX[-c(251, 617, 703, 723, 861), ]
sum(is.na(DAX))

na.locf(FTSE)
sum(is.na(FTSE))
na.omit(FTSE)
sum(is.na(FTSE))
which(is.na(FTSE))
FTSE <- FTSE[-c(250, 253, 1513), ]
sum(is.na(FTSE))

na.locf(Nikkei)
sum(is.na(Nikkei))
na.omit(Nikkei)
sum(is.na(Nikkei))
which(is.na(Nikkei))
Nikkei <- Nikkei[-c(623, 668, 683,  716,  743,  744,  745,  748,  773,  800,  828, 
                    831,  832,  928,  933,  943,  977, 998, 1003), ]
sum(is.na(Nikkei))

na.locf(CAC40)
sum(is.na(CAC40))
na.omit(CAC40)
sum(is.na(CAC40))
which(is.na(CAC40))
CAC40 <- CAC40[-1275, ]
sum(is.na(CAC40))

##############################
#LOG RETURNS======
##############################

# lets add log-returns to the data
SP500$Returns <- diff.xts(log(SP500$SP500.Close))
DAX$Returns <- diff.xts(log(DAX$DAX.Close))
FTSE$Returns <- diff.xts(log(FTSE$FTSE.Close))
Nikkei$Returns <- diff.xts(log(Nikkei$Nikkei.Close))
CAC40$Returns <- diff.xts(log(CAC40$CAC40.Close))

#################################################
#PLOTS===========
################################################

par(mfrow=c(2,3))
plot(SP500,
     multi.panel = 2, 
     main = "SP500 price vs daily returns",
     col = c("red", "blue"),
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 1,
     yaxis.same = FALSE, # otherwise scale is the same for each column!
     cex = 0.4)

plot(DAX,
     multi.panel = 2, 
     main = "DAX price vs daily returns",
     col = c("red", "blue"),
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 1,
     yaxis.same = FALSE, # otherwise scale is the same for each column!
     cex = 0.4)

plot(FTSE,
     multi.panel = 2, 
     main = "FTSE price vs daily returns",
     col = c("red", "blue"),
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 1,
     yaxis.same = FALSE, # otherwise scale is the same for each column!
     cex = 0.4)

plot(Nikkei,
     multi.panel = 2, 
     main = "Nikkei price vs daily returns",
     col = c("red", "blue"),
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 1,
     yaxis.same = FALSE, # otherwise scale is the same for each column!
     cex = 0.4)

plot(CAC40,
     multi.panel = 2, 
     main = "CAC40 price vs daily returns",
     col = c("red", "blue"),
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 1,
     yaxis.same = FALSE, # otherwise scale is the same for each column!
     cex = 0.4)

par(mfrow=c(1,1))

##############################
#ARCH effects======
##############################
#Let's plot squared log-returns to see if we can observe 
#some autoregressive/MA relations among returns

par(mfrow=c(2,3))

# and do the same for the SQUARED log-returns
acf(SP500$Returns^2, 
    lag.max = 36, 
    na.action = na.pass,
    ylim = c(0,0.5), # we rescale the vertical axis
    col = "darkblue", 
    lwd = 7, 
    main = "ACF of SQUARED log-returns of SP500")

acf(DAX$Returns^2, 
    lag.max = 36, 
    na.action = na.pass,
    ylim = c(0,0.5), # we rescale the vertical axis
    col = "darkblue", 
    lwd = 7, 
    main = "ACF of SQUARED log-returns of DAX")

acf(FTSE$Returns^2, 
    lag.max = 36, 
    na.action = na.pass,
    ylim = c(0,0.5), # we rescale the vertical axis
    col = "darkblue", 
    lwd = 7, 
    main = "ACF of SQUARED log-returns of FTSE")

acf(Nikkei$Returns^2, 
    lag.max = 36, 
    na.action = na.pass,
    ylim = c(0,0.5), # we rescale the vertical axis
    col = "darkblue", 
    lwd = 7, 
    main = "ACF of SQUARED log-returns of Nikkei")

acf(CAC40$Returns^2, 
    lag.max = 36, 
    na.action = na.pass,
    ylim = c(0,0.5), # we rescale the vertical axis
    col = "darkblue", 
    lwd = 7, 
    main = "ACF of SQUARED log-returns of CAC40")

par(mfrow=c(1,1))

#Autoregression among long returns strongly significant.
#It seems that ACF for square returns is decaying pretty to 0. 
#That might be the evidence of autoregressive relationship
# among squared returns

#We dont' care about the order of autoregression, we care only about that the
#autoregressive relationship exists

#This in turn indicates some autoregressive relations 
# among SQUARED returns (their variance!) which 
# can be used to build a (G)ARCH model

#Let's formally test ARCH effects among log-returnsusing ARCH LM-test
#(autocorrelation of SQUARED returns)
#H0 - lack of ARCH effect (autocorrelation) among 
#squared returns

#SP500
(SP500_ARCH_effects <- ArchTest(SP500$Returns,  # here we use a vector of returns as input
         lags = 5)) # and maximum order of ARCH effect

# null about lack of ARCH effects strongly rejected!

#DAX
(DAX_Arch_effects <- ArchTest(DAX$Returns,  # here we use a vector of returns as input
         lags = 5)) # and maximum order of ARCH effect

# null about lack of ARCH effects strongly rejected!

#FTSE
(FTSE_ARCH_effects <- ArchTest(FTSE$Returns,  # here we use a vector of returns as input
         lags = 5)) # and maximum order of ARCH effect

# null about lack of ARCH effects strongly rejected!

#Nikkei
(Nikkei_ARCH_effects <- ArchTest(Nikkei$Returns,  # here we use a vector of returns as input
         lags = 5)) # and maximum order of ARCH effect

# null about lack of ARCH effects strongly rejected!

#CAC40
(CAC40_ARCH_effects <- ArchTest(CAC40$Returns,  # here we use a vector of returns as input
         lags = 5)) # and maximum order of ARCH effect

# null about lack of ARCH effects strongly rejected!


#So, we are certain that volatiliy is not constant, therefore we can start to find
#the best model with will measure the current volatility


##############################
#GARCH(p,q) model======
##############################

#We are not trying to fit the best model to the data. We rather stick to
#GARCH(1,1) model. We believe that it delivers a good fit and accurate 
#predictions, and thanks to using one GARCH model to the five different 
#data, we will avoid overfitting.

#ARCH(q)
#q - lag structure of squared shocks

#GARCH(q,p)
#p- lag structure of previous variance

#ARMA (p,q)
#AR(p) - PACF informs about lagged time series
#MA(q)- ACF MA informs about residuals

# mu - constant term in the mean equation
# omega(alfa0 in the lecture notes) - constant term in the variance equation
# alpha1 - arch1 parameter in the variance equation
# alfa has to be 0 < alfa1 < 1
#sum of the alfa < 1 

#Let's delete 1st row of log-returns (NA)

SP500 <- SP500[-1, ]
DAX <- DAX[-1, ]
FTSE <- FTSE[-1, ]
Nikkei <- Nikkei[-1, ]
CAC40 <- CAC40[-1, ]

#Ljung-Box Test:
#H0- residuals are white noise  / no serial correlation among the model residuals

#Jarque-Bera Test:
#H0 - normality distribution of residuals

#Shapiro-Wilk pretty the same, but this test isn't reliable with larger data, e.g.
#n > 2000

SP500.GARCH11 <- garchFit(~garch(1, 1),
                          data = SP500$Returns,
                          include.mean = TRUE,
                          cond.dist = "norm", 
                          trace = FALSE) 

summary(SP500.GARCH11)


# All parameters are significant 
# Ljung-box test for R^2 shows there is no more autocorrelation between 
#the current and past standardized SQUARED residuals (variance)
# all ARCH effect (LM ARCH test) removed

DAX.GARCH11 <- garchFit(~garch(1, 1),
                          data = DAX$Returns,
                          include.mean = TRUE,
                          cond.dist = "norm", 
                          trace = FALSE) 

summary(DAX.GARCH11)

# All parameters are significant 
# Ljung-box test for R^2 shows there is no more autocorrelation between 
#the current and past standardized SQUARED residuals (variance)
# all ARCH effect (LM ARCH test) removed

FTSE.GARCH11 <- garchFit(~garch(1, 1),
                        data = FTSE$Returns,
                        include.mean = TRUE,
                        cond.dist = "norm", 
                        trace = FALSE) 

summary(FTSE.GARCH11)

#mu (constant term in the mean equation) are not significant
#To resolve this problem, we can omit mu term in our model --> include.mean = FALSE
#However, let's keep consistency in our research, at leave it without any improvements

# Ljung-box test for R^2 shows there is no more autocorrelation between 
#the current and past standardized SQUARED residuals (variance)
# all ARCH effect (LM ARCH test) removed

Nikkei.GARCH11 <- garchFit(~garch(1, 1),
                         data = Nikkei$Returns,
                         include.mean = TRUE,
                         cond.dist = "norm", 
                         trace = FALSE) 

summary(Nikkei.GARCH11)

# All parameters are significant 
# Ljung-box test for R^2 shows there is no more autocorrelation between 
#the current and past standardized SQUARED residuals (variance)
# all ARCH effect (LM ARCH test) removed

CAC40.GARCH11 <- garchFit(~garch(1, 1),
                           data = CAC40$Returns,
                           include.mean = TRUE,
                           cond.dist = "norm", 
                           trace = FALSE) 

summary(CAC40.GARCH11)

# All parameters are significant 
# Ljung-box test for R^2 shows there is no more autocorrelation between 
#the current and past standardized SQUARED residuals (variance)
# all ARCH effect (LM ARCH test) removed


########################################################
#GARCH EXTENSIONS====
########################################################

########################################################
#The EGARCH model=====
########################################################

# Let's examine whether conditional variance reacts asymmetrically 
# to the news arriving to the market.

#here we first define a model specification
#EGARCH model despite GARCH

spec <- ugarchspec(# variance equation
    variance.model = list(model = "eGARCH", 
                          garchOrder = c(1, 1)),
    # sGARCH would stand for standard GARCH model
    # mean equation - lets use AR(0)
    mean.model = list(armaOrder = c(0, 0), 
                      include.mean = TRUE), 
    # assumed distribution of errors
    distribution.model = "norm")

###################################
#SP500 EGARCH(1,1)
###################################

#Model estimation

SP500.EGARCH <- ugarchfit(spec = spec, #specification of the model
                           data = SP500$Returns) #vector of returns

SP500.EGARCH

plot(SP500.EGARCH)

#All the parameters are statistically significant

# coefficient alpha1 is the assymetry term 
# of EGARCH model (measuring the leverage effect).
# (for model specification see e.g. page 2
# in http://faculty.chicagobooth.edu/ruey.tsay/teaching/bs41202/sp2015/IntroPackages.pdf)

# It is negative and significant (<0.05), so the asymmetry is found!

###################################
#DAX EGARCH(1,1)
###################################

#Model estimation

DAX.EGARCH <- ugarchfit(spec = spec, #specification of the model
                          data = DAX$Returns) #vector of returns

DAX.EGARCH

#mu is not statistically significant, 
#to better fit mode we can omit including mean

# coefficient alpha1 is the assymetry term 
# It is negative and significant (<0.05), so the asymmetry is found!

###################################
#FTSE EGARCH(1,1)
###################################

#Model estimation

FTSE.EGARCH <- ugarchfit(spec = spec, #specification of the model
                        data = FTSE$Returns) #vector of returns

FTSE.EGARCH

#mu is not statistically significant, to better fit mode we can omit mean

# coefficient alpha1 is the assymetry term 
# It is negative and significant (<0.05), so the asymmetry is found!


###################################
#Nikkei EGARCH(1,1)
###################################

#Model estimation

Nikkei.EGARCH <- ugarchfit(spec = spec, #specification of the model
                         data = Nikkei$Returns) #vector of returns

Nikkei.EGARCH

#mu and ar1 is not statistically significant, to better fit mode we can omit mean
#and use AR(0,0)

# coefficient alpha1 is the assymetry term 
# It is negative and significant (<0.05), so the asymmetry is found!

###################################
#CAC40 EGARCH(1,1)
###################################

#Model estimation

CAC40.EGARCH <- ugarchfit(spec = spec, #specification of the model
                           data = CAC40$Returns) #vector of returns

CAC40.EGARCH

#mu and ar1 is not statistically significant, to better fit mode we can omit mean
#and use AR(0,0)

# coefficient alpha1 is the assymetry term 
# It is negative and significant (<0.05), so the asymmetry is found!

########################################################
#GARCH-M, GARCH-in-Mean=====
########################################################

#One expects that the higher the risk, the higher the return 
#(on average) – which is called the risk premium

#the existence of risk premium is therefore confirmed if arcm coefficient
#is significantly positive and statistically significant

# lets first define a model specification
spec <- ugarchspec(# variance equation
    variance.model = list(model = "sGARCH", 
                          # sGARCH = standard GARCH
                          garchOrder = c(1, 1)),
    # mean equation
    mean.model = list(armaOrder = c(0, 0), 
                      include.mean = TRUE,
                      # we add an element to the mean equation,
                      # which can be either stdev (archpow 1)
                      # or var (archpow=2)
                      archm = TRUE, archpow = 1), 
    # assumed distribution of errors
    distribution.model = "norm")

#############################
#SP500 GARCH-M
#############################

#Model estimation

SP500.GARCHM <- ugarchfit(spec = spec, 
                           data = SP500$Returns)

SP500.GARCHM

#archm coefficient is significantly positive, but, when we apply robust
#standard errors, the archm is not statistically significant
#so we DO NOT find the proof for risk premium in the model

#############################
#DAX GARCH-M
#############################

#Model estimation

DAX.GARCHM <- ugarchfit(spec = spec, 
                          data = DAX$Returns)

DAX.GARCHM

#archm coefficient - we DO NOT find the proof for risk premium in the model

#############################
#FTSE GARCH-M
#############################

#Model estimation

FTSE.GARCHM <- ugarchfit(spec = spec, 
                        data = FTSE$Returns)

FTSE.GARCHM

#archm coefficient - we find the proof for risk premium in the model

#############################
#Nikkei GARCH-M
#############################

#Model estimation

Nikkei.GARCHM <- ugarchfit(spec = spec, 
                         data = Nikkei$Returns)

Nikkei.GARCHM

#archm coefficient - we find the proof for risk premium in the model

#############################
#CAC40 GARCH-M
#############################

#Model estimation

CAC40.GARCHM <- ugarchfit(spec = spec, 
                           data = CAC40$Returns)

CAC40.GARCHM

#archm coefficient - we do not find the proof for risk premium in the model


########################################################
#GARCH-t=====
########################################################

#t-Student distribution instead of normal distribution

# lets first define a model specification
spec <- ugarchspec(# variance equation
    variance.model = list(model = "sGARCH", 
                          garchOrder = c(1, 1)),
    # mean equation
    mean.model = list(armaOrder = c(0, 0), 
                      include.mean = TRUE), 
    # assumed distribution of errors
    distribution.model = "std") # std = t-Student

#############################
#SP500 GARCH-t
#############################

#Model estimation
SP500.GARCHt <- ugarchfit(spec = spec, 
                           data = SP500$Returns)

SP500.GARCHt

# shape is the number of degrees of freedom for the t-Student distribution.
# In our case this equals to roghly 5, so we have fatter 
#tails than in normal distribution, and shape is statistically significant
# dla 6 stopni swobody mamy fatter tails than normal dist.
#So, t-student distribtion is better choice than normal distr.

#############################
#DAX GARCH-t
#############################

#Model estimation
DAX.GARCHt <- ugarchfit(spec = spec, 
                          data = DAX$Returns)

DAX.GARCHt

#Shape = 4,77, and statistically significant
#So, t-student distribtion is better choice than normal distr.

#############################
#FTSE GARCH-t
#############################

#Model estimation
FTSE.GARCHt <- ugarchfit(spec = spec, 
                        data = FTSE$Returns)

FTSE.GARCHt

#Shape = 5.6, and statistically significant
#So, t-student distribtion is better choice than normal distr.

#############################
#Nikkei GARCH-t
#############################

#Model estimation
Nikkei.GARCHt <- ugarchfit(spec = spec, 
                         data = Nikkei$Returns)

Nikkei.GARCHt

#Shape = 4.4, and statistically significant
#So, t-student distribtion is better choice than normal distr.

#############################
#CAC40 GARCH-t
#############################

#Model estimation
CAC40.GARCHt <- ugarchfit(spec = spec, 
                           data = CAC40$Returns)

CAC40.GARCHt

#Shape = 4.7, and statistically significant
#So, t-student distribtion is better choice than normal distr.


###################################################
#THE BEST MODEL====
###################################################
# lets compare information criteria for all models

#################
#SP500
##################

#GARCH(1,1)
compare.ICs.GARCH("SP500.GARCH11")

# AIC       BIC       SIC      HQIC         model
#  -6.744708 -6.731285 -6.744721 -6.739724 SP500.GARCH11

compare.ICs.ugarchfit(c("SP500.EGARCH", 
                        "SP500.GARCHM", 
                        "SP500.GARCHt"))


# all criteria indicate that GARCH-t is the best (the lowest information criterion)


#################
#DAX
##################

#GARCH(1,1)
compare.ICs.GARCH("DAX.GARCH11")

#AIC       BIC       SIC      HQIC       model
# -6.156782 -6.143398 -6.156794 -6.151813 DAX.GARCH11

compare.ICs.ugarchfit(c("DAX.EGARCH", 
                        "DAX.GARCHM", 
                        "DAX.GARCHt"))


# all criteria indicate that GARCH-t is the best (the lowest information criterion)

#################
#FTSE
##################

#GARCH(1,1)
compare.ICs.GARCH("FTSE.GARCH11")

#AIC       BIC      SIC      HQIC        model
# -6.585298 -6.571901 -6.58531 -6.580324 FTSE.GARCH11

compare.ICs.ugarchfit(c("FTSE.EGARCH", 
                        "FTSE.GARCHM", 
                        "FTSE.GARCHt"))


# all criteria indicate that GARCH-t is the best (the lowest information criterion)

#################
#Nikkei
##################

#GARCH(1,1)
compare.ICs.GARCH("Nikkei.GARCH11")

#AIC       BIC       SIC      HQIC          model
# -6.103671 -6.089918 -6.103684 -6.098557 Nikkei.GARCH11


compare.ICs.ugarchfit(c("Nikkei.EGARCH", 
                        "Nikkei.GARCHM", 
                        "Nikkei.GARCHt"))


# all criteria indicate that GARCH-t is the best (the lowest information criterion)


#################
#CAC40
##################

#GARCH(1,1)
compare.ICs.GARCH("CAC40.GARCH11")

#AIC       BIC       SIC      HQIC         model
# -6.306444 -6.293182 -6.306456 -6.301523 CAC40.GARCH11


compare.ICs.ugarchfit(c("CAC40.EGARCH", 
                        "CAC40.GARCHM", 
                        "CAC40.GARCHt"))


# all criteria indicate that GARCH-t is the best (the lowest information criterion)



#############################################
#GARCH-t model is the best to model volatility
#############################################

