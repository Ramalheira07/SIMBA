# SIMBA
Monitoring system for marine biotoxins in bivalve molluscs based on machine learning

Requirements:
> = Version 3.0 for R software
> = 4G RAM
Latest version of ORCA

This system uses public data generated by the Portuguese Institute of Sea and Atmosphere (2014-2018).
The web version can be found at: https://ipma.shinyapps.io/SIMBA/.

The Autoregressive  Integrated  Moving  Average(ARIMA) model is a linear model that is commonly used to make predictions based on AR models Au-autoregressive  (AR) and  the  Moving  Averages(MA).   This  model is normally used when the data show evidence of nonstationarity,  transforming the time series into stationary time series through a difference operator, called a differentiation operator.  In the ARIMA model, the predictedvalue for a variable is a result of a linear function that uses observations and past random errors [6],assuming a relationship between past and current values.  In the modelARIMA(p,d,q), prepresentsthe autoregressive order, the degree of differentiation, andqthe moving average order. 


Vector  Autoregression(VAR) is an extension of the  AR  model,  a multivariate model that unlike the  AR  model it uses a vector composed of  multiple variable values as input data.   VAR  models the dynamic relationships between variables,  be-ing  promising for multivariate series that represent dependency relationships between variables.

Autoregressive Moving Averages Vector(VARMA) consists of two simple models (AR and  MA).  VARMA uses a value vector where each value belongs to a  variable, thus becoming a multivariate model. Similarly to the   VARmodel,  the  VARMA model allows you to study dynamic relationships between variables,   which generally improves predictive accuracy, and can model multiple dependent time series at the same time.

Recurrent  Neuronal  Network(RNN) is a more complex ANN that requires domain knowledge for better fit and interpretation.  RNNs handle chrono-logical data with a return cycle, with previous outputs being used to predict the new value.   In addition to forwarding the value to the new layer, an RNN also sends the value to the current layer’s input.  All values are processed to generate a new output.  


How to run the application:

Open Rstudio, direct the terminal to the folder where the application files are located.

Then just execute the following command:

shiny :: runApp ()





 
