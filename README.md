# Web application for stocks analysis based on historical time series
Web application that allows users to select a stock from Yahoo and choose an analysis period, returning an analysis based on ARIMA modeling.

I decided to create an app to make it easier to change the ticker used for the analysis previously carried out in the repository “Stock price analysis using ARIMA model”

I used the shiny library to build the application, lubridate to simplify the conversion of future dates, and zoo to solve an issue related to the use of monthly data.

I also added an extension that, in addition to the ARIMA model, applies the ETS method in order to obtain a more accurate analysis.
