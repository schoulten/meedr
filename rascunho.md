# Title: Quick and easy access to market expectations for economic indicators from Focus/BCB
# Description:


This package provides quick and easy access to the data and statistics of market expectations regarding the economic indicators of the Focus report of the Central Bank of Brazil, through the Expectations System API. This data comes from several financial institutions, such as: banks, brokers, funds, consultancies, etc. The expectations package offers an R interface to the API and other advantages:

- Use of a caching system with package memoise to speed up repeated requests of data;
- User can utilize all cores of the machine (parallel computing) when fetching a large batch of time series.
