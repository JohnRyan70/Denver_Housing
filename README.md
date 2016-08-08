# The Denver Housing Market: or, Where Should I Move?

This project is designed to assess the Denver housing market, at the neighborhood level, during 2014 (the most recent year available for relevant data). In particular, it highlights the highs and lows as well as analyzes the most influential factors upon housing values within the five-county area. By integrating the Census Bureau's American Community Survey and a separate source to assess school district quality, and using LASSO regression to reduce the effects of collinearity, the results provide a complex picture of the housing market.

# Part One: AcquiredData
This part of the project collects and integrates the various Census and other datasets into one initial dataset, ACSData9_14.

# Part Two: ExploredData
This part of the project provides first glances into Denver housing values: highest, lowest, areas of four-year growth and decline. It then analyzes possible influential factors within neighborhoods such as population age and income, school district quality, and the overall housing quality (house age, vacancies, rentals). It includes graphs and maps to provide visual insights.

# Part Three: ModeledData
This part chooses from the most salient initial factors (found in Part Two) and analyzes housing values using OLS regression. It then includes a more advanced technique - LASSO regression - to account for multicollinearity problems. It then discusses the results - expected factors, surprising factors - and offers concluding thoughts.

# Part Four: Shiny
This part includes a Shiny leaflet interactive map for use in R, for users to visualize the quality of neighborhoods based on some relevant factors.

This master folder includes a PowerPoint presentation, reviewing the above.
