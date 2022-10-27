# Seattle rental housing monitoring 

This repository contains data, code, and a report on Seattle's housing market. The project was completed by [TrueRoll](https://www.trueroll.io/) in September and October of 2022 in partnership with the City of Seattle's Department of Constructions and Inspections. 

[Download the final report.](https://github.com/trueroll/tr_rental_monitoring_seattle/blob/main/outputs/Housing-Affordability-Trends-in-Seattle-WA.html)

## Project scope

The initial project scope was to study any of the following:

- The current state and changes over time in the composition of rental housing stock, with an emphasis on loss of gain of different size classes, such as single-family/single-unit properties, small multi-unit properties, and larger rental properties.
- The current state and changes over time in the ownership of rental housing, with an emphasis on the gain or loss of “small” landlords (those who own one or few rental properties and for whom operating a rental property may not be a primary source of income), professional landlords, and large corporate landlords. Part of the study may include defining types of landlords.
- The current state and changes over time in the rents charged in different ownership types of rental properties. Part of the goal is to determine if “small” landlords and larger landlords follow different pricing practices.	
-Other dynamics and implications associated with changes in the composition of landlords who own rental property in Seattle. (Topics of interest include practices related to property maintenance and remodeling, practices related to leasing and evictions impacting tenants’ housing stability, and impact on supply of single-family homes for purchase and rent.) 
- How the nature and type of rental property ownership is likely to change in the future.
- Information and data gaps. Recommendations for ongoing rent data collection.
- To the extent possible, this information should be broken into geographic sub areas.
- The study should be presented in a report format, with underlying data and mapping provided electronically. The consultant may be asked to present findings to City officials and community members.
- The study should also include recommendations for ongoing rental market data gathering and analysis on the changing nature of rental-housing ownership and its impacts.

Our final report addressed points one through three. We did not speculate about the future of rental property in Seattle. We had no data that gave us visibility on the items listed in point four.

## Repository structure

Some elements of our analysis leveraged proprietary data or code. We have stripped those elements from this repository and its history. This limits reproducibility to some degree but is necessary to protect TrueRoll's IP. `outputs/Housing Affordability Trends in Seattle WA.Rmd` produces the final report. Each graph and table is numbered. Corresponding tables can be found in the `outputs` folder. 

The data folder contains raw and pre-processed data. Much of this cannot be reproduced without access to TrueRoll's systems. For example, any database call with a 'query#' argument is a call where we have masked the content of the query.

## Contact

If you have questions about this work, please contact [TrueRoll](mailto:info@trueroll.io).
