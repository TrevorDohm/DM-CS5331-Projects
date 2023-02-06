# Google Global Mobility Report

See how your community is moving around differently due to COVID-19
As global communities respond to COVID-19, we've heard from public health officials that the same type of aggregated, anonymized insights we use in products such as Google Maps could be helpful as they make critical decisions to combat COVID-19.

These Community Mobility Reports aim to provide insights into what has changed in response to policies aimed at combating COVID-19. The reports chart movement trends over time by geography, across different categories of places such as retail and recreation, groceries and pharmacies, parks, transit stations, workplaces, and residential.


Source: https://www.google.com/covid19/mobility/index.html

File: Global_Mobility_Report.csv



# USAFacts (COVID infections/deaths) + Census data

This data from USAFacts  provides US COVID-19 case and death counts by state and county. This data is sourced from the CDC, and state and local health agencies. 
Census data, by itself, provides useful information about the population, but it can also be joined with other datasets to provide valuable insight about the population. One area where this can be beneficial is in healthcare and population health. Census data can be combined with other publicly available datasets such as Medicare data, Center for Disease Control (CDC) data, and many others to provide valuable insights such as hospitalization trends, healthcare utilization, and so on.

File: COVID-19_cases_plus_census.csv

Sources: 
https://console.cloud.google.com/marketplace/details/usafacts-public-data/covid19-us-cases?filter=solution-type:dataset&filter=category:covid19&id=3eaff9c5-fbaf-47bb-a441-89db1e1395ab
https://usafacts.org/issues/coronavirus/

Note: You can go to the link and get more data using queries like:

SELECT
  *
FROM `bigquery-public-data.covid19_usafacts.summary` covid19
JOIN `bigquery-public-data.census_bureau_acs.county_2017_5yr` acs
ON covid19.county_fips_code = acs.geo_id
WHERE date = DATE_SUB(CURRENT_DATE(), INTERVAL 7 day) # yesterday



Cases Texas

File: COVID-19_cases_TX.csv

Sources:
https://console.cloud.google.com/marketplace/details/usafacts-public-data/covid19-us-cases?filter=solution-type:dataset&filter=category:covid19&id=3eaff9c5-fbaf-47bb-a441-89db1e1395ab

SELECT
  *
FROM `bigquery-public-data.covid19_usafacts.summary` covid19 WHERE state = "TX"


More data

https://console.cloud.google.com/marketplace/browse?filter=solution-type:dataset&filter=category:covid19
