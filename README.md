# average_spei_nuts
Average monthly SPEI Global Drought Monitor over NUTS2016 polygons

## List of Variables

```{r var-list, echo=FALSE}
library(dplyr)
var_list <- data.frame(
  variable = c("indicator_code", "unit", "geo", "time", "value",
				 "year", "month", "day", "frequency", "estimate", "db_source"),
  class = c("string", "string", "string", "date", "integer",
				 "integer", "integer", "integer", "string", "string", "string"),
  description = c("Unique code for the indicator", "Measurement unit", "NUTS2016 geographic code of observation",
                "Time of observation formatted as date or first day of the year followed by year for annual observations",
                "Value of observation in numeric form",
				"Year of observation", "Month of observation", "Day of observation", "Frequency of observation, currently *A*nnual, *Q*uarterly, *M*onthly, *D*aily, or unknown",
                "Annotated *missing* for missing values",
                "Database source code labeled as unique name with snake_case")
)


var_list %>%
knitr::kable()

```
## List of datasets included
"nuts0_spei01_average.csv" = average weighted SPEI values on the NUTS0 level
"nuts1_spei01_average.csv" = average weighted SPEI values on the NUTS1 level
"nuts2_spei01_average.csv" = average weighted SPEI values on the NUTS2 level
"nuts3_spei01_average.csv" = average weighted SPEI values on the NUTS3 level
