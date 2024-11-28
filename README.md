## European Statistics Awards on Nowcasting Gas Consumption

This repository contains the files submitted for official evaluation as
well as a brief summary of the nowcasting approach (see below).

### Competition Overview

The Energy Nowcasting Challenge - Gas is part of the European Statistics
Awards and focuses on developing methods to accurately nowcast gas
consumption across several European countries. Organized by the Jožef
Stefan Institute on behalf of Eurostat, the competition invited teams
worldwide to submit monthly nowcasts from June 2023 to March 2024. Teams
were evaluated based on *accuracy* (measured against official figures
once available) and *reproducibility* (assessed by a panel). Prizes were
awarded in both categories.

![](https://github.com/maxofn/ESA_GAS/blob/main/prediction-vs-official.png)

### Team and Achievements

This document provides an overview of the submission by team “PM10”,
consisting of:

-   Daniel Strenger (Graz University of Technology),
-   Maximilian Ofner (Graz University of Technology).

Our team was awarded the 1st Prize for Accuracy for the most accurate
forecasts and the Reproducibility Award.

### Acknowledgements

We express our gratitude to the organizers for this competition and
extend our congratulations to all participating and
prize-winning teams.

## Description of the Approach

### Target Gas Data

The competition focused on nowcasting monthly inland gas consumption
(\[NRG_CB_GASM\]),

-   \[G3000\] Natural gas
-   \[IC_CAL_MG\] Inland consumption of gas
-   \[TJ_GCV\] Terajoule (gross calorific value - GCV)

Official figures are published by Eurostat and can be accessed
[here](https://ec.europa.eu/eurostat/databrowser/view/nrg_cb_gasm/default/table?lang=en).
These figures are derived from country-specific questionnaires and
typically become available after a delay of one or more months.

### Purpose of Nowcasting

Due to the delay in the publication of official numbers, the aim is to
benchmark different methodologies and the use of external data
sources to provide timely estimates for the monthly gas data. An
effective nowcasting method could enable authorities to estimate monthly
inland gas consumption (say for December 2024) already by the end of
the corresponding month (December 31, 2024).

### Strategy Overview

To address this problem, we utilized external data sources with timely
availability to nowcast official gas consumption figures. Specifically,
we employed gas flow data from various stations provided by the European
Network of Transmission System Operators for Gas (ENTSOG). These data
are [publically available](https://transparency.entsog.eu) and generally
published within a few days, in contrast to the months-long delay of
Eurostat figures. Using these data, we computed nowcasts for each
country separately via a simple linear regression model. The methodology
is described below.

### Software Used

The methodology was implemented using the R programming language.

## Illustration

Below, we demonstrate the approach by computing a nowcast for the
monthly gas demand of March 2024 in Slovenia. As of March 31 (the
submission deadline for the March 2024 nowcast), the
official gas demand of Slovenia was available until February 2024.

### Define Time Window

As a first step, we select the time window for training data.

``` r
# Select time window for training data
time.begin <- "2020-04-01"
time.end   <- "2024-02-29"

# Create sequence of dates
months <- seq(as.Date(time.begin), as.Date(time.end), "months")
```

### Collect gas flow data

We download gas flow data (measured in gigawatt-hours, GWh) from a
central distribution station in Slovenia. Although the process is
automated via the ENTSOG API, we provide monthly aggregates in a CSV
file for a simplified presentation.

``` r
flow <- read.csv("data_example/gas_flow_monthly.csv")$flow
```

### Collect Official Gas Demand Data

Similarly, we download official Eurostat gas demand data (measured in
terajoules, TJ) and provide the preprocessed data as a CSV file.

``` r
demand <- read.csv("data_example/gas_demand.csv")$demand
```

### Visualize Data

The time series for gas flow and demand are visualized to assess their
relationship.

``` r
par(mar = c(5, 5, 4, 5) + 0.1)

# Plot demand
plot(months, demand, type = "l", col = "blue", lwd = 2,
     xlab = "Time", ylab = "Gas Demand (TJ)", xaxt = "n",
     ylim = range(demand), main = "Gas Demand and Flow Over Time")
axis(1, at = seq(from = min(months), to = max(months), by = "years"), 
     labels = format(seq(from = min(months), to = max(months), by = "years"), "%Y"))
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted", lwd = 0.8)

# Add flow on a secondary y-axis
par(new = TRUE)
plot(months, flow, type = "l", col = "red", lwd = 2, axes = FALSE, xlab = "", ylab = "", ylim = range(flow))
axis(4, las = 1)
mtext("Gas Flow (GWh)", side = 4, line = 3)

# Add legend
legend(x = months[25], y = 1280, legend = c("Gas Demand (TJ)", "Gas Flow (GWh)"), 
       col = c("blue", "red"), lwd = 2, bty = "n")
```

![](https://github.com/maxofn/ESA_GAS/blob/main/data_example/demand_flow_ts.png)

The time series exhibit a very strong correlation. This can also seen by
plotting gas demand against gas flow:

``` r
plot(demand, flow, xlab = "Gas Flow (GWh)", ylab = "Gas Demand (TJ)",
     main = "Gas Demand versus Gas Flow")
```

![](https://github.com/maxofn/ESA_GAS/blob/main/data_example/demand_vs_flow.png)

### Fit a Linear Regression Model

In principle, estimating the total gas flow should yield reasonably accurate
estimates for the demand (apart from unit conversion). However, this
approach may not fully account for systematic factors (e.g., loss of gas during transport,
additional stations that were not identified). To address these potential
discrepancies, we fit a simple linear regression model, considering monthly gas flow
as the predictor and gas demand as the response.

``` r
fit <- lm(demand ~ flow)
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = demand ~ flow)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -31.152 -12.804   1.586  11.422  27.118 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.867924   7.057691   0.123    0.903    
    ## flow        3.619226   0.008648 418.509   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 14.54 on 45 degrees of freedom
    ## Multiple R-squared:  0.9997, Adjusted R-squared:  0.9997 
    ## F-statistic: 1.751e+05 on 1 and 45 DF,  p-value: < 2.2e-16

According to the regression summary, the mean gas demand of a specific
month `i` is modeled by 

```math
E(\texttt{demand}_i) = 0.87 + 3.62 \times \texttt{flow}_{i}.
```

Here, the slope coefficient of 3.62 reflects the conversion factor from
GWh to TJ. For the case of Slovenia, the intercept term is not significant.
However, we retain it in the model, noting that it was found to be significant
in other countries.

### Obtain Nowcast

To nowcast March 2024, we need a proxy for the total gas flow in this
month. As of March 31, 2024, gas flow data were available for March 1–30. In other months
and countries, data for multiple days were often missing. To address
this, we calculate the average daily flow by dividing the total flow of
the available days by the number of observed days. We then multiply
this average by the total number of days in the month to estimate the
total flow for the month.

``` r
flow_march <- read.csv("data_example/gas_flow_march.csv")$flow
ndays <- 31 # There are 31 days in March
flow_march_aggregate <- mean(flow_march) * ndays
```

Using the regression model, we can then nowcast the month March.

``` r
nowcast <- predict(fit, newdata = data.frame("flow" = flow_march_aggregate))
nowcast
```

    ##        1 
    ## 3309.962

The nowcasted inland gas demand for Slovenia in March 2024 is 3309.962
GWh. A similar approach was applied to other countries, using data from
multiple stations if necessary. Note that correlations between flow and
demand vary by country.


## Key Takeaways

This competition highlights two main insights:

1.  The use of publicly available ENTSOG data demonstrates significant
    potential for reducing delays in publishing official figures.
    Identifying relevant gas distribution stations, however, is an issue that
    needs to be addressed.

2.  For nowcasting, the selection of suitable external data sources is
    arguably even more important than the choice of the forecasting
    algorithm. In this example, a simple linear regression model yielded
    accurate nowcasts, requiring minimal computation time and providing
    interpretable results. An extension of the methodology (e.g., to
    construct prediction intervals) is straightforward.

## References

ENTSOG TP (2024). *ENTSOG Transparency Platform*. European Network of
Transmission System Operators for Gas, Brussels, Belgium.
<https://transparency.entsog.eu>

Eurostat (2024). *R: Supply, transformation and consumption of gas -
monthly data*. Luxemburg, Luxemburg.
<https://doi.org/10.2908/NRG_CB_GASM>

R Core Team (2024). *R: A Language and Environment for Statistical
Computing*. R Foundation for Statistical Computing, Vienna, Austria.
<https://www.R-project.org/>
