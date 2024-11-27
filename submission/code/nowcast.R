#
# NOWCAST GAS DEMAND
#
# The Second Round of the European Statistics Awards for Nowcasting.
#
# Authors: Maximilian Ofner, Daniel Strenger
# Affiliation: Graz University of Technology
#

################################################################################
# Packages and functions --------------------------------------------------
################################################################################

# Load required R-packages for downloading data and pre-processing
library(RJSONIO)
library(lubridate)
library(dplyr)
library(MASS)

# Define function to read files in R line by line
# Source: https://stackoverflow.com/questions/12626637/read-a-text-file-in-r-line-by-line

processFile = function(filepath) {
  stations <- c()
  con = file(filepath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    stations <- c(stations, line)
  }
  close(con)
  return(stations)
}

################################################################################
# Preliminaries -----------------------------------------------------------
################################################################################

# Select time window for training data
time.begin <- "2020-04-01"
time.end   <- "2024-02-29"

# Select month (last day) in consideration for nowcast
time.nowcast <- "2024-03-31"

# Set working directory
directory <- dirname(rstudioapi::getSourceEditorContext()$path)
directory <- paste0(directory, "/", format(as.Date(time.nowcast),"%B"))
setwd(directory)

# Generate sequences of days and months
days <- seq(as.Date(time.begin), as.Date(time.end), "days")
months <- seq(as.Date(time.begin), as.Date(time.end), "months")
first.days <- floor_date(days, "month")

# Create vector with acronyms of EU countries
countries.total <- c("AT", "BE", "BG", "CZ", "DE", "DK", "EE", "EL", "ES", "FI",
                     "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL",
                     "PL", "PT", "RO", "SE", "SI", "SK")

# Choose subset of countries to be nowcasted. Information of corresponding
# "distribution" and "final consumer" points could be easily extracted for those
# countries. Other countries may be added in the future.
countries <- c("BE", "BG", "DE", "ES", "FR", "HR", "HU", "IT", "LV", "NL", "PL",
               "PT", "RO", "SI")

# Initialize dataframes containing the R-squared measure and nowcasts
rsq <- data.frame(country = countries, R2 = numeric(length(countries)))
pred <- data.frame(country = countries, nowcast = numeric(length(countries)))

# Compute nowcasts for each country separately.

for(country in countries) {
  print(country)
  
  ##############################################################################
  # Download and process ENTSOG data ----------------------------------------
  ##############################################################################
    
  # Concatenate API call to access ENTSOG data
  # Station IDs are contained in a separate file
  prefix <- "https://transparency.entsog.eu/api/v1/operationaldata.csv?forceDownload=true&pointDirection="
  postfix <- "&indicator=Physical%20Flow&periodType=day&timezone=CET&periodize=0&limit=-1&isTransportData=true&dataset=1&delimiter=comma"
  stations <- processFile(paste0(getwd(), "/API CALLS/", country, ".txt"))
  
  # Download training data via API call
  download.file(paste0(prefix, stations[1], "from=", time.begin, "&to=", time.end, postfix), paste0(getwd(), "/ENTSOG/gas_", country, ".csv"))
  
  # Process the data
  data_raw <- read.csv(paste0(getwd(), "/ENTSOG/gas_", country, ".csv"), header = TRUE)
  data_raw <- data_raw[c("periodFrom", "pointKey", "value")]
  data_raw$periodFrom <- as.Date(data_raw$periodFrom)
  data_reshaped <- reshape(data_raw, idvar = "pointKey", timevar = "periodFrom", direction = "wide")
  rownames(data_reshaped) <- data_reshaped[,1]
  data_reshaped <- data_reshaped[,-1]
  
  # Add flows for all points for each day and convert from KWh to GWh
  distribution_data <- colSums(data_reshaped, na.rm = TRUE)/1e6

  df <- data.frame(time = first.days, value = unname(distribution_data))
  gas_distribution <- (df %>%
                         group_by(time) %>%
                         summarize(sum = sum(value)))$sum  # aggregate month-wise
  

  # Download data of the month in consideration for nowcasting
  download.file(paste0(prefix, stations[1], "from=", as.Date(time.end) + 1, "&to=",
                       as.Date(time.nowcast) - 1, postfix),
                paste0(getwd(), "/ENTSOG/cm_gas_", country, ".csv"))
  
  # Process the data
  data_cm_raw <- read.csv(paste0(getwd(), "/ENTSOG/cm_gas_", country, ".csv"), header = TRUE)
  data_cm_raw <- data_cm_raw[c("periodFrom", "pointKey", "value")]
  data_cm_raw$periodFrom <- as.Date(data_cm_raw$periodFrom)
  
  data_cm_reshaped <- reshape(data_cm_raw, idvar = "pointKey",
                              timevar = "periodFrom", direction = "wide")
  rownames(data_cm_reshaped) <- data_cm_reshaped[,1]
  data_cm_reshaped <- data_cm_reshaped[,-1]
  nr.days <- lubridate::days_in_month(as.Date(time.nowcast))
  data_cm_reshaped <- data_cm_reshaped[which(rowSums(!is.na(data_cm_reshaped))>0),]
  
  # Account for missing days in the current month
  for(i in 1:dim(data_cm_reshaped)[1]) {
    
    # Number of days for which data is available
    nr.obs <- rowSums(!is.na(data_cm_reshaped[i,]))
    
    # Estimate flow over the whole month from the data that is already available
    data_cm_reshaped[i,] <- data_cm_reshaped[i,] /nr.obs * nr.days
  }
  
  # Conversion from KWh to GWh
  distribution_cm <- sum(data_cm_reshaped, na.rm = TRUE)/1e6
  
  
  # If both "distribution" and "final consumer" data are relevant, repeat the
  # above steps for the other station type.
  
  if(length(stations) > 1) {
    # There is another relevant station type
    
    # Download training data via API call
    download.file(paste0(prefix, stations[2], "from=", time.begin, "&to=",
                         time.end, postfix),
                  paste0(getwd(), "/ENTSOG/gas2_", country, ".csv"))
    
    # Process the data
    data_raw <- read.csv(paste0(getwd(), "/ENTSOG/gas2_", country, ".csv"), header = TRUE)
    data_raw <- data_raw[c("periodFrom", "pointKey", "value")]
    data_raw$periodFrom <- as.Date(data_raw$periodFrom)
    data_reshaped <- reshape(data_raw, idvar = "pointKey", timevar = "periodFrom", direction = "wide")
    rownames(data_reshaped) <- data_reshaped[,1]
    data_reshaped <- data_reshaped[,-1]
    
    # Conversion from KWh to GWh
    distribution_data <- colSums(data_reshaped, na.rm = TRUE)/1e6 # Conversion from KWh to GWh
    
    df <- data.frame(time = first.days, value = unname(distribution_data))
    gas_distribution2 <- (df %>%
                           group_by(time) %>%
                           summarize(sum = sum(value)))$sum # aggregate month-wise
    
    # Download data of the month in consideration for nowcasting
    download.file(paste0(prefix, stations[2], "from=", as.Date(time.end) + 1, "&to=",
                         as.Date(time.nowcast) - 1, postfix),
                  paste0(getwd(), "/ENTSOG/cm_gas2_", country, ".csv"))
    
    # Process the data
    data_cm_raw <- read.csv(paste0(getwd(), "/ENTSOG/cm_gas2_", country, ".csv"), header = TRUE)
    data_cm_raw <- data_cm_raw[c("periodFrom", "pointKey", "value")]
    data_cm_raw$periodFrom <- as.Date(data_cm_raw$periodFrom)
    data_cm_reshaped <- reshape(data_cm_raw, idvar = "pointKey",
                                timevar = "periodFrom", direction = "wide")
    rownames(data_cm_reshaped) <- data_cm_reshaped[,1]
    data_cm_reshaped <- data_cm_reshaped[,-1]
    nr.days <- lubridate::days_in_month(as.Date(time.nowcast))
    data_cm_reshaped <- data_cm_reshaped[which(rowSums(!is.na(data_cm_reshaped))>0),]
    
    # Account for missing days in the current month
    for(i in 1:dim(data_cm_reshaped)[1]) {
      
      # Number of days for which data is available
      nr.obs <- rowSums(!is.na(data_cm_reshaped[i,]))
      
      # Estimate flow over the whole month from the data that is already available
      data_cm_reshaped[i,] <- data_cm_reshaped[i,] /nr.obs * nr.days
    }
    
    # Conversion from KWh to GWh
    distribution2_cm <- sum(data_cm_reshaped, na.rm = TRUE)/1e6
  }
  
  ##############################################################################
  # Download and process EUROSTAT data ---------------------------------------
  ##############################################################################
  
  # Concatenate API call to access EUROSTAT data
  prefix <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/NRG_CB_GASM/M.IC_CAL_MG.G3000.TJ_GCV."
  postfix <- "/?format=SDMX-CSV&compressed=false&"
  
  # Download the data
  download.file(paste0(prefix, country, postfix, "startPeriod=",
                       format(as.Date(time.begin), "%Y-%m"), "&endPeriod=",
                       format(as.Date(time.end), "%Y-%m")),
                paste0(getwd(), "/EUROSTAT/demand_", country, ".csv"))
  
  # Process the data
  data_raw <- read.csv(paste0(getwd(), "/EUROSTAT/demand_", country, ".csv"), header = TRUE)
  gas_demand <- rep(NA, length(months))
  gas_demand[1:length(data_raw$OBS_VALUE)] <- data_raw$OBS_VALUE
  
  #
  # Perform nowcasting using linear regression ------------------------------
  #

  # The indicator NRG_CB_GASM is the quantity that should be nowcasted and the
  # response variable for our linear model.
  
  if(length(stations) == 1) {
    
    # Use a linear model with only one predictor if either "distribution" or
    # "final consumer" data is given
    
    # Some preliminaries
    data <- data.frame(time = months, distribution = gas_distribution, demand = gas_demand)
    idx_last <- dim(data)[1]
    newdat <- data.frame(time = as.Date(time.end) + 1, distribution = distribution_cm, demand = NA)
    
    # Train model with historical data
    lm <- lm(demand ~ distribution, data = data)
    
    # Compute nowcast
    nowcast <- predict(lm, newdata = newdat)
  } else {
    
    # Use a linear model with only two predictors if both "distribution" and
    # "final consumer" data are given
    
    # Some preliminaries
    data <- data.frame(time = months, distribution1 = gas_distribution,
                       distribution2 = gas_distribution2, demand = gas_demand)
    newdat <- data.frame(time = as.Date(time.end) + 1, distribution1 = distribution_cm,
                         distribution2 = distribution2_cm, demand = NA)
    
    # Train model with historical data
    lm <- lm(demand ~ distribution1 + distribution2, data = data)
    
    # Compute nowcast
    nowcast <- predict(lm, newdata = newdat)
  }
  
  # Save the data that was used
  data <- rbind(data, newdat)
  write.csv(data, paste0(getwd(), "/data/demand_", country, ".csv")) 
  
  rsq$R2[which(rsq$country == country)] <- summary(lm)$r.squared
  pred$nowcast[which(rsq$country == country)] <- nowcast
}

################################################################################
# Save nowcasts in JSON file ----------------------------------------------
################################################################################

df <- rep(NA, length(countries.total))
df[match(countries, countries.total)] <- pred$nowcast
names(df) <- countries.total
write(toJSON(df), "Submission/nowcasts.json")
