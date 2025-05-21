# Valuation Office API - Data generation Script for downloading data
if(!require("pacman")){install.packages("pacman");library("pacman")}


## Pre-requisite: that you've setup R correctly to use the Bank proxy - check DOF instructions on this. Otherwise, you cannot download from an API at all (You won't be able to install packages either)

pacman::p_load(data.table, httr2, stringr, glue, jsonlite, this.path)

# Setup ------------------
ValOffice = "https://opendata.tailte.ie/api/Property/GetProperties?Fields=*&Format=json&Download=true"

# test = "https://opendata.tailte.ie/api/Property/GetProperties?Fields=*&Format=json&Download=true"

# test_dub = "https://opendata.tailte.ie/api/Property/GetProperties?Fields=*&Format=json&Download=true&LocalAuthority=DUBLIN%20CITY%20COUNCIL"

# test_dub_2 = "https://opendata.tailte.ie/api/Property/GetProperties?Fields=*&Format=geojson&Download=true&LocalAuthority=DUBLIN%20CITY%20COUNCIL"

councils = toupper(unlist(
  strsplit("Carlow, Cavan, Clare, Cork, Cork City, Donegal, Dublin City Council, Dun Laoghaire Rathdown, Fingal, Galway, Galway City, Kerry, Kildare, Kilkenny, Laois, Leitrim, Limerick, Longford, Louth, Mayo, Meath, Monaghan, Offaly, Roscommon, Sligo, South Dublin County Council, Tipperary, Westmeath, Wexford, Wicklow, Waterford", split = ", ")
))


LocalAuthorityList = data.table::fcase(
  grepl("LIMERICK|WATERFORD", councils), paste(councils, "CITY AND COUNTY COUNCIL"),
  grepl("DUN LAOGHAIRE RATHDOWN", councils), paste(councils, "CO CO"),
  grepl("CORK CITY|GALWAY CITY", councils), paste(councils, "COUNCIL"),
  grepl("COUNCIL", councils), councils,
  !grepl("COUNCIL|LIMERICK|WATERFORD|DUN LAOGHAIRE", councils), paste(councils, "COUNTY COUNCIL")
)

req <- httr2::request(ValOffice)

requests <- lapply(LocalAuthorityList, function(x) { httr2::req_url_query(req, LocalAuthority = x) } )

# Collecting Data ---------------------------------
## NOTE: IN the below command, You are sending 31 requests - 1 for each Local Authority. This script automates the data collection. 
## Because it sends 31 requests at ONE TIME, it can error and not actually grab all LAs. 
## This only usually happens once - however, I have set ' on_error = "stop" ' So that you have something pop-up when it occurs
## This mainly happens when it gets to downloading the Dublin City Council data 
## All you have to do is run the command again, it should be fine the second time
## If it continues to happen, use httr2::req_perform_sequential()
## Same syntax as below, but goes one by one, so slower. 
### req_perform_sequential(requests, on_error = "stop", progress = TRUE)


### Docs: ?httr2::req_perform_parallel and ?httr2::req_perform_sequential()

responses <- httr2::req_perform_parallel(requests, on_error = "stop", progress = TRUE)

# Test case.

# test <- fromJSON(resp_body_string(responses[[1]])) 
# head(test)

# Processing Data -----------------------
# Data excludes "Central Valuation List" - not for purposes of any internaal analysis on CRE in the Bank.

collect_response <- function(response) {
  encoded <- httr2::resp_body_string(response)
  data <- jsonlite::fromJSON(encoded)
  data.table::setDT(data)
  return(data)
}

clean_response <- function(data, date_cols = c("PublicationDate", "ValuationDate")) {
  data[, (date_cols) := lapply(.SD, as.Date, "%d/%m/%Y"), .SDcols = date_cols]
  # NOTE: Below - If you use:
  # data[Category != "CENTRAL VALUATION LIST"]
  # It will also remove NA values (i.e. all of Cork data.) For now - do the following:
  
  data = data[!(Category %in% "CENTRAL VALUATION LIST")]
  data[, Eircode := gsub(" ", "", Eircode)]
  setorder(data, -PublicationDate, ValuationDate)
  setcolorder(data, c("PublicationDate", "PropertyNumber", "LocalAuthority", "ValuationDate", "Xitm", "Yitm", "Valuation", "ValuationReport", "Uses", "Category", "CarPark", "AdditionalItems", "Address1", "Address2", "Address3", "Address4", "Address5", "Eircode", "County"))
  return(data)
}

collected <- lapply(responses, collect_response)
valuations <- lapply(collected, clean_response)
out <- data.table::rbindlist(valuations)

# out[, .(`No. of Local Authorities` = uniqueN(LocalAuthority), NAV = scales::label_comma()(sum(Valuation))), keyby = .(ValuationDate)]

# out2 <- copy(out)

# Outputting Data -----------------------
getwd(this.path::this.path())

data_out_path = paste0(this.path::this.dir(),"/data/cleaned/valuations_data/", Sys.Date(), "-")

saveRDS(out, file = paste0(data_out_path, "ValOff_Valuations.rds"))

## CSV Output ---------------
## Note - dealing with nested data-frames with the data.frame. CSV does not like this at all. Have to split the "ValuationReport" data for each property (contains all the juicy stuff - floors, floor area, and value per m2) from the "Stock" data
