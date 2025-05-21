# 1_processing-data-script-fsn
# Takes the RAW data for valuations, spits out much more useful data with CVs, for use across files. 
if(!require("pacman")){install.packages("pacman");library("pacman")}
pacman::p_load(data.table, tidyverse, lubridate, readxl, knitr, scales)

# Loading ------------
# 0_data_extraction.R covers how to grab data from the API.

# Valuations file
valuations <- readRDS("data/cleaned/valuations_data/2024-05-09-ValOff_Valuations.rds")
valuations <- valuations[County != "CORK"]

# Rental Growth Index
# msci_rental_seg <- readxl::read_xlsx("data/cleaned/msci/msci_rental_growth_2023q4_subsegmented.xlsx", sheet = 1)
msci_rental_seg <- readxl::read_xlsx("data/cleaned/msci/msci_rental_growth_2024q1_subsegmented.xlsx", sheet = 1)

# Yields data 
yields_segmented <- readxl::read_xlsx("data/cleaned/cbre_yields/cbre_yields_2024q1.xlsx")


setDT(msci_rental_seg)
setDT(yields_segmented)

# Processing ----------------
## Simple Columns -------
msci_rental_seg <- msci_rental_seg[, date := as.Date(Period)][order(Segmentation, Segment1, Segment2, date)]

msci_rental_seg[, `:=` (yqtr = zoo::as.yearqtr(date),
                        Dataset = NULL,
                        Filter = NULL)]

setcolorder(msci_rental_seg, c("yqtr"))

valuations[,val_qtr := zoo::as.yearqtr(ValuationDate)]


setcolorder(valuations, c("val_qtr", "ValuationDate", "Valuation", "LocalAuthority"))

## Segmentation ----------------------------
valuations[, segment := fcase(
  Category %chin% c("RETAIL (SHOPS)", "RETAIL (WAREHOUSE)"), "retail",
  Category == "OFFICE", "office",
  Category %chin% c("INDUSTRIAL USES"), "industrial",
  default = "all"
)]

### Office Prime/Non-Prime Split -------
# Offices are easy - Prime / Non-Prime

valuations[Category == "OFFICE", office_gen := 
             fcase(
               grepl("4th gen|3rd gen", Uses, ignore.case = TRUE, perl = TRUE), "Prime",
               default = "Non-Prime"
             )]


## Floor Area data --------
# Found in valuation report nested frame.

# valuations[, NetFloorArea := purrr::map_dbl(ValuationReport, ~sum(.x$Area))]
valuation_report = data.frame(PropertyNumber = valuations[rep(seq_len(nrow(valuations)), vapply(valuations$ValuationReport, nrow, 0L)),PropertyNumber], data.table::rbindlist(valuations$ValuationReport))

setDT(valuation_report)
val_areas <- valuation_report[Area >= 0, .(NetFloorArea = sum(Area)), by = .(PropertyNumber)]
valuations <- merge(valuations, val_areas, all.x = TRUE)
valuations[is.na(NetFloorArea), NetFloorArea := 0]

## Address and Eircode Routing Key --------------
## Address
valuations[, Address := do.call(paste, c(lapply(.SD, function(x) {replace(x, is.na(x), "")}), sep=" ")), .SDcols = Address1:Address5]

## Exclusion of certain commercial property ----------------
valuations <- valuations[ !( !(Category %chin% c("OFFICE", "RETAIL (SHOPS)", "RETAIL (WAREHOUSE)")) & grepl("generating|incinerator|tolls|terminal|airport|\\bport\\b|\\bbus\\b|\\bfuneral\\b|\\bfire\\b \\bstation\\b|\\bantenna|\\bcaravan|\\basylum", Uses, ignore.case = TRUE)),]
valuations <- valuations[!(Category == "UTILITY" & !grepl("\\bwind\\b", Uses, ignore.case = TRUE)), ]
valuations <- valuations[!(Category %chin% c("NON-LIST", "CHECK CATEGORY", "NO CATEGORY SELECTED", "MINERALS"))]




# Rental Indexing -----------
## Subsegment Creation --------
msci_rental_seg <- msci_rental_seg[!(Segment1 %chin% c("Retail - Shopping Centre", "Retail - Provincial"))]

msci_rental_seg[, `:=` (Segment_Full = fifelse(!is.na(Segment2), paste(Segment1, Segment2, sep = " - "),
                                               Segment1),
                        Location = fcase(
                          grepl("^All Segment|^Global Property Classification sectors", Segmentation), "NATIONAL",
                          grepl("^Global Cities|^Sector by Region", Segmentation) & Segment1 != "Retail Warehouse", "DUBLIN",
                          Segment1 == "Retail Warehouse", "NATIONAL",
                          default = NA)
)]


msci_rental_seg[, Subsegment := fifelse(Location == "NATIONAL", Segment1,
                                        fifelse(Segment1 == "Dublin", Segment2, Segment1)
)]

msci_rental_seg[, regional_multiplier := Value[Period == "2024-03-31"]/Value, by = .(Location, Subsegment)]

msci_rental_seg <- msci_rental_seg[, .(yqtr, Location, Subsegment, regional_multiplier)]

dublin_dupes <- msci_rental_seg[Subsegment %chin% c("Retail Warehouse", "Retail"), ][, Location := "DUBLIN"]

msci_rental_seg <- rbind(msci_rental_seg, dublin_dupes)

valuations[, Location := fifelse(County == "DUBLIN", "DUBLIN", "NATIONAL")]

valuations[, Subsegment := fcase(
  Location == "NATIONAL" & Category != "RETAIL (WAREHOUSE)", segment,
  Category == "RETAIL (WAREHOUSE)", "retail warehouse",
  Location == "DUBLIN" & !(segment %chin% c("office", "retail")), segment,
  segment == "office", fifelse(LocalAuthority == "DUBLIN CITY COUNCIL", "office - central dublin", "Office - Rest of Dublin"),
  segment == "retail" & Category != "RETAIL (WAREHOUSE)" & LocalAuthority == "DUBLIN CITY COUNCIL", fifelse(grepl("grafton", Address, perl = TRUE, ignore.case = TRUE), "retail - grafton street",   fifelse(grepl("henry|mary s", Address, perl = TRUE, ignore.case = TRUE), "Retail - Henry/MaryStreet", "retail - other city centre")                                                               ),
  default = "retail"
)]

valuations[!(Subsegment %chin% c("Retail - Henry/MaryStreet", "Office - Rest of Dublin")), Subsegment := stringr::str_to_title(Subsegment)]

### 
## Eircodes
eircode_pattern = "([AC-FHKNPRTV-Y]{1}[0-9]{2}|D6W)[ ]?[0-9AC-FHKNPRTV-Y]{4}"

valuations[, Eircode := fcoalesce(
  Eircode,
  gsub(" ", "", str_extract(Address, eircode_pattern))
)]

valuations[, RoutingKey := substr(Eircode, 1, 3)]

valuations[Location == "DUBLIN" & is.na(Eircode), missing_routing_key := sprintf("D%02d", as.numeric(
  str_extract(Address, regex("(?<=dublin[ ]{0,3})[0-9]{1,2}", ignore_case = TRUE))))]

valuations[!is.na(missing_routing_key) & missing_routing_key == "DNA", missing_routing_key := NA_character_]

valuations[, RoutingKey := fcoalesce(RoutingKey, missing_routing_key)]

#

## Indexation -----------
valuations <- msci_rental_seg[valuations, on = c("yqtr" = "val_qtr", "Location", "Subsegment")][, indexed_vals := Valuation*regional_multiplier]

valuations[Location == "DUBLIN" & Subsegment == "Retail", Subsegment := "Retail - Rest of Dublin"]

# Capitalisation -------------
yields_segmented <- yields_segmented[, .(Location, Subsegment, yield)]

valuations <- yields_segmented[valuations, on = c("Location", "Subsegment")][, `:=` (capital_value =  indexed_vals*(100/yield))]

valuations[, segment := str_to_title(segment)]


#
valuations[, .(Rent = scales::label_comma()(sum(indexed_vals)), `Capital Value` = label_comma()(sum(capital_value)))] |> kable(align = "cc")

# 117.2bn

valuations[County == "DUBLIN", .(Rent = scales::label_comma()(sum(indexed_vals)), `Capital Value` = label_comma()(sum(capital_value)))] |> kable(align = "cc")


## Cork --------
### Assumptions -----------
# For Office, we know from private reporting - 15% of the size of Dublin on floor area basis (Savils)
# For I&L - can't find Dublin, so scaling by population

cork_imputed <- valuations[County == "DUBLIN", .(indexed_vals = sum(indexed_vals)), keyby = .(segment)]

cork_imputed[, scalars := c(0.35, 0.4, 0.1535, 0.4)]

cork_imputed[, imputed_rents := indexed_vals*scalars]

cork_imputed[, Location := "NATIONAL"]

cork_imputed <- cork_imputed[yields_segmented,  on = c("segment" = "Subsegment", "Location"), nomatch = 0][, capital_value := imputed_rents*(100/yield)][,]

##
cork_imputed[, ':=' (Location = NULL,
                     County = "CORK")]

saveRDS(valuations, paste0("data/fsn/", Sys.Date(), "-valuations_processed.rds"))
saveRDS(cork_imputed, "data/fsn/cork_summary_processed.rds")
