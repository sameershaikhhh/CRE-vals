pacman::p_load(terra, tmap, httr2, ggplot2, tidyverse, leaflet, data.table, jsonlite, sf)

val_api <- "https://opendata.tailte.ie/api/Property/GetProperties?Fields=*&Format=json&Download=true"

val_req <- httr2::request(val_api)

dublin <- c("DUN LAOGHAIRE RATHDOWN CO CO", "DUBLIN CITY COUNCIL", "FINGAL COUNTY COUNCIL", "SOUTH DUBLIN COUNTY COUNCIL", "MEATH COUNTY COUNCIL")

requests <- lapply(dublin, function(x) {val_req |> req_url_query(LocalAuthority = x)})

dcc <- val_req |>
  httr2::req_url_query(LocalAuthority = "DUBLIN CITY COUNCIL") |>
  httr2::req_perform()

dcc_DT <- setDT(jsonlite::fromJSON(rawToChar(dcc$body)))

dub <- httr2::req_perform_parallel(requests, progress = TRUE)

dub_dt <- lapply(dub, \(x) {jsonlite::fromJSON(rawToChar(x$body)) |> setDT()})
dub_dt <- rbindlist(dub_dt)
dub_dt <- dub_dt[Category != "CENTRAL VALUATION LIST"]


dcc_DT <- dcc_DT[Category != "CENTRAL VALUATION LIST",]

setcolorder(dcc_DT, c("PublicationDate", "PropertyNumber", "Category", "Uses", "Valuation", "ValuationReport", "AdditionalItems", "CarPark", "Xitm", "Yitm", "LocalAuthority", "ValuationDate", "Address1", "Address2", "Address3", "Address4", "Address5", "County", "Eircode"))

date_cols <- grep("Date", names(dcc_DT), value = TRUE, perl = TRUE)

dub_dt[, (date_cols) := lapply(.SD, as.Date, "%d/%m/%Y"), .SDcols = date_cols]

dub_dt[, Eircode := gsub(" ", "", Eircode)]

dcc_DT[, (date_cols) := lapply(.SD, as.Date, "%d/%m/%Y"), .SDcols = date_cols]

dcc_DT[, Eircode := gsub(" ", "", Eircode)]

dub_office <- dcc_DT[Category == "OFFICE" & !is.na(Xitm),]
dub_office[, office_gen := fifelse(grepl("3rd gen|4th gen", Uses, perl = TRUE, ignore.case = TRUE), "Prime", "Non-Prime")]

dub_office[, val_quantile := cut(Valuation, quantile(Valuation, probs = 0:4/4), include.lowest = TRUE, labels = FALSE), by = .(LocalAuthority)]


dub_office[, office_select := fcase(
  grepl("3rd gen|4th gen", Uses, perl = TRUE, ignore.case = TRUE), "3rd & 4th Gen",
  grepl("2nd gen|1st gen", Uses, perl = TRUE, ignore.case = TRUE), "1st & 2nd Gen (lower quality)",
  grepl("heritage|victorian|georgian", Uses, perl = TRUE, ignore.case = TRUE), "Heritage Office",
  default = "Other / Own Door / Over The Shop"
)]

dub_office_st <- sf::st_as_sf(dub_office, coords = c("Xitm", "Yitm"), crs = 2157)

dub_office_st <- dub_office_st |> filter(!(PublicationDate %in% c("2022-04-29", "2024-04-22")))

tmap_mode("view")

tm_basemap(leaflet::providers$Stadia.AlidadeSmoothDark) + 
  tm_shape(dub_office_st) + 
  tm_dots(col = "office_select", palette = "Set3", size = 0.04, popup.vars = c("PropertyNumber", "office_gen", "Uses", "Address1", "Address2", "Eircode")) 

tm_shape(dub_office_st) + tm_shape(dub_office_st) + 
  tm_dots(col = "office_gen", palette = "Set1") 

tm_basemap(leaflet::providers$Stadia.StamenTonerBackground) + 
  tm_shape(dub_office_st) + 
  tm_dots(size = 0.05, popup.vars = c("PropertyNumber", "office_gen", "Uses", "Address1", "Address2", "Eircode")) + tm_facets("office_gen", nrow = 1)

tm_basemap(leaflet::providers$Stadia.StamenTonerBackground) + 
  tm_shape(dub_office_st) + 
  tm_dots(size = 0.05, popup.vars = c("PropertyNumber", "office_gen", "Uses", "Address1", "Address2", "Eircode")) + tm_facets("office_gen", nrow = 1)
# -----------------------------
dcc_ind <- dub_dt[Category == "INDUSTRIAL USES" & !is.na(Xitm),]
dcc_ind[, use_case := fcase(
  grepl("data", Uses, perl = TRUE, ignore.case = TRUE), "Data Centre",
  grepl("warehouse", Uses, perl = TRUE, ignore.case = TRUE), "Warehouse",
  default = "Other"
)]

dcc_ind_st <- sf::st_as_sf(dcc_ind, coords = c("Xitm", "Yitm"), crs = 2157)

tm_basemap(leaflet::providers$OpenStreetMap) + 
  tm_shape(dcc_ind_st |> filter(use_case == "Data Centre")) +
  tm_dots( size = 0.03)

# Dub industrial ---------


dub_ind <- dub_dt[Category == "INDUSTRIAL USES" & !is.na(Xitm),]
dub_ind[, use_case := fcase(
  grepl("data", Uses, perl = TRUE, ignore.case = TRUE), "Data Centre",
  grepl("warehouse", Uses, perl = TRUE, ignore.case = TRUE), "Warehouse",
  grepl("factory", Uses, perl = TRUE, ignore.case = TRUE), "Factory",
  grepl("workshop", Uses, perl = TRUE, ignore.case = TRUE), "Workshop",
  default = "Other"
)]

dub_ind[, val_quantile := cut(Valuation, quantile(Valuation, probs = 0:4/4), include.lowest = TRUE, labels = FALSE), by = .(LocalAuthority)]



dub_ind_st <- sf::st_as_sf(dub_ind, coords = c("Xitm", "Yitm"), crs = 2157)

dub_ind_st <- dub_ind_st |> filter(PropertyNumber != 5021061)

tm_basemap(leaflet::providers$Stadia.StamenTonerBackground) + 
  tm_shape(dub_ind_st |> select(!ValuationReport) |> filter(use_case == "Data Centre")) +
  tm_dots(size = 0.02, popup.vars = c("PropertyNumber", "Uses", "Address1", "Address2", "Address3", "Eircode", "Valuation"), col = "green")

tm_basemap(leaflet::providers$Stadia.StamenToner) + 
  tm_shape(dub_ind_st |> select(!ValuationReport) |> filter(use_case != "Other")) +
  tm_dots("val_quantile", size = 0.03)
  
tm_basemap(leaflet::providers$Stadia.StamenToner) + 
  tm_shape(dub_ind_st |> select(!ValuationReport) |> filter(use_case != "Other")) +
  tm_dots("use_case", size = 0.05, popup.vars = c("PropertyNumber", "use_case", "Uses", "Address1", "Address2", "Address3", "Eircode"))

dcc_ind <- dcc_DT[Category == "INDUSTRIAL USES" & !is.na(Xitm),]
dcc_ind[, use_case := fcase(
  grepl("data", Uses, perl = TRUE, ignore.case = TRUE), "Data Centre",
  grepl("warehouse", Uses, perl = TRUE, ignore.case = TRUE), "Warehouse",
  default = "Other"
)]

dcc_ind_st <- sf::st_as_sf(dcc_ind, coords = c("Xitm", "Yitm"), crs = 2157)

tm_basemap(leaflet::providers$OpenStreetMap) + 
  tm_shape(dcc_ind_st |> filter(use_case != "Other")) +
  tm_dots("use_case", size = 0.03)

# Step 2: Random spatial stuff -------------------

dubld <- sf::st_read("C:/Users/cool-/Documents/Research Projects/Spatial/DUBLD2022/Dublin Building Database Points 2022.shp")
dubld <- st_transform(dubld, "EPSG:4326")

dubld_comm <- dubld |> filter(Use %in% c("Com", "Mix"))

tmap_mode("view")
dub_comm_bld <- tm_basemap(leaflet::providers$OpenStreetMap) + 
  tm_shape(dubld_comm) + 
  tm_dots("Year")

dub_comm_bld

dubld_res <- dubld |> filter(Use %in% c("Res"))

dub_res_bld <- tm_basemap(leaflet::providers$OpenStreetMap) + 
  tm_shape(dubld_res |> filter(Year != "-1899")) + 
  tm_dots("Year") +
  tm_facets("Year")

dub_res_bld


######### Spatial ----

dc <- dub_ind[use_case == "Data Centre" & PropertyNumber != 5021061] 
dc_sf <- sf::st_as_sf(dc, coords = c("Xitm", "Yitm"), crs = 2157)
ggplot() + geom_sf(data = dc_sf, lims_method = "geometry_bbox") 
