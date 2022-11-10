# ----- Intro ------------------------------------------------------------------
# This file contains code that ingestts and munges a number of different 
# data sets. Some of these are used in the final report, while others are not
# Some of these blocks are sequential.
# I have tried to indicate what is and is not used in my comments.

#---- Poverty Threshold----
# These are the federal poverty thresholds in each year for a family of 4
# 2 adults 2 children
# I had to just manually copy these from the individual data files online
# This was not used in the final report since affordability was outside the
# Scope of work
poverty <- data.frame(
  threshold = c(21811, 20244, 20578, 19642, 19740, 19318
                , 19078, 19055, 18751, 18480, 18106, 17552)
  , Year = c(2021, 2020, 2019, 2018, 2017, 2016
             , 2015, 2014, 2013, 2012, 2011, 2010)
)

download_dir = "/Users/rob/Downloads"

# ---- PUMS -----
# iPUMS for Seattle
ddi <- read_ipums_ddi(file.path(download_dir, "usa_00007.xml"))
census <- read_ipums_micro(ddi)
write_parquet(census, file.path(data_folder, "seattle_ipums.parquet"))

# iPUMS for Washington, people who moved from Seattle
ddi <- read_ipums_ddi(file.path(download_dir, "usa_00005.xml"))
census <- read_ipums_micro(ddi) %>%
  dplyr::filter(MIGPUMA1 == 11600)
write_parquet(census, file.path(data_folder, "seattle_outmovers.parquet"))

# Munge out movers 
census_movers <- read_parquet(file.path(data_folder, "seattle_outmovers.parquet")) %>%
  dplyr::filter(
    OWNERSHPD %in% c(11, 13, 20, 22) # Either own or rent
    & PERNUM == 1 #Transform data to household level from person level
    & (between(as.numeric(RENT),115, 8887) | between(as.numeric(MORTAMT1), 115, 10000)) #
    & (
      (as.numeric(RENT) == 0 & as.numeric(MORTAMT1) != 0) | 
        (as.numeric(RENT) != 0 & as.numeric(MORTAMT1) == 0)
    ) #Not both rent and own
  )%>%
  dplyr::mutate(
    OWNCOST = ifelse(OWNCOST == 99999, 0, OWNCOST)
    , `Housing Cost` = (as.numeric(RENT) + OWNCOST)*12
    , Ownership = ifelse(OWNERSHP == 1, 'Own', "Rent")
    # , `Building size` = haven::as_factor(UNITSSTR)
    , Year = as.numeric(YEAR)
  ) %>%
  dplyr::rename (`HH Income` = HHINCOME) 
write_parquet(census_movers, file.path(data_folder, "seattle_outmovers.parquet"))

# Munge Seattle
census_PUMS <- read_parquet(file.path(data_folder, "seattle_ipums.parquet")) %>%
  dplyr::filter(
    OWNERSHPD %in% c(11, 13, 20, 22) # Either own or rent
    & PERNUM == 1 #Transform data to household level from person level
    & (between(as.numeric(RENT),115, 8887) | between(as.numeric(MORTAMT1), 115, 10000)) #
    & (
      (as.numeric(RENT) == 0 & as.numeric(MORTAMT1) != 0) | 
        (as.numeric(RENT) != 0 & as.numeric(MORTAMT1) == 0)
    ) #Not both rent and own
  ) %>%
  dplyr::mutate(
    OWNCOST = ifelse(OWNCOST == 99999, 0, OWNCOST)
    , `Housing Cost` = (as.numeric(RENT) + OWNCOST)*12
    , Ownership = ifelse(OWNERSHP == 1, 'Own', "Rent")
    , UNITSSTR = as.numeric(UNITSSTR)
    , `Building size` = factor(
      case_when(
        UNITSSTR <= 4 ~ 'Single family'
        , between(UNITSSTR,5,6) ~ '2 - 4 units'
        , between(UNITSSTR, 7,8) ~ '5 - 19 units'
        , UNITSSTR == 9 ~ '20 - 49 units'
        , UNITSSTR == 10 ~ '50 + units'
      )
      , levels = c('Single family', '2 - 4 units', '5 - 19 units', '20 - 49 units', '50 + units')
    )
    # , `Building size` = haven::as_factor(UNITSSTR)
    , Year = as.numeric(YEAR)
  ) %>%
  dplyr::rename (`HH Income` = HHINCOME) %>%
  dplyr::filter(
    `Housing Cost` > 0 
    & `HH Income` > 0
  ) %>%
  dplyr::mutate(
    `Housing share of HH income` = `Housing Cost`/`HH Income`
    , `Burden` = ifelse(`Housing share of HH income` <= .3 , 'Not burdened', "Burdened")) 
write_parquet(census_PUMS, file.path(data_folder, "PUMS.parquet"))

#---- King building permits ----
permits <- read.csv(here::here(
  "tr_rental_monitoring_seattle", "data", "Residential_Building_Permits_Issued_and_Final.csv"))

recode <- read.csv(here::here(
  "tr_rental_monitoring_seattle", "data", "permit_type_recode.csv"))

permits <- permits %>%
  inner_join(recode) %>%
  dplyr::filter(APTYPE %in% c(
    "Construction Permit", "Demolition Permit", "Phased Project Permit")
    & tr_type != '' ) %>%
  dplyr::rename(
    `Building type`= tr_type
    , `Net dwelling units` = NET_UNITS
    , `Year` = YEAR_FINAL
  ) %>%
  dplyr::mutate(
    `Building size` = 
      factor(
        case_when(
          NEW <= 1 ~ 'Single family'
          , between(NEW, 2,4) ~ '2 - 4 units'
          , between(NEW, 5, 19) ~ '5 - 19 units'
          , between(NEW, 20, 49) ~ '20 - 49 units'
          , NEW >= 50 ~ '50 + units'
        )
        , levels = c('Single family', '2 - 4 units'
                     , '5 - 19 units', '20 - 49 units'
                     , '50 + units')
      )
  ) %>%
  dplyr::select(RECORDNUMBER, DEVSITE_ID, X, Y,
                NEIGHBORHOOD, APTYPE, `Building type`, 
                DEMO, NEW,
                `Net dwelling units`, `Building size`,
                `Year`)


write_parquet(permits, here::here("tr_rental_monitoring_seattle", "data", "permits.parquet")) 
rm(permits, recode)

#---- King County Taxroll----
# Download files from https://info.kingcounty.gov/assessor/datadownload/default.aspx
# The section that follows this one depends on outputs from this section

apartments <- read.csv(file.path(download_dir, "EXTR_AptComplex.CSV")
                       , colClasses=c(Major="character", Minor='character')) %>%
  dplyr::mutate(parcel_num = paste0(Major, "-", Minor)) %>%
  dplyr::rename(units_count = NbrUnits) %>%
  dplyr::select(parcel_num, NbrBldgs, NbrStories
                ,units_count, AvgUnitSize, YrBuilt, BldgQuality)

residential <- read.csv(file.path(download_dir, "EXTR_ResBldg.csv")
                        , colClasses=c(Major="character", Minor='character')) %>%
  dplyr::mutate(parcel_num = paste0(Major, "-", Minor)) %>%
  dplyr::rename(
    units_count = NbrLivingUnits, BldgQuality = Condition
    ) %>%
  dplyr::select(parcel_num, units_count, YrBuilt, BldgQuality) 

condos <- read.csv(file.path(download_dir, "EXTR_CondoUnit2.csv")
                   , colClasses=c(Major="character", Minor='character')) %>%
  dplyr::mutate(parcel_num = paste0(Major, "-", Minor)) %>%
  dplyr::mutate(condo = T) %>%
  dplyr::select(parcel_num, condo, NbrBedrooms, Grade)

parcels <- read.csv(file.path(download_dir, "EXTR_Parcel.csv")
                    , colClasses=c(Major="character", Minor='character')) %>%
  dplyr::mutate(
    parcel_num = paste0(Major, "-", Minor)
    , units_count = 1) %>%
  dplyr::select(parcel_num, PropType, Major) 

king_taxroll <- plyr::rbind.fill(apartments, residential) %>%
  inner_join(parcels) %>%
  plyr::rbind.fill(condos) %>%
    dplyr::mutate(
      PropType = case_when(
        PropType == "C" ~ "Commercial multi-unit"
        , PropType == "R" & units_count == 1 ~ "Single-family"
        , PropType == "R" & units_count > 1 ~ "Small multi-unit"
        , condo ~ "Residential condominium"
        , TRUE ~ NA_character_
      )
      , `Building size` = 
        factor(
          case_when(
            units_count <= 1 ~ 'Single family'
            , between(units_count, 2,4) ~ '2 - 4 units'
            , between(units_count, 5, 19) ~ '5 - 19 units'
            , between(units_count, 20, 49) ~ '20 - 49 units'
            , units_count >= 50 ~ '50 + units'
          )
          , levels = c('Single family', '2 - 4 units'
                       , '5 - 19 units', '20 - 49 units'
                       , '50 + units')
        )
  ) %>%
  dplyr::filter(!is.na(PropType)) %>%
  dplyr::select(parcel_num, `Building size`, PropType, condo)

rm(residential,parcels, condos)

# >>>>>>------- 

#----TR Properties----
# The section that follows this one depends on outputs from this section
properties_qry = "SELECT * FROM wa_tr_rental_monitoring_seattle.mv_parcel_sizes_and_qualities"
properties <- dbGetQuery(con, properties_qry)

# >>>>>>------- 
#----TR Owners----
# Owner 
owners <- dbGetQuery(con, owners_query) %>%
  dplyr::mutate(
    `Owner residence` = case_when(
        (grepl("SEATTLE", mail_last_line)) ~ "Seattle"
      , (grepl("KING, WA", mail_county) & !grepl("SEATTLE", mail_last_line)) ~ "Seattle suburbs"
      , (grepl(", WA", mail_last_line) & !grepl("KING, WA", mail_county) & !grepl("SEATTLE", mail_last_line)) ~ "Washington state"
      , TRUE ~ "Not in WA"
    )
  )

# Also identify owner-occupied properties
residency_qry = "SELECT * FROM wa_tr_rental_monitoring_seattle.mv_owner_occupied_parcels"
residency <- dbGetQuery(con, residency_qry) %>%
  dplyr::select(parcel_num_standardized) %>%
  dplyr::rename(parcel_num = parcel_num_standardized) %>%
  dplyr::mutate(`Owner occupied` = TRUE) %>%
  dplyr::mutate(parcel_num = paste0(substr(parcel_num,0,6), "-", substr(parcel_num,7,10))) 

# Combine the three parts: properties, owners, and residency
owners <- inner_join(owners, properties) %>%
  left_join(residency) %>%
  dplyr::mutate(
    `Owner occupied` = ifelse(is.na(`Owner occupied`), FALSE, TRUE)
    , `Owner is company` = (corp_flag)
                )

# Count the number of properties each owner owns
owners <- owners %>%
  dplyr::group_by(owner_names) %>%
  dplyr::summarise(
    `Owner portfolio: parcels` = n()
    , `Owner portfolio: buildings` = sum(nbr_bldgs)
    , `Owner portfolio: units` = sum(units_count)
  ) %>%
  inner_join(owners) %>%
  dplyr::select(parcel_num, `Owner residence`, `Owner occupied`
                , prop_type, owner_names
                , geo_id, year_built, `Owner is company`
                , `Owner portfolio: parcels`:`Owner portfolio: units`
  )  %>%
  # Convert classes to eliminate annoying dplyr warning
  dplyr::mutate(
    `Owner portfolio: parcels` = as.numeric(`Owner portfolio: parcels`)
    , `Owner portfolio: buildings` = as.numeric(`Owner portfolio: buildings`)
    , `Owner portfolio: units` = as.numeric(`Owner portfolio: units`)
  ) %>%
  # Create a mom-and-pop indicator
  dplyr::mutate(
    `Owner type` = case_when(
      `Owner portfolio: parcels` <= 3 
      & (`Owner portfolio: buildings` <= 6 | is.na(`Owner portfolio: buildings` ))
      & `Owner portfolio: units` <= 10
      & !(`Owner is company`)
      ~ 'Mom-and-pop'
      , `Owner portfolio: parcels` <= 3 
      & (`Owner portfolio: buildings` <= 6 | is.na(`Owner portfolio: buildings` ))
      & `Owner portfolio: units` <= 10
      & `Owner is company` 
      ~ 'Small property manager'
      , between(`Owner portfolio: parcels`, 3, 20)
      | between(`Owner portfolio: units`, 11, 50)
      ~ 'Midsized property manager'
      , `Owner portfolio: parcels` > 20 
      | `Owner portfolio: units` > 50
      ~ 'Large property manager'
      , TRUE ~ 'Other'
    )
    ,  `Owner type` = factor( `Owner type`
                              , levels = c('Large property manager', 'Midsized property manager'
                                           , 'Small property manager', 'Mom-and-pop'))
  ) 

rm(residency, properties)

# >>>>>>------- 
#---- TR Rentals----
rentals <- dbGetQuery(con, query3) %>%
  dplyr::select(
    parcel_num, 
    seen_on, listed_on, removed_on, price_numeric,
    beds, baths, sqft, listing_unit_level_id
  ) %>%
  # TRs weird raw data model has three potential dates
  dplyr::mutate(
    date = case_when(
      !is.na(listed_on) ~ listed_on
      ,  !is.na(removed_on) ~ removed_on
      ,  !is.na(seen_on) ~ seen_on
    )
    , Year = lubridate::year(date)) %>%
  rename(price = price_numeric) %>%
# We want to remove 'repeat' listings of the same unit
  dplyr::group_by(parcel_num, Year, listing_unit_level_id) %>%
  dplyr::summarise(
    n_listings = n()
    , seen_on = last(listed_on)
    , last_price = last(price)
    , first_price = first(price)
    , mean_price = mean(price, na.rm = T)
    , beds = max(beds)
    , baths = max(baths)
    , sqft = max(sqft)
  ) %>%
  dplyr::filter(!is.na(Year)) %>%
  dplyr::rename(list_rent = last_price) %>%
  dplyr::mutate(list_rent = list_rent * 12) %>%
  as.data.frame()
# >>>>>>------- 
#---- Rental data ----
# First, we combine all the different data sets into one single frame
# with each row representing a unit by year
rental_data <- rentals %>%
  left_join(king_taxroll) %>% # Join parcels to King County Assessor records.
  # I need this to get the PropType variable to distinguish between condos and 
  # apartments.
  # left_join(buildings) %>% # Join units to buildings by parcel number
  left_join(owners) %>% # Join owners by parcel_num
  # left_join(residency) %>% # Join residency by parcel_num
  dplyr::ungroup() %>%
  # Filters 
  dplyr::filter(list_rent > 0 #Remove listings with silly rental values
                & !is.na(list_rent) 
                & list_rent/12 <= 10000
                & !is.na(beds) # We need bedroom counts
                & !is.na(PropType) # These are records in vendor that aren't on the King County files
                # & !is.na(baths) # Need bathroom counts
                & !is.na(geo_id) # Need to know where the property is
                & Year >= 2019 # Use data after the start of active monitoring
  ) %>%
  # Feature engineering
  dplyr::mutate(
    Bedrooms = case_when(
      beds == 0 ~ "Studio"
      , beds == 1 ~ "1 BR"
      , beds == 2 ~ "2 BR"
      , beds == 3 ~ "3 BR"
      , beds == 4 ~ "4 BR"
      , beds >= 5 ~ "5+ BR"
    )
    , Bedrooms = factor(Bedrooms, levels = c("Studio", "1 BR", "2 BR", "3 BR", "4 BR", "5+ BR"))
    , `Year structure built` = as.numeric(year_built)
  ) %>%
# We want to account for the fact that
# property owners may post one listing for many units, or many listings for one
# unit. To account for different listing practices, I 
# aggregate listings to each parcel number, in each year,
# by apartment type (beds and baths and owner)
  group_by(parcel_num, Year, Bedrooms, baths, `Building size`
           , PropType, owner_names, `Owner residence`
           , `Owner occupied`, geo_id, year_built, `Owner is company`
           , `Owner portfolio: parcels`, `Owner portfolio: buildings`
           , `Owner portfolio: units`, `Owner type`) %>%
  dplyr::summarise(
      `Listing count` = sum(n_listings)
    , `Unit count` = n_distinct(listing_unit_level_id)
    , `Annual rental costs` = mean(list_rent)
    , `Square feet` = mean(sqft, na.rm = T)
  ) %>%
  dplyr::rename(
      `Parcel number` = parcel_num
    , `Property type` = PropType
    , `Owner names` = owner_names
    , Baths = baths
    , `Year structure built` = year_built
    , `Census tract` = geo_id
  ) 

write_parquet(rental_data, here::here("tr_rental_monitoring_seattle", "data", "rental_data.parquet"))

#-----DD Coverage of rental data ----
dd_coverage <- rentals %>%
  full_join(king_taxroll) %>%
  dplyr::mutate(
    coverage = (!is.na(n_listings))
  ) %>%
  group_by(PropType) %>%
  dplyr::summarise(
    Coverage = mean(coverage)
    , Listings = sum(n_listings, na.rm=T)
  ) %>%
  dplyr::filter(!is.na(PropType))

write_csv(dd_coverage, here::here("tr_rental_monitoring_seattle", "outputs", 'T15.csv'))

# ----- DD Owner type counts ----
rental_data %>%
  dplyr::group_by(`Owner type`) %>%
  dplyr::summarise(
    owners = n_distinct(`Owner names`)
  ) 

test <- subset(rental_data
               , is.na(`Owner type`)
               , select = c(`Owner type`, `Owner is company`
                            , `Owner portfolio: parcels`, `Owner portfolio: buildings`
                            , `Owner portfolio: units`)
)

T16 <- rental_data %>%
  dplyr::group_by(`Owner type`, `Property type`) %>%
  dplyr::summarise(
    owners = n_distinct(`Owner names`)
  )
others <- rental_data %>% dplyr::filter(`Owner type` == 'Other')


#---- Rental registry----
a <- read_parquet(here::here("tr_rental_monitoring_seattle", "data", "rental_data.parquet")) %>%
  dplyr::filter(Year > 2020) %>%
  group_by(`Parcel number`) %>%
  summarise(
    `TrueRoll unit count` = sum(`Unit count`, na.rm = T)
  ) 

# Want to attempt to estimate compliance by joining rental listings into
# Seattle's rental registry
registry <- readxl::read_xlsx(here::here("tr_rental_monitoring_seattle", "data"
                                         , "seattle_rental_registry.xlsx")) %>%
  dplyr::filter(RECORD_STATUS == "Active Registration") %>%
  dplyr::mutate(
    `Parcel number` = paste0(substr(PARCEL_NBR, 0, 6), "-", substr(PARCEL_NBR, 7, 14))
    , `Registered units` = as.numeric(Registed_Units)
  ) %>%
  dplyr::select(
    RECORD_ID, `Parcel number`, `Registered units`
  ) %>%
  full_join(a)

write_parquet(registry, here::here("tr_rental_monitoring_seattle", "data", "rental_registry.parquet"))
rm(registry, a)


#---- TR Ownership records time series-----
dbGetQuery(con, query7) %>%
  dplyr::rename(
    `Owner type` = owner_type
    , `Units` = n_units
    , Year = roll_yr
  ) %>%
  dplyr::mutate(
    `Owner type` = case_when(
      `Owner type` == "mom and pop" ~ 'Mom-and-pop'
      , `Owner type` == "small property manager" ~ 'Small property manager'
      , `Owner type` == "midsized property manager" ~ 'Midsized property manager'
      , `Owner type` == "large property manager" ~ 'Large property manager'
      , TRUE ~ 'Other'
      )
    )%>%
write_parquet(here::here("tr_rental_monitoring_seattle", "data", "ownership_records.parquet"))
