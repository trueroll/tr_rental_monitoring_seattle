# ------Intro-----------------------------------------------------------------------
# This file contains code that loads the data prepared by 0_ingest and creates
# various outputs, graphs, models, etc.
# DD ==> Due Diligence 
# G --> Graph

#----DD Compare Household count in IPUMS to census 344,629 ----
read_parquet(here::here("tr_rental_monitoring_seattle", "data", "PUMS.parquet"))  %>%
  dplyr::filter(YEAR == 2020) %>%
  dplyr::summarize(n = sum(HHWT))

#---- G1 Seattle population ----
T1 <- read_parquet(here::here("tr_rental_monitoring_seattle", "data", "PUMS.parquet"))  %>% 
  dplyr::filter(Year >= 2010) %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    `Households` = sum(HHWT)
    , `HH Income` = median(`HH Income`)
  ) %>%
  pivot_longer(cols = `Households`:`HH Income`
               , names_to = 'Estimate') %>%
  dplyr::group_by(Estimate) %>%
  dplyr::arrange(Year) %>%
  dplyr::mutate(
    value2 = value / value[1] *100
  )

T1 <- read_parquet(here::here("tr_rental_monitoring_seattle", "data", "PUMS.parquet")) %>%
  dplyr::filter(Ownership == 'Rent' & Year >= 2010) %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    value = median(`Housing Cost`)
  ) %>%
  dplyr::mutate(Estimate = "Median annual rental costs") %>%
  dplyr::arrange(Year) %>%
  dplyr::mutate(
    value2 = value / value[1] *100
  ) %>%
  rbind(T1)

G1 <- T1 %>%
  ggplot(aes(x = Year, y = value2, color = Estimate)) +
  geom_line() + geom_point() +
  theme_minimal() +
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  scale_color_manual(values= c(tr_colors)) +
  labs(
    y = "Indexed value"
    , x = "Year"
    , title = "Houshold Counts & Rents Over Time"
    , subtitle = "Seattle, WA"
    , caption = "G1. Data from Census IPUMS ACS 5-year estimates.") +
    scale_x_continuous(breaks = 2010:2020)

write_csv(T1, here::here("tr_rental_monitoring_seattle", "outputs",  "T1.csv") )
save(G1, file = here::here("tr_rental_monitoring_seattle", "outputs", 'G1.gph'))

rm(T1, G1)

#---- G2 HH income vs. housing cost ----
T2 <-  read_parquet(here::here("tr_rental_monitoring_seattle", "data", "PUMS.parquet")) %>%
  dplyr::filter(`Housing share of HH income` < .8) %>%
  dplyr::group_by(Year, Ownership) %>%
  dplyr::summarise(
    `Median HH Income` = Hmisc::wtd.quantile(`HH Income`, probs = .5, weights = HHWT)
    , `Median Housing Cost` = Hmisc::wtd.quantile(`Housing Cost`, probs = .5, weights = HHWT)
  ) %>%
  dplyr::group_by(Ownership) %>%
  dplyr::arrange(Ownership, Year) %>%
  dplyr::mutate(
    `Median HH Income Index` = `Median HH Income`/`Median HH Income`[1] *100
    , `Median Housing Cost Index` = `Median Housing Cost`/`Median Housing Cost`[1] * 100
  ) %>%
  pivot_longer(
    cols = c(`Median HH Income Index`,`Median Housing Cost Index`)
    , names_to = "Statistic"
  ) 

G2 <- T2 %>%
  ggplot(aes(x = Year, y = `value`, color = Statistic)) +
  geom_line() + geom_point() +
  facet_grid(cols = vars(Ownership)) +
  theme_minimal() +
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  scale_color_manual(values = tr_colors) +
  labs(title = "Household Income and Housing Costs in Seattle, WA"
       , caption = "G2. Data from US Census IPUMS."
       , subtitle = "By ownership status over time") +
  ylab("Index")

save(G2, file = here::here("tr_rental_monitoring_seattle", "outputs", 'G2.gph'))
write_csv(T2, here::here("tr_rental_monitoring_seattle", "outputs", "T2.csv") )

rm(G2, T2)

# Table of sample sizes
read_parquet(here::here("tr_rental_monitoring_seattle", "data", "PUMS.parquet")) %>%
  dplyr::filter(`Housing Cost` <50000 & YEAR == 2020) %>%
  dplyr::group_by(`Building size`, OWNERSHP) %>%
  dplyr::summarise(n = n()) %>%
  pivot_wider(id_cols = `Building size`, names_from = OWNERSHP, values_from = n)

#---- G3 HH by Building Size ----
T3 <-  read_parquet(here::here("tr_rental_monitoring_seattle", "data", "PUMS.parquet")) %>%
  dplyr::filter(Ownership == "Rent") %>%
  dplyr::group_by(`Building size`, Year) %>%
  dplyr::summarise(
    `Households` = sum(HHWT)
  ) %>%
  dplyr::arrange(Year) %>%
  dplyr::mutate(`Households (100 = 2005)` = Households / Households [1] *100 )

G3 <- T3 %>%
  ggplot(aes(x= Year, y = `Households`, color = `Building size`)) +
  geom_line() + geom_point() +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_color_manual(values= tr_colors) +
  labs(title = "Household Count by Building Size in Seattle, WA"
       , subtitle = "Renters"
       , caption = "G3. Data from US Census IPUMS.")

save(G3, file = here::here("tr_rental_monitoring_seattle", "outputs", 'G3.gph'))
write.csv(T3, here::here("tr_rental_monitoring_seattle", "outputs", "T3.csv"))

# test 
T3 %>% group_by(Year) %>% summarise(Renters = sum(Households))

rm(G3, T3)
# ---- G4 Building Permits ----
T4 <- read_parquet(here::here("tr_rental_monitoring_seattle", "data", "permits.parquet"))  %>%
  dplyr::filter(APTYPE %in% c("Construction Permit", "Demolition Permit",
                              "Phased Project Permit")
  ) %>%
  group_by(`Building size`, Year) %>%
  summarise(
    `Gross dwelling units removed` = sum(DEMO, na.rm= T)
    , `Gross dwelling units added` = sum(NEW, na.rm= T)
    , `Net dwelling units` = sum(`Net dwelling units`, na.rm = T)
    , `Total permits` = n_distinct(RECORDNUMBER)
    , `Total development sites` = n_distinct(DEVSITE_ID)) 

G4.1 <- T4 %>%
  dplyr::filter(`Net dwelling units` < 10000) %>%
  ggplot(aes(x = Year, y = `Net dwelling units` , color = `Building size`)) +
  geom_line() + geom_point() +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_color_manual(values= tr_colors) +
  labs(title = "Net change in dwelling units in Seattle, WA"
       , caption = "Seattle building permit data")

G4.2 <- T4 %>%
  dplyr::select(`Building size`, contains("Gross")) %>%
  dplyr::rename(
    Additions = `Gross dwelling units added`
    , Demolitions = `Gross dwelling units removed`
  ) %>%
  pivot_longer(cols = c(Additions, Demolitions)
               , names_to = "Change") %>%
  dplyr::group_by(`Building size`, Change) %>%
  summarise(
    `Gross change in units` = sum(value, na.rm = T)
  ) %>%
  ggplot(aes(x = `Building size`, y = `Gross change in units`
             , fill = Change)) +
  geom_bar(stat = "identity", position = 'dodge') +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values= tr_colors) +
  scale_color_manual(values= tr_colors) +
  labs(
    title = "Gross changes in dwelling units in Seattle, WA"
    , subtitle = "2005 to 2021"
       , caption = "G4. Seattle building permit data.")

save(G4.1, file = here::here("tr_rental_monitoring_seattle", "outputs", 'G4.1.gph'))
save(G4.2, file = here::here("tr_rental_monitoring_seattle", "outputs", 'G4.2.gph'))
write_csv(T4, here::here("tr_rental_monitoring_seattle", "outputs", "T4.csv") )
rm(G4.1,G4.2, T4)

#---- G5 Ownership over time ----
# This section looks at the ownership of parcels over time
T5 <- read_parquet(here::here("tr_rental_monitoring_seattle", "data", "ownership_records.parquet")) %>%
  dplyr::filter(!is.na(`Owner type`) & `Owner type` != "Other" & !is.na(Year)) %>%
  dplyr::group_by(Year, `Owner type`) %>%
  dplyr::summarise(
    Owners = n()
    , Units = sum(`Units`, na.rm = T)
  ) %>%
  dplyr::group_by(`Owner type`) %>%
  arrange(Year) %>%
  dplyr::mutate(
    `Indexed unit count` = Units / Units[1] * 100
    , `Indexed owner count` = Owners / Owners[1] * 100
  )
# Compare unit counts ownership data and IPUMS
T5 %>% ungroup() %>% dplyr::filter(Year == 2020) %>% summarise(Units = sum(Units))
read.csv(here::here("tr_rental_monitoring_seattle", "outputs", "T3.csv")) %>%
  dplyr::filter(Year == 2020) %>%
  ungroup() %>%
  dplyr::summarise(
    Units = sum(Households)
  )

G5.1 <- T5 %>%
  ggplot(aes(x = Year, y = `Indexed unit count`, color = `Owner type`)) +
  geom_line() + geom_point() +
  theme_minimal() +
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  scale_color_manual(values= c(tr_colors)) +
  labs(
    y = "Indexed unit count"
    , x = "Year"
    , title = "Unit counts by owner type"
    , caption = "G5.1. Data from TrueRoll.") +
  scale_x_continuous(breaks = 2010:2020)

G5.2 <- T5 %>%
  ggplot(aes(x = Year, y = `Indexed owner count`, color = `Owner type`)) +
  geom_line() + geom_point() +
  theme_minimal() +
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  scale_color_manual(values= c(tr_colors)) +
  labs(
    y = "Indexed owner count"
    , x = "Year"
    , title = "Owner counts by owner type"
    , caption = "G5.2. Data from TrueRoll.") +
  scale_x_continuous(breaks = 2010:2020)

save(G5.1, file = here::here("tr_rental_monitoring_seattle", "outputs", "G5.1.gph"))
save(G5.2, file = here::here("tr_rental_monitoring_seattle", "outputs", "G5.2.gph"))
write.csv(T5, file = here::here("tr_rental_monitoring_seattle", "outputs", "T5.csv"))
rm(T5, G5.1, G5.2)


#---- G7 Cost by bldg size ----
T7 <- read_parquet(here::here("data", "PUMS.parquet")) %>%
  dplyr::filter(
    Ownership == "Rent" 
    & `Housing share of HH income` < .8) %>%
  dplyr::group_by(`Building size`, Year) %>%
  dplyr::summarise(
    `Housing cost` = weighted.mean(`Housing Cost`, HHWT)) %>%
  dplyr::mutate(`Data source` = 'US Census IPUMS') %>%
  dplyr::ungroup()

T7 <- read_parquet(here::here( "data", "rental_data.parquet"))  %>%
  dplyr::filter(Year >= 2021 & !(is.na(`Building size`))) %>%
  dplyr::group_by(`Building size`, Year) %>%
  dplyr::summarise(
    `Housing cost` = mean(`Annual rental costs`, na.rm = T)
  )  %>%
  dplyr::mutate(`Data source` = 'TrueRoll rental data') %>%
  dplyr::ungroup() %>%
  plyr::rbind.fill(T7)

G7 <- T7 %>%
  ggplot(aes( x = Year, y = `Housing cost`, color = `Building size`)) +
  geom_line() + geom_point() + 
  geom_vline(xintercept = 2020.5, size = 1, linetype = "dotdash") +
  annotate(geom="text", x=2017, y=40000, label="Different data source after 2020") +
  theme_minimal() +
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_color_manual(values= c(tr_colors)) +
  labs(title = "Annual Rental Costs in Seattle, WA"
       , subtitle = "Renters"
       , caption = "G6. '05 to '20 data from US Census IPUMS. '21 to '22 data from TrueRoll.")


save(G7, file = here::here("outputs", 'G7.gph'))
write_csv(T7, here::here( "outputs", "T7.csv") )
rm(T7, G7)


#---- Rental registry ----
T12_dict <- data.frame(
  column = c("Parcels", "Registered parcels")
  , definition = c(
    "Total parcels captured in either the rental registry or TR rental data"
    , "Parcels on Seattle's rental registry")
)

T12 <- read_parquet(here::here("tr_rental_monitoring_seattle", "data", "rental_registry.parquet")) %>%
  dplyr::summarise(
      Parcels = n_distinct(`Parcel number`)
      , `Registered parcels` = n_distinct(RECORD_ID)
      , `TrueRoll unit count` = sum(`TrueRoll unit count`, na.rm = T)
      , `Registered units` = sum( `Registered units`, na.rm = T)
  )
 
T12 <- read_parquet(here::here("tr_rental_monitoring_seattle", "data", "rental_registry.parquet")) %>%
  dplyr::filter(is.na(`Registered units`)) %>%
  dplyr::summarise(
    `Unregistered units` = sum(`TrueRoll unit count`)
  ) %>%
  cbind(T12) %>%
  dplyr::mutate(
    `Unregistered parcels`  = Parcels - `Registered parcels`
  )

write.csv(T12, file = here::here("tr_rental_monitoring_seattle", "outputs", 'T12.csv'))
write.csv(read_parquet(here::here("tr_rental_monitoring_seattle", "data", "rental_registry.parquet")) 
          , file = here::here("tr_rental_monitoring_seattle", "outputs", 'rental_registry.csv'))

rm(T12, T12_dict)

#---- Regression 1: owner type----
rental_data <-  read_parquet(here::here("tr_rental_monitoring_seattle", "data", "rental_data.parquet")) %>%
  dplyr::filter(
    !is.na(Bedrooms) & !is.na(`Building size`) & !is.na(Baths)
    & !is.na(`Year structure built`)
  )
model1 <- lm(`Annual rental costs` ~ -1 + `Owner type`
             , data = rental_data)
model1.1 <- lm(`Annual rental costs` ~ -1 + `Owner type` 
               + Year +Year^2 + Year^3 +
               + Bedrooms + Baths + `Census tract` + `Building size`
               + `Year structure built`  + `Year structure built`^2
               + `Year structure built`^3
               , data = rental_data)

save(model1, file = here::here("tr_rental_monitoring_seattle", "outputs", 'model1.RData'))
save(model1.1, file = here::here("tr_rental_monitoring_seattle", "outputs", 'model1.1.RData'))

rm(model1,model1.1, rental_data)

#---- G9 Cost by owner and bedrooms ----
G9 <- read_parquet(here::here("data", "rental_data.parquet")) %>%
  group_by(`Owner type`, Bedrooms) %>%
  dplyr::summarise(
    `Median annual rental costs` = median(`Annual rental costs`)
  ) %>%
  dplyr::filter(Bedrooms %in% c('1 BR', '2 BR', '3 BR')
                & !is.na(`Owner type`)) %>%
  ggplot(aes(x = Bedrooms, y = `Median annual rental costs`, fill = `Owner type`)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal() +
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_fill_manual(values= c(tr_colors)) +
  labs(title = "Annual rental costs by bedroom and owner type"
       , subtitle = "2019 - Q12022 TrueRoll Data"
       , caption = "G9. Data from TrueRoll.")

save(G9, file = here::here( "outputs", 'G9.gph'))
rm(G9)

#---- Regression 2: Owner home location----
rental_data <-  read_parquet(here::here("tr_rental_monitoring_seattle", "data", "rental_data.parquet")) %>%
  dplyr::filter(
    !is.na(Bedrooms) & !is.na(`Building size`) & !is.na(Baths)
    & !is.na(`Year structure built`)
  )

model2 <- lm(`Annual rental costs` ~ -1 + `Owner residence`
             , data = rental_data)
model2.1 <- lm(`Annual rental costs` ~  -1 + `Owner residence` +
                 + Bedrooms + Baths + `Census tract` + `Building size`
               + `Year structure built`  + `Year structure built`^2
               + `Year structure built`^3
               , data = rental_data)

save(model2, file = here::here("tr_rental_monitoring_seattle", "outputs", 'model2.RData'))
save(model2.1, file = here::here("tr_rental_monitoring_seattle", "outputs", 'model2.1.RData'))

rm(model2, model2.1)

#----- Regression 3: Owner occupancy ----
rental_data <-  read_parquet(here::here("tr_rental_monitoring_seattle", "data", "rental_data.parquet")) %>%
  dplyr::filter(
    !is.na(Bedrooms) & !is.na(`Building size`) & !is.na(Baths)
    & !is.na(`Year structure built`)
  )

model3 <- lm(`Annual rental costs` ~ -1 + `Owner occupied`
             , data = rental_data)

model3.1 <- lm(`Annual rental costs` ~ -1 + `Owner occupied`
               + Bedrooms + Baths + `Census tract` + `Building size`
               + `Year structure built`  + `Year structure built`^2
               + `Year structure built`^3
               , data = rental_data)

save(model3, file = here::here("tr_rental_monitoring_seattle", "outputs", 'model3.RData'))
save(model3.1, file = here::here("tr_rental_monitoring_seattle", "outputs", 'model3.1.RData'))

rm(model3, model3.1, rental_data)


#----- Regression 5: Building size ----
rental_data <-  read_parquet(here::here("tr_rental_monitoring_seattle", "data", "rental_data.parquet")) %>%
  dplyr::filter(
    !is.na(Bedrooms) & !is.na(`Building size`) & !is.na(Baths)
    & !is.na(`Year structure built`)
  )

ipums <- read_parquet(here::here("tr_rental_monitoring_seattle", "data", "PUMS.parquet"))  %>%
  dplyr::filter(
    Ownership == "Rent" 
    & `Housing share of HH income` < .8) %>%
  dplyr::rename(`Annual rental costs` = `Housing Cost`)

model5 <- lm(`Annual rental costs` ~ -1 + `Building size`
               , data = ipums)
model5.1<- lm(`Annual rental costs` ~ -1 + `Building size`
             , data = rental_data)
model5.2 <- lm(`Annual rental costs` ~ -1 + `Building size`
               + Bedrooms + Baths + `Census tract` 
               + `Year structure built`  + `Year structure built`^2
               + `Year structure built`^3
               , data = rental_data)

save(model5, file = here::here("tr_rental_monitoring_seattle", "outputs", 'model5.RData'))
save(model5.1, file = here::here("tr_rental_monitoring_seattle", "outputs", 'model5.1.RData'))
save(model5.2, file = here::here("tr_rental_monitoring_seattle", "outputs", 'model5.2.RData'))

rm(model5, model5.1,model5.2, rental_data, ipums)

#---- G8 Cost by size and bedrooms ----
G8 <- read_parquet(here::here("data", "rental_data.parquet")) %>%
  group_by(`Building size`, Bedrooms) %>%
  dplyr::summarise(
    `Median annual rental costs` = median(`Annual rental costs`)
  ) %>%
  dplyr::filter(Bedrooms %in% c('1 BR', '2 BR', '3 BR')
                & !is.na(`Building size`)) %>%
  ggplot(aes(x = Bedrooms, y = `Median annual rental costs`, fill = `Building size`)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal() +
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_fill_manual(values= c(tr_colors)) +
  labs(title = "Annual rental costs by bedroom and building size"
       , subtitle = "2019 - Q12022 TrueRoll Data"
       , caption = "G8. Data from TruRoll.")

save(G8, file = here::here("outputs", 'G8.gph'))
rm(G8)

#----Frequency table----
T13 <- read_parquet(here::here("tr_rental_monitoring_seattle", "data", "rental_data.parquet")) %>%
  dplyr::group_by(`Property type`, Bedrooms) %>%
  dplyr::summarise(
    `Mean rent` = mean(`Annual rental costs`)
    , `Mean sqrft` = mean(`Square feet`, na.rm = T)
    , `Listings` = sum(`Listing count`)
    , `Units` = sum(`Unit count`)
    , `Parcels` = n_distinct(`Parcel number`)
  ) 
write_csv(T13, here::here("tr_rental_monitoring_seattle", "outputs", 'T13.csv'))
rm(T13)

