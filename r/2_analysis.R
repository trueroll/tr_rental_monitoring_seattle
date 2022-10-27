# ------Intro-----------------------------------------------------------------------
# This file contains code that loads the data prepared by 0_ingest and creates
# various outputs, graphs, models, etc.
# DD ==> Due Diligence 
# G --> Graph

#----DD Compare Household count in IPUMS to census 344,629 ----
read_parquet(here::here("seattle_rental_study", "data", "PUMS.parquet"))  %>%
  dplyr::filter(YEAR == 2020) %>%
  dplyr::summarize(n = sum(HHWT))

#---- G1 Seattle population ----
T1 <- read_parquet(here::here("seattle_rental_study", "data", "PUMS.parquet"))  %>% 
  inner_join(poverty) %>%
  dplyr::mutate(
    poverty = (`HH Income` <= 1.5 * threshold)*HHWT
  ) %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    `Households` = sum(HHWT)
    , `Households with income < 1.5 times poverty threshold` = sum(poverty)
  ) %>%
  pivot_longer(cols = `Households`:`Households with income < 1.5 times poverty threshold`
               , names_to = 'Estimate') %>%
  dplyr::group_by(Estimate) %>%
  dplyr::arrange(Year) %>%
  dplyr::mutate(
    value2 = value / value[1] *100
  )

T1 <- read_parquet(here::here("seattle_rental_study", "data", "PUMS.parquet")) %>%
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
    y = "Indexed count"
    , x = "Year"
    , title = "Houshold Counts & Rents Over Time"
    , subtitle = "Seattle, WA"
    , caption = "G1. Data from Census IPUMS.") +
    scale_x_continuous(breaks = 2010:2020)

write_csv(T1, here::here("seattle_rental_study", "outputs",  "T1.csv") )
save(G1, file = here::here("seattle_rental_study", "outputs", 'G1.gph'))

rm(T1, G1)

#---- G2 HH income vs. housing cost ----
T2 <-  read_parquet(here::here("seattle_rental_study", "data", "PUMS.parquet")) %>%
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

save(G2, file = here::here("seattle_rental_study", "outputs", 'G2.gph'))
write_csv(T2, here::here("seattle_rental_study", "outputs", "T2.csv") )

rm(G2, T2)

# Table of sample sizes
read_parquet(here::here("seattle_rental_study", "data", "PUMS.parquet")) %>%
  dplyr::filter(`Housing Cost` <50000 & YEAR == 2020) %>%
  dplyr::group_by(`Building size`, OWNERSHP) %>%
  dplyr::summarise(n = n()) %>%
  pivot_wider(id_cols = `Building size`, names_from = OWNERSHP, values_from = n)

#---- G3 HH by Building Size ----
T3 <-  read_parquet(here::here("seattle_rental_study", "data", "PUMS.parquet")) %>%
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

save(G3, file = here::here("seattle_rental_study", "outputs", 'G3.gph'))
write.csv(T3, here::here("seattle_rental_study", "outputs", "T3.csv"))

# test 
T3 %>% group_by(Year) %>% summarise(Renters = sum(Households))

rm(G3, T3)
# ---- G4 Building Permits ----
T4 <- read_parquet(here::here("seattle_rental_study", "data", "permits.parquet"))  %>%
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

save(G4.1, file = here::here("seattle_rental_study", "outputs", 'G4.1.gph'))
save(G4.2, file = here::here("seattle_rental_study", "outputs", 'G4.2.gph'))
write_csv(T4, here::here("seattle_rental_study", "outputs", "T4.csv") )
rm(G4.1,G4.2, T4)

#---- G5 Ownership over time ----
# This section looks at the ownership of parcels over time
T5 <- read_parquet(here::here("seattle_rental_study", "data", "ownership_records.parquet")) %>%
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
read.csv(here::here("seattle_rental_study", "outputs", "T3.csv")) %>%
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

save(G5.1, file = here::here("seattle_rental_study", "outputs", "G5.1.gph"))
save(G5.2, file = here::here("seattle_rental_study", "outputs", "G5.2.gph"))
write.csv(T5, file = here::here("seattle_rental_study", "outputs", "T5.csv"))
rm(T5, G5.1, G5.2)


#---- G7 Cost by bldg size ----
T6 <- read_parquet(here::here("seattle_rental_study", "data", "PUMS.parquet")) %>%
  dplyr::filter(
    Ownership == "Rent" 
    & `Housing share of HH income` < .8) %>%
  dplyr::group_by(`Building size`, Year) %>%
  dplyr::summarise(
    `Housing cost` = weighted.mean(`Housing Cost`, HHWT)) %>%
  dplyr::mutate(`Data source` = 'US Census IPUMS') %>%
  dplyr::ungroup()

T6 <- read_parquet(here::here("seattle_rental_study", "data", "rental_data.parquet"))  %>%
  dplyr::filter(Year >= 2021 & !(is.na(`Building size`))) %>%
  dplyr::group_by(`Building size`, Year) %>%
  dplyr::summarise(
    `Housing cost` = mean(`Annnual rental costs`, na.rm = T)
  )  %>%
  dplyr::mutate(`Data source` = 'TrueRoll rental data') %>%
  dplyr::ungroup() %>%
  plyr::rbind.fill(T6)

G6 <- T6 %>%
  ggplot(aes( x = Year, y = `Housing cost`, color = `Building size`)) +
  geom_line() + geom_point() + 
  geom_vline(xintercept = 2020.5) +
  theme_minimal() +
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_color_manual(values= c(tr_colors)) +
  labs(title = "Annual Rental Costs in Seattle, WA"
       , subtitle = "Renters"
       , caption = "G6. '05 to '20 data from US Census IPUMS. '21 to '22 data from TrueRoll.")


save(G6, file = here::here("seattle_rental_study", "outputs", 'G6.gph'))
write_csv(T6, here::here("seattle_rental_study", "outputs", "T6.csv") )
rm(T6, G6)

#---- Rental registry ----
T12_dict <- data.frame(
  column = c("Parcels", "Registered parcels")
  , definition = c(
    "Total parcels captured in either the rental registry or TR rental data"
    , "Parcels on Seattle's rental registry")
)

T12 <- read_parquet(here::here("seattle_rental_study", "data", "rental_registry.parquet")) %>%
  dplyr::summarise(
      Parcels = n_distinct(`Parcel number`)
      , `Registered parcels` = n_distinct(RECORD_ID)
      , `TrueRoll unit count` = sum(`TrueRoll unit count`, na.rm = T)
      , `Registered units` = sum( `Registered units`, na.rm = T)
  )
 
T12 <- read_parquet(here::here("seattle_rental_study", "data", "rental_registry.parquet")) %>%
  dplyr::filter(is.na(`Registered units`)) %>%
  dplyr::summarise(
    `Unregistered units` = sum(`TrueRoll unit count`)
  ) %>%
  cbind(T12) %>%
  dplyr::mutate(
    `Unregistered parcels`  = Parcels - `Registered parcels`
  )

write.csv(T12, file = here::here("seattle_rental_study", "outputs", 'T12.csv'))
write.csv(read_parquet(here::here("seattle_rental_study", "data", "rental_registry.parquet")) 
          , file = here::here("seattle_rental_study", "outputs", 'rental_registry.csv'))

rm(T12, T12_dict)

#---- Regression 1: owner type----
rental_data <-  read_parquet(here::here("seattle_rental_study", "data", "rental_data.parquet")) %>%
  dplyr::filter(
    !is.na(Bedrooms) & !is.na(`Building size`) & !is.na(Baths)
    & !is.na(`Year structure built`)
  )
model1 <- lm(`Annnual rental costs` ~ -1 + `Owner type`
             , data = rental_data)
model1.1 <- lm(`Annnual rental costs` ~ -1 + `Owner type` 
               + Year +Year^2 + Year^3 +
               + Bedrooms + Baths + `Census tract` + `Building size`
               + `Year structure built`  + `Year structure built`^2
               + `Year structure built`^3
               , data = rental_data)

save(model1, file = here::here("seattle_rental_study", "outputs", 'model1.RData'))
save(model1.1, file = here::here("seattle_rental_study", "outputs", 'model1.1.RData'))

rm(model1,model1.1, rental_data)

#---- Regression 2: Owner home location----
rental_data <-  read_parquet(here::here("seattle_rental_study", "data", "rental_data.parquet")) %>%
  dplyr::filter(
    !is.na(Bedrooms) & !is.na(`Building size`) & !is.na(Baths)
    & !is.na(`Year structure built`)
  )

model2 <- lm(`Annnual rental costs` ~ -1 + `Owner residence`
             , data = rental_data)
model2.1 <- lm(`Annnual rental costs` ~  -1 + `Owner residence` +
                 + Bedrooms + Baths + `Census tract` + `Building size`
               + `Year structure built`  + `Year structure built`^2
               + `Year structure built`^3
               , data = rental_data)

save(model2, file = here::here("seattle_rental_study", "outputs", 'model2.RData'))
save(model2.1, file = here::here("seattle_rental_study", "outputs", 'model2.1.RData'))

rm(model2, model2.1)

#----- Regression 3: Owner occupancy ----
rental_data <-  read_parquet(here::here("seattle_rental_study", "data", "rental_data.parquet")) %>%
  dplyr::filter(
    !is.na(Bedrooms) & !is.na(`Building size`) & !is.na(Baths)
    & !is.na(`Year structure built`)
  )

model3 <- lm(`Annnual rental costs` ~ -1 + `Owner occupied`
             , data = rental_data)

model3.1 <- lm(`Annnual rental costs` ~ -1 + `Owner occupied`
               + Bedrooms + Baths + `Census tract` + `Building size`
               + `Year structure built`  + `Year structure built`^2
               + `Year structure built`^3
               , data = rental_data)

save(model3, file = here::here("seattle_rental_study", "outputs", 'model3.RData'))
save(model3.1, file = here::here("seattle_rental_study", "outputs", 'model3.1.RData'))

rm(model3, model3.1, rental_data)


#----- Regression 5: Building size ----
rental_data <-  read_parquet(here::here("seattle_rental_study", "data", "rental_data.parquet")) %>%
  dplyr::filter(
    !is.na(Bedrooms) & !is.na(`Building size`) & !is.na(Baths)
    & !is.na(`Year structure built`)
  )

ipums <- read_parquet(here::here("seattle_rental_study", "data", "PUMS.parquet"))  %>%
  dplyr::filter(
    Ownership == "Rent" 
    & `Housing share of HH income` < .8) %>%
  dplyr::rename(`Annnual rental costs` = `Housing Cost`)

model5 <- lm(`Annnual rental costs` ~ -1 + `Building size`
               , data = ipums)
model5.1<- lm(`Annnual rental costs` ~ -1 + `Building size`
             , data = rental_data)
model5.2 <- lm(`Annnual rental costs` ~ -1 + `Building size`
               + Bedrooms + Baths + `Census tract` 
               + `Year structure built`  + `Year structure built`^2
               + `Year structure built`^3
               , data = rental_data)

save(model5, file = here::here("seattle_rental_study", "outputs", 'model5.RData'))
save(model5.1, file = here::here("seattle_rental_study", "outputs", 'model5.1.RData'))
save(model5.2, file = here::here("seattle_rental_study", "outputs", 'model5.2.RData'))

rm(model5, model5.1,model5.2, rental_data, ipums)

#----Frequency table----
T13 <- read_parquet(here::here("seattle_rental_study", "data", "rental_data.parquet")) %>%
  dplyr::group_by(`Property type`, Bedrooms) %>%
  dplyr::summarise(
    `Mean rent` = mean(`Annnual rental costs`)
    , `Mean sqrft` = mean(`Square feet`, na.rm = T)
    , `Listings` = sum(`Listing count`)
    , `Units` = sum(`Unit count`)
    , `Parcels` = n_distinct(`Parcel number`)
  ) 
write_csv(T13, here::here("seattle_rental_study", "outputs", 'T13.csv'))
rm(T13)

