library(tidyverse)
library(plm)
library(manifestoR)
library(ordbetareg)
library(marginaleffects)
library(modelsummary)
library(tidybayes)
# Read in Manifesto Project data and process

#mp <- read_csv("MPDataset_MPDS2023a.csv")
mp <- mp_maindataset(apikey = "APIKEY")
mp$year <- year(as.Date(mp$edate, format = "%d/%m/%Y"))
mp <- mp %>% 
  distinct(partyname, countryname, year, .keep_all = TRUE)

# Read in electoral system data 
es1 <- read_csv("es_data-v4_1.csv")

# Create political spectrum variables
# First: logit rile
mp$logit_rile <- logit_rile(mp)

# Second: Franzmann-Kaiser
mp$franzmann_rile <- franzmann_kaiser(mp)

# Third: Three-indicator measure
mp$tripartite_rw <- mp$per608 + mp$per603 + mp$per601 - mp$per607 - mp$per604 - mp$per602

# Combine manifesto and electoral system data
# Create three-level categorical variable for proportional, mixed, majoritarian systems


mp_matched <- mp %>%
  inner_join(es1, by = c("countryname" = "country", "year" = "year")) %>% 
  mutate(pervote = case_when(is.na(pervote) ~ 0,
                             TRUE ~ pervote)) %>% 
  mutate(absseat = case_when(is.na(absseat) ~ 0,
                             TRUE ~ absseat)) %>% 
  mutate(seat_pct = absseat/totseats)

mp_matched <- mp_matched %>% 
  mutate(es_cat = case_when(elecrule %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Majoritarian",
                            elecrule %in% c(8, 9, 10) ~ "Proportional",
                            TRUE ~ "Mixed"))

# Read in cabinet data
whogov <- read_csv("WhoGov_within_V2.0.csv")

# Create cabinet counts and percentages
whogov %>% 
  #filter(!is.na(prestige_1)) %>% 
  filter(!is.na(partyfacts_id)) %>%
  count(year, partyfacts_id, country_name) %>% 
  mutate(year_lagged = year - 1) -> whogov_counts

whogov_year_counts <- whogov_counts %>% 
  group_by(year, country_name) %>% 
  summarize(sum_year_cab = sum(n))# 

whogov_counts <- whogov_counts %>% 
  left_join(whogov_year_counts, by = c("country_name", "year"))

whogov_counts <- whogov_counts %>% 
  mutate(cab_pct = n/sum_year_cab)

# Download partyfacts data for MP

file_name <- "partyfacts-mapping.csv"
if( ! file_name %in% list.files()) {
  url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
  download.file(url, file_name)
}
partyfacts_raw <- read_csv(file_name)
partyfacts <- partyfacts_raw |> filter(! is.na(partyfacts_id))

dataset_1 <- partyfacts |> filter(dataset_key == "manifesto")

# Join cabinet data to manifesto project data

mp_matched <- mp_matched %>% 
  mutate(id = as.character(party)) %>%
  left_join(dataset_1 %>% select(key = dataset_party_id, partyfacts_id), by = c("id" = "key"))

mp_matched_whogov <- mp_matched %>% 
  left_join(whogov_counts, by = c("year" = "year_lagged", "partyfacts_id" = "partyfacts_id")) %>% 
  mutate(cab_pct = case_when(is.na(cab_pct) ~ 0,
                             TRUE ~ cab_pct)) %>% 
  distinct(partyfacts_id, year, countryname, .keep_all = TRUE) 

mp_matched_whogov %>% 
  count(countryname, year) %>% 
  rename(country_count = n) -> country_party_counts

mp_matched_whogov <- mp_matched_whogov %>%
  left_join(country_party_counts, by = c("countryname" = "countryname", "year" = "year"))

# Adding controls

up <- read_csv("unemployment.csv")

up <- up %>% 
  pivot_longer(cols = `1960`:`2022`, names_to = "year", values_to = "unemployment") %>% 
  select(`Country Name`, `Country Code`, year, unemployment) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(`Country Name` = case_when(`Country Name` == "Czechia" ~ "Czech Repubic",
                                    `Country Name` == "Slovak Republic" ~ "Slovakia",
                                    TRUE ~ `Country Name`))


gdp <- read_csv("gdppercap.csv", skip = 4) %>% 
  pivot_longer(cols = `1960`:`2022`, names_to = "year", values_to = "gdppercap") %>% 
  select(`Country Name`, year, gdppercap) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(`Country Name` = case_when(`Country Name` == "Czechia" ~ "Czech Repubic",
                                    `Country Name` == "Slovak Republic" ~ "Slovakia",
                                    TRUE ~ `Country Name`))

mig <- read_csv("migration_flows.csv") %>% 
  mutate(`Region, subregion, country or area *` = case_when(`Region, subregion, country or area *` == "Czechia" ~ "Czech Republic",
                             TRUE ~ `Region, subregion, country or area *`))

mig <- mig %>% 
  select(country = `Region, subregion, country or area *`, 
         Year, netmig = `Net Migration Rate (per 1,000 population)`)

mp_matched_whogov <- mp_matched_whogov %>% 
  left_join(mig, by = c("countryname" = "country", "year" = "Year"))

mp_matched_whogov <- mp_matched_whogov %>% 
  left_join(up, by = c("countryname" = "Country Name", "year" = "year"))

mp_matched_whogov <- mp_matched_whogov %>% 
  left_join(gdp, by = c("countryname" = "Country Name", "year" = "year"))

mp_matched_whogov$netmig <- as.numeric(mp_matched_whogov$netmig)

polity <- readxl::read_xls("p5v2018.xls") %>% 
  mutate(year_plus = year + 1) %>% 
  select(country, year_plus, polity2)

mp_matched_whogov <- mp_matched_whogov %>% 
  left_join(polity, by = c("countryname" = "country", "year" = "year_plus"))

# Far-right parties in Populist

populist <- read_delim("populist.csv", delim = ";")

populist <- populist %>% 
  filter(farright == TRUE) %>% 
  select(partyfacts_id)

mp_matched_whogov_populist <- mp_matched_whogov %>% 
  filter(partyfacts_id %in% populist$partyfacts_id) %>% 
  mutate(cab_pct = case_when(is.na(cab_pct) ~ 0,
                             TRUE ~ cab_pct))

mp_matched_whogov_populist_country <- mp_matched_whogov_populist %>% 
  group_by(countryname, year) %>% 
  summarize(farright_pct = sum(pervote), unemployment = max(unemployment),
            country_count = max(country_count), netmig = max(netmig), gdppercap = max(gdppercap), 
            es_cat = max(es_cat), cab_pct = sum(cab_pct), seat_pct = sum(seat_pct), polity2 = max(polity2), pervote = sum(pervote)) %>% 
  ungroup()


# Filter by rile quantities
mp_matched_whogov_quantiles_rile <- mp_matched_whogov %>% 
  #filter(western_europe == TRUE) %>%
  group_by(es_cat) %>% 
  arrange(desc(rile)) %>% 
  filter(rile > quantile(rile, .9, na.rm = TRUE)) %>%
  ungroup()

mp_matched_whogov_quantiles_logit <- mp_matched_whogov %>% 
  #filter(western_europe == TRUE) %>%
  group_by(es_cat) %>% 
  arrange(desc(logit_rile)) %>% 
  filter(rile > quantile(logit_rile, .9, na.rm = TRUE)) %>%
  ungroup()

mp_matched_whogov_quantiles_franz <- mp_matched_whogov %>% 
  #filter(western_europe == TRUE) %>%
  group_by(es_cat) %>% 
  arrange(desc(franzmann_rile)) %>% 
  filter(rile > quantile(franzmann_rile, .9, na.rm = TRUE)) %>%
  ungroup()

mp_matched_whogov_quantiles_tri <- mp_matched_whogov %>% 
  #filter(western_europe == TRUE) %>%
  group_by(es_cat) %>% 
  arrange(desc(tripartite_rw)) %>% 
  filter(rile > quantile(tripartite_rw, .9, na.rm = TRUE)) %>%
  ungroup()

mp_matched_whogov_rile_country <- mp_matched_whogov_quantiles_rile %>% 
  group_by(countryname, year) %>% 
  summarize(farright_pct = sum(pervote), unemployment = max(unemployment),
            country_count = max(country_count), netmig = max(netmig), gdppercap = max(gdppercap), 
            es_cat = max(es_cat), cab_pct = sum(cab_pct), seat_pct = sum(seat_pct), polity2 = max(polity2), pervote = sum(pervote)) %>% 
  ungroup()

mp_matched_whogov_logit_country <- mp_matched_whogov_quantiles_logit %>% 
  group_by(countryname, year) %>% 
  summarize(farright_pct = sum(pervote), unemployment = max(unemployment),
            country_count = max(country_count), netmig = max(netmig), gdppercap = max(gdppercap), 
            es_cat = max(es_cat), cab_pct = sum(cab_pct), seat_pct = sum(seat_pct), polity2 = max(polity2), pervote = sum(pervote)) %>% 
  ungroup()


mp_matched_whogov_tri_country <- mp_matched_whogov_quantiles_tri %>% 
  group_by(countryname, year) %>% 
  summarize(farright_pct = sum(pervote), unemployment = max(unemployment),
            country_count = max(country_count), netmig = max(netmig), gdppercap = max(gdppercap), 
            es_cat = max(es_cat), cab_pct = sum(cab_pct), seat_pct = sum(seat_pct), polity2 = max(polity2), pervote = sum(pervote)) %>% 
  ungroup()

# Linear models
mp_matched_whogov_populist_country <- mp_matched_whogov_populist_country %>% 
  mutate(maj_indicator = case_when(es_cat == "Proportional" ~ "Proportional",
                                   TRUE ~ "Majoritarian/Mixed")) 

mp_matched_whogov_populist_country$pervote <- mp_matched_whogov_populist_country$pervote / 100
  
populist_vote <- plm(pervote ~ maj_indicator + country_count + unemployment + netmig + log(gdppercap) + polity2, data = mp_matched_whogov_populist_country, index=c("countryname", "year"), model="within")
populist_seat <- plm(seat_pct ~ maj_indicator + country_count + unemployment + netmig + log(gdppercap) + polity2, data = mp_matched_whogov_populist_country, index=c("countryname", "year"), model="within")
populist_cab <- plm(cab_pct ~ maj_indicator + country_count + unemployment + netmig + log(gdppercap) + polity2, data = mp_matched_whogov_populist_country, index=c("countryname", "year"), model="within")

stargazer::stargazer(populist_vote, populist_seat, populist_cab)

fit_ord_est <- ordbetareg(cab_pct ~ maj_indicator + country_count + unemployment + netmig + log(gdppercap) + polity2,
                          data=mp_matched_whogov_populist_country,
                          chains=24, cores = 24, iter=10000,refresh=0)
 
marginaleffects::avg_slopes(fit_ord_est) %>%
  select(Variable="term",
         Level="contrast",
         `5% Quantile`="conf.low",
         `Posterior Mean`="estimate",
         `95% Quantile`="conf.high") %>% 
  knitr::kable(caption = "Marginal Effect of Electoral System on Far-Right Party Success",
               format = "latex",
               align=c('llccc'))


fit_ord_est_seat <- ordbetareg(seat_pct ~ maj_indicator + country_count + unemployment + netmig + log(gdppercap) + polity2,
                          data=mp_matched_whogov_populist_country,
                          chains=24, cores = 24, iter=10000,refresh=0)



marginaleffects::avg_slopes(fit_ord_est_seat) %>%
  select(Variable="term",
         Level="contrast",
         `5% Quantile`="conf.low",
         `Posterior Mean`="estimate",
         `95% Quantile`="conf.high") %>% 
  knitr::kable(caption = "Marginal Effect of Education on Professor Thermometer",
               format = "latex",
               align=c('llccc'))


marginaleffects::plot_slopes(fit_ord_est, variables = "es_cat", condition = c("netmig"))

stargazer::stargazer(populist_vote, populist_seat, populist_cab)

# Rile models
mp_matched_whogov_rile_country <- mp_matched_whogov_rile_country %>% 
  mutate(maj_indicator = case_when(es_cat == "Proportional" ~ "Proportional",
                                   TRUE ~ "Majoritarian/Mixed")) 

rile_vote <- plm(pervote ~ es_cat + country_count + unemployment + netmig + log(gdppercap) + polity2, data = mp_matched_whogov_rile_country, index=c("countryname", "year"), model="within")
rile_seat <- plm(seat_pct ~ es_cat + country_count + unemployment + netmig + log(gdppercap) + polity2, data = mp_matched_whogov_rile_country, index=c("countryname", "year"), model="within")
rile_cab <- plm(cab_pct ~ es_cat + country_count + unemployment + netmig + log(gdppercap) + polity2, data = mp_matched_whogov_rile_country, index=c("countryname", "year"), model="within")

summary(rile_vote)

stargazer::stargazer(rile_vote, rile_seat, rile_cab)


fit_ord_est_seat_rile <- ordbetareg(seat_pct ~ es_cat + country_count + unemployment + netmig + log(gdppercap) + polity2,
                                   data=mp_matched_whogov_rile_country,
                                   chains=18, cores = 18, iter=10000,refresh=0)

fit_ord_est_cab_rile <- ordbetareg(cab_pct ~ es_cat + country_count + unemployment + netmig + log(gdppercap) + polity2,
                          data=mp_matched_whogov_rile_country,
                          chains=18, cores = 18, iter=10000,refresh=0)

marginaleffects::avg_slopes(fit_ord_est) %>%
  select(Variable="term",
         Level="contrast",
         `5% Quantile`="conf.low",
         `Posterior Mean`="estimate",
         `95% Quantile`="conf.high") %>% 
  knitr::kable(caption = "Marginal Effect of Electoral System on Far-Right Cabinet Share",
               format.args=list(digits=2),
               align=c('llccc'), format = "latex")


# Logit-scale DV models

logit_vote <- plm(pervote ~ es_cat + country_count + unemployment + netmig + log(gdppercap) + polity2, data = mp_matched_whogov_logit_country, index=c("countryname", "year"), model="within")
logit_seat <- plm(seat_pct ~ es_cat + country_count + unemployment + netmig + log(gdppercap) + polity2, data = mp_matched_whogov_logit_country, index=c("countryname", "year"), model="within")
logit_cab <- plm(cab_pct ~ es_cat + country_count + unemployment + netmig + log(gdppercap) + polity2, data = mp_matched_whogov_logit_country, index=c("countryname", "year"), model="within")

summary(logit_cab)

stargazer::stargazer(logit_vote, logit_seat, logit_cab)

fit_ord_est_seat_logit <- ordbetareg(seat_pct ~ es_cat + country_count + unemployment + netmig + log(gdppercap) + polity2,
                                    data=mp_matched_whogov_logit_country,
                                    chains=18, cores = 18, iter=10000,refresh=0)

fit_ord_est_cab_logit <- ordbetareg(cab_pct ~ es_cat + country_count + unemployment + netmig + log(gdppercap) + polity2,
                                   data=mp_matched_whogov_logit_country,
                                   chains=18, cores = 18, iter=10000,refresh=0)

plots <- pp_check_ordbeta(fit_ord_est_cab_logit,
                          ndraws=100,
                          outcome_label="Cabinet Share",
                          new_theme=ggthemes::theme_economist())


marginaleffects::avg_slopes(fit_ord_est_seat_logit) %>%
  select(Variable="term",
         Level="contrast",
         `5% Quantile`="conf.low",
         `Posterior Mean`="estimate",
         `95% Quantile`="conf.high") %>% 
  knitr::kable(caption = "Marginal Effect of Electoral System on Far-Right Seat Share",
               format.args=list(digits=2),
               align=c('llccc'))

# Tri-scale DV models

tri_vote <- plm(pervote ~ es_cat + country_count + unemployment + netmig + log(gdppercap) + polity2, data = mp_matched_whogov_tri_country, index=c("countryname", "year"), model="within")
tri_seat <- plm(seat_pct ~ es_cat + country_count + unemployment + netmig + log(gdppercap) + polity2, data = mp_matched_whogov_tri_country, index=c("countryname", "year"), model="within")
tri_cab <- plm(cab_pct ~ es_cat + country_count + unemployment + netmig + log(gdppercap) + polity2, data = mp_matched_whogov_tri_country, index=c("countryname", "year"), model="within")

summary(logit_cab)

stargazer::stargazer(tri_vote, tri_seat, tri_cab)

fit_ord_est_seat_tri <- ordbetareg(seat_pct ~ es_cat + country_count + unemployment + netmig + log(gdppercap) + polity2,
                                     data=mp_matched_whogov_logit_country,
                                     chains=18, cores = 18, iter=10000,refresh=0)

fit_ord_est_cab_tri <- ordbetareg(cab_pct ~ es_cat + country_count + unemployment + netmig + log(gdppercap) + polity2,
                                    data=mp_matched_whogov_logit_country,
                                    chains=18, cores = 18, iter=10000,refresh=0)


avg_slopes_rile <- marginaleffects::avg_slopes(fit_ord_est_seat_rile)
avg_slopes_logit <- marginaleffects::avg_slopes(fit_ord_est_seat_logit)
avg_slopes_tri <- marginaleffects::avg_slopes(fit_ord_est_seat_tri)

avg_slopes_rile_cab <- marginaleffects::avg_slopes(fit_ord_est_cab_rile)
avg_slopes_logit_cab <- marginaleffects::avg_slopes(fit_ord_est_cab_logit)
avg_slopes_tri_cab <- marginaleffects::avg_slopes(fit_ord_est_cab_tri)



avg_slopes_logit %>%
  mutate(method = "Logit Scale") %>% 
  rbind(avg_slopes_rile %>% mutate(method = "Rile Scale")) %>% 
  rbind(avg_slopes_tri %>% mutate(method = "Tripartite Scale")) %>%
  mutate(variable = case_when(term == "es_cat" ~ contrast,
                              TRUE ~ term)) %>% 
  mutate(variable = case_when(variable == "polity2" ~ "Polity Score",
                              variable == "netmig" ~ "Net Migration",
                              variable == "country_count" ~ "Number of Parties Active",
                              variable == "unemployment" ~ "Unemployment",
                              variable == "gdppercap" ~ "log(GDP Per Capita", 
                              TRUE ~ variable)) %>% 
  ggplot(aes(x=reorder(variable, -estimate), y=estimate, color = method)) + 
  geom_point(position = position_dodge(width = .9)) +
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=0, position = position_dodge(width = .9))+
  coord_flip() + 
  labs(x = "Covariate", 
       y = "Beta Regression Marginal Effect Estimate", 
       title = "Ordered Beta Estimates Across Numeric Methods of Far-Right Classification", 
       color = "Classification Method")


avg_slopes_logit_cab %>%
  mutate(method = "Logit Scale") %>% 
  rbind(avg_slopes_rile_cab %>% mutate(method = "Rile Scale")) %>% 
  rbind(avg_slopes_tri_cab %>% mutate(method = "Tripartite Scale")) %>% 
  mutate(variable = case_when(term == "es_cat" ~ contrast,
                              TRUE ~ term)) %>% 
  mutate(variable = case_when(variable == "polity2" ~ "Polity Score",
                              variable == "netmig" ~ "Net Migration",
                              variable == "country_count" ~ "Number of Parties Active",
                              variable == "unemployment" ~ "Unemployment",
                              variable == "gdppercap" ~ "log(GDP Per Capita", 
                              TRUE ~ variable)) %>% 
  ggplot(aes(x=reorder(variable, -estimate), y=estimate, color = method)) + 
  geom_point(position = position_dodge(width = .9)) +
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=0, position = position_dodge(width = .9))+
  coord_flip() + 
  labs(x = "Covariate", 
       y = "Beta Regression Marginal Effect Estimate", 
       title = "Ordered Beta Estimates Across Numeric Methods of Far-Right Classification", 
       color = "Classification Method")
