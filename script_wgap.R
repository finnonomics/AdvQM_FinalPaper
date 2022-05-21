#### Adv. Quantitative Methods - Final Paper #####

# Creator: Finn Hagemann
# First version: 2022-05-13
# This version: 2022-05-21

#### Packages & setup ####

#Prevent scientific notations
options(scipen=999)

#set working directory
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/02 I Studium/4. Semester/AdvQuant/Adv.Q. Final Paper")

#Load packages
library(WDI)
library(tidyverse)
library(stargazer)
library(knitr)
library(kableExtra)
library(readxl)
library(countrycode)
library(plm)
library(modelsummary)
library(lmtest)
library(correlation)

#Surpress dplyr info on summarize grouping
options(dplyr.summarise.inform = FALSE)

#### Load OECD data ####

#Load given OECD dataset on Gender Wage Gaps 
wgap <- read_csv("data_input/GenderWageGapOECD.csv") %>% 
  rename(country = Entity,
         code = Code, 
         year = Year, 
         wgap_pc = `Gender wage gap (%)`)

#Add variable with continents as defined by the WB
wgap <- wgap %>% 
  mutate(region_wb = countrycode(country, "country.name", "region"),
         subregion_wb = countrycode(country, "country.name", "region23"))

#### Load ILO data ####

#Load ILO dataset on union density
uniondensity <- read_csv("data_input/ILO_uniondensity.csv") %>% 
  dplyr::select(ref_area, time, obs_value) %>% 
  rename(code = ref_area, 
         year = time, 
         uniondensity = obs_value)

#Load ILO dataset on collective bargaining coverage
cbcoverage <- read_csv("data_input/ILO_CBcoverage.csv") %>% 
  dplyr::select(ref_area, time, obs_value) %>% 
  rename(code = ref_area, 
         year = time, 
         cbcoverage = obs_value)

#Load ILO dataset on percentage of labor income
laborincome <- read_csv("data_input/ILO_laborincomepc.csv") %>% 
  dplyr::select(ref_area, time, obs_value) %>% 
  rename(code = ref_area, 
         year = time, 
         laborincome_pc = obs_value)

#### Load WB data ####

#Create vector with all countries in OECD data in iso2c format (readable for API use)
countries <- wgap %>% 
  distinct(country) %>% 
  mutate(country = countrycode(country, "country.name", "iso2c")) %>% 
  pull(country)

#Load economic data via WB API
wb_data <- WDI(country = countries,
           indicator = c("NY.GNP.PCAP.CD", #Gross national product per capita (USD, Atlas meth.)
                         "SP.DYN.CBRT.IN", #Birth rate, crude (per 1,000 people)
                         "SG.GEN.PARL.ZS", #% of parliament seats held by woman
                         "SE.TER.CUAT.ST.FE.ZS", #% of woman with at least short cycle tertiary
                         "SL.TLF.PART.FE.ZS"), #% of woman in part time employment
           start = 1980, 
           end = 2016) %>% 
  rename(gnp_pcap = NY.GNP.PCAP.CD,
         birthrate = SP.DYN.CBRT.IN,
         womenparl = SG.GEN.PARL.ZS,
         womaneduc = SE.TER.CUAT.ST.FE.ZS,
         womanpart = SL.TLF.PART.FE.ZS) %>% 
  dplyr::select(-iso2c)

#### Merge data #### 

#Merge all datasets
wgap <- wgap %>% 
  left_join(uniondensity,
            by = c("code" = "code", "year" = "year")) %>% 
  left_join(cbcoverage,
            by = c("code" = "code", "year" = "year")) %>% 
  left_join(laborincome,
            by = c("code" = "code", "year" = "year")) %>% 
  left_join(wb_data,
            by = c("country" = "country", "year" = "year")) 

#### Analysis OECD data ####

#Number of countries, observations and mean gender pay gap by continent over all years
wgap %>%
  dplyr::select(country, region_wb, wgap_pc) %>% 
  group_by(region_wb) %>% 
  mutate(n_obs_region = n(),
         n_obs = n_distinct(country),
         mean_wgap_region = mean(wgap_pc)) %>%
  arrange(desc(n_obs_region)) %>% 
  ungroup() %>% 
  distinct(region_wb, .keep_all = T) %>% 
  add_row(country = NA, region_wb = "Global", wgap_pc = NA, 
          n_obs_region = sum(.$n_obs_region), n_obs = sum(.$n_obs),
          mean_wgap_region = mean(.$mean_wgap_region)) %>% 
  dplyr::select(-country, -wgap_pc) %>% 
  mutate(across(where(is.numeric), round, 2)) %>%
  kable(col.names = c("Region", "No. of observations", "No. of countries", "Mean pay gap")) %>%
  kable_styling() %>% 
  save_kable(file = "figures_tables/table_overview.html",
             bs_theme = "readable")

#Density plot of observations over years by continent
wgap %>% 
  filter(region_wb == "Latin America & Caribbean" |
         region_wb == "Europe & Central Asia") %>% 
  ggplot(aes(x = year, fill = region_wb)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#ba0020", "#90c9df")) +
  labs(x = "Years",
       y = "Density",
       fill = "") +
  theme_bw()

ggsave("figures_tables/plot_density.png",
       width = 2000,
       height = 1250,
       units = "px")

#Create data for Latin American & Caribbean mean gender pay gap values per year
wgap_latin <- wgap %>% 
  dplyr::select(region_wb, country, year, wgap_pc) %>%
  group_by(year) %>% 
  summarize(region_wb, country, year, wgap_pc = mean(wgap_pc)) %>% 
  ungroup() %>% 
  mutate(country = "Region mean") %>% 
  distinct(year, .keep_all = T)

#Gender pay gap for exemplary Latin American & Caribbean countries
wgap %>% 
  group_by(country) %>% 
  mutate(n_obs_country = n()) %>% 
  filter(n_obs_country >= 22) %>% 
  dplyr::select(year, region_wb, country, wgap_pc) %>%
  rbind(., wgap_latin) %>% 
  group_by(year) %>% 
  ggplot(aes(x = year, y = wgap_pc, color = "region_wb")) +
  geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_color_manual(name="", values=c("#ba0020")) +
  facet_wrap(~country) +
  labs(x = "Years", 
       y = "Gender pay gap (%)") +
  guides(color = "none") +
  theme_bw()

ggsave("figures_tables/plot_ex_wgap.png",
       width = 2000,
       height = 1250,
       units = "px")

#### Regression for regions on gender pay gap ####

#Regression calculation
m_regions <- wgap %>% 
  filter(region_wb == "Latin America & Caribbean" |
           region_wb == "Europe & Central Asia") %>% 
  lm(wgap_pc ~ region_wb, data = .)

#Preparing variable names for modelsummary table
names <- c("(Intercept)" = "Europe & Central Asia",
           "region_wbLatin America & Caribbean" = "Latin America & Caribbean")

#Preparing model names for modelsummary table
mod <- list("Gender pay gap" = m_regions)

#Allowing knitr in modelsummary
options(modelsummary_html = "kableExtra")

#Building OLS table with modelsummary and knitr
modelsummary(mod,
             coef_map = names,
             gof_omit = "R2 Adj.|AIC|BIC|Log.Lik|RMSE|F",
             stars = T) %>% 
  kable_styling() %>% 
  save_kable(file = "figures_tables/table_ols_regions.html",
             bs_theme = "readable")

#### Analysis ILO data ####

#Analyzing observation frequency for union density
wgap %>% 
  drop_na(uniondensity) %>% 
  group_by(year) %>% 
  mutate(n = n()) %>% 
  distinct(year, .keep_all = T) %>% 
  dplyr::select(year, n) %>% 
  arrange(desc(n))

#Analyzing observation frequency for collective bargaining
wgap %>% 
  drop_na(cbcoverage) %>% 
  group_by(year) %>% 
  mutate(n = n()) %>% 
  distinct(year, .keep_all = T) %>% 
  dplyr::select(year, n) %>% 
  arrange(desc(n))

#Create scatterplot for wgap & union density/collective bargaining
wgap %>% 
  drop_na(uniondensity, cbcoverage) %>% 
  filter(region_wb == "Latin America & Caribbean") %>%
  mutate(`Country` = ifelse(country == "Argentina", "Argentina",
                             ifelse(country == "Colombia", "Colombia", 
                                    ifelse(country == "El Salvador", "El Salvador", "Other")))) %>% 
  ggplot(aes(x = uniondensity, y = wgap_pc)) +
  geom_point(aes(x = uniondensity, y = wgap_pc, color = "Union density", shape = `Country`)) +
  geom_point(aes(x = cbcoverage, y = wgap_pc, color = "Collective bargaining", shape = `Country`)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_manual(name = "", values = c("#ba0020", "#90c9df")) +
  labs(x = "Coverage (%)",
       y = "Gender pay gap (%)") +
  guides(fill = "none") + 
  theme_bw()

ggsave("figures_tables/plot_scatter.png",
       width = 2000,
       height = 1250,
       units = "px")

#Calculating pearson's r for both explanatory variables and gender pay gap
wgap %>% 
  filter(region_wb == "Latin America & Caribbean") %>%
  mutate(year = as.numeric(year)) %>% 
  drop_na(wgap_pc, cbcoverage) %>% 
  dplyr::select(wgap_pc, cbcoverage, year) %>% 
  correlation(partial = T)

wgap %>% 
  filter(region_wb == "Latin America & Caribbean") %>%
  mutate(year = as.numeric(year)) %>% 
  drop_na(wgap_pc, uniondensity) %>% 
  dplyr::select(wgap_pc, uniondensity, year) %>% 
  correlation(partial = T)

#Create partial correlations for cbcoverage controlling for year by country
r_cbcov <- wgap %>% 
  filter(region_wb == "Latin America & Caribbean") %>%
  drop_na(cbcoverage, wgap_pc) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(subregion_wb) %>% 
  mutate(n_obs = n()) %>% 
  dplyr::select(subregion_wb, cbcoverage, wgap_pc, year) %>% 
  correlation(partial = T) %>% 
  tibble() %>% 
  filter(Parameter1 == "cbcoverage" &
           Parameter2 == "wgap_pc") %>% 
  dplyr::select(Group, r, n_Obs) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  arrange(r)

#Create partial correlations for uniondensity controlling for year by country
wgap %>% 
  filter(region_wb == "Latin America & Caribbean") %>%
  drop_na(uniondensity, wgap_pc) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(subregion_wb) %>% 
  mutate(n_obs = n()) %>% 
  dplyr::select(subregion_wb, uniondensity, wgap_pc, year) %>% 
  correlation(partial = T) %>% 
  tibble() %>% 
  filter(Parameter1 == "uniondensity" &
         Parameter2 == "wgap_pc") %>% 
  dplyr::select(Group, r, n_Obs) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  arrange(r) %>% 
  full_join(., r_cbcov, by = c(Group = "Group")) %>% 
  add_row(Group = "All", r.x = -0.03, n_Obs.x = 110, r.y = 0.25, n_Obs.y = 64) %>% 
  kable(col.names = c("Country", "r", "obs.", "r", "obs.")) %>%
  kable_styling() %>% 
  add_header_above(c("",
                     "Union density" = 2,
                     "Collective bargaining" = 2)) %>% 
  add_footnote("Partial correlation controlling for years; NAs are produced for sub-regions with less than 3 observations", 
               notation = "symbol") %>% 
  save_kable(file = "figures_tables/table_r.html",
             bs_theme = "readable")
  

#Create data for mean difference in union density and pay gap
diff_full <- wgap %>% 
  drop_na(uniondensity, cbcoverage) %>% 
  filter(region_wb == "Latin America & Caribbean") %>%
  mutate(diff_ud_wgap = (uniondensity - wgap_pc)) %>%
  dplyr::select(year, region_wb, country, diff_ud_wgap) %>% 
  group_by(year) %>% 
  summarize(region_wb, country, year, diff_ud_wgap = mean(diff_ud_wgap)) %>% 
  ungroup() %>% 
  mutate(country = "Region mean") %>% 
  distinct(year, .keep_all = T) %>% 

#Create facet plot for exemplary countries for difference in union density and pay gap
wgap %>% 
  drop_na(uniondensity, cbcoverage) %>% 
  filter(region_wb == "Latin America & Caribbean") %>%
  mutate(diff_ud_wgap = (uniondensity - wgap_pc)) %>% 
  group_by(country) %>% 
  mutate(n_obs_country = n()) %>% 
  filter(n_obs_country >= 6) %>% 
  dplyr::select(year, region_wb, country, diff_ud_wgap, uniondensity, wgap_pc) %>% 
  rbind(., diff_full) %>% 
  group_by(year) %>% 
  ggplot(aes(x = year, y = diff_ud_wgap, color = "region_wb")) +
  geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_color_manual(name="", values=c("#ba0020")) +
  facet_wrap(~country) +
  labs(x = "Years", 
       y = "Difference in union density & gender pay gap") +
  guides(color = "none") +
  theme_bw()

ggsave("figures_tables/plot_ex_diff.png",
       width = 2000,
       height = 1250,
       units = "px")

#### Regression Analysis ####

#Create naive FE-model for cbcoverage
naive_cb <- wgap %>% 
  filter(region_wb == "Latin America & Caribbean") %>%
  plm(wgap_pc ~ cbcoverage,
      data = ., 
      index = c("country", "year"),
      model = "within", 
      effect = "twoways")

#Create full FE-regression model for cbcoverage
model_cb <- wgap %>% 
  filter(region_wb == "Latin America & Caribbean") %>%
  plm(wgap_pc ~ cbcoverage + gnp_pcap + birthrate + womenparl + womaneduc,
      data = ., 
      index = c("country", "year"),
      model = "within", 
      effect = "twoways")

#Test collective bargaining model for serial correlation (found: proceed with robust se)
pbgtest(model_cb, order = 2)

#Create naive FE-model for uniondensity
naive_ud <- wgap %>% 
  filter(region_wb == "Latin America & Caribbean") %>%
  plm(wgap_pc ~ uniondensity,
      data = ., 
      index = c("country", "year"),
      model = "within", 
      effect = "twoways")

#Create full FE-regression model for uniondensity
model_ud <- wgap %>% 
  filter(region_wb == "Latin America & Caribbean") %>%
  plm(wgap_pc ~ uniondensity + gnp_pcap + birthrate + womenparl + womaneduc,
      data = ., 
      index = c("country", "year"),
      model = "within", 
      effect = "twoways")

#Test union density model for serial correlation (found: proceed with robust se)
pbgtest(model_ud, order = 2)

#Preparing variable names for modelsummary table
names_cb <- c("cbcoverage" = "Collective bargaining coverage (%)",
              "uniondensity" = "Union density (%)",
              "gnp_pcap" = "GNP per capita",
              "birthrate" = "Birth rate (per 1000 people)",
              "womenparl" = "Women in Parliament (%)",
              "womaneduc" = "Education: Completed short-cycle tertiary, women (%)")

#Preparing model names for modelsummary table
mod_cb <- list("Naive" = naive_cb,
               "Full" = model_cb,
               "Naive" = naive_ud,
               "Full" = model_ud)

#Allowing knitr in modelsummary
options(modelsummary_html = "kableExtra")

#Building OLS table with modelsummary and knitr
modelsummary(mod_cb,
        vcov = vcovHC(model_cb, type = "sss"),
        coef_map = names_cb,
        gof_omit = "AIC|BIC|Log.Lik|RMSE|F",
        stars = T,
        notes = "Note: All standard errors are robust; Country- and time fixed-effects are used") %>% 
  kable_styling() %>% 
  add_header_above(c("", 
                     "Collective Bargaining" = 2, 
                     "Union Density" = 2)) %>% 
  save_kable(file = "figures_tables/table_model.html",
             bs_theme = "readable")






