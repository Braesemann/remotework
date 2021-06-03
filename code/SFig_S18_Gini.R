#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# EGOLM
# 2021-04-14
# Gini over time
# Fabian Braesemann 
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

#%#%#%#%#%#%#%#%#%#%
# Loading packages
#%#%#%#%#%#%#%#%#%#%
library(tidyverse)    # numerous data wrangling packages
library(data.table)   # quick data loadinglibrary(lubridate)
library(lubridate)    # Working with dates
library(RColorBrewer) # Fancy colours
library(ggrepel)      # Fancy labels in plot
library(scales)       # Log-transformed axis labels
library(ggpubr)       # Arranging several ggplots
library(ggrepel)      # Fancy geom-labels 
library(stargazer)    # LaTeX regression tables
'%!in%' <- function(x,y)!('%in%'(x,y)) # opposite of %in% command
options(stringsAsFactors = FALSE)

library("reldist")    # Package to compute GINI
library(countrycode)  # Package to iso-code country names

#%#%#%#%#%#%#%#%#%
# Prepare WB data
#%#%#%#%#%#%#%#%#%

olm_country <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/olm_data/OLM_Country_Data.csv")

wb_country <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/official_statistics/county_wb_data.csv")
wb_country <- wb_country %>% dplyr::select(-long, -country)
wb_country$year <- as.Date(as.character(wb_country$year), format = "%Y")
wb_country$year <- floor_date(wb_country$year, unit = "year")

wb_country <- wb_country %>% filter(short == "WB.POP.TOTL") %>% rename(WB.POP.TOTL = value) %>% select(-short)

olm_country$year <- as.Date(as.character(olm_country$year), format = "%d/%m/%Y")

olm_country <- olm_country %>% select(-country)

olm_country <- merge(olm_country, wb_country, by.x = c("ISO_Code", "year"), by.y = c("iso3", "year"))

olm_country$country <- countrycode(olm_country$ISO_Code, origin = "iso3c", destination = "country.name")
olm_country$region <- countrycode(olm_country$ISO_Code, origin = "iso3c", destination = "region")

olm_country <- olm_country %>% mutate(OLM.PRJ.CNT.PC = project_count / WB.POP.TOTL * 1000000)

olm_country_allYears <- olm_country

#%#%#%#%#%#%#%#%#%
# Prepare OECD data
#%#%#%#%#%#%#%#%#%

OECD_stats <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/official_statistics/region_oecd_data.csv")

OECD_stats$iso2 <- sapply(OECD_stats$region_id, function(x) substr(x,1,2)[[1]][1])

OECD_stats$iso2 <- ifelse(OECD_stats$iso2 == "EL", "GR", 
                          ifelse(OECD_stats$iso2 == "UK", "GB", OECD_stats$iso2))

OECD_stats$iso3 <- countrycode(OECD_stats$iso2, origin = "iso2c", destination = "iso3c")

OECD_stats <- OECD_stats %>% filter(iso3 %!in% c("TUN", "PER"))

OECD_stats$year <- as.Date(as.character(OECD_stats$year), format = "%Y")
OECD_stats$year <- floor_date(OECD_stats$year, unit = "year")

OECD_stats %>% filter(long == "Region holds country capital") %>% summarise(sum(OLM.PRJ.CNT))

OECD_capital <- OECD_stats %>% filter(long == "Region holds country capital", year == as.Date("2020-01-01")) %>% dplyr::select(region_id, capital = value)

OECD_stats2 <- merge(OECD_stats, OECD_capital, by = "region_id")

OECD_stats_allYears <- OECD_stats2 %>% filter(long == "Population count") %>% dplyr::select(year, iso3, region_id, region, OLM.PRJ.CNT, OLM.WG.MN, OCD.PPL.CNT = value, capital)

OECD_stats_allYears <- OECD_stats_allYears %>% dplyr::select(region_id, year, OLM.PRJ.CNT) #%>% spread(gdlCode, year, project_count, fil = 0)

OECD_stats_allYears <- OECD_stats_allYears %>% group_by(region_id, year) %>% filter(row_number(OLM.PRJ.CNT) == 1)

OECD_stats_allYears2 <- pivot_wider(OECD_stats_allYears, names_from = year, values_from = OLM.PRJ.CNT, values_fill = 0)

OECD_stats_allYears2 <- OECD_stats_allYears2 %>% gather(year, OLM.PRJ.CNT, - region_id)

OECD_stats_allYears2$iso2 <- sapply(OECD_stats_allYears2$region_id, function(x) substr(x,1,2)[[1]][1])

OECD_stats_allYears2$iso2 <- ifelse(OECD_stats_allYears2$iso2 == "EL", "GR", 
                                    ifelse(OECD_stats_allYears2$iso2 == "UK", "GB", OECD_stats_allYears2$iso2))

OECD_stats_allYears2$iso3 <- countrycode(OECD_stats_allYears2$iso2, origin = "iso2c", destination = "iso3c")

OECD_stats_allYears2$iso3 <- ifelse(OECD_stats_allYears2$iso3 == "MNE", "MEX", OECD_stats_allYears2$iso3)

OECD_stats_allYears3 <- merge(OECD_stats_allYears2, OECD_capital, by = "region_id")

OECD_stats_allYears3 <- OECD_stats_allYears3 %>% group_by(region_id, year) %>% filter(row_number(OLM.PRJ.CNT) == 1)

#%#%#%#%#%#%#%#%#%
# Prepare GDL data
#%#%#%#%#%#%#%#%#%

olm_GDL <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/olm_data/OLM_GDL_Data.csv")
olm_GDL$year <- as.Date(olm_GDL$year, format = "%d/%m/%Y")
olm_GDL <- olm_GDL %>% filter(Country %!in% c("Brazil", "Costa Rica", "China", "India", "Indonesia", "Mexico", "South Africa", "Colombia"))

olm_GDL <- olm_GDL %>% mutate(MEX = str_detect(gdlCode, "MEX")) %>% filter(MEX == 0) %>% dplyr::select(-MEX)

olm_GDL <- olm_GDL %>% dplyr::select(year, region_id = gdlCode, mean_wage, region = Region, project_count)

GDL_stats <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/official_statistics/region_gdl_data.csv")

GDL_stats$year <- as.Date(as.character(GDL_stats$year), format = "%Y")
GDL_stats$year <- floor_date(GDL_stats$year, unit = "year")

GDL_stats <- GDL_stats %>% filter(short %in% c("GDL.PPL.CNT", "GDL.CPT.YES")) %>%
  dplyr::select(region_id, iso3, year, short, value)

GDL_stats <- GDL_stats %>% filter(iso3 %!in% c("BRA", "CRI", "CHN", "IND", "IDN", "MEX", "ZAF", "COL"))

GDL_stats <- GDL_stats %>% spread(short, value)

GDL_stats <- merge(olm_GDL, GDL_stats, by = c("region_id", "year"))

GDL_stats$GN <- "Global South"

GDL_stats <- GDL_stats %>% dplyr::select(GN, iso3, year, region_id, region, project_count, mean_wage, population = GDL.PPL.CNT, capital = GDL.CPT.YES)

GDL_capital <- GDL_stats %>% dplyr::select(year, region_id, capital)

olm_GDL_allYears <- GDL_stats %>% dplyr::select(region_id, year, project_count)

olm_GDL_allYears <- olm_GDL_allYears %>% group_by(region_id, year) %>% filter(row_number(project_count) == 1)

olm_GDL_allYears2 <- pivot_wider(olm_GDL_allYears, names_from = year, values_from = project_count, values_fill = 0)

olm_GDL_allYears2 <- olm_GDL_allYears2 %>% gather(year, project_count, - region_id)

olm_GDL_allYears2$iso3 <- sapply(olm_GDL_allYears2$region_id, function(x) substr(x,1,3)[[1]][1])

olm_GDL_allYears2 <- olm_GDL_allYears2 %>% mutate(year = as.Date(year, format = "%Y-%m-%d"))

olm_GDL_allYears3 <- merge(olm_GDL_allYears2, GDL_capital, by = c("region_id", "year"), all.x = T)

olm_GDL_allYears3 <- olm_GDL_allYears3 %>% mutate(capital = ifelse(is.na(capital), 0, capital))

#%#%#%#%#%#%#%#%#%
# Between Gini
#%#%#%#%#%#%#%#%#%

# Country
ginis_wb_between <- olm_country_allYears %>% group_by(year) %>% summarise(value = gini(project_count)) %>% mutate(geography = "Global (countries)", key = "Gini coefficient (between countries)")

# OECD
ginis_oecd_between <- OECD_stats_allYears3 %>% group_by(year) %>% summarise(value = gini(OLM.PRJ.CNT)) %>% mutate(geography = "OECD+ (regions)", key = "Gini coefficient (between countries)")

# Check whether all OECD countries are in the data set
gini_oecd_countries <- (OECD_stats_allYears3 %>% group_by(iso3) %>% summarise(count = sum(OLM.PRJ.CNT)))$iso3

# Remove OECD+ countries from GDL list
olm_GDL_allYears3 <- olm_GDL_allYears3 %>% filter(iso3 %!in% c("BRA", "CHN", "COL", "CRI", "IDN", "IND", "MNE", "ZAF"))

ginis_gdl_between <- olm_GDL_allYears3 %>% group_by(year) %>% summarise(value = gini(project_count)) %>% mutate(geography = "GDL", key = "Gini coefficient (between countries)")

# Check whether all GDL countries are in the data set
gini_gdl_countries <- (olm_GDL_allYears3 %>% group_by(iso3) %>% summarise(count = n()))$iso3

gini_df_between <- rbind(ginis_wb_between, ginis_oecd_between, ginis_gdl_between)

gini_df_between <- gini_df_between %>% mutate(geography = ifelse(geography == "GDL", "Global South (regions)",geography),
                                              geography = factor(geography, levels = c("Global (countries)", "OECD+ (regions)", "Global South (regions)")))

# Plot (A) Regression
regression_df <- data.frame(key = "Gini coefficient (between countries)", value = c(0.92,0.76,0.84), 
                            geography = c("Global South (regions)", "OECD+ (regions)", "Global (countries)"), year = as.Date("2019-01-01"), 
                            coefficient = c(
                              paste("b = ", round(summary(lm(value ~ time(value), data = ginis_gdl_between))$coefficients[,1][2],3), "**", sep = ""),
                              paste("b = ", round(summary(lm(value ~ time(value), data = ginis_oecd_between))$coefficients[,1][2],3), "**", sep = ""),
                              paste("b = ", round(summary(lm(value ~ time(value), data = ginis_wb_between))$coefficients[,1][2],3), "***", sep = "")))

participation_oecd <- OECD_stats_allYears2 %>% group_by(year) %>% filter(OLM.PRJ.CNT > 0) %>% summarise(value = n()) %>% mutate(geography = "OECD+ (regions)", key = "Participating countries / regions")
participation_gdl <- olm_GDL_allYears2 %>% group_by(year) %>% filter(project_count > 0) %>% summarise(value = n()) %>% mutate(geography = "Global South (regions)", key = "Participating countries / regions")
participation_wb <- olm_country_allYears %>% group_by(year) %>% filter(project_count > 0) %>% summarise(value = n()) %>% mutate(geography = "Global (countries)", key = "Participating countries / regions")

gini_df_between <- rbind(gini_df_between, participation_oecd, participation_gdl, participation_wb)

# Plot (B) Regression
regression_df2 <- data.frame(key = "Participating countries / regions", value = c(430,570,220), 
                             geography = c("Global South (regions)", "OECD+ (regions)", "Global (countries)"), year = c(as.Date("2019-01-01"), as.Date("2014-06-01"), as.Date("2019-01-01")), 
                             coefficient = c(
                               paste("b = ", round(summary(lm(value ~ time(value), data = participation_gdl))$coefficients[,1][2],0), ".", sep = ""),
                               paste("b = ", round(summary(lm(value ~ time(value), data = participation_oecd))$coefficients[,1][2],0), "", sep = ""),
                               paste("b = ", round(summary(lm(value ~ time(value), data = participation_wb))$coefficients[,1][2],0), ".", sep = "")))


gini_df_between$key <- factor(gini_df_between$key, levels = c("Participating countries / regions",
                                                              "Gini coefficient (between countries)"))

# Plot A
plot1a <- gini_df_between %>% filter(key == "Participating countries / regions") %>%
  ggplot(aes(x = year, y = value, colour = geography)) + 
  facet_wrap(~key, scales = "free_y") +
  geom_line(lty = 3, key_glyph = "rect") + 
  geom_point(shape = 21) +
  geom_text(data = regression_df2, aes(label = coefficient)) +
  scale_colour_manual(values = c("#ac7921",brewer.pal(11,"RdBu")[10], 
                                 brewer.pal(11,"RdBu")[2])) +
  geom_smooth(method = "lm", se = F, lwd = 0.5) +
  theme_bw() +
  labs(colour = "", x = "", y = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), legend.position = "bottom",
        text = element_text(size = 14))

# Plot B
plot1b <- gini_df_between %>% filter(key == "Gini coefficient (between countries)") %>%
  ggplot(aes(x = year, y = value, colour = geography)) + 
  facet_wrap(~key, scales = "free_y") +
  geom_line(lty = 3, key_glyph = "rect") + 
  geom_point(shape = 21) +
  geom_text(data = regression_df, aes(label = coefficient)) +
  scale_colour_manual(values = c("#ac7921",brewer.pal(11,"RdBu")[10], 
                                 brewer.pal(11,"RdBu")[2])) +
  geom_smooth(method = "lm", se = F, lwd = 0.5) +
  theme_bw() +
  labs(colour = "", x = "", y = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), legend.position = "bottom",
        text = element_text(size = 14))

#%#%#%#%#%#%#%#%#%
# Within Gini
#%#%#%#%#%#%#%#%#%

# OECD
ginis_oecd_within <- OECD_stats_allYears3 %>% filter(capital == 0) %>% group_by(iso3, year) %>% summarise(value = gini(OLM.PRJ.CNT))  %>% mutate(geography = "OECD+ (regions)", key = "within")

ginis_oecd_within$year <- as.Date(ginis_oecd_within$year, format = "%Y-%m-%d")

ginis_oecd_within <- data.frame(ginis_oecd_within)

# GDL

# We have to remove those country-years without projects as they do not allow us to compute the GINI (without variation, no GINI)
deleter <- (olm_GDL_allYears3 %>% group_by(iso3, year) %>% summarise(count = n(), project_count = sum(project_count)) %>% filter(count == 1 | project_count == 0) %>% mutate(country_year = paste(iso3, year, sep ="_")))$country_year

ginis_gdl_within <- olm_GDL_allYears3 %>% mutate(country_year = paste(iso3, year, sep ="_")) %>% 
  filter(country_year %!in% deleter, 
         capital == 0) %>% 
  group_by(iso3, year) %>% summarise(value = gini(project_count)) %>% mutate(geography = "GDL", key = "within")

ginis_gdl_within <- data.frame(ginis_gdl_within)

gini_df_within <- rbind(ginis_oecd_within, ginis_gdl_within)

gini_df_within$year <- as.Date(gini_df_within$year, format = "%Y-%m-%d")

gini_df_within <- gini_df_within %>% mutate(geography = ifelse(geography == "GDL", "Within-country Gini coefficient Global South (non-capital regions)", "Within-country Gini coefficient OECD+ (non-capital regions)"),
                                            geography = factor(geography, levels = c("Within-country Gini coefficient OECD+ (non-capital regions)", "Within-country Gini coefficient Global South (non-capital regions)")))

ginis_oecd_within_means <- ginis_oecd_within %>% group_by(year) %>% summarise(value = mean(value, na.rm = T))
ginis_gdl_within_means <- ginis_gdl_within %>% group_by(year) %>% summarise(value = mean(value, na.rm = T))

# Within regression 
regression_df3 <- data.frame(value = c(0.33,0.93), 
                             geography = c("Within-country Gini coefficient Global South (non-capital regions)", "Within-country Gini coefficient OECD+ (non-capital regions)"), year = as.Date("2019-01-01"), 
                             coefficient = c(
                               paste("b = ", round(summary(lm(value ~ time(value), data = ginis_gdl_within_means))$coefficients[,1][2],3), "", sep = ""),
                               paste("b = ", round(summary(lm(value ~ time(value), data = ginis_oecd_within_means))$coefficients[,1][2],3), "", sep = "")))

# Within Gini Plot
plot2 <- gini_df_within %>%
  ggplot(aes(x = year, y = value, colour = geography, group = year)) + 
  geom_boxplot(outlier.alpha = 0, coef = 1) + 
  facet_wrap(~geography) +
  geom_smooth(aes(group = geography), method = "lm", se = F) +
  geom_text(data = regression_df3, aes(label = coefficient)) +
  theme_bw() +
  scale_colour_manual(values = c(brewer.pal(11,"RdBu")[10], 
                                 brewer.pal(11,"RdBu")[2])) +
  labs(colour = "", x = "", y = "")+#, caption = "(***p < 0.001, **p < 0.01, *p < 0.05, .p < 0.1)") +
  theme(panel.grid.minor = element_blank(), plot.caption = element_text(size = 10),
        panel.grid.major.x = element_blank(), legend.position = "none",
        text = element_text(size = 14))

#%#%#%#%#%#%#%#%#%
# City share plot
#%#%#%#%#%#%#%#%#%

# Calculate share of capital in GDL OECD+
city_share_GDL <- olm_GDL_allYears3 %>% group_by(capital = factor(capital), year) %>% summarise(total_count = sum(project_count)) %>% group_by(year) %>% 
  mutate(OLM.PRJ.CNT = total_count / sum(total_count)) %>% mutate(key = "Share of projects in capital regions (Global South)") %>%  filter(capital == 1)
city_share_OECD <- OECD_stats_allYears3 %>% group_by(capital = factor(capital), year) %>% summarise(total_count = sum(OLM.PRJ.CNT)) %>% group_by(year) %>% 
  mutate(OLM.PRJ.CNT = total_count / sum(total_count))%>% mutate(key = "Share of projects in capital regions (OECD+)") %>%  filter(capital == 1)
city_share_OECD$year <- as.Date(city_share_OECD$year, format = "%Y-%m-%d")

city_share_total <- rbind(city_share_GDL, city_share_OECD)

# Regerssion for plot (D)
regression_df4 <- data.frame(key = c("Share of projects in capital regions (Global South)", "Share of projects in capital regions (OECD+)"), 
                             OLM.PRJ.CNT = c(0.42,0.2), 
                             year = as.Date("2019-01-01"),
                             coefficient = c(
                               paste("b = ", round(summary(lm(OLM.PRJ.CNT ~ time(OLM.PRJ.CNT), data = city_share_GDL))$coefficients[,1][2],3), "", sep = ""),
                               paste("b = ", round(summary(lm(OLM.PRJ.CNT ~ time(OLM.PRJ.CNT), data = city_share_OECD))$coefficients[,1][2],3), "***", sep = "")))


plot3 <- city_share_total %>% 
  ggplot(aes(x = as.Date(year, format = "%Y-%m-%d"), y = OLM.PRJ.CNT, colour = factor(key))) + 
  geom_point(shape = 21) +
  geom_line(lty = 3) + 
  facet_wrap(~factor(key), ncol = 2, scales = "free") + geom_smooth(method = "lm", se = F) +
  geom_text(data = regression_df4, aes(label = coefficient)) +
  theme_bw() +
  scale_colour_manual(values = c(brewer.pal(11,"RdBu")[2], 
                                 brewer.pal(11,"RdBu")[10])) +
  labs(colour = "", x = "", y = "", caption = "(***p < 0.001, **p < 0.01, *p < 0.05, .p < 0.1)") +
  theme(panel.grid.minor = element_blank(), plot.caption = element_text(size = 10),
        panel.grid.major.x = element_blank(), legend.position = "none",
        text = element_text(size = 14),
        strip.text = element_text(size = 12)
  )



ggarrange(
  ggarrange(plot1a, plot1b, ncol = 2, labels = c("A", "B"), common.legend = T, legend = "bottom"),
  plot2,  plot3,              # First row with line plot
  # Second row with box and dot plots
  nrow = 3, 
  labels = c("","C", "D")
)

#%#%#%#%#%#%#%#%#%#%#%