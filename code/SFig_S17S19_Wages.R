#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# EGOLM
# 2021-04-15
# What you do is more important than where you do it
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
library(broom)
library(quantreg)
library(countrycode)  # Package to iso-code country names
'%!in%' <- function(x,y)!('%in%'(x,y)) # opposite of %in% command
options(stringsAsFactors = FALSE)


#%#%#%#%#%#%#%#%#%

olm_country <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/olm_data/OLM_Country_Data.csv")

occupations3 <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/olm_data/OLM_Occupation_Wages_Data.csv", sep = ";")

quantile(olm_country$median_wage, na.rm = T, probs = c(0.99))

olm_country_wages <- olm_country %>% group_by(unit = ISO_Code, year) %>% 
  filter(project_count > 25) %>% 
  dplyr::select(unit, year, mean_wage) %>%
  mutate(key = "Countries", year = as.Date(year, format = "%d/%m/%Y"))

occupations_wages <- occupations3 %>% filter(count > 24) %>% 
  dplyr::select(unit = short_name,year, mean_wage) %>% 
  mutate(key = "Occupations", year = as.Date(year, format = "%d.%m.%y"))

olm_country_wages <- data.frame(olm_country_wages)
occupations_wages <- data.frame(occupations_wages)

wages_data <- rbind(olm_country_wages, occupations_wages)

wages_data %>% group_by(key) %>% summarise(sd(mean_wage))

quantile_data05 <- wages_data %>% group_by(key) %>% 
  summarise(quant05 = quantile(mean_wage, probs = c(0.05)),
 quant05_text = paste("5% quantile = ", round(quant05,0), " USD", sep = "")) %>%
  mutate(x = c(20,10))

quantile_data95 <- wages_data %>% group_by(key) %>% summarise(#sd(mean_wage, na.rm = T),
  quant95 = quantile(mean_wage, probs = c(0.95)),
  quant95_text = paste("95% quantile = ", round(quant95,0), " USD", sep = ""))

wages_data %>% filter(mean_wage < 50) %>%
  ggplot(aes(reorder(unit,mean_wage), y = mean_wage, colour = key)) +
  geom_hline(data = quantile_data05, aes(yintercept = quant05, colour = key), lty = 2) +
  geom_hline(data = quantile_data95, aes(yintercept = quant95, colour = key), lty = 2) +
  geom_text(data = quantile_data05, aes(label = quant05_text, y = quant05 - 3), x = c(110,37), size = 3.5) +
  geom_text(data = quantile_data95, aes(label = quant95_text, y = quant95 + 3), x = c(110,37), size = 3.5) +
  facet_wrap(~key, ncol = 1, scales = "free_x") +
  scale_color_manual(values = c(brewer.pal(11,"RdBu")[10], brewer.pal(11,"RdBu")[2])) +
  geom_boxplot(outlier.alpha = 0, coef = 1) + 
  theme_bw() + labs(y = "Average hourly wage ($)", x = "",col = "")  + 
  theme(legend.position = "none", strip.text = element_text(size = 14),
        panel.grid.minor = element_blank(),panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 5, angle = 90, hjust = 1))

#%#%#%#%#%#%#%#%#%#%#%
# Figure occupations wages over time
#%#%#%#%#%#%#%#%#%#%#%

wages_data %>% filter(key == "Occupations", year == "2013-01-01") %>% summarise(quant33 = quantile(mean_wage, 0.33),
                                                                                quant66 = quantile(mean_wage, 0.66))

wages_data_occ <- wages_data %>% mutate(wage_group2013 = ifelse(key == "Occupations" & year == "2013-01-01" & mean_wage <= 10.547, "bottom",
                                                            ifelse(key == "Occupations" & year == "2013-01-01" & mean_wage > 10.547 & mean_wage <= 14.6, "middle",
                                                                   ifelse(key == "Occupations" & year == "2013-01-01" & mean_wage > 14.6, "top",NA))))


bottom_group <- (wages_data_occ %>% filter(wage_group2013 == "bottom"))$unit 

middle_group <- (wages_data_occ %>% filter(wage_group2013 == "middle"))$unit 

top_group <- (wages_data_occ %>% filter(wage_group2013 == "top"))$unit 

wages_data_occ <- wages_data_occ %>% mutate(wage_group_total = ifelse(unit %in% bottom_group, "bottom",
                                                              ifelse(unit %in% middle_group, "middle",
                                                                     ifelse(unit %in% top_group, "top",
                                                                            ifelse(key == "Occupations" & unit %!in% c("bottom", "middle", "top"), 
                                                                                   "new jobs",NA)))))

stars_occ <- c("**","**","***")

regression_df_occ <- wages_data_occ %>% filter(mean_wage < 50, key == "Occupations", wage_group_total != "new jobs") %>% 
  group_by(wage_group_total, year) %>%
  summarise(mean_wage = mean(mean_wage)) %>%
  do(fitHour = tidy(lm(mean_wage ~ time(mean_wage), data = .))) %>% 
  unnest(fitHour) %>% filter(term == "time(mean_wage)") %>% dplyr::select(wage_group_total, estimate) %>%
  mutate(key = "Occupations", year = as.Date("2019-01-01"), mean_wage = c(18,24,34), label = paste("b = ", round(estimate,2), stars_occ, sep = ""))

#%#%#%#%#%#%#%#%#%#%#%
# Figure country wages over time
#%#%#%#%#%#%#%#%#%#%#%

wages_data %>% filter(key == "Countries", year == "2013-01-01") %>% summarise(quant33 = quantile(mean_wage, 0.33),
                                                                                quant66 = quantile(mean_wage, 0.66))

wages_data_country <- wages_data %>% mutate(wage_group2013 = ifelse(key == "Countries" & year == "2013-01-01" & mean_wage <= 11.83, "bottom",
                                                            ifelse(key == "Countries" & year == "2013-01-01" & mean_wage > 11.83 & mean_wage <= 15.32, "middle",
                                                                   ifelse(key == "Countries" & year == "2013-01-01" & mean_wage > 15.32, "top",NA))))


bottom_group <- (wages_data_country %>% filter(wage_group2013 == "bottom"))$unit 

middle_group <- (wages_data_country %>% filter(wage_group2013 == "middle"))$unit 

top_group <- (wages_data_country %>% filter(wage_group2013 == "top"))$unit 

wages_data_country <- wages_data_country %>% mutate(wage_group_total = ifelse(unit %in% bottom_group, "bottom",
                                                              ifelse(unit %in% middle_group, "middle",
                                                                     ifelse(unit %in% top_group, "top",
                                                                            ifelse(key == "Countries" & unit %!in% c("bottom", "middle", "top"), 
                                                                                   "new country",NA)))))

stars_country <- c(".","***","**")

regression_df_country <- wages_data_country %>% filter(mean_wage < 50, key == "Countries", wage_group_total != "new country") %>% 
  group_by(wage_group_total, year) %>%
  summarise(mean_wage = mean(mean_wage)) %>%
  do(fitHour = tidy(lm(mean_wage ~ time(mean_wage), data = .))) %>% 
  unnest(fitHour) %>% filter(term == "time(mean_wage)") %>% dplyr::select(wage_group_total, estimate) %>%
  mutate(key = "Countries", year = as.Date("2019-01-01"), mean_wage = c(16,25,30), label = paste("b = ", round(estimate,2), stars_country, sep = ""))

wages_data_both <- rbind(wages_data_occ, wages_data_country)

plot1 <- wages_data_both %>% filter(mean_wage < 50, wage_group_total %!in% c("new country", "new jobs"), !is.na(wage_group_total)) %>% 
  group_by(wage_group_total, year, key) %>% summarise(mean_wage = mean(mean_wage)) %>%
  ggplot(aes(x = year, y = mean_wage, colour = wage_group_total)) + 
  #geom_boxplot(outlier.alpha = 0, coef = 0, aes(group = year)) 
  facet_wrap(~key) +
  geom_point(shape = 21) +
  geom_line(lty = 3, key_glyph = "rect") + 
  geom_smooth(method = "lm", se = F, aes(group = wage_group_total)) +
  scale_colour_manual(values = c( brewer.pal(11,"RdBu")[2], "#ac7921",
                                  brewer.pal(11,"RdBu")[10])) +
  geom_text(data = regression_df_country, aes(label =  label)) +
  geom_text(data = regression_df_occ, aes(label =  label)) +
  labs(colour = "Wage group", x = "", y = "Average hourly wage ($)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), plot.caption = element_text(size = 10),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank(), legend.position = "bottom",
        text = element_text(size = 12),
        strip.text = element_text(size = 12)
  )


#%#%#%#%#%#%#%#%#%#%#%
# Figure capital wages over time
#%#%#%#%#%#%#%#%#%#%#%

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

OECD_stats_allYears <- OECD_stats_allYears %>% filter(OLM.PRJ.CNT > 25)

oecd_within_wage <- OECD_stats_allYears %>% group_by(capital, year) %>% summarise(mean_wage = median(OLM.WG.MN, na.rm = T)) %>%
  mutate(geography = "OECD+")


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

GDL_stats <- GDL_stats %>% filter(project_count > 25)

GDL_stats <- GDL_stats %>% filter(iso3 != "MNE")

gdl_within_wage <- GDL_stats %>% group_by(capital, year) %>% summarise(mean_wage = median(mean_wage, na.rm = T)) %>%
  mutate(geography = "Global South")

capital_wage <- rbind(oecd_within_wage, gdl_within_wage)

capital_wage <- capital_wage %>% mutate(capital = ifelse(capital == 1, "Capital regions", "Other regions"))

stars_country <- c("**","***","*","****")

regression_df_capital <- capital_wage %>%
  group_by(geography, capital, year) %>%
  summarise(mean_wage = mean(mean_wage)) %>%
  do(fitHour = tidy(lm(mean_wage ~ time(mean_wage), data = .))) %>% 
  unnest(fitHour) %>% filter(term == "time(mean_wage)") %>% dplyr::select(geography, capital, estimate) %>%
  mutate(year = as.Date("2019-01-01"), mean_wage = c(18,13,28.5,25.5), label = paste("b = ", round(estimate,2), stars_country, sep = ""))


plot2 <- capital_wage %>%
  ggplot(aes(x = year, y = mean_wage, colour = factor(capital))) +
  facet_wrap(~geography) +
  geom_point(shape = 21) +
  geom_line(lty = 3, key_glyph = "rect") + 
  geom_smooth(method = "lm", se = F, aes(group = factor(capital)))+
  scale_colour_manual(values = c( brewer.pal(11,"RdBu")[2],
                                  brewer.pal(11,"RdBu")[10])) +
  geom_text(data = regression_df_capital, aes(label =  label)) +
  labs(colour = "", x = "", y = "Average hourly wage ($)", caption = "(***p < 0.001, **p < 0.01, *p < 0.05, .p < 0.1)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), plot.caption = element_text(size = 10),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank(), legend.position = "bottom",
        text = element_text(size = 12),
        strip.text = element_text(size = 12)
  )

ggarrange(plot1, plot2, labels = c("A", "B"), nrow = 2)

#%%%%%%%%%%%%%%%%%%