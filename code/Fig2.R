#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Economic Geography of OLM | Analysing DFs
# 25-11-2020 | Fabian Stephany | fabian.stephany@oii.ox.ac.uk #
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Material
# 
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Read packages
library(dplyr) # Modify DFs
library(tidyr) # Modify DFs
library(zoo)   # For RowMean replace
library(ggplot2) # Plotting
require(tidyverse) #fill is part of tidyr
library(MASS) # GLM Negative Binomial
library(mgcv) # Calculate slope of looes fit
library(stargazer) # Plot tables in tex
library(plm) # Allow panel data regression
library(ggpubr) # Allow multi-panel figures
library(RColorBrewer)
library(scales)
library(ggrepel)
# Define mean impute
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
# Inverse Hyperbolic Sine Transformation
ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}
# Function for calculation of group statistics
cf_intervals <- function(x) { 
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Read GitHub data
## Country
df_country_wb <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/official_statistics/egolm_country_data.csv")
df_country_olm <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/olm_data/OLM_Country_Data.csv")
names(df_country_olm) <-c("iso3", "country", "year", "OLM.PRJ.CNT", "OLM.WG.MD", "OLM.WG.MN")
## OECD Regions
df_oecd_reg <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/official_statistics/egolm_oecd_data.csv")
df_oecd_olm <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/olm_data/OLM_OECD_Data.csv")
names(df_oecd_olm) <-c("region_id", "region", "year", "OLM.PRJ.CNT", "OLM.WG.MD", "OLM.WG.MN")
## GDL Regions
df_gdl_reg <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/official_statistics/egolm_gdl_data.csv")
df_gdl_olm <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/olm_data/OLM_GDL_Data.csv")
names(df_gdl_olm) <-c("region_id", "region", "year", "iso3", "country", "OLM.PRJ.CNT", "OLM.WG.MD", "OLM.WG.MN")
##################################################################################
##################################################################################
# Merge Dataframes
## Country Level
## Reshape and merge
df_country_olm <- df_country_olm %>% mutate(year = str_sub(year,-4,-1)) %>% dplyr:: select(iso3, year, OLM.PRJ.CNT, OLM.WG.MD, OLM.WG.MN)
df_country_wb <- df_country_wb %>% dplyr::select(-long) %>% spread(short, value) %>% mutate(year = as.factor(year))
df_country <- left_join(df_country_olm,df_country_wb)

## OECD Region Level
df_oecd_olm <- df_oecd_olm %>% mutate(year = as.integer(str_sub(year,-4,-1))) %>% mutate(year = as.factor(year))
df_oecd_reg <- df_oecd_reg %>% dplyr::select(-long) %>% spread(short, value) %>% mutate(year = as.factor(year))
df_oecd <- left_join(df_oecd_olm,df_oecd_reg)

## GDL Region Level
df_gdl_olm <- df_gdl_olm %>% mutate(year = str_sub(year,-4,-1)) %>% dplyr::select(region_id, year, OLM.PRJ.CNT, OLM.WG.MD, OLM.WG.MN) %>% 
  mutate(year = as.factor(year))
df_gdl_reg <- df_gdl_reg %>% dplyr::select(-long) %>% spread(short, value) %>% mutate(year = as.factor(year))
df_gdl <- left_join(df_gdl_reg, df_gdl_olm)

#########################################################################################
### COUNTRY PROJECTS ### COUNTRY PROJECTS ### COUNTRY PROJECTS ### COUNTRY PROJECTS ###
#########################################################################################

df_country <- df_country %>% mutate(OLM.PRJ.CNT.NA = ifelse(OLM.PRJ.CNT == 0, NA, OLM.PRJ.CNT),
                                    OLM.PRJ.CNT.IHS = ihs(OLM.PRJ.CNT))


df_country <- df_country %>% mutate(all_vars = ifelse(!is.na(OLM.PRJ.CNT.IHS) & !is.na(WB.POP.TOTL) & !is.na(WB.SEC.NENR) & !is.na(WB.NET.BBND.P2) &
                                                         !is.na(WK.ENG.LNG) & !is.na(WB.GSR.CCIS.ZS) & !is.na(WB.NUS.PPP) & !is.na(WB.GDP.PCAP.CD), 1, 0))


country_projects_final <- plm(OLM.PRJ.CNT.IHS ~ log(WB.POP.TOTL) + WB.SEC.NENR + WB.NET.BBND.P2 + WK.ENG.LNG + log(WB.GSR.CCIS.ZS) + I(WB.NUS.PPP/1000) + I(WB.GDP.PCAP.CD / 1000),
                    data=df_country %>% filter(all_vars==1), index = c("year"), model = "within")


#########################
# Residual plot regressions

res_optimal <-  plm(OLM.PRJ.CNT.IHS ~ log(WB.POP.TOTL) + WB.SEC.NENR + WB.NET.BBND.P2 + WK.ENG.LNG + log(WB.GSR.CCIS.ZS) + I(WB.NUS.PPP/1000) + I(WB.GDP.PCAP.CD / 1000),
                    data=df_country %>% filter(all_vars==1), index = c("year"), model = "within")

res_df <- data.frame(iso3 = (df_country %>% filter(all_vars==1))$iso3,
                     year = (df_country %>% filter(all_vars==1))$year,
                     OLM.PRJ.CNT.IHS = (df_country %>% filter(all_vars==1))$OLM.PRJ.CNT.IHS,
                     res_optimal = res_optimal$residuals)

res_long <- res_df %>% gather(key, value, - iso3, - year)

res_long$key <- ifelse(res_long$key == "OLM.PRJ.CNT.IHS", "OLM project count (ihs-transf.)","Residuals of the optimised model (1)")

res_long$key <- factor(res_long$key, levels = c("OLM project count (ihs-transf.)", "Residuals of the optimised model (1)"))


res_long$labeller <- ifelse(res_long$iso3 %in% c("USA"), "United States",
                            ifelse(res_long$iso3 %in% c("UKR"), "Ukraine",
                                   ifelse(res_long$iso3 %in% c("MOZ"), "Mozambique",
                                          ifelse(res_long$iso3 %in% c("YEM"), "Yemen",
                                                 ifelse(res_long$iso3 %in% c("IND"), "India",
                                                        ifelse(res_long$iso3 %in% c("SYC"), "Seychelles",NA))))))

res_long$label_col <- ifelse(!is.na(res_long$labeller), "a", "b")

res_long1 <- res_long

res_long1$facet <- "Online labour project count per country in 2020"

res_long1 <- res_long1 %>% filter(year == "2020") %>%
  group_by(key) %>% mutate(mean_value = mean(value), value2 = value - mean_value)

res_long1 %>% group_by(key) %>% summarise(
                                          var2 = sum(value2^2), count = n()) %>% 
  mutate(var2_avg = var2 / (count), sd_avg = sqrt(var2_avg))

label_data <- data.frame(
  key = c("OLM project count (ihs-transf.)", "Residuals of the optimised model (1)"),
  value2 = c(8),
  iso3 = "DEU",
  label_col = "b",
  labels = c("Variance:\n7.39 (100%)", "Variance:\n2.13 (28%)")
)


res_boxplot1 <- res_long1 %>% filter(key %in% c("OLM project count (ihs-transf.)","Residuals of the optimised model (1)")) %>%
  ggplot(aes(key, y = value2, fill = label_col, col = label_col)) + 
  geom_boxplot(fill = NA, aes(group = key), outlier.alpha = 0, coef = 1, width = 0.6) +
  geom_point(position = position_jitter(0.25, 0.2, seed = 1), shape = 21, size = 2.25, alpha = 0.8) + 
  facet_wrap(~facet) +
  geom_text_repel(aes(label = labeller), position = position_jitter(0.25, 0.2,seed = 1), colour = "black", fill = NA,
                  size = 4.5) +
  scale_fill_manual(values = c(brewer.pal(11,"RdBu")[2],brewer.pal(11,"RdBu")[10]),guide=FALSE) +
  scale_colour_manual(values = c(brewer.pal(11,"RdBu")[2],brewer.pal(11,"RdBu")[10]),guide=FALSE) +
  geom_label(data = label_data, aes(label = labels), fill = "white", colour = "black", size = 4.5) +
  labs(x = "", y = "Spread of OLM project count (ihs-transf.) around mean / residual") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), text = element_text(size = 12), strip.text = element_text(size = 12))

#########################################################################################
### COUNTRY WAGE ### COUNTRY WAGE ### COUNTRY WAGE ### COUNTRY WAGE ### COUNTRY WAGE ###
#########################################################################################

df_country <- df_country %>% mutate(OLM.WG.MN.NA = ifelse(OLM.WG.MN == 0, NA, OLM.WG.MN), OLM.WG.MN.IHS = ihs(OLM.WG.MN),
                                    OLM.WG.MD.NA = ifelse(OLM.WG.MD == 0, NA, OLM.WG.MD), OLM.WG.MD.IHS = ihs(OLM.WG.MD))

df_country <- df_country %>% mutate(all_vars = ifelse(!is.na(OLM.WG.MN.IHS) & !is.na(OLM.PRJ.CNT.IHS) & !is.na(WB.POP.TOTL) & !is.na(WB.SEC.NENR) & !is.na(WB.NET.BBND.P2) &
                                                        !is.na(WK.ENG.LNG) & !is.na(WB.GSR.CCIS.ZS) & !is.na(WB.NUS.PPP) & !is.na(WB.GDP.PCAP.CD), 1, 0))

country_wage_final <- plm(OLM.WG.MN.IHS ~ WB.NET.BBND.P2 + WB.SEC.NENR + log(WB.POP.TOTL) + WK.ENG.LNG + log(WB.GSR.CCIS.ZS) + I(WB.NUS.PPP/1000) + I(WB.GDP.PCAP.CD/1000),
                          data=df_country %>% filter(all_vars==1, OLM.WG.MN < 250, OLM.PRJ.CNT > 24), index = c("year"), model = "within")

#########################
# Residual plot regressions
res_optimal <- plm(OLM.WG.MN.IHS ~ WB.NET.BBND.P2 + WB.SEC.NENR + log(WB.POP.TOTL) + WK.ENG.LNG + log(WB.GSR.CCIS.ZS) + I(WB.NUS.PPP/1000) + I(WB.GDP.PCAP.CD/1000),
                   data=df_country %>% filter(all_vars==1, OLM.WG.MN < 250, OLM.PRJ.CNT > 24), index = c("year"), model = "within")

res_df <- data.frame(iso3 = (df_country %>% filter(all_vars==1, OLM.WG.MN < 250, OLM.PRJ.CNT > 24))$iso3,
                     year = (df_country %>% filter(all_vars==1, OLM.WG.MN < 250, OLM.PRJ.CNT > 24))$year,
                     OLM.WG.MN = (df_country %>% filter(all_vars==1, OLM.WG.MN < 250, OLM.PRJ.CNT > 24))$OLM.WG.MN.IHS,
                     res_optimal = res_optimal$residuals)

res_long <- res_df %>% gather(key, value, - iso3, - year)

res_long$key <- ifelse(res_long$key == "OLM.WG.MN", "OLM country avg. wage (ihs-transf.)","Residuals of the optimised model (4)")

res_long$key <- factor(res_long$key, levels = c("OLM country avg. wage (ihs-transf.)", "Residuals of the optimised model (4)"))

res_long$labeller <- ifelse(res_long$iso3 %in% c("USA"), "United States",
                            ifelse(res_long$iso3 %in% c("LUX"), "Luxembourg",
                                   ifelse(res_long$iso3 %in% c("NIC"), "Nicaragua",
                                          ifelse(res_long$iso3 %in% c("MDG"), "Madagascar",
                                                 ifelse(res_long$iso3 %in% c("JAM"), "Jamaica",
                                                        ifelse(res_long$iso3 %in% c("AUS"), "Australia",NA))))))

res_long$label_col <- ifelse(!is.na(res_long$labeller), "a", "b")

res_long2 <- res_long

res_long2$facet <- "Online labur avg. wage per country in 2020"

res_long2 <- res_long2 %>% filter(year == "2020") %>% group_by(key) %>% mutate(value2 = value - mean(value))

res_long2 %>% group_by(key) %>% summarise(var2 = sum(value2^2), count = n()) %>% 
  mutate(var2_avg = var2 / (count), sd_avg = sqrt(var2_avg))

label_data <- data.frame(
  key = c("OLM country avg. wage (ihs-transf.)", "Residuals of the optimised model (4)"),
  value2 = c(1.5),
  iso3 = "DEU",
  label_col = "b",
  labels = c("Variance:\n0.18 (100%)", "Variance:\n0.10 (56%)")
)

res_boxplot2 <- res_long2 %>% filter(key %in% c("OLM country avg. wage (ihs-transf.)","Residuals of the optimised model (4)")) %>%
  ggplot(aes(key, y = value2, fill = label_col, col = label_col)) + 
  geom_boxplot(fill = NA, aes(group = key), outlier.alpha = 0, coef = 1, width = 0.6) +
  geom_point(position = position_jitter(0.25, 0.2, seed = 1), shape = 21, size = 2.25, alpha = 0.8) + 
  facet_wrap(~facet) +
  geom_text_repel(aes(label = labeller), position = position_jitter(0.25, 0.2,seed = 1), colour = "black", fill = NA,
                  size = 4.5) +
  scale_fill_manual(values = c(brewer.pal(11,"RdBu")[2],brewer.pal(11,"RdBu")[10]),guide=FALSE) +
  scale_colour_manual(values = c(brewer.pal(11,"RdBu")[2],brewer.pal(11,"RdBu")[10]),guide=FALSE) +
  geom_label(data = label_data, aes(label = labels), fill = "white", colour = "black", size = 4.5) +
  labs(x = "", y = "Spread of OLM avg. wage (ihs-transf.) around mean / residual") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), text = element_text(size = 12), strip.text = element_text(size = 12))

ggarrange(res_boxplot1, res_boxplot2, nrow = 1)


#########################################################################################
### OECD PROJECTS ### OECD PROJECTS ### OECD PROJECTS ### OECD PROJECTS ### OECD PROJECTS
#########################################################################################

df_oecd$iso2 <- sapply(df_oecd$region_id, function(x) substr(x,1,2))

df_oecd <- df_oecd %>% filter(!iso2 %in% c("PE","TN"))

df_oecd <- df_oecd %>% mutate(OLM.PRJ.CNT.NA = ifelse(OLM.PRJ.CNT == 0, NA, OLM.PRJ.CNT),
                              OLM.PRJ.CNT.IHS = ihs(OLM.PRJ.CNT))

df_oecd <- df_oecd %>% mutate(all_vars = ifelse(!is.na(OLM.PRJ.CNT.IHS) & !is.na(OCD.BBD.PCT) & !is.na(OCD.GDP.PC) & !is.na(OCD.ICT.GVA) &
                                                  !is.na(OCD.TED.PCT) & !is.na(OCD.CPT.YES) & !is.na(OCD.PPL.CNT), 1, 0))

oecd_projects_final <- plm(OLM.PRJ.CNT.IHS ~ log(OCD.PPL.CNT) + OCD.BBD.PCT + OCD.TED.PCT + OCD.CPT.YES + log(OCD.GDP.PC) + log(OCD.ICT.GVA),
                           data=df_oecd %>% filter(all_vars==1) %>% mutate(iso_year = paste(str_sub(region_id,0,2), "_", year)), index = c("iso_year"), model = "within")


#########################################################################################
### OECD WAGE ### OECD WAGE ### OECD WAGE ### OECD WAGE ### OECD WAGE ### OECD WAGE ### 
#########################################################################################

df_oecd <- df_oecd %>% mutate(OLM.WG.MN.NA = ifelse(OLM.WG.MN == 0, NA, OLM.WG.MN), OLM.WG.MN.IHS = ihs(OLM.WG.MN),
                              OLM.WG.MD.NA = ifelse(OLM.WG.MD == 0, NA, OLM.WG.MD), OLM.WG.MD.IHS = ihs(OLM.WG.MD))

df_oecd <- df_oecd %>% mutate(all_vars = ifelse(!is.na(OLM.WG.MN.IHS) & !is.na(OCD.BBD.PCT) & !is.na(OCD.GDP.PC) & !is.na(OCD.ICT.GVA) &
                                                  !is.na(OCD.TED.PCT) & !is.na(OCD.CPT.YES) & !is.na(OCD.PPL.CNT) &  OLM.WG.MN < 200, 1, 0))

oecd_wage_final <- plm(OLM.WG.MN.IHS ~ OCD.TED.PCT + OCD.BBD.PCT + log(OCD.ICT.GVA) + OCD.CPT.YES + log(OCD.PPL.CNT) + log(OCD.GDP.PC),
                       data=df_oecd %>% filter(all_vars==1, OLM.WG.MN < 250, OLM.PRJ.CNT > 24)%>% 
                         mutate(iso = str_sub(region_id,0,2), iso_year = paste(iso, "_", year)), index = c("iso_year"), model = "random")


#########################################################################################
### GDL PROJECTS ### GDL PROJECTS ### GDL PROJECTS ### GDL PROJECTS ### GDL PROJECTS ###
#########################################################################################

df_gdl <- df_gdl %>% filter(!country %in% c("China", "Brazil", "Costa Rica", "India", "Indonesia", "Mexico", "South Africa"))

df_gdl <- df_gdl %>% mutate(OLM.PRJ.CNT.NA = ifelse(OLM.PRJ.CNT == 0, NA, OLM.PRJ.CNT),
                              OLM.PRJ.CNT.IHS = ihs(OLM.PRJ.CNT))

df_gdl <- df_gdl %>% filter(GDL.PPL.CNT > 0)

df_gdl <- df_gdl %>% mutate(all_vars = ifelse(!is.na(GDL.PPL.CNT) & !is.na(GDL.INT.PCT) & !is.na(GDL.INC.USD) & !is.na(GDL.IWI.IND) &
                                                  !is.na(GDL.FRM.PCT) & !is.na(GDL.URB.PCT) & !is.na(GDL.EDU.YRS) & !is.na(GDL.CPT.YES), 1, 0))

gdl_projects_final <- plm(OLM.PRJ.CNT.IHS ~ log(GDL.PPL.CNT) + GDL.CPT.YES + GDL.INT.PCT + GDL.INC.USD + GDL.EDU.YRS,
                            data=df_gdl %>% filter(all_vars==1) %>% mutate(iso_year = paste(iso3, "_", year)), index = c("iso_year"), model = "within")

#########################################################################################
### GDL WAGE ### GDL WAGE ### GDL WAGE ### GDL WAGE ### GDL WAGE ### GDL WAGE ###
#########################################################################################

df_gdl <- df_gdl %>% mutate(OLM.WG.MN.NA = ifelse(OLM.WG.MN == 0, NA, OLM.WG.MN), OLM.WG.MN.IHS = ihs(OLM.WG.MN),
                              OLM.WG.MD.NA = ifelse(OLM.WG.MD == 0, NA, OLM.WG.MD), OLM.WG.MD.IHS = ihs(OLM.WG.MD))

df_gdl <- df_gdl %>% mutate(all_vars = ifelse(!is.na(GDL.PPL.CNT) & !is.na(GDL.INT.PCT) & !is.na(GDL.INC.USD) & !is.na(GDL.IWI.IND) &
                                                !is.na(GDL.FRM.PCT) & !is.na(GDL.URB.PCT) & !is.na(GDL.EDU.YRS) & !is.na(GDL.CPT.YES), 1, 0))



gdl_wage_final <- plm(OLM.WG.MN.IHS ~ GDL.INC.USD + GDL.EDU.YRS + GDL.INT.PCT + log(GDL.PPL.CNT) + GDL.CPT.YES,
                         data=df_gdl %>% filter(all_vars==1, OLM.WG.MN < 250, OLM.PRJ.CNT > 24) %>% mutate(iso_year = paste(iso3, "_", year)), index = c("iso_year"), model = "random")


####################################################################################
# Get the final LaTeX tables
stargazer(country_projects_final,oecd_projects_final,gdl_projects_final,country_wage_final,oecd_wage_final,gdl_wage_final, digits = 2, type = "text")

