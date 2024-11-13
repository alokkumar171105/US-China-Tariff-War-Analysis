# Installing Libraries
# install.packages("readxl")
# install.packages("fixest")
# install.packages("dplyr")

# Importing Libraries

library(readxl)   
library(fixest)
library(dplyr)

# Uploading and Refining Dataset

file_path <- "Y:/ECO412A/Project/US-China Dataset.xlsx"
data <- read_excel(file_path, sheet = 1)
names(data)[names(data) == "Imp. Value (1000$)"] <- "Imp_Value"
names(data)[names(data) == "GDP (Imp) (mn $)"] <- "GDP_Imp"
names(data)[names(data) == "GDP (Exp) (mn $)"] <- "GDP_Exp"
names(data)[names(data) == "Avg. Tariff"] <- "ATE"
names(data)[names(data) == "NTB (Imp)"] <- "NTB_Imp"
names(data)[names(data) == "NTB(Exp)"] <- "NTB_Exp"
names(data)[names(data) == "ASEAN+"] <- "ASEAN"

data$log_Imp_Value <- log(data$Imp_Value+1)
data$log_GDP_Imp <- log(data$GDP_Imp)
data$log_GDP_Exp <- log(data$GDP_Exp)
data$log_ATE <- log(data$ATE+1)
data$log_NTB_Imp <- log(data$NTB_Imp) 
data$log_NTB_Exp <- log(data$NTB_Exp)


# ---------------------------X-----------------General Eq. Effects-----------------X--------------------------- #

#Fixed Effects Poisson (GE-Model)

model <- fepois(log_Imp_Value ~ log_GDP_Imp + log_GDP_Exp + log_ATE + log_NTB_Imp + log_NTB_Exp + Distance + COB | Exporter + Importer, data = data)
summary(model)

#Fixed Effects Data frames of Importers and Exporters

exporter_fe <- fixef(model)$Exporter
importer_fe <- fixef(model)$Importer

exporter_fe_df <- data.frame(Country = names(exporter_fe), Fixed_Effect = exporter_fe)
importer_fe_df <- data.frame(Country = names(importer_fe), Fixed_Effect = importer_fe)

print(exporter_fe_df)
print(importer_fe_df)

#GDP Change

gdp_changes <- data %>%
  group_by(Country = Importer, Year) %>%  
  summarize(GDP = mean(GDP_Imp, na.rm = TRUE)) %>%  
  arrange(Country, Year) %>%
  mutate(GDP_Change = (GDP - lag(GDP)) / lag(GDP) * 100) %>%
  filter(Year >= 2018) %>%
  ungroup()

print(gdp_changes)

#FGP Change

fgp_data <- read_excel(file_path, sheet = "GE-Data")

fgp_changes <- fgp_data %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(PPI_Change = (PPI - lag(PPI)) / lag(PPI) * 100) %>%
  filter(Year >= 2018) %>%
  ungroup()

print(fgp_changes)

#Overall Welfare Change

welfare_data <- data %>%
  left_join(fgp_changes, by = c("Importer" = "Country", "Year" = "Year")) %>%
  arrange(Importer, Year) %>%
  group_by(Importer) %>%
  mutate(
    GDP_Growth = (GDP_Imp - lag(GDP_Imp)) / lag(GDP_Imp) * 100,
    Welfare_Change = GDP_Growth - PPI_Change
  ) %>%
  ungroup()

# World Welfare (Taking these 9 countries as a Mini World)

world_welfare <- welfare_data %>%
  group_by(Year) %>%
  summarize(World_Welfare = mean(Welfare_Change, na.rm = TRUE)) %>%
  filter(Year >= 2018)

print(world_welfare)

# Unique Relative Welfare for Each Country

welfare_data_relative <- welfare_data %>%
  left_join(world_welfare, by = "Year") %>%
  mutate(Relative_Welfare = Welfare_Change / World_Welfare) %>%
  select(Importer, Year, Welfare_Change, World_Welfare, Relative_Welfare)

welfare_stats_table <- welfare_data_relative %>%
  filter(Year >= 2018) %>%
  select(Country = Importer, Year, Welfare_Statistic = Relative_Welfare) %>%
  arrange(Year, desc(Welfare_Statistic)) %>%
  distinct(Country, Year, .keep_all = TRUE) 

print(welfare_stats_table)

# Net Foreign Export Change

annual_exports <- data %>%
  group_by(Exporter = Importer, Year) %>%  
  summarize(Annual_Exp_Value = sum(Imp_Value, na.rm = TRUE)) %>%
  ungroup()

annual_exports <- annual_exports %>%
  arrange(Exporter, Year) %>%
  group_by(Exporter) %>%
  mutate(
    Exp_Value_Change_Pct = (Annual_Exp_Value - lag(Annual_Exp_Value)) / lag(Annual_Exp_Value) * 100
  ) %>%
  filter(Year >= 2018) %>%
  ungroup()

print(annual_exports)


# OMR and IMR Change

omr_yearly <- list()
imr_yearly <- list()

for (year in unique(data$Year)) {

  data_year <- data %>% filter(Year == year)
  
  model_ols <- feols(
    log_Imp_Value ~ log_GDP_Imp + log_GDP_Exp + log_ATE + log_NTB_Imp + log_NTB_Exp + Distance |
      Exporter + Importer, 
    data = data_year
  )
  
  omr <- fixef(model_ols)$Exporter
  imr <- fixef(model_ols)$Importer
  
  imr <- imr + mean(imr, na.rm = TRUE)
  
  omr_yearly[[as.character(year)]] <- data.frame(Country = names(omr), Year = year, OMR = omr)
  imr_yearly[[as.character(year)]] <- data.frame(Country = names(imr), Year = year, IMR = imr)
}

omr_df <- bind_rows(omr_yearly)
imr_df <- bind_rows(imr_yearly)

omr_changes <- omr_df %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(OMR_Change = OMR - lag(OMR)) %>%
  filter(Year >= 2018) %>%
  ungroup()

imr_changes <- imr_df %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(IMR_Change = IMR - lag(IMR)) %>%
  filter(Year >= 2018) %>%
  ungroup()

print(omr_changes %>% select(Country, Year, OMR_Change))
print(imr_changes %>% select(Country, Year, IMR_Change))

# ---------------------------X-----------------Trade Impact Effects-----------------X--------------------------- #

# Estimating Coefficients of Trade Agreements 

model_ti <- feols(
  log_Imp_Value ~ log_GDP_Imp + log_GDP_Exp + Distance + COB + USMCA + RCEP + ASEAN |
    Exporter + Importer, 
  data = data
)

coeff_usmca <- coef(model_ti)["USMCA"]
coeff_rcep <- coef(model_ti)["RCEP"]
coeff_asean <- coef(model_ti)["ASEAN"]
coeff_cob <- coef(model_ti)["COB"]

print(coeff_usmca)
print(coeff_rcep)
print(coeff_asean)
print(coeff_cob)

#PTI (Partial Trade Impact) Effects 

pti_usmca <- exp(coef(model_ti)["USMCA"])
pti_rcep <- exp(coef(model_ti)["RCEP"])
pti_asean <- exp(coef(model_ti)["ASEAN"])
pti_cob <- exp(coef(model_ti)["COB"])

print(pti_usmca)
print(pti_rcep)
print(pti_asean)
print(pti_cob)

#MTI (Modular Trade Impact) Effects 

model_usmca <- feols(
  log_Imp_Value ~ Distance + USMCA | Exporter + Importer,
  data = data
)

mti_usmca <- exp(coef(model_usmca)["USMCA"])
print(mti_usmca)

model_rcep <- feols(
  log_Imp_Value ~ Distance + RCEP | Exporter + Importer,
  data = data
)

mti_rcep <- exp(coef(model_rcep)["RCEP"])
print(mti_rcep)

model_asean <- feols(
  log_Imp_Value ~ Distance + ASEAN| Exporter + Importer,
  data = data
)

mti_asean <- exp(coef(model_asean)["ASEAN"])
print(mti_asean)

model_cob <- feols(
  log_Imp_Value ~ Distance + COB| Exporter + Importer,
  data = data
)

mti_cob <- exp(coef(model_cob)["COB"])
print(mti_cob)

#GETI (General Equilibrium Trade Impact) Effects

model_geti <- fepois(
  Imp_Value ~ log_GDP_Imp + log_GDP_Exp + Distance + COB + USMCA + RCEP + ASEAN |
    Exporter + Importer,
  data = data
)

geti_usmca <- exp(coef(model_geti)["USMCA"])
geti_rcep <- exp(coef(model_geti)["RCEP"])
geti_asean <- exp(coef(model_geti)["ASEAN"])
geti_cob <- exp(coef(model_geti)["COB"])

print(geti_usmca)
print(geti_rcep)
print(geti_asean)
print(geti_cob)

