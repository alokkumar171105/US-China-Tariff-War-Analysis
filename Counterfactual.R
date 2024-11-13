library(readxl)   
library(fixest)
library(dplyr)

file_path <- "Y:/ECO412A/Project/US-China Dataset.xlsx"
data <- read_excel(file_path, sheet = 1)
names(data)[names(data) == "Imp. Value (1000$)"] <- "Imp_Value"
names(data)[names(data) == "GDP (Imp) (mn $)"] <- "GDP_Imp"
names(data)[names(data) == "GDP (Exp) (mn $)"] <- "GDP_Exp"
names(data)[names(data) == "Avg. Tariff"] <- "ATE"
names(data)[names(data) == "NTB (Imp)"] <- "NTB_Imp"
names(data)[names(data) == "NTB(Exp)"] <- "NTB_Exp"

data$log_Imp_Value <- log(data$Imp_Value+1)
data$log_GDP_Imp <- log(data$GDP_Imp)
data$log_GDP_Exp <- log(data$GDP_Exp)
data$log_NTB_Imp <- log(data$NTB_Imp) 
data$log_NTB_Exp <- log(data$NTB_Exp)

#Two-Way Tariff Increment Imposition Model

data_scenario1 <- data %>%
  mutate(
    ATE_1 = case_when(
      (Exporter == "United States" & Importer == "China") | (Exporter == "China" & Importer == "United States") ~ ATE * 3,
      TRUE ~ ATE
    )
  )

model_scenario1 <- fepois(
  log_Imp_Value ~ log_GDP_Imp + log_GDP_Exp + log(ATE_1 +1) + Distance + COB |
    Exporter + Importer, 
  data = data_scenario1
)


summary(model_scenario1)

#One-Way Tariff Increment Imposition Model

data_scenario2 <- data %>%
  mutate(
    ATE_2 = case_when(
      (Exporter == "United States" & Importer == "China") ~ ATE * 3,
      TRUE ~ ATE
    )
  )

model_scenario2 <- fepois(
  log_Imp_Value ~ log_GDP_Imp + log_GDP_Exp + log(ATE_2 + 1)  + Distance + COB |
    Exporter + Importer, 
  data = data_scenario2
)

summary(model_scenario2)

#Comparing Hypothetical Trade Inflows 

data_scenario1 <- data_scenario1 %>%
  mutate(Hypothetical_Trade_Inflows_Scenario1 = exp(fitted(model_scenario1)))

trade_inflows_scenario1 <- data_scenario1 %>%
  filter(Exporter %in% c("United States", "China")) %>%
  group_by(Exporter, Year) %>%
  summarize(Total_Trade_Inflows_Scenario1 = sum(Hypothetical_Trade_Inflows_Scenario1)) %>%
  ungroup()

data_scenario2 <- data_scenario2 %>%
  mutate(Hypothetical_Trade_Inflows_Scenario2 = exp(fitted(model_scenario2)))

trade_inflows_scenario2 <- data_scenario2 %>%
  filter(Exporter %in% c("United States", "China")) %>%
  group_by(Exporter, Year) %>%
  summarize(Total_Trade_Inflows_Scenario2 = sum(Hypothetical_Trade_Inflows_Scenario2)) %>%
  ungroup()

trade_inflows_comparison <- trade_inflows_scenario1 %>%
  left_join(trade_inflows_scenario2, by = c("Exporter", "Year"))

print(trade_inflows_comparison)

