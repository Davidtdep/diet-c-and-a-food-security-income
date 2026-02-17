#*******************************************************************************
#### 0. LIBRARIES ####
#*******************************************************************************

library(readxl)
library(dplyr)
library(tibble)
library(purrr)
library(MASS)      
library(sandwich)  
library(lmtest)    
library(lme4)
library(glmmTMB)


#*******************************************************************************
#### 1. INPUT ####
#*******************************************************************************

data = read_excel("~/data.xlsx")




#*******************************************************************************
#### 2. DEPURATION ####
#*******************************************************************************

data_filtered = data

##### 2.1. Replace "-" and "0" with NA #####
data_filtered$Cuartil[data_filtered$Cuartil %in% c("-", "0")] <- NA


##### 2.2. Decimales #####
data_filtered <- data_filtered |>
  mutate(
    across(
      c(
        `NUMBER OF DALYS`,
        `NUMBER OF DEATHS`,
        `Population in year`,
        `Population,ages 65+`,
        `Sex ratio`,
        `Lifespan Inequality in wome`,
        `Lifespan Inequality in man`,
        `Gini Coefficient`,
        `Multidimensional Poverty Index`,
        `Number of people living in extreme poverty`,
        `GDP per capita`,
        `Human Development Index`,
        `Income inequality: Atkinson index`,
        `Human rights index`,
        `Rigorous and impartial public administration index`,
        `State capacity index`,
        `Functioning government index`,
        `Political corruption index`,
        `OFFICIAL DEVELOPMENT ASSISTANCE (ODA) FOR MEDICAL RESEARCH AND BASIC HEALTH SECTORS PER CAPITA, BY REGION`,
        `Share of GDP from agriculture, 1960 to 2024`,
        `Indicator of food price anomalies, 2010 to 2022`,
        `Number of people that cannot afford a calorie sufficient diet, 2021`,
        `Number of people that cannot afford a healthy diet, 2024`,
        `Number of people that cannot afford a nutrient adequate diet, 2021`,
        `Number of people who are undernourished`,
        `Death rate from malnutrition, 2021`,
        `Global Hunger Index, 2021`,
        `Inequality in per capita calorie intake, 2020`,
        `Number of people who are moderately or severely food insecure, 2022`,
        `Number of people who are severely food insecure, 2022`,
        `Hidden Hunger Index in pre-school children`
      ),
      ~ .x |>
        trimws() |>                  # 1. Remove leading/trailing spaces
        (\(z) gsub(",", "", z))() |> # 2. Remove thousand separators (commas)
        as.numeric()                 # 3. Convert to numeric (keeps decimals)
    )
  )


##### 2.3. Percentage #####
data_filtered <- data_filtered |>
  mutate(
    across(
      c(`DEATH RATE`,
        `OUT-OF-POCKED EXPENDITURE ON HEALTH`,
        `Child mortality rate`,
        `Share of population with no formal education`,
        `Elderly Literacy Rate`,
        `Youth Literacy Rate`,
        `Percentage of territory effectively controlled by government`,
        `Share of workers in informal employment in the agricultural sector, 2000\r\nto 2023`,
        `Share of population that cannot afford a healthy diet, 2024`,
        `Share of consumer expenditure spent on food, 2017 to 2023`,
        `Share of population that cannot afford a calorie sufficient diet, 2021`,
        `Cost of a healthy diet as a share of average food spending, 2021`,
        `Cost of a calorie sufficient diet as a share of average food expenditure,\r\n2021`,
        `Cost of a nutrient adequate diet as a share of average food expenditure,\r\n2021`,
        `Malnutrition: Share of children who are stunted`,
        `Malnutrition: Share of children who are underweight, 2024`,
        `Share of people who are undernourished`,
        `Share of population with moderate or severe food insecurity, 2022`,
        `Share of population with severe food insecurity, 2022`,
        `Share of children receiving vitamin A supplementation`,
        `Share of children who have vitamin A deficiency`,
        `Share of children who have anemia`,
        `Share of households consuming iodized salt, 2020`,
        `Share of people who have zinc deficiency, 2005`,
        `Share of women of reproductive age who have anemia`,
        `Share of pregnant women who have vitamin A deficiency`,
        `Share of pregnant women who have anemia`
        ),
      ~ .x |>
        trimws() |>                   # 1. Remove leading/trailing spaces
        (\(z) gsub("%", "", z))() |>  # 2. Remove percent symbol
        as.numeric() / 100            # 3. Convert to proportion (0–1)
    )
  )


##### 2.4. Years #####
data_filtered <- data_filtered |>
  mutate(
    across(
      c(`Life expectancy at birth`,
        `Sex gap in life expectancy`,
        `Healthy life expectancy`,
        `Average years of schooling`
        ),
      ~ .x |>
        trimws() |>                        # 1. Remove leading/trailing spaces
        (\(z) gsub("[^0-9.]", "", z))() |> # 2. Keep only digits and decimal point
        na_if("") |>                       # 3. Convert empty strings to NA
        as.numeric()                       # 4. Convert to numeric
    )
  )



##### 2.5. Food with a final "t"   #####
data_filtered <- data_filtered |>
  mutate(
    across(
      c(
        62:102,
        `Dietary composition by country, 1961 to 2022`,
        `Fruit consumption per capita, 1961 to 2022`,
        `Vegetable consumption per capita, 1961 to 2022`
      ),
      ~ .x |>
        as.character() |>                 # 1. Ensure the values are character
        trimws() |>                       # 2. Remove leading/trailing spaces
        (\(z) gsub("[^0-9.]", "", z))() |># 3. Keep only digits and decimal point
        na_if("") |>                      # 4. Turn empty strings into NA
        as.numeric()                      # 5. Convert to numeric
    )
  )



##### 2.6. Dollar symbol $ #####
data_filtered <- data_filtered |>
  mutate(
    across(
      c(
        `Agricultural value added per worker, 2023`,
        `Average income of small-scale food producers, 2022`,
        `Daily cost of a calorie sufficient diet, 2021`,
        `Daily cost of a healthy diet, 2024`,
        `Daily cost of a nutrient adequate diet, 2021`,
        `Average income of large-scale food producers, 2022`
      ),
      ~ .x |>
        as.character() |>                  # 1. Ensure values are character
        trimws() |>                        # 2. Remove leading/trailing spaces
        (\(z) gsub("[^0-9.-]", "", z))() |># 3. Keep digits, decimal point, and minus sign
        na_if("") |>                       # 4. Convert empty strings to NA
        as.numeric()                       # 5. Convert to numeric
    )
  )





#*******************************************************************************
#### 3. AGGREGATION ####
#*******************************************************************************

##### 3.1. By Income Group #####

data_filtered_income <- data_filtered %>%
  group_by(income, PY) %>%
  summarise(
    total_publications = n(),
    citations = sum(TC, na.rm = TRUE),
    Q1 = sum(Cuartil == "Q1", na.rm = TRUE),
    Q2 = sum(Cuartil == "Q2", na.rm = TRUE),
    Q3 = sum(Cuartil == "Q3", na.rm = TRUE),
    Q4 = sum(Cuartil == "Q4", na.rm = TRUE),
    .groups = "drop"
  )




######  3.1.1. Define the indicator columns and the weighting variable ######

# All indicator columns live in positions 18:145 of data_filtered
indicator_cols <- names(data_filtered)[18:145]

# This is the variable we will use as the weight for the weighted means
# (total population in that country-year)
weight_var <- "Population in year"

# Remove the weight variable from the list of indicators
# (we don't want to compute a weighted mean of the weight itself)
indicator_cols <- setdiff(indicator_cols, weight_var)


######  3.1.2. Collapse to income–year–country level ######
# Because indicators are country-level, we first aggregate all articles
# from the same country, in the same income group, and same year.

data_country_year <- data_filtered %>%
  # Drop rows with missing country or income (cannot be grouped properly)
  filter(!is.na(country), !is.na(income)) %>%
  group_by(income, PY, country) %>%
  summarise(
    # For each country-year-income, take the mean of all indicators
    # (in case there are multiple articles from the same country-year)
    across(
      all_of(c(indicator_cols, weight_var)),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )


###### 3.1.3. Compute population-weighted averages by income–year ###### 
# Now we aggregate from country-level to income-group-level for each year,
# using "Population in year" as the weight.

indicators_income <- data_country_year %>%
  group_by(income, PY) %>%
  summarise(
    # For each indicator, compute the population-weighted mean
    across(
      all_of(indicator_cols),
      ~ weighted.mean(.x, w = `Population in year`, na.rm = TRUE)
    ),
    # Also keep the total population for that income–year
    `Population in year` = sum(`Population in year`, na.rm = TRUE),
    .groups = "drop"
  )


######  3.1.4. Original publication-level summary (counts, citations, quartiles) ###### 

data_filtered_income <- data_filtered %>%
  group_by(income, PY) %>%
  summarise(
    # Number of publications in that income–year
    total_publications = n(),
    # Total citations in that income–year
    citations = sum(TC, na.rm = TRUE),
    # Number of papers in each quartile
    Q1 = sum(Cuartil == "Q1", na.rm = TRUE),
    Q2 = sum(Cuartil == "Q2", na.rm = TRUE),
    Q3 = sum(Cuartil == "Q3", na.rm = TRUE),
    Q4 = sum(Cuartil == "Q4", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 5. Join the publication summary with the weighted indicators
  left_join(indicators_income, by = c("income", "PY"))





#*******************************************************************************
#### 4. PRE-ANALYSIS ####
#*******************************************************************************

##### 4.1. Define group of variables #####

## OJO: cambia `df` por el nombre real de tu data.frame si lo usas luego para subsetear

indicator_clusters <- list(
  # 1. Sistema de salud: financiación, recursos y acceso
  health_system_and_financing = c(
    "CURRENT HEALTH EXPENDITURE (% OF GDP)",
    "PHYSICIANS (PER 1,000 PEOPLE)",
    "NURSES AND MIDWIVES (PER 1,000 PEOPLE)",
    "HEALTHCARE ACCESS AND QUALITY",
    "OUT-OF-POCKED EXPENDITURE ON HEALTH"
  ),
  
  # 2. Resultados en salud y demografía
  health_outcomes_and_demography = c(
    "NUMBER OF DALYS",
    "NUMBER OF DEATHS",
    "DEATH RATE",
    "Population,ages 65+",
    "Child mortality rate",
    "Life expectancy at birth",
    "Healthy life expectancy",
    "Lifespan Inequality in wome",
    "Lifespan Inequality in man",
    "Sex ratio",
    "Sex gap in life expectancy"
  ),
  
  # 3. Socioeconómico, pobreza, educación y desarrollo humano
  socioeconomic_poverty_education = c(
    "Gini Coefficient",
    "Multidimensional Poverty Index",
    "Number of people living in extreme poverty",
    "GDP per capita",
    "Share of population with no formal education",
    "Average years of schooling",
    "Elderly Literacy Rate",
    "Youth Literacy Rate",
    "Human Development Index",
    "Income inequality: Atkinson index"
  ),
  
  # 4. Gobernanza e instituciones
  governance_and_institutions = c(
    "Human rights index",
    "Percentage of territory effectively controlled by government",
    "Rigorous and impartial public administration index",
    "State capacity index",
    "Functioning government index",
    "Political corruption index",
    "Corruption Perception Index"
  ),
  
  # 5. Sistema de investigación, innovación y conocimiento
  research_and_innovation_system = c(
    "RESEARCH AND DEVELOPMENT EXPENDITURE (% OF GDP)",
    "CHARGES FOR THE USE OF INTELLECTUAL PROPERTY, PAYMENTS (BOP, CURRENT US$)",
    "higher education institutions offering disciplines related to research for health in 2023 by region",
    "higher education institutions offering disciplines related to research for health in 2023 by income",
    "HEALTH RESEARCHERS (IN FULL-TIME EQUIVALENT) PER MILLION INHABITANTS, BY WHO REGION",
    "HEALTH RESEARCHERS (IN FULL-TIME EQUIVALENT) PER MILLION INHABITANTS, BY INCOME GROUP",
    "HEALTH RESEARCHERS (IN FULL-TIME EQUIVALENT) PER MILLION INHABITANTS, BY COUNTRY"
  ),
  
  # 6. Ayuda externa para salud e I+D
  health_aid_and_external_finance = c(
    "OFFICIAL DEVELOPMENT ASSISTANCE (ODA) FOR MEDICAL RESEARCH AND BASIC HEALTH SECTORS PER CAPITA, BY REGION",
    "OFFICIAL DEVELOPMENT ASSISTANCE (ODA) FOR MEDICAL RESEARCH AND BASIC HEALTH SECTORS PER CAPITA, BY INCOME",
    "OFFICIAL DEVELOPMENT ASSISTANCE (ODA) FOR MEDICAL RESEARCH AND BASIC HEALTH SECTORS PER CAPITA, BY RECIPIENT COUNTRY"
  ),
  
  # 7. Producción agrícola (crops)
  agricultural_crop_production = c(
    "Cereal production, 1961 to 2023",
    "Corn production, 1961 to 2023",
    "Rice production, 2023",
    "Agricultural output, 1961 to 2023",
    "Apple production, 2023",
    "Avocado production, 2023",
    "Banana production, 2023",
    "Banana production by region, 1961 to 2023",
    "Barley production, 2023",
    "Bean production, 2023",
    "Cashew nut production, 2023",
    "Cocoa bean production, 2023",
    "Cocoa bean production by region, 1961 to 2023",
    "Coffee production by region, 1961 to 2023",
    "Green coffee beans production, 2023",
    "Oil palm production",
    "Potato production, 2023",
    "Sesame seed production, 2023",
    "Soybean production, 2023",
    "Sugar cane production, 2023",
    "Tomato production, 2023",
    "Value of agricultural production, 2023",
    "Wheat production, 2023",
    "Wine production, 2022",
    "Yams production, 2023"
  ),
  
  # 8. Producción pecuaria y pesquera
  agricultural_livestock_fish_production = c(
    "Global meat production, 1961 to 2023",
    "Beef production, 2023",
    "Poultry production, 2023",
    "Pig meat production, 2023",
    "Meat supply per person, 2022",
    "Milk production, 2023",
    "Milk supply per person, 2022",
    "Egg production, 2023",
    "Fish and seafood production, 2022",
    "Capture fishery production, 2022",
    "Aquaculture production, 2022"
  ),
  
  # 9. Insumos agrícolas y presión ambiental
  agricultural_inputs_and_environment = c(
    "Fertilizer consumption, 1961 to 2022",
    "Excess phosphorus from croplands"
  ),
  
  # 10. Estructura, productividad y empleo agrícola
  agricultural_structure_productivity_employment = c(
    "Average farm size",
    "Productivity of small-scale food producers, 2021",
    "Agricultural value added per worker, 2023",
    "Share of the labor force employed in agriculture, 2019",
    "Share of GDP from agriculture, 1960 to 2024",
    "Share of workers in informal employment in the agricultural sector, 2000 to 2023",
    "Average income of small-scale food producers, 2022",
    "Average income of large-scale food producers, 2022"
  ),
  
  # 11. Costo y asequibilidad de la dieta / precios
  diet_cost_and_affordability = c(
    "Share of population that cannot afford a healthy diet, 2024",
    "Food expenditure per person, 2017 to 2023",
    "Share of consumer expenditure spent on food, 2017 to 2023",
    "Share of population that cannot afford a calorie sufficient diet, 2021",
    "Cost of a healthy diet as a share of average food spending, 2021",
    "Cost of a calorie sufficient diet as a share of average food expenditure, 2021",
    "Cost of a nutrient adequate diet as a share of average food expenditure, 2021",
    "Daily cost of a calorie sufficient diet, 2021",
    "Daily cost of a healthy diet, 2024",
    "Daily cost of a nutrient adequate diet, 2021",
    "Indicator of food price anomalies, 2010 to 2022",
    "Number of people that cannot afford a calorie sufficient diet, 2021",
    "Number of people that cannot afford a healthy diet, 2024",
    "Number of people that cannot afford a nutrient adequate diet, 2021"
  ),
  
  # 12. Inseguridad alimentaria y hambre
  food_insecurity_and_hunger = c(
    "Malnutrition: Share of children who are stunted",
    "Malnutrition: Share of children who are underweight, 2024",
    "Share of people who are undernourished",
    "Number of people who are undernourished",
    "Death rate from malnutrition, 2021",
    "Global Hunger Index, 2021",
    "Inequality in per capita calorie intake, 2020",
    "Number of people who are moderately or severely food insecure, 2022",
    "Number of people who are severely food insecure, 2022",
    "Share of population with moderate or severe food insecurity, 2022",
    "Share of population with severe food insecurity, 2022"
  ),
  
  # 13. Patrones dietarios y consumo
  dietary_patterns_and_intake = c(
    "Dietary composition by country, 1961 to 2022",
    "Fruit consumption per capita, 1961 to 2022",
    "Vegetable consumption per capita, 1961 to 2022"
  ),
  
  # 14. Micronutrientes y deficiencias específicas
  micronutrient_and_specific_deficiencies = c(
    "Hidden Hunger Index in pre-school children",
    "Share of children receiving vitamin A supplementation",
    "Share of children who have vitamin A deficiency",
    "Share of children who have anemia",
    "Share of households consuming iodized salt, 2020",
    "Share of people who have zinc deficiency, 2005",
    "Share of women of reproductive age who have anemia",
    "Share of pregnant women who have vitamin A deficiency",
    "Share of pregnant women who have anemia"
  ),
  
  # 15. Tamaño poblacional (denominador general)
  population_size = c(
    "Population in year"
  )
)




##### 4.2. Define direction of variables #####


indicator_roles <- tribble(
  ~variable, ~role,
  "CURRENT HEALTH EXPENDITURE (% OF GDP)", "dependent",
  "PHYSICIANS (PER 1,000 PEOPLE)", "independent",
  "NURSES AND MIDWIVES (PER 1,000 PEOPLE)", "independent",
  "RESEARCH AND DEVELOPMENT EXPENDITURE (% OF GDP)", "dependent",
  "CHARGES FOR THE USE OF INTELLECTUAL PROPERTY, PAYMENTS (BOP, CURRENT US$)", "dependent",
  "NUMBER OF DALYS", "dependent",
  "NUMBER OF DEATHS", "dependent",
  "DEATH RATE", "dependent",
  "HEALTHCARE ACCESS AND QUALITY", "dependent",
  "OUT-OF-POCKED EXPENDITURE ON HEALTH", "dependent",
  "Population,ages 65+", "independent",
  "Child mortality rate", "dependent",
  "Life expectancy at birth", "dependent",
  "Sex ratio", "independent",
  "Sex gap in life expectancy", "dependent",
  "Healthy life expectancy", "dependent",
  "Lifespan Inequality in wome", "dependent",
  "Lifespan Inequality in man", "dependent",
  "Gini Coefficient", "dependent",
  "Multidimensional Poverty Index", "dependent",
  "Number of people living in extreme poverty", "dependent",
  "GDP per capita", "dependent",
  "Share of population with no formal education", "dependent",
  "Average years of schooling", "dependent",
  "Elderly Literacy Rate", "dependent",
  "Youth Literacy Rate", "dependent",
  "Human Development Index", "dependent",
  "Income inequality: Atkinson index", "dependent",
  "Human rights index", "dependent",
  "Percentage of territory effectively controlled by government", "independent",
  "Rigorous and impartial public administration index", "dependent",
  "State capacity index", "dependent",
  "Functioning government index", "dependent",
  "Political corruption index", "independent",
  "Corruption Perception Index", "independent",
  "higher education institutions offering disciplines related to research for health in 2023 by region", "independent",
  "higher education institutions offering disciplines related to research for health in 2023 by income", "independent",
  "HEALTH RESEARCHERS (IN FULL-TIME EQUIVALENT) PER MILLION INHABITANTS, BY WHO REGION", "independent",
  "HEALTH RESEARCHERS (IN FULL-TIME EQUIVALENT) PER MILLION INHABITANTS, BY INCOME GROUP", "independent",
  "HEALTH RESEARCHERS (IN FULL-TIME EQUIVALENT) PER MILLION INHABITANTS, BY COUNTRY", "independent",
  "OFFICIAL DEVELOPMENT ASSISTANCE (ODA) FOR MEDICAL RESEARCH AND BASIC HEALTH SECTORS PER CAPITA, BY REGION", "dependent",
  "OFFICIAL DEVELOPMENT ASSISTANCE (ODA) FOR MEDICAL RESEARCH AND BASIC HEALTH SECTORS PER CAPITA, BY INCOME", "dependent",
  "OFFICIAL DEVELOPMENT ASSISTANCE (ODA) FOR MEDICAL RESEARCH AND BASIC HEALTH SECTORS PER CAPITA, BY RECIPIENT COUNTRY", "dependent",
  "Cereal production, 1961 to 2023", "dependent",
  "Corn production, 1961 to 2023", "dependent",
  "Rice production, 2023", "dependent",
  "Agricultural output, 1961 to 2023", "dependent",
  "Apple production, 2023", "dependent",
  "Average farm size", "dependent",
  "Avocado production, 2023", "dependent",
  "Banana production, 2023", "dependent",
  "Banana production by region, 1961 to 2023", "dependent",
  "Barley production, 2023", "dependent",
  "Bean production, 2023", "dependent",
  "Cashew nut production, 2023", "dependent",
  "Chicken meat production, 2023", "dependent",
  "Cocoa bean production, 2023", "dependent",
  "Cocoa bean production by region, 1961 to 2023", "dependent",
  "Coffee production by region, 1961 to 2023", "dependent",
  "Green coffee beans production, 2023", "dependent",
  "Oil palm production", "dependent",
  "Potato production, 2023", "dependent",
  "Productivity of small-scale food producers, 2021", "dependent",
  "Sesame seed production, 2023", "dependent",
  "Soybean production, 2023", "dependent",
  "Sugar cane production, 2023", "dependent",
  "Tomato production, 2023", "dependent",
  "Value of agricultural production, 2023", "dependent",
  "Wheat production, 2023", "dependent",
  "Wine production, 2022", "dependent",
  "Yams production, 2023", "dependent",
  "Global meat production, 1961 to 2023", "dependent",
  "Beef production, 2023", "dependent",
  "Poultry production, 2023", "dependent",
  "Pig meat production, 2023", "dependent",
  "Meat supply per person, 2022", "dependent",
  "Milk production, 2023", "dependent",
  "Milk supply per person, 2022", "dependent",
  "Egg production, 2023", "dependent",
  "Fish and seafood production, 2022", "dependent",
  "Capture fishery production, 2022", "dependent",
  "Aquaculture production, 2022", "dependent",
  "Fertilizer consumption, 1961 to 2022", "dependent",
  "Excess phosphorus from croplands", "dependent",
  "Agricultural value added per worker, 2023", "dependent",
  "Share of the labor force employed in agriculture, 2019", "dependent",
  "Share of GDP from agriculture, 1960 to 2024", "dependent",
  "Share of workers in informal employment in the agricultural sector, 2000\r\nto 2023", "dependent",
  "Share of population that cannot afford a healthy diet, 2024", "dependent",
  "Food expenditure per person, 2017 to 2023", "dependent",
  "Share of consumer expenditure spent on food, 2017 to 2023", "dependent",
  "Share of population that cannot afford a calorie sufficient diet, 2021", "dependent",
  "Average income of small-scale food producers, 2022", "dependent",
  "Average income of large-scale food producers, 2022", "dependent",
  "Cost of a healthy diet as a share of average food spending, 2021", "dependent",
  "Cost of a calorie sufficient diet as a share of average food expenditure,\r\n2021", "dependent",
  "Cost of a nutrient adequate diet as a share of average food expenditure,\r\n2021", "dependent",
  "Daily cost of a calorie sufficient diet, 2021", "dependent",
  "Daily cost of a healthy diet, 2024", "dependent",
  "Daily cost of a nutrient adequate diet, 2021", "dependent",
  "Indicator of food price anomalies, 2010 to 2022", "dependent",
  "Number of people that cannot afford a calorie sufficient diet, 2021", "dependent",
  "Number of people that cannot afford a healthy diet, 2024", "dependent",
  "Number of people that cannot afford a nutrient adequate diet, 2021", "dependent",
  "Malnutrition: Share of children who are stunted", "dependent",
  "Malnutrition: Share of children who are underweight, 2024", "dependent",
  "Share of people who are undernourished", "dependent",
  "Number of people who are undernourished", "dependent",
  "Death rate from malnutrition, 2021", "dependent",
  "Global Hunger Index, 2021", "dependent",
  "Inequality in per capita calorie intake, 2020", "dependent",
  "Number of people who are moderately or severely food insecure, 2022", "dependent",
  "Number of people who are severely food insecure, 2022", "dependent",
  "Share of population with moderate or severe food insecurity, 2022", "dependent",
  "Share of population with severe food insecurity, 2022", "dependent",
  "Dietary composition by country, 1961 to 2022", "dependent",
  "Fruit consumption per capita, 1961 to 2022", "dependent",
  "Vegetable consumption per capita, 1961 to 2022", "dependent",
  "Hidden Hunger Index in pre-school children", "dependent",
  "Share of children receiving vitamin A supplementation", "dependent",
  "Share of children who have vitamin A deficiency", "dependent",
  "Share of children who have anemia", "dependent",
  "Share of households consuming iodized salt, 2020", "dependent",
  "Share of people who have zinc deficiency, 2005", "dependent",
  "Share of women of reproductive age who have anemia", "dependent",
  "Share of pregnant women who have vitamin A deficiency", "dependent",
  "Share of pregnant women who have anemia", "dependent",
  "Population in year", "independent"
)









#*******************************************************************************
#### 5. REGRESSION MODELS ####
#*******************************************************************************


##### 5.1. Helper functions: variable type detection #####

# Integer-like, non-negative -> treat as counts
is_count_like <- function(x, tol = 1e-8) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(FALSE)
  all(x >= 0) && all(abs(x - round(x)) < tol)
}

# Bounded in [0,1] -> treat as proportions / fractional outcomes
is_proportion_like <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(FALSE)
  all(x >= 0 & x <= 1)
}

# Simple heuristic for strong right-skew among strictly positive continuous outcomes
# If 95th percentile is >10x the median, we prefer a log-linear model.
is_strongly_skewed_positive <- function(x) {
  x <- x[is.finite(x)]
  x <- x[x > 0]
  if (length(x) < 10) return(FALSE)
  med <- stats::median(x)
  q95 <- stats::quantile(x, 0.95, names = FALSE)
  if (med <= 0) return(FALSE)
  (q95 / med) > 10
}

# Build a formula with backticks to safely handle spaces/special characters in column names
bt_formula <- function(y, x) {
  as.formula(paste0("`", y, "` ~ `", x, "`"))
}

# Name-based guard to avoid misclassifying "continuous-but-integer" variables
# (e.g., income/GDP/cost/index scores that are stored as integers) as count outcomes.
# This is critical to prevent Poisson/NB being selected just because values are integers.
is_noncount_by_name <- function(var_name) {
  grepl(
    "income|gdp|cost|expend|spend|usd|cop|peso|dollar|price|per\\s*capita|per_capita|index|rate\\b|percent|%|median|mean|average",
    var_name,
    ignore.case = TRUE
  )
}


##### 5.2. Model selection (GLOBAL per indicator) + model fitting (FIXED per group) ######

# Choose ONE model type per (y,x) using pooled data across ALL groups.
# Then enforce feasibility across groups so all strata use the SAME model type.
# This guarantees comparability within income-group runs and within region runs.
choose_global_model_spec <- function(df_all, group_var, groups, y, x, pub_var,
                                     min_n = 8) {
  
  # Use pooled data (across all groups) for model choice
  d2_all <- df_all %>%
    dplyr::select(all_of(c("PY", group_var, y, x))) %>%
    dplyr::filter(is.finite(.data[[y]]), is.finite(.data[[x]])) %>%
    dplyr::arrange(PY)
  
  if (nrow(d2_all) < min_n) {
    return(list(
      chosen = "Too few observations",
      model_type = "Too few observations",
      transform = NA_character_
    ))
  }
  
  yv_all <- d2_all[[y]]
  
  # RULE 1: Publications as outcome are always treated as count data
  force_count <- identical(y, pub_var)
  
  # RULE 2: Count detection is allowed only if not clearly continuous by name
  allow_count_by_value <- is_count_like(yv_all) && !is_noncount_by_name(y)
  
  # A) Count outcomes -> (Poisson vs NB) chosen once, then enforced across groups
  if (force_count || allow_count_by_value) {
    
    # Overdispersion check on pooled data
    mu <- mean(yv_all, na.rm = TRUE)
    va <- stats::var(yv_all, na.rm = TRUE)
    disp <- ifelse(mu > 0, va / mu, Inf)
    
    prefer_nb <- is.finite(disp) && disp > 1.5
    
    # Feasibility enforcement for NB:
    # Only choose NB if NB fits in *all* groups with n >= min_n.
    nb_fits_all <- FALSE
    if (prefer_nb) {
      nb_fits_all <- TRUE
      fml <- bt_formula(y, x)
      
      for (g in groups) {
        d_g <- d2_all %>% dplyr::filter(.data[[group_var]] == g)
        if (nrow(d_g) < min_n) next
        
        mod_nb <- tryCatch(MASS::glm.nb(fml, data = d_g), error = function(e) NULL)
        if (is.null(mod_nb)) {
          nb_fits_all <- FALSE
          break
        }
      }
    }
    
    if (prefer_nb && nb_fits_all) {
      return(list(
        chosen = "count_nb",
        model_type = "Negative binomial (log link)",
        transform = "log-link"
      ))
    } else {
      return(list(
        chosen = "count_pois",
        model_type = "Poisson (log link)",
        transform = "log-link"
      ))
    }
  }
  
  # B) Proportions/fractional outcomes in [0,1] -> quasi-binomial (logit)
  if (is_proportion_like(yv_all)) {
    return(list(
      chosen = "proportion_qb",
      model_type = "Quasi-binomial (logit link)",
      transform = "logit-link"
    ))
  }
  
  # C) Continuous outcomes: log-linear if strictly positive AND strongly skewed.
  # Enforce feasibility: if ANY group has non-positive y (among groups with n >= min_n),
  # then we cannot use log(y) uniformly -> fall back to Linear Gaussian.
  if (all(yv_all > 0, na.rm = TRUE) && is_strongly_skewed_positive(yv_all)) {
    
    log_feasible_all <- TRUE
    for (g in groups) {
      d_g <- d2_all %>% dplyr::filter(.data[[group_var]] == g)
      if (nrow(d_g) < min_n) next
      
      yv_g <- d_g[[y]]
      if (any(!is.finite(yv_g)) || any(yv_g <= 0, na.rm = TRUE)) {
        log_feasible_all <- FALSE
        break
      }
    }
    
    if (log_feasible_all) {
      return(list(
        chosen = "continuous_loglin",
        model_type = "Log-linear Gaussian (log y)",
        transform = "log(y)"
      ))
    }
  }
  
  # Default: linear Gaussian on original scale
  list(
    chosen = "continuous_lm",
    model_type = "Linear Gaussian",
    transform = "identity"
  )
}

# Fit the PRE-SELECTED model type within each group.
# (No more within-group model switching.)
fit_fixed_2var_model <- function(d, y, x, spec, min_n = 8) {
  
  d2 <- d %>%
    dplyr::select(all_of(c("PY", y, x))) %>%
    dplyr::filter(is.finite(.data[[y]]), is.finite(.data[[x]])) %>%
    dplyr::arrange(PY)
  
  n <- nrow(d2)
  if (n < min_n) {
    return(list(model = NULL, model_type = "Too few observations", n = n,
                y_used = y, x_used = x, transform = NA_character_))
  }
  
  fml <- bt_formula(y, x)
  
  # Count models
  if (identical(spec$chosen, "count_nb")) {
    mod_nb <- tryCatch(MASS::glm.nb(fml, data = d2), error = function(e) NULL)
    if (!is.null(mod_nb)) {
      return(list(model = mod_nb, model_type = "Negative binomial (log link)", n = n,
                  y_used = y, x_used = x, transform = "log-link"))
    }
    # If NB unexpectedly fails in a specific group, return NULL (keeps model-type uniform intention)
    return(list(model = NULL, model_type = "Negative binomial (log link) fit failed", n = n,
                y_used = y, x_used = x, transform = "log-link"))
  }
  
  if (identical(spec$chosen, "count_pois")) {
    mod_pois <- tryCatch(glm(fml, data = d2, family = poisson(link = "log")),
                         error = function(e) NULL)
    return(list(model = mod_pois, model_type = "Poisson (log link)", n = n,
                y_used = y, x_used = x, transform = "log-link"))
  }
  
  # Proportions
  if (identical(spec$chosen, "proportion_qb")) {
    mod_qb <- tryCatch(glm(fml, data = d2, family = quasibinomial(link = "logit")),
                       error = function(e) NULL)
    return(list(model = mod_qb, model_type = "Quasi-binomial (logit link)", n = n,
                y_used = y, x_used = x, transform = "logit-link"))
  }
  
  # Log-linear Gaussian (log y)
  if (identical(spec$chosen, "continuous_loglin")) {
    
    # Safety: enforce positivity (should already be enforced globally, but keep robust)
    if (any(d2[[y]] <= 0, na.rm = TRUE)) {
      return(list(model = NULL, model_type = "Log-linear Gaussian (log y) not feasible (non-positive y)", n = n,
                  y_used = y, x_used = x, transform = "log(y)"))
    }
    
    d2 <- d2 %>% dplyr::mutate(`.__y_log` = log(.data[[y]]))
    
    # Use a safe formula that backticks the predictor name (works with spaces/special chars)
    fml_log <- as.formula(paste0("`.__y_log` ~ `", x, "`"))
    
    mod_ll <- tryCatch(lm(fml_log, data = d2), error = function(e) NULL)
    return(list(model = mod_ll, model_type = "Log-linear Gaussian (log y)", n = n,
                y_used = y, x_used = x, transform = "log(y)"))
  }
  
  # Default: linear Gaussian
  mod_lm <- tryCatch(lm(fml, data = d2), error = function(e) NULL)
  return(list(model = mod_lm, model_type = "Linear Gaussian", n = n,
              y_used = y, x_used = x, transform = "identity"))
}


#####  5.3. Effect extraction: robust inference + interpretable units ######
extract_effect <- function(fit_obj, predictor_name) {
  
  mod <- fit_obj$model
  
  # If the model did not fit, return NA row but keep structure
  if (is.null(mod)) {
    return(tibble(
      estimate_raw = NA_real_, se_robust = NA_real_,
      ci_low_raw = NA_real_, ci_high_raw = NA_real_,
      p_value = NA_real_,
      effect_unit = NA_character_,
      estimate_interpretable = NA_real_,
      ci_low_interpretable = NA_real_,
      ci_high_interpretable = NA_real_,
      effect_ci_interpretable = NA_character_
    ))
  }
  
  # Robust covariance: HAC preferred (time-ordered PY); fallback to HC3
  V <- tryCatch(sandwich::vcovHAC(mod), error = function(e) NULL)
  if (is.null(V)) V <- tryCatch(sandwich::vcovHC(mod, type = "HC3"), error = function(e) NULL)
  
  if (is.null(V)) {
    return(tibble(
      estimate_raw = NA_real_, se_robust = NA_real_,
      ci_low_raw = NA_real_, ci_high_raw = NA_real_,
      p_value = NA_real_,
      effect_unit = NA_character_,
      estimate_interpretable = NA_real_,
      ci_low_interpretable = NA_real_,
      ci_high_interpretable = NA_real_,
      effect_ci_interpretable = NA_character_
    ))
  }
  
  ct <- tryCatch(lmtest::coeftest(mod, vcov. = V), error = function(e) NULL)
  if (is.null(ct)) {
    return(tibble(
      estimate_raw = NA_real_, se_robust = NA_real_,
      ci_low_raw = NA_real_, ci_high_raw = NA_real_,
      p_value = NA_real_,
      effect_unit = NA_character_,
      estimate_interpretable = NA_real_,
      ci_low_interpretable = NA_real_,
      ci_high_interpretable = NA_real_,
      effect_ci_interpretable = NA_character_
    ))
  }
  
  rn <- rownames(ct)
  
  # Prefer exact match with/without backticks
  idx <- which(rn == paste0("`", predictor_name, "`") | rn == predictor_name)
  
  # Fallback: take the first non-intercept term
  if (length(idx) == 0) idx <- which(rn != "(Intercept)")
  
  # Intercept-only / non-estimable model
  if (length(idx) == 0) {
    return(tibble(
      estimate_raw = NA_real_, se_robust = NA_real_,
      ci_low_raw = NA_real_, ci_high_raw = NA_real_,
      p_value = NA_real_,
      effect_unit = NA_character_,
      estimate_interpretable = NA_real_,
      ci_low_interpretable = NA_real_,
      ci_high_interpretable = NA_real_,
      effect_ci_interpretable = NA_character_
    ))
  }
  
  idx <- idx[1]
  
  est <- suppressWarnings(as.numeric(ct[idx, 1]))
  se  <- suppressWarnings(as.numeric(ct[idx, 2]))
  p   <- suppressWarnings(as.numeric(ct[idx, 4]))
  
  if (!is.finite(est) || !is.finite(se)) {
    return(tibble(
      estimate_raw = NA_real_, se_robust = NA_real_,
      ci_low_raw = NA_real_, ci_high_raw = NA_real_,
      p_value = NA_real_,
      effect_unit = NA_character_,
      estimate_interpretable = NA_real_,
      ci_low_interpretable = NA_real_,
      ci_high_interpretable = NA_real_,
      effect_ci_interpretable = NA_character_
    ))
  }
  
  # Wald CI on model (raw) scale
  z <- 1.96
  ci_low  <- est - z * se
  ci_high <- est + z * se
  
  # Convert to interpretable units depending on link/transform
  model_type <- fit_obj$model_type
  
  if (grepl("logit link", model_type, fixed = TRUE)) {
    unit <- "OR"
    est_i <- exp(est); lo_i <- exp(ci_low); hi_i <- exp(ci_high)
    txt <- sprintf("OR: %.3f [%.3f - %.3f]", est_i, lo_i, hi_i)
    
  } else if (grepl("Poisson", model_type, fixed = TRUE) ||
             grepl("Negative binomial", model_type, fixed = TRUE)) {
    unit <- "IRR"
    est_i <- exp(est); lo_i <- exp(ci_low); hi_i <- exp(ci_high)
    txt <- sprintf("IRR: %.3f [%.3f - %.3f]", est_i, lo_i, hi_i)
    
  } else if (grepl("Log-linear", model_type, fixed = TRUE)) {
    unit <- "Ratio"
    est_i <- exp(est); lo_i <- exp(ci_low); hi_i <- exp(ci_high)
    txt <- sprintf("Ratio: %.3f [%.3f - %.3f]", est_i, lo_i, hi_i)
    
  } else {
    unit <- "β"
    est_i <- est; lo_i <- ci_low; hi_i <- ci_high
    txt <- sprintf("β: %.3f [%.3f - %.3f]", est_i, lo_i, hi_i)
  }
  
  tibble(
    estimate_raw = est,
    se_robust = se,
    ci_low_raw = ci_low,
    ci_high_raw = ci_high,
    p_value = p,
    effect_unit = unit,
    estimate_interpretable = est_i,
    ci_low_interpretable = lo_i,
    ci_high_interpretable = hi_i,
    effect_ci_interpretable = txt
  )
}


#####  5.4. Generic runner: stratify by income OR region (same engine) ######

run_two_var_models_by_group <- function(data,
                                        group_var,
                                        indicator_roles,
                                        indicator_col_range = 9:136,
                                        pub_var = "total_publications",
                                        min_year = 2000,
                                        min_n = 8,
                                        verbose = TRUE) {
  
  stopifnot(group_var %in% names(data))
  stopifnot(pub_var %in% names(data))
  
  # Filter by year window
  data <- data %>% dplyr::filter(PY >= min_year)
  
  # Indicators present in the dataset (by position)
  indicator_vars <- colnames(data)[indicator_col_range]
  
  # Map: indicator -> role ("dependent" / "independent")
  roles_map <- setNames(indicator_roles$role, indicator_roles$variable)
  
  # Keep only indicators that exist in roles table
  indicator_vars_used <- intersect(indicator_vars, names(roles_map))
  
  if (verbose) {
    dropped <- setdiff(indicator_vars, indicator_vars_used)
    message("Using ", length(indicator_vars_used), " indicators (", length(dropped), " dropped due to missing roles).")
    if (length(dropped) > 0) message("First dropped examples: ", paste(head(dropped, 5), collapse = " | "))
  }
  
  # Drop missing stratum labels
  df0 <- data %>% dplyr::filter(!is.na(.data[[group_var]]))
  groups <- sort(unique(df0[[group_var]]))
  
  # Pre-compute ONE model specification per (y,x) using pooled data df0.
  # This spec is then reused for every group to ensure model-type consistency.
  spec_key <- function(y, x) paste0(y, "||", x)
  
  specs <- purrr::map(indicator_vars_used, function(ind_var) {
    
    role <- roles_map[[ind_var]]
    
    if (identical(role, "dependent")) {
      y <- ind_var
      x <- pub_var
    } else if (identical(role, "independent")) {
      y <- pub_var
      x <- ind_var
    } else {
      return(NULL)
    }
    
    spec <- choose_global_model_spec(
      df_all   = df0,
      group_var = group_var,
      groups   = groups,
      y        = y,
      x        = x,
      pub_var  = pub_var,
      min_n    = min_n
    )
    
    list(key = spec_key(y, x), y = y, x = x, spec = spec)
  })
  
  specs <- specs[!vapply(specs, is.null, logical(1))]
  
  spec_map <- setNames(lapply(specs, function(z) z$spec),
                       vapply(specs, function(z) z$key, character(1)))
  
  # Main loop: fit within each group using the pre-selected spec
  res <- purrr::map_dfr(groups, function(g) {
    
    d_g <- df0 %>% dplyr::filter(.data[[group_var]] == g)
    
    purrr::map_dfr(indicator_vars_used, function(ind_var) {
      
      role <- roles_map[[ind_var]]
      
      if (identical(role, "dependent")) {
        y <- ind_var
        x <- pub_var
      } else if (identical(role, "independent")) {
        y <- pub_var
        x <- ind_var
      } else {
        return(NULL)
      }
      
      spec <- spec_map[[spec_key(y, x)]]
      
      fit <- fit_fixed_2var_model(d_g, y = y, x = x, spec = spec, min_n = min_n)
      eff <- extract_effect(fit, predictor_name = x)
      
      tibble(
        group_name = as.character(g),
        dependent_var = y,
        independent_var = x,
        model_type = fit$model_type,  # will be consistent by design (spec is fixed)
        n_obs = fit$n
      ) %>% dplyr::bind_cols(eff)
    })
  }) %>%
    # Holm adjustment across ALL fitted models in this run (FWER control)
    dplyr::mutate(p_value_adj_holm = p.adjust(p_value, method = "holm"))
  
  res
}


#####  5.5. Run models: income ######

# Income-group models
results.regressions.income <- run_two_var_models_by_group(
  data = data_filtered_income,
  group_var = "income",
  indicator_roles = indicator_roles,
  indicator_col_range = 9:136,
  pub_var = "total_publications",
  min_year = 2000,
  min_n = 8,
  verbose = TRUE
) %>%
  dplyr::rename(income_group = group_name)






#*******************************************************************************
#### 6. HIERARCHICAL (MIXED-EFFECTS) REGRESSION MODELS ####
#*******************************************************************************

#####  1) Small utilities #####

# Backtick-safe formula builder for variables with spaces/special chars
bt <- function(x) paste0("`", x, "`")

bt_formula <- function(y, x, year_term = NULL, re_term = NULL) {
  rhs <- c(bt(x), year_term, re_term)
  rhs <- rhs[!is.na(rhs) & nzchar(rhs)]
  as.formula(paste(bt(y), "~", paste(rhs, collapse = " + ")))
}

# Decide if a variable is proportion-like (bounded [0,1])
is_proportion_like <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(FALSE)
  all(x >= 0 & x <= 1)
}

# Simple flag for strong right skew among positive values
is_strongly_skewed_positive <- function(x) {
  x <- x[is.finite(x)]
  x <- x[x > 0]
  if (length(x) < 10) return(FALSE)
  med <- stats::median(x)
  q95 <- stats::quantile(x, 0.95, names = FALSE)
  if (!is.finite(med) || med <= 0) return(FALSE)
  (q95 / med) > 10
}

# Beta regression requires 0<y<1; this is the standard Smithson–Verkuilen adjustment
# to move exact 0/1 into the open interval (0,1) without ad-hoc jittering.
beta_adjust_01 <- function(y) {
  n <- sum(is.finite(y))
  if (n <= 1) return(y)
  y <- (y * (n - 1) + 0.5) / n
  pmin(pmax(y, 1e-12), 1 - 1e-12)
}

# Heuristic: auto-scale predictor if it is very large (stability/convergence)
needs_scaling <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 5) return(FALSE)
  sx <- stats::sd(x)
  mx <- max(abs(x))
  is.finite(sx) && (sx > 1e3 || mx > 1e6)
}

# Robust p-values for lmer: prefer lmerTest if available; otherwise normal approximation
p_value_lmer <- function(model, term_name) {
  if (requireNamespace("lmerTest", quietly = TRUE)) {
    # lmerTest adds p-values in summary()
    sm <- summary(model)
    ct <- sm$coefficients
    if (term_name %in% rownames(ct) && "Pr(>|t|)" %in% colnames(ct)) {
      return(as.numeric(ct[term_name, "Pr(>|t|)"]))
    }
  }
  # Fallback: treat t as approx normal (reasonable with moderate n)
  sm <- summary(model)
  ct <- sm$coefficients
  if (!(term_name %in% rownames(ct))) return(NA_real_)
  tval <- as.numeric(ct[term_name, "t value"])
  2 * stats::pnorm(abs(tval), lower.tail = FALSE)
}


##### 2) Fit one mixed model (2 variables + year)     #####
# Panel structure: repeated measures by group across years
# Model includes:
#  - fixed effect: main predictor (x)
#  - fixed effect: centered year
#  - random intercept by group
#  - optional random slope for x by group (used when estimable)

fit_mixed_panel <- function(data,
                            y, x,
                            group_var,
                            year_var = "PY",
                            pub_var = "total_publications",
                            min_n = 20,
                            try_random_slope = TRUE) {
  
  stopifnot(group_var %in% names(data), year_var %in% names(data))
  stopifnot(y %in% names(data), x %in% names(data))
  
  # Keep only required cols; drop missing; sort by time
  d <- data %>%
    dplyr::select(all_of(c(group_var, year_var, y, x))) %>%
    filter(!is.na(.data[[group_var]]),
           is.finite(.data[[year_var]]),
           is.finite(.data[[y]]),
           is.finite(.data[[x]])) %>%
    arrange(.data[[group_var]], .data[[year_var]])
  
  n <- nrow(d)
  n_groups <- dplyr::n_distinct(d[[group_var]])
  if (n < min_n || n_groups < 2) {
    return(list(
      model = NULL,
      model_type = "Too few observations / groups",
      family = NA_character_,
      link = NA_character_,
      n = n,
      n_groups = n_groups,
      random_structure = NA_character_,
      x_used = x,
      y_used = y,
      x_scaled = FALSE,
      x_scale_sd = NA_real_,
      converged = NA,
      warning_msg = NA_character_
    ))
  }
  
  # Center year (controls time trend while keeping interpretability)
  d <- d %>% mutate(.__year_c = .data[[year_var]] - mean(.data[[year_var]], na.rm = TRUE))
  
  # Optionally scale x if extremely large (stability)
  x_scaled <- FALSE
  x_scale_sd <- NA_real_
  x_term <- x
  if (needs_scaling(d[[x]])) {
    x_scaled <- TRUE
    x_scale_sd <- stats::sd(d[[x]], na.rm = TRUE)
    d <- d %>% mutate(.__x_scaled = as.numeric(scale(.data[[x]])))
    x_term <- ".__x_scaled"
  }
  
  # Random effects structure
  # Prefer random slope if it is plausible; fallback to random intercept if it fails.
  re_int  <- paste0("(1 | ", group_var, ")")
  re_slope <- paste0("(1 + ", bt(x_term), " | ", group_var, ")")
  
  use_slope <- FALSE
  if (try_random_slope) {
    # need enough info per group to estimate slope reasonably
    min_per_group <- min(table(d[[group_var]]))
    use_slope <- (n_groups >= 3 && min_per_group >= 5)
  }
  re_term <- if (use_slope) re_slope else re_int
  random_structure <- if (use_slope) "(1 + x | group)" else "(1 | group)"
  
  # Decide distribution by the OUTCOME scale
  yv <- d[[y]]
  
  # Case 1: outcome is publications => count model (NB preferred)
  if (identical(y, pub_var)) {
    # Formula: y ~ x + year + (RE)
    fml <- bt_formula(y = y, x = x_term, year_term = ".__year_c", re_term = re_term)
    
    warn <- character(0)
    mod <- withCallingHandlers(
      tryCatch(
        glmmTMB::glmmTMB(fml, data = d, family = glmmTMB::nbinom2(link = "log")),
        error = function(e) NULL
      ),
      warning = function(w) { warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning") }
    )
    
    # Fallback to Poisson if NB fails
    fam <- "nbinom2"
    if (is.null(mod)) {
      mod <- withCallingHandlers(
        tryCatch(
          glmmTMB::glmmTMB(fml, data = d, family = poisson(link = "log")),
          error = function(e) NULL
        ),
        warning = function(w) { warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning") }
      )
      fam <- "poisson"
    }
    
    conv <- if (!is.null(mod)) isTRUE(mod$sdr$pdHess) else NA
    
    return(list(
      model = mod,
      model_type = paste0("GLMM (", fam, ", log link)"),
      family = fam,
      link = "log",
      n = n,
      n_groups = n_groups,
      random_structure = random_structure,
      x_used = x,
      y_used = y,
      x_scaled = x_scaled,
      x_scale_sd = x_scale_sd,
      converged = conv,
      warning_msg = if (length(warn)) paste(unique(warn), collapse = " | ") else NA_character_
    ))
  }
  
  # Case 2: outcome is an indicator
  # 2a) Proportion-like => Beta regression with logit link (after boundary adjustment)
  if (is_proportion_like(yv)) {
    d[[y]] <- beta_adjust_01(d[[y]])
    fml <- bt_formula(y = y, x = x_term, year_term = ".__year_c", re_term = re_term)
    
    warn <- character(0)
    mod <- withCallingHandlers(
      tryCatch(
        glmmTMB::glmmTMB(fml, data = d, family = glmmTMB::beta_family(link = "logit")),
        error = function(e) NULL
      ),
      warning = function(w) { warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning") }
    )
    
    conv <- if (!is.null(mod)) isTRUE(mod$sdr$pdHess) else NA
    
    return(list(
      model = mod,
      model_type = "GLMM (beta, logit link)",
      family = "beta",
      link = "logit",
      n = n,
      n_groups = n_groups,
      random_structure = random_structure,
      x_used = x,
      y_used = y,
      x_scaled = x_scaled,
      x_scale_sd = x_scale_sd,
      converged = conv,
      warning_msg = if (length(warn)) paste(unique(warn), collapse = " | ") else NA_character_
    ))
  }
  
  # 2b) Positive & strongly skewed => log-Gaussian LMM
  if (all(yv > 0, na.rm = TRUE) && is_strongly_skewed_positive(yv)) {
    d <- d %>% mutate(.__y_log = log(.data[[y]]))
    fml <- bt_formula(y = ".__y_log", x = x_term, year_term = ".__year_c", re_term = re_term)
    
    warn <- character(0)
    mod <- withCallingHandlers(
      tryCatch(
        lme4::lmer(fml, data = d, REML = FALSE, control = lmerControl(optimizer = "bobyqa")),
        error = function(e) NULL
      ),
      warning = function(w) { warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning") }
    )
    
    return(list(
      model = mod,
      model_type = "LMM (log y, Gaussian)",
      family = "gaussian",
      link = "log(y)",
      n = n,
      n_groups = n_groups,
      random_structure = random_structure,
      x_used = x,
      y_used = y,
      x_scaled = x_scaled,
      x_scale_sd = x_scale_sd,
      converged = !is.null(mod),
      warning_msg = if (length(warn)) paste(unique(warn), collapse = " | ") else NA_character_
    ))
  }
  
  # 2c) Otherwise => Gaussian LMM on original scale
  fml <- bt_formula(y = y, x = x_term, year_term = ".__year_c", re_term = re_term)
  
  warn <- character(0)
  mod <- withCallingHandlers(
    tryCatch(
      lme4::lmer(fml, data = d, REML = FALSE, control = lmerControl(optimizer = "bobyqa")),
      error = function(e) NULL
    ),
    warning = function(w) { warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  
  return(list(
    model = mod,
    model_type = "LMM (Gaussian)",
    family = "gaussian",
    link = "identity",
    n = n,
    n_groups = n_groups,
    random_structure = random_structure,
    x_used = x,
    y_used = y,
    x_scaled = x_scaled,
    x_scale_sd = x_scale_sd,
    converged = !is.null(mod),
    warning_msg = if (length(warn)) paste(unique(warn), collapse = " | ") else NA_character_
  ))
}


##### 3) Extract the main fixed effect (x) from the model  #####
extract_main_effect <- function(fit_obj) {
  
  mod <- fit_obj$model
  if (is.null(mod)) {
    return(tibble(
      estimate_raw = NA_real_, se = NA_real_,
      ci_low_raw = NA_real_, ci_high_raw = NA_real_,
      p_value = NA_real_,
      effect_unit = NA_character_,
      estimate_interpretable = NA_real_,
      ci_low_interpretable = NA_real_,
      ci_high_interpretable = NA_real_,
      effect_ci_interpretable = NA_character_
    ))
  }
  
  # Identify the coefficient name used in the model:
  # - if scaled: .__x_scaled
  # - else: original predictor name (with spaces handled by backticks in formula)
  term_name <- if (isTRUE(fit_obj$x_scaled)) ".__x_scaled" else fit_obj$x_used
  
  # Pull coefficient table
  if (inherits(mod, "glmmTMB")) {
    sm <- summary(mod)
    ct <- sm$coefficients$cond
    if (!(term_name %in% rownames(ct))) {
      # fallback: first non-intercept
      term_name <- setdiff(rownames(ct), "(Intercept)")[1]
    }
    est <- as.numeric(ct[term_name, "Estimate"])
    se  <- as.numeric(ct[term_name, "Std. Error"])
    p   <- as.numeric(ct[term_name, "Pr(>|z|)"])
    
  } else {
    sm <- summary(mod)
    ct <- sm$coefficients
    if (!(term_name %in% rownames(ct))) {
      # fallback: first non-intercept
      term_name <- setdiff(rownames(ct), "(Intercept)")[1]
    }
    est <- as.numeric(ct[term_name, "Estimate"])
    se  <- as.numeric(ct[term_name, "Std. Error"])
    p   <- p_value_lmer(mod, term_name)
  }
  
  if (!is.finite(est) || !is.finite(se)) {
    return(tibble(
      estimate_raw = NA_real_, se = NA_real_,
      ci_low_raw = NA_real_, ci_high_raw = NA_real_,
      p_value = NA_real_,
      effect_unit = NA_character_,
      estimate_interpretable = NA_real_,
      ci_low_interpretable = NA_real_,
      ci_high_interpretable = NA_real_,
      effect_ci_interpretable = NA_character_
    ))
  }
  
  # Wald CI on raw (link) scale
  z <- 1.96
  ci_low  <- est - z * se
  ci_high <- est + z * se
  
  # Map to interpretable units
  # - log link => IRR
  # - logit link => OR
  # - log(y) gaussian => Ratio (multiplicative change)
  # - identity gaussian => beta (β)
  unit <- "β"
  est_i <- est; lo_i <- ci_low; hi_i <- ci_high
  
  if (identical(fit_obj$link, "log")) {
    unit <- "IRR"
    est_i <- exp(est); lo_i <- exp(ci_low); hi_i <- exp(ci_high)
  } else if (identical(fit_obj$link, "logit")) {
    unit <- "OR"
    est_i <- exp(est); lo_i <- exp(ci_low); hi_i <- exp(ci_high)
  } else if (identical(fit_obj$link, "log(y)")) {
    unit <- "Ratio"
    est_i <- exp(est); lo_i <- exp(ci_low); hi_i <- exp(ci_high)
  }
  
  txt <- sprintf("%s: %.3f [%.3f - %.3f]", unit, est_i, lo_i, hi_i)
  
  tibble(
    estimate_raw = est,
    se = se,
    ci_low_raw = ci_low,
    ci_high_raw = ci_high,
    p_value = p,
    effect_unit = unit,
    estimate_interpretable = est_i,
    ci_low_interpretable = lo_i,
    ci_high_interpretable = hi_i,
    effect_ci_interpretable = txt
  )
}


##### 4) Runner: fit ALL indicator models for one dataset        #####
run_hierarchical_models <- function(data,
                                    group_var,
                                    indicator_roles,
                                    pub_var = "total_publications",
                                    year_var = "PY",
                                    min_year = 2000,
                                    min_n = 20,
                                    try_random_slope = TRUE) {
  
  stopifnot(group_var %in% names(data), year_var %in% names(data))
  
  # Panel restriction: years >= 2000 (as you requested earlier)
  data <- data %>% filter(.data[[year_var]] >= min_year)
  
  # Role map: indicator name -> role
  roles_map <- setNames(indicator_roles$role, indicator_roles$variable)
  
  # Robust indicator set: everything except the known non-indicator columns
  non_indicators <- c(group_var, year_var, pub_var, "citations", "Q1", "Q2", "Q3", "Q4")
  candidate_indicators <- setdiff(names(data), non_indicators)
  
  # Keep only those that have a role defined
  indicator_vars <- intersect(candidate_indicators, names(roles_map))
  
  # Fit one model per indicator
  res <- purrr::map_dfr(indicator_vars, function(ind_var) {
    
    role <- roles_map[[ind_var]]
    
    # Directionality rule (same as your 2-variable approach):
    # - If indicator is "dependent": indicator ~ publications
    # - If indicator is "independent": publications ~ indicator
    if (identical(role, "dependent")) {
      y <- ind_var
      x <- pub_var
    } else if (identical(role, "independent")) {
      y <- pub_var
      x <- ind_var
    } else {
      return(NULL)
    }
    
    fit <- fit_mixed_panel(
      data = data,
      y = y, x = x,
      group_var = group_var,
      year_var = year_var,
      pub_var = pub_var,
      min_n = min_n,
      try_random_slope = try_random_slope
    )
    
    eff <- extract_main_effect(fit)
    
    tibble(
      group_var = group_var,
      dependent_var = y,
      independent_var = x,
      role_of_indicator = role,
      model_type = fit$model_type,
      family = fit$family,
      link = fit$link,
      random_structure = fit$random_structure,
      converged = fit$converged,
      warning_msg = fit$warning_msg,
      n_obs = fit$n,
      n_groups = fit$n_groups,
      predictor_scaled = fit$x_scaled,
      predictor_scale_sd = fit$x_scale_sd
    ) %>%
      bind_cols(eff) %>%
      mutate(
        # Make explicit what a "1 unit" means if we scaled
        effect_per = ifelse(isTRUE(predictor_scaled), "1 SD increase in predictor", "1 unit increase in predictor")
      )
  })
  
  # Multiple-testing adjustment across ALL models in this table
  res %>%
    mutate(
      p_adj_fdr_bh = p.adjust(p_value, method = "BH"),
      p_adj_holm   = p.adjust(p_value, method = "holm")
    )
}


##### 5) Run for INCOME  #####

#results.hierarchical.income = read_excel("~/Desktop/food-security/data/income/Supplementary Material 2.xlsx")

results.hierarchical.income <- run_hierarchical_models(
  data = data_filtered_income,
  group_var = "income",
  indicator_roles = indicator_roles,
  pub_var = "total_publications",
  year_var = "PY",
  min_year = 2000,
  min_n = 20,
  try_random_slope = TRUE
) %>%
  rename(income_group = group_var)






#*******************************************************************************
#### 7. MIXED-EFFECTS MODERATOR SCREENING (CLUSTER-RESTRICTED MODERATORS) ####
#*******************************************************************************

# Goal: For each base association (publications ↔ indicator),
#       test ONLY the moderators listed in indicator_clusters (1-at-a-time)
#       via the interaction X:Z in a mixed-effects model with random intercept
#       for income/region and fixed time (PY, centered).
#
# Outputs:
#   - results.moderator_screening.income
#   - results.moderator_screening.region
#
# Notes:
#   - Years restricted to PY >= 2000
#   - Moderator model: y ~ x + z + x:z + PY + (1|group)
#   - Outcome family chosen automatically (Poisson / NegBin / Beta / Gaussian / log-Gaussian)
#   - Multiple testing: BH/FDR across ALL interaction tests per output table

# Optional but recommended for p-values in Gaussian mixed models:
has_lmerTest <- requireNamespace("lmerTest", quietly = TRUE)


#####  0) Moderator clusters input #####

# Your list as provided (keep as-is)
indicator_clusters <- list(
  health_system_and_financing = c(
    "CURRENT HEALTH EXPENDITURE (% OF GDP)",
    "PHYSICIANS (PER 1,000 PEOPLE)",
    "NURSES AND MIDWIVES (PER 1,000 PEOPLE)",
    "HEALTHCARE ACCESS AND QUALITY",
    "OUT-OF-POCKED EXPENDITURE ON HEALTH"
  ),
  health_outcomes_and_demography = c(
    "NUMBER OF DALYS",
    "NUMBER OF DEATHS",
    "DEATH RATE",
    "Population,ages 65+",
    "Child mortality rate",
    "Life expectancy at birth",
    "Healthy life expectancy",
    "Lifespan Inequality in wome",
    "Lifespan Inequality in man",
    "Sex ratio",
    "Sex gap in life expectancy"
  ),
  socioeconomic_poverty_education = c(
    "Gini Coefficient",
    "Multidimensional Poverty Index",
    "Number of people living in extreme poverty",
    "GDP per capita",
    "Share of population with no formal education",
    "Average years of schooling",
    "Elderly Literacy Rate",
    "Youth Literacy Rate",
    "Human Development Index",
    "Income inequality: Atkinson index"
  ),
  governance_and_institutions = c(
    "Human rights index",
    "Percentage of territory effectively controlled by government",
    "Rigorous and impartial public administration index",
    "State capacity index",
    "Functioning government index",
    "Political corruption index",
    "Corruption Perception Index"
  ),
  research_and_innovation_system = c(
    "RESEARCH AND DEVELOPMENT EXPENDITURE (% OF GDP)",
    "CHARGES FOR THE USE OF INTELLECTUAL PROPERTY, PAYMENTS (BOP, CURRENT US$)",
    "higher education institutions offering disciplines related to research for health in 2023 by region",
    "higher education institutions offering disciplines related to research for health in 2023 by income",
    "HEALTH RESEARCHERS (IN FULL-TIME EQUIVALENT) PER MILLION INHABITANTS, BY WHO REGION",
    "HEALTH RESEARCHERS (IN FULL-TIME EQUIVALENT) PER MILLION INHABITANTS, BY INCOME GROUP",
    "HEALTH RESEARCHERS (IN FULL-TIME EQUIVALENT) PER MILLION INHABITANTS, BY COUNTRY"
  ),
  health_aid_and_external_finance = c(
    "OFFICIAL DEVELOPMENT ASSISTANCE (ODA) FOR MEDICAL RESEARCH AND BASIC HEALTH SECTORS PER CAPITA, BY REGION",
    "OFFICIAL DEVELOPMENT ASSISTANCE (ODA) FOR MEDICAL RESEARCH AND BASIC HEALTH SECTORS PER CAPITA, BY INCOME",
    "OFFICIAL DEVELOPMENT ASSISTANCE (ODA) FOR MEDICAL RESEARCH AND BASIC HEALTH SECTORS PER CAPITA, BY RECIPIENT COUNTRY"
  )
)

# Flatten moderator list (unique) while keeping cluster labels available later if needed
moderators_from_clusters <- unique(unlist(indicator_clusters, use.names = FALSE))


##### 1) Helper functions         #####

is_count_like <- function(x, tol = 1e-8) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(FALSE)
  all(x >= 0) && all(abs(x - round(x)) < tol)
}

is_proportion_like <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(FALSE)
  all(x >= 0 & x <= 1)
}

is_strongly_skewed_positive <- function(x) {
  x <- x[is.finite(x)]
  x <- x[x > 0]
  if (length(x) < 10) return(FALSE)
  med <- stats::median(x)
  q95 <- stats::quantile(x, 0.95, names = FALSE)
  if (med <= 0) return(FALSE)
  (q95 / med) > 10
}

squeeze_to_open_unit <- function(y) {
  y <- as.numeric(y)
  ok <- is.finite(y)
  y2 <- y
  n <- sum(ok)
  if (n <= 1) return(y2)
  y2[ok] <- (y[ok] * (n - 1) + 0.5) / n
  y2[ok] <- pmin(pmax(y2[ok], 1e-12), 1 - 1e-12)
  y2
}

choose_outcome_model <- function(y_vec) {
  yv <- y_vec[is.finite(y_vec)]
  if (length(yv) < 8) return(list(kind = "fail"))

  if (is_count_like(yv)) {
    mu <- mean(yv)
    va <- stats::var(yv)
    disp <- ifelse(mu > 0, va / mu, Inf)
    if (is.finite(disp) && disp > 1.5) {
      return(list(kind = "count", family = "nbinom2", link = "log", transform = "none"))
    } else {
      return(list(kind = "count", family = "poisson", link = "log", transform = "none"))
    }
  }

  if (is_proportion_like(yv)) {
    return(list(kind = "prop", family = "beta", link = "logit", transform = "squeeze_0_1"))
  }

  if (all(yv > 0) && is_strongly_skewed_positive(yv)) {
    return(list(kind = "cont", family = "gaussian", link = "identity", transform = "log_y"))
  }

  return(list(kind = "cont", family = "gaussian", link = "identity", transform = "none"))
}

get_fixed_table <- function(mod) {
  if (inherits(mod, "glmmTMB")) {
    tab <- summary(mod)$coefficients$cond
    return(tibble(
      term = rownames(tab),
      estimate = tab[, "Estimate"],
      std_error = tab[, "Std. Error"],
      statistic = tab[, "z value"],
      p_value = tab[, "Pr(>|z|)"]
    ))
  }

  if (inherits(mod, "merMod")) {
    if (has_lmerTest && inherits(mod, "lmerModLmerTest")) {
      tab <- summary(mod)$coefficients
      return(tibble(
        term = rownames(tab),
        estimate = tab[, "Estimate"],
        std_error = tab[, "Std. Error"],
        statistic = tab[, "t value"],
        p_value = tab[, "Pr(>|t|)"]
      ))
    } else {
      tab <- summary(mod)$coefficients
      z <- tab[, "Estimate"] / tab[, "Std. Error"]
      p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
      return(tibble(
        term = rownames(tab),
        estimate = tab[, "Estimate"],
        std_error = tab[, "Std. Error"],
        statistic = z,
        p_value = p
      ))
    }
  }

  NULL
}

get_vcov_fixed <- function(mod) {
  V <- tryCatch(stats::vcov(mod), error = function(e) NULL)
  if (is.null(V)) return(NULL)
  if (is.list(V) && !is.null(V$cond)) return(V$cond)
  V
}

compute_simple_slope <- function(bx, bix, zc, V, term_x, term_xz) {
  est <- bx + bix * zc
  se <- NA_real_
  lo <- NA_real_
  hi <- NA_real_

  if (!is.null(V) &&
      term_x %in% colnames(V) && term_x %in% rownames(V) &&
      term_xz %in% colnames(V) && term_xz %in% rownames(V)) {

    v_x  <- V[term_x, term_x]
    v_ix <- V[term_xz, term_xz]
    c_x  <- V[term_x, term_xz]

    var_s <- v_x + (zc^2) * v_ix + 2 * zc * c_x
    if (is.finite(var_s) && var_s >= 0) {
      se <- sqrt(var_s)
      lo <- est - 1.96 * se
      hi <- est + 1.96 * se
    }
  }

  list(est = est, se = se, lo = lo, hi = hi)
}

map_to_interpretable <- function(model_kind, model_family, transform, est, lo, hi) {
  if (model_kind == "count" && model_family %in% c("poisson", "nbinom2")) {
    return(list(unit = "IRR", est = exp(est), lo = exp(lo), hi = exp(hi)))
  }
  if (model_kind == "prop" && identical(model_family, "beta")) {
    return(list(unit = "OR", est = exp(est), lo = exp(lo), hi = exp(hi)))
  }
  if (model_kind == "cont" && identical(transform, "log_y")) {
    return(list(unit = "Ratio", est = exp(est), lo = exp(lo), hi = exp(hi)))
  }
  list(unit = "β", est = est, lo = lo, hi = hi)
}

fit_mixed_moderation <- function(data, y_name, x_name, z_name,
                                 group_var,
                                 year_var = "PY",
                                 min_n = 25) {

  d <- data %>%
    dplyr::select(all_of(c(group_var, year_var, y_name, x_name, z_name))) %>%
    rename(
      .group = all_of(group_var),
      .year  = all_of(year_var),
      .y_raw = all_of(y_name),
      .x_raw = all_of(x_name),
      .z_raw = all_of(z_name)
    ) %>%
    filter(is.finite(.year), is.finite(.y_raw), is.finite(.x_raw), is.finite(.z_raw)) %>%
    mutate(
      .group  = factor(.group),
      .year_c = .year - mean(.year, na.rm = TRUE),
      .x_c    = .x_raw - mean(.x_raw, na.rm = TRUE),
      .z_c    = .z_raw - mean(.z_raw, na.rm = TRUE)
    )

  n <- nrow(d)
  n_groups <- nlevels(d$.group)
  if (n < min_n || n_groups < 2) {
    return(list(model = NULL, info = list(reason = "Too few observations/groups", n = n, n_groups = n_groups)))
  }
  if (stats::sd(d$.z_c, na.rm = TRUE) == 0 || stats::sd(d$.x_c, na.rm = TRUE) == 0) {
    return(list(model = NULL, info = list(reason = "Zero variance in X or Z", n = n, n_groups = n_groups)))
  }

  spec <- choose_outcome_model(d$.y_raw)
  if (identical(spec$kind, "fail")) {
    return(list(model = NULL, info = list(reason = "Outcome type detection failed", n = n, n_groups = n_groups)))
  }

  d <- d %>% mutate(.y = .y_raw)
  if (identical(spec$transform, "squeeze_0_1")) {
    d$.y <- squeeze_to_open_unit(d$.y_raw)
  } else if (identical(spec$transform, "log_y")) {
    if (any(d$.y_raw <= 0, na.rm = TRUE)) {
      return(list(model = NULL, info = list(reason = "Nonpositive values for log(y)", n = n, n_groups = n_groups)))
    }
    d$.y <- log(d$.y_raw)
  }

  fml <- .y ~ .x_c + .z_c + .x_c:.z_c + .year_c + (1 | .group)

  mod <- tryCatch({
    if (identical(spec$family, "poisson")) {
      glmmTMB::glmmTMB(fml, data = d, family = poisson(link = "log"))
    } else if (identical(spec$family, "nbinom2")) {
      glmmTMB::glmmTMB(fml, data = d, family = glmmTMB::nbinom2(link = "log"))
    } else if (identical(spec$family, "beta")) {
      glmmTMB::glmmTMB(fml, data = d, family = glmmTMB::beta_family(link = "logit"))
    } else {
      if (has_lmerTest) {
        lmerTest::lmer(fml, data = d, control = lme4::lmerControl(optimizer = "bobyqa"))
      } else {
        lme4::lmer(fml, data = d, control = lme4::lmerControl(optimizer = "bobyqa"))
      }
    }
  }, error = function(e) NULL)

  if (is.null(mod)) {
    return(list(model = NULL, info = list(reason = "Model fit error", n = n, n_groups = n_groups)))
  }

  list(model = mod, data_used = d, spec = spec, info = list(n = n, n_groups = n_groups))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

extract_moderation_row <- function(fit, y_name, x_name, z_name, group_var_label, z_cluster = NA_character_) {
  if (is.null(fit$model)) {
    return(tibble(
      group_type = group_var_label,
      outcome = y_name,
      predictor_x = x_name,
      moderator_z = z_name,
      moderator_cluster = z_cluster,
      model_class = NA_character_,
      family = NA_character_,
      link = NA_character_,
      outcome_transform = NA_character_,
      n_obs = fit$info$n %||% NA_integer_,
      n_groups = fit$info$n_groups %||% NA_integer_,
      term = NA_character_,
      estimate_raw = NA_real_,
      se = NA_real_,
      ci_low_raw = NA_real_,
      ci_high_raw = NA_real_,
      p_value = NA_real_,
      effect_unit = NA_character_,
      estimate_interpretable = NA_real_,
      ci_low_interpretable = NA_real_,
      ci_high_interpretable = NA_real_,
      effect_ci_interpretable = NA_character_,
      z_p25 = NA_real_, z_p75 = NA_real_,
      x_slope_p25_raw = NA_real_, x_slope_p25_se = NA_real_,
      x_slope_p25_ci_low_raw = NA_real_, x_slope_p25_ci_high_raw = NA_real_,
      x_slope_p25_interpretable = NA_real_,
      x_slope_p75_raw = NA_real_, x_slope_p75_se = NA_real_,
      x_slope_p75_ci_low_raw = NA_real_, x_slope_p75_ci_high_raw = NA_real_,
      x_slope_p75_interpretable = NA_real_,
      fit_note = fit$info$reason %||% NA_character_
    ))
  }

  mod <- fit$model
  spec <- fit$spec
  d <- fit$data_used

  tab <- get_fixed_table(mod)
  V   <- get_vcov_fixed(mod)

  term_x  <- ".x_c"
  term_xz <- ".x_c:.z_c"

  row_xz <- tab %>% filter(term == term_xz)
  if (nrow(row_xz) == 0) {
    return(tibble(
      group_type = group_var_label,
      outcome = y_name,
      predictor_x = x_name,
      moderator_z = z_name,
      moderator_cluster = z_cluster,
      model_class = class(mod)[1],
      family = spec$family,
      link = spec$link,
      outcome_transform = spec$transform,
      n_obs = fit$info$n,
      n_groups = fit$info$n_groups,
      term = term_xz,
      estimate_raw = NA_real_,
      se = NA_real_,
      ci_low_raw = NA_real_,
      ci_high_raw = NA_real_,
      p_value = NA_real_,
      effect_unit = NA_character_,
      estimate_interpretable = NA_real_,
      ci_low_interpretable = NA_real_,
      ci_high_interpretable = NA_real_,
      effect_ci_interpretable = NA_character_,
      z_p25 = NA_real_, z_p75 = NA_real_,
      x_slope_p25_raw = NA_real_, x_slope_p25_se = NA_real_,
      x_slope_p25_ci_low_raw = NA_real_, x_slope_p25_ci_high_raw = NA_real_,
      x_slope_p25_interpretable = NA_real_,
      x_slope_p75_raw = NA_real_, x_slope_p75_se = NA_real_,
      x_slope_p75_ci_low_raw = NA_real_, x_slope_p75_ci_high_raw = NA_real_,
      x_slope_p75_interpretable = NA_real_,
      fit_note = "Interaction term not found"
    ))
  }

  est <- as.numeric(row_xz$estimate)
  se  <- as.numeric(row_xz$std_error)
  p   <- as.numeric(row_xz$p_value)
  ci_lo <- est - 1.96 * se
  ci_hi <- est + 1.96 * se

  mapped <- map_to_interpretable(spec$kind, spec$family, spec$transform, est, ci_lo, ci_hi)
  txt <- sprintf("%s: %.4f [%.4f - %.4f]", mapped$unit, mapped$est, mapped$lo, mapped$hi)

  z_p25 <- stats::quantile(d$.z_raw, 0.25, names = FALSE, na.rm = TRUE)
  z_p75 <- stats::quantile(d$.z_raw, 0.75, names = FALSE, na.rm = TRUE)
  zc_p25 <- z_p25 - mean(d$.z_raw, na.rm = TRUE)
  zc_p75 <- z_p75 - mean(d$.z_raw, na.rm = TRUE)

  row_x <- tab %>% filter(term == term_x)
  bx <- if (nrow(row_x) > 0) as.numeric(row_x$estimate) else NA_real_
  bix <- est

  slope25 <- compute_simple_slope(bx, bix, zc_p25, V, term_x, term_xz)
  slope75 <- compute_simple_slope(bx, bix, zc_p75, V, term_x, term_xz)

  slope25_m <- map_to_interpretable(spec$kind, spec$family, spec$transform, slope25$est, slope25$lo, slope25$hi)
  slope75_m <- map_to_interpretable(spec$kind, spec$family, spec$transform, slope75$est, slope75$lo, slope75$hi)

  tibble(
    group_type = group_var_label,
    outcome = y_name,
    predictor_x = x_name,
    moderator_z = z_name,
    moderator_cluster = z_cluster,
    model_class = class(mod)[1],
    family = spec$family,
    link = spec$link,
    outcome_transform = spec$transform,
    n_obs = fit$info$n,
    n_groups = fit$info$n_groups,
    term = term_xz,
    estimate_raw = est,
    se = se,
    ci_low_raw = ci_lo,
    ci_high_raw = ci_hi,
    p_value = p,
    effect_unit = mapped$unit,
    estimate_interpretable = mapped$est,
    ci_low_interpretable = mapped$lo,
    ci_high_interpretable = mapped$hi,
    effect_ci_interpretable = txt,
    z_p25 = z_p25,
    z_p75 = z_p75,
    x_slope_p25_raw = slope25$est,
    x_slope_p25_se = slope25$se,
    x_slope_p25_ci_low_raw = slope25$lo,
    x_slope_p25_ci_high_raw = slope25$hi,
    x_slope_p25_interpretable = slope25_m$est,
    x_slope_p75_raw = slope75$est,
    x_slope_p75_se = slope75$se,
    x_slope_p75_ci_low_raw = slope75$lo,
    x_slope_p75_ci_high_raw = slope75$hi,
    x_slope_p75_interpretable = slope75_m$est,
    fit_note = NA_character_
  )
}

# Build a named lookup: moderator -> cluster_name
make_cluster_lookup <- function(indicator_clusters) {
  tibble(
    moderator = unlist(indicator_clusters, use.names = FALSE),
    cluster = rep(names(indicator_clusters), lengths(indicator_clusters))
  ) %>%
    distinct(moderator, .keep_all = TRUE)
}


##### 2) Main runner (modified)   #####

run_moderator_screening_mixed <- function(data,
                                          group_var,                 # "income" or "region"
                                          indicator_roles,           # tibble: variable, role
                                          indicator_clusters,        # list of allowed moderators
                                          indicator_col_range = 9:136,
                                          pub_var = "total_publications",
                                          year_var = "PY",
                                          min_year = 2000,
                                          min_n = 25) {

  stopifnot(group_var %in% names(data))
  stopifnot(pub_var %in% names(data))
  stopifnot(year_var %in% names(data))

  # Enforce PY >= 2000 by default
  d0 <- data %>%
    filter(.data[[year_var]] >= min_year) %>%
    filter(!is.na(.data[[group_var]]))

  # Indicators available in dataset (by column range)
  all_indicators <- colnames(d0)[indicator_col_range]

  # Roles map from your indicator_roles
  roles_map <- setNames(indicator_roles$role, indicator_roles$variable)

  # Base indicators = those both in dataset AND in roles_map
  indicators <- all_indicators[all_indicators %in% names(roles_map)]

  # Allowed moderators = union of clusters, BUT only if they exist in dataset
  moderators_allowed <- unique(unlist(indicator_clusters, use.names = FALSE))
  moderators_allowed <- moderators_allowed[moderators_allowed %in% colnames(d0)]

  # Cluster label lookup (for reporting)
  clu_lookup <- make_cluster_lookup(indicator_clusters)

  # Build base pairs (direction depends on role of indicator)
  base_pairs <- purrr::map(indicators, function(ind) {
    role <- roles_map[[ind]]
    if (identical(role, "dependent")) {
      list(y = ind, x = pub_var)
    } else if (identical(role, "independent")) {
      list(y = pub_var, x = ind)
    } else {
      NULL
    }
  }) %>% purrr::compact()

  # Run: each base pair × each allowed moderator (excluding X/Y)
  res <- purrr::map_dfr(base_pairs, function(bp) {
    y <- bp$y
    x <- bp$x

    z_list <- setdiff(moderators_allowed, c(y, x))

    purrr::map_dfr(z_list, function(z) {

      z_cluster <- clu_lookup$cluster[match(z, clu_lookup$moderator)]
      if (length(z_cluster) == 0 || is.na(z_cluster)) z_cluster <- NA_character_

      fit <- fit_mixed_moderation(
        data = d0,
        y_name = y, x_name = x, z_name = z,
        group_var = group_var,
        year_var = year_var,
        min_n = min_n
      )

      extract_moderation_row(
        fit = fit,
        y_name = y, x_name = x, z_name = z,
        group_var_label = group_var,
        z_cluster = z_cluster
      )
    })
  })

  # Multiple testing correction (screening): BH/FDR across ALL interaction tests
  res %>%
    mutate(
      p_value_fdr_bh = p.adjust(p_value, method = "BH")
    )
}


##### 3) Run for INCOME #####

IND_RANGE <- 9:136

#results.moderator_screening.income = read_excel("~/Desktop/food-security/data/income/Supplementary Material 3.xlsx")

results.moderator_screening.income <- run_moderator_screening_mixed(
  data = data_filtered_income,
  group_var = "income",
  indicator_roles = indicator_roles,
  indicator_clusters = indicator_clusters,
  indicator_col_range = IND_RANGE,
  pub_var = "total_publications",
  year_var = "PY",
  min_year = 2000,
  min_n = 25
) %>%
  rename(income_group = group_type)


##### 4) Optional quick checks    #####

summary_income <- results.moderator_screening.income %>%
  summarise(
    n_tests = n(),
    n_failed = sum(!is.na(fit_note)),
    n_ok = sum(is.na(fit_note)),
    n_fdr_lt_0_05 = sum(p_value_fdr_bh < 0.05, na.rm = TRUE)
  )

print(summary_income)