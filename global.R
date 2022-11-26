# I- Library loading----
library(rsconnect)
library(xgboost)
library(shiny)
library(shinyFeedback)
library(shinyjs)
library(bsplus)
library(htmltools)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)
#library(shinydisconnect)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(caret)
library(kernlab)
library(tidymodels)
library(keras)
library(compositions)
library(lubridate)

new_productive <- c("crimson queen", "demoranville", "haines",
                    "hyred", "welker", "mullica queen",
                    "sundance", "scarlett knight")
old <- c("ben larocque", "ben lear", "ben-pil-35", "bergman",
         "gardner", "pilgrim", "howes", "wilcox")
stevens_grygleski_gh1 <- c("stevens", "grygleski", "gh1")

# II- Data----
## II-1- Import data-----

# Get table metadata. For now, just the fields
# Further development: also define field types
# and create inputs generically

x <- data.frame(field_name = NA,
                Regie = NA,
                N_fertilizer = NA,
                P_fertilizer = NA,
                K_fertilizer = NA,
                Mg_fertilizer = NA,
                S_fertilizer = NA,
                Ca_fertilizer = NA,
                Zn_fertilizer = NA,
                Cu_fertilizer = NA,
                B_fertilizer = NA,
                Mn_fertilizer = NA,
                Yield_curent_year = NA,
                Yield_next_year = NA,
                comment = NA) |>
  rename(`Field name` = field_name,
         `N fertilizer` = N_fertilizer,
         `P fertilizer` = P_fertilizer,
         `K fertilizer` = K_fertilizer,
         `Mg fertilizer` = Mg_fertilizer,
         `S fertilizer` = S_fertilizer,
         `Ca fertilizer` = Ca_fertilizer,
         `Zn fertilizer` = Zn_fertilizer,
         `Cu fertilizer` = Cu_fertilizer,
         `B fertilizer` = B_fertilizer,
         `Mn fertilizer` = Mn_fertilizer,
         `Yield current year` = Yield_curent_year,
         `Yield next year` = Yield_next_year,
         Comment = comment)

 data2 <- readRDS("Data/data_fol_sol_imp_app.rds")
data <-  readRDS("Data/data_merge.rds") |>
  mutate(frozen = as.numeric(frozen),
         sqrt_yield = sqrt(Rendement),
         Regie = if_else(Regie == "Organic", 0, 1),
         Variete = case_when(Variete == "stevens_grygleski_gh1" ~ 1,
                             Variete == "old" ~ 2,
                             Variete == "new_productive" ~ 3),
         Soil_type = if_else(Soil_type == "Sand", 0, 1)) |>
  select(-Rendement, -Annee, -mean_temp) 

set.seed(1765)
data_split <- initial_split(data, strata = "sqrt_yield")
data_train <- training(data_split)
data_test <- testing(data_split)

gaussian_recipe <- 
  recipe(formula = sqrt_yield ~ ., data = data_train) |>
  step_nzv(all_predictors()) |>
  step_normalize(all_numeric(), -all_outcomes()) |>
  prep()

sbp_soil <- read_csv2("Data/sbp_soil.csv")
sbp_leaf <- read_csv2("Data/sbp_leaf.csv")
source("Data/ilr/ilrDefinition.R")
soil_bal_def <- ilrDefinition(sbp_soil,
                              side = "-+", sep.elem = "",
                              sep.bal = ".", sep.left = "", sep.right = ""
)
leaf_bal_def <- ilrDefinition(sbp_leaf,
                              side = "-+", sep.elem = "",
                              sep.bal = ".", sep.left = "", sep.right = ""
)
## II-2- Load models ----
  ## Laod Gaussian model
model <- readRDS("gaussian_cranberry_model.rds")

  ## Next year xgboost models
model_Leaf_Fv_vs_AlZnMnFeCuBMgCaKPN_next <- 
  readRDS("next_year_list_xgboost/Model_Leaf_Fv.AlZnMnFeCuBMgCaKPN_next.rds")

model_Leaf_Al_vs_ZnMnFeCuBMgCaKPN <- 
  readRDS("next_year_list_xgboost/Model_Leaf_Al.ZnMnFeCuBMgCaKPN_next.rds")

model_Leaf_Mn_vs_Fe <- 
  readRDS("next_year_list_xgboost/Model_Leaf_Mn.Fe_next.rds")

model_Leaf_ZnCuBMgCaKPN_vs_MnFe <- 
  readRDS("next_year_list_xgboost/Model_Leaf_ZnCuBMgCaKPN.MnFe_next.rds")

model_Leaf_Cu_vs_Zn <- 
  readRDS("next_year_list_xgboost/Model_Leaf_Cu.Zn_next.rds")

model_Leaf_B_vs_ZnCu <- 
  readRDS("next_year_list_xgboost/Model_Leaf_B.ZnCu_next.rds")

model_Leaf_MgCaKPN_vs_ZnCuB <- 
  readRDS("next_year_list_xgboost/Model_Leaf_MgCaKPN.ZnCuB_next.rds")

model_Leaf_Ca_vs_Mg <- 
  readRDS("next_year_list_xgboost/Model_Leaf_Ca.Mg_next.rds")

model_Leaf_K_vs_MgCa <- 
  readRDS("next_year_list_xgboost/Model_Leaf_K.MgCa_next.rds")

model_Leaf_PN_vs_MgCaK <- 
  readRDS("next_year_list_xgboost/Model_Leaf_PN.MgCaK_next.rds")

model_Leaf_P_vs_N <- 
  readRDS("next_year_list_xgboost/Model_Leaf_P.N_next.rds")

model_soil_Fv_vs_FeBMnCuZnMgKAlPCa <- 
  readRDS("next_year_list_xgboost/Model_soil_Fv.FeBMnCuZnMgKAlPCa_next.rds")

model_soil_Fe_vs_Al <- 
  readRDS("next_year_list_xgboost/Model_soil_Fe.Al_next.rds")

model_soil_FeAl_vs_BMnCuZnMgKPCa <- 
  readRDS("next_year_list_xgboost/Model_soil_FeAl.BMnCuZnMgKPCa_next_next.rds")

model_soil_Mn_vs_B_next <- 
  readRDS("next_year_list_xgboost/Model_soil_Mn.B_next_next.rds")

model_soil_BMn_vs_Zn_next <- 
  readRDS("next_year_list_xgboost/Model_soil_BMn.Zn_next.rds")

model_soil_BMnZn_vs_CuMgKPCa_next <- 
  readRDS("next_year_list_xgboost/Model_soil_BMnZn.CuMgKPCa_next.rds")

model_soil_Cu_vs_Mg_next <- 
  readRDS("next_year_list_xgboost/Model_soil_Cu.Mg_next.rds")

model_soil_CuMg_vs_Ca_next <- 
  readRDS("next_year_list_xgboost/Model_soil_CuMg.Ca_next.rds")

model_soil_CuMgCa_vs_KP_next <- 
  readRDS("next_year_list_xgboost/Model_soil_CuMgCa.KP_next.rds")

model_soil_K_vs_P_next <- 
  readRDS("next_year_list_xgboost/Model_soil_K.P_next.rds")
