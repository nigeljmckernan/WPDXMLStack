## ----setup, include=FALSE--------------------------------------------------------------------
library(knitr)
opts_chunk$set(echo = TRUE)


## ----descriptions, echo=FALSE----------------------------------------------------------------

tt_desc <- tibble::tribble(
    ~"Variable", ~"Class", ~"Description",
    "row_id", "double", "Unique ID",
    "lat_deg", "double", "Latitude",
    "lon_deg", "double", "Longitude",
    "report_date", "character", "Date water source was reported on",
    "status_id", "character", "Identify if any water is available of the visit, recognizing that it may be a limited flow",
    "water_source_clean", "character", "Describe the water source",
    "water_tech", "character", "Describe the system being used to transport the water from the source to the point of collection",
    "facility_type", "character", "Categorized facility type based on JMP definitions",
    "country_name", "character", "Country Name",
    "install_year", "integer", "Install year",
    "Installer", "character", "Installer",
    "pay", "character", "Provide the payment amount and basis. If no amount is provided the basis can be provided alone. An amount without a payment basis cannot be included",
    "status", "character", "Provide a status of the physical/mechanical condition of the water point"
)

kable(tt_desc)


## ----download data, message=FALSE------------------------------------------------------------

library(tidytuesdayR) 
  
tt_data <- tt_download(tt_load_gh("2021-05-04")) %>% purrr::pluck("water")
 
tt_backup <- tt_data


## ----skim------------------------------------------------------------------------------------
skimr::skim(tt_data) 


## ----omitting installer----------------------------------------------------------------------
# tt_data <- na.omit(tt_data[, c(1:10, 12:13)])

tt_data <- na.omit(tt_data)

tt_data


## ----librariies, message=FALSE---------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(knitr)
library(stacks)


## ----repair names----------------------------------------------------------------------------

tt_data <- tt_data %>%
  
  rename_with(~ str_replace_all(.x, "_", " ")) %>% # Replacing underscores with spaces for str_to_title()
  
  rename_with(str_to_title) %>% # Converting Names to Title Case
  
  rename_with( ~ str_replace_all(.x, " ", "_")) %>% # Converting back to snake case
  
  rename(Water_Flowing = Status_Id) # This name makes more intuitive sense to me for this variable

tt_data


## ----water source count----------------------------------------------------------------------
tt_data %>%
  
  count(Water_Source, sort = TRUE) %>%
  
  mutate(n = n %>% scales::comma()) %>%
  
  kable()


## ----water source clean----------------------------------------------------------------------
# Cleaning Water Source -----------------

tt_data <- tt_data %>%
  
  mutate(
    Water_Source = Water_Source %>% str_remove_all("\\(River/Stream/Lake/Pond/Dam\\)"),
    Water_Source = Water_Source %>% str_trim() %>% str_to_title(),
    Water_Source = Water_Source %>% str_replace_all(" ", "_"),
    Water_Source = Water_Source %>% str_replace_all("-", "_")
  )

tt_data %>% 
  
  count(Water_Source, sort = TRUE) %>% 
  
  mutate(n = n %>% scales::comma()) %>%
  
  kable()


## ----water tech count------------------------------------------------------------------------

tt_data %>%
  
  count(Water_Tech, sort = TRUE) %>%
  
  mutate(n = n %>% scales::comma()) %>%
  
  kable()


## ----water tech clean------------------------------------------------------------------------
tt_data <- tt_data %>%
  
  mutate(
    Water_Tech = Water_Tech %>% str_remove_all(" - ."),
    Water_Tech = case_when(
      Water_Tech %>% str_detect("Hand Pump") ~ "Hand Pump",
      Water_Tech %>% str_detect("Mechanized Pump") ~ "Mechanized Pump",
      TRUE ~ Water_Tech
    ),
    Water_Tech = Water_Tech %>% str_replace_all(" ", "_") %>% str_trim()
  )

tt_data %>%
  
  count(Water_Tech, sort = TRUE) %>%
  
  mutate(n = n %>% scales::comma()) %>%
  
  kable()


## ----facility clean--------------------------------------------------------------------------
# Cleaning Facility Type --------------------------------

tt_data <- tt_data %>%
  
  mutate(
    Facility_Type = Facility_Type %>% str_to_title() %>% str_replace_all(" ", "_")
  )

tt_data %>%
  
  count(Facility_Type, sort = TRUE) %>%
  
  mutate(n = n %>% scales::comma()) %>%
  
  kable()


## ----status count----------------------------------------------------------------------------
tt_data %>%
  
  count(Status, sort = T) %>%
  
  DT::datatable()



## ----Status Clean----------------------------------------------------------------------------
# Cleaning Status --------------------------------

tt_data <- tt_data %>%
  
  mutate(
    Status = Status %>% str_to_title(),
    Status = case_when(
      Status %>% str_detect("Non Function") ~ "Non_Functional",
      Status %>% str_detect("Non-Function") ~ "Non_Functional",
      Status %>% str_detect("Non-function") ~ "Non_Functional",
      Status %>% str_detect("Function") ~ "Functional",
      Status %>% str_starts("Working") ~ "Functional",
      Status %>% str_starts("Satus") ~ "Functional",
      Status %>% str_detect("Broken") ~ "Non_Functional",
      Status %in% c("Operational", "Yes") ~ "Functional",
      Status %>% str_detect("Abandon") ~ "Non_Functional",
      TRUE ~ Status
    ),
    Status = Status %>% str_replace_all(" ", "_"),
    Status = Status %>% str_replace_all("-", "_")
  )

tt_data %>%
  
  count(Status, sort = TRUE) %>%
  
  DT::datatable()


## ----pay clean-------------------------------------------------------------------------------
# Converting Column Types --------------------------

tt_data <- tt_data %>%
  
  mutate(
    Pay = case_when(
      Pay %in% c("No", "No payment – its free", "No- water is free", "Never pay", "They do not pay", "0") ~ "No",
      Pay %>% str_detect("Yes") | Pay %>% str_detect("yes") | Pay %in% c("Pay monthly", "Pay when scheme fails", "Pay per bucket", "Only when there is a breakdown") ~ "Yes",
      TRUE ~ Pay
    )
  )

tt_data %>%
  
  count(Pay, sort = TRUE) %>%
  
  mutate(prop = floor(100*n/sum(n))) %>%
  
  DT::datatable()


## ----clean installer-------------------------------------------------------------------------

tt_data %>%
  
  count(Installer, sort = T) %>%
  
  mutate(
    Prop = round(n / sum(n) * 100, 2)
  )


## ----lumping---------------------------------------------------------------------------------
tt_data <- tt_data %>%
  mutate(
    Age = 2021 - Install_Year,
    Report_Date = Report_Date %>% lubridate::mdy(),
    across(where(is.character), ~ str_replace_all(.x, "\\.", "_") %>% str_replace_all(" ", "_")),
    # across(where(is.character),  ~ fct_lump_n(.x, 5L)),
    across(where(is.factor), as.character),
    Water_Flowing = Water_Flowing %>% str_to_upper(),
    ) %>%
  
  select(-c(Install_Year)) %>%
  
  filter(Water_Flowing != "U") %>%
  
  na.omit()

tt_data


## ----strata----------------------------------------------------------------------------------
tt_data %>%
  
  count(Water_Flowing) %>%
  
  mutate(Prop = n / sum(n) * 100) %>%
  
  kable()


## ----splitting-------------------------------------------------------------------------------
set.seed(123) # Pseudo-Random Numbers for Reproducibility
train_test_data <- initial_split(tt_data, strata = Water_Flowing) # Splitting the Data

train_set <- training(train_test_data) # Making our Training Set

test_set <- testing(train_test_data) # Making our Test Set


## ----folds-----------------------------------------------------------------------------------
set.seed(456) # For reproducibility
folds <- vfold_cv(train_set, v = 5)


## ----ranger spec-----------------------------------------------------------------------------
forest_spec <- 
  
  rand_forest(
    trees = tune(),
    mtry = tune(),
    min_n = tune()
  ) %>%
  
  set_engine("ranger") %>%
  
  set_mode("classification")

forest_spec %>% translate()


## ----log show engines------------------------------------------------------------------------

show_engines("logistic_reg")



## ----log spec--------------------------------------------------------------------------------
log_spec <- 
  
  logistic_reg(
    penalty = tune(),
    mixture = tune()
  ) %>%
  
  set_engine("glmnet") %>%
  
  set_mode("classification")

log_spec %>% translate()


## ----knn engines-----------------------------------------------------------------------------
show_engines("nearest_neighbor")


## ----knn spec--------------------------------------------------------------------------------
knn_spec <- 
  
  nearest_neighbor(
    neighbors = tune()
  ) %>%
  
  set_engine("kknn") %>%
  
  set_mode("classification")

knn_spec %>% translate()


## ----forest recipe---------------------------------------------------------------------------

forest_rec <-
  
  recipe(Water_Flowing ~ ., data = train_set) %>%
  
  step_nzv(all_predictors()) %>% 
  
  step_date(Report_Date) %>%
  
  step_rm(c(Report_Date, Row_Id, Lat_Deg, Lon_Deg)) %>%
  
  step_other(all_nominal_predictors()) %>%
  
  step_dummy(all_nominal_predictors()) %>%
  
  step_normalize(all_numeric_predictors())

forest_rec



## ----recipe preview--------------------------------------------------------------------------
forest_rec %>% prep() %>% bake(NULL)


## ----log rec---------------------------------------------------------------------------------
log_rec <- 
  
  recipe(Water_Flowing ~ ., data = train_set) %>%
  
  step_nzv(all_predictors()) %>%
  
  step_date(Report_Date) %>%
  
  step_rm(c(Report_Date, Row_Id, Lat_Deg, Lon_Deg)) %>%
  
  step_other(all_nominal_predictors()) %>%
  
  step_dummy(all_nominal_predictors())
  
  # step_normalize(all_numeric_predictors()) %>%



## ----log bake--------------------------------------------------------------------------------
juiced_data <- log_rec %>% prep() %>% bake(NULL)

juiced_data


## ----knn rec---------------------------------------------------------------------------------
knn_rec <- forest_rec

knn_rec


## ----workflows-------------------------------------------------------------------------------

forest_wf <- 
  
  workflow() %>%
  
  add_model(forest_spec) %>%
  
  add_recipe(forest_rec)

log_wf <- 
  
  workflow() %>%
  
  add_model(log_spec) %>%
  
  add_recipe(log_rec)

knn_wf <- 
  
  workflow() %>%
  
  add_model(knn_spec) %>%
  
  add_recipe(knn_rec)



## ----parallel, eval=FALSE--------------------------------------------------------------------
## 
## num_threads <- detectCores(logical = FALSE) - 1L
## 
## num_threads
## 


## ----n-1 cores, eval=FALSE-------------------------------------------------------------------
## 
## numberMinusOneCores <- function() {
## 
##   if(detectCores(logical = TRUE) == detectCores(logical = FALSE)) {
##     num_threads <- as.integer(detectCores() - 1L) # No Hyper-Threading Present
##     }
##     else {
##       num_threads <- as.integer(detectCores() - 2L) # Hyper-Threading Present
##     }
##   return(num_threads)
##   }
## 
## num_threads <- numberMinusOneCores()
## 
## num_threads
## 


## ----cluster, eval=FALSE---------------------------------------------------------------------
## 
## cl  <- makeCluster(num_threads)
## registerDoParallel(cl)
## 


## ----forest tune, cache=TRUE-----------------------------------------------------------------
set.seed(2021)
forest_res <-
  
  forest_wf %>%
  
  tune_grid(
    resamples = folds,
    grid = 3L,
    control = control_stack_grid()
  )

forest_res %>% show_best("roc_auc")


## ----log tune, cache=TRUE--------------------------------------------------------------------
set.seed(2021)
log_res <-
  
  log_wf %>%
  
  tune_grid(
    resamples = folds,
    grid = 5L,
    control = control_stack_grid()
  )

log_res %>% show_best("roc_auc")


## ----knn tune, cache=TRUE--------------------------------------------------------------------
set.seed(2021)
knn_res <-
  
  knn_wf %>%
  
  tune_grid(
    resamples = folds,
    grid = 3L,
    control = control_stack_grid()
  )

knn_res %>% show_best("roc_auc")


## ----stop cluster, eval=FALSE----------------------------------------------------------------
## stopCluster(cl)


## ----forest top 5----------------------------------------------------------------------------
forest_res %>% show_best("roc_auc")


## ----forest best-----------------------------------------------------------------------------
forest_best <-

  forest_res %>% select_best("roc_auc")
  
forest_best


## ----forest final wf-------------------------------------------------------------------------
forest_wf_final <-
  
  forest_wf %>%
  
  finalize_workflow(forest_best)

forest_wf_final


## ----forest fit, cache=TRUE------------------------------------------------------------------
forest_fit_final <-
  
  forest_wf_final %>%
  
  last_fit(train_test_data)

forest_fit_final


## ----forest pred-----------------------------------------------------------------------------
forest_pred <-
  
  forest_fit_final %>%
  
  collect_predictions() %>%
  
  roc_curve(Water_Flowing, .pred_N) %>%
  
  mutate(Model = "Forest")


## ----log top 5-------------------------------------------------------------------------------
log_res %>% show_best("roc_auc")


## ----log best--------------------------------------------------------------------------------
log_best <-
  
  log_res %>% select_best("roc_auc")

log_best


## ----log wf final----------------------------------------------------------------------------
log_wf_final <-
  
  log_wf %>%
  
  finalize_workflow(log_best)

log_wf_final


## ----log fit, cache=TRUE---------------------------------------------------------------------
log_fit_final <-
  
  log_wf_final %>%
  
  last_fit(train_test_data)

log_fit_final


## ----log pred--------------------------------------------------------------------------------
log_pred <-
  
  log_fit_final %>%
  
  collect_predictions() %>%
  
  roc_curve(Water_Flowing, .pred_N) %>%
  
  mutate(Model = "Logistic")

log_pred


## ----knn top 5-------------------------------------------------------------------------------
knn_res %>% show_best("roc_auc")


## ----knn best--------------------------------------------------------------------------------
knn_best <-
  
  knn_res %>%
  
  select_best("roc_auc")

knn_best


## ----wf final--------------------------------------------------------------------------------
knn_wf_final <-
  
  knn_wf %>%
  
  finalize_workflow(knn_best)

knn_wf_final


## ----knn fit, cache=TRUE---------------------------------------------------------------------
knn_fit_final <-
  
  knn_wf_final %>%
  
  last_fit(train_test_data)

knn_fit_final


## ----knn pred--------------------------------------------------------------------------------
knn_pred <-
  
  knn_fit_final %>%
  
  collect_predictions() %>%
  
  roc_curve(Water_Flowing, .pred_N) %>%
  
  mutate(Model = "KNN")

knn_pred


## ----combine pred----------------------------------------------------------------------------
all_pred <-
  
  bind_rows(knn_pred, forest_pred, log_pred)

all_pred


## ----roc plot--------------------------------------------------------------------------------
all_pred %>%
  
  ggplot(aes(x = (1 - specificity), y = sensitivity, color = Model)) + 
  
  geom_line() + 
  
  geom_abline(linetype = "dashed") +
  
  ggtitle("ROC Curves of Candidate Models") + 
  
  ylab("True Positivity Rate") + 
  
  xlab("True Negativity Rate")


## ----stacking, cache=TRUE--------------------------------------------------------------------

model_stack <-
  
  stacks() %>%
  
  add_candidates(log_res) %>%
  
  add_candidates(knn_res) %>%
  
  add_candidates(forest_res)

model_stack


## ----fit stack, cache=FALSE------------------------------------------------------------------

data_stack <-

  model_stack %>%

  blend_predictions() %>%
  
  fit_members()

data_stack



## ----add stack to predictions----------------------------------------------------------------

stack_pred <-

  test_set %>%
  
  bind_cols(predict(data_stack, ., type = "prob"))

stack_roc <-
  
  stack_pred %>%
  
  roc_curve(
    as.factor(Water_Flowing),
    .pred_N
  ) %>%
  
  mutate(
    Model = "Stack"
  )



## ----add stack to preds----------------------------------------------------------------------

all_pred <-
  
  all_pred %>%
  
  bind_rows(stack_roc)



## ----plot roc--------------------------------------------------------------------------------

all_pred %>%
  
  ggplot(aes((1 - specificity), sensitivity, color = Model)) +
  
  geom_line() +
  
  geom_abline(linetype = "dashed") +
  
  xlab("True Negativity Rate") +
  
  ylab("True Positivity Rate") +
  
  ggtitle("ROC Curves Plus Stack Predictions")



## ----final auc-------------------------------------------------------------------------------

list(forest_fit_final, log_fit_final, knn_fit_final) %>% 
  map_dfr(~ collect_predictions(.) %>% roc_auc(Water_Flowing, .pred_N)) %>% 
  mutate(Model = c("forest", "logit", "knn")) %>%
  bind_rows(
    stack_pred %>%
      roc_auc(as.factor(Water_Flowing), .pred_N) %>%
      mutate(Model = "stack")
    )

