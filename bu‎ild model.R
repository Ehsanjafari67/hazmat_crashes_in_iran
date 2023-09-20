#-------------------------------1.decision tree model---------------------------
#| cache: TRUE

set.seed(1212) 
hazmat_crash_split <- initial_split(InitialDataForAnalysis, strata = Injury_Level) 
hazmat_crash_train <- training(hazmat_crash_split) 
hazmat_crash_test <- testing(hazmat_crash_split)  

set.seed(123) 
hazmat_crash_folds <- vfold_cv(hazmat_crash_train, strata = Injury_Level) 
hazmat_crash_folds
#-------------------------------------------------------------------------------
#| cache: TRUE

names(InitialDataForAnalysis)

hazmat_crash_rec <- recipe(Injury_Level ~ ., data = hazmat_crash_train) |> 
  step_downsample(Injury_Level)  

bag_spec <- bag_tree(min_n = 10) |>    
  set_engine("rpart", times = 25) |>    
  set_mode("classification")  
hazmat_crash_wf <- workflow() |>    
  add_recipe(hazmat_crash_rec) |>    
  add_model(bag_spec)  

hazmat_crash_wf
#-------------------------------------------------------------------------------
#| cache: TRUE

doParallel::registerDoParallel() 
hazmat_crash_res <- fit_resamples(hazmat_crash_wf,   
                                  hazmat_crash_folds,   
                               control = control_resamples(save_pred = TRUE))
#-------------------------------------------------------------------------------
#| cache: TRUE

collect_metrics(hazmat_crash_res)
#-------------------------------------------------------------------------------
#| cache: TRUE

hazmat_crash_fit <- last_fit(hazmat_crash_wf, hazmat_crash_split) 
collect_metrics(hazmat_crash_fit)
#------------------------------importance plot----------------------------------
#| cache: TRUE
#| label: fig-VarImp
#| fig-cap: "The importance of predictor variables to describe the severity of accidents"

hazmat_crash_imp <- hazmat_crash_fit$.workflow[[1]]  |>    
  pull_workflow_fit()  
hazmat_crash_imp$fit$imp  |>    
  slice_max(value, n = 10)  |>    
  ggplot(aes(value, fct_reorder(term, value))) +   
  geom_col(alpha = 0.8, fill = "midnightblue") +   
  labs(x = "Variable importance score", y = NULL)
#------------------------------------ROC curve----------------------------------
#| cache: TRUE
#| label: fig-ROC
#| fig-cap: "ROC curve"

# Extract predictions from the first split
predictions <- hazmat_crash_fit$.predictions[[1]]

# Check column names
colnames(predictions)

# Create ROC curves for each level of Injury_Level
roc_fatal <- roc(ifelse(predictions$Injury_Level == "Fatal", 1, 0), predictions$.pred_Fatal)
roc_injury <- roc(ifelse(predictions$Injury_Level == "Injury", 1, 0), predictions$.pred_Injury)
roc_pdo <- roc(ifelse(predictions$Injury_Level == "PDO", 1, 0), predictions$.pred_PDO)

# Plot the ROC curves
plot(roc_fatal, col = "red", main = "Multi-Class ROC Curve")
lines(roc_injury, col = "blue")
lines(roc_pdo, col = "green")
legend("bottomright", legend = c("Fatal", "Injury", "PDO"), col = c("red", "blue", "green"))