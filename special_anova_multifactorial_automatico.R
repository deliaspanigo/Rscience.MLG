
### Section 01 of 15 - Libraries
library(stats)      # General Linear Models, Shapiro test and Bartlett test.
library(agricolae)  # Tukey test
library(gplots)     # Graphics
library(purrr)
library(dplyr)  # Para usar el operador %>%
#
set.seed(123)
database <- data.frame(
  response = rnorm(100),
  factor1 = sample(letters[1:3], 100, replace = TRUE),
  factor2 = sample(letters[4:6], 100, replace = TRUE),
  factor3 = sample(letters[7:9], 100, replace = TRUE)
)
database <- rbind.data.frame(database, database, database, database)
#database <- mtcars
#database <- rbind.data.frame(database, database, database, database)

#database <- mtcars
#colname_vr <- 'mpg'
#vector_colname_factor <- c("cyl", "am", "vs")

colname_vr <- 'response'
vector_colname_factor <- c("factor1", "factor2", "factor3")

# All selected var in specific order
vector_selected_vars <- c(colname_vr, vector_colname_factor)
vector_selected_vars



# Alpha value and Confidence value
alpha_value <- 0.05
confidence_value <- 1 - alpha_value


minibase <- na.omit(database[,vector_selected_vars])
minibase <- minibase  %>% mutate_at(vars(all_of(vector_colname_factor)), as.factor)


# Info
df_show_n <- data.frame(
  "object" = c("database", "minibase"),
  "n_col" = c(ncol(database), ncol(minibase)),
  "n_row" = c(nrow(database), nrow(minibase))
)
df_show_n

####################################################################################

# Todas las combinaciones posibles entre los factores
# Generar todas las combinaciones usando lapply
list_combinations_factors <- 1:length(vector_colname_factor) %>%
  purrr::map(~ utils::combn(vector_colname_factor, .x, simplify = FALSE)) %>%
  purrr::flatten()



list_combinations_names <- list_combinations_factors %>%
  purrr::map_chr(~ paste0(.x, collapse = "_"))


###
# Crear una lista para almacenar los resultados
list_formula_vr_graficos <- 1:length(list_combinations_factors) %>%
  purrr::map(~ {
    selected_vars <- list_combinations_factors[[.x]]
    stats::as.formula(paste(colname_vr, " ~ ", paste0(selected_vars, collapse = " + ")), env = .GlobalEnv)
  })
names(list_formula_vr_graficos) <- list_combinations_names


# Crear una lista para almacenar los resultados
list_formula_residuals_graficos <- 1:length(list_combinations_factors) %>%
  purrr::map(~ {
    selected_vars <- list_combinations_factors[[.x]]
    stats::as.formula(paste("residuals", " ~ ", paste0(selected_vars, collapse = " + ")), env = .GlobalEnv)
  })
names(list_formula_residuals_graficos) <- list_combinations_names


list_df_factor_info <- map(list_combinations_factors, ~{
  #filtered_database <-
  df_mean <- minibase %>% select(all_of(c(colname_vr, .x))) %>%
    group_by(across(all_of(.x))) %>%
    summarise(mean = mean(!!sym(colname_vr)),
              n = n())

  df_mean <- as.data.frame(df_mean)
  df_mean <- cbind.data.frame("order" = 1:nrow(df_mean), df_mean)
  df_mean$"color" <- rainbow(1:nrow(df_mean))
  df_mean
})
list_df_factor_info




list_check_unbalanced_reps <- lapply(list_df_factor_info, function(selected_table){
  check_unbalanced_reps <- length(unique(selected_table$n)) > 1
  check_unbalanced_reps

})
names(list_check_unbalanced_reps) <- list_combinations_names



# # # # # Section 06 - Anova Test ----------------------------------------------
# # # Anova test
string_anova <- paste0(colname_vr, " ~ ", paste0(vector_colname_factor, collapse = " * "), "-1")
formula_anova <- stats::as.formula(string_anova, env = .GlobalEnv)
lm_anova  <- lm(formula = formula_anova, data = minibase)
aov_anova <- aov(lm_anova)
df_table_anova <- as.data.frame(summary(aov_anova)[[1]])
df_table_anova




### Seccion  05 de 15 - Factor info
# # # Standard error from model for each level
model_error_var <- tail(df_table_anova$"Mean Sq", n = 1)
model_error_sd <- sqrt(model_error_var)


list_df_model_error <- lapply(1:length(list_df_factor_info), function(x){


  selected_table <- list_df_factor_info[[x]]

  df_model_error <- selected_table
  df_model_error$"model_error_var" <-  model_error_var
  df_model_error$"model_error_sd"  <-  model_error_sd
  df_model_error$"model_error_se"  <-  df_model_error$"model_error_sd"/sqrt(df_model_error$"n")
  new_order_col <- c("order", list_combinations_factors[[x]], "mean", "model_error_var",
                     "model_error_sd", "n", "model_error_se", "color")
  df_model_error <- df_model_error[new_order_col]
  df_model_error

})




### Seccion  05 de 15 - Factor info
# # # # # Section 07 - minibase_mod --------------------------------------------
# # # Detect rows on database there are on minibase
dt_rows_database_ok <- rowSums(!is.na(database[vector_selected_vars])) == ncol(minibase)



# # # Object minibase_mod and new cols
minibase_mod <- minibase
minibase_mod$"fitted.values" <-  fitted(aov_anova)
minibase_mod$"residuals" <- lm_anova$residuals
minibase_mod$"id_database" <- c(1:nrow(database))[dt_rows_database_ok]
minibase_mod$"id_minibase" <- 1:nrow(minibase)
minibase_mod$"studres" <- minibase_mod$"residuals"/model_error_sd


head(x=minibase_mod, n=5)


# # # Residuals variance from levels from original residuals
df_residuals_variance_levels <- data.frame(
  "order" = 1:nlevels(minibase_mod[,colname_factor]),
  "level" = levels(minibase_mod[,colname_factor]),
  "variance" = tapply(minibase_mod[,"residuals"], minibase_mod[,colname_factor], var),
  "n" = tapply(minibase_mod[,"residuals"], minibase_mod[,colname_factor], length)
)
df_residuals_variance_levels



# # # Sum for residuals
sum_residuals <- sum(minibase_mod[,"residuals"])
sum_residuals



# # # Mean for residuals
mean_residuals <- mean(minibase_mod[,"residuals"])
mean_residuals

# # # # # Section 08 - Requeriments for residuals-------------------------------
# # # Normality test (Shapiro-Wilk)
test_residuals_normality <- shapiro.test(minibase_mod[,"residuals"])
test_residuals_normality




# # # Homogeinidy test (Bartlett)
string_bartlett <- paste0("residuals ~ interaction(", paste0(vector_colname_factor, collapse = ","), ")")
formula_bartlett <- as.formula(string_bartlett)
test_residuals_homogeneity <- bartlett.test(formula = formula_bartlett, data = minibase_mod)
test_residuals_homogeneity


### Seccion  05 de 15 - Factor info
# # # # Section 09 - Tukey --------------------------------------------------
# # # Tukey test - Tukey with groups - Full version

list_tukey <- lapply(1:length(list_combinations_factors), function(x){

  selected_factors <- list_combinations_factors[[x]]
  selected_check_unbalanced_reps <- list_check_unbalanced_reps[[x]]

  tukey01_full_groups <- agricolae::HSD.test(y = lm_anova,
                                             trt = selected_factors,
                                             alpha = alpha_value,
                                             group = TRUE,
                                             console = FALSE,
                                             unbalanced = selected_check_unbalanced_reps)



  # # # Tukey test - Tukey pairs comparation - Full version
  tukey02_full_pairs <- agricolae::HSD.test(y = lm_anova,
                                            trt = selected_factors,
                                            alpha = alpha_value,
                                            group = FALSE,
                                            console = FALSE,
                                            unbalanced = selected_check_unbalanced_reps)





  # # Original table from R about Tukey
  df_tukey_original_table <- tukey01_full_groups$groups




  # # # New table about Tukey
  df_tukey_table <- data.frame(
    "level" = rownames(tukey01_full_groups$groups),
    "mean" = tukey01_full_groups$groups[,1],
    "group" = tukey01_full_groups$groups[,2]
  )

  output_list <- Hmisc::llist(tukey01_full_groups, tukey02_full_pairs,
                              df_tukey_original_table, df_tukey_table)


  output_list
})
names(list_tukey) <- list_combinations_names




  ```{r, include =T, eval =T, class.source="bg-success"}
# # # # # Section 10 - Partitioned Measures (VR)--------------------------------
# # # Partitioned Measures of Position (VR)
df_vr_position_levels <- data.frame(
  "order" = 1:nlevels(minibase[,2]),
  "level" = levels(minibase[,2]),
  "min" = tapply(minibase[,1], minibase[,2], min),
  "mean" = tapply(minibase[,1], minibase[,2], mean),
  "Q1" = tapply(minibase[,1], minibase[,2], quantile, 0.25),
  "median" = tapply(minibase[,1], minibase[,2], median),
  "Q3" = tapply(minibase[,1], minibase[,2], quantile, 0.75),
  "max" = tapply(minibase[,1], minibase[,2], max),
  "n" = tapply(minibase[,1], minibase[,2], length)
)



# # # Partitioned Measures of Dispersion (VR)
df_vr_dispersion_levels <- data.frame(
  "order" = 1:nlevels(minibase[,2]),
  "level" = levels(minibase[,2]),
  "range" = tapply(minibase[,1], minibase[,2], function(x){max(x) - min(x)}),
  "variance" = tapply(minibase[,1], minibase[,2], var),
  "standard_deviation" = tapply(minibase[,1], minibase[,2], sd),
  "standard_error" = tapply(minibase[,1], minibase[,2], function(x){sd(x)/sqrt(length(x))}),
  "n" = tapply(minibase[,1], minibase[,2], length)
)
df_vr_dispersion_levels



# # # General Measures of Position (VR)
df_vr_position_general <- data.frame(
  "min" = min(minibase[,1]),
  "mean" = mean(minibase[,1]),
  "median" = median(minibase[,1]),
  "max" = max(minibase[,1]),
  "n" = length(minibase[,1])
)
df_vr_position_general



# # # General Measures of Dispersion (VR)
df_vr_dispersion_general <- data.frame(
  "range" = max(minibase[,1]) - min(minibase[,1]),
  "variance" = var(minibase[,1]),
  "standard_deviation" = sd(minibase[,1]),
  "standard_error" = sd(minibase[,1])/(sqrt(length(minibase[,1]))),
  "n" = length(minibase[,1])
)
df_vr_dispersion_general





# # # # # Section 11 - Partitioned Measures (Residuals)-------------------------
# # # Partitioned Measures of Position (residuals)
df_residuals_position_levels <- data.frame(
  "order" = 1:nlevels(minibase_mod[,2]),
  "level" = levels(minibase_mod[,2]),
  "min" = tapply(minibase_mod$residuals, minibase_mod[,2], min),
  "mean" = tapply(minibase_mod$residuals, minibase_mod[,2], mean),
  "median" = tapply(minibase_mod$residuals, minibase_mod[,2], median),
  "max" = tapply(minibase_mod$residuals, minibase_mod[,2], max),
  "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
)
df_residuals_position_levels



# # # Partitioned Measures of Dispersion (residuals)
df_residual_dispersion_levels <- data.frame(
  "order" = 1:nlevels(minibase_mod[,2]),
  "level" = levels(minibase_mod[,2]),
  "range" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){max(x) - min(x)}),
  "variance" = tapply(minibase_mod$residuals, minibase_mod[,2], var),
  "standard_deviation" = tapply(minibase_mod$residuals, minibase_mod[,2], sd),
  "standard_error" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){sd(x)/sqrt(length(x))}),
  "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
)
df_residual_dispersion_levels



# # # General Measures of Position (residuals)
df_residuals_position_general <- data.frame(
  "min" = min(minibase_mod$residuals),
  "mean" = mean(minibase_mod$residuals),
  "median" = median(minibase_mod$residuals),
  "max" = max(minibase_mod$residuals),
  "n" = length(minibase_mod$residuals)
)
df_residuals_position_general



# # # General Measures of Dispersion (residuals)
df_residuals_dispersion_general <- data.frame(
  "range" = max(minibase_mod$residuals) - min(minibase_mod$residuals),
  "variance" = var(minibase_mod$residuals),
  "standard_deviation" = sd(minibase_mod$residuals),
  "standard_error" = sd(minibase_mod$residuals)/(sqrt(length(minibase_mod$residuals))),
  "n" = length(minibase_mod$residuals)
)
df_residuals_dispersion_general
```


```{r, include =T, eval =T, class.source="bg-success"}

# # # # # Section 12 - Model estimators ----------------------------------------
# # # Means for each level
vector_est_mu_i <- df_vr_position_levels$mean
vector_est_mu_i



# # # Mean of means
est_mu <- mean(vector_est_mu_i)
vector_est_mu <- rep(est_mu, length(vector_est_mu_i))
vector_est_mu



# # # Tau efects
vector_est_tau_i <- vector_est_mu_i - vector_est_mu
vector_est_tau_i



# # # Sum of tau efects
sum_est_tau_i <- sum(vector_est_tau_i)
sum_est_tau_i



# # # Long model information on dataframe
df_anova1way_model_long <- data.frame(
  "order" = df_factor_info$order,
  "level" = df_factor_info$level,
  "n" = df_factor_info$n,
  "est_mu" = vector_est_mu,
  "est_tau_i" = vector_est_tau_i
)
df_anova1way_model_long



# # # Short model information on dataframe
df_anova1way_model_short <- data.frame(
  "order" = df_factor_info$order,
  "level" = df_factor_info$level,
  "n" = df_factor_info$n,
  "est_mu_i" = vector_est_mu_i
)
df_anova1way_model_short
```




