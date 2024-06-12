library(dplyr)
library(tidyr)
library(tsoutliers)
library(seastests)
library(tseries)
library(TSA)

# Load data frame
landingsV2 <- read.csv("Perso/landingsV2.csv")

#test2
# Create landings-weight data frame
land_w <- landingsV2 %>%
  select(YEAR, quarter, X3A_CODE, totwghtlandg)%>%
  group_by(YEAR, quarter, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = X3A_CODE, values_from = totwghtlandg) %>%
  mutate(across(everything(), ~replace_na(., 0)))

# Create landings-value data frame
land_v <- landingsV2 %>%
  select(YEAR, quarter, X3A_CODE, totvallandg)%>%
  group_by(YEAR, quarter, X3A_CODE) %>%
  summarise(totvallandg = sum(totvallandg, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = X3A_CODE, values_from = totvallandg) %>%
  mutate(across(everything(), ~replace_na(., 0)))
# IL FAUDRAIT PLUTOT DEGAGER LES COLONNES AVEC DES NA?

################################################################################
################################################################################
################################################################################
## Correction pts atypiques----
y0 <- ts(data = land_w$ELX, start=c(2013,01),frequency=4)
tso(y0) 
fit <- tso(y0)
plot(fit)
y0adj <- fit$yadj
write(t(y0adj),file="adjusted-series.out",ncolumn=1,append=FALSE)
adf.test(y0adj) # p > 0.05 donc série non-stationnaire

## Désaisonnalisation----
y_corr <- y0adj
combined_test(y_corr) # Webel-Ollech Test
seasdum(y_corr) # Seasonal Dummies
periodogram(y_corr)
# Pour les deux test,  p-value < 0.05 donc H1 présence de saisonnalité

decomp_y_corr <- stl(y_corr, s.window="periodic")
# Obtenir la composante saisonnière de la série
seasonal_y_corr <- decomp_y_corr$time.series[, "seasonal"]
# Corriger la composante saisonnière de la série
y_corr <- y_corr - seasonal_y_corr
combined_test(y_corr) # Webel-Ollech Test
seasdum(y_corr) # Seasonal Dummies

isSeasonal(y_corr, test = "combined", freq = 4) # pas de saisonnalité
fried(y_corr) # pas de saisonnalité
kwt <- kw(y_corr)
show(kwt) # pas de saisonnalité
qst <- qs(y_corr)
show(qst)#  saisonnalité
sd <- seasdum(y_corr)
show(sd)# pas de saisonnalité

## Stationnarisation----
y1 <- diff(y_corr) # On effectue une différenciation
adf.test(y1) # série stationnaire

## Re-points atypiques----
tso(y1)
fit <- tso(y1)
plot(fit)
y1adj <- fit$yadj
write(t(y1adj),file="adjusted-series.csv",ncolumn=1,append=FALSE)



################################################################################
################################################################################
################################################################################


# Transform each variables in a time-series format----
ts_list <- list()

for (i in 3:ncol(land_w)) {
  col_name <- names(land_w)[i]
  ts_name <- paste0("ts_", col_name)
  ts_data <- assign(ts_name, ts(data = land_w[, col_name], start = c(2013, 01), frequency = 4))
  ts_list[[col_name]] <- ts_data
}


# Correction of atypical points----
for (col_name in names(ts_list)) {
  ts_name <- paste0("ts_", col_name)
  ts_name_tso <- paste0("tso_", col_name)
  
  ts_data <- get(ts_name)
  
  # Try ARIMA models
  arima_fit <- tryCatch({
    fit <- tso(ts_data)
    cat("\033[1m\033[31m", "TSO for", col_name, ":\033[0m\n")
    print(fit)
    assign(ts_name_tso, fit)
    assign(ts_name, fit$yadj)
    
    # Vérifier s'il y a des points atypiques avant d'afficher le graphique
    if (!is.null(fit$outliers) && nrow(fit$outliers) > 0) {
      plot(fit)
      title(main = paste("TSO for", col_name))
    }
    
    TRUE
  }, error = function(e) {
    cat("\033[1m\033[31m", "Error for", col_name, ":\033[0m\n")
    cat(e$message, "\n")
    FALSE
  })
  
  # If ARIMA adjustment fail, go next
  if (!arima_fit) next
  
}


# Seasonal Adjustment----
seasonal_combined_test <- c()
seasonal_seasdum <- c()

for (col_name in names(ts_list)) {
  ts_name <- paste0("ts_", col_name)  # Nom de la série temporelle ajustée
  ts_data <- get(ts_name)  # Récupérer les données de la série temporelle ajustée
  
  # Appliquer le test combined_test() sur la série temporelle ajustée
  ct_res <- combined_test(ts_data)
  
  # Vérifier chaque p-value et afficher les résultats si au moins une est < 0.05
  ct_pvals <- ct_res$Pval
  if (any(ct_pvals < 0.05)) {
    print(paste0("Résultats du combined_test pour la série ", ts_name))
    print(ct_res)
    seasonal_combined_test <- c(seasonal_combined_test, ts_name)
  }
  
  # Appliquer le test seasdum() sur la série temporelle ajustée
  sd_res <- seasdum(ts_data)
  
  # Vérifier la p-value et afficher les résultats si elle est < 0.05
  if (sd_res$Pval < 0.05) {
    print(paste0("Résultats du seasdum pour la série ", ts_name))
    print(sd_res)
    seasonal_seasdum <- c(seasonal_seasdum, ts_name)
  }
}

# Afficher les listes des séries présentant de la saisonnalité
cat("\nSéries présentant de la saisonnalité selon combined_test:\n")
print(seasonal_combined_test)
length(seasonal_combined_test)

cat("\nSéries présentant de la saisonnalité selon seasdum:\n")
print(seasonal_seasdum)
length(seasonal_seasdum)


# Calculer les différences entre les deux listes
only_combined_test <- setdiff(seasonal_combined_test, seasonal_seasdum)
only_seasdum <- setdiff(seasonal_seasdum, seasonal_combined_test)
both_tests <- intersect(seasonal_combined_test, seasonal_seasdum)

# Afficher les différences
cat("\nSéries avec saisonnalité seulement selon combined_test:\n")
print(only_combined_test)

cat("\nSéries avec saisonnalité seulement selon seasdum:\n")
print(only_seasdum)

cat("\nSéries avec saisonnalité selon les deux tests:\n")
print(both_tests)



# Correction de la saisonnalité avec STL pour les séries dans both_tests
for (ts_name in both_tests) {
  ts_data <- get(ts_name)  # Récupérer les données de la série temporelle
  decomp <- stl(ts_data[,1], s.window = "periodic")  # Décomposition STL
  seasonal <- decomp$time.series[, "seasonal"]  # Obtenir la composante saisonnière
  ts_data_adjusted <- ts_data - seasonal  # Corriger la composante saisonnière
  assign(ts_name, ts_data_adjusted)  # Mettre à jour la série temporelle ajustée
}




# Vérification de la saisonnalité après correction
seasonal_combined_test_after <- c()
seasonal_seasdum_after <- c()

for (ts_name in both_tests) {
  ts_data <- get(ts_name)  # Récupérer les données de la série temporelle ajustée
  
  # Appliquer le test combined_test() sur la série temporelle ajustée
  ct_res <- combined_test(ts_data)
  
  # Vérifier chaque p-value et afficher les résultats si au moins une est < 0.05
  ct_pvals <- ct_res$Pval
  if (any(ct_pvals < 0.05)) {
    cat("\033[1m\033[31m", "Résultats du combined_test après correction pour la série ", ts_name, ":\033[0m\n")
    print(ct_res)
    seasonal_combined_test_after <- c(seasonal_combined_test_after, ts_name)
  }
  
  # Appliquer le test seasdum() sur la série temporelle ajustée
  sd_res <- seasdum(ts_data)
  
  # Vérifier la p-value et afficher les résultats si elle est < 0.05
  if (sd_res$Pval < 0.05) {
    cat("\033[1m\033[31m", "Résultats du seasdum après correction pour la série ", ts_name, ":\033[0m\n")
    print(sd_res)
    seasonal_seasdum_after <- c(seasonal_seasdum_after, ts_name)
  }
}

# Afficher les listes des séries présentant encore de la saisonnalité après correction
cat("\nSéries présentant encore de la saisonnalité selon combined_test après correction:\n")
print(seasonal_combined_test_after)

cat("\nSéries présentant encore de la saisonnalité selon seasdum après correction:\n")
print(seasonal_seasdum_after)



