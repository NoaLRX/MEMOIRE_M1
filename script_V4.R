# Chargement des packages----
library(tseries)
library(tsoutliers)
library(ggplot2)
library(moments)
library(seastests)
library(TSA)
library(RJDemetra)
library(ggcorrplot)
library(gets)
library(leaps)
library(olsrr)
library(forecast)
library(tidyr)
library(dplyr)
library(ggdist)
library(ggridges)

# Importation & Manipulation de la BDD----
df <- read.csv2("DATA/BDD_1999_V6.csv")
df$Food <- as.numeric(df$Food)
y0 <- ts(data = df$Food, start=c(1999,01),frequency=12)

# Matrice des corrélations 

corr_matrix <- cor(df[,2:14])
print(corr_matrix)

ggcorrplot(corr_matrix, 
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("#6D9EC1", "white", "#E46726"),
           title = "Matrice de corrélation") +
  scale_fill_gradient2(low = "#53A847", mid = "#FFFFFF", high = "#6D9EC1", midpoint = 0,
                       limits = c(-1,1), name = "Corrélation", guide = "colorbar")

# Traitement variable Y----

## Correction pts atypiques----
tso(y0) # on utilise la méthode TSO pour la détection de pts atypiques
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
# Pour les deux test,  p-value > 0.05 donc H0 absence de saisonnalité

## Stationnarisation----
y1 <- diff(y_corr) # On effectue une différenciation
adf.test(y1) # série stationnaire

## Re-points atypiques----
tso(y1)
fit <- tso(y1)
plot(fit)
y1adj <- fit$yadj
write(t(y1adj),file="adjusted-series.csv",ncolumn=1,append=FALSE)

# Notre série corrigée et ajustée:
y_corr <-  y1adj 
periodogram(y_corr)

# Traitement variables X----
# Boucle pour transformer en série temporelle les variables x
df1 <- df[3:15]
for (i in 1:13) {
  ts_name <- paste0("x", i) 
  col_name <- names(df1)[i]  
  assign(ts_name, ts(data = df1[,col_name], start=c(1999,01), frequency=12))
}


# Correction des points atypiques----
for (i in 1:13) {
  ts_name <- paste0("x", i)  # Nom de la série temporelle
  ts_name_tso <- paste0("xtso", i)  # Nom de la nouvelle série temporelle
  
  fit <- tso(get(ts_name))  # Appliquer la fonction tso()
  
  # Imprimer un message avec le nom de la série temporelle
  cat("\033[1m\033[31m", "TSO pour", ts_name, ":\033[0m\n")
  print(fit)  # Imprimer les informations de fit
  
  assign(ts_name_tso, fit)  # Stocker le résultat
  assign(ts_name, fit$yadj)  # Stocker la série temporelle ajustée
  
  # Tracer le graphique
  plot(fit)
  title(main = paste("TSO pour", ts_name))
}


## Désaisonalisation----
for (i in 1:13) {
  ts_name <- paste0("x", i)  # Nom de la série temporelle
  ts_data <- get(ts_name)  # Récupérer les données de la série temporelle
  ct_res <- combined_test(ts_data)  # Appliquer le test combined_test() sur la série temporelle
  sd_res <- seasdum(ts_data)  # Appliquer le test seasdum() sur la série temporelle
  print(paste0("Résultats pour la série ", ts_name))
  print(ct_res)  # Afficher les résultats du test combined_test()
  print(sd_res)  # Afficher les résultats du test seasdum()
}

# RESULTAT : Séries présentant de la saisonnalité (p < 0.05) = X3,X5 X6,X9,X10,X12
# On va donc corriger la saisonnalité de ces séries a l'aide de la méthode STL

# Pour x3:
decomp_x3 <- stl(x3, s.window="periodic")
# Obtenir la composante saisonnière de la série
seasonal_x3 <- decomp_x3$time.series[, "seasonal"]
# Corriger la composante saisonnière de la série
x3 <- x3 - seasonal_x3

#Pour x5, x6, x9, x10 et x12:
decomp_x5 <- stl(x5, s.window="periodic")
seasonal_x5 <- decomp_x5$time.series[, "seasonal"]
x5 <- x5 - seasonal_x5
decomp_x6 <- stl(x6, s.window="periodic")
seasonal_x6 <- decomp_x6$time.series[, "seasonal"]
x6 <- x6 - seasonal_x6
decomp_x9 <- stl(x9, s.window="periodic")
seasonal_x9 <- decomp_x9$time.series[, "seasonal"]
x9 <- x9 - seasonal_x9
decomp_x10 <- stl(x10, s.window="periodic")
seasonal_x10 <- decomp_x10$time.series[, "seasonal"]
x10 <- x10 - seasonal_x10
decomp_x12 <- stl(x12, s.window="periodic")
seasonal_x12 <- decomp_x12$time.series[, "seasonal"]
x12 <- x12 - seasonal_x12


# Vérification avec les test :
combined_test(x1) 
seasdum(x1)
combined_test(x2) 
seasdum(x2)
combined_test(x3) #  p-value > 0.05 donc H0 absence de saisonnalité
seasdum(x3) # p-value > 0.05 donc H0 absence de saisonnalité
combined_test(x4) 
seasdum(x4)
combined_test(x5) # p-value > 0.05 donc H0 absence de saisonnalité
seasdum(x5) # p-value > 0.05 donc H0 absence de saisonnalité
combined_test(x6) # p-value > 0.05 donc H0 absence de saisonnalité
seasdum(x6) # p-value > 0.05 donc H0 absence de saisonnalité
combined_test(x7) 
seasdum(x7)
combined_test(x8) 
seasdum(x8)
combined_test(x9) # p-value > 0.05 donc H0 absence de saisonnalité
seasdum(x9) # p-value > 0.05 donc H0 absence de saisonnalité
combined_test(x10) 
seasdum(x10)
combined_test(x11) 
seasdum(x11)
combined_test(x12) # p-value > 0.05 donc H0 absence de saisonnalité
seasdum(x12) # p-value > 0.05 donc H0 absence de saisonnalité
combined_test(x13) # p-value > 0.05 donc H0 absence de saisonnalité
seasdum(x13) # p-value > 0.05 donc H0 absence de saisonnalité

# Les séries sont corrigées de toute saisonnalité

## Stationnarisation----

for (i in 1:13) {
  ts_name <- paste0("x", i)  # Nom de la série temporelle
  adf_result <- adf.test(get(ts_name))  # Appliquer le test ADF
  adf_result
  if (adf_result$p.value > 0.05) {  # Vérifier la significativité statistique
    assign(ts_name, diff(get(ts_name)))  # Différencier la série temporelle
    print(paste("La série", ts_name, "a été différenciée"))
  }
}
# Toutes les séries sont stationnaires
adf.test(y_corr)
adf.test(x1)
adf.test(x2)
adf.test(x3)
adf.test(x4)
adf.test(x5)
adf.test(x6)
adf.test(x7)
adf.test(x8)
adf.test(x9)
adf.test(x10)
adf.test(x11)
adf.test(x12)
adf.test(x13)


# Re-Points atypiques
for (i in 1:13) {
  ts_name <- paste0("x", i)  # Nom de la série temporelle
  ts_name_tso <- paste0("xtso", i)  # Nom de la nouvelle série temporelle
  assign(ts_name_tso, tso(get(ts_name)))  # Appliquer la fonction tso() et stocker le résultat
  assign(ts_name, get(ts_name_tso)$yadj)  # Stocker la série temporelle ajustée
}

for (i in 1:13) {
  ts_name <- paste0("x", i)  # Nom de la série temporelle
  print(paste("La longueur de", ts_name, "est", length(get(ts_name))))
}




# Créer un dataframe à partir des séries temporelles x1, x2, ..., x12
# Suppression de la première ligne de x6 pour avoir un dataframe avec le même nombre de ligne 
x6 <- window(x6, start = time(x6)[2])

# Création d'un DF général----
df <- data.frame(y_corr,x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
colnames(df) <- c("Food","Fertil", "GPRH", "Metal", "Oil", "Raw_Material", "Temperature", 
                  "VIX", "EURUSD", "Meat", "Vege_Oil", "Wheat", "Sugar")

# Graphique des séries corrigées----
ggplot(df, aes(x = seq_along(Fertil), y = Fertil)) +
  geom_line(color = "red", size = 1, alpha = 0.7)+
  labs( x = "Temps", y = "Fertil", color = "Modèles") +
  theme_minimal()

ggplot(df, aes(x = seq_along(GPRH), y = GPRH)) +
  geom_line(color = "turquoise4", size = 1, alpha = 0.7)+
  labs( x = "Temps", y = "GPRH", color = "Modèles") +
  theme_minimal()

ggplot(df, aes(x = seq_along(Metal), y = Metal)) +
  geom_line(color = "orange", size = 1, alpha = 0.7)+
  labs( x = "Temps", y = "Metal", color = "Modèles") +
  theme_minimal()

ggplot(df, aes(x = seq_along(Oil), y = Oil)) +
  geom_line(color = "purple", size = 1, alpha = 0.7)+
  labs( x = "Temps", y = "Oil", color = "Modèles") +
  theme_minimal()

ggplot(df, aes(x = seq_along(Raw_Material), y = Raw_Material)) +
  geom_line(color = "seagreen3", size = 1, alpha = 0.7)+
  labs( x = "Temps", y = "Raw Material", color = "Modèles") +
  theme_minimal()

ggplot(df, aes(x = seq_along(Temperature), y = Temperature)) +
  geom_line(color = "royalblue", size = 1, alpha = 0.7)+
  labs( x = "Temps", y = "Temperature", color = "Modèles") +
  theme_minimal()

ggplot(df, aes(x = seq_along(VIX), y = VIX)) +
  geom_line(color = "hotpink", size = 1, alpha = 0.7)+
  labs( x = "Temps", y = "VIX", color = "Modèles") +
  theme_minimal()

ggplot(df, aes(x = seq_along(EURUSD), y = EURUSD)) +
  geom_line(color = "olivedrab3", size = 1, alpha = 0.7)+
  labs( x = "Temps", y = "EURUSD", color = "Modèles") +
  theme_minimal()

ggplot(df, aes(x = seq_along(Vege_Oil), y = Vege_Oil)) +
  geom_line(color = "antiquewhite3", size = 1, alpha = 0.7)+
  labs( x = "Temps", y = "Vege Oil", color = "Modèles") +
  theme_minimal()

ggplot(df, aes(x = seq_along(Wheat), y = Wheat)) +
  geom_line(color = "lightcoral", size = 1, alpha = 0.7)+
  labs( x = "Temps", y = "Wheat", color = "Modèles") +
  theme_minimal()

ggplot(df, aes(x = seq_along(Sugar), y = Sugar)) +
  geom_line(color = "gray37", size = 1, alpha = 0.7)+
  labs( x = "Temps", y = "Sugar", color = "Modèles") +
  theme_minimal()

ggplot(data = y_corr, aes(x = "", y = y_corr)) +
  stat_boxplot(geom = "errorbar", width = 0.15, color = "black") +
  geom_boxplot(fill = "dodgerblue", alpha = 0.75, color = "black", outlier.color = "firebrick1") +
  geom_jitter(color = "black", size = 0.4, alpha = 0.4, width = 0.1) +
  theme_classic() +
  labs(y = "Food", x = "")


ggplot(data = y_corr, aes(x = y_corr)) +
  geom_histogram(fill = "dodgerblue", color = "black") +
  labs(y = "Fréquence", x="Indice des prix alimentaires") +
  theme_classic()




# Sélection des variables----
## Méthode BestSubSet----
#test_y <- as.numeric(test_y)
train_size <- floor(0.8 * nrow(df))
train <- df[1:train_size, ]

leaps <- regsubsets(Food ~ .,data= train, nbest=1, method=c("exhaustive"))
leaps
res.sum <- summary(leaps) 
data.frame(Adj.R2=which.max(res.sum$adjr2),
           CP=which.min(res.sum$cp),
           BIC=which.min(res.sum$bic))

#  plot  a  table  of  models  showing  variables  in  each  model 
plot(leaps,scale="adjr2",main="Adjusted R2")
plot(leaps,scale="Cp",main="Critère de Mallow's Cp")
plot(leaps, scale = "bic", main = "BIC")
# Variables sélectionnées: Metal, Raw, Vege, EURUSD, Wheat, Sugar


## Méthode GET avec gets---- 
mX <- data.matrix(train[, 2:13])
modele_arx <- arx(train$Food, mc = TRUE, ar = 1, mxreg = mX, vcov.type = "ordinary")
modele_arx
seuil_p_value <- 0.05
variables <- colnames(train[, 2:13])
VRAI <- TRUE
while (VRAI) {
  mX <- data.matrix(train[, variables])
  modele_arx <- arx(train$Food, mc = TRUE, ar = 1, mxreg = mX, vcov.type = "ordinary")
  
  p_values <- modele_arx[["mean.results"]][["p-value"]][-c(1, 2)] # Exclure la constante et AR(1) du calcul des p-values
  max_p_value <- max(p_values)
  
  if (max_p_value > seuil_p_value) {
    variable_a_supprimer <- variables[which.max(p_values)]
    variables <- setdiff(variables, variable_a_supprimer)
  } else {
    VRAI <- FALSE
  }
}
arx_final <- arx(train$Food, mc = TRUE, ar = 1, mxreg = mX, vcov.type = "ordinary")
modele_gets <- getsm(arx_final) # Avoir les coeff du modele ARX  + lunchbox test
modele_gets
# La variable Métal n'est pas sélectionnée par la méthode Get


# Exportation de la BDD final traitée----
#df_clean <- df[, c("Food", "Metal","Raw_Material","Vege_Oil", "Wheat", "Sugar")]
df_clean <- df[, c("Food", "Metal","Raw_Material","Vege_Oil", "Wheat", "Sugar")]
#write.csv2(df_clean, file="DATA/df_clean_vfinal.csv")


# Modele econométriques----

# Création de DF pour les modèles économétriques----
#df_opti <- df_clean[, c("Metal","Raw_Material","Vege_Oil", "Wheat", "Sugar")]
df_opti <- df_clean[, c("Metal","Raw_Material","Vege_Oil", "Wheat", "Sugar")]
df_opti <- data.frame(df_opti) # Df général
df_opti_ts <- ts(df_opti) # Df général sous forme de séries temporelles (ts)
#df_opti_ts_train <- (head(df_opti_ts, -12)) # base de TRAINING sous forme ts
#df_opti_test <- tail(df_opti, n = 12) # base de TEST
#df_opti_train <- head(df_opti, -12) # base de TRAIN
y_real <- tail(df_clean$Food, n=57)
y_real <- as.numeric(y_real)


df_clean <- read.csv2("DATA/df_clean_vfinal.csv")
# Modèles économétriques----

## Modele ARX avec GETS----
library(gets)
set.seed(829)
n <- nrow(df_clean)
train_size <- round(0.8 * n)
train_data <- df_clean[1:train_size, ]
test_data <- df_clean[(train_size+1):n, ]
mX_train <- data.matrix(train_data[,-1])  # toutes les colonnes sauf 'Food'
mX_test <- data.matrix(test_data[,-1])  # toutes les colonnes sauf 'Food'
y_train <- train_data$Food
y_test <- test_data$Food
model <- arx(y_train, mc = TRUE, ar = 1, mxreg = mX_train, vcov.type = "ordinary")
n_test <- nrow(test_data)
p_arxget <- predict(model, n.ahead = n_test, newmxreg = mX_test)
p_arxget <- as.numeric(p_arxget)


# Représentation graphique
plot(y_real, type = "l", col = "black", lwd = 2, ylim = range(c(y_real)))
# Ajouter les prévisions au graphique
lines(p_arxget, col = "red", lwd = 2)

rmse <- sqrt(mean((y_real - p_arxget)^2, na.rm = TRUE))
print(paste("RMSE :", rmse)) # 1.71



## Modele ARX avec auto_arima----
set.seed(123)
split <- round(nrow(df_clean) * 0.8)
train_df <- df_clean[1:split, ]
test_df <- df_clean[(split+1):nrow(df_clean), ]
y_train <- train_df$Food
xreg_train <- data.matrix(train_df[,-1]) # Exclure la première colonne qui est la variable dépendante
modelx_train <- auto.arima(y_train, max.q = 0, xreg = xreg_train, seasonal = FALSE, stationary = TRUE)
xreg_test <- data.matrix(test_df[,-1]) # Exclure la première colonne qui est la variable dépendante
p_arx <- predict(modelx_train, newxreg = xreg_test, n.ahead = nrow(test_df))$pred
p_arx <- as.numeric(p_arx)

plot(y_real, type = "l", col = "black", lwd = 2, ylim = range(c(y_real)))
lines(p_arxget, col="red")
lines(p_arx, col = "green",lwd=2)

rmse <- sqrt(mean((y_real - p_arx)^2, na.rm = TRUE))
print(paste("RMSE :", rmse)) # 1.97



## Modele ARMAX avec auto_arima----
set.seed(123)
split <- round(nrow(df_clean) * 0.8)
train_df <- df_clean[1:split, ]
test_df <- df_clean[(split+1):nrow(df_clean), ]
y_train <- train_df$Food
xreg_train <- data.matrix(train_df[,-1]) # Exclure la première colonne qui est la variable dépendante
modelx_train <- auto.arima(y_train, xreg = xreg_train, seasonal = FALSE, stationary = TRUE)
j <- ncol(modelx_train$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
  tstat[i,1] <- modelx_train$coef[i]/sqrt(modelx_train$var.coef[i,i])
}
tstat
xreg_test <- data.matrix(test_df[,-1]) # Exclure la première colonne qui est la variable dépendante
p_armax <- predict(modelx_train, newxreg = xreg_test, n.ahead = nrow(test_df))$pred
p_armax <- as.numeric(p_armax)
p_armax

plot(y_real, type = "l", col = "black",lwd=2, ylim = range(c(y_real)))
lines(p_arxget, col="red")
lines(p_arx, col = "green")
lines(p_armax, col = "purple",lwd=2)

rmse <- sqrt(mean((y_real - p_armax)^2, na.rm = TRUE))
print(paste("RMSE :", rmse)) # 1.98



## Naive model----
y_train <- ts(train_df$Food)
naive_model <- rwf(y_train, h=nrow(test_df))
p_naive <- naive_model$mean
p_naive <-  as.numeric(p_naive)

plot(y_real, type = "l", col = "black",lwd=3, ylim = range(c(y_real)))
lines(p_arxget, col="red")
lines(p_arx, col = "green")
lines(p_armax, col = "purple")
lines(p_naive, col = "grey",lwd=3)

rmse <- sqrt(mean((y_real - p_naive)^2, na.rm = TRUE))
print(paste("RMSE :", rmse)) # 2.25



## Model LM----
set.seed(123)
split <- round(nrow(df_clean) * 0.8)
train_df <- df_clean[1:split, ]
test_df <- df_clean[(split+1):nrow(df_clean), ]
model_lm <- lm(Food ~ Metal + Raw_Material + Vege_Oil + Wheat + Sugar, data = train_df)
p_lm <- predict(model_lm, newdata = test_df)
p_lm <- as.numeric(p_lm)

plot(y_real, type = "l", col = "black",lwd=3, ylim = range(c(y_real)))
lines(p_arxget, col="red")
lines(p_arx, col = "green")
lines(p_armax, col = "purple")
lines(p_naive, col = "grey")
lines(p_lm, col = "yellow",lwd=3)

rmse <- sqrt(mean((y_real - p_lm)^2, na.rm = TRUE))
print(paste("RMSE :", rmse)) # 2.03



## Model AR1---- 
y <- ts(df_clean$Food)
model_ar1 <- Arima(y, order = c(1, 0, 0), seasonal = list(order = c(0, 0, 0), period=12),lambda=1)
forecast_ar1 <- forecast(model_ar1, h = 57)
p_ar1 <- forecast_ar1$mean
p_ar1 <- as.numeric(p_ar1)

plot(y_test, type = "l", col = "black",lwd=3, ylim = range(c(y_test)))
lines(p_arxget, col="red")
lines(p_arx, col = "green")
lines(p_armax, col = "purple")
lines(p_naive, col = "grey")
lines(p_lm, col = "yellow")
lines(p_ar1, col = "pink",lwd=3)

rmse <- sqrt(mean((y_real - p_ar1)^2, na.rm = TRUE))
print(paste("RMSE :", rmse)) # 2.20



## Model GAM----
library(mgcv)
smp_size <- ceiling(0.80 * nrow(df_clean))
set.seed(113) # pour la reproductibilité
train_ind <- sample(seq_len(nrow(df_clean)), size = smp_size)
train <- df_clean[train_ind, ]
test <- df_clean[-train_ind, ]
model_gam <- gam(Food ~ s(Metal) + s(Raw_Material) + s(Vege_Oil) + s(Wheat) + s(Sugar), data = train)
p_gam <- predict(model_gam, newdata = test)
p_gam <- as.numeric(p_gam)

plot(y_test, type = "l", col = "black",lwd=2, ylim = range(c(y_test)))
lines(p_arxget, col="red")
lines(p_arx, col = "green")
lines(p_armax, col = "purple")
lines(p_naive, col = "grey")
lines(p_lm, col = "yellow")
lines(p_ar1, col = "pink")
lines(p_gam, col="cyan",lwd=2)

rmse <- sqrt(mean((y_real - p_gam)^2, na.rm = TRUE))
print(paste("RMSE :", rmse)) # 2.44


# Machine-Learning----
## Modele MLP----
library(neuralnet)
set.seed(123) # On fixe la graine du générateur de nombres aléatoires (qui joue un role dans le poids des neurones)
n <- nrow(df_clean)
train_size <- round(0.8 * n)
train <- df_clean[1:train_size, ]
test <- df_clean[(train_size+1):n, ]
mlp_model <- neuralnet(Food ~ Metal + Raw_Material + Vege_Oil + Wheat + Sugar, data = train, hidden = 2)
p_mlp <- compute(mlp_model, test[,-1])
p_mlp <- as.numeric(p_mlp$net.result)

plot(y_real,type = "l", col = "black",lwd=2, ylim = range(c(y_real)))
lines(p_mlp, col = "brown")
rmse <- sqrt(mean((y_real - p_mlp)^2, na.rm = TRUE))
print(paste("RMSE :", rmse)) # 1.90



## Modele MARS----
library(earth)
n <- nrow(df_clean)
train_size <- round(0.8 * n)
train <- df_clean[1:train_size, ]
test <- df_clean[(train_size+1):n, ]
mars_model <- earth(Food ~ ., data = train)
p_mars <- predict(mars_model, newdata = test)
p_mars <- as.numeric(p_mars)

plot(y_test, type = "l", col = "black",lwd=2, ylim = range(c(-4,7)))
lines(p_mlp, col = "brown")
lines(p_mars, col = "cyan",lwd=2)

rmse <- sqrt(mean((y_real - p_mars)^2, na.rm = TRUE))
print(paste("RMSE :", rmse)) # 2.00



## Modèle SVM----
library(e1071)
library(caret)
# Trouver les hyper-paramètres optimaux
#svm_grid <- expand.grid(C = c(0.1, 1, 10), sigma = c(0.001, 0.01, 0.1))
#svm_model_opt <- train(Food ~ ., data = train, method = "svmRadial", trControl = tr_control, preProcess = c("center", "scale"), tuneGrid = svm_grid)
#print(svm_model_opt) # On obtient sigma = 0.001 et C = 10.

svm_model_best <- svm(Food ~ ., data = train, kernel = "radial", gamma = 0.1, cost = 0.35, scale = TRUE)
p_svm <- predict(svm_model_best, newdata = test)
p_svm <- as.numeric(p_svm)

rmse <- sqrt(mean((y_real - p_svm)^2))
print(paste("RMSE :", rmse)) # 1.96

plot(y_test, type = "l", col = "black",lwd=2, ylim = range(c(-4,7)))
lines(p_mlp, col = "brown")
lines(p_mars, col = "cyan")
lines(p_svm, col = "darkgreen",lwd=2)



## Modèle Random Forest ----
library(randomForest)
set.seed(2)
n <- nrow(df_clean)
train_size <- round(0.8 * n)
train <- df_clean[1:train_size, ]
test <- df_clean[(train_size+1):n, ]
rf_model <- randomForest(Food ~ ., data = train, ntree=200)
p_rf <- predict(rf_model, newdata = test)

rmse <- sqrt(mean((y_real - p_rf)^2))
print(paste("RMSE :", rmse)) # 1.89

plot(y_test, type = "l", col = "black",lwd=2, ylim = range(c(-4,7)))
lines(p_mlp, col = "brown")
lines(p_mars, col = "cyan")
lines(p_svm, col = "darkgreen")
lines(p_rf, col = "bisque",lwd=4)



## Modèle XGB ----
library(xgboost)
### Optimisation
n <- nrow(df_clean)
train_size <- round(0.8 * n)
train <- df_clean[1:train_size, ]
test <- df_clean[(train_size+1):n, ]
xgb_grid <- expand.grid(nrounds = c(100, 200),
                        eta = c(0.01, 0.1),
                        max_depth = c(6, 10),
                        gamma = c(0, 1),
                        colsample_bytree = c(0.6, 0.8, 1),
                        min_child_weight = c(1, 5),
                        subsample = c(0.5, 0.75, 1))
tr_control <- trainControl(method = "cv", number = 10)
xgb_model <- train(Food ~ ., data = train, method = "xgbTree",
                   trControl = tr_control, tuneGrid = xgb_grid)
print(xgb_model$bestTune)
#xgb_model <- xgb.train(params = params, data = train_matrix, nrounds = 100)
#p_xgb <- predict(xgb_model, newdata = test_matrix)

### Modèle Optimal
set.seed(829)
n <- nrow(df_clean)
train_size <- round(0.8 * n)
train <- df_clean[1:train_size, ]
test <- df_clean[(train_size+1):n, ]
dtrain <- xgb.DMatrix(data = as.matrix(train[,-1]), label = train$Food)
dtest <- xgb.DMatrix(data = as.matrix(test[,-1]), label = test$Food)
params <- list(booster = "gbtree", objective = "reg:squarederror", 
               eta = 0.01, gamma = 1, max_depth = 10, min_child_weight = 5, 
               subsample = 0.5, colsample_bytree = 1)
xgb_model_opt <- xgb.train(params = params, data = dtrain, nrounds = 200)
p_xgb <- predict(xgb_model_opt, newdata = dtest)
p_xgb <- as.numeric(p_xgb)

rmse <- sqrt(mean((y_real - p_xgb)^2))
print(paste("RMSE :", rmse)) # 1.89

plot(y_test, type = "l", col = "black",lwd=2, ylim = range(c(-4,7)))
lines(p_mlp, col = "brown")
lines(p_mars, col = "cyan")
lines(p_svm, col = "darkgreen")
lines(p_rf, col = "bisque")
lines(p_xgb, col = "magenta",lwd=3)



## Modèle kNN ----
library(class)
set.seed(123)
total_rows <- nrow(df_clean)
train_rows <- round(0.8 * total_rows)
train_set <- df_clean[1:train_rows, ]
test_set <- df_clean[(train_rows + 1):total_rows, ]
train_labels <- train_set$Food
train_data <- train_set[, -1]
test_labels <- test_set$Food
test_data <- test_set[, -1]
### Optimisation ----
k_grid <- expand.grid(k = seq(1, 100, by = 1))
trControl <- trainControl(method = "cv", number = 10)
knn_model <- train(Food ~ ., data = train, method = "knn", tuneGrid = k_grid, trControl = trControl)
results <- knn_model$results
min_rmse <- min(results$RMSE, na.rm = TRUE)
print(min_rmse) # 1.97

knn_model <- train(Food ~ ., data = train_set, method = "knn", trControl = trControl, tuneGrid = expand.grid(k = 8))
p_knn <- predict(knn_model, newdata = test_data)
p_knn <- as.numeric(p_knn)

rmse <- sqrt(mean((y_real - p_knn)^2))
print(paste("RMSE :", rmse)) # 2.07


plot(y_test, type = "l", col = "black",lwd=2, ylim = range(c(-4,7)))
lines(p_mlp, col = "brown")
lines(p_mars, col = "cyan")
lines(p_svm, col = "darkgreen")
lines(p_rf, col = "bisque")
lines(p_xgb, col = "magenta")
lines(p_knn, col = "deeppink1",lwd=2)


## Modèle LSTM----
# Ce modèle a été réalisé dans Python (voir script_python)
# Voici les résultats
p_lstm <- c(-0.25294527, 0.1913026, 0.19904342, -0.8268727, -1.011227, -0.39776492, -0.02392626, 0.39662206, -0.6282057,
             -1.1094687, 0.5260734, -0.27537918, 0.71199733, -0.72112125, -0.06165266, 0.42729592, -0.32099512, -1.6836557,
             1.7716432, 0.70828897, 0.9745996, 1.648176, -0.82380736, -2.2641864, -2.5867476, -1.8876078, 1.2354914,
             2.109305, 2.9924333, 2.9692073, 1.0942109, 1.2744782, 2.3110309, 3.50036, 1.1870283, 2.5313935, 0.01085046,
             3.6764412, 2.58707, 0.752668, 0.7087133, -0.13846439, 0.11906552, 2.568086, -1.6623497, -0.4145909, 3.147168,
             2.1032424, 1.8134997, -0.12308434, -2.4711754, -1.7596309, -2.1869128, 0.9053879, -2.7402458, -1.2519723,
             1.836103)

rmse <- sqrt(mean((y_real - p_lstm)^2))
print(paste("RMSE :", rmse)) # 1.87


# Modèle LSTM-CNN----
# Ce modèle a été réalisé dans Python (voir script_python)
p_lstmCNN <- c(0.9445525, -0.93539023, 0.1849516, -0.5293375, 0.25189877, -0.01561911, 0.5863977, -0.7480756, -1.245021, -0.41833475, -0.382944, -0.4456981, -0.5368668, 0.2550073, 0.6520901, -0.4694371, -0.7398653, 2.4111953, 1.0097945, 1.1524918, 3.8888113, -1.0083015, -3.4491544, -1.2592658, -2.023909, -0.11957505, 2.1137626, 4.4485235, 3.6021113, 1.7200509, 1.4683067, 1.1919237, 4.8188577, 1.0666952, 4.31144, 0.45934787, 3.7395496, 2.0674832, 0.878674, 0.9505762, -0.6655151, 0.40474814, 3.820584, -1.0126293, 0.76504004, 4.6035447, 3.697545, 1.6867605, 0.620801, -0.837868, -2.5649328, -1.3666286, 0.28617516, -1.1662878, -1.1604571, 0.373197, 1.4121791)

length(p_lstmCNN)
str(p_lstmCNN)



rmse <- sqrt(mean((y_real - p_lstmCNN)^2))
print(paste("RMSE :", rmse)) # 1.96

plot(y_test, type = "l", col = "black",lwd=3, ylim = range(c(-4,7)))
lines(p_mlp, col = "brown")
lines(p_mars, col = "cyan")
lines(p_svm, col = "darkgreen")
lines(p_rf, col = "bisque")
lines(p_xgb, col = "magenta")
lines(p_knn, col = "deeppink1")
lines(p_knn, col = "orange",lwd=3)

# Modèle TDNN----
p_tdnn <- c(0.23640978, -0.05492803, -0.37495565, -0.3676901, -0.34934837, -0.11685323, -0.10121601, -0.58172035, -0.6125661, 0.26304865, -0.00501727, 0.27397847, -0.7419882, 0.36224532, 0.52654403, -0.09609937, -0.95655847, 1.8427551, 0.46310726, 0.6947826, 1.1441402, -1.3422017, -1.5597467, -1.5131004, -1.3522925, 1.2792006, 2.216979, 3.3249633, 2.9310074, 1.2521439, 1.0720325, 2.0374565, 4.2119513, 1.2165804, 2.0431392, 0.50275517, 4.007904, 2.9724317, 0.57495195, 0.5935444, -1.0560797, -0.08553259, 3.4639356, -1.4353561, -0.20790346, 3.4078264, 2.7170796, 1.8351623, 0.02346325, -2.4843822, -1.0233372, -1.4843063, 1.1749034, -2.3184998, -0.94866514, 1.305893, 2.2971244)
length(p_tdnn)
str(p_tdnn)

rmse <- sqrt(mean((y_real - p_tdnn)^2))
print(paste("RMSE :", rmse)) # 2.07


# Créer un dataframe à partir des vecteurs

prev_df <- data.frame(
  ARMAX = p_armax,
  ARX = p_arx,
  LM = p_lm,
  NAIVE = p_naive,
  ARX_GET = p_arxget,
  AR1 = p_ar1,
  GAM = p_gam,
  LM = p_lm,
  MLP = p_mlp,
  MARS = p_mars,
  SVM = p_svm,
  RF = p_rf,
  XGB = p_xgb,
  kNN = p_knn,
  LSTM = p_lstm,
  LSTM_CNN = p_lstmCNN,
  TDNN = p_tdnn,
  y_real = y_real
)


# Graphique des prévisions----

prev_df$id <- seq.int(nrow(prev_df))

## Graphique économétrique----
ggplot(prev_df, aes(x=id, group = 1)) +
  geom_line(aes(y = y_real, color = "Observé"), size = 2, show.legend = TRUE) +
  geom_line(aes(y = ARMAX, color = "ARMAX"), size = 1, alpha = 0.7) +
  geom_line(aes(y = ARX, color = "ARX"), size = 1, alpha = 0.7) +
  geom_line(aes(y = LM, color = "LM"), size = 1, alpha = 0.7) +
  geom_line(aes(y = NAIVE, color = "NAIVE"), size = 1, alpha = 0.7) +
  geom_line(aes(y = ARX_GET, color = "ARX_GETS"), size = 1, alpha = 0.7) +
  geom_line(aes(y = AR1, color = "AR1"), size = 1, alpha = 0.7) +
  scale_color_manual(values = c("Observé" = "black", "ARMAX" = "purple", "ARX" = "green", 
                                "LM" = "darkblue", "NAIVE" = "orange", "ARX_GETS" = "red", "AR1" = "cyan")) +
  labs( x = "Temps", y = "Food Price Forecast", color = "Modèles") +
  scale_y_continuous(limits = c(-6, 8), breaks = seq(-4, 6, 2)) +
  theme_minimal()

ggplot(prev_df, aes(x=id, group = 1)) +
  geom_line(aes(y = y_real, color = "Observé"), size = 2, show.legend = TRUE) +
  geom_line(aes(y = ARX_GET, color = "ARX_GETS"), size = 1)+
  geom_line(aes(y = LSTM, color = "LSTM"), size = 1)+
  geom_line(aes(y = XGB, color = "XGB"), size = 1)+
  scale_color_manual(values = c("Observé" = "black", "ARX_GETS" = "red","LSTM"="blue","XGB"="green3")) +
  labs( x = "Temps", y = "Food Price Forecast", color = "Modèles") +
  scale_y_continuous(limits = c(-6, 8), breaks = seq(-4, 6, 2)) +
  theme_minimal() +
  theme(legend.position = "bottom")

## Graphique Machine-Learning----
ggplot(prev_df, aes(x=id, group = 1)) +
  geom_line(aes(y = y_real, color = "Observé"), size = 2, show.legend = TRUE) +
  geom_line(aes(y = GAM, color = "GAM"), size = 1, alpha = 1) +
  #geom_line(aes(y = MLP, color = "MLP"), size = 1, alpha = 0.7) +
  #geom_line(aes(y = MARS, color = "MARS"), size = 1, alpha = 0.7) +
  #geom_line(aes(y = SVM, color = "SVM"), size = 1, alpha = 0.7) +
  #geom_line(aes(y = RF, color = "RF"), size = 1, alpha = 0.7) +
  #geom_line(aes(y = XGB, color = "XGB"), size = 1, alpha = 0.7) +
  #geom_line(aes(y = kNN, color = "kNN"), size = 1, alpha = 0.7) +
  #geom_line(aes(y = LSTM, color = "LSTM"), size = 1,alpha = 1) +
  #geom_line(aes(y = LSTM_CNN, color = "LSTM_CNN"), size = 1,alpha = 0.7) +
  #geom_line(aes(y = TDNN, color = "TDNN"), size = 1,alpha = 0.7) +
  scale_color_manual(values = c("Observé" = "black", "GAM" = "purple", "MLP" = "green", 
                                "MARS" = "darkblue", "SVM" = "orange", "RF" = "red", "XGB" = "cyan",
                                "kNN"="magenta1","LSTM"="grey","LSTM_CNN"="indianred","TDNN"="gold")) +
  labs( x = "Temps", y = "Food Price Forecast", color = "Modèles") +
  scale_y_continuous(limits = c(-6, 8), breaks = seq(-4, 6, 2)) +
  theme_minimal()


# Graphique concentré----
## Graphique économétriques
ggplot(prev_df, aes(x=id, group = 1)) +
  geom_line(aes(y = y_real, color = "Observé"), size = 2, show.legend = TRUE) +
  geom_line(aes(y = ARMAX, color = "ARMAX"), size = 1, alpha = 0.4) +
  geom_line(aes(y = ARX, color = "ARX"), size = 1, alpha = 0.4) +
  geom_line(aes(y = LM, color = "LM"), size = 1, alpha = 0.4) +
  geom_line(aes(y = ARX_GET, color = "ARX_GETS"), size = 1, alpha = 1) +
  scale_color_manual(values = c("Observé" = "black", "ARMAX" = "blue", "ARX" = "blue", 
                                "LM" = "blue", "ARX_GETS" = "red","GAM"="blue")) +
  labs( x = "Temps", y = "Forecast - Modèles économétriques", color = "Modèles") +
  scale_y_continuous(limits = c(-6, 8), breaks = seq(-4, 6, 2)) +
  theme_minimal()

## Graphique ML
ggplot(prev_df, aes(x=id, group = 1)) +
  geom_line(aes(y = y_real, color = "Observé"), size = 2, show.legend = TRUE) +
  geom_line(aes(y = GAM, color = "GAM"), size = 1, alpha = 0.4) +
  geom_line(aes(y = MLP, color = "MLP"), size = 1, alpha = 0.4) +
  geom_line(aes(y = MARS, color = "MARS"), size = 1, alpha = 0.4) +
  geom_line(aes(y = SVM, color = "SVM"), size = 1, alpha = 0.4) +
  geom_line(aes(y = RF, color = "RF"), size = 1, alpha = 0.4) +
  geom_line(aes(y = XGB, color = "XGB"), size = 1, alpha = 0.4) +
  geom_line(aes(y = kNN, color = "kNN"), size = 1, alpha = 0.4) +
  geom_line(aes(y = LSTM, color = "LSTM"), size = 2, alpha = 1) +
  geom_line(aes(y = LSTM_CNN, color = "LSTM_CNN"), size = 1, alpha = 0.4) +
  geom_line(aes(y = TDNN, color = "TDNN"), size = 1, alpha = 0.4) +
  scale_color_manual(values = c("Observé" = "black", "GAM" = "blue", "MLP" = "blue", 
                                "MARS" = "blue","SVM" = "blue","RF" = "blue",
                                "XGB" = "blue","kNN" = "blue","LSTM" = "red",
                                "LSTM_CNN" = "blue","TDNN" = "blue")) +
  labs( x = "Temps", y = "Forecast - Modèles économétriques", color = "Modèles") +
  scale_y_continuous(limits = c(-6, 8), breaks = seq(-4, 6, 2)) +
  theme_minimal()



# Tableau MSE, CSSED et R²OOS----

# Calcul du MSE pour chaque modèle
mse <- sapply(prev_df[, 1:17], function(x) mean((prev_df$y_real - x)^2))
rmse <- sqrt(mse)
ccsed <- sapply(prev_df[, 1:17], function(x) sqrt(sum((cumsum(x - prev_df$y_real))^2)) - sqrt(sum((cumsum(prev_df$ar1 - prev_df$y_real))^2)))
r2oos <- 1 - mse / mean((prev_df$y_real - prev_df$AR1)^2)
tableau <- data.frame(MSE = mse, RMSE = rmse, CCSED = ccsed, R2_OOS = r2oos)
tableau



# graphique des erreurs de prévision cumulées au carré (CSPE)

prev_df$CSPEnaive <- cumsum((prev_df$y_real - prev_df$NAIVE)^2)
prev_df$CSPEarmax <- cumsum((prev_df$y_real - prev_df$ARMAX)^2)
prev_df$CSPEarx <- cumsum((prev_df$y_real - prev_df$ARX)^2)
prev_df$CSPElm <- cumsum((prev_df$y_real - prev_df$LM)^2)
prev_df$CSPEarxget <- cumsum((prev_df$y_real - prev_df$ARX_GET)^2)
prev_df$CSPEar1 <- cumsum((prev_df$y_real - prev_df$AR1)^2)
prev_df$CSPEGAM <- cumsum((prev_df$y_real - prev_df$GAM)^2)
prev_df$CSPEMLP <- cumsum((prev_df$y_real - prev_df$MLP)^2)
prev_df$CSPEMARS <- cumsum((prev_df$y_real - prev_df$MARS)^2)
prev_df$CSPESVM <- cumsum((prev_df$y_real - prev_df$SVM)^2)
prev_df$CSPERF <- cumsum((prev_df$y_real - prev_df$RF)^2)
prev_df$CSPEXGB <- cumsum((prev_df$y_real - prev_df$XGB)^2)
prev_df$CSPEkNN <- cumsum((prev_df$y_real - prev_df$kNN)^2)
prev_df$CSPELSTM <- cumsum((prev_df$y_real - prev_df$LSTM)^2)
prev_df$CSPELSTMCNN <- cumsum((prev_df$y_real - prev_df$LSTM_CNN)^2)
prev_df$CSPETDNN <- cumsum((prev_df$y_real - prev_df$TDNN)^2)




# Graphique CSPE----
## Graphique général 
ggplot(prev_df, aes(x=id, group=1)) +
  geom_line(aes(y=CSPEnaive, color="NAIVE"), size=1) +
  geom_line(aes(y=CSPEarmax, color="ARMAX"), size=1) +
  geom_line(aes(y=CSPEarx, color="ARX"), size=1) +
  geom_line(aes(y=CSPElm, color="LM"), size=1) +
  geom_line(aes(y=CSPEarxget, color="ARX_GETS"), size=1) +
  geom_line(aes(y=CSPEar1, color="AR1"), size=1) +
  geom_line(aes(y=CSPEGAM, color="GAM"), size=1) +
  geom_line(aes(y = CSPEMLP, color = "MLP"), size = 1) +
  geom_line(aes(y = CSPEMARS, color = "MARS"), size = 1) +
  geom_line(aes(y = CSPESVM, color = "SVM"), size = 1) +
  geom_line(aes(y = CSPERF, color = "RF"), size = 1) +
  geom_line(aes(y = CSPEXGB, color = "XGB"), size = 1) +
  geom_line(aes(y = CSPEkNN, color = "kNN"), size = 1) +
  geom_line(aes(y = CSPELSTM, color = "LSTM"), size = 1) +
  geom_line(aes(y = CSPELSTMCNN, color = "LSTMCNN"), size = 1) +
  geom_line(aes(y = CSPETDNN, color = "TDNN"), size = 1) +
  labs(x="Mois", y="Cumul des CSPE", color="Modèles") +
  scale_color_manual("", values=c("ARMAX" = "purple", "ARX" = "green", 
                                  "LM" = "darkblue", "NAIVE" = "orange", "ARX_GETS" = "cyan", "AR1" = "red",
                                  "GAM"="yellow", "MLP"="grey","MARS"="pink","SVM"="brown","RF"="darkgreen",
                                  "XGB"="blue","kNN"="magenta","LSTM"="chocolate1","LSTMCNN"="black","TDNN"="seagreen2")) +
  theme_minimal() +
  theme(legend.position="right")


ggplot(prev_df, aes(x=id, group = 1)) +
  geom_line(aes(y = y_real, color = "Observé"), size = 2, show.legend = TRUE) +
  geom_line(aes(y = CSPEarxget, color = "ARX_GETS"), size = 1)+
  geom_line(aes(y = CSPELSTM, color = "LSTM"), size = 1)+
  geom_line(aes(y = CSPEXGB, color = "XGB"), size = 1)+
  scale_color_manual(values = c("ARX_GETS" = "red","LSTM"="blue","XGB"="green3")) +
  labs( x = "Temps", y = "Food Price Forecast", color = "Modèles") +
  theme_minimal() +
  theme(legend.position = "bottom")

## Graphique ML vs eco 
ggplot(prev_df, aes(x=id, group=1)) +
  geom_line(aes(y=CSPEarmax, color="eco"), size=1,alpha=0.7) +
  geom_line(aes(y=CSPEarx, color="eco"), size=1,alpha=0.7) +
  geom_line(aes(y=CSPElm, color="eco"), size=1,alpha=0.7) +
  geom_line(aes(y=CSPEarxget, color="eco"), size=1,alpha=0.7) +
  geom_line(aes(y=CSPEGAM, color="eco"), size=1,alpha=0.7) +
  geom_line(aes(y = CSPEMLP, color = "ML"), size = 1,alpha=0.7) +
  geom_line(aes(y = CSPEMARS, color = "ML"), size = 1,alpha=0.7) +
  geom_line(aes(y = CSPESVM, color = "ML"), size = 1,alpha=0.7) +
  geom_line(aes(y = CSPERF, color = "ML"), size = 1,alpha=0.7) +
  geom_line(aes(y = CSPEXGB, color = "ML"), size = 1,alpha=0.7) +
  geom_line(aes(y = CSPEkNN, color = "ML"), size = 1,alpha=0.7) +
  geom_line(aes(y = CSPELSTM, color = "ML"), size = 1,alpha=0.7) +
  geom_line(aes(y = CSPELSTMCNN, color = "ML"), size = 1,alpha=0.7) +
  geom_line(aes(y = CSPETDNN, color = "ML"), size = 1,alpha=0.7) +
  labs(x="Mois", y="Cumul des CSPE", color="Modèles") +
  scale_color_manual("", values=c("eco" = "blue", "ML" = "red"))+
  theme_minimal() +
  theme(legend.position="right")

## Graphique meilleurs modèles 
ggplot(prev_df, aes(x=id, group=1)) +
  geom_line(aes(y=CSPEarxget, color="ARX_GETS"), size=1) +
  geom_line(aes(y = CSPEMLP, color = "MLP"), size = 1) +
  geom_line(aes(y = CSPEMARS, color = "MARS"), size = 1) +
  geom_line(aes(y = CSPEXGB, color = "XGB"), size = 1) +
  geom_line(aes(y = CSPELSTM, color = "LSTM"), size = 1) +
  labs(x="Mois", y="Cumul des CSPE", color="Modèles") +
  scale_color_manual("", values=c("ARMAX" = "purple", "ARX" = "green", 
                                  "LM" = "darkblue", "NAIVE" = "orange", "ARX_GETS" = "cyan", "AR1" = "red",
                                  "GAM"="yellow", "MLP"="grey","MARS"="pink","SVM"="brown","RF"="darkgreen",
                                  "XGB"="blue","kNN"="magenta","LSTM"="chocolate1","LSTMCNN"="black","TDNN"="seagreen2")) +
  theme_minimal() +
  theme(legend.position="right")



# Test de Diebold-Mariano----
# Test de Diebold-Mariano----
dm_test_results <- data.frame(matrix(NA, nrow = 16, ncol = 16))
colnames(dm_test_results) <- c("ARMAX", "ARX", "ARX_GETS", "LM", "NAIVE","AR1",
                               "GAM","MLP","MARS","SVM","RF","XGB","kNN","LSTM",
                               "LSTMCNN","TDNN")
rownames(dm_test_results) <- c("ARMAX", "ARX", "ARX_GET", "LM", "NAIVE","AR1",
                               "GAM","MLP","MARS","SVM","RF","XGB","kNN","LSTM",
                               "LSTMCNN","TDNN")

# df pour avec les vecteurs d'erreur par modele  

new_df <- data.frame(Dnaive = prev_df$y_real - prev_df$NAIVE,
                     Darmax = prev_df$y_real - prev_df$ARMAX,
                     Darx = prev_df$y_real - prev_df$ARX,
                     Dlm = prev_df$y_real - prev_df$LM,
                     Darxget = prev_df$y_real - prev_df$ARX_GET,
                     Dgam = prev_df$y_real - prev_df$GAM,
                     Dmlp = prev_df$y_real - prev_df$MLP,
                     Dmars = prev_df$y_real - prev_df$MARS,
                     Dsvm = prev_df$y_real - prev_df$SVM,
                     Drf = prev_df$y_real - prev_df$RF,
                     Dxgb = prev_df$y_real - prev_df$XGB,
                     Dknn = prev_df$y_real - prev_df$kNN,
                     Dlstm = prev_df$y_real - prev_df$LSTM,
                     Dlstmcnn = prev_df$y_real - prev_df$LSTM_CNN,
                     Dtdnn = prev_df$y_real - prev_df$TDNN,
                     Dar1 = prev_df$y_real - prev_df$AR1)



# DM test sur l'ensemble modèle par rapport AR1.

dm.test(new_df$Dar1, new_df$Dnaive, alternative = "less", h = 1)
dm.test(new_df$Dar1, new_df$Darmax, alternative = "less", h = 1)
dm.test(new_df$Dar1, new_df$Darx, alternative = "less", h = 1)
dm.test(new_df$Dar1, new_df$Dlm, alternative = "less", h = 1)
dm.test(new_df$Dar1, new_df$Darxget, alternative = "less", h = 1)
dm.test(new_df$Dar1, new_df$Dgam, alternative = "less", h = 1)
dm.test(new_df$Dar1, new_df$Dmlp, alternative = "less", h = 1)
dm.test(new_df$Dar1, new_df$Dmars, alternative = "less", h = 1)
dm.test(new_df$Dar1, new_df$Dsvm, alternative = "less", h = 1)
dm.test(new_df$Dar1, new_df$Drf, alternative = "less", h = 1)
dm.test(new_df$Dar1, new_df$Dxgb, alternative = "less", h = 1)
dm.test(new_df$Dar1, new_df$Dknn, alternative = "less", h = 1)
dm.test(new_df$Dar1, new_df$Dlstm, alternative = "less", h = 1)
dm.test(new_df$Dar1, new_df$Dlstmcnn, alternative = "less", h = 1)
dm.test(new_df$Dar1, new_df$Dtdnn, alternative = "less", h = 1)



# Exportation pour Shiny
#write.csv2(prev_df, file = "DATA/prev_df.csv2", row.names = FALSE)
write.csv(tableau, file = "DATA/tableau.csv", row.names = TRUE)
write.csv(prev_df, file = "DATA/prev_df.csv", row.names = TRUE)










