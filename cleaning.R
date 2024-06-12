# Chargement des Packages----
library(ggplot2)
library(dplyr)
library(tibble)
library(readxl)
library(purrr)
library(gridExtra)

# Chargement des BDD----

## Food Price Index----
food = read.csv2("DATA/FAO_FOOD_PRICE_INDEX.csv")
food[,2:7] <- sapply(food[,2:7], as.numeric)
food$Date = as.Date(paste(food$Date, "-01", sep = ""), format = "%Y-%m-%d")
food_index = food[,c(1,2)]
names(food_index)[names(food_index) == "Food.Price.Index"] <- "Food"
food_index <- subset(food_index, Date >= as.Date("1999-01-01"))


## Cours du  pétôle----
oil = read.csv2("DATA/OIL_WTI.csv", sep=",")
oil$MCOILWTICO = as.numeric(oil$MCOILWTICO)
oil$DATE = as.Date(oil$DATE)
oil <- subset(oil, DATE >= as.Date("1999-01-01"))
names(oil)[names(oil) == "DATE"] <- "Date"
names(oil)[names(oil) == "MCOILWTICO"] <- "Oil"


## Temperature Anomaly----
temp = read.csv2("DATA/LAND_&_OCEAN_1900-2022.csv", sep=",", header = TRUE, row.names = 1)
temp <- rownames_to_column(temp, var = "Year")
temp = slice(temp, 5:nrow(temp))
colnames(temp)[2] <- "Temperature Anomalies"
# Extraire les années et les mois de la colonne "Year"
year <- substring(temp$Year, 1, 4)
month <- substring(temp$Year, 5, 6)
# Créer une nouvelle colonne "Date" avec les dates au format "YYYY-MM-DD"
temp$Date <- paste(year, month, "01", sep = "-")
temp$Date <- as.Date(temp$Date, format="%Y-%m-%d")
temp <- select(temp, -Year)
temp <- select(temp, Date,`Temperature Anomalies`)
temp$`Temperature Anomalies`= as.numeric(temp$`Temperature Anomalies`)
temp <- subset(temp, Date >= as.Date("1999-01-01"))

## Indice de la peur: VIX----
vix = read.csv2("DATA/VIX.csv", sep=",")
vix = vix[,c(1,5)]
vix$Date <- as.Date(vix$Date, format="%Y-%m-%d")
vix$Close = as.numeric(vix$Close)
#nouvelle_ligne <- data.frame(Date = as.Date("1999-01-01"), Close = as.numeric(21.00))
#vix <- rbind(nouvelle_ligne, vix)
names(vix)[names(vix) == "Close"] <- "VIX"
vix = subset(vix, Date >= as.Date("1999-01-01"))

## Indice GPR----
gpr = read_xls("DATA/data_gpr_export.xls")
gpr = gpr[,c(1,5)]
gpr$month = as.Date(gpr$month, format="%Y-%m-%d")
gpr$GPRH = as.numeric(gpr$GPRH)
gpr = subset(gpr, month >= as.Date("1999-01-01"))
gpr = subset(gpr, month < as.Date("2023-01-01"))
names(gpr)[names(gpr) == "month"] <- "Date"


## Indice Fertilizers----
fertil = read_xlsx("DATA/CMO-Historical-Data-Monthly.xlsx",sheet = "Monthly Indices", skip=9)
fertil = select(fertil, 1, 13)
fertil = rename(fertil, Date="...1", Fertil = "iFERTILIZERS")
fertil <- mutate(fertil, date = as.Date(paste0(Date, "-01"), format = "%YM%m-%d"))
fertil = fertil [,c(2,3)]
fertil$Fertil = as.numeric(fertil$Fertil)
fertil = subset(fertil, date >= as.Date("1999-01-01"))
fertil = subset(fertil, date < as.Date("2023-01-01"))
names(fertil)[names(fertil) == "date"] <- "Date"
fertil <- fertil[, c("Date", "Fertil")]


## Indice Métaux & Mineraux----
metal = read_xlsx("DATA/CMO-Historical-Data-Monthly.xlsx",sheet = "Monthly Indices", skip=9)
metal = select(metal, 1, 14)
metal = rename(metal, Date="...1", Metal = "iMETMIN")
metal <- mutate(metal, date = as.Date(paste0(Date, "-01"), format = "%YM%m-%d"))
metal = metal [,c(2,3)]
metal$Metal = as.numeric(metal$Metal)
metal = subset(metal, date >= as.Date("1999-01-01"))
metal = subset(metal, date < as.Date("2023-01-01"))
names(metal)[names(metal) == "date"] <- "Date"
metal <- metal[, c("Date", "Metal")]


## Indice Raw Materials----
raw = read_xlsx("DATA/CMO-Historical-Data-Monthly.xlsx",sheet = "Monthly Indices", skip=9)
raw = select(raw, 1, 10)
raw = rename(raw, Date="...1", raw = "iRAW_MATERIAL")
raw <- mutate(raw, date = as.Date(paste0(Date, "-01"), format = "%YM%m-%d"))
raw = raw [,c(2,3)]
raw$raw = as.numeric(raw$raw)
raw = subset(raw, date >= as.Date("1999-01-01"))
raw = subset(raw, date < as.Date("2023-01-01"))
names(raw)[names(raw) == "date"] <- "Date"
raw <- raw[, c("Date", "raw")]
names(raw)[names(raw) == "raw"] <- "Raw_Material"


## Cours EURO-DOLLARS----
eur_dol = read.csv2("DATA/EUR_USD - Données Historiques.csv",sep=",")
eur_dol = eur_dol[,c(1,2)]
eur_dol$Date <- as.Date(eur_dol$Date, format = "%d/%m/%Y")
eur_dol <- eur_dol[order(eur_dol$Date), ]
eur_dol = subset(eur_dol, Date >= as.Date("1999-01-01"))
eur_dol = rename(eur_dol, Euro_Dollars="Dernier")

## Futures Viande----
meat = read.csv2("DATA/Futures_MEAT.csv",sep=",")
meat = meat[,c(1,2)]
meat$Date <- as.Date(meat$Date, format = "%m/%d/%Y")
meat$Price = as.numeric(meat$Price)
meat <- meat[order(meat$Date), ]
meat <- rename(meat, Viande="Price")

## Futures Sucre----
sucre = read.csv2("DATA/Futures_Sucre.csv",sep=",")
sucre = sucre[,c(1,2)]
sucre$Date <- as.Date(sucre$Date, format = "%d/%m/%Y")
sucre$Price = as.numeric(sucre$Dernier)
sucre <- sucre[order(sucre$Date), ]
sucre = rename(sucre, Sucre="Dernier")
sucre = sucre[,c(1,2)]

## Futures Huiles----
huile = read.csv2("DATA/Futures_Oil.csv",sep=",")
huile = huile[,c(1,2)]
huile$Date <- as.Date(huile$Date, format = "%m/%d/%Y")
huile$Price = as.numeric(huile$Price)
huile <- huile[order(huile$Date), ]
huile = rename(huile, Huile="Price")

## Futures Wheat----
wheat = read.csv2("DATA/Futures_Wheat.csv",sep=",")
wheat = wheat[,c(1,2)]
wheat$Date <- as.Date(wheat$Date, format = "%m/%d/%Y")
wheat$Price <- gsub(",", "", wheat$Price)
wheat$Price = as.numeric(wheat$Price)
wheat <- wheat[order(wheat$Date), ]
wheat = rename(wheat, Wheat="Price")



#  Fusion des données----
base = reduce(list(food_index,fertil,gpr,metal,oil,raw,temp,vix,eur_dol), 
              full_join, by = "Date")

base2 = subset(base, Date >= as.Date("1999-01-01"))

base_1999 = reduce(list(base2,meat,huile,wheat,sucre),
              full_join, by = "Date")

#write.csv2(base_1999, file = "DATA/BDD_1999.csv", row.names = FALSE)

base_reduite = head(base_1999, -12)


#print(colSums(is.na(base)))
#test = read.csv2("DATA/BDD.csv")
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#'
#'
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#'
#'
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# Graphiques-----

##Graphique Food Price Index----

ggplot(food_index, aes(x = Date, y = Food, alpha = Food, color = Food)) +
  geom_line(size = 0.5) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_alpha(range = c(0.7, 1), guide = "none") +
  scale_color_gradient2(low = "darkblue", high = "red", mid = "darkblue", midpoint = mean(food_index$Food)) +
  theme_light() +
  theme( axis.line = element_line(colour = "black",size = 0.2, linetype = "solid"),
         plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)),
         axis.title.x = element_text(face = "bold"),
         axis.title.y = element_text(face = "bold"),
         axis.text.x = element_text(face = "bold"),
         axis.text.y = element_text(face = "bold")) +
  xlab("") + ylab("") +
  labs(x = "Temps", y = "Index", caption = "Source des données: FAO food_index Price Index") +
  geom_vline(xintercept = as.Date(c("2020-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "red", linetype = "dashed")+
  annotate("text", x = as.Date("2020-01-01", format = "%Y-%m-%d"), y = 50, label = "Covid-19", color = "red", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2007-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "red", linetype = "dashed")+
  annotate("text", x = as.Date("2007-01-01", format = "%Y-%m-%d"), y = 50, label = "Subprime", color = "red", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2022-02-24"), format = "%Y-%m-%d"), size = 0.5, color = "red", linetype = "dashed")+
  annotate("text", x = as.Date("2022-02-24", format = "%Y-%m-%d"), y = 40, label = "Ukraine", color = "red", angle=0, size = 3)+
  scale_x_date(date_labels = "%Y", breaks= "5 years") +
  theme(plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)))

##Graphique petrole brute----

ggplot(oil, aes(x = Date, y = Oil, alpha = Oil, color = Oil)) +
  geom_line(size = 0.5) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_alpha(range = c(0.7, 1), guide = "none") +
  scale_color_gradient2(low = "darkgreen", high = "red", mid = "darkgreen", midpoint = mean(oil$Oil)) +
  theme_light() +
  theme( axis.line = element_line(colour = "black",size = 0.2, linetype = "solid"),
         plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)),
         axis.title.x = element_text(face = "bold"),
         axis.title.y = element_text(face = "bold"),
         axis.text.x = element_text(face = "bold"),
         axis.text.y = element_text(face = "bold")) +
  xlab("") + ylab("") +
  labs(x = "Temps", y = "Index", caption = "Source des données: F.R.E.D") +
  geom_vline(xintercept = as.Date(c("2020-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "red", linetype = "dashed")+
  annotate("text", x = as.Date("2020-01-01", format = "%Y-%m-%d"), y = 50, label = "Covid-19", color = "red", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2007-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "red", linetype = "dashed")+
  annotate("text", x = as.Date("2007-01-01", format = "%Y-%m-%d"), y = 50, label = "Subprime", color = "red", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2022-02-24"), format = "%Y-%m-%d"), size = 0.5, color = "red", linetype = "dashed")+
  annotate("text", x = as.Date("2022-02-24", format = "%Y-%m-%d"), y = 40, label = "Ukraine", color = "red", angle=0, size = 3)+
  scale_x_date(date_labels = "%Y", breaks= "5 years") +
  theme(plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)))


##Graphique Température Anomalies----
ggplot(temp, aes(x = Date, y = `Temperature Anomalies`, alpha = `Temperature Anomalies`, color = `Temperature Anomalies`)) +
  geom_line(size = 0.5) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_alpha(range = c(0.7, 1), guide = "none") +
  scale_color_gradient2(low = "#1C86EE", high = "red", mid = "#1C86EE", midpoint = mean(temp$`Temperature Anomalies`)) +
  theme_light() +
  theme( axis.line = element_line(colour = "black",size = 0.2, linetype = "solid"),
         plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)),
         axis.title.x = element_text(face = "bold"),
         axis.title.y = element_text(face = "bold"),
         axis.text.x = element_text(face = "bold"),
         axis.text.y = element_text(face = "bold"))+  
  xlab("") + ylab("") +
  labs(x = "Temps", y = "Index", caption = "Source des données: NOAA: National Oceanic Atmospheric Administration")

##Graphique GPR----
ggplot(gpr, aes(x = Date, y = GPRH, alpha = GPRH, color = GPRH)) +
  geom_line(size = 0.5) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_alpha(range = c(0.7, 1), guide = "none") +
  scale_color_gradient2(low = "darkslategray", high = "red", mid = "darkslategray", midpoint = mean(gpr$GPRH)) +
  theme_light() +
  theme( axis.line = element_line(colour = "black",size = 0.2, linetype = "solid"),
         plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)),
         axis.title.x = element_text(face = "bold"),
         axis.title.y = element_text(face = "bold"),
         axis.text.x = element_text(face = "bold"),
         axis.text.y = element_text(face = "bold")) +
  xlab("") + ylab("") +
  labs(x = "Temps", y = "Index", caption = "Source des données: www.matteoiacoviello.com") +
  geom_vline(xintercept = as.Date(c("2020-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "red", linetype = "dashed")+
  annotate("text", x = as.Date("2020-01-01", format = "%Y-%m-%d"), y = 50, label = "Covid-19", color = "red", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2007-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "red", linetype = "dashed")+
  annotate("text", x = as.Date("2007-01-01", format = "%Y-%m-%d"), y = 50, label = "Subprime", color = "red", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2022-02-24"), format = "%Y-%m-%d"), size = 0.5, color = "red", linetype = "dashed")+
  annotate("text", x = as.Date("2022-02-24", format = "%Y-%m-%d"), y = 40, label = "Ukraine", color = "red", angle=0, size = 3)+
  scale_x_date(date_labels = "%Y", breaks= "5 years") +
  theme(plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)))


##Graphique du VIX----
ggplot(vix, aes(x = Date, y = VIX, alpha = VIX, color = VIX)) +
  geom_line(size = 0.5) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_alpha(range = c(0.7, 1), guide = "none") +
  scale_color_gradient2(low = "black", high = "red", mid = "gray11", midpoint = mean(vix$VIX)) +
  theme_light() +
  theme( axis.line = element_line(colour = "black",size = 0.2, linetype = "solid"),
         plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)),
         axis.title.x = element_text(face = "bold"),
         axis.title.y = element_text(face = "bold"),
         axis.text.x = element_text(face = "bold"),
         axis.text.y = element_text(face = "bold")) +
  xlab("") + ylab("") +
  labs(x = "Temps", y = "Index", caption = "Source des données: yahoo finance") +
  geom_vline(xintercept = as.Date(c("2020-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2020-01-01", format = "%Y-%m-%d"), y = 50, label = "Covid-19", color = "black", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2007-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2007-01-01", format = "%Y-%m-%d"), y = 50, label = "Subprime", color = "black", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2022-02-24"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2022-02-24", format = "%Y-%m-%d"), y = 40, label = "Ukraine", color = "black", angle=0, size = 3)+
  scale_x_date(date_labels = "%Y", breaks= "5 years") +
  theme(plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)))

##Graphique de fertil----
ggplot(fertil, aes(x = Date, y = Fertil, alpha = Fertil, color = Fertil)) +
  geom_line(size = 0.5) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_alpha(range = c(0.9, 1), guide = "none") +
  scale_color_gradient2(low = "seagreen4", high = "lightgoldenrod2", mid = "seagreen4", midpoint = mean(fertil$Fertil)) +
  theme_light() +
  theme( axis.line = element_line(colour = "black",size = 0.2, linetype = "solid"),
         plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)),
         axis.title.x = element_text(face = "bold"),
         axis.title.y = element_text(face = "bold"),
         axis.text.x = element_text(face = "bold"),
         axis.text.y = element_text(face = "bold")) +
  xlab("") + ylab("") +
  labs(x = "Temps", y = "Index", caption = "Source des données: World Bank - Commodity Market") +
  geom_vline(xintercept = as.Date(c("2020-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2020-01-01", format = "%Y-%m-%d"), y = 50, label = "Covid-19", color = "black", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2007-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2007-01-01", format = "%Y-%m-%d"), y = 50, label = "Subprime", color = "black", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2022-02-24"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2022-02-24", format = "%Y-%m-%d"), y = 40, label = "Ukraine", color = "black", angle=0, size = 3)+
  scale_x_date(date_labels = "%Y", breaks= "5 years") +
  theme(plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)))

##Graphique de metals----
ggplot(metal, aes(x = Date, y = Metal, alpha = Metal, color = Metal)) +
  geom_line(size = 0.5) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_alpha(range = c(0.9, 1), guide = "none") +
  scale_color_gradient2(low = "gray16", high = "gold", mid = "gray40", midpoint = mean(metal$Metal)) +
  theme_light() +
  theme( axis.line = element_line(colour = "black",size = 0.2, linetype = "solid"),
         plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)),
         axis.title.x = element_text(face = "bold"),
         axis.title.y = element_text(face = "bold"),
         axis.text.x = element_text(face = "bold"),
         axis.text.y = element_text(face = "bold")) +
  xlab("") + ylab("") +
  labs(x = "Temps", y = "Index", caption = "Source des données: World Bank - Commodity Market") +
  geom_vline(xintercept = as.Date(c("2020-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2020-01-01", format = "%Y-%m-%d"), y = 50, label = "Covid-19", color = "black", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2007-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2007-01-01", format = "%Y-%m-%d"), y = 50, label = "Subprime", color = "black", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2022-02-24"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2022-02-24", format = "%Y-%m-%d"), y = 40, label = "Ukraine", color = "black", angle=0, size = 3)+
  scale_x_date(date_labels = "%Y", breaks= "5 years") +
  theme(plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)))


##Graphique de raw materials----
ggplot(raw, aes(x = Date, y = Raw_Material , alpha = Raw_Material, color = Raw_Material)) +
  geom_line(size = 0.5) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_alpha(range = c(0.9, 1), guide = "none") +
  scale_color_gradient2(low = "black", high = "firebrick1", mid = "lightsteelblue3", midpoint = mean(raw$Raw_Material)) +
  theme_light() +
  theme( axis.line = element_line(colour = "black",size = 0.2, linetype = "solid"),
         plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)),
         axis.title.x = element_text(face = "bold"),
         axis.title.y = element_text(face = "bold"),
         axis.text.x = element_text(face = "bold"),
         axis.text.y = element_text(face = "bold")) +
  xlab("") + ylab("") +
  labs(x = "Temps", y = "Index", caption = "Source des données: World Bank - Commodity Market") +
  geom_vline(xintercept = as.Date(c("2020-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2020-01-01", format = "%Y-%m-%d"), y = 50, label = "Covid-19", color = "black", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2007-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2007-01-01", format = "%Y-%m-%d"), y = 50, label = "Subprime", color = "black", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2022-02-24"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2022-02-24", format = "%Y-%m-%d"), y = 40, label = "Ukraine", color = "black", angle=0, size = 3)+
  scale_x_date(date_labels = "%Y", breaks= "5 years") +
  theme(plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)))


##Graphique Euro-Dollars----
ggplot(eur_dol, aes(x = Date, y = Euro_Dollars, alpha = Euro_Dollars, color = Euro_Dollars)) +
  geom_line(size = 0.5) +
  scale_y_continuous(limits = c(0, 2), labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_alpha(range = c(0.7, 1), guide = "none") +
  scale_color_gradient2(low = "gray9", high = "red", mid = "gray53", midpoint = mean(eur_dol$Euro_Dollars)) +
  theme_light() +
  theme( axis.line = element_line(colour = "black",size = 0.2, linetype = "solid"),
         plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)),
         axis.title.x = element_text(face = "bold"),
         axis.title.y = element_text(face = "bold"),
         axis.text.x = element_text(face = "bold"),
         axis.text.y = element_text(face = "bold"))



##Graphique Futures Meat----
graph_meat = ggplot(base_1999, aes(x = Date, y = Viande, alpha = Viande, color = Viande)) +
  geom_line(size = 0.5) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_alpha(range = c(0.7, 1), guide = "none") +
  scale_color_gradient2(low = "darkblue", high = "red", mid = "darkblue", midpoint = mean(base_1999$Viande)) +
  theme_light() +
  theme( axis.line = element_line(colour = "black",size = 0.2, linetype = "solid"),
         plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)),
         axis.title.x = element_text(face = "bold"),
         axis.title.y = element_text(face = "bold"),
         axis.text.x = element_text(face = "bold"),
         axis.text.y = element_text(face = "bold")) +
  xlab("") + ylab("") +
  labs(x = "Temps", y = "Index", caption = "Source des données: investing.com") +
  geom_vline(xintercept = as.Date(c("2020-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2020-01-01", format = "%Y-%m-%d"), y = 50, label = "Covid-19", color = "black", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2007-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2007-01-01", format = "%Y-%m-%d"), y = 50, label = "Subprime", color = "black", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2022-02-24"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2022-02-24", format = "%Y-%m-%d"), y = 40, label = "Ukraine", color = "black", angle=0, size = 3)+
  scale_x_date(date_labels = "%Y", breaks= "5 years") +
  theme(plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)))

##Graphique Futures Sucre----

graph_sucre = ggplot(base_1999, aes(x = Date, y = Sucre, alpha = Sucre, color = Sucre)) +
  geom_line(size = 0.5) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_alpha(range = c(0.7, 1), guide = "none") +
  scale_color_gradient2(low = "darkgreen", high = "red", mid = "darkgreen", midpoint = mean(base_1999$Sucre)) +
  theme_light() +
  theme( axis.line = element_line(colour = "black",size = 0.2, linetype = "solid"),
         plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)),
         axis.title.x = element_text(face = "bold"),
         axis.title.y = element_text(face = "bold"),
         axis.text.x = element_text(face = "bold"),
         axis.text.y = element_text(face = "bold")) +
  xlab("") + ylab("") +
  labs(x = "Temps", y = "Index", caption = "Source des données: investing.com") +
  geom_vline(xintercept = as.Date(c("2020-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2020-01-01", format = "%Y-%m-%d"), y = 50, label = "Covid-19", color = "black", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2007-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2007-01-01", format = "%Y-%m-%d"), y = 50, label = "Subprime", color = "black", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2022-02-24"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2022-02-24", format = "%Y-%m-%d"), y = 40, label = "Ukraine", color = "black", angle=0, size = 3)+
  scale_x_date(date_labels = "%Y", breaks= "5 years") +
  theme(plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)))


##Graphique Futures Huiles----
graph_huile = ggplot(base_1999, aes(x = Date, y = Huile, alpha = Huile, color = Huile)) +
  geom_line(size = 0.5) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_alpha(range = c(0.7, 1), guide = "none") +
  scale_color_gradient2(low = "#1C86EE", high = "red", mid = "#1C86EE", midpoint = mean(base_1999$Huile)) +
  theme_light() +
  theme( axis.line = element_line(colour = "black",size = 0.2, linetype = "solid"),
         plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)),
         axis.title.x = element_text(face = "bold"),
         axis.title.y = element_text(face = "bold"),
         axis.text.x = element_text(face = "bold"),
         axis.text.y = element_text(face = "bold"))+
  xlab("") + ylab("") +
  labs(x = "Temps", y = "Index", caption = "Source des données: investing.com") +
  geom_vline(xintercept = as.Date(c("2020-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2020-01-01", format = "%Y-%m-%d"), y = 50, label = "Covid-19", color = "black", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2007-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2007-01-01", format = "%Y-%m-%d"), y = 50, label = "Subprime", color = "black", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2022-02-24"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2022-02-24", format = "%Y-%m-%d"), y = 40, label = "Ukraine", color = "black", angle=0, size = 3)+
  scale_x_date(date_labels = "%Y", breaks= "5 years") +
  theme(plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)))

##Graphique Futures Céréales----
graph_wheat = ggplot(base_1999, aes(x = Date, y = Wheat, alpha = Wheat, color = Wheat)) +
  geom_line(size = 0.5) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_alpha(range = c(0.7, 1), guide = "none") +
  scale_color_gradient2(low = "darkslategray", high = "red", mid = "darkslategray", midpoint = mean(base_1999$Wheat)) +
  theme_light() +
  theme( axis.line = element_line(colour = "black",size = 0.2, linetype = "solid"),
         plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)),
         axis.title.x = element_text(face = "bold"),
         axis.title.y = element_text(face = "bold"),
         axis.text.x = element_text(face = "bold"),
         axis.text.y = element_text(face = "bold")) +
  xlab("") + ylab("") +
  labs(x = "Temps", y = "Index", caption = "Source des données: tradingeconomics.com") +
  geom_vline(xintercept = as.Date(c("2020-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2020-01-01", format = "%Y-%m-%d"), y = 50, label = "Covid-19", color = "black", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2007-01-01"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2007-01-01", format = "%Y-%m-%d"), y = 50, label = "Subprime", color = "black", angle=0, size = 3)+
  geom_vline(xintercept = as.Date(c("2022-02-24"), format = "%Y-%m-%d"), size = 0.5, color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2022-02-24", format = "%Y-%m-%d"), y = 40, label = "Ukraine", color = "black", angle=0, size = 3)+
  scale_x_date(date_labels = "%Y", breaks= "5 years") +
  theme(plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 0, b = 0, l = -40)))


grid.arrange(graph_huile,graph_meat,graph_sucre,graph_wheat, nrow = 2, ncol = 2)

