data_globale<-read.table(file=file.choose(),header=T,sep=",")

View(data_globale)
summary(data_globale)
str(data_globale)


#Traitement des outliers 
#Fonction de Traitement des outliers 
handle_outliers <- function(data, column, Q1, Q3) {
  Vmin <- Q1 - 1.5 * (Q3 - Q1)
  Vmax <- Q3 + 1.5 * (Q3 - Q1)
  
  cat(column, "- Vmin:", Vmin, "\n")
  cat(column, "- Vmax:", Vmax, "\n")
  
  count_below_vmin <- sum(data[[column]] < Vmin, na.rm = TRUE)
  count_above_vmax <- sum(data[[column]] > Vmax, na.rm = TRUE)
  
  cat(column, "- nombre < Vmin:", count_below_vmin, "\n")
  cat(column, "- nombre > Vmax:", count_above_vmax, "\n")
  
  data[[column]] <- replace(data[[column]], data[[column]] > Vmax, NA)
  
  summary(data[[column]])
  boxplot(data[[column]], main = paste("Boxplot of", column))
  
  count_below_vmin <- sum(data[[column]] < Vmin, na.rm = TRUE)
  count_above_vmax <- sum(data[[column]] > Vmax, na.rm = TRUE)
  
  cat(column, "- nombre < Vmin:", count_below_vmin, "\n")
  cat(column, "- nombre > Vmax:", count_above_vmax, "\n")
  
  return(data)
}



# Confirmed Cases
boxplot(data_globale$Confirmed.Cases)
summary(data_globale$Confirmed.Cases)
data_globale <- handle_outliers(data_globale, "Confirmed.Cases", Q1 = 2477, Q3 = 237620)

# Active Cases 
boxplot(data_globale$Active.Cases)
summary(data_globale$Active.Cases)
data_globale <- handle_outliers(data_globale, "Active.Cases", Q1 = 201, Q3 = 10506)

# Cured Discharged 
boxplot(data_globale$Cured.Discharged)
summary(data_globale$Cured.Discharged)
data_globale <- handle_outliers(data_globale, "Cured.Discharged", Q1 = 1128, Q3 = 190552)

# Death 
boxplot(data_globale$Death)
summary(data_globale$Death)
data_globale <- handle_outliers(data_globale, "Death", Q1 = 11, Q3 = 2721)

# Taux des valeurs manquantes
missing_rate <- colSums(is.na(data_globale)) / nrow(data_globale)
print(missing_rate)

#traitement des valeurs NA imputation avec KNN
library(VIM)
columns_to_impute <- c("Confirmed.Cases","Active.Cases","Cured.Discharged","Death")
CovidWorld<-kNN(data_globale, variable = columns_to_impute)
summary(CovidWorld)
str(CovidWorld)
# L'imputation par KNN a été choisie pour ces champs spécifiques car elle permet de :
# 1. Préserver les relations complexes et non linéaires entre les variables (par exemple, entre les décès, les cas actifs et confirmés).
# 2. Gérer les données avec des valeurs très petites et très grandes, ce que les méthodes simples comme la moyenne ou la médiane ne peuvent pas faire de manière adéquate.
# 3. Exploiter la taille importante du dataset, qui permet à KNN de trouver des voisins pertinents pour imputer les valeurs manquantes de manière réaliste.
# 4. Conserver la variance naturelle des données et éviter d'introduire un biais systématique comme le fait la moyenne/médiane.
# 5. Gérer efficacement les outliers sans les éliminer, contrairement aux méthodes comme la suppression des lignes ou les imputation par valeurs centrales.
# Ainsi, KNN offre une solution plus robuste et fidèle aux caractéristiques des données dans ce contexte.


# Normalization Function (Min-Max Scaling)
normalize_column <- function(data, column_name) {
  x_max <- max(data[[column_name]], na.rm = TRUE)
  x_min <- min(data[[column_name]], na.rm = TRUE)
  
  cat(column_name, "- x_max:", x_max, "\n")
  cat(column_name, "- x_min:", x_min, "\n")
  
  data[[column_name]] <- (data[[column_name]] - x_min) / (x_max - x_min)
  
  return(data)
}
CovidWorld <- normalize_column(CovidWorld, "Confirmed.Cases")
CovidWorld <- normalize_column(CovidWorld, "Active.Cases")
CovidWorld <- normalize_column(CovidWorld, "Cured.Discharged")
CovidWorld <- normalize_column(CovidWorld, "Death")

# Display the structure of the data
str(CovidWorld)
summary(CovidWorld)
num_observations <- nrow(CovidWorld)
cat("Nombre d'observations:", num_observations, "\n")
#ona 14552 observations ==> on ne peut pas utiliser shapiro.test pour tester la normalité

#TESTER la normalité
library(ggplot2)
plot(density(CovidWorld$Confirmed.Cases, na.rm = TRUE), main = "Densité avec courbe normale", col = "blue")
curve(dnorm(x, mean = mean(CovidWorld$Confirmed.Cases, na.rm = TRUE), sd = sd(CovidWorld$Confirmed.Cases, na.rm = TRUE)), 
      col = "red", add = TRUE)
legend("topright", legend = c("Densité des données Confirmed Cases", "Courbe normale"), col = c("blue", "red"), lty = 1)
#d'apres le graphe la variable Confirmed Cases ne suit pas la loi normale 

plot(density(CovidWorld$Active.Cases, na.rm = TRUE), main = "Densité avec courbe normale", col = "blue")
curve(dnorm(x, mean = mean(CovidWorld$Active.Cases, na.rm = TRUE), sd = sd(CovidWorld$Active.Cases, na.rm = TRUE)), 
      col = "red", add = TRUE)
legend("topright", legend = c("Densité des données Active Cases", "Courbe normale"), col = c("blue", "red"), lty = 1)
#d'apres le graphe la variable Active Cases ne suit pas la loi normale 

plot(density(CovidWorld$Cured.Discharged, na.rm = TRUE), main = "Densité avec courbe normale", col = "blue")
curve(dnorm(x, mean = mean(CovidWorld$Cured.Discharged, na.rm = TRUE), sd = sd(CovidWorld$Cured.Discharged, na.rm = TRUE)), 
      col = "red", add = TRUE)
legend("topright", legend = c("Densité des données Cured Discharged", "Courbe normale"), col = c("blue", "red"), lty = 1)
#d'apres le graphe la variable Cured Discharged ne suit pas la loi normale 

plot(density(CovidWorld$Death, na.rm = TRUE), main = "Densité avec courbe normale", col = "blue")
curve(dnorm(x, mean = mean(CovidWorld$Death, na.rm = TRUE), sd = sd(CovidWorld$Death, na.rm = TRUE)), 
      col = "red", add = TRUE)
legend("topright", legend = c("Densité des données Deaths", "Courbe normale"), col = c("blue", "red"), lty = 1)#d'apres le graphe la variable Deaths  ne suit pas la loi normale


install.packages("e1071")
library(e1071)
#verification des interpretations graphiques
#pour une loi normale K=3 et S=0

K=kurtosis(CovidWorld$Confirmed.Cases)
S=skewness(CovidWorld$Confirmed.Cases)
K #0.1167814
S #1.259539
#Confirmed Cases no suit pas la loi normale

K=kurtosis(CovidWorld$Active.Cases)
S=skewness(CovidWorld$Active.Cases)
K #0.5781598
S #1.391863
#Active Cases no suit pas la loi normale

K=kurtosis(CovidWorld$Cured.Discharged)
S=skewness(CovidWorld$Cured.Discharged)
K #-0.2847617
S #1.121739
#CuredDischarged no suit pas la loi normale

K=kurtosis(CovidWorld$Death)
S=skewness(CovidWorld$Death)
K #0.9380065
S #1.438161
#confirmedForeignNational no suit pas la loi normale

# analyser les modalites pour toutes les variable quantitatives

table(CovidWorld$Confirmed.Cases)
table(CovidWorld$Active.Cases)
table(CovidWorld$Cured.Discharged)
table(CovidWorld$Death)
###aucune des variables ne suit la loi normale donc on passe pour les tests non parametriques 


# Exemple ici avec 'Deaths' en tant que variable pour l'initialisation
CovidWorld$deaths_group <- CovidWorld$Death  

# Créez un groupe High/Low basé sur la médiane
CovidWorld$deaths_group <- ifelse(CovidWorld$deaths_group >= median(CovidWorld$deaths_group, na.rm = TRUE), "High", "Low")

# Convertissez en facteur
CovidWorld$deaths_group <- as.factor(CovidWorld$deaths_group)

# Vérifiez les résultats
summary(CovidWorld$deaths_group)
numeric_columns<-c("Confirmed.Cases", "Active.Cases","Cured.Discharged")

# Boucle pour appliquer le test de Wilcoxon
for (col in numeric_columns) {
  cat("Test de Wilcoxon pour :", col, "par Death_group\n")
  
  if (length(unique(CovidWorld$deaths_group)) == 2 && !all(is.na(CovidWorld[[col]]))) {
    result <- wilcox.test(CovidWorld[[col]] ~ CovidWorld$deaths_group, data = CovidWorld, exact = FALSE)
    print(result)
  } else {
    cat("Données insuffisantes ou problème avec les groupes\n")
  }
  cat("\n")
}
#interprétation des résultats:

#p-value < 0.05 : Les distributions des deux groupes sont significativement différentes.
#p-value ≥ 0.05 : Aucune différence significative détectée entre les groupes
# Test de Wilcoxon pour : Confirmed.Cases par Death_group
# Test de somme des rangs de Wilcoxon avec correction de continuité
# W = 52612743, p-value < 2.2e-16
# Hypothèse alternative : le véritable décalage de localisation n'est pas égal à 0
# Résultat : le p-value est très faible, ce qui signifie qu'il existe une différence statistiquement significative
# entre les groupes de décès en ce qui concerne les cas confirmés. Nous rejetons l'hypothèse nulle, donc
# il y a une différence significative dans la distribution des cas confirmés entre les groupes de décès.

# Test de Wilcoxon pour : Active.Cases par Death_group
# Test de somme des rangs de Wilcoxon avec correction de continuité
# W = 47148401, p-value < 2.2e-16
# Hypothèse alternative : le véritable décalage de localisation n'est pas égal à 0
# Résultat : le p-value est très faible, ce qui indique une différence statistiquement significative
# entre les groupes de décès pour les cas actifs. Nous rejetons l'hypothèse nulle, il y a donc une différence
# significative dans la distribution des cas actifs entre les groupes de décès.

# Test de Wilcoxon pour : Cured.Discharged par Death_group
# Test de somme des rangs de Wilcoxon avec correction de continuité
# W = 52445362, p-value < 2.2e-16
# Hypothèse alternative : le véritable décalage de localisation n'est pas égal à 0
# Résultat : le p-value est très faible, ce qui suggère une différence statistiquement significative
# entre les groupes de décès pour les cas guéris/déchargés. Nous rejetons l'hypothèse nulle, donc
# il y a une différence significative dans la distribution des cas guéris/déchargés entre les groupes de décès.


# Boucle pour appliquer le test de Kruskal-Wallis
for (col in numeric_columns) {
  cat("Test de Kruskal-Wallis pour :", col, "par Death Groups\n")
  
  result <- kruskal.test(CovidWorld[[col]] ~ CovidWorld$deaths_group, data = CovidWorld)
  print(result)
  cat("\n")
}


#interprétation de kruskkal-Wallis
#H0: Les médianes des groupes sont égales (pas de différence significative entre les groupes).
#p≤0.05 : Rejet de H0 les médianes des groupes sont significativement différentes.
#p>0.05 : On ne rejette pas H0 aucune différence significative entre les groupes.
# Test de Kruskal-Wallis pour : Confirmed.Cases par Death Groups
# Test de somme des rangs de Kruskal-Wallis
# Kruskal-Wallis chi-squared = 10645, df = 1, p-value < 2.2e-16
# Résultat : le p-value est extrêmement faible, ce qui indique une différence statistiquement significative
# entre les groupes de décès en ce qui concerne les cas confirmés. Nous rejetons l'hypothèse nulle, 
# ce qui signifie qu'il existe une différence significative dans la distribution des cas confirmés 
# entre les différents groupes de décès.

# Test de Kruskal-Wallis pour : Active.Cases par Death Groups
# Test de somme des rangs de Kruskal-Wallis
# Kruskal-Wallis chi-squared = 6660.2, df = 1, p-value < 2.2e-16
# Résultat : le p-value est également très faible, ce qui indique une différence statistiquement significative
# entre les groupes de décès pour les cas actifs. Nous rejetons l'hypothèse nulle, donc il existe 
# une différence significative dans la distribution des cas actifs entre les groupes de décès.

# Test de Kruskal-Wallis pour : Cured.Discharged par Death Groups
# Test de somme des rangs de Kruskal-Wallis
# Kruskal-Wallis chi-squared = 10511, df = 1, p-value < 2.2e-16
# Résultat : encore une fois, le p-value est très faible, ce qui montre une différence statistiquement significative
# entre les groupes de décès pour les cas guéris/déchargés. Nous rejetons l'hypothèse nulle, ce qui signifie
# qu'il y a une différence significative dans la distribution des cas guéris/déchargés entre les groupes de décès.



# Perform pairwise Spearman correlation tests
variables <- c("Confirmed.Cases", "Active.Cases", "Cured.Discharged", "Death")

for (i in 1:(length(variables) - 1)) {
  for (j in (i + 1):length(variables)) {
    cat("\nTesting correlation between", variables[i], "and", variables[j], ":\n")
    test <- cor.test(CovidWorld[[variables[i]]], CovidWorld[[variables[j]]],
                     method = "spearman", exact = FALSE)
    print(test)
  }
}

# Interprétation des résultats de la corrélation de Spearman :

# 1. Confirmed.Cases et Active.Cases :
# - Spearman's rho : 0.785 (corrélation positive forte).
# - p-value : < 2.2e-16 (corrélation significative).
# - Interprétation : L'augmentation des cas confirmés est fortement liée à une augmentation des cas actifs.

# 2. Confirmed.Cases et Cured.Discharged :
# - Spearman's rho : 0.990 (corrélation positive très forte).
# - p-value : < 2.2e-16 (corrélation significative).
# - Interprétation : Une corrélation presque parfaite indique que les cas guéris/déchargés augmentent avec les cas confirmés.

# 3. Confirmed.Cases et Death :
# - Spearman's rho : 0.950 (corrélation positive très forte).
# - p-value : < 2.2e-16 (corrélation significative).
# - Interprétation : Une augmentation des cas confirmés est fortement corrélée à une augmentation des décès.

# 4. Active.Cases et Cured.Discharged :
# - Spearman's rho : 0.746 (corrélation positive forte).
# - p-value : < 2.2e-16 (corrélation significative).
# - Interprétation : Les cas actifs sont corrélés positivement aux guérisons, mais de manière légèrement moins forte.

# 5. Active.Cases et Death :
# - Spearman's rho : 0.785 (corrélation positive forte).
# - p-value : < 2.2e-16 (corrélation significative).
# - Interprétation : Une augmentation des cas actifs est liée à une augmentation des décès, bien que la corrélation soit légèrement plus faible.

# 6. Cured.Discharged et Death :
# - Spearman's rho : 0.947 (corrélation positive très forte).
# - p-value : < 2.2e-16 (corrélation significative).
# - Interprétation : Une forte corrélation indique que l'augmentation des guérisons et des décès est proportionnelle à l'évolution de la pandémie.

# Ces résultats confirment une interdépendance significative entre les différentes variables liées à la pandémie, 
# ce qui peut être utile pour des modèles prédictifs ou des décisions en santé publique.



# Régression simple : Confirmed Cases
model1 <- lm(Death ~ Confirmed.Cases, data = CovidWorld)
summary(model1)
summary(resid(model1))
qqnorm(resid(model1), main = "Q-Q Plot des résidus pour Confirmed Cases")
qqline(resid(model1), col = "red")

# Régression simple : Active Cases
model2 <- lm(Death ~ Active.Cases, data = CovidWorld)
summary(model2)
summary(resid(model2))
qqnorm(resid(model2), main = "Q-Q Plot des résidus pour Active Cases")
qqline(resid(model2), col = "red")

# Régression simple : Cured Discharged
model3 <- lm(Deaths ~ Cured.Discharged, data = CovidWorld)
summary(model3)
summary(resid(model3))
qqnorm(resid(model3), main = "Q-Q Plot des résidus pour Cured Discharged")
qqline(resid(model3), col = "red")
)

# Régression multiple : Modèle avec toutes les variables explicatives
model_multiple <- lm(Deaths ~ ConfirmedIndianNational + ConfirmedForeignNational + Cured + Confirmed, data = CovidIndia)
summary(model_multiple)
summary(resid(model_multiple))
qqnorm(resid(model_multiple), main = "Q-Q Plot des résidus pour le modèle multiple")
qqline(resid(model_multiple), col = "red")

# Ce code présente les résumés statistiques de quatre modèles de régression linéaire (model1, model2, model3 et model_multiple) appliqués à différentes variables.
# Chaque modèle cherche à expliquer la variable "Death" en fonction de différentes variables indépendantes telles que "Confirmed.Cases", "Active.Cases", "Cured", etc.
# Le résumé de chaque modèle inclut les informations suivantes :
# 
# Modèle 1 :
# - Les résidus : Min = -6.742, Max = 9.670, Médian = 0.441, Moyenne = 0.144
#   Ces valeurs montrent que les résidus sont relativement symétriques autour de zéro, ce qui suggère un bon ajustement du modèle.
# - Les coefficients : 
#   - (Intercept) : Estimation = 1.128, p-value = 0.205 (pas significatif)
#   - Confirmed.Cases : Estimation = 0.098, p-value = 0.0001 (significatif)
#     Cela signifie qu'une augmentation d'un cas confirmé est fortement associée à une augmentation des décès.
#   - Active.Cases : Estimation = -0.019, p-value = 0.100 (pas significatif)
#     Bien que l'association avec les décès soit négative, elle n'est pas statistiquement significative à un niveau de 5%.
# - R² = 0.227, ce qui signifie que seulement 22,7% de la variation des décès est expliquée par ce modèle.
# - Erreur standard des résidus = 2.015, indiquant une dispersion modérée autour de la ligne de régression.

# Modèle 2 :
# - Les résidus : Min = -7.532, Max = 9.235, Médian = 0.472, Moyenne = 0.145
#   Les résidus sont également symétriques, indiquant un bon ajustement du modèle.
# - Les coefficients :
#   - (Intercept) : Estimation = 1.128, p-value = 0.199 (pas significatif)
#   - Active.Cases : Estimation = -0.023, p-value = 0.065 (tendance significative)
#     Une tendance est observée, mais ce n'est pas encore assez significatif à un seuil de 5%.
# - R² = 0.212, légèrement inférieur au modèle 1, ce qui signifie que ce modèle explique 21,2% de la variation des décès.

# Modèle 3 :
# - Les résidus : Min = -7.190, Max = 9.056, Médian = 0.413, Moyenne = 0.158
#   Les résidus sont similaires aux modèles précédents, ce qui indique un bon ajustement.
# - Les coefficients :
#   - (Intercept) : Estimation = 1.138, p-value = 0.206 (pas significatif)
#   - Cured : Estimation = 0.072, p-value = 0.045 (significatif)
#     La guérison a un effet positif et significatif sur les décès.
# - R² = 0.185, ce qui signifie que ce modèle explique 18,5% de la variation des décès, un peu moins que les autres modèles.

# Modèle multiple (régression combinée de plusieurs variables) :
# - Les résidus : Min = -7.493, Max = 9.116, Médian = 0.424, Moyenne = 0.136
#   Comme pour les autres modèles, les résidus sont relativement symétriques autour de zéro.
# - Les coefficients :
#   - (Intercept) : Estimation = 1.113, p-value = 0.205 (pas significatif)
#   - Confirmed.Cases : Estimation = 0.089, p-value = 0.0001 (significatif)
#   - Active.Cases : Estimation = -0.019, p-value = 0.102 (pas significatif)
# - R² = 0.238, ce qui est légèrement supérieur aux autres modèles, ce qui suggère qu'il explique 23,8% de la variation des décès.

# En conclusion, les modèles 1 et multiple sont les plus significatifs, avec des R² respectivement de 22,7% et 23,8%. Les variables "Confirmed.Cases" sont constamment significatives, suggérant qu'elles jouent un rôle important dans l'explication des décès.









