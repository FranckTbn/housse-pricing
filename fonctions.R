
library(rpart)
library(styler)
library(ggridges) 
library(DataExplorer)
library(naniar)
library(tidyverse)
library(gt)
library(ggplot2)
library(labelled)

library(xgboost)

library(ggridges)
library(DataExplorer)
library(naniar)
library(tidyverse)
library(gt)
library(ggplot2)
library(labelled) 
library(patchwork)
library(forcats) 
library(moments) # skewness and kurtosis fonction

library(rlang) 
library(ggtext) # for adding text on graphique
library(glue) # for adding mean or or html element on graphics with ggtext

library(ggstats) # for proportion in bar plot

library(RColorBrewer) # forbeautifulcolor palette


library(ggpubr) # fonction gghistogram

library(highcharter)
graphics.off()


# Couleurs du thème

lkp_blue  <- grDevices::rgb(0, 34, 93, maxColorValue = 255)  # Bleu LinkPact
lkp_green <- grDevices::rgb(0, 136, 81, maxColorValue = 255) # Vert LinkPact
lkp_magenta <- grDevices::rgb(148, 0, 113, maxColorValue = 255) # Magenta LinkPact
lkp_grey <- grDevices::rgb(140, 142, 145, maxColorValue = 255) # Gris LinkPact
lkp_comp_blue <- grDevices::rgb(0, 113, 148, maxColorValue = 255) # Gris LinkPact
lkp_light_blue  <- grDevices::rgb(35, 95, 221, maxColorValue = 255)  # Bleu LinkPact
lkp_light_green <- grDevices::rgb(0, 227, 166, maxColorValue = 255) # Vert LinkPact

lkp_colors <- c(lkp_comp_blue, lkp_magenta, lkp_green,
                lkp_light_blue, lkp_light_green)














## beau affichage gt() personnalisé

gt_trabi <- function(data){
  data %>%
    gt() %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_column_labels(everything())
    ) %>% 
    opt_table_lines(extent = "default") %>%
    tab_options(
      column_labels.border.top.color = "white",
      column_labels.border.top.width = px(3),
      column_labels.border.bottom.color = "black",
      table_body.hlines.color = "white",
      table.border.bottom.color = "black",
      table.border.bottom.width = px(3)) %>%
    tab_source_note(md( "<br>@bineneothniel.tra <br> "))
}


# data ----
train <- read.csv("data/train.csv", na.strings = "NA")
test <- read.csv("data/test.csv", na.strings = "NA")
label <- train$SalePrice



a <- train %>% 
  select(-SalePrice) %>%
  mutate( type = "train")

b <- test %>%
  mutate( type = "test")


data <- bind_rows(a, b)


base_data <- read.csv("inputed_data.csv", na.strings = "NA")


# modalités uniques
data <- data %>% 
  select( -Id)

# quasi constante
#data <- data %>% 
#  select( -c(MiscVal, LowQualFinSF, PoolArea, PoolQC, X3SsnPorch)) 

# miscval and miscfeature have practicaly have the same information
data <- data %>% 
    select( -c(MiscVal, PoolArea)) 

# variable avec une modalité dominante dépassant 97%
# data <- data %>% 
#  select( -c(Utilities, Condition2, RoofMatl, Heating)) 


# Convertir les variables à étiquettes en facteurs



data$MSSubClass <- as.character(data$MSSubClass)
data$OverallQual <- as.character(data$OverallQual)
data$OverallCond <- as.character(data$OverallCond)

data$Fireplaces <- as.character(data$Fireplaces)
data$GarageCars <- as.character(data$GarageCars)
data$BsmtFullBath <- as.character(data$BsmtFullBath)
data$BsmtHalfBath <- as.character(data$BsmtHalfBath)

data$FullBath <- as.character(data$FullBath)

data$HalfBath <- as.character(data$HalfBath)
data$BedroomAbvGr <- as.character(data$BedroomAbvGr)
data$KitchenAbvGr <- as.character(data$KitchenAbvGr)

# à retirer

data$TotRmsAbvGrd <- as.character(data$TotRmsAbvGrd)

data$YrSold <- as.character(data$YrSold)
data$MoSold <- as.character(data$MoSold)


# Electrical

median_Electrical <- median(data$Electrical, na.rm = TRUE)
data <- data %>%
  mutate(Electrical = ifelse(is.na(Electrical), median_Electrical, Electrical))




## création de novelles variables

# YearBuilt
data <- data %>%
  mutate(housse_age = 2011 - YearBuilt) %>%
  select(-YearBuilt)

data <- data %>%
  mutate(housse_age = case_when(
    0 <= housse_age & housse_age <= 10 ~ "1",
    10 < housse_age & housse_age <= 20 ~ "2",
    20 < housse_age & housse_age <= 30 ~ "3",
    30 < housse_age & housse_age <= 40 ~ "4",
    40 < housse_age & housse_age <= 65 ~ "5",
    65 < housse_age & housse_age <= 80 ~ "6",
    80 < housse_age  ~ ">80"
  ))


# GarageYrBlt

data <- data %>%
  mutate(garage_age = 2011 - GarageYrBlt) %>%
  select(-GarageYrBlt)

data <- data %>%
  mutate(garage_age = case_when(
    is.na(garage_age) ~ "No_Garage",
    0 <= garage_age & garage_age <= 10 ~ "1",
    10 < garage_age & garage_age <= 20 ~ "2",
    20 < garage_age & garage_age <= 30 ~ "3",
    30 < garage_age & garage_age <= 40 ~ "4",
    40 < garage_age & garage_age <= 50 ~ "5",
    50 < garage_age & garage_age <= 60 ~ "6",
    60 < garage_age  ~ ">60"
  ))

# MasVnrArea




data <- data %>%
  mutate(MasVnrArea = case_when(
    is.na(MasVnrArea) ~ "None",
    MasVnrArea == 0 ~ "None",
    0 <= MasVnrArea & MasVnrArea <= 250 ~ "1",
    250 < MasVnrArea & MasVnrArea <= 500 ~ "2",
    500 < MasVnrArea  ~ ">500"
  ))


# GarageArea

data <- data %>%
  mutate(GarageArea = case_when(
    is.na(GarageArea) ~ "No_Garage",
    0 <= GarageArea & GarageArea <= 400 ~ "1",
    400 < GarageArea & GarageArea <= 600 ~ "2",
    600 < GarageArea & GarageArea <= 800 ~ "3",
    800 < GarageArea  ~ ">800"
  ))

# X1stFlrSF

data <- data %>%
  mutate(X1stFlrSF = case_when(
    is.na(X1stFlrSF) ~ "No.Garage",
    0 <= X1stFlrSF & X1stFlrSF <= 750 ~ "1",
    750 < X1stFlrSF & X1stFlrSF <= 1000 ~ "2",
    1000 < X1stFlrSF & X1stFlrSF <= 1250 ~ "3",
    1250 < X1stFlrSF & X1stFlrSF <= 1500 ~ "4",
    1500 < X1stFlrSF & X1stFlrSF <= 2000 ~ "5",
    2000 < X1stFlrSF  ~ ">2000"
  ))




# YearRemodAdd
data <- data %>%
  mutate(remod_since = 2011 - YearRemodAdd) %>%
  select(-YearRemodAdd)

data <- data %>%
  mutate(remod_since = case_when(
    0 <= remod_since & remod_since <= 10 ~ "1",
    10 <= remod_since & remod_since <= 20 ~ "1",
    20 < remod_since & remod_since <= 40 ~ "2",
    40 < remod_since & remod_since <= 60 ~ "3",
    60 < remod_since  ~ ">60"
  ))
#
##############

data <- data %>%
  mutate(TotalBsmtSF = case_when(
    TotalBsmtSF == 0 ~ "No.Basement",
    0 < TotalBsmtSF & TotalBsmtSF <= 300 ~ "1",
    300 <= TotalBsmtSF & TotalBsmtSF <= 600 ~ "2",
    600 < TotalBsmtSF & TotalBsmtSF <= 900 ~ "3",
    900 < TotalBsmtSF & TotalBsmtSF <= 1200 ~ "4",
    1200 < TotalBsmtSF & TotalBsmtSF <= 1500 ~ "5",
    1500 < TotalBsmtSF  ~ ">1500"
  ))


########################"
data <- data %>%
  mutate(BsmtFinSF1 = case_when(
    BsmtFinSF1 == 0 ~ "No.Basement",
    0 < BsmtFinSF1 & BsmtFinSF1 <= 250 ~ "1",
    250 <= BsmtFinSF1 & BsmtFinSF1 <= 500 ~ "2",
    500 < BsmtFinSF1 & BsmtFinSF1 <= 750 ~ "3",
    750 < BsmtFinSF1 & BsmtFinSF1 <= 1000 ~ "4",
    1000 < BsmtFinSF1 & BsmtFinSF1 <= 1500 ~ "5",
    1500 < BsmtFinSF1  ~ ">1500"
  ))

#################

data <- data %>%
  mutate(BsmtUnfSF = case_when(
    BsmtUnfSF == 0 ~ "No.Basement",
    0 < BsmtUnfSF & BsmtUnfSF <= 500 ~ "1",
    500 < BsmtUnfSF & BsmtUnfSF <= 1000 ~ "2",
    1000 < BsmtUnfSF   ~ ">1000"
  ))

##########################


data <- data %>%
  mutate( OpenPorchSF = ifelse(OpenPorchSF == 0, "No", "Yes"),
          EnclosedPorch = ifelse(EnclosedPorch == 0, "No", "Yes"),
          X3SsnPorch =  ifelse(X3SsnPorch == 0, "No", "Yes"),
          ScreenPorch = ifelse(ScreenPorch == 0, "No", "Yes"),
          LowQualFinSF = ifelse(LowQualFinSF == 0, "No", "Yes"),
          BsmtFinSF2 = ifelse(BsmtFinSF2 == 0, "No", "Yes")
          )








# discretisation ----

# LotArea

# data <- data %>%
#   mutate(LotArea = case_when(
#     0 <= LotArea & LotArea <= 2000 ~ "1",
#     2000 < LotArea & LotArea <= 4000 ~ "2",
#     4000 < LotArea & LotArea <= 6000 ~ "3",
#     6000 < LotArea & LotArea <= 8000 ~ "4",
#     8000 < LotArea & LotArea <= 10000 ~ "5",
#     10000 < LotArea & LotArea <= 12000 ~ "6",
#     12000 < LotArea & LotArea <= 14000 ~ "7",
#     14000 < LotArea & LotArea <= 16000 ~ "8",
#     16000 < LotArea & LotArea <= 18000 ~ "9",
#     18000 < LotArea & LotArea <= 20000 ~ "10",
#     20000 < LotArea & LotArea <= 25000 ~ "11",
#     
#     
#     25000 < LotArea & LotArea <= 50000 ~ "12",
#     50000 < LotArea  ~ ">50000",
#   ))



# GrLivArea
data <- data %>%
  mutate(GrLivArea = case_when(
    0 <= GrLivArea & GrLivArea <= 700 ~ "1",
    700 < GrLivArea & GrLivArea <= 1000 ~ "2",
    1000 < GrLivArea & GrLivArea <= 1300 ~ "3",
    1300 < GrLivArea & GrLivArea <= 1600 ~ "4",
    1600 < GrLivArea & GrLivArea <= 1800 ~ "5",
    1800 < GrLivArea & GrLivArea <= 2100 ~ "6",
    2100 < GrLivArea & GrLivArea <= 2800 ~ "7",
    2800 < GrLivArea  ~ ">2800",
  ))



# LotFrontage





# X2ndFlrSF
data <- data %>%
  mutate(X2ndFlrSF = case_when(
    X2ndFlrSF == 0 ~ "No_2nd_floor",
    0 < X2ndFlrSF & X2ndFlrSF <= 500 ~ "0-500",
    500 < X2ndFlrSF & X2ndFlrSF <= 1000 ~ "500-1000",
    1000 < X2ndFlrSF & X2ndFlrSF <= 1500 ~ "1000-1500",
    1500 < X2ndFlrSF  ~ ">1500",
  ))

# WoodDeckSF

data <- data %>%
  mutate(WoodDeckSF = case_when(
    WoodDeckSF == 0 ~ "No_WoodDeckSF",
    0 < WoodDeckSF & WoodDeckSF <= 200 ~ "0-200",
    200 < WoodDeckSF  ~ ">200",
  ))


## corrélation entre  (àvérifier)

# data <- data %>% 
#  select(- c(BsmtFinSF1, BsmtFinSF2))


#  on remplace les missings
data <- data %>%
  mutate(
    Alley = ifelse(is.na(Alley), "No.alley.access", Alley),
    
    BsmtQual = ifelse(is.na(BsmtQual), "No.Basement", BsmtQual),
    BsmtCond = ifelse(is.na(BsmtCond), "No.Basement", BsmtCond),
    BsmtExposure = ifelse(is.na(BsmtExposure), "No.Basement", BsmtExposure),
    BsmtFinType1 = ifelse(is.na(BsmtFinType1), "No.Basement", BsmtFinType1),
    BsmtFinType2 = ifelse(is.na(BsmtFinType2), "No.Basement", BsmtFinType2),
    
    MiscFeature = ifelse(is.na(MiscFeature), "None", "Yes"),
    Fence = ifelse(is.na(Fence), "None", Fence),
    FireplaceQu = ifelse(is.na(FireplaceQu), "No.Fireplace", FireplaceQu),
    
    GarageType = ifelse(is.na(GarageType), "No.Garage", GarageType),
    GarageFinish = ifelse(is.na(GarageFinish), "No.Garage", GarageFinish),
    GarageQual = ifelse(is.na(GarageQual), "No.Garage", GarageQual),
    GarageCond = ifelse(is.na(GarageCond), "No.Garage", GarageCond),
    
    Pool = ifelse(is.na(PoolQC), "No.Pool", "Yes")
  ) %>%
  select(- PoolQC)



train1 <- data[1:nrow(train), ]

train1$SalePrice = label

vartt <- data %>% select_if(~ is.factor(.) || is.character(.)) %>% as.data.frame()

clean_data <- train1




viz_train_test <- function(data, var){ 
  
  var_name <- as_label(enquo(var))
  
  data %>%
    mutate({{var}} := as.factor({{var}})) %>%
  ggplot( aes( x = {{var}}, fill = type) ) +
    geom_bar( position = "fill" ) +
    labs(
      title = paste("repartion des moadlités de ", var_name, " entre train et test"),
      x = var_name,
      y = ""
    ) +
    theme_minimal()
  
  }



# variable qualitative ----

var_quali_viz_1 <- function(data, var){
  
  var_name <- as_label(enquo(var))
  
  data %>%
    mutate({{var}} := as.factor({{var}})) %>%
    ggplot(mapping = aes(x = fct_infreq({{var}}), y = after_stat(prop), by = 1)) +
    geom_bar(colour = 4, fill = "white", stat = "prop") +
    labs(x = var_name, y = "Frequency") +
    theme(axis.title.x = element_text(size = 15,
                                      color = lkp_colors,
                                      face = "bold"),
          axis.title.y = element_text(size = 10,
                                      color = "red",
                                      face = "italic")) +
    scale_y_continuous(labels = scales::label_percent())
}



var_quali_viz_2 <- function(data, var){
  
  var_name <- as_label(enquo(var))
  
  
  data %>%
    mutate({{var}} := as.factor({{var}})) %>%
    ggplot(mapping = aes(
      x = forcats::fct_infreq({{var}}),
      y = after_stat(prop),
      by = 1
    )) +
    geom_bar(colour = "black",
             fill = "#4477AA",
             stat = "prop") +
    labs(title = var_name, y = "Frequency") +
    theme(
      axis.title.x = element_text(
        size = 15,
        color = "blue",
        face = "bold"
      ),
      axis.title.y = element_text(
        size = 10,
        color = "red",
        face = "italic"
      )
    ) +
    geom_text(
      aes(label = after_stat(prop) %>%
            scales::percent(accuracy = .1)),
      stat = "prop",
      nudge_y = .03
    ) +
    scale_y_continuous(labels = scales::label_percent()) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_blank()
    ) +
    xlab(NULL) + ylab(NULL) 
  
}




var_quali_viz_3 <- function(data, class){
  
  var_name <- as_label(enquo(class))
  
  
  data <- data %>%
    count({{class}}) %>%
    mutate(prop = n / sum(n) * 100,
           label = paste0(round(prop, 1), "%"))
  
  ggplot(data, aes(x = "", y = prop, fill = {{class}})) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y", start = 0) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 5
    ) +
    scale_color_brewer(palette = "Dark2") +
    labs(x = NULL, y = NULL, fill = var_name) +
    theme_void()+
    ggtitle("Répartition par groupe")
}




 # for color in camenber and bar plot

# Fonction de visualisation de camembert
var_quali_viz_4 <- function(data, class){
  
  var_name <- as_label(enquo(class))
  
  
  data <- data %>%
    count({{class}}) %>%
    mutate(prop = n / sum(n) * 100,
           label = paste0(round(prop, 1), "%"))
  
  ggplot(data, aes(x = 2, y = prop, fill = {{class}})) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y", start = 0) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 5
    ) +
    scale_color_brewer(palette = "Dark2") +
    labs(x = NULL, y = NULL, fill = var_name) +
    theme_void()+
    xlim(0.5, 2.5) +
    ggtitle("Répartition par groupe")
}


# variable quantitative ----





var_quanti_viz_1 <- function(data, var){
  
  var_name <- as_label(enquo(var))
  
  # Graphiques basiques
  gghistogram(
    data = data,
    x = var_name,
    fill = "steelblue",
    alpha = 1,
    bins = 15,
    add = "median",
    add_density = TRUE,
    rug = TRUE,
    title = paste("Histogram Plot of", var_name),
    xlab = var_name,
    ylab = "",
  )
  
}




# ajouter le premier quartile, , 3, le min et max

library(dplyr)
library(rlang)
library(ggplot2)
library(glue)
library(ggtext)
library(e1071) # or moments

var_quanti_viz_2 <- function(data, var, stat = FALSE, adjust = 0.8) {
  var_name <- as_label(enquo(var))
  
  # Check if the variable exists in the dataset
  if (!var_name %in% colnames(data)) {
    stop("The variable does not exist in the dataset")
  }
  
  # Calculate basic statistics
  min_value <- min(data[[var_name]], na.rm = TRUE)
  q1_value <- quantile(data[[var_name]], probs = 0.25, na.rm = TRUE)
  median_value <- quantile(data[[var_name]], probs = 0.5, na.rm = TRUE)
  mean_value <- mean(data[[var_name]], na.rm = TRUE)
  q3_value <- quantile(data[[var_name]], probs = 0.75, na.rm = TRUE)
  max_value <- max(data[[var_name]], na.rm = TRUE)
  
  # Additional statistics if stat is TRUE
  if (stat == TRUE) {
    skewness_value <- skewness(data[[var_name]], na.rm = TRUE)
    kurtosis_value <- kurtosis(data[[var_name]], na.rm = TRUE)
  }
  
  # Create the plot
  p <- ggplot(data = data, mapping = aes(x = !!enquo(var))) +
    geom_density(
      fill = "steelblue",
      alpha = 0.3,
      color = "lightblue",
      adjust = adjust
    ) +
    { 
      if (stat == TRUE) {
        annotate(
          "richtext",
          x = Inf,
          y = Inf,
          label = glue::glue(
            "<b><span style='color:blue'>Mean    :</span></b> {round(mean_value, 2)} <br> 
            <b><span style='color:red'>Median :</span></b> {round(median_value, 2)} <br> 
            <b>Skewness:</b> {round(skewness_value, 2)} <br>
            <b>Kurtosis:</b> {round(kurtosis_value, 2)}"
          ),
          hjust = 1.1,
          vjust = 1.1,
          fill = "cornsilk",
          label.color = NA,
          color = "black",
          size = 4
        ) +
          geom_vline(
            aes(xintercept = median_value),
            color = "red",
            linetype = "dotted",
            linewidth = 1
          )
      } else {
        annotate(
          "richtext",
          x = Inf,
          y = Inf,
          label = glue::glue(
            "<b>Min    :</b> {round(min_value, 2)}  <br>
            <b>1st Qu    :</b> {round(q1_value, 2)}  <br>
            <b>Median    :</b> {round(median_value, 2)}  <br>
            <b><span style='color:blue'>Mean    :</span></b> {round(mean_value, 2)}  <br>
            <b>3rd Qu    :</b> {round(q3_value, 2)}  <br>
            <b>Max    :</b> {round(max_value, 2)}"
          ),
          hjust = 1.1,
          vjust = 1.1,
          fill = "cornsilk",
          label.color = NA,
          color = "black",
          size = 4
        )
      }
    } +
    geom_vline(
      aes(xintercept = mean_value),
      color = "blue",
      linetype = "dashed",
      linewidth = 1
    ) +
    labs(
      title = paste("Density Plot of", var_name),
      x = var_name,
      y = "Density"
    ) +
    theme_minimal()
  
  p
}



var_quanti_viz_3 <- function(data = train, var){
  
  var_name <- as_label(enquo(var))
  
  data %>%
    ggplot( aes(x = factor(1)  , y = {{var}})) + 
    stat_boxplot(geom = "errorbar",
                 width = 0.25) +
    geom_boxplot(fill = "dodgerblue1",
                 colour = "black",
                 alpha = 0.7,
                 outlier.colour = "tomato2",
                 outlier.shape = 16,
                 outlier.size = 2) +
    labs(x = var_name , y = "") + 
    theme(axis.title.x = element_text(size = 15,
                                      color = "blue",
                                      face = "bold"),
          axis.title.y = element_text(size = 10,
                                      color = lkp_blue,
                                      face = "italic")) +
    theme_minimal()
}

# var_quanti_viz_3(data = train, SalePrice)




# variables quantitative / quantitative (à refaire) ----

library(ggplot2)
library(rlang)
library(dplyr)
library(broom)

var_cible_quanti <- function(data, var) {
  
  var_name <- as_label(enquo(var))
  
  # Filtrer les données pour retirer les valeurs non finies
  data <- data %>%
    filter(!is.na(!!enquo(var)), !is.na(SalePrice))
  
  # Créer une formule de régression dynamiquement
  formula <- as.formula(paste("SalePrice ~", var_name))
  
  # Ajuster un modèle de régression linéaire
  model <- lm(formula, data = data)
  
  model_summary <- summary(model)
  coef <- coef(model)
  intercept <- coef[1]
  slope <- coef[2]
  r_squared <- model_summary$r.squared
  p_value <- coef(summary(model))[2, "Pr(>|t|)"]
  
  # Créer le texte d'annotation
  significance <- ifelse(p_value < 0.05, "Significatif", "Non significatif")
  annotation_text <- paste(
    sprintf("R² = %.2f", r_squared),
    sprintf("p-value = %.4f", p_value), 
    sprintf("Coefficient = %.4f", slope),
    significance,
    sep = "\n"
  )
  
  # Créer un graphique avec ggplot2
  p <- ggplot(data, aes(x = !!enquo(var), y = SalePrice)) +
    geom_point(color = "black", size = 1.5) +                            # Ajouter des points
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(
      title = paste(var_name, " ~  SalePrice"),
      x = var_name,
      y = "SalePrice"
    )+
    theme_classic() +
    theme(axis.title.x = element_text(size = 15,
                                      color = lkp_blue, # Assurez-vous de passer une couleur de type scalaire
                                      face = "bold"),
          axis.title.y = element_text(size = 10,
                                      color = "red", # Assurez-vous de passer une couleur de type scalaire
                                      face = "italic"),
          plot.title = element_text(hjust = 0.5,
                                    face = "bold")
    ) +
    annotate(
      "richtext",
      x = Inf,
      y = Inf,
      label = glue("
R² = {sprintf('%.2f', r_squared)} <br>
p-value = {sprintf('%.4f', p_value)} <br>
Coefficient = {sprintf('%.4f', slope)} <br>
{significance}  
"),
      hjust = 1.1, vjust = 1.1,
      fill = "cornsilk",
      label.color = NA,
      color = "black",
      size = 4, fontface = "bold")
  
  # Afficher le graphique
  print(p)
}


# variables qualitative / quantitative ----

custom_theme <- theme(
  axis.title.x = element_text(size = 15, color = "darkblue", face = "bold"),
  axis.title.y = element_text(size = 13, color = "darkred", face = "italic"),
  axis.text.x = element_text(size = 12, color = "darkblue", face = "italic"),
  axis.text.y = element_text(size = 10, color = "darkred", face = "bold"),
  plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
  panel.background = element_rect(fill = "#F7F9F2"),
  panel.grid.major = element_line(color = "white"),
  panel.grid.minor = element_line(color = "#F7F9F2"),
  legend.position = "bottom",
  legend.background = element_rect(fill = "lightblue"),
  legend.title = element_text(size = 12, face = "bold"),
  legend.text = element_text(size = 10)
)

library(dplyr)
library(ggplot2)
library(rlang)

R2_coef <- function(y_pred, y){
  sst <- sum((y - mean(y))^2)
  sse <- sum((y_pred - y)^2)
  rsq <- 1 - sse/sst
  return(rsq)
}

RMSE <- function(y_pred, y) {
  # Calcul de la somme des erreurs au carré
  sse <- sum((y_pred - y)^2)
  # Calcul du nombre d'observations
  n <- length(y)
  # Calcul de la RMSE
  rmse <- sqrt(sse / n)
  return(rmse)
}

var_cible_quali_1 <- function(data, var,  label){
  
  var_name <- as_label(enquo(var))
  label_name <- as_label(enquo(label))
  
  data[[var_name]] <- as.character(data[[var_name]])
  n <- nrow(data)
  
  grouped_data <- data %>%
    group_by({{var}}) %>%
    summarise(moyenne = mean(!!sym(label_name), na.rm = TRUE))  # Calculate mean SalePrice for each group
  
  y <- rep(0, times = n)
  
  for (i in 1:n){
    ind <- which(grouped_data[[1]] == data[[var_name]][i])
    y[i] <- grouped_data$moyenne[ind]
  }
  
  R2 <- R2_coef(y, data[[label_name]])
  RMSE <- RMSE(y, data[[label_name]])
  
  grouped_data %>%
    ggplot(mapping = aes( x = {{var}}, y = moyenne,  ))+
    geom_col(fill = "#219C90",
             colour = "white",) +
    labs(x = var_name, y = paste0("mean of ", label_name),  title ="" ) +
    annotate(
      "richtext",
      x = Inf,
      y = Inf,
      label = glue(
        "<b><span style='color:blue'>R2      :</span></b> {round(R2, 3)} <br> 
          <b><span style='color:blue'>RMSE      :</span></b> {round(RMSE, 3)} "
      ),
      hjust = 1.1,
      vjust = 1.1,
      fill = "#D8EFD3",
      label.color = NA,
      color = "black",
      size = 4
    ) + custom_theme
}
var_cible_quali_1(train, MSSubClass, SalePrice)


var_cible_quali_2 <- function(data = train, var){
  
  var_name <- as_label(enquo(var))
  
  data %>%
    ggplot( aes(x = {{var}} , y = SalePrice)) + 
    stat_boxplot(geom = "errorbar",
                 width = 0.25) +
    geom_boxplot(fill = "dodgerblue1",
                 colour = "black",
                 alpha = 0.5,
                 outlier.colour = "tomato2") +
    labs(x = var_name, y = "SalePrice") + 
    theme(axis.title.x = element_text(size = 15,
                                      color = "blue",
                                      face = "bold"),
          axis.title.y = element_text(size = 10,
                                      color = "red",
                                      face = "italic"))
}




var_cible_quali_3 <- function(data = train, var){
  
  var_name <- as_label(enquo(var))
  
  data %>% 
    ggplot( aes(x = SalePrice, y = {{var}}, fill = {{var}})) +
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none") +
    labs(x = var_name, y = "SalePrice", title = "distribution de prix par zonne") 
}



# missing value ----

missing_viz_1 <- function(data, class){
  
  var_name <- as_label(enquo(class))
  
    data <- data %>%
      dplyr::select({{class}}) %>%
      mutate(
        observation = ifelse(is.na({{class}}), "missing", "value")
      )
    
    data <- data %>%
      count(observation) %>%
      mutate(prop = n / sum(n) * 100,
             label = paste0(round(prop, 1), "%"))
    
    ggplot(data, aes(x = 2, y = prop, fill = observation)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar(theta = "y", start = 0) +
      geom_text(
        aes(label = label),
        position = position_stack(vjust = 0.5),
        color = "white",
        size = 5
      ) +
      scale_color_brewer(palette = "Dark2") +
      labs(x = NULL, y = NULL) +
      theme_void()+
      xlim(0.5, 2.5) +
      ggtitle( paste0(var_name, " Missing Value") )
    
}

# missing_viz_1(data = train, class = LotFrontage )



missing_loliplot <- function(data){
  miss_data <- data %>%
    questionr::freq.na() %>%
    as.data.frame() %>%
    rownames_to_column("variables") %>%
    filter( missing > 0 )
  
  
  missing <- miss_data %>% distinct(variables) %>% as.vector()
  gg_miss_var( data %>% dplyr::select(missing$variables) )
}


missing_variables <- function(data){
  miss_data <- data %>%
    questionr::freq.na() %>%
    as.data.frame() %>%
    rownames_to_column("variables") %>%
    filter( missing > 0 )
  
  miss_data %>%
    gt_trabi()
}



# install.packages("BiocManager")

# BiocManager::install("ComplexHeatmap")

library(ComplexHeatmap)
library(circlize)






# Define the missing_heatmap function
missing_heatmap <- function(data) {
  miss_data <- data %>%
    questionr::freq.na() %>%
    as.data.frame() %>%
    rownames_to_column("variables") %>%
    filter(missing > 0)
  
  missing <- miss_data %>% distinct(variables) %>% pull(variables)
  
  # Convert data to a matrix
  data_matrix <- as.matrix(data %>% dplyr::select(all_of(missing)))
  
  # Create a logical matrix indicating missing values
  missing_data <- is.na(data_matrix)
  
  # Define a color palette
  col_fun <- colorRamp2(c(FALSE, TRUE), c("lightblue", "tomato"))
  
  # Create the heatmap
  Heatmap(
    missing_data,
    height = 600,
    show_heatmap_legend = FALSE,
    name = "Missing Values",
    col = col_fun,
    show_row_names = TRUE,
    show_column_names = TRUE,
    column_names_side = "top",
    column_names_gp = gpar(fontsize = 10),
    row_names_gp = gpar(fontsize = 10),
    heatmap_legend_param = list(
      title = "Missing",
      at = c(FALSE, TRUE),
      labels = c("value", "missing")
    ),
    cluster_rows = FALSE,
    cluster_columns = FALSE
  )
}

missing_heatmap(train)


library(circlize)

library(reshape2)
corelation_heatmap <- function(data){
  
  numeric_train <- data %>% select_if(is.numeric)
  
  # Calculate the correlation matrix
  cor_matrix <- cor(numeric_train)
  
  # Reshape the correlation matrix
  melted_cor_matrix <- melt(cor_matrix)
  
  # Create the heatmap
  ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1, 1), space = "Lab", 
                         name = "Correlation") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 5, hjust = 1)) +
    coord_fixed() +
    labs(title = "Correlation Heatmap",
         x = "Variables",
         y = "Variables")
  
}



var_type_viz_1 <- function(data){
  # nb_int <- sum(sapply(data, is.integer))
  nb_num <- sum(sapply(data, is.numeric))
  nb_char <- sum(sapply(data, is.character))
  nb_log <- sum(sapply(data, is.logical)) 
  nb_fact <- sum(sapply(data, is.factor))
  
  
  df <- data.frame(
    type = c("numeric", "character", "logical", "factor" ),
    freq = c(nb_num, nb_char, nb_log, nb_fact)
  ) %>%
    arrange(desc(freq))
  
  df %>%
    hchart(
      type = "column", hcaes(x = "type", y = "freq"),
      color = "steelblue"
    ) %>%
    hc_title(
      text = "Bar Plots",
      style = list(fontWeight = "bold", fontSize = "30px"),
      align = "center"
    ) %>% 
    hc_subtitle(
      text = "variables types", 
      style = list(fontWeight = "bold"),
      align = "center"
    ) %>% 
    hc_credits(
      enabled = TRUE, 
      text = "md(Data Source: kaggle: https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques))",
      style = list(fontSize = "10px")
    ) %>%
    hc_xAxis(title = list(text = "type")) %>%
    hc_yAxis(title = list(text = "frequency")) %>%
    hc_add_theme(hc_theme_economist())
  
  
}

data_information <- function(data) {
  if(!is.data.frame(data)) {
    cat("The input is not a data frame.\n")
    return(NULL)
  }
  
  cat("Basic Information about the Dataset:\n")
  cat("-----------------------------\n")
  
  # Dimensions of the dataset
  cat("Dimensions (rows x columns): ", dim(data)[1], " x ", dim(data)[2], "\n\n")
  
  # Column names
  cat("Column Names:\n")
  cat(paste(names(data), collapse = ", "), "\n\n")
  
  # Summary of each column
  cat("Summary of each column:\n")
  for (col in names(data)) {
    cat(col, ":\n")
    cat(paste(capture.output(summary(data[[col]])), collapse = "\n"), "\n\n")
  }
  
  # Number of missing values
  cat("Number of missing values in each column:\n")
  missing_values <- colSums(is.na(data))
  for (col in names(missing_values)) {
    cat(col, ": ", missing_values[col], "\n")
  }
  
  # Data types of each column
  cat("\nData types of each column:\n")
  for (col in names(data)) {
    cat(col, ": ", class(data[[col]]), "\n")
  }
}



numerical_corelation <- function(data, threshold = 0.6){
  cor_matrix <- cor(data %>% dplyr::select(where(is.numeric )))
  adj_matrix <- abs(cor_matrix) > threshold
  diag(adj_matrix) <- 0
  # Create a graph from the adjacency matrix
  cor_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)
  
  # Identify connected components
  components <- components(cor_graph)
  
  # Extract the membership vector indicating component membership for each variable
  membership <- components$membership
  
  # Create a data frame to list the variables in each connected component
  connected_vars <- data.frame(variable = names(membership), component = membership)
  
  # Group and print the connected variables
  connected_groups <- connected_vars %>%
    group_by(component) %>%
    summarise(variables = paste(variable, collapse = ", ")) %>%
    filter(sapply(strsplit( variables, "\\s+"),  length) > 1)
  if (nrow(cor_matrix) > 15){
    return(list(
      data = connected_groups
    ))
  }else{
    graph1 <- plot(cor_graph)
    
    graph2 <- pairs(data %>% dplyr::select(where(is.numeric )) )
    
    return(list(
      data = connected_groups,
      graph1 = graph1,
      graph2 = graph2
    ))
  }
  
  
}


library(vcd)

# Fonction pour calculer le V de Cramer
cramers_v <- function(x, y) {
  chi2 <- chisq.test(table(x, y))
  n <- sum(table(x, y))
  phi2 <- chi2$statistic / n
  r <- nrow(table(x, y))
  k <- ncol(table(x, y))
  v <- sqrt(phi2 / min(r - 1, k - 1))
  return(v)
}


# Fonction pour créer la matrice des V de Cramer
categorical_corelation <- function(data, threshold = 0.7) {
  
  cat_data <- data %>%
    mutate(across(where(is.character), as.factor)) %>%
    dplyr::select(where(is.factor ))
  
  cat_vars <- cat_data  %>% colnames()
  
  cramers_v_matrix <- matrix(0, nrow = length(cat_vars), ncol = length(cat_vars), 
                             dimnames = list(cat_vars, cat_vars))
  
  for (i in 1:length(cat_vars)) {
    for (j in 1:length(cat_vars)) {
      if (i != j) {
        cramers_v_matrix[i, j] <- cramers_v(cat_data[[i]], cat_data[[j]])
      }
    }
  }
  
  adj_matrix <- abs(cramers_v_matrix) > threshold
  diag(adj_matrix) <- 0
  # Create a graph from the adjacency matrix
  cor_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)
  graph1 <- plot(cor_graph)
  
  components <- components(cor_graph)
  
  # Extract the membership vector indicating component membership for each variable
  membership <- components$membership
  
  # Create a data frame to list the variables in each connected component
  connected_vars <- data.frame(variable = names(membership), component = membership)
  
  # Group and print the connected variables
  connected_groups <- connected_vars %>%
    group_by(component) %>%
    summarise(variables = paste(variable, collapse = ", ")) %>%
    filter(sapply(strsplit( variables, "\\s+"),  length) > 1)
  
  
  
  return(list(
    data = connected_groups,
    graph1 = graph1
  ))
  
}


create_chi2_pvalue_matrix <- function(data) {
  
  cat_data <- data %>%
    mutate(across(where(is.character), as.factor)) %>%
    dplyr::select(where(is.factor ))
  
  cat_vars <- cat_data %>% colnames()
  pvalue_matrix <- matrix(0, nrow = length(cat_vars), ncol = length(cat_vars), 
                          dimnames = list(cat_vars, cat_vars))
  
  for (i in 1:length(cat_vars)) {
    for (j in 1:length(cat_vars)) {
      if (i != j) {
        chi2_test <- chisq.test(table(cat_data[[i]], cat_data[[j]]))
        
        # Interprétation
        
        if (chi2_test$p.value < 0.05) {
          cat("Nous rejetons H0, liaisons entre : ", cat_vars[i], "et ", cat_vars[j]," .\n") }
        pvalue_matrix[i, j] <- chi2_test$p.value
      }
    }
  }
  
  return(pvalue_matrix)
}




library(broom)

viz_lm_model_1 <- function(model, n){
  model_summary <- tidy(model) %>%
    filter(term != "(Intercept)") %>%
    arrange(p.value) %>%
    head(n)
  ggplot(model_summary, aes(x = term, y = estimate)) +
    geom_segment(aes(xend = term, yend = 0), color = "grey") +
    geom_point(size = 2, color = "blue") +
    coord_flip() +
    labs(title = "Importance des variables dans le modèle de régression",
         x = "Variables",
         y = "Estimation des coefficients") +
    theme_minimal()
}

viz_rf_model_2 <- function(model){
  p1 <- importance(model) %>%
    data.frame() %>%
    rownames_to_column("term") %>%
    ggplot(aes(x = term, y = X.IncMSE)) +
    geom_segment(aes(xend = term, yend = 0), color = "grey") +
    geom_point(size = 2, color = "blue") +
    coord_flip() +
    labs(title = "random forest fearture importance",
         x = "",
         y = "%IncMSE") +
    theme_minimal()
  
  p2 <- importance(model) %>%
    data.frame() %>%
    rownames_to_column("term") %>%
    ggplot(aes(x = term, y = IncNodePurity)) +
    geom_segment(aes(xend = term, yend = 0), color = "grey") +
    geom_point(size = 2, color = "blue") +
    coord_flip() +
    labs(title = "random forest fearture importance",
         x = "",
         y = "IncNodePurity") +
    theme_minimal()
  
  p1+p2
}

plot.res=function(modele_1,titre=""){
  plot(predict(modele_1), residuals(modele_1), col="black",ylab="Résidus",
       xlab="Valeurs predites",main=titre)
  abline(h=0, col="red",lwd=2)
}

R2_coef <- function(y_pred, y){
  sst <- sum((y - mean(y))^2)
  sse <- sum((y_pred - y)^2)
  rsq <- 1 - sse/sst
  rsq
}


cat_feature_importance <- function(data, label) {
  var_name <- as_label(enquo(label))
  
  cat_data <- data %>%
    mutate(across(where(is.character), as.factor)) %>%
    dplyr::select(where(is.factor))
  
  cat_vars <- colnames(cat_data)
  #cat_vars <- cat_vars[1:3]  # Adjust the number of variables to be considered
  n <- nrow(data)
  
  R2 <- c()
  for (var in cat_vars) {
    grouped_data <- data %>%
      group_by(!!sym(var)) %>%
      summarise(mean_SalePrice = mean(!!sym(var_name), na.rm = TRUE))  # Calculate mean SalePrice for each group
    
    y <- rep(0, times = n)
    
    for (i in 1:n) {
      ind <- which(grouped_data[[1]] == data[[var]][i])
      y[i] <- grouped_data$mean_SalePrice[ind]
    }
    R2 <- c(R2, R2_coef(y, data[[var_name]]))
    
  }
  
  df <- data.frame(variables = cat_vars, R2 = R2)
  
  df %>%
    arrange(desc(R2)) %>%  # Arrange by R² in descending order
    mutate(variables = factor(variables, levels = rev(variables))) %>%  # Set the order of factors
    ggplot(aes(x = variables, y = R2)) +
    geom_segment(aes(xend = variables, yend = 0), color = "grey") +
    geom_point(size = 2, color = "blue") +
    coord_flip() +
    labs(title = "Variables catégorielles qui expliquent le mieux la variance de SalePrice", x = "", y = "R²") +
    theme_minimal()
}








is_discretisable <- function(data, var, label) {
  
  var_name <- as_label(enquo(var))
  label_name <- as_label(enquo(label))
  
  
  formula_regress <- as.formula(paste(label_name, "~", var_name))
  formula_cart <- formula_regress
  formula_poly <- as.formula(paste(label_name, " ~ poly(", var_name, ", 2, raw = TRUE)"))
  
  
  
  model_regress <- lm(formula_regress, data = data)
  model_regress_summary <- summary(model_regress)
  r_regress_squared <- model_regress_summary$r.squared
  
  
  
  model_poly <- lm(formula_poly, data = data)
  model_poly_summary <- summary(model_poly)
  r_squared_poly <- model_poly_summary$r.squared
  
  
  cart_model <- rpart(formula_cart, data = data, method = "anova", control = rpart.control(minsplit = 10, minbucket = 5) )
  # minsplit: Minimum number of observations that must exist in a node in order for a split to be attempted.
  # minbucket: Minimum number of observations in any terminal (leaf) node. This controls the complexity of the tree.
  
  # Assigner chaque observation à un nœud
  data$node <- cart_model$where
  
  # Calculer la moyenne de chaque nœud
  node_means <- aggregate(as.formula(paste(label_name, "~ node")), data = data, FUN = mean)
  names(node_means)[2] <- "NodeMean"
  
  
  # Fusionner les moyennes des nœuds avec les données d'origine
  data <- merge(data, node_means, by = "node")
  
  # Créer des bar plots pour chaque nœud
  plot1 <- ggplot(data, aes(x = as.factor(node), y = {{label}})) +
    geom_bar(fill = "#219C90",
             stat = "summary",
             fun = "mean") +
    stat_summary(
      fun = "mean",
      geom = "text",
      aes(label = round(..y.., 2)),
      vjust = -0.5,
      color = "black"
    ) +
    labs(title = "",
         x = "Nœud",
         y = paste0("Moyenne de ", label_name)) +
    custom_theme
  
  # Calculer les proportions pour chaque niveau de node
  proportions <- prop.table(table(data$node)) * 100
  
  plot2 <- ggplot(data, aes(x = as.factor(node), y = {{label}})) +
    geom_boxplot(fill = "#219C90") +
    labs(title = "", x = "Nœud", y = label_name) + annotate(
      "text",
      x = as.factor(unique(data$node)),
      y = rep(max(data[[label_name]]) + 1, length(unique(data$node))),
      label = paste0(round(proportions, 1), "%"),
      vjust = -0.5,
      size = 4
    )  + custom_theme
  
  # Afficher le R^2
  sst <- sum((data[[label_name]] - mean(data[[label_name]])) ^ 2)
  sse <- sum((data[[label_name]] - data$NodeMean) ^ 2)
  r_squared <- 1 - (sse / sst)
  
  mat <- matrix(
    c(
      "regress",
      "poly     ",
      "CART  ",
      round(r_regress_squared, 3),
      round(r_squared_poly, 3),
      round(r_squared, 3)
    ),
    nrow = 3,
    byrow = FALSE
  )
  
  # Convertir la matrice en une chaîne de caractères formatée pour afficher dans un sous-titre
  subtitle_text <- paste("- ", apply(mat, 1, paste, collapse = "       "), collapse = "\n")
  
  mat2 <- matrix(
    c(
      paste0(strrep("*", 50)),
      paste0(strrep("*", 40)),
      paste0(strrep("*", 30)),
      paste0(strrep("*", 20)),
      paste0(strrep("*", 10)),
      paste0(strrep("*", 5))
    ),
    nrow = 3,
    byrow = FALSE
  )
  
  cat(paste( mat2, collapse = "\n"))
  
  cat("            Si CART améliore le R2 \n")
  
  cat(paste0(strrep("*", 5)))
  cat("            discretise la variable \n")  
                                            
  
  
  mat2 <- matrix(
    c(
      paste0(strrep("*", 5)),
      paste0(strrep("*", 10)),
      paste0(strrep("*", 20)),
      paste0(strrep("*", 30)),
      paste0(strrep("*", 40)),
      paste0(strrep("*", 50))
    ),
    nrow = 3,
    byrow = FALSE
  )
  
  cat(paste( mat2, collapse = "\n"))
  cat("\n")
  cat("\n")
  cat(subtitle_text)
  
  combined_plot <- plot1 + plot2 +
    plot_layout(ncol = 2) +
    plot_annotation(title = var_name, subtitle = "")
  
  a <- sort(as.data.frame(cart_model$splits)$index)
  return(list(
    cuts = a,
    barplot = plot1,
    boxplot = plot2
  ))
  
}
is_discretisable(train, LotArea, SalePrice)

