
# Les résidus doivent être uniformément dispersés
# au-dessus et au-dessous de zéro 



mod0 = lm(SalePrice ~ 1, data = cleaned_train)

modele_3 = stepAIC(mod0, SalePrice ~ MSSubClass + LotArea + LandContour + 
                      LotConfig + LandSlope + Neighborhood + Condition1 + OverallQual + 
                      OverallCond + Exterior1st + BsmtQual + BsmtCond + BsmtExposure + 
                      BsmtFinType1 + LowQualFinSF + GrLivArea + BsmtFullBath + 
                      HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + 
                      Functional + Fireplaces + GarageCars + GarageQual + WoodDeckSF + 
                      X3SsnPorch + ScreenPorch + SaleCondition + remod_since
                    ,
                    data =  cleaned_train, trace = TRUE, direction = c("forward"))

summary(modele_4)

## Sélection par BIC et stepwise

# k=log(napp) pour BIC au lieu de AIC.
modele_5 = stepAIC(modele_1, ~. ,trace = TRUE,
                      direction=c("both"), k = log(4))
summary(modele_4)
#Le modèle sélectionné est plus parcimonieux



## Sélection de modèle par pénalisation
## Ridge
# Comportement des coefficients
# Calcul des coefficients pour différentes valeurs du paramètre lambda.

library(MASS)

mod.ridge = lm.ridge(SalePrice ~ ., data = cleaned_train, 
                   lambda = seq(0,20,0.1))
par(mfrow=c(1,1))
plot(mod.ridge)
# évolution des coefficients
matplot(t(mod.ridge$coef),lty=1:3,type="l",col=1:10)
legend("top",legend=rownames(mod.ridge$coef),
       col=1:10,lty=1:3)






#Sélection de modèle par pénalisation
#Lasso
#Les résultats sont obtenus avec la librairie lasso2 ou avec la librairie
#glmnet
#5.1 Librairie Lasso2

library(lasso2)
l1c.P <- l1ce(SalePrice ~ ., cleaned_train, bound=(1:100)/100,absolute.t=FALSE)








plot.res(modele_1)


pairs(cleaned_train %>% select(where(is.numeric )) )


```{r}
library(MASS)  # Pour la fonction stepAIC
modele_step <- stepAIC(modele_1 ,~., trace = TRUE, direction=c("forward"))
summary(modele_step)
```




