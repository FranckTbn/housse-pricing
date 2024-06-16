
library(dplyr)
  
modele_1 = lm(SalePrice ~ Neighborhood +
                MSSubClass +
                MSZoning +
                LotArea +
                LotConfig +
                Condition1 +
                HouseStyle +
                MasVnrType +
                ExterQual +
                BsmtQual +
                OverallQual * OverallCond + 
                ExterQual +
                GarageType * GarageQual +
                FireplaceQu +
                KitchenQual +
                housse_age +
                garage_age +
                remod_since,
              data = train)


summary(modele_1)

plot.res=function(x,y,titre="")
{
  plot(x,y,col="blue",ylab="Résidus",
       xlab="Valeurs predites",main=titre)
  abline(h=0,col="green")
}

plot.res(predict(modele_1),residuals(modele_1))

files <- data.frame(
  Id = test$Id,
  SalePrice = predict(modele_1, test)
)

write.csv(files, file = "output.csv", row.names = FALSE)
