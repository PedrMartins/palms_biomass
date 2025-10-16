library(readxl)
library(dplyr)
library(mgcv)
source("function.R")

# function class_DBH_alt (x, choice = "ind",
# class = 5, dbh_alt="alt", distribution = FALSE) default

############Processing data#####
Biomass_palms_archontophoenix<- read_excel("biomass_data_matinha_USP.xlsx")
names (Biomass_palms_archontophoenix)[4:8] <-  c("DAP_mm", "altura_cm",
                                                 "biomassa_fresca_g",
                                                 "biomass_seca_g",
                                                 "percentage_dry_biomass")

Biomass_palms_archontophoenix$DAP_cm <- c(Biomass_palms_archontophoenix$DAP_mm/10)

fator_de_correção <- mean(Biomass_palms_archontophoenix$percentage_dry_biomass,
                          na.rm = TRUE)

Biomass_palms_archontophoenix$biomass_seca_g_estimada <-
  Biomass_palms_archontophoenix$biomassa_fresca_g * fator_de_correção

Biomass_palms_archontophoenix <- Biomass_palms_archontophoenix[,-c(7,8,10)]

# View (Biomass_palms_archontophoenix [c(294,
#                                        444,
#                                        445),]) #dados influentes no modelo



#######exploratoty analyses######

anova (aov (biomass_seca_g_estimada ~ Transecto + Parcela,
     data = Biomass_palms_archontophoenix))

anova (aov (biomass_seca_g_estimada ~ Parcela,
            data = Biomass_palms_archontophoenix))
anova (aov (biomass_seca_g_estimada ~ Transecto,
            data = Biomass_palms_archontophoenix))

par (las = 1, bty = "n")

down_to_top <- range(
  log (Biomass_palms_archontophoenix$biomass_seca_g_estimada),
      na.rm = TRUE)

boxplot (log (biomass_seca_g_estimada )~ Transecto,
         data= Biomass_palms_archontophoenix,
         ylim = c(down_to_top[1] -1.5,
                  down_to_top [2] + 1.5),
         col= c("lightgreen","green","darkgreen"),
         pch = "*", xlab = "Parcela",
         ylab = "log Dry Biomass (g)")

legend("topleft" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Transecto 2","Transecto 3") #texto a ser escrito
       ,col=c ("lightgreen", "darkgreen")
       ,cex=.5		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte

#########teste de normalidade ########
source ("Processing_to_plot.R")

barplot(ind_par_transce$n~ind_par_transce$Transecto,
        ylim = c(0,800), width=1)

barplot(ind_par_parcela$n~ind_par_parcela$Parcela,
        ylab =" Indivíduos", xlab = "Parcela",
        ylim= c(0,250))


barplot(log (biomass_seca$total_biomass_seca_g)~
          biomass_seca$Parcela)


vioplot::vioplot(biomass_seca_g_estimada ~Transecto,
        data = Biomass_palms_archontophoenix, pch = "*", log = "y")

boxplot(biomass_seca_g_estimada ~Transecto,
                 data = Biomass_palms_archontophoenix, pch = "*", log = "y")

#########linear analyses#################
#diagnostico do modelo

Biomass_palms_archontophoenix$dap_square<- Biomass_palms_archontophoenix$DAP_cm^2

plot (Biomass_palms_archontophoenix [,c(5,11,13, 14)], pch = 20,
      col =rgb (0.3,0,0.5,0.3))

Biomass_palms_archontophoenix$pred <-  predict(lm_bio_H_Dsq, newdata = Biomass_palms_archontophoenix,
                                               na.rm = TRUE)

plot (Biomass_palms_archontophoenix[,c(5,13)], pch = 20,
      col =rgb (0.3,0,0.5,0.3) )
abline (lm_bio_h, col = "red",
      lty = 2)


curve (
  expr =  -105.3075 + 6.4434 *h + 5.2015 * x,
  col = "blue",
       lty=4,
  add =TRUE
  )



plot (Biomass_palms_archontophoenix[,c(11,5)], pch = 20,
      col =rgb (0.3,0,0.5,0.3))


plot (Biomass_palms_archontophoenix[,c(11,5)], pch = 20,
      col =rgb (0.3,0,0.5,0.3))

plot (Biomass_palms_archontophoenix[,c(14,5)], pch = 20,
      col =rgb (0.3,0,0.5,0.3))


lm_bio_full <- lm (biomass_seca_g_estimada~ altura_cm
                   * DAP_cm* I(DAP_cm^2)
                   * I(altura_cm^2)
                   , data =
                     Biomass_palms_archontophoenix)


lm_bio_simple <- lm (biomass_seca_g_estimada~ altura_cm * I(altura_cm^2) +
                       altura_cm:I(DAP_cm^2) +
                       DAP_cm:I(DAP_cm^2)
                     , data =
                       Biomass_palms_archontophoenix)
lm_bio_simple_extreme <- lm (biomass_seca_g_estimada~ altura_cm + DAP_cm, data =
                       Biomass_palms_archontophoenix) #melhor explicação biológico

lm_bio_h <- lm (biomass_seca_g_estimada~ altura_cm, data =
                               Biomass_palms_archontophoenix) #melhor explicação biológico

lm_bio_H_Dsq <- lm (biomass_seca_g_estimada~ altura_cm + I(DAP_cm^2), data =
                               Biomass_palms_archontophoenix)


summary(lm_bio_full)
summary (lm_bio_simple_extreme)
summary (lm_bio_simple)
summary (lm_bio_h)
summary (lm_bio_H_Dsq)

par (mfrow = c(2,2))
plot (lm_bio_H_Dsq)




shapiro.test(lm_bio_full$residuals)

AIC(lm_bio_h,
    lm_bio_H_Dsq,
    lm_bio_simple_extreme)

#resíduos sem normalidade

par (mfrow = c(2,2))
plot (lm_bio_full)
plot (lm_bio_simple)

boxplot (log (biomass_seca_g_estimada+1)~Transecto,
         data = Biomass_palms_archontophoenix)
boxplot.stats(log (Biomass_palms_archontophoenix$biomass_seca_g_estimada))

summary(lm_bio_full)
summary(lm_bio_simple)

anova(lm_bio_full,lm_bio_simple)

AIC (lm_bio_simple,
     lm_bio_full)


#########Classes alt e DBH###########


Biomass_by_alt_class=class_DBH_alt (Biomass_palms_archontophoenix, choice = "ind",
                 class = c(5,15,30,50,150),dbh_alt="alt",
                 distribution = FALSE)

Biomass_by_alt_class$Class_Alt_cm <- factor(Biomass_by_alt_class$Class_Alt_cm,
       levels = c(Biomass_by_alt_class$Class_Alt_cm))


Stats_dbh <- stats_DBH_Alt (Biomass_palms_archontophoenix,
                             dbh_alt = "dbh",
                             class=c(1,2,3,4,5,6,7))

Stats_alt <-stats_DBH_Alt (Biomass_palms_archontophoenix,
                            class= c(5,15,30,50,150),
                            dbh_alt = "alt")

colnames(Stats_alt)[-1] <- c("mean_biomass",
                               "Standard_deviation",
                               "Sample")

write.table(Stats_alt, "Stats_alt.csv",
            sep = "\t")

Biomass_by_alt_class <- Biomass_by_alt_class[order(Biomass_by_alt_class$Class_Alt_cm
                                                   , decreasing = TRUE), ]


barplot(Biomass_by_alt_class$Biomass_percentage,
        col = "lightgreen", ylim = c(0,50), ylab="Biomass (%)",
        xlab= "Alt Classes cm"
        )
mtext(c ("5","5-15",
         "15-30","30-50",
         "50-150","150"), side= 1,
      at = c(.75,1.90,3.15,4.25,5.5,6.75), cex = 0.75)

Biomass_by_dbh_class=class_DBH_alt (Biomass_palms_archontophoenix, choice = "bio",
                                    class = c(1,3,5,7),dbh_alt="dbh",
                                    distribution = FALSE)

Biomass_by_dbh_class <- Biomass_by_dbh_class[order(Biomass_by_dbh_class$Class_DAP_cm
                                                   , decreasing = FALSE), ]



barplot(Biomass_by_dbh_class$Biomass_percentage,
        col = "lightgreen", ylim = c(0,50), ylab="Biomass (%)",
        xlab= "Alt Classes"
)

mtext()

class(x)
str (x)
#####################################
