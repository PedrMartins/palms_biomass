library(readxl)
library(dplyr)
source("function.R")
library(mgcv)
Biomass_palms_archontophoenix<- read_excel("biomass_data_matinha_USP.xlsx")
names (Biomass_palms_archontophoenix)[4:8] <-  c("DAP_mm", "altura_cm",
                                                 "biomassa_fresca_g",
                                                 "biomass_seca_g",
                                                 "percentage_dry_biomass")

Biomass_palms_archontophoenix$DAP_cm <- c(Biomass_palms_archontophoenix$DAP_mm/10)
Biomass_palms_archontophoenix$Transecto_Parcela <-  paste (Biomass_palms_archontophoenix$Transecto,
                                                   Biomass_palms_archontophoenix$Parcela,
                                                   sep="_")

fator_de_correção <- mean(Biomass_palms_archontophoenix$percentage_dry_biomass,
                          na.rm = TRUE)

Biomass_palms_archontophoenix$biomass_seca_g_estimada <-
  Biomass_palms_archontophoenix$biomassa_fresca_g * fator_de_correção



shapiro.test(log(Biomass_palms_archontophoenix$biomass_seca_g_estimada))

qqnorm(log (Biomass_palms_archontophoenix$biomass_seca_g_estimada))
qqline(log (Biomass_palms_archontophoenix$biomass_seca_g_estimada))
View (Biomass_palms_archontophoenix)
plot (biomass_seca_g_estimada~altura_cm
  ,data=Biomass_palms_archontophoenix)
barplot(biomass_seca_g_estimada,
        data = Biomass_palms_archontophoenix)
dev.off()

biomass_seca <- Biomass_palms_archontophoenix %>%
  group_by(Parcela) %>%
  summarise(total_biomass_seca_g = sum(biomass_seca_g_estimada,
                                na.rm = TRUE))

barplot(ind_par_transce$n~ind_par_transce$Transecto,
        ylim = c(0,800), width=1)

barplot(ind_par_parcela$n~ind_par_parcela$Parcela, width=1)

barplot(biomass_seca$Transecto~biomass_seca$total_biomass_seca_g)

#Biomass_palms_archontophoenix [Biomass_palms_archontophoenix$percentage_dry_biomass == 0,7] <- "NA"

x <- class_DBH_alt (Biomass_palms_archontophoenix, class = c(5,15,50,150))
class_DBH_alt (Biomass_palms_archontophoenix, class = c(1.5, 5), dbh_alt = "dbh")

y <- class_DBH_alt (Biomass_palms_archontophoenix, dbh_alt = "alt",
               class = c(5,15,30,50,150,200),
               choice = "bio")

class_DBH_alt (Biomass_palms_archontophoenix,
               class = 8)

barplot(y$Biomass_ab)



names (Biomass_palms_archontophoenix)
#View (Biomass_palms_archontophoenix)
#Biomass_palms_archontophoenix$DAP_cm < 7

head (Biomass_palms_archontophoenix)

plot (biomass_seca_g_estimada~DAP_cm,
      data =Biomass_palms_archontophoenix)

barplot (biomass_seca_count$n)


bio_dap <- lm (biomass_seca_g_estimada~altura_cm+I(DAP_cm^2) , data =
                 Biomass_palms_archontophoenix)
typeof(bio_dap)
summary(bio_dap)
abline (bio_dap)


bio_dap_2 <- glm (biomassa_fresca_g~ DAP_cm * I(DAP_cm^2), data =
                 Biomass_palms_archontophoenix)
coefficients <- coef(bio_dap_2)
summary(bio_dap_2)
anova (bio_dap,bio_dap_2)





plot (biomassa_fresca_g~DAP_cm, data =
        Biomass_palms_archontophoenix)

barplot(x$Ind_percentage)
barplot(y$Class_Alt_cm)

boxplot(biomass_seca_g_estimada~Transecto,
        data=Biomass_palms_archontophoenix, log ="y")
barplot(biomassa_fresca_g~Parcela,
        data=Biomass_palms_archontophoenix, log ="y")


dev.off()
par (mfrow =c(2,2))
plot (bio_dap_2)
abline (bio_dap)
abline (bio_dap_2)

curve (coefficients[1] +
         coefficients[2]*x+
         coefficients[3]*x+
         coefficients[4]*x
       , add=TRUE)



bio_alt <- lm (biomassa_fresca_g~altura_cm, data =
      Biomass_palms_archontophoenix)
summary(bio_alt)
plot (biomassa_fresca_g~altura_cm, data =
        Biomass_palms_archontophoenix)
abline (bio_alt)
abline (h=mean(Biomass_palms_archontophoenix$biomassa_fresca_g,
             na.rm = TRUE))
abline (v=mean(Biomass_palms_archontophoenix$altura_cm,
               na.rm = TRUE))


alt_dap <- lm (DAP_cm~altura_cm, data =
                 Biomass_palms_archontophoenix)
summary(alt_dap)
plot (DAP_cm~altura_cm, data =
        Biomass_palms_archontophoenix)
abline (alt_dap)

