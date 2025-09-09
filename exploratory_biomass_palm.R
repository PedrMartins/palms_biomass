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
Biomass_palms_archontophoenix$Transecto_Parcela <-  paste (Biomass_palms_archontophoenix$Transecto,
                                                   Biomass_palms_archontophoenix$Parcela,
                                                   sep="_")

fator_de_correção <- mean(Biomass_palms_archontophoenix$percentage_dry_biomass,
                          na.rm = TRUE)

Biomass_palms_archontophoenix$biomass_seca_g_estimada <-
  Biomass_palms_archontophoenix$biomassa_fresca_g * fator_de_correção

View (Biomass_palms_archontophoenix [c(294,
                                       444,
                                       445),]) #dados influentes no modelo



#######exploratoty analyses######




#########teste de normalidade ########
source ("Processing_to_plot.R")
shapiro.test(log(Biomass_palms_archontophoenix$biomass_seca_g_estimada))

qqnorm(log (Biomass_palms_archontophoenix$biomass_seca_g_estimada))
qqline(log (Biomass_palms_archontophoenix$biomass_seca_g_estimada))


barplot(biomass_seca_g_estimada,
        data = Biomass_palms_archontophoenix)


barplot(ind_par_transce$n~ind_par_transce$Transecto,
        ylim = c(0,800), width=1)

barplot(ind_par_parcela$n~ind_par_parcela$Parcela, width=1)

barplot(biomass_seca$Transecto~biomass_seca$total_biomass_seca_g)

# Biomass_palms_archontophoenix [
#   Biomass_palms_archontophoenix$percentage_dry_biomass == 0,7] <- "NA"


#########linear analyses#################
#diagnostico do modelo

lm_bio_full <- lm (biomass_seca_g_estimada~ altura_cm * DAP_cm* I(DAP_cm^2) * I(altura_cm^2)
                   , data =
                     Biomass_palms_archontophoenix)


lm_bio_simple <- lm (biomass_seca_g_estimada~ altura_cm * I(altura_cm^2) +
                       altura_cm:I(DAP_cm^2) +
                       DAP_cm:I(DAP_cm^2)
                     , data =
                       Biomass_palms_archontophoenix)

lm_bio_simple_Without_Altsq_Interaction <- lm (biomass_seca_g_estimada~ altura_cm + I(altura_cm^2) +
                       altura_cm:I(DAP_cm^2) +
                       DAP_cm:I(DAP_cm^2)
                     , data =
                       Biomass_palms_archontophoenix)

lm_bio_simple_Without_Altsq <- lm (biomass_seca_g_estimada~ altura_cm  +
                                     altura_cm:I(DAP_cm^2) +
                                     DAP_cm:I(DAP_cm^2)
                                   , data =
                                     Biomass_palms_archontophoenix)

lm_bio_simple_Without_dapsqint <- lm (biomass_seca_g_estimada~ altura_cm * I(altura_cm^2)  +
                                     DAP_cm:I(DAP_cm^2)
                                   , data =
                                     Biomass_palms_archontophoenix)

lm_bio_simple_Without_dapsqintDap <- lm (biomass_seca_g_estimada~ altura_cm * I(altura_cm^2) +
                                     altura_cm:I(DAP_cm^2)
                                   , data =
                                     Biomass_palms_archontophoenix)

lm_bio_simple_Without_dapsqintDap_and_altinterca <- lm (biomass_seca_g_estimada~ altura_cm + I(altura_cm^2) +
                                           altura_cm:I(DAP_cm^2)
                                         , data =
                                           Biomass_palms_archontophoenix)
lm_bio_simple_Without_dapsqint_and_alrsq <- lm (biomass_seca_g_estimada~ altura_cm+
                                        DAP_cm:I(DAP_cm^2)
                                      , data =
                                        Biomass_palms_archontophoenix)
lm_bio_hiper_simple <- lm (biomass_seca_g_estimada~ altura_cm+ I(DAP_cm^2)
                                                , data =
                                                  Biomass_palms_archontophoenix)



par (mfrow = c(2,2))
plot (lm_bio_full)
plot (lm_bio_simple)
str (lm_bio_simple)


summary(lm_bio_full)
summary(lm_bio_simple)
summary (lm_bio_simple_Without_Altsq_Interaction)
summary (lm_bio_simple_Without_Altsq)

anova(lm_bio_full,lm_bio_simple)

AIC (lm_bio_simple,
     lm_bio_simple_Without_Altsq_Interaction,
     lm_bio_simple_Without_Altsq,
     lm_bio_simple_Without_dapsqint,
     lm_bio_simple_Without_dapsqintDap,
     lm_bio_simple_Without_dapsqintDap_and_altinterca,
     lm_bio_simple_Without_dapsqint_and_alrsq,
     lm_bio_hiper_simple)


#####teste#########
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
plot (biomass_seca_g_estimada~altura_cm + DAP_cm, data =
        Biomass_palms_archontophoenix)
abline (alt_dap)

plot (Biomass_palms_archontophoenix [, -c(1:3,6:12)])
names (Biomass_palms_archontophoenix)


boxplot(biomass_seca_g_estimada~Transecto,
        data=Biomass_palms_archontophoenix, log ="y")
barplot(biomassa_fresca_g~Parcela,
        data=Biomass_palms_archontophoenix, log ="y")

#######

lm1 <- lm(Fertility ~ . , data = swiss)
summary(lm1)
AIC (lm1)
