library(readxl)
Biomass_palms_archontophoenix<- read_excel("biomass_data_matinha_USP.xlsx")
names (Biomass_palms_archontophoenix)[3:7] <-  c("DAP_mm", "altura_cm",
                                                 "biomassa_fresca_g",
                                                 "biomass_seca_g",
                                                 "peso_seco_%")
Biomass_palms_archontophoenix$DAP_cm <- c(Biomass_palms_archontophoenix$DAP_mm/10)
#View (Biomass_palms_archontophoenix)
#Biomass_palms_archontophoenix$DAP_cm < 7

bio_dap <- lm (biomassa_fresca_g~DAP_cm, data =
                 Biomass_palms_archontophoenix)
bio_dap_2 <- lm (biomassa_fresca_g~I (DAP_cm*DAP_cm^2), data =
                 Biomass_palms_archontophoenix)

summary (anova (bio_dap,bio_dap_2))

summary(bio_dap)
plot (biomassa_fresca_g~DAP_cm, data =
        Biomass_palms_archontophoenix)
abline (bio_dap_2)

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

