library(readxl)
library(dplyr)
Biomass_palms_archontophoenix<- read_excel("biomass_data_matinha_USP.xlsx")
names (Biomass_palms_archontophoenix)[3:7] <-  c("DAP_mm", "altura_cm",
                                                 "biomassa_fresca_g",
                                                 "biomass_seca_g",
                                                 "percentage_dry_biomass")
Biomass_palms_archontophoenix$DAP_cm <- c(Biomass_palms_archontophoenix$DAP_mm/10)
str (Biomass_palms_archontophoenix)
#Biomass_palms_archontophoenix [Biomass_palms_archontophoenix$percentage_dry_biomass == 0,7] <- "NA"


class_DBH_alt (Biomass_palms_archontophoenix, class = c(5,6,16,31,51,151))




names (Biomass_palms_archontophoenix)
#View (Biomass_palms_archontophoenix)
#Biomass_palms_archontophoenix$DAP_cm < 7

bio_dap <- lm (biomassa_fresca_g~DAP_cm, data =
                 Biomass_palms_archontophoenix)
bio_dap_2 <- lm (biomassa_fresca_g~ DAP_cm * I(DAP_cm^2), data =
                 Biomass_palms_archontophoenix)
coefficients <- coef(bio_dap_2)
summary(bio_dap_2)
anova (bio_dap,bio_dap_2)



plot (biomassa_fresca_g~DAP_cm, data =
        Biomass_palms_archontophoenix)
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

