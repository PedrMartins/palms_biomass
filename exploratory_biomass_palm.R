library(readxl)
Biomass_palms_archontophoenix<- read_excel("biomass_data_matinha_USP.xlsx")
names (Biomass_palms_archontophoenix)[3:7] <-  c("DAP_mm", "altura_cm",
                                                 "biomassa_fresca_g",
                                                 "biomass_seca_g",
                                                 "peso_seco_%")
Biomass_palms_archontophoenix$DAP_cm <- c(Biomass_palms_archontophoenix$DAP_mm/10)
#View (Biomass_palms_archontophoenix)
#Biomass_palms_archontophoenix$DAP_cm < 7

plot (biomassa_fresca_g~DAP_cm, data = Biomass_palms_archontophoenix, log = "xy")
plot (biomassa_fresca_g~altura_cm, data = Biomass_palms_archontophoenix, log = "xy")
plot (DAP_cm~altura_cm, data = Biomass_palms_archontophoenix, log = "xy")
