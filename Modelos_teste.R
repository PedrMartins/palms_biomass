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

