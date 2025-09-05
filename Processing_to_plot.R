ind_par_transce <-  Biomass_palms_archontophoenix %>%
  count(Transecto)

ind_par_parcela <- Biomass_palms_archontophoenix %>%
  count(Parcela)

biomass_seca_count <- Biomass_palms_archontophoenix %>%
  group_by(biomass_seca_g_estimada)%>%
  count (biomass_seca_g_estimada)

biomass_seca <- Biomass_palms_archontophoenix %>%
  group_by(Parcela) %>%
  summarise(total_biomass_seca_g = sum(biomass_seca_g_estimada,
                                       na.rm = TRUE))

