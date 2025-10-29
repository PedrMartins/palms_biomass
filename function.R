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





stats_DBH_Alt <- function (x,class = 5, dbh_alt="alt"){

  site <-  x
  diametre_altura <-  c("alt", "dbh")
  diametre_altura <- match(dbh_alt, diametre_altura)

    if (length(class)==1) {
      if (diametre_altura==2){
        site<-  site [site$DAP_cm<class,]
        site <- na.omit(site)
        parcel <- unique(site$Parcela)
        data_biomass <- data.frame()
        browser()

        for (i in parcel){

          site_class <- site[site$Parcela == i,]

          data_parcel <- site_class %>%
          summarise(parcel= as.character(i),
                    dbh_class_cm =  as.character(class),
                    mean=mean(site_class$biomass_seca_g_estimada,
                                          na.rm = TRUE ),
                    SD=sd(site_class$biomass_seca_g_estimada,
                          na.rm = T),
                    basal_area = sum (pi * (site_class$DAP_cm^2/4)), #teste
                    N=length(
                      na.omit(site_class$biomass_seca_g_estimada)
                      )
                    )
        data_biomass <-  rbind(data_parcel, data_biomass)
        }

      }else {
        site<-  site [site$altura_cm<class,]
        site <- na.omit(site)
        parcel <- unique(site$Parcela)
        data_biomass <- data.frame()

         for (i in parcel){

          site_class <- site[site$Parcela == i,]

          data_parcel <- site_class %>%
            summarise(parcel = as.character(i),
                      alt_class_cm =  as.character(class),
                      mean=mean(site_class$biomass_seca_g_estimada,
                                            na.rm = TRUE ),
                      SD=sd(site_class$biomass_seca_g_estimada,
                            na.rm = T),
                      N=length(
                        na.omit(site_class$biomass_seca_g_estimada)
                      )
                      )
          data_biomass <-  rbind(data_parcel, data_biomass)
        }
      }
    } else {
      if(diametre_altura==2){

        data_biomass <- data.frame()

        for (i in seq_along(class)){
          if (i==1){
            site_class<-  site [site$DAP_cm<class [1],]
            parcel <-  unique(site$Parcela)
            for (j in parcel){
              site_class_by_parcel <- site_class[site_class$Parcela == j,]
              dap <- site_class_by_parcel$DAP_cm

              subset_data_bio <- site_class_by_parcel %>%
                summarise(parcel= as.character(j),
                          DAP_cm =  as.character(class[1]),
                          biomass_total= sum (site_class_by_parcel$biomass_seca_g_estimada,
                                              na.rm = TRUE),
                          basal_area = sum (pi * (dap^2/4), na.rm = TRUE),
                          basal_area_msqr = sum (0.00007854 * (dap^2),
                                                 na.rm = TRUE),
                          mean=mean(site_class_by_parcel$biomass_seca_g_estimada,
                                    na.rm = TRUE ),
                          SD=sd(site_class_by_parcel$biomass_seca_g_estimada,
                                na.rm = T),
                          N=length(
                            na.omit(site_class_by_parcel$biomass_seca_g_estimada)
                          ))


              data_biomass <- rbind(subset_data_bio,data_biomass)
            }
          }
          lower_bound <- class[i]
          upper_bound <- class[i + 1]
          parcel = unique(site$Parcela)
          if (is.na(upper_bound)==TRUE) {
            site_class<-  site [site$DAP_cm >= lower_bound,]
            parcel <-  unique(site$Parcela)
            for (j in parcel){
              site_class_by_parcel <- site_class[site_class$Parcela == j,]
              dap <- site_class_by_parcel$DAP_cm

              subset_data_bio <- site_class_by_parcel %>%
                summarise(parcel= as.character(j),
                          DAP_cm = as.character(lower_bound),
                          biomass_total= sum (site_class_by_parcel$biomass_seca_g_estimada,
                                              na.rm = TRUE),
                          basal_area = sum (pi * (dap^2/4), na.rm = TRUE),
                          basal_area_msqr = sum (0.00007854 * (dap^2),
                                                 na.rm = TRUE),
                          mean=mean(site_class_by_parcel$biomass_seca_g_estimada,
                                    na.rm = TRUE ),
                          SD=sd(site_class_by_parcel$biomass_seca_g_estimada,
                                na.rm = T),
                          N=length(
                            na.omit(site_class_by_parcel$biomass_seca_g_estimada)
                          ))

              data_biomass <- rbind(subset_data_bio,data_biomass)
            }

          }else {

            site_class<-   site[site$DAP_cm >= lower_bound &
                                      site$DAP_cm < upper_bound, ]


            for (j in parcel) {

              site_class_by_parcel <- site_class[site_class$Parcela == j,]
              dap <- site_class_by_parcel$DAP_cm

              subset_data_bio <- site_class_by_parcel %>%
                summarise(parcel= as.character(j),
                          DAP_cm = as.character(paste (lower_bound,upper_bound,
                                                             sep ="_")),
                          biomass_total= sum (site_class_by_parcel$biomass_seca_g_estimada,
                                              na.rm = TRUE),
                          basal_area = sum (pi * (dap^2/4), na.rm = TRUE),
                          basal_area_msqr = sum (0.00007854 * (dap^2),
                                                 na.rm = TRUE),
                          mean=mean(site_class_by_parcel$biomass_seca_g_estimada,
                                    na.rm = TRUE ),
                          SD=sd(site_class_by_parcel$biomass_seca_g_estimada,
                                na.rm = T),
                          N=length(
                            na.omit(site_class_by_parcel$biomass_seca_g_estimada)
                          ))

              data_biomass <- rbind(subset_data_bio,data_biomass)
              }
          }

        }


      }else{

        data_biomass <- data.frame()
        for (i in seq_along(class)){
          if (i==1){
            site_class<-  site [site$altura_cm<class [1],]
            parcel <-  unique(site$Parcela)
            for (j in parcel){
              site_class_by_parcel <- site_class[site_class$Parcela == j,]
              dap <- site_class_by_parcel$DAP_cm

              subset_data_bio <- site_class_by_parcel %>%
                summarise(parcel= as.character(j),
                          alt_class_cm =  as.character(class[1]),
                          biomass_total= sum (site_class_by_parcel$biomass_seca_g_estimada,
                                              na.rm = TRUE),
                          basal_area = sum (pi * (dap^2/4), na.rm = TRUE),
                          basal_area_msqr = sum (0.00007854 * (dap^2),
                                                 na.rm = TRUE),
                          mean=mean(site_class_by_parcel$biomass_seca_g_estimada,
                                    na.rm = TRUE ),
                          SD=sd(site_class_by_parcel$biomass_seca_g_estimada,
                                na.rm = T),
                          N=length(
                            na.omit(site_class_by_parcel$biomass_seca_g_estimada)
                          ))


              data_biomass <- rbind(subset_data_bio,data_biomass)
            }


          }
          lower_bound <- class[i]
          upper_bound <- class[i + 1]
          parcel = unique(site$Parcela)
          if (is.na(upper_bound)==TRUE) {
            site_class<-  site [site$altura_cm >= lower_bound,]
            for (j in parcel){
              site_class_by_parcel <- site_class[site_class$Parcela == j,]
              dap <- site_class_by_parcel$DAP_cm

              subset_data_bio <- site_class_by_parcel %>%
                summarise(parcel= as.character(j),
                          alt_class_cm = as.character(lower_bound),
                          biomass_total= sum (site_class_by_parcel$biomass_seca_g_estimada,
                                              na.rm = TRUE),
                          basal_area = sum (pi * (dap^2/4), na.rm = TRUE),
                          basal_area_msqr = sum (0.00007854 * (dap^2),
                                                 na.rm = TRUE),
                          mean=mean(site_class_by_parcel$biomass_seca_g_estimada,
                                    na.rm = TRUE ),
                          SD=sd(site_class_by_parcel$biomass_seca_g_estimada,
                                na.rm = T),
                          N=length(
                            na.omit(site_class_by_parcel$biomass_seca_g_estimada)
                          ))

              data_biomass <- rbind(subset_data_bio,data_biomass)
            }


          }else {
            site_class<-   site[site$altura_cm >= lower_bound &
                                  site$altura_cm < upper_bound, ]
            dap <- site_class_by_parcel$DAP_cm

            for (j in parcel) {

              site_class_by_parcel <- site_class[site_class$Parcela == j,]
              dap <- site_class_by_parcel$DAP_cm

              subset_data_bio <- site_class_by_parcel %>%
                summarise(parcel= as.character(j),
                          alt_class_cm = as.character(paste (lower_bound,upper_bound,
                                                             sep ="_")),
                          biomass_total= sum (site_class_by_parcel$biomass_seca_g_estimada,
                                              na.rm = TRUE),
                          basal_area = sum (pi * (dap^2/4), na.rm = TRUE),
                          basal_area_msqr = sum (0.00007854 * (dap^2),
                                                 na.rm = TRUE),
                          mean=mean(site_class_by_parcel$biomass_seca_g_estimada,
                                    na.rm = TRUE ),
                          SD=sd(site_class_by_parcel$biomass_seca_g_estimada,
                                na.rm = T),
                          N=length(
                            na.omit(site_class_by_parcel$biomass_seca_g_estimada)
                          ))

              data_biomass <- rbind(subset_data_bio,data_biomass)
            }


          }

        }

      }
    }

    result <- data_biomass


  return (result)

}


