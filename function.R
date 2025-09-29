class_DBH_alt <- function (x, choice = "ind",
                               class = 5, dbh_alt="alt"){

  site <-  x
  choices <- c ("ind","bio")
  choice <- match(choice, choices)
  diametre_altura <-  c("alt", "dbh")
  diametre_altura <- match(dbh_alt, diametre_altura)


  if (choice==1) {
    if (length(class)==1) {
      if (diametre_altura==2){
        site_class<-  site [site$DAP_cm<class,]
        site_class_number=length(site_class$DAP_cm)
        site_all_number= length(site$DAP_cm)
        site_class_percentage = (site_class_number/
                                   site_all_number) *100
        data_dap<- data.frame("Class_DAP"=class,
                              "Ind_number"=site_class_number,
                              "Ind_percentage"=site_class_percentage,
                              "Total_ind"=site_all_number)
      }else{
      site_class<-  site [site$altura_cm<class,]
      site_class_number=length(site_class$altura_cm)
      site_all_number= length(site$altura_cm)
      site_class_percentage = (site_class_number/
                                 site_all_number) *100
      data_dap<- data.frame("Class_Alt_cm"=class,
                            "Ind_number"=site_class_number,
                            "Ind_percentage"=site_class_percentage,
                            "Total_ind"=site_all_number)
      }
    } else {
      if (diametre_altura==2){
        data_dap <- data.frame()

      for (i in seq_along(class)) {
        if (i==1){
          site_class<-  site [site$DAP_cm<class[1],]
          site_class_number=length(site_class$DAP_cm)
          site_all_number= length(site$DAP_cm)
          site_class_percentage = (site_class_number/
                                     site_all_number) *100
          subset_data <- c(class[1],
                           site_class_number,
                           site_class_percentage,
                           site_all_number)
          data_dap <- rbind(subset_data,data_dap)
        }
        lower_bound <- class[i]
        upper_bound <- class[i + 1]
        if (is.na(upper_bound)==TRUE) {
          subset_data <- site[site$DAP_cm >= lower_bound,]
          site_class_number=length(subset_data$DAP_cm)
          site_all_number= length(site$DAP_cm)
          site_class_percentage = (site_class_number/
                                     site_all_number) *100
          subset_data <- c(class[i], site_class_number,
                           site_class_percentage,
                           site_all_number)

        }else {
          subset_data <- site[site$DAP_cm >= lower_bound &
                                site$DAP_cm < upper_bound, ]
          site_class_number=length(subset_data$DAP_cm)
          site_all_number= length(site$DAP_cm)
          site_class_percentage = (site_class_number/
                                     site_all_number) *100
          subset_data <- c(paste (class[i], class[i+1], sep= "_"),
                           site_class_number,
                           site_class_percentage,
                           site_all_number)
        }

        data_dap <- rbind(subset_data,data_dap)

      }
      colnames(data_dap) <- c("Class_DAP_cm","Ind_number",
                              "Ind_percentage",
                              "Total_ind")
      }else{
       data_dap <- data.frame()

        for (i in seq_along(class)) {
          if (i==1){
            site_class<-  site [site$altura_cm<class[1],]
            site_class_number=length(site_class$altura_cm)
            site_all_number= length(site$altura_cm)
            site_class_percentage = (site_class_number/
                                       site_all_number) *100
            subset_data <- c(class[1],
                             site_class_number,
                             site_class_percentage,
                             site_all_number)
            data_dap <- rbind(subset_data,data_dap)
          }
          lower_bound <- class[i]
          upper_bound <- class[i + 1]
          if (is.na(upper_bound)==TRUE) {
            subset_data <- site[site$altura_cm >= lower_bound,]
            site_class_number=length(subset_data$altura_cm)
            site_all_number= length(site$altura_cm)
            site_class_percentage = (site_class_number/
                                       site_all_number) *100
            subset_data <- c(class[i], site_class_number,
                             site_class_percentage,
                             site_all_number)

          }else {
            subset_data <- site[site$altura_cm >= lower_bound &
                                  site$altura_cm < upper_bound, ]
            site_class_number=length(subset_data$altura_cm)
            site_all_number= length(site$altura_cm)
            site_class_percentage = (site_class_number/
                                       site_all_number) *100
            subset_data <- c(paste (class[i], class[i+1], sep= "_"),
                             site_class_number,
                             site_class_percentage,
                             site_all_number)
          }
          data_dap <- rbind(subset_data,data_dap)

        }

       colnames(data_dap) <- c("Class_Alt_cm","Ind_number",
                                  "Ind_percentage",
                                  "Total_ind")


    }

    }


    result <- data_dap
  }

  if (choice==2){
    if (length(class)==1) {
      if (diametre_altura==2){
        site_class<-  site [site$DAP_cm<class,]
        site_class_number=sum (site_class$biomass_seca_g_estimada,
                               na.rm = TRUE)
        site_class_number=sum (site$biomass_seca_g_estimada,
                               na.rm = TRUE)
        site_class_percentage = (site_class_number/
                                   site_all_number) *100
        data_dap<- data.frame("Class_DAP_cm"=class,
                              "Biomass"=site_class_number,
                              "Biomass_percentage"=site_class_percentage,
                              "Total_biomass"=site_all_number)

        data_biomass <- data_dap
      }else {

        site_class<-  site [site$altura_cm<class,]
        site_class_number=sum (site_class$biomass_seca_g_estimada,
                               na.rm = TRUE) #quando tiver o fator de correção
        site_class_number=sum (site$biomass_seca_g_estimada,
                               na.rm = TRUE) #quando tiver o fator de correção
        site_class_percentage = (site_class_number/
                                   site_all_number) *100
        data_dap<- data.frame("Class_Alt_cm"=class,
                              "Biomass"=site_class_number,
                              "Biomass_percentage"=site_class_percentage,
                              "Total_biomass"=site_all_number)

        data_biomass <- data_dap
      }
    } else {
      if(diametre_altura==2){
        data_biomass <- data.frame()
          for (i in seq_along(class)){
            if (i==1){
              site_class<-  site [site$DAP_cm<class [1],]
              # site_class_biomass=sum(site_class$biomassa_fresca_g, na.rm = TRUE)
              site_class_biomass=sum(site_class$biomass_seca_g_estimada, na.rm = TRUE)
              # site_all_biomass= sum(site$biomassa_fresca_g, na.rm = TRUE)
              site_all_biomass= sum(site$biomass_seca_g_estimada, na.rm = TRUE)
              site_biomass_percentage = (site_class_biomass/
                                           site_all_biomass) *100


              subset_data_bio <- c(class[1],
                                   site_class_biomass,
                                   site_biomass_percentage,
                                   site_all_biomass)
              data_biomass <- rbind(subset_data_bio,data_biomass)
            }
            lower_bound <- class[i]
            upper_bound <- class[i + 1]
            if (is.na(upper_bound)==TRUE) {
              subset_data <- site[site$DAP_cm >= lower_bound,]
              # site_class_biomass=sum(subset_data$biomassa_fresca_g, na.rm = TRUE)
              # site_all_biomass= sum(site$biomassa_fresca_g, na.rm = TRUE)
              site_class_biomass=sum(subset_data$biomass_seca_g_estimada, na.rm = TRUE)
              site_all_biomass= sum(site$biomass_seca_g_estimada, na.rm = TRUE)
              site_biomass_percentage = (site_class_biomass/
                                           site_all_biomass) *100


              subset_data_bio <- c(class[i],
                                   site_class_biomass,
                                   site_biomass_percentage,
                                   site_all_biomass)

            }else {
              subset_data <- site[site$DAP_cm >= lower_bound &
                                               site$DAP_cm < upper_bound, ]
              # site_class_biomass=sum(subset_data$biomassa_fresca_g, na.rm = TRUE)
              # site_all_biomass= sum(site$biomassa_fresca_g, na.rm = TRUE)
             site_class_biomass=sum(subset_data$biomass_seca_g_estimada, na.rm = TRUE)
             site_all_biomass= sum(site$biomass_seca_g_estimada, na.rm = TRUE)
              site_biomass_percentage = (site_class_biomass/
                                           site_all_biomass) *100


              subset_data_bio <- c(paste (class[i], class[i+1], sep= "_"),
                                   site_class_biomass,
                                   site_biomass_percentage,
                                   site_all_biomass)

            }
            data_biomass <- rbind(subset_data_bio,data_biomass)
          }
          colnames(data_biomass) <- c("Class_DAP_cm",

                                      "Biomass_ab",
                                      "Biomass_percentage",
                                      "Total_ind")

      }else{
        data_biomass <- data.frame()
        for (i in seq_along(class)){
          if (i==1){
            site_class<-  site [site$altura_cm<class [1],]
            # site_class_biomass=sum(site_class$biomassa_fresca_g, na.rm = TRUE)
            site_class_biomass=sum(site_class$biomass_seca_g_estimada, na.rm = TRUE)
            # site_all_biomass= sum(site$biomassa_fresca_g, na.rm = TRUE)
            site_all_biomass= sum(site$biomass_seca_g_estimada, na.rm = TRUE)
            site_biomass_percentage = (site_class_biomass/
                                         site_all_biomass) *100


            subset_data_bio <- c(class[1],
                                 site_class_biomass,
                                 site_biomass_percentage,
                                 site_all_biomass)
            data_biomass <- rbind(subset_data_bio,data_biomass)
          }
          lower_bound <- class[i]
          upper_bound <- class[i + 1]
          if (is.na(upper_bound)==TRUE) {
            subset_data <- site[site$altura_cm >= lower_bound,]
            # site_class_biomass=sum(subset_data$biomassa_fresca_g, na.rm = TRUE)
            # site_all_biomass= sum(site$biomassa_fresca_g, na.rm = TRUE)
            site_class_biomass=sum(subset_data$biomass_seca_g_estimada, na.rm = TRUE)
            site_all_biomass= sum(site$biomass_seca_g_estimada, na.rm = TRUE)
            site_biomass_percentage = (site_class_biomass/
                                         site_all_biomass) *100


            subset_data_bio <- c(class[i],
                                 site_class_biomass,
                                 site_biomass_percentage,
                                 site_all_biomass)

          }else {
            subset_data <- site[site$altura_cm >= lower_bound &
                                  site$altura_cm < upper_bound, ]
            # site_class_biomass=sum(subset_data$biomassa_fresca_g, na.rm = TRUE)
            # site_all_biomass= sum(site$biomassa_fresca_g, na.rm = TRUE)
            site_class_biomass=sum(subset_data$biomass_seca_g_estimada, na.rm = TRUE)
            site_all_biomass= sum(site$biomass_seca_g_estimada, na.rm = TRUE)
            site_biomass_percentage = (site_class_biomass/
                                         site_all_biomass) *100


            subset_data_bio <- c(paste (class[i], class[i+1], sep= "_"),
                                 site_class_biomass,
                                 site_biomass_percentage,
                                 site_all_biomass)

          }
          data_biomass <- rbind(subset_data_bio,data_biomass)
        }
        colnames(data_biomass) <- c("Class_Alt_cm",

                                    "Biomass_ab",
                                    "Biomass_percentage",
                                    "Total_ind")


      }
    }

    result <- data_biomass
  }


  if (diametre_altura==2){  result <- result %>%
    mutate(across(-c(Class_DAP_cm), as.numeric))}else{result <- result %>%
      mutate(across(-c(Class_Alt_cm), as.numeric))}
  return (result)

}


stats_DBH_Alt <- function (x,class = 5, dbh_alt="alt"){

  site <-  x
  diametre_altura <-  c("alt", "dbh")
  diametre_altura <- match(dbh_alt, diametre_altura)

    if (length(class)==1) {
      if (diametre_altura==2){
        site_class<-  site [site$DAP_cm<class,]
        data_biomass <- site_class %>%
          summarise(dbh_class_cm =  as.character(class),
                    mean=mean(site_class$biomass_seca_g_estimada,
                                          na.rm = TRUE ),
                    SD=sd(site_class$biomass_seca_g_estimada,
                          na.rm = T),
                    N=length(
                      na.omit(site_class$biomass_seca_g_estimada)
                      )
                    )

      }else {

        site_class<-  site [site$altura_cm<class,]
        data_biomass <- site_class %>%
          summarise(alt_class_cm =  as.character(class),
                    mean=mean(site_class$biomass_seca_g_estimada,
                                          na.rm = TRUE ),
                    SD=sd(site_class$biomass_seca_g_estimada,
                          na.rm = T),
                    N=length(
                      na.omit(site_class$biomass_seca_g_estimada)
                    )
                    )
      }
    } else {
      if(diametre_altura==2){
        data_biomass <- data.frame()
        for (i in seq_along(class)){
          if (i==1){
            site_class<-  site [site$DAP_cm<class [1],]
            subset_data_bio <- site_class %>%
              summarise(dbh_class_cm =  as.character(class[1]),
                        mean=mean(site_class$biomass_seca_g_estimada,
                                              na.rm = TRUE ),
                        SD=sd(site_class$biomass_seca_g_estimada,
                              na.rm = T),
                        N=length(
                          na.omit(site_class$biomass_seca_g_estimada)
                        ))

            data_biomass <- rbind(subset_data_bio,data_biomass)
          }
          lower_bound <- class[i]
          upper_bound <- class[i + 1]
          if (is.na(upper_bound)==TRUE) {
            site_class<-  site [site$DAP_cm >= lower_bound,]
            subset_data_bio <- site_class %>%
              summarise(dbh_class_cm =  as.character(class[i]),
                        mean=mean(site_class$biomass_seca_g_estimada,
                                              na.rm = TRUE ),
                        SD=sd(site_class$biomass_seca_g_estimada,
                              na.rm = T),
                        N=length(
                          na.omit(site_class$biomass_seca_g_estimada)
                        )
                        )

          }else {

            site_class<-   site[site$DAP_cm >= lower_bound &
                                      site$DAP_cm < upper_bound, ]
            subset_data_bio <- site_class %>%
              summarise(dbh_class_cm =  as.character(paste (lower_bound,upper_bound,
                                                            sep ="_")),
                        mean=mean(site_class$biomass_seca_g_estimada,
                                              na.rm = TRUE ),
                        SD=sd(site_class$biomass_seca_g_estimada,
                              na.rm = T),
                        N=length(
                          na.omit(site_class$biomass_seca_g_estimada)
                        )
                        )
          }
          data_biomass <- rbind(subset_data_bio,data_biomass)
        }


      }else{
        data_biomass <- data.frame()
        for (i in seq_along(class)){
          if (i==1){
            site_class<-  site [site$altura_cm<class [1],]
            subset_data_bio <- site_class %>%
              summarise(alt_class_cm =  as.character(class[1]),
                        mean=mean(site_class$biomass_seca_g_estimada,
                                              na.rm = TRUE ),
                        SD=sd(site_class$biomass_seca_g_estimada,
                              na.rm = T),
                        N=length(
                          na.omit(site_class$biomass_seca_g_estimada)
                        ))

            data_biomass <- rbind(subset_data_bio,data_biomass)

          }
          lower_bound <- class[i]
          upper_bound <- class[i + 1]
          if (is.na(upper_bound)==TRUE) {
            site_class<-  site [site$altura_cm >= lower_bound,]
            subset_data_bio <- site_class %>%
              summarise(alt_class_cm =  as.character(class[i]),
                        mean=mean(site_class$biomass_seca_g_estimada,
                                              na.rm = TRUE ),
                        SD=sd(site_class$biomass_seca_g_estimada,
                              na.rm = T),
                        N=length(
                          na.omit(site_class$biomass_seca_g_estimada)
                        ))

          }else {
            site_class<-   site[site$altura_cm >= lower_bound &
                                  site$altura_cm < upper_bound, ]
            subset_data_bio <- site_class %>%
              summarise(alt_class_cm =  as.character(paste (lower_bound,upper_bound,
                                                            sep ="_")),
                        mean=mean(site_class$biomass_seca_g_estimada,
                                              na.rm = TRUE ),
                        SD=sd(site_class$biomass_seca_g_estimada,
                              na.rm = T),
                        N=length(
                          na.omit(site_class$biomass_seca_g_estimada)
                        ))

          }
          data_biomass <- rbind(subset_data_bio,data_biomass)
        }

      }
    }

    result <- data_biomass


  return (result)

}

