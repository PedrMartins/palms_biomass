class_DBH_alt <- function (x, choice = "ind",
                               class = 5, dbh_alt="alt", distribution = FALSE){

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
      data_dap<- data.frame("Class_Alt"=class,
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
      colnames(data_dap) <- c("Class_DAP","Ind_number",
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
          colnames(data_dap) <- c("Class_Alt","Ind_number",
                                  "Ind_percentage",
                                  "Total_ind")


    }

    }


    result <- data_dap
  }

  if (choice==2){
    if (length(class)==1) {
      if (distribution==TRUE){
        distri <- c("Temp", "Trop")
        data_sep_dist <- data.frame()
        for (i in distri){
          tag <- i
          site_sep_distri <- site[site$Distri==i,]
          site_class<-  site_sep_distri [site_sep_distri$DAP>class,]
          site_class_biomass=sum(site_class$biom)
          site_all_biomass= sum(site$biom)
          site_biomass_percentage = (site_class_biomass/
                                       site_all_biomass) *100
          data_biomass<- data.frame("Class_DAP"=class,
                                    "Distri"= tag,
                                    "Biomass_ab"=site_class_biomass,
                                    "Biomass_percentage"=site_biomass_percentage,
                                    "Total_ind"=site_all_biomass)

          data_sep_dist <- rbind(data_biomass,data_sep_dist)
        }
        data_biomass <- data_sep_dist
      }else {
        clado <- c("Gim", "Ang")
        site$Filo[site$Filo %in% c("Eud", "Mag", "Palm")] <- "Ang"
        data_sep_filo <- data.frame()

        for (i in clado){
          tag <- i
          site_sep_filo <- site[site$Filo==i,]
          site_class<-  site_sep_filo [site_sep_filo$DAP>class,]
          site_class_biomass=sum(site_class$biom)
          site_all_biomass= sum(site$biom)
          site_biomass_percentage = (site_class_biomass/
                                       site_all_biomass) *100
          data_biomass<- data.frame("Class_DAP"=class,
                                    "Filo"= tag,
                                    "Biomass_ab"=site_class_biomass,
                                    "Biomass_percentage"=site_biomass_percentage,
                                    "Total_ind"=site_all_biomass)

          data_sep_filo <- rbind(data_biomass,data_sep_filo)
        }
        data_biomass <- data_sep_filo
      }
    } else {
      if(distribution==TRUE){
        distri <- c("Temp", "Trop")
        data_sep_dist <- data.frame()
        for (i in distri){
          tag <- i
          site_sep_distri <- site[site$Distri==i,]
          data_biomass <- data.frame()
          for (j in seq_along(class)){
            if (j==1){
              site_class<-  site_sep_distri [site_sep_distri$DAP<class [1],]
              site_class_biomass=sum(site_class$biom)
              site_all_biomass= sum(site$biom)
              site_biomass_percentage = (site_class_biomass/
                                           site_all_biomass) *100


              subset_data_bio <- c(class[1], tag,
                                   site_class_biomass,
                                   site_biomass_percentage,
                                   site_all_biomass)
              data_biomass <- rbind(subset_data_bio,data_biomass)
            }
            lower_bound <- class[j]
            upper_bound <- class[j + 1]
            if (is.na(upper_bound)==TRUE) {
              subset_data <- site_sep_distri[site_sep_distri$DAP >= lower_bound,]
              site_class_biomass=sum(subset_data$biom)
              site_all_biomass= sum(site$biom)
              site_biomass_percentage = (site_class_biomass/
                                           site_all_biomass) *100


              subset_data_bio <- c(class[j], tag,
                                   site_class_biomass,
                                   site_biomass_percentage,
                                   site_all_biomass)

            }else {
              subset_data <- site_sep_distri[site_sep_distri$DAP >= lower_bound &
                                               site_sep_distri$DAP < upper_bound, ]
              site_class_biomass=sum(subset_data$biom)
              site_all_biomass= sum(site$biom)
              site_biomass_percentage = (site_class_biomass/
                                           site_all_biomass) *100


              subset_data_bio <- c(paste (class[j], class[j+1], sep= "_"),
                                   tag,
                                   site_class_biomass,
                                   site_biomass_percentage,
                                   site_all_biomass)

            }
            data_biomass <- rbind(subset_data_bio,data_biomass)
          }
          colnames(data_biomass) <- c("Class_DAP",
                                      "Distri",
                                      "Biomass_ab",
                                      "Biomass_percentage",
                                      "Total_ind")
          data_sep_dist <- rbind(data_biomass,data_sep_dist)
        }
        data_biomass <- data_sep_dist[order(data_sep_dist$Class_DAP),]
      }else{

        clado <- c("Gim", "Ang")
        site$Filo[site$Filo %in% c("Eud", "Mag", "Palm")] <- "Ang"
        data_sep_filo <- data.frame()

        for (i in clado){
          tag <- i
          site_sep_filo <- site[site$Filo==i,]
          data_biomass <- data.frame()
          for (j in seq_along(class)){
            if (j==1){
              site_class<-  site_sep_filo [site_sep_filo$DAP<class [1],]
              site_class_biomass=sum(site_class$biom)
              site_all_biomass= sum(site$biom)
              site_biomass_percentage = (site_class_biomass/
                                           site_all_biomass) *100


              subset_data_bio <- c(class[1], tag,
                                   site_class_biomass,
                                   site_biomass_percentage,
                                   site_all_biomass)
              data_biomass <- rbind(subset_data_bio,data_biomass)
            }
            lower_bound <- class[j]
            upper_bound <- class[j + 1]
            if (is.na(upper_bound)==TRUE) {
              subset_data <- site_sep_filo[site_sep_filo$DAP >= lower_bound,]
              site_class_biomass=sum(subset_data$biom)
              site_all_biomass= sum(site$biom)
              site_biomass_percentage = (site_class_biomass/
                                           site_all_biomass) *100


              subset_data_bio <- c(class[j], tag,
                                   site_class_biomass,
                                   site_biomass_percentage,
                                   site_all_biomass)

            }else {
              subset_data <- site_sep_filo[site_sep_filo$DAP >= lower_bound &
                                             site_sep_filo$DAP < upper_bound, ]
              site_class_biomass=sum(subset_data$biom)
              site_all_biomass= sum(site$biom)
              site_biomass_percentage = (site_class_biomass/
                                           site_all_biomass) *100


              subset_data_bio <- c(paste (class[j], class[j+1], sep= "_"),
                                   tag,
                                   site_class_biomass,
                                   site_biomass_percentage,
                                   site_all_biomass)

            }
            data_biomass <- rbind(subset_data_bio,data_biomass)
          }
          colnames(data_biomass) <- c("Class_DAP",
                                      "Filo",
                                      "Biomass_ab",
                                      "Biomass_percentage",
                                      "Total_ind")
          data_sep_filo <- rbind(data_biomass,data_sep_filo)
        }
        data_biomass <- data_sep_filo[order(data_sep_filo$Class_DAP),]
      }
    }

    result <- data_biomass
  }


  if (diametre_altura==2){result <- result %>%
    mutate(across(-c(Class_DAP), as.numeric))}else{result <- result %>%
      mutate(across(-c(Class_Alt), as.numeric))}
  return (result)

}
