source("function.R")

# function class_DBH_alt (x, choice = "ind",
# class = 5, dbh_alt="alt", distribution = FALSE) default

############Processing data#####

# View (Biomass_palms_archontophoenix [c(294,
#                                        444,
#                                        445),]) #dados influentes no modelo



#######exploratoty analyses######

anova (aov (biomass_seca_g_estimada ~ Transecto + Parcela,
     data = Biomass_palms_archontophoenix))

anova (aov (biomass_seca_g_estimada ~ Parcela,
            data = Biomass_palms_archontophoenix))
anova (aov (biomass_seca_g_estimada ~ Transecto,
            data = Biomass_palms_archontophoenix))

# par (las = 1, bty = "n")

down_to_top <- range(
  log (Biomass_palms_archontophoenix$biomass_seca_g_estimada),
      na.rm = TRUE)

boxplot (log (biomass_seca_g_estimada+1)~ Transecto,
         data= Biomass_palms_archontophoenix,
         ylim = c(down_to_top[1] -1.5,
                  down_to_top [2] + 1.5),
         col= c("lightgreen","green","darkgreen"),
         pch = "*", xlab = "Parcela",
         ylab = "log Dry Biomass (g)")

legend("topleft" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Transecto 2","Transecto 3") #texto a ser escrito
       ,col=c ("lightgreen", "darkgreen")
       ,cex=.5		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte

######### barplot  ########
source ("Processing_to_plot.R")

Stats_alt <-stats_DBH_Alt (Biomass_palms_archontophoenix,
                           class= c(5,15,30,50,100,150,200),
                           dbh_alt = "alt")

colnames(Stats_alt) <- c("Parcel", "Class by Altitude",
                         "Total Biomass g", "Basal Area²",
                         "Basal Area by m²" ,"Mean Biomass",
                         "Standard deviation","N Sample")

range (ind_par_transce$n)
barplot(ind_par_transce$n~ind_par_transce$Transecto,
        ylim = c(0,800), width=1)

jpeg("parcela_ind.jpg", width = 1800,height = 800)
par (mar = c(5,5,2,1), tcl=-0.3, cex.axis = 1.5,
     cex.lab= 2)

barplot(ind_par_parcela$n~ind_par_parcela$Parcela,
        ylab =" Indivíduos", xlab = "Sub-Parcela",
        ylim= c(0,250), col ="lightgreen")

dev.off()

Stats_alt$alt_class_cm <- as.character(Stats_alt$alt_class_cm)
alt_levels <- c("5", "5_15", "15_30","30_50", "50_100","100_150", "150_200", "200")
Stats_alt$alt_class_cm <- factor(
  Stats_alt$alt_class_cm,
  levels = alt_levels,
  ordered = TRUE
)


ggplot(Stats_alt, aes(x = alt_class_cm , y = mean)) +
  geom_bar(stat = "identity", position = position_dodge(0.9),
           color = "lightgreen")  +
  geom_errorbar(aes(ymin = mean - SD, ymax = mean + SD),
                position = position_dodge(0.9), width = 0.2)+
    labs(x = "Classe de Altura (cm)",
      y = "Biomassa média (g)")+
    scale_x_discrete(labels = c("5" = "5",
                                "5_15" = "6–15",
                                "15_30" = "16–30",
                                "30_50" = "31–50",
                                "50" = "50"))+
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    )
  )+
  facet_wrap(~ parcel )+
  ggsave(filename = "biomass_mean_se.jpg",
         width = 2000,height = 1500, units = "px")


barplot(log (biomass_seca$total_biomass_seca_g)~
          biomass_seca$Parcela)


boxplot (biomass_seca_g_estimada ~Transecto,
        data = Biomass_palms_archontophoenix,
        pch = "*")

jpeg(filename = "vioplot_biomass.jpg", width = 850, height = 500, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mar=c(5,5,3,2), bty = "l",cex.axis=2, cex.lab=2)

vioplot::vioplot(biomass_seca_g_estimada ~Transecto,
                 data = Biomass_palms_archontophoenix,
                 pch = "*", col ="lightgreen",
                 ylab =  "Biomassa (g)",
                 xlab = "Parcela")

dev.off ()

jpeg(filename = "barplot_biomass.jpg", width = 850, height = 500, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mar=c(5,5,3,2), bty = "l",cex=2)

barplot(ind_par_transce$n ~ind_par_transce$Transecto,
                 data = Biomass_palms_archontophoenix,
                 pch = "*", col ="lightgreen",
                 ylab =  "Indivíduos",
                 xlab = "Parcela", ylim = c(0,1200))

dev.off ()


boxplot(biomass_seca_g_estimada ~Transecto,
                 data = Biomass_palms_archontophoenix, pch = "*", log = "y")

#########linear analyses#################
#diagnostico do modelo



Biomass_palms_archontophoenix$dap_square<- Biomass_palms_archontophoenix$DAP_cm^2

plot (Biomass_palms_archontophoenix [,c(5,11,13, 14)], pch = 20,
      col =rgb (0.3,0,0.5,0.3))

par (mar=c(3,4,4,2), bty ="l")
plot (Biomass_palms_archontophoenix[,c(5,9)], pch = 20,
      col =rgb (0.3,0,0.5,0.3) )
abline (lm_bio_h, col = "red",
      lty = 2)


curve (
  expr =  -105.3075 + 6.4434 *h + 5.2015 * x,
  col = "blue",
       lty=4,
  add =TRUE
  )



plot (Biomass_palms_archontophoenix[,c(8,9)], pch = 20,
      col =rgb (0.3,0,0.5,0.3))


plot (Biomass_palms_archontophoenix[,c(5,9)], pch = 20,
      col =rgb (0.3,0,0.5,0.3))

plot (Biomass_palms_archontophoenix[,c(4,5,9)], pch = 20,
      col =rgb (0.3,0,0.5,0.3))


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

Biomass_palms_archontophoenix <- na.omit(Biomass_palms_archontophoenix)

lm_bio_h <- lm (biomass_seca_g_estimada~ altura_cm, data =
                               Biomass_palms_archontophoenix) #melhor explicação biológico


par (mar=c(5,4,4,2), bty ="l")
plot (biomass_seca_g_estimada~ altura_cm, data =
        Biomass_palms_archontophoenix, pch = "*",
      col =rgb (0.3,0,0.5,0.3),
      ylab = "Biomassa (g)",
      xlab = "Altura")
abline (lm_bio_h, lty = 2, col = "red", lwd =2)
text(x=20,y=2200, "r² = 0,93")


lm_bio_H_Dsq <- lm (biomass_seca_g_estimada~ altura_cm + I(DAP_cm^2), data =
                               Biomass_palms_archontophoenix)


summary(lm_bio_full)
summary (lm_bio_simple_extreme)
summary (lm_bio_simple)
summary (lm_bio_h)
summary (lm_bio_H_Dsq)

par (mfrow = c(2,2))
plot (lm_bio_H_Dsq)




shapiro.test(lm_bio_full$residuals)

AIC(lm_bio_h,
    lm_bio_H_Dsq,
    lm_bio_simple_extreme)

#resíduos sem normalidade

par (mfrow = c(2,2))
plot (lm_bio_full)
plot (lm_bio_simple)

boxplot (log (biomass_seca_g_estimada+1)~Transecto,
         data = Biomass_palms_archontophoenix)
boxplot.stats(log (Biomass_palms_archontophoenix$biomass_seca_g_estimada))

summary(lm_bio_full)
summary(lm_bio_simple)

anova(lm_bio_full,lm_bio_simple)

AIC (lm_bio_simple,
     lm_bio_full)


#########stats_Classes_alt_DBH###########




write.table(Stats_alt, "Stats_alt.csv",
            sep = "\t", dec = ",")

Biomass_by_alt_class <- Stats_alt[order(Stats_alt$alt_class_cm
                                        , decreasing = TRUE), ]


barplot(Biomass_by_alt_class$biomass_total ,
        col = "lightgreen", ylab="Biomass (%)",
        xlab= "Alt Classes cm"
        )
mtext(c ("5","5-15",
         "15-30","30-50",
         "50-150","150"), side= 1,
      at = c(.75,1.90,3.15,4.25,5.5,6.75), cex = 0.75)

Biomass_by_dbh_class=class_DBH_alt (Biomass_palms_archontophoenix, choice = "bio",
                                    class = c(1,3,5,7),dbh_alt="dbh",
                                    distribution = FALSE)

Biomass_by_dbh_class <- Biomass_by_dbh_class[order(Biomass_by_dbh_class$Class_DAP_cm
                                                   , decreasing = FALSE), ]



barplot(Biomass_by_dbh_class$Biomass_percentage,
        col = "lightgreen", ylim = c(0,50), ylab="Biomass (%)",
        xlab= "Alt Classes"
)

mtext()

class(x)
str (x)
#####################################

barplot(Biomass_palms_archontophoenix$Parcela ~ Biomass_palms_archontophoenix$Individuo
        )

