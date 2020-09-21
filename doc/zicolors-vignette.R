## ---- echo=FALSE---------------------------------------------------------
htmltools::img(src = knitr::image_uri(file.path("../data/logo_zi.jpg")), 
               alt = 'logo', 
               width = '256' , 
               style = 'position:absolute; top:0; right:0; padding:10px;'
              )

## ---- message=FALSE, warning=FALSE---------------------------------------

library("devtools")
library("ggplot2")
library("dplyr")
library("tidyr")
library("readxl")
library("knitr")
library("kableExtra")
library("ggrepel")
library("stringr")
devtools::install_local(path="G:/Austausch/FB3/13 Software/zicolors")

## ---- message=FALSE, warning=FALSE---------------------------------------
library("zicolors")
library("extrafont")
loadfonts(device="win", quiet=T)

## ---- warning=FALSE------------------------------------------------------
Darmkrebsvorsorge_gesamt <- 
  readxl::read_excel("G:/Austausch/FB3/13 Software/zicolors/data/Darmkrebsvorsorge Anzahl Patienten.xlsx",     
                     sheet = "GOP 01730-01748") %>% gather(Jahr,Patienten,3:dim(.)[2])
Darmkrebsvorsorge_gesamt %>% head() %>% 
   kable() %>%   kable_styling() %>% row_spec(0, bold = T, color = "white", background = zi_cols("ziblue"))

Darmkrebsvorsorge_Patienten <- readxl::read_excel("G:/Austausch/FB3/13 Software/zicolors/data/Darmkrebsvorsorge Anzahl Patienten.xlsx",     
                                                  sheet = "GOP 01741") %>%
  gather(Jahr,Patienten,3:dim(.)[2])  %>% mutate(Patienten=ifelse(Patienten=="<30",NA,as.numeric(Patienten)))
Darmkrebsvorsorge_Patienten %>% head() %>%   kable() %>%
  kable_styling() %>% row_spec(0, bold = T, color = "white", background = zi_cols("ziblue"))

## ----  fig.height = 0.5, fig.width = 6, fig.align ="left", echo=FALSE, caption="Übersicht über alle Farbskalen"----
explot <- as.data.frame(cbind("x"=seq(1:128),"y"=1)) %>%
  ggplot(.,aes(x=x,y=y,fill=x)) + geom_bar(stat="identity",width=1) + 
  theme_void() + guides(fill=FALSE)
for (zicolorsheme in names(zicolors::zi_palettes)) {
  myplot <- explot + geom_text(aes(x=20,y=.5, label=paste(zicolorsheme)),
                               size=4,hjust="left",color="white") + 
          scale_fill_zi(paste(zicolorsheme), discrete = FALSE)
  print(myplot)
}

## ---- fig.height = 4.5, fig.width = 5 , fig.align ="left"----------------
Darmkrebsvorsorge_gesamt %>% filter(GOP=="01741") %>% group_by(Jahr) %>% 
  summarise(Patienten=sum(Patienten, na.rm=T)/1000) %>% 
  ggplot(., aes(x=Jahr,y=Patienten)) + 
  geom_bar(stat="identity", fill=zi_cols("ziblue")) + 
  theme_zi() + 
  labs(title="Patienten mit Früherkennungskoloskopie", subtitle="Anzahl in 1000")

## ---- fig.height = 4.5, fig.width = 7 , fig.align ="left"----------------
Darmkrebsvorsorge_gesamt %>% filter(GOP=="01741") %>% group_by(Jahr) %>% 
  summarise(Patienten=sum(Patienten, na.rm=T)/1000) %>% 
  ggplot(., aes(x=Jahr,y=Patienten)) + 
  geom_bar(stat="identity", fill=zi_cols("ziblue")) + 
  theme_zi_titels() + 
  labs(y="Früherkennungskoloskopie\nAnzahl in Tsd.", x="Jahr")

## ---- fig.height = 4.5, fig.width = 7 , fig.align ="left"----------------
Darmkrebsvorsorge_gesamt %>% filter(GOP=="01741") %>% group_by(Jahr) %>% 
  summarise(Patienten=sum(Patienten, na.rm=T)/1000) %>% 
  ggplot(., aes(x=Jahr,y=Patienten)) + 
  geom_bar(stat="identity", fill=zi_cols("ziblue")) + 
  theme_zi_titels() + 
  labs(y="Früherkennungskoloskopie\nAnzahl in Tsd.", x="Jahr") + 
  geom_hline(yintercept = 0, size=0.75) 

## ---- fig.height = 4.5, fig.width = 7------------------------------------
Darmkrebsvorsorge_gesamt %>% filter(GOP=="01741") %>% group_by(Jahr) %>% 
  summarise(Patienten=sum(Patienten, na.rm=T)/1000) %>% 
  ggplot(., aes(x=Jahr,y=Patienten, fill=ifelse(.$Jahr==2013,"blau","grau"))) + 
  geom_bar(stat="identity") + guides(fill=F) +
  theme_zi_titels() + scale_fill_zi("bluegrey") +
  labs(y="Früherkennungskoloskopie\nAnzahl in Tsd.", x="Jahr") + 
  geom_hline(yintercept = 0, size=0.75) 

## ---- fig.height = 4.5, fig.width = 7 , fig.align ="left"----------------
Darmkrebsvorsorge_Patienten %>% 
  mutate(Geschlecht=as.character(Geschlecht)) %>% 
  filter(Geschlecht %in% c("männlich","weiblich")) %>%
  group_by(Geschlecht,Jahr) %>% summarise(Patienten=sum(Patienten, na.rm=T)/1000) %>%
  ggplot(., aes(x=Jahr,y=Patienten, color=Geschlecht, group=Geschlecht)) + 
  geom_line(size=2) +  
  geom_point(shape = 21,size=3,fill="White",stroke=2) + 
  theme_zi() + 
  labs(title="Patienten mit Früherkennungskoloskopie", subtitle="Anzahl in 1000") + 
  scale_color_zi("main2colors", guide=F) + scale_y_continuous(breaks=seq(100,300,50), 
                                                              limits=c(100,300)) +
  geom_label(data=. %>% filter(Jahr==max(Jahr)) , aes(label = Geschlecht), 
             label.size = NA, vjust=-.5,
             family="Calibri", 
             size = 4)

## ---- fig.height = 5.5, fig.width = 7 , fig.align ="left"----------------
Darmkrebsvorsorge_Patienten %>% 
  mutate(Geschlecht=as.character(Geschlecht),
         Alter = as.character(Alter)) %>% 
  filter(Geschlecht %in% c("männlich","weiblich")) %>%
  filter(Alter %in% c("40 bis 59 Jahre","60 bis 79 Jahre")) %>%
  group_by(Geschlecht,Alter,Jahr) %>% summarise(Patienten=sum(Patienten, na.rm=T)/1000) %>%
  ggplot(., aes(x=Jahr,y=Patienten, color=Geschlecht, group=Geschlecht)) + 
  geom_line(size=2) + 
  facet_grid(Alter~. ) + # , scales = "free_y"
  geom_point(shape = 21,size=3,fill="White",stroke=2) + 
  theme_zi(fontsize = 11) + 
  labs(title="Patienten mit Früherkennungskoloskopie", subtitle="Anzahl in 1000") + 
  scale_color_zi("main2colors", guide=F) +
  geom_label_repel(data=. %>% filter(Jahr==max(Jahr)) , aes(label = Geschlecht), 
             label.size = NA, vjust=0.4, fill=NA,
             family="Calibri", 
             size = 4) +
  theme(panel.spacing = unit(2, "lines"))

## ---- fig.height = 5, fig.width = 7, fig.align ="left", message=FALSE, warning=FALSE----
library(survival)
data(ovarian)
library(survminer)

my_surv <- Surv(ovarian$futime, ovarian$fustat) # make a survival-object
my_cox <- coxph(my_surv ~ rx, data=ovarian) # fit a Cox-prop.-hazard-model
my_fit <- survfit(my_surv ~ rx, conf.type="log-log", data=ovarian) # create a survfit-object for plotting

ggsp <- ggsurvplot(my_fit, censor.shape="I")
ggsp_data <- ggsp$plot$data # we need to extract the data from the ggsurvplot-object
ggsp_zi <- ggsurvplot(my_fit, censor.shape="I", size=2)$plot + theme_zi_titels() + 
  theme(panel.grid.major.x = element_line(size=0.1, color="#cbcbcb"),
        panel.grid.major.y = element_line(size=0.1, color="#cbcbcb")) + # display vertical lines since time points between curves are not aligned
  scale_color_zi(guide=FALSE) + # use zi color scheme
  scale_y_continuous(labels = scales::percent) + # display percent instead of 0-1-probability
  labs(title="Ovarian cancer survival analysis", subtitle="Treatment groups rx=1 and rx=2", x="Time (in days)") +
  geom_label_repel(data = . %>% filter(time %in% aggregate(time~strata, data=ggsp_data, max)$time),
             aes(label = strata, color=strata), 
             label.size = NA, vjust=-0.5, fill=NA,
             family="Calibri", segment.color = NA,
             size = 4) # this adds the grouping at the endpoints of lines
ggsp_zi

## ---- fig.height = 4, fig.width = 8, fig.align ="left", message=FALSE, warning=FALSE----
dumbbell_plot <- Darmkrebsvorsorge_gesamt %>% filter(Jahr %in% c(min(Jahr),max(Jahr)))  %>% filter(!is.na(Patienten)) %>%
    ggplot(aes(x=paste(str_trunc(Beschreibung,45, "right")),
               y=Patienten/1000000,
               color=Jahr, 
               group=GOP)) + 
  geom_line(color="grey", size=3) +
  geom_point(size=4) + 
  theme_zi(fontsize = 11) + scale_color_zi("main2colors") + coord_flip() +
  labs(title="Ambulante Leistungen zur Krebsfrüherkennung",
       subtitle="Anzahl in Mio. abgerechneter Leistungen")     +
  theme(legend.position =c(.9,.85) ,
        )

dumbbell_plot
  

## ---- fig.height = 4, fig.width = 6, fig.align ="left", message=FALSE, warning=FALSE----
data(iris)
library(EnvStats) # to display sample sizes
ggplot(data=iris, aes(x=Species, y=Sepal.Length, fill=Species,color=Species)) + geom_boxplot(lwd=1, fatten=1.5, alpha=.8) + theme_zi() + 
  scale_fill_zi(guide=FALSE) +scale_color_zi(guide=FALSE) +  
  labs(title="Iris data: species comparison", subtitle="Sepal length in cm") + 
  stat_n_text(y.expand.factor=0.2, size=3.5)

## ---- fig.height = 4.5, fig.width = 7,message=FALSE, warning=FALSE-------
library("ggwaffle")
KM6_2018 <- readr::read_csv2("G:/Austausch/FB3/14 Daten/KM6_Statistik/KM6_aufbereitet.csv") %>%
  filter(Jahr==2018) %>% group_by(Versichertengruppe,Geschlecht) %>% summarise(Anzahl=sum(Anzahl))
KM6_2018.waffle <- KM6_2018 %>% mutate(Anzahl=floor(Anzahl/1000000)) %>%  
  slice(rep(1:n(), each = Anzahl)) %>% arrange(Geschlecht,Versichertengruppe) %>%   bind_cols(.,waffle_iron(.,aes_d(group = Geschlecht)) %>% arrange(group) ) 

ggplot(KM6_2018.waffle %>% arrange(x,y), aes(x, y, fill = Geschlecht)) + 
  geom_waffle(show.legend = F)  + coord_fixed() + theme_void() +  geom_text(aes(label=ifelse(Geschlecht=="Männer","M","F")),color="White") + scale_fill_zi() +
  labs(title="Gesetzliche Krankenversicherung",
       subtitle="Ein Symbol entspricht 1 Mio. Versicherten im Jahr 2018")


## ------------------------------------------------------------------------
library("sf")
shppath <- "G:/Austausch/FB3/14 Daten/Shapefiles/vg1000-ew_31122017/vg1000-ew_ebenen/"
krs2017 <- sf::st_read(dsn=paste0(shppath,"VG1000_KRS.shp"))
bl2017 <- sf::st_read(dsn=paste0(shppath,"VG1000_LAN.shp"))

## ---- fig.height = 4.5, fig.width = 7------------------------------------
ggplot(bl2017 %>% filter(EWZ>0)) + geom_sf(fill=zi_cols("ziblue") , color="White", size=1) + 
  scale_fill_zi(discrete=F) + theme_zi_void(fontsize=11) + labs(title="Bundesländer")+
  labs(caption="© GeoBasis-DE / BKG 2019")

## ---- fig.height = 4.5, fig.width = 7------------------------------------
ggplot(krs2017 %>% filter(EWZ>0)) + 
  geom_sf(fill=zi_cols("ziblue") , color="White", size=0.2) + 
  geom_sf(data=bl2017 %>% filter(EWZ>0), fill=NA , color=zi_cols("zidarkblue"), size=1) + 
  scale_fill_zi(discrete=F) + theme_zi_void(fontsize = 11)  + labs(title="Landkreise und Städte") +
  labs(caption="© GeoBasis-DE / BKG 2019")

## ---- fig.height = 4.5, fig.width = 7------------------------------------
ggplot(krs2017 %>% filter(EWZ>0), aes(fill=floor(EWZ/1000))) + 
  geom_sf(color="White", size=0.2) + 
  geom_sf(data=bl2017 %>% filter(EWZ>0), fill=NA , color=zi_cols("zidarkblue"), size=1) + 
  scale_fill_zi("main4colors", discrete=F) + theme_zi_void(fontsize = 11) + 
  labs(title="Landkreise und Städte", subtitle="Einw. in Tsd.", fill="") +
  labs(caption="© GeoBasis-DE / BKG 2019") 

## ------------------------------------------------------------------------
# finalise_plot(plot_name = dumbbell_plot,
# source = "Datenbasis: Vertragsärztliche Abrechnungsdaten der KVen 2009-2017",
# width_cm = 20,
# height_cm =11.25,
# save_filepath = "data/dumbbell.jpg"
# )

## ------------------------------------------------------------------------

# ggsave(plot = dumbbell_plot + 
#          theme_zi_titels(fontsize=14) + 
#          theme(plot.title=element_blank(),
#                plot.subtitle=element_blank(), 
#                legend.position = c(.8,.05),
#                legend.direction = "horizontal") +
#          labs(x="",y="Anzahl in Mio"), 
#        filename="Export_Dumbbell_PPTX.png", 
#        width=22.2, height=12.57, units = "cm" , dpi=300)


