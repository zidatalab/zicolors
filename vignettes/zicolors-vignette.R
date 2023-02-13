## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(dev.args = list(png = list(type = "cairo")), # dpi=300,
                      fig.width = 7,
                      fig.height = 4,
                      fig.align = "center")

## ----logoforhtml, echo=FALSE--------------------------------------------------
htmltools::img(src = knitr::image_uri(file.path("../data/logo_zi.png")), 
               alt = 'logo', 
               width = '256' , 
               style = 'position:absolute; top:0; right:0; padding:10px;'
)

## ----loadlibraries,echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE----

library("devtools")
library("ggplot2")
library("dplyr")
library("tidyr")
library("readxl")
library("knitr")
library("kableExtra")
library("ggrepel")
library("stringr")
library("forcats")

## ----installpackage, echo=TRUE, eval=FALSE------------------------------------
#  # Installation des Codes von lokal
#  install_local("G:/93 Repository_Software/R_Packages/r_library_zi/zicolors")
#  
#  # Installation des Codes von github
#  devtools::install_github("zidatalab/zicolors")

## ----loadzicolors, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE----
library("zicolors")

## ----loadfonts, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE----
# Prepare fonts
library("showtext")

# myfamily <- "Roboto Condensed"
# font_add_google(name = myfamily, family = myfamily)


myfamily <- "TT Norms Pro Condensed"
font_add(family=myfamily,
         regular="G:/93 Repository_Software/Lizenzpflichtig/myfonts_ttnormspro/TTNormsProCond-Rg.ttf",
         bold="G:/93 Repository_Software/Lizenzpflichtig/myfonts_ttnormspro/TTNormsProCond-Bd.ttf")


showtext_auto()

## ----getdarmkrebsdata, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE----
Darmkrebsvorsorge_gesamt <- 
  readxl::read_excel("../data/Darmkrebsvorsorge Anzahl Patienten.xlsx",     
                     sheet = "GOP 01730-01748") %>%
  pivot_longer(cols = `2009`:`2017`, 
               names_to = "Jahr", 
               values_to = "Patienten")

Darmkrebsvorsorge_gesamt %>% 
  head() %>% 
  kable() %>% 
  kable_styling() %>% 
  row_spec(0, bold = T, color = "white", background = zi_cols("ziblaugruen"))

Darmkrebsvorsorge_Patienten <- 
  readxl::read_excel("../data/Darmkrebsvorsorge Anzahl Patienten.xlsx",
                     sheet = "GOP 01741",
                     na="<30") %>%
  pivot_longer(cols = `2009`:`2017`, 
               names_to = "Jahr",
               values_to = "Patienten") 

Darmkrebsvorsorge_Patienten %>% 
  head() %>%   
  kable() %>%
  kable_styling() %>%
  row_spec(0, bold = T, color = "white", background = zi_cols("ziblaugruen"))

## ----plotpalettes, caption="Übersicht über alle Farbskalen", echo=TRUE, message=FALSE, warning=FALSE----
n_col <- 128
img <- function(obj, nam) {
  image(1:length(obj), 1, as.matrix(1:length(obj)), col=obj, 
        main = nam, ylab = "", xaxt = "n", yaxt = "n",  bty = "n")
}

par(mfrow=c(5, 1), mar=rep(1, 4))
img(zi_pal("main")(2), "main")
img(zi_pal("shadesofblue")(n_col), "shadesofblue")
img(zi_pal("shadesofgreen")(n_col), "shadesofgreen")
img(zi_pal("intensity")(n_col), "intensity")
img(zi_pal("divergent")(n_col), "divergent")

## ----balkendiagramm, echo=TRUE, message=FALSE, warning=FALSE------------------
Darmkrebsvorsorge_01741 <- Darmkrebsvorsorge_gesamt %>%
  filter(GOP=="01741") %>% 
  group_by(Jahr) %>% 
  summarise(Patienten=sum(Patienten, na.rm=T)/1000)

ggplot(Darmkrebsvorsorge_01741, 
       aes(x=Jahr,y=Patienten)) + 
  geom_bar(stat="identity", width=0.5, fill=zi_cols("zihimmelblau")) + 
  theme_zi(font=myfamily) + 
  labs(title="Patient*innen mit Früherkennungskoloskopie", 
       subtitle="Anzahl in 1.000") + 
  geom_hline(yintercept = 0, size=0.5, col=zi_cols("ziblaugruen")) #+
# coord_cartesian(clip ="off") +
# geom_segment(x = 0.4, xend = 9.5,
#              y = 538, yend = 538,
#              color=zi_cols("ziblaugruen"), size=0.1)

## ----balken_axistitles, echo=TRUE, message=FALSE, warning=FALSE---------------
ggplot(Darmkrebsvorsorge_01741,
       aes(x=Jahr,y=Patienten)) + 
  geom_bar(stat="identity", width=0.5, fill=zi_cols("zihimmelblau")) + 
  theme_zi_axistitles(font=myfamily) + 
  labs(y="Früherkennungskoloskopie\nAnzahl in Tsd.", 
       x="Jahr") + 
  geom_hline(yintercept = 0, size=0.5, col=zi_cols("ziblaugruen")) 

## ----baken_hervorhebung, echo=TRUE, message=FALSE, warning=FALSE--------------
ggplot(Darmkrebsvorsorge_01741,
       aes(x=Jahr,y=Patienten,
           fill=ifelse(Jahr=="2013","blau","hellblau"))) + 
  geom_bar(stat="identity", width=0.5) + 
  theme_zi_axistitles(font=myfamily) + 
  scale_fill_manual(values=unname(c(
    zi_cols("zihimmelblau"), zi_cols("zihimmelblauhell")
  ))) +
  labs(y="Früherkennungskoloskopie\nAnzahl in Tsd.", x="Jahr", fill="") + 
  geom_hline(yintercept = 0, size=0.5, col=zi_cols("ziblaugruen")) 

## ----balken_horizontal, echo=TRUE, message=FALSE, warning=FALSE---------------
ggplot(Darmkrebsvorsorge_01741,
       aes(x=Jahr,y=Patienten, 
           fill=ifelse(Jahr=="2013","blau","hellblau"))) + 
  geom_bar(stat="identity", width=0.5) + 
  theme_zi_axistitles_horizontal(font=myfamily) + 
  scale_fill_manual(values=unname(c(
    zi_cols("zihimmelblau"), zi_cols("zihimmelblauhell")
  ))) +
  labs(y="Früherkennungskoloskopie\nAnzahl in Tsd.", x="Jahr", fill="") + 
  geom_hline(yintercept = 0, size=0.5, col=zi_cols("ziblaugruen")) +
  coord_flip()

## ----balken_stacked, echo=TRUE, message=FALSE, warning=FALSE------------------
set.seed(42)
beispieldaten <- tibble(X=sample(LETTERS[1:4], 1000, replace=TRUE),
                        G=sample(letters[1:3], 1000, replace=TRUE))

beispieldaten_summed <- beispieldaten %>% 
  count(X, G)

ggplot(beispieldaten_summed,
       aes(x=X,
           y=n,
           fill=G)) + 
  geom_bar(stat="identity", width=0.5, color="white") + 
  scale_fill_manual(values=unname(c(
    zi_cols("zihimmelblau"),
    zi_cols("zilindgruen"),
    zi_cols(("zihimmelblaudunkel"))))) +
  theme_zi(font=myfamily) +
  labs(fill="", 
       title="Stacked Bar Chart",
       subtitle="Anzahl") + 
  geom_hline(yintercept = 0, size=0.5, col=zi_cols("ziblaugruen"))

## ----balken_stacked_famos, echo=TRUE, message=FALSE, warning=FALSE------------
beispieldaten_summed_withposition <- beispieldaten_summed %>% 
  group_by(X) %>% 
  arrange(rev(G), .by_group = TRUE) %>% 
  mutate(myposition=cumsum(n)-n/2) %>% 
  ungroup()

ggplot(beispieldaten_summed_withposition,
       aes(x=X,
           y=n,
           fill=G)) + 
  geom_bar(stat="identity", width=0.4, color="white") + 
  scale_fill_manual(values=unname(c(
    zi_cols("zihimmelblau"),
    zi_cols("zilindgruen"),
    zi_cols(("zihimmelblaudunkel"))))) +
  theme_zi(font=myfamily) +
  labs(fill="", 
       title="Stacked Bar Chart",
       subtitle="Anzahl") + 
  geom_text(aes(label=after_stat(y), group=X),
            stat='summary', fun=sum, vjust = -1,
            family=myfamily, fontface="bold", 
            size=3,
            color=zi_cols("ziblaugruen")) +
  geom_text_repel(aes(label=n, y=myposition),
                  family=myfamily, color=zi_cols("ziblaugruen"),
                  size=3,
                  nudge_x = -0.38,
                  point.padding = 3.5) +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  coord_cartesian(clip = "off") +
  geom_hline(yintercept = 0, size=0.5, col=zi_cols("ziblaugruen"))

## ----liniendiagramm, echo=TRUE, message=FALSE, warning=FALSE------------------
plotdata <- Darmkrebsvorsorge_Patienten %>% 
  mutate(Geschlecht=as.character(Geschlecht)) %>% 
  filter(Geschlecht %in% c("männlich","weiblich")) %>%
  group_by(Geschlecht, Jahr) %>% 
  summarise(Patienten=sum(Patienten, na.rm=T)/1000)

ggplot(plotdata, aes(x=Jahr,y=Patienten, color=Geschlecht, group=Geschlecht)) + 
  geom_line(size=2) +  
  geom_point(shape = 21,size=3,fill="White",stroke=2) + 
  theme_zi(font=myfamily) + 
  ylim(0, NA) +
  labs(title="Patient*innen mit Früherkennungskoloskopie", subtitle="Anzahl in 1000",
       color="") + 
  scale_color_zi("main") + scale_y_continuous(breaks=seq(0,300,50), 
                                              limits=c(0,300)) 

## ----liniendiagramm_labels, echo=TRUE, message=FALSE, warning=FALSE-----------
ggplot(plotdata, aes(x=Jahr,y=Patienten,
                     color=Geschlecht, group=Geschlecht, label=Geschlecht)) + 
  geom_line(size=2) +  
  geom_point(shape = 21,size=3,fill="White",stroke=2) + 
  theme_zi(font=myfamily) +
  theme(legend.position = "none") +
  ylim(0, NA) +
  labs(title="Patient*innen mit Früherkennungskoloskopie", subtitle="Anzahl in 1.000") + 
  scale_color_zi("main") +
  scale_y_continuous(breaks=seq(0,300,50), 
                     limits=c(0,300)) +
  geom_text_repel(data = . %>% filter (Jahr == max(Jahr)), 
                  seed = 1,
                  family=myfamily, 
                  size = 4,
                  point.padding = 20, ## wenig padding
                  direction = 'y',
                  nudge_y = c(0,1),
                  force_pull = 100)

## ----liniendiagramm_facets, echo=TRUE, message=FALSE, warning=FALSE-----------
plotdata <- Darmkrebsvorsorge_Patienten %>% 
  mutate(Geschlecht=as.character(Geschlecht),
         Alter = as.character(Alter)) %>% 
  filter(Geschlecht %in% c("männlich","weiblich")) %>%
  filter(Alter %in% c("40 bis 59 Jahre","60 bis 79 Jahre")) %>%
  group_by(Geschlecht,Alter,Jahr) %>%
  summarise(Patienten=sum(Patienten, na.rm=T)/1000) 

ggplot(plotdata, aes(x=Jahr,y=Patienten, color=Geschlecht, group=Geschlecht)) + 
  geom_line(size=2) + 
  facet_grid(Alter~. ) + # , scales = "free_y"
  geom_point(shape = 21,size=3,fill="White",stroke=2) + 
  theme_zi(font=myfamily) + 
  ylim(0, NA) + 
  labs(title="Patient*innen mit Früherkennungskoloskopie", subtitle="Anzahl in 1.000",
       color="") +
  scale_color_zi("main") 

## ----plot_kaplanmeier, echo=TRUE, message=FALSE, warning=FALSE----------------
library(survival)
# data(ovarian)
library(survminer)

my_surv <- Surv(ovarian$futime, ovarian$fustat) # make a survival-object
my_cox <- coxph(my_surv ~ rx, data=ovarian) # fit a Cox-prop.-hazard-model
my_fit <- survfit(my_surv ~ rx, conf.type="log-log", data=ovarian) # create a survfit-object for plotting

ggsp <- ggsurvplot(my_fit, censor.shape="I")
ggsp_data <- ggsp$plot$data # we need to extract the data from the ggsurvplot-object

ggplot(ggsp_data) + aes(x=time,color=ifelse(strata=="rx=1","Gruppe 1", "Gruppe 2"),y=surv*100) + geom_step(size=2) + scale_color_zi() + theme_zi(font=myfamily) + scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100)) + labs(color="",title="Überlebensanalyse",subtitle="Überlebensrate in % nach Tagen") + theme(legend.position = "bottom" )


## ----boxplots, echo=TRUE, message=FALSE, warning=FALSE------------------------
# data(iris)
library(EnvStats) # to display sample sizes
ggplot(data=iris, aes(x=Species, y=Sepal.Length, fill=Species, color=Species)) + 
  geom_boxplot(lwd=1, fatten=1.5, alpha=.8, show.legend = FALSE) +
  theme_zi(font=myfamily) + 
  scale_fill_manual(values=unname(c(
    zi_cols("zihimmelblau"),
    zi_cols("zilindgruen"),
    zi_cols(("zihimmelblaudunkel"))))) +
  scale_color_manual(values=unname(c(
    zi_cols("zihimmelblau"),
    zi_cols("zilindgruen"),
    zi_cols(("zihimmelblaudunkel"))))) +
  labs(title="Iris data: species comparison", subtitle="Sepal length in cm") + 
  ylim(0, NA) +
  stat_n_text(y.expand.factor=0.2, size=3.5, family=myfamily, 
              color = zi_cols("ziblaugruen")) # "#194B5A"

## ----karte, echo=TRUE, message=FALSE, warning=FALSE---------------------------
library(sf)

KRS <- read_sf("../data/shp/kreise.shp")
BL <- KRS %>%
  group_by(SN_L) %>%
  summarise(geometry = sf::st_union(geometry),
            .groups="drop")

plot_karte_krs_bl <-
  KRS %>%
  # mutate(Typ=case_when(
  #   BEZ %in% c("Landkreis", "Kreis") ~ "Kreis bzw. Landkreis",
  #   BEZ %in% c("Kreisfreie Stadt", "Stadtkreis") ~ "Kreisfreie Stadt bzw. Stadtkreis",
  #   TRUE ~ "FEHLER"
  # )) %>% 
  mutate(Typ=fct_relevel(BEZ, levels=c("Stadtkreis", "Kreisfreie Stadt", "Landkreis", "Kreis"))) %>%
  ggplot() +
  geom_sf(aes(fill=Typ),
          lwd=0.1, color=zi_cols("ziblaugruenhell")) +
  geom_sf(data=BL, lwd=0.2, alpha=0, color=zi_cols("ziblaugruen")) +
  theme_zi_void(font=myfamily) +
  labs(title="Deutschlandkarte",
       subtitle="unterschiedliche Bezeichungen des Kreistyps",
       caption="Quelle: © GeoBasis-DE / BKG 2020, http://www.bkg.bund.de",
       fill="") +
  scale_fill_zi("divergent")

plot_karte_krs_bl 

## ----plot_dumbbell, echo=TRUE, message=FALSE, warning=FALSE-------------------
plotdata <- Darmkrebsvorsorge_gesamt %>% 
  filter(Jahr %in% c(min(Jahr),max(Jahr))) %>%
  filter(!is.na(Patienten))

dumbbell_plot <- ggplot(plotdata) +aes(x=str_trunc(Beschreibung,60),
                                       y=Patienten/1000000,
                                       color=Jahr, 
                                       group=GOP) + 
  geom_line(color=zi_cols("ziblaugruenhell"), size=1.5) + # linewidth
  geom_point(size=4) + 
  labs(title="GOPen zur Darmkrebsvorsorge",
       subtitle="Anzahl in Mio. Patient*innen im Jahresvergleich",
       color="") +
  theme_zi(font=myfamily) + 
  scale_color_zi("main") +
  coord_flip()
dumbbell_plot

## ----dumbbell_caption, echo=TRUE, message=FALSE, warning=FALSE----------------
dumbbell_plot_mitcaption <- dumbbell_plot +
  labs(caption="Datenbasis: Vertragsärztliche Abrechnungsdaten 2009-2017.")

showtext_opts(dpi = 300)

ggsave("data/dumbbell.png", 
       plot=dumbbell_plot_mitcaption, 
       width=16, height=9,
       units="cm", dpi=300)

ggsave("data/dumbbell.svg", 
       plot=dumbbell_plot_mitcaption, 
       device="svg",
       width=16, height=9,
       units="cm", dpi=300)

## ----dumbbell_logolinks, echo=TRUE, message=FALSE, warning=FALSE, include=TRUE----
library(magick)
finalise_plot_logolinks(plot_name = dumbbell_plot,
                        source_name = "Datenbasis: Vertragsärztliche Abrechnungsdaten 2009-2017.",
                        width = 16,
                        height = 9,
                        save_filepath = "data/dumbbell_logolinks.png"
)

## ----dumbbell_logorechts, echo=TRUE, message=FALSE, warning=FALSE, include=TRUE----
library(magick)
finalise_plot_logorechts(plot_name = dumbbell_plot,
                         source_name = "Datenbasis: Vertragsärztliche Abrechnungsdaten 2009-2017.",
                         width = 16,
                         height = 9,
                         hjust_source = 0.85,
                         save_filepath = "data/dumbbell_logorechts.png"
)

