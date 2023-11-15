library("readxl")
library("ggplot2")
library("dplyr")
library("plotly")
library("stringr")
library("hrbrthemes")
library("dplyr")
library(ggthemr)
ggthemr('solarized')

#reading dataset
my_data <- as.data.frame(read_excel("Book1.xlsx", sheet = "MPS Borough Level Crime (Histor"))

my_data <- my_data[my_data$LookUp_BoroughName != "London Heathrow and London City Airports",] ##Removing London Heathrow and London City Airports rows as they are the same as Aviation Security

total_data <- data.frame()

# Populate the data frame using a for loop
for (i in 5 : 14) {
  # Get the row data
  year <- str_sub(colnames(my_data)[i], start= -4)
  total <- sum(my_data[, i])
  
  # Populate the row
  new.row <- data.frame(year = year, total = total)
  
  # Add the row
  total_data <- rbind(total_data, new.row)
}

###################################### visualisation 1 #########################
total_data %>%
  ggplot( aes(x=year, y=total, group = 1)) +
  geom_area(fill="#69b3a2", alpha=0.5) + ##Figure out why geom_area is not working when setting ylim
  geom_line(color="#69b3a2") +
  geom_point(color="#69b3a2")+
  labs(title="Yearly Total of Crimes", x="Year", y="Total Number of Crimes" )+
  coord_cartesian(ylim=(c(700000,950000)))+ 
  scale_color_viridis_c()+ 
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 0.4
        )
  )


############################### end of visualisation 1 #########################


s <- as.data.frame(my_data %>% group_by(MajorText) %>% summarise( "2011" =sum(`Total 2011`),
                                                                  "2012" =sum(`Total 2012`),
                                                                  "2013" =sum(`Total 2013`),
                                                                  "2014" =sum(`Total 2014`),
                                                                  "2015" =sum(`Total 2015`),
                                                                  "2016" =sum(`Total 2016`),
                                                                  "2017" =sum(`Total 2017`),
                                                                  "2018" =sum(`Total 2018`),
                                                                  "2019" =sum(`Total 2019`),
                                                                  "2020" =sum(`Total 2020`),
                                                                  .groups="drop"))


s <- s[order(s$`2011`, s$`2012`, s$`2013`, s$`2014`,
             s$`2015`, s$`2016`, s$`2017`, s$`2018`,
             s$`2019`, s$`2020`,decreasing = TRUE),]
s <- s[1:5,]

top_crime <- data.frame()

# Populate the data frame using a for loop
for (i in 1 : 5) {
  # Get the row data
  crime <- s[[i,1]]
  for (j in 2:11){
    year <- colnames(s)[j]
    cnt <- s[[i, j]]
    # Populate the row
    new.row <- data.frame(crime = crime, year=year, cnt=cnt)
    # Add the row
    top_crime <- rbind(top_crime, new.row)
  }
}

############################### Visualisation No.2 #############################

ggplot( top_crime,aes(x=year, y=cnt, group=crime, color=crime)) + ##stacked line chart
  geom_line(linewidth = 1.2, linetype=1)+
  labs(title="Top 5 Crimes", x="Year", y="Number of incidents", color="Crime" )+
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 0.4
        )
  )



######################### end of visualisation 2 ###############################

homicide <- my_data[my_data$MajorText == "Violence Against the Person",]


s2 <- as.data.frame(homicide %>% group_by(MinorText) %>% summarise( "2011" =sum(`Total 2011`),
                                                                    "2012" =sum(`Total 2012`),
                                                                    "2013" =sum(`Total 2013`),
                                                                    "2014" =sum(`Total 2014`),
                                                                    "2015" =sum(`Total 2015`),
                                                                    "2016" =sum(`Total 2016`),
                                                                    "2017" =sum(`Total 2017`),
                                                                    "2018" =sum(`Total 2018`),
                                                                    "2019" =sum(`Total 2019`),
                                                                    "2020" =sum(`Total 2020`),
                                                                    .groups="drop"))

homicideT <- data.frame()
for (i in 1 : 3) {
  # Get the row data
  crime <- s2[[i,1]]
  for (j in 2:11){
    year <- colnames(s2)[j]
    cnt <- s2[[i, j]]
    # Populate the row
    new.row <- data.frame(crime = crime, year=year, cnt=cnt)
    # Add the row
    homicideT <- rbind(homicideT, new.row)
  }
}


VioHom <- rbind(top_crime[top_crime$crime=="Violence Against the Person",], homicideT)


################################### Visualisation no.3 #########################

library(ggpubr)
l1 <- homicideT[homicideT$crime=="Homicide" & homicideT$year >= 2015 ,] %>% ggplot(aes(x=year, y=cnt, group=1)) +
  geom_line( color="#69b3a2", linewidth=1, alpha=0.9) +
  labs(title="Homicide", x="Year", y="Count" )+
  scale_color_viridis_d()+ 
  theme(
    panel.border = element_rect(color = "black",
                                fill = NA,
                                size = 0.4
    )
  )

l2 <- homicideT[homicideT$crime=="Violence with Injury" & homicideT$year >= 2015,] %>% ggplot(aes(x=year, y=cnt,group=1)) +
  geom_line( color="#69b3a2", linewidth=1, alpha=0.9) +
  labs(title="Violence with Injury", x="Year", y="Count" )+
  scale_color_viridis_d()+ 
  theme(
    panel.border = element_rect(color = "black",
                                fill = NA,
                                size = 0.4
    )
  )

l3 <- homicideT[homicideT$crime=="Violence without Injury"  & homicideT$year >= 2015,] %>% ggplot(aes(x=year, y=cnt,group=1)) +
  geom_line( color="#69b3a2", linewidth=1, alpha=0.9) +
  labs(title="Violence without Injury", x="Year", y="Count" )+
  scale_color_viridis_d()+ 
  theme(
    panel.border = element_rect(color = "black",
                                fill = NA,
                                size = 0.4
    )
  )


fig <- ggarrange(l1, l2 ,l3 , 
                 ncol = 1, nrow = 3)+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 0.4
  )
  ) 
annotate_figure(fig, top = text_grob("Breakdown of Violence against Person", 
                                     color = "#586E75", face = "bold", size = 14))

############################# end of Visualisation 3 ###########################

##Reading homicide victims data
homi_data <- as.data.frame(read_excel("LDS Homicide Victims 2003-June 2022 (version 1).xlsb.xlsx", sheet = "LDS Homicide Victims 2003-June "))
class(my(homi_data$`MTH YEAR`))
my(homi_data$`MTH YEAR`)
class(homi_data$`MTH YEAR`)
homi_20 <- homi_data[homi_data$YEAR==2020, ]
homi_1920 <- homi_data[homi_data$YEAR==2020 | homi_data$YEAR == 2019,]

homi_data$`MTH YEAR` <- my(homi_data$`MTH YEAR`)
s3 <- as.data.frame(homi_20 %>% group_by(Borough) %>% summarise( cnt =sum(`Count of Victims`)))
s4 <- as.data.frame(my_data %>% group_by(LookUp_BoroughName) %>% summarise( cnt =sum(`Total 2020`)))

##################################### visualisation 4 ##########################

libs <- c("ggplot2", "rgeos", "rgdal", "maps", "mapdata", "mapproj", "maptools", "sp", "ggmap")
lapply(libs, library, character.only = TRUE)
###Reading GIS shape file
dir_2 <- "data/statistical-gis-boundaries-london/ESRI/"
ldn <- readOGR(file.path(dir_2), layer = "London_Borough_Excluding_MHW")


library("tmap")
qtm(ldn1)
ldn1 <- ldn
ldn1@data <- left_join(ldn1@data, s3, by=c('NAME'='Borough'))


ldn2 <- ldn
ldn2@data <- left_join(ldn2@data, s4, by=c('NAME'='LookUp_BoroughName'))

m1 <- tm_shape(ldn1) +
  tm_fill("cnt", style="kmeans", border.col = "black", textNA = "0/ Data not available",
          palette = 'YlGnBu', title = "Number of incidents") + 
  tm_layout("Homicides in 2020")+
  tm_text('NAME', size =0.6)+
  tm_borders(alpha=0.5)

m2 <- tm_shape(ldn2) +
  tm_fill("cnt", style="kmeans", border.col = "black", textNA = "Data not available", 
          palette = 'YlGnBu', title = "Number of incidents") + 
  tm_layout("All crimes in 2020")+
  tm_text('NAME', size =0.6)+
  tm_borders(alpha=0.5)

tmap_arrange(m2, m1, ncol = 1) + tm_layout(
  main.title = "The quick brown fox jumps over the lazy dog", 
  main.title.position = "center")

tmaannotate_figure(map1, top = text_grob("Breakdown of Violence against Person", 
                                  color = "#586E75", face = "bold", size = 14))    

############################# end of visualisation 4 ###########################



homi_20 <- homi_data[homi_data$YEAR==2020,]

x <- homi_1920 %>% group_by(YEAR, month = month(`MTH YEAR`)) %>%  summarise( count = n())

month.abb[x$month]


################################## Visualisation 5 #############################
###first lockdown on 23rd march until 15june, 2nd lockdown from Nov 5 
library(plotly)
plot1 <- ggplot(x, aes( x=month.abb[month], fill=YEAR, y= count)) + # fill=name allow to automatically dedicate a color for each group
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete(limits = month.abb)+
  labs(title="Monthly comparison between 2019 and 2020 homicides", x="Month", y="Number of incidents") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 0.4
        )) +
  scale_color_viridis_c()+ 
  annotate("rect", xmin = 2.5, 
           xmax = 6.5, ymin = 0, 
           ymax = 20, color = "white" ,alpha = .3)+
  annotate("rect", xmin = 10.5, 
           xmax = 12.5, ymin = 0, ymax = 20, 
           color = "white" ,alpha = .3)+
  annotate("rect", xmin = .7, 
           xmax = 2.3, ymin = 16.5, ymax = 17.5, 
           color = "white" ,alpha = .3) +
  annotate(geom="text", x=1.5, y=17, 
           label="<span style='color: darkred;'>1st Lockdown</span>")+
  annotate(geom = "segment", x = 2.301, 
           xend = 2.5, y=17, yend=17, 
           colour="black", alpha = .4 ) +
  annotate("rect", xmin = 8.7, 
           xmax = 10.3, ymin = 16.5, ymax = 17.5, 
           color = "white" ,alpha = .3) +
  annotate(geom="text", x=9.5, y=17, 
           label="<span style='color: darkred;'>2nd Lockdown</span>")+
  annotate(geom = "segment", x = 10.301, 
           xend = 10.5, y=17, yend=17, 
           colour="black", alpha = .4 ) 
  
    
ggplotly(plot1)  
  
################################ end of visualisation 5 ########################
  
  
################################## Visualisation 6 #############################
  
library(ggalluvial)
test <- homi_20 %>% group_by(`Age Group`,`Sex`,`Method of Killing`, `Homicide Offence Type`) %>% summarise(count=n())
homi_20 %>% group_by(`Age Group`,`Sex`,`Method of Killing`) %>% summarise(count=n(), percent = n()/133) %>%
  ggplot(aes(y=percent, axis1=Sex, axis2=`Age Group`, axis3=`Method of Killing`))+
  geom_alluvium(aes(fill=Sex))+
  geom_stratum()+
  scale_color_viridis_c()+ 
  geom_label(stat="stratum", aes(label = paste0(..stratum.., "\n", scales::percent(..count.., accuracy = .1))))+
  scale_x_discrete(limits=c("Gender", "Age Group", "Method of killing"))+
  theme(panel.background = element_blank(), axis.line.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks = element_blank())+
  labs(x="", y="", fill="Region")+
  labs(title="Analysis of 2020 homicide victims") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 0.4
        ))
################################## end of visualisation 6 ######################


