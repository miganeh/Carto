##Package##library(svglite Arial Black)
package.name <- c("readxl","openxlsx","ggplot2","dplyr","haven","rlang","tidyr","dtplyr","purrr","ggiraph","ggrepel","geojsonio","broom","svglite","grDevices")

get.packages <- function(packages, Base_R_Best_R = F){
  if(Base_R_Best_R){
    print("No packages required!")
  }
  else{
    for(i in seq.int(length(packages))){
      if(!require(packages[i], character.only = T)){
        renv::install(packages[i])
      }
      library(packages[i],character.only=TRUE)
    }
  }
}
#example
get.packages(package.name)
options(OutDec= ",")

######################################### carto ############################################
Carte_SSR47 <- read_excel("Carte SSR47_16.xlsx")
regions <- read.csv("departements-region.csv")
coordonne_Dep <- read_excel("coordonne_Dep.xlsx")
coordonne_Dep<-coordonne_Dep %>% filter(Nom_Dep %in% c("Dordogne","Gironde","Landes","Lot-et-Garonne","Pyrenees-Atlantiques"))



# Map france dep
dep <- geojson_read("a-dep2021.json",  what = "sp")
dep <- dep[ substr(dep@data$dep,1,2)  %in% c("16") , ]
spdf_fortified <- tidy(dep, region = "dep")
spdf_fortified$dep_name<-recode_factor(spdf_fortified$id,"16"="Charente")


commu<-geojson_read("a-com2021.json",what = "sp")
commu1 <- commu[ substr(commu@data$codgeo,1,2)  %in% c("16") , ]
#commu1 <- tidy(commu1, region = "dep")
p2<-ggplot(spdf_fortified, aes(long, lat, group = group))+
  geom_polygon_interactive(color = "white",size=.3)+coord_map("polyconic")

windowsFonts(JP1  = windowsFont("Arial black"))
windowsFonts(`Quicksand` = windowsFont("Quicksand"))
.labs <- rownames(df)
print(ggplot() +
        #geom_polygon_interactive(data = Region1, aes( x = long, y = lat, group = group),color = "black",size=0.5) +
        geom_polygon_interactive(data = commu1, aes( x = long, y = lat, group = group),fill=NA,color="grey70") +
        geom_polygon_interactive(data = spdf_fortified, aes( x = long, y = lat, group = group),fill=NA,color = "black",size=0.8) +
        
        geom_point(data=Carte_SSR47,aes(as.numeric(Long),as.numeric(lat),group = as.factor(COMMUNE),shape=as.factor(type),color=as.factor(type),size=as.factor(type)))+
        scale_shape_manual(values = c(17,16)) +
        scale_color_manual(values = c("#E7B800","#FC4E07"))+
        scale_size_manual(values=c(7,3))+
        
        # geom_text(data = Carte_SSR47 ,aes(as.numeric(Long),as.numeric(lat),label = as.factor(COMMUNE)),
        #                     color = 'black', family="JP1", fontface="bold.italic", size  = 3)+
        geom_text_repel_interactive(data = Carte_SSR47, aes(as.numeric(Long),as.numeric(lat),label = as.factor(COMMUNE)), color = 'black', family="Quicksand", fontface="bold.italic",size = 3)+
        
        coord_map("polyconic")+
        theme_minimal(base_family = "Quicksand")+
        theme(panel.grid= element_blank(),
              # axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text = element_blank(),
              strip.text.x = element_text(size = 9),
              legend.title=element_blank(),
              legend.text=element_text(size=9,family = "Quicksand"),
              plot.caption = element_text(face = "italic", size = rel(0.5),family = "Quicksand"))
      )

