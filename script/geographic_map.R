maptheme <- theme(panel.grid = element_blank())+
  theme(axis.text = element_blank())+
  theme(axis.ticks = element_blank())+
  theme(legend.position = c(0.08,0.25))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color="Blue", size = 24))+
  theme(panel.background = element_rect(fill="#596673"))+
  theme(axis.title = element_blank())+
  theme(legend.background = element_blank())+
  theme(legend.title = element_blank())+
  theme(legend.box.background = element_blank())+
  theme(legend.key = element_blank())+
  theme(legend.text = element_text(color = "white",size = 16, face = "bold"))

mapcoords <- coord_fixed(xlim = c(-150,180),ylim = c(-55,80))

country_shapes <- geom_polygon(aes(x=long,y=lat,group=group),
                               data=map_data("world"),
                               fill="#CECECE",color="#515151",
                               size=0.15)
p <- ggplot()+country_shapes+maptheme+mapcoords

geographic_map <- p +
  guides(color=guide_legend(override.aes = list(size=3)))+
  scale_color_brewer(palette = "Set1")+
  ggtitle("The geographic distribution of rapeseed accessions")




