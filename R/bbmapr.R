
##########################################################################################################
#
# Plot der rgb Praxenkarte
#
##########################################################################################################
#' Plot of Map
#'
#'  #@import rgdal
#' # @import maptools
#' # @import ggplot2
#'
#' @param x dataframew with long and lat
#' @param shapefile data source name (interpretation varies by driver — for some drivers, dsn is a file name, but may also be a folder)
#' @param layer layer name (varies by driver, may be a file name without extension)
#'
#'
#' @author Stefan Bartels, \email{email@biobits.eu}
#'
#' @examples
#' readOGR(dsn=paste(c(GetRDirPath(),"/Data/ShapeGER/"),collapse=""), layer="DEU_adm1",verbose=FALSE)
#'
#'
#'@export
#'
oncosys_map<-function(x,count.breaks=waiver(),shapefile,layer){

  #require("rgdal") # requires sp, will use proj.4 if installed
 # require("maptools")
  #require("ggplot2")

  pats<-x

  # Plot Theme
  theme_opts <- list(theme(panel.grid.minor = element_blank(),
                           panel.grid.major = element_blank(),
                           panel.background = element_blank(),
                           legend.key = element_rect(fill="white"),
                           legend.title = element_text(size = rel(1.5)),
                           legend.text = element_text(size = rel(1)),
                           plot.background = element_rect(fill="white"),
                           panel.border = element_blank(),
                           axis.line = element_blank(),
                           axis.text.x = element_blank(),
                           axis.text.y = element_blank(),
                           axis.ticks = element_blank(),
                           axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           plot.title = element_text(size=22)))


  deu = readOGR(dsn=shapefile, layer=layer,verbose=FALSE)
  deu@data$id=rownames(deu@data)
  deu_robin <- spTransform(deu, CRS("+proj=merc"))
  deu_df_robin <- fortify(deu_robin)
  deu.df = fortify(deu)

  #Praxendaten aufbereiten
  pats_robin_df <- project(cbind(pats$Long,pats$Lat ), "+proj=merc")
  pats_robin_df <- as.data.frame(pats_robin_df)
  names(pats_robin_df) <- c("LONGITUDE", "LATITUDE")
  pats_robin_df$Patienten <- pats$anz_anm

  #PLOT

  #ggplot(deu.df) +
  ggplot(deu_df_robin) +
    aes(long,lat,group=group) +
    geom_polygon(colour= "#4B4B4B", fill="#DEDEDE") +
    geom_path(color="#4D74AB") + theme_opts +
    geom_point(data=pats_robin_df, aes(LONGITUDE ,LATITUDE, group=NULL, fill=NULL, size=Patienten), color="#4D74AB", alpha=I(8/10))+
    coord_equal() +
    scale_fill_manual(values=c("black", "white"), guide="none")+
    scale_size_continuous(range=c(5,24),breaks=count.breaks)


}




