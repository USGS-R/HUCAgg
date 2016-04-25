#' Initialize HUC Region Files
#' 
#' Builds regional WBD files from a national shapefile. The shapefile can be found here:
#' ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/WBD/
#' 
#' @param WBDPath The path to the WBD shapefile
#' @param regionsPath The path where the regional subset rda files should go
#' @return A list of regions and sub-processing units.
#' @author David Blodgett \email{dblodgett@@usgs.gov}
#' @importFrom maptools readShapePoly
#' @importFrom sp CRS
#' @export
#' @examples
#' \dontrun{
#' WBDPath<-"/data/WBDHU12.shp"
#' regionsPath<-"regions"
#' regions<-init_regions(WBDPath, regionsPath)
#' }
init_regions<-function(WBDPath,regionsPath) {
  regions<-list(newEngland=c('01'), midAtlantic=c('02'), southAtlanticGolf=c('03'),
                greatLakes=c('04'),  mississippi=c('05','06','07','10','11','08'),
                sourisRedRainy=c('09'), texasGolf=c('12'), rioGrande=c('13'),
                colorado=c('14','15'), greatBasin=c('16'), 
                pacificNorthwest=c('1701','1702','1703','1704','1705','1706',
                                   '1707','1709','1710','1711','1712','1708'),
                california=c('18'))
  if(!dir.exists(regionsPath)) {
    print('Reading HUC shapefile, this may take a while.')
    hucPoly<-readShapePoly(WBDPath,proj4string= CRS('+init=epsg:4269'))
    i <- sapply(hucPoly@data, is.factor)
    hucPoly@data[i] <- lapply(hucPoly@data[i], as.character)
    dir.create(regionsPath)
    for(region in names(regions)) {
      print(regions[region])
      subhucList<-c()
      for(huc02 in regions[region][[1]]) { 
        for(huc in hucPoly@data$HUC) {
          if(grepl(paste0('^',huc02,'.*'),huc)) { 
            subhucList<-c(subhucList,huc) 
          }
        } 
      }
      subhucPoly<-subset(hucPoly,hucPoly@data$HUC %in% as.character(subhucList))
      save(subhucPoly, file=file.path('regions',paste0(region,'.rda')))
    }
  }
  return(regions)
}