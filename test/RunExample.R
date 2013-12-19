

rm(list=ls())


source('../R/SelectLayers.R')
source('../R/TransformSpatialScheme.R')

source('../R/Layers.R')
source('../R/SpatialSchemes.R')



source('../R/CalculateStatusComponent.R')
source('../R/CalculatePressuresComponent.R')
source('../R/CalculateResilienceComponent.R')
source('../R/CalculateSubgoal.R')


#source('../R/Halpern2012Data.R')




layers.list <- Layers('data/layers.Global2013.www2013.csv',
                      filepath.label='filename',
                      filepath.prefix='data/layers.Global2013.www2013',
                      layer.id.label='layer')



ranks = data.frame(
    habitat=c('coral', 'mangrove', 'saltmarsh', 'seagrass', 'seaice_shoreline'),
    rank=c(4,4,3,1,4)
)


status.data = SelectLayers(
    layers.list,
    layers=c('rnk_hab_extent', 'rnk_hab_health'),
    alternate.layer.names=c('extent', 'health'),
    cast=T,
    hold.cast=c('habitat'),
    join.frame=ranks)




F.CP = plyr::splat(function (habitat, extent, health, rank, ...) {
    
    extent[match('mangrove', habitat)] = Reduce('+', extent[match(c('mangrove_inland1km', 'mangrove_offshore1km'), habitat)])
    extent = extent * as.logical(rank)

    wm = stats::weighted.mean((health*rank), extent)
    
    c('CP' = ((100 * wm) / max(as.logical(extent) * rank)))


})


F.NP = plyr::splat(function () {
    
})



tmp.data = replace(status.data, is.na(status.data), 0)

tmp <- plyr::ddply(
    tmp.data,
    'rgn_id',
    F
)



print (tmp)


