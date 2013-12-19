rm(list=ls())


source('../R/SelectLayers.R')
source('../R/TransformSpatialScheme.R')

source('../R/Layers.R')
source('../R/SpatialSchemes.R')



source('../R/CalculateStatusComponent.R')
source('../R/CalculatePressuresComponent.R')
source('../R/CalculateResilienceComponent.R')
source('../R/CalculateSubgoal.R')

source('../R/ExpandTimeInvariant.R')



layers.list <- Layers('data/layers.Global2013.www2013.csv',
                      filepath.label='filename',
                      filepath.prefix='data/layers.Global2013.www2013',
                      layer.id.label='layer')




NP.data = SelectLayers(
    layers.list,
    layers=c('rnky_np_harvest_relative','rnk_np_sustainability_score',
             'rnk_np_weights_combo', 'rn_fis_status'),
    cast=T,
    alternate.layer.names = c('fis_status','S','w','H'),
    hold.cast=c('product'))







tmp = ExpandTimeInvariant(NP.data, spatial.columns = c('rgn_id'),
    layer.columns = c('fis_status','S','w','H'),
    top.level.by = c('product'), fill = T)
















NP = function(layers, 
              status_year=2008, 
              trend_years = list('corals'=2003:2007,'ornamentals'=2003:2007,'shells'=2003:2007,
                                 'fish_oil'=2004:2008,'seaweeds'=2004:2008,'sponges'=2004:2008)){
  # 2013: NP(layers, status_year=2009, trend_years = list('corals'=2004:2008,'ornamentals'=2004:2008,'shells'=2004:2008, 'fish_oil'=2005:2009,'seaweeds'=2005:2009,'sponges'=2005:2009))
  # 2012: NP(layers, status_year=2008, trend_years = list('corals'=2003:2007,'ornamentals'=2003:2007,'shells'=2003:2007, 'fish_oil'=2004:2008,'seaweeds'=2004:2008,'sponges'=2004:2008))
  # status_year=2009; trend_years = list('corals'=2004:2008,'ornamentals'=2004:2008,'shells'=2004:2008, 'fish_oil'=2005:2009,'seaweeds'=2005:2009,'sponges'=2005:2009)
    
  # layers
  lyrs = list('rky' = c('rnky_np_harvest_relative'    = 'H'),
              'rk'  = c('rnk_np_sustainability_score' = 'S',
                        'rnk_np_weights_combo'        = 'w'),
              'r'   = c('rn_fis_status'               = 'fis_status'))
  lyr_names = sub('^\\w*\\.', '', names(unlist(lyrs))) 
  
  # cast data
  D = SelectLayersData(layers, layers=lyr_names)
  rky = rename(dcast(D, id_num + category + year ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['rky']]))),
               c('id_num'='region_id', 'category'='product', lyrs[['rky']]))
  rk  = rename(dcast(D, id_num + category ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['rk']]))),
               c('id_num'='region_id', 'category'='product', lyrs[['rk']]))
  r   = rename(dcast(D, id_num ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['r']]))),
               c('id_num'='region_id', lyrs[['r']]))
  
  # turn rn_fis_status to S for fish_oil
  r$product = 'fish_oil'
  rk = merge(rk, r, all.x=T)
  rk$S[rk$product=='fish_oil'] = rk$fis_status[rk$product=='fish_oil']
  rk = rk[,names(rk)!='fis_status']
  
  # merge H with S & w
  rky = merge(rky, rk, all.x=T)
  
  
  # get status across products, per region and year
  rky$w = ifelse(is.na(rky$w), 0, rky$w)
  rky = na.omit(rky)
  ry = ddply(rky, .(region_id, year), summarize,
             status = sum(w * H * S) / sum(w) * 100); head(ry)
  r.status = subset(ry, year==status_year, c(region_id,status))
  
  # get trend per product based on product-specific trend_years
  rk.trend = rename(ddply(rky, .(region_id, product), function(x){
    lm(H * S ~ year, x[x$year %in% trend_years[[as.character(x$product[1])]],])$coefficients[['year']] * 5
  }), c('V1'='trend.k')); head(rk.trend)  
  
  # summarize trend per region
  rk.trend.w = na.omit(merge(rk.trend, rk)); summary(rk.trend.w)
  r.trend = ddply(rk.trend.w, .(region_id), summarize,
                  trend = min(1, max(-1, sum(w * trend.k) / sum(w))))
  
  # return scores
  s.status = cbind(rename(r.status, c('status'='score')), data.frame('dimension'='status'))
  s.trend  = cbind(rename(r.trend , c('trend' ='score')), data.frame('dimension'='trend'))
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='NP'))
  return(scores)  
}









