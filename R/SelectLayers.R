#' Select a set of layers.
#' 
#' @param object instance of Layers class
#' @param target {NULL|character} if not NULL, represents the Target (see 
#'     description) for which layers should be selected
#' @param layers {NULL|character} if not NULL, represents the set of layers
#'     to select
#' @param alternate.layer.names {NULL|character} if not NULL, represents the 
#'     names to give to the selected layers
#' @param join.frame {NULL|data.frame} if not NULL, a data.frame to
#'     join to the selected layers, join will be performed on columns which
#'     exist in both the selected layers and the join.frame
#' @param cast {T|F} whether to cast the resulting dataset, or leave it melted, 
#'     defaults to TRUE
#' @param hold.cast {NULL|character} if not NULL, a set of columns which 
#'     should not be casted. Only relevant if cast = T
#' @param expand.time.invariant {T|F} should layers that do not have time 
#'     information be expanded
#' @return data.frame with data from selected layers
#' @export
SelectLayers = function (object, target = NULL, layers = NULL,
                         alternate.layer.names = NULL, join.frame = NULL,
                         cast = T, hold.cast = NULL
                         ) {

    if (!is.null(layers)) {
        focus.data = plyr::rbind.fill(
            object$layer.data[object$GetLayerNames() %in% layers]
        )
    } else if (!is.null(target)) {
        focus.data = plyr::rbind.fill(
            object$layer.data[object$layers.navigation$target == target]
        )
    } else {
        print ('bad R')
        focus.data = plyr::rbind.fill(
            object$layer.data
        )
    }
    
    if (cast) {
    
    
    

        subnav = object$layers.navigation[object$GetLayerNames() %in% layers, ]
        
        cast.spatials = unique(c(
            as.character(subnav$fld_id_num[subnav$fld_id_num != '']),
            as.character(subnav$fld_id_chr[subnav$fld_id_chr != ''])
        ))
        
        
        
        cast.times = unique(
            as.character(subnav$fld_year[subnav$fld_year != ''])
        )
        
        cast.category = unique(
            as.character(subnav$fld_category[subnav$fld_category != ''])
        )
        
        cast.values = unique(c(
            as.character(subnav$fld_val_num[subnav$fld_val_num != '']),
            as.character(subnav$fld_val_chr[subnav$fld_val_chr != ''])
        ))


        left.terms = c(cast.spatials, cast.times)
        right.terms = c('layer_id', cast.category)
        
        cast.formula = as.formula(paste(
            paste(c(left.terms, hold.cast), collapse = ' + '),
            paste(right.terms[which(!right.terms %in% hold.cast)], collapse = ' + '),
            sep = ' ~ '
        ))

        names(focus.data) = gsub(paste(cast.values, collapse='|'), 
                                 'value', 
                                 names(focus.data))
        
        
        focus.data$reduced.value = Reduce('+', 
            replace(focus.data[, which(names(focus.data) == 'value')],
            is.na(focus.data[, which(names(focus.data) == 'value')]),
            0
            ))
        
        recasted.data = reshape2::dcast(focus.data, cast.formula,
            value.var = 'reduced.value')
        
        if (!is.null(alternate.layer.names)) {
            names(recasted.data)[names(recasted.data) %in% layers] = alternate.layer.names
        }
        
        
        if (!is.null(join.frame)) {
        
            join.on = names(recasted.data)[which(names(recasted.data) %in% names(join.frame))]
            return (plyr::join(recasted.data, join.frame, by=join.on))
            
        }
        
        return (recasted.data)

    } else {

        if (!is.null(join.frame)) {
            join.on = names(focus.data)[which(names(focus.data) %in% names(join.frame))]
            return (plyr::join(focus.data, join.frame, by=join.on))
        }

        return (focus.data)
    }
}




