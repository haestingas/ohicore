#' Expand Time Invariant layers
#' 
#' @param DATA (data.frame) data to be expanded.  Ideally should contain 
#'     spatial, time, and layer columns.
#' @param fill (logical) if a layer has some (but not all) data defined by year
#'     should the non-defined data be expanded into?
#' @param by.spatial (logical) should computation be processed for each spatial
#'     partitioning independently? Note: if fill = TRUE, this option will not
#'     affect the output.
#' @param top.level.by (character vector) set of layers to ddply by, for 
#'     if you had a non-casted layer.
#' @param spatial.columns (character vector) list of column names which define 
#'     spatial information. Defaults to c('rgn_id').
#' @param time.columns (character vector) list of column names which define 
#'     time information. Defaults to c('year').
#' @param layer.columns (character vector) list of column names which define 
#'     layer information. Function will return NULL if this is not defined.
#' @return (data.frame) Same shape as input DATA, except without rows where 
#'     time data is NA.  Contains expanded layer values.
#' @export
ExpandTimeInvariant = function (d, fill=T, by.spatial=T,
                                top.level.by = NULL,
                                spatial.columns = c('rgn_id'),
                                time.columns = c('year'),
                                layer.columns = NULL) {

    # Need to define layers, if nothing else
    if (is.null(layer.columns)) {
        return (NULL)
    }

    MasterFun = function (DATA) {

        # Note: if fill=T, by.spatial is irrelevant
        if (fill) {

            # This is where the values which will be expanded reside
            replacement.frame = DATA[is.na(DATA[, time.columns]), ]


            for (COLUMN in layer.columns) {
                # For each layer, find the expansion/replacement values...
                replacement.values = replacement.frame[match(
                    DATA[is.na(DATA[, COLUMN]), spatial.columns],
                    replacement.frame[, spatial.columns]), COLUMN]

                # ... and expand them into DATA
                DATA[is.na(DATA[, COLUMN]), COLUMN] = replacement.values
            }

            return (DATA[!is.na(DATA[, time.columns]), ])

        } else {

            # This same function will be used regardless of whether by.spatial=T|F
            # so let's just define it once
            DoNonFillingExpansion = function (X) {

                time.invariants = as.vector(na.omit(plyr::laply(layer.columns,
                    function (L) {
                        ifelse(!Reduce('|',
                            !is.na(X[, time.columns]) & !is.na(X[, L])), L, NA)
                    })))

                base = X[!is.na(X[, time.columns]),
                    -which(names(X) %in% time.invariants)]

                # -which(SET) is troubling if length(SET) = 0, here's a hacky fix
                if (ncol(base) > 0) {
                    for (TI in time.invariants) {
                        base[,TI] = X[is.na(X[, time.columns]), TI]
                    }
                } else {
                    base = X[!is.na(X[, time.columns]), ]
                }

                return (base)

            }

            if (by.spatial) {
                return (plyr::ddply(DATA, spatial.columns, DoNonFillingExpansion))
            } else {
                return (DoNonFillingExpansion(DATA))
            }
        }
    }

    return (ifelse(is.null(top.level.by),
                   MasterFun(d),
                   plyr::ddply(d, top.level.by, MasterFun)))
}
