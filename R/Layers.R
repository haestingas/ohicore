
Layers = setRefClass(
    'Layers',
    fields = list(
        layer.data = 'list',
        layers.navigation = 'data.frame',
        layer.id.label = 'character'
    ),
    methods = list(
        initialize = function (file, 
                               filepath.label='filepath',
                               layer.id.label='layer_id',
                               filepath.prefix='') {
            .self$layer.id.label = layer.id.label
            .self$layers.navigation = read.csv(file, header = T)
            .self$layer.data = apply(
                .self$layers.navigation,
                1,
                function (X) {
                    data = read.csv(paste(filepath.prefix, X[filepath.label], sep='/'), header = T)
                    data$layer_id = X[layer.id.label]
                    return (data)
                })
        },
        show = function () {
            print (layers.navigation[-which(names(layers.navigation) == 'filepath')])
        },
        GetLayerNames = function () {
            as.character(layers.navigation[layer.id.label][,1])
        }
    )
)


setGeneric("SelectLayers", SelectLayers)

setMethod("names", 'Layers', 
    function (x) {
        names(x$layers.navigation[
            -which(names(x$layers.navigation) == 'filepath')])
    }
)
