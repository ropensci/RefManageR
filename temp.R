setMethod("[",
    signature(x = "myclass", i = "..."),
    function (x, i, j, ..., drop = TRUE) 
    {
        stop("need a definition for the method here")
    }
)
