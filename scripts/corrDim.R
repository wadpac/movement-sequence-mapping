corrDim<- function (time.series, min.embedding.dim = 2, max.embedding.dim = 5, 
                    time.lag = 1, min.radius, max.radius, corr.order = 2, n.points.radius = 5, 
                    theiler.window = 100, do.plot = TRUE, number.boxes = NULL, 
                    ...) 
{
  if (is.null(number.boxes)) 
    number.boxes = estimateNumberBoxes(time.series, min.radius)
  takensDimMin = buildTakens(time.series = time.series, embedding.dim = min.embedding.dim, 
                             time.lag = time.lag)
  numberTakens = nrow(takensDimMin)
  lenTimeSeries = length(time.series)
  number.embeddings = max.embedding.dim - min.embedding.dim + 
    1
  corr.matrix = matrix(0, nrow = number.embeddings, ncol = n.points.radius)
  log.radius = seq(log10(max.radius), log10(min.radius), len = n.points.radius)
  radius = 10^log.radius
  if (corr.order == 2) {
    sol = .C("corrDim", timeSeries = as.double(time.series), 
             lenTimeSeries = as.integer(length(time.series)), 
             takensDimMin = as.double(takensDimMin), tau = as.integer(time.lag), 
             numberTakens = as.integer(numberTakens), minEmbeddingD = as.integer(min.embedding.dim), 
             maxEmbeddingD = as.integer(max.embedding.dim), eps = as.double(radius), 
             numberEps = as.integer(n.points.radius), numberBoxes = as.integer(number.boxes), 
             tdist = as.integer(theiler.window), corrMatrix = as.double(corr.matrix), 
             PACKAGE = "nonlinearTseries")
  }
  else {
    sol = .C("generalizedCorrDim", time.series = as.double(time.series), 
             lenTimeSeries = as.integer(lenTimeSeries), takensDimMin = as.double(takensDimMin), 
             tau = as.integer(time.lag), numberTakens = as.integer(numberTakens), 
             minEmbeddingD = as.integer(min.embedding.dim), maxEmbeddingD = as.integer(max.embedding.dim), 
             q = as.integer(corr.order), eps = as.double(radius), 
             numberEps = as.integer(n.points.radius), numberBoxes = as.integer(number.boxes), 
             tdist = as.integer(theiler.window), corrMatrix = as.double(corr.matrix), 
             PACKAGE = "nonlinearTseries")
  }
  corr.matrix = matrix(sol$corrMatrix, nrow = number.embeddings, 
                       ncol = n.points.radius)
  dimnames(corr.matrix) = list(min.embedding.dim:max.embedding.dim, 
                               radius)
  wh = which(corr.matrix == 0, arr.ind = TRUE)
  wh = unique(wh[, "col"])
  if (length(wh > 0)) {
    corr.matrix = corr.matrix[, -wh, drop = FALSE]
    radius = radius[-wh]
  }
  corr.dim = list(corr.matrix = corr.matrix, embedding.dims = min.embedding.dim:max.embedding.dim, 
                  radius = radius, corr.order = corr.order)
  class(corr.dim) = "corrDim"
  id = deparse(substitute(time.series))
  attr(corr.dim, "time.lag") = time.lag
  attr(corr.dim, "id") = id
  attr(corr.dim, "theiler.window") = theiler.window
  if (do.plot) {
    plot(corr.dim, ...)
  }
  return(corr.dim)
}
