sampleEntropy<- function (corrDim.object, do.plot = TRUE, ...) 
{
  if (!inherits(corrDim.object, "corrDim")) {
    stop("corrDim.object should be of class corrDim")
  }
  radius = radius(corrDim.object)
  corr.matrix = corrMatrix(corrDim.object)
  embeddings = embeddingDims(corrDim.object)
  number.embeddings = length(embeddings) - 1
  entropy = matrix(0, nrow = number.embeddings, ncol = length(radius))
  for (i in 1:number.embeddings) {
    entropy[i, ] = log(corr.matrix[i, ]/corr.matrix[i + 1, 
                                                    ])
  }
  dimnames(entropy) = list(head(embeddings, -1), radius)
  sample.entropy = list(sample.entropy = entropy, embedding.dims = head(embeddings, 
                                                                        -1), entr.order = nlOrder(corrDim.object), radius = radius)
  class(sample.entropy) = "sampleEntropy"
  attr(sample.entropy, "time.lag") = attr(corrDim.object, "time.lag")
  attr(sample.entropy, "id") = attr(corrDim.object, "id")
  attr(sample.entropy, "theiler.window") = attr(corrDim.object, 
                                                "theiler.window")
  if (do.plot) {
    plot(sample.entropy, ...)
  }
  return(sample.entropy)
}
