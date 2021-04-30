library(MovementSequenceMapping)
context("generate_barcode")
test_that("example input gives expected output for generate_barcode", {
  bts = c(0, 5, 10, 30)
  f = 4
  # generate random test data:
  set.seed(300)
  N = 400
  bouts_values = rep(1, N)
  bouts_lengths = round(100*rgamma(N, shape = 1, scale = 0.5))
  LR = bouts_lengths / max(bouts_lengths)
  S = matrix(0,N,5)
  S[which(LR >= 0.2), 1] = 1 # nonwear
  S[which(LR >= 0.05), 2] = 1 # sedentary
  S[which(LR >= 0.05 & LR < 0.5), 3] = 1 # light
  S[which(LR >= 0 & LR < 0.3), 4] = 1 # moderate
  S[which(LR >= 0 & LR < 0.1), 5] = 1 # vigorous
  ranone = function(x) sample(which(x == 1),1)
  bouts_values = apply(S,1,ranone)
  # feed data to generate_barcode:
  test = generate_sequence_map(bouts_values, bouts_lengths, f, bts)
  expect_equal(sum(test, na.rm = T),2270)
  expect_equal(length(test),366)
})