

suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("List object has correct names", {
  expect_silent(bfk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500))
  expect_named(bfk, c("value", "elements"))
})


test_that(" Function inputs are wrong", {
  expect_error(knapsack_dynamic("allmighty", 200))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8,], W = -10000))
})

test_that("Function is working correctly.", {
  bfk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))

  bfk <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))

  bfk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))

  bfk <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))

})
