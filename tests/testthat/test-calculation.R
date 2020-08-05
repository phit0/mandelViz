
test_that("does not stop at infinity", {
  expect_equal(
    check_infinity(complex(real = 100, imaginary = Inf),
                   it = 100), 1)
  expect_equal(
    check_infinity(complex(real = 1000, imaginary = 1000),
                   it = 100), 2)
})

test_that("input is correct", {
  expect_error( # real number
    check_infinity(2, it = 100),
  )
  expect_error( # vector
    check_infinity(complex(real = c(1, 1), imaginary = c(1, 1)),
                   it = 100)
  )
  expect_error(
    check_infinity(complex(real = 1, imaginary = 1), it = "A")
  )
  expect_error(
    check_infinity(complex(real = 1, imaginary = 1), it = 2.1)
  )
})
