context("util metric")

test_that("util_getComputeTime", {
			expect_equal(class(util_getComputeTime('timeMax')), "character")
			expect_equal(class(util_getComputeTime('timeLeft')), "character")
		})
