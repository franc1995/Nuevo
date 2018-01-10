test_that('Tests that the files names are properly generated',
  {
    expect_equal(make_filename('2016'), "accident_2016.csv.bz2")
    expect_equal(make_filename(2015), "accident_2015.csv.bz2")
  })
