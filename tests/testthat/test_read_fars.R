test_that('Tests that the files are read properly',
  {
    ## Read the file
    data <- fars_read(system.file('data', make_filename(2013), package = 'fars'))

    ## Test
    expect_that(data, is_a('data.frame'))
    expect_that(nrow(data), equals(30202))
    expect_that(ncol(data), equals(50))
    expect_that(unique(data$YEAR), equals(2013))
  })
