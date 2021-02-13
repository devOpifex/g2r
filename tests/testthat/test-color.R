test_that("Gauge colors", {
  g <- g2(cars, asp(speed, dist)) %>%
    fig_point(asp(color = dist))

  col <- gauge_color_viridis(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#FDE725")

  col <- gauge_color_plasma(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#F0F921")

  col <- gauge_color_inferno(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#FCFFA4")

  col <- gauge_color_magma(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#FCFDBF")

  col <- gauge_color_accent(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#7FC97F")

  col <- gauge_color_dark2(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#1B9E77")

  col <- gauge_color_paired(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#A6CEE3")

  col <- gauge_color_pastel1(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#FBB4AE")

  col <- gauge_color_pastel2(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#B3E2CD")

  col <- gauge_color_set1(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#E41A1C")

  col <- gauge_color_set2(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#66C2A5")

  col <- gauge_color_set3(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#8DD3C7")

  col <- gauge_color_neon(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#80ffdb")

  col <- gauge_color_std(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#f94144")

  col <- gauge_color_pink(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#fae0e4")

  col <- gauge_color_orange(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#ffb600")

  col <- gauge_color_lime(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#ffff3f")

  col <- gauge_color_blue(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#BAE7FF")

  col <- gauge_color_aw(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#F7DC05")

  col <- gauge_color_g2rq(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#731dd8")

  col <- gauge_color_g2rc(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#eedaff")  

  col <- gauge_color_g2rd(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#432892")

  col <- gauge_color_brbg(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#8c510a")

  col <- gauge_color_piyg(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#c51b7d")

  col <- gauge_color_prgn(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#762a83")

  col <- gauge_color_puor(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#b35806")

  col <- gauge_color_rdbu(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#b2182b")

  col <- gauge_color_rdgy(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#b2182b")

  col <- gauge_color_rdylbu(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#d73027")

  col <- gauge_color_rdylgn(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#d73027")

  col <- gauge_color_spectral(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#d53e4f")

  col <- gauge_color_flashy(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#70d6ff")

  col <- gauge_color_red(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#e4b1ab")

  col <- gauge_color_ryb(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#8f2d56")

  col <- gauge_color_bwg(g)
  expect_equal(col$x$views[[1]]$color[[2]][1], "#05668d")

})
