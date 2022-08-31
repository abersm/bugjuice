# 1 color -----------------------------------------------------------------
pal_1 <- list(
  green = list(
    green = c("#009872", "#00A087", "#22908D", "#107F80"),
    kelly = c("#04A66F", "#1CAC78", "#2CA25F", "#31A354"),
    forest = c("#008F51", "#20854E", "#238B45"),
    sea = list(
      light = c("#00DFB7", "#B2E2E2", "#CCEBC5", "#BAE4B3", "#99D8C9"),
      medium = c("#66C2A4", "#6FC7CF"),
      dark = c("#76B7B2", "#00B7C3")
    )
  ),
  blue = list(
    blue = c("#0072B5", "#0280C2", "#0571B0", "#2B8CBE", "#3498DB", "#377EB8", "#5B9AD2"),
    faded = c("#B3CDE3", "#95BCE3", "#7AAAD4", "#74A9CF", "#80B1D3"),
    royal = c("#2A97FC", "#0073C2", "#1874CD", "#296BC0"),
    aqua = c("#03A9F4", "#66CCFE", "#1CC5FE", "#00AAE2", "#00A1D5", "#009ACD", "#54AAD5"),
    navy = c("#2151A1", "#08519C", "#185597", "#1B6393", "#36648B", "#104E8B", "#3C5488", "#003C67", "#374E55"),
    steel = "#4F6F80"
  ),
  teal = list(
    teal = c("#00C5CD", "#139AB2", "#4DBBD5", "#70AFC7", "#028798", "#00688B", "#005E7A")
  ),
  purple = list(
    purple = c("#8DA0CB", "#9E9AC8", "#A48AD3"),
    light = c("#ECE7F2", "#CBC9E2", "#BCBDDC", "#756BB1", "#6761A8"),
    medium = c("#7F579F", "#6A51A3", "#6A00A8", "#663399"),
    dark = c("#483D8B", "#5D478B", "#54278F", "#40007F", "#3B0F70"),
    plum = c("#CB80AC", "#C968B1", "#B12A90", "#8C2981", "#8B0A50", "#8B475D", "#8B3A62")
  ),
  magenta = list(
    magenta = c("#EE1E7A", "#DA3978", "#DE4968"),
    rich = "#FF0066",
    dark = "#C71585"
  ),
  maroon = list(
    maroon = c("#8B2323", "#B40F20", "#B24745", "#B22222", "#A50F15", "#9F1C1F"),
    light = c("#E16462", "#B5695B"),
    faded = "#8B3A3A"
  ),
  red = list(
    red = c("#CD0000", "#E5550D"),
    dark = c("#E70028", "#E41A1C", "#DE2D26", "#DC0000", "#D72420", "#CB272A", "#CA0020", "#C91C03", "#BD2623"),
    brick = c("#E74C3C", "#E64B35", "#D6604D", "#CD534C", "#BC3C29")
  ),
  pink = list(
    pink = c("#FBB4AE", "#FCAE91"),
    full = "#CD3278"
  ),
  peach = list(
    peach = c("#FEE0D2", "#FBA27D", "#FC9272", "#FB7D80", "#FB8072", "#F7614F")
  ),
  orange = list(
    light = c("#FDB462", "#FCA636"),
    dark = c("#D65B27", "#D75725"),
    faded = "#FC8D62",
    rich = "#FF7F00",
    burnt = "#BB5504"
  ),
  yellow = list(
    light = c("#FDDB6D", "#FCD351"),
    gold = c("#EFA83A", "#FFC125", "#FFCD00", "#FFD500"),
    mustard = c("#F1C40F", "#EFC000", "#E7B800")
  ),
  brown = list(
    brown = "#B69D71"
  ),
  grey = list(
    light = c("#E0E0E0", "#E5E3E6", "#ECECEC", "#EFEEF0", "#F0F0F0"),
    medium = c("#969696", "#B0B0B0", "#B7B7B7", "#BABABA", "#BDBDBD", "#BFBEC1", "#C3C2C4"),
    dark = c("#4D4D4D", "#525252", "#5D5D5D", "#606161", "#636363", "#727172", "#737373", "#868686", "#909090")
  ),
  black = list(
    black = "#1C2026",
    rich = "#000000",
    charcoal = c("#3B3B3B", "#333333", "#2A2D34")
  ),
  white = list(
    white = "#FFFFFF",
    off_white = "#F7F7F7"
  )
)

# 2 colors ----------------------------------------------------------------
pal_2 <- list(
  aqua_purple = c("#14B3EC", "#9981C8"),
  aqua_purple_faded = c("#56B9E0", "#A284BA"),
  aqua_purple_light = c("#56B9E0", "#C582B2"),
  aqua_magenta_light = c("#00A1D5", "#EE1E7A"),
  aqua_magenta_dark = c("#54AAD5", "#DA3978"),
  blue_red1 = c("#0072B5", "#BC3C29"),
  blue_red2 = c("#5B9AD2", "#D75725"),
  blue_red4 = c("#377EB8", "#E41A1C"),
  blue_red5 = c("#00A1D5", "#E41A1C"),
  blue_red_light = c("#2A97FC", "#FF3E42"),
  black_magenta_rich = c("#000100", "#EE1E7A"),
  black_magenta_faded = c("#333333", "#EE1E7A"),
  black_red_rich1 = c("#000100", "#BC3C29"),
  black_red_rich2 = c("#000100", "#BD2623"),
  black_red_faded1 = c("#2A2D34", "#BC3C29"),
  black_red_faded2 = c("#2A2D34", "#BD2623"),
  black_red_faded3 = c("#2A2D34", "#CB272A"),
  blue_yellow_light = c("#0073C2", "#FDDB6D"),
  blue_yellow_dark = c("#0073C2", "#EFC000"),
  navy_yellow_light = c("#003C67", "#FDDB6D"),
  navy_yellow_dark = c("#003C67", "#EFC000"),
  navy_green1 = c("#0072B5", "#20854E"),
  navy_green2 = c("#003C67", "#1CAC78"),
  navy_green_light = c("#2151A1", "#1CAC78"),
  navy_green_dark = c("#323665", "#01948E"),
  navy_light_blue = c("#323665", "#A9C4E2"),
  navy_orange1 = c("#323665", "#FDA963"),
  navy_pink1 = c("#323665", "#EC687D"),
  navy_purple1 = c("#4D5F8E", "#C582B2"),
  navy_red_dark1 = c("#003C67", "#BC3C29"),
  navy_red_dark2 = c("#003C67", "#D75725"),
  navy_red1 = c("#3C5488", "#BC3C29"),
  navy_red2 = c("#3C5488", "#D75725"),
  nejm_dark = c("#5D86C2", "#C8543B"),
  nejm_light = c("#D8DFF0", "#D7BFBF")
)

# 3 colors ----------------------------------------------------------------
pal_3 <- list(
  aqua_magenta_black_rich = c("#00A1D5", "#EE1E7A", "#333333"),
  aqua_magenta_black_faded = c("#54AAD5", "#DA3978", "#333333"),
  aqua_red_black_rich = c("#00AAE2", "#E70028", "#333333"),
  aqua_red_black_faded = c("#00A1D5", "#D75725", "#2A2D34"),
  aqua_red_black_rich = c("#00AAE2", "#CB272A", "#2A2D34"),
  aqua_red_black_faded = c("#00A1D5", "#BC3C29", "#2A2D34"),
  blue_red_black = c("#0072B5", "#BC3C29", "#2A2D34"),
  purple_red_black_light1 = c("#7AAAD4", "#EF4234", "#333333"),
  purple_red_black_rich1 = c("#4657A2", "#CB272A", "#000100"),
  purple_red_black_rich2 = c("#4657A2", "#BC3C29", "#000100"),
  purple_red_black_dark1 = c("#4657A2", "#BD2623", "#2A2D34"),
  purple_red_black_dark2 = c("#4657A2", "#BC3C29", "#2A2D34"),
  teal_red_black = c("#028798", "#EF4234", "#333333"),
  aqua_purple_grey = c("#56B9E0", "#A284BA", "#717375"),
  blue_orange_black1 = c("#3182BD", "#E98A15", "#000000"),
  blue_orange_black2 = c("#3AAAE1", "#E98A15", "#000000"),
  blue_yellow_grey = c("#0073C2", "#EFC000", "#868686"),
  black_navy_yellow = c("#323232", "#1B6393", "#FCD351"),
  blue_green_yellow = c("#0280C2", "#00DFB7", "#FFD500"),
  green_blue_grey = c("#1C9099", "#A6BDDB", "#ECE2F0"),
  green_blue_navy = c("#028A8A", "#67A9CF", "#0570B0"),
  green_blue_yellow = c("#00DFB7", "#0280C2", "#FFD500"),
  grey_maroon_green = c("#BDBDBD", "#9E3E4F", "#0D94A3"),
  navy_orange_green = c("#323665", "#FDA963", "#01948E"),
  navy_orange_teal = c("#323665", "#FDA963", "#01A7B3"),
  navy_teal_green = c("#3C5488", "#4DBBD5", "#00A087"),
  navy_teal_yellow = c("#003C67", "#00C5CD", "#FFCD00"),
  navy_green_yellow = c("#440154", "#22908D", "#FDE725"),
  orange_blue_purple = c("#FEC299", "#9DE5F0", "#F2B7E0"),
  pastel_green_salmon_purple = c("#66C2A5", "#FC8D62", "#8DA0CB"),
  pastel_red_blue_green = c("#FBB4AE", "#B3CDE3", "#CCEBC5"),
  pastel_red_blue_orange = c("#FB8072", "#80B1D3", "#FDB462"),
  purple_navy_aqua = c("#9C27B0", "#3F51B5", "#03A9F4"),
  red_blue_green_dark = c("#BC3C29", "#0072B5", "#20854E"),
  red_blue_green_light = c("#F7614F", "#377EB8", "#76B7B2"),
  red_blue_grey = c("#D75725", "#5B9AD2", "#BDBDBD"),
  red_blue_purple1 = c("#D75725", "#5B9AD2", "#7F579F"),
  red_blue_purple2 = c("#D75725", "#5B9AD2", "#C6687C"),
  red_blue_purple3 = c("#D75725", "#5B9AD2", "#C968B1"),
  red_green_navy = c("#D75725", "#00A087", "#3C5488"),
  red_teal_navy = c("#D75725", "#4DBBD5", "#3C5488"),
  red_teal_green = c("#D75725", "#4DBBD5", "#00A087"),
  red_teal_purple = c("#D75725", "#6CB7DA", "#C6687C"),
  red_yellow_blue1 = c("#BC3C29", "#EFC000", "#0072B5"),
  red_yellow_blue2 = c("#BC3C29", "#E7B800", "#2E9FDF"),
  red_yellow_blue3 = c("#E74C3C", "#F1C40F", "#3498DB"),
  red_yellow_teal_dark = c("#B40F20", "#FFC125", "#00B7C3"),
  red_yellow_teal_light = c("#CC333F", "#EDC951", "#00A0B0"),
  nejm_dark = c("#5D86C2", "#C8543B", "#7D5FA7"),
  nejm_light = c("#D8DFF0", "#D7BFBF", "#E4DBEC"),
  viridis = c("#440154", "#22908D", "#FDE725")
)

# 4 colors ----------------------------------------------------------------
pal_4 <- list(
  aqua_red_black_grey1 = c("#00A1D5", "#BC3C29", "#2A2D34", "#B3B3B3"),
  aqua_red_black_grey2 = c("#00A1D5", "#D75725", "#333333", "#868686"),
  blue_red_black_grey1 = c("#0072B5", "#BC3C29", "#2A2D34", "#868686"),
  blue_red_black_grey2 = c("#0072B5", "#BC3C29", "#2A2D34", "#B3B3B3"),
  blue_red_black_grey3 = c("#0072B5", "#E64B35", "#333333", "#868686"),
  blue_red_black_teal = c("#7AAAD4", "#EF4234", "#333333", "#028798"),
  blue_red_green_black = c("#185597", "#D72420", "#008F51", "#000000"),
  green_blue_magenta_navy = c("#01948E", "#A9C4E2", "#E23560", "#323665"),
  magenta_navy_orange_green = c("#E23560", "#323665", "#FDA963", "#01A7B3"),
  navy_orange_teal_red = c("#374E55", "#DF8F44", "#00A1D5", "#B24745"),
  navy_red_yellow_green = c("#20558A", "#D23342", "#FFC10A", "#17805F"),
  red_teal_blue_purple = c("#D75725", "#6CB7DA", "#5B9AD2", "#C6687C"),
  red_teal_green_navy = c("#D75725", "#4DBBD5", "#00A087", "#3C5488"),
  teal_brown_red_yellow = c("#00A0B0", "#6A4A3C", "#CC333F", "#EDC951")
)

# 5 colors ----------------------------------------------------------------
pal_5 <- list(
  pal1 = c("#2A2D34", "#00A1D5", "#D75725", "#6761A8", "#009872"),
  pal2 = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#DC0000"),
  pal3 = c("#0073C2", "#EFC000", "#868686", "#CD534C", "#7AA6DC"),
  pal4 = c("#7AAAD4", "#EF4234", "#028798", "#DDCFCD", "#333333"),
  pal5 = c("#000000", "#FBC181", "#9F1C1F", "#139AB2", "#BDBDBD"),
  pal6 = c("#2E58A4", "#70AFC7", "#4F5357", "#B69D71", "#E3DED4"),
  pal7 = c("#3853A3", "#95BCE3", "#04A66F", "#EFA83A", "#D65B27"),
  pal8 = c("#01948E", "#A9C4E2", "#E23560", "#323665", "#FDA963"),
  pal9 = c("#00A0B0", "#6A4A3C", "#CC333F", "#EDC951", "#EB6841"),
  pal10 = c("#016C59", "#1C9099", "#67A9CF", "#BDC9E1", "#F6EFF7"),
  prism_pastel = c("#A48AD3", "#1CC5FE", "#6FC7CF", "#FBA27D", "#FB7D80")
)

# 6 colors ----------------------------------------------------------------
pal_6 <- list(
  pal1 = c("#2A2D34", "#00A1D5", "#D75725", "#6761A8", "#009872", "#EFC000"),
  pal2 = c("#3853A3", "#95BCE3", "#04A66F", "#CB80AC", "#EFA83A", "#D65B27"),
  pal3 = c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377"),
  pal4 = c("#EE7733", "#0077BB", "#33BBEE", "#EE3377", "#CC3311", "#009988"),
  pal5 = c("#01948E", "#A9C4E2", "#E23560", "#323665", "#FDA963", "#01A7B3"),
  magma = c("#000004", "#3B0F70", "#8C2981", "#DE4968", "#FE9F6D", "#FCFDBF"),
  plasma = c("#0D0887", "#6A00A8", "#B12A90", "#E16462", "#FCA636", "#F0F921"),
  prism = c("#000000", "#FF0066", "#107F80", "#40007F", "#AA66FF", "#66CCFE")
)

# 7 colors ----------------------------------------------------------------
pal_7 <- list(
  pal1 = c("#2A2D34", "#00A1D5", "#D75725", "#6761A8", "#009872", "#EFC000", "#003C67"),
  pal2 = c("#3853A3", "#95BCE3", "#04A66F", "#CB80AC", "#EFA83A", "#D65B27", "#606161"),
  pal3 = c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377", "#BBBBBB"),
  pal4 = c("#EE7733", "#0077BB", "#33BBEE", "#EE3377", "#CC3311", "#009988", "#BBBBBB"),
  pal5 = c("#01948E", "#A9C4E2", "#E23560", "#323665", "#FDA963", "#01A7B3", "#EC687D")
)

# 8 colors ----------------------------------------------------------------
pal_8 <- list(
  okabeito = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
)

# Heatmap palettes --------------------------------------------------------
pal_heatmap <- list(
  purple_yellow = c("#B200B2", "#FFFF66"),
  blue_red = list(
    light = c("#4500AC", "#F7F7F7", "#D60C00"),
    medium1 = c("#2166AC", "#F7F7F7", "#B2182B"),
    medium2 = c("#0571B0", "#F7F7F7", "#CA0020"),
    dark = c("#542788", "#F7F7F7", "#B35806"),
    dark_flat1 = c("#0571B0", "#F7F7F7", "#B84941"),
    dark_flat2 = c("#542788", "#F7F7F7", "#B84941")
  ),
  black_red = c("#404040", "#F7F7F7", "#CA0020")
)

# Brewer palettes ---------------------------------------------------------
## Darkest to lightest color for each palette
pal_brewer <- list(
  purple_green = c("#40004B", "#762A83", "#9970AB", "#C2A5CF", "#E7D4E8", "#F7F7F7", "#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837", "#00441B"),
  purple_orange = c("#7F3B08", "#B35806", "#E08214", "#FDB863", "#FEE0B6", "#F7F7F7", "#D8DAEB", "#B2ABD2", "#8073AC", "#542788", "#2D004B"),
  red_blue = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"),
  red_grey = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#E0E0E0", "#BABABA", "#878787", "#4D4D4D", "#1A1A1A"),
  red_purple = c("#FFF7F3", "#FDE0DD", "#FCC5C0", "#FA9FB5", "#F768A1", "#DD3497", "#AE017E", "#7A0177", "#49006A"),
  purple_red = c("#F7F4F9", "#E7E1EF", "#D4B9DA", "#C994C7", "#DF65B0", "#E7298A", "#CE1256", "#980043", "#67001F"),
  purple_blue = c("#FFF7FB", "#ECE7F2", "#D0D1E6", "#A6BDDB", "#74A9CF", "#3690C0", "#0570B0", "#045A8D", "#023858"),
  blue_purple = c("#F7FCFD", "#E0ECF4", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8C6BB1", "#88419D", "#810F7C", "#4D004B"),
  orange_red = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "#B30000", "#7F0000"),
  blue_green = c("#F7FCFD", "#E5F5F9", "#CCECE6", "#99D8C9", "#66C2A4", "#41AE76", "#238B45", "#006D2C", "#00441B"),
  green_blue = c("#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5", "#7BCCC4", "#4EB3D3", "#2B8CBE", "#0868AC", "#084081"),
  red_yellow_blue = c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090", "#FFFFBF", "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"),
  red_yellow_green = c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#D9EF8B", "#A6D96A", "#66BD63", "#1A9850", "#006837"),
  purple_blue_green = c("#FFF7FB", "#ECE2F0", "#D0D1E6", "#A6BDDB", "#67A9CF", "#3690C0", "#02818A", "#016C59", "#014636"),
  red = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#A50F15", "#67000D"),
  oranges = c("#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B", "#FD8D3C", "#F16913", "#D94801", "#A63603", "#7F2704"),
  purple = c("#FCFBFD", "#EFEDF5", "#DADAEB", "#BCBDDC", "#9E9AC8", "#807DBA", "#6A51A3", "#54278F", "#3F007D"),
  blue = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B"),
  green = c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B"),
  grey = c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696", "#737373", "#525252", "#252525", "#000000"),
  set1 = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"),
  set2 = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3"),
  set3 = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F"),
  paired = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928"),
  accent = c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666"),
  dark2 = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666"),
  pastel1 = c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2"),
  pastel2 = c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC")
)

# Shades
pal_shades <- list(
  blue = list(
    `2` = list(
      blue_dark = c("#08519C", "#BDC9E1"),
      blue2 = c("#0570B0", "#BDC9E1"),
      blue3 = c("#0571B0", "#92C5DE"),
      blue4 = c("#0072B5", "#98B8D8"),
      blue5 = c("#2B8CBE", "#ECE7F2"),
      blue6 = c("#74A9CF", "#F1EEF6")
    ),
    `3` = list(
      blue1 = c("#2B8CBE", "#A6BDDB", "#ECE7F2")
    )
  ),
  green = list(
    `2` = list(
      green1 = c("#2CA25F", "#B2E2E2"),
      green2 = c("#238B45", "#66C2A4"),
      green3 = c("#31A354", "#BAE4B3"),
      green4 = c("#66C2A4", "#EDF8FB"),
      green_light = c("#99D8C9", "#E5F5F9")
    ),
    `3` = list(
      green1 = c("#2CA25F", "#99D8C9", "#E5F5F9")
    )
  ),
  purple = list(
    `2` = list(
      purple_faded_light = c("#6A51A3", "#CBC9E2"),
      purple_faded_dark = c("#6A51A3", "#9E9AC8"),
      purple_rich_light = c("#54278F", "#CBC9E2"),
      purple_rich_dark = c("#54278F", "#9E9AC8"),
      purple_light = c("#756BB1", "#EFEDF5")
    ),
    `3` = list(
      purple1 = c("#756BB1", "#BCBDDC", "#EFEDF5")
    )
  ),
  red = list(
    `2` = list(
      red1 = c("#BC3C29", "#D6A299"),
      red2 = c("#CA0020", "#F4A582"),
      red3 = c("#D6604D", "#FDDBC7"),
      red4 = c("#DE2D26", "#FEE5D9"),
      red_dark1 = c("#A50F15", "#FCAE91"),
      red_dark2 = c("#B2182B", "#FDDBC7")
    ),
    `3` = list(
      red_light = c("#DE2D26", "#FC9272", "#FEE0D2"),
      red_dark = c("#BC3C29", "#D88D82", "#F4DEDB")
    ),
    `4` = list(
      red1 = c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7")
    ),
    `5` = list(
      red1 = c("#A50F15", "#DE2D26", "#FB6A4A", "#FCAE91", "#FEE5D9")
    )
  ),
  orange = list(
    `2` = list(
      orange1 = c("#E5550D", "#FDAE6B")
    )
  ),
  yellow = list(
    `2` = list(
      yellow1 = c("#F1C40F", "#F4E2A3")
    )
  ),
  grey = list(
    `2` = list(
      grey_charcoal_light1 = c("#333333", "#F7F7F7"),
      grey_charcoal_light2 = c("#333333", "#F0F0F0"),
      grey_charcoal_medium = c("#333333", "#E0E0E0"),
      grey_charcoal_dark1 = c("#333333", "#BABABA"),
      grey_charcoal_dark2 = c("#4D4D4D", "#BABABA"),
      grey_medium = c("#636363", "#F0F0F0")
    ),
    `3` = list(
      grey_dark1 = c("#333333", "#B0B0B0", "#ECECEC"),
      grey_dark2 = c("#333333", "#95A5A6", "#F0F0F0"),
      grey_light = c("#636363", "#B7B7B7", "#ECECEC")
    ),
    `4` = list(
      grey1 = c("#333333", "#727172", "#C3C2C4", "#EFEEF0"),
      grey2 = c("#5D5D5D", "#909090", "#BFBEC1", "#E5E3E6")
    ),
    `5` = list(
      grey_dark = c("#333333", "#525252", "#737373", "#969696", "#BDBDBD"),
      grey_light = c("#333333", "#5D5D5D", "#909090", "#BFBEC1", "#E5E3E6")
    ),
    `6` = list(
      grey1 = c("#333333", "#525252", "#737373", "#969696", "#BDBDBD", "#F0F0F0")
    ),
    `7` = list(
      grey1 = c("#333333", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")
    )
  )
)

# All palettes ------------------------------------------------------------
pals <- list(
  `1` = pal_1,
  `2` = pal_2,
  `3` = pal_3,
  `4` = pal_4,
  `5` = pal_5,
  `6` = pal_6,
  `7` = pal_7,
  `8` = pal_8,
  heatmap = pal_heatmap,
  brewer = pal_brewer,
  shades = pal_shades,
  R_base = list(R_base = c(black = "#000000", red = "#DF536B", green = "#61D04F", blue = "#2297E6", teal = "#28E2E5", magenta = "#CD0BBC", yellow = "#F5C710", grey = "#9E9E9E"))
)
