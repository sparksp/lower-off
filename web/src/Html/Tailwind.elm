module Html.Tailwind exposing
    ( absolute
    , alignBaseline
    , alignBottom
    , alignMiddle
    , alignTextBottom
    , alignTextTop
    , alignTop
    , animateBounce
    , animateNone
    , animatePing
    , animatePulse
    , animateSpin
    , antialiased
    , appearanceNone
    , bgAuto
    , bgBlack
    , bgBlue100
    , bgBlue200
    , bgBlue300
    , bgBlue400
    , bgBlue500
    , bgBlue600
    , bgBlue700
    , bgBlue800
    , bgBlue900
    , bgBottom
    , bgCenter
    , bgClipBorder
    , bgClipContent
    , bgClipPadding
    , bgClipText
    , bgContain
    , bgCover
    , bgCurrent
    , bgFixed
    , bgGradientToB
    , bgGradientToBl
    , bgGradientToBr
    , bgGradientToL
    , bgGradientToR
    , bgGradientToT
    , bgGradientToTl
    , bgGradientToTr
    , bgGray100
    , bgGray200
    , bgGray300
    , bgGray400
    , bgGray500
    , bgGray600
    , bgGray700
    , bgGray800
    , bgGray900
    , bgGreen100
    , bgGreen200
    , bgGreen300
    , bgGreen400
    , bgGreen500
    , bgGreen600
    , bgGreen700
    , bgGreen800
    , bgGreen900
    , bgIndigo100
    , bgIndigo200
    , bgIndigo300
    , bgIndigo400
    , bgIndigo500
    , bgIndigo600
    , bgIndigo700
    , bgIndigo800
    , bgIndigo900
    , bgLeft
    , bgLeftBottom
    , bgLeftTop
    , bgLocal
    , bgNoRepeat
    , bgNone
    , bgOpacity0
    , bgOpacity100
    , bgOpacity25
    , bgOpacity50
    , bgOpacity75
    , bgOrange100
    , bgOrange200
    , bgOrange300
    , bgOrange400
    , bgOrange500
    , bgOrange600
    , bgOrange700
    , bgOrange800
    , bgOrange900
    , bgPink100
    , bgPink200
    , bgPink300
    , bgPink400
    , bgPink500
    , bgPink600
    , bgPink700
    , bgPink800
    , bgPink900
    , bgPurple100
    , bgPurple200
    , bgPurple300
    , bgPurple400
    , bgPurple500
    , bgPurple600
    , bgPurple700
    , bgPurple800
    , bgPurple900
    , bgRed100
    , bgRed200
    , bgRed300
    , bgRed400
    , bgRed500
    , bgRed600
    , bgRed700
    , bgRed800
    , bgRed900
    , bgRepeat
    , bgRepeatRound
    , bgRepeatSpace
    , bgRepeatX
    , bgRepeatY
    , bgRight
    , bgRightBottom
    , bgRightTop
    , bgScroll
    , bgTeal100
    , bgTeal200
    , bgTeal300
    , bgTeal400
    , bgTeal500
    , bgTeal600
    , bgTeal700
    , bgTeal800
    , bgTeal900
    , bgTop
    , bgTransparent
    , bgWhite
    , bgYellow100
    , bgYellow200
    , bgYellow300
    , bgYellow400
    , bgYellow500
    , bgYellow600
    , bgYellow700
    , bgYellow800
    , bgYellow900
    , block
    , border
    , border0
    , border2
    , border4
    , border8
    , borderB
    , borderB0
    , borderB2
    , borderB4
    , borderB8
    , borderBlack
    , borderBlue100
    , borderBlue200
    , borderBlue300
    , borderBlue400
    , borderBlue500
    , borderBlue600
    , borderBlue700
    , borderBlue800
    , borderBlue900
    , borderCollapse
    , borderCurrent
    , borderDashed
    , borderDotted
    , borderDouble
    , borderGray100
    , borderGray200
    , borderGray300
    , borderGray400
    , borderGray500
    , borderGray600
    , borderGray700
    , borderGray800
    , borderGray900
    , borderGreen100
    , borderGreen200
    , borderGreen300
    , borderGreen400
    , borderGreen500
    , borderGreen600
    , borderGreen700
    , borderGreen800
    , borderGreen900
    , borderIndigo100
    , borderIndigo200
    , borderIndigo300
    , borderIndigo400
    , borderIndigo500
    , borderIndigo600
    , borderIndigo700
    , borderIndigo800
    , borderIndigo900
    , borderL
    , borderL0
    , borderL2
    , borderL4
    , borderL8
    , borderNone
    , borderOpacity0
    , borderOpacity100
    , borderOpacity25
    , borderOpacity50
    , borderOpacity75
    , borderOrange100
    , borderOrange200
    , borderOrange300
    , borderOrange400
    , borderOrange500
    , borderOrange600
    , borderOrange700
    , borderOrange800
    , borderOrange900
    , borderPink100
    , borderPink200
    , borderPink300
    , borderPink400
    , borderPink500
    , borderPink600
    , borderPink700
    , borderPink800
    , borderPink900
    , borderPurple100
    , borderPurple200
    , borderPurple300
    , borderPurple400
    , borderPurple500
    , borderPurple600
    , borderPurple700
    , borderPurple800
    , borderPurple900
    , borderR
    , borderR0
    , borderR2
    , borderR4
    , borderR8
    , borderRed100
    , borderRed200
    , borderRed300
    , borderRed400
    , borderRed500
    , borderRed600
    , borderRed700
    , borderRed800
    , borderRed900
    , borderSeparate
    , borderSolid
    , borderT
    , borderT0
    , borderT2
    , borderT4
    , borderT8
    , borderTeal100
    , borderTeal200
    , borderTeal300
    , borderTeal400
    , borderTeal500
    , borderTeal600
    , borderTeal700
    , borderTeal800
    , borderTeal900
    , borderTransparent
    , borderWhite
    , borderYellow100
    , borderYellow200
    , borderYellow300
    , borderYellow400
    , borderYellow500
    , borderYellow600
    , borderYellow700
    , borderYellow800
    , borderYellow900
    , bottom0
    , bottomAuto
    , boxBorder
    , boxContent
    , breakAll
    , breakNormal
    , breakWords
    , capitalize
    , classList
    , clearBoth
    , clearLeft
    , clearNone
    , clearRight
    , clearfixAfter
    , colAuto
    , colEnd1
    , colEnd10
    , colEnd11
    , colEnd12
    , colEnd13
    , colEnd2
    , colEnd3
    , colEnd4
    , colEnd5
    , colEnd6
    , colEnd7
    , colEnd8
    , colEnd9
    , colEndAuto
    , colSpan1
    , colSpan10
    , colSpan11
    , colSpan12
    , colSpan2
    , colSpan3
    , colSpan4
    , colSpan5
    , colSpan6
    , colSpan7
    , colSpan8
    , colSpan9
    , colStart1
    , colStart10
    , colStart11
    , colStart12
    , colStart13
    , colStart2
    , colStart3
    , colStart4
    , colStart5
    , colStart6
    , colStart7
    , colStart8
    , colStart9
    , colStartAuto
    , container
    , contentAround
    , contentBetween
    , contentCenter
    , contentEnd
    , contentEvenly
    , contentStart
    , contents
    , cursorAuto
    , cursorDefault
    , cursorMove
    , cursorNotAllowed
    , cursorPointer
    , cursorText
    , cursorWait
    , delay100
    , delay1000
    , delay150
    , delay200
    , delay300
    , delay500
    , delay700
    , delay75
    , diagonalFractions
    , divideBlack
    , divideBlue100
    , divideBlue200
    , divideBlue300
    , divideBlue400
    , divideBlue500
    , divideBlue600
    , divideBlue700
    , divideBlue800
    , divideBlue900
    , divideCurrent
    , divideDashed
    , divideDotted
    , divideDouble
    , divideGray100
    , divideGray200
    , divideGray300
    , divideGray400
    , divideGray500
    , divideGray600
    , divideGray700
    , divideGray800
    , divideGray900
    , divideGreen100
    , divideGreen200
    , divideGreen300
    , divideGreen400
    , divideGreen500
    , divideGreen600
    , divideGreen700
    , divideGreen800
    , divideGreen900
    , divideIndigo100
    , divideIndigo200
    , divideIndigo300
    , divideIndigo400
    , divideIndigo500
    , divideIndigo600
    , divideIndigo700
    , divideIndigo800
    , divideIndigo900
    , divideNone
    , divideOpacity0
    , divideOpacity100
    , divideOpacity25
    , divideOpacity50
    , divideOpacity75
    , divideOrange100
    , divideOrange200
    , divideOrange300
    , divideOrange400
    , divideOrange500
    , divideOrange600
    , divideOrange700
    , divideOrange800
    , divideOrange900
    , dividePink100
    , dividePink200
    , dividePink300
    , dividePink400
    , dividePink500
    , dividePink600
    , dividePink700
    , dividePink800
    , dividePink900
    , dividePurple100
    , dividePurple200
    , dividePurple300
    , dividePurple400
    , dividePurple500
    , dividePurple600
    , dividePurple700
    , dividePurple800
    , dividePurple900
    , divideRed100
    , divideRed200
    , divideRed300
    , divideRed400
    , divideRed500
    , divideRed600
    , divideRed700
    , divideRed800
    , divideRed900
    , divideSolid
    , divideTeal100
    , divideTeal200
    , divideTeal300
    , divideTeal400
    , divideTeal500
    , divideTeal600
    , divideTeal700
    , divideTeal800
    , divideTeal900
    , divideTransparent
    , divideWhite
    , divideX
    , divideX0
    , divideX2
    , divideX4
    , divideX8
    , divideXReverse
    , divideY
    , divideY0
    , divideY2
    , divideY4
    , divideY8
    , divideYReverse
    , divideYellow100
    , divideYellow200
    , divideYellow300
    , divideYellow400
    , divideYellow500
    , divideYellow600
    , divideYellow700
    , divideYellow800
    , divideYellow900
    , duration100
    , duration1000
    , duration150
    , duration200
    , duration300
    , duration500
    , duration700
    , duration75
    , easeIn
    , easeInOut
    , easeLinear
    , easeOut
    , fillCurrent
    , fixed
    , flex
    , flex1
    , flexAuto
    , flexCol
    , flexColReverse
    , flexGrow
    , flexGrow0
    , flexInitial
    , flexNoWrap
    , flexNone
    , flexRow
    , flexRowReverse
    , flexShrink
    , flexShrink0
    , flexWrap
    , flexWrapReverse
    , floatLeft
    , floatNone
    , floatRight
    , flowRoot
    , focusBgBlack
    , focusBgBlue100
    , focusBgBlue200
    , focusBgBlue300
    , focusBgBlue400
    , focusBgBlue500
    , focusBgBlue600
    , focusBgBlue700
    , focusBgBlue800
    , focusBgBlue900
    , focusBgCurrent
    , focusBgGray100
    , focusBgGray200
    , focusBgGray300
    , focusBgGray400
    , focusBgGray500
    , focusBgGray600
    , focusBgGray700
    , focusBgGray800
    , focusBgGray900
    , focusBgGreen100
    , focusBgGreen200
    , focusBgGreen300
    , focusBgGreen400
    , focusBgGreen500
    , focusBgGreen600
    , focusBgGreen700
    , focusBgGreen800
    , focusBgGreen900
    , focusBgIndigo100
    , focusBgIndigo200
    , focusBgIndigo300
    , focusBgIndigo400
    , focusBgIndigo500
    , focusBgIndigo600
    , focusBgIndigo700
    , focusBgIndigo800
    , focusBgIndigo900
    , focusBgOpacity0
    , focusBgOpacity100
    , focusBgOpacity25
    , focusBgOpacity50
    , focusBgOpacity75
    , focusBgOrange100
    , focusBgOrange200
    , focusBgOrange300
    , focusBgOrange400
    , focusBgOrange500
    , focusBgOrange600
    , focusBgOrange700
    , focusBgOrange800
    , focusBgOrange900
    , focusBgPink100
    , focusBgPink200
    , focusBgPink300
    , focusBgPink400
    , focusBgPink500
    , focusBgPink600
    , focusBgPink700
    , focusBgPink800
    , focusBgPink900
    , focusBgPurple100
    , focusBgPurple200
    , focusBgPurple300
    , focusBgPurple400
    , focusBgPurple500
    , focusBgPurple600
    , focusBgPurple700
    , focusBgPurple800
    , focusBgPurple900
    , focusBgRed100
    , focusBgRed200
    , focusBgRed300
    , focusBgRed400
    , focusBgRed500
    , focusBgRed600
    , focusBgRed700
    , focusBgRed800
    , focusBgRed900
    , focusBgTeal100
    , focusBgTeal200
    , focusBgTeal300
    , focusBgTeal400
    , focusBgTeal500
    , focusBgTeal600
    , focusBgTeal700
    , focusBgTeal800
    , focusBgTeal900
    , focusBgTransparent
    , focusBgWhite
    , focusBgYellow100
    , focusBgYellow200
    , focusBgYellow300
    , focusBgYellow400
    , focusBgYellow500
    , focusBgYellow600
    , focusBgYellow700
    , focusBgYellow800
    , focusBgYellow900
    , focusBorderBlack
    , focusBorderBlue100
    , focusBorderBlue200
    , focusBorderBlue300
    , focusBorderBlue400
    , focusBorderBlue500
    , focusBorderBlue600
    , focusBorderBlue700
    , focusBorderBlue800
    , focusBorderBlue900
    , focusBorderCurrent
    , focusBorderGray100
    , focusBorderGray200
    , focusBorderGray300
    , focusBorderGray400
    , focusBorderGray500
    , focusBorderGray600
    , focusBorderGray700
    , focusBorderGray800
    , focusBorderGray900
    , focusBorderGreen100
    , focusBorderGreen200
    , focusBorderGreen300
    , focusBorderGreen400
    , focusBorderGreen500
    , focusBorderGreen600
    , focusBorderGreen700
    , focusBorderGreen800
    , focusBorderGreen900
    , focusBorderIndigo100
    , focusBorderIndigo200
    , focusBorderIndigo300
    , focusBorderIndigo400
    , focusBorderIndigo500
    , focusBorderIndigo600
    , focusBorderIndigo700
    , focusBorderIndigo800
    , focusBorderIndigo900
    , focusBorderOpacity0
    , focusBorderOpacity100
    , focusBorderOpacity25
    , focusBorderOpacity50
    , focusBorderOpacity75
    , focusBorderOrange100
    , focusBorderOrange200
    , focusBorderOrange300
    , focusBorderOrange400
    , focusBorderOrange500
    , focusBorderOrange600
    , focusBorderOrange700
    , focusBorderOrange800
    , focusBorderOrange900
    , focusBorderPink100
    , focusBorderPink200
    , focusBorderPink300
    , focusBorderPink400
    , focusBorderPink500
    , focusBorderPink600
    , focusBorderPink700
    , focusBorderPink800
    , focusBorderPink900
    , focusBorderPurple100
    , focusBorderPurple200
    , focusBorderPurple300
    , focusBorderPurple400
    , focusBorderPurple500
    , focusBorderPurple600
    , focusBorderPurple700
    , focusBorderPurple800
    , focusBorderPurple900
    , focusBorderRed100
    , focusBorderRed200
    , focusBorderRed300
    , focusBorderRed400
    , focusBorderRed500
    , focusBorderRed600
    , focusBorderRed700
    , focusBorderRed800
    , focusBorderRed900
    , focusBorderTeal100
    , focusBorderTeal200
    , focusBorderTeal300
    , focusBorderTeal400
    , focusBorderTeal500
    , focusBorderTeal600
    , focusBorderTeal700
    , focusBorderTeal800
    , focusBorderTeal900
    , focusBorderTransparent
    , focusBorderWhite
    , focusBorderYellow100
    , focusBorderYellow200
    , focusBorderYellow300
    , focusBorderYellow400
    , focusBorderYellow500
    , focusBorderYellow600
    , focusBorderYellow700
    , focusBorderYellow800
    , focusBorderYellow900
    , focusFontBlack
    , focusFontBold
    , focusFontExtrabold
    , focusFontHairline
    , focusFontLight
    , focusFontMedium
    , focusFontNormal
    , focusFontSemibold
    , focusFontThin
    , focusFromBlack
    , focusFromBlue100
    , focusFromBlue200
    , focusFromBlue300
    , focusFromBlue400
    , focusFromBlue500
    , focusFromBlue600
    , focusFromBlue700
    , focusFromBlue800
    , focusFromBlue900
    , focusFromCurrent
    , focusFromGray100
    , focusFromGray200
    , focusFromGray300
    , focusFromGray400
    , focusFromGray500
    , focusFromGray600
    , focusFromGray700
    , focusFromGray800
    , focusFromGray900
    , focusFromGreen100
    , focusFromGreen200
    , focusFromGreen300
    , focusFromGreen400
    , focusFromGreen500
    , focusFromGreen600
    , focusFromGreen700
    , focusFromGreen800
    , focusFromGreen900
    , focusFromIndigo100
    , focusFromIndigo200
    , focusFromIndigo300
    , focusFromIndigo400
    , focusFromIndigo500
    , focusFromIndigo600
    , focusFromIndigo700
    , focusFromIndigo800
    , focusFromIndigo900
    , focusFromOrange100
    , focusFromOrange200
    , focusFromOrange300
    , focusFromOrange400
    , focusFromOrange500
    , focusFromOrange600
    , focusFromOrange700
    , focusFromOrange800
    , focusFromOrange900
    , focusFromPink100
    , focusFromPink200
    , focusFromPink300
    , focusFromPink400
    , focusFromPink500
    , focusFromPink600
    , focusFromPink700
    , focusFromPink800
    , focusFromPink900
    , focusFromPurple100
    , focusFromPurple200
    , focusFromPurple300
    , focusFromPurple400
    , focusFromPurple500
    , focusFromPurple600
    , focusFromPurple700
    , focusFromPurple800
    , focusFromPurple900
    , focusFromRed100
    , focusFromRed200
    , focusFromRed300
    , focusFromRed400
    , focusFromRed500
    , focusFromRed600
    , focusFromRed700
    , focusFromRed800
    , focusFromRed900
    , focusFromTeal100
    , focusFromTeal200
    , focusFromTeal300
    , focusFromTeal400
    , focusFromTeal500
    , focusFromTeal600
    , focusFromTeal700
    , focusFromTeal800
    , focusFromTeal900
    , focusFromTransparent
    , focusFromWhite
    , focusFromYellow100
    , focusFromYellow200
    , focusFromYellow300
    , focusFromYellow400
    , focusFromYellow500
    , focusFromYellow600
    , focusFromYellow700
    , focusFromYellow800
    , focusFromYellow900
    , focusLineThrough
    , focusNegRotate180
    , focusNegRotate45
    , focusNegRotate90
    , focusNegSkewX12
    , focusNegSkewX3
    , focusNegSkewX6
    , focusNegSkewY12
    , focusNegSkewY3
    , focusNegSkewY6
    , focusNegTranslateX1
    , focusNegTranslateX10
    , focusNegTranslateX12
    , focusNegTranslateX16
    , focusNegTranslateX1over2
    , focusNegTranslateX2
    , focusNegTranslateX20
    , focusNegTranslateX24
    , focusNegTranslateX3
    , focusNegTranslateX32
    , focusNegTranslateX4
    , focusNegTranslateX40
    , focusNegTranslateX48
    , focusNegTranslateX5
    , focusNegTranslateX56
    , focusNegTranslateX6
    , focusNegTranslateX64
    , focusNegTranslateX8
    , focusNegTranslateXFull
    , focusNegTranslateXPx
    , focusNegTranslateY1
    , focusNegTranslateY10
    , focusNegTranslateY12
    , focusNegTranslateY16
    , focusNegTranslateY1over2
    , focusNegTranslateY2
    , focusNegTranslateY20
    , focusNegTranslateY24
    , focusNegTranslateY3
    , focusNegTranslateY32
    , focusNegTranslateY4
    , focusNegTranslateY40
    , focusNegTranslateY48
    , focusNegTranslateY5
    , focusNegTranslateY56
    , focusNegTranslateY6
    , focusNegTranslateY64
    , focusNegTranslateY8
    , focusNegTranslateYFull
    , focusNegTranslateYPx
    , focusNoUnderline
    , focusNotSrOnly
    , focusOpacity0
    , focusOpacity100
    , focusOpacity25
    , focusOpacity50
    , focusOpacity75
    , focusOutlineNone
    , focusPlaceholderBlackFocus
    , focusPlaceholderBlue100Focus
    , focusPlaceholderBlue200Focus
    , focusPlaceholderBlue300Focus
    , focusPlaceholderBlue400Focus
    , focusPlaceholderBlue500Focus
    , focusPlaceholderBlue600Focus
    , focusPlaceholderBlue700Focus
    , focusPlaceholderBlue800Focus
    , focusPlaceholderBlue900Focus
    , focusPlaceholderCurrentFocus
    , focusPlaceholderGray100Focus
    , focusPlaceholderGray200Focus
    , focusPlaceholderGray300Focus
    , focusPlaceholderGray400Focus
    , focusPlaceholderGray500Focus
    , focusPlaceholderGray600Focus
    , focusPlaceholderGray700Focus
    , focusPlaceholderGray800Focus
    , focusPlaceholderGray900Focus
    , focusPlaceholderGreen100Focus
    , focusPlaceholderGreen200Focus
    , focusPlaceholderGreen300Focus
    , focusPlaceholderGreen400Focus
    , focusPlaceholderGreen500Focus
    , focusPlaceholderGreen600Focus
    , focusPlaceholderGreen700Focus
    , focusPlaceholderGreen800Focus
    , focusPlaceholderGreen900Focus
    , focusPlaceholderIndigo100Focus
    , focusPlaceholderIndigo200Focus
    , focusPlaceholderIndigo300Focus
    , focusPlaceholderIndigo400Focus
    , focusPlaceholderIndigo500Focus
    , focusPlaceholderIndigo600Focus
    , focusPlaceholderIndigo700Focus
    , focusPlaceholderIndigo800Focus
    , focusPlaceholderIndigo900Focus
    , focusPlaceholderOpacity0Focus
    , focusPlaceholderOpacity100Focus
    , focusPlaceholderOpacity25Focus
    , focusPlaceholderOpacity50Focus
    , focusPlaceholderOpacity75Focus
    , focusPlaceholderOrange100Focus
    , focusPlaceholderOrange200Focus
    , focusPlaceholderOrange300Focus
    , focusPlaceholderOrange400Focus
    , focusPlaceholderOrange500Focus
    , focusPlaceholderOrange600Focus
    , focusPlaceholderOrange700Focus
    , focusPlaceholderOrange800Focus
    , focusPlaceholderOrange900Focus
    , focusPlaceholderPink100Focus
    , focusPlaceholderPink200Focus
    , focusPlaceholderPink300Focus
    , focusPlaceholderPink400Focus
    , focusPlaceholderPink500Focus
    , focusPlaceholderPink600Focus
    , focusPlaceholderPink700Focus
    , focusPlaceholderPink800Focus
    , focusPlaceholderPink900Focus
    , focusPlaceholderPurple100Focus
    , focusPlaceholderPurple200Focus
    , focusPlaceholderPurple300Focus
    , focusPlaceholderPurple400Focus
    , focusPlaceholderPurple500Focus
    , focusPlaceholderPurple600Focus
    , focusPlaceholderPurple700Focus
    , focusPlaceholderPurple800Focus
    , focusPlaceholderPurple900Focus
    , focusPlaceholderRed100Focus
    , focusPlaceholderRed200Focus
    , focusPlaceholderRed300Focus
    , focusPlaceholderRed400Focus
    , focusPlaceholderRed500Focus
    , focusPlaceholderRed600Focus
    , focusPlaceholderRed700Focus
    , focusPlaceholderRed800Focus
    , focusPlaceholderRed900Focus
    , focusPlaceholderTeal100Focus
    , focusPlaceholderTeal200Focus
    , focusPlaceholderTeal300Focus
    , focusPlaceholderTeal400Focus
    , focusPlaceholderTeal500Focus
    , focusPlaceholderTeal600Focus
    , focusPlaceholderTeal700Focus
    , focusPlaceholderTeal800Focus
    , focusPlaceholderTeal900Focus
    , focusPlaceholderTransparentFocus
    , focusPlaceholderWhiteFocus
    , focusPlaceholderYellow100Focus
    , focusPlaceholderYellow200Focus
    , focusPlaceholderYellow300Focus
    , focusPlaceholderYellow400Focus
    , focusPlaceholderYellow500Focus
    , focusPlaceholderYellow600Focus
    , focusPlaceholderYellow700Focus
    , focusPlaceholderYellow800Focus
    , focusPlaceholderYellow900Focus
    , focusRotate0
    , focusRotate180
    , focusRotate45
    , focusRotate90
    , focusScale0
    , focusScale100
    , focusScale105
    , focusScale110
    , focusScale125
    , focusScale150
    , focusScale50
    , focusScale75
    , focusScale90
    , focusScale95
    , focusScaleX0
    , focusScaleX100
    , focusScaleX105
    , focusScaleX110
    , focusScaleX125
    , focusScaleX150
    , focusScaleX50
    , focusScaleX75
    , focusScaleX90
    , focusScaleX95
    , focusScaleY0
    , focusScaleY100
    , focusScaleY105
    , focusScaleY110
    , focusScaleY125
    , focusScaleY150
    , focusScaleY50
    , focusScaleY75
    , focusScaleY90
    , focusScaleY95
    , focusShadow
    , focusShadow2xl
    , focusShadowInner
    , focusShadowLg
    , focusShadowMd
    , focusShadowNone
    , focusShadowOutline
    , focusShadowSm
    , focusShadowXl
    , focusShadowXs
    , focusSkewX0
    , focusSkewX12
    , focusSkewX3
    , focusSkewX6
    , focusSkewY0
    , focusSkewY12
    , focusSkewY3
    , focusSkewY6
    , focusSrOnly
    , focusTextBlack
    , focusTextBlue100
    , focusTextBlue200
    , focusTextBlue300
    , focusTextBlue400
    , focusTextBlue500
    , focusTextBlue600
    , focusTextBlue700
    , focusTextBlue800
    , focusTextBlue900
    , focusTextCurrent
    , focusTextGray100
    , focusTextGray200
    , focusTextGray300
    , focusTextGray400
    , focusTextGray500
    , focusTextGray600
    , focusTextGray700
    , focusTextGray800
    , focusTextGray900
    , focusTextGreen100
    , focusTextGreen200
    , focusTextGreen300
    , focusTextGreen400
    , focusTextGreen500
    , focusTextGreen600
    , focusTextGreen700
    , focusTextGreen800
    , focusTextGreen900
    , focusTextIndigo100
    , focusTextIndigo200
    , focusTextIndigo300
    , focusTextIndigo400
    , focusTextIndigo500
    , focusTextIndigo600
    , focusTextIndigo700
    , focusTextIndigo800
    , focusTextIndigo900
    , focusTextOpacity0
    , focusTextOpacity100
    , focusTextOpacity25
    , focusTextOpacity50
    , focusTextOpacity75
    , focusTextOrange100
    , focusTextOrange200
    , focusTextOrange300
    , focusTextOrange400
    , focusTextOrange500
    , focusTextOrange600
    , focusTextOrange700
    , focusTextOrange800
    , focusTextOrange900
    , focusTextPink100
    , focusTextPink200
    , focusTextPink300
    , focusTextPink400
    , focusTextPink500
    , focusTextPink600
    , focusTextPink700
    , focusTextPink800
    , focusTextPink900
    , focusTextPurple100
    , focusTextPurple200
    , focusTextPurple300
    , focusTextPurple400
    , focusTextPurple500
    , focusTextPurple600
    , focusTextPurple700
    , focusTextPurple800
    , focusTextPurple900
    , focusTextRed100
    , focusTextRed200
    , focusTextRed300
    , focusTextRed400
    , focusTextRed500
    , focusTextRed600
    , focusTextRed700
    , focusTextRed800
    , focusTextRed900
    , focusTextTeal100
    , focusTextTeal200
    , focusTextTeal300
    , focusTextTeal400
    , focusTextTeal500
    , focusTextTeal600
    , focusTextTeal700
    , focusTextTeal800
    , focusTextTeal900
    , focusTextTransparent
    , focusTextWhite
    , focusTextYellow100
    , focusTextYellow200
    , focusTextYellow300
    , focusTextYellow400
    , focusTextYellow500
    , focusTextYellow600
    , focusTextYellow700
    , focusTextYellow800
    , focusTextYellow900
    , focusToBlack
    , focusToBlue100
    , focusToBlue200
    , focusToBlue300
    , focusToBlue400
    , focusToBlue500
    , focusToBlue600
    , focusToBlue700
    , focusToBlue800
    , focusToBlue900
    , focusToCurrent
    , focusToGray100
    , focusToGray200
    , focusToGray300
    , focusToGray400
    , focusToGray500
    , focusToGray600
    , focusToGray700
    , focusToGray800
    , focusToGray900
    , focusToGreen100
    , focusToGreen200
    , focusToGreen300
    , focusToGreen400
    , focusToGreen500
    , focusToGreen600
    , focusToGreen700
    , focusToGreen800
    , focusToGreen900
    , focusToIndigo100
    , focusToIndigo200
    , focusToIndigo300
    , focusToIndigo400
    , focusToIndigo500
    , focusToIndigo600
    , focusToIndigo700
    , focusToIndigo800
    , focusToIndigo900
    , focusToOrange100
    , focusToOrange200
    , focusToOrange300
    , focusToOrange400
    , focusToOrange500
    , focusToOrange600
    , focusToOrange700
    , focusToOrange800
    , focusToOrange900
    , focusToPink100
    , focusToPink200
    , focusToPink300
    , focusToPink400
    , focusToPink500
    , focusToPink600
    , focusToPink700
    , focusToPink800
    , focusToPink900
    , focusToPurple100
    , focusToPurple200
    , focusToPurple300
    , focusToPurple400
    , focusToPurple500
    , focusToPurple600
    , focusToPurple700
    , focusToPurple800
    , focusToPurple900
    , focusToRed100
    , focusToRed200
    , focusToRed300
    , focusToRed400
    , focusToRed500
    , focusToRed600
    , focusToRed700
    , focusToRed800
    , focusToRed900
    , focusToTeal100
    , focusToTeal200
    , focusToTeal300
    , focusToTeal400
    , focusToTeal500
    , focusToTeal600
    , focusToTeal700
    , focusToTeal800
    , focusToTeal900
    , focusToTransparent
    , focusToWhite
    , focusToYellow100
    , focusToYellow200
    , focusToYellow300
    , focusToYellow400
    , focusToYellow500
    , focusToYellow600
    , focusToYellow700
    , focusToYellow800
    , focusToYellow900
    , focusTranslateX0
    , focusTranslateX1
    , focusTranslateX10
    , focusTranslateX12
    , focusTranslateX16
    , focusTranslateX1over2
    , focusTranslateX2
    , focusTranslateX20
    , focusTranslateX24
    , focusTranslateX3
    , focusTranslateX32
    , focusTranslateX4
    , focusTranslateX40
    , focusTranslateX48
    , focusTranslateX5
    , focusTranslateX56
    , focusTranslateX6
    , focusTranslateX64
    , focusTranslateX8
    , focusTranslateXFull
    , focusTranslateXPx
    , focusTranslateY0
    , focusTranslateY1
    , focusTranslateY10
    , focusTranslateY12
    , focusTranslateY16
    , focusTranslateY1over2
    , focusTranslateY2
    , focusTranslateY20
    , focusTranslateY24
    , focusTranslateY3
    , focusTranslateY32
    , focusTranslateY4
    , focusTranslateY40
    , focusTranslateY48
    , focusTranslateY5
    , focusTranslateY56
    , focusTranslateY6
    , focusTranslateY64
    , focusTranslateY8
    , focusTranslateYFull
    , focusTranslateYPx
    , focusUnderline
    , focusViaBlack
    , focusViaBlue100
    , focusViaBlue200
    , focusViaBlue300
    , focusViaBlue400
    , focusViaBlue500
    , focusViaBlue600
    , focusViaBlue700
    , focusViaBlue800
    , focusViaBlue900
    , focusViaCurrent
    , focusViaGray100
    , focusViaGray200
    , focusViaGray300
    , focusViaGray400
    , focusViaGray500
    , focusViaGray600
    , focusViaGray700
    , focusViaGray800
    , focusViaGray900
    , focusViaGreen100
    , focusViaGreen200
    , focusViaGreen300
    , focusViaGreen400
    , focusViaGreen500
    , focusViaGreen600
    , focusViaGreen700
    , focusViaGreen800
    , focusViaGreen900
    , focusViaIndigo100
    , focusViaIndigo200
    , focusViaIndigo300
    , focusViaIndigo400
    , focusViaIndigo500
    , focusViaIndigo600
    , focusViaIndigo700
    , focusViaIndigo800
    , focusViaIndigo900
    , focusViaOrange100
    , focusViaOrange200
    , focusViaOrange300
    , focusViaOrange400
    , focusViaOrange500
    , focusViaOrange600
    , focusViaOrange700
    , focusViaOrange800
    , focusViaOrange900
    , focusViaPink100
    , focusViaPink200
    , focusViaPink300
    , focusViaPink400
    , focusViaPink500
    , focusViaPink600
    , focusViaPink700
    , focusViaPink800
    , focusViaPink900
    , focusViaPurple100
    , focusViaPurple200
    , focusViaPurple300
    , focusViaPurple400
    , focusViaPurple500
    , focusViaPurple600
    , focusViaPurple700
    , focusViaPurple800
    , focusViaPurple900
    , focusViaRed100
    , focusViaRed200
    , focusViaRed300
    , focusViaRed400
    , focusViaRed500
    , focusViaRed600
    , focusViaRed700
    , focusViaRed800
    , focusViaRed900
    , focusViaTeal100
    , focusViaTeal200
    , focusViaTeal300
    , focusViaTeal400
    , focusViaTeal500
    , focusViaTeal600
    , focusViaTeal700
    , focusViaTeal800
    , focusViaTeal900
    , focusViaTransparent
    , focusViaWhite
    , focusViaYellow100
    , focusViaYellow200
    , focusViaYellow300
    , focusViaYellow400
    , focusViaYellow500
    , focusViaYellow600
    , focusViaYellow700
    , focusViaYellow800
    , focusViaYellow900
    , fontBlack
    , fontBold
    , fontExtrabold
    , fontHairline
    , fontLight
    , fontMedium
    , fontMono
    , fontNormal
    , fontSans
    , fontSemibold
    , fontSerif
    , fontThin
    , fromBlack
    , fromBlue100
    , fromBlue200
    , fromBlue300
    , fromBlue400
    , fromBlue500
    , fromBlue600
    , fromBlue700
    , fromBlue800
    , fromBlue900
    , fromCurrent
    , fromGray100
    , fromGray200
    , fromGray300
    , fromGray400
    , fromGray500
    , fromGray600
    , fromGray700
    , fromGray800
    , fromGray900
    , fromGreen100
    , fromGreen200
    , fromGreen300
    , fromGreen400
    , fromGreen500
    , fromGreen600
    , fromGreen700
    , fromGreen800
    , fromGreen900
    , fromIndigo100
    , fromIndigo200
    , fromIndigo300
    , fromIndigo400
    , fromIndigo500
    , fromIndigo600
    , fromIndigo700
    , fromIndigo800
    , fromIndigo900
    , fromOrange100
    , fromOrange200
    , fromOrange300
    , fromOrange400
    , fromOrange500
    , fromOrange600
    , fromOrange700
    , fromOrange800
    , fromOrange900
    , fromPink100
    , fromPink200
    , fromPink300
    , fromPink400
    , fromPink500
    , fromPink600
    , fromPink700
    , fromPink800
    , fromPink900
    , fromPurple100
    , fromPurple200
    , fromPurple300
    , fromPurple400
    , fromPurple500
    , fromPurple600
    , fromPurple700
    , fromPurple800
    , fromPurple900
    , fromRed100
    , fromRed200
    , fromRed300
    , fromRed400
    , fromRed500
    , fromRed600
    , fromRed700
    , fromRed800
    , fromRed900
    , fromTeal100
    , fromTeal200
    , fromTeal300
    , fromTeal400
    , fromTeal500
    , fromTeal600
    , fromTeal700
    , fromTeal800
    , fromTeal900
    , fromTransparent
    , fromWhite
    , fromYellow100
    , fromYellow200
    , fromYellow300
    , fromYellow400
    , fromYellow500
    , fromYellow600
    , fromYellow700
    , fromYellow800
    , fromYellow900
    , gap0
    , gap1
    , gap10
    , gap12
    , gap16
    , gap2
    , gap20
    , gap24
    , gap3
    , gap32
    , gap4
    , gap40
    , gap48
    , gap5
    , gap56
    , gap6
    , gap64
    , gap8
    , gapPx
    , gapX0
    , gapX1
    , gapX10
    , gapX12
    , gapX16
    , gapX2
    , gapX20
    , gapX24
    , gapX3
    , gapX32
    , gapX4
    , gapX40
    , gapX48
    , gapX5
    , gapX56
    , gapX6
    , gapX64
    , gapX8
    , gapXPx
    , gapY0
    , gapY1
    , gapY10
    , gapY12
    , gapY16
    , gapY2
    , gapY20
    , gapY24
    , gapY3
    , gapY32
    , gapY4
    , gapY40
    , gapY48
    , gapY5
    , gapY56
    , gapY6
    , gapY64
    , gapY8
    , gapYPx
    , grid
    , gridCols1
    , gridCols10
    , gridCols11
    , gridCols12
    , gridCols2
    , gridCols3
    , gridCols4
    , gridCols5
    , gridCols6
    , gridCols7
    , gridCols8
    , gridCols9
    , gridColsNone
    , gridFlowCol
    , gridFlowColDense
    , gridFlowRow
    , gridFlowRowDense
    , gridRows1
    , gridRows2
    , gridRows3
    , gridRows4
    , gridRows5
    , gridRows6
    , gridRowsNone
    , h0
    , h1
    , h10
    , h12
    , h16
    , h2
    , h20
    , h24
    , h3
    , h32
    , h4
    , h40
    , h48
    , h5
    , h56
    , h6
    , h64
    , h8
    , hAuto
    , hFull
    , hPx
    , hScreen
    , hidden
    , hoverBgBlack
    , hoverBgBlue100
    , hoverBgBlue200
    , hoverBgBlue300
    , hoverBgBlue400
    , hoverBgBlue500
    , hoverBgBlue600
    , hoverBgBlue700
    , hoverBgBlue800
    , hoverBgBlue900
    , hoverBgCurrent
    , hoverBgGray100
    , hoverBgGray200
    , hoverBgGray300
    , hoverBgGray400
    , hoverBgGray500
    , hoverBgGray600
    , hoverBgGray700
    , hoverBgGray800
    , hoverBgGray900
    , hoverBgGreen100
    , hoverBgGreen200
    , hoverBgGreen300
    , hoverBgGreen400
    , hoverBgGreen500
    , hoverBgGreen600
    , hoverBgGreen700
    , hoverBgGreen800
    , hoverBgGreen900
    , hoverBgIndigo100
    , hoverBgIndigo200
    , hoverBgIndigo300
    , hoverBgIndigo400
    , hoverBgIndigo500
    , hoverBgIndigo600
    , hoverBgIndigo700
    , hoverBgIndigo800
    , hoverBgIndigo900
    , hoverBgOpacity0
    , hoverBgOpacity100
    , hoverBgOpacity25
    , hoverBgOpacity50
    , hoverBgOpacity75
    , hoverBgOrange100
    , hoverBgOrange200
    , hoverBgOrange300
    , hoverBgOrange400
    , hoverBgOrange500
    , hoverBgOrange600
    , hoverBgOrange700
    , hoverBgOrange800
    , hoverBgOrange900
    , hoverBgPink100
    , hoverBgPink200
    , hoverBgPink300
    , hoverBgPink400
    , hoverBgPink500
    , hoverBgPink600
    , hoverBgPink700
    , hoverBgPink800
    , hoverBgPink900
    , hoverBgPurple100
    , hoverBgPurple200
    , hoverBgPurple300
    , hoverBgPurple400
    , hoverBgPurple500
    , hoverBgPurple600
    , hoverBgPurple700
    , hoverBgPurple800
    , hoverBgPurple900
    , hoverBgRed100
    , hoverBgRed200
    , hoverBgRed300
    , hoverBgRed400
    , hoverBgRed500
    , hoverBgRed600
    , hoverBgRed700
    , hoverBgRed800
    , hoverBgRed900
    , hoverBgTeal100
    , hoverBgTeal200
    , hoverBgTeal300
    , hoverBgTeal400
    , hoverBgTeal500
    , hoverBgTeal600
    , hoverBgTeal700
    , hoverBgTeal800
    , hoverBgTeal900
    , hoverBgTransparent
    , hoverBgWhite
    , hoverBgYellow100
    , hoverBgYellow200
    , hoverBgYellow300
    , hoverBgYellow400
    , hoverBgYellow500
    , hoverBgYellow600
    , hoverBgYellow700
    , hoverBgYellow800
    , hoverBgYellow900
    , hoverBorderBlack
    , hoverBorderBlue100
    , hoverBorderBlue200
    , hoverBorderBlue300
    , hoverBorderBlue400
    , hoverBorderBlue500
    , hoverBorderBlue600
    , hoverBorderBlue700
    , hoverBorderBlue800
    , hoverBorderBlue900
    , hoverBorderCurrent
    , hoverBorderGray100
    , hoverBorderGray200
    , hoverBorderGray300
    , hoverBorderGray400
    , hoverBorderGray500
    , hoverBorderGray600
    , hoverBorderGray700
    , hoverBorderGray800
    , hoverBorderGray900
    , hoverBorderGreen100
    , hoverBorderGreen200
    , hoverBorderGreen300
    , hoverBorderGreen400
    , hoverBorderGreen500
    , hoverBorderGreen600
    , hoverBorderGreen700
    , hoverBorderGreen800
    , hoverBorderGreen900
    , hoverBorderIndigo100
    , hoverBorderIndigo200
    , hoverBorderIndigo300
    , hoverBorderIndigo400
    , hoverBorderIndigo500
    , hoverBorderIndigo600
    , hoverBorderIndigo700
    , hoverBorderIndigo800
    , hoverBorderIndigo900
    , hoverBorderOpacity0
    , hoverBorderOpacity100
    , hoverBorderOpacity25
    , hoverBorderOpacity50
    , hoverBorderOpacity75
    , hoverBorderOrange100
    , hoverBorderOrange200
    , hoverBorderOrange300
    , hoverBorderOrange400
    , hoverBorderOrange500
    , hoverBorderOrange600
    , hoverBorderOrange700
    , hoverBorderOrange800
    , hoverBorderOrange900
    , hoverBorderPink100
    , hoverBorderPink200
    , hoverBorderPink300
    , hoverBorderPink400
    , hoverBorderPink500
    , hoverBorderPink600
    , hoverBorderPink700
    , hoverBorderPink800
    , hoverBorderPink900
    , hoverBorderPurple100
    , hoverBorderPurple200
    , hoverBorderPurple300
    , hoverBorderPurple400
    , hoverBorderPurple500
    , hoverBorderPurple600
    , hoverBorderPurple700
    , hoverBorderPurple800
    , hoverBorderPurple900
    , hoverBorderRed100
    , hoverBorderRed200
    , hoverBorderRed300
    , hoverBorderRed400
    , hoverBorderRed500
    , hoverBorderRed600
    , hoverBorderRed700
    , hoverBorderRed800
    , hoverBorderRed900
    , hoverBorderTeal100
    , hoverBorderTeal200
    , hoverBorderTeal300
    , hoverBorderTeal400
    , hoverBorderTeal500
    , hoverBorderTeal600
    , hoverBorderTeal700
    , hoverBorderTeal800
    , hoverBorderTeal900
    , hoverBorderTransparent
    , hoverBorderWhite
    , hoverBorderYellow100
    , hoverBorderYellow200
    , hoverBorderYellow300
    , hoverBorderYellow400
    , hoverBorderYellow500
    , hoverBorderYellow600
    , hoverBorderYellow700
    , hoverBorderYellow800
    , hoverBorderYellow900
    , hoverFontBlack
    , hoverFontBold
    , hoverFontExtrabold
    , hoverFontHairline
    , hoverFontLight
    , hoverFontMedium
    , hoverFontNormal
    , hoverFontSemibold
    , hoverFontThin
    , hoverFromBlack
    , hoverFromBlue100
    , hoverFromBlue200
    , hoverFromBlue300
    , hoverFromBlue400
    , hoverFromBlue500
    , hoverFromBlue600
    , hoverFromBlue700
    , hoverFromBlue800
    , hoverFromBlue900
    , hoverFromCurrent
    , hoverFromGray100
    , hoverFromGray200
    , hoverFromGray300
    , hoverFromGray400
    , hoverFromGray500
    , hoverFromGray600
    , hoverFromGray700
    , hoverFromGray800
    , hoverFromGray900
    , hoverFromGreen100
    , hoverFromGreen200
    , hoverFromGreen300
    , hoverFromGreen400
    , hoverFromGreen500
    , hoverFromGreen600
    , hoverFromGreen700
    , hoverFromGreen800
    , hoverFromGreen900
    , hoverFromIndigo100
    , hoverFromIndigo200
    , hoverFromIndigo300
    , hoverFromIndigo400
    , hoverFromIndigo500
    , hoverFromIndigo600
    , hoverFromIndigo700
    , hoverFromIndigo800
    , hoverFromIndigo900
    , hoverFromOrange100
    , hoverFromOrange200
    , hoverFromOrange300
    , hoverFromOrange400
    , hoverFromOrange500
    , hoverFromOrange600
    , hoverFromOrange700
    , hoverFromOrange800
    , hoverFromOrange900
    , hoverFromPink100
    , hoverFromPink200
    , hoverFromPink300
    , hoverFromPink400
    , hoverFromPink500
    , hoverFromPink600
    , hoverFromPink700
    , hoverFromPink800
    , hoverFromPink900
    , hoverFromPurple100
    , hoverFromPurple200
    , hoverFromPurple300
    , hoverFromPurple400
    , hoverFromPurple500
    , hoverFromPurple600
    , hoverFromPurple700
    , hoverFromPurple800
    , hoverFromPurple900
    , hoverFromRed100
    , hoverFromRed200
    , hoverFromRed300
    , hoverFromRed400
    , hoverFromRed500
    , hoverFromRed600
    , hoverFromRed700
    , hoverFromRed800
    , hoverFromRed900
    , hoverFromTeal100
    , hoverFromTeal200
    , hoverFromTeal300
    , hoverFromTeal400
    , hoverFromTeal500
    , hoverFromTeal600
    , hoverFromTeal700
    , hoverFromTeal800
    , hoverFromTeal900
    , hoverFromTransparent
    , hoverFromWhite
    , hoverFromYellow100
    , hoverFromYellow200
    , hoverFromYellow300
    , hoverFromYellow400
    , hoverFromYellow500
    , hoverFromYellow600
    , hoverFromYellow700
    , hoverFromYellow800
    , hoverFromYellow900
    , hoverLineThrough
    , hoverNegRotate180
    , hoverNegRotate45
    , hoverNegRotate90
    , hoverNegSkewX12
    , hoverNegSkewX3
    , hoverNegSkewX6
    , hoverNegSkewY12
    , hoverNegSkewY3
    , hoverNegSkewY6
    , hoverNegTranslateX1
    , hoverNegTranslateX10
    , hoverNegTranslateX12
    , hoverNegTranslateX16
    , hoverNegTranslateX1over2
    , hoverNegTranslateX2
    , hoverNegTranslateX20
    , hoverNegTranslateX24
    , hoverNegTranslateX3
    , hoverNegTranslateX32
    , hoverNegTranslateX4
    , hoverNegTranslateX40
    , hoverNegTranslateX48
    , hoverNegTranslateX5
    , hoverNegTranslateX56
    , hoverNegTranslateX6
    , hoverNegTranslateX64
    , hoverNegTranslateX8
    , hoverNegTranslateXFull
    , hoverNegTranslateXPx
    , hoverNegTranslateY1
    , hoverNegTranslateY10
    , hoverNegTranslateY12
    , hoverNegTranslateY16
    , hoverNegTranslateY1over2
    , hoverNegTranslateY2
    , hoverNegTranslateY20
    , hoverNegTranslateY24
    , hoverNegTranslateY3
    , hoverNegTranslateY32
    , hoverNegTranslateY4
    , hoverNegTranslateY40
    , hoverNegTranslateY48
    , hoverNegTranslateY5
    , hoverNegTranslateY56
    , hoverNegTranslateY6
    , hoverNegTranslateY64
    , hoverNegTranslateY8
    , hoverNegTranslateYFull
    , hoverNegTranslateYPx
    , hoverNoUnderline
    , hoverOpacity0
    , hoverOpacity100
    , hoverOpacity25
    , hoverOpacity50
    , hoverOpacity75
    , hoverRotate0
    , hoverRotate180
    , hoverRotate45
    , hoverRotate90
    , hoverScale0
    , hoverScale100
    , hoverScale105
    , hoverScale110
    , hoverScale125
    , hoverScale150
    , hoverScale50
    , hoverScale75
    , hoverScale90
    , hoverScale95
    , hoverScaleX0
    , hoverScaleX100
    , hoverScaleX105
    , hoverScaleX110
    , hoverScaleX125
    , hoverScaleX150
    , hoverScaleX50
    , hoverScaleX75
    , hoverScaleX90
    , hoverScaleX95
    , hoverScaleY0
    , hoverScaleY100
    , hoverScaleY105
    , hoverScaleY110
    , hoverScaleY125
    , hoverScaleY150
    , hoverScaleY50
    , hoverScaleY75
    , hoverScaleY90
    , hoverScaleY95
    , hoverShadow
    , hoverShadow2xl
    , hoverShadowInner
    , hoverShadowLg
    , hoverShadowMd
    , hoverShadowNone
    , hoverShadowOutline
    , hoverShadowSm
    , hoverShadowXl
    , hoverShadowXs
    , hoverSkewX0
    , hoverSkewX12
    , hoverSkewX3
    , hoverSkewX6
    , hoverSkewY0
    , hoverSkewY12
    , hoverSkewY3
    , hoverSkewY6
    , hoverTextBlack
    , hoverTextBlue100
    , hoverTextBlue200
    , hoverTextBlue300
    , hoverTextBlue400
    , hoverTextBlue500
    , hoverTextBlue600
    , hoverTextBlue700
    , hoverTextBlue800
    , hoverTextBlue900
    , hoverTextCurrent
    , hoverTextGray100
    , hoverTextGray200
    , hoverTextGray300
    , hoverTextGray400
    , hoverTextGray500
    , hoverTextGray600
    , hoverTextGray700
    , hoverTextGray800
    , hoverTextGray900
    , hoverTextGreen100
    , hoverTextGreen200
    , hoverTextGreen300
    , hoverTextGreen400
    , hoverTextGreen500
    , hoverTextGreen600
    , hoverTextGreen700
    , hoverTextGreen800
    , hoverTextGreen900
    , hoverTextIndigo100
    , hoverTextIndigo200
    , hoverTextIndigo300
    , hoverTextIndigo400
    , hoverTextIndigo500
    , hoverTextIndigo600
    , hoverTextIndigo700
    , hoverTextIndigo800
    , hoverTextIndigo900
    , hoverTextOpacity0
    , hoverTextOpacity100
    , hoverTextOpacity25
    , hoverTextOpacity50
    , hoverTextOpacity75
    , hoverTextOrange100
    , hoverTextOrange200
    , hoverTextOrange300
    , hoverTextOrange400
    , hoverTextOrange500
    , hoverTextOrange600
    , hoverTextOrange700
    , hoverTextOrange800
    , hoverTextOrange900
    , hoverTextPink100
    , hoverTextPink200
    , hoverTextPink300
    , hoverTextPink400
    , hoverTextPink500
    , hoverTextPink600
    , hoverTextPink700
    , hoverTextPink800
    , hoverTextPink900
    , hoverTextPurple100
    , hoverTextPurple200
    , hoverTextPurple300
    , hoverTextPurple400
    , hoverTextPurple500
    , hoverTextPurple600
    , hoverTextPurple700
    , hoverTextPurple800
    , hoverTextPurple900
    , hoverTextRed100
    , hoverTextRed200
    , hoverTextRed300
    , hoverTextRed400
    , hoverTextRed500
    , hoverTextRed600
    , hoverTextRed700
    , hoverTextRed800
    , hoverTextRed900
    , hoverTextTeal100
    , hoverTextTeal200
    , hoverTextTeal300
    , hoverTextTeal400
    , hoverTextTeal500
    , hoverTextTeal600
    , hoverTextTeal700
    , hoverTextTeal800
    , hoverTextTeal900
    , hoverTextTransparent
    , hoverTextWhite
    , hoverTextYellow100
    , hoverTextYellow200
    , hoverTextYellow300
    , hoverTextYellow400
    , hoverTextYellow500
    , hoverTextYellow600
    , hoverTextYellow700
    , hoverTextYellow800
    , hoverTextYellow900
    , hoverToBlack
    , hoverToBlue100
    , hoverToBlue200
    , hoverToBlue300
    , hoverToBlue400
    , hoverToBlue500
    , hoverToBlue600
    , hoverToBlue700
    , hoverToBlue800
    , hoverToBlue900
    , hoverToCurrent
    , hoverToGray100
    , hoverToGray200
    , hoverToGray300
    , hoverToGray400
    , hoverToGray500
    , hoverToGray600
    , hoverToGray700
    , hoverToGray800
    , hoverToGray900
    , hoverToGreen100
    , hoverToGreen200
    , hoverToGreen300
    , hoverToGreen400
    , hoverToGreen500
    , hoverToGreen600
    , hoverToGreen700
    , hoverToGreen800
    , hoverToGreen900
    , hoverToIndigo100
    , hoverToIndigo200
    , hoverToIndigo300
    , hoverToIndigo400
    , hoverToIndigo500
    , hoverToIndigo600
    , hoverToIndigo700
    , hoverToIndigo800
    , hoverToIndigo900
    , hoverToOrange100
    , hoverToOrange200
    , hoverToOrange300
    , hoverToOrange400
    , hoverToOrange500
    , hoverToOrange600
    , hoverToOrange700
    , hoverToOrange800
    , hoverToOrange900
    , hoverToPink100
    , hoverToPink200
    , hoverToPink300
    , hoverToPink400
    , hoverToPink500
    , hoverToPink600
    , hoverToPink700
    , hoverToPink800
    , hoverToPink900
    , hoverToPurple100
    , hoverToPurple200
    , hoverToPurple300
    , hoverToPurple400
    , hoverToPurple500
    , hoverToPurple600
    , hoverToPurple700
    , hoverToPurple800
    , hoverToPurple900
    , hoverToRed100
    , hoverToRed200
    , hoverToRed300
    , hoverToRed400
    , hoverToRed500
    , hoverToRed600
    , hoverToRed700
    , hoverToRed800
    , hoverToRed900
    , hoverToTeal100
    , hoverToTeal200
    , hoverToTeal300
    , hoverToTeal400
    , hoverToTeal500
    , hoverToTeal600
    , hoverToTeal700
    , hoverToTeal800
    , hoverToTeal900
    , hoverToTransparent
    , hoverToWhite
    , hoverToYellow100
    , hoverToYellow200
    , hoverToYellow300
    , hoverToYellow400
    , hoverToYellow500
    , hoverToYellow600
    , hoverToYellow700
    , hoverToYellow800
    , hoverToYellow900
    , hoverTranslateX0
    , hoverTranslateX1
    , hoverTranslateX10
    , hoverTranslateX12
    , hoverTranslateX16
    , hoverTranslateX1over2
    , hoverTranslateX2
    , hoverTranslateX20
    , hoverTranslateX24
    , hoverTranslateX3
    , hoverTranslateX32
    , hoverTranslateX4
    , hoverTranslateX40
    , hoverTranslateX48
    , hoverTranslateX5
    , hoverTranslateX56
    , hoverTranslateX6
    , hoverTranslateX64
    , hoverTranslateX8
    , hoverTranslateXFull
    , hoverTranslateXPx
    , hoverTranslateY0
    , hoverTranslateY1
    , hoverTranslateY10
    , hoverTranslateY12
    , hoverTranslateY16
    , hoverTranslateY1over2
    , hoverTranslateY2
    , hoverTranslateY20
    , hoverTranslateY24
    , hoverTranslateY3
    , hoverTranslateY32
    , hoverTranslateY4
    , hoverTranslateY40
    , hoverTranslateY48
    , hoverTranslateY5
    , hoverTranslateY56
    , hoverTranslateY6
    , hoverTranslateY64
    , hoverTranslateY8
    , hoverTranslateYFull
    , hoverTranslateYPx
    , hoverUnderline
    , hoverViaBlack
    , hoverViaBlue100
    , hoverViaBlue200
    , hoverViaBlue300
    , hoverViaBlue400
    , hoverViaBlue500
    , hoverViaBlue600
    , hoverViaBlue700
    , hoverViaBlue800
    , hoverViaBlue900
    , hoverViaCurrent
    , hoverViaGray100
    , hoverViaGray200
    , hoverViaGray300
    , hoverViaGray400
    , hoverViaGray500
    , hoverViaGray600
    , hoverViaGray700
    , hoverViaGray800
    , hoverViaGray900
    , hoverViaGreen100
    , hoverViaGreen200
    , hoverViaGreen300
    , hoverViaGreen400
    , hoverViaGreen500
    , hoverViaGreen600
    , hoverViaGreen700
    , hoverViaGreen800
    , hoverViaGreen900
    , hoverViaIndigo100
    , hoverViaIndigo200
    , hoverViaIndigo300
    , hoverViaIndigo400
    , hoverViaIndigo500
    , hoverViaIndigo600
    , hoverViaIndigo700
    , hoverViaIndigo800
    , hoverViaIndigo900
    , hoverViaOrange100
    , hoverViaOrange200
    , hoverViaOrange300
    , hoverViaOrange400
    , hoverViaOrange500
    , hoverViaOrange600
    , hoverViaOrange700
    , hoverViaOrange800
    , hoverViaOrange900
    , hoverViaPink100
    , hoverViaPink200
    , hoverViaPink300
    , hoverViaPink400
    , hoverViaPink500
    , hoverViaPink600
    , hoverViaPink700
    , hoverViaPink800
    , hoverViaPink900
    , hoverViaPurple100
    , hoverViaPurple200
    , hoverViaPurple300
    , hoverViaPurple400
    , hoverViaPurple500
    , hoverViaPurple600
    , hoverViaPurple700
    , hoverViaPurple800
    , hoverViaPurple900
    , hoverViaRed100
    , hoverViaRed200
    , hoverViaRed300
    , hoverViaRed400
    , hoverViaRed500
    , hoverViaRed600
    , hoverViaRed700
    , hoverViaRed800
    , hoverViaRed900
    , hoverViaTeal100
    , hoverViaTeal200
    , hoverViaTeal300
    , hoverViaTeal400
    , hoverViaTeal500
    , hoverViaTeal600
    , hoverViaTeal700
    , hoverViaTeal800
    , hoverViaTeal900
    , hoverViaTransparent
    , hoverViaWhite
    , hoverViaYellow100
    , hoverViaYellow200
    , hoverViaYellow300
    , hoverViaYellow400
    , hoverViaYellow500
    , hoverViaYellow600
    , hoverViaYellow700
    , hoverViaYellow800
    , hoverViaYellow900
    , inline
    , inlineBlock
    , inlineFlex
    , inlineGrid
    , inset0
    , insetAuto
    , insetX0
    , insetXAuto
    , insetY0
    , insetYAuto
    , invisible
    , italic
    , itemsBaseline
    , itemsCenter
    , itemsEnd
    , itemsStart
    , itemsStretch
    , justifyAround
    , justifyBetween
    , justifyCenter
    , justifyEnd
    , justifyEvenly
    , justifyItemsAuto
    , justifyItemsCenter
    , justifyItemsEnd
    , justifyItemsStart
    , justifyItemsStretch
    , justifySelfAuto
    , justifySelfCenter
    , justifySelfEnd
    , justifySelfStart
    , justifySelfStretch
    , justifyStart
    , leading10
    , leading3
    , leading4
    , leading5
    , leading6
    , leading7
    , leading8
    , leading9
    , leadingLoose
    , leadingNone
    , leadingNormal
    , leadingRelaxed
    , leadingSnug
    , leadingTight
    , left0
    , leftAuto
    , lgAbsolute
    , lgAlignBaseline
    , lgAlignBottom
    , lgAlignMiddle
    , lgAlignTextBottom
    , lgAlignTextTop
    , lgAlignTop
    , lgAnimateBounce
    , lgAnimateNone
    , lgAnimatePing
    , lgAnimatePulse
    , lgAnimateSpin
    , lgAntialiased
    , lgAppearanceNone
    , lgBgAuto
    , lgBgBlack
    , lgBgBlue100
    , lgBgBlue200
    , lgBgBlue300
    , lgBgBlue400
    , lgBgBlue500
    , lgBgBlue600
    , lgBgBlue700
    , lgBgBlue800
    , lgBgBlue900
    , lgBgBottom
    , lgBgCenter
    , lgBgClipBorder
    , lgBgClipContent
    , lgBgClipPadding
    , lgBgClipText
    , lgBgContain
    , lgBgCover
    , lgBgCurrent
    , lgBgFixed
    , lgBgGradientToB
    , lgBgGradientToBl
    , lgBgGradientToBr
    , lgBgGradientToL
    , lgBgGradientToR
    , lgBgGradientToT
    , lgBgGradientToTl
    , lgBgGradientToTr
    , lgBgGray100
    , lgBgGray200
    , lgBgGray300
    , lgBgGray400
    , lgBgGray500
    , lgBgGray600
    , lgBgGray700
    , lgBgGray800
    , lgBgGray900
    , lgBgGreen100
    , lgBgGreen200
    , lgBgGreen300
    , lgBgGreen400
    , lgBgGreen500
    , lgBgGreen600
    , lgBgGreen700
    , lgBgGreen800
    , lgBgGreen900
    , lgBgIndigo100
    , lgBgIndigo200
    , lgBgIndigo300
    , lgBgIndigo400
    , lgBgIndigo500
    , lgBgIndigo600
    , lgBgIndigo700
    , lgBgIndigo800
    , lgBgIndigo900
    , lgBgLeft
    , lgBgLeftBottom
    , lgBgLeftTop
    , lgBgLocal
    , lgBgNoRepeat
    , lgBgNone
    , lgBgOpacity0
    , lgBgOpacity100
    , lgBgOpacity25
    , lgBgOpacity50
    , lgBgOpacity75
    , lgBgOrange100
    , lgBgOrange200
    , lgBgOrange300
    , lgBgOrange400
    , lgBgOrange500
    , lgBgOrange600
    , lgBgOrange700
    , lgBgOrange800
    , lgBgOrange900
    , lgBgPink100
    , lgBgPink200
    , lgBgPink300
    , lgBgPink400
    , lgBgPink500
    , lgBgPink600
    , lgBgPink700
    , lgBgPink800
    , lgBgPink900
    , lgBgPurple100
    , lgBgPurple200
    , lgBgPurple300
    , lgBgPurple400
    , lgBgPurple500
    , lgBgPurple600
    , lgBgPurple700
    , lgBgPurple800
    , lgBgPurple900
    , lgBgRed100
    , lgBgRed200
    , lgBgRed300
    , lgBgRed400
    , lgBgRed500
    , lgBgRed600
    , lgBgRed700
    , lgBgRed800
    , lgBgRed900
    , lgBgRepeat
    , lgBgRepeatRound
    , lgBgRepeatSpace
    , lgBgRepeatX
    , lgBgRepeatY
    , lgBgRight
    , lgBgRightBottom
    , lgBgRightTop
    , lgBgScroll
    , lgBgTeal100
    , lgBgTeal200
    , lgBgTeal300
    , lgBgTeal400
    , lgBgTeal500
    , lgBgTeal600
    , lgBgTeal700
    , lgBgTeal800
    , lgBgTeal900
    , lgBgTop
    , lgBgTransparent
    , lgBgWhite
    , lgBgYellow100
    , lgBgYellow200
    , lgBgYellow300
    , lgBgYellow400
    , lgBgYellow500
    , lgBgYellow600
    , lgBgYellow700
    , lgBgYellow800
    , lgBgYellow900
    , lgBlock
    , lgBorder
    , lgBorder0
    , lgBorder2
    , lgBorder4
    , lgBorder8
    , lgBorderB
    , lgBorderB0
    , lgBorderB2
    , lgBorderB4
    , lgBorderB8
    , lgBorderBlack
    , lgBorderBlue100
    , lgBorderBlue200
    , lgBorderBlue300
    , lgBorderBlue400
    , lgBorderBlue500
    , lgBorderBlue600
    , lgBorderBlue700
    , lgBorderBlue800
    , lgBorderBlue900
    , lgBorderCollapse
    , lgBorderCurrent
    , lgBorderDashed
    , lgBorderDotted
    , lgBorderDouble
    , lgBorderGray100
    , lgBorderGray200
    , lgBorderGray300
    , lgBorderGray400
    , lgBorderGray500
    , lgBorderGray600
    , lgBorderGray700
    , lgBorderGray800
    , lgBorderGray900
    , lgBorderGreen100
    , lgBorderGreen200
    , lgBorderGreen300
    , lgBorderGreen400
    , lgBorderGreen500
    , lgBorderGreen600
    , lgBorderGreen700
    , lgBorderGreen800
    , lgBorderGreen900
    , lgBorderIndigo100
    , lgBorderIndigo200
    , lgBorderIndigo300
    , lgBorderIndigo400
    , lgBorderIndigo500
    , lgBorderIndigo600
    , lgBorderIndigo700
    , lgBorderIndigo800
    , lgBorderIndigo900
    , lgBorderL
    , lgBorderL0
    , lgBorderL2
    , lgBorderL4
    , lgBorderL8
    , lgBorderNone
    , lgBorderOpacity0
    , lgBorderOpacity100
    , lgBorderOpacity25
    , lgBorderOpacity50
    , lgBorderOpacity75
    , lgBorderOrange100
    , lgBorderOrange200
    , lgBorderOrange300
    , lgBorderOrange400
    , lgBorderOrange500
    , lgBorderOrange600
    , lgBorderOrange700
    , lgBorderOrange800
    , lgBorderOrange900
    , lgBorderPink100
    , lgBorderPink200
    , lgBorderPink300
    , lgBorderPink400
    , lgBorderPink500
    , lgBorderPink600
    , lgBorderPink700
    , lgBorderPink800
    , lgBorderPink900
    , lgBorderPurple100
    , lgBorderPurple200
    , lgBorderPurple300
    , lgBorderPurple400
    , lgBorderPurple500
    , lgBorderPurple600
    , lgBorderPurple700
    , lgBorderPurple800
    , lgBorderPurple900
    , lgBorderR
    , lgBorderR0
    , lgBorderR2
    , lgBorderR4
    , lgBorderR8
    , lgBorderRed100
    , lgBorderRed200
    , lgBorderRed300
    , lgBorderRed400
    , lgBorderRed500
    , lgBorderRed600
    , lgBorderRed700
    , lgBorderRed800
    , lgBorderRed900
    , lgBorderSeparate
    , lgBorderSolid
    , lgBorderT
    , lgBorderT0
    , lgBorderT2
    , lgBorderT4
    , lgBorderT8
    , lgBorderTeal100
    , lgBorderTeal200
    , lgBorderTeal300
    , lgBorderTeal400
    , lgBorderTeal500
    , lgBorderTeal600
    , lgBorderTeal700
    , lgBorderTeal800
    , lgBorderTeal900
    , lgBorderTransparent
    , lgBorderWhite
    , lgBorderYellow100
    , lgBorderYellow200
    , lgBorderYellow300
    , lgBorderYellow400
    , lgBorderYellow500
    , lgBorderYellow600
    , lgBorderYellow700
    , lgBorderYellow800
    , lgBorderYellow900
    , lgBottom0
    , lgBottomAuto
    , lgBoxBorder
    , lgBoxContent
    , lgBreakAll
    , lgBreakNormal
    , lgBreakWords
    , lgCapitalize
    , lgClearBoth
    , lgClearLeft
    , lgClearNone
    , lgClearRight
    , lgClearfixAfter
    , lgColAuto
    , lgColEnd1
    , lgColEnd10
    , lgColEnd11
    , lgColEnd12
    , lgColEnd13
    , lgColEnd2
    , lgColEnd3
    , lgColEnd4
    , lgColEnd5
    , lgColEnd6
    , lgColEnd7
    , lgColEnd8
    , lgColEnd9
    , lgColEndAuto
    , lgColSpan1
    , lgColSpan10
    , lgColSpan11
    , lgColSpan12
    , lgColSpan2
    , lgColSpan3
    , lgColSpan4
    , lgColSpan5
    , lgColSpan6
    , lgColSpan7
    , lgColSpan8
    , lgColSpan9
    , lgColStart1
    , lgColStart10
    , lgColStart11
    , lgColStart12
    , lgColStart13
    , lgColStart2
    , lgColStart3
    , lgColStart4
    , lgColStart5
    , lgColStart6
    , lgColStart7
    , lgColStart8
    , lgColStart9
    , lgColStartAuto
    , lgContainer
    , lgContentAround
    , lgContentBetween
    , lgContentCenter
    , lgContentEnd
    , lgContentEvenly
    , lgContentStart
    , lgContents
    , lgCursorAuto
    , lgCursorDefault
    , lgCursorMove
    , lgCursorNotAllowed
    , lgCursorPointer
    , lgCursorText
    , lgCursorWait
    , lgDelay100
    , lgDelay1000
    , lgDelay150
    , lgDelay200
    , lgDelay300
    , lgDelay500
    , lgDelay700
    , lgDelay75
    , lgDiagonalFractions
    , lgDivideBlack
    , lgDivideBlue100
    , lgDivideBlue200
    , lgDivideBlue300
    , lgDivideBlue400
    , lgDivideBlue500
    , lgDivideBlue600
    , lgDivideBlue700
    , lgDivideBlue800
    , lgDivideBlue900
    , lgDivideCurrent
    , lgDivideDashed
    , lgDivideDotted
    , lgDivideDouble
    , lgDivideGray100
    , lgDivideGray200
    , lgDivideGray300
    , lgDivideGray400
    , lgDivideGray500
    , lgDivideGray600
    , lgDivideGray700
    , lgDivideGray800
    , lgDivideGray900
    , lgDivideGreen100
    , lgDivideGreen200
    , lgDivideGreen300
    , lgDivideGreen400
    , lgDivideGreen500
    , lgDivideGreen600
    , lgDivideGreen700
    , lgDivideGreen800
    , lgDivideGreen900
    , lgDivideIndigo100
    , lgDivideIndigo200
    , lgDivideIndigo300
    , lgDivideIndigo400
    , lgDivideIndigo500
    , lgDivideIndigo600
    , lgDivideIndigo700
    , lgDivideIndigo800
    , lgDivideIndigo900
    , lgDivideNone
    , lgDivideOpacity0
    , lgDivideOpacity100
    , lgDivideOpacity25
    , lgDivideOpacity50
    , lgDivideOpacity75
    , lgDivideOrange100
    , lgDivideOrange200
    , lgDivideOrange300
    , lgDivideOrange400
    , lgDivideOrange500
    , lgDivideOrange600
    , lgDivideOrange700
    , lgDivideOrange800
    , lgDivideOrange900
    , lgDividePink100
    , lgDividePink200
    , lgDividePink300
    , lgDividePink400
    , lgDividePink500
    , lgDividePink600
    , lgDividePink700
    , lgDividePink800
    , lgDividePink900
    , lgDividePurple100
    , lgDividePurple200
    , lgDividePurple300
    , lgDividePurple400
    , lgDividePurple500
    , lgDividePurple600
    , lgDividePurple700
    , lgDividePurple800
    , lgDividePurple900
    , lgDivideRed100
    , lgDivideRed200
    , lgDivideRed300
    , lgDivideRed400
    , lgDivideRed500
    , lgDivideRed600
    , lgDivideRed700
    , lgDivideRed800
    , lgDivideRed900
    , lgDivideSolid
    , lgDivideTeal100
    , lgDivideTeal200
    , lgDivideTeal300
    , lgDivideTeal400
    , lgDivideTeal500
    , lgDivideTeal600
    , lgDivideTeal700
    , lgDivideTeal800
    , lgDivideTeal900
    , lgDivideTransparent
    , lgDivideWhite
    , lgDivideX
    , lgDivideX0
    , lgDivideX2
    , lgDivideX4
    , lgDivideX8
    , lgDivideXReverse
    , lgDivideY
    , lgDivideY0
    , lgDivideY2
    , lgDivideY4
    , lgDivideY8
    , lgDivideYReverse
    , lgDivideYellow100
    , lgDivideYellow200
    , lgDivideYellow300
    , lgDivideYellow400
    , lgDivideYellow500
    , lgDivideYellow600
    , lgDivideYellow700
    , lgDivideYellow800
    , lgDivideYellow900
    , lgDuration100
    , lgDuration1000
    , lgDuration150
    , lgDuration200
    , lgDuration300
    , lgDuration500
    , lgDuration700
    , lgDuration75
    , lgEaseIn
    , lgEaseInOut
    , lgEaseLinear
    , lgEaseOut
    , lgFillCurrent
    , lgFixed
    , lgFlex
    , lgFlex1
    , lgFlexAuto
    , lgFlexCol
    , lgFlexColReverse
    , lgFlexGrow
    , lgFlexGrow0
    , lgFlexInitial
    , lgFlexNoWrap
    , lgFlexNone
    , lgFlexRow
    , lgFlexRowReverse
    , lgFlexShrink
    , lgFlexShrink0
    , lgFlexWrap
    , lgFlexWrapReverse
    , lgFloatLeft
    , lgFloatNone
    , lgFloatRight
    , lgFlowRoot
    , lgFocusBgBlack
    , lgFocusBgBlue100
    , lgFocusBgBlue200
    , lgFocusBgBlue300
    , lgFocusBgBlue400
    , lgFocusBgBlue500
    , lgFocusBgBlue600
    , lgFocusBgBlue700
    , lgFocusBgBlue800
    , lgFocusBgBlue900
    , lgFocusBgCurrent
    , lgFocusBgGray100
    , lgFocusBgGray200
    , lgFocusBgGray300
    , lgFocusBgGray400
    , lgFocusBgGray500
    , lgFocusBgGray600
    , lgFocusBgGray700
    , lgFocusBgGray800
    , lgFocusBgGray900
    , lgFocusBgGreen100
    , lgFocusBgGreen200
    , lgFocusBgGreen300
    , lgFocusBgGreen400
    , lgFocusBgGreen500
    , lgFocusBgGreen600
    , lgFocusBgGreen700
    , lgFocusBgGreen800
    , lgFocusBgGreen900
    , lgFocusBgIndigo100
    , lgFocusBgIndigo200
    , lgFocusBgIndigo300
    , lgFocusBgIndigo400
    , lgFocusBgIndigo500
    , lgFocusBgIndigo600
    , lgFocusBgIndigo700
    , lgFocusBgIndigo800
    , lgFocusBgIndigo900
    , lgFocusBgOpacity0
    , lgFocusBgOpacity100
    , lgFocusBgOpacity25
    , lgFocusBgOpacity50
    , lgFocusBgOpacity75
    , lgFocusBgOrange100
    , lgFocusBgOrange200
    , lgFocusBgOrange300
    , lgFocusBgOrange400
    , lgFocusBgOrange500
    , lgFocusBgOrange600
    , lgFocusBgOrange700
    , lgFocusBgOrange800
    , lgFocusBgOrange900
    , lgFocusBgPink100
    , lgFocusBgPink200
    , lgFocusBgPink300
    , lgFocusBgPink400
    , lgFocusBgPink500
    , lgFocusBgPink600
    , lgFocusBgPink700
    , lgFocusBgPink800
    , lgFocusBgPink900
    , lgFocusBgPurple100
    , lgFocusBgPurple200
    , lgFocusBgPurple300
    , lgFocusBgPurple400
    , lgFocusBgPurple500
    , lgFocusBgPurple600
    , lgFocusBgPurple700
    , lgFocusBgPurple800
    , lgFocusBgPurple900
    , lgFocusBgRed100
    , lgFocusBgRed200
    , lgFocusBgRed300
    , lgFocusBgRed400
    , lgFocusBgRed500
    , lgFocusBgRed600
    , lgFocusBgRed700
    , lgFocusBgRed800
    , lgFocusBgRed900
    , lgFocusBgTeal100
    , lgFocusBgTeal200
    , lgFocusBgTeal300
    , lgFocusBgTeal400
    , lgFocusBgTeal500
    , lgFocusBgTeal600
    , lgFocusBgTeal700
    , lgFocusBgTeal800
    , lgFocusBgTeal900
    , lgFocusBgTransparent
    , lgFocusBgWhite
    , lgFocusBgYellow100
    , lgFocusBgYellow200
    , lgFocusBgYellow300
    , lgFocusBgYellow400
    , lgFocusBgYellow500
    , lgFocusBgYellow600
    , lgFocusBgYellow700
    , lgFocusBgYellow800
    , lgFocusBgYellow900
    , lgFocusBorderBlack
    , lgFocusBorderBlue100
    , lgFocusBorderBlue200
    , lgFocusBorderBlue300
    , lgFocusBorderBlue400
    , lgFocusBorderBlue500
    , lgFocusBorderBlue600
    , lgFocusBorderBlue700
    , lgFocusBorderBlue800
    , lgFocusBorderBlue900
    , lgFocusBorderCurrent
    , lgFocusBorderGray100
    , lgFocusBorderGray200
    , lgFocusBorderGray300
    , lgFocusBorderGray400
    , lgFocusBorderGray500
    , lgFocusBorderGray600
    , lgFocusBorderGray700
    , lgFocusBorderGray800
    , lgFocusBorderGray900
    , lgFocusBorderGreen100
    , lgFocusBorderGreen200
    , lgFocusBorderGreen300
    , lgFocusBorderGreen400
    , lgFocusBorderGreen500
    , lgFocusBorderGreen600
    , lgFocusBorderGreen700
    , lgFocusBorderGreen800
    , lgFocusBorderGreen900
    , lgFocusBorderIndigo100
    , lgFocusBorderIndigo200
    , lgFocusBorderIndigo300
    , lgFocusBorderIndigo400
    , lgFocusBorderIndigo500
    , lgFocusBorderIndigo600
    , lgFocusBorderIndigo700
    , lgFocusBorderIndigo800
    , lgFocusBorderIndigo900
    , lgFocusBorderOpacity0
    , lgFocusBorderOpacity100
    , lgFocusBorderOpacity25
    , lgFocusBorderOpacity50
    , lgFocusBorderOpacity75
    , lgFocusBorderOrange100
    , lgFocusBorderOrange200
    , lgFocusBorderOrange300
    , lgFocusBorderOrange400
    , lgFocusBorderOrange500
    , lgFocusBorderOrange600
    , lgFocusBorderOrange700
    , lgFocusBorderOrange800
    , lgFocusBorderOrange900
    , lgFocusBorderPink100
    , lgFocusBorderPink200
    , lgFocusBorderPink300
    , lgFocusBorderPink400
    , lgFocusBorderPink500
    , lgFocusBorderPink600
    , lgFocusBorderPink700
    , lgFocusBorderPink800
    , lgFocusBorderPink900
    , lgFocusBorderPurple100
    , lgFocusBorderPurple200
    , lgFocusBorderPurple300
    , lgFocusBorderPurple400
    , lgFocusBorderPurple500
    , lgFocusBorderPurple600
    , lgFocusBorderPurple700
    , lgFocusBorderPurple800
    , lgFocusBorderPurple900
    , lgFocusBorderRed100
    , lgFocusBorderRed200
    , lgFocusBorderRed300
    , lgFocusBorderRed400
    , lgFocusBorderRed500
    , lgFocusBorderRed600
    , lgFocusBorderRed700
    , lgFocusBorderRed800
    , lgFocusBorderRed900
    , lgFocusBorderTeal100
    , lgFocusBorderTeal200
    , lgFocusBorderTeal300
    , lgFocusBorderTeal400
    , lgFocusBorderTeal500
    , lgFocusBorderTeal600
    , lgFocusBorderTeal700
    , lgFocusBorderTeal800
    , lgFocusBorderTeal900
    , lgFocusBorderTransparent
    , lgFocusBorderWhite
    , lgFocusBorderYellow100
    , lgFocusBorderYellow200
    , lgFocusBorderYellow300
    , lgFocusBorderYellow400
    , lgFocusBorderYellow500
    , lgFocusBorderYellow600
    , lgFocusBorderYellow700
    , lgFocusBorderYellow800
    , lgFocusBorderYellow900
    , lgFocusFontBlack
    , lgFocusFontBold
    , lgFocusFontExtrabold
    , lgFocusFontHairline
    , lgFocusFontLight
    , lgFocusFontMedium
    , lgFocusFontNormal
    , lgFocusFontSemibold
    , lgFocusFontThin
    , lgFocusFromBlack
    , lgFocusFromBlue100
    , lgFocusFromBlue200
    , lgFocusFromBlue300
    , lgFocusFromBlue400
    , lgFocusFromBlue500
    , lgFocusFromBlue600
    , lgFocusFromBlue700
    , lgFocusFromBlue800
    , lgFocusFromBlue900
    , lgFocusFromCurrent
    , lgFocusFromGray100
    , lgFocusFromGray200
    , lgFocusFromGray300
    , lgFocusFromGray400
    , lgFocusFromGray500
    , lgFocusFromGray600
    , lgFocusFromGray700
    , lgFocusFromGray800
    , lgFocusFromGray900
    , lgFocusFromGreen100
    , lgFocusFromGreen200
    , lgFocusFromGreen300
    , lgFocusFromGreen400
    , lgFocusFromGreen500
    , lgFocusFromGreen600
    , lgFocusFromGreen700
    , lgFocusFromGreen800
    , lgFocusFromGreen900
    , lgFocusFromIndigo100
    , lgFocusFromIndigo200
    , lgFocusFromIndigo300
    , lgFocusFromIndigo400
    , lgFocusFromIndigo500
    , lgFocusFromIndigo600
    , lgFocusFromIndigo700
    , lgFocusFromIndigo800
    , lgFocusFromIndigo900
    , lgFocusFromOrange100
    , lgFocusFromOrange200
    , lgFocusFromOrange300
    , lgFocusFromOrange400
    , lgFocusFromOrange500
    , lgFocusFromOrange600
    , lgFocusFromOrange700
    , lgFocusFromOrange800
    , lgFocusFromOrange900
    , lgFocusFromPink100
    , lgFocusFromPink200
    , lgFocusFromPink300
    , lgFocusFromPink400
    , lgFocusFromPink500
    , lgFocusFromPink600
    , lgFocusFromPink700
    , lgFocusFromPink800
    , lgFocusFromPink900
    , lgFocusFromPurple100
    , lgFocusFromPurple200
    , lgFocusFromPurple300
    , lgFocusFromPurple400
    , lgFocusFromPurple500
    , lgFocusFromPurple600
    , lgFocusFromPurple700
    , lgFocusFromPurple800
    , lgFocusFromPurple900
    , lgFocusFromRed100
    , lgFocusFromRed200
    , lgFocusFromRed300
    , lgFocusFromRed400
    , lgFocusFromRed500
    , lgFocusFromRed600
    , lgFocusFromRed700
    , lgFocusFromRed800
    , lgFocusFromRed900
    , lgFocusFromTeal100
    , lgFocusFromTeal200
    , lgFocusFromTeal300
    , lgFocusFromTeal400
    , lgFocusFromTeal500
    , lgFocusFromTeal600
    , lgFocusFromTeal700
    , lgFocusFromTeal800
    , lgFocusFromTeal900
    , lgFocusFromTransparent
    , lgFocusFromWhite
    , lgFocusFromYellow100
    , lgFocusFromYellow200
    , lgFocusFromYellow300
    , lgFocusFromYellow400
    , lgFocusFromYellow500
    , lgFocusFromYellow600
    , lgFocusFromYellow700
    , lgFocusFromYellow800
    , lgFocusFromYellow900
    , lgFocusLineThrough
    , lgFocusNegRotate180
    , lgFocusNegRotate45
    , lgFocusNegRotate90
    , lgFocusNegSkewX12
    , lgFocusNegSkewX3
    , lgFocusNegSkewX6
    , lgFocusNegSkewY12
    , lgFocusNegSkewY3
    , lgFocusNegSkewY6
    , lgFocusNegTranslateX1
    , lgFocusNegTranslateX10
    , lgFocusNegTranslateX12
    , lgFocusNegTranslateX16
    , lgFocusNegTranslateX1over2
    , lgFocusNegTranslateX2
    , lgFocusNegTranslateX20
    , lgFocusNegTranslateX24
    , lgFocusNegTranslateX3
    , lgFocusNegTranslateX32
    , lgFocusNegTranslateX4
    , lgFocusNegTranslateX40
    , lgFocusNegTranslateX48
    , lgFocusNegTranslateX5
    , lgFocusNegTranslateX56
    , lgFocusNegTranslateX6
    , lgFocusNegTranslateX64
    , lgFocusNegTranslateX8
    , lgFocusNegTranslateXFull
    , lgFocusNegTranslateXPx
    , lgFocusNegTranslateY1
    , lgFocusNegTranslateY10
    , lgFocusNegTranslateY12
    , lgFocusNegTranslateY16
    , lgFocusNegTranslateY1over2
    , lgFocusNegTranslateY2
    , lgFocusNegTranslateY20
    , lgFocusNegTranslateY24
    , lgFocusNegTranslateY3
    , lgFocusNegTranslateY32
    , lgFocusNegTranslateY4
    , lgFocusNegTranslateY40
    , lgFocusNegTranslateY48
    , lgFocusNegTranslateY5
    , lgFocusNegTranslateY56
    , lgFocusNegTranslateY6
    , lgFocusNegTranslateY64
    , lgFocusNegTranslateY8
    , lgFocusNegTranslateYFull
    , lgFocusNegTranslateYPx
    , lgFocusNoUnderline
    , lgFocusNotSrOnly
    , lgFocusOpacity0
    , lgFocusOpacity100
    , lgFocusOpacity25
    , lgFocusOpacity50
    , lgFocusOpacity75
    , lgFocusOutlineNone
    , lgFocusPlaceholderBlackFocus
    , lgFocusPlaceholderBlue100Focus
    , lgFocusPlaceholderBlue200Focus
    , lgFocusPlaceholderBlue300Focus
    , lgFocusPlaceholderBlue400Focus
    , lgFocusPlaceholderBlue500Focus
    , lgFocusPlaceholderBlue600Focus
    , lgFocusPlaceholderBlue700Focus
    , lgFocusPlaceholderBlue800Focus
    , lgFocusPlaceholderBlue900Focus
    , lgFocusPlaceholderCurrentFocus
    , lgFocusPlaceholderGray100Focus
    , lgFocusPlaceholderGray200Focus
    , lgFocusPlaceholderGray300Focus
    , lgFocusPlaceholderGray400Focus
    , lgFocusPlaceholderGray500Focus
    , lgFocusPlaceholderGray600Focus
    , lgFocusPlaceholderGray700Focus
    , lgFocusPlaceholderGray800Focus
    , lgFocusPlaceholderGray900Focus
    , lgFocusPlaceholderGreen100Focus
    , lgFocusPlaceholderGreen200Focus
    , lgFocusPlaceholderGreen300Focus
    , lgFocusPlaceholderGreen400Focus
    , lgFocusPlaceholderGreen500Focus
    , lgFocusPlaceholderGreen600Focus
    , lgFocusPlaceholderGreen700Focus
    , lgFocusPlaceholderGreen800Focus
    , lgFocusPlaceholderGreen900Focus
    , lgFocusPlaceholderIndigo100Focus
    , lgFocusPlaceholderIndigo200Focus
    , lgFocusPlaceholderIndigo300Focus
    , lgFocusPlaceholderIndigo400Focus
    , lgFocusPlaceholderIndigo500Focus
    , lgFocusPlaceholderIndigo600Focus
    , lgFocusPlaceholderIndigo700Focus
    , lgFocusPlaceholderIndigo800Focus
    , lgFocusPlaceholderIndigo900Focus
    , lgFocusPlaceholderOpacity0Focus
    , lgFocusPlaceholderOpacity100Focus
    , lgFocusPlaceholderOpacity25Focus
    , lgFocusPlaceholderOpacity50Focus
    , lgFocusPlaceholderOpacity75Focus
    , lgFocusPlaceholderOrange100Focus
    , lgFocusPlaceholderOrange200Focus
    , lgFocusPlaceholderOrange300Focus
    , lgFocusPlaceholderOrange400Focus
    , lgFocusPlaceholderOrange500Focus
    , lgFocusPlaceholderOrange600Focus
    , lgFocusPlaceholderOrange700Focus
    , lgFocusPlaceholderOrange800Focus
    , lgFocusPlaceholderOrange900Focus
    , lgFocusPlaceholderPink100Focus
    , lgFocusPlaceholderPink200Focus
    , lgFocusPlaceholderPink300Focus
    , lgFocusPlaceholderPink400Focus
    , lgFocusPlaceholderPink500Focus
    , lgFocusPlaceholderPink600Focus
    , lgFocusPlaceholderPink700Focus
    , lgFocusPlaceholderPink800Focus
    , lgFocusPlaceholderPink900Focus
    , lgFocusPlaceholderPurple100Focus
    , lgFocusPlaceholderPurple200Focus
    , lgFocusPlaceholderPurple300Focus
    , lgFocusPlaceholderPurple400Focus
    , lgFocusPlaceholderPurple500Focus
    , lgFocusPlaceholderPurple600Focus
    , lgFocusPlaceholderPurple700Focus
    , lgFocusPlaceholderPurple800Focus
    , lgFocusPlaceholderPurple900Focus
    , lgFocusPlaceholderRed100Focus
    , lgFocusPlaceholderRed200Focus
    , lgFocusPlaceholderRed300Focus
    , lgFocusPlaceholderRed400Focus
    , lgFocusPlaceholderRed500Focus
    , lgFocusPlaceholderRed600Focus
    , lgFocusPlaceholderRed700Focus
    , lgFocusPlaceholderRed800Focus
    , lgFocusPlaceholderRed900Focus
    , lgFocusPlaceholderTeal100Focus
    , lgFocusPlaceholderTeal200Focus
    , lgFocusPlaceholderTeal300Focus
    , lgFocusPlaceholderTeal400Focus
    , lgFocusPlaceholderTeal500Focus
    , lgFocusPlaceholderTeal600Focus
    , lgFocusPlaceholderTeal700Focus
    , lgFocusPlaceholderTeal800Focus
    , lgFocusPlaceholderTeal900Focus
    , lgFocusPlaceholderTransparentFocus
    , lgFocusPlaceholderWhiteFocus
    , lgFocusPlaceholderYellow100Focus
    , lgFocusPlaceholderYellow200Focus
    , lgFocusPlaceholderYellow300Focus
    , lgFocusPlaceholderYellow400Focus
    , lgFocusPlaceholderYellow500Focus
    , lgFocusPlaceholderYellow600Focus
    , lgFocusPlaceholderYellow700Focus
    , lgFocusPlaceholderYellow800Focus
    , lgFocusPlaceholderYellow900Focus
    , lgFocusRotate0
    , lgFocusRotate180
    , lgFocusRotate45
    , lgFocusRotate90
    , lgFocusScale0
    , lgFocusScale100
    , lgFocusScale105
    , lgFocusScale110
    , lgFocusScale125
    , lgFocusScale150
    , lgFocusScale50
    , lgFocusScale75
    , lgFocusScale90
    , lgFocusScale95
    , lgFocusScaleX0
    , lgFocusScaleX100
    , lgFocusScaleX105
    , lgFocusScaleX110
    , lgFocusScaleX125
    , lgFocusScaleX150
    , lgFocusScaleX50
    , lgFocusScaleX75
    , lgFocusScaleX90
    , lgFocusScaleX95
    , lgFocusScaleY0
    , lgFocusScaleY100
    , lgFocusScaleY105
    , lgFocusScaleY110
    , lgFocusScaleY125
    , lgFocusScaleY150
    , lgFocusScaleY50
    , lgFocusScaleY75
    , lgFocusScaleY90
    , lgFocusScaleY95
    , lgFocusShadow
    , lgFocusShadow2xl
    , lgFocusShadowInner
    , lgFocusShadowLg
    , lgFocusShadowMd
    , lgFocusShadowNone
    , lgFocusShadowOutline
    , lgFocusShadowSm
    , lgFocusShadowXl
    , lgFocusShadowXs
    , lgFocusSkewX0
    , lgFocusSkewX12
    , lgFocusSkewX3
    , lgFocusSkewX6
    , lgFocusSkewY0
    , lgFocusSkewY12
    , lgFocusSkewY3
    , lgFocusSkewY6
    , lgFocusSrOnly
    , lgFocusTextBlack
    , lgFocusTextBlue100
    , lgFocusTextBlue200
    , lgFocusTextBlue300
    , lgFocusTextBlue400
    , lgFocusTextBlue500
    , lgFocusTextBlue600
    , lgFocusTextBlue700
    , lgFocusTextBlue800
    , lgFocusTextBlue900
    , lgFocusTextCurrent
    , lgFocusTextGray100
    , lgFocusTextGray200
    , lgFocusTextGray300
    , lgFocusTextGray400
    , lgFocusTextGray500
    , lgFocusTextGray600
    , lgFocusTextGray700
    , lgFocusTextGray800
    , lgFocusTextGray900
    , lgFocusTextGreen100
    , lgFocusTextGreen200
    , lgFocusTextGreen300
    , lgFocusTextGreen400
    , lgFocusTextGreen500
    , lgFocusTextGreen600
    , lgFocusTextGreen700
    , lgFocusTextGreen800
    , lgFocusTextGreen900
    , lgFocusTextIndigo100
    , lgFocusTextIndigo200
    , lgFocusTextIndigo300
    , lgFocusTextIndigo400
    , lgFocusTextIndigo500
    , lgFocusTextIndigo600
    , lgFocusTextIndigo700
    , lgFocusTextIndigo800
    , lgFocusTextIndigo900
    , lgFocusTextOpacity0
    , lgFocusTextOpacity100
    , lgFocusTextOpacity25
    , lgFocusTextOpacity50
    , lgFocusTextOpacity75
    , lgFocusTextOrange100
    , lgFocusTextOrange200
    , lgFocusTextOrange300
    , lgFocusTextOrange400
    , lgFocusTextOrange500
    , lgFocusTextOrange600
    , lgFocusTextOrange700
    , lgFocusTextOrange800
    , lgFocusTextOrange900
    , lgFocusTextPink100
    , lgFocusTextPink200
    , lgFocusTextPink300
    , lgFocusTextPink400
    , lgFocusTextPink500
    , lgFocusTextPink600
    , lgFocusTextPink700
    , lgFocusTextPink800
    , lgFocusTextPink900
    , lgFocusTextPurple100
    , lgFocusTextPurple200
    , lgFocusTextPurple300
    , lgFocusTextPurple400
    , lgFocusTextPurple500
    , lgFocusTextPurple600
    , lgFocusTextPurple700
    , lgFocusTextPurple800
    , lgFocusTextPurple900
    , lgFocusTextRed100
    , lgFocusTextRed200
    , lgFocusTextRed300
    , lgFocusTextRed400
    , lgFocusTextRed500
    , lgFocusTextRed600
    , lgFocusTextRed700
    , lgFocusTextRed800
    , lgFocusTextRed900
    , lgFocusTextTeal100
    , lgFocusTextTeal200
    , lgFocusTextTeal300
    , lgFocusTextTeal400
    , lgFocusTextTeal500
    , lgFocusTextTeal600
    , lgFocusTextTeal700
    , lgFocusTextTeal800
    , lgFocusTextTeal900
    , lgFocusTextTransparent
    , lgFocusTextWhite
    , lgFocusTextYellow100
    , lgFocusTextYellow200
    , lgFocusTextYellow300
    , lgFocusTextYellow400
    , lgFocusTextYellow500
    , lgFocusTextYellow600
    , lgFocusTextYellow700
    , lgFocusTextYellow800
    , lgFocusTextYellow900
    , lgFocusToBlack
    , lgFocusToBlue100
    , lgFocusToBlue200
    , lgFocusToBlue300
    , lgFocusToBlue400
    , lgFocusToBlue500
    , lgFocusToBlue600
    , lgFocusToBlue700
    , lgFocusToBlue800
    , lgFocusToBlue900
    , lgFocusToCurrent
    , lgFocusToGray100
    , lgFocusToGray200
    , lgFocusToGray300
    , lgFocusToGray400
    , lgFocusToGray500
    , lgFocusToGray600
    , lgFocusToGray700
    , lgFocusToGray800
    , lgFocusToGray900
    , lgFocusToGreen100
    , lgFocusToGreen200
    , lgFocusToGreen300
    , lgFocusToGreen400
    , lgFocusToGreen500
    , lgFocusToGreen600
    , lgFocusToGreen700
    , lgFocusToGreen800
    , lgFocusToGreen900
    , lgFocusToIndigo100
    , lgFocusToIndigo200
    , lgFocusToIndigo300
    , lgFocusToIndigo400
    , lgFocusToIndigo500
    , lgFocusToIndigo600
    , lgFocusToIndigo700
    , lgFocusToIndigo800
    , lgFocusToIndigo900
    , lgFocusToOrange100
    , lgFocusToOrange200
    , lgFocusToOrange300
    , lgFocusToOrange400
    , lgFocusToOrange500
    , lgFocusToOrange600
    , lgFocusToOrange700
    , lgFocusToOrange800
    , lgFocusToOrange900
    , lgFocusToPink100
    , lgFocusToPink200
    , lgFocusToPink300
    , lgFocusToPink400
    , lgFocusToPink500
    , lgFocusToPink600
    , lgFocusToPink700
    , lgFocusToPink800
    , lgFocusToPink900
    , lgFocusToPurple100
    , lgFocusToPurple200
    , lgFocusToPurple300
    , lgFocusToPurple400
    , lgFocusToPurple500
    , lgFocusToPurple600
    , lgFocusToPurple700
    , lgFocusToPurple800
    , lgFocusToPurple900
    , lgFocusToRed100
    , lgFocusToRed200
    , lgFocusToRed300
    , lgFocusToRed400
    , lgFocusToRed500
    , lgFocusToRed600
    , lgFocusToRed700
    , lgFocusToRed800
    , lgFocusToRed900
    , lgFocusToTeal100
    , lgFocusToTeal200
    , lgFocusToTeal300
    , lgFocusToTeal400
    , lgFocusToTeal500
    , lgFocusToTeal600
    , lgFocusToTeal700
    , lgFocusToTeal800
    , lgFocusToTeal900
    , lgFocusToTransparent
    , lgFocusToWhite
    , lgFocusToYellow100
    , lgFocusToYellow200
    , lgFocusToYellow300
    , lgFocusToYellow400
    , lgFocusToYellow500
    , lgFocusToYellow600
    , lgFocusToYellow700
    , lgFocusToYellow800
    , lgFocusToYellow900
    , lgFocusTranslateX0
    , lgFocusTranslateX1
    , lgFocusTranslateX10
    , lgFocusTranslateX12
    , lgFocusTranslateX16
    , lgFocusTranslateX1over2
    , lgFocusTranslateX2
    , lgFocusTranslateX20
    , lgFocusTranslateX24
    , lgFocusTranslateX3
    , lgFocusTranslateX32
    , lgFocusTranslateX4
    , lgFocusTranslateX40
    , lgFocusTranslateX48
    , lgFocusTranslateX5
    , lgFocusTranslateX56
    , lgFocusTranslateX6
    , lgFocusTranslateX64
    , lgFocusTranslateX8
    , lgFocusTranslateXFull
    , lgFocusTranslateXPx
    , lgFocusTranslateY0
    , lgFocusTranslateY1
    , lgFocusTranslateY10
    , lgFocusTranslateY12
    , lgFocusTranslateY16
    , lgFocusTranslateY1over2
    , lgFocusTranslateY2
    , lgFocusTranslateY20
    , lgFocusTranslateY24
    , lgFocusTranslateY3
    , lgFocusTranslateY32
    , lgFocusTranslateY4
    , lgFocusTranslateY40
    , lgFocusTranslateY48
    , lgFocusTranslateY5
    , lgFocusTranslateY56
    , lgFocusTranslateY6
    , lgFocusTranslateY64
    , lgFocusTranslateY8
    , lgFocusTranslateYFull
    , lgFocusTranslateYPx
    , lgFocusUnderline
    , lgFocusViaBlack
    , lgFocusViaBlue100
    , lgFocusViaBlue200
    , lgFocusViaBlue300
    , lgFocusViaBlue400
    , lgFocusViaBlue500
    , lgFocusViaBlue600
    , lgFocusViaBlue700
    , lgFocusViaBlue800
    , lgFocusViaBlue900
    , lgFocusViaCurrent
    , lgFocusViaGray100
    , lgFocusViaGray200
    , lgFocusViaGray300
    , lgFocusViaGray400
    , lgFocusViaGray500
    , lgFocusViaGray600
    , lgFocusViaGray700
    , lgFocusViaGray800
    , lgFocusViaGray900
    , lgFocusViaGreen100
    , lgFocusViaGreen200
    , lgFocusViaGreen300
    , lgFocusViaGreen400
    , lgFocusViaGreen500
    , lgFocusViaGreen600
    , lgFocusViaGreen700
    , lgFocusViaGreen800
    , lgFocusViaGreen900
    , lgFocusViaIndigo100
    , lgFocusViaIndigo200
    , lgFocusViaIndigo300
    , lgFocusViaIndigo400
    , lgFocusViaIndigo500
    , lgFocusViaIndigo600
    , lgFocusViaIndigo700
    , lgFocusViaIndigo800
    , lgFocusViaIndigo900
    , lgFocusViaOrange100
    , lgFocusViaOrange200
    , lgFocusViaOrange300
    , lgFocusViaOrange400
    , lgFocusViaOrange500
    , lgFocusViaOrange600
    , lgFocusViaOrange700
    , lgFocusViaOrange800
    , lgFocusViaOrange900
    , lgFocusViaPink100
    , lgFocusViaPink200
    , lgFocusViaPink300
    , lgFocusViaPink400
    , lgFocusViaPink500
    , lgFocusViaPink600
    , lgFocusViaPink700
    , lgFocusViaPink800
    , lgFocusViaPink900
    , lgFocusViaPurple100
    , lgFocusViaPurple200
    , lgFocusViaPurple300
    , lgFocusViaPurple400
    , lgFocusViaPurple500
    , lgFocusViaPurple600
    , lgFocusViaPurple700
    , lgFocusViaPurple800
    , lgFocusViaPurple900
    , lgFocusViaRed100
    , lgFocusViaRed200
    , lgFocusViaRed300
    , lgFocusViaRed400
    , lgFocusViaRed500
    , lgFocusViaRed600
    , lgFocusViaRed700
    , lgFocusViaRed800
    , lgFocusViaRed900
    , lgFocusViaTeal100
    , lgFocusViaTeal200
    , lgFocusViaTeal300
    , lgFocusViaTeal400
    , lgFocusViaTeal500
    , lgFocusViaTeal600
    , lgFocusViaTeal700
    , lgFocusViaTeal800
    , lgFocusViaTeal900
    , lgFocusViaTransparent
    , lgFocusViaWhite
    , lgFocusViaYellow100
    , lgFocusViaYellow200
    , lgFocusViaYellow300
    , lgFocusViaYellow400
    , lgFocusViaYellow500
    , lgFocusViaYellow600
    , lgFocusViaYellow700
    , lgFocusViaYellow800
    , lgFocusViaYellow900
    , lgFontBlack
    , lgFontBold
    , lgFontExtrabold
    , lgFontHairline
    , lgFontLight
    , lgFontMedium
    , lgFontMono
    , lgFontNormal
    , lgFontSans
    , lgFontSemibold
    , lgFontSerif
    , lgFontThin
    , lgFromBlack
    , lgFromBlue100
    , lgFromBlue200
    , lgFromBlue300
    , lgFromBlue400
    , lgFromBlue500
    , lgFromBlue600
    , lgFromBlue700
    , lgFromBlue800
    , lgFromBlue900
    , lgFromCurrent
    , lgFromGray100
    , lgFromGray200
    , lgFromGray300
    , lgFromGray400
    , lgFromGray500
    , lgFromGray600
    , lgFromGray700
    , lgFromGray800
    , lgFromGray900
    , lgFromGreen100
    , lgFromGreen200
    , lgFromGreen300
    , lgFromGreen400
    , lgFromGreen500
    , lgFromGreen600
    , lgFromGreen700
    , lgFromGreen800
    , lgFromGreen900
    , lgFromIndigo100
    , lgFromIndigo200
    , lgFromIndigo300
    , lgFromIndigo400
    , lgFromIndigo500
    , lgFromIndigo600
    , lgFromIndigo700
    , lgFromIndigo800
    , lgFromIndigo900
    , lgFromOrange100
    , lgFromOrange200
    , lgFromOrange300
    , lgFromOrange400
    , lgFromOrange500
    , lgFromOrange600
    , lgFromOrange700
    , lgFromOrange800
    , lgFromOrange900
    , lgFromPink100
    , lgFromPink200
    , lgFromPink300
    , lgFromPink400
    , lgFromPink500
    , lgFromPink600
    , lgFromPink700
    , lgFromPink800
    , lgFromPink900
    , lgFromPurple100
    , lgFromPurple200
    , lgFromPurple300
    , lgFromPurple400
    , lgFromPurple500
    , lgFromPurple600
    , lgFromPurple700
    , lgFromPurple800
    , lgFromPurple900
    , lgFromRed100
    , lgFromRed200
    , lgFromRed300
    , lgFromRed400
    , lgFromRed500
    , lgFromRed600
    , lgFromRed700
    , lgFromRed800
    , lgFromRed900
    , lgFromTeal100
    , lgFromTeal200
    , lgFromTeal300
    , lgFromTeal400
    , lgFromTeal500
    , lgFromTeal600
    , lgFromTeal700
    , lgFromTeal800
    , lgFromTeal900
    , lgFromTransparent
    , lgFromWhite
    , lgFromYellow100
    , lgFromYellow200
    , lgFromYellow300
    , lgFromYellow400
    , lgFromYellow500
    , lgFromYellow600
    , lgFromYellow700
    , lgFromYellow800
    , lgFromYellow900
    , lgGap0
    , lgGap1
    , lgGap10
    , lgGap12
    , lgGap16
    , lgGap2
    , lgGap20
    , lgGap24
    , lgGap3
    , lgGap32
    , lgGap4
    , lgGap40
    , lgGap48
    , lgGap5
    , lgGap56
    , lgGap6
    , lgGap64
    , lgGap8
    , lgGapPx
    , lgGapX0
    , lgGapX1
    , lgGapX10
    , lgGapX12
    , lgGapX16
    , lgGapX2
    , lgGapX20
    , lgGapX24
    , lgGapX3
    , lgGapX32
    , lgGapX4
    , lgGapX40
    , lgGapX48
    , lgGapX5
    , lgGapX56
    , lgGapX6
    , lgGapX64
    , lgGapX8
    , lgGapXPx
    , lgGapY0
    , lgGapY1
    , lgGapY10
    , lgGapY12
    , lgGapY16
    , lgGapY2
    , lgGapY20
    , lgGapY24
    , lgGapY3
    , lgGapY32
    , lgGapY4
    , lgGapY40
    , lgGapY48
    , lgGapY5
    , lgGapY56
    , lgGapY6
    , lgGapY64
    , lgGapY8
    , lgGapYPx
    , lgGrid
    , lgGridCols1
    , lgGridCols10
    , lgGridCols11
    , lgGridCols12
    , lgGridCols2
    , lgGridCols3
    , lgGridCols4
    , lgGridCols5
    , lgGridCols6
    , lgGridCols7
    , lgGridCols8
    , lgGridCols9
    , lgGridColsNone
    , lgGridFlowCol
    , lgGridFlowColDense
    , lgGridFlowRow
    , lgGridFlowRowDense
    , lgGridRows1
    , lgGridRows2
    , lgGridRows3
    , lgGridRows4
    , lgGridRows5
    , lgGridRows6
    , lgGridRowsNone
    , lgH0
    , lgH1
    , lgH10
    , lgH12
    , lgH16
    , lgH2
    , lgH20
    , lgH24
    , lgH3
    , lgH32
    , lgH4
    , lgH40
    , lgH48
    , lgH5
    , lgH56
    , lgH6
    , lgH64
    , lgH8
    , lgHAuto
    , lgHFull
    , lgHPx
    , lgHScreen
    , lgHidden
    , lgHoverBgBlack
    , lgHoverBgBlue100
    , lgHoverBgBlue200
    , lgHoverBgBlue300
    , lgHoverBgBlue400
    , lgHoverBgBlue500
    , lgHoverBgBlue600
    , lgHoverBgBlue700
    , lgHoverBgBlue800
    , lgHoverBgBlue900
    , lgHoverBgCurrent
    , lgHoverBgGray100
    , lgHoverBgGray200
    , lgHoverBgGray300
    , lgHoverBgGray400
    , lgHoverBgGray500
    , lgHoverBgGray600
    , lgHoverBgGray700
    , lgHoverBgGray800
    , lgHoverBgGray900
    , lgHoverBgGreen100
    , lgHoverBgGreen200
    , lgHoverBgGreen300
    , lgHoverBgGreen400
    , lgHoverBgGreen500
    , lgHoverBgGreen600
    , lgHoverBgGreen700
    , lgHoverBgGreen800
    , lgHoverBgGreen900
    , lgHoverBgIndigo100
    , lgHoverBgIndigo200
    , lgHoverBgIndigo300
    , lgHoverBgIndigo400
    , lgHoverBgIndigo500
    , lgHoverBgIndigo600
    , lgHoverBgIndigo700
    , lgHoverBgIndigo800
    , lgHoverBgIndigo900
    , lgHoverBgOpacity0
    , lgHoverBgOpacity100
    , lgHoverBgOpacity25
    , lgHoverBgOpacity50
    , lgHoverBgOpacity75
    , lgHoverBgOrange100
    , lgHoverBgOrange200
    , lgHoverBgOrange300
    , lgHoverBgOrange400
    , lgHoverBgOrange500
    , lgHoverBgOrange600
    , lgHoverBgOrange700
    , lgHoverBgOrange800
    , lgHoverBgOrange900
    , lgHoverBgPink100
    , lgHoverBgPink200
    , lgHoverBgPink300
    , lgHoverBgPink400
    , lgHoverBgPink500
    , lgHoverBgPink600
    , lgHoverBgPink700
    , lgHoverBgPink800
    , lgHoverBgPink900
    , lgHoverBgPurple100
    , lgHoverBgPurple200
    , lgHoverBgPurple300
    , lgHoverBgPurple400
    , lgHoverBgPurple500
    , lgHoverBgPurple600
    , lgHoverBgPurple700
    , lgHoverBgPurple800
    , lgHoverBgPurple900
    , lgHoverBgRed100
    , lgHoverBgRed200
    , lgHoverBgRed300
    , lgHoverBgRed400
    , lgHoverBgRed500
    , lgHoverBgRed600
    , lgHoverBgRed700
    , lgHoverBgRed800
    , lgHoverBgRed900
    , lgHoverBgTeal100
    , lgHoverBgTeal200
    , lgHoverBgTeal300
    , lgHoverBgTeal400
    , lgHoverBgTeal500
    , lgHoverBgTeal600
    , lgHoverBgTeal700
    , lgHoverBgTeal800
    , lgHoverBgTeal900
    , lgHoverBgTransparent
    , lgHoverBgWhite
    , lgHoverBgYellow100
    , lgHoverBgYellow200
    , lgHoverBgYellow300
    , lgHoverBgYellow400
    , lgHoverBgYellow500
    , lgHoverBgYellow600
    , lgHoverBgYellow700
    , lgHoverBgYellow800
    , lgHoverBgYellow900
    , lgHoverBorderBlack
    , lgHoverBorderBlue100
    , lgHoverBorderBlue200
    , lgHoverBorderBlue300
    , lgHoverBorderBlue400
    , lgHoverBorderBlue500
    , lgHoverBorderBlue600
    , lgHoverBorderBlue700
    , lgHoverBorderBlue800
    , lgHoverBorderBlue900
    , lgHoverBorderCurrent
    , lgHoverBorderGray100
    , lgHoverBorderGray200
    , lgHoverBorderGray300
    , lgHoverBorderGray400
    , lgHoverBorderGray500
    , lgHoverBorderGray600
    , lgHoverBorderGray700
    , lgHoverBorderGray800
    , lgHoverBorderGray900
    , lgHoverBorderGreen100
    , lgHoverBorderGreen200
    , lgHoverBorderGreen300
    , lgHoverBorderGreen400
    , lgHoverBorderGreen500
    , lgHoverBorderGreen600
    , lgHoverBorderGreen700
    , lgHoverBorderGreen800
    , lgHoverBorderGreen900
    , lgHoverBorderIndigo100
    , lgHoverBorderIndigo200
    , lgHoverBorderIndigo300
    , lgHoverBorderIndigo400
    , lgHoverBorderIndigo500
    , lgHoverBorderIndigo600
    , lgHoverBorderIndigo700
    , lgHoverBorderIndigo800
    , lgHoverBorderIndigo900
    , lgHoverBorderOpacity0
    , lgHoverBorderOpacity100
    , lgHoverBorderOpacity25
    , lgHoverBorderOpacity50
    , lgHoverBorderOpacity75
    , lgHoverBorderOrange100
    , lgHoverBorderOrange200
    , lgHoverBorderOrange300
    , lgHoverBorderOrange400
    , lgHoverBorderOrange500
    , lgHoverBorderOrange600
    , lgHoverBorderOrange700
    , lgHoverBorderOrange800
    , lgHoverBorderOrange900
    , lgHoverBorderPink100
    , lgHoverBorderPink200
    , lgHoverBorderPink300
    , lgHoverBorderPink400
    , lgHoverBorderPink500
    , lgHoverBorderPink600
    , lgHoverBorderPink700
    , lgHoverBorderPink800
    , lgHoverBorderPink900
    , lgHoverBorderPurple100
    , lgHoverBorderPurple200
    , lgHoverBorderPurple300
    , lgHoverBorderPurple400
    , lgHoverBorderPurple500
    , lgHoverBorderPurple600
    , lgHoverBorderPurple700
    , lgHoverBorderPurple800
    , lgHoverBorderPurple900
    , lgHoverBorderRed100
    , lgHoverBorderRed200
    , lgHoverBorderRed300
    , lgHoverBorderRed400
    , lgHoverBorderRed500
    , lgHoverBorderRed600
    , lgHoverBorderRed700
    , lgHoverBorderRed800
    , lgHoverBorderRed900
    , lgHoverBorderTeal100
    , lgHoverBorderTeal200
    , lgHoverBorderTeal300
    , lgHoverBorderTeal400
    , lgHoverBorderTeal500
    , lgHoverBorderTeal600
    , lgHoverBorderTeal700
    , lgHoverBorderTeal800
    , lgHoverBorderTeal900
    , lgHoverBorderTransparent
    , lgHoverBorderWhite
    , lgHoverBorderYellow100
    , lgHoverBorderYellow200
    , lgHoverBorderYellow300
    , lgHoverBorderYellow400
    , lgHoverBorderYellow500
    , lgHoverBorderYellow600
    , lgHoverBorderYellow700
    , lgHoverBorderYellow800
    , lgHoverBorderYellow900
    , lgHoverFontBlack
    , lgHoverFontBold
    , lgHoverFontExtrabold
    , lgHoverFontHairline
    , lgHoverFontLight
    , lgHoverFontMedium
    , lgHoverFontNormal
    , lgHoverFontSemibold
    , lgHoverFontThin
    , lgHoverFromBlack
    , lgHoverFromBlue100
    , lgHoverFromBlue200
    , lgHoverFromBlue300
    , lgHoverFromBlue400
    , lgHoverFromBlue500
    , lgHoverFromBlue600
    , lgHoverFromBlue700
    , lgHoverFromBlue800
    , lgHoverFromBlue900
    , lgHoverFromCurrent
    , lgHoverFromGray100
    , lgHoverFromGray200
    , lgHoverFromGray300
    , lgHoverFromGray400
    , lgHoverFromGray500
    , lgHoverFromGray600
    , lgHoverFromGray700
    , lgHoverFromGray800
    , lgHoverFromGray900
    , lgHoverFromGreen100
    , lgHoverFromGreen200
    , lgHoverFromGreen300
    , lgHoverFromGreen400
    , lgHoverFromGreen500
    , lgHoverFromGreen600
    , lgHoverFromGreen700
    , lgHoverFromGreen800
    , lgHoverFromGreen900
    , lgHoverFromIndigo100
    , lgHoverFromIndigo200
    , lgHoverFromIndigo300
    , lgHoverFromIndigo400
    , lgHoverFromIndigo500
    , lgHoverFromIndigo600
    , lgHoverFromIndigo700
    , lgHoverFromIndigo800
    , lgHoverFromIndigo900
    , lgHoverFromOrange100
    , lgHoverFromOrange200
    , lgHoverFromOrange300
    , lgHoverFromOrange400
    , lgHoverFromOrange500
    , lgHoverFromOrange600
    , lgHoverFromOrange700
    , lgHoverFromOrange800
    , lgHoverFromOrange900
    , lgHoverFromPink100
    , lgHoverFromPink200
    , lgHoverFromPink300
    , lgHoverFromPink400
    , lgHoverFromPink500
    , lgHoverFromPink600
    , lgHoverFromPink700
    , lgHoverFromPink800
    , lgHoverFromPink900
    , lgHoverFromPurple100
    , lgHoverFromPurple200
    , lgHoverFromPurple300
    , lgHoverFromPurple400
    , lgHoverFromPurple500
    , lgHoverFromPurple600
    , lgHoverFromPurple700
    , lgHoverFromPurple800
    , lgHoverFromPurple900
    , lgHoverFromRed100
    , lgHoverFromRed200
    , lgHoverFromRed300
    , lgHoverFromRed400
    , lgHoverFromRed500
    , lgHoverFromRed600
    , lgHoverFromRed700
    , lgHoverFromRed800
    , lgHoverFromRed900
    , lgHoverFromTeal100
    , lgHoverFromTeal200
    , lgHoverFromTeal300
    , lgHoverFromTeal400
    , lgHoverFromTeal500
    , lgHoverFromTeal600
    , lgHoverFromTeal700
    , lgHoverFromTeal800
    , lgHoverFromTeal900
    , lgHoverFromTransparent
    , lgHoverFromWhite
    , lgHoverFromYellow100
    , lgHoverFromYellow200
    , lgHoverFromYellow300
    , lgHoverFromYellow400
    , lgHoverFromYellow500
    , lgHoverFromYellow600
    , lgHoverFromYellow700
    , lgHoverFromYellow800
    , lgHoverFromYellow900
    , lgHoverLineThrough
    , lgHoverNegRotate180
    , lgHoverNegRotate45
    , lgHoverNegRotate90
    , lgHoverNegSkewX12
    , lgHoverNegSkewX3
    , lgHoverNegSkewX6
    , lgHoverNegSkewY12
    , lgHoverNegSkewY3
    , lgHoverNegSkewY6
    , lgHoverNegTranslateX1
    , lgHoverNegTranslateX10
    , lgHoverNegTranslateX12
    , lgHoverNegTranslateX16
    , lgHoverNegTranslateX1over2
    , lgHoverNegTranslateX2
    , lgHoverNegTranslateX20
    , lgHoverNegTranslateX24
    , lgHoverNegTranslateX3
    , lgHoverNegTranslateX32
    , lgHoverNegTranslateX4
    , lgHoverNegTranslateX40
    , lgHoverNegTranslateX48
    , lgHoverNegTranslateX5
    , lgHoverNegTranslateX56
    , lgHoverNegTranslateX6
    , lgHoverNegTranslateX64
    , lgHoverNegTranslateX8
    , lgHoverNegTranslateXFull
    , lgHoverNegTranslateXPx
    , lgHoverNegTranslateY1
    , lgHoverNegTranslateY10
    , lgHoverNegTranslateY12
    , lgHoverNegTranslateY16
    , lgHoverNegTranslateY1over2
    , lgHoverNegTranslateY2
    , lgHoverNegTranslateY20
    , lgHoverNegTranslateY24
    , lgHoverNegTranslateY3
    , lgHoverNegTranslateY32
    , lgHoverNegTranslateY4
    , lgHoverNegTranslateY40
    , lgHoverNegTranslateY48
    , lgHoverNegTranslateY5
    , lgHoverNegTranslateY56
    , lgHoverNegTranslateY6
    , lgHoverNegTranslateY64
    , lgHoverNegTranslateY8
    , lgHoverNegTranslateYFull
    , lgHoverNegTranslateYPx
    , lgHoverNoUnderline
    , lgHoverOpacity0
    , lgHoverOpacity100
    , lgHoverOpacity25
    , lgHoverOpacity50
    , lgHoverOpacity75
    , lgHoverRotate0
    , lgHoverRotate180
    , lgHoverRotate45
    , lgHoverRotate90
    , lgHoverScale0
    , lgHoverScale100
    , lgHoverScale105
    , lgHoverScale110
    , lgHoverScale125
    , lgHoverScale150
    , lgHoverScale50
    , lgHoverScale75
    , lgHoverScale90
    , lgHoverScale95
    , lgHoverScaleX0
    , lgHoverScaleX100
    , lgHoverScaleX105
    , lgHoverScaleX110
    , lgHoverScaleX125
    , lgHoverScaleX150
    , lgHoverScaleX50
    , lgHoverScaleX75
    , lgHoverScaleX90
    , lgHoverScaleX95
    , lgHoverScaleY0
    , lgHoverScaleY100
    , lgHoverScaleY105
    , lgHoverScaleY110
    , lgHoverScaleY125
    , lgHoverScaleY150
    , lgHoverScaleY50
    , lgHoverScaleY75
    , lgHoverScaleY90
    , lgHoverScaleY95
    , lgHoverShadow
    , lgHoverShadow2xl
    , lgHoverShadowInner
    , lgHoverShadowLg
    , lgHoverShadowMd
    , lgHoverShadowNone
    , lgHoverShadowOutline
    , lgHoverShadowSm
    , lgHoverShadowXl
    , lgHoverShadowXs
    , lgHoverSkewX0
    , lgHoverSkewX12
    , lgHoverSkewX3
    , lgHoverSkewX6
    , lgHoverSkewY0
    , lgHoverSkewY12
    , lgHoverSkewY3
    , lgHoverSkewY6
    , lgHoverTextBlack
    , lgHoverTextBlue100
    , lgHoverTextBlue200
    , lgHoverTextBlue300
    , lgHoverTextBlue400
    , lgHoverTextBlue500
    , lgHoverTextBlue600
    , lgHoverTextBlue700
    , lgHoverTextBlue800
    , lgHoverTextBlue900
    , lgHoverTextCurrent
    , lgHoverTextGray100
    , lgHoverTextGray200
    , lgHoverTextGray300
    , lgHoverTextGray400
    , lgHoverTextGray500
    , lgHoverTextGray600
    , lgHoverTextGray700
    , lgHoverTextGray800
    , lgHoverTextGray900
    , lgHoverTextGreen100
    , lgHoverTextGreen200
    , lgHoverTextGreen300
    , lgHoverTextGreen400
    , lgHoverTextGreen500
    , lgHoverTextGreen600
    , lgHoverTextGreen700
    , lgHoverTextGreen800
    , lgHoverTextGreen900
    , lgHoverTextIndigo100
    , lgHoverTextIndigo200
    , lgHoverTextIndigo300
    , lgHoverTextIndigo400
    , lgHoverTextIndigo500
    , lgHoverTextIndigo600
    , lgHoverTextIndigo700
    , lgHoverTextIndigo800
    , lgHoverTextIndigo900
    , lgHoverTextOpacity0
    , lgHoverTextOpacity100
    , lgHoverTextOpacity25
    , lgHoverTextOpacity50
    , lgHoverTextOpacity75
    , lgHoverTextOrange100
    , lgHoverTextOrange200
    , lgHoverTextOrange300
    , lgHoverTextOrange400
    , lgHoverTextOrange500
    , lgHoverTextOrange600
    , lgHoverTextOrange700
    , lgHoverTextOrange800
    , lgHoverTextOrange900
    , lgHoverTextPink100
    , lgHoverTextPink200
    , lgHoverTextPink300
    , lgHoverTextPink400
    , lgHoverTextPink500
    , lgHoverTextPink600
    , lgHoverTextPink700
    , lgHoverTextPink800
    , lgHoverTextPink900
    , lgHoverTextPurple100
    , lgHoverTextPurple200
    , lgHoverTextPurple300
    , lgHoverTextPurple400
    , lgHoverTextPurple500
    , lgHoverTextPurple600
    , lgHoverTextPurple700
    , lgHoverTextPurple800
    , lgHoverTextPurple900
    , lgHoverTextRed100
    , lgHoverTextRed200
    , lgHoverTextRed300
    , lgHoverTextRed400
    , lgHoverTextRed500
    , lgHoverTextRed600
    , lgHoverTextRed700
    , lgHoverTextRed800
    , lgHoverTextRed900
    , lgHoverTextTeal100
    , lgHoverTextTeal200
    , lgHoverTextTeal300
    , lgHoverTextTeal400
    , lgHoverTextTeal500
    , lgHoverTextTeal600
    , lgHoverTextTeal700
    , lgHoverTextTeal800
    , lgHoverTextTeal900
    , lgHoverTextTransparent
    , lgHoverTextWhite
    , lgHoverTextYellow100
    , lgHoverTextYellow200
    , lgHoverTextYellow300
    , lgHoverTextYellow400
    , lgHoverTextYellow500
    , lgHoverTextYellow600
    , lgHoverTextYellow700
    , lgHoverTextYellow800
    , lgHoverTextYellow900
    , lgHoverToBlack
    , lgHoverToBlue100
    , lgHoverToBlue200
    , lgHoverToBlue300
    , lgHoverToBlue400
    , lgHoverToBlue500
    , lgHoverToBlue600
    , lgHoverToBlue700
    , lgHoverToBlue800
    , lgHoverToBlue900
    , lgHoverToCurrent
    , lgHoverToGray100
    , lgHoverToGray200
    , lgHoverToGray300
    , lgHoverToGray400
    , lgHoverToGray500
    , lgHoverToGray600
    , lgHoverToGray700
    , lgHoverToGray800
    , lgHoverToGray900
    , lgHoverToGreen100
    , lgHoverToGreen200
    , lgHoverToGreen300
    , lgHoverToGreen400
    , lgHoverToGreen500
    , lgHoverToGreen600
    , lgHoverToGreen700
    , lgHoverToGreen800
    , lgHoverToGreen900
    , lgHoverToIndigo100
    , lgHoverToIndigo200
    , lgHoverToIndigo300
    , lgHoverToIndigo400
    , lgHoverToIndigo500
    , lgHoverToIndigo600
    , lgHoverToIndigo700
    , lgHoverToIndigo800
    , lgHoverToIndigo900
    , lgHoverToOrange100
    , lgHoverToOrange200
    , lgHoverToOrange300
    , lgHoverToOrange400
    , lgHoverToOrange500
    , lgHoverToOrange600
    , lgHoverToOrange700
    , lgHoverToOrange800
    , lgHoverToOrange900
    , lgHoverToPink100
    , lgHoverToPink200
    , lgHoverToPink300
    , lgHoverToPink400
    , lgHoverToPink500
    , lgHoverToPink600
    , lgHoverToPink700
    , lgHoverToPink800
    , lgHoverToPink900
    , lgHoverToPurple100
    , lgHoverToPurple200
    , lgHoverToPurple300
    , lgHoverToPurple400
    , lgHoverToPurple500
    , lgHoverToPurple600
    , lgHoverToPurple700
    , lgHoverToPurple800
    , lgHoverToPurple900
    , lgHoverToRed100
    , lgHoverToRed200
    , lgHoverToRed300
    , lgHoverToRed400
    , lgHoverToRed500
    , lgHoverToRed600
    , lgHoverToRed700
    , lgHoverToRed800
    , lgHoverToRed900
    , lgHoverToTeal100
    , lgHoverToTeal200
    , lgHoverToTeal300
    , lgHoverToTeal400
    , lgHoverToTeal500
    , lgHoverToTeal600
    , lgHoverToTeal700
    , lgHoverToTeal800
    , lgHoverToTeal900
    , lgHoverToTransparent
    , lgHoverToWhite
    , lgHoverToYellow100
    , lgHoverToYellow200
    , lgHoverToYellow300
    , lgHoverToYellow400
    , lgHoverToYellow500
    , lgHoverToYellow600
    , lgHoverToYellow700
    , lgHoverToYellow800
    , lgHoverToYellow900
    , lgHoverTranslateX0
    , lgHoverTranslateX1
    , lgHoverTranslateX10
    , lgHoverTranslateX12
    , lgHoverTranslateX16
    , lgHoverTranslateX1over2
    , lgHoverTranslateX2
    , lgHoverTranslateX20
    , lgHoverTranslateX24
    , lgHoverTranslateX3
    , lgHoverTranslateX32
    , lgHoverTranslateX4
    , lgHoverTranslateX40
    , lgHoverTranslateX48
    , lgHoverTranslateX5
    , lgHoverTranslateX56
    , lgHoverTranslateX6
    , lgHoverTranslateX64
    , lgHoverTranslateX8
    , lgHoverTranslateXFull
    , lgHoverTranslateXPx
    , lgHoverTranslateY0
    , lgHoverTranslateY1
    , lgHoverTranslateY10
    , lgHoverTranslateY12
    , lgHoverTranslateY16
    , lgHoverTranslateY1over2
    , lgHoverTranslateY2
    , lgHoverTranslateY20
    , lgHoverTranslateY24
    , lgHoverTranslateY3
    , lgHoverTranslateY32
    , lgHoverTranslateY4
    , lgHoverTranslateY40
    , lgHoverTranslateY48
    , lgHoverTranslateY5
    , lgHoverTranslateY56
    , lgHoverTranslateY6
    , lgHoverTranslateY64
    , lgHoverTranslateY8
    , lgHoverTranslateYFull
    , lgHoverTranslateYPx
    , lgHoverUnderline
    , lgHoverViaBlack
    , lgHoverViaBlue100
    , lgHoverViaBlue200
    , lgHoverViaBlue300
    , lgHoverViaBlue400
    , lgHoverViaBlue500
    , lgHoverViaBlue600
    , lgHoverViaBlue700
    , lgHoverViaBlue800
    , lgHoverViaBlue900
    , lgHoverViaCurrent
    , lgHoverViaGray100
    , lgHoverViaGray200
    , lgHoverViaGray300
    , lgHoverViaGray400
    , lgHoverViaGray500
    , lgHoverViaGray600
    , lgHoverViaGray700
    , lgHoverViaGray800
    , lgHoverViaGray900
    , lgHoverViaGreen100
    , lgHoverViaGreen200
    , lgHoverViaGreen300
    , lgHoverViaGreen400
    , lgHoverViaGreen500
    , lgHoverViaGreen600
    , lgHoverViaGreen700
    , lgHoverViaGreen800
    , lgHoverViaGreen900
    , lgHoverViaIndigo100
    , lgHoverViaIndigo200
    , lgHoverViaIndigo300
    , lgHoverViaIndigo400
    , lgHoverViaIndigo500
    , lgHoverViaIndigo600
    , lgHoverViaIndigo700
    , lgHoverViaIndigo800
    , lgHoverViaIndigo900
    , lgHoverViaOrange100
    , lgHoverViaOrange200
    , lgHoverViaOrange300
    , lgHoverViaOrange400
    , lgHoverViaOrange500
    , lgHoverViaOrange600
    , lgHoverViaOrange700
    , lgHoverViaOrange800
    , lgHoverViaOrange900
    , lgHoverViaPink100
    , lgHoverViaPink200
    , lgHoverViaPink300
    , lgHoverViaPink400
    , lgHoverViaPink500
    , lgHoverViaPink600
    , lgHoverViaPink700
    , lgHoverViaPink800
    , lgHoverViaPink900
    , lgHoverViaPurple100
    , lgHoverViaPurple200
    , lgHoverViaPurple300
    , lgHoverViaPurple400
    , lgHoverViaPurple500
    , lgHoverViaPurple600
    , lgHoverViaPurple700
    , lgHoverViaPurple800
    , lgHoverViaPurple900
    , lgHoverViaRed100
    , lgHoverViaRed200
    , lgHoverViaRed300
    , lgHoverViaRed400
    , lgHoverViaRed500
    , lgHoverViaRed600
    , lgHoverViaRed700
    , lgHoverViaRed800
    , lgHoverViaRed900
    , lgHoverViaTeal100
    , lgHoverViaTeal200
    , lgHoverViaTeal300
    , lgHoverViaTeal400
    , lgHoverViaTeal500
    , lgHoverViaTeal600
    , lgHoverViaTeal700
    , lgHoverViaTeal800
    , lgHoverViaTeal900
    , lgHoverViaTransparent
    , lgHoverViaWhite
    , lgHoverViaYellow100
    , lgHoverViaYellow200
    , lgHoverViaYellow300
    , lgHoverViaYellow400
    , lgHoverViaYellow500
    , lgHoverViaYellow600
    , lgHoverViaYellow700
    , lgHoverViaYellow800
    , lgHoverViaYellow900
    , lgInline
    , lgInlineBlock
    , lgInlineFlex
    , lgInlineGrid
    , lgInset0
    , lgInsetAuto
    , lgInsetX0
    , lgInsetXAuto
    , lgInsetY0
    , lgInsetYAuto
    , lgInvisible
    , lgItalic
    , lgItemsBaseline
    , lgItemsCenter
    , lgItemsEnd
    , lgItemsStart
    , lgItemsStretch
    , lgJustifyAround
    , lgJustifyBetween
    , lgJustifyCenter
    , lgJustifyEnd
    , lgJustifyEvenly
    , lgJustifyItemsAuto
    , lgJustifyItemsCenter
    , lgJustifyItemsEnd
    , lgJustifyItemsStart
    , lgJustifyItemsStretch
    , lgJustifySelfAuto
    , lgJustifySelfCenter
    , lgJustifySelfEnd
    , lgJustifySelfStart
    , lgJustifySelfStretch
    , lgJustifyStart
    , lgLeading10
    , lgLeading3
    , lgLeading4
    , lgLeading5
    , lgLeading6
    , lgLeading7
    , lgLeading8
    , lgLeading9
    , lgLeadingLoose
    , lgLeadingNone
    , lgLeadingNormal
    , lgLeadingRelaxed
    , lgLeadingSnug
    , lgLeadingTight
    , lgLeft0
    , lgLeftAuto
    , lgLineThrough
    , lgLiningNums
    , lgListDecimal
    , lgListDisc
    , lgListInside
    , lgListNone
    , lgListOutside
    , lgLowercase
    , lgM0
    , lgM1
    , lgM10
    , lgM12
    , lgM16
    , lgM2
    , lgM20
    , lgM24
    , lgM3
    , lgM32
    , lgM4
    , lgM40
    , lgM48
    , lgM5
    , lgM56
    , lgM6
    , lgM64
    , lgM8
    , lgMAuto
    , lgMPx
    , lgMaxHFull
    , lgMaxHScreen
    , lgMaxW2xl
    , lgMaxW3xl
    , lgMaxW4xl
    , lgMaxW5xl
    , lgMaxW6xl
    , lgMaxWFull
    , lgMaxWLg
    , lgMaxWMd
    , lgMaxWNone
    , lgMaxWScreenLg
    , lgMaxWScreenMd
    , lgMaxWScreenSm
    , lgMaxWScreenXl
    , lgMaxWSm
    , lgMaxWXl
    , lgMaxWXs
    , lgMb0
    , lgMb1
    , lgMb10
    , lgMb12
    , lgMb16
    , lgMb2
    , lgMb20
    , lgMb24
    , lgMb3
    , lgMb32
    , lgMb4
    , lgMb40
    , lgMb48
    , lgMb5
    , lgMb56
    , lgMb6
    , lgMb64
    , lgMb8
    , lgMbAuto
    , lgMbPx
    , lgMinH0
    , lgMinHFull
    , lgMinHScreen
    , lgMinW0
    , lgMinWFull
    , lgMl0
    , lgMl1
    , lgMl10
    , lgMl12
    , lgMl16
    , lgMl2
    , lgMl20
    , lgMl24
    , lgMl3
    , lgMl32
    , lgMl4
    , lgMl40
    , lgMl48
    , lgMl5
    , lgMl56
    , lgMl6
    , lgMl64
    , lgMl8
    , lgMlAuto
    , lgMlPx
    , lgMr0
    , lgMr1
    , lgMr10
    , lgMr12
    , lgMr16
    , lgMr2
    , lgMr20
    , lgMr24
    , lgMr3
    , lgMr32
    , lgMr4
    , lgMr40
    , lgMr48
    , lgMr5
    , lgMr56
    , lgMr6
    , lgMr64
    , lgMr8
    , lgMrAuto
    , lgMrPx
    , lgMt0
    , lgMt1
    , lgMt10
    , lgMt12
    , lgMt16
    , lgMt2
    , lgMt20
    , lgMt24
    , lgMt3
    , lgMt32
    , lgMt4
    , lgMt40
    , lgMt48
    , lgMt5
    , lgMt56
    , lgMt6
    , lgMt64
    , lgMt8
    , lgMtAuto
    , lgMtPx
    , lgMx0
    , lgMx1
    , lgMx10
    , lgMx12
    , lgMx16
    , lgMx2
    , lgMx20
    , lgMx24
    , lgMx3
    , lgMx32
    , lgMx4
    , lgMx40
    , lgMx48
    , lgMx5
    , lgMx56
    , lgMx6
    , lgMx64
    , lgMx8
    , lgMxAuto
    , lgMxPx
    , lgMy0
    , lgMy1
    , lgMy10
    , lgMy12
    , lgMy16
    , lgMy2
    , lgMy20
    , lgMy24
    , lgMy3
    , lgMy32
    , lgMy4
    , lgMy40
    , lgMy48
    , lgMy5
    , lgMy56
    , lgMy6
    , lgMy64
    , lgMy8
    , lgMyAuto
    , lgMyPx
    , lgNegM1
    , lgNegM10
    , lgNegM12
    , lgNegM16
    , lgNegM2
    , lgNegM20
    , lgNegM24
    , lgNegM3
    , lgNegM32
    , lgNegM4
    , lgNegM40
    , lgNegM48
    , lgNegM5
    , lgNegM56
    , lgNegM6
    , lgNegM64
    , lgNegM8
    , lgNegMPx
    , lgNegMb1
    , lgNegMb10
    , lgNegMb12
    , lgNegMb16
    , lgNegMb2
    , lgNegMb20
    , lgNegMb24
    , lgNegMb3
    , lgNegMb32
    , lgNegMb4
    , lgNegMb40
    , lgNegMb48
    , lgNegMb5
    , lgNegMb56
    , lgNegMb6
    , lgNegMb64
    , lgNegMb8
    , lgNegMbPx
    , lgNegMl1
    , lgNegMl10
    , lgNegMl12
    , lgNegMl16
    , lgNegMl2
    , lgNegMl20
    , lgNegMl24
    , lgNegMl3
    , lgNegMl32
    , lgNegMl4
    , lgNegMl40
    , lgNegMl48
    , lgNegMl5
    , lgNegMl56
    , lgNegMl6
    , lgNegMl64
    , lgNegMl8
    , lgNegMlPx
    , lgNegMr1
    , lgNegMr10
    , lgNegMr12
    , lgNegMr16
    , lgNegMr2
    , lgNegMr20
    , lgNegMr24
    , lgNegMr3
    , lgNegMr32
    , lgNegMr4
    , lgNegMr40
    , lgNegMr48
    , lgNegMr5
    , lgNegMr56
    , lgNegMr6
    , lgNegMr64
    , lgNegMr8
    , lgNegMrPx
    , lgNegMt1
    , lgNegMt10
    , lgNegMt12
    , lgNegMt16
    , lgNegMt2
    , lgNegMt20
    , lgNegMt24
    , lgNegMt3
    , lgNegMt32
    , lgNegMt4
    , lgNegMt40
    , lgNegMt48
    , lgNegMt5
    , lgNegMt56
    , lgNegMt6
    , lgNegMt64
    , lgNegMt8
    , lgNegMtPx
    , lgNegMx1
    , lgNegMx10
    , lgNegMx12
    , lgNegMx16
    , lgNegMx2
    , lgNegMx20
    , lgNegMx24
    , lgNegMx3
    , lgNegMx32
    , lgNegMx4
    , lgNegMx40
    , lgNegMx48
    , lgNegMx5
    , lgNegMx56
    , lgNegMx6
    , lgNegMx64
    , lgNegMx8
    , lgNegMxPx
    , lgNegMy1
    , lgNegMy10
    , lgNegMy12
    , lgNegMy16
    , lgNegMy2
    , lgNegMy20
    , lgNegMy24
    , lgNegMy3
    , lgNegMy32
    , lgNegMy4
    , lgNegMy40
    , lgNegMy48
    , lgNegMy5
    , lgNegMy56
    , lgNegMy6
    , lgNegMy64
    , lgNegMy8
    , lgNegMyPx
    , lgNegRotate180
    , lgNegRotate45
    , lgNegRotate90
    , lgNegSkewX12
    , lgNegSkewX3
    , lgNegSkewX6
    , lgNegSkewY12
    , lgNegSkewY3
    , lgNegSkewY6
    , lgNegSpaceX1
    , lgNegSpaceX10
    , lgNegSpaceX12
    , lgNegSpaceX16
    , lgNegSpaceX2
    , lgNegSpaceX20
    , lgNegSpaceX24
    , lgNegSpaceX3
    , lgNegSpaceX32
    , lgNegSpaceX4
    , lgNegSpaceX40
    , lgNegSpaceX48
    , lgNegSpaceX5
    , lgNegSpaceX56
    , lgNegSpaceX6
    , lgNegSpaceX64
    , lgNegSpaceX8
    , lgNegSpaceXPx
    , lgNegSpaceY1
    , lgNegSpaceY10
    , lgNegSpaceY12
    , lgNegSpaceY16
    , lgNegSpaceY2
    , lgNegSpaceY20
    , lgNegSpaceY24
    , lgNegSpaceY3
    , lgNegSpaceY32
    , lgNegSpaceY4
    , lgNegSpaceY40
    , lgNegSpaceY48
    , lgNegSpaceY5
    , lgNegSpaceY56
    , lgNegSpaceY6
    , lgNegSpaceY64
    , lgNegSpaceY8
    , lgNegSpaceYPx
    , lgNegTranslateX1
    , lgNegTranslateX10
    , lgNegTranslateX12
    , lgNegTranslateX16
    , lgNegTranslateX1over2
    , lgNegTranslateX2
    , lgNegTranslateX20
    , lgNegTranslateX24
    , lgNegTranslateX3
    , lgNegTranslateX32
    , lgNegTranslateX4
    , lgNegTranslateX40
    , lgNegTranslateX48
    , lgNegTranslateX5
    , lgNegTranslateX56
    , lgNegTranslateX6
    , lgNegTranslateX64
    , lgNegTranslateX8
    , lgNegTranslateXFull
    , lgNegTranslateXPx
    , lgNegTranslateY1
    , lgNegTranslateY10
    , lgNegTranslateY12
    , lgNegTranslateY16
    , lgNegTranslateY1over2
    , lgNegTranslateY2
    , lgNegTranslateY20
    , lgNegTranslateY24
    , lgNegTranslateY3
    , lgNegTranslateY32
    , lgNegTranslateY4
    , lgNegTranslateY40
    , lgNegTranslateY48
    , lgNegTranslateY5
    , lgNegTranslateY56
    , lgNegTranslateY6
    , lgNegTranslateY64
    , lgNegTranslateY8
    , lgNegTranslateYFull
    , lgNegTranslateYPx
    , lgNoUnderline
    , lgNormalCase
    , lgNormalNums
    , lgNotItalic
    , lgNotSrOnly
    , lgObjectBottom
    , lgObjectCenter
    , lgObjectContain
    , lgObjectCover
    , lgObjectFill
    , lgObjectLeft
    , lgObjectLeftBottom
    , lgObjectLeftTop
    , lgObjectNone
    , lgObjectRight
    , lgObjectRightBottom
    , lgObjectRightTop
    , lgObjectScaleDown
    , lgObjectTop
    , lgOldstyleNums
    , lgOpacity0
    , lgOpacity100
    , lgOpacity25
    , lgOpacity50
    , lgOpacity75
    , lgOrder1
    , lgOrder10
    , lgOrder11
    , lgOrder12
    , lgOrder2
    , lgOrder3
    , lgOrder4
    , lgOrder5
    , lgOrder6
    , lgOrder7
    , lgOrder8
    , lgOrder9
    , lgOrderFirst
    , lgOrderLast
    , lgOrderNone
    , lgOrdinal
    , lgOriginBottom
    , lgOriginBottomLeft
    , lgOriginBottomRight
    , lgOriginCenter
    , lgOriginLeft
    , lgOriginRight
    , lgOriginTop
    , lgOriginTopLeft
    , lgOriginTopRight
    , lgOutlineNone
    , lgOverflowAuto
    , lgOverflowHidden
    , lgOverflowScroll
    , lgOverflowVisible
    , lgOverflowXAuto
    , lgOverflowXHidden
    , lgOverflowXScroll
    , lgOverflowXVisible
    , lgOverflowYAuto
    , lgOverflowYHidden
    , lgOverflowYScroll
    , lgOverflowYVisible
    , lgOverscrollAuto
    , lgOverscrollContain
    , lgOverscrollNone
    , lgOverscrollXAuto
    , lgOverscrollXContain
    , lgOverscrollXNone
    , lgOverscrollYAuto
    , lgOverscrollYContain
    , lgOverscrollYNone
    , lgP0
    , lgP1
    , lgP10
    , lgP12
    , lgP16
    , lgP2
    , lgP20
    , lgP24
    , lgP3
    , lgP32
    , lgP4
    , lgP40
    , lgP48
    , lgP5
    , lgP56
    , lgP6
    , lgP64
    , lgP8
    , lgPPx
    , lgPb0
    , lgPb1
    , lgPb10
    , lgPb12
    , lgPb16
    , lgPb2
    , lgPb20
    , lgPb24
    , lgPb3
    , lgPb32
    , lgPb4
    , lgPb40
    , lgPb48
    , lgPb5
    , lgPb56
    , lgPb6
    , lgPb64
    , lgPb8
    , lgPbPx
    , lgPl0
    , lgPl1
    , lgPl10
    , lgPl12
    , lgPl16
    , lgPl2
    , lgPl20
    , lgPl24
    , lgPl3
    , lgPl32
    , lgPl4
    , lgPl40
    , lgPl48
    , lgPl5
    , lgPl56
    , lgPl6
    , lgPl64
    , lgPl8
    , lgPlPx
    , lgPlaceContentAround
    , lgPlaceContentBetween
    , lgPlaceContentCenter
    , lgPlaceContentEnd
    , lgPlaceContentEvenly
    , lgPlaceContentStart
    , lgPlaceContentStretch
    , lgPlaceItemsAuto
    , lgPlaceItemsCenter
    , lgPlaceItemsEnd
    , lgPlaceItemsStart
    , lgPlaceItemsStretch
    , lgPlaceSelfAuto
    , lgPlaceSelfCenter
    , lgPlaceSelfEnd
    , lgPlaceSelfStart
    , lgPlaceSelfStretch
    , lgPlaceholderBlack
    , lgPlaceholderBlue100
    , lgPlaceholderBlue200
    , lgPlaceholderBlue300
    , lgPlaceholderBlue400
    , lgPlaceholderBlue500
    , lgPlaceholderBlue600
    , lgPlaceholderBlue700
    , lgPlaceholderBlue800
    , lgPlaceholderBlue900
    , lgPlaceholderCurrent
    , lgPlaceholderGray100
    , lgPlaceholderGray200
    , lgPlaceholderGray300
    , lgPlaceholderGray400
    , lgPlaceholderGray500
    , lgPlaceholderGray600
    , lgPlaceholderGray700
    , lgPlaceholderGray800
    , lgPlaceholderGray900
    , lgPlaceholderGreen100
    , lgPlaceholderGreen200
    , lgPlaceholderGreen300
    , lgPlaceholderGreen400
    , lgPlaceholderGreen500
    , lgPlaceholderGreen600
    , lgPlaceholderGreen700
    , lgPlaceholderGreen800
    , lgPlaceholderGreen900
    , lgPlaceholderIndigo100
    , lgPlaceholderIndigo200
    , lgPlaceholderIndigo300
    , lgPlaceholderIndigo400
    , lgPlaceholderIndigo500
    , lgPlaceholderIndigo600
    , lgPlaceholderIndigo700
    , lgPlaceholderIndigo800
    , lgPlaceholderIndigo900
    , lgPlaceholderOpacity0
    , lgPlaceholderOpacity100
    , lgPlaceholderOpacity25
    , lgPlaceholderOpacity50
    , lgPlaceholderOpacity75
    , lgPlaceholderOrange100
    , lgPlaceholderOrange200
    , lgPlaceholderOrange300
    , lgPlaceholderOrange400
    , lgPlaceholderOrange500
    , lgPlaceholderOrange600
    , lgPlaceholderOrange700
    , lgPlaceholderOrange800
    , lgPlaceholderOrange900
    , lgPlaceholderPink100
    , lgPlaceholderPink200
    , lgPlaceholderPink300
    , lgPlaceholderPink400
    , lgPlaceholderPink500
    , lgPlaceholderPink600
    , lgPlaceholderPink700
    , lgPlaceholderPink800
    , lgPlaceholderPink900
    , lgPlaceholderPurple100
    , lgPlaceholderPurple200
    , lgPlaceholderPurple300
    , lgPlaceholderPurple400
    , lgPlaceholderPurple500
    , lgPlaceholderPurple600
    , lgPlaceholderPurple700
    , lgPlaceholderPurple800
    , lgPlaceholderPurple900
    , lgPlaceholderRed100
    , lgPlaceholderRed200
    , lgPlaceholderRed300
    , lgPlaceholderRed400
    , lgPlaceholderRed500
    , lgPlaceholderRed600
    , lgPlaceholderRed700
    , lgPlaceholderRed800
    , lgPlaceholderRed900
    , lgPlaceholderTeal100
    , lgPlaceholderTeal200
    , lgPlaceholderTeal300
    , lgPlaceholderTeal400
    , lgPlaceholderTeal500
    , lgPlaceholderTeal600
    , lgPlaceholderTeal700
    , lgPlaceholderTeal800
    , lgPlaceholderTeal900
    , lgPlaceholderTransparent
    , lgPlaceholderWhite
    , lgPlaceholderYellow100
    , lgPlaceholderYellow200
    , lgPlaceholderYellow300
    , lgPlaceholderYellow400
    , lgPlaceholderYellow500
    , lgPlaceholderYellow600
    , lgPlaceholderYellow700
    , lgPlaceholderYellow800
    , lgPlaceholderYellow900
    , lgPointerEventsAuto
    , lgPointerEventsNone
    , lgPr0
    , lgPr1
    , lgPr10
    , lgPr12
    , lgPr16
    , lgPr2
    , lgPr20
    , lgPr24
    , lgPr3
    , lgPr32
    , lgPr4
    , lgPr40
    , lgPr48
    , lgPr5
    , lgPr56
    , lgPr6
    , lgPr64
    , lgPr8
    , lgPrPx
    , lgProportionalNums
    , lgPt0
    , lgPt1
    , lgPt10
    , lgPt12
    , lgPt16
    , lgPt2
    , lgPt20
    , lgPt24
    , lgPt3
    , lgPt32
    , lgPt4
    , lgPt40
    , lgPt48
    , lgPt5
    , lgPt56
    , lgPt6
    , lgPt64
    , lgPt8
    , lgPtPx
    , lgPx0
    , lgPx1
    , lgPx10
    , lgPx12
    , lgPx16
    , lgPx2
    , lgPx20
    , lgPx24
    , lgPx3
    , lgPx32
    , lgPx4
    , lgPx40
    , lgPx48
    , lgPx5
    , lgPx56
    , lgPx6
    , lgPx64
    , lgPx8
    , lgPxPx
    , lgPy0
    , lgPy1
    , lgPy10
    , lgPy12
    , lgPy16
    , lgPy2
    , lgPy20
    , lgPy24
    , lgPy3
    , lgPy32
    , lgPy4
    , lgPy40
    , lgPy48
    , lgPy5
    , lgPy56
    , lgPy6
    , lgPy64
    , lgPy8
    , lgPyPx
    , lgRelative
    , lgResize
    , lgResizeNone
    , lgResizeX
    , lgResizeY
    , lgRight0
    , lgRightAuto
    , lgRotate0
    , lgRotate180
    , lgRotate45
    , lgRotate90
    , lgRounded
    , lgRoundedB
    , lgRoundedBFull
    , lgRoundedBLg
    , lgRoundedBMd
    , lgRoundedBNone
    , lgRoundedBSm
    , lgRoundedBl
    , lgRoundedBlFull
    , lgRoundedBlLg
    , lgRoundedBlMd
    , lgRoundedBlNone
    , lgRoundedBlSm
    , lgRoundedBr
    , lgRoundedBrFull
    , lgRoundedBrLg
    , lgRoundedBrMd
    , lgRoundedBrNone
    , lgRoundedBrSm
    , lgRoundedFull
    , lgRoundedL
    , lgRoundedLFull
    , lgRoundedLLg
    , lgRoundedLMd
    , lgRoundedLNone
    , lgRoundedLSm
    , lgRoundedLg
    , lgRoundedMd
    , lgRoundedNone
    , lgRoundedR
    , lgRoundedRFull
    , lgRoundedRLg
    , lgRoundedRMd
    , lgRoundedRNone
    , lgRoundedRSm
    , lgRoundedSm
    , lgRoundedT
    , lgRoundedTFull
    , lgRoundedTLg
    , lgRoundedTMd
    , lgRoundedTNone
    , lgRoundedTSm
    , lgRoundedTl
    , lgRoundedTlFull
    , lgRoundedTlLg
    , lgRoundedTlMd
    , lgRoundedTlNone
    , lgRoundedTlSm
    , lgRoundedTr
    , lgRoundedTrFull
    , lgRoundedTrLg
    , lgRoundedTrMd
    , lgRoundedTrNone
    , lgRoundedTrSm
    , lgRowAuto
    , lgRowEnd1
    , lgRowEnd2
    , lgRowEnd3
    , lgRowEnd4
    , lgRowEnd5
    , lgRowEnd6
    , lgRowEnd7
    , lgRowEndAuto
    , lgRowSpan1
    , lgRowSpan2
    , lgRowSpan3
    , lgRowSpan4
    , lgRowSpan5
    , lgRowSpan6
    , lgRowStart1
    , lgRowStart2
    , lgRowStart3
    , lgRowStart4
    , lgRowStart5
    , lgRowStart6
    , lgRowStart7
    , lgRowStartAuto
    , lgScale0
    , lgScale100
    , lgScale105
    , lgScale110
    , lgScale125
    , lgScale150
    , lgScale50
    , lgScale75
    , lgScale90
    , lgScale95
    , lgScaleX0
    , lgScaleX100
    , lgScaleX105
    , lgScaleX110
    , lgScaleX125
    , lgScaleX150
    , lgScaleX50
    , lgScaleX75
    , lgScaleX90
    , lgScaleX95
    , lgScaleY0
    , lgScaleY100
    , lgScaleY105
    , lgScaleY110
    , lgScaleY125
    , lgScaleY150
    , lgScaleY50
    , lgScaleY75
    , lgScaleY90
    , lgScaleY95
    , lgScrollingAuto
    , lgScrollingTouch
    , lgSelectAll
    , lgSelectAuto
    , lgSelectNone
    , lgSelectText
    , lgSelfAuto
    , lgSelfCenter
    , lgSelfEnd
    , lgSelfStart
    , lgSelfStretch
    , lgShadow
    , lgShadow2xl
    , lgShadowInner
    , lgShadowLg
    , lgShadowMd
    , lgShadowNone
    , lgShadowOutline
    , lgShadowSm
    , lgShadowXl
    , lgShadowXs
    , lgSkewX0
    , lgSkewX12
    , lgSkewX3
    , lgSkewX6
    , lgSkewY0
    , lgSkewY12
    , lgSkewY3
    , lgSkewY6
    , lgSlashedZero
    , lgSpaceX0
    , lgSpaceX1
    , lgSpaceX10
    , lgSpaceX12
    , lgSpaceX16
    , lgSpaceX2
    , lgSpaceX20
    , lgSpaceX24
    , lgSpaceX3
    , lgSpaceX32
    , lgSpaceX4
    , lgSpaceX40
    , lgSpaceX48
    , lgSpaceX5
    , lgSpaceX56
    , lgSpaceX6
    , lgSpaceX64
    , lgSpaceX8
    , lgSpaceXPx
    , lgSpaceXReverse
    , lgSpaceY0
    , lgSpaceY1
    , lgSpaceY10
    , lgSpaceY12
    , lgSpaceY16
    , lgSpaceY2
    , lgSpaceY20
    , lgSpaceY24
    , lgSpaceY3
    , lgSpaceY32
    , lgSpaceY4
    , lgSpaceY40
    , lgSpaceY48
    , lgSpaceY5
    , lgSpaceY56
    , lgSpaceY6
    , lgSpaceY64
    , lgSpaceY8
    , lgSpaceYPx
    , lgSpaceYReverse
    , lgSrOnly
    , lgStackedFractions
    , lgStatic
    , lgSticky
    , lgStroke0
    , lgStroke1
    , lgStroke2
    , lgStrokeCurrent
    , lgSubpixelAntialiased
    , lgTable
    , lgTableAuto
    , lgTableCaption
    , lgTableCell
    , lgTableColumn
    , lgTableColumnGroup
    , lgTableFixed
    , lgTableFooterGroup
    , lgTableHeaderGroup
    , lgTableRow
    , lgTableRowGroup
    , lgTabularNums
    , lgText2xl
    , lgText3xl
    , lgText4xl
    , lgText5xl
    , lgText6xl
    , lgTextBase
    , lgTextBlack
    , lgTextBlue100
    , lgTextBlue200
    , lgTextBlue300
    , lgTextBlue400
    , lgTextBlue500
    , lgTextBlue600
    , lgTextBlue700
    , lgTextBlue800
    , lgTextBlue900
    , lgTextCenter
    , lgTextCurrent
    , lgTextGray100
    , lgTextGray200
    , lgTextGray300
    , lgTextGray400
    , lgTextGray500
    , lgTextGray600
    , lgTextGray700
    , lgTextGray800
    , lgTextGray900
    , lgTextGreen100
    , lgTextGreen200
    , lgTextGreen300
    , lgTextGreen400
    , lgTextGreen500
    , lgTextGreen600
    , lgTextGreen700
    , lgTextGreen800
    , lgTextGreen900
    , lgTextIndigo100
    , lgTextIndigo200
    , lgTextIndigo300
    , lgTextIndigo400
    , lgTextIndigo500
    , lgTextIndigo600
    , lgTextIndigo700
    , lgTextIndigo800
    , lgTextIndigo900
    , lgTextJustify
    , lgTextLeft
    , lgTextLg
    , lgTextOpacity0
    , lgTextOpacity100
    , lgTextOpacity25
    , lgTextOpacity50
    , lgTextOpacity75
    , lgTextOrange100
    , lgTextOrange200
    , lgTextOrange300
    , lgTextOrange400
    , lgTextOrange500
    , lgTextOrange600
    , lgTextOrange700
    , lgTextOrange800
    , lgTextOrange900
    , lgTextPink100
    , lgTextPink200
    , lgTextPink300
    , lgTextPink400
    , lgTextPink500
    , lgTextPink600
    , lgTextPink700
    , lgTextPink800
    , lgTextPink900
    , lgTextPurple100
    , lgTextPurple200
    , lgTextPurple300
    , lgTextPurple400
    , lgTextPurple500
    , lgTextPurple600
    , lgTextPurple700
    , lgTextPurple800
    , lgTextPurple900
    , lgTextRed100
    , lgTextRed200
    , lgTextRed300
    , lgTextRed400
    , lgTextRed500
    , lgTextRed600
    , lgTextRed700
    , lgTextRed800
    , lgTextRed900
    , lgTextRight
    , lgTextSm
    , lgTextTeal100
    , lgTextTeal200
    , lgTextTeal300
    , lgTextTeal400
    , lgTextTeal500
    , lgTextTeal600
    , lgTextTeal700
    , lgTextTeal800
    , lgTextTeal900
    , lgTextTransparent
    , lgTextWhite
    , lgTextXl
    , lgTextXs
    , lgTextYellow100
    , lgTextYellow200
    , lgTextYellow300
    , lgTextYellow400
    , lgTextYellow500
    , lgTextYellow600
    , lgTextYellow700
    , lgTextYellow800
    , lgTextYellow900
    , lgToBlack
    , lgToBlue100
    , lgToBlue200
    , lgToBlue300
    , lgToBlue400
    , lgToBlue500
    , lgToBlue600
    , lgToBlue700
    , lgToBlue800
    , lgToBlue900
    , lgToCurrent
    , lgToGray100
    , lgToGray200
    , lgToGray300
    , lgToGray400
    , lgToGray500
    , lgToGray600
    , lgToGray700
    , lgToGray800
    , lgToGray900
    , lgToGreen100
    , lgToGreen200
    , lgToGreen300
    , lgToGreen400
    , lgToGreen500
    , lgToGreen600
    , lgToGreen700
    , lgToGreen800
    , lgToGreen900
    , lgToIndigo100
    , lgToIndigo200
    , lgToIndigo300
    , lgToIndigo400
    , lgToIndigo500
    , lgToIndigo600
    , lgToIndigo700
    , lgToIndigo800
    , lgToIndigo900
    , lgToOrange100
    , lgToOrange200
    , lgToOrange300
    , lgToOrange400
    , lgToOrange500
    , lgToOrange600
    , lgToOrange700
    , lgToOrange800
    , lgToOrange900
    , lgToPink100
    , lgToPink200
    , lgToPink300
    , lgToPink400
    , lgToPink500
    , lgToPink600
    , lgToPink700
    , lgToPink800
    , lgToPink900
    , lgToPurple100
    , lgToPurple200
    , lgToPurple300
    , lgToPurple400
    , lgToPurple500
    , lgToPurple600
    , lgToPurple700
    , lgToPurple800
    , lgToPurple900
    , lgToRed100
    , lgToRed200
    , lgToRed300
    , lgToRed400
    , lgToRed500
    , lgToRed600
    , lgToRed700
    , lgToRed800
    , lgToRed900
    , lgToTeal100
    , lgToTeal200
    , lgToTeal300
    , lgToTeal400
    , lgToTeal500
    , lgToTeal600
    , lgToTeal700
    , lgToTeal800
    , lgToTeal900
    , lgToTransparent
    , lgToWhite
    , lgToYellow100
    , lgToYellow200
    , lgToYellow300
    , lgToYellow400
    , lgToYellow500
    , lgToYellow600
    , lgToYellow700
    , lgToYellow800
    , lgToYellow900
    , lgTop0
    , lgTopAuto
    , lgTrackingNormal
    , lgTrackingTight
    , lgTrackingTighter
    , lgTrackingWide
    , lgTrackingWider
    , lgTrackingWidest
    , lgTransform
    , lgTransformNone
    , lgTransition
    , lgTransitionAll
    , lgTransitionColors
    , lgTransitionNone
    , lgTransitionOpacity
    , lgTransitionShadow
    , lgTransitionTransform
    , lgTranslateX0
    , lgTranslateX1
    , lgTranslateX10
    , lgTranslateX12
    , lgTranslateX16
    , lgTranslateX1over2
    , lgTranslateX2
    , lgTranslateX20
    , lgTranslateX24
    , lgTranslateX3
    , lgTranslateX32
    , lgTranslateX4
    , lgTranslateX40
    , lgTranslateX48
    , lgTranslateX5
    , lgTranslateX56
    , lgTranslateX6
    , lgTranslateX64
    , lgTranslateX8
    , lgTranslateXFull
    , lgTranslateXPx
    , lgTranslateY0
    , lgTranslateY1
    , lgTranslateY10
    , lgTranslateY12
    , lgTranslateY16
    , lgTranslateY1over2
    , lgTranslateY2
    , lgTranslateY20
    , lgTranslateY24
    , lgTranslateY3
    , lgTranslateY32
    , lgTranslateY4
    , lgTranslateY40
    , lgTranslateY48
    , lgTranslateY5
    , lgTranslateY56
    , lgTranslateY6
    , lgTranslateY64
    , lgTranslateY8
    , lgTranslateYFull
    , lgTranslateYPx
    , lgTruncate
    , lgUnderline
    , lgUppercase
    , lgViaBlack
    , lgViaBlue100
    , lgViaBlue200
    , lgViaBlue300
    , lgViaBlue400
    , lgViaBlue500
    , lgViaBlue600
    , lgViaBlue700
    , lgViaBlue800
    , lgViaBlue900
    , lgViaCurrent
    , lgViaGray100
    , lgViaGray200
    , lgViaGray300
    , lgViaGray400
    , lgViaGray500
    , lgViaGray600
    , lgViaGray700
    , lgViaGray800
    , lgViaGray900
    , lgViaGreen100
    , lgViaGreen200
    , lgViaGreen300
    , lgViaGreen400
    , lgViaGreen500
    , lgViaGreen600
    , lgViaGreen700
    , lgViaGreen800
    , lgViaGreen900
    , lgViaIndigo100
    , lgViaIndigo200
    , lgViaIndigo300
    , lgViaIndigo400
    , lgViaIndigo500
    , lgViaIndigo600
    , lgViaIndigo700
    , lgViaIndigo800
    , lgViaIndigo900
    , lgViaOrange100
    , lgViaOrange200
    , lgViaOrange300
    , lgViaOrange400
    , lgViaOrange500
    , lgViaOrange600
    , lgViaOrange700
    , lgViaOrange800
    , lgViaOrange900
    , lgViaPink100
    , lgViaPink200
    , lgViaPink300
    , lgViaPink400
    , lgViaPink500
    , lgViaPink600
    , lgViaPink700
    , lgViaPink800
    , lgViaPink900
    , lgViaPurple100
    , lgViaPurple200
    , lgViaPurple300
    , lgViaPurple400
    , lgViaPurple500
    , lgViaPurple600
    , lgViaPurple700
    , lgViaPurple800
    , lgViaPurple900
    , lgViaRed100
    , lgViaRed200
    , lgViaRed300
    , lgViaRed400
    , lgViaRed500
    , lgViaRed600
    , lgViaRed700
    , lgViaRed800
    , lgViaRed900
    , lgViaTeal100
    , lgViaTeal200
    , lgViaTeal300
    , lgViaTeal400
    , lgViaTeal500
    , lgViaTeal600
    , lgViaTeal700
    , lgViaTeal800
    , lgViaTeal900
    , lgViaTransparent
    , lgViaWhite
    , lgViaYellow100
    , lgViaYellow200
    , lgViaYellow300
    , lgViaYellow400
    , lgViaYellow500
    , lgViaYellow600
    , lgViaYellow700
    , lgViaYellow800
    , lgViaYellow900
    , lgVisible
    , lgW0
    , lgW1
    , lgW10
    , lgW10over12
    , lgW11over12
    , lgW12
    , lgW16
    , lgW1over12
    , lgW1over2
    , lgW1over3
    , lgW1over4
    , lgW1over5
    , lgW1over6
    , lgW2
    , lgW20
    , lgW24
    , lgW2over12
    , lgW2over3
    , lgW2over4
    , lgW2over5
    , lgW2over6
    , lgW3
    , lgW32
    , lgW3over12
    , lgW3over4
    , lgW3over5
    , lgW3over6
    , lgW4
    , lgW40
    , lgW48
    , lgW4over12
    , lgW4over5
    , lgW4over6
    , lgW5
    , lgW56
    , lgW5over12
    , lgW5over6
    , lgW6
    , lgW64
    , lgW6over12
    , lgW7over12
    , lgW8
    , lgW8over12
    , lgW9over12
    , lgWAuto
    , lgWFull
    , lgWPx
    , lgWScreen
    , lgWhitespaceNoWrap
    , lgWhitespaceNormal
    , lgWhitespacePre
    , lgWhitespacePreLine
    , lgWhitespacePreWrap
    , lgZ0
    , lgZ10
    , lgZ20
    , lgZ30
    , lgZ40
    , lgZ50
    , lgZAuto
    , lineThrough
    , liningNums
    , listDecimal
    , listDisc
    , listInside
    , listNone
    , listOutside
    , lowercase
    , m0
    , m1
    , m10
    , m12
    , m16
    , m2
    , m20
    , m24
    , m3
    , m32
    , m4
    , m40
    , m48
    , m5
    , m56
    , m6
    , m64
    , m8
    , mAuto
    , mPx
    , maxHFull
    , maxHScreen
    , maxW2xl
    , maxW3xl
    , maxW4xl
    , maxW5xl
    , maxW6xl
    , maxWFull
    , maxWLg
    , maxWMd
    , maxWNone
    , maxWScreenLg
    , maxWScreenMd
    , maxWScreenSm
    , maxWScreenXl
    , maxWSm
    , maxWXl
    , maxWXs
    , mb0
    , mb1
    , mb10
    , mb12
    , mb16
    , mb2
    , mb20
    , mb24
    , mb3
    , mb32
    , mb4
    , mb40
    , mb48
    , mb5
    , mb56
    , mb6
    , mb64
    , mb8
    , mbAuto
    , mbPx
    , mdAbsolute
    , mdAlignBaseline
    , mdAlignBottom
    , mdAlignMiddle
    , mdAlignTextBottom
    , mdAlignTextTop
    , mdAlignTop
    , mdAnimateBounce
    , mdAnimateNone
    , mdAnimatePing
    , mdAnimatePulse
    , mdAnimateSpin
    , mdAntialiased
    , mdAppearanceNone
    , mdBgAuto
    , mdBgBlack
    , mdBgBlue100
    , mdBgBlue200
    , mdBgBlue300
    , mdBgBlue400
    , mdBgBlue500
    , mdBgBlue600
    , mdBgBlue700
    , mdBgBlue800
    , mdBgBlue900
    , mdBgBottom
    , mdBgCenter
    , mdBgClipBorder
    , mdBgClipContent
    , mdBgClipPadding
    , mdBgClipText
    , mdBgContain
    , mdBgCover
    , mdBgCurrent
    , mdBgFixed
    , mdBgGradientToB
    , mdBgGradientToBl
    , mdBgGradientToBr
    , mdBgGradientToL
    , mdBgGradientToR
    , mdBgGradientToT
    , mdBgGradientToTl
    , mdBgGradientToTr
    , mdBgGray100
    , mdBgGray200
    , mdBgGray300
    , mdBgGray400
    , mdBgGray500
    , mdBgGray600
    , mdBgGray700
    , mdBgGray800
    , mdBgGray900
    , mdBgGreen100
    , mdBgGreen200
    , mdBgGreen300
    , mdBgGreen400
    , mdBgGreen500
    , mdBgGreen600
    , mdBgGreen700
    , mdBgGreen800
    , mdBgGreen900
    , mdBgIndigo100
    , mdBgIndigo200
    , mdBgIndigo300
    , mdBgIndigo400
    , mdBgIndigo500
    , mdBgIndigo600
    , mdBgIndigo700
    , mdBgIndigo800
    , mdBgIndigo900
    , mdBgLeft
    , mdBgLeftBottom
    , mdBgLeftTop
    , mdBgLocal
    , mdBgNoRepeat
    , mdBgNone
    , mdBgOpacity0
    , mdBgOpacity100
    , mdBgOpacity25
    , mdBgOpacity50
    , mdBgOpacity75
    , mdBgOrange100
    , mdBgOrange200
    , mdBgOrange300
    , mdBgOrange400
    , mdBgOrange500
    , mdBgOrange600
    , mdBgOrange700
    , mdBgOrange800
    , mdBgOrange900
    , mdBgPink100
    , mdBgPink200
    , mdBgPink300
    , mdBgPink400
    , mdBgPink500
    , mdBgPink600
    , mdBgPink700
    , mdBgPink800
    , mdBgPink900
    , mdBgPurple100
    , mdBgPurple200
    , mdBgPurple300
    , mdBgPurple400
    , mdBgPurple500
    , mdBgPurple600
    , mdBgPurple700
    , mdBgPurple800
    , mdBgPurple900
    , mdBgRed100
    , mdBgRed200
    , mdBgRed300
    , mdBgRed400
    , mdBgRed500
    , mdBgRed600
    , mdBgRed700
    , mdBgRed800
    , mdBgRed900
    , mdBgRepeat
    , mdBgRepeatRound
    , mdBgRepeatSpace
    , mdBgRepeatX
    , mdBgRepeatY
    , mdBgRight
    , mdBgRightBottom
    , mdBgRightTop
    , mdBgScroll
    , mdBgTeal100
    , mdBgTeal200
    , mdBgTeal300
    , mdBgTeal400
    , mdBgTeal500
    , mdBgTeal600
    , mdBgTeal700
    , mdBgTeal800
    , mdBgTeal900
    , mdBgTop
    , mdBgTransparent
    , mdBgWhite
    , mdBgYellow100
    , mdBgYellow200
    , mdBgYellow300
    , mdBgYellow400
    , mdBgYellow500
    , mdBgYellow600
    , mdBgYellow700
    , mdBgYellow800
    , mdBgYellow900
    , mdBlock
    , mdBorder
    , mdBorder0
    , mdBorder2
    , mdBorder4
    , mdBorder8
    , mdBorderB
    , mdBorderB0
    , mdBorderB2
    , mdBorderB4
    , mdBorderB8
    , mdBorderBlack
    , mdBorderBlue100
    , mdBorderBlue200
    , mdBorderBlue300
    , mdBorderBlue400
    , mdBorderBlue500
    , mdBorderBlue600
    , mdBorderBlue700
    , mdBorderBlue800
    , mdBorderBlue900
    , mdBorderCollapse
    , mdBorderCurrent
    , mdBorderDashed
    , mdBorderDotted
    , mdBorderDouble
    , mdBorderGray100
    , mdBorderGray200
    , mdBorderGray300
    , mdBorderGray400
    , mdBorderGray500
    , mdBorderGray600
    , mdBorderGray700
    , mdBorderGray800
    , mdBorderGray900
    , mdBorderGreen100
    , mdBorderGreen200
    , mdBorderGreen300
    , mdBorderGreen400
    , mdBorderGreen500
    , mdBorderGreen600
    , mdBorderGreen700
    , mdBorderGreen800
    , mdBorderGreen900
    , mdBorderIndigo100
    , mdBorderIndigo200
    , mdBorderIndigo300
    , mdBorderIndigo400
    , mdBorderIndigo500
    , mdBorderIndigo600
    , mdBorderIndigo700
    , mdBorderIndigo800
    , mdBorderIndigo900
    , mdBorderL
    , mdBorderL0
    , mdBorderL2
    , mdBorderL4
    , mdBorderL8
    , mdBorderNone
    , mdBorderOpacity0
    , mdBorderOpacity100
    , mdBorderOpacity25
    , mdBorderOpacity50
    , mdBorderOpacity75
    , mdBorderOrange100
    , mdBorderOrange200
    , mdBorderOrange300
    , mdBorderOrange400
    , mdBorderOrange500
    , mdBorderOrange600
    , mdBorderOrange700
    , mdBorderOrange800
    , mdBorderOrange900
    , mdBorderPink100
    , mdBorderPink200
    , mdBorderPink300
    , mdBorderPink400
    , mdBorderPink500
    , mdBorderPink600
    , mdBorderPink700
    , mdBorderPink800
    , mdBorderPink900
    , mdBorderPurple100
    , mdBorderPurple200
    , mdBorderPurple300
    , mdBorderPurple400
    , mdBorderPurple500
    , mdBorderPurple600
    , mdBorderPurple700
    , mdBorderPurple800
    , mdBorderPurple900
    , mdBorderR
    , mdBorderR0
    , mdBorderR2
    , mdBorderR4
    , mdBorderR8
    , mdBorderRed100
    , mdBorderRed200
    , mdBorderRed300
    , mdBorderRed400
    , mdBorderRed500
    , mdBorderRed600
    , mdBorderRed700
    , mdBorderRed800
    , mdBorderRed900
    , mdBorderSeparate
    , mdBorderSolid
    , mdBorderT
    , mdBorderT0
    , mdBorderT2
    , mdBorderT4
    , mdBorderT8
    , mdBorderTeal100
    , mdBorderTeal200
    , mdBorderTeal300
    , mdBorderTeal400
    , mdBorderTeal500
    , mdBorderTeal600
    , mdBorderTeal700
    , mdBorderTeal800
    , mdBorderTeal900
    , mdBorderTransparent
    , mdBorderWhite
    , mdBorderYellow100
    , mdBorderYellow200
    , mdBorderYellow300
    , mdBorderYellow400
    , mdBorderYellow500
    , mdBorderYellow600
    , mdBorderYellow700
    , mdBorderYellow800
    , mdBorderYellow900
    , mdBottom0
    , mdBottomAuto
    , mdBoxBorder
    , mdBoxContent
    , mdBreakAll
    , mdBreakNormal
    , mdBreakWords
    , mdCapitalize
    , mdClearBoth
    , mdClearLeft
    , mdClearNone
    , mdClearRight
    , mdClearfixAfter
    , mdColAuto
    , mdColEnd1
    , mdColEnd10
    , mdColEnd11
    , mdColEnd12
    , mdColEnd13
    , mdColEnd2
    , mdColEnd3
    , mdColEnd4
    , mdColEnd5
    , mdColEnd6
    , mdColEnd7
    , mdColEnd8
    , mdColEnd9
    , mdColEndAuto
    , mdColSpan1
    , mdColSpan10
    , mdColSpan11
    , mdColSpan12
    , mdColSpan2
    , mdColSpan3
    , mdColSpan4
    , mdColSpan5
    , mdColSpan6
    , mdColSpan7
    , mdColSpan8
    , mdColSpan9
    , mdColStart1
    , mdColStart10
    , mdColStart11
    , mdColStart12
    , mdColStart13
    , mdColStart2
    , mdColStart3
    , mdColStart4
    , mdColStart5
    , mdColStart6
    , mdColStart7
    , mdColStart8
    , mdColStart9
    , mdColStartAuto
    , mdContainer
    , mdContentAround
    , mdContentBetween
    , mdContentCenter
    , mdContentEnd
    , mdContentEvenly
    , mdContentStart
    , mdContents
    , mdCursorAuto
    , mdCursorDefault
    , mdCursorMove
    , mdCursorNotAllowed
    , mdCursorPointer
    , mdCursorText
    , mdCursorWait
    , mdDelay100
    , mdDelay1000
    , mdDelay150
    , mdDelay200
    , mdDelay300
    , mdDelay500
    , mdDelay700
    , mdDelay75
    , mdDiagonalFractions
    , mdDivideBlack
    , mdDivideBlue100
    , mdDivideBlue200
    , mdDivideBlue300
    , mdDivideBlue400
    , mdDivideBlue500
    , mdDivideBlue600
    , mdDivideBlue700
    , mdDivideBlue800
    , mdDivideBlue900
    , mdDivideCurrent
    , mdDivideDashed
    , mdDivideDotted
    , mdDivideDouble
    , mdDivideGray100
    , mdDivideGray200
    , mdDivideGray300
    , mdDivideGray400
    , mdDivideGray500
    , mdDivideGray600
    , mdDivideGray700
    , mdDivideGray800
    , mdDivideGray900
    , mdDivideGreen100
    , mdDivideGreen200
    , mdDivideGreen300
    , mdDivideGreen400
    , mdDivideGreen500
    , mdDivideGreen600
    , mdDivideGreen700
    , mdDivideGreen800
    , mdDivideGreen900
    , mdDivideIndigo100
    , mdDivideIndigo200
    , mdDivideIndigo300
    , mdDivideIndigo400
    , mdDivideIndigo500
    , mdDivideIndigo600
    , mdDivideIndigo700
    , mdDivideIndigo800
    , mdDivideIndigo900
    , mdDivideNone
    , mdDivideOpacity0
    , mdDivideOpacity100
    , mdDivideOpacity25
    , mdDivideOpacity50
    , mdDivideOpacity75
    , mdDivideOrange100
    , mdDivideOrange200
    , mdDivideOrange300
    , mdDivideOrange400
    , mdDivideOrange500
    , mdDivideOrange600
    , mdDivideOrange700
    , mdDivideOrange800
    , mdDivideOrange900
    , mdDividePink100
    , mdDividePink200
    , mdDividePink300
    , mdDividePink400
    , mdDividePink500
    , mdDividePink600
    , mdDividePink700
    , mdDividePink800
    , mdDividePink900
    , mdDividePurple100
    , mdDividePurple200
    , mdDividePurple300
    , mdDividePurple400
    , mdDividePurple500
    , mdDividePurple600
    , mdDividePurple700
    , mdDividePurple800
    , mdDividePurple900
    , mdDivideRed100
    , mdDivideRed200
    , mdDivideRed300
    , mdDivideRed400
    , mdDivideRed500
    , mdDivideRed600
    , mdDivideRed700
    , mdDivideRed800
    , mdDivideRed900
    , mdDivideSolid
    , mdDivideTeal100
    , mdDivideTeal200
    , mdDivideTeal300
    , mdDivideTeal400
    , mdDivideTeal500
    , mdDivideTeal600
    , mdDivideTeal700
    , mdDivideTeal800
    , mdDivideTeal900
    , mdDivideTransparent
    , mdDivideWhite
    , mdDivideX
    , mdDivideX0
    , mdDivideX2
    , mdDivideX4
    , mdDivideX8
    , mdDivideXReverse
    , mdDivideY
    , mdDivideY0
    , mdDivideY2
    , mdDivideY4
    , mdDivideY8
    , mdDivideYReverse
    , mdDivideYellow100
    , mdDivideYellow200
    , mdDivideYellow300
    , mdDivideYellow400
    , mdDivideYellow500
    , mdDivideYellow600
    , mdDivideYellow700
    , mdDivideYellow800
    , mdDivideYellow900
    , mdDuration100
    , mdDuration1000
    , mdDuration150
    , mdDuration200
    , mdDuration300
    , mdDuration500
    , mdDuration700
    , mdDuration75
    , mdEaseIn
    , mdEaseInOut
    , mdEaseLinear
    , mdEaseOut
    , mdFillCurrent
    , mdFixed
    , mdFlex
    , mdFlex1
    , mdFlexAuto
    , mdFlexCol
    , mdFlexColReverse
    , mdFlexGrow
    , mdFlexGrow0
    , mdFlexInitial
    , mdFlexNoWrap
    , mdFlexNone
    , mdFlexRow
    , mdFlexRowReverse
    , mdFlexShrink
    , mdFlexShrink0
    , mdFlexWrap
    , mdFlexWrapReverse
    , mdFloatLeft
    , mdFloatNone
    , mdFloatRight
    , mdFlowRoot
    , mdFocusBgBlack
    , mdFocusBgBlue100
    , mdFocusBgBlue200
    , mdFocusBgBlue300
    , mdFocusBgBlue400
    , mdFocusBgBlue500
    , mdFocusBgBlue600
    , mdFocusBgBlue700
    , mdFocusBgBlue800
    , mdFocusBgBlue900
    , mdFocusBgCurrent
    , mdFocusBgGray100
    , mdFocusBgGray200
    , mdFocusBgGray300
    , mdFocusBgGray400
    , mdFocusBgGray500
    , mdFocusBgGray600
    , mdFocusBgGray700
    , mdFocusBgGray800
    , mdFocusBgGray900
    , mdFocusBgGreen100
    , mdFocusBgGreen200
    , mdFocusBgGreen300
    , mdFocusBgGreen400
    , mdFocusBgGreen500
    , mdFocusBgGreen600
    , mdFocusBgGreen700
    , mdFocusBgGreen800
    , mdFocusBgGreen900
    , mdFocusBgIndigo100
    , mdFocusBgIndigo200
    , mdFocusBgIndigo300
    , mdFocusBgIndigo400
    , mdFocusBgIndigo500
    , mdFocusBgIndigo600
    , mdFocusBgIndigo700
    , mdFocusBgIndigo800
    , mdFocusBgIndigo900
    , mdFocusBgOpacity0
    , mdFocusBgOpacity100
    , mdFocusBgOpacity25
    , mdFocusBgOpacity50
    , mdFocusBgOpacity75
    , mdFocusBgOrange100
    , mdFocusBgOrange200
    , mdFocusBgOrange300
    , mdFocusBgOrange400
    , mdFocusBgOrange500
    , mdFocusBgOrange600
    , mdFocusBgOrange700
    , mdFocusBgOrange800
    , mdFocusBgOrange900
    , mdFocusBgPink100
    , mdFocusBgPink200
    , mdFocusBgPink300
    , mdFocusBgPink400
    , mdFocusBgPink500
    , mdFocusBgPink600
    , mdFocusBgPink700
    , mdFocusBgPink800
    , mdFocusBgPink900
    , mdFocusBgPurple100
    , mdFocusBgPurple200
    , mdFocusBgPurple300
    , mdFocusBgPurple400
    , mdFocusBgPurple500
    , mdFocusBgPurple600
    , mdFocusBgPurple700
    , mdFocusBgPurple800
    , mdFocusBgPurple900
    , mdFocusBgRed100
    , mdFocusBgRed200
    , mdFocusBgRed300
    , mdFocusBgRed400
    , mdFocusBgRed500
    , mdFocusBgRed600
    , mdFocusBgRed700
    , mdFocusBgRed800
    , mdFocusBgRed900
    , mdFocusBgTeal100
    , mdFocusBgTeal200
    , mdFocusBgTeal300
    , mdFocusBgTeal400
    , mdFocusBgTeal500
    , mdFocusBgTeal600
    , mdFocusBgTeal700
    , mdFocusBgTeal800
    , mdFocusBgTeal900
    , mdFocusBgTransparent
    , mdFocusBgWhite
    , mdFocusBgYellow100
    , mdFocusBgYellow200
    , mdFocusBgYellow300
    , mdFocusBgYellow400
    , mdFocusBgYellow500
    , mdFocusBgYellow600
    , mdFocusBgYellow700
    , mdFocusBgYellow800
    , mdFocusBgYellow900
    , mdFocusBorderBlack
    , mdFocusBorderBlue100
    , mdFocusBorderBlue200
    , mdFocusBorderBlue300
    , mdFocusBorderBlue400
    , mdFocusBorderBlue500
    , mdFocusBorderBlue600
    , mdFocusBorderBlue700
    , mdFocusBorderBlue800
    , mdFocusBorderBlue900
    , mdFocusBorderCurrent
    , mdFocusBorderGray100
    , mdFocusBorderGray200
    , mdFocusBorderGray300
    , mdFocusBorderGray400
    , mdFocusBorderGray500
    , mdFocusBorderGray600
    , mdFocusBorderGray700
    , mdFocusBorderGray800
    , mdFocusBorderGray900
    , mdFocusBorderGreen100
    , mdFocusBorderGreen200
    , mdFocusBorderGreen300
    , mdFocusBorderGreen400
    , mdFocusBorderGreen500
    , mdFocusBorderGreen600
    , mdFocusBorderGreen700
    , mdFocusBorderGreen800
    , mdFocusBorderGreen900
    , mdFocusBorderIndigo100
    , mdFocusBorderIndigo200
    , mdFocusBorderIndigo300
    , mdFocusBorderIndigo400
    , mdFocusBorderIndigo500
    , mdFocusBorderIndigo600
    , mdFocusBorderIndigo700
    , mdFocusBorderIndigo800
    , mdFocusBorderIndigo900
    , mdFocusBorderOpacity0
    , mdFocusBorderOpacity100
    , mdFocusBorderOpacity25
    , mdFocusBorderOpacity50
    , mdFocusBorderOpacity75
    , mdFocusBorderOrange100
    , mdFocusBorderOrange200
    , mdFocusBorderOrange300
    , mdFocusBorderOrange400
    , mdFocusBorderOrange500
    , mdFocusBorderOrange600
    , mdFocusBorderOrange700
    , mdFocusBorderOrange800
    , mdFocusBorderOrange900
    , mdFocusBorderPink100
    , mdFocusBorderPink200
    , mdFocusBorderPink300
    , mdFocusBorderPink400
    , mdFocusBorderPink500
    , mdFocusBorderPink600
    , mdFocusBorderPink700
    , mdFocusBorderPink800
    , mdFocusBorderPink900
    , mdFocusBorderPurple100
    , mdFocusBorderPurple200
    , mdFocusBorderPurple300
    , mdFocusBorderPurple400
    , mdFocusBorderPurple500
    , mdFocusBorderPurple600
    , mdFocusBorderPurple700
    , mdFocusBorderPurple800
    , mdFocusBorderPurple900
    , mdFocusBorderRed100
    , mdFocusBorderRed200
    , mdFocusBorderRed300
    , mdFocusBorderRed400
    , mdFocusBorderRed500
    , mdFocusBorderRed600
    , mdFocusBorderRed700
    , mdFocusBorderRed800
    , mdFocusBorderRed900
    , mdFocusBorderTeal100
    , mdFocusBorderTeal200
    , mdFocusBorderTeal300
    , mdFocusBorderTeal400
    , mdFocusBorderTeal500
    , mdFocusBorderTeal600
    , mdFocusBorderTeal700
    , mdFocusBorderTeal800
    , mdFocusBorderTeal900
    , mdFocusBorderTransparent
    , mdFocusBorderWhite
    , mdFocusBorderYellow100
    , mdFocusBorderYellow200
    , mdFocusBorderYellow300
    , mdFocusBorderYellow400
    , mdFocusBorderYellow500
    , mdFocusBorderYellow600
    , mdFocusBorderYellow700
    , mdFocusBorderYellow800
    , mdFocusBorderYellow900
    , mdFocusFontBlack
    , mdFocusFontBold
    , mdFocusFontExtrabold
    , mdFocusFontHairline
    , mdFocusFontLight
    , mdFocusFontMedium
    , mdFocusFontNormal
    , mdFocusFontSemibold
    , mdFocusFontThin
    , mdFocusFromBlack
    , mdFocusFromBlue100
    , mdFocusFromBlue200
    , mdFocusFromBlue300
    , mdFocusFromBlue400
    , mdFocusFromBlue500
    , mdFocusFromBlue600
    , mdFocusFromBlue700
    , mdFocusFromBlue800
    , mdFocusFromBlue900
    , mdFocusFromCurrent
    , mdFocusFromGray100
    , mdFocusFromGray200
    , mdFocusFromGray300
    , mdFocusFromGray400
    , mdFocusFromGray500
    , mdFocusFromGray600
    , mdFocusFromGray700
    , mdFocusFromGray800
    , mdFocusFromGray900
    , mdFocusFromGreen100
    , mdFocusFromGreen200
    , mdFocusFromGreen300
    , mdFocusFromGreen400
    , mdFocusFromGreen500
    , mdFocusFromGreen600
    , mdFocusFromGreen700
    , mdFocusFromGreen800
    , mdFocusFromGreen900
    , mdFocusFromIndigo100
    , mdFocusFromIndigo200
    , mdFocusFromIndigo300
    , mdFocusFromIndigo400
    , mdFocusFromIndigo500
    , mdFocusFromIndigo600
    , mdFocusFromIndigo700
    , mdFocusFromIndigo800
    , mdFocusFromIndigo900
    , mdFocusFromOrange100
    , mdFocusFromOrange200
    , mdFocusFromOrange300
    , mdFocusFromOrange400
    , mdFocusFromOrange500
    , mdFocusFromOrange600
    , mdFocusFromOrange700
    , mdFocusFromOrange800
    , mdFocusFromOrange900
    , mdFocusFromPink100
    , mdFocusFromPink200
    , mdFocusFromPink300
    , mdFocusFromPink400
    , mdFocusFromPink500
    , mdFocusFromPink600
    , mdFocusFromPink700
    , mdFocusFromPink800
    , mdFocusFromPink900
    , mdFocusFromPurple100
    , mdFocusFromPurple200
    , mdFocusFromPurple300
    , mdFocusFromPurple400
    , mdFocusFromPurple500
    , mdFocusFromPurple600
    , mdFocusFromPurple700
    , mdFocusFromPurple800
    , mdFocusFromPurple900
    , mdFocusFromRed100
    , mdFocusFromRed200
    , mdFocusFromRed300
    , mdFocusFromRed400
    , mdFocusFromRed500
    , mdFocusFromRed600
    , mdFocusFromRed700
    , mdFocusFromRed800
    , mdFocusFromRed900
    , mdFocusFromTeal100
    , mdFocusFromTeal200
    , mdFocusFromTeal300
    , mdFocusFromTeal400
    , mdFocusFromTeal500
    , mdFocusFromTeal600
    , mdFocusFromTeal700
    , mdFocusFromTeal800
    , mdFocusFromTeal900
    , mdFocusFromTransparent
    , mdFocusFromWhite
    , mdFocusFromYellow100
    , mdFocusFromYellow200
    , mdFocusFromYellow300
    , mdFocusFromYellow400
    , mdFocusFromYellow500
    , mdFocusFromYellow600
    , mdFocusFromYellow700
    , mdFocusFromYellow800
    , mdFocusFromYellow900
    , mdFocusLineThrough
    , mdFocusNegRotate180
    , mdFocusNegRotate45
    , mdFocusNegRotate90
    , mdFocusNegSkewX12
    , mdFocusNegSkewX3
    , mdFocusNegSkewX6
    , mdFocusNegSkewY12
    , mdFocusNegSkewY3
    , mdFocusNegSkewY6
    , mdFocusNegTranslateX1
    , mdFocusNegTranslateX10
    , mdFocusNegTranslateX12
    , mdFocusNegTranslateX16
    , mdFocusNegTranslateX1over2
    , mdFocusNegTranslateX2
    , mdFocusNegTranslateX20
    , mdFocusNegTranslateX24
    , mdFocusNegTranslateX3
    , mdFocusNegTranslateX32
    , mdFocusNegTranslateX4
    , mdFocusNegTranslateX40
    , mdFocusNegTranslateX48
    , mdFocusNegTranslateX5
    , mdFocusNegTranslateX56
    , mdFocusNegTranslateX6
    , mdFocusNegTranslateX64
    , mdFocusNegTranslateX8
    , mdFocusNegTranslateXFull
    , mdFocusNegTranslateXPx
    , mdFocusNegTranslateY1
    , mdFocusNegTranslateY10
    , mdFocusNegTranslateY12
    , mdFocusNegTranslateY16
    , mdFocusNegTranslateY1over2
    , mdFocusNegTranslateY2
    , mdFocusNegTranslateY20
    , mdFocusNegTranslateY24
    , mdFocusNegTranslateY3
    , mdFocusNegTranslateY32
    , mdFocusNegTranslateY4
    , mdFocusNegTranslateY40
    , mdFocusNegTranslateY48
    , mdFocusNegTranslateY5
    , mdFocusNegTranslateY56
    , mdFocusNegTranslateY6
    , mdFocusNegTranslateY64
    , mdFocusNegTranslateY8
    , mdFocusNegTranslateYFull
    , mdFocusNegTranslateYPx
    , mdFocusNoUnderline
    , mdFocusNotSrOnly
    , mdFocusOpacity0
    , mdFocusOpacity100
    , mdFocusOpacity25
    , mdFocusOpacity50
    , mdFocusOpacity75
    , mdFocusOutlineNone
    , mdFocusPlaceholderBlackFocus
    , mdFocusPlaceholderBlue100Focus
    , mdFocusPlaceholderBlue200Focus
    , mdFocusPlaceholderBlue300Focus
    , mdFocusPlaceholderBlue400Focus
    , mdFocusPlaceholderBlue500Focus
    , mdFocusPlaceholderBlue600Focus
    , mdFocusPlaceholderBlue700Focus
    , mdFocusPlaceholderBlue800Focus
    , mdFocusPlaceholderBlue900Focus
    , mdFocusPlaceholderCurrentFocus
    , mdFocusPlaceholderGray100Focus
    , mdFocusPlaceholderGray200Focus
    , mdFocusPlaceholderGray300Focus
    , mdFocusPlaceholderGray400Focus
    , mdFocusPlaceholderGray500Focus
    , mdFocusPlaceholderGray600Focus
    , mdFocusPlaceholderGray700Focus
    , mdFocusPlaceholderGray800Focus
    , mdFocusPlaceholderGray900Focus
    , mdFocusPlaceholderGreen100Focus
    , mdFocusPlaceholderGreen200Focus
    , mdFocusPlaceholderGreen300Focus
    , mdFocusPlaceholderGreen400Focus
    , mdFocusPlaceholderGreen500Focus
    , mdFocusPlaceholderGreen600Focus
    , mdFocusPlaceholderGreen700Focus
    , mdFocusPlaceholderGreen800Focus
    , mdFocusPlaceholderGreen900Focus
    , mdFocusPlaceholderIndigo100Focus
    , mdFocusPlaceholderIndigo200Focus
    , mdFocusPlaceholderIndigo300Focus
    , mdFocusPlaceholderIndigo400Focus
    , mdFocusPlaceholderIndigo500Focus
    , mdFocusPlaceholderIndigo600Focus
    , mdFocusPlaceholderIndigo700Focus
    , mdFocusPlaceholderIndigo800Focus
    , mdFocusPlaceholderIndigo900Focus
    , mdFocusPlaceholderOpacity0Focus
    , mdFocusPlaceholderOpacity100Focus
    , mdFocusPlaceholderOpacity25Focus
    , mdFocusPlaceholderOpacity50Focus
    , mdFocusPlaceholderOpacity75Focus
    , mdFocusPlaceholderOrange100Focus
    , mdFocusPlaceholderOrange200Focus
    , mdFocusPlaceholderOrange300Focus
    , mdFocusPlaceholderOrange400Focus
    , mdFocusPlaceholderOrange500Focus
    , mdFocusPlaceholderOrange600Focus
    , mdFocusPlaceholderOrange700Focus
    , mdFocusPlaceholderOrange800Focus
    , mdFocusPlaceholderOrange900Focus
    , mdFocusPlaceholderPink100Focus
    , mdFocusPlaceholderPink200Focus
    , mdFocusPlaceholderPink300Focus
    , mdFocusPlaceholderPink400Focus
    , mdFocusPlaceholderPink500Focus
    , mdFocusPlaceholderPink600Focus
    , mdFocusPlaceholderPink700Focus
    , mdFocusPlaceholderPink800Focus
    , mdFocusPlaceholderPink900Focus
    , mdFocusPlaceholderPurple100Focus
    , mdFocusPlaceholderPurple200Focus
    , mdFocusPlaceholderPurple300Focus
    , mdFocusPlaceholderPurple400Focus
    , mdFocusPlaceholderPurple500Focus
    , mdFocusPlaceholderPurple600Focus
    , mdFocusPlaceholderPurple700Focus
    , mdFocusPlaceholderPurple800Focus
    , mdFocusPlaceholderPurple900Focus
    , mdFocusPlaceholderRed100Focus
    , mdFocusPlaceholderRed200Focus
    , mdFocusPlaceholderRed300Focus
    , mdFocusPlaceholderRed400Focus
    , mdFocusPlaceholderRed500Focus
    , mdFocusPlaceholderRed600Focus
    , mdFocusPlaceholderRed700Focus
    , mdFocusPlaceholderRed800Focus
    , mdFocusPlaceholderRed900Focus
    , mdFocusPlaceholderTeal100Focus
    , mdFocusPlaceholderTeal200Focus
    , mdFocusPlaceholderTeal300Focus
    , mdFocusPlaceholderTeal400Focus
    , mdFocusPlaceholderTeal500Focus
    , mdFocusPlaceholderTeal600Focus
    , mdFocusPlaceholderTeal700Focus
    , mdFocusPlaceholderTeal800Focus
    , mdFocusPlaceholderTeal900Focus
    , mdFocusPlaceholderTransparentFocus
    , mdFocusPlaceholderWhiteFocus
    , mdFocusPlaceholderYellow100Focus
    , mdFocusPlaceholderYellow200Focus
    , mdFocusPlaceholderYellow300Focus
    , mdFocusPlaceholderYellow400Focus
    , mdFocusPlaceholderYellow500Focus
    , mdFocusPlaceholderYellow600Focus
    , mdFocusPlaceholderYellow700Focus
    , mdFocusPlaceholderYellow800Focus
    , mdFocusPlaceholderYellow900Focus
    , mdFocusRotate0
    , mdFocusRotate180
    , mdFocusRotate45
    , mdFocusRotate90
    , mdFocusScale0
    , mdFocusScale100
    , mdFocusScale105
    , mdFocusScale110
    , mdFocusScale125
    , mdFocusScale150
    , mdFocusScale50
    , mdFocusScale75
    , mdFocusScale90
    , mdFocusScale95
    , mdFocusScaleX0
    , mdFocusScaleX100
    , mdFocusScaleX105
    , mdFocusScaleX110
    , mdFocusScaleX125
    , mdFocusScaleX150
    , mdFocusScaleX50
    , mdFocusScaleX75
    , mdFocusScaleX90
    , mdFocusScaleX95
    , mdFocusScaleY0
    , mdFocusScaleY100
    , mdFocusScaleY105
    , mdFocusScaleY110
    , mdFocusScaleY125
    , mdFocusScaleY150
    , mdFocusScaleY50
    , mdFocusScaleY75
    , mdFocusScaleY90
    , mdFocusScaleY95
    , mdFocusShadow
    , mdFocusShadow2xl
    , mdFocusShadowInner
    , mdFocusShadowLg
    , mdFocusShadowMd
    , mdFocusShadowNone
    , mdFocusShadowOutline
    , mdFocusShadowSm
    , mdFocusShadowXl
    , mdFocusShadowXs
    , mdFocusSkewX0
    , mdFocusSkewX12
    , mdFocusSkewX3
    , mdFocusSkewX6
    , mdFocusSkewY0
    , mdFocusSkewY12
    , mdFocusSkewY3
    , mdFocusSkewY6
    , mdFocusSrOnly
    , mdFocusTextBlack
    , mdFocusTextBlue100
    , mdFocusTextBlue200
    , mdFocusTextBlue300
    , mdFocusTextBlue400
    , mdFocusTextBlue500
    , mdFocusTextBlue600
    , mdFocusTextBlue700
    , mdFocusTextBlue800
    , mdFocusTextBlue900
    , mdFocusTextCurrent
    , mdFocusTextGray100
    , mdFocusTextGray200
    , mdFocusTextGray300
    , mdFocusTextGray400
    , mdFocusTextGray500
    , mdFocusTextGray600
    , mdFocusTextGray700
    , mdFocusTextGray800
    , mdFocusTextGray900
    , mdFocusTextGreen100
    , mdFocusTextGreen200
    , mdFocusTextGreen300
    , mdFocusTextGreen400
    , mdFocusTextGreen500
    , mdFocusTextGreen600
    , mdFocusTextGreen700
    , mdFocusTextGreen800
    , mdFocusTextGreen900
    , mdFocusTextIndigo100
    , mdFocusTextIndigo200
    , mdFocusTextIndigo300
    , mdFocusTextIndigo400
    , mdFocusTextIndigo500
    , mdFocusTextIndigo600
    , mdFocusTextIndigo700
    , mdFocusTextIndigo800
    , mdFocusTextIndigo900
    , mdFocusTextOpacity0
    , mdFocusTextOpacity100
    , mdFocusTextOpacity25
    , mdFocusTextOpacity50
    , mdFocusTextOpacity75
    , mdFocusTextOrange100
    , mdFocusTextOrange200
    , mdFocusTextOrange300
    , mdFocusTextOrange400
    , mdFocusTextOrange500
    , mdFocusTextOrange600
    , mdFocusTextOrange700
    , mdFocusTextOrange800
    , mdFocusTextOrange900
    , mdFocusTextPink100
    , mdFocusTextPink200
    , mdFocusTextPink300
    , mdFocusTextPink400
    , mdFocusTextPink500
    , mdFocusTextPink600
    , mdFocusTextPink700
    , mdFocusTextPink800
    , mdFocusTextPink900
    , mdFocusTextPurple100
    , mdFocusTextPurple200
    , mdFocusTextPurple300
    , mdFocusTextPurple400
    , mdFocusTextPurple500
    , mdFocusTextPurple600
    , mdFocusTextPurple700
    , mdFocusTextPurple800
    , mdFocusTextPurple900
    , mdFocusTextRed100
    , mdFocusTextRed200
    , mdFocusTextRed300
    , mdFocusTextRed400
    , mdFocusTextRed500
    , mdFocusTextRed600
    , mdFocusTextRed700
    , mdFocusTextRed800
    , mdFocusTextRed900
    , mdFocusTextTeal100
    , mdFocusTextTeal200
    , mdFocusTextTeal300
    , mdFocusTextTeal400
    , mdFocusTextTeal500
    , mdFocusTextTeal600
    , mdFocusTextTeal700
    , mdFocusTextTeal800
    , mdFocusTextTeal900
    , mdFocusTextTransparent
    , mdFocusTextWhite
    , mdFocusTextYellow100
    , mdFocusTextYellow200
    , mdFocusTextYellow300
    , mdFocusTextYellow400
    , mdFocusTextYellow500
    , mdFocusTextYellow600
    , mdFocusTextYellow700
    , mdFocusTextYellow800
    , mdFocusTextYellow900
    , mdFocusToBlack
    , mdFocusToBlue100
    , mdFocusToBlue200
    , mdFocusToBlue300
    , mdFocusToBlue400
    , mdFocusToBlue500
    , mdFocusToBlue600
    , mdFocusToBlue700
    , mdFocusToBlue800
    , mdFocusToBlue900
    , mdFocusToCurrent
    , mdFocusToGray100
    , mdFocusToGray200
    , mdFocusToGray300
    , mdFocusToGray400
    , mdFocusToGray500
    , mdFocusToGray600
    , mdFocusToGray700
    , mdFocusToGray800
    , mdFocusToGray900
    , mdFocusToGreen100
    , mdFocusToGreen200
    , mdFocusToGreen300
    , mdFocusToGreen400
    , mdFocusToGreen500
    , mdFocusToGreen600
    , mdFocusToGreen700
    , mdFocusToGreen800
    , mdFocusToGreen900
    , mdFocusToIndigo100
    , mdFocusToIndigo200
    , mdFocusToIndigo300
    , mdFocusToIndigo400
    , mdFocusToIndigo500
    , mdFocusToIndigo600
    , mdFocusToIndigo700
    , mdFocusToIndigo800
    , mdFocusToIndigo900
    , mdFocusToOrange100
    , mdFocusToOrange200
    , mdFocusToOrange300
    , mdFocusToOrange400
    , mdFocusToOrange500
    , mdFocusToOrange600
    , mdFocusToOrange700
    , mdFocusToOrange800
    , mdFocusToOrange900
    , mdFocusToPink100
    , mdFocusToPink200
    , mdFocusToPink300
    , mdFocusToPink400
    , mdFocusToPink500
    , mdFocusToPink600
    , mdFocusToPink700
    , mdFocusToPink800
    , mdFocusToPink900
    , mdFocusToPurple100
    , mdFocusToPurple200
    , mdFocusToPurple300
    , mdFocusToPurple400
    , mdFocusToPurple500
    , mdFocusToPurple600
    , mdFocusToPurple700
    , mdFocusToPurple800
    , mdFocusToPurple900
    , mdFocusToRed100
    , mdFocusToRed200
    , mdFocusToRed300
    , mdFocusToRed400
    , mdFocusToRed500
    , mdFocusToRed600
    , mdFocusToRed700
    , mdFocusToRed800
    , mdFocusToRed900
    , mdFocusToTeal100
    , mdFocusToTeal200
    , mdFocusToTeal300
    , mdFocusToTeal400
    , mdFocusToTeal500
    , mdFocusToTeal600
    , mdFocusToTeal700
    , mdFocusToTeal800
    , mdFocusToTeal900
    , mdFocusToTransparent
    , mdFocusToWhite
    , mdFocusToYellow100
    , mdFocusToYellow200
    , mdFocusToYellow300
    , mdFocusToYellow400
    , mdFocusToYellow500
    , mdFocusToYellow600
    , mdFocusToYellow700
    , mdFocusToYellow800
    , mdFocusToYellow900
    , mdFocusTranslateX0
    , mdFocusTranslateX1
    , mdFocusTranslateX10
    , mdFocusTranslateX12
    , mdFocusTranslateX16
    , mdFocusTranslateX1over2
    , mdFocusTranslateX2
    , mdFocusTranslateX20
    , mdFocusTranslateX24
    , mdFocusTranslateX3
    , mdFocusTranslateX32
    , mdFocusTranslateX4
    , mdFocusTranslateX40
    , mdFocusTranslateX48
    , mdFocusTranslateX5
    , mdFocusTranslateX56
    , mdFocusTranslateX6
    , mdFocusTranslateX64
    , mdFocusTranslateX8
    , mdFocusTranslateXFull
    , mdFocusTranslateXPx
    , mdFocusTranslateY0
    , mdFocusTranslateY1
    , mdFocusTranslateY10
    , mdFocusTranslateY12
    , mdFocusTranslateY16
    , mdFocusTranslateY1over2
    , mdFocusTranslateY2
    , mdFocusTranslateY20
    , mdFocusTranslateY24
    , mdFocusTranslateY3
    , mdFocusTranslateY32
    , mdFocusTranslateY4
    , mdFocusTranslateY40
    , mdFocusTranslateY48
    , mdFocusTranslateY5
    , mdFocusTranslateY56
    , mdFocusTranslateY6
    , mdFocusTranslateY64
    , mdFocusTranslateY8
    , mdFocusTranslateYFull
    , mdFocusTranslateYPx
    , mdFocusUnderline
    , mdFocusViaBlack
    , mdFocusViaBlue100
    , mdFocusViaBlue200
    , mdFocusViaBlue300
    , mdFocusViaBlue400
    , mdFocusViaBlue500
    , mdFocusViaBlue600
    , mdFocusViaBlue700
    , mdFocusViaBlue800
    , mdFocusViaBlue900
    , mdFocusViaCurrent
    , mdFocusViaGray100
    , mdFocusViaGray200
    , mdFocusViaGray300
    , mdFocusViaGray400
    , mdFocusViaGray500
    , mdFocusViaGray600
    , mdFocusViaGray700
    , mdFocusViaGray800
    , mdFocusViaGray900
    , mdFocusViaGreen100
    , mdFocusViaGreen200
    , mdFocusViaGreen300
    , mdFocusViaGreen400
    , mdFocusViaGreen500
    , mdFocusViaGreen600
    , mdFocusViaGreen700
    , mdFocusViaGreen800
    , mdFocusViaGreen900
    , mdFocusViaIndigo100
    , mdFocusViaIndigo200
    , mdFocusViaIndigo300
    , mdFocusViaIndigo400
    , mdFocusViaIndigo500
    , mdFocusViaIndigo600
    , mdFocusViaIndigo700
    , mdFocusViaIndigo800
    , mdFocusViaIndigo900
    , mdFocusViaOrange100
    , mdFocusViaOrange200
    , mdFocusViaOrange300
    , mdFocusViaOrange400
    , mdFocusViaOrange500
    , mdFocusViaOrange600
    , mdFocusViaOrange700
    , mdFocusViaOrange800
    , mdFocusViaOrange900
    , mdFocusViaPink100
    , mdFocusViaPink200
    , mdFocusViaPink300
    , mdFocusViaPink400
    , mdFocusViaPink500
    , mdFocusViaPink600
    , mdFocusViaPink700
    , mdFocusViaPink800
    , mdFocusViaPink900
    , mdFocusViaPurple100
    , mdFocusViaPurple200
    , mdFocusViaPurple300
    , mdFocusViaPurple400
    , mdFocusViaPurple500
    , mdFocusViaPurple600
    , mdFocusViaPurple700
    , mdFocusViaPurple800
    , mdFocusViaPurple900
    , mdFocusViaRed100
    , mdFocusViaRed200
    , mdFocusViaRed300
    , mdFocusViaRed400
    , mdFocusViaRed500
    , mdFocusViaRed600
    , mdFocusViaRed700
    , mdFocusViaRed800
    , mdFocusViaRed900
    , mdFocusViaTeal100
    , mdFocusViaTeal200
    , mdFocusViaTeal300
    , mdFocusViaTeal400
    , mdFocusViaTeal500
    , mdFocusViaTeal600
    , mdFocusViaTeal700
    , mdFocusViaTeal800
    , mdFocusViaTeal900
    , mdFocusViaTransparent
    , mdFocusViaWhite
    , mdFocusViaYellow100
    , mdFocusViaYellow200
    , mdFocusViaYellow300
    , mdFocusViaYellow400
    , mdFocusViaYellow500
    , mdFocusViaYellow600
    , mdFocusViaYellow700
    , mdFocusViaYellow800
    , mdFocusViaYellow900
    , mdFontBlack
    , mdFontBold
    , mdFontExtrabold
    , mdFontHairline
    , mdFontLight
    , mdFontMedium
    , mdFontMono
    , mdFontNormal
    , mdFontSans
    , mdFontSemibold
    , mdFontSerif
    , mdFontThin
    , mdFromBlack
    , mdFromBlue100
    , mdFromBlue200
    , mdFromBlue300
    , mdFromBlue400
    , mdFromBlue500
    , mdFromBlue600
    , mdFromBlue700
    , mdFromBlue800
    , mdFromBlue900
    , mdFromCurrent
    , mdFromGray100
    , mdFromGray200
    , mdFromGray300
    , mdFromGray400
    , mdFromGray500
    , mdFromGray600
    , mdFromGray700
    , mdFromGray800
    , mdFromGray900
    , mdFromGreen100
    , mdFromGreen200
    , mdFromGreen300
    , mdFromGreen400
    , mdFromGreen500
    , mdFromGreen600
    , mdFromGreen700
    , mdFromGreen800
    , mdFromGreen900
    , mdFromIndigo100
    , mdFromIndigo200
    , mdFromIndigo300
    , mdFromIndigo400
    , mdFromIndigo500
    , mdFromIndigo600
    , mdFromIndigo700
    , mdFromIndigo800
    , mdFromIndigo900
    , mdFromOrange100
    , mdFromOrange200
    , mdFromOrange300
    , mdFromOrange400
    , mdFromOrange500
    , mdFromOrange600
    , mdFromOrange700
    , mdFromOrange800
    , mdFromOrange900
    , mdFromPink100
    , mdFromPink200
    , mdFromPink300
    , mdFromPink400
    , mdFromPink500
    , mdFromPink600
    , mdFromPink700
    , mdFromPink800
    , mdFromPink900
    , mdFromPurple100
    , mdFromPurple200
    , mdFromPurple300
    , mdFromPurple400
    , mdFromPurple500
    , mdFromPurple600
    , mdFromPurple700
    , mdFromPurple800
    , mdFromPurple900
    , mdFromRed100
    , mdFromRed200
    , mdFromRed300
    , mdFromRed400
    , mdFromRed500
    , mdFromRed600
    , mdFromRed700
    , mdFromRed800
    , mdFromRed900
    , mdFromTeal100
    , mdFromTeal200
    , mdFromTeal300
    , mdFromTeal400
    , mdFromTeal500
    , mdFromTeal600
    , mdFromTeal700
    , mdFromTeal800
    , mdFromTeal900
    , mdFromTransparent
    , mdFromWhite
    , mdFromYellow100
    , mdFromYellow200
    , mdFromYellow300
    , mdFromYellow400
    , mdFromYellow500
    , mdFromYellow600
    , mdFromYellow700
    , mdFromYellow800
    , mdFromYellow900
    , mdGap0
    , mdGap1
    , mdGap10
    , mdGap12
    , mdGap16
    , mdGap2
    , mdGap20
    , mdGap24
    , mdGap3
    , mdGap32
    , mdGap4
    , mdGap40
    , mdGap48
    , mdGap5
    , mdGap56
    , mdGap6
    , mdGap64
    , mdGap8
    , mdGapPx
    , mdGapX0
    , mdGapX1
    , mdGapX10
    , mdGapX12
    , mdGapX16
    , mdGapX2
    , mdGapX20
    , mdGapX24
    , mdGapX3
    , mdGapX32
    , mdGapX4
    , mdGapX40
    , mdGapX48
    , mdGapX5
    , mdGapX56
    , mdGapX6
    , mdGapX64
    , mdGapX8
    , mdGapXPx
    , mdGapY0
    , mdGapY1
    , mdGapY10
    , mdGapY12
    , mdGapY16
    , mdGapY2
    , mdGapY20
    , mdGapY24
    , mdGapY3
    , mdGapY32
    , mdGapY4
    , mdGapY40
    , mdGapY48
    , mdGapY5
    , mdGapY56
    , mdGapY6
    , mdGapY64
    , mdGapY8
    , mdGapYPx
    , mdGrid
    , mdGridCols1
    , mdGridCols10
    , mdGridCols11
    , mdGridCols12
    , mdGridCols2
    , mdGridCols3
    , mdGridCols4
    , mdGridCols5
    , mdGridCols6
    , mdGridCols7
    , mdGridCols8
    , mdGridCols9
    , mdGridColsNone
    , mdGridFlowCol
    , mdGridFlowColDense
    , mdGridFlowRow
    , mdGridFlowRowDense
    , mdGridRows1
    , mdGridRows2
    , mdGridRows3
    , mdGridRows4
    , mdGridRows5
    , mdGridRows6
    , mdGridRowsNone
    , mdH0
    , mdH1
    , mdH10
    , mdH12
    , mdH16
    , mdH2
    , mdH20
    , mdH24
    , mdH3
    , mdH32
    , mdH4
    , mdH40
    , mdH48
    , mdH5
    , mdH56
    , mdH6
    , mdH64
    , mdH8
    , mdHAuto
    , mdHFull
    , mdHPx
    , mdHScreen
    , mdHidden
    , mdHoverBgBlack
    , mdHoverBgBlue100
    , mdHoverBgBlue200
    , mdHoverBgBlue300
    , mdHoverBgBlue400
    , mdHoverBgBlue500
    , mdHoverBgBlue600
    , mdHoverBgBlue700
    , mdHoverBgBlue800
    , mdHoverBgBlue900
    , mdHoverBgCurrent
    , mdHoverBgGray100
    , mdHoverBgGray200
    , mdHoverBgGray300
    , mdHoverBgGray400
    , mdHoverBgGray500
    , mdHoverBgGray600
    , mdHoverBgGray700
    , mdHoverBgGray800
    , mdHoverBgGray900
    , mdHoverBgGreen100
    , mdHoverBgGreen200
    , mdHoverBgGreen300
    , mdHoverBgGreen400
    , mdHoverBgGreen500
    , mdHoverBgGreen600
    , mdHoverBgGreen700
    , mdHoverBgGreen800
    , mdHoverBgGreen900
    , mdHoverBgIndigo100
    , mdHoverBgIndigo200
    , mdHoverBgIndigo300
    , mdHoverBgIndigo400
    , mdHoverBgIndigo500
    , mdHoverBgIndigo600
    , mdHoverBgIndigo700
    , mdHoverBgIndigo800
    , mdHoverBgIndigo900
    , mdHoverBgOpacity0
    , mdHoverBgOpacity100
    , mdHoverBgOpacity25
    , mdHoverBgOpacity50
    , mdHoverBgOpacity75
    , mdHoverBgOrange100
    , mdHoverBgOrange200
    , mdHoverBgOrange300
    , mdHoverBgOrange400
    , mdHoverBgOrange500
    , mdHoverBgOrange600
    , mdHoverBgOrange700
    , mdHoverBgOrange800
    , mdHoverBgOrange900
    , mdHoverBgPink100
    , mdHoverBgPink200
    , mdHoverBgPink300
    , mdHoverBgPink400
    , mdHoverBgPink500
    , mdHoverBgPink600
    , mdHoverBgPink700
    , mdHoverBgPink800
    , mdHoverBgPink900
    , mdHoverBgPurple100
    , mdHoverBgPurple200
    , mdHoverBgPurple300
    , mdHoverBgPurple400
    , mdHoverBgPurple500
    , mdHoverBgPurple600
    , mdHoverBgPurple700
    , mdHoverBgPurple800
    , mdHoverBgPurple900
    , mdHoverBgRed100
    , mdHoverBgRed200
    , mdHoverBgRed300
    , mdHoverBgRed400
    , mdHoverBgRed500
    , mdHoverBgRed600
    , mdHoverBgRed700
    , mdHoverBgRed800
    , mdHoverBgRed900
    , mdHoverBgTeal100
    , mdHoverBgTeal200
    , mdHoverBgTeal300
    , mdHoverBgTeal400
    , mdHoverBgTeal500
    , mdHoverBgTeal600
    , mdHoverBgTeal700
    , mdHoverBgTeal800
    , mdHoverBgTeal900
    , mdHoverBgTransparent
    , mdHoverBgWhite
    , mdHoverBgYellow100
    , mdHoverBgYellow200
    , mdHoverBgYellow300
    , mdHoverBgYellow400
    , mdHoverBgYellow500
    , mdHoverBgYellow600
    , mdHoverBgYellow700
    , mdHoverBgYellow800
    , mdHoverBgYellow900
    , mdHoverBorderBlack
    , mdHoverBorderBlue100
    , mdHoverBorderBlue200
    , mdHoverBorderBlue300
    , mdHoverBorderBlue400
    , mdHoverBorderBlue500
    , mdHoverBorderBlue600
    , mdHoverBorderBlue700
    , mdHoverBorderBlue800
    , mdHoverBorderBlue900
    , mdHoverBorderCurrent
    , mdHoverBorderGray100
    , mdHoverBorderGray200
    , mdHoverBorderGray300
    , mdHoverBorderGray400
    , mdHoverBorderGray500
    , mdHoverBorderGray600
    , mdHoverBorderGray700
    , mdHoverBorderGray800
    , mdHoverBorderGray900
    , mdHoverBorderGreen100
    , mdHoverBorderGreen200
    , mdHoverBorderGreen300
    , mdHoverBorderGreen400
    , mdHoverBorderGreen500
    , mdHoverBorderGreen600
    , mdHoverBorderGreen700
    , mdHoverBorderGreen800
    , mdHoverBorderGreen900
    , mdHoverBorderIndigo100
    , mdHoverBorderIndigo200
    , mdHoverBorderIndigo300
    , mdHoverBorderIndigo400
    , mdHoverBorderIndigo500
    , mdHoverBorderIndigo600
    , mdHoverBorderIndigo700
    , mdHoverBorderIndigo800
    , mdHoverBorderIndigo900
    , mdHoverBorderOpacity0
    , mdHoverBorderOpacity100
    , mdHoverBorderOpacity25
    , mdHoverBorderOpacity50
    , mdHoverBorderOpacity75
    , mdHoverBorderOrange100
    , mdHoverBorderOrange200
    , mdHoverBorderOrange300
    , mdHoverBorderOrange400
    , mdHoverBorderOrange500
    , mdHoverBorderOrange600
    , mdHoverBorderOrange700
    , mdHoverBorderOrange800
    , mdHoverBorderOrange900
    , mdHoverBorderPink100
    , mdHoverBorderPink200
    , mdHoverBorderPink300
    , mdHoverBorderPink400
    , mdHoverBorderPink500
    , mdHoverBorderPink600
    , mdHoverBorderPink700
    , mdHoverBorderPink800
    , mdHoverBorderPink900
    , mdHoverBorderPurple100
    , mdHoverBorderPurple200
    , mdHoverBorderPurple300
    , mdHoverBorderPurple400
    , mdHoverBorderPurple500
    , mdHoverBorderPurple600
    , mdHoverBorderPurple700
    , mdHoverBorderPurple800
    , mdHoverBorderPurple900
    , mdHoverBorderRed100
    , mdHoverBorderRed200
    , mdHoverBorderRed300
    , mdHoverBorderRed400
    , mdHoverBorderRed500
    , mdHoverBorderRed600
    , mdHoverBorderRed700
    , mdHoverBorderRed800
    , mdHoverBorderRed900
    , mdHoverBorderTeal100
    , mdHoverBorderTeal200
    , mdHoverBorderTeal300
    , mdHoverBorderTeal400
    , mdHoverBorderTeal500
    , mdHoverBorderTeal600
    , mdHoverBorderTeal700
    , mdHoverBorderTeal800
    , mdHoverBorderTeal900
    , mdHoverBorderTransparent
    , mdHoverBorderWhite
    , mdHoverBorderYellow100
    , mdHoverBorderYellow200
    , mdHoverBorderYellow300
    , mdHoverBorderYellow400
    , mdHoverBorderYellow500
    , mdHoverBorderYellow600
    , mdHoverBorderYellow700
    , mdHoverBorderYellow800
    , mdHoverBorderYellow900
    , mdHoverFontBlack
    , mdHoverFontBold
    , mdHoverFontExtrabold
    , mdHoverFontHairline
    , mdHoverFontLight
    , mdHoverFontMedium
    , mdHoverFontNormal
    , mdHoverFontSemibold
    , mdHoverFontThin
    , mdHoverFromBlack
    , mdHoverFromBlue100
    , mdHoverFromBlue200
    , mdHoverFromBlue300
    , mdHoverFromBlue400
    , mdHoverFromBlue500
    , mdHoverFromBlue600
    , mdHoverFromBlue700
    , mdHoverFromBlue800
    , mdHoverFromBlue900
    , mdHoverFromCurrent
    , mdHoverFromGray100
    , mdHoverFromGray200
    , mdHoverFromGray300
    , mdHoverFromGray400
    , mdHoverFromGray500
    , mdHoverFromGray600
    , mdHoverFromGray700
    , mdHoverFromGray800
    , mdHoverFromGray900
    , mdHoverFromGreen100
    , mdHoverFromGreen200
    , mdHoverFromGreen300
    , mdHoverFromGreen400
    , mdHoverFromGreen500
    , mdHoverFromGreen600
    , mdHoverFromGreen700
    , mdHoverFromGreen800
    , mdHoverFromGreen900
    , mdHoverFromIndigo100
    , mdHoverFromIndigo200
    , mdHoverFromIndigo300
    , mdHoverFromIndigo400
    , mdHoverFromIndigo500
    , mdHoverFromIndigo600
    , mdHoverFromIndigo700
    , mdHoverFromIndigo800
    , mdHoverFromIndigo900
    , mdHoverFromOrange100
    , mdHoverFromOrange200
    , mdHoverFromOrange300
    , mdHoverFromOrange400
    , mdHoverFromOrange500
    , mdHoverFromOrange600
    , mdHoverFromOrange700
    , mdHoverFromOrange800
    , mdHoverFromOrange900
    , mdHoverFromPink100
    , mdHoverFromPink200
    , mdHoverFromPink300
    , mdHoverFromPink400
    , mdHoverFromPink500
    , mdHoverFromPink600
    , mdHoverFromPink700
    , mdHoverFromPink800
    , mdHoverFromPink900
    , mdHoverFromPurple100
    , mdHoverFromPurple200
    , mdHoverFromPurple300
    , mdHoverFromPurple400
    , mdHoverFromPurple500
    , mdHoverFromPurple600
    , mdHoverFromPurple700
    , mdHoverFromPurple800
    , mdHoverFromPurple900
    , mdHoverFromRed100
    , mdHoverFromRed200
    , mdHoverFromRed300
    , mdHoverFromRed400
    , mdHoverFromRed500
    , mdHoverFromRed600
    , mdHoverFromRed700
    , mdHoverFromRed800
    , mdHoverFromRed900
    , mdHoverFromTeal100
    , mdHoverFromTeal200
    , mdHoverFromTeal300
    , mdHoverFromTeal400
    , mdHoverFromTeal500
    , mdHoverFromTeal600
    , mdHoverFromTeal700
    , mdHoverFromTeal800
    , mdHoverFromTeal900
    , mdHoverFromTransparent
    , mdHoverFromWhite
    , mdHoverFromYellow100
    , mdHoverFromYellow200
    , mdHoverFromYellow300
    , mdHoverFromYellow400
    , mdHoverFromYellow500
    , mdHoverFromYellow600
    , mdHoverFromYellow700
    , mdHoverFromYellow800
    , mdHoverFromYellow900
    , mdHoverLineThrough
    , mdHoverNegRotate180
    , mdHoverNegRotate45
    , mdHoverNegRotate90
    , mdHoverNegSkewX12
    , mdHoverNegSkewX3
    , mdHoverNegSkewX6
    , mdHoverNegSkewY12
    , mdHoverNegSkewY3
    , mdHoverNegSkewY6
    , mdHoverNegTranslateX1
    , mdHoverNegTranslateX10
    , mdHoverNegTranslateX12
    , mdHoverNegTranslateX16
    , mdHoverNegTranslateX1over2
    , mdHoverNegTranslateX2
    , mdHoverNegTranslateX20
    , mdHoverNegTranslateX24
    , mdHoverNegTranslateX3
    , mdHoverNegTranslateX32
    , mdHoverNegTranslateX4
    , mdHoverNegTranslateX40
    , mdHoverNegTranslateX48
    , mdHoverNegTranslateX5
    , mdHoverNegTranslateX56
    , mdHoverNegTranslateX6
    , mdHoverNegTranslateX64
    , mdHoverNegTranslateX8
    , mdHoverNegTranslateXFull
    , mdHoverNegTranslateXPx
    , mdHoverNegTranslateY1
    , mdHoverNegTranslateY10
    , mdHoverNegTranslateY12
    , mdHoverNegTranslateY16
    , mdHoverNegTranslateY1over2
    , mdHoverNegTranslateY2
    , mdHoverNegTranslateY20
    , mdHoverNegTranslateY24
    , mdHoverNegTranslateY3
    , mdHoverNegTranslateY32
    , mdHoverNegTranslateY4
    , mdHoverNegTranslateY40
    , mdHoverNegTranslateY48
    , mdHoverNegTranslateY5
    , mdHoverNegTranslateY56
    , mdHoverNegTranslateY6
    , mdHoverNegTranslateY64
    , mdHoverNegTranslateY8
    , mdHoverNegTranslateYFull
    , mdHoverNegTranslateYPx
    , mdHoverNoUnderline
    , mdHoverOpacity0
    , mdHoverOpacity100
    , mdHoverOpacity25
    , mdHoverOpacity50
    , mdHoverOpacity75
    , mdHoverRotate0
    , mdHoverRotate180
    , mdHoverRotate45
    , mdHoverRotate90
    , mdHoverScale0
    , mdHoverScale100
    , mdHoverScale105
    , mdHoverScale110
    , mdHoverScale125
    , mdHoverScale150
    , mdHoverScale50
    , mdHoverScale75
    , mdHoverScale90
    , mdHoverScale95
    , mdHoverScaleX0
    , mdHoverScaleX100
    , mdHoverScaleX105
    , mdHoverScaleX110
    , mdHoverScaleX125
    , mdHoverScaleX150
    , mdHoverScaleX50
    , mdHoverScaleX75
    , mdHoverScaleX90
    , mdHoverScaleX95
    , mdHoverScaleY0
    , mdHoverScaleY100
    , mdHoverScaleY105
    , mdHoverScaleY110
    , mdHoverScaleY125
    , mdHoverScaleY150
    , mdHoverScaleY50
    , mdHoverScaleY75
    , mdHoverScaleY90
    , mdHoverScaleY95
    , mdHoverShadow
    , mdHoverShadow2xl
    , mdHoverShadowInner
    , mdHoverShadowLg
    , mdHoverShadowMd
    , mdHoverShadowNone
    , mdHoverShadowOutline
    , mdHoverShadowSm
    , mdHoverShadowXl
    , mdHoverShadowXs
    , mdHoverSkewX0
    , mdHoverSkewX12
    , mdHoverSkewX3
    , mdHoverSkewX6
    , mdHoverSkewY0
    , mdHoverSkewY12
    , mdHoverSkewY3
    , mdHoverSkewY6
    , mdHoverTextBlack
    , mdHoverTextBlue100
    , mdHoverTextBlue200
    , mdHoverTextBlue300
    , mdHoverTextBlue400
    , mdHoverTextBlue500
    , mdHoverTextBlue600
    , mdHoverTextBlue700
    , mdHoverTextBlue800
    , mdHoverTextBlue900
    , mdHoverTextCurrent
    , mdHoverTextGray100
    , mdHoverTextGray200
    , mdHoverTextGray300
    , mdHoverTextGray400
    , mdHoverTextGray500
    , mdHoverTextGray600
    , mdHoverTextGray700
    , mdHoverTextGray800
    , mdHoverTextGray900
    , mdHoverTextGreen100
    , mdHoverTextGreen200
    , mdHoverTextGreen300
    , mdHoverTextGreen400
    , mdHoverTextGreen500
    , mdHoverTextGreen600
    , mdHoverTextGreen700
    , mdHoverTextGreen800
    , mdHoverTextGreen900
    , mdHoverTextIndigo100
    , mdHoverTextIndigo200
    , mdHoverTextIndigo300
    , mdHoverTextIndigo400
    , mdHoverTextIndigo500
    , mdHoverTextIndigo600
    , mdHoverTextIndigo700
    , mdHoverTextIndigo800
    , mdHoverTextIndigo900
    , mdHoverTextOpacity0
    , mdHoverTextOpacity100
    , mdHoverTextOpacity25
    , mdHoverTextOpacity50
    , mdHoverTextOpacity75
    , mdHoverTextOrange100
    , mdHoverTextOrange200
    , mdHoverTextOrange300
    , mdHoverTextOrange400
    , mdHoverTextOrange500
    , mdHoverTextOrange600
    , mdHoverTextOrange700
    , mdHoverTextOrange800
    , mdHoverTextOrange900
    , mdHoverTextPink100
    , mdHoverTextPink200
    , mdHoverTextPink300
    , mdHoverTextPink400
    , mdHoverTextPink500
    , mdHoverTextPink600
    , mdHoverTextPink700
    , mdHoverTextPink800
    , mdHoverTextPink900
    , mdHoverTextPurple100
    , mdHoverTextPurple200
    , mdHoverTextPurple300
    , mdHoverTextPurple400
    , mdHoverTextPurple500
    , mdHoverTextPurple600
    , mdHoverTextPurple700
    , mdHoverTextPurple800
    , mdHoverTextPurple900
    , mdHoverTextRed100
    , mdHoverTextRed200
    , mdHoverTextRed300
    , mdHoverTextRed400
    , mdHoverTextRed500
    , mdHoverTextRed600
    , mdHoverTextRed700
    , mdHoverTextRed800
    , mdHoverTextRed900
    , mdHoverTextTeal100
    , mdHoverTextTeal200
    , mdHoverTextTeal300
    , mdHoverTextTeal400
    , mdHoverTextTeal500
    , mdHoverTextTeal600
    , mdHoverTextTeal700
    , mdHoverTextTeal800
    , mdHoverTextTeal900
    , mdHoverTextTransparent
    , mdHoverTextWhite
    , mdHoverTextYellow100
    , mdHoverTextYellow200
    , mdHoverTextYellow300
    , mdHoverTextYellow400
    , mdHoverTextYellow500
    , mdHoverTextYellow600
    , mdHoverTextYellow700
    , mdHoverTextYellow800
    , mdHoverTextYellow900
    , mdHoverToBlack
    , mdHoverToBlue100
    , mdHoverToBlue200
    , mdHoverToBlue300
    , mdHoverToBlue400
    , mdHoverToBlue500
    , mdHoverToBlue600
    , mdHoverToBlue700
    , mdHoverToBlue800
    , mdHoverToBlue900
    , mdHoverToCurrent
    , mdHoverToGray100
    , mdHoverToGray200
    , mdHoverToGray300
    , mdHoverToGray400
    , mdHoverToGray500
    , mdHoverToGray600
    , mdHoverToGray700
    , mdHoverToGray800
    , mdHoverToGray900
    , mdHoverToGreen100
    , mdHoverToGreen200
    , mdHoverToGreen300
    , mdHoverToGreen400
    , mdHoverToGreen500
    , mdHoverToGreen600
    , mdHoverToGreen700
    , mdHoverToGreen800
    , mdHoverToGreen900
    , mdHoverToIndigo100
    , mdHoverToIndigo200
    , mdHoverToIndigo300
    , mdHoverToIndigo400
    , mdHoverToIndigo500
    , mdHoverToIndigo600
    , mdHoverToIndigo700
    , mdHoverToIndigo800
    , mdHoverToIndigo900
    , mdHoverToOrange100
    , mdHoverToOrange200
    , mdHoverToOrange300
    , mdHoverToOrange400
    , mdHoverToOrange500
    , mdHoverToOrange600
    , mdHoverToOrange700
    , mdHoverToOrange800
    , mdHoverToOrange900
    , mdHoverToPink100
    , mdHoverToPink200
    , mdHoverToPink300
    , mdHoverToPink400
    , mdHoverToPink500
    , mdHoverToPink600
    , mdHoverToPink700
    , mdHoverToPink800
    , mdHoverToPink900
    , mdHoverToPurple100
    , mdHoverToPurple200
    , mdHoverToPurple300
    , mdHoverToPurple400
    , mdHoverToPurple500
    , mdHoverToPurple600
    , mdHoverToPurple700
    , mdHoverToPurple800
    , mdHoverToPurple900
    , mdHoverToRed100
    , mdHoverToRed200
    , mdHoverToRed300
    , mdHoverToRed400
    , mdHoverToRed500
    , mdHoverToRed600
    , mdHoverToRed700
    , mdHoverToRed800
    , mdHoverToRed900
    , mdHoverToTeal100
    , mdHoverToTeal200
    , mdHoverToTeal300
    , mdHoverToTeal400
    , mdHoverToTeal500
    , mdHoverToTeal600
    , mdHoverToTeal700
    , mdHoverToTeal800
    , mdHoverToTeal900
    , mdHoverToTransparent
    , mdHoverToWhite
    , mdHoverToYellow100
    , mdHoverToYellow200
    , mdHoverToYellow300
    , mdHoverToYellow400
    , mdHoverToYellow500
    , mdHoverToYellow600
    , mdHoverToYellow700
    , mdHoverToYellow800
    , mdHoverToYellow900
    , mdHoverTranslateX0
    , mdHoverTranslateX1
    , mdHoverTranslateX10
    , mdHoverTranslateX12
    , mdHoverTranslateX16
    , mdHoverTranslateX1over2
    , mdHoverTranslateX2
    , mdHoverTranslateX20
    , mdHoverTranslateX24
    , mdHoverTranslateX3
    , mdHoverTranslateX32
    , mdHoverTranslateX4
    , mdHoverTranslateX40
    , mdHoverTranslateX48
    , mdHoverTranslateX5
    , mdHoverTranslateX56
    , mdHoverTranslateX6
    , mdHoverTranslateX64
    , mdHoverTranslateX8
    , mdHoverTranslateXFull
    , mdHoverTranslateXPx
    , mdHoverTranslateY0
    , mdHoverTranslateY1
    , mdHoverTranslateY10
    , mdHoverTranslateY12
    , mdHoverTranslateY16
    , mdHoverTranslateY1over2
    , mdHoverTranslateY2
    , mdHoverTranslateY20
    , mdHoverTranslateY24
    , mdHoverTranslateY3
    , mdHoverTranslateY32
    , mdHoverTranslateY4
    , mdHoverTranslateY40
    , mdHoverTranslateY48
    , mdHoverTranslateY5
    , mdHoverTranslateY56
    , mdHoverTranslateY6
    , mdHoverTranslateY64
    , mdHoverTranslateY8
    , mdHoverTranslateYFull
    , mdHoverTranslateYPx
    , mdHoverUnderline
    , mdHoverViaBlack
    , mdHoverViaBlue100
    , mdHoverViaBlue200
    , mdHoverViaBlue300
    , mdHoverViaBlue400
    , mdHoverViaBlue500
    , mdHoverViaBlue600
    , mdHoverViaBlue700
    , mdHoverViaBlue800
    , mdHoverViaBlue900
    , mdHoverViaCurrent
    , mdHoverViaGray100
    , mdHoverViaGray200
    , mdHoverViaGray300
    , mdHoverViaGray400
    , mdHoverViaGray500
    , mdHoverViaGray600
    , mdHoverViaGray700
    , mdHoverViaGray800
    , mdHoverViaGray900
    , mdHoverViaGreen100
    , mdHoverViaGreen200
    , mdHoverViaGreen300
    , mdHoverViaGreen400
    , mdHoverViaGreen500
    , mdHoverViaGreen600
    , mdHoverViaGreen700
    , mdHoverViaGreen800
    , mdHoverViaGreen900
    , mdHoverViaIndigo100
    , mdHoverViaIndigo200
    , mdHoverViaIndigo300
    , mdHoverViaIndigo400
    , mdHoverViaIndigo500
    , mdHoverViaIndigo600
    , mdHoverViaIndigo700
    , mdHoverViaIndigo800
    , mdHoverViaIndigo900
    , mdHoverViaOrange100
    , mdHoverViaOrange200
    , mdHoverViaOrange300
    , mdHoverViaOrange400
    , mdHoverViaOrange500
    , mdHoverViaOrange600
    , mdHoverViaOrange700
    , mdHoverViaOrange800
    , mdHoverViaOrange900
    , mdHoverViaPink100
    , mdHoverViaPink200
    , mdHoverViaPink300
    , mdHoverViaPink400
    , mdHoverViaPink500
    , mdHoverViaPink600
    , mdHoverViaPink700
    , mdHoverViaPink800
    , mdHoverViaPink900
    , mdHoverViaPurple100
    , mdHoverViaPurple200
    , mdHoverViaPurple300
    , mdHoverViaPurple400
    , mdHoverViaPurple500
    , mdHoverViaPurple600
    , mdHoverViaPurple700
    , mdHoverViaPurple800
    , mdHoverViaPurple900
    , mdHoverViaRed100
    , mdHoverViaRed200
    , mdHoverViaRed300
    , mdHoverViaRed400
    , mdHoverViaRed500
    , mdHoverViaRed600
    , mdHoverViaRed700
    , mdHoverViaRed800
    , mdHoverViaRed900
    , mdHoverViaTeal100
    , mdHoverViaTeal200
    , mdHoverViaTeal300
    , mdHoverViaTeal400
    , mdHoverViaTeal500
    , mdHoverViaTeal600
    , mdHoverViaTeal700
    , mdHoverViaTeal800
    , mdHoverViaTeal900
    , mdHoverViaTransparent
    , mdHoverViaWhite
    , mdHoverViaYellow100
    , mdHoverViaYellow200
    , mdHoverViaYellow300
    , mdHoverViaYellow400
    , mdHoverViaYellow500
    , mdHoverViaYellow600
    , mdHoverViaYellow700
    , mdHoverViaYellow800
    , mdHoverViaYellow900
    , mdInline
    , mdInlineBlock
    , mdInlineFlex
    , mdInlineGrid
    , mdInset0
    , mdInsetAuto
    , mdInsetX0
    , mdInsetXAuto
    , mdInsetY0
    , mdInsetYAuto
    , mdInvisible
    , mdItalic
    , mdItemsBaseline
    , mdItemsCenter
    , mdItemsEnd
    , mdItemsStart
    , mdItemsStretch
    , mdJustifyAround
    , mdJustifyBetween
    , mdJustifyCenter
    , mdJustifyEnd
    , mdJustifyEvenly
    , mdJustifyItemsAuto
    , mdJustifyItemsCenter
    , mdJustifyItemsEnd
    , mdJustifyItemsStart
    , mdJustifyItemsStretch
    , mdJustifySelfAuto
    , mdJustifySelfCenter
    , mdJustifySelfEnd
    , mdJustifySelfStart
    , mdJustifySelfStretch
    , mdJustifyStart
    , mdLeading10
    , mdLeading3
    , mdLeading4
    , mdLeading5
    , mdLeading6
    , mdLeading7
    , mdLeading8
    , mdLeading9
    , mdLeadingLoose
    , mdLeadingNone
    , mdLeadingNormal
    , mdLeadingRelaxed
    , mdLeadingSnug
    , mdLeadingTight
    , mdLeft0
    , mdLeftAuto
    , mdLineThrough
    , mdLiningNums
    , mdListDecimal
    , mdListDisc
    , mdListInside
    , mdListNone
    , mdListOutside
    , mdLowercase
    , mdM0
    , mdM1
    , mdM10
    , mdM12
    , mdM16
    , mdM2
    , mdM20
    , mdM24
    , mdM3
    , mdM32
    , mdM4
    , mdM40
    , mdM48
    , mdM5
    , mdM56
    , mdM6
    , mdM64
    , mdM8
    , mdMAuto
    , mdMPx
    , mdMaxHFull
    , mdMaxHScreen
    , mdMaxW2xl
    , mdMaxW3xl
    , mdMaxW4xl
    , mdMaxW5xl
    , mdMaxW6xl
    , mdMaxWFull
    , mdMaxWLg
    , mdMaxWMd
    , mdMaxWNone
    , mdMaxWScreenLg
    , mdMaxWScreenMd
    , mdMaxWScreenSm
    , mdMaxWScreenXl
    , mdMaxWSm
    , mdMaxWXl
    , mdMaxWXs
    , mdMb0
    , mdMb1
    , mdMb10
    , mdMb12
    , mdMb16
    , mdMb2
    , mdMb20
    , mdMb24
    , mdMb3
    , mdMb32
    , mdMb4
    , mdMb40
    , mdMb48
    , mdMb5
    , mdMb56
    , mdMb6
    , mdMb64
    , mdMb8
    , mdMbAuto
    , mdMbPx
    , mdMinH0
    , mdMinHFull
    , mdMinHScreen
    , mdMinW0
    , mdMinWFull
    , mdMl0
    , mdMl1
    , mdMl10
    , mdMl12
    , mdMl16
    , mdMl2
    , mdMl20
    , mdMl24
    , mdMl3
    , mdMl32
    , mdMl4
    , mdMl40
    , mdMl48
    , mdMl5
    , mdMl56
    , mdMl6
    , mdMl64
    , mdMl8
    , mdMlAuto
    , mdMlPx
    , mdMr0
    , mdMr1
    , mdMr10
    , mdMr12
    , mdMr16
    , mdMr2
    , mdMr20
    , mdMr24
    , mdMr3
    , mdMr32
    , mdMr4
    , mdMr40
    , mdMr48
    , mdMr5
    , mdMr56
    , mdMr6
    , mdMr64
    , mdMr8
    , mdMrAuto
    , mdMrPx
    , mdMt0
    , mdMt1
    , mdMt10
    , mdMt12
    , mdMt16
    , mdMt2
    , mdMt20
    , mdMt24
    , mdMt3
    , mdMt32
    , mdMt4
    , mdMt40
    , mdMt48
    , mdMt5
    , mdMt56
    , mdMt6
    , mdMt64
    , mdMt8
    , mdMtAuto
    , mdMtPx
    , mdMx0
    , mdMx1
    , mdMx10
    , mdMx12
    , mdMx16
    , mdMx2
    , mdMx20
    , mdMx24
    , mdMx3
    , mdMx32
    , mdMx4
    , mdMx40
    , mdMx48
    , mdMx5
    , mdMx56
    , mdMx6
    , mdMx64
    , mdMx8
    , mdMxAuto
    , mdMxPx
    , mdMy0
    , mdMy1
    , mdMy10
    , mdMy12
    , mdMy16
    , mdMy2
    , mdMy20
    , mdMy24
    , mdMy3
    , mdMy32
    , mdMy4
    , mdMy40
    , mdMy48
    , mdMy5
    , mdMy56
    , mdMy6
    , mdMy64
    , mdMy8
    , mdMyAuto
    , mdMyPx
    , mdNegM1
    , mdNegM10
    , mdNegM12
    , mdNegM16
    , mdNegM2
    , mdNegM20
    , mdNegM24
    , mdNegM3
    , mdNegM32
    , mdNegM4
    , mdNegM40
    , mdNegM48
    , mdNegM5
    , mdNegM56
    , mdNegM6
    , mdNegM64
    , mdNegM8
    , mdNegMPx
    , mdNegMb1
    , mdNegMb10
    , mdNegMb12
    , mdNegMb16
    , mdNegMb2
    , mdNegMb20
    , mdNegMb24
    , mdNegMb3
    , mdNegMb32
    , mdNegMb4
    , mdNegMb40
    , mdNegMb48
    , mdNegMb5
    , mdNegMb56
    , mdNegMb6
    , mdNegMb64
    , mdNegMb8
    , mdNegMbPx
    , mdNegMl1
    , mdNegMl10
    , mdNegMl12
    , mdNegMl16
    , mdNegMl2
    , mdNegMl20
    , mdNegMl24
    , mdNegMl3
    , mdNegMl32
    , mdNegMl4
    , mdNegMl40
    , mdNegMl48
    , mdNegMl5
    , mdNegMl56
    , mdNegMl6
    , mdNegMl64
    , mdNegMl8
    , mdNegMlPx
    , mdNegMr1
    , mdNegMr10
    , mdNegMr12
    , mdNegMr16
    , mdNegMr2
    , mdNegMr20
    , mdNegMr24
    , mdNegMr3
    , mdNegMr32
    , mdNegMr4
    , mdNegMr40
    , mdNegMr48
    , mdNegMr5
    , mdNegMr56
    , mdNegMr6
    , mdNegMr64
    , mdNegMr8
    , mdNegMrPx
    , mdNegMt1
    , mdNegMt10
    , mdNegMt12
    , mdNegMt16
    , mdNegMt2
    , mdNegMt20
    , mdNegMt24
    , mdNegMt3
    , mdNegMt32
    , mdNegMt4
    , mdNegMt40
    , mdNegMt48
    , mdNegMt5
    , mdNegMt56
    , mdNegMt6
    , mdNegMt64
    , mdNegMt8
    , mdNegMtPx
    , mdNegMx1
    , mdNegMx10
    , mdNegMx12
    , mdNegMx16
    , mdNegMx2
    , mdNegMx20
    , mdNegMx24
    , mdNegMx3
    , mdNegMx32
    , mdNegMx4
    , mdNegMx40
    , mdNegMx48
    , mdNegMx5
    , mdNegMx56
    , mdNegMx6
    , mdNegMx64
    , mdNegMx8
    , mdNegMxPx
    , mdNegMy1
    , mdNegMy10
    , mdNegMy12
    , mdNegMy16
    , mdNegMy2
    , mdNegMy20
    , mdNegMy24
    , mdNegMy3
    , mdNegMy32
    , mdNegMy4
    , mdNegMy40
    , mdNegMy48
    , mdNegMy5
    , mdNegMy56
    , mdNegMy6
    , mdNegMy64
    , mdNegMy8
    , mdNegMyPx
    , mdNegRotate180
    , mdNegRotate45
    , mdNegRotate90
    , mdNegSkewX12
    , mdNegSkewX3
    , mdNegSkewX6
    , mdNegSkewY12
    , mdNegSkewY3
    , mdNegSkewY6
    , mdNegSpaceX1
    , mdNegSpaceX10
    , mdNegSpaceX12
    , mdNegSpaceX16
    , mdNegSpaceX2
    , mdNegSpaceX20
    , mdNegSpaceX24
    , mdNegSpaceX3
    , mdNegSpaceX32
    , mdNegSpaceX4
    , mdNegSpaceX40
    , mdNegSpaceX48
    , mdNegSpaceX5
    , mdNegSpaceX56
    , mdNegSpaceX6
    , mdNegSpaceX64
    , mdNegSpaceX8
    , mdNegSpaceXPx
    , mdNegSpaceY1
    , mdNegSpaceY10
    , mdNegSpaceY12
    , mdNegSpaceY16
    , mdNegSpaceY2
    , mdNegSpaceY20
    , mdNegSpaceY24
    , mdNegSpaceY3
    , mdNegSpaceY32
    , mdNegSpaceY4
    , mdNegSpaceY40
    , mdNegSpaceY48
    , mdNegSpaceY5
    , mdNegSpaceY56
    , mdNegSpaceY6
    , mdNegSpaceY64
    , mdNegSpaceY8
    , mdNegSpaceYPx
    , mdNegTranslateX1
    , mdNegTranslateX10
    , mdNegTranslateX12
    , mdNegTranslateX16
    , mdNegTranslateX1over2
    , mdNegTranslateX2
    , mdNegTranslateX20
    , mdNegTranslateX24
    , mdNegTranslateX3
    , mdNegTranslateX32
    , mdNegTranslateX4
    , mdNegTranslateX40
    , mdNegTranslateX48
    , mdNegTranslateX5
    , mdNegTranslateX56
    , mdNegTranslateX6
    , mdNegTranslateX64
    , mdNegTranslateX8
    , mdNegTranslateXFull
    , mdNegTranslateXPx
    , mdNegTranslateY1
    , mdNegTranslateY10
    , mdNegTranslateY12
    , mdNegTranslateY16
    , mdNegTranslateY1over2
    , mdNegTranslateY2
    , mdNegTranslateY20
    , mdNegTranslateY24
    , mdNegTranslateY3
    , mdNegTranslateY32
    , mdNegTranslateY4
    , mdNegTranslateY40
    , mdNegTranslateY48
    , mdNegTranslateY5
    , mdNegTranslateY56
    , mdNegTranslateY6
    , mdNegTranslateY64
    , mdNegTranslateY8
    , mdNegTranslateYFull
    , mdNegTranslateYPx
    , mdNoUnderline
    , mdNormalCase
    , mdNormalNums
    , mdNotItalic
    , mdNotSrOnly
    , mdObjectBottom
    , mdObjectCenter
    , mdObjectContain
    , mdObjectCover
    , mdObjectFill
    , mdObjectLeft
    , mdObjectLeftBottom
    , mdObjectLeftTop
    , mdObjectNone
    , mdObjectRight
    , mdObjectRightBottom
    , mdObjectRightTop
    , mdObjectScaleDown
    , mdObjectTop
    , mdOldstyleNums
    , mdOpacity0
    , mdOpacity100
    , mdOpacity25
    , mdOpacity50
    , mdOpacity75
    , mdOrder1
    , mdOrder10
    , mdOrder11
    , mdOrder12
    , mdOrder2
    , mdOrder3
    , mdOrder4
    , mdOrder5
    , mdOrder6
    , mdOrder7
    , mdOrder8
    , mdOrder9
    , mdOrderFirst
    , mdOrderLast
    , mdOrderNone
    , mdOrdinal
    , mdOriginBottom
    , mdOriginBottomLeft
    , mdOriginBottomRight
    , mdOriginCenter
    , mdOriginLeft
    , mdOriginRight
    , mdOriginTop
    , mdOriginTopLeft
    , mdOriginTopRight
    , mdOutlineNone
    , mdOverflowAuto
    , mdOverflowHidden
    , mdOverflowScroll
    , mdOverflowVisible
    , mdOverflowXAuto
    , mdOverflowXHidden
    , mdOverflowXScroll
    , mdOverflowXVisible
    , mdOverflowYAuto
    , mdOverflowYHidden
    , mdOverflowYScroll
    , mdOverflowYVisible
    , mdOverscrollAuto
    , mdOverscrollContain
    , mdOverscrollNone
    , mdOverscrollXAuto
    , mdOverscrollXContain
    , mdOverscrollXNone
    , mdOverscrollYAuto
    , mdOverscrollYContain
    , mdOverscrollYNone
    , mdP0
    , mdP1
    , mdP10
    , mdP12
    , mdP16
    , mdP2
    , mdP20
    , mdP24
    , mdP3
    , mdP32
    , mdP4
    , mdP40
    , mdP48
    , mdP5
    , mdP56
    , mdP6
    , mdP64
    , mdP8
    , mdPPx
    , mdPb0
    , mdPb1
    , mdPb10
    , mdPb12
    , mdPb16
    , mdPb2
    , mdPb20
    , mdPb24
    , mdPb3
    , mdPb32
    , mdPb4
    , mdPb40
    , mdPb48
    , mdPb5
    , mdPb56
    , mdPb6
    , mdPb64
    , mdPb8
    , mdPbPx
    , mdPl0
    , mdPl1
    , mdPl10
    , mdPl12
    , mdPl16
    , mdPl2
    , mdPl20
    , mdPl24
    , mdPl3
    , mdPl32
    , mdPl4
    , mdPl40
    , mdPl48
    , mdPl5
    , mdPl56
    , mdPl6
    , mdPl64
    , mdPl8
    , mdPlPx
    , mdPlaceContentAround
    , mdPlaceContentBetween
    , mdPlaceContentCenter
    , mdPlaceContentEnd
    , mdPlaceContentEvenly
    , mdPlaceContentStart
    , mdPlaceContentStretch
    , mdPlaceItemsAuto
    , mdPlaceItemsCenter
    , mdPlaceItemsEnd
    , mdPlaceItemsStart
    , mdPlaceItemsStretch
    , mdPlaceSelfAuto
    , mdPlaceSelfCenter
    , mdPlaceSelfEnd
    , mdPlaceSelfStart
    , mdPlaceSelfStretch
    , mdPlaceholderBlack
    , mdPlaceholderBlue100
    , mdPlaceholderBlue200
    , mdPlaceholderBlue300
    , mdPlaceholderBlue400
    , mdPlaceholderBlue500
    , mdPlaceholderBlue600
    , mdPlaceholderBlue700
    , mdPlaceholderBlue800
    , mdPlaceholderBlue900
    , mdPlaceholderCurrent
    , mdPlaceholderGray100
    , mdPlaceholderGray200
    , mdPlaceholderGray300
    , mdPlaceholderGray400
    , mdPlaceholderGray500
    , mdPlaceholderGray600
    , mdPlaceholderGray700
    , mdPlaceholderGray800
    , mdPlaceholderGray900
    , mdPlaceholderGreen100
    , mdPlaceholderGreen200
    , mdPlaceholderGreen300
    , mdPlaceholderGreen400
    , mdPlaceholderGreen500
    , mdPlaceholderGreen600
    , mdPlaceholderGreen700
    , mdPlaceholderGreen800
    , mdPlaceholderGreen900
    , mdPlaceholderIndigo100
    , mdPlaceholderIndigo200
    , mdPlaceholderIndigo300
    , mdPlaceholderIndigo400
    , mdPlaceholderIndigo500
    , mdPlaceholderIndigo600
    , mdPlaceholderIndigo700
    , mdPlaceholderIndigo800
    , mdPlaceholderIndigo900
    , mdPlaceholderOpacity0
    , mdPlaceholderOpacity100
    , mdPlaceholderOpacity25
    , mdPlaceholderOpacity50
    , mdPlaceholderOpacity75
    , mdPlaceholderOrange100
    , mdPlaceholderOrange200
    , mdPlaceholderOrange300
    , mdPlaceholderOrange400
    , mdPlaceholderOrange500
    , mdPlaceholderOrange600
    , mdPlaceholderOrange700
    , mdPlaceholderOrange800
    , mdPlaceholderOrange900
    , mdPlaceholderPink100
    , mdPlaceholderPink200
    , mdPlaceholderPink300
    , mdPlaceholderPink400
    , mdPlaceholderPink500
    , mdPlaceholderPink600
    , mdPlaceholderPink700
    , mdPlaceholderPink800
    , mdPlaceholderPink900
    , mdPlaceholderPurple100
    , mdPlaceholderPurple200
    , mdPlaceholderPurple300
    , mdPlaceholderPurple400
    , mdPlaceholderPurple500
    , mdPlaceholderPurple600
    , mdPlaceholderPurple700
    , mdPlaceholderPurple800
    , mdPlaceholderPurple900
    , mdPlaceholderRed100
    , mdPlaceholderRed200
    , mdPlaceholderRed300
    , mdPlaceholderRed400
    , mdPlaceholderRed500
    , mdPlaceholderRed600
    , mdPlaceholderRed700
    , mdPlaceholderRed800
    , mdPlaceholderRed900
    , mdPlaceholderTeal100
    , mdPlaceholderTeal200
    , mdPlaceholderTeal300
    , mdPlaceholderTeal400
    , mdPlaceholderTeal500
    , mdPlaceholderTeal600
    , mdPlaceholderTeal700
    , mdPlaceholderTeal800
    , mdPlaceholderTeal900
    , mdPlaceholderTransparent
    , mdPlaceholderWhite
    , mdPlaceholderYellow100
    , mdPlaceholderYellow200
    , mdPlaceholderYellow300
    , mdPlaceholderYellow400
    , mdPlaceholderYellow500
    , mdPlaceholderYellow600
    , mdPlaceholderYellow700
    , mdPlaceholderYellow800
    , mdPlaceholderYellow900
    , mdPointerEventsAuto
    , mdPointerEventsNone
    , mdPr0
    , mdPr1
    , mdPr10
    , mdPr12
    , mdPr16
    , mdPr2
    , mdPr20
    , mdPr24
    , mdPr3
    , mdPr32
    , mdPr4
    , mdPr40
    , mdPr48
    , mdPr5
    , mdPr56
    , mdPr6
    , mdPr64
    , mdPr8
    , mdPrPx
    , mdProportionalNums
    , mdPt0
    , mdPt1
    , mdPt10
    , mdPt12
    , mdPt16
    , mdPt2
    , mdPt20
    , mdPt24
    , mdPt3
    , mdPt32
    , mdPt4
    , mdPt40
    , mdPt48
    , mdPt5
    , mdPt56
    , mdPt6
    , mdPt64
    , mdPt8
    , mdPtPx
    , mdPx0
    , mdPx1
    , mdPx10
    , mdPx12
    , mdPx16
    , mdPx2
    , mdPx20
    , mdPx24
    , mdPx3
    , mdPx32
    , mdPx4
    , mdPx40
    , mdPx48
    , mdPx5
    , mdPx56
    , mdPx6
    , mdPx64
    , mdPx8
    , mdPxPx
    , mdPy0
    , mdPy1
    , mdPy10
    , mdPy12
    , mdPy16
    , mdPy2
    , mdPy20
    , mdPy24
    , mdPy3
    , mdPy32
    , mdPy4
    , mdPy40
    , mdPy48
    , mdPy5
    , mdPy56
    , mdPy6
    , mdPy64
    , mdPy8
    , mdPyPx
    , mdRelative
    , mdResize
    , mdResizeNone
    , mdResizeX
    , mdResizeY
    , mdRight0
    , mdRightAuto
    , mdRotate0
    , mdRotate180
    , mdRotate45
    , mdRotate90
    , mdRounded
    , mdRoundedB
    , mdRoundedBFull
    , mdRoundedBLg
    , mdRoundedBMd
    , mdRoundedBNone
    , mdRoundedBSm
    , mdRoundedBl
    , mdRoundedBlFull
    , mdRoundedBlLg
    , mdRoundedBlMd
    , mdRoundedBlNone
    , mdRoundedBlSm
    , mdRoundedBr
    , mdRoundedBrFull
    , mdRoundedBrLg
    , mdRoundedBrMd
    , mdRoundedBrNone
    , mdRoundedBrSm
    , mdRoundedFull
    , mdRoundedL
    , mdRoundedLFull
    , mdRoundedLLg
    , mdRoundedLMd
    , mdRoundedLNone
    , mdRoundedLSm
    , mdRoundedLg
    , mdRoundedMd
    , mdRoundedNone
    , mdRoundedR
    , mdRoundedRFull
    , mdRoundedRLg
    , mdRoundedRMd
    , mdRoundedRNone
    , mdRoundedRSm
    , mdRoundedSm
    , mdRoundedT
    , mdRoundedTFull
    , mdRoundedTLg
    , mdRoundedTMd
    , mdRoundedTNone
    , mdRoundedTSm
    , mdRoundedTl
    , mdRoundedTlFull
    , mdRoundedTlLg
    , mdRoundedTlMd
    , mdRoundedTlNone
    , mdRoundedTlSm
    , mdRoundedTr
    , mdRoundedTrFull
    , mdRoundedTrLg
    , mdRoundedTrMd
    , mdRoundedTrNone
    , mdRoundedTrSm
    , mdRowAuto
    , mdRowEnd1
    , mdRowEnd2
    , mdRowEnd3
    , mdRowEnd4
    , mdRowEnd5
    , mdRowEnd6
    , mdRowEnd7
    , mdRowEndAuto
    , mdRowSpan1
    , mdRowSpan2
    , mdRowSpan3
    , mdRowSpan4
    , mdRowSpan5
    , mdRowSpan6
    , mdRowStart1
    , mdRowStart2
    , mdRowStart3
    , mdRowStart4
    , mdRowStart5
    , mdRowStart6
    , mdRowStart7
    , mdRowStartAuto
    , mdScale0
    , mdScale100
    , mdScale105
    , mdScale110
    , mdScale125
    , mdScale150
    , mdScale50
    , mdScale75
    , mdScale90
    , mdScale95
    , mdScaleX0
    , mdScaleX100
    , mdScaleX105
    , mdScaleX110
    , mdScaleX125
    , mdScaleX150
    , mdScaleX50
    , mdScaleX75
    , mdScaleX90
    , mdScaleX95
    , mdScaleY0
    , mdScaleY100
    , mdScaleY105
    , mdScaleY110
    , mdScaleY125
    , mdScaleY150
    , mdScaleY50
    , mdScaleY75
    , mdScaleY90
    , mdScaleY95
    , mdScrollingAuto
    , mdScrollingTouch
    , mdSelectAll
    , mdSelectAuto
    , mdSelectNone
    , mdSelectText
    , mdSelfAuto
    , mdSelfCenter
    , mdSelfEnd
    , mdSelfStart
    , mdSelfStretch
    , mdShadow
    , mdShadow2xl
    , mdShadowInner
    , mdShadowLg
    , mdShadowMd
    , mdShadowNone
    , mdShadowOutline
    , mdShadowSm
    , mdShadowXl
    , mdShadowXs
    , mdSkewX0
    , mdSkewX12
    , mdSkewX3
    , mdSkewX6
    , mdSkewY0
    , mdSkewY12
    , mdSkewY3
    , mdSkewY6
    , mdSlashedZero
    , mdSpaceX0
    , mdSpaceX1
    , mdSpaceX10
    , mdSpaceX12
    , mdSpaceX16
    , mdSpaceX2
    , mdSpaceX20
    , mdSpaceX24
    , mdSpaceX3
    , mdSpaceX32
    , mdSpaceX4
    , mdSpaceX40
    , mdSpaceX48
    , mdSpaceX5
    , mdSpaceX56
    , mdSpaceX6
    , mdSpaceX64
    , mdSpaceX8
    , mdSpaceXPx
    , mdSpaceXReverse
    , mdSpaceY0
    , mdSpaceY1
    , mdSpaceY10
    , mdSpaceY12
    , mdSpaceY16
    , mdSpaceY2
    , mdSpaceY20
    , mdSpaceY24
    , mdSpaceY3
    , mdSpaceY32
    , mdSpaceY4
    , mdSpaceY40
    , mdSpaceY48
    , mdSpaceY5
    , mdSpaceY56
    , mdSpaceY6
    , mdSpaceY64
    , mdSpaceY8
    , mdSpaceYPx
    , mdSpaceYReverse
    , mdSrOnly
    , mdStackedFractions
    , mdStatic
    , mdSticky
    , mdStroke0
    , mdStroke1
    , mdStroke2
    , mdStrokeCurrent
    , mdSubpixelAntialiased
    , mdTable
    , mdTableAuto
    , mdTableCaption
    , mdTableCell
    , mdTableColumn
    , mdTableColumnGroup
    , mdTableFixed
    , mdTableFooterGroup
    , mdTableHeaderGroup
    , mdTableRow
    , mdTableRowGroup
    , mdTabularNums
    , mdText2xl
    , mdText3xl
    , mdText4xl
    , mdText5xl
    , mdText6xl
    , mdTextBase
    , mdTextBlack
    , mdTextBlue100
    , mdTextBlue200
    , mdTextBlue300
    , mdTextBlue400
    , mdTextBlue500
    , mdTextBlue600
    , mdTextBlue700
    , mdTextBlue800
    , mdTextBlue900
    , mdTextCenter
    , mdTextCurrent
    , mdTextGray100
    , mdTextGray200
    , mdTextGray300
    , mdTextGray400
    , mdTextGray500
    , mdTextGray600
    , mdTextGray700
    , mdTextGray800
    , mdTextGray900
    , mdTextGreen100
    , mdTextGreen200
    , mdTextGreen300
    , mdTextGreen400
    , mdTextGreen500
    , mdTextGreen600
    , mdTextGreen700
    , mdTextGreen800
    , mdTextGreen900
    , mdTextIndigo100
    , mdTextIndigo200
    , mdTextIndigo300
    , mdTextIndigo400
    , mdTextIndigo500
    , mdTextIndigo600
    , mdTextIndigo700
    , mdTextIndigo800
    , mdTextIndigo900
    , mdTextJustify
    , mdTextLeft
    , mdTextLg
    , mdTextOpacity0
    , mdTextOpacity100
    , mdTextOpacity25
    , mdTextOpacity50
    , mdTextOpacity75
    , mdTextOrange100
    , mdTextOrange200
    , mdTextOrange300
    , mdTextOrange400
    , mdTextOrange500
    , mdTextOrange600
    , mdTextOrange700
    , mdTextOrange800
    , mdTextOrange900
    , mdTextPink100
    , mdTextPink200
    , mdTextPink300
    , mdTextPink400
    , mdTextPink500
    , mdTextPink600
    , mdTextPink700
    , mdTextPink800
    , mdTextPink900
    , mdTextPurple100
    , mdTextPurple200
    , mdTextPurple300
    , mdTextPurple400
    , mdTextPurple500
    , mdTextPurple600
    , mdTextPurple700
    , mdTextPurple800
    , mdTextPurple900
    , mdTextRed100
    , mdTextRed200
    , mdTextRed300
    , mdTextRed400
    , mdTextRed500
    , mdTextRed600
    , mdTextRed700
    , mdTextRed800
    , mdTextRed900
    , mdTextRight
    , mdTextSm
    , mdTextTeal100
    , mdTextTeal200
    , mdTextTeal300
    , mdTextTeal400
    , mdTextTeal500
    , mdTextTeal600
    , mdTextTeal700
    , mdTextTeal800
    , mdTextTeal900
    , mdTextTransparent
    , mdTextWhite
    , mdTextXl
    , mdTextXs
    , mdTextYellow100
    , mdTextYellow200
    , mdTextYellow300
    , mdTextYellow400
    , mdTextYellow500
    , mdTextYellow600
    , mdTextYellow700
    , mdTextYellow800
    , mdTextYellow900
    , mdToBlack
    , mdToBlue100
    , mdToBlue200
    , mdToBlue300
    , mdToBlue400
    , mdToBlue500
    , mdToBlue600
    , mdToBlue700
    , mdToBlue800
    , mdToBlue900
    , mdToCurrent
    , mdToGray100
    , mdToGray200
    , mdToGray300
    , mdToGray400
    , mdToGray500
    , mdToGray600
    , mdToGray700
    , mdToGray800
    , mdToGray900
    , mdToGreen100
    , mdToGreen200
    , mdToGreen300
    , mdToGreen400
    , mdToGreen500
    , mdToGreen600
    , mdToGreen700
    , mdToGreen800
    , mdToGreen900
    , mdToIndigo100
    , mdToIndigo200
    , mdToIndigo300
    , mdToIndigo400
    , mdToIndigo500
    , mdToIndigo600
    , mdToIndigo700
    , mdToIndigo800
    , mdToIndigo900
    , mdToOrange100
    , mdToOrange200
    , mdToOrange300
    , mdToOrange400
    , mdToOrange500
    , mdToOrange600
    , mdToOrange700
    , mdToOrange800
    , mdToOrange900
    , mdToPink100
    , mdToPink200
    , mdToPink300
    , mdToPink400
    , mdToPink500
    , mdToPink600
    , mdToPink700
    , mdToPink800
    , mdToPink900
    , mdToPurple100
    , mdToPurple200
    , mdToPurple300
    , mdToPurple400
    , mdToPurple500
    , mdToPurple600
    , mdToPurple700
    , mdToPurple800
    , mdToPurple900
    , mdToRed100
    , mdToRed200
    , mdToRed300
    , mdToRed400
    , mdToRed500
    , mdToRed600
    , mdToRed700
    , mdToRed800
    , mdToRed900
    , mdToTeal100
    , mdToTeal200
    , mdToTeal300
    , mdToTeal400
    , mdToTeal500
    , mdToTeal600
    , mdToTeal700
    , mdToTeal800
    , mdToTeal900
    , mdToTransparent
    , mdToWhite
    , mdToYellow100
    , mdToYellow200
    , mdToYellow300
    , mdToYellow400
    , mdToYellow500
    , mdToYellow600
    , mdToYellow700
    , mdToYellow800
    , mdToYellow900
    , mdTop0
    , mdTopAuto
    , mdTrackingNormal
    , mdTrackingTight
    , mdTrackingTighter
    , mdTrackingWide
    , mdTrackingWider
    , mdTrackingWidest
    , mdTransform
    , mdTransformNone
    , mdTransition
    , mdTransitionAll
    , mdTransitionColors
    , mdTransitionNone
    , mdTransitionOpacity
    , mdTransitionShadow
    , mdTransitionTransform
    , mdTranslateX0
    , mdTranslateX1
    , mdTranslateX10
    , mdTranslateX12
    , mdTranslateX16
    , mdTranslateX1over2
    , mdTranslateX2
    , mdTranslateX20
    , mdTranslateX24
    , mdTranslateX3
    , mdTranslateX32
    , mdTranslateX4
    , mdTranslateX40
    , mdTranslateX48
    , mdTranslateX5
    , mdTranslateX56
    , mdTranslateX6
    , mdTranslateX64
    , mdTranslateX8
    , mdTranslateXFull
    , mdTranslateXPx
    , mdTranslateY0
    , mdTranslateY1
    , mdTranslateY10
    , mdTranslateY12
    , mdTranslateY16
    , mdTranslateY1over2
    , mdTranslateY2
    , mdTranslateY20
    , mdTranslateY24
    , mdTranslateY3
    , mdTranslateY32
    , mdTranslateY4
    , mdTranslateY40
    , mdTranslateY48
    , mdTranslateY5
    , mdTranslateY56
    , mdTranslateY6
    , mdTranslateY64
    , mdTranslateY8
    , mdTranslateYFull
    , mdTranslateYPx
    , mdTruncate
    , mdUnderline
    , mdUppercase
    , mdViaBlack
    , mdViaBlue100
    , mdViaBlue200
    , mdViaBlue300
    , mdViaBlue400
    , mdViaBlue500
    , mdViaBlue600
    , mdViaBlue700
    , mdViaBlue800
    , mdViaBlue900
    , mdViaCurrent
    , mdViaGray100
    , mdViaGray200
    , mdViaGray300
    , mdViaGray400
    , mdViaGray500
    , mdViaGray600
    , mdViaGray700
    , mdViaGray800
    , mdViaGray900
    , mdViaGreen100
    , mdViaGreen200
    , mdViaGreen300
    , mdViaGreen400
    , mdViaGreen500
    , mdViaGreen600
    , mdViaGreen700
    , mdViaGreen800
    , mdViaGreen900
    , mdViaIndigo100
    , mdViaIndigo200
    , mdViaIndigo300
    , mdViaIndigo400
    , mdViaIndigo500
    , mdViaIndigo600
    , mdViaIndigo700
    , mdViaIndigo800
    , mdViaIndigo900
    , mdViaOrange100
    , mdViaOrange200
    , mdViaOrange300
    , mdViaOrange400
    , mdViaOrange500
    , mdViaOrange600
    , mdViaOrange700
    , mdViaOrange800
    , mdViaOrange900
    , mdViaPink100
    , mdViaPink200
    , mdViaPink300
    , mdViaPink400
    , mdViaPink500
    , mdViaPink600
    , mdViaPink700
    , mdViaPink800
    , mdViaPink900
    , mdViaPurple100
    , mdViaPurple200
    , mdViaPurple300
    , mdViaPurple400
    , mdViaPurple500
    , mdViaPurple600
    , mdViaPurple700
    , mdViaPurple800
    , mdViaPurple900
    , mdViaRed100
    , mdViaRed200
    , mdViaRed300
    , mdViaRed400
    , mdViaRed500
    , mdViaRed600
    , mdViaRed700
    , mdViaRed800
    , mdViaRed900
    , mdViaTeal100
    , mdViaTeal200
    , mdViaTeal300
    , mdViaTeal400
    , mdViaTeal500
    , mdViaTeal600
    , mdViaTeal700
    , mdViaTeal800
    , mdViaTeal900
    , mdViaTransparent
    , mdViaWhite
    , mdViaYellow100
    , mdViaYellow200
    , mdViaYellow300
    , mdViaYellow400
    , mdViaYellow500
    , mdViaYellow600
    , mdViaYellow700
    , mdViaYellow800
    , mdViaYellow900
    , mdVisible
    , mdW0
    , mdW1
    , mdW10
    , mdW10over12
    , mdW11over12
    , mdW12
    , mdW16
    , mdW1over12
    , mdW1over2
    , mdW1over3
    , mdW1over4
    , mdW1over5
    , mdW1over6
    , mdW2
    , mdW20
    , mdW24
    , mdW2over12
    , mdW2over3
    , mdW2over4
    , mdW2over5
    , mdW2over6
    , mdW3
    , mdW32
    , mdW3over12
    , mdW3over4
    , mdW3over5
    , mdW3over6
    , mdW4
    , mdW40
    , mdW48
    , mdW4over12
    , mdW4over5
    , mdW4over6
    , mdW5
    , mdW56
    , mdW5over12
    , mdW5over6
    , mdW6
    , mdW64
    , mdW6over12
    , mdW7over12
    , mdW8
    , mdW8over12
    , mdW9over12
    , mdWAuto
    , mdWFull
    , mdWPx
    , mdWScreen
    , mdWhitespaceNoWrap
    , mdWhitespaceNormal
    , mdWhitespacePre
    , mdWhitespacePreLine
    , mdWhitespacePreWrap
    , mdZ0
    , mdZ10
    , mdZ20
    , mdZ30
    , mdZ40
    , mdZ50
    , mdZAuto
    , minH0
    , minHFull
    , minHScreen
    , minW0
    , minWFull
    , ml0
    , ml1
    , ml10
    , ml12
    , ml16
    , ml2
    , ml20
    , ml24
    , ml3
    , ml32
    , ml4
    , ml40
    , ml48
    , ml5
    , ml56
    , ml6
    , ml64
    , ml8
    , mlAuto
    , mlPx
    , mr0
    , mr1
    , mr10
    , mr12
    , mr16
    , mr2
    , mr20
    , mr24
    , mr3
    , mr32
    , mr4
    , mr40
    , mr48
    , mr5
    , mr56
    , mr6
    , mr64
    , mr8
    , mrAuto
    , mrPx
    , mt0
    , mt1
    , mt10
    , mt12
    , mt16
    , mt2
    , mt20
    , mt24
    , mt3
    , mt32
    , mt4
    , mt40
    , mt48
    , mt5
    , mt56
    , mt6
    , mt64
    , mt8
    , mtAuto
    , mtPx
    , mx0
    , mx1
    , mx10
    , mx12
    , mx16
    , mx2
    , mx20
    , mx24
    , mx3
    , mx32
    , mx4
    , mx40
    , mx48
    , mx5
    , mx56
    , mx6
    , mx64
    , mx8
    , mxAuto
    , mxPx
    , my0
    , my1
    , my10
    , my12
    , my16
    , my2
    , my20
    , my24
    , my3
    , my32
    , my4
    , my40
    , my48
    , my5
    , my56
    , my6
    , my64
    , my8
    , myAuto
    , myPx
    , negM1
    , negM10
    , negM12
    , negM16
    , negM2
    , negM20
    , negM24
    , negM3
    , negM32
    , negM4
    , negM40
    , negM48
    , negM5
    , negM56
    , negM6
    , negM64
    , negM8
    , negMPx
    , negMb1
    , negMb10
    , negMb12
    , negMb16
    , negMb2
    , negMb20
    , negMb24
    , negMb3
    , negMb32
    , negMb4
    , negMb40
    , negMb48
    , negMb5
    , negMb56
    , negMb6
    , negMb64
    , negMb8
    , negMbPx
    , negMl1
    , negMl10
    , negMl12
    , negMl16
    , negMl2
    , negMl20
    , negMl24
    , negMl3
    , negMl32
    , negMl4
    , negMl40
    , negMl48
    , negMl5
    , negMl56
    , negMl6
    , negMl64
    , negMl8
    , negMlPx
    , negMr1
    , negMr10
    , negMr12
    , negMr16
    , negMr2
    , negMr20
    , negMr24
    , negMr3
    , negMr32
    , negMr4
    , negMr40
    , negMr48
    , negMr5
    , negMr56
    , negMr6
    , negMr64
    , negMr8
    , negMrPx
    , negMt1
    , negMt10
    , negMt12
    , negMt16
    , negMt2
    , negMt20
    , negMt24
    , negMt3
    , negMt32
    , negMt4
    , negMt40
    , negMt48
    , negMt5
    , negMt56
    , negMt6
    , negMt64
    , negMt8
    , negMtPx
    , negMx1
    , negMx10
    , negMx12
    , negMx16
    , negMx2
    , negMx20
    , negMx24
    , negMx3
    , negMx32
    , negMx4
    , negMx40
    , negMx48
    , negMx5
    , negMx56
    , negMx6
    , negMx64
    , negMx8
    , negMxPx
    , negMy1
    , negMy10
    , negMy12
    , negMy16
    , negMy2
    , negMy20
    , negMy24
    , negMy3
    , negMy32
    , negMy4
    , negMy40
    , negMy48
    , negMy5
    , negMy56
    , negMy6
    , negMy64
    , negMy8
    , negMyPx
    , negRotate180
    , negRotate45
    , negRotate90
    , negSkewX12
    , negSkewX3
    , negSkewX6
    , negSkewY12
    , negSkewY3
    , negSkewY6
    , negSpaceX1
    , negSpaceX10
    , negSpaceX12
    , negSpaceX16
    , negSpaceX2
    , negSpaceX20
    , negSpaceX24
    , negSpaceX3
    , negSpaceX32
    , negSpaceX4
    , negSpaceX40
    , negSpaceX48
    , negSpaceX5
    , negSpaceX56
    , negSpaceX6
    , negSpaceX64
    , negSpaceX8
    , negSpaceXPx
    , negSpaceY1
    , negSpaceY10
    , negSpaceY12
    , negSpaceY16
    , negSpaceY2
    , negSpaceY20
    , negSpaceY24
    , negSpaceY3
    , negSpaceY32
    , negSpaceY4
    , negSpaceY40
    , negSpaceY48
    , negSpaceY5
    , negSpaceY56
    , negSpaceY6
    , negSpaceY64
    , negSpaceY8
    , negSpaceYPx
    , negTranslateX1
    , negTranslateX10
    , negTranslateX12
    , negTranslateX16
    , negTranslateX1over2
    , negTranslateX2
    , negTranslateX20
    , negTranslateX24
    , negTranslateX3
    , negTranslateX32
    , negTranslateX4
    , negTranslateX40
    , negTranslateX48
    , negTranslateX5
    , negTranslateX56
    , negTranslateX6
    , negTranslateX64
    , negTranslateX8
    , negTranslateXFull
    , negTranslateXPx
    , negTranslateY1
    , negTranslateY10
    , negTranslateY12
    , negTranslateY16
    , negTranslateY1over2
    , negTranslateY2
    , negTranslateY20
    , negTranslateY24
    , negTranslateY3
    , negTranslateY32
    , negTranslateY4
    , negTranslateY40
    , negTranslateY48
    , negTranslateY5
    , negTranslateY56
    , negTranslateY6
    , negTranslateY64
    , negTranslateY8
    , negTranslateYFull
    , negTranslateYPx
    , noUnderline
    , normalCase
    , normalNums
    , notItalic
    , notSrOnly
    , objectBottom
    , objectCenter
    , objectContain
    , objectCover
    , objectFill
    , objectLeft
    , objectLeftBottom
    , objectLeftTop
    , objectNone
    , objectRight
    , objectRightBottom
    , objectRightTop
    , objectScaleDown
    , objectTop
    , oldstyleNums
    , opacity0
    , opacity100
    , opacity25
    , opacity50
    , opacity75
    , order1
    , order10
    , order11
    , order12
    , order2
    , order3
    , order4
    , order5
    , order6
    , order7
    , order8
    , order9
    , orderFirst
    , orderLast
    , orderNone
    , ordinal
    , originBottom
    , originBottomLeft
    , originBottomRight
    , originCenter
    , originLeft
    , originRight
    , originTop
    , originTopLeft
    , originTopRight
    , outlineNone
    , overflowAuto
    , overflowHidden
    , overflowScroll
    , overflowVisible
    , overflowXAuto
    , overflowXHidden
    , overflowXScroll
    , overflowXVisible
    , overflowYAuto
    , overflowYHidden
    , overflowYScroll
    , overflowYVisible
    , overscrollAuto
    , overscrollContain
    , overscrollNone
    , overscrollXAuto
    , overscrollXContain
    , overscrollXNone
    , overscrollYAuto
    , overscrollYContain
    , overscrollYNone
    , p0
    , p1
    , p10
    , p12
    , p16
    , p2
    , p20
    , p24
    , p3
    , p32
    , p4
    , p40
    , p48
    , p5
    , p56
    , p6
    , p64
    , p8
    , pPx
    , pb0
    , pb1
    , pb10
    , pb12
    , pb16
    , pb2
    , pb20
    , pb24
    , pb3
    , pb32
    , pb4
    , pb40
    , pb48
    , pb5
    , pb56
    , pb6
    , pb64
    , pb8
    , pbPx
    , pl0
    , pl1
    , pl10
    , pl12
    , pl16
    , pl2
    , pl20
    , pl24
    , pl3
    , pl32
    , pl4
    , pl40
    , pl48
    , pl5
    , pl56
    , pl6
    , pl64
    , pl8
    , plPx
    , placeContentAround
    , placeContentBetween
    , placeContentCenter
    , placeContentEnd
    , placeContentEvenly
    , placeContentStart
    , placeContentStretch
    , placeItemsAuto
    , placeItemsCenter
    , placeItemsEnd
    , placeItemsStart
    , placeItemsStretch
    , placeSelfAuto
    , placeSelfCenter
    , placeSelfEnd
    , placeSelfStart
    , placeSelfStretch
    , placeholderBlack
    , placeholderBlue100
    , placeholderBlue200
    , placeholderBlue300
    , placeholderBlue400
    , placeholderBlue500
    , placeholderBlue600
    , placeholderBlue700
    , placeholderBlue800
    , placeholderBlue900
    , placeholderCurrent
    , placeholderGray100
    , placeholderGray200
    , placeholderGray300
    , placeholderGray400
    , placeholderGray500
    , placeholderGray600
    , placeholderGray700
    , placeholderGray800
    , placeholderGray900
    , placeholderGreen100
    , placeholderGreen200
    , placeholderGreen300
    , placeholderGreen400
    , placeholderGreen500
    , placeholderGreen600
    , placeholderGreen700
    , placeholderGreen800
    , placeholderGreen900
    , placeholderIndigo100
    , placeholderIndigo200
    , placeholderIndigo300
    , placeholderIndigo400
    , placeholderIndigo500
    , placeholderIndigo600
    , placeholderIndigo700
    , placeholderIndigo800
    , placeholderIndigo900
    , placeholderOpacity0
    , placeholderOpacity100
    , placeholderOpacity25
    , placeholderOpacity50
    , placeholderOpacity75
    , placeholderOrange100
    , placeholderOrange200
    , placeholderOrange300
    , placeholderOrange400
    , placeholderOrange500
    , placeholderOrange600
    , placeholderOrange700
    , placeholderOrange800
    , placeholderOrange900
    , placeholderPink100
    , placeholderPink200
    , placeholderPink300
    , placeholderPink400
    , placeholderPink500
    , placeholderPink600
    , placeholderPink700
    , placeholderPink800
    , placeholderPink900
    , placeholderPurple100
    , placeholderPurple200
    , placeholderPurple300
    , placeholderPurple400
    , placeholderPurple500
    , placeholderPurple600
    , placeholderPurple700
    , placeholderPurple800
    , placeholderPurple900
    , placeholderRed100
    , placeholderRed200
    , placeholderRed300
    , placeholderRed400
    , placeholderRed500
    , placeholderRed600
    , placeholderRed700
    , placeholderRed800
    , placeholderRed900
    , placeholderTeal100
    , placeholderTeal200
    , placeholderTeal300
    , placeholderTeal400
    , placeholderTeal500
    , placeholderTeal600
    , placeholderTeal700
    , placeholderTeal800
    , placeholderTeal900
    , placeholderTransparent
    , placeholderWhite
    , placeholderYellow100
    , placeholderYellow200
    , placeholderYellow300
    , placeholderYellow400
    , placeholderYellow500
    , placeholderYellow600
    , placeholderYellow700
    , placeholderYellow800
    , placeholderYellow900
    , pointerEventsAuto
    , pointerEventsNone
    , pr0
    , pr1
    , pr10
    , pr12
    , pr16
    , pr2
    , pr20
    , pr24
    , pr3
    , pr32
    , pr4
    , pr40
    , pr48
    , pr5
    , pr56
    , pr6
    , pr64
    , pr8
    , prPx
    , proportionalNums
    , pt0
    , pt1
    , pt10
    , pt12
    , pt16
    , pt2
    , pt20
    , pt24
    , pt3
    , pt32
    , pt4
    , pt40
    , pt48
    , pt5
    , pt56
    , pt6
    , pt64
    , pt8
    , ptPx
    , px0
    , px1
    , px10
    , px12
    , px16
    , px2
    , px20
    , px24
    , px3
    , px32
    , px4
    , px40
    , px48
    , px5
    , px56
    , px6
    , px64
    , px8
    , pxPx
    , py0
    , py1
    , py10
    , py12
    , py16
    , py2
    , py20
    , py24
    , py3
    , py32
    , py4
    , py40
    , py48
    , py5
    , py56
    , py6
    , py64
    , py8
    , pyPx
    , relative
    , resize
    , resizeNone
    , resizeX
    , resizeY
    , right0
    , rightAuto
    , rotate0
    , rotate180
    , rotate45
    , rotate90
    , rounded
    , roundedB
    , roundedBFull
    , roundedBLg
    , roundedBMd
    , roundedBNone
    , roundedBSm
    , roundedBl
    , roundedBlFull
    , roundedBlLg
    , roundedBlMd
    , roundedBlNone
    , roundedBlSm
    , roundedBr
    , roundedBrFull
    , roundedBrLg
    , roundedBrMd
    , roundedBrNone
    , roundedBrSm
    , roundedFull
    , roundedL
    , roundedLFull
    , roundedLLg
    , roundedLMd
    , roundedLNone
    , roundedLSm
    , roundedLg
    , roundedMd
    , roundedNone
    , roundedR
    , roundedRFull
    , roundedRLg
    , roundedRMd
    , roundedRNone
    , roundedRSm
    , roundedSm
    , roundedT
    , roundedTFull
    , roundedTLg
    , roundedTMd
    , roundedTNone
    , roundedTSm
    , roundedTl
    , roundedTlFull
    , roundedTlLg
    , roundedTlMd
    , roundedTlNone
    , roundedTlSm
    , roundedTr
    , roundedTrFull
    , roundedTrLg
    , roundedTrMd
    , roundedTrNone
    , roundedTrSm
    , rowAuto
    , rowEnd1
    , rowEnd2
    , rowEnd3
    , rowEnd4
    , rowEnd5
    , rowEnd6
    , rowEnd7
    , rowEndAuto
    , rowSpan1
    , rowSpan2
    , rowSpan3
    , rowSpan4
    , rowSpan5
    , rowSpan6
    , rowStart1
    , rowStart2
    , rowStart3
    , rowStart4
    , rowStart5
    , rowStart6
    , rowStart7
    , rowStartAuto
    , scale0
    , scale100
    , scale105
    , scale110
    , scale125
    , scale150
    , scale50
    , scale75
    , scale90
    , scale95
    , scaleX0
    , scaleX100
    , scaleX105
    , scaleX110
    , scaleX125
    , scaleX150
    , scaleX50
    , scaleX75
    , scaleX90
    , scaleX95
    , scaleY0
    , scaleY100
    , scaleY105
    , scaleY110
    , scaleY125
    , scaleY150
    , scaleY50
    , scaleY75
    , scaleY90
    , scaleY95
    , scrollingAuto
    , scrollingTouch
    , selectAll
    , selectAuto
    , selectNone
    , selectText
    , selfAuto
    , selfCenter
    , selfEnd
    , selfStart
    , selfStretch
    , shadow
    , shadow2xl
    , shadowInner
    , shadowLg
    , shadowMd
    , shadowNone
    , shadowOutline
    , shadowSm
    , shadowXl
    , shadowXs
    , skewX0
    , skewX12
    , skewX3
    , skewX6
    , skewY0
    , skewY12
    , skewY3
    , skewY6
    , slashedZero
    , smAbsolute
    , smAlignBaseline
    , smAlignBottom
    , smAlignMiddle
    , smAlignTextBottom
    , smAlignTextTop
    , smAlignTop
    , smAnimateBounce
    , smAnimateNone
    , smAnimatePing
    , smAnimatePulse
    , smAnimateSpin
    , smAntialiased
    , smAppearanceNone
    , smBgAuto
    , smBgBlack
    , smBgBlue100
    , smBgBlue200
    , smBgBlue300
    , smBgBlue400
    , smBgBlue500
    , smBgBlue600
    , smBgBlue700
    , smBgBlue800
    , smBgBlue900
    , smBgBottom
    , smBgCenter
    , smBgClipBorder
    , smBgClipContent
    , smBgClipPadding
    , smBgClipText
    , smBgContain
    , smBgCover
    , smBgCurrent
    , smBgFixed
    , smBgGradientToB
    , smBgGradientToBl
    , smBgGradientToBr
    , smBgGradientToL
    , smBgGradientToR
    , smBgGradientToT
    , smBgGradientToTl
    , smBgGradientToTr
    , smBgGray100
    , smBgGray200
    , smBgGray300
    , smBgGray400
    , smBgGray500
    , smBgGray600
    , smBgGray700
    , smBgGray800
    , smBgGray900
    , smBgGreen100
    , smBgGreen200
    , smBgGreen300
    , smBgGreen400
    , smBgGreen500
    , smBgGreen600
    , smBgGreen700
    , smBgGreen800
    , smBgGreen900
    , smBgIndigo100
    , smBgIndigo200
    , smBgIndigo300
    , smBgIndigo400
    , smBgIndigo500
    , smBgIndigo600
    , smBgIndigo700
    , smBgIndigo800
    , smBgIndigo900
    , smBgLeft
    , smBgLeftBottom
    , smBgLeftTop
    , smBgLocal
    , smBgNoRepeat
    , smBgNone
    , smBgOpacity0
    , smBgOpacity100
    , smBgOpacity25
    , smBgOpacity50
    , smBgOpacity75
    , smBgOrange100
    , smBgOrange200
    , smBgOrange300
    , smBgOrange400
    , smBgOrange500
    , smBgOrange600
    , smBgOrange700
    , smBgOrange800
    , smBgOrange900
    , smBgPink100
    , smBgPink200
    , smBgPink300
    , smBgPink400
    , smBgPink500
    , smBgPink600
    , smBgPink700
    , smBgPink800
    , smBgPink900
    , smBgPurple100
    , smBgPurple200
    , smBgPurple300
    , smBgPurple400
    , smBgPurple500
    , smBgPurple600
    , smBgPurple700
    , smBgPurple800
    , smBgPurple900
    , smBgRed100
    , smBgRed200
    , smBgRed300
    , smBgRed400
    , smBgRed500
    , smBgRed600
    , smBgRed700
    , smBgRed800
    , smBgRed900
    , smBgRepeat
    , smBgRepeatRound
    , smBgRepeatSpace
    , smBgRepeatX
    , smBgRepeatY
    , smBgRight
    , smBgRightBottom
    , smBgRightTop
    , smBgScroll
    , smBgTeal100
    , smBgTeal200
    , smBgTeal300
    , smBgTeal400
    , smBgTeal500
    , smBgTeal600
    , smBgTeal700
    , smBgTeal800
    , smBgTeal900
    , smBgTop
    , smBgTransparent
    , smBgWhite
    , smBgYellow100
    , smBgYellow200
    , smBgYellow300
    , smBgYellow400
    , smBgYellow500
    , smBgYellow600
    , smBgYellow700
    , smBgYellow800
    , smBgYellow900
    , smBlock
    , smBorder
    , smBorder0
    , smBorder2
    , smBorder4
    , smBorder8
    , smBorderB
    , smBorderB0
    , smBorderB2
    , smBorderB4
    , smBorderB8
    , smBorderBlack
    , smBorderBlue100
    , smBorderBlue200
    , smBorderBlue300
    , smBorderBlue400
    , smBorderBlue500
    , smBorderBlue600
    , smBorderBlue700
    , smBorderBlue800
    , smBorderBlue900
    , smBorderCollapse
    , smBorderCurrent
    , smBorderDashed
    , smBorderDotted
    , smBorderDouble
    , smBorderGray100
    , smBorderGray200
    , smBorderGray300
    , smBorderGray400
    , smBorderGray500
    , smBorderGray600
    , smBorderGray700
    , smBorderGray800
    , smBorderGray900
    , smBorderGreen100
    , smBorderGreen200
    , smBorderGreen300
    , smBorderGreen400
    , smBorderGreen500
    , smBorderGreen600
    , smBorderGreen700
    , smBorderGreen800
    , smBorderGreen900
    , smBorderIndigo100
    , smBorderIndigo200
    , smBorderIndigo300
    , smBorderIndigo400
    , smBorderIndigo500
    , smBorderIndigo600
    , smBorderIndigo700
    , smBorderIndigo800
    , smBorderIndigo900
    , smBorderL
    , smBorderL0
    , smBorderL2
    , smBorderL4
    , smBorderL8
    , smBorderNone
    , smBorderOpacity0
    , smBorderOpacity100
    , smBorderOpacity25
    , smBorderOpacity50
    , smBorderOpacity75
    , smBorderOrange100
    , smBorderOrange200
    , smBorderOrange300
    , smBorderOrange400
    , smBorderOrange500
    , smBorderOrange600
    , smBorderOrange700
    , smBorderOrange800
    , smBorderOrange900
    , smBorderPink100
    , smBorderPink200
    , smBorderPink300
    , smBorderPink400
    , smBorderPink500
    , smBorderPink600
    , smBorderPink700
    , smBorderPink800
    , smBorderPink900
    , smBorderPurple100
    , smBorderPurple200
    , smBorderPurple300
    , smBorderPurple400
    , smBorderPurple500
    , smBorderPurple600
    , smBorderPurple700
    , smBorderPurple800
    , smBorderPurple900
    , smBorderR
    , smBorderR0
    , smBorderR2
    , smBorderR4
    , smBorderR8
    , smBorderRed100
    , smBorderRed200
    , smBorderRed300
    , smBorderRed400
    , smBorderRed500
    , smBorderRed600
    , smBorderRed700
    , smBorderRed800
    , smBorderRed900
    , smBorderSeparate
    , smBorderSolid
    , smBorderT
    , smBorderT0
    , smBorderT2
    , smBorderT4
    , smBorderT8
    , smBorderTeal100
    , smBorderTeal200
    , smBorderTeal300
    , smBorderTeal400
    , smBorderTeal500
    , smBorderTeal600
    , smBorderTeal700
    , smBorderTeal800
    , smBorderTeal900
    , smBorderTransparent
    , smBorderWhite
    , smBorderYellow100
    , smBorderYellow200
    , smBorderYellow300
    , smBorderYellow400
    , smBorderYellow500
    , smBorderYellow600
    , smBorderYellow700
    , smBorderYellow800
    , smBorderYellow900
    , smBottom0
    , smBottomAuto
    , smBoxBorder
    , smBoxContent
    , smBreakAll
    , smBreakNormal
    , smBreakWords
    , smCapitalize
    , smClearBoth
    , smClearLeft
    , smClearNone
    , smClearRight
    , smClearfixAfter
    , smColAuto
    , smColEnd1
    , smColEnd10
    , smColEnd11
    , smColEnd12
    , smColEnd13
    , smColEnd2
    , smColEnd3
    , smColEnd4
    , smColEnd5
    , smColEnd6
    , smColEnd7
    , smColEnd8
    , smColEnd9
    , smColEndAuto
    , smColSpan1
    , smColSpan10
    , smColSpan11
    , smColSpan12
    , smColSpan2
    , smColSpan3
    , smColSpan4
    , smColSpan5
    , smColSpan6
    , smColSpan7
    , smColSpan8
    , smColSpan9
    , smColStart1
    , smColStart10
    , smColStart11
    , smColStart12
    , smColStart13
    , smColStart2
    , smColStart3
    , smColStart4
    , smColStart5
    , smColStart6
    , smColStart7
    , smColStart8
    , smColStart9
    , smColStartAuto
    , smContainer
    , smContentAround
    , smContentBetween
    , smContentCenter
    , smContentEnd
    , smContentEvenly
    , smContentStart
    , smContents
    , smCursorAuto
    , smCursorDefault
    , smCursorMove
    , smCursorNotAllowed
    , smCursorPointer
    , smCursorText
    , smCursorWait
    , smDelay100
    , smDelay1000
    , smDelay150
    , smDelay200
    , smDelay300
    , smDelay500
    , smDelay700
    , smDelay75
    , smDiagonalFractions
    , smDivideBlack
    , smDivideBlue100
    , smDivideBlue200
    , smDivideBlue300
    , smDivideBlue400
    , smDivideBlue500
    , smDivideBlue600
    , smDivideBlue700
    , smDivideBlue800
    , smDivideBlue900
    , smDivideCurrent
    , smDivideDashed
    , smDivideDotted
    , smDivideDouble
    , smDivideGray100
    , smDivideGray200
    , smDivideGray300
    , smDivideGray400
    , smDivideGray500
    , smDivideGray600
    , smDivideGray700
    , smDivideGray800
    , smDivideGray900
    , smDivideGreen100
    , smDivideGreen200
    , smDivideGreen300
    , smDivideGreen400
    , smDivideGreen500
    , smDivideGreen600
    , smDivideGreen700
    , smDivideGreen800
    , smDivideGreen900
    , smDivideIndigo100
    , smDivideIndigo200
    , smDivideIndigo300
    , smDivideIndigo400
    , smDivideIndigo500
    , smDivideIndigo600
    , smDivideIndigo700
    , smDivideIndigo800
    , smDivideIndigo900
    , smDivideNone
    , smDivideOpacity0
    , smDivideOpacity100
    , smDivideOpacity25
    , smDivideOpacity50
    , smDivideOpacity75
    , smDivideOrange100
    , smDivideOrange200
    , smDivideOrange300
    , smDivideOrange400
    , smDivideOrange500
    , smDivideOrange600
    , smDivideOrange700
    , smDivideOrange800
    , smDivideOrange900
    , smDividePink100
    , smDividePink200
    , smDividePink300
    , smDividePink400
    , smDividePink500
    , smDividePink600
    , smDividePink700
    , smDividePink800
    , smDividePink900
    , smDividePurple100
    , smDividePurple200
    , smDividePurple300
    , smDividePurple400
    , smDividePurple500
    , smDividePurple600
    , smDividePurple700
    , smDividePurple800
    , smDividePurple900
    , smDivideRed100
    , smDivideRed200
    , smDivideRed300
    , smDivideRed400
    , smDivideRed500
    , smDivideRed600
    , smDivideRed700
    , smDivideRed800
    , smDivideRed900
    , smDivideSolid
    , smDivideTeal100
    , smDivideTeal200
    , smDivideTeal300
    , smDivideTeal400
    , smDivideTeal500
    , smDivideTeal600
    , smDivideTeal700
    , smDivideTeal800
    , smDivideTeal900
    , smDivideTransparent
    , smDivideWhite
    , smDivideX
    , smDivideX0
    , smDivideX2
    , smDivideX4
    , smDivideX8
    , smDivideXReverse
    , smDivideY
    , smDivideY0
    , smDivideY2
    , smDivideY4
    , smDivideY8
    , smDivideYReverse
    , smDivideYellow100
    , smDivideYellow200
    , smDivideYellow300
    , smDivideYellow400
    , smDivideYellow500
    , smDivideYellow600
    , smDivideYellow700
    , smDivideYellow800
    , smDivideYellow900
    , smDuration100
    , smDuration1000
    , smDuration150
    , smDuration200
    , smDuration300
    , smDuration500
    , smDuration700
    , smDuration75
    , smEaseIn
    , smEaseInOut
    , smEaseLinear
    , smEaseOut
    , smFillCurrent
    , smFixed
    , smFlex
    , smFlex1
    , smFlexAuto
    , smFlexCol
    , smFlexColReverse
    , smFlexGrow
    , smFlexGrow0
    , smFlexInitial
    , smFlexNoWrap
    , smFlexNone
    , smFlexRow
    , smFlexRowReverse
    , smFlexShrink
    , smFlexShrink0
    , smFlexWrap
    , smFlexWrapReverse
    , smFloatLeft
    , smFloatNone
    , smFloatRight
    , smFlowRoot
    , smFocusBgBlack
    , smFocusBgBlue100
    , smFocusBgBlue200
    , smFocusBgBlue300
    , smFocusBgBlue400
    , smFocusBgBlue500
    , smFocusBgBlue600
    , smFocusBgBlue700
    , smFocusBgBlue800
    , smFocusBgBlue900
    , smFocusBgCurrent
    , smFocusBgGray100
    , smFocusBgGray200
    , smFocusBgGray300
    , smFocusBgGray400
    , smFocusBgGray500
    , smFocusBgGray600
    , smFocusBgGray700
    , smFocusBgGray800
    , smFocusBgGray900
    , smFocusBgGreen100
    , smFocusBgGreen200
    , smFocusBgGreen300
    , smFocusBgGreen400
    , smFocusBgGreen500
    , smFocusBgGreen600
    , smFocusBgGreen700
    , smFocusBgGreen800
    , smFocusBgGreen900
    , smFocusBgIndigo100
    , smFocusBgIndigo200
    , smFocusBgIndigo300
    , smFocusBgIndigo400
    , smFocusBgIndigo500
    , smFocusBgIndigo600
    , smFocusBgIndigo700
    , smFocusBgIndigo800
    , smFocusBgIndigo900
    , smFocusBgOpacity0
    , smFocusBgOpacity100
    , smFocusBgOpacity25
    , smFocusBgOpacity50
    , smFocusBgOpacity75
    , smFocusBgOrange100
    , smFocusBgOrange200
    , smFocusBgOrange300
    , smFocusBgOrange400
    , smFocusBgOrange500
    , smFocusBgOrange600
    , smFocusBgOrange700
    , smFocusBgOrange800
    , smFocusBgOrange900
    , smFocusBgPink100
    , smFocusBgPink200
    , smFocusBgPink300
    , smFocusBgPink400
    , smFocusBgPink500
    , smFocusBgPink600
    , smFocusBgPink700
    , smFocusBgPink800
    , smFocusBgPink900
    , smFocusBgPurple100
    , smFocusBgPurple200
    , smFocusBgPurple300
    , smFocusBgPurple400
    , smFocusBgPurple500
    , smFocusBgPurple600
    , smFocusBgPurple700
    , smFocusBgPurple800
    , smFocusBgPurple900
    , smFocusBgRed100
    , smFocusBgRed200
    , smFocusBgRed300
    , smFocusBgRed400
    , smFocusBgRed500
    , smFocusBgRed600
    , smFocusBgRed700
    , smFocusBgRed800
    , smFocusBgRed900
    , smFocusBgTeal100
    , smFocusBgTeal200
    , smFocusBgTeal300
    , smFocusBgTeal400
    , smFocusBgTeal500
    , smFocusBgTeal600
    , smFocusBgTeal700
    , smFocusBgTeal800
    , smFocusBgTeal900
    , smFocusBgTransparent
    , smFocusBgWhite
    , smFocusBgYellow100
    , smFocusBgYellow200
    , smFocusBgYellow300
    , smFocusBgYellow400
    , smFocusBgYellow500
    , smFocusBgYellow600
    , smFocusBgYellow700
    , smFocusBgYellow800
    , smFocusBgYellow900
    , smFocusBorderBlack
    , smFocusBorderBlue100
    , smFocusBorderBlue200
    , smFocusBorderBlue300
    , smFocusBorderBlue400
    , smFocusBorderBlue500
    , smFocusBorderBlue600
    , smFocusBorderBlue700
    , smFocusBorderBlue800
    , smFocusBorderBlue900
    , smFocusBorderCurrent
    , smFocusBorderGray100
    , smFocusBorderGray200
    , smFocusBorderGray300
    , smFocusBorderGray400
    , smFocusBorderGray500
    , smFocusBorderGray600
    , smFocusBorderGray700
    , smFocusBorderGray800
    , smFocusBorderGray900
    , smFocusBorderGreen100
    , smFocusBorderGreen200
    , smFocusBorderGreen300
    , smFocusBorderGreen400
    , smFocusBorderGreen500
    , smFocusBorderGreen600
    , smFocusBorderGreen700
    , smFocusBorderGreen800
    , smFocusBorderGreen900
    , smFocusBorderIndigo100
    , smFocusBorderIndigo200
    , smFocusBorderIndigo300
    , smFocusBorderIndigo400
    , smFocusBorderIndigo500
    , smFocusBorderIndigo600
    , smFocusBorderIndigo700
    , smFocusBorderIndigo800
    , smFocusBorderIndigo900
    , smFocusBorderOpacity0
    , smFocusBorderOpacity100
    , smFocusBorderOpacity25
    , smFocusBorderOpacity50
    , smFocusBorderOpacity75
    , smFocusBorderOrange100
    , smFocusBorderOrange200
    , smFocusBorderOrange300
    , smFocusBorderOrange400
    , smFocusBorderOrange500
    , smFocusBorderOrange600
    , smFocusBorderOrange700
    , smFocusBorderOrange800
    , smFocusBorderOrange900
    , smFocusBorderPink100
    , smFocusBorderPink200
    , smFocusBorderPink300
    , smFocusBorderPink400
    , smFocusBorderPink500
    , smFocusBorderPink600
    , smFocusBorderPink700
    , smFocusBorderPink800
    , smFocusBorderPink900
    , smFocusBorderPurple100
    , smFocusBorderPurple200
    , smFocusBorderPurple300
    , smFocusBorderPurple400
    , smFocusBorderPurple500
    , smFocusBorderPurple600
    , smFocusBorderPurple700
    , smFocusBorderPurple800
    , smFocusBorderPurple900
    , smFocusBorderRed100
    , smFocusBorderRed200
    , smFocusBorderRed300
    , smFocusBorderRed400
    , smFocusBorderRed500
    , smFocusBorderRed600
    , smFocusBorderRed700
    , smFocusBorderRed800
    , smFocusBorderRed900
    , smFocusBorderTeal100
    , smFocusBorderTeal200
    , smFocusBorderTeal300
    , smFocusBorderTeal400
    , smFocusBorderTeal500
    , smFocusBorderTeal600
    , smFocusBorderTeal700
    , smFocusBorderTeal800
    , smFocusBorderTeal900
    , smFocusBorderTransparent
    , smFocusBorderWhite
    , smFocusBorderYellow100
    , smFocusBorderYellow200
    , smFocusBorderYellow300
    , smFocusBorderYellow400
    , smFocusBorderYellow500
    , smFocusBorderYellow600
    , smFocusBorderYellow700
    , smFocusBorderYellow800
    , smFocusBorderYellow900
    , smFocusFontBlack
    , smFocusFontBold
    , smFocusFontExtrabold
    , smFocusFontHairline
    , smFocusFontLight
    , smFocusFontMedium
    , smFocusFontNormal
    , smFocusFontSemibold
    , smFocusFontThin
    , smFocusFromBlack
    , smFocusFromBlue100
    , smFocusFromBlue200
    , smFocusFromBlue300
    , smFocusFromBlue400
    , smFocusFromBlue500
    , smFocusFromBlue600
    , smFocusFromBlue700
    , smFocusFromBlue800
    , smFocusFromBlue900
    , smFocusFromCurrent
    , smFocusFromGray100
    , smFocusFromGray200
    , smFocusFromGray300
    , smFocusFromGray400
    , smFocusFromGray500
    , smFocusFromGray600
    , smFocusFromGray700
    , smFocusFromGray800
    , smFocusFromGray900
    , smFocusFromGreen100
    , smFocusFromGreen200
    , smFocusFromGreen300
    , smFocusFromGreen400
    , smFocusFromGreen500
    , smFocusFromGreen600
    , smFocusFromGreen700
    , smFocusFromGreen800
    , smFocusFromGreen900
    , smFocusFromIndigo100
    , smFocusFromIndigo200
    , smFocusFromIndigo300
    , smFocusFromIndigo400
    , smFocusFromIndigo500
    , smFocusFromIndigo600
    , smFocusFromIndigo700
    , smFocusFromIndigo800
    , smFocusFromIndigo900
    , smFocusFromOrange100
    , smFocusFromOrange200
    , smFocusFromOrange300
    , smFocusFromOrange400
    , smFocusFromOrange500
    , smFocusFromOrange600
    , smFocusFromOrange700
    , smFocusFromOrange800
    , smFocusFromOrange900
    , smFocusFromPink100
    , smFocusFromPink200
    , smFocusFromPink300
    , smFocusFromPink400
    , smFocusFromPink500
    , smFocusFromPink600
    , smFocusFromPink700
    , smFocusFromPink800
    , smFocusFromPink900
    , smFocusFromPurple100
    , smFocusFromPurple200
    , smFocusFromPurple300
    , smFocusFromPurple400
    , smFocusFromPurple500
    , smFocusFromPurple600
    , smFocusFromPurple700
    , smFocusFromPurple800
    , smFocusFromPurple900
    , smFocusFromRed100
    , smFocusFromRed200
    , smFocusFromRed300
    , smFocusFromRed400
    , smFocusFromRed500
    , smFocusFromRed600
    , smFocusFromRed700
    , smFocusFromRed800
    , smFocusFromRed900
    , smFocusFromTeal100
    , smFocusFromTeal200
    , smFocusFromTeal300
    , smFocusFromTeal400
    , smFocusFromTeal500
    , smFocusFromTeal600
    , smFocusFromTeal700
    , smFocusFromTeal800
    , smFocusFromTeal900
    , smFocusFromTransparent
    , smFocusFromWhite
    , smFocusFromYellow100
    , smFocusFromYellow200
    , smFocusFromYellow300
    , smFocusFromYellow400
    , smFocusFromYellow500
    , smFocusFromYellow600
    , smFocusFromYellow700
    , smFocusFromYellow800
    , smFocusFromYellow900
    , smFocusLineThrough
    , smFocusNegRotate180
    , smFocusNegRotate45
    , smFocusNegRotate90
    , smFocusNegSkewX12
    , smFocusNegSkewX3
    , smFocusNegSkewX6
    , smFocusNegSkewY12
    , smFocusNegSkewY3
    , smFocusNegSkewY6
    , smFocusNegTranslateX1
    , smFocusNegTranslateX10
    , smFocusNegTranslateX12
    , smFocusNegTranslateX16
    , smFocusNegTranslateX1over2
    , smFocusNegTranslateX2
    , smFocusNegTranslateX20
    , smFocusNegTranslateX24
    , smFocusNegTranslateX3
    , smFocusNegTranslateX32
    , smFocusNegTranslateX4
    , smFocusNegTranslateX40
    , smFocusNegTranslateX48
    , smFocusNegTranslateX5
    , smFocusNegTranslateX56
    , smFocusNegTranslateX6
    , smFocusNegTranslateX64
    , smFocusNegTranslateX8
    , smFocusNegTranslateXFull
    , smFocusNegTranslateXPx
    , smFocusNegTranslateY1
    , smFocusNegTranslateY10
    , smFocusNegTranslateY12
    , smFocusNegTranslateY16
    , smFocusNegTranslateY1over2
    , smFocusNegTranslateY2
    , smFocusNegTranslateY20
    , smFocusNegTranslateY24
    , smFocusNegTranslateY3
    , smFocusNegTranslateY32
    , smFocusNegTranslateY4
    , smFocusNegTranslateY40
    , smFocusNegTranslateY48
    , smFocusNegTranslateY5
    , smFocusNegTranslateY56
    , smFocusNegTranslateY6
    , smFocusNegTranslateY64
    , smFocusNegTranslateY8
    , smFocusNegTranslateYFull
    , smFocusNegTranslateYPx
    , smFocusNoUnderline
    , smFocusNotSrOnly
    , smFocusOpacity0
    , smFocusOpacity100
    , smFocusOpacity25
    , smFocusOpacity50
    , smFocusOpacity75
    , smFocusOutlineNone
    , smFocusPlaceholderBlackFocus
    , smFocusPlaceholderBlue100Focus
    , smFocusPlaceholderBlue200Focus
    , smFocusPlaceholderBlue300Focus
    , smFocusPlaceholderBlue400Focus
    , smFocusPlaceholderBlue500Focus
    , smFocusPlaceholderBlue600Focus
    , smFocusPlaceholderBlue700Focus
    , smFocusPlaceholderBlue800Focus
    , smFocusPlaceholderBlue900Focus
    , smFocusPlaceholderCurrentFocus
    , smFocusPlaceholderGray100Focus
    , smFocusPlaceholderGray200Focus
    , smFocusPlaceholderGray300Focus
    , smFocusPlaceholderGray400Focus
    , smFocusPlaceholderGray500Focus
    , smFocusPlaceholderGray600Focus
    , smFocusPlaceholderGray700Focus
    , smFocusPlaceholderGray800Focus
    , smFocusPlaceholderGray900Focus
    , smFocusPlaceholderGreen100Focus
    , smFocusPlaceholderGreen200Focus
    , smFocusPlaceholderGreen300Focus
    , smFocusPlaceholderGreen400Focus
    , smFocusPlaceholderGreen500Focus
    , smFocusPlaceholderGreen600Focus
    , smFocusPlaceholderGreen700Focus
    , smFocusPlaceholderGreen800Focus
    , smFocusPlaceholderGreen900Focus
    , smFocusPlaceholderIndigo100Focus
    , smFocusPlaceholderIndigo200Focus
    , smFocusPlaceholderIndigo300Focus
    , smFocusPlaceholderIndigo400Focus
    , smFocusPlaceholderIndigo500Focus
    , smFocusPlaceholderIndigo600Focus
    , smFocusPlaceholderIndigo700Focus
    , smFocusPlaceholderIndigo800Focus
    , smFocusPlaceholderIndigo900Focus
    , smFocusPlaceholderOpacity0Focus
    , smFocusPlaceholderOpacity100Focus
    , smFocusPlaceholderOpacity25Focus
    , smFocusPlaceholderOpacity50Focus
    , smFocusPlaceholderOpacity75Focus
    , smFocusPlaceholderOrange100Focus
    , smFocusPlaceholderOrange200Focus
    , smFocusPlaceholderOrange300Focus
    , smFocusPlaceholderOrange400Focus
    , smFocusPlaceholderOrange500Focus
    , smFocusPlaceholderOrange600Focus
    , smFocusPlaceholderOrange700Focus
    , smFocusPlaceholderOrange800Focus
    , smFocusPlaceholderOrange900Focus
    , smFocusPlaceholderPink100Focus
    , smFocusPlaceholderPink200Focus
    , smFocusPlaceholderPink300Focus
    , smFocusPlaceholderPink400Focus
    , smFocusPlaceholderPink500Focus
    , smFocusPlaceholderPink600Focus
    , smFocusPlaceholderPink700Focus
    , smFocusPlaceholderPink800Focus
    , smFocusPlaceholderPink900Focus
    , smFocusPlaceholderPurple100Focus
    , smFocusPlaceholderPurple200Focus
    , smFocusPlaceholderPurple300Focus
    , smFocusPlaceholderPurple400Focus
    , smFocusPlaceholderPurple500Focus
    , smFocusPlaceholderPurple600Focus
    , smFocusPlaceholderPurple700Focus
    , smFocusPlaceholderPurple800Focus
    , smFocusPlaceholderPurple900Focus
    , smFocusPlaceholderRed100Focus
    , smFocusPlaceholderRed200Focus
    , smFocusPlaceholderRed300Focus
    , smFocusPlaceholderRed400Focus
    , smFocusPlaceholderRed500Focus
    , smFocusPlaceholderRed600Focus
    , smFocusPlaceholderRed700Focus
    , smFocusPlaceholderRed800Focus
    , smFocusPlaceholderRed900Focus
    , smFocusPlaceholderTeal100Focus
    , smFocusPlaceholderTeal200Focus
    , smFocusPlaceholderTeal300Focus
    , smFocusPlaceholderTeal400Focus
    , smFocusPlaceholderTeal500Focus
    , smFocusPlaceholderTeal600Focus
    , smFocusPlaceholderTeal700Focus
    , smFocusPlaceholderTeal800Focus
    , smFocusPlaceholderTeal900Focus
    , smFocusPlaceholderTransparentFocus
    , smFocusPlaceholderWhiteFocus
    , smFocusPlaceholderYellow100Focus
    , smFocusPlaceholderYellow200Focus
    , smFocusPlaceholderYellow300Focus
    , smFocusPlaceholderYellow400Focus
    , smFocusPlaceholderYellow500Focus
    , smFocusPlaceholderYellow600Focus
    , smFocusPlaceholderYellow700Focus
    , smFocusPlaceholderYellow800Focus
    , smFocusPlaceholderYellow900Focus
    , smFocusRotate0
    , smFocusRotate180
    , smFocusRotate45
    , smFocusRotate90
    , smFocusScale0
    , smFocusScale100
    , smFocusScale105
    , smFocusScale110
    , smFocusScale125
    , smFocusScale150
    , smFocusScale50
    , smFocusScale75
    , smFocusScale90
    , smFocusScale95
    , smFocusScaleX0
    , smFocusScaleX100
    , smFocusScaleX105
    , smFocusScaleX110
    , smFocusScaleX125
    , smFocusScaleX150
    , smFocusScaleX50
    , smFocusScaleX75
    , smFocusScaleX90
    , smFocusScaleX95
    , smFocusScaleY0
    , smFocusScaleY100
    , smFocusScaleY105
    , smFocusScaleY110
    , smFocusScaleY125
    , smFocusScaleY150
    , smFocusScaleY50
    , smFocusScaleY75
    , smFocusScaleY90
    , smFocusScaleY95
    , smFocusShadow
    , smFocusShadow2xl
    , smFocusShadowInner
    , smFocusShadowLg
    , smFocusShadowMd
    , smFocusShadowNone
    , smFocusShadowOutline
    , smFocusShadowSm
    , smFocusShadowXl
    , smFocusShadowXs
    , smFocusSkewX0
    , smFocusSkewX12
    , smFocusSkewX3
    , smFocusSkewX6
    , smFocusSkewY0
    , smFocusSkewY12
    , smFocusSkewY3
    , smFocusSkewY6
    , smFocusSrOnly
    , smFocusTextBlack
    , smFocusTextBlue100
    , smFocusTextBlue200
    , smFocusTextBlue300
    , smFocusTextBlue400
    , smFocusTextBlue500
    , smFocusTextBlue600
    , smFocusTextBlue700
    , smFocusTextBlue800
    , smFocusTextBlue900
    , smFocusTextCurrent
    , smFocusTextGray100
    , smFocusTextGray200
    , smFocusTextGray300
    , smFocusTextGray400
    , smFocusTextGray500
    , smFocusTextGray600
    , smFocusTextGray700
    , smFocusTextGray800
    , smFocusTextGray900
    , smFocusTextGreen100
    , smFocusTextGreen200
    , smFocusTextGreen300
    , smFocusTextGreen400
    , smFocusTextGreen500
    , smFocusTextGreen600
    , smFocusTextGreen700
    , smFocusTextGreen800
    , smFocusTextGreen900
    , smFocusTextIndigo100
    , smFocusTextIndigo200
    , smFocusTextIndigo300
    , smFocusTextIndigo400
    , smFocusTextIndigo500
    , smFocusTextIndigo600
    , smFocusTextIndigo700
    , smFocusTextIndigo800
    , smFocusTextIndigo900
    , smFocusTextOpacity0
    , smFocusTextOpacity100
    , smFocusTextOpacity25
    , smFocusTextOpacity50
    , smFocusTextOpacity75
    , smFocusTextOrange100
    , smFocusTextOrange200
    , smFocusTextOrange300
    , smFocusTextOrange400
    , smFocusTextOrange500
    , smFocusTextOrange600
    , smFocusTextOrange700
    , smFocusTextOrange800
    , smFocusTextOrange900
    , smFocusTextPink100
    , smFocusTextPink200
    , smFocusTextPink300
    , smFocusTextPink400
    , smFocusTextPink500
    , smFocusTextPink600
    , smFocusTextPink700
    , smFocusTextPink800
    , smFocusTextPink900
    , smFocusTextPurple100
    , smFocusTextPurple200
    , smFocusTextPurple300
    , smFocusTextPurple400
    , smFocusTextPurple500
    , smFocusTextPurple600
    , smFocusTextPurple700
    , smFocusTextPurple800
    , smFocusTextPurple900
    , smFocusTextRed100
    , smFocusTextRed200
    , smFocusTextRed300
    , smFocusTextRed400
    , smFocusTextRed500
    , smFocusTextRed600
    , smFocusTextRed700
    , smFocusTextRed800
    , smFocusTextRed900
    , smFocusTextTeal100
    , smFocusTextTeal200
    , smFocusTextTeal300
    , smFocusTextTeal400
    , smFocusTextTeal500
    , smFocusTextTeal600
    , smFocusTextTeal700
    , smFocusTextTeal800
    , smFocusTextTeal900
    , smFocusTextTransparent
    , smFocusTextWhite
    , smFocusTextYellow100
    , smFocusTextYellow200
    , smFocusTextYellow300
    , smFocusTextYellow400
    , smFocusTextYellow500
    , smFocusTextYellow600
    , smFocusTextYellow700
    , smFocusTextYellow800
    , smFocusTextYellow900
    , smFocusToBlack
    , smFocusToBlue100
    , smFocusToBlue200
    , smFocusToBlue300
    , smFocusToBlue400
    , smFocusToBlue500
    , smFocusToBlue600
    , smFocusToBlue700
    , smFocusToBlue800
    , smFocusToBlue900
    , smFocusToCurrent
    , smFocusToGray100
    , smFocusToGray200
    , smFocusToGray300
    , smFocusToGray400
    , smFocusToGray500
    , smFocusToGray600
    , smFocusToGray700
    , smFocusToGray800
    , smFocusToGray900
    , smFocusToGreen100
    , smFocusToGreen200
    , smFocusToGreen300
    , smFocusToGreen400
    , smFocusToGreen500
    , smFocusToGreen600
    , smFocusToGreen700
    , smFocusToGreen800
    , smFocusToGreen900
    , smFocusToIndigo100
    , smFocusToIndigo200
    , smFocusToIndigo300
    , smFocusToIndigo400
    , smFocusToIndigo500
    , smFocusToIndigo600
    , smFocusToIndigo700
    , smFocusToIndigo800
    , smFocusToIndigo900
    , smFocusToOrange100
    , smFocusToOrange200
    , smFocusToOrange300
    , smFocusToOrange400
    , smFocusToOrange500
    , smFocusToOrange600
    , smFocusToOrange700
    , smFocusToOrange800
    , smFocusToOrange900
    , smFocusToPink100
    , smFocusToPink200
    , smFocusToPink300
    , smFocusToPink400
    , smFocusToPink500
    , smFocusToPink600
    , smFocusToPink700
    , smFocusToPink800
    , smFocusToPink900
    , smFocusToPurple100
    , smFocusToPurple200
    , smFocusToPurple300
    , smFocusToPurple400
    , smFocusToPurple500
    , smFocusToPurple600
    , smFocusToPurple700
    , smFocusToPurple800
    , smFocusToPurple900
    , smFocusToRed100
    , smFocusToRed200
    , smFocusToRed300
    , smFocusToRed400
    , smFocusToRed500
    , smFocusToRed600
    , smFocusToRed700
    , smFocusToRed800
    , smFocusToRed900
    , smFocusToTeal100
    , smFocusToTeal200
    , smFocusToTeal300
    , smFocusToTeal400
    , smFocusToTeal500
    , smFocusToTeal600
    , smFocusToTeal700
    , smFocusToTeal800
    , smFocusToTeal900
    , smFocusToTransparent
    , smFocusToWhite
    , smFocusToYellow100
    , smFocusToYellow200
    , smFocusToYellow300
    , smFocusToYellow400
    , smFocusToYellow500
    , smFocusToYellow600
    , smFocusToYellow700
    , smFocusToYellow800
    , smFocusToYellow900
    , smFocusTranslateX0
    , smFocusTranslateX1
    , smFocusTranslateX10
    , smFocusTranslateX12
    , smFocusTranslateX16
    , smFocusTranslateX1over2
    , smFocusTranslateX2
    , smFocusTranslateX20
    , smFocusTranslateX24
    , smFocusTranslateX3
    , smFocusTranslateX32
    , smFocusTranslateX4
    , smFocusTranslateX40
    , smFocusTranslateX48
    , smFocusTranslateX5
    , smFocusTranslateX56
    , smFocusTranslateX6
    , smFocusTranslateX64
    , smFocusTranslateX8
    , smFocusTranslateXFull
    , smFocusTranslateXPx
    , smFocusTranslateY0
    , smFocusTranslateY1
    , smFocusTranslateY10
    , smFocusTranslateY12
    , smFocusTranslateY16
    , smFocusTranslateY1over2
    , smFocusTranslateY2
    , smFocusTranslateY20
    , smFocusTranslateY24
    , smFocusTranslateY3
    , smFocusTranslateY32
    , smFocusTranslateY4
    , smFocusTranslateY40
    , smFocusTranslateY48
    , smFocusTranslateY5
    , smFocusTranslateY56
    , smFocusTranslateY6
    , smFocusTranslateY64
    , smFocusTranslateY8
    , smFocusTranslateYFull
    , smFocusTranslateYPx
    , smFocusUnderline
    , smFocusViaBlack
    , smFocusViaBlue100
    , smFocusViaBlue200
    , smFocusViaBlue300
    , smFocusViaBlue400
    , smFocusViaBlue500
    , smFocusViaBlue600
    , smFocusViaBlue700
    , smFocusViaBlue800
    , smFocusViaBlue900
    , smFocusViaCurrent
    , smFocusViaGray100
    , smFocusViaGray200
    , smFocusViaGray300
    , smFocusViaGray400
    , smFocusViaGray500
    , smFocusViaGray600
    , smFocusViaGray700
    , smFocusViaGray800
    , smFocusViaGray900
    , smFocusViaGreen100
    , smFocusViaGreen200
    , smFocusViaGreen300
    , smFocusViaGreen400
    , smFocusViaGreen500
    , smFocusViaGreen600
    , smFocusViaGreen700
    , smFocusViaGreen800
    , smFocusViaGreen900
    , smFocusViaIndigo100
    , smFocusViaIndigo200
    , smFocusViaIndigo300
    , smFocusViaIndigo400
    , smFocusViaIndigo500
    , smFocusViaIndigo600
    , smFocusViaIndigo700
    , smFocusViaIndigo800
    , smFocusViaIndigo900
    , smFocusViaOrange100
    , smFocusViaOrange200
    , smFocusViaOrange300
    , smFocusViaOrange400
    , smFocusViaOrange500
    , smFocusViaOrange600
    , smFocusViaOrange700
    , smFocusViaOrange800
    , smFocusViaOrange900
    , smFocusViaPink100
    , smFocusViaPink200
    , smFocusViaPink300
    , smFocusViaPink400
    , smFocusViaPink500
    , smFocusViaPink600
    , smFocusViaPink700
    , smFocusViaPink800
    , smFocusViaPink900
    , smFocusViaPurple100
    , smFocusViaPurple200
    , smFocusViaPurple300
    , smFocusViaPurple400
    , smFocusViaPurple500
    , smFocusViaPurple600
    , smFocusViaPurple700
    , smFocusViaPurple800
    , smFocusViaPurple900
    , smFocusViaRed100
    , smFocusViaRed200
    , smFocusViaRed300
    , smFocusViaRed400
    , smFocusViaRed500
    , smFocusViaRed600
    , smFocusViaRed700
    , smFocusViaRed800
    , smFocusViaRed900
    , smFocusViaTeal100
    , smFocusViaTeal200
    , smFocusViaTeal300
    , smFocusViaTeal400
    , smFocusViaTeal500
    , smFocusViaTeal600
    , smFocusViaTeal700
    , smFocusViaTeal800
    , smFocusViaTeal900
    , smFocusViaTransparent
    , smFocusViaWhite
    , smFocusViaYellow100
    , smFocusViaYellow200
    , smFocusViaYellow300
    , smFocusViaYellow400
    , smFocusViaYellow500
    , smFocusViaYellow600
    , smFocusViaYellow700
    , smFocusViaYellow800
    , smFocusViaYellow900
    , smFontBlack
    , smFontBold
    , smFontExtrabold
    , smFontHairline
    , smFontLight
    , smFontMedium
    , smFontMono
    , smFontNormal
    , smFontSans
    , smFontSemibold
    , smFontSerif
    , smFontThin
    , smFromBlack
    , smFromBlue100
    , smFromBlue200
    , smFromBlue300
    , smFromBlue400
    , smFromBlue500
    , smFromBlue600
    , smFromBlue700
    , smFromBlue800
    , smFromBlue900
    , smFromCurrent
    , smFromGray100
    , smFromGray200
    , smFromGray300
    , smFromGray400
    , smFromGray500
    , smFromGray600
    , smFromGray700
    , smFromGray800
    , smFromGray900
    , smFromGreen100
    , smFromGreen200
    , smFromGreen300
    , smFromGreen400
    , smFromGreen500
    , smFromGreen600
    , smFromGreen700
    , smFromGreen800
    , smFromGreen900
    , smFromIndigo100
    , smFromIndigo200
    , smFromIndigo300
    , smFromIndigo400
    , smFromIndigo500
    , smFromIndigo600
    , smFromIndigo700
    , smFromIndigo800
    , smFromIndigo900
    , smFromOrange100
    , smFromOrange200
    , smFromOrange300
    , smFromOrange400
    , smFromOrange500
    , smFromOrange600
    , smFromOrange700
    , smFromOrange800
    , smFromOrange900
    , smFromPink100
    , smFromPink200
    , smFromPink300
    , smFromPink400
    , smFromPink500
    , smFromPink600
    , smFromPink700
    , smFromPink800
    , smFromPink900
    , smFromPurple100
    , smFromPurple200
    , smFromPurple300
    , smFromPurple400
    , smFromPurple500
    , smFromPurple600
    , smFromPurple700
    , smFromPurple800
    , smFromPurple900
    , smFromRed100
    , smFromRed200
    , smFromRed300
    , smFromRed400
    , smFromRed500
    , smFromRed600
    , smFromRed700
    , smFromRed800
    , smFromRed900
    , smFromTeal100
    , smFromTeal200
    , smFromTeal300
    , smFromTeal400
    , smFromTeal500
    , smFromTeal600
    , smFromTeal700
    , smFromTeal800
    , smFromTeal900
    , smFromTransparent
    , smFromWhite
    , smFromYellow100
    , smFromYellow200
    , smFromYellow300
    , smFromYellow400
    , smFromYellow500
    , smFromYellow600
    , smFromYellow700
    , smFromYellow800
    , smFromYellow900
    , smGap0
    , smGap1
    , smGap10
    , smGap12
    , smGap16
    , smGap2
    , smGap20
    , smGap24
    , smGap3
    , smGap32
    , smGap4
    , smGap40
    , smGap48
    , smGap5
    , smGap56
    , smGap6
    , smGap64
    , smGap8
    , smGapPx
    , smGapX0
    , smGapX1
    , smGapX10
    , smGapX12
    , smGapX16
    , smGapX2
    , smGapX20
    , smGapX24
    , smGapX3
    , smGapX32
    , smGapX4
    , smGapX40
    , smGapX48
    , smGapX5
    , smGapX56
    , smGapX6
    , smGapX64
    , smGapX8
    , smGapXPx
    , smGapY0
    , smGapY1
    , smGapY10
    , smGapY12
    , smGapY16
    , smGapY2
    , smGapY20
    , smGapY24
    , smGapY3
    , smGapY32
    , smGapY4
    , smGapY40
    , smGapY48
    , smGapY5
    , smGapY56
    , smGapY6
    , smGapY64
    , smGapY8
    , smGapYPx
    , smGrid
    , smGridCols1
    , smGridCols10
    , smGridCols11
    , smGridCols12
    , smGridCols2
    , smGridCols3
    , smGridCols4
    , smGridCols5
    , smGridCols6
    , smGridCols7
    , smGridCols8
    , smGridCols9
    , smGridColsNone
    , smGridFlowCol
    , smGridFlowColDense
    , smGridFlowRow
    , smGridFlowRowDense
    , smGridRows1
    , smGridRows2
    , smGridRows3
    , smGridRows4
    , smGridRows5
    , smGridRows6
    , smGridRowsNone
    , smH0
    , smH1
    , smH10
    , smH12
    , smH16
    , smH2
    , smH20
    , smH24
    , smH3
    , smH32
    , smH4
    , smH40
    , smH48
    , smH5
    , smH56
    , smH6
    , smH64
    , smH8
    , smHAuto
    , smHFull
    , smHPx
    , smHScreen
    , smHidden
    , smHoverBgBlack
    , smHoverBgBlue100
    , smHoverBgBlue200
    , smHoverBgBlue300
    , smHoverBgBlue400
    , smHoverBgBlue500
    , smHoverBgBlue600
    , smHoverBgBlue700
    , smHoverBgBlue800
    , smHoverBgBlue900
    , smHoverBgCurrent
    , smHoverBgGray100
    , smHoverBgGray200
    , smHoverBgGray300
    , smHoverBgGray400
    , smHoverBgGray500
    , smHoverBgGray600
    , smHoverBgGray700
    , smHoverBgGray800
    , smHoverBgGray900
    , smHoverBgGreen100
    , smHoverBgGreen200
    , smHoverBgGreen300
    , smHoverBgGreen400
    , smHoverBgGreen500
    , smHoverBgGreen600
    , smHoverBgGreen700
    , smHoverBgGreen800
    , smHoverBgGreen900
    , smHoverBgIndigo100
    , smHoverBgIndigo200
    , smHoverBgIndigo300
    , smHoverBgIndigo400
    , smHoverBgIndigo500
    , smHoverBgIndigo600
    , smHoverBgIndigo700
    , smHoverBgIndigo800
    , smHoverBgIndigo900
    , smHoverBgOpacity0
    , smHoverBgOpacity100
    , smHoverBgOpacity25
    , smHoverBgOpacity50
    , smHoverBgOpacity75
    , smHoverBgOrange100
    , smHoverBgOrange200
    , smHoverBgOrange300
    , smHoverBgOrange400
    , smHoverBgOrange500
    , smHoverBgOrange600
    , smHoverBgOrange700
    , smHoverBgOrange800
    , smHoverBgOrange900
    , smHoverBgPink100
    , smHoverBgPink200
    , smHoverBgPink300
    , smHoverBgPink400
    , smHoverBgPink500
    , smHoverBgPink600
    , smHoverBgPink700
    , smHoverBgPink800
    , smHoverBgPink900
    , smHoverBgPurple100
    , smHoverBgPurple200
    , smHoverBgPurple300
    , smHoverBgPurple400
    , smHoverBgPurple500
    , smHoverBgPurple600
    , smHoverBgPurple700
    , smHoverBgPurple800
    , smHoverBgPurple900
    , smHoverBgRed100
    , smHoverBgRed200
    , smHoverBgRed300
    , smHoverBgRed400
    , smHoverBgRed500
    , smHoverBgRed600
    , smHoverBgRed700
    , smHoverBgRed800
    , smHoverBgRed900
    , smHoverBgTeal100
    , smHoverBgTeal200
    , smHoverBgTeal300
    , smHoverBgTeal400
    , smHoverBgTeal500
    , smHoverBgTeal600
    , smHoverBgTeal700
    , smHoverBgTeal800
    , smHoverBgTeal900
    , smHoverBgTransparent
    , smHoverBgWhite
    , smHoverBgYellow100
    , smHoverBgYellow200
    , smHoverBgYellow300
    , smHoverBgYellow400
    , smHoverBgYellow500
    , smHoverBgYellow600
    , smHoverBgYellow700
    , smHoverBgYellow800
    , smHoverBgYellow900
    , smHoverBorderBlack
    , smHoverBorderBlue100
    , smHoverBorderBlue200
    , smHoverBorderBlue300
    , smHoverBorderBlue400
    , smHoverBorderBlue500
    , smHoverBorderBlue600
    , smHoverBorderBlue700
    , smHoverBorderBlue800
    , smHoverBorderBlue900
    , smHoverBorderCurrent
    , smHoverBorderGray100
    , smHoverBorderGray200
    , smHoverBorderGray300
    , smHoverBorderGray400
    , smHoverBorderGray500
    , smHoverBorderGray600
    , smHoverBorderGray700
    , smHoverBorderGray800
    , smHoverBorderGray900
    , smHoverBorderGreen100
    , smHoverBorderGreen200
    , smHoverBorderGreen300
    , smHoverBorderGreen400
    , smHoverBorderGreen500
    , smHoverBorderGreen600
    , smHoverBorderGreen700
    , smHoverBorderGreen800
    , smHoverBorderGreen900
    , smHoverBorderIndigo100
    , smHoverBorderIndigo200
    , smHoverBorderIndigo300
    , smHoverBorderIndigo400
    , smHoverBorderIndigo500
    , smHoverBorderIndigo600
    , smHoverBorderIndigo700
    , smHoverBorderIndigo800
    , smHoverBorderIndigo900
    , smHoverBorderOpacity0
    , smHoverBorderOpacity100
    , smHoverBorderOpacity25
    , smHoverBorderOpacity50
    , smHoverBorderOpacity75
    , smHoverBorderOrange100
    , smHoverBorderOrange200
    , smHoverBorderOrange300
    , smHoverBorderOrange400
    , smHoverBorderOrange500
    , smHoverBorderOrange600
    , smHoverBorderOrange700
    , smHoverBorderOrange800
    , smHoverBorderOrange900
    , smHoverBorderPink100
    , smHoverBorderPink200
    , smHoverBorderPink300
    , smHoverBorderPink400
    , smHoverBorderPink500
    , smHoverBorderPink600
    , smHoverBorderPink700
    , smHoverBorderPink800
    , smHoverBorderPink900
    , smHoverBorderPurple100
    , smHoverBorderPurple200
    , smHoverBorderPurple300
    , smHoverBorderPurple400
    , smHoverBorderPurple500
    , smHoverBorderPurple600
    , smHoverBorderPurple700
    , smHoverBorderPurple800
    , smHoverBorderPurple900
    , smHoverBorderRed100
    , smHoverBorderRed200
    , smHoverBorderRed300
    , smHoverBorderRed400
    , smHoverBorderRed500
    , smHoverBorderRed600
    , smHoverBorderRed700
    , smHoverBorderRed800
    , smHoverBorderRed900
    , smHoverBorderTeal100
    , smHoverBorderTeal200
    , smHoverBorderTeal300
    , smHoverBorderTeal400
    , smHoverBorderTeal500
    , smHoverBorderTeal600
    , smHoverBorderTeal700
    , smHoverBorderTeal800
    , smHoverBorderTeal900
    , smHoverBorderTransparent
    , smHoverBorderWhite
    , smHoverBorderYellow100
    , smHoverBorderYellow200
    , smHoverBorderYellow300
    , smHoverBorderYellow400
    , smHoverBorderYellow500
    , smHoverBorderYellow600
    , smHoverBorderYellow700
    , smHoverBorderYellow800
    , smHoverBorderYellow900
    , smHoverFontBlack
    , smHoverFontBold
    , smHoverFontExtrabold
    , smHoverFontHairline
    , smHoverFontLight
    , smHoverFontMedium
    , smHoverFontNormal
    , smHoverFontSemibold
    , smHoverFontThin
    , smHoverFromBlack
    , smHoverFromBlue100
    , smHoverFromBlue200
    , smHoverFromBlue300
    , smHoverFromBlue400
    , smHoverFromBlue500
    , smHoverFromBlue600
    , smHoverFromBlue700
    , smHoverFromBlue800
    , smHoverFromBlue900
    , smHoverFromCurrent
    , smHoverFromGray100
    , smHoverFromGray200
    , smHoverFromGray300
    , smHoverFromGray400
    , smHoverFromGray500
    , smHoverFromGray600
    , smHoverFromGray700
    , smHoverFromGray800
    , smHoverFromGray900
    , smHoverFromGreen100
    , smHoverFromGreen200
    , smHoverFromGreen300
    , smHoverFromGreen400
    , smHoverFromGreen500
    , smHoverFromGreen600
    , smHoverFromGreen700
    , smHoverFromGreen800
    , smHoverFromGreen900
    , smHoverFromIndigo100
    , smHoverFromIndigo200
    , smHoverFromIndigo300
    , smHoverFromIndigo400
    , smHoverFromIndigo500
    , smHoverFromIndigo600
    , smHoverFromIndigo700
    , smHoverFromIndigo800
    , smHoverFromIndigo900
    , smHoverFromOrange100
    , smHoverFromOrange200
    , smHoverFromOrange300
    , smHoverFromOrange400
    , smHoverFromOrange500
    , smHoverFromOrange600
    , smHoverFromOrange700
    , smHoverFromOrange800
    , smHoverFromOrange900
    , smHoverFromPink100
    , smHoverFromPink200
    , smHoverFromPink300
    , smHoverFromPink400
    , smHoverFromPink500
    , smHoverFromPink600
    , smHoverFromPink700
    , smHoverFromPink800
    , smHoverFromPink900
    , smHoverFromPurple100
    , smHoverFromPurple200
    , smHoverFromPurple300
    , smHoverFromPurple400
    , smHoverFromPurple500
    , smHoverFromPurple600
    , smHoverFromPurple700
    , smHoverFromPurple800
    , smHoverFromPurple900
    , smHoverFromRed100
    , smHoverFromRed200
    , smHoverFromRed300
    , smHoverFromRed400
    , smHoverFromRed500
    , smHoverFromRed600
    , smHoverFromRed700
    , smHoverFromRed800
    , smHoverFromRed900
    , smHoverFromTeal100
    , smHoverFromTeal200
    , smHoverFromTeal300
    , smHoverFromTeal400
    , smHoverFromTeal500
    , smHoverFromTeal600
    , smHoverFromTeal700
    , smHoverFromTeal800
    , smHoverFromTeal900
    , smHoverFromTransparent
    , smHoverFromWhite
    , smHoverFromYellow100
    , smHoverFromYellow200
    , smHoverFromYellow300
    , smHoverFromYellow400
    , smHoverFromYellow500
    , smHoverFromYellow600
    , smHoverFromYellow700
    , smHoverFromYellow800
    , smHoverFromYellow900
    , smHoverLineThrough
    , smHoverNegRotate180
    , smHoverNegRotate45
    , smHoverNegRotate90
    , smHoverNegSkewX12
    , smHoverNegSkewX3
    , smHoverNegSkewX6
    , smHoverNegSkewY12
    , smHoverNegSkewY3
    , smHoverNegSkewY6
    , smHoverNegTranslateX1
    , smHoverNegTranslateX10
    , smHoverNegTranslateX12
    , smHoverNegTranslateX16
    , smHoverNegTranslateX1over2
    , smHoverNegTranslateX2
    , smHoverNegTranslateX20
    , smHoverNegTranslateX24
    , smHoverNegTranslateX3
    , smHoverNegTranslateX32
    , smHoverNegTranslateX4
    , smHoverNegTranslateX40
    , smHoverNegTranslateX48
    , smHoverNegTranslateX5
    , smHoverNegTranslateX56
    , smHoverNegTranslateX6
    , smHoverNegTranslateX64
    , smHoverNegTranslateX8
    , smHoverNegTranslateXFull
    , smHoverNegTranslateXPx
    , smHoverNegTranslateY1
    , smHoverNegTranslateY10
    , smHoverNegTranslateY12
    , smHoverNegTranslateY16
    , smHoverNegTranslateY1over2
    , smHoverNegTranslateY2
    , smHoverNegTranslateY20
    , smHoverNegTranslateY24
    , smHoverNegTranslateY3
    , smHoverNegTranslateY32
    , smHoverNegTranslateY4
    , smHoverNegTranslateY40
    , smHoverNegTranslateY48
    , smHoverNegTranslateY5
    , smHoverNegTranslateY56
    , smHoverNegTranslateY6
    , smHoverNegTranslateY64
    , smHoverNegTranslateY8
    , smHoverNegTranslateYFull
    , smHoverNegTranslateYPx
    , smHoverNoUnderline
    , smHoverOpacity0
    , smHoverOpacity100
    , smHoverOpacity25
    , smHoverOpacity50
    , smHoverOpacity75
    , smHoverRotate0
    , smHoverRotate180
    , smHoverRotate45
    , smHoverRotate90
    , smHoverScale0
    , smHoverScale100
    , smHoverScale105
    , smHoverScale110
    , smHoverScale125
    , smHoverScale150
    , smHoverScale50
    , smHoverScale75
    , smHoverScale90
    , smHoverScale95
    , smHoverScaleX0
    , smHoverScaleX100
    , smHoverScaleX105
    , smHoverScaleX110
    , smHoverScaleX125
    , smHoverScaleX150
    , smHoverScaleX50
    , smHoverScaleX75
    , smHoverScaleX90
    , smHoverScaleX95
    , smHoverScaleY0
    , smHoverScaleY100
    , smHoverScaleY105
    , smHoverScaleY110
    , smHoverScaleY125
    , smHoverScaleY150
    , smHoverScaleY50
    , smHoverScaleY75
    , smHoverScaleY90
    , smHoverScaleY95
    , smHoverShadow
    , smHoverShadow2xl
    , smHoverShadowInner
    , smHoverShadowLg
    , smHoverShadowMd
    , smHoverShadowNone
    , smHoverShadowOutline
    , smHoverShadowSm
    , smHoverShadowXl
    , smHoverShadowXs
    , smHoverSkewX0
    , smHoverSkewX12
    , smHoverSkewX3
    , smHoverSkewX6
    , smHoverSkewY0
    , smHoverSkewY12
    , smHoverSkewY3
    , smHoverSkewY6
    , smHoverTextBlack
    , smHoverTextBlue100
    , smHoverTextBlue200
    , smHoverTextBlue300
    , smHoverTextBlue400
    , smHoverTextBlue500
    , smHoverTextBlue600
    , smHoverTextBlue700
    , smHoverTextBlue800
    , smHoverTextBlue900
    , smHoverTextCurrent
    , smHoverTextGray100
    , smHoverTextGray200
    , smHoverTextGray300
    , smHoverTextGray400
    , smHoverTextGray500
    , smHoverTextGray600
    , smHoverTextGray700
    , smHoverTextGray800
    , smHoverTextGray900
    , smHoverTextGreen100
    , smHoverTextGreen200
    , smHoverTextGreen300
    , smHoverTextGreen400
    , smHoverTextGreen500
    , smHoverTextGreen600
    , smHoverTextGreen700
    , smHoverTextGreen800
    , smHoverTextGreen900
    , smHoverTextIndigo100
    , smHoverTextIndigo200
    , smHoverTextIndigo300
    , smHoverTextIndigo400
    , smHoverTextIndigo500
    , smHoverTextIndigo600
    , smHoverTextIndigo700
    , smHoverTextIndigo800
    , smHoverTextIndigo900
    , smHoverTextOpacity0
    , smHoverTextOpacity100
    , smHoverTextOpacity25
    , smHoverTextOpacity50
    , smHoverTextOpacity75
    , smHoverTextOrange100
    , smHoverTextOrange200
    , smHoverTextOrange300
    , smHoverTextOrange400
    , smHoverTextOrange500
    , smHoverTextOrange600
    , smHoverTextOrange700
    , smHoverTextOrange800
    , smHoverTextOrange900
    , smHoverTextPink100
    , smHoverTextPink200
    , smHoverTextPink300
    , smHoverTextPink400
    , smHoverTextPink500
    , smHoverTextPink600
    , smHoverTextPink700
    , smHoverTextPink800
    , smHoverTextPink900
    , smHoverTextPurple100
    , smHoverTextPurple200
    , smHoverTextPurple300
    , smHoverTextPurple400
    , smHoverTextPurple500
    , smHoverTextPurple600
    , smHoverTextPurple700
    , smHoverTextPurple800
    , smHoverTextPurple900
    , smHoverTextRed100
    , smHoverTextRed200
    , smHoverTextRed300
    , smHoverTextRed400
    , smHoverTextRed500
    , smHoverTextRed600
    , smHoverTextRed700
    , smHoverTextRed800
    , smHoverTextRed900
    , smHoverTextTeal100
    , smHoverTextTeal200
    , smHoverTextTeal300
    , smHoverTextTeal400
    , smHoverTextTeal500
    , smHoverTextTeal600
    , smHoverTextTeal700
    , smHoverTextTeal800
    , smHoverTextTeal900
    , smHoverTextTransparent
    , smHoverTextWhite
    , smHoverTextYellow100
    , smHoverTextYellow200
    , smHoverTextYellow300
    , smHoverTextYellow400
    , smHoverTextYellow500
    , smHoverTextYellow600
    , smHoverTextYellow700
    , smHoverTextYellow800
    , smHoverTextYellow900
    , smHoverToBlack
    , smHoverToBlue100
    , smHoverToBlue200
    , smHoverToBlue300
    , smHoverToBlue400
    , smHoverToBlue500
    , smHoverToBlue600
    , smHoverToBlue700
    , smHoverToBlue800
    , smHoverToBlue900
    , smHoverToCurrent
    , smHoverToGray100
    , smHoverToGray200
    , smHoverToGray300
    , smHoverToGray400
    , smHoverToGray500
    , smHoverToGray600
    , smHoverToGray700
    , smHoverToGray800
    , smHoverToGray900
    , smHoverToGreen100
    , smHoverToGreen200
    , smHoverToGreen300
    , smHoverToGreen400
    , smHoverToGreen500
    , smHoverToGreen600
    , smHoverToGreen700
    , smHoverToGreen800
    , smHoverToGreen900
    , smHoverToIndigo100
    , smHoverToIndigo200
    , smHoverToIndigo300
    , smHoverToIndigo400
    , smHoverToIndigo500
    , smHoverToIndigo600
    , smHoverToIndigo700
    , smHoverToIndigo800
    , smHoverToIndigo900
    , smHoverToOrange100
    , smHoverToOrange200
    , smHoverToOrange300
    , smHoverToOrange400
    , smHoverToOrange500
    , smHoverToOrange600
    , smHoverToOrange700
    , smHoverToOrange800
    , smHoverToOrange900
    , smHoverToPink100
    , smHoverToPink200
    , smHoverToPink300
    , smHoverToPink400
    , smHoverToPink500
    , smHoverToPink600
    , smHoverToPink700
    , smHoverToPink800
    , smHoverToPink900
    , smHoverToPurple100
    , smHoverToPurple200
    , smHoverToPurple300
    , smHoverToPurple400
    , smHoverToPurple500
    , smHoverToPurple600
    , smHoverToPurple700
    , smHoverToPurple800
    , smHoverToPurple900
    , smHoverToRed100
    , smHoverToRed200
    , smHoverToRed300
    , smHoverToRed400
    , smHoverToRed500
    , smHoverToRed600
    , smHoverToRed700
    , smHoverToRed800
    , smHoverToRed900
    , smHoverToTeal100
    , smHoverToTeal200
    , smHoverToTeal300
    , smHoverToTeal400
    , smHoverToTeal500
    , smHoverToTeal600
    , smHoverToTeal700
    , smHoverToTeal800
    , smHoverToTeal900
    , smHoverToTransparent
    , smHoverToWhite
    , smHoverToYellow100
    , smHoverToYellow200
    , smHoverToYellow300
    , smHoverToYellow400
    , smHoverToYellow500
    , smHoverToYellow600
    , smHoverToYellow700
    , smHoverToYellow800
    , smHoverToYellow900
    , smHoverTranslateX0
    , smHoverTranslateX1
    , smHoverTranslateX10
    , smHoverTranslateX12
    , smHoverTranslateX16
    , smHoverTranslateX1over2
    , smHoverTranslateX2
    , smHoverTranslateX20
    , smHoverTranslateX24
    , smHoverTranslateX3
    , smHoverTranslateX32
    , smHoverTranslateX4
    , smHoverTranslateX40
    , smHoverTranslateX48
    , smHoverTranslateX5
    , smHoverTranslateX56
    , smHoverTranslateX6
    , smHoverTranslateX64
    , smHoverTranslateX8
    , smHoverTranslateXFull
    , smHoverTranslateXPx
    , smHoverTranslateY0
    , smHoverTranslateY1
    , smHoverTranslateY10
    , smHoverTranslateY12
    , smHoverTranslateY16
    , smHoverTranslateY1over2
    , smHoverTranslateY2
    , smHoverTranslateY20
    , smHoverTranslateY24
    , smHoverTranslateY3
    , smHoverTranslateY32
    , smHoverTranslateY4
    , smHoverTranslateY40
    , smHoverTranslateY48
    , smHoverTranslateY5
    , smHoverTranslateY56
    , smHoverTranslateY6
    , smHoverTranslateY64
    , smHoverTranslateY8
    , smHoverTranslateYFull
    , smHoverTranslateYPx
    , smHoverUnderline
    , smHoverViaBlack
    , smHoverViaBlue100
    , smHoverViaBlue200
    , smHoverViaBlue300
    , smHoverViaBlue400
    , smHoverViaBlue500
    , smHoverViaBlue600
    , smHoverViaBlue700
    , smHoverViaBlue800
    , smHoverViaBlue900
    , smHoverViaCurrent
    , smHoverViaGray100
    , smHoverViaGray200
    , smHoverViaGray300
    , smHoverViaGray400
    , smHoverViaGray500
    , smHoverViaGray600
    , smHoverViaGray700
    , smHoverViaGray800
    , smHoverViaGray900
    , smHoverViaGreen100
    , smHoverViaGreen200
    , smHoverViaGreen300
    , smHoverViaGreen400
    , smHoverViaGreen500
    , smHoverViaGreen600
    , smHoverViaGreen700
    , smHoverViaGreen800
    , smHoverViaGreen900
    , smHoverViaIndigo100
    , smHoverViaIndigo200
    , smHoverViaIndigo300
    , smHoverViaIndigo400
    , smHoverViaIndigo500
    , smHoverViaIndigo600
    , smHoverViaIndigo700
    , smHoverViaIndigo800
    , smHoverViaIndigo900
    , smHoverViaOrange100
    , smHoverViaOrange200
    , smHoverViaOrange300
    , smHoverViaOrange400
    , smHoverViaOrange500
    , smHoverViaOrange600
    , smHoverViaOrange700
    , smHoverViaOrange800
    , smHoverViaOrange900
    , smHoverViaPink100
    , smHoverViaPink200
    , smHoverViaPink300
    , smHoverViaPink400
    , smHoverViaPink500
    , smHoverViaPink600
    , smHoverViaPink700
    , smHoverViaPink800
    , smHoverViaPink900
    , smHoverViaPurple100
    , smHoverViaPurple200
    , smHoverViaPurple300
    , smHoverViaPurple400
    , smHoverViaPurple500
    , smHoverViaPurple600
    , smHoverViaPurple700
    , smHoverViaPurple800
    , smHoverViaPurple900
    , smHoverViaRed100
    , smHoverViaRed200
    , smHoverViaRed300
    , smHoverViaRed400
    , smHoverViaRed500
    , smHoverViaRed600
    , smHoverViaRed700
    , smHoverViaRed800
    , smHoverViaRed900
    , smHoverViaTeal100
    , smHoverViaTeal200
    , smHoverViaTeal300
    , smHoverViaTeal400
    , smHoverViaTeal500
    , smHoverViaTeal600
    , smHoverViaTeal700
    , smHoverViaTeal800
    , smHoverViaTeal900
    , smHoverViaTransparent
    , smHoverViaWhite
    , smHoverViaYellow100
    , smHoverViaYellow200
    , smHoverViaYellow300
    , smHoverViaYellow400
    , smHoverViaYellow500
    , smHoverViaYellow600
    , smHoverViaYellow700
    , smHoverViaYellow800
    , smHoverViaYellow900
    , smInline
    , smInlineBlock
    , smInlineFlex
    , smInlineGrid
    , smInset0
    , smInsetAuto
    , smInsetX0
    , smInsetXAuto
    , smInsetY0
    , smInsetYAuto
    , smInvisible
    , smItalic
    , smItemsBaseline
    , smItemsCenter
    , smItemsEnd
    , smItemsStart
    , smItemsStretch
    , smJustifyAround
    , smJustifyBetween
    , smJustifyCenter
    , smJustifyEnd
    , smJustifyEvenly
    , smJustifyItemsAuto
    , smJustifyItemsCenter
    , smJustifyItemsEnd
    , smJustifyItemsStart
    , smJustifyItemsStretch
    , smJustifySelfAuto
    , smJustifySelfCenter
    , smJustifySelfEnd
    , smJustifySelfStart
    , smJustifySelfStretch
    , smJustifyStart
    , smLeading10
    , smLeading3
    , smLeading4
    , smLeading5
    , smLeading6
    , smLeading7
    , smLeading8
    , smLeading9
    , smLeadingLoose
    , smLeadingNone
    , smLeadingNormal
    , smLeadingRelaxed
    , smLeadingSnug
    , smLeadingTight
    , smLeft0
    , smLeftAuto
    , smLineThrough
    , smLiningNums
    , smListDecimal
    , smListDisc
    , smListInside
    , smListNone
    , smListOutside
    , smLowercase
    , smM0
    , smM1
    , smM10
    , smM12
    , smM16
    , smM2
    , smM20
    , smM24
    , smM3
    , smM32
    , smM4
    , smM40
    , smM48
    , smM5
    , smM56
    , smM6
    , smM64
    , smM8
    , smMAuto
    , smMPx
    , smMaxHFull
    , smMaxHScreen
    , smMaxW2xl
    , smMaxW3xl
    , smMaxW4xl
    , smMaxW5xl
    , smMaxW6xl
    , smMaxWFull
    , smMaxWLg
    , smMaxWMd
    , smMaxWNone
    , smMaxWScreenLg
    , smMaxWScreenMd
    , smMaxWScreenSm
    , smMaxWScreenXl
    , smMaxWSm
    , smMaxWXl
    , smMaxWXs
    , smMb0
    , smMb1
    , smMb10
    , smMb12
    , smMb16
    , smMb2
    , smMb20
    , smMb24
    , smMb3
    , smMb32
    , smMb4
    , smMb40
    , smMb48
    , smMb5
    , smMb56
    , smMb6
    , smMb64
    , smMb8
    , smMbAuto
    , smMbPx
    , smMinH0
    , smMinHFull
    , smMinHScreen
    , smMinW0
    , smMinWFull
    , smMl0
    , smMl1
    , smMl10
    , smMl12
    , smMl16
    , smMl2
    , smMl20
    , smMl24
    , smMl3
    , smMl32
    , smMl4
    , smMl40
    , smMl48
    , smMl5
    , smMl56
    , smMl6
    , smMl64
    , smMl8
    , smMlAuto
    , smMlPx
    , smMr0
    , smMr1
    , smMr10
    , smMr12
    , smMr16
    , smMr2
    , smMr20
    , smMr24
    , smMr3
    , smMr32
    , smMr4
    , smMr40
    , smMr48
    , smMr5
    , smMr56
    , smMr6
    , smMr64
    , smMr8
    , smMrAuto
    , smMrPx
    , smMt0
    , smMt1
    , smMt10
    , smMt12
    , smMt16
    , smMt2
    , smMt20
    , smMt24
    , smMt3
    , smMt32
    , smMt4
    , smMt40
    , smMt48
    , smMt5
    , smMt56
    , smMt6
    , smMt64
    , smMt8
    , smMtAuto
    , smMtPx
    , smMx0
    , smMx1
    , smMx10
    , smMx12
    , smMx16
    , smMx2
    , smMx20
    , smMx24
    , smMx3
    , smMx32
    , smMx4
    , smMx40
    , smMx48
    , smMx5
    , smMx56
    , smMx6
    , smMx64
    , smMx8
    , smMxAuto
    , smMxPx
    , smMy0
    , smMy1
    , smMy10
    , smMy12
    , smMy16
    , smMy2
    , smMy20
    , smMy24
    , smMy3
    , smMy32
    , smMy4
    , smMy40
    , smMy48
    , smMy5
    , smMy56
    , smMy6
    , smMy64
    , smMy8
    , smMyAuto
    , smMyPx
    , smNegM1
    , smNegM10
    , smNegM12
    , smNegM16
    , smNegM2
    , smNegM20
    , smNegM24
    , smNegM3
    , smNegM32
    , smNegM4
    , smNegM40
    , smNegM48
    , smNegM5
    , smNegM56
    , smNegM6
    , smNegM64
    , smNegM8
    , smNegMPx
    , smNegMb1
    , smNegMb10
    , smNegMb12
    , smNegMb16
    , smNegMb2
    , smNegMb20
    , smNegMb24
    , smNegMb3
    , smNegMb32
    , smNegMb4
    , smNegMb40
    , smNegMb48
    , smNegMb5
    , smNegMb56
    , smNegMb6
    , smNegMb64
    , smNegMb8
    , smNegMbPx
    , smNegMl1
    , smNegMl10
    , smNegMl12
    , smNegMl16
    , smNegMl2
    , smNegMl20
    , smNegMl24
    , smNegMl3
    , smNegMl32
    , smNegMl4
    , smNegMl40
    , smNegMl48
    , smNegMl5
    , smNegMl56
    , smNegMl6
    , smNegMl64
    , smNegMl8
    , smNegMlPx
    , smNegMr1
    , smNegMr10
    , smNegMr12
    , smNegMr16
    , smNegMr2
    , smNegMr20
    , smNegMr24
    , smNegMr3
    , smNegMr32
    , smNegMr4
    , smNegMr40
    , smNegMr48
    , smNegMr5
    , smNegMr56
    , smNegMr6
    , smNegMr64
    , smNegMr8
    , smNegMrPx
    , smNegMt1
    , smNegMt10
    , smNegMt12
    , smNegMt16
    , smNegMt2
    , smNegMt20
    , smNegMt24
    , smNegMt3
    , smNegMt32
    , smNegMt4
    , smNegMt40
    , smNegMt48
    , smNegMt5
    , smNegMt56
    , smNegMt6
    , smNegMt64
    , smNegMt8
    , smNegMtPx
    , smNegMx1
    , smNegMx10
    , smNegMx12
    , smNegMx16
    , smNegMx2
    , smNegMx20
    , smNegMx24
    , smNegMx3
    , smNegMx32
    , smNegMx4
    , smNegMx40
    , smNegMx48
    , smNegMx5
    , smNegMx56
    , smNegMx6
    , smNegMx64
    , smNegMx8
    , smNegMxPx
    , smNegMy1
    , smNegMy10
    , smNegMy12
    , smNegMy16
    , smNegMy2
    , smNegMy20
    , smNegMy24
    , smNegMy3
    , smNegMy32
    , smNegMy4
    , smNegMy40
    , smNegMy48
    , smNegMy5
    , smNegMy56
    , smNegMy6
    , smNegMy64
    , smNegMy8
    , smNegMyPx
    , smNegRotate180
    , smNegRotate45
    , smNegRotate90
    , smNegSkewX12
    , smNegSkewX3
    , smNegSkewX6
    , smNegSkewY12
    , smNegSkewY3
    , smNegSkewY6
    , smNegSpaceX1
    , smNegSpaceX10
    , smNegSpaceX12
    , smNegSpaceX16
    , smNegSpaceX2
    , smNegSpaceX20
    , smNegSpaceX24
    , smNegSpaceX3
    , smNegSpaceX32
    , smNegSpaceX4
    , smNegSpaceX40
    , smNegSpaceX48
    , smNegSpaceX5
    , smNegSpaceX56
    , smNegSpaceX6
    , smNegSpaceX64
    , smNegSpaceX8
    , smNegSpaceXPx
    , smNegSpaceY1
    , smNegSpaceY10
    , smNegSpaceY12
    , smNegSpaceY16
    , smNegSpaceY2
    , smNegSpaceY20
    , smNegSpaceY24
    , smNegSpaceY3
    , smNegSpaceY32
    , smNegSpaceY4
    , smNegSpaceY40
    , smNegSpaceY48
    , smNegSpaceY5
    , smNegSpaceY56
    , smNegSpaceY6
    , smNegSpaceY64
    , smNegSpaceY8
    , smNegSpaceYPx
    , smNegTranslateX1
    , smNegTranslateX10
    , smNegTranslateX12
    , smNegTranslateX16
    , smNegTranslateX1over2
    , smNegTranslateX2
    , smNegTranslateX20
    , smNegTranslateX24
    , smNegTranslateX3
    , smNegTranslateX32
    , smNegTranslateX4
    , smNegTranslateX40
    , smNegTranslateX48
    , smNegTranslateX5
    , smNegTranslateX56
    , smNegTranslateX6
    , smNegTranslateX64
    , smNegTranslateX8
    , smNegTranslateXFull
    , smNegTranslateXPx
    , smNegTranslateY1
    , smNegTranslateY10
    , smNegTranslateY12
    , smNegTranslateY16
    , smNegTranslateY1over2
    , smNegTranslateY2
    , smNegTranslateY20
    , smNegTranslateY24
    , smNegTranslateY3
    , smNegTranslateY32
    , smNegTranslateY4
    , smNegTranslateY40
    , smNegTranslateY48
    , smNegTranslateY5
    , smNegTranslateY56
    , smNegTranslateY6
    , smNegTranslateY64
    , smNegTranslateY8
    , smNegTranslateYFull
    , smNegTranslateYPx
    , smNoUnderline
    , smNormalCase
    , smNormalNums
    , smNotItalic
    , smNotSrOnly
    , smObjectBottom
    , smObjectCenter
    , smObjectContain
    , smObjectCover
    , smObjectFill
    , smObjectLeft
    , smObjectLeftBottom
    , smObjectLeftTop
    , smObjectNone
    , smObjectRight
    , smObjectRightBottom
    , smObjectRightTop
    , smObjectScaleDown
    , smObjectTop
    , smOldstyleNums
    , smOpacity0
    , smOpacity100
    , smOpacity25
    , smOpacity50
    , smOpacity75
    , smOrder1
    , smOrder10
    , smOrder11
    , smOrder12
    , smOrder2
    , smOrder3
    , smOrder4
    , smOrder5
    , smOrder6
    , smOrder7
    , smOrder8
    , smOrder9
    , smOrderFirst
    , smOrderLast
    , smOrderNone
    , smOrdinal
    , smOriginBottom
    , smOriginBottomLeft
    , smOriginBottomRight
    , smOriginCenter
    , smOriginLeft
    , smOriginRight
    , smOriginTop
    , smOriginTopLeft
    , smOriginTopRight
    , smOutlineNone
    , smOverflowAuto
    , smOverflowHidden
    , smOverflowScroll
    , smOverflowVisible
    , smOverflowXAuto
    , smOverflowXHidden
    , smOverflowXScroll
    , smOverflowXVisible
    , smOverflowYAuto
    , smOverflowYHidden
    , smOverflowYScroll
    , smOverflowYVisible
    , smOverscrollAuto
    , smOverscrollContain
    , smOverscrollNone
    , smOverscrollXAuto
    , smOverscrollXContain
    , smOverscrollXNone
    , smOverscrollYAuto
    , smOverscrollYContain
    , smOverscrollYNone
    , smP0
    , smP1
    , smP10
    , smP12
    , smP16
    , smP2
    , smP20
    , smP24
    , smP3
    , smP32
    , smP4
    , smP40
    , smP48
    , smP5
    , smP56
    , smP6
    , smP64
    , smP8
    , smPPx
    , smPb0
    , smPb1
    , smPb10
    , smPb12
    , smPb16
    , smPb2
    , smPb20
    , smPb24
    , smPb3
    , smPb32
    , smPb4
    , smPb40
    , smPb48
    , smPb5
    , smPb56
    , smPb6
    , smPb64
    , smPb8
    , smPbPx
    , smPl0
    , smPl1
    , smPl10
    , smPl12
    , smPl16
    , smPl2
    , smPl20
    , smPl24
    , smPl3
    , smPl32
    , smPl4
    , smPl40
    , smPl48
    , smPl5
    , smPl56
    , smPl6
    , smPl64
    , smPl8
    , smPlPx
    , smPlaceContentAround
    , smPlaceContentBetween
    , smPlaceContentCenter
    , smPlaceContentEnd
    , smPlaceContentEvenly
    , smPlaceContentStart
    , smPlaceContentStretch
    , smPlaceItemsAuto
    , smPlaceItemsCenter
    , smPlaceItemsEnd
    , smPlaceItemsStart
    , smPlaceItemsStretch
    , smPlaceSelfAuto
    , smPlaceSelfCenter
    , smPlaceSelfEnd
    , smPlaceSelfStart
    , smPlaceSelfStretch
    , smPlaceholderBlack
    , smPlaceholderBlue100
    , smPlaceholderBlue200
    , smPlaceholderBlue300
    , smPlaceholderBlue400
    , smPlaceholderBlue500
    , smPlaceholderBlue600
    , smPlaceholderBlue700
    , smPlaceholderBlue800
    , smPlaceholderBlue900
    , smPlaceholderCurrent
    , smPlaceholderGray100
    , smPlaceholderGray200
    , smPlaceholderGray300
    , smPlaceholderGray400
    , smPlaceholderGray500
    , smPlaceholderGray600
    , smPlaceholderGray700
    , smPlaceholderGray800
    , smPlaceholderGray900
    , smPlaceholderGreen100
    , smPlaceholderGreen200
    , smPlaceholderGreen300
    , smPlaceholderGreen400
    , smPlaceholderGreen500
    , smPlaceholderGreen600
    , smPlaceholderGreen700
    , smPlaceholderGreen800
    , smPlaceholderGreen900
    , smPlaceholderIndigo100
    , smPlaceholderIndigo200
    , smPlaceholderIndigo300
    , smPlaceholderIndigo400
    , smPlaceholderIndigo500
    , smPlaceholderIndigo600
    , smPlaceholderIndigo700
    , smPlaceholderIndigo800
    , smPlaceholderIndigo900
    , smPlaceholderOpacity0
    , smPlaceholderOpacity100
    , smPlaceholderOpacity25
    , smPlaceholderOpacity50
    , smPlaceholderOpacity75
    , smPlaceholderOrange100
    , smPlaceholderOrange200
    , smPlaceholderOrange300
    , smPlaceholderOrange400
    , smPlaceholderOrange500
    , smPlaceholderOrange600
    , smPlaceholderOrange700
    , smPlaceholderOrange800
    , smPlaceholderOrange900
    , smPlaceholderPink100
    , smPlaceholderPink200
    , smPlaceholderPink300
    , smPlaceholderPink400
    , smPlaceholderPink500
    , smPlaceholderPink600
    , smPlaceholderPink700
    , smPlaceholderPink800
    , smPlaceholderPink900
    , smPlaceholderPurple100
    , smPlaceholderPurple200
    , smPlaceholderPurple300
    , smPlaceholderPurple400
    , smPlaceholderPurple500
    , smPlaceholderPurple600
    , smPlaceholderPurple700
    , smPlaceholderPurple800
    , smPlaceholderPurple900
    , smPlaceholderRed100
    , smPlaceholderRed200
    , smPlaceholderRed300
    , smPlaceholderRed400
    , smPlaceholderRed500
    , smPlaceholderRed600
    , smPlaceholderRed700
    , smPlaceholderRed800
    , smPlaceholderRed900
    , smPlaceholderTeal100
    , smPlaceholderTeal200
    , smPlaceholderTeal300
    , smPlaceholderTeal400
    , smPlaceholderTeal500
    , smPlaceholderTeal600
    , smPlaceholderTeal700
    , smPlaceholderTeal800
    , smPlaceholderTeal900
    , smPlaceholderTransparent
    , smPlaceholderWhite
    , smPlaceholderYellow100
    , smPlaceholderYellow200
    , smPlaceholderYellow300
    , smPlaceholderYellow400
    , smPlaceholderYellow500
    , smPlaceholderYellow600
    , smPlaceholderYellow700
    , smPlaceholderYellow800
    , smPlaceholderYellow900
    , smPointerEventsAuto
    , smPointerEventsNone
    , smPr0
    , smPr1
    , smPr10
    , smPr12
    , smPr16
    , smPr2
    , smPr20
    , smPr24
    , smPr3
    , smPr32
    , smPr4
    , smPr40
    , smPr48
    , smPr5
    , smPr56
    , smPr6
    , smPr64
    , smPr8
    , smPrPx
    , smProportionalNums
    , smPt0
    , smPt1
    , smPt10
    , smPt12
    , smPt16
    , smPt2
    , smPt20
    , smPt24
    , smPt3
    , smPt32
    , smPt4
    , smPt40
    , smPt48
    , smPt5
    , smPt56
    , smPt6
    , smPt64
    , smPt8
    , smPtPx
    , smPx0
    , smPx1
    , smPx10
    , smPx12
    , smPx16
    , smPx2
    , smPx20
    , smPx24
    , smPx3
    , smPx32
    , smPx4
    , smPx40
    , smPx48
    , smPx5
    , smPx56
    , smPx6
    , smPx64
    , smPx8
    , smPxPx
    , smPy0
    , smPy1
    , smPy10
    , smPy12
    , smPy16
    , smPy2
    , smPy20
    , smPy24
    , smPy3
    , smPy32
    , smPy4
    , smPy40
    , smPy48
    , smPy5
    , smPy56
    , smPy6
    , smPy64
    , smPy8
    , smPyPx
    , smRelative
    , smResize
    , smResizeNone
    , smResizeX
    , smResizeY
    , smRight0
    , smRightAuto
    , smRotate0
    , smRotate180
    , smRotate45
    , smRotate90
    , smRounded
    , smRoundedB
    , smRoundedBFull
    , smRoundedBLg
    , smRoundedBMd
    , smRoundedBNone
    , smRoundedBSm
    , smRoundedBl
    , smRoundedBlFull
    , smRoundedBlLg
    , smRoundedBlMd
    , smRoundedBlNone
    , smRoundedBlSm
    , smRoundedBr
    , smRoundedBrFull
    , smRoundedBrLg
    , smRoundedBrMd
    , smRoundedBrNone
    , smRoundedBrSm
    , smRoundedFull
    , smRoundedL
    , smRoundedLFull
    , smRoundedLLg
    , smRoundedLMd
    , smRoundedLNone
    , smRoundedLSm
    , smRoundedLg
    , smRoundedMd
    , smRoundedNone
    , smRoundedR
    , smRoundedRFull
    , smRoundedRLg
    , smRoundedRMd
    , smRoundedRNone
    , smRoundedRSm
    , smRoundedSm
    , smRoundedT
    , smRoundedTFull
    , smRoundedTLg
    , smRoundedTMd
    , smRoundedTNone
    , smRoundedTSm
    , smRoundedTl
    , smRoundedTlFull
    , smRoundedTlLg
    , smRoundedTlMd
    , smRoundedTlNone
    , smRoundedTlSm
    , smRoundedTr
    , smRoundedTrFull
    , smRoundedTrLg
    , smRoundedTrMd
    , smRoundedTrNone
    , smRoundedTrSm
    , smRowAuto
    , smRowEnd1
    , smRowEnd2
    , smRowEnd3
    , smRowEnd4
    , smRowEnd5
    , smRowEnd6
    , smRowEnd7
    , smRowEndAuto
    , smRowSpan1
    , smRowSpan2
    , smRowSpan3
    , smRowSpan4
    , smRowSpan5
    , smRowSpan6
    , smRowStart1
    , smRowStart2
    , smRowStart3
    , smRowStart4
    , smRowStart5
    , smRowStart6
    , smRowStart7
    , smRowStartAuto
    , smScale0
    , smScale100
    , smScale105
    , smScale110
    , smScale125
    , smScale150
    , smScale50
    , smScale75
    , smScale90
    , smScale95
    , smScaleX0
    , smScaleX100
    , smScaleX105
    , smScaleX110
    , smScaleX125
    , smScaleX150
    , smScaleX50
    , smScaleX75
    , smScaleX90
    , smScaleX95
    , smScaleY0
    , smScaleY100
    , smScaleY105
    , smScaleY110
    , smScaleY125
    , smScaleY150
    , smScaleY50
    , smScaleY75
    , smScaleY90
    , smScaleY95
    , smScrollingAuto
    , smScrollingTouch
    , smSelectAll
    , smSelectAuto
    , smSelectNone
    , smSelectText
    , smSelfAuto
    , smSelfCenter
    , smSelfEnd
    , smSelfStart
    , smSelfStretch
    , smShadow
    , smShadow2xl
    , smShadowInner
    , smShadowLg
    , smShadowMd
    , smShadowNone
    , smShadowOutline
    , smShadowSm
    , smShadowXl
    , smShadowXs
    , smSkewX0
    , smSkewX12
    , smSkewX3
    , smSkewX6
    , smSkewY0
    , smSkewY12
    , smSkewY3
    , smSkewY6
    , smSlashedZero
    , smSpaceX0
    , smSpaceX1
    , smSpaceX10
    , smSpaceX12
    , smSpaceX16
    , smSpaceX2
    , smSpaceX20
    , smSpaceX24
    , smSpaceX3
    , smSpaceX32
    , smSpaceX4
    , smSpaceX40
    , smSpaceX48
    , smSpaceX5
    , smSpaceX56
    , smSpaceX6
    , smSpaceX64
    , smSpaceX8
    , smSpaceXPx
    , smSpaceXReverse
    , smSpaceY0
    , smSpaceY1
    , smSpaceY10
    , smSpaceY12
    , smSpaceY16
    , smSpaceY2
    , smSpaceY20
    , smSpaceY24
    , smSpaceY3
    , smSpaceY32
    , smSpaceY4
    , smSpaceY40
    , smSpaceY48
    , smSpaceY5
    , smSpaceY56
    , smSpaceY6
    , smSpaceY64
    , smSpaceY8
    , smSpaceYPx
    , smSpaceYReverse
    , smSrOnly
    , smStackedFractions
    , smStatic
    , smSticky
    , smStroke0
    , smStroke1
    , smStroke2
    , smStrokeCurrent
    , smSubpixelAntialiased
    , smTable
    , smTableAuto
    , smTableCaption
    , smTableCell
    , smTableColumn
    , smTableColumnGroup
    , smTableFixed
    , smTableFooterGroup
    , smTableHeaderGroup
    , smTableRow
    , smTableRowGroup
    , smTabularNums
    , smText2xl
    , smText3xl
    , smText4xl
    , smText5xl
    , smText6xl
    , smTextBase
    , smTextBlack
    , smTextBlue100
    , smTextBlue200
    , smTextBlue300
    , smTextBlue400
    , smTextBlue500
    , smTextBlue600
    , smTextBlue700
    , smTextBlue800
    , smTextBlue900
    , smTextCenter
    , smTextCurrent
    , smTextGray100
    , smTextGray200
    , smTextGray300
    , smTextGray400
    , smTextGray500
    , smTextGray600
    , smTextGray700
    , smTextGray800
    , smTextGray900
    , smTextGreen100
    , smTextGreen200
    , smTextGreen300
    , smTextGreen400
    , smTextGreen500
    , smTextGreen600
    , smTextGreen700
    , smTextGreen800
    , smTextGreen900
    , smTextIndigo100
    , smTextIndigo200
    , smTextIndigo300
    , smTextIndigo400
    , smTextIndigo500
    , smTextIndigo600
    , smTextIndigo700
    , smTextIndigo800
    , smTextIndigo900
    , smTextJustify
    , smTextLeft
    , smTextLg
    , smTextOpacity0
    , smTextOpacity100
    , smTextOpacity25
    , smTextOpacity50
    , smTextOpacity75
    , smTextOrange100
    , smTextOrange200
    , smTextOrange300
    , smTextOrange400
    , smTextOrange500
    , smTextOrange600
    , smTextOrange700
    , smTextOrange800
    , smTextOrange900
    , smTextPink100
    , smTextPink200
    , smTextPink300
    , smTextPink400
    , smTextPink500
    , smTextPink600
    , smTextPink700
    , smTextPink800
    , smTextPink900
    , smTextPurple100
    , smTextPurple200
    , smTextPurple300
    , smTextPurple400
    , smTextPurple500
    , smTextPurple600
    , smTextPurple700
    , smTextPurple800
    , smTextPurple900
    , smTextRed100
    , smTextRed200
    , smTextRed300
    , smTextRed400
    , smTextRed500
    , smTextRed600
    , smTextRed700
    , smTextRed800
    , smTextRed900
    , smTextRight
    , smTextSm
    , smTextTeal100
    , smTextTeal200
    , smTextTeal300
    , smTextTeal400
    , smTextTeal500
    , smTextTeal600
    , smTextTeal700
    , smTextTeal800
    , smTextTeal900
    , smTextTransparent
    , smTextWhite
    , smTextXl
    , smTextXs
    , smTextYellow100
    , smTextYellow200
    , smTextYellow300
    , smTextYellow400
    , smTextYellow500
    , smTextYellow600
    , smTextYellow700
    , smTextYellow800
    , smTextYellow900
    , smToBlack
    , smToBlue100
    , smToBlue200
    , smToBlue300
    , smToBlue400
    , smToBlue500
    , smToBlue600
    , smToBlue700
    , smToBlue800
    , smToBlue900
    , smToCurrent
    , smToGray100
    , smToGray200
    , smToGray300
    , smToGray400
    , smToGray500
    , smToGray600
    , smToGray700
    , smToGray800
    , smToGray900
    , smToGreen100
    , smToGreen200
    , smToGreen300
    , smToGreen400
    , smToGreen500
    , smToGreen600
    , smToGreen700
    , smToGreen800
    , smToGreen900
    , smToIndigo100
    , smToIndigo200
    , smToIndigo300
    , smToIndigo400
    , smToIndigo500
    , smToIndigo600
    , smToIndigo700
    , smToIndigo800
    , smToIndigo900
    , smToOrange100
    , smToOrange200
    , smToOrange300
    , smToOrange400
    , smToOrange500
    , smToOrange600
    , smToOrange700
    , smToOrange800
    , smToOrange900
    , smToPink100
    , smToPink200
    , smToPink300
    , smToPink400
    , smToPink500
    , smToPink600
    , smToPink700
    , smToPink800
    , smToPink900
    , smToPurple100
    , smToPurple200
    , smToPurple300
    , smToPurple400
    , smToPurple500
    , smToPurple600
    , smToPurple700
    , smToPurple800
    , smToPurple900
    , smToRed100
    , smToRed200
    , smToRed300
    , smToRed400
    , smToRed500
    , smToRed600
    , smToRed700
    , smToRed800
    , smToRed900
    , smToTeal100
    , smToTeal200
    , smToTeal300
    , smToTeal400
    , smToTeal500
    , smToTeal600
    , smToTeal700
    , smToTeal800
    , smToTeal900
    , smToTransparent
    , smToWhite
    , smToYellow100
    , smToYellow200
    , smToYellow300
    , smToYellow400
    , smToYellow500
    , smToYellow600
    , smToYellow700
    , smToYellow800
    , smToYellow900
    , smTop0
    , smTopAuto
    , smTrackingNormal
    , smTrackingTight
    , smTrackingTighter
    , smTrackingWide
    , smTrackingWider
    , smTrackingWidest
    , smTransform
    , smTransformNone
    , smTransition
    , smTransitionAll
    , smTransitionColors
    , smTransitionNone
    , smTransitionOpacity
    , smTransitionShadow
    , smTransitionTransform
    , smTranslateX0
    , smTranslateX1
    , smTranslateX10
    , smTranslateX12
    , smTranslateX16
    , smTranslateX1over2
    , smTranslateX2
    , smTranslateX20
    , smTranslateX24
    , smTranslateX3
    , smTranslateX32
    , smTranslateX4
    , smTranslateX40
    , smTranslateX48
    , smTranslateX5
    , smTranslateX56
    , smTranslateX6
    , smTranslateX64
    , smTranslateX8
    , smTranslateXFull
    , smTranslateXPx
    , smTranslateY0
    , smTranslateY1
    , smTranslateY10
    , smTranslateY12
    , smTranslateY16
    , smTranslateY1over2
    , smTranslateY2
    , smTranslateY20
    , smTranslateY24
    , smTranslateY3
    , smTranslateY32
    , smTranslateY4
    , smTranslateY40
    , smTranslateY48
    , smTranslateY5
    , smTranslateY56
    , smTranslateY6
    , smTranslateY64
    , smTranslateY8
    , smTranslateYFull
    , smTranslateYPx
    , smTruncate
    , smUnderline
    , smUppercase
    , smViaBlack
    , smViaBlue100
    , smViaBlue200
    , smViaBlue300
    , smViaBlue400
    , smViaBlue500
    , smViaBlue600
    , smViaBlue700
    , smViaBlue800
    , smViaBlue900
    , smViaCurrent
    , smViaGray100
    , smViaGray200
    , smViaGray300
    , smViaGray400
    , smViaGray500
    , smViaGray600
    , smViaGray700
    , smViaGray800
    , smViaGray900
    , smViaGreen100
    , smViaGreen200
    , smViaGreen300
    , smViaGreen400
    , smViaGreen500
    , smViaGreen600
    , smViaGreen700
    , smViaGreen800
    , smViaGreen900
    , smViaIndigo100
    , smViaIndigo200
    , smViaIndigo300
    , smViaIndigo400
    , smViaIndigo500
    , smViaIndigo600
    , smViaIndigo700
    , smViaIndigo800
    , smViaIndigo900
    , smViaOrange100
    , smViaOrange200
    , smViaOrange300
    , smViaOrange400
    , smViaOrange500
    , smViaOrange600
    , smViaOrange700
    , smViaOrange800
    , smViaOrange900
    , smViaPink100
    , smViaPink200
    , smViaPink300
    , smViaPink400
    , smViaPink500
    , smViaPink600
    , smViaPink700
    , smViaPink800
    , smViaPink900
    , smViaPurple100
    , smViaPurple200
    , smViaPurple300
    , smViaPurple400
    , smViaPurple500
    , smViaPurple600
    , smViaPurple700
    , smViaPurple800
    , smViaPurple900
    , smViaRed100
    , smViaRed200
    , smViaRed300
    , smViaRed400
    , smViaRed500
    , smViaRed600
    , smViaRed700
    , smViaRed800
    , smViaRed900
    , smViaTeal100
    , smViaTeal200
    , smViaTeal300
    , smViaTeal400
    , smViaTeal500
    , smViaTeal600
    , smViaTeal700
    , smViaTeal800
    , smViaTeal900
    , smViaTransparent
    , smViaWhite
    , smViaYellow100
    , smViaYellow200
    , smViaYellow300
    , smViaYellow400
    , smViaYellow500
    , smViaYellow600
    , smViaYellow700
    , smViaYellow800
    , smViaYellow900
    , smVisible
    , smW0
    , smW1
    , smW10
    , smW10over12
    , smW11over12
    , smW12
    , smW16
    , smW1over12
    , smW1over2
    , smW1over3
    , smW1over4
    , smW1over5
    , smW1over6
    , smW2
    , smW20
    , smW24
    , smW2over12
    , smW2over3
    , smW2over4
    , smW2over5
    , smW2over6
    , smW3
    , smW32
    , smW3over12
    , smW3over4
    , smW3over5
    , smW3over6
    , smW4
    , smW40
    , smW48
    , smW4over12
    , smW4over5
    , smW4over6
    , smW5
    , smW56
    , smW5over12
    , smW5over6
    , smW6
    , smW64
    , smW6over12
    , smW7over12
    , smW8
    , smW8over12
    , smW9over12
    , smWAuto
    , smWFull
    , smWPx
    , smWScreen
    , smWhitespaceNoWrap
    , smWhitespaceNormal
    , smWhitespacePre
    , smWhitespacePreLine
    , smWhitespacePreWrap
    , smZ0
    , smZ10
    , smZ20
    , smZ30
    , smZ40
    , smZ50
    , smZAuto
    , spaceX0
    , spaceX1
    , spaceX10
    , spaceX12
    , spaceX16
    , spaceX2
    , spaceX20
    , spaceX24
    , spaceX3
    , spaceX32
    , spaceX4
    , spaceX40
    , spaceX48
    , spaceX5
    , spaceX56
    , spaceX6
    , spaceX64
    , spaceX8
    , spaceXPx
    , spaceXReverse
    , spaceY0
    , spaceY1
    , spaceY10
    , spaceY12
    , spaceY16
    , spaceY2
    , spaceY20
    , spaceY24
    , spaceY3
    , spaceY32
    , spaceY4
    , spaceY40
    , spaceY48
    , spaceY5
    , spaceY56
    , spaceY6
    , spaceY64
    , spaceY8
    , spaceYPx
    , spaceYReverse
    , srOnly
    , stackedFractions
    , static
    , sticky
    , stroke0
    , stroke1
    , stroke2
    , strokeCurrent
    , subpixelAntialiased
    , table
    , tableAuto
    , tableCaption
    , tableCell
    , tableColumn
    , tableColumnGroup
    , tableFixed
    , tableFooterGroup
    , tableHeaderGroup
    , tableRow
    , tableRowGroup
    , tabularNums
    , text2xl
    , text3xl
    , text4xl
    , text5xl
    , text6xl
    , textBase
    , textBlack
    , textBlue100
    , textBlue200
    , textBlue300
    , textBlue400
    , textBlue500
    , textBlue600
    , textBlue700
    , textBlue800
    , textBlue900
    , textCenter
    , textCurrent
    , textGray100
    , textGray200
    , textGray300
    , textGray400
    , textGray500
    , textGray600
    , textGray700
    , textGray800
    , textGray900
    , textGreen100
    , textGreen200
    , textGreen300
    , textGreen400
    , textGreen500
    , textGreen600
    , textGreen700
    , textGreen800
    , textGreen900
    , textIndigo100
    , textIndigo200
    , textIndigo300
    , textIndigo400
    , textIndigo500
    , textIndigo600
    , textIndigo700
    , textIndigo800
    , textIndigo900
    , textJustify
    , textLeft
    , textLg
    , textOpacity0
    , textOpacity100
    , textOpacity25
    , textOpacity50
    , textOpacity75
    , textOrange100
    , textOrange200
    , textOrange300
    , textOrange400
    , textOrange500
    , textOrange600
    , textOrange700
    , textOrange800
    , textOrange900
    , textPink100
    , textPink200
    , textPink300
    , textPink400
    , textPink500
    , textPink600
    , textPink700
    , textPink800
    , textPink900
    , textPurple100
    , textPurple200
    , textPurple300
    , textPurple400
    , textPurple500
    , textPurple600
    , textPurple700
    , textPurple800
    , textPurple900
    , textRed100
    , textRed200
    , textRed300
    , textRed400
    , textRed500
    , textRed600
    , textRed700
    , textRed800
    , textRed900
    , textRight
    , textSm
    , textTeal100
    , textTeal200
    , textTeal300
    , textTeal400
    , textTeal500
    , textTeal600
    , textTeal700
    , textTeal800
    , textTeal900
    , textTransparent
    , textWhite
    , textXl
    , textXs
    , textYellow100
    , textYellow200
    , textYellow300
    , textYellow400
    , textYellow500
    , textYellow600
    , textYellow700
    , textYellow800
    , textYellow900
    , toBlack
    , toBlue100
    , toBlue200
    , toBlue300
    , toBlue400
    , toBlue500
    , toBlue600
    , toBlue700
    , toBlue800
    , toBlue900
    , toCurrent
    , toGray100
    , toGray200
    , toGray300
    , toGray400
    , toGray500
    , toGray600
    , toGray700
    , toGray800
    , toGray900
    , toGreen100
    , toGreen200
    , toGreen300
    , toGreen400
    , toGreen500
    , toGreen600
    , toGreen700
    , toGreen800
    , toGreen900
    , toIndigo100
    , toIndigo200
    , toIndigo300
    , toIndigo400
    , toIndigo500
    , toIndigo600
    , toIndigo700
    , toIndigo800
    , toIndigo900
    , toOrange100
    , toOrange200
    , toOrange300
    , toOrange400
    , toOrange500
    , toOrange600
    , toOrange700
    , toOrange800
    , toOrange900
    , toPink100
    , toPink200
    , toPink300
    , toPink400
    , toPink500
    , toPink600
    , toPink700
    , toPink800
    , toPink900
    , toPurple100
    , toPurple200
    , toPurple300
    , toPurple400
    , toPurple500
    , toPurple600
    , toPurple700
    , toPurple800
    , toPurple900
    , toRed100
    , toRed200
    , toRed300
    , toRed400
    , toRed500
    , toRed600
    , toRed700
    , toRed800
    , toRed900
    , toTeal100
    , toTeal200
    , toTeal300
    , toTeal400
    , toTeal500
    , toTeal600
    , toTeal700
    , toTeal800
    , toTeal900
    , toTransparent
    , toWhite
    , toYellow100
    , toYellow200
    , toYellow300
    , toYellow400
    , toYellow500
    , toYellow600
    , toYellow700
    , toYellow800
    , toYellow900
    , top0
    , topAuto
    , trackingNormal
    , trackingTight
    , trackingTighter
    , trackingWide
    , trackingWider
    , trackingWidest
    , transform
    , transformNone
    , transition
    , transitionAll
    , transitionColors
    , transitionNone
    , transitionOpacity
    , transitionShadow
    , transitionTransform
    , translateX0
    , translateX1
    , translateX10
    , translateX12
    , translateX16
    , translateX1over2
    , translateX2
    , translateX20
    , translateX24
    , translateX3
    , translateX32
    , translateX4
    , translateX40
    , translateX48
    , translateX5
    , translateX56
    , translateX6
    , translateX64
    , translateX8
    , translateXFull
    , translateXPx
    , translateY0
    , translateY1
    , translateY10
    , translateY12
    , translateY16
    , translateY1over2
    , translateY2
    , translateY20
    , translateY24
    , translateY3
    , translateY32
    , translateY4
    , translateY40
    , translateY48
    , translateY5
    , translateY56
    , translateY6
    , translateY64
    , translateY8
    , translateYFull
    , translateYPx
    , truncate
    , underline
    , uppercase
    , viaBlack
    , viaBlue100
    , viaBlue200
    , viaBlue300
    , viaBlue400
    , viaBlue500
    , viaBlue600
    , viaBlue700
    , viaBlue800
    , viaBlue900
    , viaCurrent
    , viaGray100
    , viaGray200
    , viaGray300
    , viaGray400
    , viaGray500
    , viaGray600
    , viaGray700
    , viaGray800
    , viaGray900
    , viaGreen100
    , viaGreen200
    , viaGreen300
    , viaGreen400
    , viaGreen500
    , viaGreen600
    , viaGreen700
    , viaGreen800
    , viaGreen900
    , viaIndigo100
    , viaIndigo200
    , viaIndigo300
    , viaIndigo400
    , viaIndigo500
    , viaIndigo600
    , viaIndigo700
    , viaIndigo800
    , viaIndigo900
    , viaOrange100
    , viaOrange200
    , viaOrange300
    , viaOrange400
    , viaOrange500
    , viaOrange600
    , viaOrange700
    , viaOrange800
    , viaOrange900
    , viaPink100
    , viaPink200
    , viaPink300
    , viaPink400
    , viaPink500
    , viaPink600
    , viaPink700
    , viaPink800
    , viaPink900
    , viaPurple100
    , viaPurple200
    , viaPurple300
    , viaPurple400
    , viaPurple500
    , viaPurple600
    , viaPurple700
    , viaPurple800
    , viaPurple900
    , viaRed100
    , viaRed200
    , viaRed300
    , viaRed400
    , viaRed500
    , viaRed600
    , viaRed700
    , viaRed800
    , viaRed900
    , viaTeal100
    , viaTeal200
    , viaTeal300
    , viaTeal400
    , viaTeal500
    , viaTeal600
    , viaTeal700
    , viaTeal800
    , viaTeal900
    , viaTransparent
    , viaWhite
    , viaYellow100
    , viaYellow200
    , viaYellow300
    , viaYellow400
    , viaYellow500
    , viaYellow600
    , viaYellow700
    , viaYellow800
    , viaYellow900
    , visible
    , w0
    , w1
    , w10
    , w10over12
    , w11over12
    , w12
    , w16
    , w1over12
    , w1over2
    , w1over3
    , w1over4
    , w1over5
    , w1over6
    , w2
    , w20
    , w24
    , w2over12
    , w2over3
    , w2over4
    , w2over5
    , w2over6
    , w3
    , w32
    , w3over12
    , w3over4
    , w3over5
    , w3over6
    , w4
    , w40
    , w48
    , w4over12
    , w4over5
    , w4over6
    , w5
    , w56
    , w5over12
    , w5over6
    , w6
    , w64
    , w6over12
    , w7over12
    , w8
    , w8over12
    , w9over12
    , wAuto
    , wFull
    , wPx
    , wScreen
    , whitespaceNoWrap
    , whitespaceNormal
    , whitespacePre
    , whitespacePreLine
    , whitespacePreWrap
    , xlAbsolute
    , xlAlignBaseline
    , xlAlignBottom
    , xlAlignMiddle
    , xlAlignTextBottom
    , xlAlignTextTop
    , xlAlignTop
    , xlAnimateBounce
    , xlAnimateNone
    , xlAnimatePing
    , xlAnimatePulse
    , xlAnimateSpin
    , xlAntialiased
    , xlAppearanceNone
    , xlBgAuto
    , xlBgBlack
    , xlBgBlue100
    , xlBgBlue200
    , xlBgBlue300
    , xlBgBlue400
    , xlBgBlue500
    , xlBgBlue600
    , xlBgBlue700
    , xlBgBlue800
    , xlBgBlue900
    , xlBgBottom
    , xlBgCenter
    , xlBgClipBorder
    , xlBgClipContent
    , xlBgClipPadding
    , xlBgClipText
    , xlBgContain
    , xlBgCover
    , xlBgCurrent
    , xlBgFixed
    , xlBgGradientToB
    , xlBgGradientToBl
    , xlBgGradientToBr
    , xlBgGradientToL
    , xlBgGradientToR
    , xlBgGradientToT
    , xlBgGradientToTl
    , xlBgGradientToTr
    , xlBgGray100
    , xlBgGray200
    , xlBgGray300
    , xlBgGray400
    , xlBgGray500
    , xlBgGray600
    , xlBgGray700
    , xlBgGray800
    , xlBgGray900
    , xlBgGreen100
    , xlBgGreen200
    , xlBgGreen300
    , xlBgGreen400
    , xlBgGreen500
    , xlBgGreen600
    , xlBgGreen700
    , xlBgGreen800
    , xlBgGreen900
    , xlBgIndigo100
    , xlBgIndigo200
    , xlBgIndigo300
    , xlBgIndigo400
    , xlBgIndigo500
    , xlBgIndigo600
    , xlBgIndigo700
    , xlBgIndigo800
    , xlBgIndigo900
    , xlBgLeft
    , xlBgLeftBottom
    , xlBgLeftTop
    , xlBgLocal
    , xlBgNoRepeat
    , xlBgNone
    , xlBgOpacity0
    , xlBgOpacity100
    , xlBgOpacity25
    , xlBgOpacity50
    , xlBgOpacity75
    , xlBgOrange100
    , xlBgOrange200
    , xlBgOrange300
    , xlBgOrange400
    , xlBgOrange500
    , xlBgOrange600
    , xlBgOrange700
    , xlBgOrange800
    , xlBgOrange900
    , xlBgPink100
    , xlBgPink200
    , xlBgPink300
    , xlBgPink400
    , xlBgPink500
    , xlBgPink600
    , xlBgPink700
    , xlBgPink800
    , xlBgPink900
    , xlBgPurple100
    , xlBgPurple200
    , xlBgPurple300
    , xlBgPurple400
    , xlBgPurple500
    , xlBgPurple600
    , xlBgPurple700
    , xlBgPurple800
    , xlBgPurple900
    , xlBgRed100
    , xlBgRed200
    , xlBgRed300
    , xlBgRed400
    , xlBgRed500
    , xlBgRed600
    , xlBgRed700
    , xlBgRed800
    , xlBgRed900
    , xlBgRepeat
    , xlBgRepeatRound
    , xlBgRepeatSpace
    , xlBgRepeatX
    , xlBgRepeatY
    , xlBgRight
    , xlBgRightBottom
    , xlBgRightTop
    , xlBgScroll
    , xlBgTeal100
    , xlBgTeal200
    , xlBgTeal300
    , xlBgTeal400
    , xlBgTeal500
    , xlBgTeal600
    , xlBgTeal700
    , xlBgTeal800
    , xlBgTeal900
    , xlBgTop
    , xlBgTransparent
    , xlBgWhite
    , xlBgYellow100
    , xlBgYellow200
    , xlBgYellow300
    , xlBgYellow400
    , xlBgYellow500
    , xlBgYellow600
    , xlBgYellow700
    , xlBgYellow800
    , xlBgYellow900
    , xlBlock
    , xlBorder
    , xlBorder0
    , xlBorder2
    , xlBorder4
    , xlBorder8
    , xlBorderB
    , xlBorderB0
    , xlBorderB2
    , xlBorderB4
    , xlBorderB8
    , xlBorderBlack
    , xlBorderBlue100
    , xlBorderBlue200
    , xlBorderBlue300
    , xlBorderBlue400
    , xlBorderBlue500
    , xlBorderBlue600
    , xlBorderBlue700
    , xlBorderBlue800
    , xlBorderBlue900
    , xlBorderCollapse
    , xlBorderCurrent
    , xlBorderDashed
    , xlBorderDotted
    , xlBorderDouble
    , xlBorderGray100
    , xlBorderGray200
    , xlBorderGray300
    , xlBorderGray400
    , xlBorderGray500
    , xlBorderGray600
    , xlBorderGray700
    , xlBorderGray800
    , xlBorderGray900
    , xlBorderGreen100
    , xlBorderGreen200
    , xlBorderGreen300
    , xlBorderGreen400
    , xlBorderGreen500
    , xlBorderGreen600
    , xlBorderGreen700
    , xlBorderGreen800
    , xlBorderGreen900
    , xlBorderIndigo100
    , xlBorderIndigo200
    , xlBorderIndigo300
    , xlBorderIndigo400
    , xlBorderIndigo500
    , xlBorderIndigo600
    , xlBorderIndigo700
    , xlBorderIndigo800
    , xlBorderIndigo900
    , xlBorderL
    , xlBorderL0
    , xlBorderL2
    , xlBorderL4
    , xlBorderL8
    , xlBorderNone
    , xlBorderOpacity0
    , xlBorderOpacity100
    , xlBorderOpacity25
    , xlBorderOpacity50
    , xlBorderOpacity75
    , xlBorderOrange100
    , xlBorderOrange200
    , xlBorderOrange300
    , xlBorderOrange400
    , xlBorderOrange500
    , xlBorderOrange600
    , xlBorderOrange700
    , xlBorderOrange800
    , xlBorderOrange900
    , xlBorderPink100
    , xlBorderPink200
    , xlBorderPink300
    , xlBorderPink400
    , xlBorderPink500
    , xlBorderPink600
    , xlBorderPink700
    , xlBorderPink800
    , xlBorderPink900
    , xlBorderPurple100
    , xlBorderPurple200
    , xlBorderPurple300
    , xlBorderPurple400
    , xlBorderPurple500
    , xlBorderPurple600
    , xlBorderPurple700
    , xlBorderPurple800
    , xlBorderPurple900
    , xlBorderR
    , xlBorderR0
    , xlBorderR2
    , xlBorderR4
    , xlBorderR8
    , xlBorderRed100
    , xlBorderRed200
    , xlBorderRed300
    , xlBorderRed400
    , xlBorderRed500
    , xlBorderRed600
    , xlBorderRed700
    , xlBorderRed800
    , xlBorderRed900
    , xlBorderSeparate
    , xlBorderSolid
    , xlBorderT
    , xlBorderT0
    , xlBorderT2
    , xlBorderT4
    , xlBorderT8
    , xlBorderTeal100
    , xlBorderTeal200
    , xlBorderTeal300
    , xlBorderTeal400
    , xlBorderTeal500
    , xlBorderTeal600
    , xlBorderTeal700
    , xlBorderTeal800
    , xlBorderTeal900
    , xlBorderTransparent
    , xlBorderWhite
    , xlBorderYellow100
    , xlBorderYellow200
    , xlBorderYellow300
    , xlBorderYellow400
    , xlBorderYellow500
    , xlBorderYellow600
    , xlBorderYellow700
    , xlBorderYellow800
    , xlBorderYellow900
    , xlBottom0
    , xlBottomAuto
    , xlBoxBorder
    , xlBoxContent
    , xlBreakAll
    , xlBreakNormal
    , xlBreakWords
    , xlCapitalize
    , xlClearBoth
    , xlClearLeft
    , xlClearNone
    , xlClearRight
    , xlClearfixAfter
    , xlColAuto
    , xlColEnd1
    , xlColEnd10
    , xlColEnd11
    , xlColEnd12
    , xlColEnd13
    , xlColEnd2
    , xlColEnd3
    , xlColEnd4
    , xlColEnd5
    , xlColEnd6
    , xlColEnd7
    , xlColEnd8
    , xlColEnd9
    , xlColEndAuto
    , xlColSpan1
    , xlColSpan10
    , xlColSpan11
    , xlColSpan12
    , xlColSpan2
    , xlColSpan3
    , xlColSpan4
    , xlColSpan5
    , xlColSpan6
    , xlColSpan7
    , xlColSpan8
    , xlColSpan9
    , xlColStart1
    , xlColStart10
    , xlColStart11
    , xlColStart12
    , xlColStart13
    , xlColStart2
    , xlColStart3
    , xlColStart4
    , xlColStart5
    , xlColStart6
    , xlColStart7
    , xlColStart8
    , xlColStart9
    , xlColStartAuto
    , xlContainer
    , xlContentAround
    , xlContentBetween
    , xlContentCenter
    , xlContentEnd
    , xlContentEvenly
    , xlContentStart
    , xlContents
    , xlCursorAuto
    , xlCursorDefault
    , xlCursorMove
    , xlCursorNotAllowed
    , xlCursorPointer
    , xlCursorText
    , xlCursorWait
    , xlDelay100
    , xlDelay1000
    , xlDelay150
    , xlDelay200
    , xlDelay300
    , xlDelay500
    , xlDelay700
    , xlDelay75
    , xlDiagonalFractions
    , xlDivideBlack
    , xlDivideBlue100
    , xlDivideBlue200
    , xlDivideBlue300
    , xlDivideBlue400
    , xlDivideBlue500
    , xlDivideBlue600
    , xlDivideBlue700
    , xlDivideBlue800
    , xlDivideBlue900
    , xlDivideCurrent
    , xlDivideDashed
    , xlDivideDotted
    , xlDivideDouble
    , xlDivideGray100
    , xlDivideGray200
    , xlDivideGray300
    , xlDivideGray400
    , xlDivideGray500
    , xlDivideGray600
    , xlDivideGray700
    , xlDivideGray800
    , xlDivideGray900
    , xlDivideGreen100
    , xlDivideGreen200
    , xlDivideGreen300
    , xlDivideGreen400
    , xlDivideGreen500
    , xlDivideGreen600
    , xlDivideGreen700
    , xlDivideGreen800
    , xlDivideGreen900
    , xlDivideIndigo100
    , xlDivideIndigo200
    , xlDivideIndigo300
    , xlDivideIndigo400
    , xlDivideIndigo500
    , xlDivideIndigo600
    , xlDivideIndigo700
    , xlDivideIndigo800
    , xlDivideIndigo900
    , xlDivideNone
    , xlDivideOpacity0
    , xlDivideOpacity100
    , xlDivideOpacity25
    , xlDivideOpacity50
    , xlDivideOpacity75
    , xlDivideOrange100
    , xlDivideOrange200
    , xlDivideOrange300
    , xlDivideOrange400
    , xlDivideOrange500
    , xlDivideOrange600
    , xlDivideOrange700
    , xlDivideOrange800
    , xlDivideOrange900
    , xlDividePink100
    , xlDividePink200
    , xlDividePink300
    , xlDividePink400
    , xlDividePink500
    , xlDividePink600
    , xlDividePink700
    , xlDividePink800
    , xlDividePink900
    , xlDividePurple100
    , xlDividePurple200
    , xlDividePurple300
    , xlDividePurple400
    , xlDividePurple500
    , xlDividePurple600
    , xlDividePurple700
    , xlDividePurple800
    , xlDividePurple900
    , xlDivideRed100
    , xlDivideRed200
    , xlDivideRed300
    , xlDivideRed400
    , xlDivideRed500
    , xlDivideRed600
    , xlDivideRed700
    , xlDivideRed800
    , xlDivideRed900
    , xlDivideSolid
    , xlDivideTeal100
    , xlDivideTeal200
    , xlDivideTeal300
    , xlDivideTeal400
    , xlDivideTeal500
    , xlDivideTeal600
    , xlDivideTeal700
    , xlDivideTeal800
    , xlDivideTeal900
    , xlDivideTransparent
    , xlDivideWhite
    , xlDivideX
    , xlDivideX0
    , xlDivideX2
    , xlDivideX4
    , xlDivideX8
    , xlDivideXReverse
    , xlDivideY
    , xlDivideY0
    , xlDivideY2
    , xlDivideY4
    , xlDivideY8
    , xlDivideYReverse
    , xlDivideYellow100
    , xlDivideYellow200
    , xlDivideYellow300
    , xlDivideYellow400
    , xlDivideYellow500
    , xlDivideYellow600
    , xlDivideYellow700
    , xlDivideYellow800
    , xlDivideYellow900
    , xlDuration100
    , xlDuration1000
    , xlDuration150
    , xlDuration200
    , xlDuration300
    , xlDuration500
    , xlDuration700
    , xlDuration75
    , xlEaseIn
    , xlEaseInOut
    , xlEaseLinear
    , xlEaseOut
    , xlFillCurrent
    , xlFixed
    , xlFlex
    , xlFlex1
    , xlFlexAuto
    , xlFlexCol
    , xlFlexColReverse
    , xlFlexGrow
    , xlFlexGrow0
    , xlFlexInitial
    , xlFlexNoWrap
    , xlFlexNone
    , xlFlexRow
    , xlFlexRowReverse
    , xlFlexShrink
    , xlFlexShrink0
    , xlFlexWrap
    , xlFlexWrapReverse
    , xlFloatLeft
    , xlFloatNone
    , xlFloatRight
    , xlFlowRoot
    , xlFocusBgBlack
    , xlFocusBgBlue100
    , xlFocusBgBlue200
    , xlFocusBgBlue300
    , xlFocusBgBlue400
    , xlFocusBgBlue500
    , xlFocusBgBlue600
    , xlFocusBgBlue700
    , xlFocusBgBlue800
    , xlFocusBgBlue900
    , xlFocusBgCurrent
    , xlFocusBgGray100
    , xlFocusBgGray200
    , xlFocusBgGray300
    , xlFocusBgGray400
    , xlFocusBgGray500
    , xlFocusBgGray600
    , xlFocusBgGray700
    , xlFocusBgGray800
    , xlFocusBgGray900
    , xlFocusBgGreen100
    , xlFocusBgGreen200
    , xlFocusBgGreen300
    , xlFocusBgGreen400
    , xlFocusBgGreen500
    , xlFocusBgGreen600
    , xlFocusBgGreen700
    , xlFocusBgGreen800
    , xlFocusBgGreen900
    , xlFocusBgIndigo100
    , xlFocusBgIndigo200
    , xlFocusBgIndigo300
    , xlFocusBgIndigo400
    , xlFocusBgIndigo500
    , xlFocusBgIndigo600
    , xlFocusBgIndigo700
    , xlFocusBgIndigo800
    , xlFocusBgIndigo900
    , xlFocusBgOpacity0
    , xlFocusBgOpacity100
    , xlFocusBgOpacity25
    , xlFocusBgOpacity50
    , xlFocusBgOpacity75
    , xlFocusBgOrange100
    , xlFocusBgOrange200
    , xlFocusBgOrange300
    , xlFocusBgOrange400
    , xlFocusBgOrange500
    , xlFocusBgOrange600
    , xlFocusBgOrange700
    , xlFocusBgOrange800
    , xlFocusBgOrange900
    , xlFocusBgPink100
    , xlFocusBgPink200
    , xlFocusBgPink300
    , xlFocusBgPink400
    , xlFocusBgPink500
    , xlFocusBgPink600
    , xlFocusBgPink700
    , xlFocusBgPink800
    , xlFocusBgPink900
    , xlFocusBgPurple100
    , xlFocusBgPurple200
    , xlFocusBgPurple300
    , xlFocusBgPurple400
    , xlFocusBgPurple500
    , xlFocusBgPurple600
    , xlFocusBgPurple700
    , xlFocusBgPurple800
    , xlFocusBgPurple900
    , xlFocusBgRed100
    , xlFocusBgRed200
    , xlFocusBgRed300
    , xlFocusBgRed400
    , xlFocusBgRed500
    , xlFocusBgRed600
    , xlFocusBgRed700
    , xlFocusBgRed800
    , xlFocusBgRed900
    , xlFocusBgTeal100
    , xlFocusBgTeal200
    , xlFocusBgTeal300
    , xlFocusBgTeal400
    , xlFocusBgTeal500
    , xlFocusBgTeal600
    , xlFocusBgTeal700
    , xlFocusBgTeal800
    , xlFocusBgTeal900
    , xlFocusBgTransparent
    , xlFocusBgWhite
    , xlFocusBgYellow100
    , xlFocusBgYellow200
    , xlFocusBgYellow300
    , xlFocusBgYellow400
    , xlFocusBgYellow500
    , xlFocusBgYellow600
    , xlFocusBgYellow700
    , xlFocusBgYellow800
    , xlFocusBgYellow900
    , xlFocusBorderBlack
    , xlFocusBorderBlue100
    , xlFocusBorderBlue200
    , xlFocusBorderBlue300
    , xlFocusBorderBlue400
    , xlFocusBorderBlue500
    , xlFocusBorderBlue600
    , xlFocusBorderBlue700
    , xlFocusBorderBlue800
    , xlFocusBorderBlue900
    , xlFocusBorderCurrent
    , xlFocusBorderGray100
    , xlFocusBorderGray200
    , xlFocusBorderGray300
    , xlFocusBorderGray400
    , xlFocusBorderGray500
    , xlFocusBorderGray600
    , xlFocusBorderGray700
    , xlFocusBorderGray800
    , xlFocusBorderGray900
    , xlFocusBorderGreen100
    , xlFocusBorderGreen200
    , xlFocusBorderGreen300
    , xlFocusBorderGreen400
    , xlFocusBorderGreen500
    , xlFocusBorderGreen600
    , xlFocusBorderGreen700
    , xlFocusBorderGreen800
    , xlFocusBorderGreen900
    , xlFocusBorderIndigo100
    , xlFocusBorderIndigo200
    , xlFocusBorderIndigo300
    , xlFocusBorderIndigo400
    , xlFocusBorderIndigo500
    , xlFocusBorderIndigo600
    , xlFocusBorderIndigo700
    , xlFocusBorderIndigo800
    , xlFocusBorderIndigo900
    , xlFocusBorderOpacity0
    , xlFocusBorderOpacity100
    , xlFocusBorderOpacity25
    , xlFocusBorderOpacity50
    , xlFocusBorderOpacity75
    , xlFocusBorderOrange100
    , xlFocusBorderOrange200
    , xlFocusBorderOrange300
    , xlFocusBorderOrange400
    , xlFocusBorderOrange500
    , xlFocusBorderOrange600
    , xlFocusBorderOrange700
    , xlFocusBorderOrange800
    , xlFocusBorderOrange900
    , xlFocusBorderPink100
    , xlFocusBorderPink200
    , xlFocusBorderPink300
    , xlFocusBorderPink400
    , xlFocusBorderPink500
    , xlFocusBorderPink600
    , xlFocusBorderPink700
    , xlFocusBorderPink800
    , xlFocusBorderPink900
    , xlFocusBorderPurple100
    , xlFocusBorderPurple200
    , xlFocusBorderPurple300
    , xlFocusBorderPurple400
    , xlFocusBorderPurple500
    , xlFocusBorderPurple600
    , xlFocusBorderPurple700
    , xlFocusBorderPurple800
    , xlFocusBorderPurple900
    , xlFocusBorderRed100
    , xlFocusBorderRed200
    , xlFocusBorderRed300
    , xlFocusBorderRed400
    , xlFocusBorderRed500
    , xlFocusBorderRed600
    , xlFocusBorderRed700
    , xlFocusBorderRed800
    , xlFocusBorderRed900
    , xlFocusBorderTeal100
    , xlFocusBorderTeal200
    , xlFocusBorderTeal300
    , xlFocusBorderTeal400
    , xlFocusBorderTeal500
    , xlFocusBorderTeal600
    , xlFocusBorderTeal700
    , xlFocusBorderTeal800
    , xlFocusBorderTeal900
    , xlFocusBorderTransparent
    , xlFocusBorderWhite
    , xlFocusBorderYellow100
    , xlFocusBorderYellow200
    , xlFocusBorderYellow300
    , xlFocusBorderYellow400
    , xlFocusBorderYellow500
    , xlFocusBorderYellow600
    , xlFocusBorderYellow700
    , xlFocusBorderYellow800
    , xlFocusBorderYellow900
    , xlFocusFontBlack
    , xlFocusFontBold
    , xlFocusFontExtrabold
    , xlFocusFontHairline
    , xlFocusFontLight
    , xlFocusFontMedium
    , xlFocusFontNormal
    , xlFocusFontSemibold
    , xlFocusFontThin
    , xlFocusFromBlack
    , xlFocusFromBlue100
    , xlFocusFromBlue200
    , xlFocusFromBlue300
    , xlFocusFromBlue400
    , xlFocusFromBlue500
    , xlFocusFromBlue600
    , xlFocusFromBlue700
    , xlFocusFromBlue800
    , xlFocusFromBlue900
    , xlFocusFromCurrent
    , xlFocusFromGray100
    , xlFocusFromGray200
    , xlFocusFromGray300
    , xlFocusFromGray400
    , xlFocusFromGray500
    , xlFocusFromGray600
    , xlFocusFromGray700
    , xlFocusFromGray800
    , xlFocusFromGray900
    , xlFocusFromGreen100
    , xlFocusFromGreen200
    , xlFocusFromGreen300
    , xlFocusFromGreen400
    , xlFocusFromGreen500
    , xlFocusFromGreen600
    , xlFocusFromGreen700
    , xlFocusFromGreen800
    , xlFocusFromGreen900
    , xlFocusFromIndigo100
    , xlFocusFromIndigo200
    , xlFocusFromIndigo300
    , xlFocusFromIndigo400
    , xlFocusFromIndigo500
    , xlFocusFromIndigo600
    , xlFocusFromIndigo700
    , xlFocusFromIndigo800
    , xlFocusFromIndigo900
    , xlFocusFromOrange100
    , xlFocusFromOrange200
    , xlFocusFromOrange300
    , xlFocusFromOrange400
    , xlFocusFromOrange500
    , xlFocusFromOrange600
    , xlFocusFromOrange700
    , xlFocusFromOrange800
    , xlFocusFromOrange900
    , xlFocusFromPink100
    , xlFocusFromPink200
    , xlFocusFromPink300
    , xlFocusFromPink400
    , xlFocusFromPink500
    , xlFocusFromPink600
    , xlFocusFromPink700
    , xlFocusFromPink800
    , xlFocusFromPink900
    , xlFocusFromPurple100
    , xlFocusFromPurple200
    , xlFocusFromPurple300
    , xlFocusFromPurple400
    , xlFocusFromPurple500
    , xlFocusFromPurple600
    , xlFocusFromPurple700
    , xlFocusFromPurple800
    , xlFocusFromPurple900
    , xlFocusFromRed100
    , xlFocusFromRed200
    , xlFocusFromRed300
    , xlFocusFromRed400
    , xlFocusFromRed500
    , xlFocusFromRed600
    , xlFocusFromRed700
    , xlFocusFromRed800
    , xlFocusFromRed900
    , xlFocusFromTeal100
    , xlFocusFromTeal200
    , xlFocusFromTeal300
    , xlFocusFromTeal400
    , xlFocusFromTeal500
    , xlFocusFromTeal600
    , xlFocusFromTeal700
    , xlFocusFromTeal800
    , xlFocusFromTeal900
    , xlFocusFromTransparent
    , xlFocusFromWhite
    , xlFocusFromYellow100
    , xlFocusFromYellow200
    , xlFocusFromYellow300
    , xlFocusFromYellow400
    , xlFocusFromYellow500
    , xlFocusFromYellow600
    , xlFocusFromYellow700
    , xlFocusFromYellow800
    , xlFocusFromYellow900
    , xlFocusLineThrough
    , xlFocusNegRotate180
    , xlFocusNegRotate45
    , xlFocusNegRotate90
    , xlFocusNegSkewX12
    , xlFocusNegSkewX3
    , xlFocusNegSkewX6
    , xlFocusNegSkewY12
    , xlFocusNegSkewY3
    , xlFocusNegSkewY6
    , xlFocusNegTranslateX1
    , xlFocusNegTranslateX10
    , xlFocusNegTranslateX12
    , xlFocusNegTranslateX16
    , xlFocusNegTranslateX1over2
    , xlFocusNegTranslateX2
    , xlFocusNegTranslateX20
    , xlFocusNegTranslateX24
    , xlFocusNegTranslateX3
    , xlFocusNegTranslateX32
    , xlFocusNegTranslateX4
    , xlFocusNegTranslateX40
    , xlFocusNegTranslateX48
    , xlFocusNegTranslateX5
    , xlFocusNegTranslateX56
    , xlFocusNegTranslateX6
    , xlFocusNegTranslateX64
    , xlFocusNegTranslateX8
    , xlFocusNegTranslateXFull
    , xlFocusNegTranslateXPx
    , xlFocusNegTranslateY1
    , xlFocusNegTranslateY10
    , xlFocusNegTranslateY12
    , xlFocusNegTranslateY16
    , xlFocusNegTranslateY1over2
    , xlFocusNegTranslateY2
    , xlFocusNegTranslateY20
    , xlFocusNegTranslateY24
    , xlFocusNegTranslateY3
    , xlFocusNegTranslateY32
    , xlFocusNegTranslateY4
    , xlFocusNegTranslateY40
    , xlFocusNegTranslateY48
    , xlFocusNegTranslateY5
    , xlFocusNegTranslateY56
    , xlFocusNegTranslateY6
    , xlFocusNegTranslateY64
    , xlFocusNegTranslateY8
    , xlFocusNegTranslateYFull
    , xlFocusNegTranslateYPx
    , xlFocusNoUnderline
    , xlFocusNotSrOnly
    , xlFocusOpacity0
    , xlFocusOpacity100
    , xlFocusOpacity25
    , xlFocusOpacity50
    , xlFocusOpacity75
    , xlFocusOutlineNone
    , xlFocusPlaceholderBlackFocus
    , xlFocusPlaceholderBlue100Focus
    , xlFocusPlaceholderBlue200Focus
    , xlFocusPlaceholderBlue300Focus
    , xlFocusPlaceholderBlue400Focus
    , xlFocusPlaceholderBlue500Focus
    , xlFocusPlaceholderBlue600Focus
    , xlFocusPlaceholderBlue700Focus
    , xlFocusPlaceholderBlue800Focus
    , xlFocusPlaceholderBlue900Focus
    , xlFocusPlaceholderCurrentFocus
    , xlFocusPlaceholderGray100Focus
    , xlFocusPlaceholderGray200Focus
    , xlFocusPlaceholderGray300Focus
    , xlFocusPlaceholderGray400Focus
    , xlFocusPlaceholderGray500Focus
    , xlFocusPlaceholderGray600Focus
    , xlFocusPlaceholderGray700Focus
    , xlFocusPlaceholderGray800Focus
    , xlFocusPlaceholderGray900Focus
    , xlFocusPlaceholderGreen100Focus
    , xlFocusPlaceholderGreen200Focus
    , xlFocusPlaceholderGreen300Focus
    , xlFocusPlaceholderGreen400Focus
    , xlFocusPlaceholderGreen500Focus
    , xlFocusPlaceholderGreen600Focus
    , xlFocusPlaceholderGreen700Focus
    , xlFocusPlaceholderGreen800Focus
    , xlFocusPlaceholderGreen900Focus
    , xlFocusPlaceholderIndigo100Focus
    , xlFocusPlaceholderIndigo200Focus
    , xlFocusPlaceholderIndigo300Focus
    , xlFocusPlaceholderIndigo400Focus
    , xlFocusPlaceholderIndigo500Focus
    , xlFocusPlaceholderIndigo600Focus
    , xlFocusPlaceholderIndigo700Focus
    , xlFocusPlaceholderIndigo800Focus
    , xlFocusPlaceholderIndigo900Focus
    , xlFocusPlaceholderOpacity0Focus
    , xlFocusPlaceholderOpacity100Focus
    , xlFocusPlaceholderOpacity25Focus
    , xlFocusPlaceholderOpacity50Focus
    , xlFocusPlaceholderOpacity75Focus
    , xlFocusPlaceholderOrange100Focus
    , xlFocusPlaceholderOrange200Focus
    , xlFocusPlaceholderOrange300Focus
    , xlFocusPlaceholderOrange400Focus
    , xlFocusPlaceholderOrange500Focus
    , xlFocusPlaceholderOrange600Focus
    , xlFocusPlaceholderOrange700Focus
    , xlFocusPlaceholderOrange800Focus
    , xlFocusPlaceholderOrange900Focus
    , xlFocusPlaceholderPink100Focus
    , xlFocusPlaceholderPink200Focus
    , xlFocusPlaceholderPink300Focus
    , xlFocusPlaceholderPink400Focus
    , xlFocusPlaceholderPink500Focus
    , xlFocusPlaceholderPink600Focus
    , xlFocusPlaceholderPink700Focus
    , xlFocusPlaceholderPink800Focus
    , xlFocusPlaceholderPink900Focus
    , xlFocusPlaceholderPurple100Focus
    , xlFocusPlaceholderPurple200Focus
    , xlFocusPlaceholderPurple300Focus
    , xlFocusPlaceholderPurple400Focus
    , xlFocusPlaceholderPurple500Focus
    , xlFocusPlaceholderPurple600Focus
    , xlFocusPlaceholderPurple700Focus
    , xlFocusPlaceholderPurple800Focus
    , xlFocusPlaceholderPurple900Focus
    , xlFocusPlaceholderRed100Focus
    , xlFocusPlaceholderRed200Focus
    , xlFocusPlaceholderRed300Focus
    , xlFocusPlaceholderRed400Focus
    , xlFocusPlaceholderRed500Focus
    , xlFocusPlaceholderRed600Focus
    , xlFocusPlaceholderRed700Focus
    , xlFocusPlaceholderRed800Focus
    , xlFocusPlaceholderRed900Focus
    , xlFocusPlaceholderTeal100Focus
    , xlFocusPlaceholderTeal200Focus
    , xlFocusPlaceholderTeal300Focus
    , xlFocusPlaceholderTeal400Focus
    , xlFocusPlaceholderTeal500Focus
    , xlFocusPlaceholderTeal600Focus
    , xlFocusPlaceholderTeal700Focus
    , xlFocusPlaceholderTeal800Focus
    , xlFocusPlaceholderTeal900Focus
    , xlFocusPlaceholderTransparentFocus
    , xlFocusPlaceholderWhiteFocus
    , xlFocusPlaceholderYellow100Focus
    , xlFocusPlaceholderYellow200Focus
    , xlFocusPlaceholderYellow300Focus
    , xlFocusPlaceholderYellow400Focus
    , xlFocusPlaceholderYellow500Focus
    , xlFocusPlaceholderYellow600Focus
    , xlFocusPlaceholderYellow700Focus
    , xlFocusPlaceholderYellow800Focus
    , xlFocusPlaceholderYellow900Focus
    , xlFocusRotate0
    , xlFocusRotate180
    , xlFocusRotate45
    , xlFocusRotate90
    , xlFocusScale0
    , xlFocusScale100
    , xlFocusScale105
    , xlFocusScale110
    , xlFocusScale125
    , xlFocusScale150
    , xlFocusScale50
    , xlFocusScale75
    , xlFocusScale90
    , xlFocusScale95
    , xlFocusScaleX0
    , xlFocusScaleX100
    , xlFocusScaleX105
    , xlFocusScaleX110
    , xlFocusScaleX125
    , xlFocusScaleX150
    , xlFocusScaleX50
    , xlFocusScaleX75
    , xlFocusScaleX90
    , xlFocusScaleX95
    , xlFocusScaleY0
    , xlFocusScaleY100
    , xlFocusScaleY105
    , xlFocusScaleY110
    , xlFocusScaleY125
    , xlFocusScaleY150
    , xlFocusScaleY50
    , xlFocusScaleY75
    , xlFocusScaleY90
    , xlFocusScaleY95
    , xlFocusShadow
    , xlFocusShadow2xl
    , xlFocusShadowInner
    , xlFocusShadowLg
    , xlFocusShadowMd
    , xlFocusShadowNone
    , xlFocusShadowOutline
    , xlFocusShadowSm
    , xlFocusShadowXl
    , xlFocusShadowXs
    , xlFocusSkewX0
    , xlFocusSkewX12
    , xlFocusSkewX3
    , xlFocusSkewX6
    , xlFocusSkewY0
    , xlFocusSkewY12
    , xlFocusSkewY3
    , xlFocusSkewY6
    , xlFocusSrOnly
    , xlFocusTextBlack
    , xlFocusTextBlue100
    , xlFocusTextBlue200
    , xlFocusTextBlue300
    , xlFocusTextBlue400
    , xlFocusTextBlue500
    , xlFocusTextBlue600
    , xlFocusTextBlue700
    , xlFocusTextBlue800
    , xlFocusTextBlue900
    , xlFocusTextCurrent
    , xlFocusTextGray100
    , xlFocusTextGray200
    , xlFocusTextGray300
    , xlFocusTextGray400
    , xlFocusTextGray500
    , xlFocusTextGray600
    , xlFocusTextGray700
    , xlFocusTextGray800
    , xlFocusTextGray900
    , xlFocusTextGreen100
    , xlFocusTextGreen200
    , xlFocusTextGreen300
    , xlFocusTextGreen400
    , xlFocusTextGreen500
    , xlFocusTextGreen600
    , xlFocusTextGreen700
    , xlFocusTextGreen800
    , xlFocusTextGreen900
    , xlFocusTextIndigo100
    , xlFocusTextIndigo200
    , xlFocusTextIndigo300
    , xlFocusTextIndigo400
    , xlFocusTextIndigo500
    , xlFocusTextIndigo600
    , xlFocusTextIndigo700
    , xlFocusTextIndigo800
    , xlFocusTextIndigo900
    , xlFocusTextOpacity0
    , xlFocusTextOpacity100
    , xlFocusTextOpacity25
    , xlFocusTextOpacity50
    , xlFocusTextOpacity75
    , xlFocusTextOrange100
    , xlFocusTextOrange200
    , xlFocusTextOrange300
    , xlFocusTextOrange400
    , xlFocusTextOrange500
    , xlFocusTextOrange600
    , xlFocusTextOrange700
    , xlFocusTextOrange800
    , xlFocusTextOrange900
    , xlFocusTextPink100
    , xlFocusTextPink200
    , xlFocusTextPink300
    , xlFocusTextPink400
    , xlFocusTextPink500
    , xlFocusTextPink600
    , xlFocusTextPink700
    , xlFocusTextPink800
    , xlFocusTextPink900
    , xlFocusTextPurple100
    , xlFocusTextPurple200
    , xlFocusTextPurple300
    , xlFocusTextPurple400
    , xlFocusTextPurple500
    , xlFocusTextPurple600
    , xlFocusTextPurple700
    , xlFocusTextPurple800
    , xlFocusTextPurple900
    , xlFocusTextRed100
    , xlFocusTextRed200
    , xlFocusTextRed300
    , xlFocusTextRed400
    , xlFocusTextRed500
    , xlFocusTextRed600
    , xlFocusTextRed700
    , xlFocusTextRed800
    , xlFocusTextRed900
    , xlFocusTextTeal100
    , xlFocusTextTeal200
    , xlFocusTextTeal300
    , xlFocusTextTeal400
    , xlFocusTextTeal500
    , xlFocusTextTeal600
    , xlFocusTextTeal700
    , xlFocusTextTeal800
    , xlFocusTextTeal900
    , xlFocusTextTransparent
    , xlFocusTextWhite
    , xlFocusTextYellow100
    , xlFocusTextYellow200
    , xlFocusTextYellow300
    , xlFocusTextYellow400
    , xlFocusTextYellow500
    , xlFocusTextYellow600
    , xlFocusTextYellow700
    , xlFocusTextYellow800
    , xlFocusTextYellow900
    , xlFocusToBlack
    , xlFocusToBlue100
    , xlFocusToBlue200
    , xlFocusToBlue300
    , xlFocusToBlue400
    , xlFocusToBlue500
    , xlFocusToBlue600
    , xlFocusToBlue700
    , xlFocusToBlue800
    , xlFocusToBlue900
    , xlFocusToCurrent
    , xlFocusToGray100
    , xlFocusToGray200
    , xlFocusToGray300
    , xlFocusToGray400
    , xlFocusToGray500
    , xlFocusToGray600
    , xlFocusToGray700
    , xlFocusToGray800
    , xlFocusToGray900
    , xlFocusToGreen100
    , xlFocusToGreen200
    , xlFocusToGreen300
    , xlFocusToGreen400
    , xlFocusToGreen500
    , xlFocusToGreen600
    , xlFocusToGreen700
    , xlFocusToGreen800
    , xlFocusToGreen900
    , xlFocusToIndigo100
    , xlFocusToIndigo200
    , xlFocusToIndigo300
    , xlFocusToIndigo400
    , xlFocusToIndigo500
    , xlFocusToIndigo600
    , xlFocusToIndigo700
    , xlFocusToIndigo800
    , xlFocusToIndigo900
    , xlFocusToOrange100
    , xlFocusToOrange200
    , xlFocusToOrange300
    , xlFocusToOrange400
    , xlFocusToOrange500
    , xlFocusToOrange600
    , xlFocusToOrange700
    , xlFocusToOrange800
    , xlFocusToOrange900
    , xlFocusToPink100
    , xlFocusToPink200
    , xlFocusToPink300
    , xlFocusToPink400
    , xlFocusToPink500
    , xlFocusToPink600
    , xlFocusToPink700
    , xlFocusToPink800
    , xlFocusToPink900
    , xlFocusToPurple100
    , xlFocusToPurple200
    , xlFocusToPurple300
    , xlFocusToPurple400
    , xlFocusToPurple500
    , xlFocusToPurple600
    , xlFocusToPurple700
    , xlFocusToPurple800
    , xlFocusToPurple900
    , xlFocusToRed100
    , xlFocusToRed200
    , xlFocusToRed300
    , xlFocusToRed400
    , xlFocusToRed500
    , xlFocusToRed600
    , xlFocusToRed700
    , xlFocusToRed800
    , xlFocusToRed900
    , xlFocusToTeal100
    , xlFocusToTeal200
    , xlFocusToTeal300
    , xlFocusToTeal400
    , xlFocusToTeal500
    , xlFocusToTeal600
    , xlFocusToTeal700
    , xlFocusToTeal800
    , xlFocusToTeal900
    , xlFocusToTransparent
    , xlFocusToWhite
    , xlFocusToYellow100
    , xlFocusToYellow200
    , xlFocusToYellow300
    , xlFocusToYellow400
    , xlFocusToYellow500
    , xlFocusToYellow600
    , xlFocusToYellow700
    , xlFocusToYellow800
    , xlFocusToYellow900
    , xlFocusTranslateX0
    , xlFocusTranslateX1
    , xlFocusTranslateX10
    , xlFocusTranslateX12
    , xlFocusTranslateX16
    , xlFocusTranslateX1over2
    , xlFocusTranslateX2
    , xlFocusTranslateX20
    , xlFocusTranslateX24
    , xlFocusTranslateX3
    , xlFocusTranslateX32
    , xlFocusTranslateX4
    , xlFocusTranslateX40
    , xlFocusTranslateX48
    , xlFocusTranslateX5
    , xlFocusTranslateX56
    , xlFocusTranslateX6
    , xlFocusTranslateX64
    , xlFocusTranslateX8
    , xlFocusTranslateXFull
    , xlFocusTranslateXPx
    , xlFocusTranslateY0
    , xlFocusTranslateY1
    , xlFocusTranslateY10
    , xlFocusTranslateY12
    , xlFocusTranslateY16
    , xlFocusTranslateY1over2
    , xlFocusTranslateY2
    , xlFocusTranslateY20
    , xlFocusTranslateY24
    , xlFocusTranslateY3
    , xlFocusTranslateY32
    , xlFocusTranslateY4
    , xlFocusTranslateY40
    , xlFocusTranslateY48
    , xlFocusTranslateY5
    , xlFocusTranslateY56
    , xlFocusTranslateY6
    , xlFocusTranslateY64
    , xlFocusTranslateY8
    , xlFocusTranslateYFull
    , xlFocusTranslateYPx
    , xlFocusUnderline
    , xlFocusViaBlack
    , xlFocusViaBlue100
    , xlFocusViaBlue200
    , xlFocusViaBlue300
    , xlFocusViaBlue400
    , xlFocusViaBlue500
    , xlFocusViaBlue600
    , xlFocusViaBlue700
    , xlFocusViaBlue800
    , xlFocusViaBlue900
    , xlFocusViaCurrent
    , xlFocusViaGray100
    , xlFocusViaGray200
    , xlFocusViaGray300
    , xlFocusViaGray400
    , xlFocusViaGray500
    , xlFocusViaGray600
    , xlFocusViaGray700
    , xlFocusViaGray800
    , xlFocusViaGray900
    , xlFocusViaGreen100
    , xlFocusViaGreen200
    , xlFocusViaGreen300
    , xlFocusViaGreen400
    , xlFocusViaGreen500
    , xlFocusViaGreen600
    , xlFocusViaGreen700
    , xlFocusViaGreen800
    , xlFocusViaGreen900
    , xlFocusViaIndigo100
    , xlFocusViaIndigo200
    , xlFocusViaIndigo300
    , xlFocusViaIndigo400
    , xlFocusViaIndigo500
    , xlFocusViaIndigo600
    , xlFocusViaIndigo700
    , xlFocusViaIndigo800
    , xlFocusViaIndigo900
    , xlFocusViaOrange100
    , xlFocusViaOrange200
    , xlFocusViaOrange300
    , xlFocusViaOrange400
    , xlFocusViaOrange500
    , xlFocusViaOrange600
    , xlFocusViaOrange700
    , xlFocusViaOrange800
    , xlFocusViaOrange900
    , xlFocusViaPink100
    , xlFocusViaPink200
    , xlFocusViaPink300
    , xlFocusViaPink400
    , xlFocusViaPink500
    , xlFocusViaPink600
    , xlFocusViaPink700
    , xlFocusViaPink800
    , xlFocusViaPink900
    , xlFocusViaPurple100
    , xlFocusViaPurple200
    , xlFocusViaPurple300
    , xlFocusViaPurple400
    , xlFocusViaPurple500
    , xlFocusViaPurple600
    , xlFocusViaPurple700
    , xlFocusViaPurple800
    , xlFocusViaPurple900
    , xlFocusViaRed100
    , xlFocusViaRed200
    , xlFocusViaRed300
    , xlFocusViaRed400
    , xlFocusViaRed500
    , xlFocusViaRed600
    , xlFocusViaRed700
    , xlFocusViaRed800
    , xlFocusViaRed900
    , xlFocusViaTeal100
    , xlFocusViaTeal200
    , xlFocusViaTeal300
    , xlFocusViaTeal400
    , xlFocusViaTeal500
    , xlFocusViaTeal600
    , xlFocusViaTeal700
    , xlFocusViaTeal800
    , xlFocusViaTeal900
    , xlFocusViaTransparent
    , xlFocusViaWhite
    , xlFocusViaYellow100
    , xlFocusViaYellow200
    , xlFocusViaYellow300
    , xlFocusViaYellow400
    , xlFocusViaYellow500
    , xlFocusViaYellow600
    , xlFocusViaYellow700
    , xlFocusViaYellow800
    , xlFocusViaYellow900
    , xlFontBlack
    , xlFontBold
    , xlFontExtrabold
    , xlFontHairline
    , xlFontLight
    , xlFontMedium
    , xlFontMono
    , xlFontNormal
    , xlFontSans
    , xlFontSemibold
    , xlFontSerif
    , xlFontThin
    , xlFromBlack
    , xlFromBlue100
    , xlFromBlue200
    , xlFromBlue300
    , xlFromBlue400
    , xlFromBlue500
    , xlFromBlue600
    , xlFromBlue700
    , xlFromBlue800
    , xlFromBlue900
    , xlFromCurrent
    , xlFromGray100
    , xlFromGray200
    , xlFromGray300
    , xlFromGray400
    , xlFromGray500
    , xlFromGray600
    , xlFromGray700
    , xlFromGray800
    , xlFromGray900
    , xlFromGreen100
    , xlFromGreen200
    , xlFromGreen300
    , xlFromGreen400
    , xlFromGreen500
    , xlFromGreen600
    , xlFromGreen700
    , xlFromGreen800
    , xlFromGreen900
    , xlFromIndigo100
    , xlFromIndigo200
    , xlFromIndigo300
    , xlFromIndigo400
    , xlFromIndigo500
    , xlFromIndigo600
    , xlFromIndigo700
    , xlFromIndigo800
    , xlFromIndigo900
    , xlFromOrange100
    , xlFromOrange200
    , xlFromOrange300
    , xlFromOrange400
    , xlFromOrange500
    , xlFromOrange600
    , xlFromOrange700
    , xlFromOrange800
    , xlFromOrange900
    , xlFromPink100
    , xlFromPink200
    , xlFromPink300
    , xlFromPink400
    , xlFromPink500
    , xlFromPink600
    , xlFromPink700
    , xlFromPink800
    , xlFromPink900
    , xlFromPurple100
    , xlFromPurple200
    , xlFromPurple300
    , xlFromPurple400
    , xlFromPurple500
    , xlFromPurple600
    , xlFromPurple700
    , xlFromPurple800
    , xlFromPurple900
    , xlFromRed100
    , xlFromRed200
    , xlFromRed300
    , xlFromRed400
    , xlFromRed500
    , xlFromRed600
    , xlFromRed700
    , xlFromRed800
    , xlFromRed900
    , xlFromTeal100
    , xlFromTeal200
    , xlFromTeal300
    , xlFromTeal400
    , xlFromTeal500
    , xlFromTeal600
    , xlFromTeal700
    , xlFromTeal800
    , xlFromTeal900
    , xlFromTransparent
    , xlFromWhite
    , xlFromYellow100
    , xlFromYellow200
    , xlFromYellow300
    , xlFromYellow400
    , xlFromYellow500
    , xlFromYellow600
    , xlFromYellow700
    , xlFromYellow800
    , xlFromYellow900
    , xlGap0
    , xlGap1
    , xlGap10
    , xlGap12
    , xlGap16
    , xlGap2
    , xlGap20
    , xlGap24
    , xlGap3
    , xlGap32
    , xlGap4
    , xlGap40
    , xlGap48
    , xlGap5
    , xlGap56
    , xlGap6
    , xlGap64
    , xlGap8
    , xlGapPx
    , xlGapX0
    , xlGapX1
    , xlGapX10
    , xlGapX12
    , xlGapX16
    , xlGapX2
    , xlGapX20
    , xlGapX24
    , xlGapX3
    , xlGapX32
    , xlGapX4
    , xlGapX40
    , xlGapX48
    , xlGapX5
    , xlGapX56
    , xlGapX6
    , xlGapX64
    , xlGapX8
    , xlGapXPx
    , xlGapY0
    , xlGapY1
    , xlGapY10
    , xlGapY12
    , xlGapY16
    , xlGapY2
    , xlGapY20
    , xlGapY24
    , xlGapY3
    , xlGapY32
    , xlGapY4
    , xlGapY40
    , xlGapY48
    , xlGapY5
    , xlGapY56
    , xlGapY6
    , xlGapY64
    , xlGapY8
    , xlGapYPx
    , xlGrid
    , xlGridCols1
    , xlGridCols10
    , xlGridCols11
    , xlGridCols12
    , xlGridCols2
    , xlGridCols3
    , xlGridCols4
    , xlGridCols5
    , xlGridCols6
    , xlGridCols7
    , xlGridCols8
    , xlGridCols9
    , xlGridColsNone
    , xlGridFlowCol
    , xlGridFlowColDense
    , xlGridFlowRow
    , xlGridFlowRowDense
    , xlGridRows1
    , xlGridRows2
    , xlGridRows3
    , xlGridRows4
    , xlGridRows5
    , xlGridRows6
    , xlGridRowsNone
    , xlH0
    , xlH1
    , xlH10
    , xlH12
    , xlH16
    , xlH2
    , xlH20
    , xlH24
    , xlH3
    , xlH32
    , xlH4
    , xlH40
    , xlH48
    , xlH5
    , xlH56
    , xlH6
    , xlH64
    , xlH8
    , xlHAuto
    , xlHFull
    , xlHPx
    , xlHScreen
    , xlHidden
    , xlHoverBgBlack
    , xlHoverBgBlue100
    , xlHoverBgBlue200
    , xlHoverBgBlue300
    , xlHoverBgBlue400
    , xlHoverBgBlue500
    , xlHoverBgBlue600
    , xlHoverBgBlue700
    , xlHoverBgBlue800
    , xlHoverBgBlue900
    , xlHoverBgCurrent
    , xlHoverBgGray100
    , xlHoverBgGray200
    , xlHoverBgGray300
    , xlHoverBgGray400
    , xlHoverBgGray500
    , xlHoverBgGray600
    , xlHoverBgGray700
    , xlHoverBgGray800
    , xlHoverBgGray900
    , xlHoverBgGreen100
    , xlHoverBgGreen200
    , xlHoverBgGreen300
    , xlHoverBgGreen400
    , xlHoverBgGreen500
    , xlHoverBgGreen600
    , xlHoverBgGreen700
    , xlHoverBgGreen800
    , xlHoverBgGreen900
    , xlHoverBgIndigo100
    , xlHoverBgIndigo200
    , xlHoverBgIndigo300
    , xlHoverBgIndigo400
    , xlHoverBgIndigo500
    , xlHoverBgIndigo600
    , xlHoverBgIndigo700
    , xlHoverBgIndigo800
    , xlHoverBgIndigo900
    , xlHoverBgOpacity0
    , xlHoverBgOpacity100
    , xlHoverBgOpacity25
    , xlHoverBgOpacity50
    , xlHoverBgOpacity75
    , xlHoverBgOrange100
    , xlHoverBgOrange200
    , xlHoverBgOrange300
    , xlHoverBgOrange400
    , xlHoverBgOrange500
    , xlHoverBgOrange600
    , xlHoverBgOrange700
    , xlHoverBgOrange800
    , xlHoverBgOrange900
    , xlHoverBgPink100
    , xlHoverBgPink200
    , xlHoverBgPink300
    , xlHoverBgPink400
    , xlHoverBgPink500
    , xlHoverBgPink600
    , xlHoverBgPink700
    , xlHoverBgPink800
    , xlHoverBgPink900
    , xlHoverBgPurple100
    , xlHoverBgPurple200
    , xlHoverBgPurple300
    , xlHoverBgPurple400
    , xlHoverBgPurple500
    , xlHoverBgPurple600
    , xlHoverBgPurple700
    , xlHoverBgPurple800
    , xlHoverBgPurple900
    , xlHoverBgRed100
    , xlHoverBgRed200
    , xlHoverBgRed300
    , xlHoverBgRed400
    , xlHoverBgRed500
    , xlHoverBgRed600
    , xlHoverBgRed700
    , xlHoverBgRed800
    , xlHoverBgRed900
    , xlHoverBgTeal100
    , xlHoverBgTeal200
    , xlHoverBgTeal300
    , xlHoverBgTeal400
    , xlHoverBgTeal500
    , xlHoverBgTeal600
    , xlHoverBgTeal700
    , xlHoverBgTeal800
    , xlHoverBgTeal900
    , xlHoverBgTransparent
    , xlHoverBgWhite
    , xlHoverBgYellow100
    , xlHoverBgYellow200
    , xlHoverBgYellow300
    , xlHoverBgYellow400
    , xlHoverBgYellow500
    , xlHoverBgYellow600
    , xlHoverBgYellow700
    , xlHoverBgYellow800
    , xlHoverBgYellow900
    , xlHoverBorderBlack
    , xlHoverBorderBlue100
    , xlHoverBorderBlue200
    , xlHoverBorderBlue300
    , xlHoverBorderBlue400
    , xlHoverBorderBlue500
    , xlHoverBorderBlue600
    , xlHoverBorderBlue700
    , xlHoverBorderBlue800
    , xlHoverBorderBlue900
    , xlHoverBorderCurrent
    , xlHoverBorderGray100
    , xlHoverBorderGray200
    , xlHoverBorderGray300
    , xlHoverBorderGray400
    , xlHoverBorderGray500
    , xlHoverBorderGray600
    , xlHoverBorderGray700
    , xlHoverBorderGray800
    , xlHoverBorderGray900
    , xlHoverBorderGreen100
    , xlHoverBorderGreen200
    , xlHoverBorderGreen300
    , xlHoverBorderGreen400
    , xlHoverBorderGreen500
    , xlHoverBorderGreen600
    , xlHoverBorderGreen700
    , xlHoverBorderGreen800
    , xlHoverBorderGreen900
    , xlHoverBorderIndigo100
    , xlHoverBorderIndigo200
    , xlHoverBorderIndigo300
    , xlHoverBorderIndigo400
    , xlHoverBorderIndigo500
    , xlHoverBorderIndigo600
    , xlHoverBorderIndigo700
    , xlHoverBorderIndigo800
    , xlHoverBorderIndigo900
    , xlHoverBorderOpacity0
    , xlHoverBorderOpacity100
    , xlHoverBorderOpacity25
    , xlHoverBorderOpacity50
    , xlHoverBorderOpacity75
    , xlHoverBorderOrange100
    , xlHoverBorderOrange200
    , xlHoverBorderOrange300
    , xlHoverBorderOrange400
    , xlHoverBorderOrange500
    , xlHoverBorderOrange600
    , xlHoverBorderOrange700
    , xlHoverBorderOrange800
    , xlHoverBorderOrange900
    , xlHoverBorderPink100
    , xlHoverBorderPink200
    , xlHoverBorderPink300
    , xlHoverBorderPink400
    , xlHoverBorderPink500
    , xlHoverBorderPink600
    , xlHoverBorderPink700
    , xlHoverBorderPink800
    , xlHoverBorderPink900
    , xlHoverBorderPurple100
    , xlHoverBorderPurple200
    , xlHoverBorderPurple300
    , xlHoverBorderPurple400
    , xlHoverBorderPurple500
    , xlHoverBorderPurple600
    , xlHoverBorderPurple700
    , xlHoverBorderPurple800
    , xlHoverBorderPurple900
    , xlHoverBorderRed100
    , xlHoverBorderRed200
    , xlHoverBorderRed300
    , xlHoverBorderRed400
    , xlHoverBorderRed500
    , xlHoverBorderRed600
    , xlHoverBorderRed700
    , xlHoverBorderRed800
    , xlHoverBorderRed900
    , xlHoverBorderTeal100
    , xlHoverBorderTeal200
    , xlHoverBorderTeal300
    , xlHoverBorderTeal400
    , xlHoverBorderTeal500
    , xlHoverBorderTeal600
    , xlHoverBorderTeal700
    , xlHoverBorderTeal800
    , xlHoverBorderTeal900
    , xlHoverBorderTransparent
    , xlHoverBorderWhite
    , xlHoverBorderYellow100
    , xlHoverBorderYellow200
    , xlHoverBorderYellow300
    , xlHoverBorderYellow400
    , xlHoverBorderYellow500
    , xlHoverBorderYellow600
    , xlHoverBorderYellow700
    , xlHoverBorderYellow800
    , xlHoverBorderYellow900
    , xlHoverFontBlack
    , xlHoverFontBold
    , xlHoverFontExtrabold
    , xlHoverFontHairline
    , xlHoverFontLight
    , xlHoverFontMedium
    , xlHoverFontNormal
    , xlHoverFontSemibold
    , xlHoverFontThin
    , xlHoverFromBlack
    , xlHoverFromBlue100
    , xlHoverFromBlue200
    , xlHoverFromBlue300
    , xlHoverFromBlue400
    , xlHoverFromBlue500
    , xlHoverFromBlue600
    , xlHoverFromBlue700
    , xlHoverFromBlue800
    , xlHoverFromBlue900
    , xlHoverFromCurrent
    , xlHoverFromGray100
    , xlHoverFromGray200
    , xlHoverFromGray300
    , xlHoverFromGray400
    , xlHoverFromGray500
    , xlHoverFromGray600
    , xlHoverFromGray700
    , xlHoverFromGray800
    , xlHoverFromGray900
    , xlHoverFromGreen100
    , xlHoverFromGreen200
    , xlHoverFromGreen300
    , xlHoverFromGreen400
    , xlHoverFromGreen500
    , xlHoverFromGreen600
    , xlHoverFromGreen700
    , xlHoverFromGreen800
    , xlHoverFromGreen900
    , xlHoverFromIndigo100
    , xlHoverFromIndigo200
    , xlHoverFromIndigo300
    , xlHoverFromIndigo400
    , xlHoverFromIndigo500
    , xlHoverFromIndigo600
    , xlHoverFromIndigo700
    , xlHoverFromIndigo800
    , xlHoverFromIndigo900
    , xlHoverFromOrange100
    , xlHoverFromOrange200
    , xlHoverFromOrange300
    , xlHoverFromOrange400
    , xlHoverFromOrange500
    , xlHoverFromOrange600
    , xlHoverFromOrange700
    , xlHoverFromOrange800
    , xlHoverFromOrange900
    , xlHoverFromPink100
    , xlHoverFromPink200
    , xlHoverFromPink300
    , xlHoverFromPink400
    , xlHoverFromPink500
    , xlHoverFromPink600
    , xlHoverFromPink700
    , xlHoverFromPink800
    , xlHoverFromPink900
    , xlHoverFromPurple100
    , xlHoverFromPurple200
    , xlHoverFromPurple300
    , xlHoverFromPurple400
    , xlHoverFromPurple500
    , xlHoverFromPurple600
    , xlHoverFromPurple700
    , xlHoverFromPurple800
    , xlHoverFromPurple900
    , xlHoverFromRed100
    , xlHoverFromRed200
    , xlHoverFromRed300
    , xlHoverFromRed400
    , xlHoverFromRed500
    , xlHoverFromRed600
    , xlHoverFromRed700
    , xlHoverFromRed800
    , xlHoverFromRed900
    , xlHoverFromTeal100
    , xlHoverFromTeal200
    , xlHoverFromTeal300
    , xlHoverFromTeal400
    , xlHoverFromTeal500
    , xlHoverFromTeal600
    , xlHoverFromTeal700
    , xlHoverFromTeal800
    , xlHoverFromTeal900
    , xlHoverFromTransparent
    , xlHoverFromWhite
    , xlHoverFromYellow100
    , xlHoverFromYellow200
    , xlHoverFromYellow300
    , xlHoverFromYellow400
    , xlHoverFromYellow500
    , xlHoverFromYellow600
    , xlHoverFromYellow700
    , xlHoverFromYellow800
    , xlHoverFromYellow900
    , xlHoverLineThrough
    , xlHoverNegRotate180
    , xlHoverNegRotate45
    , xlHoverNegRotate90
    , xlHoverNegSkewX12
    , xlHoverNegSkewX3
    , xlHoverNegSkewX6
    , xlHoverNegSkewY12
    , xlHoverNegSkewY3
    , xlHoverNegSkewY6
    , xlHoverNegTranslateX1
    , xlHoverNegTranslateX10
    , xlHoverNegTranslateX12
    , xlHoverNegTranslateX16
    , xlHoverNegTranslateX1over2
    , xlHoverNegTranslateX2
    , xlHoverNegTranslateX20
    , xlHoverNegTranslateX24
    , xlHoverNegTranslateX3
    , xlHoverNegTranslateX32
    , xlHoverNegTranslateX4
    , xlHoverNegTranslateX40
    , xlHoverNegTranslateX48
    , xlHoverNegTranslateX5
    , xlHoverNegTranslateX56
    , xlHoverNegTranslateX6
    , xlHoverNegTranslateX64
    , xlHoverNegTranslateX8
    , xlHoverNegTranslateXFull
    , xlHoverNegTranslateXPx
    , xlHoverNegTranslateY1
    , xlHoverNegTranslateY10
    , xlHoverNegTranslateY12
    , xlHoverNegTranslateY16
    , xlHoverNegTranslateY1over2
    , xlHoverNegTranslateY2
    , xlHoverNegTranslateY20
    , xlHoverNegTranslateY24
    , xlHoverNegTranslateY3
    , xlHoverNegTranslateY32
    , xlHoverNegTranslateY4
    , xlHoverNegTranslateY40
    , xlHoverNegTranslateY48
    , xlHoverNegTranslateY5
    , xlHoverNegTranslateY56
    , xlHoverNegTranslateY6
    , xlHoverNegTranslateY64
    , xlHoverNegTranslateY8
    , xlHoverNegTranslateYFull
    , xlHoverNegTranslateYPx
    , xlHoverNoUnderline
    , xlHoverOpacity0
    , xlHoverOpacity100
    , xlHoverOpacity25
    , xlHoverOpacity50
    , xlHoverOpacity75
    , xlHoverRotate0
    , xlHoverRotate180
    , xlHoverRotate45
    , xlHoverRotate90
    , xlHoverScale0
    , xlHoverScale100
    , xlHoverScale105
    , xlHoverScale110
    , xlHoverScale125
    , xlHoverScale150
    , xlHoverScale50
    , xlHoverScale75
    , xlHoverScale90
    , xlHoverScale95
    , xlHoverScaleX0
    , xlHoverScaleX100
    , xlHoverScaleX105
    , xlHoverScaleX110
    , xlHoverScaleX125
    , xlHoverScaleX150
    , xlHoverScaleX50
    , xlHoverScaleX75
    , xlHoverScaleX90
    , xlHoverScaleX95
    , xlHoverScaleY0
    , xlHoverScaleY100
    , xlHoverScaleY105
    , xlHoverScaleY110
    , xlHoverScaleY125
    , xlHoverScaleY150
    , xlHoverScaleY50
    , xlHoverScaleY75
    , xlHoverScaleY90
    , xlHoverScaleY95
    , xlHoverShadow
    , xlHoverShadow2xl
    , xlHoverShadowInner
    , xlHoverShadowLg
    , xlHoverShadowMd
    , xlHoverShadowNone
    , xlHoverShadowOutline
    , xlHoverShadowSm
    , xlHoverShadowXl
    , xlHoverShadowXs
    , xlHoverSkewX0
    , xlHoverSkewX12
    , xlHoverSkewX3
    , xlHoverSkewX6
    , xlHoverSkewY0
    , xlHoverSkewY12
    , xlHoverSkewY3
    , xlHoverSkewY6
    , xlHoverTextBlack
    , xlHoverTextBlue100
    , xlHoverTextBlue200
    , xlHoverTextBlue300
    , xlHoverTextBlue400
    , xlHoverTextBlue500
    , xlHoverTextBlue600
    , xlHoverTextBlue700
    , xlHoverTextBlue800
    , xlHoverTextBlue900
    , xlHoverTextCurrent
    , xlHoverTextGray100
    , xlHoverTextGray200
    , xlHoverTextGray300
    , xlHoverTextGray400
    , xlHoverTextGray500
    , xlHoverTextGray600
    , xlHoverTextGray700
    , xlHoverTextGray800
    , xlHoverTextGray900
    , xlHoverTextGreen100
    , xlHoverTextGreen200
    , xlHoverTextGreen300
    , xlHoverTextGreen400
    , xlHoverTextGreen500
    , xlHoverTextGreen600
    , xlHoverTextGreen700
    , xlHoverTextGreen800
    , xlHoverTextGreen900
    , xlHoverTextIndigo100
    , xlHoverTextIndigo200
    , xlHoverTextIndigo300
    , xlHoverTextIndigo400
    , xlHoverTextIndigo500
    , xlHoverTextIndigo600
    , xlHoverTextIndigo700
    , xlHoverTextIndigo800
    , xlHoverTextIndigo900
    , xlHoverTextOpacity0
    , xlHoverTextOpacity100
    , xlHoverTextOpacity25
    , xlHoverTextOpacity50
    , xlHoverTextOpacity75
    , xlHoverTextOrange100
    , xlHoverTextOrange200
    , xlHoverTextOrange300
    , xlHoverTextOrange400
    , xlHoverTextOrange500
    , xlHoverTextOrange600
    , xlHoverTextOrange700
    , xlHoverTextOrange800
    , xlHoverTextOrange900
    , xlHoverTextPink100
    , xlHoverTextPink200
    , xlHoverTextPink300
    , xlHoverTextPink400
    , xlHoverTextPink500
    , xlHoverTextPink600
    , xlHoverTextPink700
    , xlHoverTextPink800
    , xlHoverTextPink900
    , xlHoverTextPurple100
    , xlHoverTextPurple200
    , xlHoverTextPurple300
    , xlHoverTextPurple400
    , xlHoverTextPurple500
    , xlHoverTextPurple600
    , xlHoverTextPurple700
    , xlHoverTextPurple800
    , xlHoverTextPurple900
    , xlHoverTextRed100
    , xlHoverTextRed200
    , xlHoverTextRed300
    , xlHoverTextRed400
    , xlHoverTextRed500
    , xlHoverTextRed600
    , xlHoverTextRed700
    , xlHoverTextRed800
    , xlHoverTextRed900
    , xlHoverTextTeal100
    , xlHoverTextTeal200
    , xlHoverTextTeal300
    , xlHoverTextTeal400
    , xlHoverTextTeal500
    , xlHoverTextTeal600
    , xlHoverTextTeal700
    , xlHoverTextTeal800
    , xlHoverTextTeal900
    , xlHoverTextTransparent
    , xlHoverTextWhite
    , xlHoverTextYellow100
    , xlHoverTextYellow200
    , xlHoverTextYellow300
    , xlHoverTextYellow400
    , xlHoverTextYellow500
    , xlHoverTextYellow600
    , xlHoverTextYellow700
    , xlHoverTextYellow800
    , xlHoverTextYellow900
    , xlHoverToBlack
    , xlHoverToBlue100
    , xlHoverToBlue200
    , xlHoverToBlue300
    , xlHoverToBlue400
    , xlHoverToBlue500
    , xlHoverToBlue600
    , xlHoverToBlue700
    , xlHoverToBlue800
    , xlHoverToBlue900
    , xlHoverToCurrent
    , xlHoverToGray100
    , xlHoverToGray200
    , xlHoverToGray300
    , xlHoverToGray400
    , xlHoverToGray500
    , xlHoverToGray600
    , xlHoverToGray700
    , xlHoverToGray800
    , xlHoverToGray900
    , xlHoverToGreen100
    , xlHoverToGreen200
    , xlHoverToGreen300
    , xlHoverToGreen400
    , xlHoverToGreen500
    , xlHoverToGreen600
    , xlHoverToGreen700
    , xlHoverToGreen800
    , xlHoverToGreen900
    , xlHoverToIndigo100
    , xlHoverToIndigo200
    , xlHoverToIndigo300
    , xlHoverToIndigo400
    , xlHoverToIndigo500
    , xlHoverToIndigo600
    , xlHoverToIndigo700
    , xlHoverToIndigo800
    , xlHoverToIndigo900
    , xlHoverToOrange100
    , xlHoverToOrange200
    , xlHoverToOrange300
    , xlHoverToOrange400
    , xlHoverToOrange500
    , xlHoverToOrange600
    , xlHoverToOrange700
    , xlHoverToOrange800
    , xlHoverToOrange900
    , xlHoverToPink100
    , xlHoverToPink200
    , xlHoverToPink300
    , xlHoverToPink400
    , xlHoverToPink500
    , xlHoverToPink600
    , xlHoverToPink700
    , xlHoverToPink800
    , xlHoverToPink900
    , xlHoverToPurple100
    , xlHoverToPurple200
    , xlHoverToPurple300
    , xlHoverToPurple400
    , xlHoverToPurple500
    , xlHoverToPurple600
    , xlHoverToPurple700
    , xlHoverToPurple800
    , xlHoverToPurple900
    , xlHoverToRed100
    , xlHoverToRed200
    , xlHoverToRed300
    , xlHoverToRed400
    , xlHoverToRed500
    , xlHoverToRed600
    , xlHoverToRed700
    , xlHoverToRed800
    , xlHoverToRed900
    , xlHoverToTeal100
    , xlHoverToTeal200
    , xlHoverToTeal300
    , xlHoverToTeal400
    , xlHoverToTeal500
    , xlHoverToTeal600
    , xlHoverToTeal700
    , xlHoverToTeal800
    , xlHoverToTeal900
    , xlHoverToTransparent
    , xlHoverToWhite
    , xlHoverToYellow100
    , xlHoverToYellow200
    , xlHoverToYellow300
    , xlHoverToYellow400
    , xlHoverToYellow500
    , xlHoverToYellow600
    , xlHoverToYellow700
    , xlHoverToYellow800
    , xlHoverToYellow900
    , xlHoverTranslateX0
    , xlHoverTranslateX1
    , xlHoverTranslateX10
    , xlHoverTranslateX12
    , xlHoverTranslateX16
    , xlHoverTranslateX1over2
    , xlHoverTranslateX2
    , xlHoverTranslateX20
    , xlHoverTranslateX24
    , xlHoverTranslateX3
    , xlHoverTranslateX32
    , xlHoverTranslateX4
    , xlHoverTranslateX40
    , xlHoverTranslateX48
    , xlHoverTranslateX5
    , xlHoverTranslateX56
    , xlHoverTranslateX6
    , xlHoverTranslateX64
    , xlHoverTranslateX8
    , xlHoverTranslateXFull
    , xlHoverTranslateXPx
    , xlHoverTranslateY0
    , xlHoverTranslateY1
    , xlHoverTranslateY10
    , xlHoverTranslateY12
    , xlHoverTranslateY16
    , xlHoverTranslateY1over2
    , xlHoverTranslateY2
    , xlHoverTranslateY20
    , xlHoverTranslateY24
    , xlHoverTranslateY3
    , xlHoverTranslateY32
    , xlHoverTranslateY4
    , xlHoverTranslateY40
    , xlHoverTranslateY48
    , xlHoverTranslateY5
    , xlHoverTranslateY56
    , xlHoverTranslateY6
    , xlHoverTranslateY64
    , xlHoverTranslateY8
    , xlHoverTranslateYFull
    , xlHoverTranslateYPx
    , xlHoverUnderline
    , xlHoverViaBlack
    , xlHoverViaBlue100
    , xlHoverViaBlue200
    , xlHoverViaBlue300
    , xlHoverViaBlue400
    , xlHoverViaBlue500
    , xlHoverViaBlue600
    , xlHoverViaBlue700
    , xlHoverViaBlue800
    , xlHoverViaBlue900
    , xlHoverViaCurrent
    , xlHoverViaGray100
    , xlHoverViaGray200
    , xlHoverViaGray300
    , xlHoverViaGray400
    , xlHoverViaGray500
    , xlHoverViaGray600
    , xlHoverViaGray700
    , xlHoverViaGray800
    , xlHoverViaGray900
    , xlHoverViaGreen100
    , xlHoverViaGreen200
    , xlHoverViaGreen300
    , xlHoverViaGreen400
    , xlHoverViaGreen500
    , xlHoverViaGreen600
    , xlHoverViaGreen700
    , xlHoverViaGreen800
    , xlHoverViaGreen900
    , xlHoverViaIndigo100
    , xlHoverViaIndigo200
    , xlHoverViaIndigo300
    , xlHoverViaIndigo400
    , xlHoverViaIndigo500
    , xlHoverViaIndigo600
    , xlHoverViaIndigo700
    , xlHoverViaIndigo800
    , xlHoverViaIndigo900
    , xlHoverViaOrange100
    , xlHoverViaOrange200
    , xlHoverViaOrange300
    , xlHoverViaOrange400
    , xlHoverViaOrange500
    , xlHoverViaOrange600
    , xlHoverViaOrange700
    , xlHoverViaOrange800
    , xlHoverViaOrange900
    , xlHoverViaPink100
    , xlHoverViaPink200
    , xlHoverViaPink300
    , xlHoverViaPink400
    , xlHoverViaPink500
    , xlHoverViaPink600
    , xlHoverViaPink700
    , xlHoverViaPink800
    , xlHoverViaPink900
    , xlHoverViaPurple100
    , xlHoverViaPurple200
    , xlHoverViaPurple300
    , xlHoverViaPurple400
    , xlHoverViaPurple500
    , xlHoverViaPurple600
    , xlHoverViaPurple700
    , xlHoverViaPurple800
    , xlHoverViaPurple900
    , xlHoverViaRed100
    , xlHoverViaRed200
    , xlHoverViaRed300
    , xlHoverViaRed400
    , xlHoverViaRed500
    , xlHoverViaRed600
    , xlHoverViaRed700
    , xlHoverViaRed800
    , xlHoverViaRed900
    , xlHoverViaTeal100
    , xlHoverViaTeal200
    , xlHoverViaTeal300
    , xlHoverViaTeal400
    , xlHoverViaTeal500
    , xlHoverViaTeal600
    , xlHoverViaTeal700
    , xlHoverViaTeal800
    , xlHoverViaTeal900
    , xlHoverViaTransparent
    , xlHoverViaWhite
    , xlHoverViaYellow100
    , xlHoverViaYellow200
    , xlHoverViaYellow300
    , xlHoverViaYellow400
    , xlHoverViaYellow500
    , xlHoverViaYellow600
    , xlHoverViaYellow700
    , xlHoverViaYellow800
    , xlHoverViaYellow900
    , xlInline
    , xlInlineBlock
    , xlInlineFlex
    , xlInlineGrid
    , xlInset0
    , xlInsetAuto
    , xlInsetX0
    , xlInsetXAuto
    , xlInsetY0
    , xlInsetYAuto
    , xlInvisible
    , xlItalic
    , xlItemsBaseline
    , xlItemsCenter
    , xlItemsEnd
    , xlItemsStart
    , xlItemsStretch
    , xlJustifyAround
    , xlJustifyBetween
    , xlJustifyCenter
    , xlJustifyEnd
    , xlJustifyEvenly
    , xlJustifyItemsAuto
    , xlJustifyItemsCenter
    , xlJustifyItemsEnd
    , xlJustifyItemsStart
    , xlJustifyItemsStretch
    , xlJustifySelfAuto
    , xlJustifySelfCenter
    , xlJustifySelfEnd
    , xlJustifySelfStart
    , xlJustifySelfStretch
    , xlJustifyStart
    , xlLeading10
    , xlLeading3
    , xlLeading4
    , xlLeading5
    , xlLeading6
    , xlLeading7
    , xlLeading8
    , xlLeading9
    , xlLeadingLoose
    , xlLeadingNone
    , xlLeadingNormal
    , xlLeadingRelaxed
    , xlLeadingSnug
    , xlLeadingTight
    , xlLeft0
    , xlLeftAuto
    , xlLineThrough
    , xlLiningNums
    , xlListDecimal
    , xlListDisc
    , xlListInside
    , xlListNone
    , xlListOutside
    , xlLowercase
    , xlM0
    , xlM1
    , xlM10
    , xlM12
    , xlM16
    , xlM2
    , xlM20
    , xlM24
    , xlM3
    , xlM32
    , xlM4
    , xlM40
    , xlM48
    , xlM5
    , xlM56
    , xlM6
    , xlM64
    , xlM8
    , xlMAuto
    , xlMPx
    , xlMaxHFull
    , xlMaxHScreen
    , xlMaxW2xl
    , xlMaxW3xl
    , xlMaxW4xl
    , xlMaxW5xl
    , xlMaxW6xl
    , xlMaxWFull
    , xlMaxWLg
    , xlMaxWMd
    , xlMaxWNone
    , xlMaxWScreenLg
    , xlMaxWScreenMd
    , xlMaxWScreenSm
    , xlMaxWScreenXl
    , xlMaxWSm
    , xlMaxWXl
    , xlMaxWXs
    , xlMb0
    , xlMb1
    , xlMb10
    , xlMb12
    , xlMb16
    , xlMb2
    , xlMb20
    , xlMb24
    , xlMb3
    , xlMb32
    , xlMb4
    , xlMb40
    , xlMb48
    , xlMb5
    , xlMb56
    , xlMb6
    , xlMb64
    , xlMb8
    , xlMbAuto
    , xlMbPx
    , xlMinH0
    , xlMinHFull
    , xlMinHScreen
    , xlMinW0
    , xlMinWFull
    , xlMl0
    , xlMl1
    , xlMl10
    , xlMl12
    , xlMl16
    , xlMl2
    , xlMl20
    , xlMl24
    , xlMl3
    , xlMl32
    , xlMl4
    , xlMl40
    , xlMl48
    , xlMl5
    , xlMl56
    , xlMl6
    , xlMl64
    , xlMl8
    , xlMlAuto
    , xlMlPx
    , xlMr0
    , xlMr1
    , xlMr10
    , xlMr12
    , xlMr16
    , xlMr2
    , xlMr20
    , xlMr24
    , xlMr3
    , xlMr32
    , xlMr4
    , xlMr40
    , xlMr48
    , xlMr5
    , xlMr56
    , xlMr6
    , xlMr64
    , xlMr8
    , xlMrAuto
    , xlMrPx
    , xlMt0
    , xlMt1
    , xlMt10
    , xlMt12
    , xlMt16
    , xlMt2
    , xlMt20
    , xlMt24
    , xlMt3
    , xlMt32
    , xlMt4
    , xlMt40
    , xlMt48
    , xlMt5
    , xlMt56
    , xlMt6
    , xlMt64
    , xlMt8
    , xlMtAuto
    , xlMtPx
    , xlMx0
    , xlMx1
    , xlMx10
    , xlMx12
    , xlMx16
    , xlMx2
    , xlMx20
    , xlMx24
    , xlMx3
    , xlMx32
    , xlMx4
    , xlMx40
    , xlMx48
    , xlMx5
    , xlMx56
    , xlMx6
    , xlMx64
    , xlMx8
    , xlMxAuto
    , xlMxPx
    , xlMy0
    , xlMy1
    , xlMy10
    , xlMy12
    , xlMy16
    , xlMy2
    , xlMy20
    , xlMy24
    , xlMy3
    , xlMy32
    , xlMy4
    , xlMy40
    , xlMy48
    , xlMy5
    , xlMy56
    , xlMy6
    , xlMy64
    , xlMy8
    , xlMyAuto
    , xlMyPx
    , xlNegM1
    , xlNegM10
    , xlNegM12
    , xlNegM16
    , xlNegM2
    , xlNegM20
    , xlNegM24
    , xlNegM3
    , xlNegM32
    , xlNegM4
    , xlNegM40
    , xlNegM48
    , xlNegM5
    , xlNegM56
    , xlNegM6
    , xlNegM64
    , xlNegM8
    , xlNegMPx
    , xlNegMb1
    , xlNegMb10
    , xlNegMb12
    , xlNegMb16
    , xlNegMb2
    , xlNegMb20
    , xlNegMb24
    , xlNegMb3
    , xlNegMb32
    , xlNegMb4
    , xlNegMb40
    , xlNegMb48
    , xlNegMb5
    , xlNegMb56
    , xlNegMb6
    , xlNegMb64
    , xlNegMb8
    , xlNegMbPx
    , xlNegMl1
    , xlNegMl10
    , xlNegMl12
    , xlNegMl16
    , xlNegMl2
    , xlNegMl20
    , xlNegMl24
    , xlNegMl3
    , xlNegMl32
    , xlNegMl4
    , xlNegMl40
    , xlNegMl48
    , xlNegMl5
    , xlNegMl56
    , xlNegMl6
    , xlNegMl64
    , xlNegMl8
    , xlNegMlPx
    , xlNegMr1
    , xlNegMr10
    , xlNegMr12
    , xlNegMr16
    , xlNegMr2
    , xlNegMr20
    , xlNegMr24
    , xlNegMr3
    , xlNegMr32
    , xlNegMr4
    , xlNegMr40
    , xlNegMr48
    , xlNegMr5
    , xlNegMr56
    , xlNegMr6
    , xlNegMr64
    , xlNegMr8
    , xlNegMrPx
    , xlNegMt1
    , xlNegMt10
    , xlNegMt12
    , xlNegMt16
    , xlNegMt2
    , xlNegMt20
    , xlNegMt24
    , xlNegMt3
    , xlNegMt32
    , xlNegMt4
    , xlNegMt40
    , xlNegMt48
    , xlNegMt5
    , xlNegMt56
    , xlNegMt6
    , xlNegMt64
    , xlNegMt8
    , xlNegMtPx
    , xlNegMx1
    , xlNegMx10
    , xlNegMx12
    , xlNegMx16
    , xlNegMx2
    , xlNegMx20
    , xlNegMx24
    , xlNegMx3
    , xlNegMx32
    , xlNegMx4
    , xlNegMx40
    , xlNegMx48
    , xlNegMx5
    , xlNegMx56
    , xlNegMx6
    , xlNegMx64
    , xlNegMx8
    , xlNegMxPx
    , xlNegMy1
    , xlNegMy10
    , xlNegMy12
    , xlNegMy16
    , xlNegMy2
    , xlNegMy20
    , xlNegMy24
    , xlNegMy3
    , xlNegMy32
    , xlNegMy4
    , xlNegMy40
    , xlNegMy48
    , xlNegMy5
    , xlNegMy56
    , xlNegMy6
    , xlNegMy64
    , xlNegMy8
    , xlNegMyPx
    , xlNegRotate180
    , xlNegRotate45
    , xlNegRotate90
    , xlNegSkewX12
    , xlNegSkewX3
    , xlNegSkewX6
    , xlNegSkewY12
    , xlNegSkewY3
    , xlNegSkewY6
    , xlNegSpaceX1
    , xlNegSpaceX10
    , xlNegSpaceX12
    , xlNegSpaceX16
    , xlNegSpaceX2
    , xlNegSpaceX20
    , xlNegSpaceX24
    , xlNegSpaceX3
    , xlNegSpaceX32
    , xlNegSpaceX4
    , xlNegSpaceX40
    , xlNegSpaceX48
    , xlNegSpaceX5
    , xlNegSpaceX56
    , xlNegSpaceX6
    , xlNegSpaceX64
    , xlNegSpaceX8
    , xlNegSpaceXPx
    , xlNegSpaceY1
    , xlNegSpaceY10
    , xlNegSpaceY12
    , xlNegSpaceY16
    , xlNegSpaceY2
    , xlNegSpaceY20
    , xlNegSpaceY24
    , xlNegSpaceY3
    , xlNegSpaceY32
    , xlNegSpaceY4
    , xlNegSpaceY40
    , xlNegSpaceY48
    , xlNegSpaceY5
    , xlNegSpaceY56
    , xlNegSpaceY6
    , xlNegSpaceY64
    , xlNegSpaceY8
    , xlNegSpaceYPx
    , xlNegTranslateX1
    , xlNegTranslateX10
    , xlNegTranslateX12
    , xlNegTranslateX16
    , xlNegTranslateX1over2
    , xlNegTranslateX2
    , xlNegTranslateX20
    , xlNegTranslateX24
    , xlNegTranslateX3
    , xlNegTranslateX32
    , xlNegTranslateX4
    , xlNegTranslateX40
    , xlNegTranslateX48
    , xlNegTranslateX5
    , xlNegTranslateX56
    , xlNegTranslateX6
    , xlNegTranslateX64
    , xlNegTranslateX8
    , xlNegTranslateXFull
    , xlNegTranslateXPx
    , xlNegTranslateY1
    , xlNegTranslateY10
    , xlNegTranslateY12
    , xlNegTranslateY16
    , xlNegTranslateY1over2
    , xlNegTranslateY2
    , xlNegTranslateY20
    , xlNegTranslateY24
    , xlNegTranslateY3
    , xlNegTranslateY32
    , xlNegTranslateY4
    , xlNegTranslateY40
    , xlNegTranslateY48
    , xlNegTranslateY5
    , xlNegTranslateY56
    , xlNegTranslateY6
    , xlNegTranslateY64
    , xlNegTranslateY8
    , xlNegTranslateYFull
    , xlNegTranslateYPx
    , xlNoUnderline
    , xlNormalCase
    , xlNormalNums
    , xlNotItalic
    , xlNotSrOnly
    , xlObjectBottom
    , xlObjectCenter
    , xlObjectContain
    , xlObjectCover
    , xlObjectFill
    , xlObjectLeft
    , xlObjectLeftBottom
    , xlObjectLeftTop
    , xlObjectNone
    , xlObjectRight
    , xlObjectRightBottom
    , xlObjectRightTop
    , xlObjectScaleDown
    , xlObjectTop
    , xlOldstyleNums
    , xlOpacity0
    , xlOpacity100
    , xlOpacity25
    , xlOpacity50
    , xlOpacity75
    , xlOrder1
    , xlOrder10
    , xlOrder11
    , xlOrder12
    , xlOrder2
    , xlOrder3
    , xlOrder4
    , xlOrder5
    , xlOrder6
    , xlOrder7
    , xlOrder8
    , xlOrder9
    , xlOrderFirst
    , xlOrderLast
    , xlOrderNone
    , xlOrdinal
    , xlOriginBottom
    , xlOriginBottomLeft
    , xlOriginBottomRight
    , xlOriginCenter
    , xlOriginLeft
    , xlOriginRight
    , xlOriginTop
    , xlOriginTopLeft
    , xlOriginTopRight
    , xlOutlineNone
    , xlOverflowAuto
    , xlOverflowHidden
    , xlOverflowScroll
    , xlOverflowVisible
    , xlOverflowXAuto
    , xlOverflowXHidden
    , xlOverflowXScroll
    , xlOverflowXVisible
    , xlOverflowYAuto
    , xlOverflowYHidden
    , xlOverflowYScroll
    , xlOverflowYVisible
    , xlOverscrollAuto
    , xlOverscrollContain
    , xlOverscrollNone
    , xlOverscrollXAuto
    , xlOverscrollXContain
    , xlOverscrollXNone
    , xlOverscrollYAuto
    , xlOverscrollYContain
    , xlOverscrollYNone
    , xlP0
    , xlP1
    , xlP10
    , xlP12
    , xlP16
    , xlP2
    , xlP20
    , xlP24
    , xlP3
    , xlP32
    , xlP4
    , xlP40
    , xlP48
    , xlP5
    , xlP56
    , xlP6
    , xlP64
    , xlP8
    , xlPPx
    , xlPb0
    , xlPb1
    , xlPb10
    , xlPb12
    , xlPb16
    , xlPb2
    , xlPb20
    , xlPb24
    , xlPb3
    , xlPb32
    , xlPb4
    , xlPb40
    , xlPb48
    , xlPb5
    , xlPb56
    , xlPb6
    , xlPb64
    , xlPb8
    , xlPbPx
    , xlPl0
    , xlPl1
    , xlPl10
    , xlPl12
    , xlPl16
    , xlPl2
    , xlPl20
    , xlPl24
    , xlPl3
    , xlPl32
    , xlPl4
    , xlPl40
    , xlPl48
    , xlPl5
    , xlPl56
    , xlPl6
    , xlPl64
    , xlPl8
    , xlPlPx
    , xlPlaceContentAround
    , xlPlaceContentBetween
    , xlPlaceContentCenter
    , xlPlaceContentEnd
    , xlPlaceContentEvenly
    , xlPlaceContentStart
    , xlPlaceContentStretch
    , xlPlaceItemsAuto
    , xlPlaceItemsCenter
    , xlPlaceItemsEnd
    , xlPlaceItemsStart
    , xlPlaceItemsStretch
    , xlPlaceSelfAuto
    , xlPlaceSelfCenter
    , xlPlaceSelfEnd
    , xlPlaceSelfStart
    , xlPlaceSelfStretch
    , xlPlaceholderBlack
    , xlPlaceholderBlue100
    , xlPlaceholderBlue200
    , xlPlaceholderBlue300
    , xlPlaceholderBlue400
    , xlPlaceholderBlue500
    , xlPlaceholderBlue600
    , xlPlaceholderBlue700
    , xlPlaceholderBlue800
    , xlPlaceholderBlue900
    , xlPlaceholderCurrent
    , xlPlaceholderGray100
    , xlPlaceholderGray200
    , xlPlaceholderGray300
    , xlPlaceholderGray400
    , xlPlaceholderGray500
    , xlPlaceholderGray600
    , xlPlaceholderGray700
    , xlPlaceholderGray800
    , xlPlaceholderGray900
    , xlPlaceholderGreen100
    , xlPlaceholderGreen200
    , xlPlaceholderGreen300
    , xlPlaceholderGreen400
    , xlPlaceholderGreen500
    , xlPlaceholderGreen600
    , xlPlaceholderGreen700
    , xlPlaceholderGreen800
    , xlPlaceholderGreen900
    , xlPlaceholderIndigo100
    , xlPlaceholderIndigo200
    , xlPlaceholderIndigo300
    , xlPlaceholderIndigo400
    , xlPlaceholderIndigo500
    , xlPlaceholderIndigo600
    , xlPlaceholderIndigo700
    , xlPlaceholderIndigo800
    , xlPlaceholderIndigo900
    , xlPlaceholderOpacity0
    , xlPlaceholderOpacity100
    , xlPlaceholderOpacity25
    , xlPlaceholderOpacity50
    , xlPlaceholderOpacity75
    , xlPlaceholderOrange100
    , xlPlaceholderOrange200
    , xlPlaceholderOrange300
    , xlPlaceholderOrange400
    , xlPlaceholderOrange500
    , xlPlaceholderOrange600
    , xlPlaceholderOrange700
    , xlPlaceholderOrange800
    , xlPlaceholderOrange900
    , xlPlaceholderPink100
    , xlPlaceholderPink200
    , xlPlaceholderPink300
    , xlPlaceholderPink400
    , xlPlaceholderPink500
    , xlPlaceholderPink600
    , xlPlaceholderPink700
    , xlPlaceholderPink800
    , xlPlaceholderPink900
    , xlPlaceholderPurple100
    , xlPlaceholderPurple200
    , xlPlaceholderPurple300
    , xlPlaceholderPurple400
    , xlPlaceholderPurple500
    , xlPlaceholderPurple600
    , xlPlaceholderPurple700
    , xlPlaceholderPurple800
    , xlPlaceholderPurple900
    , xlPlaceholderRed100
    , xlPlaceholderRed200
    , xlPlaceholderRed300
    , xlPlaceholderRed400
    , xlPlaceholderRed500
    , xlPlaceholderRed600
    , xlPlaceholderRed700
    , xlPlaceholderRed800
    , xlPlaceholderRed900
    , xlPlaceholderTeal100
    , xlPlaceholderTeal200
    , xlPlaceholderTeal300
    , xlPlaceholderTeal400
    , xlPlaceholderTeal500
    , xlPlaceholderTeal600
    , xlPlaceholderTeal700
    , xlPlaceholderTeal800
    , xlPlaceholderTeal900
    , xlPlaceholderTransparent
    , xlPlaceholderWhite
    , xlPlaceholderYellow100
    , xlPlaceholderYellow200
    , xlPlaceholderYellow300
    , xlPlaceholderYellow400
    , xlPlaceholderYellow500
    , xlPlaceholderYellow600
    , xlPlaceholderYellow700
    , xlPlaceholderYellow800
    , xlPlaceholderYellow900
    , xlPointerEventsAuto
    , xlPointerEventsNone
    , xlPr0
    , xlPr1
    , xlPr10
    , xlPr12
    , xlPr16
    , xlPr2
    , xlPr20
    , xlPr24
    , xlPr3
    , xlPr32
    , xlPr4
    , xlPr40
    , xlPr48
    , xlPr5
    , xlPr56
    , xlPr6
    , xlPr64
    , xlPr8
    , xlPrPx
    , xlProportionalNums
    , xlPt0
    , xlPt1
    , xlPt10
    , xlPt12
    , xlPt16
    , xlPt2
    , xlPt20
    , xlPt24
    , xlPt3
    , xlPt32
    , xlPt4
    , xlPt40
    , xlPt48
    , xlPt5
    , xlPt56
    , xlPt6
    , xlPt64
    , xlPt8
    , xlPtPx
    , xlPx0
    , xlPx1
    , xlPx10
    , xlPx12
    , xlPx16
    , xlPx2
    , xlPx20
    , xlPx24
    , xlPx3
    , xlPx32
    , xlPx4
    , xlPx40
    , xlPx48
    , xlPx5
    , xlPx56
    , xlPx6
    , xlPx64
    , xlPx8
    , xlPxPx
    , xlPy0
    , xlPy1
    , xlPy10
    , xlPy12
    , xlPy16
    , xlPy2
    , xlPy20
    , xlPy24
    , xlPy3
    , xlPy32
    , xlPy4
    , xlPy40
    , xlPy48
    , xlPy5
    , xlPy56
    , xlPy6
    , xlPy64
    , xlPy8
    , xlPyPx
    , xlRelative
    , xlResize
    , xlResizeNone
    , xlResizeX
    , xlResizeY
    , xlRight0
    , xlRightAuto
    , xlRotate0
    , xlRotate180
    , xlRotate45
    , xlRotate90
    , xlRounded
    , xlRoundedB
    , xlRoundedBFull
    , xlRoundedBLg
    , xlRoundedBMd
    , xlRoundedBNone
    , xlRoundedBSm
    , xlRoundedBl
    , xlRoundedBlFull
    , xlRoundedBlLg
    , xlRoundedBlMd
    , xlRoundedBlNone
    , xlRoundedBlSm
    , xlRoundedBr
    , xlRoundedBrFull
    , xlRoundedBrLg
    , xlRoundedBrMd
    , xlRoundedBrNone
    , xlRoundedBrSm
    , xlRoundedFull
    , xlRoundedL
    , xlRoundedLFull
    , xlRoundedLLg
    , xlRoundedLMd
    , xlRoundedLNone
    , xlRoundedLSm
    , xlRoundedLg
    , xlRoundedMd
    , xlRoundedNone
    , xlRoundedR
    , xlRoundedRFull
    , xlRoundedRLg
    , xlRoundedRMd
    , xlRoundedRNone
    , xlRoundedRSm
    , xlRoundedSm
    , xlRoundedT
    , xlRoundedTFull
    , xlRoundedTLg
    , xlRoundedTMd
    , xlRoundedTNone
    , xlRoundedTSm
    , xlRoundedTl
    , xlRoundedTlFull
    , xlRoundedTlLg
    , xlRoundedTlMd
    , xlRoundedTlNone
    , xlRoundedTlSm
    , xlRoundedTr
    , xlRoundedTrFull
    , xlRoundedTrLg
    , xlRoundedTrMd
    , xlRoundedTrNone
    , xlRoundedTrSm
    , xlRowAuto
    , xlRowEnd1
    , xlRowEnd2
    , xlRowEnd3
    , xlRowEnd4
    , xlRowEnd5
    , xlRowEnd6
    , xlRowEnd7
    , xlRowEndAuto
    , xlRowSpan1
    , xlRowSpan2
    , xlRowSpan3
    , xlRowSpan4
    , xlRowSpan5
    , xlRowSpan6
    , xlRowStart1
    , xlRowStart2
    , xlRowStart3
    , xlRowStart4
    , xlRowStart5
    , xlRowStart6
    , xlRowStart7
    , xlRowStartAuto
    , xlScale0
    , xlScale100
    , xlScale105
    , xlScale110
    , xlScale125
    , xlScale150
    , xlScale50
    , xlScale75
    , xlScale90
    , xlScale95
    , xlScaleX0
    , xlScaleX100
    , xlScaleX105
    , xlScaleX110
    , xlScaleX125
    , xlScaleX150
    , xlScaleX50
    , xlScaleX75
    , xlScaleX90
    , xlScaleX95
    , xlScaleY0
    , xlScaleY100
    , xlScaleY105
    , xlScaleY110
    , xlScaleY125
    , xlScaleY150
    , xlScaleY50
    , xlScaleY75
    , xlScaleY90
    , xlScaleY95
    , xlScrollingAuto
    , xlScrollingTouch
    , xlSelectAll
    , xlSelectAuto
    , xlSelectNone
    , xlSelectText
    , xlSelfAuto
    , xlSelfCenter
    , xlSelfEnd
    , xlSelfStart
    , xlSelfStretch
    , xlShadow
    , xlShadow2xl
    , xlShadowInner
    , xlShadowLg
    , xlShadowMd
    , xlShadowNone
    , xlShadowOutline
    , xlShadowSm
    , xlShadowXl
    , xlShadowXs
    , xlSkewX0
    , xlSkewX12
    , xlSkewX3
    , xlSkewX6
    , xlSkewY0
    , xlSkewY12
    , xlSkewY3
    , xlSkewY6
    , xlSlashedZero
    , xlSpaceX0
    , xlSpaceX1
    , xlSpaceX10
    , xlSpaceX12
    , xlSpaceX16
    , xlSpaceX2
    , xlSpaceX20
    , xlSpaceX24
    , xlSpaceX3
    , xlSpaceX32
    , xlSpaceX4
    , xlSpaceX40
    , xlSpaceX48
    , xlSpaceX5
    , xlSpaceX56
    , xlSpaceX6
    , xlSpaceX64
    , xlSpaceX8
    , xlSpaceXPx
    , xlSpaceXReverse
    , xlSpaceY0
    , xlSpaceY1
    , xlSpaceY10
    , xlSpaceY12
    , xlSpaceY16
    , xlSpaceY2
    , xlSpaceY20
    , xlSpaceY24
    , xlSpaceY3
    , xlSpaceY32
    , xlSpaceY4
    , xlSpaceY40
    , xlSpaceY48
    , xlSpaceY5
    , xlSpaceY56
    , xlSpaceY6
    , xlSpaceY64
    , xlSpaceY8
    , xlSpaceYPx
    , xlSpaceYReverse
    , xlSrOnly
    , xlStackedFractions
    , xlStatic
    , xlSticky
    , xlStroke0
    , xlStroke1
    , xlStroke2
    , xlStrokeCurrent
    , xlSubpixelAntialiased
    , xlTable
    , xlTableAuto
    , xlTableCaption
    , xlTableCell
    , xlTableColumn
    , xlTableColumnGroup
    , xlTableFixed
    , xlTableFooterGroup
    , xlTableHeaderGroup
    , xlTableRow
    , xlTableRowGroup
    , xlTabularNums
    , xlText2xl
    , xlText3xl
    , xlText4xl
    , xlText5xl
    , xlText6xl
    , xlTextBase
    , xlTextBlack
    , xlTextBlue100
    , xlTextBlue200
    , xlTextBlue300
    , xlTextBlue400
    , xlTextBlue500
    , xlTextBlue600
    , xlTextBlue700
    , xlTextBlue800
    , xlTextBlue900
    , xlTextCenter
    , xlTextCurrent
    , xlTextGray100
    , xlTextGray200
    , xlTextGray300
    , xlTextGray400
    , xlTextGray500
    , xlTextGray600
    , xlTextGray700
    , xlTextGray800
    , xlTextGray900
    , xlTextGreen100
    , xlTextGreen200
    , xlTextGreen300
    , xlTextGreen400
    , xlTextGreen500
    , xlTextGreen600
    , xlTextGreen700
    , xlTextGreen800
    , xlTextGreen900
    , xlTextIndigo100
    , xlTextIndigo200
    , xlTextIndigo300
    , xlTextIndigo400
    , xlTextIndigo500
    , xlTextIndigo600
    , xlTextIndigo700
    , xlTextIndigo800
    , xlTextIndigo900
    , xlTextJustify
    , xlTextLeft
    , xlTextLg
    , xlTextOpacity0
    , xlTextOpacity100
    , xlTextOpacity25
    , xlTextOpacity50
    , xlTextOpacity75
    , xlTextOrange100
    , xlTextOrange200
    , xlTextOrange300
    , xlTextOrange400
    , xlTextOrange500
    , xlTextOrange600
    , xlTextOrange700
    , xlTextOrange800
    , xlTextOrange900
    , xlTextPink100
    , xlTextPink200
    , xlTextPink300
    , xlTextPink400
    , xlTextPink500
    , xlTextPink600
    , xlTextPink700
    , xlTextPink800
    , xlTextPink900
    , xlTextPurple100
    , xlTextPurple200
    , xlTextPurple300
    , xlTextPurple400
    , xlTextPurple500
    , xlTextPurple600
    , xlTextPurple700
    , xlTextPurple800
    , xlTextPurple900
    , xlTextRed100
    , xlTextRed200
    , xlTextRed300
    , xlTextRed400
    , xlTextRed500
    , xlTextRed600
    , xlTextRed700
    , xlTextRed800
    , xlTextRed900
    , xlTextRight
    , xlTextSm
    , xlTextTeal100
    , xlTextTeal200
    , xlTextTeal300
    , xlTextTeal400
    , xlTextTeal500
    , xlTextTeal600
    , xlTextTeal700
    , xlTextTeal800
    , xlTextTeal900
    , xlTextTransparent
    , xlTextWhite
    , xlTextXl
    , xlTextXs
    , xlTextYellow100
    , xlTextYellow200
    , xlTextYellow300
    , xlTextYellow400
    , xlTextYellow500
    , xlTextYellow600
    , xlTextYellow700
    , xlTextYellow800
    , xlTextYellow900
    , xlToBlack
    , xlToBlue100
    , xlToBlue200
    , xlToBlue300
    , xlToBlue400
    , xlToBlue500
    , xlToBlue600
    , xlToBlue700
    , xlToBlue800
    , xlToBlue900
    , xlToCurrent
    , xlToGray100
    , xlToGray200
    , xlToGray300
    , xlToGray400
    , xlToGray500
    , xlToGray600
    , xlToGray700
    , xlToGray800
    , xlToGray900
    , xlToGreen100
    , xlToGreen200
    , xlToGreen300
    , xlToGreen400
    , xlToGreen500
    , xlToGreen600
    , xlToGreen700
    , xlToGreen800
    , xlToGreen900
    , xlToIndigo100
    , xlToIndigo200
    , xlToIndigo300
    , xlToIndigo400
    , xlToIndigo500
    , xlToIndigo600
    , xlToIndigo700
    , xlToIndigo800
    , xlToIndigo900
    , xlToOrange100
    , xlToOrange200
    , xlToOrange300
    , xlToOrange400
    , xlToOrange500
    , xlToOrange600
    , xlToOrange700
    , xlToOrange800
    , xlToOrange900
    , xlToPink100
    , xlToPink200
    , xlToPink300
    , xlToPink400
    , xlToPink500
    , xlToPink600
    , xlToPink700
    , xlToPink800
    , xlToPink900
    , xlToPurple100
    , xlToPurple200
    , xlToPurple300
    , xlToPurple400
    , xlToPurple500
    , xlToPurple600
    , xlToPurple700
    , xlToPurple800
    , xlToPurple900
    , xlToRed100
    , xlToRed200
    , xlToRed300
    , xlToRed400
    , xlToRed500
    , xlToRed600
    , xlToRed700
    , xlToRed800
    , xlToRed900
    , xlToTeal100
    , xlToTeal200
    , xlToTeal300
    , xlToTeal400
    , xlToTeal500
    , xlToTeal600
    , xlToTeal700
    , xlToTeal800
    , xlToTeal900
    , xlToTransparent
    , xlToWhite
    , xlToYellow100
    , xlToYellow200
    , xlToYellow300
    , xlToYellow400
    , xlToYellow500
    , xlToYellow600
    , xlToYellow700
    , xlToYellow800
    , xlToYellow900
    , xlTop0
    , xlTopAuto
    , xlTrackingNormal
    , xlTrackingTight
    , xlTrackingTighter
    , xlTrackingWide
    , xlTrackingWider
    , xlTrackingWidest
    , xlTransform
    , xlTransformNone
    , xlTransition
    , xlTransitionAll
    , xlTransitionColors
    , xlTransitionNone
    , xlTransitionOpacity
    , xlTransitionShadow
    , xlTransitionTransform
    , xlTranslateX0
    , xlTranslateX1
    , xlTranslateX10
    , xlTranslateX12
    , xlTranslateX16
    , xlTranslateX1over2
    , xlTranslateX2
    , xlTranslateX20
    , xlTranslateX24
    , xlTranslateX3
    , xlTranslateX32
    , xlTranslateX4
    , xlTranslateX40
    , xlTranslateX48
    , xlTranslateX5
    , xlTranslateX56
    , xlTranslateX6
    , xlTranslateX64
    , xlTranslateX8
    , xlTranslateXFull
    , xlTranslateXPx
    , xlTranslateY0
    , xlTranslateY1
    , xlTranslateY10
    , xlTranslateY12
    , xlTranslateY16
    , xlTranslateY1over2
    , xlTranslateY2
    , xlTranslateY20
    , xlTranslateY24
    , xlTranslateY3
    , xlTranslateY32
    , xlTranslateY4
    , xlTranslateY40
    , xlTranslateY48
    , xlTranslateY5
    , xlTranslateY56
    , xlTranslateY6
    , xlTranslateY64
    , xlTranslateY8
    , xlTranslateYFull
    , xlTranslateYPx
    , xlTruncate
    , xlUnderline
    , xlUppercase
    , xlViaBlack
    , xlViaBlue100
    , xlViaBlue200
    , xlViaBlue300
    , xlViaBlue400
    , xlViaBlue500
    , xlViaBlue600
    , xlViaBlue700
    , xlViaBlue800
    , xlViaBlue900
    , xlViaCurrent
    , xlViaGray100
    , xlViaGray200
    , xlViaGray300
    , xlViaGray400
    , xlViaGray500
    , xlViaGray600
    , xlViaGray700
    , xlViaGray800
    , xlViaGray900
    , xlViaGreen100
    , xlViaGreen200
    , xlViaGreen300
    , xlViaGreen400
    , xlViaGreen500
    , xlViaGreen600
    , xlViaGreen700
    , xlViaGreen800
    , xlViaGreen900
    , xlViaIndigo100
    , xlViaIndigo200
    , xlViaIndigo300
    , xlViaIndigo400
    , xlViaIndigo500
    , xlViaIndigo600
    , xlViaIndigo700
    , xlViaIndigo800
    , xlViaIndigo900
    , xlViaOrange100
    , xlViaOrange200
    , xlViaOrange300
    , xlViaOrange400
    , xlViaOrange500
    , xlViaOrange600
    , xlViaOrange700
    , xlViaOrange800
    , xlViaOrange900
    , xlViaPink100
    , xlViaPink200
    , xlViaPink300
    , xlViaPink400
    , xlViaPink500
    , xlViaPink600
    , xlViaPink700
    , xlViaPink800
    , xlViaPink900
    , xlViaPurple100
    , xlViaPurple200
    , xlViaPurple300
    , xlViaPurple400
    , xlViaPurple500
    , xlViaPurple600
    , xlViaPurple700
    , xlViaPurple800
    , xlViaPurple900
    , xlViaRed100
    , xlViaRed200
    , xlViaRed300
    , xlViaRed400
    , xlViaRed500
    , xlViaRed600
    , xlViaRed700
    , xlViaRed800
    , xlViaRed900
    , xlViaTeal100
    , xlViaTeal200
    , xlViaTeal300
    , xlViaTeal400
    , xlViaTeal500
    , xlViaTeal600
    , xlViaTeal700
    , xlViaTeal800
    , xlViaTeal900
    , xlViaTransparent
    , xlViaWhite
    , xlViaYellow100
    , xlViaYellow200
    , xlViaYellow300
    , xlViaYellow400
    , xlViaYellow500
    , xlViaYellow600
    , xlViaYellow700
    , xlViaYellow800
    , xlViaYellow900
    , xlVisible
    , xlW0
    , xlW1
    , xlW10
    , xlW10over12
    , xlW11over12
    , xlW12
    , xlW16
    , xlW1over12
    , xlW1over2
    , xlW1over3
    , xlW1over4
    , xlW1over5
    , xlW1over6
    , xlW2
    , xlW20
    , xlW24
    , xlW2over12
    , xlW2over3
    , xlW2over4
    , xlW2over5
    , xlW2over6
    , xlW3
    , xlW32
    , xlW3over12
    , xlW3over4
    , xlW3over5
    , xlW3over6
    , xlW4
    , xlW40
    , xlW48
    , xlW4over12
    , xlW4over5
    , xlW4over6
    , xlW5
    , xlW56
    , xlW5over12
    , xlW5over6
    , xlW6
    , xlW64
    , xlW6over12
    , xlW7over12
    , xlW8
    , xlW8over12
    , xlW9over12
    , xlWAuto
    , xlWFull
    , xlWPx
    , xlWScreen
    , xlWhitespaceNoWrap
    , xlWhitespaceNormal
    , xlWhitespacePre
    , xlWhitespacePreLine
    , xlWhitespacePreWrap
    , xlZ0
    , xlZ10
    , xlZ20
    , xlZ30
    , xlZ40
    , xlZ50
    , xlZAuto
    , z0
    , z10
    , z20
    , z30
    , z40
    , z50
    , zAuto
    )

import Html
import Html.Attributes as A


classList : List ( Html.Attribute msg, Bool ) -> List (Html.Attribute msg)
classList classes =
    List.map Tuple.first <| List.filter Tuple.second classes


container : Html.Attribute msg
container =
    A.class "container"


spaceY0 : Html.Attribute msg
spaceY0 =
    A.class "space-y-0"


spaceX0 : Html.Attribute msg
spaceX0 =
    A.class "space-x-0"


spaceY1 : Html.Attribute msg
spaceY1 =
    A.class "space-y-1"


spaceX1 : Html.Attribute msg
spaceX1 =
    A.class "space-x-1"


spaceY2 : Html.Attribute msg
spaceY2 =
    A.class "space-y-2"


spaceX2 : Html.Attribute msg
spaceX2 =
    A.class "space-x-2"


spaceY3 : Html.Attribute msg
spaceY3 =
    A.class "space-y-3"


spaceX3 : Html.Attribute msg
spaceX3 =
    A.class "space-x-3"


spaceY4 : Html.Attribute msg
spaceY4 =
    A.class "space-y-4"


spaceX4 : Html.Attribute msg
spaceX4 =
    A.class "space-x-4"


spaceY5 : Html.Attribute msg
spaceY5 =
    A.class "space-y-5"


spaceX5 : Html.Attribute msg
spaceX5 =
    A.class "space-x-5"


spaceY6 : Html.Attribute msg
spaceY6 =
    A.class "space-y-6"


spaceX6 : Html.Attribute msg
spaceX6 =
    A.class "space-x-6"


spaceY8 : Html.Attribute msg
spaceY8 =
    A.class "space-y-8"


spaceX8 : Html.Attribute msg
spaceX8 =
    A.class "space-x-8"


spaceY10 : Html.Attribute msg
spaceY10 =
    A.class "space-y-10"


spaceX10 : Html.Attribute msg
spaceX10 =
    A.class "space-x-10"


spaceY12 : Html.Attribute msg
spaceY12 =
    A.class "space-y-12"


spaceX12 : Html.Attribute msg
spaceX12 =
    A.class "space-x-12"


spaceY16 : Html.Attribute msg
spaceY16 =
    A.class "space-y-16"


spaceX16 : Html.Attribute msg
spaceX16 =
    A.class "space-x-16"


spaceY20 : Html.Attribute msg
spaceY20 =
    A.class "space-y-20"


spaceX20 : Html.Attribute msg
spaceX20 =
    A.class "space-x-20"


spaceY24 : Html.Attribute msg
spaceY24 =
    A.class "space-y-24"


spaceX24 : Html.Attribute msg
spaceX24 =
    A.class "space-x-24"


spaceY32 : Html.Attribute msg
spaceY32 =
    A.class "space-y-32"


spaceX32 : Html.Attribute msg
spaceX32 =
    A.class "space-x-32"


spaceY40 : Html.Attribute msg
spaceY40 =
    A.class "space-y-40"


spaceX40 : Html.Attribute msg
spaceX40 =
    A.class "space-x-40"


spaceY48 : Html.Attribute msg
spaceY48 =
    A.class "space-y-48"


spaceX48 : Html.Attribute msg
spaceX48 =
    A.class "space-x-48"


spaceY56 : Html.Attribute msg
spaceY56 =
    A.class "space-y-56"


spaceX56 : Html.Attribute msg
spaceX56 =
    A.class "space-x-56"


spaceY64 : Html.Attribute msg
spaceY64 =
    A.class "space-y-64"


spaceX64 : Html.Attribute msg
spaceX64 =
    A.class "space-x-64"


spaceYPx : Html.Attribute msg
spaceYPx =
    A.class "space-y-px"


spaceXPx : Html.Attribute msg
spaceXPx =
    A.class "space-x-px"


negSpaceY1 : Html.Attribute msg
negSpaceY1 =
    A.class "-space-y-1"


negSpaceX1 : Html.Attribute msg
negSpaceX1 =
    A.class "-space-x-1"


negSpaceY2 : Html.Attribute msg
negSpaceY2 =
    A.class "-space-y-2"


negSpaceX2 : Html.Attribute msg
negSpaceX2 =
    A.class "-space-x-2"


negSpaceY3 : Html.Attribute msg
negSpaceY3 =
    A.class "-space-y-3"


negSpaceX3 : Html.Attribute msg
negSpaceX3 =
    A.class "-space-x-3"


negSpaceY4 : Html.Attribute msg
negSpaceY4 =
    A.class "-space-y-4"


negSpaceX4 : Html.Attribute msg
negSpaceX4 =
    A.class "-space-x-4"


negSpaceY5 : Html.Attribute msg
negSpaceY5 =
    A.class "-space-y-5"


negSpaceX5 : Html.Attribute msg
negSpaceX5 =
    A.class "-space-x-5"


negSpaceY6 : Html.Attribute msg
negSpaceY6 =
    A.class "-space-y-6"


negSpaceX6 : Html.Attribute msg
negSpaceX6 =
    A.class "-space-x-6"


negSpaceY8 : Html.Attribute msg
negSpaceY8 =
    A.class "-space-y-8"


negSpaceX8 : Html.Attribute msg
negSpaceX8 =
    A.class "-space-x-8"


negSpaceY10 : Html.Attribute msg
negSpaceY10 =
    A.class "-space-y-10"


negSpaceX10 : Html.Attribute msg
negSpaceX10 =
    A.class "-space-x-10"


negSpaceY12 : Html.Attribute msg
negSpaceY12 =
    A.class "-space-y-12"


negSpaceX12 : Html.Attribute msg
negSpaceX12 =
    A.class "-space-x-12"


negSpaceY16 : Html.Attribute msg
negSpaceY16 =
    A.class "-space-y-16"


negSpaceX16 : Html.Attribute msg
negSpaceX16 =
    A.class "-space-x-16"


negSpaceY20 : Html.Attribute msg
negSpaceY20 =
    A.class "-space-y-20"


negSpaceX20 : Html.Attribute msg
negSpaceX20 =
    A.class "-space-x-20"


negSpaceY24 : Html.Attribute msg
negSpaceY24 =
    A.class "-space-y-24"


negSpaceX24 : Html.Attribute msg
negSpaceX24 =
    A.class "-space-x-24"


negSpaceY32 : Html.Attribute msg
negSpaceY32 =
    A.class "-space-y-32"


negSpaceX32 : Html.Attribute msg
negSpaceX32 =
    A.class "-space-x-32"


negSpaceY40 : Html.Attribute msg
negSpaceY40 =
    A.class "-space-y-40"


negSpaceX40 : Html.Attribute msg
negSpaceX40 =
    A.class "-space-x-40"


negSpaceY48 : Html.Attribute msg
negSpaceY48 =
    A.class "-space-y-48"


negSpaceX48 : Html.Attribute msg
negSpaceX48 =
    A.class "-space-x-48"


negSpaceY56 : Html.Attribute msg
negSpaceY56 =
    A.class "-space-y-56"


negSpaceX56 : Html.Attribute msg
negSpaceX56 =
    A.class "-space-x-56"


negSpaceY64 : Html.Attribute msg
negSpaceY64 =
    A.class "-space-y-64"


negSpaceX64 : Html.Attribute msg
negSpaceX64 =
    A.class "-space-x-64"


negSpaceYPx : Html.Attribute msg
negSpaceYPx =
    A.class "-space-y-px"


negSpaceXPx : Html.Attribute msg
negSpaceXPx =
    A.class "-space-x-px"


spaceYReverse : Html.Attribute msg
spaceYReverse =
    A.class "space-y-reverse"


spaceXReverse : Html.Attribute msg
spaceXReverse =
    A.class "space-x-reverse"


divideY0 : Html.Attribute msg
divideY0 =
    A.class "divide-y-0"


divideX0 : Html.Attribute msg
divideX0 =
    A.class "divide-x-0"


divideY2 : Html.Attribute msg
divideY2 =
    A.class "divide-y-2"


divideX2 : Html.Attribute msg
divideX2 =
    A.class "divide-x-2"


divideY4 : Html.Attribute msg
divideY4 =
    A.class "divide-y-4"


divideX4 : Html.Attribute msg
divideX4 =
    A.class "divide-x-4"


divideY8 : Html.Attribute msg
divideY8 =
    A.class "divide-y-8"


divideX8 : Html.Attribute msg
divideX8 =
    A.class "divide-x-8"


divideY : Html.Attribute msg
divideY =
    A.class "divide-y"


divideX : Html.Attribute msg
divideX =
    A.class "divide-x"


divideYReverse : Html.Attribute msg
divideYReverse =
    A.class "divide-y-reverse"


divideXReverse : Html.Attribute msg
divideXReverse =
    A.class "divide-x-reverse"


divideTransparent : Html.Attribute msg
divideTransparent =
    A.class "divide-transparent"


divideCurrent : Html.Attribute msg
divideCurrent =
    A.class "divide-current"


divideBlack : Html.Attribute msg
divideBlack =
    A.class "divide-black"


divideWhite : Html.Attribute msg
divideWhite =
    A.class "divide-white"


divideGray100 : Html.Attribute msg
divideGray100 =
    A.class "divide-gray-100"


divideGray200 : Html.Attribute msg
divideGray200 =
    A.class "divide-gray-200"


divideGray300 : Html.Attribute msg
divideGray300 =
    A.class "divide-gray-300"


divideGray400 : Html.Attribute msg
divideGray400 =
    A.class "divide-gray-400"


divideGray500 : Html.Attribute msg
divideGray500 =
    A.class "divide-gray-500"


divideGray600 : Html.Attribute msg
divideGray600 =
    A.class "divide-gray-600"


divideGray700 : Html.Attribute msg
divideGray700 =
    A.class "divide-gray-700"


divideGray800 : Html.Attribute msg
divideGray800 =
    A.class "divide-gray-800"


divideGray900 : Html.Attribute msg
divideGray900 =
    A.class "divide-gray-900"


divideRed100 : Html.Attribute msg
divideRed100 =
    A.class "divide-red-100"


divideRed200 : Html.Attribute msg
divideRed200 =
    A.class "divide-red-200"


divideRed300 : Html.Attribute msg
divideRed300 =
    A.class "divide-red-300"


divideRed400 : Html.Attribute msg
divideRed400 =
    A.class "divide-red-400"


divideRed500 : Html.Attribute msg
divideRed500 =
    A.class "divide-red-500"


divideRed600 : Html.Attribute msg
divideRed600 =
    A.class "divide-red-600"


divideRed700 : Html.Attribute msg
divideRed700 =
    A.class "divide-red-700"


divideRed800 : Html.Attribute msg
divideRed800 =
    A.class "divide-red-800"


divideRed900 : Html.Attribute msg
divideRed900 =
    A.class "divide-red-900"


divideOrange100 : Html.Attribute msg
divideOrange100 =
    A.class "divide-orange-100"


divideOrange200 : Html.Attribute msg
divideOrange200 =
    A.class "divide-orange-200"


divideOrange300 : Html.Attribute msg
divideOrange300 =
    A.class "divide-orange-300"


divideOrange400 : Html.Attribute msg
divideOrange400 =
    A.class "divide-orange-400"


divideOrange500 : Html.Attribute msg
divideOrange500 =
    A.class "divide-orange-500"


divideOrange600 : Html.Attribute msg
divideOrange600 =
    A.class "divide-orange-600"


divideOrange700 : Html.Attribute msg
divideOrange700 =
    A.class "divide-orange-700"


divideOrange800 : Html.Attribute msg
divideOrange800 =
    A.class "divide-orange-800"


divideOrange900 : Html.Attribute msg
divideOrange900 =
    A.class "divide-orange-900"


divideYellow100 : Html.Attribute msg
divideYellow100 =
    A.class "divide-yellow-100"


divideYellow200 : Html.Attribute msg
divideYellow200 =
    A.class "divide-yellow-200"


divideYellow300 : Html.Attribute msg
divideYellow300 =
    A.class "divide-yellow-300"


divideYellow400 : Html.Attribute msg
divideYellow400 =
    A.class "divide-yellow-400"


divideYellow500 : Html.Attribute msg
divideYellow500 =
    A.class "divide-yellow-500"


divideYellow600 : Html.Attribute msg
divideYellow600 =
    A.class "divide-yellow-600"


divideYellow700 : Html.Attribute msg
divideYellow700 =
    A.class "divide-yellow-700"


divideYellow800 : Html.Attribute msg
divideYellow800 =
    A.class "divide-yellow-800"


divideYellow900 : Html.Attribute msg
divideYellow900 =
    A.class "divide-yellow-900"


divideGreen100 : Html.Attribute msg
divideGreen100 =
    A.class "divide-green-100"


divideGreen200 : Html.Attribute msg
divideGreen200 =
    A.class "divide-green-200"


divideGreen300 : Html.Attribute msg
divideGreen300 =
    A.class "divide-green-300"


divideGreen400 : Html.Attribute msg
divideGreen400 =
    A.class "divide-green-400"


divideGreen500 : Html.Attribute msg
divideGreen500 =
    A.class "divide-green-500"


divideGreen600 : Html.Attribute msg
divideGreen600 =
    A.class "divide-green-600"


divideGreen700 : Html.Attribute msg
divideGreen700 =
    A.class "divide-green-700"


divideGreen800 : Html.Attribute msg
divideGreen800 =
    A.class "divide-green-800"


divideGreen900 : Html.Attribute msg
divideGreen900 =
    A.class "divide-green-900"


divideTeal100 : Html.Attribute msg
divideTeal100 =
    A.class "divide-teal-100"


divideTeal200 : Html.Attribute msg
divideTeal200 =
    A.class "divide-teal-200"


divideTeal300 : Html.Attribute msg
divideTeal300 =
    A.class "divide-teal-300"


divideTeal400 : Html.Attribute msg
divideTeal400 =
    A.class "divide-teal-400"


divideTeal500 : Html.Attribute msg
divideTeal500 =
    A.class "divide-teal-500"


divideTeal600 : Html.Attribute msg
divideTeal600 =
    A.class "divide-teal-600"


divideTeal700 : Html.Attribute msg
divideTeal700 =
    A.class "divide-teal-700"


divideTeal800 : Html.Attribute msg
divideTeal800 =
    A.class "divide-teal-800"


divideTeal900 : Html.Attribute msg
divideTeal900 =
    A.class "divide-teal-900"


divideBlue100 : Html.Attribute msg
divideBlue100 =
    A.class "divide-blue-100"


divideBlue200 : Html.Attribute msg
divideBlue200 =
    A.class "divide-blue-200"


divideBlue300 : Html.Attribute msg
divideBlue300 =
    A.class "divide-blue-300"


divideBlue400 : Html.Attribute msg
divideBlue400 =
    A.class "divide-blue-400"


divideBlue500 : Html.Attribute msg
divideBlue500 =
    A.class "divide-blue-500"


divideBlue600 : Html.Attribute msg
divideBlue600 =
    A.class "divide-blue-600"


divideBlue700 : Html.Attribute msg
divideBlue700 =
    A.class "divide-blue-700"


divideBlue800 : Html.Attribute msg
divideBlue800 =
    A.class "divide-blue-800"


divideBlue900 : Html.Attribute msg
divideBlue900 =
    A.class "divide-blue-900"


divideIndigo100 : Html.Attribute msg
divideIndigo100 =
    A.class "divide-indigo-100"


divideIndigo200 : Html.Attribute msg
divideIndigo200 =
    A.class "divide-indigo-200"


divideIndigo300 : Html.Attribute msg
divideIndigo300 =
    A.class "divide-indigo-300"


divideIndigo400 : Html.Attribute msg
divideIndigo400 =
    A.class "divide-indigo-400"


divideIndigo500 : Html.Attribute msg
divideIndigo500 =
    A.class "divide-indigo-500"


divideIndigo600 : Html.Attribute msg
divideIndigo600 =
    A.class "divide-indigo-600"


divideIndigo700 : Html.Attribute msg
divideIndigo700 =
    A.class "divide-indigo-700"


divideIndigo800 : Html.Attribute msg
divideIndigo800 =
    A.class "divide-indigo-800"


divideIndigo900 : Html.Attribute msg
divideIndigo900 =
    A.class "divide-indigo-900"


dividePurple100 : Html.Attribute msg
dividePurple100 =
    A.class "divide-purple-100"


dividePurple200 : Html.Attribute msg
dividePurple200 =
    A.class "divide-purple-200"


dividePurple300 : Html.Attribute msg
dividePurple300 =
    A.class "divide-purple-300"


dividePurple400 : Html.Attribute msg
dividePurple400 =
    A.class "divide-purple-400"


dividePurple500 : Html.Attribute msg
dividePurple500 =
    A.class "divide-purple-500"


dividePurple600 : Html.Attribute msg
dividePurple600 =
    A.class "divide-purple-600"


dividePurple700 : Html.Attribute msg
dividePurple700 =
    A.class "divide-purple-700"


dividePurple800 : Html.Attribute msg
dividePurple800 =
    A.class "divide-purple-800"


dividePurple900 : Html.Attribute msg
dividePurple900 =
    A.class "divide-purple-900"


dividePink100 : Html.Attribute msg
dividePink100 =
    A.class "divide-pink-100"


dividePink200 : Html.Attribute msg
dividePink200 =
    A.class "divide-pink-200"


dividePink300 : Html.Attribute msg
dividePink300 =
    A.class "divide-pink-300"


dividePink400 : Html.Attribute msg
dividePink400 =
    A.class "divide-pink-400"


dividePink500 : Html.Attribute msg
dividePink500 =
    A.class "divide-pink-500"


dividePink600 : Html.Attribute msg
dividePink600 =
    A.class "divide-pink-600"


dividePink700 : Html.Attribute msg
dividePink700 =
    A.class "divide-pink-700"


dividePink800 : Html.Attribute msg
dividePink800 =
    A.class "divide-pink-800"


dividePink900 : Html.Attribute msg
dividePink900 =
    A.class "divide-pink-900"


divideSolid : Html.Attribute msg
divideSolid =
    A.class "divide-solid"


divideDashed : Html.Attribute msg
divideDashed =
    A.class "divide-dashed"


divideDotted : Html.Attribute msg
divideDotted =
    A.class "divide-dotted"


divideDouble : Html.Attribute msg
divideDouble =
    A.class "divide-double"


divideNone : Html.Attribute msg
divideNone =
    A.class "divide-none"


divideOpacity0 : Html.Attribute msg
divideOpacity0 =
    A.class "divide-opacity-0"


divideOpacity25 : Html.Attribute msg
divideOpacity25 =
    A.class "divide-opacity-25"


divideOpacity50 : Html.Attribute msg
divideOpacity50 =
    A.class "divide-opacity-50"


divideOpacity75 : Html.Attribute msg
divideOpacity75 =
    A.class "divide-opacity-75"


divideOpacity100 : Html.Attribute msg
divideOpacity100 =
    A.class "divide-opacity-100"


srOnly : Html.Attribute msg
srOnly =
    A.class "sr-only"


notSrOnly : Html.Attribute msg
notSrOnly =
    A.class "not-sr-only"


focusSrOnly : Html.Attribute msg
focusSrOnly =
    A.class "focus:sr-only"


focusNotSrOnly : Html.Attribute msg
focusNotSrOnly =
    A.class "focus:not-sr-only"


appearanceNone : Html.Attribute msg
appearanceNone =
    A.class "appearance-none"


bgFixed : Html.Attribute msg
bgFixed =
    A.class "bg-fixed"


bgLocal : Html.Attribute msg
bgLocal =
    A.class "bg-local"


bgScroll : Html.Attribute msg
bgScroll =
    A.class "bg-scroll"


bgClipBorder : Html.Attribute msg
bgClipBorder =
    A.class "bg-clip-border"


bgClipPadding : Html.Attribute msg
bgClipPadding =
    A.class "bg-clip-padding"


bgClipContent : Html.Attribute msg
bgClipContent =
    A.class "bg-clip-content"


bgClipText : Html.Attribute msg
bgClipText =
    A.class "bg-clip-text"


bgTransparent : Html.Attribute msg
bgTransparent =
    A.class "bg-transparent"


bgCurrent : Html.Attribute msg
bgCurrent =
    A.class "bg-current"


bgBlack : Html.Attribute msg
bgBlack =
    A.class "bg-black"


bgWhite : Html.Attribute msg
bgWhite =
    A.class "bg-white"


bgGray100 : Html.Attribute msg
bgGray100 =
    A.class "bg-gray-100"


bgGray200 : Html.Attribute msg
bgGray200 =
    A.class "bg-gray-200"


bgGray300 : Html.Attribute msg
bgGray300 =
    A.class "bg-gray-300"


bgGray400 : Html.Attribute msg
bgGray400 =
    A.class "bg-gray-400"


bgGray500 : Html.Attribute msg
bgGray500 =
    A.class "bg-gray-500"


bgGray600 : Html.Attribute msg
bgGray600 =
    A.class "bg-gray-600"


bgGray700 : Html.Attribute msg
bgGray700 =
    A.class "bg-gray-700"


bgGray800 : Html.Attribute msg
bgGray800 =
    A.class "bg-gray-800"


bgGray900 : Html.Attribute msg
bgGray900 =
    A.class "bg-gray-900"


bgRed100 : Html.Attribute msg
bgRed100 =
    A.class "bg-red-100"


bgRed200 : Html.Attribute msg
bgRed200 =
    A.class "bg-red-200"


bgRed300 : Html.Attribute msg
bgRed300 =
    A.class "bg-red-300"


bgRed400 : Html.Attribute msg
bgRed400 =
    A.class "bg-red-400"


bgRed500 : Html.Attribute msg
bgRed500 =
    A.class "bg-red-500"


bgRed600 : Html.Attribute msg
bgRed600 =
    A.class "bg-red-600"


bgRed700 : Html.Attribute msg
bgRed700 =
    A.class "bg-red-700"


bgRed800 : Html.Attribute msg
bgRed800 =
    A.class "bg-red-800"


bgRed900 : Html.Attribute msg
bgRed900 =
    A.class "bg-red-900"


bgOrange100 : Html.Attribute msg
bgOrange100 =
    A.class "bg-orange-100"


bgOrange200 : Html.Attribute msg
bgOrange200 =
    A.class "bg-orange-200"


bgOrange300 : Html.Attribute msg
bgOrange300 =
    A.class "bg-orange-300"


bgOrange400 : Html.Attribute msg
bgOrange400 =
    A.class "bg-orange-400"


bgOrange500 : Html.Attribute msg
bgOrange500 =
    A.class "bg-orange-500"


bgOrange600 : Html.Attribute msg
bgOrange600 =
    A.class "bg-orange-600"


bgOrange700 : Html.Attribute msg
bgOrange700 =
    A.class "bg-orange-700"


bgOrange800 : Html.Attribute msg
bgOrange800 =
    A.class "bg-orange-800"


bgOrange900 : Html.Attribute msg
bgOrange900 =
    A.class "bg-orange-900"


bgYellow100 : Html.Attribute msg
bgYellow100 =
    A.class "bg-yellow-100"


bgYellow200 : Html.Attribute msg
bgYellow200 =
    A.class "bg-yellow-200"


bgYellow300 : Html.Attribute msg
bgYellow300 =
    A.class "bg-yellow-300"


bgYellow400 : Html.Attribute msg
bgYellow400 =
    A.class "bg-yellow-400"


bgYellow500 : Html.Attribute msg
bgYellow500 =
    A.class "bg-yellow-500"


bgYellow600 : Html.Attribute msg
bgYellow600 =
    A.class "bg-yellow-600"


bgYellow700 : Html.Attribute msg
bgYellow700 =
    A.class "bg-yellow-700"


bgYellow800 : Html.Attribute msg
bgYellow800 =
    A.class "bg-yellow-800"


bgYellow900 : Html.Attribute msg
bgYellow900 =
    A.class "bg-yellow-900"


bgGreen100 : Html.Attribute msg
bgGreen100 =
    A.class "bg-green-100"


bgGreen200 : Html.Attribute msg
bgGreen200 =
    A.class "bg-green-200"


bgGreen300 : Html.Attribute msg
bgGreen300 =
    A.class "bg-green-300"


bgGreen400 : Html.Attribute msg
bgGreen400 =
    A.class "bg-green-400"


bgGreen500 : Html.Attribute msg
bgGreen500 =
    A.class "bg-green-500"


bgGreen600 : Html.Attribute msg
bgGreen600 =
    A.class "bg-green-600"


bgGreen700 : Html.Attribute msg
bgGreen700 =
    A.class "bg-green-700"


bgGreen800 : Html.Attribute msg
bgGreen800 =
    A.class "bg-green-800"


bgGreen900 : Html.Attribute msg
bgGreen900 =
    A.class "bg-green-900"


bgTeal100 : Html.Attribute msg
bgTeal100 =
    A.class "bg-teal-100"


bgTeal200 : Html.Attribute msg
bgTeal200 =
    A.class "bg-teal-200"


bgTeal300 : Html.Attribute msg
bgTeal300 =
    A.class "bg-teal-300"


bgTeal400 : Html.Attribute msg
bgTeal400 =
    A.class "bg-teal-400"


bgTeal500 : Html.Attribute msg
bgTeal500 =
    A.class "bg-teal-500"


bgTeal600 : Html.Attribute msg
bgTeal600 =
    A.class "bg-teal-600"


bgTeal700 : Html.Attribute msg
bgTeal700 =
    A.class "bg-teal-700"


bgTeal800 : Html.Attribute msg
bgTeal800 =
    A.class "bg-teal-800"


bgTeal900 : Html.Attribute msg
bgTeal900 =
    A.class "bg-teal-900"


bgBlue100 : Html.Attribute msg
bgBlue100 =
    A.class "bg-blue-100"


bgBlue200 : Html.Attribute msg
bgBlue200 =
    A.class "bg-blue-200"


bgBlue300 : Html.Attribute msg
bgBlue300 =
    A.class "bg-blue-300"


bgBlue400 : Html.Attribute msg
bgBlue400 =
    A.class "bg-blue-400"


bgBlue500 : Html.Attribute msg
bgBlue500 =
    A.class "bg-blue-500"


bgBlue600 : Html.Attribute msg
bgBlue600 =
    A.class "bg-blue-600"


bgBlue700 : Html.Attribute msg
bgBlue700 =
    A.class "bg-blue-700"


bgBlue800 : Html.Attribute msg
bgBlue800 =
    A.class "bg-blue-800"


bgBlue900 : Html.Attribute msg
bgBlue900 =
    A.class "bg-blue-900"


bgIndigo100 : Html.Attribute msg
bgIndigo100 =
    A.class "bg-indigo-100"


bgIndigo200 : Html.Attribute msg
bgIndigo200 =
    A.class "bg-indigo-200"


bgIndigo300 : Html.Attribute msg
bgIndigo300 =
    A.class "bg-indigo-300"


bgIndigo400 : Html.Attribute msg
bgIndigo400 =
    A.class "bg-indigo-400"


bgIndigo500 : Html.Attribute msg
bgIndigo500 =
    A.class "bg-indigo-500"


bgIndigo600 : Html.Attribute msg
bgIndigo600 =
    A.class "bg-indigo-600"


bgIndigo700 : Html.Attribute msg
bgIndigo700 =
    A.class "bg-indigo-700"


bgIndigo800 : Html.Attribute msg
bgIndigo800 =
    A.class "bg-indigo-800"


bgIndigo900 : Html.Attribute msg
bgIndigo900 =
    A.class "bg-indigo-900"


bgPurple100 : Html.Attribute msg
bgPurple100 =
    A.class "bg-purple-100"


bgPurple200 : Html.Attribute msg
bgPurple200 =
    A.class "bg-purple-200"


bgPurple300 : Html.Attribute msg
bgPurple300 =
    A.class "bg-purple-300"


bgPurple400 : Html.Attribute msg
bgPurple400 =
    A.class "bg-purple-400"


bgPurple500 : Html.Attribute msg
bgPurple500 =
    A.class "bg-purple-500"


bgPurple600 : Html.Attribute msg
bgPurple600 =
    A.class "bg-purple-600"


bgPurple700 : Html.Attribute msg
bgPurple700 =
    A.class "bg-purple-700"


bgPurple800 : Html.Attribute msg
bgPurple800 =
    A.class "bg-purple-800"


bgPurple900 : Html.Attribute msg
bgPurple900 =
    A.class "bg-purple-900"


bgPink100 : Html.Attribute msg
bgPink100 =
    A.class "bg-pink-100"


bgPink200 : Html.Attribute msg
bgPink200 =
    A.class "bg-pink-200"


bgPink300 : Html.Attribute msg
bgPink300 =
    A.class "bg-pink-300"


bgPink400 : Html.Attribute msg
bgPink400 =
    A.class "bg-pink-400"


bgPink500 : Html.Attribute msg
bgPink500 =
    A.class "bg-pink-500"


bgPink600 : Html.Attribute msg
bgPink600 =
    A.class "bg-pink-600"


bgPink700 : Html.Attribute msg
bgPink700 =
    A.class "bg-pink-700"


bgPink800 : Html.Attribute msg
bgPink800 =
    A.class "bg-pink-800"


bgPink900 : Html.Attribute msg
bgPink900 =
    A.class "bg-pink-900"


hoverBgTransparent : Html.Attribute msg
hoverBgTransparent =
    A.class "hover:bg-transparent"


hoverBgCurrent : Html.Attribute msg
hoverBgCurrent =
    A.class "hover:bg-current"


hoverBgBlack : Html.Attribute msg
hoverBgBlack =
    A.class "hover:bg-black"


hoverBgWhite : Html.Attribute msg
hoverBgWhite =
    A.class "hover:bg-white"


hoverBgGray100 : Html.Attribute msg
hoverBgGray100 =
    A.class "hover:bg-gray-100"


hoverBgGray200 : Html.Attribute msg
hoverBgGray200 =
    A.class "hover:bg-gray-200"


hoverBgGray300 : Html.Attribute msg
hoverBgGray300 =
    A.class "hover:bg-gray-300"


hoverBgGray400 : Html.Attribute msg
hoverBgGray400 =
    A.class "hover:bg-gray-400"


hoverBgGray500 : Html.Attribute msg
hoverBgGray500 =
    A.class "hover:bg-gray-500"


hoverBgGray600 : Html.Attribute msg
hoverBgGray600 =
    A.class "hover:bg-gray-600"


hoverBgGray700 : Html.Attribute msg
hoverBgGray700 =
    A.class "hover:bg-gray-700"


hoverBgGray800 : Html.Attribute msg
hoverBgGray800 =
    A.class "hover:bg-gray-800"


hoverBgGray900 : Html.Attribute msg
hoverBgGray900 =
    A.class "hover:bg-gray-900"


hoverBgRed100 : Html.Attribute msg
hoverBgRed100 =
    A.class "hover:bg-red-100"


hoverBgRed200 : Html.Attribute msg
hoverBgRed200 =
    A.class "hover:bg-red-200"


hoverBgRed300 : Html.Attribute msg
hoverBgRed300 =
    A.class "hover:bg-red-300"


hoverBgRed400 : Html.Attribute msg
hoverBgRed400 =
    A.class "hover:bg-red-400"


hoverBgRed500 : Html.Attribute msg
hoverBgRed500 =
    A.class "hover:bg-red-500"


hoverBgRed600 : Html.Attribute msg
hoverBgRed600 =
    A.class "hover:bg-red-600"


hoverBgRed700 : Html.Attribute msg
hoverBgRed700 =
    A.class "hover:bg-red-700"


hoverBgRed800 : Html.Attribute msg
hoverBgRed800 =
    A.class "hover:bg-red-800"


hoverBgRed900 : Html.Attribute msg
hoverBgRed900 =
    A.class "hover:bg-red-900"


hoverBgOrange100 : Html.Attribute msg
hoverBgOrange100 =
    A.class "hover:bg-orange-100"


hoverBgOrange200 : Html.Attribute msg
hoverBgOrange200 =
    A.class "hover:bg-orange-200"


hoverBgOrange300 : Html.Attribute msg
hoverBgOrange300 =
    A.class "hover:bg-orange-300"


hoverBgOrange400 : Html.Attribute msg
hoverBgOrange400 =
    A.class "hover:bg-orange-400"


hoverBgOrange500 : Html.Attribute msg
hoverBgOrange500 =
    A.class "hover:bg-orange-500"


hoverBgOrange600 : Html.Attribute msg
hoverBgOrange600 =
    A.class "hover:bg-orange-600"


hoverBgOrange700 : Html.Attribute msg
hoverBgOrange700 =
    A.class "hover:bg-orange-700"


hoverBgOrange800 : Html.Attribute msg
hoverBgOrange800 =
    A.class "hover:bg-orange-800"


hoverBgOrange900 : Html.Attribute msg
hoverBgOrange900 =
    A.class "hover:bg-orange-900"


hoverBgYellow100 : Html.Attribute msg
hoverBgYellow100 =
    A.class "hover:bg-yellow-100"


hoverBgYellow200 : Html.Attribute msg
hoverBgYellow200 =
    A.class "hover:bg-yellow-200"


hoverBgYellow300 : Html.Attribute msg
hoverBgYellow300 =
    A.class "hover:bg-yellow-300"


hoverBgYellow400 : Html.Attribute msg
hoverBgYellow400 =
    A.class "hover:bg-yellow-400"


hoverBgYellow500 : Html.Attribute msg
hoverBgYellow500 =
    A.class "hover:bg-yellow-500"


hoverBgYellow600 : Html.Attribute msg
hoverBgYellow600 =
    A.class "hover:bg-yellow-600"


hoverBgYellow700 : Html.Attribute msg
hoverBgYellow700 =
    A.class "hover:bg-yellow-700"


hoverBgYellow800 : Html.Attribute msg
hoverBgYellow800 =
    A.class "hover:bg-yellow-800"


hoverBgYellow900 : Html.Attribute msg
hoverBgYellow900 =
    A.class "hover:bg-yellow-900"


hoverBgGreen100 : Html.Attribute msg
hoverBgGreen100 =
    A.class "hover:bg-green-100"


hoverBgGreen200 : Html.Attribute msg
hoverBgGreen200 =
    A.class "hover:bg-green-200"


hoverBgGreen300 : Html.Attribute msg
hoverBgGreen300 =
    A.class "hover:bg-green-300"


hoverBgGreen400 : Html.Attribute msg
hoverBgGreen400 =
    A.class "hover:bg-green-400"


hoverBgGreen500 : Html.Attribute msg
hoverBgGreen500 =
    A.class "hover:bg-green-500"


hoverBgGreen600 : Html.Attribute msg
hoverBgGreen600 =
    A.class "hover:bg-green-600"


hoverBgGreen700 : Html.Attribute msg
hoverBgGreen700 =
    A.class "hover:bg-green-700"


hoverBgGreen800 : Html.Attribute msg
hoverBgGreen800 =
    A.class "hover:bg-green-800"


hoverBgGreen900 : Html.Attribute msg
hoverBgGreen900 =
    A.class "hover:bg-green-900"


hoverBgTeal100 : Html.Attribute msg
hoverBgTeal100 =
    A.class "hover:bg-teal-100"


hoverBgTeal200 : Html.Attribute msg
hoverBgTeal200 =
    A.class "hover:bg-teal-200"


hoverBgTeal300 : Html.Attribute msg
hoverBgTeal300 =
    A.class "hover:bg-teal-300"


hoverBgTeal400 : Html.Attribute msg
hoverBgTeal400 =
    A.class "hover:bg-teal-400"


hoverBgTeal500 : Html.Attribute msg
hoverBgTeal500 =
    A.class "hover:bg-teal-500"


hoverBgTeal600 : Html.Attribute msg
hoverBgTeal600 =
    A.class "hover:bg-teal-600"


hoverBgTeal700 : Html.Attribute msg
hoverBgTeal700 =
    A.class "hover:bg-teal-700"


hoverBgTeal800 : Html.Attribute msg
hoverBgTeal800 =
    A.class "hover:bg-teal-800"


hoverBgTeal900 : Html.Attribute msg
hoverBgTeal900 =
    A.class "hover:bg-teal-900"


hoverBgBlue100 : Html.Attribute msg
hoverBgBlue100 =
    A.class "hover:bg-blue-100"


hoverBgBlue200 : Html.Attribute msg
hoverBgBlue200 =
    A.class "hover:bg-blue-200"


hoverBgBlue300 : Html.Attribute msg
hoverBgBlue300 =
    A.class "hover:bg-blue-300"


hoverBgBlue400 : Html.Attribute msg
hoverBgBlue400 =
    A.class "hover:bg-blue-400"


hoverBgBlue500 : Html.Attribute msg
hoverBgBlue500 =
    A.class "hover:bg-blue-500"


hoverBgBlue600 : Html.Attribute msg
hoverBgBlue600 =
    A.class "hover:bg-blue-600"


hoverBgBlue700 : Html.Attribute msg
hoverBgBlue700 =
    A.class "hover:bg-blue-700"


hoverBgBlue800 : Html.Attribute msg
hoverBgBlue800 =
    A.class "hover:bg-blue-800"


hoverBgBlue900 : Html.Attribute msg
hoverBgBlue900 =
    A.class "hover:bg-blue-900"


hoverBgIndigo100 : Html.Attribute msg
hoverBgIndigo100 =
    A.class "hover:bg-indigo-100"


hoverBgIndigo200 : Html.Attribute msg
hoverBgIndigo200 =
    A.class "hover:bg-indigo-200"


hoverBgIndigo300 : Html.Attribute msg
hoverBgIndigo300 =
    A.class "hover:bg-indigo-300"


hoverBgIndigo400 : Html.Attribute msg
hoverBgIndigo400 =
    A.class "hover:bg-indigo-400"


hoverBgIndigo500 : Html.Attribute msg
hoverBgIndigo500 =
    A.class "hover:bg-indigo-500"


hoverBgIndigo600 : Html.Attribute msg
hoverBgIndigo600 =
    A.class "hover:bg-indigo-600"


hoverBgIndigo700 : Html.Attribute msg
hoverBgIndigo700 =
    A.class "hover:bg-indigo-700"


hoverBgIndigo800 : Html.Attribute msg
hoverBgIndigo800 =
    A.class "hover:bg-indigo-800"


hoverBgIndigo900 : Html.Attribute msg
hoverBgIndigo900 =
    A.class "hover:bg-indigo-900"


hoverBgPurple100 : Html.Attribute msg
hoverBgPurple100 =
    A.class "hover:bg-purple-100"


hoverBgPurple200 : Html.Attribute msg
hoverBgPurple200 =
    A.class "hover:bg-purple-200"


hoverBgPurple300 : Html.Attribute msg
hoverBgPurple300 =
    A.class "hover:bg-purple-300"


hoverBgPurple400 : Html.Attribute msg
hoverBgPurple400 =
    A.class "hover:bg-purple-400"


hoverBgPurple500 : Html.Attribute msg
hoverBgPurple500 =
    A.class "hover:bg-purple-500"


hoverBgPurple600 : Html.Attribute msg
hoverBgPurple600 =
    A.class "hover:bg-purple-600"


hoverBgPurple700 : Html.Attribute msg
hoverBgPurple700 =
    A.class "hover:bg-purple-700"


hoverBgPurple800 : Html.Attribute msg
hoverBgPurple800 =
    A.class "hover:bg-purple-800"


hoverBgPurple900 : Html.Attribute msg
hoverBgPurple900 =
    A.class "hover:bg-purple-900"


hoverBgPink100 : Html.Attribute msg
hoverBgPink100 =
    A.class "hover:bg-pink-100"


hoverBgPink200 : Html.Attribute msg
hoverBgPink200 =
    A.class "hover:bg-pink-200"


hoverBgPink300 : Html.Attribute msg
hoverBgPink300 =
    A.class "hover:bg-pink-300"


hoverBgPink400 : Html.Attribute msg
hoverBgPink400 =
    A.class "hover:bg-pink-400"


hoverBgPink500 : Html.Attribute msg
hoverBgPink500 =
    A.class "hover:bg-pink-500"


hoverBgPink600 : Html.Attribute msg
hoverBgPink600 =
    A.class "hover:bg-pink-600"


hoverBgPink700 : Html.Attribute msg
hoverBgPink700 =
    A.class "hover:bg-pink-700"


hoverBgPink800 : Html.Attribute msg
hoverBgPink800 =
    A.class "hover:bg-pink-800"


hoverBgPink900 : Html.Attribute msg
hoverBgPink900 =
    A.class "hover:bg-pink-900"


focusBgTransparent : Html.Attribute msg
focusBgTransparent =
    A.class "focus:bg-transparent"


focusBgCurrent : Html.Attribute msg
focusBgCurrent =
    A.class "focus:bg-current"


focusBgBlack : Html.Attribute msg
focusBgBlack =
    A.class "focus:bg-black"


focusBgWhite : Html.Attribute msg
focusBgWhite =
    A.class "focus:bg-white"


focusBgGray100 : Html.Attribute msg
focusBgGray100 =
    A.class "focus:bg-gray-100"


focusBgGray200 : Html.Attribute msg
focusBgGray200 =
    A.class "focus:bg-gray-200"


focusBgGray300 : Html.Attribute msg
focusBgGray300 =
    A.class "focus:bg-gray-300"


focusBgGray400 : Html.Attribute msg
focusBgGray400 =
    A.class "focus:bg-gray-400"


focusBgGray500 : Html.Attribute msg
focusBgGray500 =
    A.class "focus:bg-gray-500"


focusBgGray600 : Html.Attribute msg
focusBgGray600 =
    A.class "focus:bg-gray-600"


focusBgGray700 : Html.Attribute msg
focusBgGray700 =
    A.class "focus:bg-gray-700"


focusBgGray800 : Html.Attribute msg
focusBgGray800 =
    A.class "focus:bg-gray-800"


focusBgGray900 : Html.Attribute msg
focusBgGray900 =
    A.class "focus:bg-gray-900"


focusBgRed100 : Html.Attribute msg
focusBgRed100 =
    A.class "focus:bg-red-100"


focusBgRed200 : Html.Attribute msg
focusBgRed200 =
    A.class "focus:bg-red-200"


focusBgRed300 : Html.Attribute msg
focusBgRed300 =
    A.class "focus:bg-red-300"


focusBgRed400 : Html.Attribute msg
focusBgRed400 =
    A.class "focus:bg-red-400"


focusBgRed500 : Html.Attribute msg
focusBgRed500 =
    A.class "focus:bg-red-500"


focusBgRed600 : Html.Attribute msg
focusBgRed600 =
    A.class "focus:bg-red-600"


focusBgRed700 : Html.Attribute msg
focusBgRed700 =
    A.class "focus:bg-red-700"


focusBgRed800 : Html.Attribute msg
focusBgRed800 =
    A.class "focus:bg-red-800"


focusBgRed900 : Html.Attribute msg
focusBgRed900 =
    A.class "focus:bg-red-900"


focusBgOrange100 : Html.Attribute msg
focusBgOrange100 =
    A.class "focus:bg-orange-100"


focusBgOrange200 : Html.Attribute msg
focusBgOrange200 =
    A.class "focus:bg-orange-200"


focusBgOrange300 : Html.Attribute msg
focusBgOrange300 =
    A.class "focus:bg-orange-300"


focusBgOrange400 : Html.Attribute msg
focusBgOrange400 =
    A.class "focus:bg-orange-400"


focusBgOrange500 : Html.Attribute msg
focusBgOrange500 =
    A.class "focus:bg-orange-500"


focusBgOrange600 : Html.Attribute msg
focusBgOrange600 =
    A.class "focus:bg-orange-600"


focusBgOrange700 : Html.Attribute msg
focusBgOrange700 =
    A.class "focus:bg-orange-700"


focusBgOrange800 : Html.Attribute msg
focusBgOrange800 =
    A.class "focus:bg-orange-800"


focusBgOrange900 : Html.Attribute msg
focusBgOrange900 =
    A.class "focus:bg-orange-900"


focusBgYellow100 : Html.Attribute msg
focusBgYellow100 =
    A.class "focus:bg-yellow-100"


focusBgYellow200 : Html.Attribute msg
focusBgYellow200 =
    A.class "focus:bg-yellow-200"


focusBgYellow300 : Html.Attribute msg
focusBgYellow300 =
    A.class "focus:bg-yellow-300"


focusBgYellow400 : Html.Attribute msg
focusBgYellow400 =
    A.class "focus:bg-yellow-400"


focusBgYellow500 : Html.Attribute msg
focusBgYellow500 =
    A.class "focus:bg-yellow-500"


focusBgYellow600 : Html.Attribute msg
focusBgYellow600 =
    A.class "focus:bg-yellow-600"


focusBgYellow700 : Html.Attribute msg
focusBgYellow700 =
    A.class "focus:bg-yellow-700"


focusBgYellow800 : Html.Attribute msg
focusBgYellow800 =
    A.class "focus:bg-yellow-800"


focusBgYellow900 : Html.Attribute msg
focusBgYellow900 =
    A.class "focus:bg-yellow-900"


focusBgGreen100 : Html.Attribute msg
focusBgGreen100 =
    A.class "focus:bg-green-100"


focusBgGreen200 : Html.Attribute msg
focusBgGreen200 =
    A.class "focus:bg-green-200"


focusBgGreen300 : Html.Attribute msg
focusBgGreen300 =
    A.class "focus:bg-green-300"


focusBgGreen400 : Html.Attribute msg
focusBgGreen400 =
    A.class "focus:bg-green-400"


focusBgGreen500 : Html.Attribute msg
focusBgGreen500 =
    A.class "focus:bg-green-500"


focusBgGreen600 : Html.Attribute msg
focusBgGreen600 =
    A.class "focus:bg-green-600"


focusBgGreen700 : Html.Attribute msg
focusBgGreen700 =
    A.class "focus:bg-green-700"


focusBgGreen800 : Html.Attribute msg
focusBgGreen800 =
    A.class "focus:bg-green-800"


focusBgGreen900 : Html.Attribute msg
focusBgGreen900 =
    A.class "focus:bg-green-900"


focusBgTeal100 : Html.Attribute msg
focusBgTeal100 =
    A.class "focus:bg-teal-100"


focusBgTeal200 : Html.Attribute msg
focusBgTeal200 =
    A.class "focus:bg-teal-200"


focusBgTeal300 : Html.Attribute msg
focusBgTeal300 =
    A.class "focus:bg-teal-300"


focusBgTeal400 : Html.Attribute msg
focusBgTeal400 =
    A.class "focus:bg-teal-400"


focusBgTeal500 : Html.Attribute msg
focusBgTeal500 =
    A.class "focus:bg-teal-500"


focusBgTeal600 : Html.Attribute msg
focusBgTeal600 =
    A.class "focus:bg-teal-600"


focusBgTeal700 : Html.Attribute msg
focusBgTeal700 =
    A.class "focus:bg-teal-700"


focusBgTeal800 : Html.Attribute msg
focusBgTeal800 =
    A.class "focus:bg-teal-800"


focusBgTeal900 : Html.Attribute msg
focusBgTeal900 =
    A.class "focus:bg-teal-900"


focusBgBlue100 : Html.Attribute msg
focusBgBlue100 =
    A.class "focus:bg-blue-100"


focusBgBlue200 : Html.Attribute msg
focusBgBlue200 =
    A.class "focus:bg-blue-200"


focusBgBlue300 : Html.Attribute msg
focusBgBlue300 =
    A.class "focus:bg-blue-300"


focusBgBlue400 : Html.Attribute msg
focusBgBlue400 =
    A.class "focus:bg-blue-400"


focusBgBlue500 : Html.Attribute msg
focusBgBlue500 =
    A.class "focus:bg-blue-500"


focusBgBlue600 : Html.Attribute msg
focusBgBlue600 =
    A.class "focus:bg-blue-600"


focusBgBlue700 : Html.Attribute msg
focusBgBlue700 =
    A.class "focus:bg-blue-700"


focusBgBlue800 : Html.Attribute msg
focusBgBlue800 =
    A.class "focus:bg-blue-800"


focusBgBlue900 : Html.Attribute msg
focusBgBlue900 =
    A.class "focus:bg-blue-900"


focusBgIndigo100 : Html.Attribute msg
focusBgIndigo100 =
    A.class "focus:bg-indigo-100"


focusBgIndigo200 : Html.Attribute msg
focusBgIndigo200 =
    A.class "focus:bg-indigo-200"


focusBgIndigo300 : Html.Attribute msg
focusBgIndigo300 =
    A.class "focus:bg-indigo-300"


focusBgIndigo400 : Html.Attribute msg
focusBgIndigo400 =
    A.class "focus:bg-indigo-400"


focusBgIndigo500 : Html.Attribute msg
focusBgIndigo500 =
    A.class "focus:bg-indigo-500"


focusBgIndigo600 : Html.Attribute msg
focusBgIndigo600 =
    A.class "focus:bg-indigo-600"


focusBgIndigo700 : Html.Attribute msg
focusBgIndigo700 =
    A.class "focus:bg-indigo-700"


focusBgIndigo800 : Html.Attribute msg
focusBgIndigo800 =
    A.class "focus:bg-indigo-800"


focusBgIndigo900 : Html.Attribute msg
focusBgIndigo900 =
    A.class "focus:bg-indigo-900"


focusBgPurple100 : Html.Attribute msg
focusBgPurple100 =
    A.class "focus:bg-purple-100"


focusBgPurple200 : Html.Attribute msg
focusBgPurple200 =
    A.class "focus:bg-purple-200"


focusBgPurple300 : Html.Attribute msg
focusBgPurple300 =
    A.class "focus:bg-purple-300"


focusBgPurple400 : Html.Attribute msg
focusBgPurple400 =
    A.class "focus:bg-purple-400"


focusBgPurple500 : Html.Attribute msg
focusBgPurple500 =
    A.class "focus:bg-purple-500"


focusBgPurple600 : Html.Attribute msg
focusBgPurple600 =
    A.class "focus:bg-purple-600"


focusBgPurple700 : Html.Attribute msg
focusBgPurple700 =
    A.class "focus:bg-purple-700"


focusBgPurple800 : Html.Attribute msg
focusBgPurple800 =
    A.class "focus:bg-purple-800"


focusBgPurple900 : Html.Attribute msg
focusBgPurple900 =
    A.class "focus:bg-purple-900"


focusBgPink100 : Html.Attribute msg
focusBgPink100 =
    A.class "focus:bg-pink-100"


focusBgPink200 : Html.Attribute msg
focusBgPink200 =
    A.class "focus:bg-pink-200"


focusBgPink300 : Html.Attribute msg
focusBgPink300 =
    A.class "focus:bg-pink-300"


focusBgPink400 : Html.Attribute msg
focusBgPink400 =
    A.class "focus:bg-pink-400"


focusBgPink500 : Html.Attribute msg
focusBgPink500 =
    A.class "focus:bg-pink-500"


focusBgPink600 : Html.Attribute msg
focusBgPink600 =
    A.class "focus:bg-pink-600"


focusBgPink700 : Html.Attribute msg
focusBgPink700 =
    A.class "focus:bg-pink-700"


focusBgPink800 : Html.Attribute msg
focusBgPink800 =
    A.class "focus:bg-pink-800"


focusBgPink900 : Html.Attribute msg
focusBgPink900 =
    A.class "focus:bg-pink-900"


bgNone : Html.Attribute msg
bgNone =
    A.class "bg-none"


bgGradientToT : Html.Attribute msg
bgGradientToT =
    A.class "bg-gradient-to-t"


bgGradientToTr : Html.Attribute msg
bgGradientToTr =
    A.class "bg-gradient-to-tr"


bgGradientToR : Html.Attribute msg
bgGradientToR =
    A.class "bg-gradient-to-r"


bgGradientToBr : Html.Attribute msg
bgGradientToBr =
    A.class "bg-gradient-to-br"


bgGradientToB : Html.Attribute msg
bgGradientToB =
    A.class "bg-gradient-to-b"


bgGradientToBl : Html.Attribute msg
bgGradientToBl =
    A.class "bg-gradient-to-bl"


bgGradientToL : Html.Attribute msg
bgGradientToL =
    A.class "bg-gradient-to-l"


bgGradientToTl : Html.Attribute msg
bgGradientToTl =
    A.class "bg-gradient-to-tl"


fromTransparent : Html.Attribute msg
fromTransparent =
    A.class "from-transparent"


fromCurrent : Html.Attribute msg
fromCurrent =
    A.class "from-current"


fromBlack : Html.Attribute msg
fromBlack =
    A.class "from-black"


fromWhite : Html.Attribute msg
fromWhite =
    A.class "from-white"


fromGray100 : Html.Attribute msg
fromGray100 =
    A.class "from-gray-100"


fromGray200 : Html.Attribute msg
fromGray200 =
    A.class "from-gray-200"


fromGray300 : Html.Attribute msg
fromGray300 =
    A.class "from-gray-300"


fromGray400 : Html.Attribute msg
fromGray400 =
    A.class "from-gray-400"


fromGray500 : Html.Attribute msg
fromGray500 =
    A.class "from-gray-500"


fromGray600 : Html.Attribute msg
fromGray600 =
    A.class "from-gray-600"


fromGray700 : Html.Attribute msg
fromGray700 =
    A.class "from-gray-700"


fromGray800 : Html.Attribute msg
fromGray800 =
    A.class "from-gray-800"


fromGray900 : Html.Attribute msg
fromGray900 =
    A.class "from-gray-900"


fromRed100 : Html.Attribute msg
fromRed100 =
    A.class "from-red-100"


fromRed200 : Html.Attribute msg
fromRed200 =
    A.class "from-red-200"


fromRed300 : Html.Attribute msg
fromRed300 =
    A.class "from-red-300"


fromRed400 : Html.Attribute msg
fromRed400 =
    A.class "from-red-400"


fromRed500 : Html.Attribute msg
fromRed500 =
    A.class "from-red-500"


fromRed600 : Html.Attribute msg
fromRed600 =
    A.class "from-red-600"


fromRed700 : Html.Attribute msg
fromRed700 =
    A.class "from-red-700"


fromRed800 : Html.Attribute msg
fromRed800 =
    A.class "from-red-800"


fromRed900 : Html.Attribute msg
fromRed900 =
    A.class "from-red-900"


fromOrange100 : Html.Attribute msg
fromOrange100 =
    A.class "from-orange-100"


fromOrange200 : Html.Attribute msg
fromOrange200 =
    A.class "from-orange-200"


fromOrange300 : Html.Attribute msg
fromOrange300 =
    A.class "from-orange-300"


fromOrange400 : Html.Attribute msg
fromOrange400 =
    A.class "from-orange-400"


fromOrange500 : Html.Attribute msg
fromOrange500 =
    A.class "from-orange-500"


fromOrange600 : Html.Attribute msg
fromOrange600 =
    A.class "from-orange-600"


fromOrange700 : Html.Attribute msg
fromOrange700 =
    A.class "from-orange-700"


fromOrange800 : Html.Attribute msg
fromOrange800 =
    A.class "from-orange-800"


fromOrange900 : Html.Attribute msg
fromOrange900 =
    A.class "from-orange-900"


fromYellow100 : Html.Attribute msg
fromYellow100 =
    A.class "from-yellow-100"


fromYellow200 : Html.Attribute msg
fromYellow200 =
    A.class "from-yellow-200"


fromYellow300 : Html.Attribute msg
fromYellow300 =
    A.class "from-yellow-300"


fromYellow400 : Html.Attribute msg
fromYellow400 =
    A.class "from-yellow-400"


fromYellow500 : Html.Attribute msg
fromYellow500 =
    A.class "from-yellow-500"


fromYellow600 : Html.Attribute msg
fromYellow600 =
    A.class "from-yellow-600"


fromYellow700 : Html.Attribute msg
fromYellow700 =
    A.class "from-yellow-700"


fromYellow800 : Html.Attribute msg
fromYellow800 =
    A.class "from-yellow-800"


fromYellow900 : Html.Attribute msg
fromYellow900 =
    A.class "from-yellow-900"


fromGreen100 : Html.Attribute msg
fromGreen100 =
    A.class "from-green-100"


fromGreen200 : Html.Attribute msg
fromGreen200 =
    A.class "from-green-200"


fromGreen300 : Html.Attribute msg
fromGreen300 =
    A.class "from-green-300"


fromGreen400 : Html.Attribute msg
fromGreen400 =
    A.class "from-green-400"


fromGreen500 : Html.Attribute msg
fromGreen500 =
    A.class "from-green-500"


fromGreen600 : Html.Attribute msg
fromGreen600 =
    A.class "from-green-600"


fromGreen700 : Html.Attribute msg
fromGreen700 =
    A.class "from-green-700"


fromGreen800 : Html.Attribute msg
fromGreen800 =
    A.class "from-green-800"


fromGreen900 : Html.Attribute msg
fromGreen900 =
    A.class "from-green-900"


fromTeal100 : Html.Attribute msg
fromTeal100 =
    A.class "from-teal-100"


fromTeal200 : Html.Attribute msg
fromTeal200 =
    A.class "from-teal-200"


fromTeal300 : Html.Attribute msg
fromTeal300 =
    A.class "from-teal-300"


fromTeal400 : Html.Attribute msg
fromTeal400 =
    A.class "from-teal-400"


fromTeal500 : Html.Attribute msg
fromTeal500 =
    A.class "from-teal-500"


fromTeal600 : Html.Attribute msg
fromTeal600 =
    A.class "from-teal-600"


fromTeal700 : Html.Attribute msg
fromTeal700 =
    A.class "from-teal-700"


fromTeal800 : Html.Attribute msg
fromTeal800 =
    A.class "from-teal-800"


fromTeal900 : Html.Attribute msg
fromTeal900 =
    A.class "from-teal-900"


fromBlue100 : Html.Attribute msg
fromBlue100 =
    A.class "from-blue-100"


fromBlue200 : Html.Attribute msg
fromBlue200 =
    A.class "from-blue-200"


fromBlue300 : Html.Attribute msg
fromBlue300 =
    A.class "from-blue-300"


fromBlue400 : Html.Attribute msg
fromBlue400 =
    A.class "from-blue-400"


fromBlue500 : Html.Attribute msg
fromBlue500 =
    A.class "from-blue-500"


fromBlue600 : Html.Attribute msg
fromBlue600 =
    A.class "from-blue-600"


fromBlue700 : Html.Attribute msg
fromBlue700 =
    A.class "from-blue-700"


fromBlue800 : Html.Attribute msg
fromBlue800 =
    A.class "from-blue-800"


fromBlue900 : Html.Attribute msg
fromBlue900 =
    A.class "from-blue-900"


fromIndigo100 : Html.Attribute msg
fromIndigo100 =
    A.class "from-indigo-100"


fromIndigo200 : Html.Attribute msg
fromIndigo200 =
    A.class "from-indigo-200"


fromIndigo300 : Html.Attribute msg
fromIndigo300 =
    A.class "from-indigo-300"


fromIndigo400 : Html.Attribute msg
fromIndigo400 =
    A.class "from-indigo-400"


fromIndigo500 : Html.Attribute msg
fromIndigo500 =
    A.class "from-indigo-500"


fromIndigo600 : Html.Attribute msg
fromIndigo600 =
    A.class "from-indigo-600"


fromIndigo700 : Html.Attribute msg
fromIndigo700 =
    A.class "from-indigo-700"


fromIndigo800 : Html.Attribute msg
fromIndigo800 =
    A.class "from-indigo-800"


fromIndigo900 : Html.Attribute msg
fromIndigo900 =
    A.class "from-indigo-900"


fromPurple100 : Html.Attribute msg
fromPurple100 =
    A.class "from-purple-100"


fromPurple200 : Html.Attribute msg
fromPurple200 =
    A.class "from-purple-200"


fromPurple300 : Html.Attribute msg
fromPurple300 =
    A.class "from-purple-300"


fromPurple400 : Html.Attribute msg
fromPurple400 =
    A.class "from-purple-400"


fromPurple500 : Html.Attribute msg
fromPurple500 =
    A.class "from-purple-500"


fromPurple600 : Html.Attribute msg
fromPurple600 =
    A.class "from-purple-600"


fromPurple700 : Html.Attribute msg
fromPurple700 =
    A.class "from-purple-700"


fromPurple800 : Html.Attribute msg
fromPurple800 =
    A.class "from-purple-800"


fromPurple900 : Html.Attribute msg
fromPurple900 =
    A.class "from-purple-900"


fromPink100 : Html.Attribute msg
fromPink100 =
    A.class "from-pink-100"


fromPink200 : Html.Attribute msg
fromPink200 =
    A.class "from-pink-200"


fromPink300 : Html.Attribute msg
fromPink300 =
    A.class "from-pink-300"


fromPink400 : Html.Attribute msg
fromPink400 =
    A.class "from-pink-400"


fromPink500 : Html.Attribute msg
fromPink500 =
    A.class "from-pink-500"


fromPink600 : Html.Attribute msg
fromPink600 =
    A.class "from-pink-600"


fromPink700 : Html.Attribute msg
fromPink700 =
    A.class "from-pink-700"


fromPink800 : Html.Attribute msg
fromPink800 =
    A.class "from-pink-800"


fromPink900 : Html.Attribute msg
fromPink900 =
    A.class "from-pink-900"


viaTransparent : Html.Attribute msg
viaTransparent =
    A.class "via-transparent"


viaCurrent : Html.Attribute msg
viaCurrent =
    A.class "via-current"


viaBlack : Html.Attribute msg
viaBlack =
    A.class "via-black"


viaWhite : Html.Attribute msg
viaWhite =
    A.class "via-white"


viaGray100 : Html.Attribute msg
viaGray100 =
    A.class "via-gray-100"


viaGray200 : Html.Attribute msg
viaGray200 =
    A.class "via-gray-200"


viaGray300 : Html.Attribute msg
viaGray300 =
    A.class "via-gray-300"


viaGray400 : Html.Attribute msg
viaGray400 =
    A.class "via-gray-400"


viaGray500 : Html.Attribute msg
viaGray500 =
    A.class "via-gray-500"


viaGray600 : Html.Attribute msg
viaGray600 =
    A.class "via-gray-600"


viaGray700 : Html.Attribute msg
viaGray700 =
    A.class "via-gray-700"


viaGray800 : Html.Attribute msg
viaGray800 =
    A.class "via-gray-800"


viaGray900 : Html.Attribute msg
viaGray900 =
    A.class "via-gray-900"


viaRed100 : Html.Attribute msg
viaRed100 =
    A.class "via-red-100"


viaRed200 : Html.Attribute msg
viaRed200 =
    A.class "via-red-200"


viaRed300 : Html.Attribute msg
viaRed300 =
    A.class "via-red-300"


viaRed400 : Html.Attribute msg
viaRed400 =
    A.class "via-red-400"


viaRed500 : Html.Attribute msg
viaRed500 =
    A.class "via-red-500"


viaRed600 : Html.Attribute msg
viaRed600 =
    A.class "via-red-600"


viaRed700 : Html.Attribute msg
viaRed700 =
    A.class "via-red-700"


viaRed800 : Html.Attribute msg
viaRed800 =
    A.class "via-red-800"


viaRed900 : Html.Attribute msg
viaRed900 =
    A.class "via-red-900"


viaOrange100 : Html.Attribute msg
viaOrange100 =
    A.class "via-orange-100"


viaOrange200 : Html.Attribute msg
viaOrange200 =
    A.class "via-orange-200"


viaOrange300 : Html.Attribute msg
viaOrange300 =
    A.class "via-orange-300"


viaOrange400 : Html.Attribute msg
viaOrange400 =
    A.class "via-orange-400"


viaOrange500 : Html.Attribute msg
viaOrange500 =
    A.class "via-orange-500"


viaOrange600 : Html.Attribute msg
viaOrange600 =
    A.class "via-orange-600"


viaOrange700 : Html.Attribute msg
viaOrange700 =
    A.class "via-orange-700"


viaOrange800 : Html.Attribute msg
viaOrange800 =
    A.class "via-orange-800"


viaOrange900 : Html.Attribute msg
viaOrange900 =
    A.class "via-orange-900"


viaYellow100 : Html.Attribute msg
viaYellow100 =
    A.class "via-yellow-100"


viaYellow200 : Html.Attribute msg
viaYellow200 =
    A.class "via-yellow-200"


viaYellow300 : Html.Attribute msg
viaYellow300 =
    A.class "via-yellow-300"


viaYellow400 : Html.Attribute msg
viaYellow400 =
    A.class "via-yellow-400"


viaYellow500 : Html.Attribute msg
viaYellow500 =
    A.class "via-yellow-500"


viaYellow600 : Html.Attribute msg
viaYellow600 =
    A.class "via-yellow-600"


viaYellow700 : Html.Attribute msg
viaYellow700 =
    A.class "via-yellow-700"


viaYellow800 : Html.Attribute msg
viaYellow800 =
    A.class "via-yellow-800"


viaYellow900 : Html.Attribute msg
viaYellow900 =
    A.class "via-yellow-900"


viaGreen100 : Html.Attribute msg
viaGreen100 =
    A.class "via-green-100"


viaGreen200 : Html.Attribute msg
viaGreen200 =
    A.class "via-green-200"


viaGreen300 : Html.Attribute msg
viaGreen300 =
    A.class "via-green-300"


viaGreen400 : Html.Attribute msg
viaGreen400 =
    A.class "via-green-400"


viaGreen500 : Html.Attribute msg
viaGreen500 =
    A.class "via-green-500"


viaGreen600 : Html.Attribute msg
viaGreen600 =
    A.class "via-green-600"


viaGreen700 : Html.Attribute msg
viaGreen700 =
    A.class "via-green-700"


viaGreen800 : Html.Attribute msg
viaGreen800 =
    A.class "via-green-800"


viaGreen900 : Html.Attribute msg
viaGreen900 =
    A.class "via-green-900"


viaTeal100 : Html.Attribute msg
viaTeal100 =
    A.class "via-teal-100"


viaTeal200 : Html.Attribute msg
viaTeal200 =
    A.class "via-teal-200"


viaTeal300 : Html.Attribute msg
viaTeal300 =
    A.class "via-teal-300"


viaTeal400 : Html.Attribute msg
viaTeal400 =
    A.class "via-teal-400"


viaTeal500 : Html.Attribute msg
viaTeal500 =
    A.class "via-teal-500"


viaTeal600 : Html.Attribute msg
viaTeal600 =
    A.class "via-teal-600"


viaTeal700 : Html.Attribute msg
viaTeal700 =
    A.class "via-teal-700"


viaTeal800 : Html.Attribute msg
viaTeal800 =
    A.class "via-teal-800"


viaTeal900 : Html.Attribute msg
viaTeal900 =
    A.class "via-teal-900"


viaBlue100 : Html.Attribute msg
viaBlue100 =
    A.class "via-blue-100"


viaBlue200 : Html.Attribute msg
viaBlue200 =
    A.class "via-blue-200"


viaBlue300 : Html.Attribute msg
viaBlue300 =
    A.class "via-blue-300"


viaBlue400 : Html.Attribute msg
viaBlue400 =
    A.class "via-blue-400"


viaBlue500 : Html.Attribute msg
viaBlue500 =
    A.class "via-blue-500"


viaBlue600 : Html.Attribute msg
viaBlue600 =
    A.class "via-blue-600"


viaBlue700 : Html.Attribute msg
viaBlue700 =
    A.class "via-blue-700"


viaBlue800 : Html.Attribute msg
viaBlue800 =
    A.class "via-blue-800"


viaBlue900 : Html.Attribute msg
viaBlue900 =
    A.class "via-blue-900"


viaIndigo100 : Html.Attribute msg
viaIndigo100 =
    A.class "via-indigo-100"


viaIndigo200 : Html.Attribute msg
viaIndigo200 =
    A.class "via-indigo-200"


viaIndigo300 : Html.Attribute msg
viaIndigo300 =
    A.class "via-indigo-300"


viaIndigo400 : Html.Attribute msg
viaIndigo400 =
    A.class "via-indigo-400"


viaIndigo500 : Html.Attribute msg
viaIndigo500 =
    A.class "via-indigo-500"


viaIndigo600 : Html.Attribute msg
viaIndigo600 =
    A.class "via-indigo-600"


viaIndigo700 : Html.Attribute msg
viaIndigo700 =
    A.class "via-indigo-700"


viaIndigo800 : Html.Attribute msg
viaIndigo800 =
    A.class "via-indigo-800"


viaIndigo900 : Html.Attribute msg
viaIndigo900 =
    A.class "via-indigo-900"


viaPurple100 : Html.Attribute msg
viaPurple100 =
    A.class "via-purple-100"


viaPurple200 : Html.Attribute msg
viaPurple200 =
    A.class "via-purple-200"


viaPurple300 : Html.Attribute msg
viaPurple300 =
    A.class "via-purple-300"


viaPurple400 : Html.Attribute msg
viaPurple400 =
    A.class "via-purple-400"


viaPurple500 : Html.Attribute msg
viaPurple500 =
    A.class "via-purple-500"


viaPurple600 : Html.Attribute msg
viaPurple600 =
    A.class "via-purple-600"


viaPurple700 : Html.Attribute msg
viaPurple700 =
    A.class "via-purple-700"


viaPurple800 : Html.Attribute msg
viaPurple800 =
    A.class "via-purple-800"


viaPurple900 : Html.Attribute msg
viaPurple900 =
    A.class "via-purple-900"


viaPink100 : Html.Attribute msg
viaPink100 =
    A.class "via-pink-100"


viaPink200 : Html.Attribute msg
viaPink200 =
    A.class "via-pink-200"


viaPink300 : Html.Attribute msg
viaPink300 =
    A.class "via-pink-300"


viaPink400 : Html.Attribute msg
viaPink400 =
    A.class "via-pink-400"


viaPink500 : Html.Attribute msg
viaPink500 =
    A.class "via-pink-500"


viaPink600 : Html.Attribute msg
viaPink600 =
    A.class "via-pink-600"


viaPink700 : Html.Attribute msg
viaPink700 =
    A.class "via-pink-700"


viaPink800 : Html.Attribute msg
viaPink800 =
    A.class "via-pink-800"


viaPink900 : Html.Attribute msg
viaPink900 =
    A.class "via-pink-900"


toTransparent : Html.Attribute msg
toTransparent =
    A.class "to-transparent"


toCurrent : Html.Attribute msg
toCurrent =
    A.class "to-current"


toBlack : Html.Attribute msg
toBlack =
    A.class "to-black"


toWhite : Html.Attribute msg
toWhite =
    A.class "to-white"


toGray100 : Html.Attribute msg
toGray100 =
    A.class "to-gray-100"


toGray200 : Html.Attribute msg
toGray200 =
    A.class "to-gray-200"


toGray300 : Html.Attribute msg
toGray300 =
    A.class "to-gray-300"


toGray400 : Html.Attribute msg
toGray400 =
    A.class "to-gray-400"


toGray500 : Html.Attribute msg
toGray500 =
    A.class "to-gray-500"


toGray600 : Html.Attribute msg
toGray600 =
    A.class "to-gray-600"


toGray700 : Html.Attribute msg
toGray700 =
    A.class "to-gray-700"


toGray800 : Html.Attribute msg
toGray800 =
    A.class "to-gray-800"


toGray900 : Html.Attribute msg
toGray900 =
    A.class "to-gray-900"


toRed100 : Html.Attribute msg
toRed100 =
    A.class "to-red-100"


toRed200 : Html.Attribute msg
toRed200 =
    A.class "to-red-200"


toRed300 : Html.Attribute msg
toRed300 =
    A.class "to-red-300"


toRed400 : Html.Attribute msg
toRed400 =
    A.class "to-red-400"


toRed500 : Html.Attribute msg
toRed500 =
    A.class "to-red-500"


toRed600 : Html.Attribute msg
toRed600 =
    A.class "to-red-600"


toRed700 : Html.Attribute msg
toRed700 =
    A.class "to-red-700"


toRed800 : Html.Attribute msg
toRed800 =
    A.class "to-red-800"


toRed900 : Html.Attribute msg
toRed900 =
    A.class "to-red-900"


toOrange100 : Html.Attribute msg
toOrange100 =
    A.class "to-orange-100"


toOrange200 : Html.Attribute msg
toOrange200 =
    A.class "to-orange-200"


toOrange300 : Html.Attribute msg
toOrange300 =
    A.class "to-orange-300"


toOrange400 : Html.Attribute msg
toOrange400 =
    A.class "to-orange-400"


toOrange500 : Html.Attribute msg
toOrange500 =
    A.class "to-orange-500"


toOrange600 : Html.Attribute msg
toOrange600 =
    A.class "to-orange-600"


toOrange700 : Html.Attribute msg
toOrange700 =
    A.class "to-orange-700"


toOrange800 : Html.Attribute msg
toOrange800 =
    A.class "to-orange-800"


toOrange900 : Html.Attribute msg
toOrange900 =
    A.class "to-orange-900"


toYellow100 : Html.Attribute msg
toYellow100 =
    A.class "to-yellow-100"


toYellow200 : Html.Attribute msg
toYellow200 =
    A.class "to-yellow-200"


toYellow300 : Html.Attribute msg
toYellow300 =
    A.class "to-yellow-300"


toYellow400 : Html.Attribute msg
toYellow400 =
    A.class "to-yellow-400"


toYellow500 : Html.Attribute msg
toYellow500 =
    A.class "to-yellow-500"


toYellow600 : Html.Attribute msg
toYellow600 =
    A.class "to-yellow-600"


toYellow700 : Html.Attribute msg
toYellow700 =
    A.class "to-yellow-700"


toYellow800 : Html.Attribute msg
toYellow800 =
    A.class "to-yellow-800"


toYellow900 : Html.Attribute msg
toYellow900 =
    A.class "to-yellow-900"


toGreen100 : Html.Attribute msg
toGreen100 =
    A.class "to-green-100"


toGreen200 : Html.Attribute msg
toGreen200 =
    A.class "to-green-200"


toGreen300 : Html.Attribute msg
toGreen300 =
    A.class "to-green-300"


toGreen400 : Html.Attribute msg
toGreen400 =
    A.class "to-green-400"


toGreen500 : Html.Attribute msg
toGreen500 =
    A.class "to-green-500"


toGreen600 : Html.Attribute msg
toGreen600 =
    A.class "to-green-600"


toGreen700 : Html.Attribute msg
toGreen700 =
    A.class "to-green-700"


toGreen800 : Html.Attribute msg
toGreen800 =
    A.class "to-green-800"


toGreen900 : Html.Attribute msg
toGreen900 =
    A.class "to-green-900"


toTeal100 : Html.Attribute msg
toTeal100 =
    A.class "to-teal-100"


toTeal200 : Html.Attribute msg
toTeal200 =
    A.class "to-teal-200"


toTeal300 : Html.Attribute msg
toTeal300 =
    A.class "to-teal-300"


toTeal400 : Html.Attribute msg
toTeal400 =
    A.class "to-teal-400"


toTeal500 : Html.Attribute msg
toTeal500 =
    A.class "to-teal-500"


toTeal600 : Html.Attribute msg
toTeal600 =
    A.class "to-teal-600"


toTeal700 : Html.Attribute msg
toTeal700 =
    A.class "to-teal-700"


toTeal800 : Html.Attribute msg
toTeal800 =
    A.class "to-teal-800"


toTeal900 : Html.Attribute msg
toTeal900 =
    A.class "to-teal-900"


toBlue100 : Html.Attribute msg
toBlue100 =
    A.class "to-blue-100"


toBlue200 : Html.Attribute msg
toBlue200 =
    A.class "to-blue-200"


toBlue300 : Html.Attribute msg
toBlue300 =
    A.class "to-blue-300"


toBlue400 : Html.Attribute msg
toBlue400 =
    A.class "to-blue-400"


toBlue500 : Html.Attribute msg
toBlue500 =
    A.class "to-blue-500"


toBlue600 : Html.Attribute msg
toBlue600 =
    A.class "to-blue-600"


toBlue700 : Html.Attribute msg
toBlue700 =
    A.class "to-blue-700"


toBlue800 : Html.Attribute msg
toBlue800 =
    A.class "to-blue-800"


toBlue900 : Html.Attribute msg
toBlue900 =
    A.class "to-blue-900"


toIndigo100 : Html.Attribute msg
toIndigo100 =
    A.class "to-indigo-100"


toIndigo200 : Html.Attribute msg
toIndigo200 =
    A.class "to-indigo-200"


toIndigo300 : Html.Attribute msg
toIndigo300 =
    A.class "to-indigo-300"


toIndigo400 : Html.Attribute msg
toIndigo400 =
    A.class "to-indigo-400"


toIndigo500 : Html.Attribute msg
toIndigo500 =
    A.class "to-indigo-500"


toIndigo600 : Html.Attribute msg
toIndigo600 =
    A.class "to-indigo-600"


toIndigo700 : Html.Attribute msg
toIndigo700 =
    A.class "to-indigo-700"


toIndigo800 : Html.Attribute msg
toIndigo800 =
    A.class "to-indigo-800"


toIndigo900 : Html.Attribute msg
toIndigo900 =
    A.class "to-indigo-900"


toPurple100 : Html.Attribute msg
toPurple100 =
    A.class "to-purple-100"


toPurple200 : Html.Attribute msg
toPurple200 =
    A.class "to-purple-200"


toPurple300 : Html.Attribute msg
toPurple300 =
    A.class "to-purple-300"


toPurple400 : Html.Attribute msg
toPurple400 =
    A.class "to-purple-400"


toPurple500 : Html.Attribute msg
toPurple500 =
    A.class "to-purple-500"


toPurple600 : Html.Attribute msg
toPurple600 =
    A.class "to-purple-600"


toPurple700 : Html.Attribute msg
toPurple700 =
    A.class "to-purple-700"


toPurple800 : Html.Attribute msg
toPurple800 =
    A.class "to-purple-800"


toPurple900 : Html.Attribute msg
toPurple900 =
    A.class "to-purple-900"


toPink100 : Html.Attribute msg
toPink100 =
    A.class "to-pink-100"


toPink200 : Html.Attribute msg
toPink200 =
    A.class "to-pink-200"


toPink300 : Html.Attribute msg
toPink300 =
    A.class "to-pink-300"


toPink400 : Html.Attribute msg
toPink400 =
    A.class "to-pink-400"


toPink500 : Html.Attribute msg
toPink500 =
    A.class "to-pink-500"


toPink600 : Html.Attribute msg
toPink600 =
    A.class "to-pink-600"


toPink700 : Html.Attribute msg
toPink700 =
    A.class "to-pink-700"


toPink800 : Html.Attribute msg
toPink800 =
    A.class "to-pink-800"


toPink900 : Html.Attribute msg
toPink900 =
    A.class "to-pink-900"


hoverFromTransparent : Html.Attribute msg
hoverFromTransparent =
    A.class "hover:from-transparent"


hoverFromCurrent : Html.Attribute msg
hoverFromCurrent =
    A.class "hover:from-current"


hoverFromBlack : Html.Attribute msg
hoverFromBlack =
    A.class "hover:from-black"


hoverFromWhite : Html.Attribute msg
hoverFromWhite =
    A.class "hover:from-white"


hoverFromGray100 : Html.Attribute msg
hoverFromGray100 =
    A.class "hover:from-gray-100"


hoverFromGray200 : Html.Attribute msg
hoverFromGray200 =
    A.class "hover:from-gray-200"


hoverFromGray300 : Html.Attribute msg
hoverFromGray300 =
    A.class "hover:from-gray-300"


hoverFromGray400 : Html.Attribute msg
hoverFromGray400 =
    A.class "hover:from-gray-400"


hoverFromGray500 : Html.Attribute msg
hoverFromGray500 =
    A.class "hover:from-gray-500"


hoverFromGray600 : Html.Attribute msg
hoverFromGray600 =
    A.class "hover:from-gray-600"


hoverFromGray700 : Html.Attribute msg
hoverFromGray700 =
    A.class "hover:from-gray-700"


hoverFromGray800 : Html.Attribute msg
hoverFromGray800 =
    A.class "hover:from-gray-800"


hoverFromGray900 : Html.Attribute msg
hoverFromGray900 =
    A.class "hover:from-gray-900"


hoverFromRed100 : Html.Attribute msg
hoverFromRed100 =
    A.class "hover:from-red-100"


hoverFromRed200 : Html.Attribute msg
hoverFromRed200 =
    A.class "hover:from-red-200"


hoverFromRed300 : Html.Attribute msg
hoverFromRed300 =
    A.class "hover:from-red-300"


hoverFromRed400 : Html.Attribute msg
hoverFromRed400 =
    A.class "hover:from-red-400"


hoverFromRed500 : Html.Attribute msg
hoverFromRed500 =
    A.class "hover:from-red-500"


hoverFromRed600 : Html.Attribute msg
hoverFromRed600 =
    A.class "hover:from-red-600"


hoverFromRed700 : Html.Attribute msg
hoverFromRed700 =
    A.class "hover:from-red-700"


hoverFromRed800 : Html.Attribute msg
hoverFromRed800 =
    A.class "hover:from-red-800"


hoverFromRed900 : Html.Attribute msg
hoverFromRed900 =
    A.class "hover:from-red-900"


hoverFromOrange100 : Html.Attribute msg
hoverFromOrange100 =
    A.class "hover:from-orange-100"


hoverFromOrange200 : Html.Attribute msg
hoverFromOrange200 =
    A.class "hover:from-orange-200"


hoverFromOrange300 : Html.Attribute msg
hoverFromOrange300 =
    A.class "hover:from-orange-300"


hoverFromOrange400 : Html.Attribute msg
hoverFromOrange400 =
    A.class "hover:from-orange-400"


hoverFromOrange500 : Html.Attribute msg
hoverFromOrange500 =
    A.class "hover:from-orange-500"


hoverFromOrange600 : Html.Attribute msg
hoverFromOrange600 =
    A.class "hover:from-orange-600"


hoverFromOrange700 : Html.Attribute msg
hoverFromOrange700 =
    A.class "hover:from-orange-700"


hoverFromOrange800 : Html.Attribute msg
hoverFromOrange800 =
    A.class "hover:from-orange-800"


hoverFromOrange900 : Html.Attribute msg
hoverFromOrange900 =
    A.class "hover:from-orange-900"


hoverFromYellow100 : Html.Attribute msg
hoverFromYellow100 =
    A.class "hover:from-yellow-100"


hoverFromYellow200 : Html.Attribute msg
hoverFromYellow200 =
    A.class "hover:from-yellow-200"


hoverFromYellow300 : Html.Attribute msg
hoverFromYellow300 =
    A.class "hover:from-yellow-300"


hoverFromYellow400 : Html.Attribute msg
hoverFromYellow400 =
    A.class "hover:from-yellow-400"


hoverFromYellow500 : Html.Attribute msg
hoverFromYellow500 =
    A.class "hover:from-yellow-500"


hoverFromYellow600 : Html.Attribute msg
hoverFromYellow600 =
    A.class "hover:from-yellow-600"


hoverFromYellow700 : Html.Attribute msg
hoverFromYellow700 =
    A.class "hover:from-yellow-700"


hoverFromYellow800 : Html.Attribute msg
hoverFromYellow800 =
    A.class "hover:from-yellow-800"


hoverFromYellow900 : Html.Attribute msg
hoverFromYellow900 =
    A.class "hover:from-yellow-900"


hoverFromGreen100 : Html.Attribute msg
hoverFromGreen100 =
    A.class "hover:from-green-100"


hoverFromGreen200 : Html.Attribute msg
hoverFromGreen200 =
    A.class "hover:from-green-200"


hoverFromGreen300 : Html.Attribute msg
hoverFromGreen300 =
    A.class "hover:from-green-300"


hoverFromGreen400 : Html.Attribute msg
hoverFromGreen400 =
    A.class "hover:from-green-400"


hoverFromGreen500 : Html.Attribute msg
hoverFromGreen500 =
    A.class "hover:from-green-500"


hoverFromGreen600 : Html.Attribute msg
hoverFromGreen600 =
    A.class "hover:from-green-600"


hoverFromGreen700 : Html.Attribute msg
hoverFromGreen700 =
    A.class "hover:from-green-700"


hoverFromGreen800 : Html.Attribute msg
hoverFromGreen800 =
    A.class "hover:from-green-800"


hoverFromGreen900 : Html.Attribute msg
hoverFromGreen900 =
    A.class "hover:from-green-900"


hoverFromTeal100 : Html.Attribute msg
hoverFromTeal100 =
    A.class "hover:from-teal-100"


hoverFromTeal200 : Html.Attribute msg
hoverFromTeal200 =
    A.class "hover:from-teal-200"


hoverFromTeal300 : Html.Attribute msg
hoverFromTeal300 =
    A.class "hover:from-teal-300"


hoverFromTeal400 : Html.Attribute msg
hoverFromTeal400 =
    A.class "hover:from-teal-400"


hoverFromTeal500 : Html.Attribute msg
hoverFromTeal500 =
    A.class "hover:from-teal-500"


hoverFromTeal600 : Html.Attribute msg
hoverFromTeal600 =
    A.class "hover:from-teal-600"


hoverFromTeal700 : Html.Attribute msg
hoverFromTeal700 =
    A.class "hover:from-teal-700"


hoverFromTeal800 : Html.Attribute msg
hoverFromTeal800 =
    A.class "hover:from-teal-800"


hoverFromTeal900 : Html.Attribute msg
hoverFromTeal900 =
    A.class "hover:from-teal-900"


hoverFromBlue100 : Html.Attribute msg
hoverFromBlue100 =
    A.class "hover:from-blue-100"


hoverFromBlue200 : Html.Attribute msg
hoverFromBlue200 =
    A.class "hover:from-blue-200"


hoverFromBlue300 : Html.Attribute msg
hoverFromBlue300 =
    A.class "hover:from-blue-300"


hoverFromBlue400 : Html.Attribute msg
hoverFromBlue400 =
    A.class "hover:from-blue-400"


hoverFromBlue500 : Html.Attribute msg
hoverFromBlue500 =
    A.class "hover:from-blue-500"


hoverFromBlue600 : Html.Attribute msg
hoverFromBlue600 =
    A.class "hover:from-blue-600"


hoverFromBlue700 : Html.Attribute msg
hoverFromBlue700 =
    A.class "hover:from-blue-700"


hoverFromBlue800 : Html.Attribute msg
hoverFromBlue800 =
    A.class "hover:from-blue-800"


hoverFromBlue900 : Html.Attribute msg
hoverFromBlue900 =
    A.class "hover:from-blue-900"


hoverFromIndigo100 : Html.Attribute msg
hoverFromIndigo100 =
    A.class "hover:from-indigo-100"


hoverFromIndigo200 : Html.Attribute msg
hoverFromIndigo200 =
    A.class "hover:from-indigo-200"


hoverFromIndigo300 : Html.Attribute msg
hoverFromIndigo300 =
    A.class "hover:from-indigo-300"


hoverFromIndigo400 : Html.Attribute msg
hoverFromIndigo400 =
    A.class "hover:from-indigo-400"


hoverFromIndigo500 : Html.Attribute msg
hoverFromIndigo500 =
    A.class "hover:from-indigo-500"


hoverFromIndigo600 : Html.Attribute msg
hoverFromIndigo600 =
    A.class "hover:from-indigo-600"


hoverFromIndigo700 : Html.Attribute msg
hoverFromIndigo700 =
    A.class "hover:from-indigo-700"


hoverFromIndigo800 : Html.Attribute msg
hoverFromIndigo800 =
    A.class "hover:from-indigo-800"


hoverFromIndigo900 : Html.Attribute msg
hoverFromIndigo900 =
    A.class "hover:from-indigo-900"


hoverFromPurple100 : Html.Attribute msg
hoverFromPurple100 =
    A.class "hover:from-purple-100"


hoverFromPurple200 : Html.Attribute msg
hoverFromPurple200 =
    A.class "hover:from-purple-200"


hoverFromPurple300 : Html.Attribute msg
hoverFromPurple300 =
    A.class "hover:from-purple-300"


hoverFromPurple400 : Html.Attribute msg
hoverFromPurple400 =
    A.class "hover:from-purple-400"


hoverFromPurple500 : Html.Attribute msg
hoverFromPurple500 =
    A.class "hover:from-purple-500"


hoverFromPurple600 : Html.Attribute msg
hoverFromPurple600 =
    A.class "hover:from-purple-600"


hoverFromPurple700 : Html.Attribute msg
hoverFromPurple700 =
    A.class "hover:from-purple-700"


hoverFromPurple800 : Html.Attribute msg
hoverFromPurple800 =
    A.class "hover:from-purple-800"


hoverFromPurple900 : Html.Attribute msg
hoverFromPurple900 =
    A.class "hover:from-purple-900"


hoverFromPink100 : Html.Attribute msg
hoverFromPink100 =
    A.class "hover:from-pink-100"


hoverFromPink200 : Html.Attribute msg
hoverFromPink200 =
    A.class "hover:from-pink-200"


hoverFromPink300 : Html.Attribute msg
hoverFromPink300 =
    A.class "hover:from-pink-300"


hoverFromPink400 : Html.Attribute msg
hoverFromPink400 =
    A.class "hover:from-pink-400"


hoverFromPink500 : Html.Attribute msg
hoverFromPink500 =
    A.class "hover:from-pink-500"


hoverFromPink600 : Html.Attribute msg
hoverFromPink600 =
    A.class "hover:from-pink-600"


hoverFromPink700 : Html.Attribute msg
hoverFromPink700 =
    A.class "hover:from-pink-700"


hoverFromPink800 : Html.Attribute msg
hoverFromPink800 =
    A.class "hover:from-pink-800"


hoverFromPink900 : Html.Attribute msg
hoverFromPink900 =
    A.class "hover:from-pink-900"


hoverViaTransparent : Html.Attribute msg
hoverViaTransparent =
    A.class "hover:via-transparent"


hoverViaCurrent : Html.Attribute msg
hoverViaCurrent =
    A.class "hover:via-current"


hoverViaBlack : Html.Attribute msg
hoverViaBlack =
    A.class "hover:via-black"


hoverViaWhite : Html.Attribute msg
hoverViaWhite =
    A.class "hover:via-white"


hoverViaGray100 : Html.Attribute msg
hoverViaGray100 =
    A.class "hover:via-gray-100"


hoverViaGray200 : Html.Attribute msg
hoverViaGray200 =
    A.class "hover:via-gray-200"


hoverViaGray300 : Html.Attribute msg
hoverViaGray300 =
    A.class "hover:via-gray-300"


hoverViaGray400 : Html.Attribute msg
hoverViaGray400 =
    A.class "hover:via-gray-400"


hoverViaGray500 : Html.Attribute msg
hoverViaGray500 =
    A.class "hover:via-gray-500"


hoverViaGray600 : Html.Attribute msg
hoverViaGray600 =
    A.class "hover:via-gray-600"


hoverViaGray700 : Html.Attribute msg
hoverViaGray700 =
    A.class "hover:via-gray-700"


hoverViaGray800 : Html.Attribute msg
hoverViaGray800 =
    A.class "hover:via-gray-800"


hoverViaGray900 : Html.Attribute msg
hoverViaGray900 =
    A.class "hover:via-gray-900"


hoverViaRed100 : Html.Attribute msg
hoverViaRed100 =
    A.class "hover:via-red-100"


hoverViaRed200 : Html.Attribute msg
hoverViaRed200 =
    A.class "hover:via-red-200"


hoverViaRed300 : Html.Attribute msg
hoverViaRed300 =
    A.class "hover:via-red-300"


hoverViaRed400 : Html.Attribute msg
hoverViaRed400 =
    A.class "hover:via-red-400"


hoverViaRed500 : Html.Attribute msg
hoverViaRed500 =
    A.class "hover:via-red-500"


hoverViaRed600 : Html.Attribute msg
hoverViaRed600 =
    A.class "hover:via-red-600"


hoverViaRed700 : Html.Attribute msg
hoverViaRed700 =
    A.class "hover:via-red-700"


hoverViaRed800 : Html.Attribute msg
hoverViaRed800 =
    A.class "hover:via-red-800"


hoverViaRed900 : Html.Attribute msg
hoverViaRed900 =
    A.class "hover:via-red-900"


hoverViaOrange100 : Html.Attribute msg
hoverViaOrange100 =
    A.class "hover:via-orange-100"


hoverViaOrange200 : Html.Attribute msg
hoverViaOrange200 =
    A.class "hover:via-orange-200"


hoverViaOrange300 : Html.Attribute msg
hoverViaOrange300 =
    A.class "hover:via-orange-300"


hoverViaOrange400 : Html.Attribute msg
hoverViaOrange400 =
    A.class "hover:via-orange-400"


hoverViaOrange500 : Html.Attribute msg
hoverViaOrange500 =
    A.class "hover:via-orange-500"


hoverViaOrange600 : Html.Attribute msg
hoverViaOrange600 =
    A.class "hover:via-orange-600"


hoverViaOrange700 : Html.Attribute msg
hoverViaOrange700 =
    A.class "hover:via-orange-700"


hoverViaOrange800 : Html.Attribute msg
hoverViaOrange800 =
    A.class "hover:via-orange-800"


hoverViaOrange900 : Html.Attribute msg
hoverViaOrange900 =
    A.class "hover:via-orange-900"


hoverViaYellow100 : Html.Attribute msg
hoverViaYellow100 =
    A.class "hover:via-yellow-100"


hoverViaYellow200 : Html.Attribute msg
hoverViaYellow200 =
    A.class "hover:via-yellow-200"


hoverViaYellow300 : Html.Attribute msg
hoverViaYellow300 =
    A.class "hover:via-yellow-300"


hoverViaYellow400 : Html.Attribute msg
hoverViaYellow400 =
    A.class "hover:via-yellow-400"


hoverViaYellow500 : Html.Attribute msg
hoverViaYellow500 =
    A.class "hover:via-yellow-500"


hoverViaYellow600 : Html.Attribute msg
hoverViaYellow600 =
    A.class "hover:via-yellow-600"


hoverViaYellow700 : Html.Attribute msg
hoverViaYellow700 =
    A.class "hover:via-yellow-700"


hoverViaYellow800 : Html.Attribute msg
hoverViaYellow800 =
    A.class "hover:via-yellow-800"


hoverViaYellow900 : Html.Attribute msg
hoverViaYellow900 =
    A.class "hover:via-yellow-900"


hoverViaGreen100 : Html.Attribute msg
hoverViaGreen100 =
    A.class "hover:via-green-100"


hoverViaGreen200 : Html.Attribute msg
hoverViaGreen200 =
    A.class "hover:via-green-200"


hoverViaGreen300 : Html.Attribute msg
hoverViaGreen300 =
    A.class "hover:via-green-300"


hoverViaGreen400 : Html.Attribute msg
hoverViaGreen400 =
    A.class "hover:via-green-400"


hoverViaGreen500 : Html.Attribute msg
hoverViaGreen500 =
    A.class "hover:via-green-500"


hoverViaGreen600 : Html.Attribute msg
hoverViaGreen600 =
    A.class "hover:via-green-600"


hoverViaGreen700 : Html.Attribute msg
hoverViaGreen700 =
    A.class "hover:via-green-700"


hoverViaGreen800 : Html.Attribute msg
hoverViaGreen800 =
    A.class "hover:via-green-800"


hoverViaGreen900 : Html.Attribute msg
hoverViaGreen900 =
    A.class "hover:via-green-900"


hoverViaTeal100 : Html.Attribute msg
hoverViaTeal100 =
    A.class "hover:via-teal-100"


hoverViaTeal200 : Html.Attribute msg
hoverViaTeal200 =
    A.class "hover:via-teal-200"


hoverViaTeal300 : Html.Attribute msg
hoverViaTeal300 =
    A.class "hover:via-teal-300"


hoverViaTeal400 : Html.Attribute msg
hoverViaTeal400 =
    A.class "hover:via-teal-400"


hoverViaTeal500 : Html.Attribute msg
hoverViaTeal500 =
    A.class "hover:via-teal-500"


hoverViaTeal600 : Html.Attribute msg
hoverViaTeal600 =
    A.class "hover:via-teal-600"


hoverViaTeal700 : Html.Attribute msg
hoverViaTeal700 =
    A.class "hover:via-teal-700"


hoverViaTeal800 : Html.Attribute msg
hoverViaTeal800 =
    A.class "hover:via-teal-800"


hoverViaTeal900 : Html.Attribute msg
hoverViaTeal900 =
    A.class "hover:via-teal-900"


hoverViaBlue100 : Html.Attribute msg
hoverViaBlue100 =
    A.class "hover:via-blue-100"


hoverViaBlue200 : Html.Attribute msg
hoverViaBlue200 =
    A.class "hover:via-blue-200"


hoverViaBlue300 : Html.Attribute msg
hoverViaBlue300 =
    A.class "hover:via-blue-300"


hoverViaBlue400 : Html.Attribute msg
hoverViaBlue400 =
    A.class "hover:via-blue-400"


hoverViaBlue500 : Html.Attribute msg
hoverViaBlue500 =
    A.class "hover:via-blue-500"


hoverViaBlue600 : Html.Attribute msg
hoverViaBlue600 =
    A.class "hover:via-blue-600"


hoverViaBlue700 : Html.Attribute msg
hoverViaBlue700 =
    A.class "hover:via-blue-700"


hoverViaBlue800 : Html.Attribute msg
hoverViaBlue800 =
    A.class "hover:via-blue-800"


hoverViaBlue900 : Html.Attribute msg
hoverViaBlue900 =
    A.class "hover:via-blue-900"


hoverViaIndigo100 : Html.Attribute msg
hoverViaIndigo100 =
    A.class "hover:via-indigo-100"


hoverViaIndigo200 : Html.Attribute msg
hoverViaIndigo200 =
    A.class "hover:via-indigo-200"


hoverViaIndigo300 : Html.Attribute msg
hoverViaIndigo300 =
    A.class "hover:via-indigo-300"


hoverViaIndigo400 : Html.Attribute msg
hoverViaIndigo400 =
    A.class "hover:via-indigo-400"


hoverViaIndigo500 : Html.Attribute msg
hoverViaIndigo500 =
    A.class "hover:via-indigo-500"


hoverViaIndigo600 : Html.Attribute msg
hoverViaIndigo600 =
    A.class "hover:via-indigo-600"


hoverViaIndigo700 : Html.Attribute msg
hoverViaIndigo700 =
    A.class "hover:via-indigo-700"


hoverViaIndigo800 : Html.Attribute msg
hoverViaIndigo800 =
    A.class "hover:via-indigo-800"


hoverViaIndigo900 : Html.Attribute msg
hoverViaIndigo900 =
    A.class "hover:via-indigo-900"


hoverViaPurple100 : Html.Attribute msg
hoverViaPurple100 =
    A.class "hover:via-purple-100"


hoverViaPurple200 : Html.Attribute msg
hoverViaPurple200 =
    A.class "hover:via-purple-200"


hoverViaPurple300 : Html.Attribute msg
hoverViaPurple300 =
    A.class "hover:via-purple-300"


hoverViaPurple400 : Html.Attribute msg
hoverViaPurple400 =
    A.class "hover:via-purple-400"


hoverViaPurple500 : Html.Attribute msg
hoverViaPurple500 =
    A.class "hover:via-purple-500"


hoverViaPurple600 : Html.Attribute msg
hoverViaPurple600 =
    A.class "hover:via-purple-600"


hoverViaPurple700 : Html.Attribute msg
hoverViaPurple700 =
    A.class "hover:via-purple-700"


hoverViaPurple800 : Html.Attribute msg
hoverViaPurple800 =
    A.class "hover:via-purple-800"


hoverViaPurple900 : Html.Attribute msg
hoverViaPurple900 =
    A.class "hover:via-purple-900"


hoverViaPink100 : Html.Attribute msg
hoverViaPink100 =
    A.class "hover:via-pink-100"


hoverViaPink200 : Html.Attribute msg
hoverViaPink200 =
    A.class "hover:via-pink-200"


hoverViaPink300 : Html.Attribute msg
hoverViaPink300 =
    A.class "hover:via-pink-300"


hoverViaPink400 : Html.Attribute msg
hoverViaPink400 =
    A.class "hover:via-pink-400"


hoverViaPink500 : Html.Attribute msg
hoverViaPink500 =
    A.class "hover:via-pink-500"


hoverViaPink600 : Html.Attribute msg
hoverViaPink600 =
    A.class "hover:via-pink-600"


hoverViaPink700 : Html.Attribute msg
hoverViaPink700 =
    A.class "hover:via-pink-700"


hoverViaPink800 : Html.Attribute msg
hoverViaPink800 =
    A.class "hover:via-pink-800"


hoverViaPink900 : Html.Attribute msg
hoverViaPink900 =
    A.class "hover:via-pink-900"


hoverToTransparent : Html.Attribute msg
hoverToTransparent =
    A.class "hover:to-transparent"


hoverToCurrent : Html.Attribute msg
hoverToCurrent =
    A.class "hover:to-current"


hoverToBlack : Html.Attribute msg
hoverToBlack =
    A.class "hover:to-black"


hoverToWhite : Html.Attribute msg
hoverToWhite =
    A.class "hover:to-white"


hoverToGray100 : Html.Attribute msg
hoverToGray100 =
    A.class "hover:to-gray-100"


hoverToGray200 : Html.Attribute msg
hoverToGray200 =
    A.class "hover:to-gray-200"


hoverToGray300 : Html.Attribute msg
hoverToGray300 =
    A.class "hover:to-gray-300"


hoverToGray400 : Html.Attribute msg
hoverToGray400 =
    A.class "hover:to-gray-400"


hoverToGray500 : Html.Attribute msg
hoverToGray500 =
    A.class "hover:to-gray-500"


hoverToGray600 : Html.Attribute msg
hoverToGray600 =
    A.class "hover:to-gray-600"


hoverToGray700 : Html.Attribute msg
hoverToGray700 =
    A.class "hover:to-gray-700"


hoverToGray800 : Html.Attribute msg
hoverToGray800 =
    A.class "hover:to-gray-800"


hoverToGray900 : Html.Attribute msg
hoverToGray900 =
    A.class "hover:to-gray-900"


hoverToRed100 : Html.Attribute msg
hoverToRed100 =
    A.class "hover:to-red-100"


hoverToRed200 : Html.Attribute msg
hoverToRed200 =
    A.class "hover:to-red-200"


hoverToRed300 : Html.Attribute msg
hoverToRed300 =
    A.class "hover:to-red-300"


hoverToRed400 : Html.Attribute msg
hoverToRed400 =
    A.class "hover:to-red-400"


hoverToRed500 : Html.Attribute msg
hoverToRed500 =
    A.class "hover:to-red-500"


hoverToRed600 : Html.Attribute msg
hoverToRed600 =
    A.class "hover:to-red-600"


hoverToRed700 : Html.Attribute msg
hoverToRed700 =
    A.class "hover:to-red-700"


hoverToRed800 : Html.Attribute msg
hoverToRed800 =
    A.class "hover:to-red-800"


hoverToRed900 : Html.Attribute msg
hoverToRed900 =
    A.class "hover:to-red-900"


hoverToOrange100 : Html.Attribute msg
hoverToOrange100 =
    A.class "hover:to-orange-100"


hoverToOrange200 : Html.Attribute msg
hoverToOrange200 =
    A.class "hover:to-orange-200"


hoverToOrange300 : Html.Attribute msg
hoverToOrange300 =
    A.class "hover:to-orange-300"


hoverToOrange400 : Html.Attribute msg
hoverToOrange400 =
    A.class "hover:to-orange-400"


hoverToOrange500 : Html.Attribute msg
hoverToOrange500 =
    A.class "hover:to-orange-500"


hoverToOrange600 : Html.Attribute msg
hoverToOrange600 =
    A.class "hover:to-orange-600"


hoverToOrange700 : Html.Attribute msg
hoverToOrange700 =
    A.class "hover:to-orange-700"


hoverToOrange800 : Html.Attribute msg
hoverToOrange800 =
    A.class "hover:to-orange-800"


hoverToOrange900 : Html.Attribute msg
hoverToOrange900 =
    A.class "hover:to-orange-900"


hoverToYellow100 : Html.Attribute msg
hoverToYellow100 =
    A.class "hover:to-yellow-100"


hoverToYellow200 : Html.Attribute msg
hoverToYellow200 =
    A.class "hover:to-yellow-200"


hoverToYellow300 : Html.Attribute msg
hoverToYellow300 =
    A.class "hover:to-yellow-300"


hoverToYellow400 : Html.Attribute msg
hoverToYellow400 =
    A.class "hover:to-yellow-400"


hoverToYellow500 : Html.Attribute msg
hoverToYellow500 =
    A.class "hover:to-yellow-500"


hoverToYellow600 : Html.Attribute msg
hoverToYellow600 =
    A.class "hover:to-yellow-600"


hoverToYellow700 : Html.Attribute msg
hoverToYellow700 =
    A.class "hover:to-yellow-700"


hoverToYellow800 : Html.Attribute msg
hoverToYellow800 =
    A.class "hover:to-yellow-800"


hoverToYellow900 : Html.Attribute msg
hoverToYellow900 =
    A.class "hover:to-yellow-900"


hoverToGreen100 : Html.Attribute msg
hoverToGreen100 =
    A.class "hover:to-green-100"


hoverToGreen200 : Html.Attribute msg
hoverToGreen200 =
    A.class "hover:to-green-200"


hoverToGreen300 : Html.Attribute msg
hoverToGreen300 =
    A.class "hover:to-green-300"


hoverToGreen400 : Html.Attribute msg
hoverToGreen400 =
    A.class "hover:to-green-400"


hoverToGreen500 : Html.Attribute msg
hoverToGreen500 =
    A.class "hover:to-green-500"


hoverToGreen600 : Html.Attribute msg
hoverToGreen600 =
    A.class "hover:to-green-600"


hoverToGreen700 : Html.Attribute msg
hoverToGreen700 =
    A.class "hover:to-green-700"


hoverToGreen800 : Html.Attribute msg
hoverToGreen800 =
    A.class "hover:to-green-800"


hoverToGreen900 : Html.Attribute msg
hoverToGreen900 =
    A.class "hover:to-green-900"


hoverToTeal100 : Html.Attribute msg
hoverToTeal100 =
    A.class "hover:to-teal-100"


hoverToTeal200 : Html.Attribute msg
hoverToTeal200 =
    A.class "hover:to-teal-200"


hoverToTeal300 : Html.Attribute msg
hoverToTeal300 =
    A.class "hover:to-teal-300"


hoverToTeal400 : Html.Attribute msg
hoverToTeal400 =
    A.class "hover:to-teal-400"


hoverToTeal500 : Html.Attribute msg
hoverToTeal500 =
    A.class "hover:to-teal-500"


hoverToTeal600 : Html.Attribute msg
hoverToTeal600 =
    A.class "hover:to-teal-600"


hoverToTeal700 : Html.Attribute msg
hoverToTeal700 =
    A.class "hover:to-teal-700"


hoverToTeal800 : Html.Attribute msg
hoverToTeal800 =
    A.class "hover:to-teal-800"


hoverToTeal900 : Html.Attribute msg
hoverToTeal900 =
    A.class "hover:to-teal-900"


hoverToBlue100 : Html.Attribute msg
hoverToBlue100 =
    A.class "hover:to-blue-100"


hoverToBlue200 : Html.Attribute msg
hoverToBlue200 =
    A.class "hover:to-blue-200"


hoverToBlue300 : Html.Attribute msg
hoverToBlue300 =
    A.class "hover:to-blue-300"


hoverToBlue400 : Html.Attribute msg
hoverToBlue400 =
    A.class "hover:to-blue-400"


hoverToBlue500 : Html.Attribute msg
hoverToBlue500 =
    A.class "hover:to-blue-500"


hoverToBlue600 : Html.Attribute msg
hoverToBlue600 =
    A.class "hover:to-blue-600"


hoverToBlue700 : Html.Attribute msg
hoverToBlue700 =
    A.class "hover:to-blue-700"


hoverToBlue800 : Html.Attribute msg
hoverToBlue800 =
    A.class "hover:to-blue-800"


hoverToBlue900 : Html.Attribute msg
hoverToBlue900 =
    A.class "hover:to-blue-900"


hoverToIndigo100 : Html.Attribute msg
hoverToIndigo100 =
    A.class "hover:to-indigo-100"


hoverToIndigo200 : Html.Attribute msg
hoverToIndigo200 =
    A.class "hover:to-indigo-200"


hoverToIndigo300 : Html.Attribute msg
hoverToIndigo300 =
    A.class "hover:to-indigo-300"


hoverToIndigo400 : Html.Attribute msg
hoverToIndigo400 =
    A.class "hover:to-indigo-400"


hoverToIndigo500 : Html.Attribute msg
hoverToIndigo500 =
    A.class "hover:to-indigo-500"


hoverToIndigo600 : Html.Attribute msg
hoverToIndigo600 =
    A.class "hover:to-indigo-600"


hoverToIndigo700 : Html.Attribute msg
hoverToIndigo700 =
    A.class "hover:to-indigo-700"


hoverToIndigo800 : Html.Attribute msg
hoverToIndigo800 =
    A.class "hover:to-indigo-800"


hoverToIndigo900 : Html.Attribute msg
hoverToIndigo900 =
    A.class "hover:to-indigo-900"


hoverToPurple100 : Html.Attribute msg
hoverToPurple100 =
    A.class "hover:to-purple-100"


hoverToPurple200 : Html.Attribute msg
hoverToPurple200 =
    A.class "hover:to-purple-200"


hoverToPurple300 : Html.Attribute msg
hoverToPurple300 =
    A.class "hover:to-purple-300"


hoverToPurple400 : Html.Attribute msg
hoverToPurple400 =
    A.class "hover:to-purple-400"


hoverToPurple500 : Html.Attribute msg
hoverToPurple500 =
    A.class "hover:to-purple-500"


hoverToPurple600 : Html.Attribute msg
hoverToPurple600 =
    A.class "hover:to-purple-600"


hoverToPurple700 : Html.Attribute msg
hoverToPurple700 =
    A.class "hover:to-purple-700"


hoverToPurple800 : Html.Attribute msg
hoverToPurple800 =
    A.class "hover:to-purple-800"


hoverToPurple900 : Html.Attribute msg
hoverToPurple900 =
    A.class "hover:to-purple-900"


hoverToPink100 : Html.Attribute msg
hoverToPink100 =
    A.class "hover:to-pink-100"


hoverToPink200 : Html.Attribute msg
hoverToPink200 =
    A.class "hover:to-pink-200"


hoverToPink300 : Html.Attribute msg
hoverToPink300 =
    A.class "hover:to-pink-300"


hoverToPink400 : Html.Attribute msg
hoverToPink400 =
    A.class "hover:to-pink-400"


hoverToPink500 : Html.Attribute msg
hoverToPink500 =
    A.class "hover:to-pink-500"


hoverToPink600 : Html.Attribute msg
hoverToPink600 =
    A.class "hover:to-pink-600"


hoverToPink700 : Html.Attribute msg
hoverToPink700 =
    A.class "hover:to-pink-700"


hoverToPink800 : Html.Attribute msg
hoverToPink800 =
    A.class "hover:to-pink-800"


hoverToPink900 : Html.Attribute msg
hoverToPink900 =
    A.class "hover:to-pink-900"


focusFromTransparent : Html.Attribute msg
focusFromTransparent =
    A.class "focus:from-transparent"


focusFromCurrent : Html.Attribute msg
focusFromCurrent =
    A.class "focus:from-current"


focusFromBlack : Html.Attribute msg
focusFromBlack =
    A.class "focus:from-black"


focusFromWhite : Html.Attribute msg
focusFromWhite =
    A.class "focus:from-white"


focusFromGray100 : Html.Attribute msg
focusFromGray100 =
    A.class "focus:from-gray-100"


focusFromGray200 : Html.Attribute msg
focusFromGray200 =
    A.class "focus:from-gray-200"


focusFromGray300 : Html.Attribute msg
focusFromGray300 =
    A.class "focus:from-gray-300"


focusFromGray400 : Html.Attribute msg
focusFromGray400 =
    A.class "focus:from-gray-400"


focusFromGray500 : Html.Attribute msg
focusFromGray500 =
    A.class "focus:from-gray-500"


focusFromGray600 : Html.Attribute msg
focusFromGray600 =
    A.class "focus:from-gray-600"


focusFromGray700 : Html.Attribute msg
focusFromGray700 =
    A.class "focus:from-gray-700"


focusFromGray800 : Html.Attribute msg
focusFromGray800 =
    A.class "focus:from-gray-800"


focusFromGray900 : Html.Attribute msg
focusFromGray900 =
    A.class "focus:from-gray-900"


focusFromRed100 : Html.Attribute msg
focusFromRed100 =
    A.class "focus:from-red-100"


focusFromRed200 : Html.Attribute msg
focusFromRed200 =
    A.class "focus:from-red-200"


focusFromRed300 : Html.Attribute msg
focusFromRed300 =
    A.class "focus:from-red-300"


focusFromRed400 : Html.Attribute msg
focusFromRed400 =
    A.class "focus:from-red-400"


focusFromRed500 : Html.Attribute msg
focusFromRed500 =
    A.class "focus:from-red-500"


focusFromRed600 : Html.Attribute msg
focusFromRed600 =
    A.class "focus:from-red-600"


focusFromRed700 : Html.Attribute msg
focusFromRed700 =
    A.class "focus:from-red-700"


focusFromRed800 : Html.Attribute msg
focusFromRed800 =
    A.class "focus:from-red-800"


focusFromRed900 : Html.Attribute msg
focusFromRed900 =
    A.class "focus:from-red-900"


focusFromOrange100 : Html.Attribute msg
focusFromOrange100 =
    A.class "focus:from-orange-100"


focusFromOrange200 : Html.Attribute msg
focusFromOrange200 =
    A.class "focus:from-orange-200"


focusFromOrange300 : Html.Attribute msg
focusFromOrange300 =
    A.class "focus:from-orange-300"


focusFromOrange400 : Html.Attribute msg
focusFromOrange400 =
    A.class "focus:from-orange-400"


focusFromOrange500 : Html.Attribute msg
focusFromOrange500 =
    A.class "focus:from-orange-500"


focusFromOrange600 : Html.Attribute msg
focusFromOrange600 =
    A.class "focus:from-orange-600"


focusFromOrange700 : Html.Attribute msg
focusFromOrange700 =
    A.class "focus:from-orange-700"


focusFromOrange800 : Html.Attribute msg
focusFromOrange800 =
    A.class "focus:from-orange-800"


focusFromOrange900 : Html.Attribute msg
focusFromOrange900 =
    A.class "focus:from-orange-900"


focusFromYellow100 : Html.Attribute msg
focusFromYellow100 =
    A.class "focus:from-yellow-100"


focusFromYellow200 : Html.Attribute msg
focusFromYellow200 =
    A.class "focus:from-yellow-200"


focusFromYellow300 : Html.Attribute msg
focusFromYellow300 =
    A.class "focus:from-yellow-300"


focusFromYellow400 : Html.Attribute msg
focusFromYellow400 =
    A.class "focus:from-yellow-400"


focusFromYellow500 : Html.Attribute msg
focusFromYellow500 =
    A.class "focus:from-yellow-500"


focusFromYellow600 : Html.Attribute msg
focusFromYellow600 =
    A.class "focus:from-yellow-600"


focusFromYellow700 : Html.Attribute msg
focusFromYellow700 =
    A.class "focus:from-yellow-700"


focusFromYellow800 : Html.Attribute msg
focusFromYellow800 =
    A.class "focus:from-yellow-800"


focusFromYellow900 : Html.Attribute msg
focusFromYellow900 =
    A.class "focus:from-yellow-900"


focusFromGreen100 : Html.Attribute msg
focusFromGreen100 =
    A.class "focus:from-green-100"


focusFromGreen200 : Html.Attribute msg
focusFromGreen200 =
    A.class "focus:from-green-200"


focusFromGreen300 : Html.Attribute msg
focusFromGreen300 =
    A.class "focus:from-green-300"


focusFromGreen400 : Html.Attribute msg
focusFromGreen400 =
    A.class "focus:from-green-400"


focusFromGreen500 : Html.Attribute msg
focusFromGreen500 =
    A.class "focus:from-green-500"


focusFromGreen600 : Html.Attribute msg
focusFromGreen600 =
    A.class "focus:from-green-600"


focusFromGreen700 : Html.Attribute msg
focusFromGreen700 =
    A.class "focus:from-green-700"


focusFromGreen800 : Html.Attribute msg
focusFromGreen800 =
    A.class "focus:from-green-800"


focusFromGreen900 : Html.Attribute msg
focusFromGreen900 =
    A.class "focus:from-green-900"


focusFromTeal100 : Html.Attribute msg
focusFromTeal100 =
    A.class "focus:from-teal-100"


focusFromTeal200 : Html.Attribute msg
focusFromTeal200 =
    A.class "focus:from-teal-200"


focusFromTeal300 : Html.Attribute msg
focusFromTeal300 =
    A.class "focus:from-teal-300"


focusFromTeal400 : Html.Attribute msg
focusFromTeal400 =
    A.class "focus:from-teal-400"


focusFromTeal500 : Html.Attribute msg
focusFromTeal500 =
    A.class "focus:from-teal-500"


focusFromTeal600 : Html.Attribute msg
focusFromTeal600 =
    A.class "focus:from-teal-600"


focusFromTeal700 : Html.Attribute msg
focusFromTeal700 =
    A.class "focus:from-teal-700"


focusFromTeal800 : Html.Attribute msg
focusFromTeal800 =
    A.class "focus:from-teal-800"


focusFromTeal900 : Html.Attribute msg
focusFromTeal900 =
    A.class "focus:from-teal-900"


focusFromBlue100 : Html.Attribute msg
focusFromBlue100 =
    A.class "focus:from-blue-100"


focusFromBlue200 : Html.Attribute msg
focusFromBlue200 =
    A.class "focus:from-blue-200"


focusFromBlue300 : Html.Attribute msg
focusFromBlue300 =
    A.class "focus:from-blue-300"


focusFromBlue400 : Html.Attribute msg
focusFromBlue400 =
    A.class "focus:from-blue-400"


focusFromBlue500 : Html.Attribute msg
focusFromBlue500 =
    A.class "focus:from-blue-500"


focusFromBlue600 : Html.Attribute msg
focusFromBlue600 =
    A.class "focus:from-blue-600"


focusFromBlue700 : Html.Attribute msg
focusFromBlue700 =
    A.class "focus:from-blue-700"


focusFromBlue800 : Html.Attribute msg
focusFromBlue800 =
    A.class "focus:from-blue-800"


focusFromBlue900 : Html.Attribute msg
focusFromBlue900 =
    A.class "focus:from-blue-900"


focusFromIndigo100 : Html.Attribute msg
focusFromIndigo100 =
    A.class "focus:from-indigo-100"


focusFromIndigo200 : Html.Attribute msg
focusFromIndigo200 =
    A.class "focus:from-indigo-200"


focusFromIndigo300 : Html.Attribute msg
focusFromIndigo300 =
    A.class "focus:from-indigo-300"


focusFromIndigo400 : Html.Attribute msg
focusFromIndigo400 =
    A.class "focus:from-indigo-400"


focusFromIndigo500 : Html.Attribute msg
focusFromIndigo500 =
    A.class "focus:from-indigo-500"


focusFromIndigo600 : Html.Attribute msg
focusFromIndigo600 =
    A.class "focus:from-indigo-600"


focusFromIndigo700 : Html.Attribute msg
focusFromIndigo700 =
    A.class "focus:from-indigo-700"


focusFromIndigo800 : Html.Attribute msg
focusFromIndigo800 =
    A.class "focus:from-indigo-800"


focusFromIndigo900 : Html.Attribute msg
focusFromIndigo900 =
    A.class "focus:from-indigo-900"


focusFromPurple100 : Html.Attribute msg
focusFromPurple100 =
    A.class "focus:from-purple-100"


focusFromPurple200 : Html.Attribute msg
focusFromPurple200 =
    A.class "focus:from-purple-200"


focusFromPurple300 : Html.Attribute msg
focusFromPurple300 =
    A.class "focus:from-purple-300"


focusFromPurple400 : Html.Attribute msg
focusFromPurple400 =
    A.class "focus:from-purple-400"


focusFromPurple500 : Html.Attribute msg
focusFromPurple500 =
    A.class "focus:from-purple-500"


focusFromPurple600 : Html.Attribute msg
focusFromPurple600 =
    A.class "focus:from-purple-600"


focusFromPurple700 : Html.Attribute msg
focusFromPurple700 =
    A.class "focus:from-purple-700"


focusFromPurple800 : Html.Attribute msg
focusFromPurple800 =
    A.class "focus:from-purple-800"


focusFromPurple900 : Html.Attribute msg
focusFromPurple900 =
    A.class "focus:from-purple-900"


focusFromPink100 : Html.Attribute msg
focusFromPink100 =
    A.class "focus:from-pink-100"


focusFromPink200 : Html.Attribute msg
focusFromPink200 =
    A.class "focus:from-pink-200"


focusFromPink300 : Html.Attribute msg
focusFromPink300 =
    A.class "focus:from-pink-300"


focusFromPink400 : Html.Attribute msg
focusFromPink400 =
    A.class "focus:from-pink-400"


focusFromPink500 : Html.Attribute msg
focusFromPink500 =
    A.class "focus:from-pink-500"


focusFromPink600 : Html.Attribute msg
focusFromPink600 =
    A.class "focus:from-pink-600"


focusFromPink700 : Html.Attribute msg
focusFromPink700 =
    A.class "focus:from-pink-700"


focusFromPink800 : Html.Attribute msg
focusFromPink800 =
    A.class "focus:from-pink-800"


focusFromPink900 : Html.Attribute msg
focusFromPink900 =
    A.class "focus:from-pink-900"


focusViaTransparent : Html.Attribute msg
focusViaTransparent =
    A.class "focus:via-transparent"


focusViaCurrent : Html.Attribute msg
focusViaCurrent =
    A.class "focus:via-current"


focusViaBlack : Html.Attribute msg
focusViaBlack =
    A.class "focus:via-black"


focusViaWhite : Html.Attribute msg
focusViaWhite =
    A.class "focus:via-white"


focusViaGray100 : Html.Attribute msg
focusViaGray100 =
    A.class "focus:via-gray-100"


focusViaGray200 : Html.Attribute msg
focusViaGray200 =
    A.class "focus:via-gray-200"


focusViaGray300 : Html.Attribute msg
focusViaGray300 =
    A.class "focus:via-gray-300"


focusViaGray400 : Html.Attribute msg
focusViaGray400 =
    A.class "focus:via-gray-400"


focusViaGray500 : Html.Attribute msg
focusViaGray500 =
    A.class "focus:via-gray-500"


focusViaGray600 : Html.Attribute msg
focusViaGray600 =
    A.class "focus:via-gray-600"


focusViaGray700 : Html.Attribute msg
focusViaGray700 =
    A.class "focus:via-gray-700"


focusViaGray800 : Html.Attribute msg
focusViaGray800 =
    A.class "focus:via-gray-800"


focusViaGray900 : Html.Attribute msg
focusViaGray900 =
    A.class "focus:via-gray-900"


focusViaRed100 : Html.Attribute msg
focusViaRed100 =
    A.class "focus:via-red-100"


focusViaRed200 : Html.Attribute msg
focusViaRed200 =
    A.class "focus:via-red-200"


focusViaRed300 : Html.Attribute msg
focusViaRed300 =
    A.class "focus:via-red-300"


focusViaRed400 : Html.Attribute msg
focusViaRed400 =
    A.class "focus:via-red-400"


focusViaRed500 : Html.Attribute msg
focusViaRed500 =
    A.class "focus:via-red-500"


focusViaRed600 : Html.Attribute msg
focusViaRed600 =
    A.class "focus:via-red-600"


focusViaRed700 : Html.Attribute msg
focusViaRed700 =
    A.class "focus:via-red-700"


focusViaRed800 : Html.Attribute msg
focusViaRed800 =
    A.class "focus:via-red-800"


focusViaRed900 : Html.Attribute msg
focusViaRed900 =
    A.class "focus:via-red-900"


focusViaOrange100 : Html.Attribute msg
focusViaOrange100 =
    A.class "focus:via-orange-100"


focusViaOrange200 : Html.Attribute msg
focusViaOrange200 =
    A.class "focus:via-orange-200"


focusViaOrange300 : Html.Attribute msg
focusViaOrange300 =
    A.class "focus:via-orange-300"


focusViaOrange400 : Html.Attribute msg
focusViaOrange400 =
    A.class "focus:via-orange-400"


focusViaOrange500 : Html.Attribute msg
focusViaOrange500 =
    A.class "focus:via-orange-500"


focusViaOrange600 : Html.Attribute msg
focusViaOrange600 =
    A.class "focus:via-orange-600"


focusViaOrange700 : Html.Attribute msg
focusViaOrange700 =
    A.class "focus:via-orange-700"


focusViaOrange800 : Html.Attribute msg
focusViaOrange800 =
    A.class "focus:via-orange-800"


focusViaOrange900 : Html.Attribute msg
focusViaOrange900 =
    A.class "focus:via-orange-900"


focusViaYellow100 : Html.Attribute msg
focusViaYellow100 =
    A.class "focus:via-yellow-100"


focusViaYellow200 : Html.Attribute msg
focusViaYellow200 =
    A.class "focus:via-yellow-200"


focusViaYellow300 : Html.Attribute msg
focusViaYellow300 =
    A.class "focus:via-yellow-300"


focusViaYellow400 : Html.Attribute msg
focusViaYellow400 =
    A.class "focus:via-yellow-400"


focusViaYellow500 : Html.Attribute msg
focusViaYellow500 =
    A.class "focus:via-yellow-500"


focusViaYellow600 : Html.Attribute msg
focusViaYellow600 =
    A.class "focus:via-yellow-600"


focusViaYellow700 : Html.Attribute msg
focusViaYellow700 =
    A.class "focus:via-yellow-700"


focusViaYellow800 : Html.Attribute msg
focusViaYellow800 =
    A.class "focus:via-yellow-800"


focusViaYellow900 : Html.Attribute msg
focusViaYellow900 =
    A.class "focus:via-yellow-900"


focusViaGreen100 : Html.Attribute msg
focusViaGreen100 =
    A.class "focus:via-green-100"


focusViaGreen200 : Html.Attribute msg
focusViaGreen200 =
    A.class "focus:via-green-200"


focusViaGreen300 : Html.Attribute msg
focusViaGreen300 =
    A.class "focus:via-green-300"


focusViaGreen400 : Html.Attribute msg
focusViaGreen400 =
    A.class "focus:via-green-400"


focusViaGreen500 : Html.Attribute msg
focusViaGreen500 =
    A.class "focus:via-green-500"


focusViaGreen600 : Html.Attribute msg
focusViaGreen600 =
    A.class "focus:via-green-600"


focusViaGreen700 : Html.Attribute msg
focusViaGreen700 =
    A.class "focus:via-green-700"


focusViaGreen800 : Html.Attribute msg
focusViaGreen800 =
    A.class "focus:via-green-800"


focusViaGreen900 : Html.Attribute msg
focusViaGreen900 =
    A.class "focus:via-green-900"


focusViaTeal100 : Html.Attribute msg
focusViaTeal100 =
    A.class "focus:via-teal-100"


focusViaTeal200 : Html.Attribute msg
focusViaTeal200 =
    A.class "focus:via-teal-200"


focusViaTeal300 : Html.Attribute msg
focusViaTeal300 =
    A.class "focus:via-teal-300"


focusViaTeal400 : Html.Attribute msg
focusViaTeal400 =
    A.class "focus:via-teal-400"


focusViaTeal500 : Html.Attribute msg
focusViaTeal500 =
    A.class "focus:via-teal-500"


focusViaTeal600 : Html.Attribute msg
focusViaTeal600 =
    A.class "focus:via-teal-600"


focusViaTeal700 : Html.Attribute msg
focusViaTeal700 =
    A.class "focus:via-teal-700"


focusViaTeal800 : Html.Attribute msg
focusViaTeal800 =
    A.class "focus:via-teal-800"


focusViaTeal900 : Html.Attribute msg
focusViaTeal900 =
    A.class "focus:via-teal-900"


focusViaBlue100 : Html.Attribute msg
focusViaBlue100 =
    A.class "focus:via-blue-100"


focusViaBlue200 : Html.Attribute msg
focusViaBlue200 =
    A.class "focus:via-blue-200"


focusViaBlue300 : Html.Attribute msg
focusViaBlue300 =
    A.class "focus:via-blue-300"


focusViaBlue400 : Html.Attribute msg
focusViaBlue400 =
    A.class "focus:via-blue-400"


focusViaBlue500 : Html.Attribute msg
focusViaBlue500 =
    A.class "focus:via-blue-500"


focusViaBlue600 : Html.Attribute msg
focusViaBlue600 =
    A.class "focus:via-blue-600"


focusViaBlue700 : Html.Attribute msg
focusViaBlue700 =
    A.class "focus:via-blue-700"


focusViaBlue800 : Html.Attribute msg
focusViaBlue800 =
    A.class "focus:via-blue-800"


focusViaBlue900 : Html.Attribute msg
focusViaBlue900 =
    A.class "focus:via-blue-900"


focusViaIndigo100 : Html.Attribute msg
focusViaIndigo100 =
    A.class "focus:via-indigo-100"


focusViaIndigo200 : Html.Attribute msg
focusViaIndigo200 =
    A.class "focus:via-indigo-200"


focusViaIndigo300 : Html.Attribute msg
focusViaIndigo300 =
    A.class "focus:via-indigo-300"


focusViaIndigo400 : Html.Attribute msg
focusViaIndigo400 =
    A.class "focus:via-indigo-400"


focusViaIndigo500 : Html.Attribute msg
focusViaIndigo500 =
    A.class "focus:via-indigo-500"


focusViaIndigo600 : Html.Attribute msg
focusViaIndigo600 =
    A.class "focus:via-indigo-600"


focusViaIndigo700 : Html.Attribute msg
focusViaIndigo700 =
    A.class "focus:via-indigo-700"


focusViaIndigo800 : Html.Attribute msg
focusViaIndigo800 =
    A.class "focus:via-indigo-800"


focusViaIndigo900 : Html.Attribute msg
focusViaIndigo900 =
    A.class "focus:via-indigo-900"


focusViaPurple100 : Html.Attribute msg
focusViaPurple100 =
    A.class "focus:via-purple-100"


focusViaPurple200 : Html.Attribute msg
focusViaPurple200 =
    A.class "focus:via-purple-200"


focusViaPurple300 : Html.Attribute msg
focusViaPurple300 =
    A.class "focus:via-purple-300"


focusViaPurple400 : Html.Attribute msg
focusViaPurple400 =
    A.class "focus:via-purple-400"


focusViaPurple500 : Html.Attribute msg
focusViaPurple500 =
    A.class "focus:via-purple-500"


focusViaPurple600 : Html.Attribute msg
focusViaPurple600 =
    A.class "focus:via-purple-600"


focusViaPurple700 : Html.Attribute msg
focusViaPurple700 =
    A.class "focus:via-purple-700"


focusViaPurple800 : Html.Attribute msg
focusViaPurple800 =
    A.class "focus:via-purple-800"


focusViaPurple900 : Html.Attribute msg
focusViaPurple900 =
    A.class "focus:via-purple-900"


focusViaPink100 : Html.Attribute msg
focusViaPink100 =
    A.class "focus:via-pink-100"


focusViaPink200 : Html.Attribute msg
focusViaPink200 =
    A.class "focus:via-pink-200"


focusViaPink300 : Html.Attribute msg
focusViaPink300 =
    A.class "focus:via-pink-300"


focusViaPink400 : Html.Attribute msg
focusViaPink400 =
    A.class "focus:via-pink-400"


focusViaPink500 : Html.Attribute msg
focusViaPink500 =
    A.class "focus:via-pink-500"


focusViaPink600 : Html.Attribute msg
focusViaPink600 =
    A.class "focus:via-pink-600"


focusViaPink700 : Html.Attribute msg
focusViaPink700 =
    A.class "focus:via-pink-700"


focusViaPink800 : Html.Attribute msg
focusViaPink800 =
    A.class "focus:via-pink-800"


focusViaPink900 : Html.Attribute msg
focusViaPink900 =
    A.class "focus:via-pink-900"


focusToTransparent : Html.Attribute msg
focusToTransparent =
    A.class "focus:to-transparent"


focusToCurrent : Html.Attribute msg
focusToCurrent =
    A.class "focus:to-current"


focusToBlack : Html.Attribute msg
focusToBlack =
    A.class "focus:to-black"


focusToWhite : Html.Attribute msg
focusToWhite =
    A.class "focus:to-white"


focusToGray100 : Html.Attribute msg
focusToGray100 =
    A.class "focus:to-gray-100"


focusToGray200 : Html.Attribute msg
focusToGray200 =
    A.class "focus:to-gray-200"


focusToGray300 : Html.Attribute msg
focusToGray300 =
    A.class "focus:to-gray-300"


focusToGray400 : Html.Attribute msg
focusToGray400 =
    A.class "focus:to-gray-400"


focusToGray500 : Html.Attribute msg
focusToGray500 =
    A.class "focus:to-gray-500"


focusToGray600 : Html.Attribute msg
focusToGray600 =
    A.class "focus:to-gray-600"


focusToGray700 : Html.Attribute msg
focusToGray700 =
    A.class "focus:to-gray-700"


focusToGray800 : Html.Attribute msg
focusToGray800 =
    A.class "focus:to-gray-800"


focusToGray900 : Html.Attribute msg
focusToGray900 =
    A.class "focus:to-gray-900"


focusToRed100 : Html.Attribute msg
focusToRed100 =
    A.class "focus:to-red-100"


focusToRed200 : Html.Attribute msg
focusToRed200 =
    A.class "focus:to-red-200"


focusToRed300 : Html.Attribute msg
focusToRed300 =
    A.class "focus:to-red-300"


focusToRed400 : Html.Attribute msg
focusToRed400 =
    A.class "focus:to-red-400"


focusToRed500 : Html.Attribute msg
focusToRed500 =
    A.class "focus:to-red-500"


focusToRed600 : Html.Attribute msg
focusToRed600 =
    A.class "focus:to-red-600"


focusToRed700 : Html.Attribute msg
focusToRed700 =
    A.class "focus:to-red-700"


focusToRed800 : Html.Attribute msg
focusToRed800 =
    A.class "focus:to-red-800"


focusToRed900 : Html.Attribute msg
focusToRed900 =
    A.class "focus:to-red-900"


focusToOrange100 : Html.Attribute msg
focusToOrange100 =
    A.class "focus:to-orange-100"


focusToOrange200 : Html.Attribute msg
focusToOrange200 =
    A.class "focus:to-orange-200"


focusToOrange300 : Html.Attribute msg
focusToOrange300 =
    A.class "focus:to-orange-300"


focusToOrange400 : Html.Attribute msg
focusToOrange400 =
    A.class "focus:to-orange-400"


focusToOrange500 : Html.Attribute msg
focusToOrange500 =
    A.class "focus:to-orange-500"


focusToOrange600 : Html.Attribute msg
focusToOrange600 =
    A.class "focus:to-orange-600"


focusToOrange700 : Html.Attribute msg
focusToOrange700 =
    A.class "focus:to-orange-700"


focusToOrange800 : Html.Attribute msg
focusToOrange800 =
    A.class "focus:to-orange-800"


focusToOrange900 : Html.Attribute msg
focusToOrange900 =
    A.class "focus:to-orange-900"


focusToYellow100 : Html.Attribute msg
focusToYellow100 =
    A.class "focus:to-yellow-100"


focusToYellow200 : Html.Attribute msg
focusToYellow200 =
    A.class "focus:to-yellow-200"


focusToYellow300 : Html.Attribute msg
focusToYellow300 =
    A.class "focus:to-yellow-300"


focusToYellow400 : Html.Attribute msg
focusToYellow400 =
    A.class "focus:to-yellow-400"


focusToYellow500 : Html.Attribute msg
focusToYellow500 =
    A.class "focus:to-yellow-500"


focusToYellow600 : Html.Attribute msg
focusToYellow600 =
    A.class "focus:to-yellow-600"


focusToYellow700 : Html.Attribute msg
focusToYellow700 =
    A.class "focus:to-yellow-700"


focusToYellow800 : Html.Attribute msg
focusToYellow800 =
    A.class "focus:to-yellow-800"


focusToYellow900 : Html.Attribute msg
focusToYellow900 =
    A.class "focus:to-yellow-900"


focusToGreen100 : Html.Attribute msg
focusToGreen100 =
    A.class "focus:to-green-100"


focusToGreen200 : Html.Attribute msg
focusToGreen200 =
    A.class "focus:to-green-200"


focusToGreen300 : Html.Attribute msg
focusToGreen300 =
    A.class "focus:to-green-300"


focusToGreen400 : Html.Attribute msg
focusToGreen400 =
    A.class "focus:to-green-400"


focusToGreen500 : Html.Attribute msg
focusToGreen500 =
    A.class "focus:to-green-500"


focusToGreen600 : Html.Attribute msg
focusToGreen600 =
    A.class "focus:to-green-600"


focusToGreen700 : Html.Attribute msg
focusToGreen700 =
    A.class "focus:to-green-700"


focusToGreen800 : Html.Attribute msg
focusToGreen800 =
    A.class "focus:to-green-800"


focusToGreen900 : Html.Attribute msg
focusToGreen900 =
    A.class "focus:to-green-900"


focusToTeal100 : Html.Attribute msg
focusToTeal100 =
    A.class "focus:to-teal-100"


focusToTeal200 : Html.Attribute msg
focusToTeal200 =
    A.class "focus:to-teal-200"


focusToTeal300 : Html.Attribute msg
focusToTeal300 =
    A.class "focus:to-teal-300"


focusToTeal400 : Html.Attribute msg
focusToTeal400 =
    A.class "focus:to-teal-400"


focusToTeal500 : Html.Attribute msg
focusToTeal500 =
    A.class "focus:to-teal-500"


focusToTeal600 : Html.Attribute msg
focusToTeal600 =
    A.class "focus:to-teal-600"


focusToTeal700 : Html.Attribute msg
focusToTeal700 =
    A.class "focus:to-teal-700"


focusToTeal800 : Html.Attribute msg
focusToTeal800 =
    A.class "focus:to-teal-800"


focusToTeal900 : Html.Attribute msg
focusToTeal900 =
    A.class "focus:to-teal-900"


focusToBlue100 : Html.Attribute msg
focusToBlue100 =
    A.class "focus:to-blue-100"


focusToBlue200 : Html.Attribute msg
focusToBlue200 =
    A.class "focus:to-blue-200"


focusToBlue300 : Html.Attribute msg
focusToBlue300 =
    A.class "focus:to-blue-300"


focusToBlue400 : Html.Attribute msg
focusToBlue400 =
    A.class "focus:to-blue-400"


focusToBlue500 : Html.Attribute msg
focusToBlue500 =
    A.class "focus:to-blue-500"


focusToBlue600 : Html.Attribute msg
focusToBlue600 =
    A.class "focus:to-blue-600"


focusToBlue700 : Html.Attribute msg
focusToBlue700 =
    A.class "focus:to-blue-700"


focusToBlue800 : Html.Attribute msg
focusToBlue800 =
    A.class "focus:to-blue-800"


focusToBlue900 : Html.Attribute msg
focusToBlue900 =
    A.class "focus:to-blue-900"


focusToIndigo100 : Html.Attribute msg
focusToIndigo100 =
    A.class "focus:to-indigo-100"


focusToIndigo200 : Html.Attribute msg
focusToIndigo200 =
    A.class "focus:to-indigo-200"


focusToIndigo300 : Html.Attribute msg
focusToIndigo300 =
    A.class "focus:to-indigo-300"


focusToIndigo400 : Html.Attribute msg
focusToIndigo400 =
    A.class "focus:to-indigo-400"


focusToIndigo500 : Html.Attribute msg
focusToIndigo500 =
    A.class "focus:to-indigo-500"


focusToIndigo600 : Html.Attribute msg
focusToIndigo600 =
    A.class "focus:to-indigo-600"


focusToIndigo700 : Html.Attribute msg
focusToIndigo700 =
    A.class "focus:to-indigo-700"


focusToIndigo800 : Html.Attribute msg
focusToIndigo800 =
    A.class "focus:to-indigo-800"


focusToIndigo900 : Html.Attribute msg
focusToIndigo900 =
    A.class "focus:to-indigo-900"


focusToPurple100 : Html.Attribute msg
focusToPurple100 =
    A.class "focus:to-purple-100"


focusToPurple200 : Html.Attribute msg
focusToPurple200 =
    A.class "focus:to-purple-200"


focusToPurple300 : Html.Attribute msg
focusToPurple300 =
    A.class "focus:to-purple-300"


focusToPurple400 : Html.Attribute msg
focusToPurple400 =
    A.class "focus:to-purple-400"


focusToPurple500 : Html.Attribute msg
focusToPurple500 =
    A.class "focus:to-purple-500"


focusToPurple600 : Html.Attribute msg
focusToPurple600 =
    A.class "focus:to-purple-600"


focusToPurple700 : Html.Attribute msg
focusToPurple700 =
    A.class "focus:to-purple-700"


focusToPurple800 : Html.Attribute msg
focusToPurple800 =
    A.class "focus:to-purple-800"


focusToPurple900 : Html.Attribute msg
focusToPurple900 =
    A.class "focus:to-purple-900"


focusToPink100 : Html.Attribute msg
focusToPink100 =
    A.class "focus:to-pink-100"


focusToPink200 : Html.Attribute msg
focusToPink200 =
    A.class "focus:to-pink-200"


focusToPink300 : Html.Attribute msg
focusToPink300 =
    A.class "focus:to-pink-300"


focusToPink400 : Html.Attribute msg
focusToPink400 =
    A.class "focus:to-pink-400"


focusToPink500 : Html.Attribute msg
focusToPink500 =
    A.class "focus:to-pink-500"


focusToPink600 : Html.Attribute msg
focusToPink600 =
    A.class "focus:to-pink-600"


focusToPink700 : Html.Attribute msg
focusToPink700 =
    A.class "focus:to-pink-700"


focusToPink800 : Html.Attribute msg
focusToPink800 =
    A.class "focus:to-pink-800"


focusToPink900 : Html.Attribute msg
focusToPink900 =
    A.class "focus:to-pink-900"


bgOpacity0 : Html.Attribute msg
bgOpacity0 =
    A.class "bg-opacity-0"


bgOpacity25 : Html.Attribute msg
bgOpacity25 =
    A.class "bg-opacity-25"


bgOpacity50 : Html.Attribute msg
bgOpacity50 =
    A.class "bg-opacity-50"


bgOpacity75 : Html.Attribute msg
bgOpacity75 =
    A.class "bg-opacity-75"


bgOpacity100 : Html.Attribute msg
bgOpacity100 =
    A.class "bg-opacity-100"


hoverBgOpacity0 : Html.Attribute msg
hoverBgOpacity0 =
    A.class "hover:bg-opacity-0"


hoverBgOpacity25 : Html.Attribute msg
hoverBgOpacity25 =
    A.class "hover:bg-opacity-25"


hoverBgOpacity50 : Html.Attribute msg
hoverBgOpacity50 =
    A.class "hover:bg-opacity-50"


hoverBgOpacity75 : Html.Attribute msg
hoverBgOpacity75 =
    A.class "hover:bg-opacity-75"


hoverBgOpacity100 : Html.Attribute msg
hoverBgOpacity100 =
    A.class "hover:bg-opacity-100"


focusBgOpacity0 : Html.Attribute msg
focusBgOpacity0 =
    A.class "focus:bg-opacity-0"


focusBgOpacity25 : Html.Attribute msg
focusBgOpacity25 =
    A.class "focus:bg-opacity-25"


focusBgOpacity50 : Html.Attribute msg
focusBgOpacity50 =
    A.class "focus:bg-opacity-50"


focusBgOpacity75 : Html.Attribute msg
focusBgOpacity75 =
    A.class "focus:bg-opacity-75"


focusBgOpacity100 : Html.Attribute msg
focusBgOpacity100 =
    A.class "focus:bg-opacity-100"


bgBottom : Html.Attribute msg
bgBottom =
    A.class "bg-bottom"


bgCenter : Html.Attribute msg
bgCenter =
    A.class "bg-center"


bgLeft : Html.Attribute msg
bgLeft =
    A.class "bg-left"


bgLeftBottom : Html.Attribute msg
bgLeftBottom =
    A.class "bg-left-bottom"


bgLeftTop : Html.Attribute msg
bgLeftTop =
    A.class "bg-left-top"


bgRight : Html.Attribute msg
bgRight =
    A.class "bg-right"


bgRightBottom : Html.Attribute msg
bgRightBottom =
    A.class "bg-right-bottom"


bgRightTop : Html.Attribute msg
bgRightTop =
    A.class "bg-right-top"


bgTop : Html.Attribute msg
bgTop =
    A.class "bg-top"


bgRepeat : Html.Attribute msg
bgRepeat =
    A.class "bg-repeat"


bgNoRepeat : Html.Attribute msg
bgNoRepeat =
    A.class "bg-no-repeat"


bgRepeatX : Html.Attribute msg
bgRepeatX =
    A.class "bg-repeat-x"


bgRepeatY : Html.Attribute msg
bgRepeatY =
    A.class "bg-repeat-y"


bgRepeatRound : Html.Attribute msg
bgRepeatRound =
    A.class "bg-repeat-round"


bgRepeatSpace : Html.Attribute msg
bgRepeatSpace =
    A.class "bg-repeat-space"


bgAuto : Html.Attribute msg
bgAuto =
    A.class "bg-auto"


bgCover : Html.Attribute msg
bgCover =
    A.class "bg-cover"


bgContain : Html.Attribute msg
bgContain =
    A.class "bg-contain"


borderCollapse : Html.Attribute msg
borderCollapse =
    A.class "border-collapse"


borderSeparate : Html.Attribute msg
borderSeparate =
    A.class "border-separate"


borderTransparent : Html.Attribute msg
borderTransparent =
    A.class "border-transparent"


borderCurrent : Html.Attribute msg
borderCurrent =
    A.class "border-current"


borderBlack : Html.Attribute msg
borderBlack =
    A.class "border-black"


borderWhite : Html.Attribute msg
borderWhite =
    A.class "border-white"


borderGray100 : Html.Attribute msg
borderGray100 =
    A.class "border-gray-100"


borderGray200 : Html.Attribute msg
borderGray200 =
    A.class "border-gray-200"


borderGray300 : Html.Attribute msg
borderGray300 =
    A.class "border-gray-300"


borderGray400 : Html.Attribute msg
borderGray400 =
    A.class "border-gray-400"


borderGray500 : Html.Attribute msg
borderGray500 =
    A.class "border-gray-500"


borderGray600 : Html.Attribute msg
borderGray600 =
    A.class "border-gray-600"


borderGray700 : Html.Attribute msg
borderGray700 =
    A.class "border-gray-700"


borderGray800 : Html.Attribute msg
borderGray800 =
    A.class "border-gray-800"


borderGray900 : Html.Attribute msg
borderGray900 =
    A.class "border-gray-900"


borderRed100 : Html.Attribute msg
borderRed100 =
    A.class "border-red-100"


borderRed200 : Html.Attribute msg
borderRed200 =
    A.class "border-red-200"


borderRed300 : Html.Attribute msg
borderRed300 =
    A.class "border-red-300"


borderRed400 : Html.Attribute msg
borderRed400 =
    A.class "border-red-400"


borderRed500 : Html.Attribute msg
borderRed500 =
    A.class "border-red-500"


borderRed600 : Html.Attribute msg
borderRed600 =
    A.class "border-red-600"


borderRed700 : Html.Attribute msg
borderRed700 =
    A.class "border-red-700"


borderRed800 : Html.Attribute msg
borderRed800 =
    A.class "border-red-800"


borderRed900 : Html.Attribute msg
borderRed900 =
    A.class "border-red-900"


borderOrange100 : Html.Attribute msg
borderOrange100 =
    A.class "border-orange-100"


borderOrange200 : Html.Attribute msg
borderOrange200 =
    A.class "border-orange-200"


borderOrange300 : Html.Attribute msg
borderOrange300 =
    A.class "border-orange-300"


borderOrange400 : Html.Attribute msg
borderOrange400 =
    A.class "border-orange-400"


borderOrange500 : Html.Attribute msg
borderOrange500 =
    A.class "border-orange-500"


borderOrange600 : Html.Attribute msg
borderOrange600 =
    A.class "border-orange-600"


borderOrange700 : Html.Attribute msg
borderOrange700 =
    A.class "border-orange-700"


borderOrange800 : Html.Attribute msg
borderOrange800 =
    A.class "border-orange-800"


borderOrange900 : Html.Attribute msg
borderOrange900 =
    A.class "border-orange-900"


borderYellow100 : Html.Attribute msg
borderYellow100 =
    A.class "border-yellow-100"


borderYellow200 : Html.Attribute msg
borderYellow200 =
    A.class "border-yellow-200"


borderYellow300 : Html.Attribute msg
borderYellow300 =
    A.class "border-yellow-300"


borderYellow400 : Html.Attribute msg
borderYellow400 =
    A.class "border-yellow-400"


borderYellow500 : Html.Attribute msg
borderYellow500 =
    A.class "border-yellow-500"


borderYellow600 : Html.Attribute msg
borderYellow600 =
    A.class "border-yellow-600"


borderYellow700 : Html.Attribute msg
borderYellow700 =
    A.class "border-yellow-700"


borderYellow800 : Html.Attribute msg
borderYellow800 =
    A.class "border-yellow-800"


borderYellow900 : Html.Attribute msg
borderYellow900 =
    A.class "border-yellow-900"


borderGreen100 : Html.Attribute msg
borderGreen100 =
    A.class "border-green-100"


borderGreen200 : Html.Attribute msg
borderGreen200 =
    A.class "border-green-200"


borderGreen300 : Html.Attribute msg
borderGreen300 =
    A.class "border-green-300"


borderGreen400 : Html.Attribute msg
borderGreen400 =
    A.class "border-green-400"


borderGreen500 : Html.Attribute msg
borderGreen500 =
    A.class "border-green-500"


borderGreen600 : Html.Attribute msg
borderGreen600 =
    A.class "border-green-600"


borderGreen700 : Html.Attribute msg
borderGreen700 =
    A.class "border-green-700"


borderGreen800 : Html.Attribute msg
borderGreen800 =
    A.class "border-green-800"


borderGreen900 : Html.Attribute msg
borderGreen900 =
    A.class "border-green-900"


borderTeal100 : Html.Attribute msg
borderTeal100 =
    A.class "border-teal-100"


borderTeal200 : Html.Attribute msg
borderTeal200 =
    A.class "border-teal-200"


borderTeal300 : Html.Attribute msg
borderTeal300 =
    A.class "border-teal-300"


borderTeal400 : Html.Attribute msg
borderTeal400 =
    A.class "border-teal-400"


borderTeal500 : Html.Attribute msg
borderTeal500 =
    A.class "border-teal-500"


borderTeal600 : Html.Attribute msg
borderTeal600 =
    A.class "border-teal-600"


borderTeal700 : Html.Attribute msg
borderTeal700 =
    A.class "border-teal-700"


borderTeal800 : Html.Attribute msg
borderTeal800 =
    A.class "border-teal-800"


borderTeal900 : Html.Attribute msg
borderTeal900 =
    A.class "border-teal-900"


borderBlue100 : Html.Attribute msg
borderBlue100 =
    A.class "border-blue-100"


borderBlue200 : Html.Attribute msg
borderBlue200 =
    A.class "border-blue-200"


borderBlue300 : Html.Attribute msg
borderBlue300 =
    A.class "border-blue-300"


borderBlue400 : Html.Attribute msg
borderBlue400 =
    A.class "border-blue-400"


borderBlue500 : Html.Attribute msg
borderBlue500 =
    A.class "border-blue-500"


borderBlue600 : Html.Attribute msg
borderBlue600 =
    A.class "border-blue-600"


borderBlue700 : Html.Attribute msg
borderBlue700 =
    A.class "border-blue-700"


borderBlue800 : Html.Attribute msg
borderBlue800 =
    A.class "border-blue-800"


borderBlue900 : Html.Attribute msg
borderBlue900 =
    A.class "border-blue-900"


borderIndigo100 : Html.Attribute msg
borderIndigo100 =
    A.class "border-indigo-100"


borderIndigo200 : Html.Attribute msg
borderIndigo200 =
    A.class "border-indigo-200"


borderIndigo300 : Html.Attribute msg
borderIndigo300 =
    A.class "border-indigo-300"


borderIndigo400 : Html.Attribute msg
borderIndigo400 =
    A.class "border-indigo-400"


borderIndigo500 : Html.Attribute msg
borderIndigo500 =
    A.class "border-indigo-500"


borderIndigo600 : Html.Attribute msg
borderIndigo600 =
    A.class "border-indigo-600"


borderIndigo700 : Html.Attribute msg
borderIndigo700 =
    A.class "border-indigo-700"


borderIndigo800 : Html.Attribute msg
borderIndigo800 =
    A.class "border-indigo-800"


borderIndigo900 : Html.Attribute msg
borderIndigo900 =
    A.class "border-indigo-900"


borderPurple100 : Html.Attribute msg
borderPurple100 =
    A.class "border-purple-100"


borderPurple200 : Html.Attribute msg
borderPurple200 =
    A.class "border-purple-200"


borderPurple300 : Html.Attribute msg
borderPurple300 =
    A.class "border-purple-300"


borderPurple400 : Html.Attribute msg
borderPurple400 =
    A.class "border-purple-400"


borderPurple500 : Html.Attribute msg
borderPurple500 =
    A.class "border-purple-500"


borderPurple600 : Html.Attribute msg
borderPurple600 =
    A.class "border-purple-600"


borderPurple700 : Html.Attribute msg
borderPurple700 =
    A.class "border-purple-700"


borderPurple800 : Html.Attribute msg
borderPurple800 =
    A.class "border-purple-800"


borderPurple900 : Html.Attribute msg
borderPurple900 =
    A.class "border-purple-900"


borderPink100 : Html.Attribute msg
borderPink100 =
    A.class "border-pink-100"


borderPink200 : Html.Attribute msg
borderPink200 =
    A.class "border-pink-200"


borderPink300 : Html.Attribute msg
borderPink300 =
    A.class "border-pink-300"


borderPink400 : Html.Attribute msg
borderPink400 =
    A.class "border-pink-400"


borderPink500 : Html.Attribute msg
borderPink500 =
    A.class "border-pink-500"


borderPink600 : Html.Attribute msg
borderPink600 =
    A.class "border-pink-600"


borderPink700 : Html.Attribute msg
borderPink700 =
    A.class "border-pink-700"


borderPink800 : Html.Attribute msg
borderPink800 =
    A.class "border-pink-800"


borderPink900 : Html.Attribute msg
borderPink900 =
    A.class "border-pink-900"


hoverBorderTransparent : Html.Attribute msg
hoverBorderTransparent =
    A.class "hover:border-transparent"


hoverBorderCurrent : Html.Attribute msg
hoverBorderCurrent =
    A.class "hover:border-current"


hoverBorderBlack : Html.Attribute msg
hoverBorderBlack =
    A.class "hover:border-black"


hoverBorderWhite : Html.Attribute msg
hoverBorderWhite =
    A.class "hover:border-white"


hoverBorderGray100 : Html.Attribute msg
hoverBorderGray100 =
    A.class "hover:border-gray-100"


hoverBorderGray200 : Html.Attribute msg
hoverBorderGray200 =
    A.class "hover:border-gray-200"


hoverBorderGray300 : Html.Attribute msg
hoverBorderGray300 =
    A.class "hover:border-gray-300"


hoverBorderGray400 : Html.Attribute msg
hoverBorderGray400 =
    A.class "hover:border-gray-400"


hoverBorderGray500 : Html.Attribute msg
hoverBorderGray500 =
    A.class "hover:border-gray-500"


hoverBorderGray600 : Html.Attribute msg
hoverBorderGray600 =
    A.class "hover:border-gray-600"


hoverBorderGray700 : Html.Attribute msg
hoverBorderGray700 =
    A.class "hover:border-gray-700"


hoverBorderGray800 : Html.Attribute msg
hoverBorderGray800 =
    A.class "hover:border-gray-800"


hoverBorderGray900 : Html.Attribute msg
hoverBorderGray900 =
    A.class "hover:border-gray-900"


hoverBorderRed100 : Html.Attribute msg
hoverBorderRed100 =
    A.class "hover:border-red-100"


hoverBorderRed200 : Html.Attribute msg
hoverBorderRed200 =
    A.class "hover:border-red-200"


hoverBorderRed300 : Html.Attribute msg
hoverBorderRed300 =
    A.class "hover:border-red-300"


hoverBorderRed400 : Html.Attribute msg
hoverBorderRed400 =
    A.class "hover:border-red-400"


hoverBorderRed500 : Html.Attribute msg
hoverBorderRed500 =
    A.class "hover:border-red-500"


hoverBorderRed600 : Html.Attribute msg
hoverBorderRed600 =
    A.class "hover:border-red-600"


hoverBorderRed700 : Html.Attribute msg
hoverBorderRed700 =
    A.class "hover:border-red-700"


hoverBorderRed800 : Html.Attribute msg
hoverBorderRed800 =
    A.class "hover:border-red-800"


hoverBorderRed900 : Html.Attribute msg
hoverBorderRed900 =
    A.class "hover:border-red-900"


hoverBorderOrange100 : Html.Attribute msg
hoverBorderOrange100 =
    A.class "hover:border-orange-100"


hoverBorderOrange200 : Html.Attribute msg
hoverBorderOrange200 =
    A.class "hover:border-orange-200"


hoverBorderOrange300 : Html.Attribute msg
hoverBorderOrange300 =
    A.class "hover:border-orange-300"


hoverBorderOrange400 : Html.Attribute msg
hoverBorderOrange400 =
    A.class "hover:border-orange-400"


hoverBorderOrange500 : Html.Attribute msg
hoverBorderOrange500 =
    A.class "hover:border-orange-500"


hoverBorderOrange600 : Html.Attribute msg
hoverBorderOrange600 =
    A.class "hover:border-orange-600"


hoverBorderOrange700 : Html.Attribute msg
hoverBorderOrange700 =
    A.class "hover:border-orange-700"


hoverBorderOrange800 : Html.Attribute msg
hoverBorderOrange800 =
    A.class "hover:border-orange-800"


hoverBorderOrange900 : Html.Attribute msg
hoverBorderOrange900 =
    A.class "hover:border-orange-900"


hoverBorderYellow100 : Html.Attribute msg
hoverBorderYellow100 =
    A.class "hover:border-yellow-100"


hoverBorderYellow200 : Html.Attribute msg
hoverBorderYellow200 =
    A.class "hover:border-yellow-200"


hoverBorderYellow300 : Html.Attribute msg
hoverBorderYellow300 =
    A.class "hover:border-yellow-300"


hoverBorderYellow400 : Html.Attribute msg
hoverBorderYellow400 =
    A.class "hover:border-yellow-400"


hoverBorderYellow500 : Html.Attribute msg
hoverBorderYellow500 =
    A.class "hover:border-yellow-500"


hoverBorderYellow600 : Html.Attribute msg
hoverBorderYellow600 =
    A.class "hover:border-yellow-600"


hoverBorderYellow700 : Html.Attribute msg
hoverBorderYellow700 =
    A.class "hover:border-yellow-700"


hoverBorderYellow800 : Html.Attribute msg
hoverBorderYellow800 =
    A.class "hover:border-yellow-800"


hoverBorderYellow900 : Html.Attribute msg
hoverBorderYellow900 =
    A.class "hover:border-yellow-900"


hoverBorderGreen100 : Html.Attribute msg
hoverBorderGreen100 =
    A.class "hover:border-green-100"


hoverBorderGreen200 : Html.Attribute msg
hoverBorderGreen200 =
    A.class "hover:border-green-200"


hoverBorderGreen300 : Html.Attribute msg
hoverBorderGreen300 =
    A.class "hover:border-green-300"


hoverBorderGreen400 : Html.Attribute msg
hoverBorderGreen400 =
    A.class "hover:border-green-400"


hoverBorderGreen500 : Html.Attribute msg
hoverBorderGreen500 =
    A.class "hover:border-green-500"


hoverBorderGreen600 : Html.Attribute msg
hoverBorderGreen600 =
    A.class "hover:border-green-600"


hoverBorderGreen700 : Html.Attribute msg
hoverBorderGreen700 =
    A.class "hover:border-green-700"


hoverBorderGreen800 : Html.Attribute msg
hoverBorderGreen800 =
    A.class "hover:border-green-800"


hoverBorderGreen900 : Html.Attribute msg
hoverBorderGreen900 =
    A.class "hover:border-green-900"


hoverBorderTeal100 : Html.Attribute msg
hoverBorderTeal100 =
    A.class "hover:border-teal-100"


hoverBorderTeal200 : Html.Attribute msg
hoverBorderTeal200 =
    A.class "hover:border-teal-200"


hoverBorderTeal300 : Html.Attribute msg
hoverBorderTeal300 =
    A.class "hover:border-teal-300"


hoverBorderTeal400 : Html.Attribute msg
hoverBorderTeal400 =
    A.class "hover:border-teal-400"


hoverBorderTeal500 : Html.Attribute msg
hoverBorderTeal500 =
    A.class "hover:border-teal-500"


hoverBorderTeal600 : Html.Attribute msg
hoverBorderTeal600 =
    A.class "hover:border-teal-600"


hoverBorderTeal700 : Html.Attribute msg
hoverBorderTeal700 =
    A.class "hover:border-teal-700"


hoverBorderTeal800 : Html.Attribute msg
hoverBorderTeal800 =
    A.class "hover:border-teal-800"


hoverBorderTeal900 : Html.Attribute msg
hoverBorderTeal900 =
    A.class "hover:border-teal-900"


hoverBorderBlue100 : Html.Attribute msg
hoverBorderBlue100 =
    A.class "hover:border-blue-100"


hoverBorderBlue200 : Html.Attribute msg
hoverBorderBlue200 =
    A.class "hover:border-blue-200"


hoverBorderBlue300 : Html.Attribute msg
hoverBorderBlue300 =
    A.class "hover:border-blue-300"


hoverBorderBlue400 : Html.Attribute msg
hoverBorderBlue400 =
    A.class "hover:border-blue-400"


hoverBorderBlue500 : Html.Attribute msg
hoverBorderBlue500 =
    A.class "hover:border-blue-500"


hoverBorderBlue600 : Html.Attribute msg
hoverBorderBlue600 =
    A.class "hover:border-blue-600"


hoverBorderBlue700 : Html.Attribute msg
hoverBorderBlue700 =
    A.class "hover:border-blue-700"


hoverBorderBlue800 : Html.Attribute msg
hoverBorderBlue800 =
    A.class "hover:border-blue-800"


hoverBorderBlue900 : Html.Attribute msg
hoverBorderBlue900 =
    A.class "hover:border-blue-900"


hoverBorderIndigo100 : Html.Attribute msg
hoverBorderIndigo100 =
    A.class "hover:border-indigo-100"


hoverBorderIndigo200 : Html.Attribute msg
hoverBorderIndigo200 =
    A.class "hover:border-indigo-200"


hoverBorderIndigo300 : Html.Attribute msg
hoverBorderIndigo300 =
    A.class "hover:border-indigo-300"


hoverBorderIndigo400 : Html.Attribute msg
hoverBorderIndigo400 =
    A.class "hover:border-indigo-400"


hoverBorderIndigo500 : Html.Attribute msg
hoverBorderIndigo500 =
    A.class "hover:border-indigo-500"


hoverBorderIndigo600 : Html.Attribute msg
hoverBorderIndigo600 =
    A.class "hover:border-indigo-600"


hoverBorderIndigo700 : Html.Attribute msg
hoverBorderIndigo700 =
    A.class "hover:border-indigo-700"


hoverBorderIndigo800 : Html.Attribute msg
hoverBorderIndigo800 =
    A.class "hover:border-indigo-800"


hoverBorderIndigo900 : Html.Attribute msg
hoverBorderIndigo900 =
    A.class "hover:border-indigo-900"


hoverBorderPurple100 : Html.Attribute msg
hoverBorderPurple100 =
    A.class "hover:border-purple-100"


hoverBorderPurple200 : Html.Attribute msg
hoverBorderPurple200 =
    A.class "hover:border-purple-200"


hoverBorderPurple300 : Html.Attribute msg
hoverBorderPurple300 =
    A.class "hover:border-purple-300"


hoverBorderPurple400 : Html.Attribute msg
hoverBorderPurple400 =
    A.class "hover:border-purple-400"


hoverBorderPurple500 : Html.Attribute msg
hoverBorderPurple500 =
    A.class "hover:border-purple-500"


hoverBorderPurple600 : Html.Attribute msg
hoverBorderPurple600 =
    A.class "hover:border-purple-600"


hoverBorderPurple700 : Html.Attribute msg
hoverBorderPurple700 =
    A.class "hover:border-purple-700"


hoverBorderPurple800 : Html.Attribute msg
hoverBorderPurple800 =
    A.class "hover:border-purple-800"


hoverBorderPurple900 : Html.Attribute msg
hoverBorderPurple900 =
    A.class "hover:border-purple-900"


hoverBorderPink100 : Html.Attribute msg
hoverBorderPink100 =
    A.class "hover:border-pink-100"


hoverBorderPink200 : Html.Attribute msg
hoverBorderPink200 =
    A.class "hover:border-pink-200"


hoverBorderPink300 : Html.Attribute msg
hoverBorderPink300 =
    A.class "hover:border-pink-300"


hoverBorderPink400 : Html.Attribute msg
hoverBorderPink400 =
    A.class "hover:border-pink-400"


hoverBorderPink500 : Html.Attribute msg
hoverBorderPink500 =
    A.class "hover:border-pink-500"


hoverBorderPink600 : Html.Attribute msg
hoverBorderPink600 =
    A.class "hover:border-pink-600"


hoverBorderPink700 : Html.Attribute msg
hoverBorderPink700 =
    A.class "hover:border-pink-700"


hoverBorderPink800 : Html.Attribute msg
hoverBorderPink800 =
    A.class "hover:border-pink-800"


hoverBorderPink900 : Html.Attribute msg
hoverBorderPink900 =
    A.class "hover:border-pink-900"


focusBorderTransparent : Html.Attribute msg
focusBorderTransparent =
    A.class "focus:border-transparent"


focusBorderCurrent : Html.Attribute msg
focusBorderCurrent =
    A.class "focus:border-current"


focusBorderBlack : Html.Attribute msg
focusBorderBlack =
    A.class "focus:border-black"


focusBorderWhite : Html.Attribute msg
focusBorderWhite =
    A.class "focus:border-white"


focusBorderGray100 : Html.Attribute msg
focusBorderGray100 =
    A.class "focus:border-gray-100"


focusBorderGray200 : Html.Attribute msg
focusBorderGray200 =
    A.class "focus:border-gray-200"


focusBorderGray300 : Html.Attribute msg
focusBorderGray300 =
    A.class "focus:border-gray-300"


focusBorderGray400 : Html.Attribute msg
focusBorderGray400 =
    A.class "focus:border-gray-400"


focusBorderGray500 : Html.Attribute msg
focusBorderGray500 =
    A.class "focus:border-gray-500"


focusBorderGray600 : Html.Attribute msg
focusBorderGray600 =
    A.class "focus:border-gray-600"


focusBorderGray700 : Html.Attribute msg
focusBorderGray700 =
    A.class "focus:border-gray-700"


focusBorderGray800 : Html.Attribute msg
focusBorderGray800 =
    A.class "focus:border-gray-800"


focusBorderGray900 : Html.Attribute msg
focusBorderGray900 =
    A.class "focus:border-gray-900"


focusBorderRed100 : Html.Attribute msg
focusBorderRed100 =
    A.class "focus:border-red-100"


focusBorderRed200 : Html.Attribute msg
focusBorderRed200 =
    A.class "focus:border-red-200"


focusBorderRed300 : Html.Attribute msg
focusBorderRed300 =
    A.class "focus:border-red-300"


focusBorderRed400 : Html.Attribute msg
focusBorderRed400 =
    A.class "focus:border-red-400"


focusBorderRed500 : Html.Attribute msg
focusBorderRed500 =
    A.class "focus:border-red-500"


focusBorderRed600 : Html.Attribute msg
focusBorderRed600 =
    A.class "focus:border-red-600"


focusBorderRed700 : Html.Attribute msg
focusBorderRed700 =
    A.class "focus:border-red-700"


focusBorderRed800 : Html.Attribute msg
focusBorderRed800 =
    A.class "focus:border-red-800"


focusBorderRed900 : Html.Attribute msg
focusBorderRed900 =
    A.class "focus:border-red-900"


focusBorderOrange100 : Html.Attribute msg
focusBorderOrange100 =
    A.class "focus:border-orange-100"


focusBorderOrange200 : Html.Attribute msg
focusBorderOrange200 =
    A.class "focus:border-orange-200"


focusBorderOrange300 : Html.Attribute msg
focusBorderOrange300 =
    A.class "focus:border-orange-300"


focusBorderOrange400 : Html.Attribute msg
focusBorderOrange400 =
    A.class "focus:border-orange-400"


focusBorderOrange500 : Html.Attribute msg
focusBorderOrange500 =
    A.class "focus:border-orange-500"


focusBorderOrange600 : Html.Attribute msg
focusBorderOrange600 =
    A.class "focus:border-orange-600"


focusBorderOrange700 : Html.Attribute msg
focusBorderOrange700 =
    A.class "focus:border-orange-700"


focusBorderOrange800 : Html.Attribute msg
focusBorderOrange800 =
    A.class "focus:border-orange-800"


focusBorderOrange900 : Html.Attribute msg
focusBorderOrange900 =
    A.class "focus:border-orange-900"


focusBorderYellow100 : Html.Attribute msg
focusBorderYellow100 =
    A.class "focus:border-yellow-100"


focusBorderYellow200 : Html.Attribute msg
focusBorderYellow200 =
    A.class "focus:border-yellow-200"


focusBorderYellow300 : Html.Attribute msg
focusBorderYellow300 =
    A.class "focus:border-yellow-300"


focusBorderYellow400 : Html.Attribute msg
focusBorderYellow400 =
    A.class "focus:border-yellow-400"


focusBorderYellow500 : Html.Attribute msg
focusBorderYellow500 =
    A.class "focus:border-yellow-500"


focusBorderYellow600 : Html.Attribute msg
focusBorderYellow600 =
    A.class "focus:border-yellow-600"


focusBorderYellow700 : Html.Attribute msg
focusBorderYellow700 =
    A.class "focus:border-yellow-700"


focusBorderYellow800 : Html.Attribute msg
focusBorderYellow800 =
    A.class "focus:border-yellow-800"


focusBorderYellow900 : Html.Attribute msg
focusBorderYellow900 =
    A.class "focus:border-yellow-900"


focusBorderGreen100 : Html.Attribute msg
focusBorderGreen100 =
    A.class "focus:border-green-100"


focusBorderGreen200 : Html.Attribute msg
focusBorderGreen200 =
    A.class "focus:border-green-200"


focusBorderGreen300 : Html.Attribute msg
focusBorderGreen300 =
    A.class "focus:border-green-300"


focusBorderGreen400 : Html.Attribute msg
focusBorderGreen400 =
    A.class "focus:border-green-400"


focusBorderGreen500 : Html.Attribute msg
focusBorderGreen500 =
    A.class "focus:border-green-500"


focusBorderGreen600 : Html.Attribute msg
focusBorderGreen600 =
    A.class "focus:border-green-600"


focusBorderGreen700 : Html.Attribute msg
focusBorderGreen700 =
    A.class "focus:border-green-700"


focusBorderGreen800 : Html.Attribute msg
focusBorderGreen800 =
    A.class "focus:border-green-800"


focusBorderGreen900 : Html.Attribute msg
focusBorderGreen900 =
    A.class "focus:border-green-900"


focusBorderTeal100 : Html.Attribute msg
focusBorderTeal100 =
    A.class "focus:border-teal-100"


focusBorderTeal200 : Html.Attribute msg
focusBorderTeal200 =
    A.class "focus:border-teal-200"


focusBorderTeal300 : Html.Attribute msg
focusBorderTeal300 =
    A.class "focus:border-teal-300"


focusBorderTeal400 : Html.Attribute msg
focusBorderTeal400 =
    A.class "focus:border-teal-400"


focusBorderTeal500 : Html.Attribute msg
focusBorderTeal500 =
    A.class "focus:border-teal-500"


focusBorderTeal600 : Html.Attribute msg
focusBorderTeal600 =
    A.class "focus:border-teal-600"


focusBorderTeal700 : Html.Attribute msg
focusBorderTeal700 =
    A.class "focus:border-teal-700"


focusBorderTeal800 : Html.Attribute msg
focusBorderTeal800 =
    A.class "focus:border-teal-800"


focusBorderTeal900 : Html.Attribute msg
focusBorderTeal900 =
    A.class "focus:border-teal-900"


focusBorderBlue100 : Html.Attribute msg
focusBorderBlue100 =
    A.class "focus:border-blue-100"


focusBorderBlue200 : Html.Attribute msg
focusBorderBlue200 =
    A.class "focus:border-blue-200"


focusBorderBlue300 : Html.Attribute msg
focusBorderBlue300 =
    A.class "focus:border-blue-300"


focusBorderBlue400 : Html.Attribute msg
focusBorderBlue400 =
    A.class "focus:border-blue-400"


focusBorderBlue500 : Html.Attribute msg
focusBorderBlue500 =
    A.class "focus:border-blue-500"


focusBorderBlue600 : Html.Attribute msg
focusBorderBlue600 =
    A.class "focus:border-blue-600"


focusBorderBlue700 : Html.Attribute msg
focusBorderBlue700 =
    A.class "focus:border-blue-700"


focusBorderBlue800 : Html.Attribute msg
focusBorderBlue800 =
    A.class "focus:border-blue-800"


focusBorderBlue900 : Html.Attribute msg
focusBorderBlue900 =
    A.class "focus:border-blue-900"


focusBorderIndigo100 : Html.Attribute msg
focusBorderIndigo100 =
    A.class "focus:border-indigo-100"


focusBorderIndigo200 : Html.Attribute msg
focusBorderIndigo200 =
    A.class "focus:border-indigo-200"


focusBorderIndigo300 : Html.Attribute msg
focusBorderIndigo300 =
    A.class "focus:border-indigo-300"


focusBorderIndigo400 : Html.Attribute msg
focusBorderIndigo400 =
    A.class "focus:border-indigo-400"


focusBorderIndigo500 : Html.Attribute msg
focusBorderIndigo500 =
    A.class "focus:border-indigo-500"


focusBorderIndigo600 : Html.Attribute msg
focusBorderIndigo600 =
    A.class "focus:border-indigo-600"


focusBorderIndigo700 : Html.Attribute msg
focusBorderIndigo700 =
    A.class "focus:border-indigo-700"


focusBorderIndigo800 : Html.Attribute msg
focusBorderIndigo800 =
    A.class "focus:border-indigo-800"


focusBorderIndigo900 : Html.Attribute msg
focusBorderIndigo900 =
    A.class "focus:border-indigo-900"


focusBorderPurple100 : Html.Attribute msg
focusBorderPurple100 =
    A.class "focus:border-purple-100"


focusBorderPurple200 : Html.Attribute msg
focusBorderPurple200 =
    A.class "focus:border-purple-200"


focusBorderPurple300 : Html.Attribute msg
focusBorderPurple300 =
    A.class "focus:border-purple-300"


focusBorderPurple400 : Html.Attribute msg
focusBorderPurple400 =
    A.class "focus:border-purple-400"


focusBorderPurple500 : Html.Attribute msg
focusBorderPurple500 =
    A.class "focus:border-purple-500"


focusBorderPurple600 : Html.Attribute msg
focusBorderPurple600 =
    A.class "focus:border-purple-600"


focusBorderPurple700 : Html.Attribute msg
focusBorderPurple700 =
    A.class "focus:border-purple-700"


focusBorderPurple800 : Html.Attribute msg
focusBorderPurple800 =
    A.class "focus:border-purple-800"


focusBorderPurple900 : Html.Attribute msg
focusBorderPurple900 =
    A.class "focus:border-purple-900"


focusBorderPink100 : Html.Attribute msg
focusBorderPink100 =
    A.class "focus:border-pink-100"


focusBorderPink200 : Html.Attribute msg
focusBorderPink200 =
    A.class "focus:border-pink-200"


focusBorderPink300 : Html.Attribute msg
focusBorderPink300 =
    A.class "focus:border-pink-300"


focusBorderPink400 : Html.Attribute msg
focusBorderPink400 =
    A.class "focus:border-pink-400"


focusBorderPink500 : Html.Attribute msg
focusBorderPink500 =
    A.class "focus:border-pink-500"


focusBorderPink600 : Html.Attribute msg
focusBorderPink600 =
    A.class "focus:border-pink-600"


focusBorderPink700 : Html.Attribute msg
focusBorderPink700 =
    A.class "focus:border-pink-700"


focusBorderPink800 : Html.Attribute msg
focusBorderPink800 =
    A.class "focus:border-pink-800"


focusBorderPink900 : Html.Attribute msg
focusBorderPink900 =
    A.class "focus:border-pink-900"


borderOpacity0 : Html.Attribute msg
borderOpacity0 =
    A.class "border-opacity-0"


borderOpacity25 : Html.Attribute msg
borderOpacity25 =
    A.class "border-opacity-25"


borderOpacity50 : Html.Attribute msg
borderOpacity50 =
    A.class "border-opacity-50"


borderOpacity75 : Html.Attribute msg
borderOpacity75 =
    A.class "border-opacity-75"


borderOpacity100 : Html.Attribute msg
borderOpacity100 =
    A.class "border-opacity-100"


hoverBorderOpacity0 : Html.Attribute msg
hoverBorderOpacity0 =
    A.class "hover:border-opacity-0"


hoverBorderOpacity25 : Html.Attribute msg
hoverBorderOpacity25 =
    A.class "hover:border-opacity-25"


hoverBorderOpacity50 : Html.Attribute msg
hoverBorderOpacity50 =
    A.class "hover:border-opacity-50"


hoverBorderOpacity75 : Html.Attribute msg
hoverBorderOpacity75 =
    A.class "hover:border-opacity-75"


hoverBorderOpacity100 : Html.Attribute msg
hoverBorderOpacity100 =
    A.class "hover:border-opacity-100"


focusBorderOpacity0 : Html.Attribute msg
focusBorderOpacity0 =
    A.class "focus:border-opacity-0"


focusBorderOpacity25 : Html.Attribute msg
focusBorderOpacity25 =
    A.class "focus:border-opacity-25"


focusBorderOpacity50 : Html.Attribute msg
focusBorderOpacity50 =
    A.class "focus:border-opacity-50"


focusBorderOpacity75 : Html.Attribute msg
focusBorderOpacity75 =
    A.class "focus:border-opacity-75"


focusBorderOpacity100 : Html.Attribute msg
focusBorderOpacity100 =
    A.class "focus:border-opacity-100"


roundedNone : Html.Attribute msg
roundedNone =
    A.class "rounded-none"


roundedSm : Html.Attribute msg
roundedSm =
    A.class "rounded-sm"


rounded : Html.Attribute msg
rounded =
    A.class "rounded"


roundedMd : Html.Attribute msg
roundedMd =
    A.class "rounded-md"


roundedLg : Html.Attribute msg
roundedLg =
    A.class "rounded-lg"


roundedFull : Html.Attribute msg
roundedFull =
    A.class "rounded-full"


roundedTNone : Html.Attribute msg
roundedTNone =
    A.class "rounded-t-none"


roundedRNone : Html.Attribute msg
roundedRNone =
    A.class "rounded-r-none"


roundedBNone : Html.Attribute msg
roundedBNone =
    A.class "rounded-b-none"


roundedLNone : Html.Attribute msg
roundedLNone =
    A.class "rounded-l-none"


roundedTSm : Html.Attribute msg
roundedTSm =
    A.class "rounded-t-sm"


roundedRSm : Html.Attribute msg
roundedRSm =
    A.class "rounded-r-sm"


roundedBSm : Html.Attribute msg
roundedBSm =
    A.class "rounded-b-sm"


roundedLSm : Html.Attribute msg
roundedLSm =
    A.class "rounded-l-sm"


roundedT : Html.Attribute msg
roundedT =
    A.class "rounded-t"


roundedR : Html.Attribute msg
roundedR =
    A.class "rounded-r"


roundedB : Html.Attribute msg
roundedB =
    A.class "rounded-b"


roundedL : Html.Attribute msg
roundedL =
    A.class "rounded-l"


roundedTMd : Html.Attribute msg
roundedTMd =
    A.class "rounded-t-md"


roundedRMd : Html.Attribute msg
roundedRMd =
    A.class "rounded-r-md"


roundedBMd : Html.Attribute msg
roundedBMd =
    A.class "rounded-b-md"


roundedLMd : Html.Attribute msg
roundedLMd =
    A.class "rounded-l-md"


roundedTLg : Html.Attribute msg
roundedTLg =
    A.class "rounded-t-lg"


roundedRLg : Html.Attribute msg
roundedRLg =
    A.class "rounded-r-lg"


roundedBLg : Html.Attribute msg
roundedBLg =
    A.class "rounded-b-lg"


roundedLLg : Html.Attribute msg
roundedLLg =
    A.class "rounded-l-lg"


roundedTFull : Html.Attribute msg
roundedTFull =
    A.class "rounded-t-full"


roundedRFull : Html.Attribute msg
roundedRFull =
    A.class "rounded-r-full"


roundedBFull : Html.Attribute msg
roundedBFull =
    A.class "rounded-b-full"


roundedLFull : Html.Attribute msg
roundedLFull =
    A.class "rounded-l-full"


roundedTlNone : Html.Attribute msg
roundedTlNone =
    A.class "rounded-tl-none"


roundedTrNone : Html.Attribute msg
roundedTrNone =
    A.class "rounded-tr-none"


roundedBrNone : Html.Attribute msg
roundedBrNone =
    A.class "rounded-br-none"


roundedBlNone : Html.Attribute msg
roundedBlNone =
    A.class "rounded-bl-none"


roundedTlSm : Html.Attribute msg
roundedTlSm =
    A.class "rounded-tl-sm"


roundedTrSm : Html.Attribute msg
roundedTrSm =
    A.class "rounded-tr-sm"


roundedBrSm : Html.Attribute msg
roundedBrSm =
    A.class "rounded-br-sm"


roundedBlSm : Html.Attribute msg
roundedBlSm =
    A.class "rounded-bl-sm"


roundedTl : Html.Attribute msg
roundedTl =
    A.class "rounded-tl"


roundedTr : Html.Attribute msg
roundedTr =
    A.class "rounded-tr"


roundedBr : Html.Attribute msg
roundedBr =
    A.class "rounded-br"


roundedBl : Html.Attribute msg
roundedBl =
    A.class "rounded-bl"


roundedTlMd : Html.Attribute msg
roundedTlMd =
    A.class "rounded-tl-md"


roundedTrMd : Html.Attribute msg
roundedTrMd =
    A.class "rounded-tr-md"


roundedBrMd : Html.Attribute msg
roundedBrMd =
    A.class "rounded-br-md"


roundedBlMd : Html.Attribute msg
roundedBlMd =
    A.class "rounded-bl-md"


roundedTlLg : Html.Attribute msg
roundedTlLg =
    A.class "rounded-tl-lg"


roundedTrLg : Html.Attribute msg
roundedTrLg =
    A.class "rounded-tr-lg"


roundedBrLg : Html.Attribute msg
roundedBrLg =
    A.class "rounded-br-lg"


roundedBlLg : Html.Attribute msg
roundedBlLg =
    A.class "rounded-bl-lg"


roundedTlFull : Html.Attribute msg
roundedTlFull =
    A.class "rounded-tl-full"


roundedTrFull : Html.Attribute msg
roundedTrFull =
    A.class "rounded-tr-full"


roundedBrFull : Html.Attribute msg
roundedBrFull =
    A.class "rounded-br-full"


roundedBlFull : Html.Attribute msg
roundedBlFull =
    A.class "rounded-bl-full"


borderSolid : Html.Attribute msg
borderSolid =
    A.class "border-solid"


borderDashed : Html.Attribute msg
borderDashed =
    A.class "border-dashed"


borderDotted : Html.Attribute msg
borderDotted =
    A.class "border-dotted"


borderDouble : Html.Attribute msg
borderDouble =
    A.class "border-double"


borderNone : Html.Attribute msg
borderNone =
    A.class "border-none"


border0 : Html.Attribute msg
border0 =
    A.class "border-0"


border2 : Html.Attribute msg
border2 =
    A.class "border-2"


border4 : Html.Attribute msg
border4 =
    A.class "border-4"


border8 : Html.Attribute msg
border8 =
    A.class "border-8"


border : Html.Attribute msg
border =
    A.class "border"


borderT0 : Html.Attribute msg
borderT0 =
    A.class "border-t-0"


borderR0 : Html.Attribute msg
borderR0 =
    A.class "border-r-0"


borderB0 : Html.Attribute msg
borderB0 =
    A.class "border-b-0"


borderL0 : Html.Attribute msg
borderL0 =
    A.class "border-l-0"


borderT2 : Html.Attribute msg
borderT2 =
    A.class "border-t-2"


borderR2 : Html.Attribute msg
borderR2 =
    A.class "border-r-2"


borderB2 : Html.Attribute msg
borderB2 =
    A.class "border-b-2"


borderL2 : Html.Attribute msg
borderL2 =
    A.class "border-l-2"


borderT4 : Html.Attribute msg
borderT4 =
    A.class "border-t-4"


borderR4 : Html.Attribute msg
borderR4 =
    A.class "border-r-4"


borderB4 : Html.Attribute msg
borderB4 =
    A.class "border-b-4"


borderL4 : Html.Attribute msg
borderL4 =
    A.class "border-l-4"


borderT8 : Html.Attribute msg
borderT8 =
    A.class "border-t-8"


borderR8 : Html.Attribute msg
borderR8 =
    A.class "border-r-8"


borderB8 : Html.Attribute msg
borderB8 =
    A.class "border-b-8"


borderL8 : Html.Attribute msg
borderL8 =
    A.class "border-l-8"


borderT : Html.Attribute msg
borderT =
    A.class "border-t"


borderR : Html.Attribute msg
borderR =
    A.class "border-r"


borderB : Html.Attribute msg
borderB =
    A.class "border-b"


borderL : Html.Attribute msg
borderL =
    A.class "border-l"


boxBorder : Html.Attribute msg
boxBorder =
    A.class "box-border"


boxContent : Html.Attribute msg
boxContent =
    A.class "box-content"


cursorAuto : Html.Attribute msg
cursorAuto =
    A.class "cursor-auto"


cursorDefault : Html.Attribute msg
cursorDefault =
    A.class "cursor-default"


cursorPointer : Html.Attribute msg
cursorPointer =
    A.class "cursor-pointer"


cursorWait : Html.Attribute msg
cursorWait =
    A.class "cursor-wait"


cursorText : Html.Attribute msg
cursorText =
    A.class "cursor-text"


cursorMove : Html.Attribute msg
cursorMove =
    A.class "cursor-move"


cursorNotAllowed : Html.Attribute msg
cursorNotAllowed =
    A.class "cursor-not-allowed"


block : Html.Attribute msg
block =
    A.class "block"


inlineBlock : Html.Attribute msg
inlineBlock =
    A.class "inline-block"


inline : Html.Attribute msg
inline =
    A.class "inline"


flex : Html.Attribute msg
flex =
    A.class "flex"


inlineFlex : Html.Attribute msg
inlineFlex =
    A.class "inline-flex"


table : Html.Attribute msg
table =
    A.class "table"


tableCaption : Html.Attribute msg
tableCaption =
    A.class "table-caption"


tableCell : Html.Attribute msg
tableCell =
    A.class "table-cell"


tableColumn : Html.Attribute msg
tableColumn =
    A.class "table-column"


tableColumnGroup : Html.Attribute msg
tableColumnGroup =
    A.class "table-column-group"


tableFooterGroup : Html.Attribute msg
tableFooterGroup =
    A.class "table-footer-group"


tableHeaderGroup : Html.Attribute msg
tableHeaderGroup =
    A.class "table-header-group"


tableRowGroup : Html.Attribute msg
tableRowGroup =
    A.class "table-row-group"


tableRow : Html.Attribute msg
tableRow =
    A.class "table-row"


flowRoot : Html.Attribute msg
flowRoot =
    A.class "flow-root"


grid : Html.Attribute msg
grid =
    A.class "grid"


inlineGrid : Html.Attribute msg
inlineGrid =
    A.class "inline-grid"


contents : Html.Attribute msg
contents =
    A.class "contents"


hidden : Html.Attribute msg
hidden =
    A.class "hidden"


flexRow : Html.Attribute msg
flexRow =
    A.class "flex-row"


flexRowReverse : Html.Attribute msg
flexRowReverse =
    A.class "flex-row-reverse"


flexCol : Html.Attribute msg
flexCol =
    A.class "flex-col"


flexColReverse : Html.Attribute msg
flexColReverse =
    A.class "flex-col-reverse"


flexWrap : Html.Attribute msg
flexWrap =
    A.class "flex-wrap"


flexWrapReverse : Html.Attribute msg
flexWrapReverse =
    A.class "flex-wrap-reverse"


flexNoWrap : Html.Attribute msg
flexNoWrap =
    A.class "flex-no-wrap"


placeItemsAuto : Html.Attribute msg
placeItemsAuto =
    A.class "place-items-auto"


placeItemsStart : Html.Attribute msg
placeItemsStart =
    A.class "place-items-start"


placeItemsEnd : Html.Attribute msg
placeItemsEnd =
    A.class "place-items-end"


placeItemsCenter : Html.Attribute msg
placeItemsCenter =
    A.class "place-items-center"


placeItemsStretch : Html.Attribute msg
placeItemsStretch =
    A.class "place-items-stretch"


placeContentCenter : Html.Attribute msg
placeContentCenter =
    A.class "place-content-center"


placeContentStart : Html.Attribute msg
placeContentStart =
    A.class "place-content-start"


placeContentEnd : Html.Attribute msg
placeContentEnd =
    A.class "place-content-end"


placeContentBetween : Html.Attribute msg
placeContentBetween =
    A.class "place-content-between"


placeContentAround : Html.Attribute msg
placeContentAround =
    A.class "place-content-around"


placeContentEvenly : Html.Attribute msg
placeContentEvenly =
    A.class "place-content-evenly"


placeContentStretch : Html.Attribute msg
placeContentStretch =
    A.class "place-content-stretch"


placeSelfAuto : Html.Attribute msg
placeSelfAuto =
    A.class "place-self-auto"


placeSelfStart : Html.Attribute msg
placeSelfStart =
    A.class "place-self-start"


placeSelfEnd : Html.Attribute msg
placeSelfEnd =
    A.class "place-self-end"


placeSelfCenter : Html.Attribute msg
placeSelfCenter =
    A.class "place-self-center"


placeSelfStretch : Html.Attribute msg
placeSelfStretch =
    A.class "place-self-stretch"


itemsStart : Html.Attribute msg
itemsStart =
    A.class "items-start"


itemsEnd : Html.Attribute msg
itemsEnd =
    A.class "items-end"


itemsCenter : Html.Attribute msg
itemsCenter =
    A.class "items-center"


itemsBaseline : Html.Attribute msg
itemsBaseline =
    A.class "items-baseline"


itemsStretch : Html.Attribute msg
itemsStretch =
    A.class "items-stretch"


contentCenter : Html.Attribute msg
contentCenter =
    A.class "content-center"


contentStart : Html.Attribute msg
contentStart =
    A.class "content-start"


contentEnd : Html.Attribute msg
contentEnd =
    A.class "content-end"


contentBetween : Html.Attribute msg
contentBetween =
    A.class "content-between"


contentAround : Html.Attribute msg
contentAround =
    A.class "content-around"


contentEvenly : Html.Attribute msg
contentEvenly =
    A.class "content-evenly"


selfAuto : Html.Attribute msg
selfAuto =
    A.class "self-auto"


selfStart : Html.Attribute msg
selfStart =
    A.class "self-start"


selfEnd : Html.Attribute msg
selfEnd =
    A.class "self-end"


selfCenter : Html.Attribute msg
selfCenter =
    A.class "self-center"


selfStretch : Html.Attribute msg
selfStretch =
    A.class "self-stretch"


justifyItemsAuto : Html.Attribute msg
justifyItemsAuto =
    A.class "justify-items-auto"


justifyItemsStart : Html.Attribute msg
justifyItemsStart =
    A.class "justify-items-start"


justifyItemsEnd : Html.Attribute msg
justifyItemsEnd =
    A.class "justify-items-end"


justifyItemsCenter : Html.Attribute msg
justifyItemsCenter =
    A.class "justify-items-center"


justifyItemsStretch : Html.Attribute msg
justifyItemsStretch =
    A.class "justify-items-stretch"


justifyStart : Html.Attribute msg
justifyStart =
    A.class "justify-start"


justifyEnd : Html.Attribute msg
justifyEnd =
    A.class "justify-end"


justifyCenter : Html.Attribute msg
justifyCenter =
    A.class "justify-center"


justifyBetween : Html.Attribute msg
justifyBetween =
    A.class "justify-between"


justifyAround : Html.Attribute msg
justifyAround =
    A.class "justify-around"


justifyEvenly : Html.Attribute msg
justifyEvenly =
    A.class "justify-evenly"


justifySelfAuto : Html.Attribute msg
justifySelfAuto =
    A.class "justify-self-auto"


justifySelfStart : Html.Attribute msg
justifySelfStart =
    A.class "justify-self-start"


justifySelfEnd : Html.Attribute msg
justifySelfEnd =
    A.class "justify-self-end"


justifySelfCenter : Html.Attribute msg
justifySelfCenter =
    A.class "justify-self-center"


justifySelfStretch : Html.Attribute msg
justifySelfStretch =
    A.class "justify-self-stretch"


flex1 : Html.Attribute msg
flex1 =
    A.class "flex-1"


flexAuto : Html.Attribute msg
flexAuto =
    A.class "flex-auto"


flexInitial : Html.Attribute msg
flexInitial =
    A.class "flex-initial"


flexNone : Html.Attribute msg
flexNone =
    A.class "flex-none"


flexGrow0 : Html.Attribute msg
flexGrow0 =
    A.class "flex-grow-0"


flexGrow : Html.Attribute msg
flexGrow =
    A.class "flex-grow"


flexShrink0 : Html.Attribute msg
flexShrink0 =
    A.class "flex-shrink-0"


flexShrink : Html.Attribute msg
flexShrink =
    A.class "flex-shrink"


order1 : Html.Attribute msg
order1 =
    A.class "order-1"


order2 : Html.Attribute msg
order2 =
    A.class "order-2"


order3 : Html.Attribute msg
order3 =
    A.class "order-3"


order4 : Html.Attribute msg
order4 =
    A.class "order-4"


order5 : Html.Attribute msg
order5 =
    A.class "order-5"


order6 : Html.Attribute msg
order6 =
    A.class "order-6"


order7 : Html.Attribute msg
order7 =
    A.class "order-7"


order8 : Html.Attribute msg
order8 =
    A.class "order-8"


order9 : Html.Attribute msg
order9 =
    A.class "order-9"


order10 : Html.Attribute msg
order10 =
    A.class "order-10"


order11 : Html.Attribute msg
order11 =
    A.class "order-11"


order12 : Html.Attribute msg
order12 =
    A.class "order-12"


orderFirst : Html.Attribute msg
orderFirst =
    A.class "order-first"


orderLast : Html.Attribute msg
orderLast =
    A.class "order-last"


orderNone : Html.Attribute msg
orderNone =
    A.class "order-none"


floatRight : Html.Attribute msg
floatRight =
    A.class "float-right"


floatLeft : Html.Attribute msg
floatLeft =
    A.class "float-left"


floatNone : Html.Attribute msg
floatNone =
    A.class "float-none"


clearfixAfter : Html.Attribute msg
clearfixAfter =
    A.class "clearfix:after"


clearLeft : Html.Attribute msg
clearLeft =
    A.class "clear-left"


clearRight : Html.Attribute msg
clearRight =
    A.class "clear-right"


clearBoth : Html.Attribute msg
clearBoth =
    A.class "clear-both"


clearNone : Html.Attribute msg
clearNone =
    A.class "clear-none"


fontSans : Html.Attribute msg
fontSans =
    A.class "font-sans"


fontSerif : Html.Attribute msg
fontSerif =
    A.class "font-serif"


fontMono : Html.Attribute msg
fontMono =
    A.class "font-mono"


fontHairline : Html.Attribute msg
fontHairline =
    A.class "font-hairline"


fontThin : Html.Attribute msg
fontThin =
    A.class "font-thin"


fontLight : Html.Attribute msg
fontLight =
    A.class "font-light"


fontNormal : Html.Attribute msg
fontNormal =
    A.class "font-normal"


fontMedium : Html.Attribute msg
fontMedium =
    A.class "font-medium"


fontSemibold : Html.Attribute msg
fontSemibold =
    A.class "font-semibold"


fontBold : Html.Attribute msg
fontBold =
    A.class "font-bold"


fontExtrabold : Html.Attribute msg
fontExtrabold =
    A.class "font-extrabold"


fontBlack : Html.Attribute msg
fontBlack =
    A.class "font-black"


hoverFontHairline : Html.Attribute msg
hoverFontHairline =
    A.class "hover:font-hairline"


hoverFontThin : Html.Attribute msg
hoverFontThin =
    A.class "hover:font-thin"


hoverFontLight : Html.Attribute msg
hoverFontLight =
    A.class "hover:font-light"


hoverFontNormal : Html.Attribute msg
hoverFontNormal =
    A.class "hover:font-normal"


hoverFontMedium : Html.Attribute msg
hoverFontMedium =
    A.class "hover:font-medium"


hoverFontSemibold : Html.Attribute msg
hoverFontSemibold =
    A.class "hover:font-semibold"


hoverFontBold : Html.Attribute msg
hoverFontBold =
    A.class "hover:font-bold"


hoverFontExtrabold : Html.Attribute msg
hoverFontExtrabold =
    A.class "hover:font-extrabold"


hoverFontBlack : Html.Attribute msg
hoverFontBlack =
    A.class "hover:font-black"


focusFontHairline : Html.Attribute msg
focusFontHairline =
    A.class "focus:font-hairline"


focusFontThin : Html.Attribute msg
focusFontThin =
    A.class "focus:font-thin"


focusFontLight : Html.Attribute msg
focusFontLight =
    A.class "focus:font-light"


focusFontNormal : Html.Attribute msg
focusFontNormal =
    A.class "focus:font-normal"


focusFontMedium : Html.Attribute msg
focusFontMedium =
    A.class "focus:font-medium"


focusFontSemibold : Html.Attribute msg
focusFontSemibold =
    A.class "focus:font-semibold"


focusFontBold : Html.Attribute msg
focusFontBold =
    A.class "focus:font-bold"


focusFontExtrabold : Html.Attribute msg
focusFontExtrabold =
    A.class "focus:font-extrabold"


focusFontBlack : Html.Attribute msg
focusFontBlack =
    A.class "focus:font-black"


h0 : Html.Attribute msg
h0 =
    A.class "h-0"


h1 : Html.Attribute msg
h1 =
    A.class "h-1"


h2 : Html.Attribute msg
h2 =
    A.class "h-2"


h3 : Html.Attribute msg
h3 =
    A.class "h-3"


h4 : Html.Attribute msg
h4 =
    A.class "h-4"


h5 : Html.Attribute msg
h5 =
    A.class "h-5"


h6 : Html.Attribute msg
h6 =
    A.class "h-6"


h8 : Html.Attribute msg
h8 =
    A.class "h-8"


h10 : Html.Attribute msg
h10 =
    A.class "h-10"


h12 : Html.Attribute msg
h12 =
    A.class "h-12"


h16 : Html.Attribute msg
h16 =
    A.class "h-16"


h20 : Html.Attribute msg
h20 =
    A.class "h-20"


h24 : Html.Attribute msg
h24 =
    A.class "h-24"


h32 : Html.Attribute msg
h32 =
    A.class "h-32"


h40 : Html.Attribute msg
h40 =
    A.class "h-40"


h48 : Html.Attribute msg
h48 =
    A.class "h-48"


h56 : Html.Attribute msg
h56 =
    A.class "h-56"


h64 : Html.Attribute msg
h64 =
    A.class "h-64"


hAuto : Html.Attribute msg
hAuto =
    A.class "h-auto"


hPx : Html.Attribute msg
hPx =
    A.class "h-px"


hFull : Html.Attribute msg
hFull =
    A.class "h-full"


hScreen : Html.Attribute msg
hScreen =
    A.class "h-screen"


textXs : Html.Attribute msg
textXs =
    A.class "text-xs"


textSm : Html.Attribute msg
textSm =
    A.class "text-sm"


textBase : Html.Attribute msg
textBase =
    A.class "text-base"


textLg : Html.Attribute msg
textLg =
    A.class "text-lg"


textXl : Html.Attribute msg
textXl =
    A.class "text-xl"


text2xl : Html.Attribute msg
text2xl =
    A.class "text-2xl"


text3xl : Html.Attribute msg
text3xl =
    A.class "text-3xl"


text4xl : Html.Attribute msg
text4xl =
    A.class "text-4xl"


text5xl : Html.Attribute msg
text5xl =
    A.class "text-5xl"


text6xl : Html.Attribute msg
text6xl =
    A.class "text-6xl"


leading3 : Html.Attribute msg
leading3 =
    A.class "leading-3"


leading4 : Html.Attribute msg
leading4 =
    A.class "leading-4"


leading5 : Html.Attribute msg
leading5 =
    A.class "leading-5"


leading6 : Html.Attribute msg
leading6 =
    A.class "leading-6"


leading7 : Html.Attribute msg
leading7 =
    A.class "leading-7"


leading8 : Html.Attribute msg
leading8 =
    A.class "leading-8"


leading9 : Html.Attribute msg
leading9 =
    A.class "leading-9"


leading10 : Html.Attribute msg
leading10 =
    A.class "leading-10"


leadingNone : Html.Attribute msg
leadingNone =
    A.class "leading-none"


leadingTight : Html.Attribute msg
leadingTight =
    A.class "leading-tight"


leadingSnug : Html.Attribute msg
leadingSnug =
    A.class "leading-snug"


leadingNormal : Html.Attribute msg
leadingNormal =
    A.class "leading-normal"


leadingRelaxed : Html.Attribute msg
leadingRelaxed =
    A.class "leading-relaxed"


leadingLoose : Html.Attribute msg
leadingLoose =
    A.class "leading-loose"


listInside : Html.Attribute msg
listInside =
    A.class "list-inside"


listOutside : Html.Attribute msg
listOutside =
    A.class "list-outside"


listNone : Html.Attribute msg
listNone =
    A.class "list-none"


listDisc : Html.Attribute msg
listDisc =
    A.class "list-disc"


listDecimal : Html.Attribute msg
listDecimal =
    A.class "list-decimal"


m0 : Html.Attribute msg
m0 =
    A.class "m-0"


m1 : Html.Attribute msg
m1 =
    A.class "m-1"


m2 : Html.Attribute msg
m2 =
    A.class "m-2"


m3 : Html.Attribute msg
m3 =
    A.class "m-3"


m4 : Html.Attribute msg
m4 =
    A.class "m-4"


m5 : Html.Attribute msg
m5 =
    A.class "m-5"


m6 : Html.Attribute msg
m6 =
    A.class "m-6"


m8 : Html.Attribute msg
m8 =
    A.class "m-8"


m10 : Html.Attribute msg
m10 =
    A.class "m-10"


m12 : Html.Attribute msg
m12 =
    A.class "m-12"


m16 : Html.Attribute msg
m16 =
    A.class "m-16"


m20 : Html.Attribute msg
m20 =
    A.class "m-20"


m24 : Html.Attribute msg
m24 =
    A.class "m-24"


m32 : Html.Attribute msg
m32 =
    A.class "m-32"


m40 : Html.Attribute msg
m40 =
    A.class "m-40"


m48 : Html.Attribute msg
m48 =
    A.class "m-48"


m56 : Html.Attribute msg
m56 =
    A.class "m-56"


m64 : Html.Attribute msg
m64 =
    A.class "m-64"


mAuto : Html.Attribute msg
mAuto =
    A.class "m-auto"


mPx : Html.Attribute msg
mPx =
    A.class "m-px"


negM1 : Html.Attribute msg
negM1 =
    A.class "-m-1"


negM2 : Html.Attribute msg
negM2 =
    A.class "-m-2"


negM3 : Html.Attribute msg
negM3 =
    A.class "-m-3"


negM4 : Html.Attribute msg
negM4 =
    A.class "-m-4"


negM5 : Html.Attribute msg
negM5 =
    A.class "-m-5"


negM6 : Html.Attribute msg
negM6 =
    A.class "-m-6"


negM8 : Html.Attribute msg
negM8 =
    A.class "-m-8"


negM10 : Html.Attribute msg
negM10 =
    A.class "-m-10"


negM12 : Html.Attribute msg
negM12 =
    A.class "-m-12"


negM16 : Html.Attribute msg
negM16 =
    A.class "-m-16"


negM20 : Html.Attribute msg
negM20 =
    A.class "-m-20"


negM24 : Html.Attribute msg
negM24 =
    A.class "-m-24"


negM32 : Html.Attribute msg
negM32 =
    A.class "-m-32"


negM40 : Html.Attribute msg
negM40 =
    A.class "-m-40"


negM48 : Html.Attribute msg
negM48 =
    A.class "-m-48"


negM56 : Html.Attribute msg
negM56 =
    A.class "-m-56"


negM64 : Html.Attribute msg
negM64 =
    A.class "-m-64"


negMPx : Html.Attribute msg
negMPx =
    A.class "-m-px"


my0 : Html.Attribute msg
my0 =
    A.class "my-0"


mx0 : Html.Attribute msg
mx0 =
    A.class "mx-0"


my1 : Html.Attribute msg
my1 =
    A.class "my-1"


mx1 : Html.Attribute msg
mx1 =
    A.class "mx-1"


my2 : Html.Attribute msg
my2 =
    A.class "my-2"


mx2 : Html.Attribute msg
mx2 =
    A.class "mx-2"


my3 : Html.Attribute msg
my3 =
    A.class "my-3"


mx3 : Html.Attribute msg
mx3 =
    A.class "mx-3"


my4 : Html.Attribute msg
my4 =
    A.class "my-4"


mx4 : Html.Attribute msg
mx4 =
    A.class "mx-4"


my5 : Html.Attribute msg
my5 =
    A.class "my-5"


mx5 : Html.Attribute msg
mx5 =
    A.class "mx-5"


my6 : Html.Attribute msg
my6 =
    A.class "my-6"


mx6 : Html.Attribute msg
mx6 =
    A.class "mx-6"


my8 : Html.Attribute msg
my8 =
    A.class "my-8"


mx8 : Html.Attribute msg
mx8 =
    A.class "mx-8"


my10 : Html.Attribute msg
my10 =
    A.class "my-10"


mx10 : Html.Attribute msg
mx10 =
    A.class "mx-10"


my12 : Html.Attribute msg
my12 =
    A.class "my-12"


mx12 : Html.Attribute msg
mx12 =
    A.class "mx-12"


my16 : Html.Attribute msg
my16 =
    A.class "my-16"


mx16 : Html.Attribute msg
mx16 =
    A.class "mx-16"


my20 : Html.Attribute msg
my20 =
    A.class "my-20"


mx20 : Html.Attribute msg
mx20 =
    A.class "mx-20"


my24 : Html.Attribute msg
my24 =
    A.class "my-24"


mx24 : Html.Attribute msg
mx24 =
    A.class "mx-24"


my32 : Html.Attribute msg
my32 =
    A.class "my-32"


mx32 : Html.Attribute msg
mx32 =
    A.class "mx-32"


my40 : Html.Attribute msg
my40 =
    A.class "my-40"


mx40 : Html.Attribute msg
mx40 =
    A.class "mx-40"


my48 : Html.Attribute msg
my48 =
    A.class "my-48"


mx48 : Html.Attribute msg
mx48 =
    A.class "mx-48"


my56 : Html.Attribute msg
my56 =
    A.class "my-56"


mx56 : Html.Attribute msg
mx56 =
    A.class "mx-56"


my64 : Html.Attribute msg
my64 =
    A.class "my-64"


mx64 : Html.Attribute msg
mx64 =
    A.class "mx-64"


myAuto : Html.Attribute msg
myAuto =
    A.class "my-auto"


mxAuto : Html.Attribute msg
mxAuto =
    A.class "mx-auto"


myPx : Html.Attribute msg
myPx =
    A.class "my-px"


mxPx : Html.Attribute msg
mxPx =
    A.class "mx-px"


negMy1 : Html.Attribute msg
negMy1 =
    A.class "-my-1"


negMx1 : Html.Attribute msg
negMx1 =
    A.class "-mx-1"


negMy2 : Html.Attribute msg
negMy2 =
    A.class "-my-2"


negMx2 : Html.Attribute msg
negMx2 =
    A.class "-mx-2"


negMy3 : Html.Attribute msg
negMy3 =
    A.class "-my-3"


negMx3 : Html.Attribute msg
negMx3 =
    A.class "-mx-3"


negMy4 : Html.Attribute msg
negMy4 =
    A.class "-my-4"


negMx4 : Html.Attribute msg
negMx4 =
    A.class "-mx-4"


negMy5 : Html.Attribute msg
negMy5 =
    A.class "-my-5"


negMx5 : Html.Attribute msg
negMx5 =
    A.class "-mx-5"


negMy6 : Html.Attribute msg
negMy6 =
    A.class "-my-6"


negMx6 : Html.Attribute msg
negMx6 =
    A.class "-mx-6"


negMy8 : Html.Attribute msg
negMy8 =
    A.class "-my-8"


negMx8 : Html.Attribute msg
negMx8 =
    A.class "-mx-8"


negMy10 : Html.Attribute msg
negMy10 =
    A.class "-my-10"


negMx10 : Html.Attribute msg
negMx10 =
    A.class "-mx-10"


negMy12 : Html.Attribute msg
negMy12 =
    A.class "-my-12"


negMx12 : Html.Attribute msg
negMx12 =
    A.class "-mx-12"


negMy16 : Html.Attribute msg
negMy16 =
    A.class "-my-16"


negMx16 : Html.Attribute msg
negMx16 =
    A.class "-mx-16"


negMy20 : Html.Attribute msg
negMy20 =
    A.class "-my-20"


negMx20 : Html.Attribute msg
negMx20 =
    A.class "-mx-20"


negMy24 : Html.Attribute msg
negMy24 =
    A.class "-my-24"


negMx24 : Html.Attribute msg
negMx24 =
    A.class "-mx-24"


negMy32 : Html.Attribute msg
negMy32 =
    A.class "-my-32"


negMx32 : Html.Attribute msg
negMx32 =
    A.class "-mx-32"


negMy40 : Html.Attribute msg
negMy40 =
    A.class "-my-40"


negMx40 : Html.Attribute msg
negMx40 =
    A.class "-mx-40"


negMy48 : Html.Attribute msg
negMy48 =
    A.class "-my-48"


negMx48 : Html.Attribute msg
negMx48 =
    A.class "-mx-48"


negMy56 : Html.Attribute msg
negMy56 =
    A.class "-my-56"


negMx56 : Html.Attribute msg
negMx56 =
    A.class "-mx-56"


negMy64 : Html.Attribute msg
negMy64 =
    A.class "-my-64"


negMx64 : Html.Attribute msg
negMx64 =
    A.class "-mx-64"


negMyPx : Html.Attribute msg
negMyPx =
    A.class "-my-px"


negMxPx : Html.Attribute msg
negMxPx =
    A.class "-mx-px"


mt0 : Html.Attribute msg
mt0 =
    A.class "mt-0"


mr0 : Html.Attribute msg
mr0 =
    A.class "mr-0"


mb0 : Html.Attribute msg
mb0 =
    A.class "mb-0"


ml0 : Html.Attribute msg
ml0 =
    A.class "ml-0"


mt1 : Html.Attribute msg
mt1 =
    A.class "mt-1"


mr1 : Html.Attribute msg
mr1 =
    A.class "mr-1"


mb1 : Html.Attribute msg
mb1 =
    A.class "mb-1"


ml1 : Html.Attribute msg
ml1 =
    A.class "ml-1"


mt2 : Html.Attribute msg
mt2 =
    A.class "mt-2"


mr2 : Html.Attribute msg
mr2 =
    A.class "mr-2"


mb2 : Html.Attribute msg
mb2 =
    A.class "mb-2"


ml2 : Html.Attribute msg
ml2 =
    A.class "ml-2"


mt3 : Html.Attribute msg
mt3 =
    A.class "mt-3"


mr3 : Html.Attribute msg
mr3 =
    A.class "mr-3"


mb3 : Html.Attribute msg
mb3 =
    A.class "mb-3"


ml3 : Html.Attribute msg
ml3 =
    A.class "ml-3"


mt4 : Html.Attribute msg
mt4 =
    A.class "mt-4"


mr4 : Html.Attribute msg
mr4 =
    A.class "mr-4"


mb4 : Html.Attribute msg
mb4 =
    A.class "mb-4"


ml4 : Html.Attribute msg
ml4 =
    A.class "ml-4"


mt5 : Html.Attribute msg
mt5 =
    A.class "mt-5"


mr5 : Html.Attribute msg
mr5 =
    A.class "mr-5"


mb5 : Html.Attribute msg
mb5 =
    A.class "mb-5"


ml5 : Html.Attribute msg
ml5 =
    A.class "ml-5"


mt6 : Html.Attribute msg
mt6 =
    A.class "mt-6"


mr6 : Html.Attribute msg
mr6 =
    A.class "mr-6"


mb6 : Html.Attribute msg
mb6 =
    A.class "mb-6"


ml6 : Html.Attribute msg
ml6 =
    A.class "ml-6"


mt8 : Html.Attribute msg
mt8 =
    A.class "mt-8"


mr8 : Html.Attribute msg
mr8 =
    A.class "mr-8"


mb8 : Html.Attribute msg
mb8 =
    A.class "mb-8"


ml8 : Html.Attribute msg
ml8 =
    A.class "ml-8"


mt10 : Html.Attribute msg
mt10 =
    A.class "mt-10"


mr10 : Html.Attribute msg
mr10 =
    A.class "mr-10"


mb10 : Html.Attribute msg
mb10 =
    A.class "mb-10"


ml10 : Html.Attribute msg
ml10 =
    A.class "ml-10"


mt12 : Html.Attribute msg
mt12 =
    A.class "mt-12"


mr12 : Html.Attribute msg
mr12 =
    A.class "mr-12"


mb12 : Html.Attribute msg
mb12 =
    A.class "mb-12"


ml12 : Html.Attribute msg
ml12 =
    A.class "ml-12"


mt16 : Html.Attribute msg
mt16 =
    A.class "mt-16"


mr16 : Html.Attribute msg
mr16 =
    A.class "mr-16"


mb16 : Html.Attribute msg
mb16 =
    A.class "mb-16"


ml16 : Html.Attribute msg
ml16 =
    A.class "ml-16"


mt20 : Html.Attribute msg
mt20 =
    A.class "mt-20"


mr20 : Html.Attribute msg
mr20 =
    A.class "mr-20"


mb20 : Html.Attribute msg
mb20 =
    A.class "mb-20"


ml20 : Html.Attribute msg
ml20 =
    A.class "ml-20"


mt24 : Html.Attribute msg
mt24 =
    A.class "mt-24"


mr24 : Html.Attribute msg
mr24 =
    A.class "mr-24"


mb24 : Html.Attribute msg
mb24 =
    A.class "mb-24"


ml24 : Html.Attribute msg
ml24 =
    A.class "ml-24"


mt32 : Html.Attribute msg
mt32 =
    A.class "mt-32"


mr32 : Html.Attribute msg
mr32 =
    A.class "mr-32"


mb32 : Html.Attribute msg
mb32 =
    A.class "mb-32"


ml32 : Html.Attribute msg
ml32 =
    A.class "ml-32"


mt40 : Html.Attribute msg
mt40 =
    A.class "mt-40"


mr40 : Html.Attribute msg
mr40 =
    A.class "mr-40"


mb40 : Html.Attribute msg
mb40 =
    A.class "mb-40"


ml40 : Html.Attribute msg
ml40 =
    A.class "ml-40"


mt48 : Html.Attribute msg
mt48 =
    A.class "mt-48"


mr48 : Html.Attribute msg
mr48 =
    A.class "mr-48"


mb48 : Html.Attribute msg
mb48 =
    A.class "mb-48"


ml48 : Html.Attribute msg
ml48 =
    A.class "ml-48"


mt56 : Html.Attribute msg
mt56 =
    A.class "mt-56"


mr56 : Html.Attribute msg
mr56 =
    A.class "mr-56"


mb56 : Html.Attribute msg
mb56 =
    A.class "mb-56"


ml56 : Html.Attribute msg
ml56 =
    A.class "ml-56"


mt64 : Html.Attribute msg
mt64 =
    A.class "mt-64"


mr64 : Html.Attribute msg
mr64 =
    A.class "mr-64"


mb64 : Html.Attribute msg
mb64 =
    A.class "mb-64"


ml64 : Html.Attribute msg
ml64 =
    A.class "ml-64"


mtAuto : Html.Attribute msg
mtAuto =
    A.class "mt-auto"


mrAuto : Html.Attribute msg
mrAuto =
    A.class "mr-auto"


mbAuto : Html.Attribute msg
mbAuto =
    A.class "mb-auto"


mlAuto : Html.Attribute msg
mlAuto =
    A.class "ml-auto"


mtPx : Html.Attribute msg
mtPx =
    A.class "mt-px"


mrPx : Html.Attribute msg
mrPx =
    A.class "mr-px"


mbPx : Html.Attribute msg
mbPx =
    A.class "mb-px"


mlPx : Html.Attribute msg
mlPx =
    A.class "ml-px"


negMt1 : Html.Attribute msg
negMt1 =
    A.class "-mt-1"


negMr1 : Html.Attribute msg
negMr1 =
    A.class "-mr-1"


negMb1 : Html.Attribute msg
negMb1 =
    A.class "-mb-1"


negMl1 : Html.Attribute msg
negMl1 =
    A.class "-ml-1"


negMt2 : Html.Attribute msg
negMt2 =
    A.class "-mt-2"


negMr2 : Html.Attribute msg
negMr2 =
    A.class "-mr-2"


negMb2 : Html.Attribute msg
negMb2 =
    A.class "-mb-2"


negMl2 : Html.Attribute msg
negMl2 =
    A.class "-ml-2"


negMt3 : Html.Attribute msg
negMt3 =
    A.class "-mt-3"


negMr3 : Html.Attribute msg
negMr3 =
    A.class "-mr-3"


negMb3 : Html.Attribute msg
negMb3 =
    A.class "-mb-3"


negMl3 : Html.Attribute msg
negMl3 =
    A.class "-ml-3"


negMt4 : Html.Attribute msg
negMt4 =
    A.class "-mt-4"


negMr4 : Html.Attribute msg
negMr4 =
    A.class "-mr-4"


negMb4 : Html.Attribute msg
negMb4 =
    A.class "-mb-4"


negMl4 : Html.Attribute msg
negMl4 =
    A.class "-ml-4"


negMt5 : Html.Attribute msg
negMt5 =
    A.class "-mt-5"


negMr5 : Html.Attribute msg
negMr5 =
    A.class "-mr-5"


negMb5 : Html.Attribute msg
negMb5 =
    A.class "-mb-5"


negMl5 : Html.Attribute msg
negMl5 =
    A.class "-ml-5"


negMt6 : Html.Attribute msg
negMt6 =
    A.class "-mt-6"


negMr6 : Html.Attribute msg
negMr6 =
    A.class "-mr-6"


negMb6 : Html.Attribute msg
negMb6 =
    A.class "-mb-6"


negMl6 : Html.Attribute msg
negMl6 =
    A.class "-ml-6"


negMt8 : Html.Attribute msg
negMt8 =
    A.class "-mt-8"


negMr8 : Html.Attribute msg
negMr8 =
    A.class "-mr-8"


negMb8 : Html.Attribute msg
negMb8 =
    A.class "-mb-8"


negMl8 : Html.Attribute msg
negMl8 =
    A.class "-ml-8"


negMt10 : Html.Attribute msg
negMt10 =
    A.class "-mt-10"


negMr10 : Html.Attribute msg
negMr10 =
    A.class "-mr-10"


negMb10 : Html.Attribute msg
negMb10 =
    A.class "-mb-10"


negMl10 : Html.Attribute msg
negMl10 =
    A.class "-ml-10"


negMt12 : Html.Attribute msg
negMt12 =
    A.class "-mt-12"


negMr12 : Html.Attribute msg
negMr12 =
    A.class "-mr-12"


negMb12 : Html.Attribute msg
negMb12 =
    A.class "-mb-12"


negMl12 : Html.Attribute msg
negMl12 =
    A.class "-ml-12"


negMt16 : Html.Attribute msg
negMt16 =
    A.class "-mt-16"


negMr16 : Html.Attribute msg
negMr16 =
    A.class "-mr-16"


negMb16 : Html.Attribute msg
negMb16 =
    A.class "-mb-16"


negMl16 : Html.Attribute msg
negMl16 =
    A.class "-ml-16"


negMt20 : Html.Attribute msg
negMt20 =
    A.class "-mt-20"


negMr20 : Html.Attribute msg
negMr20 =
    A.class "-mr-20"


negMb20 : Html.Attribute msg
negMb20 =
    A.class "-mb-20"


negMl20 : Html.Attribute msg
negMl20 =
    A.class "-ml-20"


negMt24 : Html.Attribute msg
negMt24 =
    A.class "-mt-24"


negMr24 : Html.Attribute msg
negMr24 =
    A.class "-mr-24"


negMb24 : Html.Attribute msg
negMb24 =
    A.class "-mb-24"


negMl24 : Html.Attribute msg
negMl24 =
    A.class "-ml-24"


negMt32 : Html.Attribute msg
negMt32 =
    A.class "-mt-32"


negMr32 : Html.Attribute msg
negMr32 =
    A.class "-mr-32"


negMb32 : Html.Attribute msg
negMb32 =
    A.class "-mb-32"


negMl32 : Html.Attribute msg
negMl32 =
    A.class "-ml-32"


negMt40 : Html.Attribute msg
negMt40 =
    A.class "-mt-40"


negMr40 : Html.Attribute msg
negMr40 =
    A.class "-mr-40"


negMb40 : Html.Attribute msg
negMb40 =
    A.class "-mb-40"


negMl40 : Html.Attribute msg
negMl40 =
    A.class "-ml-40"


negMt48 : Html.Attribute msg
negMt48 =
    A.class "-mt-48"


negMr48 : Html.Attribute msg
negMr48 =
    A.class "-mr-48"


negMb48 : Html.Attribute msg
negMb48 =
    A.class "-mb-48"


negMl48 : Html.Attribute msg
negMl48 =
    A.class "-ml-48"


negMt56 : Html.Attribute msg
negMt56 =
    A.class "-mt-56"


negMr56 : Html.Attribute msg
negMr56 =
    A.class "-mr-56"


negMb56 : Html.Attribute msg
negMb56 =
    A.class "-mb-56"


negMl56 : Html.Attribute msg
negMl56 =
    A.class "-ml-56"


negMt64 : Html.Attribute msg
negMt64 =
    A.class "-mt-64"


negMr64 : Html.Attribute msg
negMr64 =
    A.class "-mr-64"


negMb64 : Html.Attribute msg
negMb64 =
    A.class "-mb-64"


negMl64 : Html.Attribute msg
negMl64 =
    A.class "-ml-64"


negMtPx : Html.Attribute msg
negMtPx =
    A.class "-mt-px"


negMrPx : Html.Attribute msg
negMrPx =
    A.class "-mr-px"


negMbPx : Html.Attribute msg
negMbPx =
    A.class "-mb-px"


negMlPx : Html.Attribute msg
negMlPx =
    A.class "-ml-px"


maxHFull : Html.Attribute msg
maxHFull =
    A.class "max-h-full"


maxHScreen : Html.Attribute msg
maxHScreen =
    A.class "max-h-screen"


maxWNone : Html.Attribute msg
maxWNone =
    A.class "max-w-none"


maxWXs : Html.Attribute msg
maxWXs =
    A.class "max-w-xs"


maxWSm : Html.Attribute msg
maxWSm =
    A.class "max-w-sm"


maxWMd : Html.Attribute msg
maxWMd =
    A.class "max-w-md"


maxWLg : Html.Attribute msg
maxWLg =
    A.class "max-w-lg"


maxWXl : Html.Attribute msg
maxWXl =
    A.class "max-w-xl"


maxW2xl : Html.Attribute msg
maxW2xl =
    A.class "max-w-2xl"


maxW3xl : Html.Attribute msg
maxW3xl =
    A.class "max-w-3xl"


maxW4xl : Html.Attribute msg
maxW4xl =
    A.class "max-w-4xl"


maxW5xl : Html.Attribute msg
maxW5xl =
    A.class "max-w-5xl"


maxW6xl : Html.Attribute msg
maxW6xl =
    A.class "max-w-6xl"


maxWFull : Html.Attribute msg
maxWFull =
    A.class "max-w-full"


maxWScreenSm : Html.Attribute msg
maxWScreenSm =
    A.class "max-w-screen-sm"


maxWScreenMd : Html.Attribute msg
maxWScreenMd =
    A.class "max-w-screen-md"


maxWScreenLg : Html.Attribute msg
maxWScreenLg =
    A.class "max-w-screen-lg"


maxWScreenXl : Html.Attribute msg
maxWScreenXl =
    A.class "max-w-screen-xl"


minH0 : Html.Attribute msg
minH0 =
    A.class "min-h-0"


minHFull : Html.Attribute msg
minHFull =
    A.class "min-h-full"


minHScreen : Html.Attribute msg
minHScreen =
    A.class "min-h-screen"


minW0 : Html.Attribute msg
minW0 =
    A.class "min-w-0"


minWFull : Html.Attribute msg
minWFull =
    A.class "min-w-full"


objectContain : Html.Attribute msg
objectContain =
    A.class "object-contain"


objectCover : Html.Attribute msg
objectCover =
    A.class "object-cover"


objectFill : Html.Attribute msg
objectFill =
    A.class "object-fill"


objectNone : Html.Attribute msg
objectNone =
    A.class "object-none"


objectScaleDown : Html.Attribute msg
objectScaleDown =
    A.class "object-scale-down"


objectBottom : Html.Attribute msg
objectBottom =
    A.class "object-bottom"


objectCenter : Html.Attribute msg
objectCenter =
    A.class "object-center"


objectLeft : Html.Attribute msg
objectLeft =
    A.class "object-left"


objectLeftBottom : Html.Attribute msg
objectLeftBottom =
    A.class "object-left-bottom"


objectLeftTop : Html.Attribute msg
objectLeftTop =
    A.class "object-left-top"


objectRight : Html.Attribute msg
objectRight =
    A.class "object-right"


objectRightBottom : Html.Attribute msg
objectRightBottom =
    A.class "object-right-bottom"


objectRightTop : Html.Attribute msg
objectRightTop =
    A.class "object-right-top"


objectTop : Html.Attribute msg
objectTop =
    A.class "object-top"


opacity0 : Html.Attribute msg
opacity0 =
    A.class "opacity-0"


opacity25 : Html.Attribute msg
opacity25 =
    A.class "opacity-25"


opacity50 : Html.Attribute msg
opacity50 =
    A.class "opacity-50"


opacity75 : Html.Attribute msg
opacity75 =
    A.class "opacity-75"


opacity100 : Html.Attribute msg
opacity100 =
    A.class "opacity-100"


hoverOpacity0 : Html.Attribute msg
hoverOpacity0 =
    A.class "hover:opacity-0"


hoverOpacity25 : Html.Attribute msg
hoverOpacity25 =
    A.class "hover:opacity-25"


hoverOpacity50 : Html.Attribute msg
hoverOpacity50 =
    A.class "hover:opacity-50"


hoverOpacity75 : Html.Attribute msg
hoverOpacity75 =
    A.class "hover:opacity-75"


hoverOpacity100 : Html.Attribute msg
hoverOpacity100 =
    A.class "hover:opacity-100"


focusOpacity0 : Html.Attribute msg
focusOpacity0 =
    A.class "focus:opacity-0"


focusOpacity25 : Html.Attribute msg
focusOpacity25 =
    A.class "focus:opacity-25"


focusOpacity50 : Html.Attribute msg
focusOpacity50 =
    A.class "focus:opacity-50"


focusOpacity75 : Html.Attribute msg
focusOpacity75 =
    A.class "focus:opacity-75"


focusOpacity100 : Html.Attribute msg
focusOpacity100 =
    A.class "focus:opacity-100"


outlineNone : Html.Attribute msg
outlineNone =
    A.class "outline-none"


focusOutlineNone : Html.Attribute msg
focusOutlineNone =
    A.class "focus:outline-none"


overflowAuto : Html.Attribute msg
overflowAuto =
    A.class "overflow-auto"


overflowHidden : Html.Attribute msg
overflowHidden =
    A.class "overflow-hidden"


overflowVisible : Html.Attribute msg
overflowVisible =
    A.class "overflow-visible"


overflowScroll : Html.Attribute msg
overflowScroll =
    A.class "overflow-scroll"


overflowXAuto : Html.Attribute msg
overflowXAuto =
    A.class "overflow-x-auto"


overflowYAuto : Html.Attribute msg
overflowYAuto =
    A.class "overflow-y-auto"


overflowXHidden : Html.Attribute msg
overflowXHidden =
    A.class "overflow-x-hidden"


overflowYHidden : Html.Attribute msg
overflowYHidden =
    A.class "overflow-y-hidden"


overflowXVisible : Html.Attribute msg
overflowXVisible =
    A.class "overflow-x-visible"


overflowYVisible : Html.Attribute msg
overflowYVisible =
    A.class "overflow-y-visible"


overflowXScroll : Html.Attribute msg
overflowXScroll =
    A.class "overflow-x-scroll"


overflowYScroll : Html.Attribute msg
overflowYScroll =
    A.class "overflow-y-scroll"


scrollingTouch : Html.Attribute msg
scrollingTouch =
    A.class "scrolling-touch"


scrollingAuto : Html.Attribute msg
scrollingAuto =
    A.class "scrolling-auto"


overscrollAuto : Html.Attribute msg
overscrollAuto =
    A.class "overscroll-auto"


overscrollContain : Html.Attribute msg
overscrollContain =
    A.class "overscroll-contain"


overscrollNone : Html.Attribute msg
overscrollNone =
    A.class "overscroll-none"


overscrollYAuto : Html.Attribute msg
overscrollYAuto =
    A.class "overscroll-y-auto"


overscrollYContain : Html.Attribute msg
overscrollYContain =
    A.class "overscroll-y-contain"


overscrollYNone : Html.Attribute msg
overscrollYNone =
    A.class "overscroll-y-none"


overscrollXAuto : Html.Attribute msg
overscrollXAuto =
    A.class "overscroll-x-auto"


overscrollXContain : Html.Attribute msg
overscrollXContain =
    A.class "overscroll-x-contain"


overscrollXNone : Html.Attribute msg
overscrollXNone =
    A.class "overscroll-x-none"


p0 : Html.Attribute msg
p0 =
    A.class "p-0"


p1 : Html.Attribute msg
p1 =
    A.class "p-1"


p2 : Html.Attribute msg
p2 =
    A.class "p-2"


p3 : Html.Attribute msg
p3 =
    A.class "p-3"


p4 : Html.Attribute msg
p4 =
    A.class "p-4"


p5 : Html.Attribute msg
p5 =
    A.class "p-5"


p6 : Html.Attribute msg
p6 =
    A.class "p-6"


p8 : Html.Attribute msg
p8 =
    A.class "p-8"


p10 : Html.Attribute msg
p10 =
    A.class "p-10"


p12 : Html.Attribute msg
p12 =
    A.class "p-12"


p16 : Html.Attribute msg
p16 =
    A.class "p-16"


p20 : Html.Attribute msg
p20 =
    A.class "p-20"


p24 : Html.Attribute msg
p24 =
    A.class "p-24"


p32 : Html.Attribute msg
p32 =
    A.class "p-32"


p40 : Html.Attribute msg
p40 =
    A.class "p-40"


p48 : Html.Attribute msg
p48 =
    A.class "p-48"


p56 : Html.Attribute msg
p56 =
    A.class "p-56"


p64 : Html.Attribute msg
p64 =
    A.class "p-64"


pPx : Html.Attribute msg
pPx =
    A.class "p-px"


py0 : Html.Attribute msg
py0 =
    A.class "py-0"


px0 : Html.Attribute msg
px0 =
    A.class "px-0"


py1 : Html.Attribute msg
py1 =
    A.class "py-1"


px1 : Html.Attribute msg
px1 =
    A.class "px-1"


py2 : Html.Attribute msg
py2 =
    A.class "py-2"


px2 : Html.Attribute msg
px2 =
    A.class "px-2"


py3 : Html.Attribute msg
py3 =
    A.class "py-3"


px3 : Html.Attribute msg
px3 =
    A.class "px-3"


py4 : Html.Attribute msg
py4 =
    A.class "py-4"


px4 : Html.Attribute msg
px4 =
    A.class "px-4"


py5 : Html.Attribute msg
py5 =
    A.class "py-5"


px5 : Html.Attribute msg
px5 =
    A.class "px-5"


py6 : Html.Attribute msg
py6 =
    A.class "py-6"


px6 : Html.Attribute msg
px6 =
    A.class "px-6"


py8 : Html.Attribute msg
py8 =
    A.class "py-8"


px8 : Html.Attribute msg
px8 =
    A.class "px-8"


py10 : Html.Attribute msg
py10 =
    A.class "py-10"


px10 : Html.Attribute msg
px10 =
    A.class "px-10"


py12 : Html.Attribute msg
py12 =
    A.class "py-12"


px12 : Html.Attribute msg
px12 =
    A.class "px-12"


py16 : Html.Attribute msg
py16 =
    A.class "py-16"


px16 : Html.Attribute msg
px16 =
    A.class "px-16"


py20 : Html.Attribute msg
py20 =
    A.class "py-20"


px20 : Html.Attribute msg
px20 =
    A.class "px-20"


py24 : Html.Attribute msg
py24 =
    A.class "py-24"


px24 : Html.Attribute msg
px24 =
    A.class "px-24"


py32 : Html.Attribute msg
py32 =
    A.class "py-32"


px32 : Html.Attribute msg
px32 =
    A.class "px-32"


py40 : Html.Attribute msg
py40 =
    A.class "py-40"


px40 : Html.Attribute msg
px40 =
    A.class "px-40"


py48 : Html.Attribute msg
py48 =
    A.class "py-48"


px48 : Html.Attribute msg
px48 =
    A.class "px-48"


py56 : Html.Attribute msg
py56 =
    A.class "py-56"


px56 : Html.Attribute msg
px56 =
    A.class "px-56"


py64 : Html.Attribute msg
py64 =
    A.class "py-64"


px64 : Html.Attribute msg
px64 =
    A.class "px-64"


pyPx : Html.Attribute msg
pyPx =
    A.class "py-px"


pxPx : Html.Attribute msg
pxPx =
    A.class "px-px"


pt0 : Html.Attribute msg
pt0 =
    A.class "pt-0"


pr0 : Html.Attribute msg
pr0 =
    A.class "pr-0"


pb0 : Html.Attribute msg
pb0 =
    A.class "pb-0"


pl0 : Html.Attribute msg
pl0 =
    A.class "pl-0"


pt1 : Html.Attribute msg
pt1 =
    A.class "pt-1"


pr1 : Html.Attribute msg
pr1 =
    A.class "pr-1"


pb1 : Html.Attribute msg
pb1 =
    A.class "pb-1"


pl1 : Html.Attribute msg
pl1 =
    A.class "pl-1"


pt2 : Html.Attribute msg
pt2 =
    A.class "pt-2"


pr2 : Html.Attribute msg
pr2 =
    A.class "pr-2"


pb2 : Html.Attribute msg
pb2 =
    A.class "pb-2"


pl2 : Html.Attribute msg
pl2 =
    A.class "pl-2"


pt3 : Html.Attribute msg
pt3 =
    A.class "pt-3"


pr3 : Html.Attribute msg
pr3 =
    A.class "pr-3"


pb3 : Html.Attribute msg
pb3 =
    A.class "pb-3"


pl3 : Html.Attribute msg
pl3 =
    A.class "pl-3"


pt4 : Html.Attribute msg
pt4 =
    A.class "pt-4"


pr4 : Html.Attribute msg
pr4 =
    A.class "pr-4"


pb4 : Html.Attribute msg
pb4 =
    A.class "pb-4"


pl4 : Html.Attribute msg
pl4 =
    A.class "pl-4"


pt5 : Html.Attribute msg
pt5 =
    A.class "pt-5"


pr5 : Html.Attribute msg
pr5 =
    A.class "pr-5"


pb5 : Html.Attribute msg
pb5 =
    A.class "pb-5"


pl5 : Html.Attribute msg
pl5 =
    A.class "pl-5"


pt6 : Html.Attribute msg
pt6 =
    A.class "pt-6"


pr6 : Html.Attribute msg
pr6 =
    A.class "pr-6"


pb6 : Html.Attribute msg
pb6 =
    A.class "pb-6"


pl6 : Html.Attribute msg
pl6 =
    A.class "pl-6"


pt8 : Html.Attribute msg
pt8 =
    A.class "pt-8"


pr8 : Html.Attribute msg
pr8 =
    A.class "pr-8"


pb8 : Html.Attribute msg
pb8 =
    A.class "pb-8"


pl8 : Html.Attribute msg
pl8 =
    A.class "pl-8"


pt10 : Html.Attribute msg
pt10 =
    A.class "pt-10"


pr10 : Html.Attribute msg
pr10 =
    A.class "pr-10"


pb10 : Html.Attribute msg
pb10 =
    A.class "pb-10"


pl10 : Html.Attribute msg
pl10 =
    A.class "pl-10"


pt12 : Html.Attribute msg
pt12 =
    A.class "pt-12"


pr12 : Html.Attribute msg
pr12 =
    A.class "pr-12"


pb12 : Html.Attribute msg
pb12 =
    A.class "pb-12"


pl12 : Html.Attribute msg
pl12 =
    A.class "pl-12"


pt16 : Html.Attribute msg
pt16 =
    A.class "pt-16"


pr16 : Html.Attribute msg
pr16 =
    A.class "pr-16"


pb16 : Html.Attribute msg
pb16 =
    A.class "pb-16"


pl16 : Html.Attribute msg
pl16 =
    A.class "pl-16"


pt20 : Html.Attribute msg
pt20 =
    A.class "pt-20"


pr20 : Html.Attribute msg
pr20 =
    A.class "pr-20"


pb20 : Html.Attribute msg
pb20 =
    A.class "pb-20"


pl20 : Html.Attribute msg
pl20 =
    A.class "pl-20"


pt24 : Html.Attribute msg
pt24 =
    A.class "pt-24"


pr24 : Html.Attribute msg
pr24 =
    A.class "pr-24"


pb24 : Html.Attribute msg
pb24 =
    A.class "pb-24"


pl24 : Html.Attribute msg
pl24 =
    A.class "pl-24"


pt32 : Html.Attribute msg
pt32 =
    A.class "pt-32"


pr32 : Html.Attribute msg
pr32 =
    A.class "pr-32"


pb32 : Html.Attribute msg
pb32 =
    A.class "pb-32"


pl32 : Html.Attribute msg
pl32 =
    A.class "pl-32"


pt40 : Html.Attribute msg
pt40 =
    A.class "pt-40"


pr40 : Html.Attribute msg
pr40 =
    A.class "pr-40"


pb40 : Html.Attribute msg
pb40 =
    A.class "pb-40"


pl40 : Html.Attribute msg
pl40 =
    A.class "pl-40"


pt48 : Html.Attribute msg
pt48 =
    A.class "pt-48"


pr48 : Html.Attribute msg
pr48 =
    A.class "pr-48"


pb48 : Html.Attribute msg
pb48 =
    A.class "pb-48"


pl48 : Html.Attribute msg
pl48 =
    A.class "pl-48"


pt56 : Html.Attribute msg
pt56 =
    A.class "pt-56"


pr56 : Html.Attribute msg
pr56 =
    A.class "pr-56"


pb56 : Html.Attribute msg
pb56 =
    A.class "pb-56"


pl56 : Html.Attribute msg
pl56 =
    A.class "pl-56"


pt64 : Html.Attribute msg
pt64 =
    A.class "pt-64"


pr64 : Html.Attribute msg
pr64 =
    A.class "pr-64"


pb64 : Html.Attribute msg
pb64 =
    A.class "pb-64"


pl64 : Html.Attribute msg
pl64 =
    A.class "pl-64"


ptPx : Html.Attribute msg
ptPx =
    A.class "pt-px"


prPx : Html.Attribute msg
prPx =
    A.class "pr-px"


pbPx : Html.Attribute msg
pbPx =
    A.class "pb-px"


plPx : Html.Attribute msg
plPx =
    A.class "pl-px"


placeholderTransparent : Html.Attribute msg
placeholderTransparent =
    A.class "placeholder-transparent"


placeholderCurrent : Html.Attribute msg
placeholderCurrent =
    A.class "placeholder-current"


placeholderBlack : Html.Attribute msg
placeholderBlack =
    A.class "placeholder-black"


placeholderWhite : Html.Attribute msg
placeholderWhite =
    A.class "placeholder-white"


placeholderGray100 : Html.Attribute msg
placeholderGray100 =
    A.class "placeholder-gray-100"


placeholderGray200 : Html.Attribute msg
placeholderGray200 =
    A.class "placeholder-gray-200"


placeholderGray300 : Html.Attribute msg
placeholderGray300 =
    A.class "placeholder-gray-300"


placeholderGray400 : Html.Attribute msg
placeholderGray400 =
    A.class "placeholder-gray-400"


placeholderGray500 : Html.Attribute msg
placeholderGray500 =
    A.class "placeholder-gray-500"


placeholderGray600 : Html.Attribute msg
placeholderGray600 =
    A.class "placeholder-gray-600"


placeholderGray700 : Html.Attribute msg
placeholderGray700 =
    A.class "placeholder-gray-700"


placeholderGray800 : Html.Attribute msg
placeholderGray800 =
    A.class "placeholder-gray-800"


placeholderGray900 : Html.Attribute msg
placeholderGray900 =
    A.class "placeholder-gray-900"


placeholderRed100 : Html.Attribute msg
placeholderRed100 =
    A.class "placeholder-red-100"


placeholderRed200 : Html.Attribute msg
placeholderRed200 =
    A.class "placeholder-red-200"


placeholderRed300 : Html.Attribute msg
placeholderRed300 =
    A.class "placeholder-red-300"


placeholderRed400 : Html.Attribute msg
placeholderRed400 =
    A.class "placeholder-red-400"


placeholderRed500 : Html.Attribute msg
placeholderRed500 =
    A.class "placeholder-red-500"


placeholderRed600 : Html.Attribute msg
placeholderRed600 =
    A.class "placeholder-red-600"


placeholderRed700 : Html.Attribute msg
placeholderRed700 =
    A.class "placeholder-red-700"


placeholderRed800 : Html.Attribute msg
placeholderRed800 =
    A.class "placeholder-red-800"


placeholderRed900 : Html.Attribute msg
placeholderRed900 =
    A.class "placeholder-red-900"


placeholderOrange100 : Html.Attribute msg
placeholderOrange100 =
    A.class "placeholder-orange-100"


placeholderOrange200 : Html.Attribute msg
placeholderOrange200 =
    A.class "placeholder-orange-200"


placeholderOrange300 : Html.Attribute msg
placeholderOrange300 =
    A.class "placeholder-orange-300"


placeholderOrange400 : Html.Attribute msg
placeholderOrange400 =
    A.class "placeholder-orange-400"


placeholderOrange500 : Html.Attribute msg
placeholderOrange500 =
    A.class "placeholder-orange-500"


placeholderOrange600 : Html.Attribute msg
placeholderOrange600 =
    A.class "placeholder-orange-600"


placeholderOrange700 : Html.Attribute msg
placeholderOrange700 =
    A.class "placeholder-orange-700"


placeholderOrange800 : Html.Attribute msg
placeholderOrange800 =
    A.class "placeholder-orange-800"


placeholderOrange900 : Html.Attribute msg
placeholderOrange900 =
    A.class "placeholder-orange-900"


placeholderYellow100 : Html.Attribute msg
placeholderYellow100 =
    A.class "placeholder-yellow-100"


placeholderYellow200 : Html.Attribute msg
placeholderYellow200 =
    A.class "placeholder-yellow-200"


placeholderYellow300 : Html.Attribute msg
placeholderYellow300 =
    A.class "placeholder-yellow-300"


placeholderYellow400 : Html.Attribute msg
placeholderYellow400 =
    A.class "placeholder-yellow-400"


placeholderYellow500 : Html.Attribute msg
placeholderYellow500 =
    A.class "placeholder-yellow-500"


placeholderYellow600 : Html.Attribute msg
placeholderYellow600 =
    A.class "placeholder-yellow-600"


placeholderYellow700 : Html.Attribute msg
placeholderYellow700 =
    A.class "placeholder-yellow-700"


placeholderYellow800 : Html.Attribute msg
placeholderYellow800 =
    A.class "placeholder-yellow-800"


placeholderYellow900 : Html.Attribute msg
placeholderYellow900 =
    A.class "placeholder-yellow-900"


placeholderGreen100 : Html.Attribute msg
placeholderGreen100 =
    A.class "placeholder-green-100"


placeholderGreen200 : Html.Attribute msg
placeholderGreen200 =
    A.class "placeholder-green-200"


placeholderGreen300 : Html.Attribute msg
placeholderGreen300 =
    A.class "placeholder-green-300"


placeholderGreen400 : Html.Attribute msg
placeholderGreen400 =
    A.class "placeholder-green-400"


placeholderGreen500 : Html.Attribute msg
placeholderGreen500 =
    A.class "placeholder-green-500"


placeholderGreen600 : Html.Attribute msg
placeholderGreen600 =
    A.class "placeholder-green-600"


placeholderGreen700 : Html.Attribute msg
placeholderGreen700 =
    A.class "placeholder-green-700"


placeholderGreen800 : Html.Attribute msg
placeholderGreen800 =
    A.class "placeholder-green-800"


placeholderGreen900 : Html.Attribute msg
placeholderGreen900 =
    A.class "placeholder-green-900"


placeholderTeal100 : Html.Attribute msg
placeholderTeal100 =
    A.class "placeholder-teal-100"


placeholderTeal200 : Html.Attribute msg
placeholderTeal200 =
    A.class "placeholder-teal-200"


placeholderTeal300 : Html.Attribute msg
placeholderTeal300 =
    A.class "placeholder-teal-300"


placeholderTeal400 : Html.Attribute msg
placeholderTeal400 =
    A.class "placeholder-teal-400"


placeholderTeal500 : Html.Attribute msg
placeholderTeal500 =
    A.class "placeholder-teal-500"


placeholderTeal600 : Html.Attribute msg
placeholderTeal600 =
    A.class "placeholder-teal-600"


placeholderTeal700 : Html.Attribute msg
placeholderTeal700 =
    A.class "placeholder-teal-700"


placeholderTeal800 : Html.Attribute msg
placeholderTeal800 =
    A.class "placeholder-teal-800"


placeholderTeal900 : Html.Attribute msg
placeholderTeal900 =
    A.class "placeholder-teal-900"


placeholderBlue100 : Html.Attribute msg
placeholderBlue100 =
    A.class "placeholder-blue-100"


placeholderBlue200 : Html.Attribute msg
placeholderBlue200 =
    A.class "placeholder-blue-200"


placeholderBlue300 : Html.Attribute msg
placeholderBlue300 =
    A.class "placeholder-blue-300"


placeholderBlue400 : Html.Attribute msg
placeholderBlue400 =
    A.class "placeholder-blue-400"


placeholderBlue500 : Html.Attribute msg
placeholderBlue500 =
    A.class "placeholder-blue-500"


placeholderBlue600 : Html.Attribute msg
placeholderBlue600 =
    A.class "placeholder-blue-600"


placeholderBlue700 : Html.Attribute msg
placeholderBlue700 =
    A.class "placeholder-blue-700"


placeholderBlue800 : Html.Attribute msg
placeholderBlue800 =
    A.class "placeholder-blue-800"


placeholderBlue900 : Html.Attribute msg
placeholderBlue900 =
    A.class "placeholder-blue-900"


placeholderIndigo100 : Html.Attribute msg
placeholderIndigo100 =
    A.class "placeholder-indigo-100"


placeholderIndigo200 : Html.Attribute msg
placeholderIndigo200 =
    A.class "placeholder-indigo-200"


placeholderIndigo300 : Html.Attribute msg
placeholderIndigo300 =
    A.class "placeholder-indigo-300"


placeholderIndigo400 : Html.Attribute msg
placeholderIndigo400 =
    A.class "placeholder-indigo-400"


placeholderIndigo500 : Html.Attribute msg
placeholderIndigo500 =
    A.class "placeholder-indigo-500"


placeholderIndigo600 : Html.Attribute msg
placeholderIndigo600 =
    A.class "placeholder-indigo-600"


placeholderIndigo700 : Html.Attribute msg
placeholderIndigo700 =
    A.class "placeholder-indigo-700"


placeholderIndigo800 : Html.Attribute msg
placeholderIndigo800 =
    A.class "placeholder-indigo-800"


placeholderIndigo900 : Html.Attribute msg
placeholderIndigo900 =
    A.class "placeholder-indigo-900"


placeholderPurple100 : Html.Attribute msg
placeholderPurple100 =
    A.class "placeholder-purple-100"


placeholderPurple200 : Html.Attribute msg
placeholderPurple200 =
    A.class "placeholder-purple-200"


placeholderPurple300 : Html.Attribute msg
placeholderPurple300 =
    A.class "placeholder-purple-300"


placeholderPurple400 : Html.Attribute msg
placeholderPurple400 =
    A.class "placeholder-purple-400"


placeholderPurple500 : Html.Attribute msg
placeholderPurple500 =
    A.class "placeholder-purple-500"


placeholderPurple600 : Html.Attribute msg
placeholderPurple600 =
    A.class "placeholder-purple-600"


placeholderPurple700 : Html.Attribute msg
placeholderPurple700 =
    A.class "placeholder-purple-700"


placeholderPurple800 : Html.Attribute msg
placeholderPurple800 =
    A.class "placeholder-purple-800"


placeholderPurple900 : Html.Attribute msg
placeholderPurple900 =
    A.class "placeholder-purple-900"


placeholderPink100 : Html.Attribute msg
placeholderPink100 =
    A.class "placeholder-pink-100"


placeholderPink200 : Html.Attribute msg
placeholderPink200 =
    A.class "placeholder-pink-200"


placeholderPink300 : Html.Attribute msg
placeholderPink300 =
    A.class "placeholder-pink-300"


placeholderPink400 : Html.Attribute msg
placeholderPink400 =
    A.class "placeholder-pink-400"


placeholderPink500 : Html.Attribute msg
placeholderPink500 =
    A.class "placeholder-pink-500"


placeholderPink600 : Html.Attribute msg
placeholderPink600 =
    A.class "placeholder-pink-600"


placeholderPink700 : Html.Attribute msg
placeholderPink700 =
    A.class "placeholder-pink-700"


placeholderPink800 : Html.Attribute msg
placeholderPink800 =
    A.class "placeholder-pink-800"


placeholderPink900 : Html.Attribute msg
placeholderPink900 =
    A.class "placeholder-pink-900"


focusPlaceholderTransparentFocus : Html.Attribute msg
focusPlaceholderTransparentFocus =
    A.class "focus:placeholder-transparent:focus"


focusPlaceholderCurrentFocus : Html.Attribute msg
focusPlaceholderCurrentFocus =
    A.class "focus:placeholder-current:focus"


focusPlaceholderBlackFocus : Html.Attribute msg
focusPlaceholderBlackFocus =
    A.class "focus:placeholder-black:focus"


focusPlaceholderWhiteFocus : Html.Attribute msg
focusPlaceholderWhiteFocus =
    A.class "focus:placeholder-white:focus"


focusPlaceholderGray100Focus : Html.Attribute msg
focusPlaceholderGray100Focus =
    A.class "focus:placeholder-gray-100:focus"


focusPlaceholderGray200Focus : Html.Attribute msg
focusPlaceholderGray200Focus =
    A.class "focus:placeholder-gray-200:focus"


focusPlaceholderGray300Focus : Html.Attribute msg
focusPlaceholderGray300Focus =
    A.class "focus:placeholder-gray-300:focus"


focusPlaceholderGray400Focus : Html.Attribute msg
focusPlaceholderGray400Focus =
    A.class "focus:placeholder-gray-400:focus"


focusPlaceholderGray500Focus : Html.Attribute msg
focusPlaceholderGray500Focus =
    A.class "focus:placeholder-gray-500:focus"


focusPlaceholderGray600Focus : Html.Attribute msg
focusPlaceholderGray600Focus =
    A.class "focus:placeholder-gray-600:focus"


focusPlaceholderGray700Focus : Html.Attribute msg
focusPlaceholderGray700Focus =
    A.class "focus:placeholder-gray-700:focus"


focusPlaceholderGray800Focus : Html.Attribute msg
focusPlaceholderGray800Focus =
    A.class "focus:placeholder-gray-800:focus"


focusPlaceholderGray900Focus : Html.Attribute msg
focusPlaceholderGray900Focus =
    A.class "focus:placeholder-gray-900:focus"


focusPlaceholderRed100Focus : Html.Attribute msg
focusPlaceholderRed100Focus =
    A.class "focus:placeholder-red-100:focus"


focusPlaceholderRed200Focus : Html.Attribute msg
focusPlaceholderRed200Focus =
    A.class "focus:placeholder-red-200:focus"


focusPlaceholderRed300Focus : Html.Attribute msg
focusPlaceholderRed300Focus =
    A.class "focus:placeholder-red-300:focus"


focusPlaceholderRed400Focus : Html.Attribute msg
focusPlaceholderRed400Focus =
    A.class "focus:placeholder-red-400:focus"


focusPlaceholderRed500Focus : Html.Attribute msg
focusPlaceholderRed500Focus =
    A.class "focus:placeholder-red-500:focus"


focusPlaceholderRed600Focus : Html.Attribute msg
focusPlaceholderRed600Focus =
    A.class "focus:placeholder-red-600:focus"


focusPlaceholderRed700Focus : Html.Attribute msg
focusPlaceholderRed700Focus =
    A.class "focus:placeholder-red-700:focus"


focusPlaceholderRed800Focus : Html.Attribute msg
focusPlaceholderRed800Focus =
    A.class "focus:placeholder-red-800:focus"


focusPlaceholderRed900Focus : Html.Attribute msg
focusPlaceholderRed900Focus =
    A.class "focus:placeholder-red-900:focus"


focusPlaceholderOrange100Focus : Html.Attribute msg
focusPlaceholderOrange100Focus =
    A.class "focus:placeholder-orange-100:focus"


focusPlaceholderOrange200Focus : Html.Attribute msg
focusPlaceholderOrange200Focus =
    A.class "focus:placeholder-orange-200:focus"


focusPlaceholderOrange300Focus : Html.Attribute msg
focusPlaceholderOrange300Focus =
    A.class "focus:placeholder-orange-300:focus"


focusPlaceholderOrange400Focus : Html.Attribute msg
focusPlaceholderOrange400Focus =
    A.class "focus:placeholder-orange-400:focus"


focusPlaceholderOrange500Focus : Html.Attribute msg
focusPlaceholderOrange500Focus =
    A.class "focus:placeholder-orange-500:focus"


focusPlaceholderOrange600Focus : Html.Attribute msg
focusPlaceholderOrange600Focus =
    A.class "focus:placeholder-orange-600:focus"


focusPlaceholderOrange700Focus : Html.Attribute msg
focusPlaceholderOrange700Focus =
    A.class "focus:placeholder-orange-700:focus"


focusPlaceholderOrange800Focus : Html.Attribute msg
focusPlaceholderOrange800Focus =
    A.class "focus:placeholder-orange-800:focus"


focusPlaceholderOrange900Focus : Html.Attribute msg
focusPlaceholderOrange900Focus =
    A.class "focus:placeholder-orange-900:focus"


focusPlaceholderYellow100Focus : Html.Attribute msg
focusPlaceholderYellow100Focus =
    A.class "focus:placeholder-yellow-100:focus"


focusPlaceholderYellow200Focus : Html.Attribute msg
focusPlaceholderYellow200Focus =
    A.class "focus:placeholder-yellow-200:focus"


focusPlaceholderYellow300Focus : Html.Attribute msg
focusPlaceholderYellow300Focus =
    A.class "focus:placeholder-yellow-300:focus"


focusPlaceholderYellow400Focus : Html.Attribute msg
focusPlaceholderYellow400Focus =
    A.class "focus:placeholder-yellow-400:focus"


focusPlaceholderYellow500Focus : Html.Attribute msg
focusPlaceholderYellow500Focus =
    A.class "focus:placeholder-yellow-500:focus"


focusPlaceholderYellow600Focus : Html.Attribute msg
focusPlaceholderYellow600Focus =
    A.class "focus:placeholder-yellow-600:focus"


focusPlaceholderYellow700Focus : Html.Attribute msg
focusPlaceholderYellow700Focus =
    A.class "focus:placeholder-yellow-700:focus"


focusPlaceholderYellow800Focus : Html.Attribute msg
focusPlaceholderYellow800Focus =
    A.class "focus:placeholder-yellow-800:focus"


focusPlaceholderYellow900Focus : Html.Attribute msg
focusPlaceholderYellow900Focus =
    A.class "focus:placeholder-yellow-900:focus"


focusPlaceholderGreen100Focus : Html.Attribute msg
focusPlaceholderGreen100Focus =
    A.class "focus:placeholder-green-100:focus"


focusPlaceholderGreen200Focus : Html.Attribute msg
focusPlaceholderGreen200Focus =
    A.class "focus:placeholder-green-200:focus"


focusPlaceholderGreen300Focus : Html.Attribute msg
focusPlaceholderGreen300Focus =
    A.class "focus:placeholder-green-300:focus"


focusPlaceholderGreen400Focus : Html.Attribute msg
focusPlaceholderGreen400Focus =
    A.class "focus:placeholder-green-400:focus"


focusPlaceholderGreen500Focus : Html.Attribute msg
focusPlaceholderGreen500Focus =
    A.class "focus:placeholder-green-500:focus"


focusPlaceholderGreen600Focus : Html.Attribute msg
focusPlaceholderGreen600Focus =
    A.class "focus:placeholder-green-600:focus"


focusPlaceholderGreen700Focus : Html.Attribute msg
focusPlaceholderGreen700Focus =
    A.class "focus:placeholder-green-700:focus"


focusPlaceholderGreen800Focus : Html.Attribute msg
focusPlaceholderGreen800Focus =
    A.class "focus:placeholder-green-800:focus"


focusPlaceholderGreen900Focus : Html.Attribute msg
focusPlaceholderGreen900Focus =
    A.class "focus:placeholder-green-900:focus"


focusPlaceholderTeal100Focus : Html.Attribute msg
focusPlaceholderTeal100Focus =
    A.class "focus:placeholder-teal-100:focus"


focusPlaceholderTeal200Focus : Html.Attribute msg
focusPlaceholderTeal200Focus =
    A.class "focus:placeholder-teal-200:focus"


focusPlaceholderTeal300Focus : Html.Attribute msg
focusPlaceholderTeal300Focus =
    A.class "focus:placeholder-teal-300:focus"


focusPlaceholderTeal400Focus : Html.Attribute msg
focusPlaceholderTeal400Focus =
    A.class "focus:placeholder-teal-400:focus"


focusPlaceholderTeal500Focus : Html.Attribute msg
focusPlaceholderTeal500Focus =
    A.class "focus:placeholder-teal-500:focus"


focusPlaceholderTeal600Focus : Html.Attribute msg
focusPlaceholderTeal600Focus =
    A.class "focus:placeholder-teal-600:focus"


focusPlaceholderTeal700Focus : Html.Attribute msg
focusPlaceholderTeal700Focus =
    A.class "focus:placeholder-teal-700:focus"


focusPlaceholderTeal800Focus : Html.Attribute msg
focusPlaceholderTeal800Focus =
    A.class "focus:placeholder-teal-800:focus"


focusPlaceholderTeal900Focus : Html.Attribute msg
focusPlaceholderTeal900Focus =
    A.class "focus:placeholder-teal-900:focus"


focusPlaceholderBlue100Focus : Html.Attribute msg
focusPlaceholderBlue100Focus =
    A.class "focus:placeholder-blue-100:focus"


focusPlaceholderBlue200Focus : Html.Attribute msg
focusPlaceholderBlue200Focus =
    A.class "focus:placeholder-blue-200:focus"


focusPlaceholderBlue300Focus : Html.Attribute msg
focusPlaceholderBlue300Focus =
    A.class "focus:placeholder-blue-300:focus"


focusPlaceholderBlue400Focus : Html.Attribute msg
focusPlaceholderBlue400Focus =
    A.class "focus:placeholder-blue-400:focus"


focusPlaceholderBlue500Focus : Html.Attribute msg
focusPlaceholderBlue500Focus =
    A.class "focus:placeholder-blue-500:focus"


focusPlaceholderBlue600Focus : Html.Attribute msg
focusPlaceholderBlue600Focus =
    A.class "focus:placeholder-blue-600:focus"


focusPlaceholderBlue700Focus : Html.Attribute msg
focusPlaceholderBlue700Focus =
    A.class "focus:placeholder-blue-700:focus"


focusPlaceholderBlue800Focus : Html.Attribute msg
focusPlaceholderBlue800Focus =
    A.class "focus:placeholder-blue-800:focus"


focusPlaceholderBlue900Focus : Html.Attribute msg
focusPlaceholderBlue900Focus =
    A.class "focus:placeholder-blue-900:focus"


focusPlaceholderIndigo100Focus : Html.Attribute msg
focusPlaceholderIndigo100Focus =
    A.class "focus:placeholder-indigo-100:focus"


focusPlaceholderIndigo200Focus : Html.Attribute msg
focusPlaceholderIndigo200Focus =
    A.class "focus:placeholder-indigo-200:focus"


focusPlaceholderIndigo300Focus : Html.Attribute msg
focusPlaceholderIndigo300Focus =
    A.class "focus:placeholder-indigo-300:focus"


focusPlaceholderIndigo400Focus : Html.Attribute msg
focusPlaceholderIndigo400Focus =
    A.class "focus:placeholder-indigo-400:focus"


focusPlaceholderIndigo500Focus : Html.Attribute msg
focusPlaceholderIndigo500Focus =
    A.class "focus:placeholder-indigo-500:focus"


focusPlaceholderIndigo600Focus : Html.Attribute msg
focusPlaceholderIndigo600Focus =
    A.class "focus:placeholder-indigo-600:focus"


focusPlaceholderIndigo700Focus : Html.Attribute msg
focusPlaceholderIndigo700Focus =
    A.class "focus:placeholder-indigo-700:focus"


focusPlaceholderIndigo800Focus : Html.Attribute msg
focusPlaceholderIndigo800Focus =
    A.class "focus:placeholder-indigo-800:focus"


focusPlaceholderIndigo900Focus : Html.Attribute msg
focusPlaceholderIndigo900Focus =
    A.class "focus:placeholder-indigo-900:focus"


focusPlaceholderPurple100Focus : Html.Attribute msg
focusPlaceholderPurple100Focus =
    A.class "focus:placeholder-purple-100:focus"


focusPlaceholderPurple200Focus : Html.Attribute msg
focusPlaceholderPurple200Focus =
    A.class "focus:placeholder-purple-200:focus"


focusPlaceholderPurple300Focus : Html.Attribute msg
focusPlaceholderPurple300Focus =
    A.class "focus:placeholder-purple-300:focus"


focusPlaceholderPurple400Focus : Html.Attribute msg
focusPlaceholderPurple400Focus =
    A.class "focus:placeholder-purple-400:focus"


focusPlaceholderPurple500Focus : Html.Attribute msg
focusPlaceholderPurple500Focus =
    A.class "focus:placeholder-purple-500:focus"


focusPlaceholderPurple600Focus : Html.Attribute msg
focusPlaceholderPurple600Focus =
    A.class "focus:placeholder-purple-600:focus"


focusPlaceholderPurple700Focus : Html.Attribute msg
focusPlaceholderPurple700Focus =
    A.class "focus:placeholder-purple-700:focus"


focusPlaceholderPurple800Focus : Html.Attribute msg
focusPlaceholderPurple800Focus =
    A.class "focus:placeholder-purple-800:focus"


focusPlaceholderPurple900Focus : Html.Attribute msg
focusPlaceholderPurple900Focus =
    A.class "focus:placeholder-purple-900:focus"


focusPlaceholderPink100Focus : Html.Attribute msg
focusPlaceholderPink100Focus =
    A.class "focus:placeholder-pink-100:focus"


focusPlaceholderPink200Focus : Html.Attribute msg
focusPlaceholderPink200Focus =
    A.class "focus:placeholder-pink-200:focus"


focusPlaceholderPink300Focus : Html.Attribute msg
focusPlaceholderPink300Focus =
    A.class "focus:placeholder-pink-300:focus"


focusPlaceholderPink400Focus : Html.Attribute msg
focusPlaceholderPink400Focus =
    A.class "focus:placeholder-pink-400:focus"


focusPlaceholderPink500Focus : Html.Attribute msg
focusPlaceholderPink500Focus =
    A.class "focus:placeholder-pink-500:focus"


focusPlaceholderPink600Focus : Html.Attribute msg
focusPlaceholderPink600Focus =
    A.class "focus:placeholder-pink-600:focus"


focusPlaceholderPink700Focus : Html.Attribute msg
focusPlaceholderPink700Focus =
    A.class "focus:placeholder-pink-700:focus"


focusPlaceholderPink800Focus : Html.Attribute msg
focusPlaceholderPink800Focus =
    A.class "focus:placeholder-pink-800:focus"


focusPlaceholderPink900Focus : Html.Attribute msg
focusPlaceholderPink900Focus =
    A.class "focus:placeholder-pink-900:focus"


placeholderOpacity0 : Html.Attribute msg
placeholderOpacity0 =
    A.class "placeholder-opacity-0"


placeholderOpacity25 : Html.Attribute msg
placeholderOpacity25 =
    A.class "placeholder-opacity-25"


placeholderOpacity50 : Html.Attribute msg
placeholderOpacity50 =
    A.class "placeholder-opacity-50"


placeholderOpacity75 : Html.Attribute msg
placeholderOpacity75 =
    A.class "placeholder-opacity-75"


placeholderOpacity100 : Html.Attribute msg
placeholderOpacity100 =
    A.class "placeholder-opacity-100"


focusPlaceholderOpacity0Focus : Html.Attribute msg
focusPlaceholderOpacity0Focus =
    A.class "focus:placeholder-opacity-0:focus"


focusPlaceholderOpacity25Focus : Html.Attribute msg
focusPlaceholderOpacity25Focus =
    A.class "focus:placeholder-opacity-25:focus"


focusPlaceholderOpacity50Focus : Html.Attribute msg
focusPlaceholderOpacity50Focus =
    A.class "focus:placeholder-opacity-50:focus"


focusPlaceholderOpacity75Focus : Html.Attribute msg
focusPlaceholderOpacity75Focus =
    A.class "focus:placeholder-opacity-75:focus"


focusPlaceholderOpacity100Focus : Html.Attribute msg
focusPlaceholderOpacity100Focus =
    A.class "focus:placeholder-opacity-100:focus"


pointerEventsNone : Html.Attribute msg
pointerEventsNone =
    A.class "pointer-events-none"


pointerEventsAuto : Html.Attribute msg
pointerEventsAuto =
    A.class "pointer-events-auto"


static : Html.Attribute msg
static =
    A.class "static"


fixed : Html.Attribute msg
fixed =
    A.class "fixed"


absolute : Html.Attribute msg
absolute =
    A.class "absolute"


relative : Html.Attribute msg
relative =
    A.class "relative"


sticky : Html.Attribute msg
sticky =
    A.class "sticky"


inset0 : Html.Attribute msg
inset0 =
    A.class "inset-0"


insetAuto : Html.Attribute msg
insetAuto =
    A.class "inset-auto"


insetY0 : Html.Attribute msg
insetY0 =
    A.class "inset-y-0"


insetX0 : Html.Attribute msg
insetX0 =
    A.class "inset-x-0"


insetYAuto : Html.Attribute msg
insetYAuto =
    A.class "inset-y-auto"


insetXAuto : Html.Attribute msg
insetXAuto =
    A.class "inset-x-auto"


top0 : Html.Attribute msg
top0 =
    A.class "top-0"


right0 : Html.Attribute msg
right0 =
    A.class "right-0"


bottom0 : Html.Attribute msg
bottom0 =
    A.class "bottom-0"


left0 : Html.Attribute msg
left0 =
    A.class "left-0"


topAuto : Html.Attribute msg
topAuto =
    A.class "top-auto"


rightAuto : Html.Attribute msg
rightAuto =
    A.class "right-auto"


bottomAuto : Html.Attribute msg
bottomAuto =
    A.class "bottom-auto"


leftAuto : Html.Attribute msg
leftAuto =
    A.class "left-auto"


resizeNone : Html.Attribute msg
resizeNone =
    A.class "resize-none"


resizeY : Html.Attribute msg
resizeY =
    A.class "resize-y"


resizeX : Html.Attribute msg
resizeX =
    A.class "resize-x"


resize : Html.Attribute msg
resize =
    A.class "resize"


shadowXs : Html.Attribute msg
shadowXs =
    A.class "shadow-xs"


shadowSm : Html.Attribute msg
shadowSm =
    A.class "shadow-sm"


shadow : Html.Attribute msg
shadow =
    A.class "shadow"


shadowMd : Html.Attribute msg
shadowMd =
    A.class "shadow-md"


shadowLg : Html.Attribute msg
shadowLg =
    A.class "shadow-lg"


shadowXl : Html.Attribute msg
shadowXl =
    A.class "shadow-xl"


shadow2xl : Html.Attribute msg
shadow2xl =
    A.class "shadow-2xl"


shadowInner : Html.Attribute msg
shadowInner =
    A.class "shadow-inner"


shadowOutline : Html.Attribute msg
shadowOutline =
    A.class "shadow-outline"


shadowNone : Html.Attribute msg
shadowNone =
    A.class "shadow-none"


hoverShadowXs : Html.Attribute msg
hoverShadowXs =
    A.class "hover:shadow-xs"


hoverShadowSm : Html.Attribute msg
hoverShadowSm =
    A.class "hover:shadow-sm"


hoverShadow : Html.Attribute msg
hoverShadow =
    A.class "hover:shadow"


hoverShadowMd : Html.Attribute msg
hoverShadowMd =
    A.class "hover:shadow-md"


hoverShadowLg : Html.Attribute msg
hoverShadowLg =
    A.class "hover:shadow-lg"


hoverShadowXl : Html.Attribute msg
hoverShadowXl =
    A.class "hover:shadow-xl"


hoverShadow2xl : Html.Attribute msg
hoverShadow2xl =
    A.class "hover:shadow-2xl"


hoverShadowInner : Html.Attribute msg
hoverShadowInner =
    A.class "hover:shadow-inner"


hoverShadowOutline : Html.Attribute msg
hoverShadowOutline =
    A.class "hover:shadow-outline"


hoverShadowNone : Html.Attribute msg
hoverShadowNone =
    A.class "hover:shadow-none"


focusShadowXs : Html.Attribute msg
focusShadowXs =
    A.class "focus:shadow-xs"


focusShadowSm : Html.Attribute msg
focusShadowSm =
    A.class "focus:shadow-sm"


focusShadow : Html.Attribute msg
focusShadow =
    A.class "focus:shadow"


focusShadowMd : Html.Attribute msg
focusShadowMd =
    A.class "focus:shadow-md"


focusShadowLg : Html.Attribute msg
focusShadowLg =
    A.class "focus:shadow-lg"


focusShadowXl : Html.Attribute msg
focusShadowXl =
    A.class "focus:shadow-xl"


focusShadow2xl : Html.Attribute msg
focusShadow2xl =
    A.class "focus:shadow-2xl"


focusShadowInner : Html.Attribute msg
focusShadowInner =
    A.class "focus:shadow-inner"


focusShadowOutline : Html.Attribute msg
focusShadowOutline =
    A.class "focus:shadow-outline"


focusShadowNone : Html.Attribute msg
focusShadowNone =
    A.class "focus:shadow-none"


fillCurrent : Html.Attribute msg
fillCurrent =
    A.class "fill-current"


strokeCurrent : Html.Attribute msg
strokeCurrent =
    A.class "stroke-current"


stroke0 : Html.Attribute msg
stroke0 =
    A.class "stroke-0"


stroke1 : Html.Attribute msg
stroke1 =
    A.class "stroke-1"


stroke2 : Html.Attribute msg
stroke2 =
    A.class "stroke-2"


tableAuto : Html.Attribute msg
tableAuto =
    A.class "table-auto"


tableFixed : Html.Attribute msg
tableFixed =
    A.class "table-fixed"


textLeft : Html.Attribute msg
textLeft =
    A.class "text-left"


textCenter : Html.Attribute msg
textCenter =
    A.class "text-center"


textRight : Html.Attribute msg
textRight =
    A.class "text-right"


textJustify : Html.Attribute msg
textJustify =
    A.class "text-justify"


textTransparent : Html.Attribute msg
textTransparent =
    A.class "text-transparent"


textCurrent : Html.Attribute msg
textCurrent =
    A.class "text-current"


textBlack : Html.Attribute msg
textBlack =
    A.class "text-black"


textWhite : Html.Attribute msg
textWhite =
    A.class "text-white"


textGray100 : Html.Attribute msg
textGray100 =
    A.class "text-gray-100"


textGray200 : Html.Attribute msg
textGray200 =
    A.class "text-gray-200"


textGray300 : Html.Attribute msg
textGray300 =
    A.class "text-gray-300"


textGray400 : Html.Attribute msg
textGray400 =
    A.class "text-gray-400"


textGray500 : Html.Attribute msg
textGray500 =
    A.class "text-gray-500"


textGray600 : Html.Attribute msg
textGray600 =
    A.class "text-gray-600"


textGray700 : Html.Attribute msg
textGray700 =
    A.class "text-gray-700"


textGray800 : Html.Attribute msg
textGray800 =
    A.class "text-gray-800"


textGray900 : Html.Attribute msg
textGray900 =
    A.class "text-gray-900"


textRed100 : Html.Attribute msg
textRed100 =
    A.class "text-red-100"


textRed200 : Html.Attribute msg
textRed200 =
    A.class "text-red-200"


textRed300 : Html.Attribute msg
textRed300 =
    A.class "text-red-300"


textRed400 : Html.Attribute msg
textRed400 =
    A.class "text-red-400"


textRed500 : Html.Attribute msg
textRed500 =
    A.class "text-red-500"


textRed600 : Html.Attribute msg
textRed600 =
    A.class "text-red-600"


textRed700 : Html.Attribute msg
textRed700 =
    A.class "text-red-700"


textRed800 : Html.Attribute msg
textRed800 =
    A.class "text-red-800"


textRed900 : Html.Attribute msg
textRed900 =
    A.class "text-red-900"


textOrange100 : Html.Attribute msg
textOrange100 =
    A.class "text-orange-100"


textOrange200 : Html.Attribute msg
textOrange200 =
    A.class "text-orange-200"


textOrange300 : Html.Attribute msg
textOrange300 =
    A.class "text-orange-300"


textOrange400 : Html.Attribute msg
textOrange400 =
    A.class "text-orange-400"


textOrange500 : Html.Attribute msg
textOrange500 =
    A.class "text-orange-500"


textOrange600 : Html.Attribute msg
textOrange600 =
    A.class "text-orange-600"


textOrange700 : Html.Attribute msg
textOrange700 =
    A.class "text-orange-700"


textOrange800 : Html.Attribute msg
textOrange800 =
    A.class "text-orange-800"


textOrange900 : Html.Attribute msg
textOrange900 =
    A.class "text-orange-900"


textYellow100 : Html.Attribute msg
textYellow100 =
    A.class "text-yellow-100"


textYellow200 : Html.Attribute msg
textYellow200 =
    A.class "text-yellow-200"


textYellow300 : Html.Attribute msg
textYellow300 =
    A.class "text-yellow-300"


textYellow400 : Html.Attribute msg
textYellow400 =
    A.class "text-yellow-400"


textYellow500 : Html.Attribute msg
textYellow500 =
    A.class "text-yellow-500"


textYellow600 : Html.Attribute msg
textYellow600 =
    A.class "text-yellow-600"


textYellow700 : Html.Attribute msg
textYellow700 =
    A.class "text-yellow-700"


textYellow800 : Html.Attribute msg
textYellow800 =
    A.class "text-yellow-800"


textYellow900 : Html.Attribute msg
textYellow900 =
    A.class "text-yellow-900"


textGreen100 : Html.Attribute msg
textGreen100 =
    A.class "text-green-100"


textGreen200 : Html.Attribute msg
textGreen200 =
    A.class "text-green-200"


textGreen300 : Html.Attribute msg
textGreen300 =
    A.class "text-green-300"


textGreen400 : Html.Attribute msg
textGreen400 =
    A.class "text-green-400"


textGreen500 : Html.Attribute msg
textGreen500 =
    A.class "text-green-500"


textGreen600 : Html.Attribute msg
textGreen600 =
    A.class "text-green-600"


textGreen700 : Html.Attribute msg
textGreen700 =
    A.class "text-green-700"


textGreen800 : Html.Attribute msg
textGreen800 =
    A.class "text-green-800"


textGreen900 : Html.Attribute msg
textGreen900 =
    A.class "text-green-900"


textTeal100 : Html.Attribute msg
textTeal100 =
    A.class "text-teal-100"


textTeal200 : Html.Attribute msg
textTeal200 =
    A.class "text-teal-200"


textTeal300 : Html.Attribute msg
textTeal300 =
    A.class "text-teal-300"


textTeal400 : Html.Attribute msg
textTeal400 =
    A.class "text-teal-400"


textTeal500 : Html.Attribute msg
textTeal500 =
    A.class "text-teal-500"


textTeal600 : Html.Attribute msg
textTeal600 =
    A.class "text-teal-600"


textTeal700 : Html.Attribute msg
textTeal700 =
    A.class "text-teal-700"


textTeal800 : Html.Attribute msg
textTeal800 =
    A.class "text-teal-800"


textTeal900 : Html.Attribute msg
textTeal900 =
    A.class "text-teal-900"


textBlue100 : Html.Attribute msg
textBlue100 =
    A.class "text-blue-100"


textBlue200 : Html.Attribute msg
textBlue200 =
    A.class "text-blue-200"


textBlue300 : Html.Attribute msg
textBlue300 =
    A.class "text-blue-300"


textBlue400 : Html.Attribute msg
textBlue400 =
    A.class "text-blue-400"


textBlue500 : Html.Attribute msg
textBlue500 =
    A.class "text-blue-500"


textBlue600 : Html.Attribute msg
textBlue600 =
    A.class "text-blue-600"


textBlue700 : Html.Attribute msg
textBlue700 =
    A.class "text-blue-700"


textBlue800 : Html.Attribute msg
textBlue800 =
    A.class "text-blue-800"


textBlue900 : Html.Attribute msg
textBlue900 =
    A.class "text-blue-900"


textIndigo100 : Html.Attribute msg
textIndigo100 =
    A.class "text-indigo-100"


textIndigo200 : Html.Attribute msg
textIndigo200 =
    A.class "text-indigo-200"


textIndigo300 : Html.Attribute msg
textIndigo300 =
    A.class "text-indigo-300"


textIndigo400 : Html.Attribute msg
textIndigo400 =
    A.class "text-indigo-400"


textIndigo500 : Html.Attribute msg
textIndigo500 =
    A.class "text-indigo-500"


textIndigo600 : Html.Attribute msg
textIndigo600 =
    A.class "text-indigo-600"


textIndigo700 : Html.Attribute msg
textIndigo700 =
    A.class "text-indigo-700"


textIndigo800 : Html.Attribute msg
textIndigo800 =
    A.class "text-indigo-800"


textIndigo900 : Html.Attribute msg
textIndigo900 =
    A.class "text-indigo-900"


textPurple100 : Html.Attribute msg
textPurple100 =
    A.class "text-purple-100"


textPurple200 : Html.Attribute msg
textPurple200 =
    A.class "text-purple-200"


textPurple300 : Html.Attribute msg
textPurple300 =
    A.class "text-purple-300"


textPurple400 : Html.Attribute msg
textPurple400 =
    A.class "text-purple-400"


textPurple500 : Html.Attribute msg
textPurple500 =
    A.class "text-purple-500"


textPurple600 : Html.Attribute msg
textPurple600 =
    A.class "text-purple-600"


textPurple700 : Html.Attribute msg
textPurple700 =
    A.class "text-purple-700"


textPurple800 : Html.Attribute msg
textPurple800 =
    A.class "text-purple-800"


textPurple900 : Html.Attribute msg
textPurple900 =
    A.class "text-purple-900"


textPink100 : Html.Attribute msg
textPink100 =
    A.class "text-pink-100"


textPink200 : Html.Attribute msg
textPink200 =
    A.class "text-pink-200"


textPink300 : Html.Attribute msg
textPink300 =
    A.class "text-pink-300"


textPink400 : Html.Attribute msg
textPink400 =
    A.class "text-pink-400"


textPink500 : Html.Attribute msg
textPink500 =
    A.class "text-pink-500"


textPink600 : Html.Attribute msg
textPink600 =
    A.class "text-pink-600"


textPink700 : Html.Attribute msg
textPink700 =
    A.class "text-pink-700"


textPink800 : Html.Attribute msg
textPink800 =
    A.class "text-pink-800"


textPink900 : Html.Attribute msg
textPink900 =
    A.class "text-pink-900"


hoverTextTransparent : Html.Attribute msg
hoverTextTransparent =
    A.class "hover:text-transparent"


hoverTextCurrent : Html.Attribute msg
hoverTextCurrent =
    A.class "hover:text-current"


hoverTextBlack : Html.Attribute msg
hoverTextBlack =
    A.class "hover:text-black"


hoverTextWhite : Html.Attribute msg
hoverTextWhite =
    A.class "hover:text-white"


hoverTextGray100 : Html.Attribute msg
hoverTextGray100 =
    A.class "hover:text-gray-100"


hoverTextGray200 : Html.Attribute msg
hoverTextGray200 =
    A.class "hover:text-gray-200"


hoverTextGray300 : Html.Attribute msg
hoverTextGray300 =
    A.class "hover:text-gray-300"


hoverTextGray400 : Html.Attribute msg
hoverTextGray400 =
    A.class "hover:text-gray-400"


hoverTextGray500 : Html.Attribute msg
hoverTextGray500 =
    A.class "hover:text-gray-500"


hoverTextGray600 : Html.Attribute msg
hoverTextGray600 =
    A.class "hover:text-gray-600"


hoverTextGray700 : Html.Attribute msg
hoverTextGray700 =
    A.class "hover:text-gray-700"


hoverTextGray800 : Html.Attribute msg
hoverTextGray800 =
    A.class "hover:text-gray-800"


hoverTextGray900 : Html.Attribute msg
hoverTextGray900 =
    A.class "hover:text-gray-900"


hoverTextRed100 : Html.Attribute msg
hoverTextRed100 =
    A.class "hover:text-red-100"


hoverTextRed200 : Html.Attribute msg
hoverTextRed200 =
    A.class "hover:text-red-200"


hoverTextRed300 : Html.Attribute msg
hoverTextRed300 =
    A.class "hover:text-red-300"


hoverTextRed400 : Html.Attribute msg
hoverTextRed400 =
    A.class "hover:text-red-400"


hoverTextRed500 : Html.Attribute msg
hoverTextRed500 =
    A.class "hover:text-red-500"


hoverTextRed600 : Html.Attribute msg
hoverTextRed600 =
    A.class "hover:text-red-600"


hoverTextRed700 : Html.Attribute msg
hoverTextRed700 =
    A.class "hover:text-red-700"


hoverTextRed800 : Html.Attribute msg
hoverTextRed800 =
    A.class "hover:text-red-800"


hoverTextRed900 : Html.Attribute msg
hoverTextRed900 =
    A.class "hover:text-red-900"


hoverTextOrange100 : Html.Attribute msg
hoverTextOrange100 =
    A.class "hover:text-orange-100"


hoverTextOrange200 : Html.Attribute msg
hoverTextOrange200 =
    A.class "hover:text-orange-200"


hoverTextOrange300 : Html.Attribute msg
hoverTextOrange300 =
    A.class "hover:text-orange-300"


hoverTextOrange400 : Html.Attribute msg
hoverTextOrange400 =
    A.class "hover:text-orange-400"


hoverTextOrange500 : Html.Attribute msg
hoverTextOrange500 =
    A.class "hover:text-orange-500"


hoverTextOrange600 : Html.Attribute msg
hoverTextOrange600 =
    A.class "hover:text-orange-600"


hoverTextOrange700 : Html.Attribute msg
hoverTextOrange700 =
    A.class "hover:text-orange-700"


hoverTextOrange800 : Html.Attribute msg
hoverTextOrange800 =
    A.class "hover:text-orange-800"


hoverTextOrange900 : Html.Attribute msg
hoverTextOrange900 =
    A.class "hover:text-orange-900"


hoverTextYellow100 : Html.Attribute msg
hoverTextYellow100 =
    A.class "hover:text-yellow-100"


hoverTextYellow200 : Html.Attribute msg
hoverTextYellow200 =
    A.class "hover:text-yellow-200"


hoverTextYellow300 : Html.Attribute msg
hoverTextYellow300 =
    A.class "hover:text-yellow-300"


hoverTextYellow400 : Html.Attribute msg
hoverTextYellow400 =
    A.class "hover:text-yellow-400"


hoverTextYellow500 : Html.Attribute msg
hoverTextYellow500 =
    A.class "hover:text-yellow-500"


hoverTextYellow600 : Html.Attribute msg
hoverTextYellow600 =
    A.class "hover:text-yellow-600"


hoverTextYellow700 : Html.Attribute msg
hoverTextYellow700 =
    A.class "hover:text-yellow-700"


hoverTextYellow800 : Html.Attribute msg
hoverTextYellow800 =
    A.class "hover:text-yellow-800"


hoverTextYellow900 : Html.Attribute msg
hoverTextYellow900 =
    A.class "hover:text-yellow-900"


hoverTextGreen100 : Html.Attribute msg
hoverTextGreen100 =
    A.class "hover:text-green-100"


hoverTextGreen200 : Html.Attribute msg
hoverTextGreen200 =
    A.class "hover:text-green-200"


hoverTextGreen300 : Html.Attribute msg
hoverTextGreen300 =
    A.class "hover:text-green-300"


hoverTextGreen400 : Html.Attribute msg
hoverTextGreen400 =
    A.class "hover:text-green-400"


hoverTextGreen500 : Html.Attribute msg
hoverTextGreen500 =
    A.class "hover:text-green-500"


hoverTextGreen600 : Html.Attribute msg
hoverTextGreen600 =
    A.class "hover:text-green-600"


hoverTextGreen700 : Html.Attribute msg
hoverTextGreen700 =
    A.class "hover:text-green-700"


hoverTextGreen800 : Html.Attribute msg
hoverTextGreen800 =
    A.class "hover:text-green-800"


hoverTextGreen900 : Html.Attribute msg
hoverTextGreen900 =
    A.class "hover:text-green-900"


hoverTextTeal100 : Html.Attribute msg
hoverTextTeal100 =
    A.class "hover:text-teal-100"


hoverTextTeal200 : Html.Attribute msg
hoverTextTeal200 =
    A.class "hover:text-teal-200"


hoverTextTeal300 : Html.Attribute msg
hoverTextTeal300 =
    A.class "hover:text-teal-300"


hoverTextTeal400 : Html.Attribute msg
hoverTextTeal400 =
    A.class "hover:text-teal-400"


hoverTextTeal500 : Html.Attribute msg
hoverTextTeal500 =
    A.class "hover:text-teal-500"


hoverTextTeal600 : Html.Attribute msg
hoverTextTeal600 =
    A.class "hover:text-teal-600"


hoverTextTeal700 : Html.Attribute msg
hoverTextTeal700 =
    A.class "hover:text-teal-700"


hoverTextTeal800 : Html.Attribute msg
hoverTextTeal800 =
    A.class "hover:text-teal-800"


hoverTextTeal900 : Html.Attribute msg
hoverTextTeal900 =
    A.class "hover:text-teal-900"


hoverTextBlue100 : Html.Attribute msg
hoverTextBlue100 =
    A.class "hover:text-blue-100"


hoverTextBlue200 : Html.Attribute msg
hoverTextBlue200 =
    A.class "hover:text-blue-200"


hoverTextBlue300 : Html.Attribute msg
hoverTextBlue300 =
    A.class "hover:text-blue-300"


hoverTextBlue400 : Html.Attribute msg
hoverTextBlue400 =
    A.class "hover:text-blue-400"


hoverTextBlue500 : Html.Attribute msg
hoverTextBlue500 =
    A.class "hover:text-blue-500"


hoverTextBlue600 : Html.Attribute msg
hoverTextBlue600 =
    A.class "hover:text-blue-600"


hoverTextBlue700 : Html.Attribute msg
hoverTextBlue700 =
    A.class "hover:text-blue-700"


hoverTextBlue800 : Html.Attribute msg
hoverTextBlue800 =
    A.class "hover:text-blue-800"


hoverTextBlue900 : Html.Attribute msg
hoverTextBlue900 =
    A.class "hover:text-blue-900"


hoverTextIndigo100 : Html.Attribute msg
hoverTextIndigo100 =
    A.class "hover:text-indigo-100"


hoverTextIndigo200 : Html.Attribute msg
hoverTextIndigo200 =
    A.class "hover:text-indigo-200"


hoverTextIndigo300 : Html.Attribute msg
hoverTextIndigo300 =
    A.class "hover:text-indigo-300"


hoverTextIndigo400 : Html.Attribute msg
hoverTextIndigo400 =
    A.class "hover:text-indigo-400"


hoverTextIndigo500 : Html.Attribute msg
hoverTextIndigo500 =
    A.class "hover:text-indigo-500"


hoverTextIndigo600 : Html.Attribute msg
hoverTextIndigo600 =
    A.class "hover:text-indigo-600"


hoverTextIndigo700 : Html.Attribute msg
hoverTextIndigo700 =
    A.class "hover:text-indigo-700"


hoverTextIndigo800 : Html.Attribute msg
hoverTextIndigo800 =
    A.class "hover:text-indigo-800"


hoverTextIndigo900 : Html.Attribute msg
hoverTextIndigo900 =
    A.class "hover:text-indigo-900"


hoverTextPurple100 : Html.Attribute msg
hoverTextPurple100 =
    A.class "hover:text-purple-100"


hoverTextPurple200 : Html.Attribute msg
hoverTextPurple200 =
    A.class "hover:text-purple-200"


hoverTextPurple300 : Html.Attribute msg
hoverTextPurple300 =
    A.class "hover:text-purple-300"


hoverTextPurple400 : Html.Attribute msg
hoverTextPurple400 =
    A.class "hover:text-purple-400"


hoverTextPurple500 : Html.Attribute msg
hoverTextPurple500 =
    A.class "hover:text-purple-500"


hoverTextPurple600 : Html.Attribute msg
hoverTextPurple600 =
    A.class "hover:text-purple-600"


hoverTextPurple700 : Html.Attribute msg
hoverTextPurple700 =
    A.class "hover:text-purple-700"


hoverTextPurple800 : Html.Attribute msg
hoverTextPurple800 =
    A.class "hover:text-purple-800"


hoverTextPurple900 : Html.Attribute msg
hoverTextPurple900 =
    A.class "hover:text-purple-900"


hoverTextPink100 : Html.Attribute msg
hoverTextPink100 =
    A.class "hover:text-pink-100"


hoverTextPink200 : Html.Attribute msg
hoverTextPink200 =
    A.class "hover:text-pink-200"


hoverTextPink300 : Html.Attribute msg
hoverTextPink300 =
    A.class "hover:text-pink-300"


hoverTextPink400 : Html.Attribute msg
hoverTextPink400 =
    A.class "hover:text-pink-400"


hoverTextPink500 : Html.Attribute msg
hoverTextPink500 =
    A.class "hover:text-pink-500"


hoverTextPink600 : Html.Attribute msg
hoverTextPink600 =
    A.class "hover:text-pink-600"


hoverTextPink700 : Html.Attribute msg
hoverTextPink700 =
    A.class "hover:text-pink-700"


hoverTextPink800 : Html.Attribute msg
hoverTextPink800 =
    A.class "hover:text-pink-800"


hoverTextPink900 : Html.Attribute msg
hoverTextPink900 =
    A.class "hover:text-pink-900"


focusTextTransparent : Html.Attribute msg
focusTextTransparent =
    A.class "focus:text-transparent"


focusTextCurrent : Html.Attribute msg
focusTextCurrent =
    A.class "focus:text-current"


focusTextBlack : Html.Attribute msg
focusTextBlack =
    A.class "focus:text-black"


focusTextWhite : Html.Attribute msg
focusTextWhite =
    A.class "focus:text-white"


focusTextGray100 : Html.Attribute msg
focusTextGray100 =
    A.class "focus:text-gray-100"


focusTextGray200 : Html.Attribute msg
focusTextGray200 =
    A.class "focus:text-gray-200"


focusTextGray300 : Html.Attribute msg
focusTextGray300 =
    A.class "focus:text-gray-300"


focusTextGray400 : Html.Attribute msg
focusTextGray400 =
    A.class "focus:text-gray-400"


focusTextGray500 : Html.Attribute msg
focusTextGray500 =
    A.class "focus:text-gray-500"


focusTextGray600 : Html.Attribute msg
focusTextGray600 =
    A.class "focus:text-gray-600"


focusTextGray700 : Html.Attribute msg
focusTextGray700 =
    A.class "focus:text-gray-700"


focusTextGray800 : Html.Attribute msg
focusTextGray800 =
    A.class "focus:text-gray-800"


focusTextGray900 : Html.Attribute msg
focusTextGray900 =
    A.class "focus:text-gray-900"


focusTextRed100 : Html.Attribute msg
focusTextRed100 =
    A.class "focus:text-red-100"


focusTextRed200 : Html.Attribute msg
focusTextRed200 =
    A.class "focus:text-red-200"


focusTextRed300 : Html.Attribute msg
focusTextRed300 =
    A.class "focus:text-red-300"


focusTextRed400 : Html.Attribute msg
focusTextRed400 =
    A.class "focus:text-red-400"


focusTextRed500 : Html.Attribute msg
focusTextRed500 =
    A.class "focus:text-red-500"


focusTextRed600 : Html.Attribute msg
focusTextRed600 =
    A.class "focus:text-red-600"


focusTextRed700 : Html.Attribute msg
focusTextRed700 =
    A.class "focus:text-red-700"


focusTextRed800 : Html.Attribute msg
focusTextRed800 =
    A.class "focus:text-red-800"


focusTextRed900 : Html.Attribute msg
focusTextRed900 =
    A.class "focus:text-red-900"


focusTextOrange100 : Html.Attribute msg
focusTextOrange100 =
    A.class "focus:text-orange-100"


focusTextOrange200 : Html.Attribute msg
focusTextOrange200 =
    A.class "focus:text-orange-200"


focusTextOrange300 : Html.Attribute msg
focusTextOrange300 =
    A.class "focus:text-orange-300"


focusTextOrange400 : Html.Attribute msg
focusTextOrange400 =
    A.class "focus:text-orange-400"


focusTextOrange500 : Html.Attribute msg
focusTextOrange500 =
    A.class "focus:text-orange-500"


focusTextOrange600 : Html.Attribute msg
focusTextOrange600 =
    A.class "focus:text-orange-600"


focusTextOrange700 : Html.Attribute msg
focusTextOrange700 =
    A.class "focus:text-orange-700"


focusTextOrange800 : Html.Attribute msg
focusTextOrange800 =
    A.class "focus:text-orange-800"


focusTextOrange900 : Html.Attribute msg
focusTextOrange900 =
    A.class "focus:text-orange-900"


focusTextYellow100 : Html.Attribute msg
focusTextYellow100 =
    A.class "focus:text-yellow-100"


focusTextYellow200 : Html.Attribute msg
focusTextYellow200 =
    A.class "focus:text-yellow-200"


focusTextYellow300 : Html.Attribute msg
focusTextYellow300 =
    A.class "focus:text-yellow-300"


focusTextYellow400 : Html.Attribute msg
focusTextYellow400 =
    A.class "focus:text-yellow-400"


focusTextYellow500 : Html.Attribute msg
focusTextYellow500 =
    A.class "focus:text-yellow-500"


focusTextYellow600 : Html.Attribute msg
focusTextYellow600 =
    A.class "focus:text-yellow-600"


focusTextYellow700 : Html.Attribute msg
focusTextYellow700 =
    A.class "focus:text-yellow-700"


focusTextYellow800 : Html.Attribute msg
focusTextYellow800 =
    A.class "focus:text-yellow-800"


focusTextYellow900 : Html.Attribute msg
focusTextYellow900 =
    A.class "focus:text-yellow-900"


focusTextGreen100 : Html.Attribute msg
focusTextGreen100 =
    A.class "focus:text-green-100"


focusTextGreen200 : Html.Attribute msg
focusTextGreen200 =
    A.class "focus:text-green-200"


focusTextGreen300 : Html.Attribute msg
focusTextGreen300 =
    A.class "focus:text-green-300"


focusTextGreen400 : Html.Attribute msg
focusTextGreen400 =
    A.class "focus:text-green-400"


focusTextGreen500 : Html.Attribute msg
focusTextGreen500 =
    A.class "focus:text-green-500"


focusTextGreen600 : Html.Attribute msg
focusTextGreen600 =
    A.class "focus:text-green-600"


focusTextGreen700 : Html.Attribute msg
focusTextGreen700 =
    A.class "focus:text-green-700"


focusTextGreen800 : Html.Attribute msg
focusTextGreen800 =
    A.class "focus:text-green-800"


focusTextGreen900 : Html.Attribute msg
focusTextGreen900 =
    A.class "focus:text-green-900"


focusTextTeal100 : Html.Attribute msg
focusTextTeal100 =
    A.class "focus:text-teal-100"


focusTextTeal200 : Html.Attribute msg
focusTextTeal200 =
    A.class "focus:text-teal-200"


focusTextTeal300 : Html.Attribute msg
focusTextTeal300 =
    A.class "focus:text-teal-300"


focusTextTeal400 : Html.Attribute msg
focusTextTeal400 =
    A.class "focus:text-teal-400"


focusTextTeal500 : Html.Attribute msg
focusTextTeal500 =
    A.class "focus:text-teal-500"


focusTextTeal600 : Html.Attribute msg
focusTextTeal600 =
    A.class "focus:text-teal-600"


focusTextTeal700 : Html.Attribute msg
focusTextTeal700 =
    A.class "focus:text-teal-700"


focusTextTeal800 : Html.Attribute msg
focusTextTeal800 =
    A.class "focus:text-teal-800"


focusTextTeal900 : Html.Attribute msg
focusTextTeal900 =
    A.class "focus:text-teal-900"


focusTextBlue100 : Html.Attribute msg
focusTextBlue100 =
    A.class "focus:text-blue-100"


focusTextBlue200 : Html.Attribute msg
focusTextBlue200 =
    A.class "focus:text-blue-200"


focusTextBlue300 : Html.Attribute msg
focusTextBlue300 =
    A.class "focus:text-blue-300"


focusTextBlue400 : Html.Attribute msg
focusTextBlue400 =
    A.class "focus:text-blue-400"


focusTextBlue500 : Html.Attribute msg
focusTextBlue500 =
    A.class "focus:text-blue-500"


focusTextBlue600 : Html.Attribute msg
focusTextBlue600 =
    A.class "focus:text-blue-600"


focusTextBlue700 : Html.Attribute msg
focusTextBlue700 =
    A.class "focus:text-blue-700"


focusTextBlue800 : Html.Attribute msg
focusTextBlue800 =
    A.class "focus:text-blue-800"


focusTextBlue900 : Html.Attribute msg
focusTextBlue900 =
    A.class "focus:text-blue-900"


focusTextIndigo100 : Html.Attribute msg
focusTextIndigo100 =
    A.class "focus:text-indigo-100"


focusTextIndigo200 : Html.Attribute msg
focusTextIndigo200 =
    A.class "focus:text-indigo-200"


focusTextIndigo300 : Html.Attribute msg
focusTextIndigo300 =
    A.class "focus:text-indigo-300"


focusTextIndigo400 : Html.Attribute msg
focusTextIndigo400 =
    A.class "focus:text-indigo-400"


focusTextIndigo500 : Html.Attribute msg
focusTextIndigo500 =
    A.class "focus:text-indigo-500"


focusTextIndigo600 : Html.Attribute msg
focusTextIndigo600 =
    A.class "focus:text-indigo-600"


focusTextIndigo700 : Html.Attribute msg
focusTextIndigo700 =
    A.class "focus:text-indigo-700"


focusTextIndigo800 : Html.Attribute msg
focusTextIndigo800 =
    A.class "focus:text-indigo-800"


focusTextIndigo900 : Html.Attribute msg
focusTextIndigo900 =
    A.class "focus:text-indigo-900"


focusTextPurple100 : Html.Attribute msg
focusTextPurple100 =
    A.class "focus:text-purple-100"


focusTextPurple200 : Html.Attribute msg
focusTextPurple200 =
    A.class "focus:text-purple-200"


focusTextPurple300 : Html.Attribute msg
focusTextPurple300 =
    A.class "focus:text-purple-300"


focusTextPurple400 : Html.Attribute msg
focusTextPurple400 =
    A.class "focus:text-purple-400"


focusTextPurple500 : Html.Attribute msg
focusTextPurple500 =
    A.class "focus:text-purple-500"


focusTextPurple600 : Html.Attribute msg
focusTextPurple600 =
    A.class "focus:text-purple-600"


focusTextPurple700 : Html.Attribute msg
focusTextPurple700 =
    A.class "focus:text-purple-700"


focusTextPurple800 : Html.Attribute msg
focusTextPurple800 =
    A.class "focus:text-purple-800"


focusTextPurple900 : Html.Attribute msg
focusTextPurple900 =
    A.class "focus:text-purple-900"


focusTextPink100 : Html.Attribute msg
focusTextPink100 =
    A.class "focus:text-pink-100"


focusTextPink200 : Html.Attribute msg
focusTextPink200 =
    A.class "focus:text-pink-200"


focusTextPink300 : Html.Attribute msg
focusTextPink300 =
    A.class "focus:text-pink-300"


focusTextPink400 : Html.Attribute msg
focusTextPink400 =
    A.class "focus:text-pink-400"


focusTextPink500 : Html.Attribute msg
focusTextPink500 =
    A.class "focus:text-pink-500"


focusTextPink600 : Html.Attribute msg
focusTextPink600 =
    A.class "focus:text-pink-600"


focusTextPink700 : Html.Attribute msg
focusTextPink700 =
    A.class "focus:text-pink-700"


focusTextPink800 : Html.Attribute msg
focusTextPink800 =
    A.class "focus:text-pink-800"


focusTextPink900 : Html.Attribute msg
focusTextPink900 =
    A.class "focus:text-pink-900"


textOpacity0 : Html.Attribute msg
textOpacity0 =
    A.class "text-opacity-0"


textOpacity25 : Html.Attribute msg
textOpacity25 =
    A.class "text-opacity-25"


textOpacity50 : Html.Attribute msg
textOpacity50 =
    A.class "text-opacity-50"


textOpacity75 : Html.Attribute msg
textOpacity75 =
    A.class "text-opacity-75"


textOpacity100 : Html.Attribute msg
textOpacity100 =
    A.class "text-opacity-100"


hoverTextOpacity0 : Html.Attribute msg
hoverTextOpacity0 =
    A.class "hover:text-opacity-0"


hoverTextOpacity25 : Html.Attribute msg
hoverTextOpacity25 =
    A.class "hover:text-opacity-25"


hoverTextOpacity50 : Html.Attribute msg
hoverTextOpacity50 =
    A.class "hover:text-opacity-50"


hoverTextOpacity75 : Html.Attribute msg
hoverTextOpacity75 =
    A.class "hover:text-opacity-75"


hoverTextOpacity100 : Html.Attribute msg
hoverTextOpacity100 =
    A.class "hover:text-opacity-100"


focusTextOpacity0 : Html.Attribute msg
focusTextOpacity0 =
    A.class "focus:text-opacity-0"


focusTextOpacity25 : Html.Attribute msg
focusTextOpacity25 =
    A.class "focus:text-opacity-25"


focusTextOpacity50 : Html.Attribute msg
focusTextOpacity50 =
    A.class "focus:text-opacity-50"


focusTextOpacity75 : Html.Attribute msg
focusTextOpacity75 =
    A.class "focus:text-opacity-75"


focusTextOpacity100 : Html.Attribute msg
focusTextOpacity100 =
    A.class "focus:text-opacity-100"


italic : Html.Attribute msg
italic =
    A.class "italic"


notItalic : Html.Attribute msg
notItalic =
    A.class "not-italic"


uppercase : Html.Attribute msg
uppercase =
    A.class "uppercase"


lowercase : Html.Attribute msg
lowercase =
    A.class "lowercase"


capitalize : Html.Attribute msg
capitalize =
    A.class "capitalize"


normalCase : Html.Attribute msg
normalCase =
    A.class "normal-case"


underline : Html.Attribute msg
underline =
    A.class "underline"


lineThrough : Html.Attribute msg
lineThrough =
    A.class "line-through"


noUnderline : Html.Attribute msg
noUnderline =
    A.class "no-underline"


hoverUnderline : Html.Attribute msg
hoverUnderline =
    A.class "hover:underline"


hoverLineThrough : Html.Attribute msg
hoverLineThrough =
    A.class "hover:line-through"


hoverNoUnderline : Html.Attribute msg
hoverNoUnderline =
    A.class "hover:no-underline"


focusUnderline : Html.Attribute msg
focusUnderline =
    A.class "focus:underline"


focusLineThrough : Html.Attribute msg
focusLineThrough =
    A.class "focus:line-through"


focusNoUnderline : Html.Attribute msg
focusNoUnderline =
    A.class "focus:no-underline"


antialiased : Html.Attribute msg
antialiased =
    A.class "antialiased"


subpixelAntialiased : Html.Attribute msg
subpixelAntialiased =
    A.class "subpixel-antialiased"


ordinal : Html.Attribute msg
ordinal =
    A.class "ordinal"


slashedZero : Html.Attribute msg
slashedZero =
    A.class "slashed-zero"


liningNums : Html.Attribute msg
liningNums =
    A.class "lining-nums"


oldstyleNums : Html.Attribute msg
oldstyleNums =
    A.class "oldstyle-nums"


proportionalNums : Html.Attribute msg
proportionalNums =
    A.class "proportional-nums"


tabularNums : Html.Attribute msg
tabularNums =
    A.class "tabular-nums"


diagonalFractions : Html.Attribute msg
diagonalFractions =
    A.class "diagonal-fractions"


stackedFractions : Html.Attribute msg
stackedFractions =
    A.class "stacked-fractions"


normalNums : Html.Attribute msg
normalNums =
    A.class "normal-nums"


trackingTighter : Html.Attribute msg
trackingTighter =
    A.class "tracking-tighter"


trackingTight : Html.Attribute msg
trackingTight =
    A.class "tracking-tight"


trackingNormal : Html.Attribute msg
trackingNormal =
    A.class "tracking-normal"


trackingWide : Html.Attribute msg
trackingWide =
    A.class "tracking-wide"


trackingWider : Html.Attribute msg
trackingWider =
    A.class "tracking-wider"


trackingWidest : Html.Attribute msg
trackingWidest =
    A.class "tracking-widest"


selectNone : Html.Attribute msg
selectNone =
    A.class "select-none"


selectText : Html.Attribute msg
selectText =
    A.class "select-text"


selectAll : Html.Attribute msg
selectAll =
    A.class "select-all"


selectAuto : Html.Attribute msg
selectAuto =
    A.class "select-auto"


alignBaseline : Html.Attribute msg
alignBaseline =
    A.class "align-baseline"


alignTop : Html.Attribute msg
alignTop =
    A.class "align-top"


alignMiddle : Html.Attribute msg
alignMiddle =
    A.class "align-middle"


alignBottom : Html.Attribute msg
alignBottom =
    A.class "align-bottom"


alignTextTop : Html.Attribute msg
alignTextTop =
    A.class "align-text-top"


alignTextBottom : Html.Attribute msg
alignTextBottom =
    A.class "align-text-bottom"


visible : Html.Attribute msg
visible =
    A.class "visible"


invisible : Html.Attribute msg
invisible =
    A.class "invisible"


whitespaceNormal : Html.Attribute msg
whitespaceNormal =
    A.class "whitespace-normal"


whitespaceNoWrap : Html.Attribute msg
whitespaceNoWrap =
    A.class "whitespace-no-wrap"


whitespacePre : Html.Attribute msg
whitespacePre =
    A.class "whitespace-pre"


whitespacePreLine : Html.Attribute msg
whitespacePreLine =
    A.class "whitespace-pre-line"


whitespacePreWrap : Html.Attribute msg
whitespacePreWrap =
    A.class "whitespace-pre-wrap"


breakNormal : Html.Attribute msg
breakNormal =
    A.class "break-normal"


breakWords : Html.Attribute msg
breakWords =
    A.class "break-words"


breakAll : Html.Attribute msg
breakAll =
    A.class "break-all"


truncate : Html.Attribute msg
truncate =
    A.class "truncate"


w0 : Html.Attribute msg
w0 =
    A.class "w-0"


w1 : Html.Attribute msg
w1 =
    A.class "w-1"


w2 : Html.Attribute msg
w2 =
    A.class "w-2"


w3 : Html.Attribute msg
w3 =
    A.class "w-3"


w4 : Html.Attribute msg
w4 =
    A.class "w-4"


w5 : Html.Attribute msg
w5 =
    A.class "w-5"


w6 : Html.Attribute msg
w6 =
    A.class "w-6"


w8 : Html.Attribute msg
w8 =
    A.class "w-8"


w10 : Html.Attribute msg
w10 =
    A.class "w-10"


w12 : Html.Attribute msg
w12 =
    A.class "w-12"


w16 : Html.Attribute msg
w16 =
    A.class "w-16"


w20 : Html.Attribute msg
w20 =
    A.class "w-20"


w24 : Html.Attribute msg
w24 =
    A.class "w-24"


w32 : Html.Attribute msg
w32 =
    A.class "w-32"


w40 : Html.Attribute msg
w40 =
    A.class "w-40"


w48 : Html.Attribute msg
w48 =
    A.class "w-48"


w56 : Html.Attribute msg
w56 =
    A.class "w-56"


w64 : Html.Attribute msg
w64 =
    A.class "w-64"


wAuto : Html.Attribute msg
wAuto =
    A.class "w-auto"


wPx : Html.Attribute msg
wPx =
    A.class "w-px"


w1over2 : Html.Attribute msg
w1over2 =
    A.class "w-1/2"


w1over3 : Html.Attribute msg
w1over3 =
    A.class "w-1/3"


w2over3 : Html.Attribute msg
w2over3 =
    A.class "w-2/3"


w1over4 : Html.Attribute msg
w1over4 =
    A.class "w-1/4"


w2over4 : Html.Attribute msg
w2over4 =
    A.class "w-2/4"


w3over4 : Html.Attribute msg
w3over4 =
    A.class "w-3/4"


w1over5 : Html.Attribute msg
w1over5 =
    A.class "w-1/5"


w2over5 : Html.Attribute msg
w2over5 =
    A.class "w-2/5"


w3over5 : Html.Attribute msg
w3over5 =
    A.class "w-3/5"


w4over5 : Html.Attribute msg
w4over5 =
    A.class "w-4/5"


w1over6 : Html.Attribute msg
w1over6 =
    A.class "w-1/6"


w2over6 : Html.Attribute msg
w2over6 =
    A.class "w-2/6"


w3over6 : Html.Attribute msg
w3over6 =
    A.class "w-3/6"


w4over6 : Html.Attribute msg
w4over6 =
    A.class "w-4/6"


w5over6 : Html.Attribute msg
w5over6 =
    A.class "w-5/6"


w1over12 : Html.Attribute msg
w1over12 =
    A.class "w-1/12"


w2over12 : Html.Attribute msg
w2over12 =
    A.class "w-2/12"


w3over12 : Html.Attribute msg
w3over12 =
    A.class "w-3/12"


w4over12 : Html.Attribute msg
w4over12 =
    A.class "w-4/12"


w5over12 : Html.Attribute msg
w5over12 =
    A.class "w-5/12"


w6over12 : Html.Attribute msg
w6over12 =
    A.class "w-6/12"


w7over12 : Html.Attribute msg
w7over12 =
    A.class "w-7/12"


w8over12 : Html.Attribute msg
w8over12 =
    A.class "w-8/12"


w9over12 : Html.Attribute msg
w9over12 =
    A.class "w-9/12"


w10over12 : Html.Attribute msg
w10over12 =
    A.class "w-10/12"


w11over12 : Html.Attribute msg
w11over12 =
    A.class "w-11/12"


wFull : Html.Attribute msg
wFull =
    A.class "w-full"


wScreen : Html.Attribute msg
wScreen =
    A.class "w-screen"


z0 : Html.Attribute msg
z0 =
    A.class "z-0"


z10 : Html.Attribute msg
z10 =
    A.class "z-10"


z20 : Html.Attribute msg
z20 =
    A.class "z-20"


z30 : Html.Attribute msg
z30 =
    A.class "z-30"


z40 : Html.Attribute msg
z40 =
    A.class "z-40"


z50 : Html.Attribute msg
z50 =
    A.class "z-50"


zAuto : Html.Attribute msg
zAuto =
    A.class "z-auto"


gap0 : Html.Attribute msg
gap0 =
    A.class "gap-0"


gap1 : Html.Attribute msg
gap1 =
    A.class "gap-1"


gap2 : Html.Attribute msg
gap2 =
    A.class "gap-2"


gap3 : Html.Attribute msg
gap3 =
    A.class "gap-3"


gap4 : Html.Attribute msg
gap4 =
    A.class "gap-4"


gap5 : Html.Attribute msg
gap5 =
    A.class "gap-5"


gap6 : Html.Attribute msg
gap6 =
    A.class "gap-6"


gap8 : Html.Attribute msg
gap8 =
    A.class "gap-8"


gap10 : Html.Attribute msg
gap10 =
    A.class "gap-10"


gap12 : Html.Attribute msg
gap12 =
    A.class "gap-12"


gap16 : Html.Attribute msg
gap16 =
    A.class "gap-16"


gap20 : Html.Attribute msg
gap20 =
    A.class "gap-20"


gap24 : Html.Attribute msg
gap24 =
    A.class "gap-24"


gap32 : Html.Attribute msg
gap32 =
    A.class "gap-32"


gap40 : Html.Attribute msg
gap40 =
    A.class "gap-40"


gap48 : Html.Attribute msg
gap48 =
    A.class "gap-48"


gap56 : Html.Attribute msg
gap56 =
    A.class "gap-56"


gap64 : Html.Attribute msg
gap64 =
    A.class "gap-64"


gapPx : Html.Attribute msg
gapPx =
    A.class "gap-px"


gapX0 : Html.Attribute msg
gapX0 =
    A.class "gap-x-0"


gapX1 : Html.Attribute msg
gapX1 =
    A.class "gap-x-1"


gapX2 : Html.Attribute msg
gapX2 =
    A.class "gap-x-2"


gapX3 : Html.Attribute msg
gapX3 =
    A.class "gap-x-3"


gapX4 : Html.Attribute msg
gapX4 =
    A.class "gap-x-4"


gapX5 : Html.Attribute msg
gapX5 =
    A.class "gap-x-5"


gapX6 : Html.Attribute msg
gapX6 =
    A.class "gap-x-6"


gapX8 : Html.Attribute msg
gapX8 =
    A.class "gap-x-8"


gapX10 : Html.Attribute msg
gapX10 =
    A.class "gap-x-10"


gapX12 : Html.Attribute msg
gapX12 =
    A.class "gap-x-12"


gapX16 : Html.Attribute msg
gapX16 =
    A.class "gap-x-16"


gapX20 : Html.Attribute msg
gapX20 =
    A.class "gap-x-20"


gapX24 : Html.Attribute msg
gapX24 =
    A.class "gap-x-24"


gapX32 : Html.Attribute msg
gapX32 =
    A.class "gap-x-32"


gapX40 : Html.Attribute msg
gapX40 =
    A.class "gap-x-40"


gapX48 : Html.Attribute msg
gapX48 =
    A.class "gap-x-48"


gapX56 : Html.Attribute msg
gapX56 =
    A.class "gap-x-56"


gapX64 : Html.Attribute msg
gapX64 =
    A.class "gap-x-64"


gapXPx : Html.Attribute msg
gapXPx =
    A.class "gap-x-px"


gapY0 : Html.Attribute msg
gapY0 =
    A.class "gap-y-0"


gapY1 : Html.Attribute msg
gapY1 =
    A.class "gap-y-1"


gapY2 : Html.Attribute msg
gapY2 =
    A.class "gap-y-2"


gapY3 : Html.Attribute msg
gapY3 =
    A.class "gap-y-3"


gapY4 : Html.Attribute msg
gapY4 =
    A.class "gap-y-4"


gapY5 : Html.Attribute msg
gapY5 =
    A.class "gap-y-5"


gapY6 : Html.Attribute msg
gapY6 =
    A.class "gap-y-6"


gapY8 : Html.Attribute msg
gapY8 =
    A.class "gap-y-8"


gapY10 : Html.Attribute msg
gapY10 =
    A.class "gap-y-10"


gapY12 : Html.Attribute msg
gapY12 =
    A.class "gap-y-12"


gapY16 : Html.Attribute msg
gapY16 =
    A.class "gap-y-16"


gapY20 : Html.Attribute msg
gapY20 =
    A.class "gap-y-20"


gapY24 : Html.Attribute msg
gapY24 =
    A.class "gap-y-24"


gapY32 : Html.Attribute msg
gapY32 =
    A.class "gap-y-32"


gapY40 : Html.Attribute msg
gapY40 =
    A.class "gap-y-40"


gapY48 : Html.Attribute msg
gapY48 =
    A.class "gap-y-48"


gapY56 : Html.Attribute msg
gapY56 =
    A.class "gap-y-56"


gapY64 : Html.Attribute msg
gapY64 =
    A.class "gap-y-64"


gapYPx : Html.Attribute msg
gapYPx =
    A.class "gap-y-px"


gridFlowRow : Html.Attribute msg
gridFlowRow =
    A.class "grid-flow-row"


gridFlowCol : Html.Attribute msg
gridFlowCol =
    A.class "grid-flow-col"


gridFlowRowDense : Html.Attribute msg
gridFlowRowDense =
    A.class "grid-flow-row-dense"


gridFlowColDense : Html.Attribute msg
gridFlowColDense =
    A.class "grid-flow-col-dense"


gridCols1 : Html.Attribute msg
gridCols1 =
    A.class "grid-cols-1"


gridCols2 : Html.Attribute msg
gridCols2 =
    A.class "grid-cols-2"


gridCols3 : Html.Attribute msg
gridCols3 =
    A.class "grid-cols-3"


gridCols4 : Html.Attribute msg
gridCols4 =
    A.class "grid-cols-4"


gridCols5 : Html.Attribute msg
gridCols5 =
    A.class "grid-cols-5"


gridCols6 : Html.Attribute msg
gridCols6 =
    A.class "grid-cols-6"


gridCols7 : Html.Attribute msg
gridCols7 =
    A.class "grid-cols-7"


gridCols8 : Html.Attribute msg
gridCols8 =
    A.class "grid-cols-8"


gridCols9 : Html.Attribute msg
gridCols9 =
    A.class "grid-cols-9"


gridCols10 : Html.Attribute msg
gridCols10 =
    A.class "grid-cols-10"


gridCols11 : Html.Attribute msg
gridCols11 =
    A.class "grid-cols-11"


gridCols12 : Html.Attribute msg
gridCols12 =
    A.class "grid-cols-12"


gridColsNone : Html.Attribute msg
gridColsNone =
    A.class "grid-cols-none"


colAuto : Html.Attribute msg
colAuto =
    A.class "col-auto"


colSpan1 : Html.Attribute msg
colSpan1 =
    A.class "col-span-1"


colSpan2 : Html.Attribute msg
colSpan2 =
    A.class "col-span-2"


colSpan3 : Html.Attribute msg
colSpan3 =
    A.class "col-span-3"


colSpan4 : Html.Attribute msg
colSpan4 =
    A.class "col-span-4"


colSpan5 : Html.Attribute msg
colSpan5 =
    A.class "col-span-5"


colSpan6 : Html.Attribute msg
colSpan6 =
    A.class "col-span-6"


colSpan7 : Html.Attribute msg
colSpan7 =
    A.class "col-span-7"


colSpan8 : Html.Attribute msg
colSpan8 =
    A.class "col-span-8"


colSpan9 : Html.Attribute msg
colSpan9 =
    A.class "col-span-9"


colSpan10 : Html.Attribute msg
colSpan10 =
    A.class "col-span-10"


colSpan11 : Html.Attribute msg
colSpan11 =
    A.class "col-span-11"


colSpan12 : Html.Attribute msg
colSpan12 =
    A.class "col-span-12"


colStart1 : Html.Attribute msg
colStart1 =
    A.class "col-start-1"


colStart2 : Html.Attribute msg
colStart2 =
    A.class "col-start-2"


colStart3 : Html.Attribute msg
colStart3 =
    A.class "col-start-3"


colStart4 : Html.Attribute msg
colStart4 =
    A.class "col-start-4"


colStart5 : Html.Attribute msg
colStart5 =
    A.class "col-start-5"


colStart6 : Html.Attribute msg
colStart6 =
    A.class "col-start-6"


colStart7 : Html.Attribute msg
colStart7 =
    A.class "col-start-7"


colStart8 : Html.Attribute msg
colStart8 =
    A.class "col-start-8"


colStart9 : Html.Attribute msg
colStart9 =
    A.class "col-start-9"


colStart10 : Html.Attribute msg
colStart10 =
    A.class "col-start-10"


colStart11 : Html.Attribute msg
colStart11 =
    A.class "col-start-11"


colStart12 : Html.Attribute msg
colStart12 =
    A.class "col-start-12"


colStart13 : Html.Attribute msg
colStart13 =
    A.class "col-start-13"


colStartAuto : Html.Attribute msg
colStartAuto =
    A.class "col-start-auto"


colEnd1 : Html.Attribute msg
colEnd1 =
    A.class "col-end-1"


colEnd2 : Html.Attribute msg
colEnd2 =
    A.class "col-end-2"


colEnd3 : Html.Attribute msg
colEnd3 =
    A.class "col-end-3"


colEnd4 : Html.Attribute msg
colEnd4 =
    A.class "col-end-4"


colEnd5 : Html.Attribute msg
colEnd5 =
    A.class "col-end-5"


colEnd6 : Html.Attribute msg
colEnd6 =
    A.class "col-end-6"


colEnd7 : Html.Attribute msg
colEnd7 =
    A.class "col-end-7"


colEnd8 : Html.Attribute msg
colEnd8 =
    A.class "col-end-8"


colEnd9 : Html.Attribute msg
colEnd9 =
    A.class "col-end-9"


colEnd10 : Html.Attribute msg
colEnd10 =
    A.class "col-end-10"


colEnd11 : Html.Attribute msg
colEnd11 =
    A.class "col-end-11"


colEnd12 : Html.Attribute msg
colEnd12 =
    A.class "col-end-12"


colEnd13 : Html.Attribute msg
colEnd13 =
    A.class "col-end-13"


colEndAuto : Html.Attribute msg
colEndAuto =
    A.class "col-end-auto"


gridRows1 : Html.Attribute msg
gridRows1 =
    A.class "grid-rows-1"


gridRows2 : Html.Attribute msg
gridRows2 =
    A.class "grid-rows-2"


gridRows3 : Html.Attribute msg
gridRows3 =
    A.class "grid-rows-3"


gridRows4 : Html.Attribute msg
gridRows4 =
    A.class "grid-rows-4"


gridRows5 : Html.Attribute msg
gridRows5 =
    A.class "grid-rows-5"


gridRows6 : Html.Attribute msg
gridRows6 =
    A.class "grid-rows-6"


gridRowsNone : Html.Attribute msg
gridRowsNone =
    A.class "grid-rows-none"


rowAuto : Html.Attribute msg
rowAuto =
    A.class "row-auto"


rowSpan1 : Html.Attribute msg
rowSpan1 =
    A.class "row-span-1"


rowSpan2 : Html.Attribute msg
rowSpan2 =
    A.class "row-span-2"


rowSpan3 : Html.Attribute msg
rowSpan3 =
    A.class "row-span-3"


rowSpan4 : Html.Attribute msg
rowSpan4 =
    A.class "row-span-4"


rowSpan5 : Html.Attribute msg
rowSpan5 =
    A.class "row-span-5"


rowSpan6 : Html.Attribute msg
rowSpan6 =
    A.class "row-span-6"


rowStart1 : Html.Attribute msg
rowStart1 =
    A.class "row-start-1"


rowStart2 : Html.Attribute msg
rowStart2 =
    A.class "row-start-2"


rowStart3 : Html.Attribute msg
rowStart3 =
    A.class "row-start-3"


rowStart4 : Html.Attribute msg
rowStart4 =
    A.class "row-start-4"


rowStart5 : Html.Attribute msg
rowStart5 =
    A.class "row-start-5"


rowStart6 : Html.Attribute msg
rowStart6 =
    A.class "row-start-6"


rowStart7 : Html.Attribute msg
rowStart7 =
    A.class "row-start-7"


rowStartAuto : Html.Attribute msg
rowStartAuto =
    A.class "row-start-auto"


rowEnd1 : Html.Attribute msg
rowEnd1 =
    A.class "row-end-1"


rowEnd2 : Html.Attribute msg
rowEnd2 =
    A.class "row-end-2"


rowEnd3 : Html.Attribute msg
rowEnd3 =
    A.class "row-end-3"


rowEnd4 : Html.Attribute msg
rowEnd4 =
    A.class "row-end-4"


rowEnd5 : Html.Attribute msg
rowEnd5 =
    A.class "row-end-5"


rowEnd6 : Html.Attribute msg
rowEnd6 =
    A.class "row-end-6"


rowEnd7 : Html.Attribute msg
rowEnd7 =
    A.class "row-end-7"


rowEndAuto : Html.Attribute msg
rowEndAuto =
    A.class "row-end-auto"


transform : Html.Attribute msg
transform =
    A.class "transform"


transformNone : Html.Attribute msg
transformNone =
    A.class "transform-none"


originCenter : Html.Attribute msg
originCenter =
    A.class "origin-center"


originTop : Html.Attribute msg
originTop =
    A.class "origin-top"


originTopRight : Html.Attribute msg
originTopRight =
    A.class "origin-top-right"


originRight : Html.Attribute msg
originRight =
    A.class "origin-right"


originBottomRight : Html.Attribute msg
originBottomRight =
    A.class "origin-bottom-right"


originBottom : Html.Attribute msg
originBottom =
    A.class "origin-bottom"


originBottomLeft : Html.Attribute msg
originBottomLeft =
    A.class "origin-bottom-left"


originLeft : Html.Attribute msg
originLeft =
    A.class "origin-left"


originTopLeft : Html.Attribute msg
originTopLeft =
    A.class "origin-top-left"


scale0 : Html.Attribute msg
scale0 =
    A.class "scale-0"


scale50 : Html.Attribute msg
scale50 =
    A.class "scale-50"


scale75 : Html.Attribute msg
scale75 =
    A.class "scale-75"


scale90 : Html.Attribute msg
scale90 =
    A.class "scale-90"


scale95 : Html.Attribute msg
scale95 =
    A.class "scale-95"


scale100 : Html.Attribute msg
scale100 =
    A.class "scale-100"


scale105 : Html.Attribute msg
scale105 =
    A.class "scale-105"


scale110 : Html.Attribute msg
scale110 =
    A.class "scale-110"


scale125 : Html.Attribute msg
scale125 =
    A.class "scale-125"


scale150 : Html.Attribute msg
scale150 =
    A.class "scale-150"


scaleX0 : Html.Attribute msg
scaleX0 =
    A.class "scale-x-0"


scaleX50 : Html.Attribute msg
scaleX50 =
    A.class "scale-x-50"


scaleX75 : Html.Attribute msg
scaleX75 =
    A.class "scale-x-75"


scaleX90 : Html.Attribute msg
scaleX90 =
    A.class "scale-x-90"


scaleX95 : Html.Attribute msg
scaleX95 =
    A.class "scale-x-95"


scaleX100 : Html.Attribute msg
scaleX100 =
    A.class "scale-x-100"


scaleX105 : Html.Attribute msg
scaleX105 =
    A.class "scale-x-105"


scaleX110 : Html.Attribute msg
scaleX110 =
    A.class "scale-x-110"


scaleX125 : Html.Attribute msg
scaleX125 =
    A.class "scale-x-125"


scaleX150 : Html.Attribute msg
scaleX150 =
    A.class "scale-x-150"


scaleY0 : Html.Attribute msg
scaleY0 =
    A.class "scale-y-0"


scaleY50 : Html.Attribute msg
scaleY50 =
    A.class "scale-y-50"


scaleY75 : Html.Attribute msg
scaleY75 =
    A.class "scale-y-75"


scaleY90 : Html.Attribute msg
scaleY90 =
    A.class "scale-y-90"


scaleY95 : Html.Attribute msg
scaleY95 =
    A.class "scale-y-95"


scaleY100 : Html.Attribute msg
scaleY100 =
    A.class "scale-y-100"


scaleY105 : Html.Attribute msg
scaleY105 =
    A.class "scale-y-105"


scaleY110 : Html.Attribute msg
scaleY110 =
    A.class "scale-y-110"


scaleY125 : Html.Attribute msg
scaleY125 =
    A.class "scale-y-125"


scaleY150 : Html.Attribute msg
scaleY150 =
    A.class "scale-y-150"


hoverScale0 : Html.Attribute msg
hoverScale0 =
    A.class "hover:scale-0"


hoverScale50 : Html.Attribute msg
hoverScale50 =
    A.class "hover:scale-50"


hoverScale75 : Html.Attribute msg
hoverScale75 =
    A.class "hover:scale-75"


hoverScale90 : Html.Attribute msg
hoverScale90 =
    A.class "hover:scale-90"


hoverScale95 : Html.Attribute msg
hoverScale95 =
    A.class "hover:scale-95"


hoverScale100 : Html.Attribute msg
hoverScale100 =
    A.class "hover:scale-100"


hoverScale105 : Html.Attribute msg
hoverScale105 =
    A.class "hover:scale-105"


hoverScale110 : Html.Attribute msg
hoverScale110 =
    A.class "hover:scale-110"


hoverScale125 : Html.Attribute msg
hoverScale125 =
    A.class "hover:scale-125"


hoverScale150 : Html.Attribute msg
hoverScale150 =
    A.class "hover:scale-150"


hoverScaleX0 : Html.Attribute msg
hoverScaleX0 =
    A.class "hover:scale-x-0"


hoverScaleX50 : Html.Attribute msg
hoverScaleX50 =
    A.class "hover:scale-x-50"


hoverScaleX75 : Html.Attribute msg
hoverScaleX75 =
    A.class "hover:scale-x-75"


hoverScaleX90 : Html.Attribute msg
hoverScaleX90 =
    A.class "hover:scale-x-90"


hoverScaleX95 : Html.Attribute msg
hoverScaleX95 =
    A.class "hover:scale-x-95"


hoverScaleX100 : Html.Attribute msg
hoverScaleX100 =
    A.class "hover:scale-x-100"


hoverScaleX105 : Html.Attribute msg
hoverScaleX105 =
    A.class "hover:scale-x-105"


hoverScaleX110 : Html.Attribute msg
hoverScaleX110 =
    A.class "hover:scale-x-110"


hoverScaleX125 : Html.Attribute msg
hoverScaleX125 =
    A.class "hover:scale-x-125"


hoverScaleX150 : Html.Attribute msg
hoverScaleX150 =
    A.class "hover:scale-x-150"


hoverScaleY0 : Html.Attribute msg
hoverScaleY0 =
    A.class "hover:scale-y-0"


hoverScaleY50 : Html.Attribute msg
hoverScaleY50 =
    A.class "hover:scale-y-50"


hoverScaleY75 : Html.Attribute msg
hoverScaleY75 =
    A.class "hover:scale-y-75"


hoverScaleY90 : Html.Attribute msg
hoverScaleY90 =
    A.class "hover:scale-y-90"


hoverScaleY95 : Html.Attribute msg
hoverScaleY95 =
    A.class "hover:scale-y-95"


hoverScaleY100 : Html.Attribute msg
hoverScaleY100 =
    A.class "hover:scale-y-100"


hoverScaleY105 : Html.Attribute msg
hoverScaleY105 =
    A.class "hover:scale-y-105"


hoverScaleY110 : Html.Attribute msg
hoverScaleY110 =
    A.class "hover:scale-y-110"


hoverScaleY125 : Html.Attribute msg
hoverScaleY125 =
    A.class "hover:scale-y-125"


hoverScaleY150 : Html.Attribute msg
hoverScaleY150 =
    A.class "hover:scale-y-150"


focusScale0 : Html.Attribute msg
focusScale0 =
    A.class "focus:scale-0"


focusScale50 : Html.Attribute msg
focusScale50 =
    A.class "focus:scale-50"


focusScale75 : Html.Attribute msg
focusScale75 =
    A.class "focus:scale-75"


focusScale90 : Html.Attribute msg
focusScale90 =
    A.class "focus:scale-90"


focusScale95 : Html.Attribute msg
focusScale95 =
    A.class "focus:scale-95"


focusScale100 : Html.Attribute msg
focusScale100 =
    A.class "focus:scale-100"


focusScale105 : Html.Attribute msg
focusScale105 =
    A.class "focus:scale-105"


focusScale110 : Html.Attribute msg
focusScale110 =
    A.class "focus:scale-110"


focusScale125 : Html.Attribute msg
focusScale125 =
    A.class "focus:scale-125"


focusScale150 : Html.Attribute msg
focusScale150 =
    A.class "focus:scale-150"


focusScaleX0 : Html.Attribute msg
focusScaleX0 =
    A.class "focus:scale-x-0"


focusScaleX50 : Html.Attribute msg
focusScaleX50 =
    A.class "focus:scale-x-50"


focusScaleX75 : Html.Attribute msg
focusScaleX75 =
    A.class "focus:scale-x-75"


focusScaleX90 : Html.Attribute msg
focusScaleX90 =
    A.class "focus:scale-x-90"


focusScaleX95 : Html.Attribute msg
focusScaleX95 =
    A.class "focus:scale-x-95"


focusScaleX100 : Html.Attribute msg
focusScaleX100 =
    A.class "focus:scale-x-100"


focusScaleX105 : Html.Attribute msg
focusScaleX105 =
    A.class "focus:scale-x-105"


focusScaleX110 : Html.Attribute msg
focusScaleX110 =
    A.class "focus:scale-x-110"


focusScaleX125 : Html.Attribute msg
focusScaleX125 =
    A.class "focus:scale-x-125"


focusScaleX150 : Html.Attribute msg
focusScaleX150 =
    A.class "focus:scale-x-150"


focusScaleY0 : Html.Attribute msg
focusScaleY0 =
    A.class "focus:scale-y-0"


focusScaleY50 : Html.Attribute msg
focusScaleY50 =
    A.class "focus:scale-y-50"


focusScaleY75 : Html.Attribute msg
focusScaleY75 =
    A.class "focus:scale-y-75"


focusScaleY90 : Html.Attribute msg
focusScaleY90 =
    A.class "focus:scale-y-90"


focusScaleY95 : Html.Attribute msg
focusScaleY95 =
    A.class "focus:scale-y-95"


focusScaleY100 : Html.Attribute msg
focusScaleY100 =
    A.class "focus:scale-y-100"


focusScaleY105 : Html.Attribute msg
focusScaleY105 =
    A.class "focus:scale-y-105"


focusScaleY110 : Html.Attribute msg
focusScaleY110 =
    A.class "focus:scale-y-110"


focusScaleY125 : Html.Attribute msg
focusScaleY125 =
    A.class "focus:scale-y-125"


focusScaleY150 : Html.Attribute msg
focusScaleY150 =
    A.class "focus:scale-y-150"


rotate0 : Html.Attribute msg
rotate0 =
    A.class "rotate-0"


rotate45 : Html.Attribute msg
rotate45 =
    A.class "rotate-45"


rotate90 : Html.Attribute msg
rotate90 =
    A.class "rotate-90"


rotate180 : Html.Attribute msg
rotate180 =
    A.class "rotate-180"


negRotate180 : Html.Attribute msg
negRotate180 =
    A.class "-rotate-180"


negRotate90 : Html.Attribute msg
negRotate90 =
    A.class "-rotate-90"


negRotate45 : Html.Attribute msg
negRotate45 =
    A.class "-rotate-45"


hoverRotate0 : Html.Attribute msg
hoverRotate0 =
    A.class "hover:rotate-0"


hoverRotate45 : Html.Attribute msg
hoverRotate45 =
    A.class "hover:rotate-45"


hoverRotate90 : Html.Attribute msg
hoverRotate90 =
    A.class "hover:rotate-90"


hoverRotate180 : Html.Attribute msg
hoverRotate180 =
    A.class "hover:rotate-180"


hoverNegRotate180 : Html.Attribute msg
hoverNegRotate180 =
    A.class "hover:-rotate-180"


hoverNegRotate90 : Html.Attribute msg
hoverNegRotate90 =
    A.class "hover:-rotate-90"


hoverNegRotate45 : Html.Attribute msg
hoverNegRotate45 =
    A.class "hover:-rotate-45"


focusRotate0 : Html.Attribute msg
focusRotate0 =
    A.class "focus:rotate-0"


focusRotate45 : Html.Attribute msg
focusRotate45 =
    A.class "focus:rotate-45"


focusRotate90 : Html.Attribute msg
focusRotate90 =
    A.class "focus:rotate-90"


focusRotate180 : Html.Attribute msg
focusRotate180 =
    A.class "focus:rotate-180"


focusNegRotate180 : Html.Attribute msg
focusNegRotate180 =
    A.class "focus:-rotate-180"


focusNegRotate90 : Html.Attribute msg
focusNegRotate90 =
    A.class "focus:-rotate-90"


focusNegRotate45 : Html.Attribute msg
focusNegRotate45 =
    A.class "focus:-rotate-45"


translateX0 : Html.Attribute msg
translateX0 =
    A.class "translate-x-0"


translateX1 : Html.Attribute msg
translateX1 =
    A.class "translate-x-1"


translateX2 : Html.Attribute msg
translateX2 =
    A.class "translate-x-2"


translateX3 : Html.Attribute msg
translateX3 =
    A.class "translate-x-3"


translateX4 : Html.Attribute msg
translateX4 =
    A.class "translate-x-4"


translateX5 : Html.Attribute msg
translateX5 =
    A.class "translate-x-5"


translateX6 : Html.Attribute msg
translateX6 =
    A.class "translate-x-6"


translateX8 : Html.Attribute msg
translateX8 =
    A.class "translate-x-8"


translateX10 : Html.Attribute msg
translateX10 =
    A.class "translate-x-10"


translateX12 : Html.Attribute msg
translateX12 =
    A.class "translate-x-12"


translateX16 : Html.Attribute msg
translateX16 =
    A.class "translate-x-16"


translateX20 : Html.Attribute msg
translateX20 =
    A.class "translate-x-20"


translateX24 : Html.Attribute msg
translateX24 =
    A.class "translate-x-24"


translateX32 : Html.Attribute msg
translateX32 =
    A.class "translate-x-32"


translateX40 : Html.Attribute msg
translateX40 =
    A.class "translate-x-40"


translateX48 : Html.Attribute msg
translateX48 =
    A.class "translate-x-48"


translateX56 : Html.Attribute msg
translateX56 =
    A.class "translate-x-56"


translateX64 : Html.Attribute msg
translateX64 =
    A.class "translate-x-64"


translateXPx : Html.Attribute msg
translateXPx =
    A.class "translate-x-px"


negTranslateX1 : Html.Attribute msg
negTranslateX1 =
    A.class "-translate-x-1"


negTranslateX2 : Html.Attribute msg
negTranslateX2 =
    A.class "-translate-x-2"


negTranslateX3 : Html.Attribute msg
negTranslateX3 =
    A.class "-translate-x-3"


negTranslateX4 : Html.Attribute msg
negTranslateX4 =
    A.class "-translate-x-4"


negTranslateX5 : Html.Attribute msg
negTranslateX5 =
    A.class "-translate-x-5"


negTranslateX6 : Html.Attribute msg
negTranslateX6 =
    A.class "-translate-x-6"


negTranslateX8 : Html.Attribute msg
negTranslateX8 =
    A.class "-translate-x-8"


negTranslateX10 : Html.Attribute msg
negTranslateX10 =
    A.class "-translate-x-10"


negTranslateX12 : Html.Attribute msg
negTranslateX12 =
    A.class "-translate-x-12"


negTranslateX16 : Html.Attribute msg
negTranslateX16 =
    A.class "-translate-x-16"


negTranslateX20 : Html.Attribute msg
negTranslateX20 =
    A.class "-translate-x-20"


negTranslateX24 : Html.Attribute msg
negTranslateX24 =
    A.class "-translate-x-24"


negTranslateX32 : Html.Attribute msg
negTranslateX32 =
    A.class "-translate-x-32"


negTranslateX40 : Html.Attribute msg
negTranslateX40 =
    A.class "-translate-x-40"


negTranslateX48 : Html.Attribute msg
negTranslateX48 =
    A.class "-translate-x-48"


negTranslateX56 : Html.Attribute msg
negTranslateX56 =
    A.class "-translate-x-56"


negTranslateX64 : Html.Attribute msg
negTranslateX64 =
    A.class "-translate-x-64"


negTranslateXPx : Html.Attribute msg
negTranslateXPx =
    A.class "-translate-x-px"


negTranslateXFull : Html.Attribute msg
negTranslateXFull =
    A.class "-translate-x-full"


negTranslateX1over2 : Html.Attribute msg
negTranslateX1over2 =
    A.class "-translate-x-1/2"


translateX1over2 : Html.Attribute msg
translateX1over2 =
    A.class "translate-x-1/2"


translateXFull : Html.Attribute msg
translateXFull =
    A.class "translate-x-full"


translateY0 : Html.Attribute msg
translateY0 =
    A.class "translate-y-0"


translateY1 : Html.Attribute msg
translateY1 =
    A.class "translate-y-1"


translateY2 : Html.Attribute msg
translateY2 =
    A.class "translate-y-2"


translateY3 : Html.Attribute msg
translateY3 =
    A.class "translate-y-3"


translateY4 : Html.Attribute msg
translateY4 =
    A.class "translate-y-4"


translateY5 : Html.Attribute msg
translateY5 =
    A.class "translate-y-5"


translateY6 : Html.Attribute msg
translateY6 =
    A.class "translate-y-6"


translateY8 : Html.Attribute msg
translateY8 =
    A.class "translate-y-8"


translateY10 : Html.Attribute msg
translateY10 =
    A.class "translate-y-10"


translateY12 : Html.Attribute msg
translateY12 =
    A.class "translate-y-12"


translateY16 : Html.Attribute msg
translateY16 =
    A.class "translate-y-16"


translateY20 : Html.Attribute msg
translateY20 =
    A.class "translate-y-20"


translateY24 : Html.Attribute msg
translateY24 =
    A.class "translate-y-24"


translateY32 : Html.Attribute msg
translateY32 =
    A.class "translate-y-32"


translateY40 : Html.Attribute msg
translateY40 =
    A.class "translate-y-40"


translateY48 : Html.Attribute msg
translateY48 =
    A.class "translate-y-48"


translateY56 : Html.Attribute msg
translateY56 =
    A.class "translate-y-56"


translateY64 : Html.Attribute msg
translateY64 =
    A.class "translate-y-64"


translateYPx : Html.Attribute msg
translateYPx =
    A.class "translate-y-px"


negTranslateY1 : Html.Attribute msg
negTranslateY1 =
    A.class "-translate-y-1"


negTranslateY2 : Html.Attribute msg
negTranslateY2 =
    A.class "-translate-y-2"


negTranslateY3 : Html.Attribute msg
negTranslateY3 =
    A.class "-translate-y-3"


negTranslateY4 : Html.Attribute msg
negTranslateY4 =
    A.class "-translate-y-4"


negTranslateY5 : Html.Attribute msg
negTranslateY5 =
    A.class "-translate-y-5"


negTranslateY6 : Html.Attribute msg
negTranslateY6 =
    A.class "-translate-y-6"


negTranslateY8 : Html.Attribute msg
negTranslateY8 =
    A.class "-translate-y-8"


negTranslateY10 : Html.Attribute msg
negTranslateY10 =
    A.class "-translate-y-10"


negTranslateY12 : Html.Attribute msg
negTranslateY12 =
    A.class "-translate-y-12"


negTranslateY16 : Html.Attribute msg
negTranslateY16 =
    A.class "-translate-y-16"


negTranslateY20 : Html.Attribute msg
negTranslateY20 =
    A.class "-translate-y-20"


negTranslateY24 : Html.Attribute msg
negTranslateY24 =
    A.class "-translate-y-24"


negTranslateY32 : Html.Attribute msg
negTranslateY32 =
    A.class "-translate-y-32"


negTranslateY40 : Html.Attribute msg
negTranslateY40 =
    A.class "-translate-y-40"


negTranslateY48 : Html.Attribute msg
negTranslateY48 =
    A.class "-translate-y-48"


negTranslateY56 : Html.Attribute msg
negTranslateY56 =
    A.class "-translate-y-56"


negTranslateY64 : Html.Attribute msg
negTranslateY64 =
    A.class "-translate-y-64"


negTranslateYPx : Html.Attribute msg
negTranslateYPx =
    A.class "-translate-y-px"


negTranslateYFull : Html.Attribute msg
negTranslateYFull =
    A.class "-translate-y-full"


negTranslateY1over2 : Html.Attribute msg
negTranslateY1over2 =
    A.class "-translate-y-1/2"


translateY1over2 : Html.Attribute msg
translateY1over2 =
    A.class "translate-y-1/2"


translateYFull : Html.Attribute msg
translateYFull =
    A.class "translate-y-full"


hoverTranslateX0 : Html.Attribute msg
hoverTranslateX0 =
    A.class "hover:translate-x-0"


hoverTranslateX1 : Html.Attribute msg
hoverTranslateX1 =
    A.class "hover:translate-x-1"


hoverTranslateX2 : Html.Attribute msg
hoverTranslateX2 =
    A.class "hover:translate-x-2"


hoverTranslateX3 : Html.Attribute msg
hoverTranslateX3 =
    A.class "hover:translate-x-3"


hoverTranslateX4 : Html.Attribute msg
hoverTranslateX4 =
    A.class "hover:translate-x-4"


hoverTranslateX5 : Html.Attribute msg
hoverTranslateX5 =
    A.class "hover:translate-x-5"


hoverTranslateX6 : Html.Attribute msg
hoverTranslateX6 =
    A.class "hover:translate-x-6"


hoverTranslateX8 : Html.Attribute msg
hoverTranslateX8 =
    A.class "hover:translate-x-8"


hoverTranslateX10 : Html.Attribute msg
hoverTranslateX10 =
    A.class "hover:translate-x-10"


hoverTranslateX12 : Html.Attribute msg
hoverTranslateX12 =
    A.class "hover:translate-x-12"


hoverTranslateX16 : Html.Attribute msg
hoverTranslateX16 =
    A.class "hover:translate-x-16"


hoverTranslateX20 : Html.Attribute msg
hoverTranslateX20 =
    A.class "hover:translate-x-20"


hoverTranslateX24 : Html.Attribute msg
hoverTranslateX24 =
    A.class "hover:translate-x-24"


hoverTranslateX32 : Html.Attribute msg
hoverTranslateX32 =
    A.class "hover:translate-x-32"


hoverTranslateX40 : Html.Attribute msg
hoverTranslateX40 =
    A.class "hover:translate-x-40"


hoverTranslateX48 : Html.Attribute msg
hoverTranslateX48 =
    A.class "hover:translate-x-48"


hoverTranslateX56 : Html.Attribute msg
hoverTranslateX56 =
    A.class "hover:translate-x-56"


hoverTranslateX64 : Html.Attribute msg
hoverTranslateX64 =
    A.class "hover:translate-x-64"


hoverTranslateXPx : Html.Attribute msg
hoverTranslateXPx =
    A.class "hover:translate-x-px"


hoverNegTranslateX1 : Html.Attribute msg
hoverNegTranslateX1 =
    A.class "hover:-translate-x-1"


hoverNegTranslateX2 : Html.Attribute msg
hoverNegTranslateX2 =
    A.class "hover:-translate-x-2"


hoverNegTranslateX3 : Html.Attribute msg
hoverNegTranslateX3 =
    A.class "hover:-translate-x-3"


hoverNegTranslateX4 : Html.Attribute msg
hoverNegTranslateX4 =
    A.class "hover:-translate-x-4"


hoverNegTranslateX5 : Html.Attribute msg
hoverNegTranslateX5 =
    A.class "hover:-translate-x-5"


hoverNegTranslateX6 : Html.Attribute msg
hoverNegTranslateX6 =
    A.class "hover:-translate-x-6"


hoverNegTranslateX8 : Html.Attribute msg
hoverNegTranslateX8 =
    A.class "hover:-translate-x-8"


hoverNegTranslateX10 : Html.Attribute msg
hoverNegTranslateX10 =
    A.class "hover:-translate-x-10"


hoverNegTranslateX12 : Html.Attribute msg
hoverNegTranslateX12 =
    A.class "hover:-translate-x-12"


hoverNegTranslateX16 : Html.Attribute msg
hoverNegTranslateX16 =
    A.class "hover:-translate-x-16"


hoverNegTranslateX20 : Html.Attribute msg
hoverNegTranslateX20 =
    A.class "hover:-translate-x-20"


hoverNegTranslateX24 : Html.Attribute msg
hoverNegTranslateX24 =
    A.class "hover:-translate-x-24"


hoverNegTranslateX32 : Html.Attribute msg
hoverNegTranslateX32 =
    A.class "hover:-translate-x-32"


hoverNegTranslateX40 : Html.Attribute msg
hoverNegTranslateX40 =
    A.class "hover:-translate-x-40"


hoverNegTranslateX48 : Html.Attribute msg
hoverNegTranslateX48 =
    A.class "hover:-translate-x-48"


hoverNegTranslateX56 : Html.Attribute msg
hoverNegTranslateX56 =
    A.class "hover:-translate-x-56"


hoverNegTranslateX64 : Html.Attribute msg
hoverNegTranslateX64 =
    A.class "hover:-translate-x-64"


hoverNegTranslateXPx : Html.Attribute msg
hoverNegTranslateXPx =
    A.class "hover:-translate-x-px"


hoverNegTranslateXFull : Html.Attribute msg
hoverNegTranslateXFull =
    A.class "hover:-translate-x-full"


hoverNegTranslateX1over2 : Html.Attribute msg
hoverNegTranslateX1over2 =
    A.class "hover:-translate-x-1/2"


hoverTranslateX1over2 : Html.Attribute msg
hoverTranslateX1over2 =
    A.class "hover:translate-x-1/2"


hoverTranslateXFull : Html.Attribute msg
hoverTranslateXFull =
    A.class "hover:translate-x-full"


hoverTranslateY0 : Html.Attribute msg
hoverTranslateY0 =
    A.class "hover:translate-y-0"


hoverTranslateY1 : Html.Attribute msg
hoverTranslateY1 =
    A.class "hover:translate-y-1"


hoverTranslateY2 : Html.Attribute msg
hoverTranslateY2 =
    A.class "hover:translate-y-2"


hoverTranslateY3 : Html.Attribute msg
hoverTranslateY3 =
    A.class "hover:translate-y-3"


hoverTranslateY4 : Html.Attribute msg
hoverTranslateY4 =
    A.class "hover:translate-y-4"


hoverTranslateY5 : Html.Attribute msg
hoverTranslateY5 =
    A.class "hover:translate-y-5"


hoverTranslateY6 : Html.Attribute msg
hoverTranslateY6 =
    A.class "hover:translate-y-6"


hoverTranslateY8 : Html.Attribute msg
hoverTranslateY8 =
    A.class "hover:translate-y-8"


hoverTranslateY10 : Html.Attribute msg
hoverTranslateY10 =
    A.class "hover:translate-y-10"


hoverTranslateY12 : Html.Attribute msg
hoverTranslateY12 =
    A.class "hover:translate-y-12"


hoverTranslateY16 : Html.Attribute msg
hoverTranslateY16 =
    A.class "hover:translate-y-16"


hoverTranslateY20 : Html.Attribute msg
hoverTranslateY20 =
    A.class "hover:translate-y-20"


hoverTranslateY24 : Html.Attribute msg
hoverTranslateY24 =
    A.class "hover:translate-y-24"


hoverTranslateY32 : Html.Attribute msg
hoverTranslateY32 =
    A.class "hover:translate-y-32"


hoverTranslateY40 : Html.Attribute msg
hoverTranslateY40 =
    A.class "hover:translate-y-40"


hoverTranslateY48 : Html.Attribute msg
hoverTranslateY48 =
    A.class "hover:translate-y-48"


hoverTranslateY56 : Html.Attribute msg
hoverTranslateY56 =
    A.class "hover:translate-y-56"


hoverTranslateY64 : Html.Attribute msg
hoverTranslateY64 =
    A.class "hover:translate-y-64"


hoverTranslateYPx : Html.Attribute msg
hoverTranslateYPx =
    A.class "hover:translate-y-px"


hoverNegTranslateY1 : Html.Attribute msg
hoverNegTranslateY1 =
    A.class "hover:-translate-y-1"


hoverNegTranslateY2 : Html.Attribute msg
hoverNegTranslateY2 =
    A.class "hover:-translate-y-2"


hoverNegTranslateY3 : Html.Attribute msg
hoverNegTranslateY3 =
    A.class "hover:-translate-y-3"


hoverNegTranslateY4 : Html.Attribute msg
hoverNegTranslateY4 =
    A.class "hover:-translate-y-4"


hoverNegTranslateY5 : Html.Attribute msg
hoverNegTranslateY5 =
    A.class "hover:-translate-y-5"


hoverNegTranslateY6 : Html.Attribute msg
hoverNegTranslateY6 =
    A.class "hover:-translate-y-6"


hoverNegTranslateY8 : Html.Attribute msg
hoverNegTranslateY8 =
    A.class "hover:-translate-y-8"


hoverNegTranslateY10 : Html.Attribute msg
hoverNegTranslateY10 =
    A.class "hover:-translate-y-10"


hoverNegTranslateY12 : Html.Attribute msg
hoverNegTranslateY12 =
    A.class "hover:-translate-y-12"


hoverNegTranslateY16 : Html.Attribute msg
hoverNegTranslateY16 =
    A.class "hover:-translate-y-16"


hoverNegTranslateY20 : Html.Attribute msg
hoverNegTranslateY20 =
    A.class "hover:-translate-y-20"


hoverNegTranslateY24 : Html.Attribute msg
hoverNegTranslateY24 =
    A.class "hover:-translate-y-24"


hoverNegTranslateY32 : Html.Attribute msg
hoverNegTranslateY32 =
    A.class "hover:-translate-y-32"


hoverNegTranslateY40 : Html.Attribute msg
hoverNegTranslateY40 =
    A.class "hover:-translate-y-40"


hoverNegTranslateY48 : Html.Attribute msg
hoverNegTranslateY48 =
    A.class "hover:-translate-y-48"


hoverNegTranslateY56 : Html.Attribute msg
hoverNegTranslateY56 =
    A.class "hover:-translate-y-56"


hoverNegTranslateY64 : Html.Attribute msg
hoverNegTranslateY64 =
    A.class "hover:-translate-y-64"


hoverNegTranslateYPx : Html.Attribute msg
hoverNegTranslateYPx =
    A.class "hover:-translate-y-px"


hoverNegTranslateYFull : Html.Attribute msg
hoverNegTranslateYFull =
    A.class "hover:-translate-y-full"


hoverNegTranslateY1over2 : Html.Attribute msg
hoverNegTranslateY1over2 =
    A.class "hover:-translate-y-1/2"


hoverTranslateY1over2 : Html.Attribute msg
hoverTranslateY1over2 =
    A.class "hover:translate-y-1/2"


hoverTranslateYFull : Html.Attribute msg
hoverTranslateYFull =
    A.class "hover:translate-y-full"


focusTranslateX0 : Html.Attribute msg
focusTranslateX0 =
    A.class "focus:translate-x-0"


focusTranslateX1 : Html.Attribute msg
focusTranslateX1 =
    A.class "focus:translate-x-1"


focusTranslateX2 : Html.Attribute msg
focusTranslateX2 =
    A.class "focus:translate-x-2"


focusTranslateX3 : Html.Attribute msg
focusTranslateX3 =
    A.class "focus:translate-x-3"


focusTranslateX4 : Html.Attribute msg
focusTranslateX4 =
    A.class "focus:translate-x-4"


focusTranslateX5 : Html.Attribute msg
focusTranslateX5 =
    A.class "focus:translate-x-5"


focusTranslateX6 : Html.Attribute msg
focusTranslateX6 =
    A.class "focus:translate-x-6"


focusTranslateX8 : Html.Attribute msg
focusTranslateX8 =
    A.class "focus:translate-x-8"


focusTranslateX10 : Html.Attribute msg
focusTranslateX10 =
    A.class "focus:translate-x-10"


focusTranslateX12 : Html.Attribute msg
focusTranslateX12 =
    A.class "focus:translate-x-12"


focusTranslateX16 : Html.Attribute msg
focusTranslateX16 =
    A.class "focus:translate-x-16"


focusTranslateX20 : Html.Attribute msg
focusTranslateX20 =
    A.class "focus:translate-x-20"


focusTranslateX24 : Html.Attribute msg
focusTranslateX24 =
    A.class "focus:translate-x-24"


focusTranslateX32 : Html.Attribute msg
focusTranslateX32 =
    A.class "focus:translate-x-32"


focusTranslateX40 : Html.Attribute msg
focusTranslateX40 =
    A.class "focus:translate-x-40"


focusTranslateX48 : Html.Attribute msg
focusTranslateX48 =
    A.class "focus:translate-x-48"


focusTranslateX56 : Html.Attribute msg
focusTranslateX56 =
    A.class "focus:translate-x-56"


focusTranslateX64 : Html.Attribute msg
focusTranslateX64 =
    A.class "focus:translate-x-64"


focusTranslateXPx : Html.Attribute msg
focusTranslateXPx =
    A.class "focus:translate-x-px"


focusNegTranslateX1 : Html.Attribute msg
focusNegTranslateX1 =
    A.class "focus:-translate-x-1"


focusNegTranslateX2 : Html.Attribute msg
focusNegTranslateX2 =
    A.class "focus:-translate-x-2"


focusNegTranslateX3 : Html.Attribute msg
focusNegTranslateX3 =
    A.class "focus:-translate-x-3"


focusNegTranslateX4 : Html.Attribute msg
focusNegTranslateX4 =
    A.class "focus:-translate-x-4"


focusNegTranslateX5 : Html.Attribute msg
focusNegTranslateX5 =
    A.class "focus:-translate-x-5"


focusNegTranslateX6 : Html.Attribute msg
focusNegTranslateX6 =
    A.class "focus:-translate-x-6"


focusNegTranslateX8 : Html.Attribute msg
focusNegTranslateX8 =
    A.class "focus:-translate-x-8"


focusNegTranslateX10 : Html.Attribute msg
focusNegTranslateX10 =
    A.class "focus:-translate-x-10"


focusNegTranslateX12 : Html.Attribute msg
focusNegTranslateX12 =
    A.class "focus:-translate-x-12"


focusNegTranslateX16 : Html.Attribute msg
focusNegTranslateX16 =
    A.class "focus:-translate-x-16"


focusNegTranslateX20 : Html.Attribute msg
focusNegTranslateX20 =
    A.class "focus:-translate-x-20"


focusNegTranslateX24 : Html.Attribute msg
focusNegTranslateX24 =
    A.class "focus:-translate-x-24"


focusNegTranslateX32 : Html.Attribute msg
focusNegTranslateX32 =
    A.class "focus:-translate-x-32"


focusNegTranslateX40 : Html.Attribute msg
focusNegTranslateX40 =
    A.class "focus:-translate-x-40"


focusNegTranslateX48 : Html.Attribute msg
focusNegTranslateX48 =
    A.class "focus:-translate-x-48"


focusNegTranslateX56 : Html.Attribute msg
focusNegTranslateX56 =
    A.class "focus:-translate-x-56"


focusNegTranslateX64 : Html.Attribute msg
focusNegTranslateX64 =
    A.class "focus:-translate-x-64"


focusNegTranslateXPx : Html.Attribute msg
focusNegTranslateXPx =
    A.class "focus:-translate-x-px"


focusNegTranslateXFull : Html.Attribute msg
focusNegTranslateXFull =
    A.class "focus:-translate-x-full"


focusNegTranslateX1over2 : Html.Attribute msg
focusNegTranslateX1over2 =
    A.class "focus:-translate-x-1/2"


focusTranslateX1over2 : Html.Attribute msg
focusTranslateX1over2 =
    A.class "focus:translate-x-1/2"


focusTranslateXFull : Html.Attribute msg
focusTranslateXFull =
    A.class "focus:translate-x-full"


focusTranslateY0 : Html.Attribute msg
focusTranslateY0 =
    A.class "focus:translate-y-0"


focusTranslateY1 : Html.Attribute msg
focusTranslateY1 =
    A.class "focus:translate-y-1"


focusTranslateY2 : Html.Attribute msg
focusTranslateY2 =
    A.class "focus:translate-y-2"


focusTranslateY3 : Html.Attribute msg
focusTranslateY3 =
    A.class "focus:translate-y-3"


focusTranslateY4 : Html.Attribute msg
focusTranslateY4 =
    A.class "focus:translate-y-4"


focusTranslateY5 : Html.Attribute msg
focusTranslateY5 =
    A.class "focus:translate-y-5"


focusTranslateY6 : Html.Attribute msg
focusTranslateY6 =
    A.class "focus:translate-y-6"


focusTranslateY8 : Html.Attribute msg
focusTranslateY8 =
    A.class "focus:translate-y-8"


focusTranslateY10 : Html.Attribute msg
focusTranslateY10 =
    A.class "focus:translate-y-10"


focusTranslateY12 : Html.Attribute msg
focusTranslateY12 =
    A.class "focus:translate-y-12"


focusTranslateY16 : Html.Attribute msg
focusTranslateY16 =
    A.class "focus:translate-y-16"


focusTranslateY20 : Html.Attribute msg
focusTranslateY20 =
    A.class "focus:translate-y-20"


focusTranslateY24 : Html.Attribute msg
focusTranslateY24 =
    A.class "focus:translate-y-24"


focusTranslateY32 : Html.Attribute msg
focusTranslateY32 =
    A.class "focus:translate-y-32"


focusTranslateY40 : Html.Attribute msg
focusTranslateY40 =
    A.class "focus:translate-y-40"


focusTranslateY48 : Html.Attribute msg
focusTranslateY48 =
    A.class "focus:translate-y-48"


focusTranslateY56 : Html.Attribute msg
focusTranslateY56 =
    A.class "focus:translate-y-56"


focusTranslateY64 : Html.Attribute msg
focusTranslateY64 =
    A.class "focus:translate-y-64"


focusTranslateYPx : Html.Attribute msg
focusTranslateYPx =
    A.class "focus:translate-y-px"


focusNegTranslateY1 : Html.Attribute msg
focusNegTranslateY1 =
    A.class "focus:-translate-y-1"


focusNegTranslateY2 : Html.Attribute msg
focusNegTranslateY2 =
    A.class "focus:-translate-y-2"


focusNegTranslateY3 : Html.Attribute msg
focusNegTranslateY3 =
    A.class "focus:-translate-y-3"


focusNegTranslateY4 : Html.Attribute msg
focusNegTranslateY4 =
    A.class "focus:-translate-y-4"


focusNegTranslateY5 : Html.Attribute msg
focusNegTranslateY5 =
    A.class "focus:-translate-y-5"


focusNegTranslateY6 : Html.Attribute msg
focusNegTranslateY6 =
    A.class "focus:-translate-y-6"


focusNegTranslateY8 : Html.Attribute msg
focusNegTranslateY8 =
    A.class "focus:-translate-y-8"


focusNegTranslateY10 : Html.Attribute msg
focusNegTranslateY10 =
    A.class "focus:-translate-y-10"


focusNegTranslateY12 : Html.Attribute msg
focusNegTranslateY12 =
    A.class "focus:-translate-y-12"


focusNegTranslateY16 : Html.Attribute msg
focusNegTranslateY16 =
    A.class "focus:-translate-y-16"


focusNegTranslateY20 : Html.Attribute msg
focusNegTranslateY20 =
    A.class "focus:-translate-y-20"


focusNegTranslateY24 : Html.Attribute msg
focusNegTranslateY24 =
    A.class "focus:-translate-y-24"


focusNegTranslateY32 : Html.Attribute msg
focusNegTranslateY32 =
    A.class "focus:-translate-y-32"


focusNegTranslateY40 : Html.Attribute msg
focusNegTranslateY40 =
    A.class "focus:-translate-y-40"


focusNegTranslateY48 : Html.Attribute msg
focusNegTranslateY48 =
    A.class "focus:-translate-y-48"


focusNegTranslateY56 : Html.Attribute msg
focusNegTranslateY56 =
    A.class "focus:-translate-y-56"


focusNegTranslateY64 : Html.Attribute msg
focusNegTranslateY64 =
    A.class "focus:-translate-y-64"


focusNegTranslateYPx : Html.Attribute msg
focusNegTranslateYPx =
    A.class "focus:-translate-y-px"


focusNegTranslateYFull : Html.Attribute msg
focusNegTranslateYFull =
    A.class "focus:-translate-y-full"


focusNegTranslateY1over2 : Html.Attribute msg
focusNegTranslateY1over2 =
    A.class "focus:-translate-y-1/2"


focusTranslateY1over2 : Html.Attribute msg
focusTranslateY1over2 =
    A.class "focus:translate-y-1/2"


focusTranslateYFull : Html.Attribute msg
focusTranslateYFull =
    A.class "focus:translate-y-full"


skewX0 : Html.Attribute msg
skewX0 =
    A.class "skew-x-0"


skewX3 : Html.Attribute msg
skewX3 =
    A.class "skew-x-3"


skewX6 : Html.Attribute msg
skewX6 =
    A.class "skew-x-6"


skewX12 : Html.Attribute msg
skewX12 =
    A.class "skew-x-12"


negSkewX12 : Html.Attribute msg
negSkewX12 =
    A.class "-skew-x-12"


negSkewX6 : Html.Attribute msg
negSkewX6 =
    A.class "-skew-x-6"


negSkewX3 : Html.Attribute msg
negSkewX3 =
    A.class "-skew-x-3"


skewY0 : Html.Attribute msg
skewY0 =
    A.class "skew-y-0"


skewY3 : Html.Attribute msg
skewY3 =
    A.class "skew-y-3"


skewY6 : Html.Attribute msg
skewY6 =
    A.class "skew-y-6"


skewY12 : Html.Attribute msg
skewY12 =
    A.class "skew-y-12"


negSkewY12 : Html.Attribute msg
negSkewY12 =
    A.class "-skew-y-12"


negSkewY6 : Html.Attribute msg
negSkewY6 =
    A.class "-skew-y-6"


negSkewY3 : Html.Attribute msg
negSkewY3 =
    A.class "-skew-y-3"


hoverSkewX0 : Html.Attribute msg
hoverSkewX0 =
    A.class "hover:skew-x-0"


hoverSkewX3 : Html.Attribute msg
hoverSkewX3 =
    A.class "hover:skew-x-3"


hoverSkewX6 : Html.Attribute msg
hoverSkewX6 =
    A.class "hover:skew-x-6"


hoverSkewX12 : Html.Attribute msg
hoverSkewX12 =
    A.class "hover:skew-x-12"


hoverNegSkewX12 : Html.Attribute msg
hoverNegSkewX12 =
    A.class "hover:-skew-x-12"


hoverNegSkewX6 : Html.Attribute msg
hoverNegSkewX6 =
    A.class "hover:-skew-x-6"


hoverNegSkewX3 : Html.Attribute msg
hoverNegSkewX3 =
    A.class "hover:-skew-x-3"


hoverSkewY0 : Html.Attribute msg
hoverSkewY0 =
    A.class "hover:skew-y-0"


hoverSkewY3 : Html.Attribute msg
hoverSkewY3 =
    A.class "hover:skew-y-3"


hoverSkewY6 : Html.Attribute msg
hoverSkewY6 =
    A.class "hover:skew-y-6"


hoverSkewY12 : Html.Attribute msg
hoverSkewY12 =
    A.class "hover:skew-y-12"


hoverNegSkewY12 : Html.Attribute msg
hoverNegSkewY12 =
    A.class "hover:-skew-y-12"


hoverNegSkewY6 : Html.Attribute msg
hoverNegSkewY6 =
    A.class "hover:-skew-y-6"


hoverNegSkewY3 : Html.Attribute msg
hoverNegSkewY3 =
    A.class "hover:-skew-y-3"


focusSkewX0 : Html.Attribute msg
focusSkewX0 =
    A.class "focus:skew-x-0"


focusSkewX3 : Html.Attribute msg
focusSkewX3 =
    A.class "focus:skew-x-3"


focusSkewX6 : Html.Attribute msg
focusSkewX6 =
    A.class "focus:skew-x-6"


focusSkewX12 : Html.Attribute msg
focusSkewX12 =
    A.class "focus:skew-x-12"


focusNegSkewX12 : Html.Attribute msg
focusNegSkewX12 =
    A.class "focus:-skew-x-12"


focusNegSkewX6 : Html.Attribute msg
focusNegSkewX6 =
    A.class "focus:-skew-x-6"


focusNegSkewX3 : Html.Attribute msg
focusNegSkewX3 =
    A.class "focus:-skew-x-3"


focusSkewY0 : Html.Attribute msg
focusSkewY0 =
    A.class "focus:skew-y-0"


focusSkewY3 : Html.Attribute msg
focusSkewY3 =
    A.class "focus:skew-y-3"


focusSkewY6 : Html.Attribute msg
focusSkewY6 =
    A.class "focus:skew-y-6"


focusSkewY12 : Html.Attribute msg
focusSkewY12 =
    A.class "focus:skew-y-12"


focusNegSkewY12 : Html.Attribute msg
focusNegSkewY12 =
    A.class "focus:-skew-y-12"


focusNegSkewY6 : Html.Attribute msg
focusNegSkewY6 =
    A.class "focus:-skew-y-6"


focusNegSkewY3 : Html.Attribute msg
focusNegSkewY3 =
    A.class "focus:-skew-y-3"


transitionNone : Html.Attribute msg
transitionNone =
    A.class "transition-none"


transitionAll : Html.Attribute msg
transitionAll =
    A.class "transition-all"


transition : Html.Attribute msg
transition =
    A.class "transition"


transitionColors : Html.Attribute msg
transitionColors =
    A.class "transition-colors"


transitionOpacity : Html.Attribute msg
transitionOpacity =
    A.class "transition-opacity"


transitionShadow : Html.Attribute msg
transitionShadow =
    A.class "transition-shadow"


transitionTransform : Html.Attribute msg
transitionTransform =
    A.class "transition-transform"


easeLinear : Html.Attribute msg
easeLinear =
    A.class "ease-linear"


easeIn : Html.Attribute msg
easeIn =
    A.class "ease-in"


easeOut : Html.Attribute msg
easeOut =
    A.class "ease-out"


easeInOut : Html.Attribute msg
easeInOut =
    A.class "ease-in-out"


duration75 : Html.Attribute msg
duration75 =
    A.class "duration-75"


duration100 : Html.Attribute msg
duration100 =
    A.class "duration-100"


duration150 : Html.Attribute msg
duration150 =
    A.class "duration-150"


duration200 : Html.Attribute msg
duration200 =
    A.class "duration-200"


duration300 : Html.Attribute msg
duration300 =
    A.class "duration-300"


duration500 : Html.Attribute msg
duration500 =
    A.class "duration-500"


duration700 : Html.Attribute msg
duration700 =
    A.class "duration-700"


duration1000 : Html.Attribute msg
duration1000 =
    A.class "duration-1000"


delay75 : Html.Attribute msg
delay75 =
    A.class "delay-75"


delay100 : Html.Attribute msg
delay100 =
    A.class "delay-100"


delay150 : Html.Attribute msg
delay150 =
    A.class "delay-150"


delay200 : Html.Attribute msg
delay200 =
    A.class "delay-200"


delay300 : Html.Attribute msg
delay300 =
    A.class "delay-300"


delay500 : Html.Attribute msg
delay500 =
    A.class "delay-500"


delay700 : Html.Attribute msg
delay700 =
    A.class "delay-700"


delay1000 : Html.Attribute msg
delay1000 =
    A.class "delay-1000"


animateNone : Html.Attribute msg
animateNone =
    A.class "animate-none"


animateSpin : Html.Attribute msg
animateSpin =
    A.class "animate-spin"


animatePing : Html.Attribute msg
animatePing =
    A.class "animate-ping"


animatePulse : Html.Attribute msg
animatePulse =
    A.class "animate-pulse"


animateBounce : Html.Attribute msg
animateBounce =
    A.class "animate-bounce"


smContainer : Html.Attribute msg
smContainer =
    A.class "sm:container"


smSpaceY0 : Html.Attribute msg
smSpaceY0 =
    A.class "sm:space-y-0"


smSpaceX0 : Html.Attribute msg
smSpaceX0 =
    A.class "sm:space-x-0"


smSpaceY1 : Html.Attribute msg
smSpaceY1 =
    A.class "sm:space-y-1"


smSpaceX1 : Html.Attribute msg
smSpaceX1 =
    A.class "sm:space-x-1"


smSpaceY2 : Html.Attribute msg
smSpaceY2 =
    A.class "sm:space-y-2"


smSpaceX2 : Html.Attribute msg
smSpaceX2 =
    A.class "sm:space-x-2"


smSpaceY3 : Html.Attribute msg
smSpaceY3 =
    A.class "sm:space-y-3"


smSpaceX3 : Html.Attribute msg
smSpaceX3 =
    A.class "sm:space-x-3"


smSpaceY4 : Html.Attribute msg
smSpaceY4 =
    A.class "sm:space-y-4"


smSpaceX4 : Html.Attribute msg
smSpaceX4 =
    A.class "sm:space-x-4"


smSpaceY5 : Html.Attribute msg
smSpaceY5 =
    A.class "sm:space-y-5"


smSpaceX5 : Html.Attribute msg
smSpaceX5 =
    A.class "sm:space-x-5"


smSpaceY6 : Html.Attribute msg
smSpaceY6 =
    A.class "sm:space-y-6"


smSpaceX6 : Html.Attribute msg
smSpaceX6 =
    A.class "sm:space-x-6"


smSpaceY8 : Html.Attribute msg
smSpaceY8 =
    A.class "sm:space-y-8"


smSpaceX8 : Html.Attribute msg
smSpaceX8 =
    A.class "sm:space-x-8"


smSpaceY10 : Html.Attribute msg
smSpaceY10 =
    A.class "sm:space-y-10"


smSpaceX10 : Html.Attribute msg
smSpaceX10 =
    A.class "sm:space-x-10"


smSpaceY12 : Html.Attribute msg
smSpaceY12 =
    A.class "sm:space-y-12"


smSpaceX12 : Html.Attribute msg
smSpaceX12 =
    A.class "sm:space-x-12"


smSpaceY16 : Html.Attribute msg
smSpaceY16 =
    A.class "sm:space-y-16"


smSpaceX16 : Html.Attribute msg
smSpaceX16 =
    A.class "sm:space-x-16"


smSpaceY20 : Html.Attribute msg
smSpaceY20 =
    A.class "sm:space-y-20"


smSpaceX20 : Html.Attribute msg
smSpaceX20 =
    A.class "sm:space-x-20"


smSpaceY24 : Html.Attribute msg
smSpaceY24 =
    A.class "sm:space-y-24"


smSpaceX24 : Html.Attribute msg
smSpaceX24 =
    A.class "sm:space-x-24"


smSpaceY32 : Html.Attribute msg
smSpaceY32 =
    A.class "sm:space-y-32"


smSpaceX32 : Html.Attribute msg
smSpaceX32 =
    A.class "sm:space-x-32"


smSpaceY40 : Html.Attribute msg
smSpaceY40 =
    A.class "sm:space-y-40"


smSpaceX40 : Html.Attribute msg
smSpaceX40 =
    A.class "sm:space-x-40"


smSpaceY48 : Html.Attribute msg
smSpaceY48 =
    A.class "sm:space-y-48"


smSpaceX48 : Html.Attribute msg
smSpaceX48 =
    A.class "sm:space-x-48"


smSpaceY56 : Html.Attribute msg
smSpaceY56 =
    A.class "sm:space-y-56"


smSpaceX56 : Html.Attribute msg
smSpaceX56 =
    A.class "sm:space-x-56"


smSpaceY64 : Html.Attribute msg
smSpaceY64 =
    A.class "sm:space-y-64"


smSpaceX64 : Html.Attribute msg
smSpaceX64 =
    A.class "sm:space-x-64"


smSpaceYPx : Html.Attribute msg
smSpaceYPx =
    A.class "sm:space-y-px"


smSpaceXPx : Html.Attribute msg
smSpaceXPx =
    A.class "sm:space-x-px"


smNegSpaceY1 : Html.Attribute msg
smNegSpaceY1 =
    A.class "sm:-space-y-1"


smNegSpaceX1 : Html.Attribute msg
smNegSpaceX1 =
    A.class "sm:-space-x-1"


smNegSpaceY2 : Html.Attribute msg
smNegSpaceY2 =
    A.class "sm:-space-y-2"


smNegSpaceX2 : Html.Attribute msg
smNegSpaceX2 =
    A.class "sm:-space-x-2"


smNegSpaceY3 : Html.Attribute msg
smNegSpaceY3 =
    A.class "sm:-space-y-3"


smNegSpaceX3 : Html.Attribute msg
smNegSpaceX3 =
    A.class "sm:-space-x-3"


smNegSpaceY4 : Html.Attribute msg
smNegSpaceY4 =
    A.class "sm:-space-y-4"


smNegSpaceX4 : Html.Attribute msg
smNegSpaceX4 =
    A.class "sm:-space-x-4"


smNegSpaceY5 : Html.Attribute msg
smNegSpaceY5 =
    A.class "sm:-space-y-5"


smNegSpaceX5 : Html.Attribute msg
smNegSpaceX5 =
    A.class "sm:-space-x-5"


smNegSpaceY6 : Html.Attribute msg
smNegSpaceY6 =
    A.class "sm:-space-y-6"


smNegSpaceX6 : Html.Attribute msg
smNegSpaceX6 =
    A.class "sm:-space-x-6"


smNegSpaceY8 : Html.Attribute msg
smNegSpaceY8 =
    A.class "sm:-space-y-8"


smNegSpaceX8 : Html.Attribute msg
smNegSpaceX8 =
    A.class "sm:-space-x-8"


smNegSpaceY10 : Html.Attribute msg
smNegSpaceY10 =
    A.class "sm:-space-y-10"


smNegSpaceX10 : Html.Attribute msg
smNegSpaceX10 =
    A.class "sm:-space-x-10"


smNegSpaceY12 : Html.Attribute msg
smNegSpaceY12 =
    A.class "sm:-space-y-12"


smNegSpaceX12 : Html.Attribute msg
smNegSpaceX12 =
    A.class "sm:-space-x-12"


smNegSpaceY16 : Html.Attribute msg
smNegSpaceY16 =
    A.class "sm:-space-y-16"


smNegSpaceX16 : Html.Attribute msg
smNegSpaceX16 =
    A.class "sm:-space-x-16"


smNegSpaceY20 : Html.Attribute msg
smNegSpaceY20 =
    A.class "sm:-space-y-20"


smNegSpaceX20 : Html.Attribute msg
smNegSpaceX20 =
    A.class "sm:-space-x-20"


smNegSpaceY24 : Html.Attribute msg
smNegSpaceY24 =
    A.class "sm:-space-y-24"


smNegSpaceX24 : Html.Attribute msg
smNegSpaceX24 =
    A.class "sm:-space-x-24"


smNegSpaceY32 : Html.Attribute msg
smNegSpaceY32 =
    A.class "sm:-space-y-32"


smNegSpaceX32 : Html.Attribute msg
smNegSpaceX32 =
    A.class "sm:-space-x-32"


smNegSpaceY40 : Html.Attribute msg
smNegSpaceY40 =
    A.class "sm:-space-y-40"


smNegSpaceX40 : Html.Attribute msg
smNegSpaceX40 =
    A.class "sm:-space-x-40"


smNegSpaceY48 : Html.Attribute msg
smNegSpaceY48 =
    A.class "sm:-space-y-48"


smNegSpaceX48 : Html.Attribute msg
smNegSpaceX48 =
    A.class "sm:-space-x-48"


smNegSpaceY56 : Html.Attribute msg
smNegSpaceY56 =
    A.class "sm:-space-y-56"


smNegSpaceX56 : Html.Attribute msg
smNegSpaceX56 =
    A.class "sm:-space-x-56"


smNegSpaceY64 : Html.Attribute msg
smNegSpaceY64 =
    A.class "sm:-space-y-64"


smNegSpaceX64 : Html.Attribute msg
smNegSpaceX64 =
    A.class "sm:-space-x-64"


smNegSpaceYPx : Html.Attribute msg
smNegSpaceYPx =
    A.class "sm:-space-y-px"


smNegSpaceXPx : Html.Attribute msg
smNegSpaceXPx =
    A.class "sm:-space-x-px"


smSpaceYReverse : Html.Attribute msg
smSpaceYReverse =
    A.class "sm:space-y-reverse"


smSpaceXReverse : Html.Attribute msg
smSpaceXReverse =
    A.class "sm:space-x-reverse"


smDivideY0 : Html.Attribute msg
smDivideY0 =
    A.class "sm:divide-y-0"


smDivideX0 : Html.Attribute msg
smDivideX0 =
    A.class "sm:divide-x-0"


smDivideY2 : Html.Attribute msg
smDivideY2 =
    A.class "sm:divide-y-2"


smDivideX2 : Html.Attribute msg
smDivideX2 =
    A.class "sm:divide-x-2"


smDivideY4 : Html.Attribute msg
smDivideY4 =
    A.class "sm:divide-y-4"


smDivideX4 : Html.Attribute msg
smDivideX4 =
    A.class "sm:divide-x-4"


smDivideY8 : Html.Attribute msg
smDivideY8 =
    A.class "sm:divide-y-8"


smDivideX8 : Html.Attribute msg
smDivideX8 =
    A.class "sm:divide-x-8"


smDivideY : Html.Attribute msg
smDivideY =
    A.class "sm:divide-y"


smDivideX : Html.Attribute msg
smDivideX =
    A.class "sm:divide-x"


smDivideYReverse : Html.Attribute msg
smDivideYReverse =
    A.class "sm:divide-y-reverse"


smDivideXReverse : Html.Attribute msg
smDivideXReverse =
    A.class "sm:divide-x-reverse"


smDivideTransparent : Html.Attribute msg
smDivideTransparent =
    A.class "sm:divide-transparent"


smDivideCurrent : Html.Attribute msg
smDivideCurrent =
    A.class "sm:divide-current"


smDivideBlack : Html.Attribute msg
smDivideBlack =
    A.class "sm:divide-black"


smDivideWhite : Html.Attribute msg
smDivideWhite =
    A.class "sm:divide-white"


smDivideGray100 : Html.Attribute msg
smDivideGray100 =
    A.class "sm:divide-gray-100"


smDivideGray200 : Html.Attribute msg
smDivideGray200 =
    A.class "sm:divide-gray-200"


smDivideGray300 : Html.Attribute msg
smDivideGray300 =
    A.class "sm:divide-gray-300"


smDivideGray400 : Html.Attribute msg
smDivideGray400 =
    A.class "sm:divide-gray-400"


smDivideGray500 : Html.Attribute msg
smDivideGray500 =
    A.class "sm:divide-gray-500"


smDivideGray600 : Html.Attribute msg
smDivideGray600 =
    A.class "sm:divide-gray-600"


smDivideGray700 : Html.Attribute msg
smDivideGray700 =
    A.class "sm:divide-gray-700"


smDivideGray800 : Html.Attribute msg
smDivideGray800 =
    A.class "sm:divide-gray-800"


smDivideGray900 : Html.Attribute msg
smDivideGray900 =
    A.class "sm:divide-gray-900"


smDivideRed100 : Html.Attribute msg
smDivideRed100 =
    A.class "sm:divide-red-100"


smDivideRed200 : Html.Attribute msg
smDivideRed200 =
    A.class "sm:divide-red-200"


smDivideRed300 : Html.Attribute msg
smDivideRed300 =
    A.class "sm:divide-red-300"


smDivideRed400 : Html.Attribute msg
smDivideRed400 =
    A.class "sm:divide-red-400"


smDivideRed500 : Html.Attribute msg
smDivideRed500 =
    A.class "sm:divide-red-500"


smDivideRed600 : Html.Attribute msg
smDivideRed600 =
    A.class "sm:divide-red-600"


smDivideRed700 : Html.Attribute msg
smDivideRed700 =
    A.class "sm:divide-red-700"


smDivideRed800 : Html.Attribute msg
smDivideRed800 =
    A.class "sm:divide-red-800"


smDivideRed900 : Html.Attribute msg
smDivideRed900 =
    A.class "sm:divide-red-900"


smDivideOrange100 : Html.Attribute msg
smDivideOrange100 =
    A.class "sm:divide-orange-100"


smDivideOrange200 : Html.Attribute msg
smDivideOrange200 =
    A.class "sm:divide-orange-200"


smDivideOrange300 : Html.Attribute msg
smDivideOrange300 =
    A.class "sm:divide-orange-300"


smDivideOrange400 : Html.Attribute msg
smDivideOrange400 =
    A.class "sm:divide-orange-400"


smDivideOrange500 : Html.Attribute msg
smDivideOrange500 =
    A.class "sm:divide-orange-500"


smDivideOrange600 : Html.Attribute msg
smDivideOrange600 =
    A.class "sm:divide-orange-600"


smDivideOrange700 : Html.Attribute msg
smDivideOrange700 =
    A.class "sm:divide-orange-700"


smDivideOrange800 : Html.Attribute msg
smDivideOrange800 =
    A.class "sm:divide-orange-800"


smDivideOrange900 : Html.Attribute msg
smDivideOrange900 =
    A.class "sm:divide-orange-900"


smDivideYellow100 : Html.Attribute msg
smDivideYellow100 =
    A.class "sm:divide-yellow-100"


smDivideYellow200 : Html.Attribute msg
smDivideYellow200 =
    A.class "sm:divide-yellow-200"


smDivideYellow300 : Html.Attribute msg
smDivideYellow300 =
    A.class "sm:divide-yellow-300"


smDivideYellow400 : Html.Attribute msg
smDivideYellow400 =
    A.class "sm:divide-yellow-400"


smDivideYellow500 : Html.Attribute msg
smDivideYellow500 =
    A.class "sm:divide-yellow-500"


smDivideYellow600 : Html.Attribute msg
smDivideYellow600 =
    A.class "sm:divide-yellow-600"


smDivideYellow700 : Html.Attribute msg
smDivideYellow700 =
    A.class "sm:divide-yellow-700"


smDivideYellow800 : Html.Attribute msg
smDivideYellow800 =
    A.class "sm:divide-yellow-800"


smDivideYellow900 : Html.Attribute msg
smDivideYellow900 =
    A.class "sm:divide-yellow-900"


smDivideGreen100 : Html.Attribute msg
smDivideGreen100 =
    A.class "sm:divide-green-100"


smDivideGreen200 : Html.Attribute msg
smDivideGreen200 =
    A.class "sm:divide-green-200"


smDivideGreen300 : Html.Attribute msg
smDivideGreen300 =
    A.class "sm:divide-green-300"


smDivideGreen400 : Html.Attribute msg
smDivideGreen400 =
    A.class "sm:divide-green-400"


smDivideGreen500 : Html.Attribute msg
smDivideGreen500 =
    A.class "sm:divide-green-500"


smDivideGreen600 : Html.Attribute msg
smDivideGreen600 =
    A.class "sm:divide-green-600"


smDivideGreen700 : Html.Attribute msg
smDivideGreen700 =
    A.class "sm:divide-green-700"


smDivideGreen800 : Html.Attribute msg
smDivideGreen800 =
    A.class "sm:divide-green-800"


smDivideGreen900 : Html.Attribute msg
smDivideGreen900 =
    A.class "sm:divide-green-900"


smDivideTeal100 : Html.Attribute msg
smDivideTeal100 =
    A.class "sm:divide-teal-100"


smDivideTeal200 : Html.Attribute msg
smDivideTeal200 =
    A.class "sm:divide-teal-200"


smDivideTeal300 : Html.Attribute msg
smDivideTeal300 =
    A.class "sm:divide-teal-300"


smDivideTeal400 : Html.Attribute msg
smDivideTeal400 =
    A.class "sm:divide-teal-400"


smDivideTeal500 : Html.Attribute msg
smDivideTeal500 =
    A.class "sm:divide-teal-500"


smDivideTeal600 : Html.Attribute msg
smDivideTeal600 =
    A.class "sm:divide-teal-600"


smDivideTeal700 : Html.Attribute msg
smDivideTeal700 =
    A.class "sm:divide-teal-700"


smDivideTeal800 : Html.Attribute msg
smDivideTeal800 =
    A.class "sm:divide-teal-800"


smDivideTeal900 : Html.Attribute msg
smDivideTeal900 =
    A.class "sm:divide-teal-900"


smDivideBlue100 : Html.Attribute msg
smDivideBlue100 =
    A.class "sm:divide-blue-100"


smDivideBlue200 : Html.Attribute msg
smDivideBlue200 =
    A.class "sm:divide-blue-200"


smDivideBlue300 : Html.Attribute msg
smDivideBlue300 =
    A.class "sm:divide-blue-300"


smDivideBlue400 : Html.Attribute msg
smDivideBlue400 =
    A.class "sm:divide-blue-400"


smDivideBlue500 : Html.Attribute msg
smDivideBlue500 =
    A.class "sm:divide-blue-500"


smDivideBlue600 : Html.Attribute msg
smDivideBlue600 =
    A.class "sm:divide-blue-600"


smDivideBlue700 : Html.Attribute msg
smDivideBlue700 =
    A.class "sm:divide-blue-700"


smDivideBlue800 : Html.Attribute msg
smDivideBlue800 =
    A.class "sm:divide-blue-800"


smDivideBlue900 : Html.Attribute msg
smDivideBlue900 =
    A.class "sm:divide-blue-900"


smDivideIndigo100 : Html.Attribute msg
smDivideIndigo100 =
    A.class "sm:divide-indigo-100"


smDivideIndigo200 : Html.Attribute msg
smDivideIndigo200 =
    A.class "sm:divide-indigo-200"


smDivideIndigo300 : Html.Attribute msg
smDivideIndigo300 =
    A.class "sm:divide-indigo-300"


smDivideIndigo400 : Html.Attribute msg
smDivideIndigo400 =
    A.class "sm:divide-indigo-400"


smDivideIndigo500 : Html.Attribute msg
smDivideIndigo500 =
    A.class "sm:divide-indigo-500"


smDivideIndigo600 : Html.Attribute msg
smDivideIndigo600 =
    A.class "sm:divide-indigo-600"


smDivideIndigo700 : Html.Attribute msg
smDivideIndigo700 =
    A.class "sm:divide-indigo-700"


smDivideIndigo800 : Html.Attribute msg
smDivideIndigo800 =
    A.class "sm:divide-indigo-800"


smDivideIndigo900 : Html.Attribute msg
smDivideIndigo900 =
    A.class "sm:divide-indigo-900"


smDividePurple100 : Html.Attribute msg
smDividePurple100 =
    A.class "sm:divide-purple-100"


smDividePurple200 : Html.Attribute msg
smDividePurple200 =
    A.class "sm:divide-purple-200"


smDividePurple300 : Html.Attribute msg
smDividePurple300 =
    A.class "sm:divide-purple-300"


smDividePurple400 : Html.Attribute msg
smDividePurple400 =
    A.class "sm:divide-purple-400"


smDividePurple500 : Html.Attribute msg
smDividePurple500 =
    A.class "sm:divide-purple-500"


smDividePurple600 : Html.Attribute msg
smDividePurple600 =
    A.class "sm:divide-purple-600"


smDividePurple700 : Html.Attribute msg
smDividePurple700 =
    A.class "sm:divide-purple-700"


smDividePurple800 : Html.Attribute msg
smDividePurple800 =
    A.class "sm:divide-purple-800"


smDividePurple900 : Html.Attribute msg
smDividePurple900 =
    A.class "sm:divide-purple-900"


smDividePink100 : Html.Attribute msg
smDividePink100 =
    A.class "sm:divide-pink-100"


smDividePink200 : Html.Attribute msg
smDividePink200 =
    A.class "sm:divide-pink-200"


smDividePink300 : Html.Attribute msg
smDividePink300 =
    A.class "sm:divide-pink-300"


smDividePink400 : Html.Attribute msg
smDividePink400 =
    A.class "sm:divide-pink-400"


smDividePink500 : Html.Attribute msg
smDividePink500 =
    A.class "sm:divide-pink-500"


smDividePink600 : Html.Attribute msg
smDividePink600 =
    A.class "sm:divide-pink-600"


smDividePink700 : Html.Attribute msg
smDividePink700 =
    A.class "sm:divide-pink-700"


smDividePink800 : Html.Attribute msg
smDividePink800 =
    A.class "sm:divide-pink-800"


smDividePink900 : Html.Attribute msg
smDividePink900 =
    A.class "sm:divide-pink-900"


smDivideSolid : Html.Attribute msg
smDivideSolid =
    A.class "sm:divide-solid"


smDivideDashed : Html.Attribute msg
smDivideDashed =
    A.class "sm:divide-dashed"


smDivideDotted : Html.Attribute msg
smDivideDotted =
    A.class "sm:divide-dotted"


smDivideDouble : Html.Attribute msg
smDivideDouble =
    A.class "sm:divide-double"


smDivideNone : Html.Attribute msg
smDivideNone =
    A.class "sm:divide-none"


smDivideOpacity0 : Html.Attribute msg
smDivideOpacity0 =
    A.class "sm:divide-opacity-0"


smDivideOpacity25 : Html.Attribute msg
smDivideOpacity25 =
    A.class "sm:divide-opacity-25"


smDivideOpacity50 : Html.Attribute msg
smDivideOpacity50 =
    A.class "sm:divide-opacity-50"


smDivideOpacity75 : Html.Attribute msg
smDivideOpacity75 =
    A.class "sm:divide-opacity-75"


smDivideOpacity100 : Html.Attribute msg
smDivideOpacity100 =
    A.class "sm:divide-opacity-100"


smSrOnly : Html.Attribute msg
smSrOnly =
    A.class "sm:sr-only"


smNotSrOnly : Html.Attribute msg
smNotSrOnly =
    A.class "sm:not-sr-only"


smFocusSrOnly : Html.Attribute msg
smFocusSrOnly =
    A.class "sm:focus:sr-only"


smFocusNotSrOnly : Html.Attribute msg
smFocusNotSrOnly =
    A.class "sm:focus:not-sr-only"


smAppearanceNone : Html.Attribute msg
smAppearanceNone =
    A.class "sm:appearance-none"


smBgFixed : Html.Attribute msg
smBgFixed =
    A.class "sm:bg-fixed"


smBgLocal : Html.Attribute msg
smBgLocal =
    A.class "sm:bg-local"


smBgScroll : Html.Attribute msg
smBgScroll =
    A.class "sm:bg-scroll"


smBgClipBorder : Html.Attribute msg
smBgClipBorder =
    A.class "sm:bg-clip-border"


smBgClipPadding : Html.Attribute msg
smBgClipPadding =
    A.class "sm:bg-clip-padding"


smBgClipContent : Html.Attribute msg
smBgClipContent =
    A.class "sm:bg-clip-content"


smBgClipText : Html.Attribute msg
smBgClipText =
    A.class "sm:bg-clip-text"


smBgTransparent : Html.Attribute msg
smBgTransparent =
    A.class "sm:bg-transparent"


smBgCurrent : Html.Attribute msg
smBgCurrent =
    A.class "sm:bg-current"


smBgBlack : Html.Attribute msg
smBgBlack =
    A.class "sm:bg-black"


smBgWhite : Html.Attribute msg
smBgWhite =
    A.class "sm:bg-white"


smBgGray100 : Html.Attribute msg
smBgGray100 =
    A.class "sm:bg-gray-100"


smBgGray200 : Html.Attribute msg
smBgGray200 =
    A.class "sm:bg-gray-200"


smBgGray300 : Html.Attribute msg
smBgGray300 =
    A.class "sm:bg-gray-300"


smBgGray400 : Html.Attribute msg
smBgGray400 =
    A.class "sm:bg-gray-400"


smBgGray500 : Html.Attribute msg
smBgGray500 =
    A.class "sm:bg-gray-500"


smBgGray600 : Html.Attribute msg
smBgGray600 =
    A.class "sm:bg-gray-600"


smBgGray700 : Html.Attribute msg
smBgGray700 =
    A.class "sm:bg-gray-700"


smBgGray800 : Html.Attribute msg
smBgGray800 =
    A.class "sm:bg-gray-800"


smBgGray900 : Html.Attribute msg
smBgGray900 =
    A.class "sm:bg-gray-900"


smBgRed100 : Html.Attribute msg
smBgRed100 =
    A.class "sm:bg-red-100"


smBgRed200 : Html.Attribute msg
smBgRed200 =
    A.class "sm:bg-red-200"


smBgRed300 : Html.Attribute msg
smBgRed300 =
    A.class "sm:bg-red-300"


smBgRed400 : Html.Attribute msg
smBgRed400 =
    A.class "sm:bg-red-400"


smBgRed500 : Html.Attribute msg
smBgRed500 =
    A.class "sm:bg-red-500"


smBgRed600 : Html.Attribute msg
smBgRed600 =
    A.class "sm:bg-red-600"


smBgRed700 : Html.Attribute msg
smBgRed700 =
    A.class "sm:bg-red-700"


smBgRed800 : Html.Attribute msg
smBgRed800 =
    A.class "sm:bg-red-800"


smBgRed900 : Html.Attribute msg
smBgRed900 =
    A.class "sm:bg-red-900"


smBgOrange100 : Html.Attribute msg
smBgOrange100 =
    A.class "sm:bg-orange-100"


smBgOrange200 : Html.Attribute msg
smBgOrange200 =
    A.class "sm:bg-orange-200"


smBgOrange300 : Html.Attribute msg
smBgOrange300 =
    A.class "sm:bg-orange-300"


smBgOrange400 : Html.Attribute msg
smBgOrange400 =
    A.class "sm:bg-orange-400"


smBgOrange500 : Html.Attribute msg
smBgOrange500 =
    A.class "sm:bg-orange-500"


smBgOrange600 : Html.Attribute msg
smBgOrange600 =
    A.class "sm:bg-orange-600"


smBgOrange700 : Html.Attribute msg
smBgOrange700 =
    A.class "sm:bg-orange-700"


smBgOrange800 : Html.Attribute msg
smBgOrange800 =
    A.class "sm:bg-orange-800"


smBgOrange900 : Html.Attribute msg
smBgOrange900 =
    A.class "sm:bg-orange-900"


smBgYellow100 : Html.Attribute msg
smBgYellow100 =
    A.class "sm:bg-yellow-100"


smBgYellow200 : Html.Attribute msg
smBgYellow200 =
    A.class "sm:bg-yellow-200"


smBgYellow300 : Html.Attribute msg
smBgYellow300 =
    A.class "sm:bg-yellow-300"


smBgYellow400 : Html.Attribute msg
smBgYellow400 =
    A.class "sm:bg-yellow-400"


smBgYellow500 : Html.Attribute msg
smBgYellow500 =
    A.class "sm:bg-yellow-500"


smBgYellow600 : Html.Attribute msg
smBgYellow600 =
    A.class "sm:bg-yellow-600"


smBgYellow700 : Html.Attribute msg
smBgYellow700 =
    A.class "sm:bg-yellow-700"


smBgYellow800 : Html.Attribute msg
smBgYellow800 =
    A.class "sm:bg-yellow-800"


smBgYellow900 : Html.Attribute msg
smBgYellow900 =
    A.class "sm:bg-yellow-900"


smBgGreen100 : Html.Attribute msg
smBgGreen100 =
    A.class "sm:bg-green-100"


smBgGreen200 : Html.Attribute msg
smBgGreen200 =
    A.class "sm:bg-green-200"


smBgGreen300 : Html.Attribute msg
smBgGreen300 =
    A.class "sm:bg-green-300"


smBgGreen400 : Html.Attribute msg
smBgGreen400 =
    A.class "sm:bg-green-400"


smBgGreen500 : Html.Attribute msg
smBgGreen500 =
    A.class "sm:bg-green-500"


smBgGreen600 : Html.Attribute msg
smBgGreen600 =
    A.class "sm:bg-green-600"


smBgGreen700 : Html.Attribute msg
smBgGreen700 =
    A.class "sm:bg-green-700"


smBgGreen800 : Html.Attribute msg
smBgGreen800 =
    A.class "sm:bg-green-800"


smBgGreen900 : Html.Attribute msg
smBgGreen900 =
    A.class "sm:bg-green-900"


smBgTeal100 : Html.Attribute msg
smBgTeal100 =
    A.class "sm:bg-teal-100"


smBgTeal200 : Html.Attribute msg
smBgTeal200 =
    A.class "sm:bg-teal-200"


smBgTeal300 : Html.Attribute msg
smBgTeal300 =
    A.class "sm:bg-teal-300"


smBgTeal400 : Html.Attribute msg
smBgTeal400 =
    A.class "sm:bg-teal-400"


smBgTeal500 : Html.Attribute msg
smBgTeal500 =
    A.class "sm:bg-teal-500"


smBgTeal600 : Html.Attribute msg
smBgTeal600 =
    A.class "sm:bg-teal-600"


smBgTeal700 : Html.Attribute msg
smBgTeal700 =
    A.class "sm:bg-teal-700"


smBgTeal800 : Html.Attribute msg
smBgTeal800 =
    A.class "sm:bg-teal-800"


smBgTeal900 : Html.Attribute msg
smBgTeal900 =
    A.class "sm:bg-teal-900"


smBgBlue100 : Html.Attribute msg
smBgBlue100 =
    A.class "sm:bg-blue-100"


smBgBlue200 : Html.Attribute msg
smBgBlue200 =
    A.class "sm:bg-blue-200"


smBgBlue300 : Html.Attribute msg
smBgBlue300 =
    A.class "sm:bg-blue-300"


smBgBlue400 : Html.Attribute msg
smBgBlue400 =
    A.class "sm:bg-blue-400"


smBgBlue500 : Html.Attribute msg
smBgBlue500 =
    A.class "sm:bg-blue-500"


smBgBlue600 : Html.Attribute msg
smBgBlue600 =
    A.class "sm:bg-blue-600"


smBgBlue700 : Html.Attribute msg
smBgBlue700 =
    A.class "sm:bg-blue-700"


smBgBlue800 : Html.Attribute msg
smBgBlue800 =
    A.class "sm:bg-blue-800"


smBgBlue900 : Html.Attribute msg
smBgBlue900 =
    A.class "sm:bg-blue-900"


smBgIndigo100 : Html.Attribute msg
smBgIndigo100 =
    A.class "sm:bg-indigo-100"


smBgIndigo200 : Html.Attribute msg
smBgIndigo200 =
    A.class "sm:bg-indigo-200"


smBgIndigo300 : Html.Attribute msg
smBgIndigo300 =
    A.class "sm:bg-indigo-300"


smBgIndigo400 : Html.Attribute msg
smBgIndigo400 =
    A.class "sm:bg-indigo-400"


smBgIndigo500 : Html.Attribute msg
smBgIndigo500 =
    A.class "sm:bg-indigo-500"


smBgIndigo600 : Html.Attribute msg
smBgIndigo600 =
    A.class "sm:bg-indigo-600"


smBgIndigo700 : Html.Attribute msg
smBgIndigo700 =
    A.class "sm:bg-indigo-700"


smBgIndigo800 : Html.Attribute msg
smBgIndigo800 =
    A.class "sm:bg-indigo-800"


smBgIndigo900 : Html.Attribute msg
smBgIndigo900 =
    A.class "sm:bg-indigo-900"


smBgPurple100 : Html.Attribute msg
smBgPurple100 =
    A.class "sm:bg-purple-100"


smBgPurple200 : Html.Attribute msg
smBgPurple200 =
    A.class "sm:bg-purple-200"


smBgPurple300 : Html.Attribute msg
smBgPurple300 =
    A.class "sm:bg-purple-300"


smBgPurple400 : Html.Attribute msg
smBgPurple400 =
    A.class "sm:bg-purple-400"


smBgPurple500 : Html.Attribute msg
smBgPurple500 =
    A.class "sm:bg-purple-500"


smBgPurple600 : Html.Attribute msg
smBgPurple600 =
    A.class "sm:bg-purple-600"


smBgPurple700 : Html.Attribute msg
smBgPurple700 =
    A.class "sm:bg-purple-700"


smBgPurple800 : Html.Attribute msg
smBgPurple800 =
    A.class "sm:bg-purple-800"


smBgPurple900 : Html.Attribute msg
smBgPurple900 =
    A.class "sm:bg-purple-900"


smBgPink100 : Html.Attribute msg
smBgPink100 =
    A.class "sm:bg-pink-100"


smBgPink200 : Html.Attribute msg
smBgPink200 =
    A.class "sm:bg-pink-200"


smBgPink300 : Html.Attribute msg
smBgPink300 =
    A.class "sm:bg-pink-300"


smBgPink400 : Html.Attribute msg
smBgPink400 =
    A.class "sm:bg-pink-400"


smBgPink500 : Html.Attribute msg
smBgPink500 =
    A.class "sm:bg-pink-500"


smBgPink600 : Html.Attribute msg
smBgPink600 =
    A.class "sm:bg-pink-600"


smBgPink700 : Html.Attribute msg
smBgPink700 =
    A.class "sm:bg-pink-700"


smBgPink800 : Html.Attribute msg
smBgPink800 =
    A.class "sm:bg-pink-800"


smBgPink900 : Html.Attribute msg
smBgPink900 =
    A.class "sm:bg-pink-900"


smHoverBgTransparent : Html.Attribute msg
smHoverBgTransparent =
    A.class "sm:hover:bg-transparent"


smHoverBgCurrent : Html.Attribute msg
smHoverBgCurrent =
    A.class "sm:hover:bg-current"


smHoverBgBlack : Html.Attribute msg
smHoverBgBlack =
    A.class "sm:hover:bg-black"


smHoverBgWhite : Html.Attribute msg
smHoverBgWhite =
    A.class "sm:hover:bg-white"


smHoverBgGray100 : Html.Attribute msg
smHoverBgGray100 =
    A.class "sm:hover:bg-gray-100"


smHoverBgGray200 : Html.Attribute msg
smHoverBgGray200 =
    A.class "sm:hover:bg-gray-200"


smHoverBgGray300 : Html.Attribute msg
smHoverBgGray300 =
    A.class "sm:hover:bg-gray-300"


smHoverBgGray400 : Html.Attribute msg
smHoverBgGray400 =
    A.class "sm:hover:bg-gray-400"


smHoverBgGray500 : Html.Attribute msg
smHoverBgGray500 =
    A.class "sm:hover:bg-gray-500"


smHoverBgGray600 : Html.Attribute msg
smHoverBgGray600 =
    A.class "sm:hover:bg-gray-600"


smHoverBgGray700 : Html.Attribute msg
smHoverBgGray700 =
    A.class "sm:hover:bg-gray-700"


smHoverBgGray800 : Html.Attribute msg
smHoverBgGray800 =
    A.class "sm:hover:bg-gray-800"


smHoverBgGray900 : Html.Attribute msg
smHoverBgGray900 =
    A.class "sm:hover:bg-gray-900"


smHoverBgRed100 : Html.Attribute msg
smHoverBgRed100 =
    A.class "sm:hover:bg-red-100"


smHoverBgRed200 : Html.Attribute msg
smHoverBgRed200 =
    A.class "sm:hover:bg-red-200"


smHoverBgRed300 : Html.Attribute msg
smHoverBgRed300 =
    A.class "sm:hover:bg-red-300"


smHoverBgRed400 : Html.Attribute msg
smHoverBgRed400 =
    A.class "sm:hover:bg-red-400"


smHoverBgRed500 : Html.Attribute msg
smHoverBgRed500 =
    A.class "sm:hover:bg-red-500"


smHoverBgRed600 : Html.Attribute msg
smHoverBgRed600 =
    A.class "sm:hover:bg-red-600"


smHoverBgRed700 : Html.Attribute msg
smHoverBgRed700 =
    A.class "sm:hover:bg-red-700"


smHoverBgRed800 : Html.Attribute msg
smHoverBgRed800 =
    A.class "sm:hover:bg-red-800"


smHoverBgRed900 : Html.Attribute msg
smHoverBgRed900 =
    A.class "sm:hover:bg-red-900"


smHoverBgOrange100 : Html.Attribute msg
smHoverBgOrange100 =
    A.class "sm:hover:bg-orange-100"


smHoverBgOrange200 : Html.Attribute msg
smHoverBgOrange200 =
    A.class "sm:hover:bg-orange-200"


smHoverBgOrange300 : Html.Attribute msg
smHoverBgOrange300 =
    A.class "sm:hover:bg-orange-300"


smHoverBgOrange400 : Html.Attribute msg
smHoverBgOrange400 =
    A.class "sm:hover:bg-orange-400"


smHoverBgOrange500 : Html.Attribute msg
smHoverBgOrange500 =
    A.class "sm:hover:bg-orange-500"


smHoverBgOrange600 : Html.Attribute msg
smHoverBgOrange600 =
    A.class "sm:hover:bg-orange-600"


smHoverBgOrange700 : Html.Attribute msg
smHoverBgOrange700 =
    A.class "sm:hover:bg-orange-700"


smHoverBgOrange800 : Html.Attribute msg
smHoverBgOrange800 =
    A.class "sm:hover:bg-orange-800"


smHoverBgOrange900 : Html.Attribute msg
smHoverBgOrange900 =
    A.class "sm:hover:bg-orange-900"


smHoverBgYellow100 : Html.Attribute msg
smHoverBgYellow100 =
    A.class "sm:hover:bg-yellow-100"


smHoverBgYellow200 : Html.Attribute msg
smHoverBgYellow200 =
    A.class "sm:hover:bg-yellow-200"


smHoverBgYellow300 : Html.Attribute msg
smHoverBgYellow300 =
    A.class "sm:hover:bg-yellow-300"


smHoverBgYellow400 : Html.Attribute msg
smHoverBgYellow400 =
    A.class "sm:hover:bg-yellow-400"


smHoverBgYellow500 : Html.Attribute msg
smHoverBgYellow500 =
    A.class "sm:hover:bg-yellow-500"


smHoverBgYellow600 : Html.Attribute msg
smHoverBgYellow600 =
    A.class "sm:hover:bg-yellow-600"


smHoverBgYellow700 : Html.Attribute msg
smHoverBgYellow700 =
    A.class "sm:hover:bg-yellow-700"


smHoverBgYellow800 : Html.Attribute msg
smHoverBgYellow800 =
    A.class "sm:hover:bg-yellow-800"


smHoverBgYellow900 : Html.Attribute msg
smHoverBgYellow900 =
    A.class "sm:hover:bg-yellow-900"


smHoverBgGreen100 : Html.Attribute msg
smHoverBgGreen100 =
    A.class "sm:hover:bg-green-100"


smHoverBgGreen200 : Html.Attribute msg
smHoverBgGreen200 =
    A.class "sm:hover:bg-green-200"


smHoverBgGreen300 : Html.Attribute msg
smHoverBgGreen300 =
    A.class "sm:hover:bg-green-300"


smHoverBgGreen400 : Html.Attribute msg
smHoverBgGreen400 =
    A.class "sm:hover:bg-green-400"


smHoverBgGreen500 : Html.Attribute msg
smHoverBgGreen500 =
    A.class "sm:hover:bg-green-500"


smHoverBgGreen600 : Html.Attribute msg
smHoverBgGreen600 =
    A.class "sm:hover:bg-green-600"


smHoverBgGreen700 : Html.Attribute msg
smHoverBgGreen700 =
    A.class "sm:hover:bg-green-700"


smHoverBgGreen800 : Html.Attribute msg
smHoverBgGreen800 =
    A.class "sm:hover:bg-green-800"


smHoverBgGreen900 : Html.Attribute msg
smHoverBgGreen900 =
    A.class "sm:hover:bg-green-900"


smHoverBgTeal100 : Html.Attribute msg
smHoverBgTeal100 =
    A.class "sm:hover:bg-teal-100"


smHoverBgTeal200 : Html.Attribute msg
smHoverBgTeal200 =
    A.class "sm:hover:bg-teal-200"


smHoverBgTeal300 : Html.Attribute msg
smHoverBgTeal300 =
    A.class "sm:hover:bg-teal-300"


smHoverBgTeal400 : Html.Attribute msg
smHoverBgTeal400 =
    A.class "sm:hover:bg-teal-400"


smHoverBgTeal500 : Html.Attribute msg
smHoverBgTeal500 =
    A.class "sm:hover:bg-teal-500"


smHoverBgTeal600 : Html.Attribute msg
smHoverBgTeal600 =
    A.class "sm:hover:bg-teal-600"


smHoverBgTeal700 : Html.Attribute msg
smHoverBgTeal700 =
    A.class "sm:hover:bg-teal-700"


smHoverBgTeal800 : Html.Attribute msg
smHoverBgTeal800 =
    A.class "sm:hover:bg-teal-800"


smHoverBgTeal900 : Html.Attribute msg
smHoverBgTeal900 =
    A.class "sm:hover:bg-teal-900"


smHoverBgBlue100 : Html.Attribute msg
smHoverBgBlue100 =
    A.class "sm:hover:bg-blue-100"


smHoverBgBlue200 : Html.Attribute msg
smHoverBgBlue200 =
    A.class "sm:hover:bg-blue-200"


smHoverBgBlue300 : Html.Attribute msg
smHoverBgBlue300 =
    A.class "sm:hover:bg-blue-300"


smHoverBgBlue400 : Html.Attribute msg
smHoverBgBlue400 =
    A.class "sm:hover:bg-blue-400"


smHoverBgBlue500 : Html.Attribute msg
smHoverBgBlue500 =
    A.class "sm:hover:bg-blue-500"


smHoverBgBlue600 : Html.Attribute msg
smHoverBgBlue600 =
    A.class "sm:hover:bg-blue-600"


smHoverBgBlue700 : Html.Attribute msg
smHoverBgBlue700 =
    A.class "sm:hover:bg-blue-700"


smHoverBgBlue800 : Html.Attribute msg
smHoverBgBlue800 =
    A.class "sm:hover:bg-blue-800"


smHoverBgBlue900 : Html.Attribute msg
smHoverBgBlue900 =
    A.class "sm:hover:bg-blue-900"


smHoverBgIndigo100 : Html.Attribute msg
smHoverBgIndigo100 =
    A.class "sm:hover:bg-indigo-100"


smHoverBgIndigo200 : Html.Attribute msg
smHoverBgIndigo200 =
    A.class "sm:hover:bg-indigo-200"


smHoverBgIndigo300 : Html.Attribute msg
smHoverBgIndigo300 =
    A.class "sm:hover:bg-indigo-300"


smHoverBgIndigo400 : Html.Attribute msg
smHoverBgIndigo400 =
    A.class "sm:hover:bg-indigo-400"


smHoverBgIndigo500 : Html.Attribute msg
smHoverBgIndigo500 =
    A.class "sm:hover:bg-indigo-500"


smHoverBgIndigo600 : Html.Attribute msg
smHoverBgIndigo600 =
    A.class "sm:hover:bg-indigo-600"


smHoverBgIndigo700 : Html.Attribute msg
smHoverBgIndigo700 =
    A.class "sm:hover:bg-indigo-700"


smHoverBgIndigo800 : Html.Attribute msg
smHoverBgIndigo800 =
    A.class "sm:hover:bg-indigo-800"


smHoverBgIndigo900 : Html.Attribute msg
smHoverBgIndigo900 =
    A.class "sm:hover:bg-indigo-900"


smHoverBgPurple100 : Html.Attribute msg
smHoverBgPurple100 =
    A.class "sm:hover:bg-purple-100"


smHoverBgPurple200 : Html.Attribute msg
smHoverBgPurple200 =
    A.class "sm:hover:bg-purple-200"


smHoverBgPurple300 : Html.Attribute msg
smHoverBgPurple300 =
    A.class "sm:hover:bg-purple-300"


smHoverBgPurple400 : Html.Attribute msg
smHoverBgPurple400 =
    A.class "sm:hover:bg-purple-400"


smHoverBgPurple500 : Html.Attribute msg
smHoverBgPurple500 =
    A.class "sm:hover:bg-purple-500"


smHoverBgPurple600 : Html.Attribute msg
smHoverBgPurple600 =
    A.class "sm:hover:bg-purple-600"


smHoverBgPurple700 : Html.Attribute msg
smHoverBgPurple700 =
    A.class "sm:hover:bg-purple-700"


smHoverBgPurple800 : Html.Attribute msg
smHoverBgPurple800 =
    A.class "sm:hover:bg-purple-800"


smHoverBgPurple900 : Html.Attribute msg
smHoverBgPurple900 =
    A.class "sm:hover:bg-purple-900"


smHoverBgPink100 : Html.Attribute msg
smHoverBgPink100 =
    A.class "sm:hover:bg-pink-100"


smHoverBgPink200 : Html.Attribute msg
smHoverBgPink200 =
    A.class "sm:hover:bg-pink-200"


smHoverBgPink300 : Html.Attribute msg
smHoverBgPink300 =
    A.class "sm:hover:bg-pink-300"


smHoverBgPink400 : Html.Attribute msg
smHoverBgPink400 =
    A.class "sm:hover:bg-pink-400"


smHoverBgPink500 : Html.Attribute msg
smHoverBgPink500 =
    A.class "sm:hover:bg-pink-500"


smHoverBgPink600 : Html.Attribute msg
smHoverBgPink600 =
    A.class "sm:hover:bg-pink-600"


smHoverBgPink700 : Html.Attribute msg
smHoverBgPink700 =
    A.class "sm:hover:bg-pink-700"


smHoverBgPink800 : Html.Attribute msg
smHoverBgPink800 =
    A.class "sm:hover:bg-pink-800"


smHoverBgPink900 : Html.Attribute msg
smHoverBgPink900 =
    A.class "sm:hover:bg-pink-900"


smFocusBgTransparent : Html.Attribute msg
smFocusBgTransparent =
    A.class "sm:focus:bg-transparent"


smFocusBgCurrent : Html.Attribute msg
smFocusBgCurrent =
    A.class "sm:focus:bg-current"


smFocusBgBlack : Html.Attribute msg
smFocusBgBlack =
    A.class "sm:focus:bg-black"


smFocusBgWhite : Html.Attribute msg
smFocusBgWhite =
    A.class "sm:focus:bg-white"


smFocusBgGray100 : Html.Attribute msg
smFocusBgGray100 =
    A.class "sm:focus:bg-gray-100"


smFocusBgGray200 : Html.Attribute msg
smFocusBgGray200 =
    A.class "sm:focus:bg-gray-200"


smFocusBgGray300 : Html.Attribute msg
smFocusBgGray300 =
    A.class "sm:focus:bg-gray-300"


smFocusBgGray400 : Html.Attribute msg
smFocusBgGray400 =
    A.class "sm:focus:bg-gray-400"


smFocusBgGray500 : Html.Attribute msg
smFocusBgGray500 =
    A.class "sm:focus:bg-gray-500"


smFocusBgGray600 : Html.Attribute msg
smFocusBgGray600 =
    A.class "sm:focus:bg-gray-600"


smFocusBgGray700 : Html.Attribute msg
smFocusBgGray700 =
    A.class "sm:focus:bg-gray-700"


smFocusBgGray800 : Html.Attribute msg
smFocusBgGray800 =
    A.class "sm:focus:bg-gray-800"


smFocusBgGray900 : Html.Attribute msg
smFocusBgGray900 =
    A.class "sm:focus:bg-gray-900"


smFocusBgRed100 : Html.Attribute msg
smFocusBgRed100 =
    A.class "sm:focus:bg-red-100"


smFocusBgRed200 : Html.Attribute msg
smFocusBgRed200 =
    A.class "sm:focus:bg-red-200"


smFocusBgRed300 : Html.Attribute msg
smFocusBgRed300 =
    A.class "sm:focus:bg-red-300"


smFocusBgRed400 : Html.Attribute msg
smFocusBgRed400 =
    A.class "sm:focus:bg-red-400"


smFocusBgRed500 : Html.Attribute msg
smFocusBgRed500 =
    A.class "sm:focus:bg-red-500"


smFocusBgRed600 : Html.Attribute msg
smFocusBgRed600 =
    A.class "sm:focus:bg-red-600"


smFocusBgRed700 : Html.Attribute msg
smFocusBgRed700 =
    A.class "sm:focus:bg-red-700"


smFocusBgRed800 : Html.Attribute msg
smFocusBgRed800 =
    A.class "sm:focus:bg-red-800"


smFocusBgRed900 : Html.Attribute msg
smFocusBgRed900 =
    A.class "sm:focus:bg-red-900"


smFocusBgOrange100 : Html.Attribute msg
smFocusBgOrange100 =
    A.class "sm:focus:bg-orange-100"


smFocusBgOrange200 : Html.Attribute msg
smFocusBgOrange200 =
    A.class "sm:focus:bg-orange-200"


smFocusBgOrange300 : Html.Attribute msg
smFocusBgOrange300 =
    A.class "sm:focus:bg-orange-300"


smFocusBgOrange400 : Html.Attribute msg
smFocusBgOrange400 =
    A.class "sm:focus:bg-orange-400"


smFocusBgOrange500 : Html.Attribute msg
smFocusBgOrange500 =
    A.class "sm:focus:bg-orange-500"


smFocusBgOrange600 : Html.Attribute msg
smFocusBgOrange600 =
    A.class "sm:focus:bg-orange-600"


smFocusBgOrange700 : Html.Attribute msg
smFocusBgOrange700 =
    A.class "sm:focus:bg-orange-700"


smFocusBgOrange800 : Html.Attribute msg
smFocusBgOrange800 =
    A.class "sm:focus:bg-orange-800"


smFocusBgOrange900 : Html.Attribute msg
smFocusBgOrange900 =
    A.class "sm:focus:bg-orange-900"


smFocusBgYellow100 : Html.Attribute msg
smFocusBgYellow100 =
    A.class "sm:focus:bg-yellow-100"


smFocusBgYellow200 : Html.Attribute msg
smFocusBgYellow200 =
    A.class "sm:focus:bg-yellow-200"


smFocusBgYellow300 : Html.Attribute msg
smFocusBgYellow300 =
    A.class "sm:focus:bg-yellow-300"


smFocusBgYellow400 : Html.Attribute msg
smFocusBgYellow400 =
    A.class "sm:focus:bg-yellow-400"


smFocusBgYellow500 : Html.Attribute msg
smFocusBgYellow500 =
    A.class "sm:focus:bg-yellow-500"


smFocusBgYellow600 : Html.Attribute msg
smFocusBgYellow600 =
    A.class "sm:focus:bg-yellow-600"


smFocusBgYellow700 : Html.Attribute msg
smFocusBgYellow700 =
    A.class "sm:focus:bg-yellow-700"


smFocusBgYellow800 : Html.Attribute msg
smFocusBgYellow800 =
    A.class "sm:focus:bg-yellow-800"


smFocusBgYellow900 : Html.Attribute msg
smFocusBgYellow900 =
    A.class "sm:focus:bg-yellow-900"


smFocusBgGreen100 : Html.Attribute msg
smFocusBgGreen100 =
    A.class "sm:focus:bg-green-100"


smFocusBgGreen200 : Html.Attribute msg
smFocusBgGreen200 =
    A.class "sm:focus:bg-green-200"


smFocusBgGreen300 : Html.Attribute msg
smFocusBgGreen300 =
    A.class "sm:focus:bg-green-300"


smFocusBgGreen400 : Html.Attribute msg
smFocusBgGreen400 =
    A.class "sm:focus:bg-green-400"


smFocusBgGreen500 : Html.Attribute msg
smFocusBgGreen500 =
    A.class "sm:focus:bg-green-500"


smFocusBgGreen600 : Html.Attribute msg
smFocusBgGreen600 =
    A.class "sm:focus:bg-green-600"


smFocusBgGreen700 : Html.Attribute msg
smFocusBgGreen700 =
    A.class "sm:focus:bg-green-700"


smFocusBgGreen800 : Html.Attribute msg
smFocusBgGreen800 =
    A.class "sm:focus:bg-green-800"


smFocusBgGreen900 : Html.Attribute msg
smFocusBgGreen900 =
    A.class "sm:focus:bg-green-900"


smFocusBgTeal100 : Html.Attribute msg
smFocusBgTeal100 =
    A.class "sm:focus:bg-teal-100"


smFocusBgTeal200 : Html.Attribute msg
smFocusBgTeal200 =
    A.class "sm:focus:bg-teal-200"


smFocusBgTeal300 : Html.Attribute msg
smFocusBgTeal300 =
    A.class "sm:focus:bg-teal-300"


smFocusBgTeal400 : Html.Attribute msg
smFocusBgTeal400 =
    A.class "sm:focus:bg-teal-400"


smFocusBgTeal500 : Html.Attribute msg
smFocusBgTeal500 =
    A.class "sm:focus:bg-teal-500"


smFocusBgTeal600 : Html.Attribute msg
smFocusBgTeal600 =
    A.class "sm:focus:bg-teal-600"


smFocusBgTeal700 : Html.Attribute msg
smFocusBgTeal700 =
    A.class "sm:focus:bg-teal-700"


smFocusBgTeal800 : Html.Attribute msg
smFocusBgTeal800 =
    A.class "sm:focus:bg-teal-800"


smFocusBgTeal900 : Html.Attribute msg
smFocusBgTeal900 =
    A.class "sm:focus:bg-teal-900"


smFocusBgBlue100 : Html.Attribute msg
smFocusBgBlue100 =
    A.class "sm:focus:bg-blue-100"


smFocusBgBlue200 : Html.Attribute msg
smFocusBgBlue200 =
    A.class "sm:focus:bg-blue-200"


smFocusBgBlue300 : Html.Attribute msg
smFocusBgBlue300 =
    A.class "sm:focus:bg-blue-300"


smFocusBgBlue400 : Html.Attribute msg
smFocusBgBlue400 =
    A.class "sm:focus:bg-blue-400"


smFocusBgBlue500 : Html.Attribute msg
smFocusBgBlue500 =
    A.class "sm:focus:bg-blue-500"


smFocusBgBlue600 : Html.Attribute msg
smFocusBgBlue600 =
    A.class "sm:focus:bg-blue-600"


smFocusBgBlue700 : Html.Attribute msg
smFocusBgBlue700 =
    A.class "sm:focus:bg-blue-700"


smFocusBgBlue800 : Html.Attribute msg
smFocusBgBlue800 =
    A.class "sm:focus:bg-blue-800"


smFocusBgBlue900 : Html.Attribute msg
smFocusBgBlue900 =
    A.class "sm:focus:bg-blue-900"


smFocusBgIndigo100 : Html.Attribute msg
smFocusBgIndigo100 =
    A.class "sm:focus:bg-indigo-100"


smFocusBgIndigo200 : Html.Attribute msg
smFocusBgIndigo200 =
    A.class "sm:focus:bg-indigo-200"


smFocusBgIndigo300 : Html.Attribute msg
smFocusBgIndigo300 =
    A.class "sm:focus:bg-indigo-300"


smFocusBgIndigo400 : Html.Attribute msg
smFocusBgIndigo400 =
    A.class "sm:focus:bg-indigo-400"


smFocusBgIndigo500 : Html.Attribute msg
smFocusBgIndigo500 =
    A.class "sm:focus:bg-indigo-500"


smFocusBgIndigo600 : Html.Attribute msg
smFocusBgIndigo600 =
    A.class "sm:focus:bg-indigo-600"


smFocusBgIndigo700 : Html.Attribute msg
smFocusBgIndigo700 =
    A.class "sm:focus:bg-indigo-700"


smFocusBgIndigo800 : Html.Attribute msg
smFocusBgIndigo800 =
    A.class "sm:focus:bg-indigo-800"


smFocusBgIndigo900 : Html.Attribute msg
smFocusBgIndigo900 =
    A.class "sm:focus:bg-indigo-900"


smFocusBgPurple100 : Html.Attribute msg
smFocusBgPurple100 =
    A.class "sm:focus:bg-purple-100"


smFocusBgPurple200 : Html.Attribute msg
smFocusBgPurple200 =
    A.class "sm:focus:bg-purple-200"


smFocusBgPurple300 : Html.Attribute msg
smFocusBgPurple300 =
    A.class "sm:focus:bg-purple-300"


smFocusBgPurple400 : Html.Attribute msg
smFocusBgPurple400 =
    A.class "sm:focus:bg-purple-400"


smFocusBgPurple500 : Html.Attribute msg
smFocusBgPurple500 =
    A.class "sm:focus:bg-purple-500"


smFocusBgPurple600 : Html.Attribute msg
smFocusBgPurple600 =
    A.class "sm:focus:bg-purple-600"


smFocusBgPurple700 : Html.Attribute msg
smFocusBgPurple700 =
    A.class "sm:focus:bg-purple-700"


smFocusBgPurple800 : Html.Attribute msg
smFocusBgPurple800 =
    A.class "sm:focus:bg-purple-800"


smFocusBgPurple900 : Html.Attribute msg
smFocusBgPurple900 =
    A.class "sm:focus:bg-purple-900"


smFocusBgPink100 : Html.Attribute msg
smFocusBgPink100 =
    A.class "sm:focus:bg-pink-100"


smFocusBgPink200 : Html.Attribute msg
smFocusBgPink200 =
    A.class "sm:focus:bg-pink-200"


smFocusBgPink300 : Html.Attribute msg
smFocusBgPink300 =
    A.class "sm:focus:bg-pink-300"


smFocusBgPink400 : Html.Attribute msg
smFocusBgPink400 =
    A.class "sm:focus:bg-pink-400"


smFocusBgPink500 : Html.Attribute msg
smFocusBgPink500 =
    A.class "sm:focus:bg-pink-500"


smFocusBgPink600 : Html.Attribute msg
smFocusBgPink600 =
    A.class "sm:focus:bg-pink-600"


smFocusBgPink700 : Html.Attribute msg
smFocusBgPink700 =
    A.class "sm:focus:bg-pink-700"


smFocusBgPink800 : Html.Attribute msg
smFocusBgPink800 =
    A.class "sm:focus:bg-pink-800"


smFocusBgPink900 : Html.Attribute msg
smFocusBgPink900 =
    A.class "sm:focus:bg-pink-900"


smBgNone : Html.Attribute msg
smBgNone =
    A.class "sm:bg-none"


smBgGradientToT : Html.Attribute msg
smBgGradientToT =
    A.class "sm:bg-gradient-to-t"


smBgGradientToTr : Html.Attribute msg
smBgGradientToTr =
    A.class "sm:bg-gradient-to-tr"


smBgGradientToR : Html.Attribute msg
smBgGradientToR =
    A.class "sm:bg-gradient-to-r"


smBgGradientToBr : Html.Attribute msg
smBgGradientToBr =
    A.class "sm:bg-gradient-to-br"


smBgGradientToB : Html.Attribute msg
smBgGradientToB =
    A.class "sm:bg-gradient-to-b"


smBgGradientToBl : Html.Attribute msg
smBgGradientToBl =
    A.class "sm:bg-gradient-to-bl"


smBgGradientToL : Html.Attribute msg
smBgGradientToL =
    A.class "sm:bg-gradient-to-l"


smBgGradientToTl : Html.Attribute msg
smBgGradientToTl =
    A.class "sm:bg-gradient-to-tl"


smFromTransparent : Html.Attribute msg
smFromTransparent =
    A.class "sm:from-transparent"


smFromCurrent : Html.Attribute msg
smFromCurrent =
    A.class "sm:from-current"


smFromBlack : Html.Attribute msg
smFromBlack =
    A.class "sm:from-black"


smFromWhite : Html.Attribute msg
smFromWhite =
    A.class "sm:from-white"


smFromGray100 : Html.Attribute msg
smFromGray100 =
    A.class "sm:from-gray-100"


smFromGray200 : Html.Attribute msg
smFromGray200 =
    A.class "sm:from-gray-200"


smFromGray300 : Html.Attribute msg
smFromGray300 =
    A.class "sm:from-gray-300"


smFromGray400 : Html.Attribute msg
smFromGray400 =
    A.class "sm:from-gray-400"


smFromGray500 : Html.Attribute msg
smFromGray500 =
    A.class "sm:from-gray-500"


smFromGray600 : Html.Attribute msg
smFromGray600 =
    A.class "sm:from-gray-600"


smFromGray700 : Html.Attribute msg
smFromGray700 =
    A.class "sm:from-gray-700"


smFromGray800 : Html.Attribute msg
smFromGray800 =
    A.class "sm:from-gray-800"


smFromGray900 : Html.Attribute msg
smFromGray900 =
    A.class "sm:from-gray-900"


smFromRed100 : Html.Attribute msg
smFromRed100 =
    A.class "sm:from-red-100"


smFromRed200 : Html.Attribute msg
smFromRed200 =
    A.class "sm:from-red-200"


smFromRed300 : Html.Attribute msg
smFromRed300 =
    A.class "sm:from-red-300"


smFromRed400 : Html.Attribute msg
smFromRed400 =
    A.class "sm:from-red-400"


smFromRed500 : Html.Attribute msg
smFromRed500 =
    A.class "sm:from-red-500"


smFromRed600 : Html.Attribute msg
smFromRed600 =
    A.class "sm:from-red-600"


smFromRed700 : Html.Attribute msg
smFromRed700 =
    A.class "sm:from-red-700"


smFromRed800 : Html.Attribute msg
smFromRed800 =
    A.class "sm:from-red-800"


smFromRed900 : Html.Attribute msg
smFromRed900 =
    A.class "sm:from-red-900"


smFromOrange100 : Html.Attribute msg
smFromOrange100 =
    A.class "sm:from-orange-100"


smFromOrange200 : Html.Attribute msg
smFromOrange200 =
    A.class "sm:from-orange-200"


smFromOrange300 : Html.Attribute msg
smFromOrange300 =
    A.class "sm:from-orange-300"


smFromOrange400 : Html.Attribute msg
smFromOrange400 =
    A.class "sm:from-orange-400"


smFromOrange500 : Html.Attribute msg
smFromOrange500 =
    A.class "sm:from-orange-500"


smFromOrange600 : Html.Attribute msg
smFromOrange600 =
    A.class "sm:from-orange-600"


smFromOrange700 : Html.Attribute msg
smFromOrange700 =
    A.class "sm:from-orange-700"


smFromOrange800 : Html.Attribute msg
smFromOrange800 =
    A.class "sm:from-orange-800"


smFromOrange900 : Html.Attribute msg
smFromOrange900 =
    A.class "sm:from-orange-900"


smFromYellow100 : Html.Attribute msg
smFromYellow100 =
    A.class "sm:from-yellow-100"


smFromYellow200 : Html.Attribute msg
smFromYellow200 =
    A.class "sm:from-yellow-200"


smFromYellow300 : Html.Attribute msg
smFromYellow300 =
    A.class "sm:from-yellow-300"


smFromYellow400 : Html.Attribute msg
smFromYellow400 =
    A.class "sm:from-yellow-400"


smFromYellow500 : Html.Attribute msg
smFromYellow500 =
    A.class "sm:from-yellow-500"


smFromYellow600 : Html.Attribute msg
smFromYellow600 =
    A.class "sm:from-yellow-600"


smFromYellow700 : Html.Attribute msg
smFromYellow700 =
    A.class "sm:from-yellow-700"


smFromYellow800 : Html.Attribute msg
smFromYellow800 =
    A.class "sm:from-yellow-800"


smFromYellow900 : Html.Attribute msg
smFromYellow900 =
    A.class "sm:from-yellow-900"


smFromGreen100 : Html.Attribute msg
smFromGreen100 =
    A.class "sm:from-green-100"


smFromGreen200 : Html.Attribute msg
smFromGreen200 =
    A.class "sm:from-green-200"


smFromGreen300 : Html.Attribute msg
smFromGreen300 =
    A.class "sm:from-green-300"


smFromGreen400 : Html.Attribute msg
smFromGreen400 =
    A.class "sm:from-green-400"


smFromGreen500 : Html.Attribute msg
smFromGreen500 =
    A.class "sm:from-green-500"


smFromGreen600 : Html.Attribute msg
smFromGreen600 =
    A.class "sm:from-green-600"


smFromGreen700 : Html.Attribute msg
smFromGreen700 =
    A.class "sm:from-green-700"


smFromGreen800 : Html.Attribute msg
smFromGreen800 =
    A.class "sm:from-green-800"


smFromGreen900 : Html.Attribute msg
smFromGreen900 =
    A.class "sm:from-green-900"


smFromTeal100 : Html.Attribute msg
smFromTeal100 =
    A.class "sm:from-teal-100"


smFromTeal200 : Html.Attribute msg
smFromTeal200 =
    A.class "sm:from-teal-200"


smFromTeal300 : Html.Attribute msg
smFromTeal300 =
    A.class "sm:from-teal-300"


smFromTeal400 : Html.Attribute msg
smFromTeal400 =
    A.class "sm:from-teal-400"


smFromTeal500 : Html.Attribute msg
smFromTeal500 =
    A.class "sm:from-teal-500"


smFromTeal600 : Html.Attribute msg
smFromTeal600 =
    A.class "sm:from-teal-600"


smFromTeal700 : Html.Attribute msg
smFromTeal700 =
    A.class "sm:from-teal-700"


smFromTeal800 : Html.Attribute msg
smFromTeal800 =
    A.class "sm:from-teal-800"


smFromTeal900 : Html.Attribute msg
smFromTeal900 =
    A.class "sm:from-teal-900"


smFromBlue100 : Html.Attribute msg
smFromBlue100 =
    A.class "sm:from-blue-100"


smFromBlue200 : Html.Attribute msg
smFromBlue200 =
    A.class "sm:from-blue-200"


smFromBlue300 : Html.Attribute msg
smFromBlue300 =
    A.class "sm:from-blue-300"


smFromBlue400 : Html.Attribute msg
smFromBlue400 =
    A.class "sm:from-blue-400"


smFromBlue500 : Html.Attribute msg
smFromBlue500 =
    A.class "sm:from-blue-500"


smFromBlue600 : Html.Attribute msg
smFromBlue600 =
    A.class "sm:from-blue-600"


smFromBlue700 : Html.Attribute msg
smFromBlue700 =
    A.class "sm:from-blue-700"


smFromBlue800 : Html.Attribute msg
smFromBlue800 =
    A.class "sm:from-blue-800"


smFromBlue900 : Html.Attribute msg
smFromBlue900 =
    A.class "sm:from-blue-900"


smFromIndigo100 : Html.Attribute msg
smFromIndigo100 =
    A.class "sm:from-indigo-100"


smFromIndigo200 : Html.Attribute msg
smFromIndigo200 =
    A.class "sm:from-indigo-200"


smFromIndigo300 : Html.Attribute msg
smFromIndigo300 =
    A.class "sm:from-indigo-300"


smFromIndigo400 : Html.Attribute msg
smFromIndigo400 =
    A.class "sm:from-indigo-400"


smFromIndigo500 : Html.Attribute msg
smFromIndigo500 =
    A.class "sm:from-indigo-500"


smFromIndigo600 : Html.Attribute msg
smFromIndigo600 =
    A.class "sm:from-indigo-600"


smFromIndigo700 : Html.Attribute msg
smFromIndigo700 =
    A.class "sm:from-indigo-700"


smFromIndigo800 : Html.Attribute msg
smFromIndigo800 =
    A.class "sm:from-indigo-800"


smFromIndigo900 : Html.Attribute msg
smFromIndigo900 =
    A.class "sm:from-indigo-900"


smFromPurple100 : Html.Attribute msg
smFromPurple100 =
    A.class "sm:from-purple-100"


smFromPurple200 : Html.Attribute msg
smFromPurple200 =
    A.class "sm:from-purple-200"


smFromPurple300 : Html.Attribute msg
smFromPurple300 =
    A.class "sm:from-purple-300"


smFromPurple400 : Html.Attribute msg
smFromPurple400 =
    A.class "sm:from-purple-400"


smFromPurple500 : Html.Attribute msg
smFromPurple500 =
    A.class "sm:from-purple-500"


smFromPurple600 : Html.Attribute msg
smFromPurple600 =
    A.class "sm:from-purple-600"


smFromPurple700 : Html.Attribute msg
smFromPurple700 =
    A.class "sm:from-purple-700"


smFromPurple800 : Html.Attribute msg
smFromPurple800 =
    A.class "sm:from-purple-800"


smFromPurple900 : Html.Attribute msg
smFromPurple900 =
    A.class "sm:from-purple-900"


smFromPink100 : Html.Attribute msg
smFromPink100 =
    A.class "sm:from-pink-100"


smFromPink200 : Html.Attribute msg
smFromPink200 =
    A.class "sm:from-pink-200"


smFromPink300 : Html.Attribute msg
smFromPink300 =
    A.class "sm:from-pink-300"


smFromPink400 : Html.Attribute msg
smFromPink400 =
    A.class "sm:from-pink-400"


smFromPink500 : Html.Attribute msg
smFromPink500 =
    A.class "sm:from-pink-500"


smFromPink600 : Html.Attribute msg
smFromPink600 =
    A.class "sm:from-pink-600"


smFromPink700 : Html.Attribute msg
smFromPink700 =
    A.class "sm:from-pink-700"


smFromPink800 : Html.Attribute msg
smFromPink800 =
    A.class "sm:from-pink-800"


smFromPink900 : Html.Attribute msg
smFromPink900 =
    A.class "sm:from-pink-900"


smViaTransparent : Html.Attribute msg
smViaTransparent =
    A.class "sm:via-transparent"


smViaCurrent : Html.Attribute msg
smViaCurrent =
    A.class "sm:via-current"


smViaBlack : Html.Attribute msg
smViaBlack =
    A.class "sm:via-black"


smViaWhite : Html.Attribute msg
smViaWhite =
    A.class "sm:via-white"


smViaGray100 : Html.Attribute msg
smViaGray100 =
    A.class "sm:via-gray-100"


smViaGray200 : Html.Attribute msg
smViaGray200 =
    A.class "sm:via-gray-200"


smViaGray300 : Html.Attribute msg
smViaGray300 =
    A.class "sm:via-gray-300"


smViaGray400 : Html.Attribute msg
smViaGray400 =
    A.class "sm:via-gray-400"


smViaGray500 : Html.Attribute msg
smViaGray500 =
    A.class "sm:via-gray-500"


smViaGray600 : Html.Attribute msg
smViaGray600 =
    A.class "sm:via-gray-600"


smViaGray700 : Html.Attribute msg
smViaGray700 =
    A.class "sm:via-gray-700"


smViaGray800 : Html.Attribute msg
smViaGray800 =
    A.class "sm:via-gray-800"


smViaGray900 : Html.Attribute msg
smViaGray900 =
    A.class "sm:via-gray-900"


smViaRed100 : Html.Attribute msg
smViaRed100 =
    A.class "sm:via-red-100"


smViaRed200 : Html.Attribute msg
smViaRed200 =
    A.class "sm:via-red-200"


smViaRed300 : Html.Attribute msg
smViaRed300 =
    A.class "sm:via-red-300"


smViaRed400 : Html.Attribute msg
smViaRed400 =
    A.class "sm:via-red-400"


smViaRed500 : Html.Attribute msg
smViaRed500 =
    A.class "sm:via-red-500"


smViaRed600 : Html.Attribute msg
smViaRed600 =
    A.class "sm:via-red-600"


smViaRed700 : Html.Attribute msg
smViaRed700 =
    A.class "sm:via-red-700"


smViaRed800 : Html.Attribute msg
smViaRed800 =
    A.class "sm:via-red-800"


smViaRed900 : Html.Attribute msg
smViaRed900 =
    A.class "sm:via-red-900"


smViaOrange100 : Html.Attribute msg
smViaOrange100 =
    A.class "sm:via-orange-100"


smViaOrange200 : Html.Attribute msg
smViaOrange200 =
    A.class "sm:via-orange-200"


smViaOrange300 : Html.Attribute msg
smViaOrange300 =
    A.class "sm:via-orange-300"


smViaOrange400 : Html.Attribute msg
smViaOrange400 =
    A.class "sm:via-orange-400"


smViaOrange500 : Html.Attribute msg
smViaOrange500 =
    A.class "sm:via-orange-500"


smViaOrange600 : Html.Attribute msg
smViaOrange600 =
    A.class "sm:via-orange-600"


smViaOrange700 : Html.Attribute msg
smViaOrange700 =
    A.class "sm:via-orange-700"


smViaOrange800 : Html.Attribute msg
smViaOrange800 =
    A.class "sm:via-orange-800"


smViaOrange900 : Html.Attribute msg
smViaOrange900 =
    A.class "sm:via-orange-900"


smViaYellow100 : Html.Attribute msg
smViaYellow100 =
    A.class "sm:via-yellow-100"


smViaYellow200 : Html.Attribute msg
smViaYellow200 =
    A.class "sm:via-yellow-200"


smViaYellow300 : Html.Attribute msg
smViaYellow300 =
    A.class "sm:via-yellow-300"


smViaYellow400 : Html.Attribute msg
smViaYellow400 =
    A.class "sm:via-yellow-400"


smViaYellow500 : Html.Attribute msg
smViaYellow500 =
    A.class "sm:via-yellow-500"


smViaYellow600 : Html.Attribute msg
smViaYellow600 =
    A.class "sm:via-yellow-600"


smViaYellow700 : Html.Attribute msg
smViaYellow700 =
    A.class "sm:via-yellow-700"


smViaYellow800 : Html.Attribute msg
smViaYellow800 =
    A.class "sm:via-yellow-800"


smViaYellow900 : Html.Attribute msg
smViaYellow900 =
    A.class "sm:via-yellow-900"


smViaGreen100 : Html.Attribute msg
smViaGreen100 =
    A.class "sm:via-green-100"


smViaGreen200 : Html.Attribute msg
smViaGreen200 =
    A.class "sm:via-green-200"


smViaGreen300 : Html.Attribute msg
smViaGreen300 =
    A.class "sm:via-green-300"


smViaGreen400 : Html.Attribute msg
smViaGreen400 =
    A.class "sm:via-green-400"


smViaGreen500 : Html.Attribute msg
smViaGreen500 =
    A.class "sm:via-green-500"


smViaGreen600 : Html.Attribute msg
smViaGreen600 =
    A.class "sm:via-green-600"


smViaGreen700 : Html.Attribute msg
smViaGreen700 =
    A.class "sm:via-green-700"


smViaGreen800 : Html.Attribute msg
smViaGreen800 =
    A.class "sm:via-green-800"


smViaGreen900 : Html.Attribute msg
smViaGreen900 =
    A.class "sm:via-green-900"


smViaTeal100 : Html.Attribute msg
smViaTeal100 =
    A.class "sm:via-teal-100"


smViaTeal200 : Html.Attribute msg
smViaTeal200 =
    A.class "sm:via-teal-200"


smViaTeal300 : Html.Attribute msg
smViaTeal300 =
    A.class "sm:via-teal-300"


smViaTeal400 : Html.Attribute msg
smViaTeal400 =
    A.class "sm:via-teal-400"


smViaTeal500 : Html.Attribute msg
smViaTeal500 =
    A.class "sm:via-teal-500"


smViaTeal600 : Html.Attribute msg
smViaTeal600 =
    A.class "sm:via-teal-600"


smViaTeal700 : Html.Attribute msg
smViaTeal700 =
    A.class "sm:via-teal-700"


smViaTeal800 : Html.Attribute msg
smViaTeal800 =
    A.class "sm:via-teal-800"


smViaTeal900 : Html.Attribute msg
smViaTeal900 =
    A.class "sm:via-teal-900"


smViaBlue100 : Html.Attribute msg
smViaBlue100 =
    A.class "sm:via-blue-100"


smViaBlue200 : Html.Attribute msg
smViaBlue200 =
    A.class "sm:via-blue-200"


smViaBlue300 : Html.Attribute msg
smViaBlue300 =
    A.class "sm:via-blue-300"


smViaBlue400 : Html.Attribute msg
smViaBlue400 =
    A.class "sm:via-blue-400"


smViaBlue500 : Html.Attribute msg
smViaBlue500 =
    A.class "sm:via-blue-500"


smViaBlue600 : Html.Attribute msg
smViaBlue600 =
    A.class "sm:via-blue-600"


smViaBlue700 : Html.Attribute msg
smViaBlue700 =
    A.class "sm:via-blue-700"


smViaBlue800 : Html.Attribute msg
smViaBlue800 =
    A.class "sm:via-blue-800"


smViaBlue900 : Html.Attribute msg
smViaBlue900 =
    A.class "sm:via-blue-900"


smViaIndigo100 : Html.Attribute msg
smViaIndigo100 =
    A.class "sm:via-indigo-100"


smViaIndigo200 : Html.Attribute msg
smViaIndigo200 =
    A.class "sm:via-indigo-200"


smViaIndigo300 : Html.Attribute msg
smViaIndigo300 =
    A.class "sm:via-indigo-300"


smViaIndigo400 : Html.Attribute msg
smViaIndigo400 =
    A.class "sm:via-indigo-400"


smViaIndigo500 : Html.Attribute msg
smViaIndigo500 =
    A.class "sm:via-indigo-500"


smViaIndigo600 : Html.Attribute msg
smViaIndigo600 =
    A.class "sm:via-indigo-600"


smViaIndigo700 : Html.Attribute msg
smViaIndigo700 =
    A.class "sm:via-indigo-700"


smViaIndigo800 : Html.Attribute msg
smViaIndigo800 =
    A.class "sm:via-indigo-800"


smViaIndigo900 : Html.Attribute msg
smViaIndigo900 =
    A.class "sm:via-indigo-900"


smViaPurple100 : Html.Attribute msg
smViaPurple100 =
    A.class "sm:via-purple-100"


smViaPurple200 : Html.Attribute msg
smViaPurple200 =
    A.class "sm:via-purple-200"


smViaPurple300 : Html.Attribute msg
smViaPurple300 =
    A.class "sm:via-purple-300"


smViaPurple400 : Html.Attribute msg
smViaPurple400 =
    A.class "sm:via-purple-400"


smViaPurple500 : Html.Attribute msg
smViaPurple500 =
    A.class "sm:via-purple-500"


smViaPurple600 : Html.Attribute msg
smViaPurple600 =
    A.class "sm:via-purple-600"


smViaPurple700 : Html.Attribute msg
smViaPurple700 =
    A.class "sm:via-purple-700"


smViaPurple800 : Html.Attribute msg
smViaPurple800 =
    A.class "sm:via-purple-800"


smViaPurple900 : Html.Attribute msg
smViaPurple900 =
    A.class "sm:via-purple-900"


smViaPink100 : Html.Attribute msg
smViaPink100 =
    A.class "sm:via-pink-100"


smViaPink200 : Html.Attribute msg
smViaPink200 =
    A.class "sm:via-pink-200"


smViaPink300 : Html.Attribute msg
smViaPink300 =
    A.class "sm:via-pink-300"


smViaPink400 : Html.Attribute msg
smViaPink400 =
    A.class "sm:via-pink-400"


smViaPink500 : Html.Attribute msg
smViaPink500 =
    A.class "sm:via-pink-500"


smViaPink600 : Html.Attribute msg
smViaPink600 =
    A.class "sm:via-pink-600"


smViaPink700 : Html.Attribute msg
smViaPink700 =
    A.class "sm:via-pink-700"


smViaPink800 : Html.Attribute msg
smViaPink800 =
    A.class "sm:via-pink-800"


smViaPink900 : Html.Attribute msg
smViaPink900 =
    A.class "sm:via-pink-900"


smToTransparent : Html.Attribute msg
smToTransparent =
    A.class "sm:to-transparent"


smToCurrent : Html.Attribute msg
smToCurrent =
    A.class "sm:to-current"


smToBlack : Html.Attribute msg
smToBlack =
    A.class "sm:to-black"


smToWhite : Html.Attribute msg
smToWhite =
    A.class "sm:to-white"


smToGray100 : Html.Attribute msg
smToGray100 =
    A.class "sm:to-gray-100"


smToGray200 : Html.Attribute msg
smToGray200 =
    A.class "sm:to-gray-200"


smToGray300 : Html.Attribute msg
smToGray300 =
    A.class "sm:to-gray-300"


smToGray400 : Html.Attribute msg
smToGray400 =
    A.class "sm:to-gray-400"


smToGray500 : Html.Attribute msg
smToGray500 =
    A.class "sm:to-gray-500"


smToGray600 : Html.Attribute msg
smToGray600 =
    A.class "sm:to-gray-600"


smToGray700 : Html.Attribute msg
smToGray700 =
    A.class "sm:to-gray-700"


smToGray800 : Html.Attribute msg
smToGray800 =
    A.class "sm:to-gray-800"


smToGray900 : Html.Attribute msg
smToGray900 =
    A.class "sm:to-gray-900"


smToRed100 : Html.Attribute msg
smToRed100 =
    A.class "sm:to-red-100"


smToRed200 : Html.Attribute msg
smToRed200 =
    A.class "sm:to-red-200"


smToRed300 : Html.Attribute msg
smToRed300 =
    A.class "sm:to-red-300"


smToRed400 : Html.Attribute msg
smToRed400 =
    A.class "sm:to-red-400"


smToRed500 : Html.Attribute msg
smToRed500 =
    A.class "sm:to-red-500"


smToRed600 : Html.Attribute msg
smToRed600 =
    A.class "sm:to-red-600"


smToRed700 : Html.Attribute msg
smToRed700 =
    A.class "sm:to-red-700"


smToRed800 : Html.Attribute msg
smToRed800 =
    A.class "sm:to-red-800"


smToRed900 : Html.Attribute msg
smToRed900 =
    A.class "sm:to-red-900"


smToOrange100 : Html.Attribute msg
smToOrange100 =
    A.class "sm:to-orange-100"


smToOrange200 : Html.Attribute msg
smToOrange200 =
    A.class "sm:to-orange-200"


smToOrange300 : Html.Attribute msg
smToOrange300 =
    A.class "sm:to-orange-300"


smToOrange400 : Html.Attribute msg
smToOrange400 =
    A.class "sm:to-orange-400"


smToOrange500 : Html.Attribute msg
smToOrange500 =
    A.class "sm:to-orange-500"


smToOrange600 : Html.Attribute msg
smToOrange600 =
    A.class "sm:to-orange-600"


smToOrange700 : Html.Attribute msg
smToOrange700 =
    A.class "sm:to-orange-700"


smToOrange800 : Html.Attribute msg
smToOrange800 =
    A.class "sm:to-orange-800"


smToOrange900 : Html.Attribute msg
smToOrange900 =
    A.class "sm:to-orange-900"


smToYellow100 : Html.Attribute msg
smToYellow100 =
    A.class "sm:to-yellow-100"


smToYellow200 : Html.Attribute msg
smToYellow200 =
    A.class "sm:to-yellow-200"


smToYellow300 : Html.Attribute msg
smToYellow300 =
    A.class "sm:to-yellow-300"


smToYellow400 : Html.Attribute msg
smToYellow400 =
    A.class "sm:to-yellow-400"


smToYellow500 : Html.Attribute msg
smToYellow500 =
    A.class "sm:to-yellow-500"


smToYellow600 : Html.Attribute msg
smToYellow600 =
    A.class "sm:to-yellow-600"


smToYellow700 : Html.Attribute msg
smToYellow700 =
    A.class "sm:to-yellow-700"


smToYellow800 : Html.Attribute msg
smToYellow800 =
    A.class "sm:to-yellow-800"


smToYellow900 : Html.Attribute msg
smToYellow900 =
    A.class "sm:to-yellow-900"


smToGreen100 : Html.Attribute msg
smToGreen100 =
    A.class "sm:to-green-100"


smToGreen200 : Html.Attribute msg
smToGreen200 =
    A.class "sm:to-green-200"


smToGreen300 : Html.Attribute msg
smToGreen300 =
    A.class "sm:to-green-300"


smToGreen400 : Html.Attribute msg
smToGreen400 =
    A.class "sm:to-green-400"


smToGreen500 : Html.Attribute msg
smToGreen500 =
    A.class "sm:to-green-500"


smToGreen600 : Html.Attribute msg
smToGreen600 =
    A.class "sm:to-green-600"


smToGreen700 : Html.Attribute msg
smToGreen700 =
    A.class "sm:to-green-700"


smToGreen800 : Html.Attribute msg
smToGreen800 =
    A.class "sm:to-green-800"


smToGreen900 : Html.Attribute msg
smToGreen900 =
    A.class "sm:to-green-900"


smToTeal100 : Html.Attribute msg
smToTeal100 =
    A.class "sm:to-teal-100"


smToTeal200 : Html.Attribute msg
smToTeal200 =
    A.class "sm:to-teal-200"


smToTeal300 : Html.Attribute msg
smToTeal300 =
    A.class "sm:to-teal-300"


smToTeal400 : Html.Attribute msg
smToTeal400 =
    A.class "sm:to-teal-400"


smToTeal500 : Html.Attribute msg
smToTeal500 =
    A.class "sm:to-teal-500"


smToTeal600 : Html.Attribute msg
smToTeal600 =
    A.class "sm:to-teal-600"


smToTeal700 : Html.Attribute msg
smToTeal700 =
    A.class "sm:to-teal-700"


smToTeal800 : Html.Attribute msg
smToTeal800 =
    A.class "sm:to-teal-800"


smToTeal900 : Html.Attribute msg
smToTeal900 =
    A.class "sm:to-teal-900"


smToBlue100 : Html.Attribute msg
smToBlue100 =
    A.class "sm:to-blue-100"


smToBlue200 : Html.Attribute msg
smToBlue200 =
    A.class "sm:to-blue-200"


smToBlue300 : Html.Attribute msg
smToBlue300 =
    A.class "sm:to-blue-300"


smToBlue400 : Html.Attribute msg
smToBlue400 =
    A.class "sm:to-blue-400"


smToBlue500 : Html.Attribute msg
smToBlue500 =
    A.class "sm:to-blue-500"


smToBlue600 : Html.Attribute msg
smToBlue600 =
    A.class "sm:to-blue-600"


smToBlue700 : Html.Attribute msg
smToBlue700 =
    A.class "sm:to-blue-700"


smToBlue800 : Html.Attribute msg
smToBlue800 =
    A.class "sm:to-blue-800"


smToBlue900 : Html.Attribute msg
smToBlue900 =
    A.class "sm:to-blue-900"


smToIndigo100 : Html.Attribute msg
smToIndigo100 =
    A.class "sm:to-indigo-100"


smToIndigo200 : Html.Attribute msg
smToIndigo200 =
    A.class "sm:to-indigo-200"


smToIndigo300 : Html.Attribute msg
smToIndigo300 =
    A.class "sm:to-indigo-300"


smToIndigo400 : Html.Attribute msg
smToIndigo400 =
    A.class "sm:to-indigo-400"


smToIndigo500 : Html.Attribute msg
smToIndigo500 =
    A.class "sm:to-indigo-500"


smToIndigo600 : Html.Attribute msg
smToIndigo600 =
    A.class "sm:to-indigo-600"


smToIndigo700 : Html.Attribute msg
smToIndigo700 =
    A.class "sm:to-indigo-700"


smToIndigo800 : Html.Attribute msg
smToIndigo800 =
    A.class "sm:to-indigo-800"


smToIndigo900 : Html.Attribute msg
smToIndigo900 =
    A.class "sm:to-indigo-900"


smToPurple100 : Html.Attribute msg
smToPurple100 =
    A.class "sm:to-purple-100"


smToPurple200 : Html.Attribute msg
smToPurple200 =
    A.class "sm:to-purple-200"


smToPurple300 : Html.Attribute msg
smToPurple300 =
    A.class "sm:to-purple-300"


smToPurple400 : Html.Attribute msg
smToPurple400 =
    A.class "sm:to-purple-400"


smToPurple500 : Html.Attribute msg
smToPurple500 =
    A.class "sm:to-purple-500"


smToPurple600 : Html.Attribute msg
smToPurple600 =
    A.class "sm:to-purple-600"


smToPurple700 : Html.Attribute msg
smToPurple700 =
    A.class "sm:to-purple-700"


smToPurple800 : Html.Attribute msg
smToPurple800 =
    A.class "sm:to-purple-800"


smToPurple900 : Html.Attribute msg
smToPurple900 =
    A.class "sm:to-purple-900"


smToPink100 : Html.Attribute msg
smToPink100 =
    A.class "sm:to-pink-100"


smToPink200 : Html.Attribute msg
smToPink200 =
    A.class "sm:to-pink-200"


smToPink300 : Html.Attribute msg
smToPink300 =
    A.class "sm:to-pink-300"


smToPink400 : Html.Attribute msg
smToPink400 =
    A.class "sm:to-pink-400"


smToPink500 : Html.Attribute msg
smToPink500 =
    A.class "sm:to-pink-500"


smToPink600 : Html.Attribute msg
smToPink600 =
    A.class "sm:to-pink-600"


smToPink700 : Html.Attribute msg
smToPink700 =
    A.class "sm:to-pink-700"


smToPink800 : Html.Attribute msg
smToPink800 =
    A.class "sm:to-pink-800"


smToPink900 : Html.Attribute msg
smToPink900 =
    A.class "sm:to-pink-900"


smHoverFromTransparent : Html.Attribute msg
smHoverFromTransparent =
    A.class "sm:hover:from-transparent"


smHoverFromCurrent : Html.Attribute msg
smHoverFromCurrent =
    A.class "sm:hover:from-current"


smHoverFromBlack : Html.Attribute msg
smHoverFromBlack =
    A.class "sm:hover:from-black"


smHoverFromWhite : Html.Attribute msg
smHoverFromWhite =
    A.class "sm:hover:from-white"


smHoverFromGray100 : Html.Attribute msg
smHoverFromGray100 =
    A.class "sm:hover:from-gray-100"


smHoverFromGray200 : Html.Attribute msg
smHoverFromGray200 =
    A.class "sm:hover:from-gray-200"


smHoverFromGray300 : Html.Attribute msg
smHoverFromGray300 =
    A.class "sm:hover:from-gray-300"


smHoverFromGray400 : Html.Attribute msg
smHoverFromGray400 =
    A.class "sm:hover:from-gray-400"


smHoverFromGray500 : Html.Attribute msg
smHoverFromGray500 =
    A.class "sm:hover:from-gray-500"


smHoverFromGray600 : Html.Attribute msg
smHoverFromGray600 =
    A.class "sm:hover:from-gray-600"


smHoverFromGray700 : Html.Attribute msg
smHoverFromGray700 =
    A.class "sm:hover:from-gray-700"


smHoverFromGray800 : Html.Attribute msg
smHoverFromGray800 =
    A.class "sm:hover:from-gray-800"


smHoverFromGray900 : Html.Attribute msg
smHoverFromGray900 =
    A.class "sm:hover:from-gray-900"


smHoverFromRed100 : Html.Attribute msg
smHoverFromRed100 =
    A.class "sm:hover:from-red-100"


smHoverFromRed200 : Html.Attribute msg
smHoverFromRed200 =
    A.class "sm:hover:from-red-200"


smHoverFromRed300 : Html.Attribute msg
smHoverFromRed300 =
    A.class "sm:hover:from-red-300"


smHoverFromRed400 : Html.Attribute msg
smHoverFromRed400 =
    A.class "sm:hover:from-red-400"


smHoverFromRed500 : Html.Attribute msg
smHoverFromRed500 =
    A.class "sm:hover:from-red-500"


smHoverFromRed600 : Html.Attribute msg
smHoverFromRed600 =
    A.class "sm:hover:from-red-600"


smHoverFromRed700 : Html.Attribute msg
smHoverFromRed700 =
    A.class "sm:hover:from-red-700"


smHoverFromRed800 : Html.Attribute msg
smHoverFromRed800 =
    A.class "sm:hover:from-red-800"


smHoverFromRed900 : Html.Attribute msg
smHoverFromRed900 =
    A.class "sm:hover:from-red-900"


smHoverFromOrange100 : Html.Attribute msg
smHoverFromOrange100 =
    A.class "sm:hover:from-orange-100"


smHoverFromOrange200 : Html.Attribute msg
smHoverFromOrange200 =
    A.class "sm:hover:from-orange-200"


smHoverFromOrange300 : Html.Attribute msg
smHoverFromOrange300 =
    A.class "sm:hover:from-orange-300"


smHoverFromOrange400 : Html.Attribute msg
smHoverFromOrange400 =
    A.class "sm:hover:from-orange-400"


smHoverFromOrange500 : Html.Attribute msg
smHoverFromOrange500 =
    A.class "sm:hover:from-orange-500"


smHoverFromOrange600 : Html.Attribute msg
smHoverFromOrange600 =
    A.class "sm:hover:from-orange-600"


smHoverFromOrange700 : Html.Attribute msg
smHoverFromOrange700 =
    A.class "sm:hover:from-orange-700"


smHoverFromOrange800 : Html.Attribute msg
smHoverFromOrange800 =
    A.class "sm:hover:from-orange-800"


smHoverFromOrange900 : Html.Attribute msg
smHoverFromOrange900 =
    A.class "sm:hover:from-orange-900"


smHoverFromYellow100 : Html.Attribute msg
smHoverFromYellow100 =
    A.class "sm:hover:from-yellow-100"


smHoverFromYellow200 : Html.Attribute msg
smHoverFromYellow200 =
    A.class "sm:hover:from-yellow-200"


smHoverFromYellow300 : Html.Attribute msg
smHoverFromYellow300 =
    A.class "sm:hover:from-yellow-300"


smHoverFromYellow400 : Html.Attribute msg
smHoverFromYellow400 =
    A.class "sm:hover:from-yellow-400"


smHoverFromYellow500 : Html.Attribute msg
smHoverFromYellow500 =
    A.class "sm:hover:from-yellow-500"


smHoverFromYellow600 : Html.Attribute msg
smHoverFromYellow600 =
    A.class "sm:hover:from-yellow-600"


smHoverFromYellow700 : Html.Attribute msg
smHoverFromYellow700 =
    A.class "sm:hover:from-yellow-700"


smHoverFromYellow800 : Html.Attribute msg
smHoverFromYellow800 =
    A.class "sm:hover:from-yellow-800"


smHoverFromYellow900 : Html.Attribute msg
smHoverFromYellow900 =
    A.class "sm:hover:from-yellow-900"


smHoverFromGreen100 : Html.Attribute msg
smHoverFromGreen100 =
    A.class "sm:hover:from-green-100"


smHoverFromGreen200 : Html.Attribute msg
smHoverFromGreen200 =
    A.class "sm:hover:from-green-200"


smHoverFromGreen300 : Html.Attribute msg
smHoverFromGreen300 =
    A.class "sm:hover:from-green-300"


smHoverFromGreen400 : Html.Attribute msg
smHoverFromGreen400 =
    A.class "sm:hover:from-green-400"


smHoverFromGreen500 : Html.Attribute msg
smHoverFromGreen500 =
    A.class "sm:hover:from-green-500"


smHoverFromGreen600 : Html.Attribute msg
smHoverFromGreen600 =
    A.class "sm:hover:from-green-600"


smHoverFromGreen700 : Html.Attribute msg
smHoverFromGreen700 =
    A.class "sm:hover:from-green-700"


smHoverFromGreen800 : Html.Attribute msg
smHoverFromGreen800 =
    A.class "sm:hover:from-green-800"


smHoverFromGreen900 : Html.Attribute msg
smHoverFromGreen900 =
    A.class "sm:hover:from-green-900"


smHoverFromTeal100 : Html.Attribute msg
smHoverFromTeal100 =
    A.class "sm:hover:from-teal-100"


smHoverFromTeal200 : Html.Attribute msg
smHoverFromTeal200 =
    A.class "sm:hover:from-teal-200"


smHoverFromTeal300 : Html.Attribute msg
smHoverFromTeal300 =
    A.class "sm:hover:from-teal-300"


smHoverFromTeal400 : Html.Attribute msg
smHoverFromTeal400 =
    A.class "sm:hover:from-teal-400"


smHoverFromTeal500 : Html.Attribute msg
smHoverFromTeal500 =
    A.class "sm:hover:from-teal-500"


smHoverFromTeal600 : Html.Attribute msg
smHoverFromTeal600 =
    A.class "sm:hover:from-teal-600"


smHoverFromTeal700 : Html.Attribute msg
smHoverFromTeal700 =
    A.class "sm:hover:from-teal-700"


smHoverFromTeal800 : Html.Attribute msg
smHoverFromTeal800 =
    A.class "sm:hover:from-teal-800"


smHoverFromTeal900 : Html.Attribute msg
smHoverFromTeal900 =
    A.class "sm:hover:from-teal-900"


smHoverFromBlue100 : Html.Attribute msg
smHoverFromBlue100 =
    A.class "sm:hover:from-blue-100"


smHoverFromBlue200 : Html.Attribute msg
smHoverFromBlue200 =
    A.class "sm:hover:from-blue-200"


smHoverFromBlue300 : Html.Attribute msg
smHoverFromBlue300 =
    A.class "sm:hover:from-blue-300"


smHoverFromBlue400 : Html.Attribute msg
smHoverFromBlue400 =
    A.class "sm:hover:from-blue-400"


smHoverFromBlue500 : Html.Attribute msg
smHoverFromBlue500 =
    A.class "sm:hover:from-blue-500"


smHoverFromBlue600 : Html.Attribute msg
smHoverFromBlue600 =
    A.class "sm:hover:from-blue-600"


smHoverFromBlue700 : Html.Attribute msg
smHoverFromBlue700 =
    A.class "sm:hover:from-blue-700"


smHoverFromBlue800 : Html.Attribute msg
smHoverFromBlue800 =
    A.class "sm:hover:from-blue-800"


smHoverFromBlue900 : Html.Attribute msg
smHoverFromBlue900 =
    A.class "sm:hover:from-blue-900"


smHoverFromIndigo100 : Html.Attribute msg
smHoverFromIndigo100 =
    A.class "sm:hover:from-indigo-100"


smHoverFromIndigo200 : Html.Attribute msg
smHoverFromIndigo200 =
    A.class "sm:hover:from-indigo-200"


smHoverFromIndigo300 : Html.Attribute msg
smHoverFromIndigo300 =
    A.class "sm:hover:from-indigo-300"


smHoverFromIndigo400 : Html.Attribute msg
smHoverFromIndigo400 =
    A.class "sm:hover:from-indigo-400"


smHoverFromIndigo500 : Html.Attribute msg
smHoverFromIndigo500 =
    A.class "sm:hover:from-indigo-500"


smHoverFromIndigo600 : Html.Attribute msg
smHoverFromIndigo600 =
    A.class "sm:hover:from-indigo-600"


smHoverFromIndigo700 : Html.Attribute msg
smHoverFromIndigo700 =
    A.class "sm:hover:from-indigo-700"


smHoverFromIndigo800 : Html.Attribute msg
smHoverFromIndigo800 =
    A.class "sm:hover:from-indigo-800"


smHoverFromIndigo900 : Html.Attribute msg
smHoverFromIndigo900 =
    A.class "sm:hover:from-indigo-900"


smHoverFromPurple100 : Html.Attribute msg
smHoverFromPurple100 =
    A.class "sm:hover:from-purple-100"


smHoverFromPurple200 : Html.Attribute msg
smHoverFromPurple200 =
    A.class "sm:hover:from-purple-200"


smHoverFromPurple300 : Html.Attribute msg
smHoverFromPurple300 =
    A.class "sm:hover:from-purple-300"


smHoverFromPurple400 : Html.Attribute msg
smHoverFromPurple400 =
    A.class "sm:hover:from-purple-400"


smHoverFromPurple500 : Html.Attribute msg
smHoverFromPurple500 =
    A.class "sm:hover:from-purple-500"


smHoverFromPurple600 : Html.Attribute msg
smHoverFromPurple600 =
    A.class "sm:hover:from-purple-600"


smHoverFromPurple700 : Html.Attribute msg
smHoverFromPurple700 =
    A.class "sm:hover:from-purple-700"


smHoverFromPurple800 : Html.Attribute msg
smHoverFromPurple800 =
    A.class "sm:hover:from-purple-800"


smHoverFromPurple900 : Html.Attribute msg
smHoverFromPurple900 =
    A.class "sm:hover:from-purple-900"


smHoverFromPink100 : Html.Attribute msg
smHoverFromPink100 =
    A.class "sm:hover:from-pink-100"


smHoverFromPink200 : Html.Attribute msg
smHoverFromPink200 =
    A.class "sm:hover:from-pink-200"


smHoverFromPink300 : Html.Attribute msg
smHoverFromPink300 =
    A.class "sm:hover:from-pink-300"


smHoverFromPink400 : Html.Attribute msg
smHoverFromPink400 =
    A.class "sm:hover:from-pink-400"


smHoverFromPink500 : Html.Attribute msg
smHoverFromPink500 =
    A.class "sm:hover:from-pink-500"


smHoverFromPink600 : Html.Attribute msg
smHoverFromPink600 =
    A.class "sm:hover:from-pink-600"


smHoverFromPink700 : Html.Attribute msg
smHoverFromPink700 =
    A.class "sm:hover:from-pink-700"


smHoverFromPink800 : Html.Attribute msg
smHoverFromPink800 =
    A.class "sm:hover:from-pink-800"


smHoverFromPink900 : Html.Attribute msg
smHoverFromPink900 =
    A.class "sm:hover:from-pink-900"


smHoverViaTransparent : Html.Attribute msg
smHoverViaTransparent =
    A.class "sm:hover:via-transparent"


smHoverViaCurrent : Html.Attribute msg
smHoverViaCurrent =
    A.class "sm:hover:via-current"


smHoverViaBlack : Html.Attribute msg
smHoverViaBlack =
    A.class "sm:hover:via-black"


smHoverViaWhite : Html.Attribute msg
smHoverViaWhite =
    A.class "sm:hover:via-white"


smHoverViaGray100 : Html.Attribute msg
smHoverViaGray100 =
    A.class "sm:hover:via-gray-100"


smHoverViaGray200 : Html.Attribute msg
smHoverViaGray200 =
    A.class "sm:hover:via-gray-200"


smHoverViaGray300 : Html.Attribute msg
smHoverViaGray300 =
    A.class "sm:hover:via-gray-300"


smHoverViaGray400 : Html.Attribute msg
smHoverViaGray400 =
    A.class "sm:hover:via-gray-400"


smHoverViaGray500 : Html.Attribute msg
smHoverViaGray500 =
    A.class "sm:hover:via-gray-500"


smHoverViaGray600 : Html.Attribute msg
smHoverViaGray600 =
    A.class "sm:hover:via-gray-600"


smHoverViaGray700 : Html.Attribute msg
smHoverViaGray700 =
    A.class "sm:hover:via-gray-700"


smHoverViaGray800 : Html.Attribute msg
smHoverViaGray800 =
    A.class "sm:hover:via-gray-800"


smHoverViaGray900 : Html.Attribute msg
smHoverViaGray900 =
    A.class "sm:hover:via-gray-900"


smHoverViaRed100 : Html.Attribute msg
smHoverViaRed100 =
    A.class "sm:hover:via-red-100"


smHoverViaRed200 : Html.Attribute msg
smHoverViaRed200 =
    A.class "sm:hover:via-red-200"


smHoverViaRed300 : Html.Attribute msg
smHoverViaRed300 =
    A.class "sm:hover:via-red-300"


smHoverViaRed400 : Html.Attribute msg
smHoverViaRed400 =
    A.class "sm:hover:via-red-400"


smHoverViaRed500 : Html.Attribute msg
smHoverViaRed500 =
    A.class "sm:hover:via-red-500"


smHoverViaRed600 : Html.Attribute msg
smHoverViaRed600 =
    A.class "sm:hover:via-red-600"


smHoverViaRed700 : Html.Attribute msg
smHoverViaRed700 =
    A.class "sm:hover:via-red-700"


smHoverViaRed800 : Html.Attribute msg
smHoverViaRed800 =
    A.class "sm:hover:via-red-800"


smHoverViaRed900 : Html.Attribute msg
smHoverViaRed900 =
    A.class "sm:hover:via-red-900"


smHoverViaOrange100 : Html.Attribute msg
smHoverViaOrange100 =
    A.class "sm:hover:via-orange-100"


smHoverViaOrange200 : Html.Attribute msg
smHoverViaOrange200 =
    A.class "sm:hover:via-orange-200"


smHoverViaOrange300 : Html.Attribute msg
smHoverViaOrange300 =
    A.class "sm:hover:via-orange-300"


smHoverViaOrange400 : Html.Attribute msg
smHoverViaOrange400 =
    A.class "sm:hover:via-orange-400"


smHoverViaOrange500 : Html.Attribute msg
smHoverViaOrange500 =
    A.class "sm:hover:via-orange-500"


smHoverViaOrange600 : Html.Attribute msg
smHoverViaOrange600 =
    A.class "sm:hover:via-orange-600"


smHoverViaOrange700 : Html.Attribute msg
smHoverViaOrange700 =
    A.class "sm:hover:via-orange-700"


smHoverViaOrange800 : Html.Attribute msg
smHoverViaOrange800 =
    A.class "sm:hover:via-orange-800"


smHoverViaOrange900 : Html.Attribute msg
smHoverViaOrange900 =
    A.class "sm:hover:via-orange-900"


smHoverViaYellow100 : Html.Attribute msg
smHoverViaYellow100 =
    A.class "sm:hover:via-yellow-100"


smHoverViaYellow200 : Html.Attribute msg
smHoverViaYellow200 =
    A.class "sm:hover:via-yellow-200"


smHoverViaYellow300 : Html.Attribute msg
smHoverViaYellow300 =
    A.class "sm:hover:via-yellow-300"


smHoverViaYellow400 : Html.Attribute msg
smHoverViaYellow400 =
    A.class "sm:hover:via-yellow-400"


smHoverViaYellow500 : Html.Attribute msg
smHoverViaYellow500 =
    A.class "sm:hover:via-yellow-500"


smHoverViaYellow600 : Html.Attribute msg
smHoverViaYellow600 =
    A.class "sm:hover:via-yellow-600"


smHoverViaYellow700 : Html.Attribute msg
smHoverViaYellow700 =
    A.class "sm:hover:via-yellow-700"


smHoverViaYellow800 : Html.Attribute msg
smHoverViaYellow800 =
    A.class "sm:hover:via-yellow-800"


smHoverViaYellow900 : Html.Attribute msg
smHoverViaYellow900 =
    A.class "sm:hover:via-yellow-900"


smHoverViaGreen100 : Html.Attribute msg
smHoverViaGreen100 =
    A.class "sm:hover:via-green-100"


smHoverViaGreen200 : Html.Attribute msg
smHoverViaGreen200 =
    A.class "sm:hover:via-green-200"


smHoverViaGreen300 : Html.Attribute msg
smHoverViaGreen300 =
    A.class "sm:hover:via-green-300"


smHoverViaGreen400 : Html.Attribute msg
smHoverViaGreen400 =
    A.class "sm:hover:via-green-400"


smHoverViaGreen500 : Html.Attribute msg
smHoverViaGreen500 =
    A.class "sm:hover:via-green-500"


smHoverViaGreen600 : Html.Attribute msg
smHoverViaGreen600 =
    A.class "sm:hover:via-green-600"


smHoverViaGreen700 : Html.Attribute msg
smHoverViaGreen700 =
    A.class "sm:hover:via-green-700"


smHoverViaGreen800 : Html.Attribute msg
smHoverViaGreen800 =
    A.class "sm:hover:via-green-800"


smHoverViaGreen900 : Html.Attribute msg
smHoverViaGreen900 =
    A.class "sm:hover:via-green-900"


smHoverViaTeal100 : Html.Attribute msg
smHoverViaTeal100 =
    A.class "sm:hover:via-teal-100"


smHoverViaTeal200 : Html.Attribute msg
smHoverViaTeal200 =
    A.class "sm:hover:via-teal-200"


smHoverViaTeal300 : Html.Attribute msg
smHoverViaTeal300 =
    A.class "sm:hover:via-teal-300"


smHoverViaTeal400 : Html.Attribute msg
smHoverViaTeal400 =
    A.class "sm:hover:via-teal-400"


smHoverViaTeal500 : Html.Attribute msg
smHoverViaTeal500 =
    A.class "sm:hover:via-teal-500"


smHoverViaTeal600 : Html.Attribute msg
smHoverViaTeal600 =
    A.class "sm:hover:via-teal-600"


smHoverViaTeal700 : Html.Attribute msg
smHoverViaTeal700 =
    A.class "sm:hover:via-teal-700"


smHoverViaTeal800 : Html.Attribute msg
smHoverViaTeal800 =
    A.class "sm:hover:via-teal-800"


smHoverViaTeal900 : Html.Attribute msg
smHoverViaTeal900 =
    A.class "sm:hover:via-teal-900"


smHoverViaBlue100 : Html.Attribute msg
smHoverViaBlue100 =
    A.class "sm:hover:via-blue-100"


smHoverViaBlue200 : Html.Attribute msg
smHoverViaBlue200 =
    A.class "sm:hover:via-blue-200"


smHoverViaBlue300 : Html.Attribute msg
smHoverViaBlue300 =
    A.class "sm:hover:via-blue-300"


smHoverViaBlue400 : Html.Attribute msg
smHoverViaBlue400 =
    A.class "sm:hover:via-blue-400"


smHoverViaBlue500 : Html.Attribute msg
smHoverViaBlue500 =
    A.class "sm:hover:via-blue-500"


smHoverViaBlue600 : Html.Attribute msg
smHoverViaBlue600 =
    A.class "sm:hover:via-blue-600"


smHoverViaBlue700 : Html.Attribute msg
smHoverViaBlue700 =
    A.class "sm:hover:via-blue-700"


smHoverViaBlue800 : Html.Attribute msg
smHoverViaBlue800 =
    A.class "sm:hover:via-blue-800"


smHoverViaBlue900 : Html.Attribute msg
smHoverViaBlue900 =
    A.class "sm:hover:via-blue-900"


smHoverViaIndigo100 : Html.Attribute msg
smHoverViaIndigo100 =
    A.class "sm:hover:via-indigo-100"


smHoverViaIndigo200 : Html.Attribute msg
smHoverViaIndigo200 =
    A.class "sm:hover:via-indigo-200"


smHoverViaIndigo300 : Html.Attribute msg
smHoverViaIndigo300 =
    A.class "sm:hover:via-indigo-300"


smHoverViaIndigo400 : Html.Attribute msg
smHoverViaIndigo400 =
    A.class "sm:hover:via-indigo-400"


smHoverViaIndigo500 : Html.Attribute msg
smHoverViaIndigo500 =
    A.class "sm:hover:via-indigo-500"


smHoverViaIndigo600 : Html.Attribute msg
smHoverViaIndigo600 =
    A.class "sm:hover:via-indigo-600"


smHoverViaIndigo700 : Html.Attribute msg
smHoverViaIndigo700 =
    A.class "sm:hover:via-indigo-700"


smHoverViaIndigo800 : Html.Attribute msg
smHoverViaIndigo800 =
    A.class "sm:hover:via-indigo-800"


smHoverViaIndigo900 : Html.Attribute msg
smHoverViaIndigo900 =
    A.class "sm:hover:via-indigo-900"


smHoverViaPurple100 : Html.Attribute msg
smHoverViaPurple100 =
    A.class "sm:hover:via-purple-100"


smHoverViaPurple200 : Html.Attribute msg
smHoverViaPurple200 =
    A.class "sm:hover:via-purple-200"


smHoverViaPurple300 : Html.Attribute msg
smHoverViaPurple300 =
    A.class "sm:hover:via-purple-300"


smHoverViaPurple400 : Html.Attribute msg
smHoverViaPurple400 =
    A.class "sm:hover:via-purple-400"


smHoverViaPurple500 : Html.Attribute msg
smHoverViaPurple500 =
    A.class "sm:hover:via-purple-500"


smHoverViaPurple600 : Html.Attribute msg
smHoverViaPurple600 =
    A.class "sm:hover:via-purple-600"


smHoverViaPurple700 : Html.Attribute msg
smHoverViaPurple700 =
    A.class "sm:hover:via-purple-700"


smHoverViaPurple800 : Html.Attribute msg
smHoverViaPurple800 =
    A.class "sm:hover:via-purple-800"


smHoverViaPurple900 : Html.Attribute msg
smHoverViaPurple900 =
    A.class "sm:hover:via-purple-900"


smHoverViaPink100 : Html.Attribute msg
smHoverViaPink100 =
    A.class "sm:hover:via-pink-100"


smHoverViaPink200 : Html.Attribute msg
smHoverViaPink200 =
    A.class "sm:hover:via-pink-200"


smHoverViaPink300 : Html.Attribute msg
smHoverViaPink300 =
    A.class "sm:hover:via-pink-300"


smHoverViaPink400 : Html.Attribute msg
smHoverViaPink400 =
    A.class "sm:hover:via-pink-400"


smHoverViaPink500 : Html.Attribute msg
smHoverViaPink500 =
    A.class "sm:hover:via-pink-500"


smHoverViaPink600 : Html.Attribute msg
smHoverViaPink600 =
    A.class "sm:hover:via-pink-600"


smHoverViaPink700 : Html.Attribute msg
smHoverViaPink700 =
    A.class "sm:hover:via-pink-700"


smHoverViaPink800 : Html.Attribute msg
smHoverViaPink800 =
    A.class "sm:hover:via-pink-800"


smHoverViaPink900 : Html.Attribute msg
smHoverViaPink900 =
    A.class "sm:hover:via-pink-900"


smHoverToTransparent : Html.Attribute msg
smHoverToTransparent =
    A.class "sm:hover:to-transparent"


smHoverToCurrent : Html.Attribute msg
smHoverToCurrent =
    A.class "sm:hover:to-current"


smHoverToBlack : Html.Attribute msg
smHoverToBlack =
    A.class "sm:hover:to-black"


smHoverToWhite : Html.Attribute msg
smHoverToWhite =
    A.class "sm:hover:to-white"


smHoverToGray100 : Html.Attribute msg
smHoverToGray100 =
    A.class "sm:hover:to-gray-100"


smHoverToGray200 : Html.Attribute msg
smHoverToGray200 =
    A.class "sm:hover:to-gray-200"


smHoverToGray300 : Html.Attribute msg
smHoverToGray300 =
    A.class "sm:hover:to-gray-300"


smHoverToGray400 : Html.Attribute msg
smHoverToGray400 =
    A.class "sm:hover:to-gray-400"


smHoverToGray500 : Html.Attribute msg
smHoverToGray500 =
    A.class "sm:hover:to-gray-500"


smHoverToGray600 : Html.Attribute msg
smHoverToGray600 =
    A.class "sm:hover:to-gray-600"


smHoverToGray700 : Html.Attribute msg
smHoverToGray700 =
    A.class "sm:hover:to-gray-700"


smHoverToGray800 : Html.Attribute msg
smHoverToGray800 =
    A.class "sm:hover:to-gray-800"


smHoverToGray900 : Html.Attribute msg
smHoverToGray900 =
    A.class "sm:hover:to-gray-900"


smHoverToRed100 : Html.Attribute msg
smHoverToRed100 =
    A.class "sm:hover:to-red-100"


smHoverToRed200 : Html.Attribute msg
smHoverToRed200 =
    A.class "sm:hover:to-red-200"


smHoverToRed300 : Html.Attribute msg
smHoverToRed300 =
    A.class "sm:hover:to-red-300"


smHoverToRed400 : Html.Attribute msg
smHoverToRed400 =
    A.class "sm:hover:to-red-400"


smHoverToRed500 : Html.Attribute msg
smHoverToRed500 =
    A.class "sm:hover:to-red-500"


smHoverToRed600 : Html.Attribute msg
smHoverToRed600 =
    A.class "sm:hover:to-red-600"


smHoverToRed700 : Html.Attribute msg
smHoverToRed700 =
    A.class "sm:hover:to-red-700"


smHoverToRed800 : Html.Attribute msg
smHoverToRed800 =
    A.class "sm:hover:to-red-800"


smHoverToRed900 : Html.Attribute msg
smHoverToRed900 =
    A.class "sm:hover:to-red-900"


smHoverToOrange100 : Html.Attribute msg
smHoverToOrange100 =
    A.class "sm:hover:to-orange-100"


smHoverToOrange200 : Html.Attribute msg
smHoverToOrange200 =
    A.class "sm:hover:to-orange-200"


smHoverToOrange300 : Html.Attribute msg
smHoverToOrange300 =
    A.class "sm:hover:to-orange-300"


smHoverToOrange400 : Html.Attribute msg
smHoverToOrange400 =
    A.class "sm:hover:to-orange-400"


smHoverToOrange500 : Html.Attribute msg
smHoverToOrange500 =
    A.class "sm:hover:to-orange-500"


smHoverToOrange600 : Html.Attribute msg
smHoverToOrange600 =
    A.class "sm:hover:to-orange-600"


smHoverToOrange700 : Html.Attribute msg
smHoverToOrange700 =
    A.class "sm:hover:to-orange-700"


smHoverToOrange800 : Html.Attribute msg
smHoverToOrange800 =
    A.class "sm:hover:to-orange-800"


smHoverToOrange900 : Html.Attribute msg
smHoverToOrange900 =
    A.class "sm:hover:to-orange-900"


smHoverToYellow100 : Html.Attribute msg
smHoverToYellow100 =
    A.class "sm:hover:to-yellow-100"


smHoverToYellow200 : Html.Attribute msg
smHoverToYellow200 =
    A.class "sm:hover:to-yellow-200"


smHoverToYellow300 : Html.Attribute msg
smHoverToYellow300 =
    A.class "sm:hover:to-yellow-300"


smHoverToYellow400 : Html.Attribute msg
smHoverToYellow400 =
    A.class "sm:hover:to-yellow-400"


smHoverToYellow500 : Html.Attribute msg
smHoverToYellow500 =
    A.class "sm:hover:to-yellow-500"


smHoverToYellow600 : Html.Attribute msg
smHoverToYellow600 =
    A.class "sm:hover:to-yellow-600"


smHoverToYellow700 : Html.Attribute msg
smHoverToYellow700 =
    A.class "sm:hover:to-yellow-700"


smHoverToYellow800 : Html.Attribute msg
smHoverToYellow800 =
    A.class "sm:hover:to-yellow-800"


smHoverToYellow900 : Html.Attribute msg
smHoverToYellow900 =
    A.class "sm:hover:to-yellow-900"


smHoverToGreen100 : Html.Attribute msg
smHoverToGreen100 =
    A.class "sm:hover:to-green-100"


smHoverToGreen200 : Html.Attribute msg
smHoverToGreen200 =
    A.class "sm:hover:to-green-200"


smHoverToGreen300 : Html.Attribute msg
smHoverToGreen300 =
    A.class "sm:hover:to-green-300"


smHoverToGreen400 : Html.Attribute msg
smHoverToGreen400 =
    A.class "sm:hover:to-green-400"


smHoverToGreen500 : Html.Attribute msg
smHoverToGreen500 =
    A.class "sm:hover:to-green-500"


smHoverToGreen600 : Html.Attribute msg
smHoverToGreen600 =
    A.class "sm:hover:to-green-600"


smHoverToGreen700 : Html.Attribute msg
smHoverToGreen700 =
    A.class "sm:hover:to-green-700"


smHoverToGreen800 : Html.Attribute msg
smHoverToGreen800 =
    A.class "sm:hover:to-green-800"


smHoverToGreen900 : Html.Attribute msg
smHoverToGreen900 =
    A.class "sm:hover:to-green-900"


smHoverToTeal100 : Html.Attribute msg
smHoverToTeal100 =
    A.class "sm:hover:to-teal-100"


smHoverToTeal200 : Html.Attribute msg
smHoverToTeal200 =
    A.class "sm:hover:to-teal-200"


smHoverToTeal300 : Html.Attribute msg
smHoverToTeal300 =
    A.class "sm:hover:to-teal-300"


smHoverToTeal400 : Html.Attribute msg
smHoverToTeal400 =
    A.class "sm:hover:to-teal-400"


smHoverToTeal500 : Html.Attribute msg
smHoverToTeal500 =
    A.class "sm:hover:to-teal-500"


smHoverToTeal600 : Html.Attribute msg
smHoverToTeal600 =
    A.class "sm:hover:to-teal-600"


smHoverToTeal700 : Html.Attribute msg
smHoverToTeal700 =
    A.class "sm:hover:to-teal-700"


smHoverToTeal800 : Html.Attribute msg
smHoverToTeal800 =
    A.class "sm:hover:to-teal-800"


smHoverToTeal900 : Html.Attribute msg
smHoverToTeal900 =
    A.class "sm:hover:to-teal-900"


smHoverToBlue100 : Html.Attribute msg
smHoverToBlue100 =
    A.class "sm:hover:to-blue-100"


smHoverToBlue200 : Html.Attribute msg
smHoverToBlue200 =
    A.class "sm:hover:to-blue-200"


smHoverToBlue300 : Html.Attribute msg
smHoverToBlue300 =
    A.class "sm:hover:to-blue-300"


smHoverToBlue400 : Html.Attribute msg
smHoverToBlue400 =
    A.class "sm:hover:to-blue-400"


smHoverToBlue500 : Html.Attribute msg
smHoverToBlue500 =
    A.class "sm:hover:to-blue-500"


smHoverToBlue600 : Html.Attribute msg
smHoverToBlue600 =
    A.class "sm:hover:to-blue-600"


smHoverToBlue700 : Html.Attribute msg
smHoverToBlue700 =
    A.class "sm:hover:to-blue-700"


smHoverToBlue800 : Html.Attribute msg
smHoverToBlue800 =
    A.class "sm:hover:to-blue-800"


smHoverToBlue900 : Html.Attribute msg
smHoverToBlue900 =
    A.class "sm:hover:to-blue-900"


smHoverToIndigo100 : Html.Attribute msg
smHoverToIndigo100 =
    A.class "sm:hover:to-indigo-100"


smHoverToIndigo200 : Html.Attribute msg
smHoverToIndigo200 =
    A.class "sm:hover:to-indigo-200"


smHoverToIndigo300 : Html.Attribute msg
smHoverToIndigo300 =
    A.class "sm:hover:to-indigo-300"


smHoverToIndigo400 : Html.Attribute msg
smHoverToIndigo400 =
    A.class "sm:hover:to-indigo-400"


smHoverToIndigo500 : Html.Attribute msg
smHoverToIndigo500 =
    A.class "sm:hover:to-indigo-500"


smHoverToIndigo600 : Html.Attribute msg
smHoverToIndigo600 =
    A.class "sm:hover:to-indigo-600"


smHoverToIndigo700 : Html.Attribute msg
smHoverToIndigo700 =
    A.class "sm:hover:to-indigo-700"


smHoverToIndigo800 : Html.Attribute msg
smHoverToIndigo800 =
    A.class "sm:hover:to-indigo-800"


smHoverToIndigo900 : Html.Attribute msg
smHoverToIndigo900 =
    A.class "sm:hover:to-indigo-900"


smHoverToPurple100 : Html.Attribute msg
smHoverToPurple100 =
    A.class "sm:hover:to-purple-100"


smHoverToPurple200 : Html.Attribute msg
smHoverToPurple200 =
    A.class "sm:hover:to-purple-200"


smHoverToPurple300 : Html.Attribute msg
smHoverToPurple300 =
    A.class "sm:hover:to-purple-300"


smHoverToPurple400 : Html.Attribute msg
smHoverToPurple400 =
    A.class "sm:hover:to-purple-400"


smHoverToPurple500 : Html.Attribute msg
smHoverToPurple500 =
    A.class "sm:hover:to-purple-500"


smHoverToPurple600 : Html.Attribute msg
smHoverToPurple600 =
    A.class "sm:hover:to-purple-600"


smHoverToPurple700 : Html.Attribute msg
smHoverToPurple700 =
    A.class "sm:hover:to-purple-700"


smHoverToPurple800 : Html.Attribute msg
smHoverToPurple800 =
    A.class "sm:hover:to-purple-800"


smHoverToPurple900 : Html.Attribute msg
smHoverToPurple900 =
    A.class "sm:hover:to-purple-900"


smHoverToPink100 : Html.Attribute msg
smHoverToPink100 =
    A.class "sm:hover:to-pink-100"


smHoverToPink200 : Html.Attribute msg
smHoverToPink200 =
    A.class "sm:hover:to-pink-200"


smHoverToPink300 : Html.Attribute msg
smHoverToPink300 =
    A.class "sm:hover:to-pink-300"


smHoverToPink400 : Html.Attribute msg
smHoverToPink400 =
    A.class "sm:hover:to-pink-400"


smHoverToPink500 : Html.Attribute msg
smHoverToPink500 =
    A.class "sm:hover:to-pink-500"


smHoverToPink600 : Html.Attribute msg
smHoverToPink600 =
    A.class "sm:hover:to-pink-600"


smHoverToPink700 : Html.Attribute msg
smHoverToPink700 =
    A.class "sm:hover:to-pink-700"


smHoverToPink800 : Html.Attribute msg
smHoverToPink800 =
    A.class "sm:hover:to-pink-800"


smHoverToPink900 : Html.Attribute msg
smHoverToPink900 =
    A.class "sm:hover:to-pink-900"


smFocusFromTransparent : Html.Attribute msg
smFocusFromTransparent =
    A.class "sm:focus:from-transparent"


smFocusFromCurrent : Html.Attribute msg
smFocusFromCurrent =
    A.class "sm:focus:from-current"


smFocusFromBlack : Html.Attribute msg
smFocusFromBlack =
    A.class "sm:focus:from-black"


smFocusFromWhite : Html.Attribute msg
smFocusFromWhite =
    A.class "sm:focus:from-white"


smFocusFromGray100 : Html.Attribute msg
smFocusFromGray100 =
    A.class "sm:focus:from-gray-100"


smFocusFromGray200 : Html.Attribute msg
smFocusFromGray200 =
    A.class "sm:focus:from-gray-200"


smFocusFromGray300 : Html.Attribute msg
smFocusFromGray300 =
    A.class "sm:focus:from-gray-300"


smFocusFromGray400 : Html.Attribute msg
smFocusFromGray400 =
    A.class "sm:focus:from-gray-400"


smFocusFromGray500 : Html.Attribute msg
smFocusFromGray500 =
    A.class "sm:focus:from-gray-500"


smFocusFromGray600 : Html.Attribute msg
smFocusFromGray600 =
    A.class "sm:focus:from-gray-600"


smFocusFromGray700 : Html.Attribute msg
smFocusFromGray700 =
    A.class "sm:focus:from-gray-700"


smFocusFromGray800 : Html.Attribute msg
smFocusFromGray800 =
    A.class "sm:focus:from-gray-800"


smFocusFromGray900 : Html.Attribute msg
smFocusFromGray900 =
    A.class "sm:focus:from-gray-900"


smFocusFromRed100 : Html.Attribute msg
smFocusFromRed100 =
    A.class "sm:focus:from-red-100"


smFocusFromRed200 : Html.Attribute msg
smFocusFromRed200 =
    A.class "sm:focus:from-red-200"


smFocusFromRed300 : Html.Attribute msg
smFocusFromRed300 =
    A.class "sm:focus:from-red-300"


smFocusFromRed400 : Html.Attribute msg
smFocusFromRed400 =
    A.class "sm:focus:from-red-400"


smFocusFromRed500 : Html.Attribute msg
smFocusFromRed500 =
    A.class "sm:focus:from-red-500"


smFocusFromRed600 : Html.Attribute msg
smFocusFromRed600 =
    A.class "sm:focus:from-red-600"


smFocusFromRed700 : Html.Attribute msg
smFocusFromRed700 =
    A.class "sm:focus:from-red-700"


smFocusFromRed800 : Html.Attribute msg
smFocusFromRed800 =
    A.class "sm:focus:from-red-800"


smFocusFromRed900 : Html.Attribute msg
smFocusFromRed900 =
    A.class "sm:focus:from-red-900"


smFocusFromOrange100 : Html.Attribute msg
smFocusFromOrange100 =
    A.class "sm:focus:from-orange-100"


smFocusFromOrange200 : Html.Attribute msg
smFocusFromOrange200 =
    A.class "sm:focus:from-orange-200"


smFocusFromOrange300 : Html.Attribute msg
smFocusFromOrange300 =
    A.class "sm:focus:from-orange-300"


smFocusFromOrange400 : Html.Attribute msg
smFocusFromOrange400 =
    A.class "sm:focus:from-orange-400"


smFocusFromOrange500 : Html.Attribute msg
smFocusFromOrange500 =
    A.class "sm:focus:from-orange-500"


smFocusFromOrange600 : Html.Attribute msg
smFocusFromOrange600 =
    A.class "sm:focus:from-orange-600"


smFocusFromOrange700 : Html.Attribute msg
smFocusFromOrange700 =
    A.class "sm:focus:from-orange-700"


smFocusFromOrange800 : Html.Attribute msg
smFocusFromOrange800 =
    A.class "sm:focus:from-orange-800"


smFocusFromOrange900 : Html.Attribute msg
smFocusFromOrange900 =
    A.class "sm:focus:from-orange-900"


smFocusFromYellow100 : Html.Attribute msg
smFocusFromYellow100 =
    A.class "sm:focus:from-yellow-100"


smFocusFromYellow200 : Html.Attribute msg
smFocusFromYellow200 =
    A.class "sm:focus:from-yellow-200"


smFocusFromYellow300 : Html.Attribute msg
smFocusFromYellow300 =
    A.class "sm:focus:from-yellow-300"


smFocusFromYellow400 : Html.Attribute msg
smFocusFromYellow400 =
    A.class "sm:focus:from-yellow-400"


smFocusFromYellow500 : Html.Attribute msg
smFocusFromYellow500 =
    A.class "sm:focus:from-yellow-500"


smFocusFromYellow600 : Html.Attribute msg
smFocusFromYellow600 =
    A.class "sm:focus:from-yellow-600"


smFocusFromYellow700 : Html.Attribute msg
smFocusFromYellow700 =
    A.class "sm:focus:from-yellow-700"


smFocusFromYellow800 : Html.Attribute msg
smFocusFromYellow800 =
    A.class "sm:focus:from-yellow-800"


smFocusFromYellow900 : Html.Attribute msg
smFocusFromYellow900 =
    A.class "sm:focus:from-yellow-900"


smFocusFromGreen100 : Html.Attribute msg
smFocusFromGreen100 =
    A.class "sm:focus:from-green-100"


smFocusFromGreen200 : Html.Attribute msg
smFocusFromGreen200 =
    A.class "sm:focus:from-green-200"


smFocusFromGreen300 : Html.Attribute msg
smFocusFromGreen300 =
    A.class "sm:focus:from-green-300"


smFocusFromGreen400 : Html.Attribute msg
smFocusFromGreen400 =
    A.class "sm:focus:from-green-400"


smFocusFromGreen500 : Html.Attribute msg
smFocusFromGreen500 =
    A.class "sm:focus:from-green-500"


smFocusFromGreen600 : Html.Attribute msg
smFocusFromGreen600 =
    A.class "sm:focus:from-green-600"


smFocusFromGreen700 : Html.Attribute msg
smFocusFromGreen700 =
    A.class "sm:focus:from-green-700"


smFocusFromGreen800 : Html.Attribute msg
smFocusFromGreen800 =
    A.class "sm:focus:from-green-800"


smFocusFromGreen900 : Html.Attribute msg
smFocusFromGreen900 =
    A.class "sm:focus:from-green-900"


smFocusFromTeal100 : Html.Attribute msg
smFocusFromTeal100 =
    A.class "sm:focus:from-teal-100"


smFocusFromTeal200 : Html.Attribute msg
smFocusFromTeal200 =
    A.class "sm:focus:from-teal-200"


smFocusFromTeal300 : Html.Attribute msg
smFocusFromTeal300 =
    A.class "sm:focus:from-teal-300"


smFocusFromTeal400 : Html.Attribute msg
smFocusFromTeal400 =
    A.class "sm:focus:from-teal-400"


smFocusFromTeal500 : Html.Attribute msg
smFocusFromTeal500 =
    A.class "sm:focus:from-teal-500"


smFocusFromTeal600 : Html.Attribute msg
smFocusFromTeal600 =
    A.class "sm:focus:from-teal-600"


smFocusFromTeal700 : Html.Attribute msg
smFocusFromTeal700 =
    A.class "sm:focus:from-teal-700"


smFocusFromTeal800 : Html.Attribute msg
smFocusFromTeal800 =
    A.class "sm:focus:from-teal-800"


smFocusFromTeal900 : Html.Attribute msg
smFocusFromTeal900 =
    A.class "sm:focus:from-teal-900"


smFocusFromBlue100 : Html.Attribute msg
smFocusFromBlue100 =
    A.class "sm:focus:from-blue-100"


smFocusFromBlue200 : Html.Attribute msg
smFocusFromBlue200 =
    A.class "sm:focus:from-blue-200"


smFocusFromBlue300 : Html.Attribute msg
smFocusFromBlue300 =
    A.class "sm:focus:from-blue-300"


smFocusFromBlue400 : Html.Attribute msg
smFocusFromBlue400 =
    A.class "sm:focus:from-blue-400"


smFocusFromBlue500 : Html.Attribute msg
smFocusFromBlue500 =
    A.class "sm:focus:from-blue-500"


smFocusFromBlue600 : Html.Attribute msg
smFocusFromBlue600 =
    A.class "sm:focus:from-blue-600"


smFocusFromBlue700 : Html.Attribute msg
smFocusFromBlue700 =
    A.class "sm:focus:from-blue-700"


smFocusFromBlue800 : Html.Attribute msg
smFocusFromBlue800 =
    A.class "sm:focus:from-blue-800"


smFocusFromBlue900 : Html.Attribute msg
smFocusFromBlue900 =
    A.class "sm:focus:from-blue-900"


smFocusFromIndigo100 : Html.Attribute msg
smFocusFromIndigo100 =
    A.class "sm:focus:from-indigo-100"


smFocusFromIndigo200 : Html.Attribute msg
smFocusFromIndigo200 =
    A.class "sm:focus:from-indigo-200"


smFocusFromIndigo300 : Html.Attribute msg
smFocusFromIndigo300 =
    A.class "sm:focus:from-indigo-300"


smFocusFromIndigo400 : Html.Attribute msg
smFocusFromIndigo400 =
    A.class "sm:focus:from-indigo-400"


smFocusFromIndigo500 : Html.Attribute msg
smFocusFromIndigo500 =
    A.class "sm:focus:from-indigo-500"


smFocusFromIndigo600 : Html.Attribute msg
smFocusFromIndigo600 =
    A.class "sm:focus:from-indigo-600"


smFocusFromIndigo700 : Html.Attribute msg
smFocusFromIndigo700 =
    A.class "sm:focus:from-indigo-700"


smFocusFromIndigo800 : Html.Attribute msg
smFocusFromIndigo800 =
    A.class "sm:focus:from-indigo-800"


smFocusFromIndigo900 : Html.Attribute msg
smFocusFromIndigo900 =
    A.class "sm:focus:from-indigo-900"


smFocusFromPurple100 : Html.Attribute msg
smFocusFromPurple100 =
    A.class "sm:focus:from-purple-100"


smFocusFromPurple200 : Html.Attribute msg
smFocusFromPurple200 =
    A.class "sm:focus:from-purple-200"


smFocusFromPurple300 : Html.Attribute msg
smFocusFromPurple300 =
    A.class "sm:focus:from-purple-300"


smFocusFromPurple400 : Html.Attribute msg
smFocusFromPurple400 =
    A.class "sm:focus:from-purple-400"


smFocusFromPurple500 : Html.Attribute msg
smFocusFromPurple500 =
    A.class "sm:focus:from-purple-500"


smFocusFromPurple600 : Html.Attribute msg
smFocusFromPurple600 =
    A.class "sm:focus:from-purple-600"


smFocusFromPurple700 : Html.Attribute msg
smFocusFromPurple700 =
    A.class "sm:focus:from-purple-700"


smFocusFromPurple800 : Html.Attribute msg
smFocusFromPurple800 =
    A.class "sm:focus:from-purple-800"


smFocusFromPurple900 : Html.Attribute msg
smFocusFromPurple900 =
    A.class "sm:focus:from-purple-900"


smFocusFromPink100 : Html.Attribute msg
smFocusFromPink100 =
    A.class "sm:focus:from-pink-100"


smFocusFromPink200 : Html.Attribute msg
smFocusFromPink200 =
    A.class "sm:focus:from-pink-200"


smFocusFromPink300 : Html.Attribute msg
smFocusFromPink300 =
    A.class "sm:focus:from-pink-300"


smFocusFromPink400 : Html.Attribute msg
smFocusFromPink400 =
    A.class "sm:focus:from-pink-400"


smFocusFromPink500 : Html.Attribute msg
smFocusFromPink500 =
    A.class "sm:focus:from-pink-500"


smFocusFromPink600 : Html.Attribute msg
smFocusFromPink600 =
    A.class "sm:focus:from-pink-600"


smFocusFromPink700 : Html.Attribute msg
smFocusFromPink700 =
    A.class "sm:focus:from-pink-700"


smFocusFromPink800 : Html.Attribute msg
smFocusFromPink800 =
    A.class "sm:focus:from-pink-800"


smFocusFromPink900 : Html.Attribute msg
smFocusFromPink900 =
    A.class "sm:focus:from-pink-900"


smFocusViaTransparent : Html.Attribute msg
smFocusViaTransparent =
    A.class "sm:focus:via-transparent"


smFocusViaCurrent : Html.Attribute msg
smFocusViaCurrent =
    A.class "sm:focus:via-current"


smFocusViaBlack : Html.Attribute msg
smFocusViaBlack =
    A.class "sm:focus:via-black"


smFocusViaWhite : Html.Attribute msg
smFocusViaWhite =
    A.class "sm:focus:via-white"


smFocusViaGray100 : Html.Attribute msg
smFocusViaGray100 =
    A.class "sm:focus:via-gray-100"


smFocusViaGray200 : Html.Attribute msg
smFocusViaGray200 =
    A.class "sm:focus:via-gray-200"


smFocusViaGray300 : Html.Attribute msg
smFocusViaGray300 =
    A.class "sm:focus:via-gray-300"


smFocusViaGray400 : Html.Attribute msg
smFocusViaGray400 =
    A.class "sm:focus:via-gray-400"


smFocusViaGray500 : Html.Attribute msg
smFocusViaGray500 =
    A.class "sm:focus:via-gray-500"


smFocusViaGray600 : Html.Attribute msg
smFocusViaGray600 =
    A.class "sm:focus:via-gray-600"


smFocusViaGray700 : Html.Attribute msg
smFocusViaGray700 =
    A.class "sm:focus:via-gray-700"


smFocusViaGray800 : Html.Attribute msg
smFocusViaGray800 =
    A.class "sm:focus:via-gray-800"


smFocusViaGray900 : Html.Attribute msg
smFocusViaGray900 =
    A.class "sm:focus:via-gray-900"


smFocusViaRed100 : Html.Attribute msg
smFocusViaRed100 =
    A.class "sm:focus:via-red-100"


smFocusViaRed200 : Html.Attribute msg
smFocusViaRed200 =
    A.class "sm:focus:via-red-200"


smFocusViaRed300 : Html.Attribute msg
smFocusViaRed300 =
    A.class "sm:focus:via-red-300"


smFocusViaRed400 : Html.Attribute msg
smFocusViaRed400 =
    A.class "sm:focus:via-red-400"


smFocusViaRed500 : Html.Attribute msg
smFocusViaRed500 =
    A.class "sm:focus:via-red-500"


smFocusViaRed600 : Html.Attribute msg
smFocusViaRed600 =
    A.class "sm:focus:via-red-600"


smFocusViaRed700 : Html.Attribute msg
smFocusViaRed700 =
    A.class "sm:focus:via-red-700"


smFocusViaRed800 : Html.Attribute msg
smFocusViaRed800 =
    A.class "sm:focus:via-red-800"


smFocusViaRed900 : Html.Attribute msg
smFocusViaRed900 =
    A.class "sm:focus:via-red-900"


smFocusViaOrange100 : Html.Attribute msg
smFocusViaOrange100 =
    A.class "sm:focus:via-orange-100"


smFocusViaOrange200 : Html.Attribute msg
smFocusViaOrange200 =
    A.class "sm:focus:via-orange-200"


smFocusViaOrange300 : Html.Attribute msg
smFocusViaOrange300 =
    A.class "sm:focus:via-orange-300"


smFocusViaOrange400 : Html.Attribute msg
smFocusViaOrange400 =
    A.class "sm:focus:via-orange-400"


smFocusViaOrange500 : Html.Attribute msg
smFocusViaOrange500 =
    A.class "sm:focus:via-orange-500"


smFocusViaOrange600 : Html.Attribute msg
smFocusViaOrange600 =
    A.class "sm:focus:via-orange-600"


smFocusViaOrange700 : Html.Attribute msg
smFocusViaOrange700 =
    A.class "sm:focus:via-orange-700"


smFocusViaOrange800 : Html.Attribute msg
smFocusViaOrange800 =
    A.class "sm:focus:via-orange-800"


smFocusViaOrange900 : Html.Attribute msg
smFocusViaOrange900 =
    A.class "sm:focus:via-orange-900"


smFocusViaYellow100 : Html.Attribute msg
smFocusViaYellow100 =
    A.class "sm:focus:via-yellow-100"


smFocusViaYellow200 : Html.Attribute msg
smFocusViaYellow200 =
    A.class "sm:focus:via-yellow-200"


smFocusViaYellow300 : Html.Attribute msg
smFocusViaYellow300 =
    A.class "sm:focus:via-yellow-300"


smFocusViaYellow400 : Html.Attribute msg
smFocusViaYellow400 =
    A.class "sm:focus:via-yellow-400"


smFocusViaYellow500 : Html.Attribute msg
smFocusViaYellow500 =
    A.class "sm:focus:via-yellow-500"


smFocusViaYellow600 : Html.Attribute msg
smFocusViaYellow600 =
    A.class "sm:focus:via-yellow-600"


smFocusViaYellow700 : Html.Attribute msg
smFocusViaYellow700 =
    A.class "sm:focus:via-yellow-700"


smFocusViaYellow800 : Html.Attribute msg
smFocusViaYellow800 =
    A.class "sm:focus:via-yellow-800"


smFocusViaYellow900 : Html.Attribute msg
smFocusViaYellow900 =
    A.class "sm:focus:via-yellow-900"


smFocusViaGreen100 : Html.Attribute msg
smFocusViaGreen100 =
    A.class "sm:focus:via-green-100"


smFocusViaGreen200 : Html.Attribute msg
smFocusViaGreen200 =
    A.class "sm:focus:via-green-200"


smFocusViaGreen300 : Html.Attribute msg
smFocusViaGreen300 =
    A.class "sm:focus:via-green-300"


smFocusViaGreen400 : Html.Attribute msg
smFocusViaGreen400 =
    A.class "sm:focus:via-green-400"


smFocusViaGreen500 : Html.Attribute msg
smFocusViaGreen500 =
    A.class "sm:focus:via-green-500"


smFocusViaGreen600 : Html.Attribute msg
smFocusViaGreen600 =
    A.class "sm:focus:via-green-600"


smFocusViaGreen700 : Html.Attribute msg
smFocusViaGreen700 =
    A.class "sm:focus:via-green-700"


smFocusViaGreen800 : Html.Attribute msg
smFocusViaGreen800 =
    A.class "sm:focus:via-green-800"


smFocusViaGreen900 : Html.Attribute msg
smFocusViaGreen900 =
    A.class "sm:focus:via-green-900"


smFocusViaTeal100 : Html.Attribute msg
smFocusViaTeal100 =
    A.class "sm:focus:via-teal-100"


smFocusViaTeal200 : Html.Attribute msg
smFocusViaTeal200 =
    A.class "sm:focus:via-teal-200"


smFocusViaTeal300 : Html.Attribute msg
smFocusViaTeal300 =
    A.class "sm:focus:via-teal-300"


smFocusViaTeal400 : Html.Attribute msg
smFocusViaTeal400 =
    A.class "sm:focus:via-teal-400"


smFocusViaTeal500 : Html.Attribute msg
smFocusViaTeal500 =
    A.class "sm:focus:via-teal-500"


smFocusViaTeal600 : Html.Attribute msg
smFocusViaTeal600 =
    A.class "sm:focus:via-teal-600"


smFocusViaTeal700 : Html.Attribute msg
smFocusViaTeal700 =
    A.class "sm:focus:via-teal-700"


smFocusViaTeal800 : Html.Attribute msg
smFocusViaTeal800 =
    A.class "sm:focus:via-teal-800"


smFocusViaTeal900 : Html.Attribute msg
smFocusViaTeal900 =
    A.class "sm:focus:via-teal-900"


smFocusViaBlue100 : Html.Attribute msg
smFocusViaBlue100 =
    A.class "sm:focus:via-blue-100"


smFocusViaBlue200 : Html.Attribute msg
smFocusViaBlue200 =
    A.class "sm:focus:via-blue-200"


smFocusViaBlue300 : Html.Attribute msg
smFocusViaBlue300 =
    A.class "sm:focus:via-blue-300"


smFocusViaBlue400 : Html.Attribute msg
smFocusViaBlue400 =
    A.class "sm:focus:via-blue-400"


smFocusViaBlue500 : Html.Attribute msg
smFocusViaBlue500 =
    A.class "sm:focus:via-blue-500"


smFocusViaBlue600 : Html.Attribute msg
smFocusViaBlue600 =
    A.class "sm:focus:via-blue-600"


smFocusViaBlue700 : Html.Attribute msg
smFocusViaBlue700 =
    A.class "sm:focus:via-blue-700"


smFocusViaBlue800 : Html.Attribute msg
smFocusViaBlue800 =
    A.class "sm:focus:via-blue-800"


smFocusViaBlue900 : Html.Attribute msg
smFocusViaBlue900 =
    A.class "sm:focus:via-blue-900"


smFocusViaIndigo100 : Html.Attribute msg
smFocusViaIndigo100 =
    A.class "sm:focus:via-indigo-100"


smFocusViaIndigo200 : Html.Attribute msg
smFocusViaIndigo200 =
    A.class "sm:focus:via-indigo-200"


smFocusViaIndigo300 : Html.Attribute msg
smFocusViaIndigo300 =
    A.class "sm:focus:via-indigo-300"


smFocusViaIndigo400 : Html.Attribute msg
smFocusViaIndigo400 =
    A.class "sm:focus:via-indigo-400"


smFocusViaIndigo500 : Html.Attribute msg
smFocusViaIndigo500 =
    A.class "sm:focus:via-indigo-500"


smFocusViaIndigo600 : Html.Attribute msg
smFocusViaIndigo600 =
    A.class "sm:focus:via-indigo-600"


smFocusViaIndigo700 : Html.Attribute msg
smFocusViaIndigo700 =
    A.class "sm:focus:via-indigo-700"


smFocusViaIndigo800 : Html.Attribute msg
smFocusViaIndigo800 =
    A.class "sm:focus:via-indigo-800"


smFocusViaIndigo900 : Html.Attribute msg
smFocusViaIndigo900 =
    A.class "sm:focus:via-indigo-900"


smFocusViaPurple100 : Html.Attribute msg
smFocusViaPurple100 =
    A.class "sm:focus:via-purple-100"


smFocusViaPurple200 : Html.Attribute msg
smFocusViaPurple200 =
    A.class "sm:focus:via-purple-200"


smFocusViaPurple300 : Html.Attribute msg
smFocusViaPurple300 =
    A.class "sm:focus:via-purple-300"


smFocusViaPurple400 : Html.Attribute msg
smFocusViaPurple400 =
    A.class "sm:focus:via-purple-400"


smFocusViaPurple500 : Html.Attribute msg
smFocusViaPurple500 =
    A.class "sm:focus:via-purple-500"


smFocusViaPurple600 : Html.Attribute msg
smFocusViaPurple600 =
    A.class "sm:focus:via-purple-600"


smFocusViaPurple700 : Html.Attribute msg
smFocusViaPurple700 =
    A.class "sm:focus:via-purple-700"


smFocusViaPurple800 : Html.Attribute msg
smFocusViaPurple800 =
    A.class "sm:focus:via-purple-800"


smFocusViaPurple900 : Html.Attribute msg
smFocusViaPurple900 =
    A.class "sm:focus:via-purple-900"


smFocusViaPink100 : Html.Attribute msg
smFocusViaPink100 =
    A.class "sm:focus:via-pink-100"


smFocusViaPink200 : Html.Attribute msg
smFocusViaPink200 =
    A.class "sm:focus:via-pink-200"


smFocusViaPink300 : Html.Attribute msg
smFocusViaPink300 =
    A.class "sm:focus:via-pink-300"


smFocusViaPink400 : Html.Attribute msg
smFocusViaPink400 =
    A.class "sm:focus:via-pink-400"


smFocusViaPink500 : Html.Attribute msg
smFocusViaPink500 =
    A.class "sm:focus:via-pink-500"


smFocusViaPink600 : Html.Attribute msg
smFocusViaPink600 =
    A.class "sm:focus:via-pink-600"


smFocusViaPink700 : Html.Attribute msg
smFocusViaPink700 =
    A.class "sm:focus:via-pink-700"


smFocusViaPink800 : Html.Attribute msg
smFocusViaPink800 =
    A.class "sm:focus:via-pink-800"


smFocusViaPink900 : Html.Attribute msg
smFocusViaPink900 =
    A.class "sm:focus:via-pink-900"


smFocusToTransparent : Html.Attribute msg
smFocusToTransparent =
    A.class "sm:focus:to-transparent"


smFocusToCurrent : Html.Attribute msg
smFocusToCurrent =
    A.class "sm:focus:to-current"


smFocusToBlack : Html.Attribute msg
smFocusToBlack =
    A.class "sm:focus:to-black"


smFocusToWhite : Html.Attribute msg
smFocusToWhite =
    A.class "sm:focus:to-white"


smFocusToGray100 : Html.Attribute msg
smFocusToGray100 =
    A.class "sm:focus:to-gray-100"


smFocusToGray200 : Html.Attribute msg
smFocusToGray200 =
    A.class "sm:focus:to-gray-200"


smFocusToGray300 : Html.Attribute msg
smFocusToGray300 =
    A.class "sm:focus:to-gray-300"


smFocusToGray400 : Html.Attribute msg
smFocusToGray400 =
    A.class "sm:focus:to-gray-400"


smFocusToGray500 : Html.Attribute msg
smFocusToGray500 =
    A.class "sm:focus:to-gray-500"


smFocusToGray600 : Html.Attribute msg
smFocusToGray600 =
    A.class "sm:focus:to-gray-600"


smFocusToGray700 : Html.Attribute msg
smFocusToGray700 =
    A.class "sm:focus:to-gray-700"


smFocusToGray800 : Html.Attribute msg
smFocusToGray800 =
    A.class "sm:focus:to-gray-800"


smFocusToGray900 : Html.Attribute msg
smFocusToGray900 =
    A.class "sm:focus:to-gray-900"


smFocusToRed100 : Html.Attribute msg
smFocusToRed100 =
    A.class "sm:focus:to-red-100"


smFocusToRed200 : Html.Attribute msg
smFocusToRed200 =
    A.class "sm:focus:to-red-200"


smFocusToRed300 : Html.Attribute msg
smFocusToRed300 =
    A.class "sm:focus:to-red-300"


smFocusToRed400 : Html.Attribute msg
smFocusToRed400 =
    A.class "sm:focus:to-red-400"


smFocusToRed500 : Html.Attribute msg
smFocusToRed500 =
    A.class "sm:focus:to-red-500"


smFocusToRed600 : Html.Attribute msg
smFocusToRed600 =
    A.class "sm:focus:to-red-600"


smFocusToRed700 : Html.Attribute msg
smFocusToRed700 =
    A.class "sm:focus:to-red-700"


smFocusToRed800 : Html.Attribute msg
smFocusToRed800 =
    A.class "sm:focus:to-red-800"


smFocusToRed900 : Html.Attribute msg
smFocusToRed900 =
    A.class "sm:focus:to-red-900"


smFocusToOrange100 : Html.Attribute msg
smFocusToOrange100 =
    A.class "sm:focus:to-orange-100"


smFocusToOrange200 : Html.Attribute msg
smFocusToOrange200 =
    A.class "sm:focus:to-orange-200"


smFocusToOrange300 : Html.Attribute msg
smFocusToOrange300 =
    A.class "sm:focus:to-orange-300"


smFocusToOrange400 : Html.Attribute msg
smFocusToOrange400 =
    A.class "sm:focus:to-orange-400"


smFocusToOrange500 : Html.Attribute msg
smFocusToOrange500 =
    A.class "sm:focus:to-orange-500"


smFocusToOrange600 : Html.Attribute msg
smFocusToOrange600 =
    A.class "sm:focus:to-orange-600"


smFocusToOrange700 : Html.Attribute msg
smFocusToOrange700 =
    A.class "sm:focus:to-orange-700"


smFocusToOrange800 : Html.Attribute msg
smFocusToOrange800 =
    A.class "sm:focus:to-orange-800"


smFocusToOrange900 : Html.Attribute msg
smFocusToOrange900 =
    A.class "sm:focus:to-orange-900"


smFocusToYellow100 : Html.Attribute msg
smFocusToYellow100 =
    A.class "sm:focus:to-yellow-100"


smFocusToYellow200 : Html.Attribute msg
smFocusToYellow200 =
    A.class "sm:focus:to-yellow-200"


smFocusToYellow300 : Html.Attribute msg
smFocusToYellow300 =
    A.class "sm:focus:to-yellow-300"


smFocusToYellow400 : Html.Attribute msg
smFocusToYellow400 =
    A.class "sm:focus:to-yellow-400"


smFocusToYellow500 : Html.Attribute msg
smFocusToYellow500 =
    A.class "sm:focus:to-yellow-500"


smFocusToYellow600 : Html.Attribute msg
smFocusToYellow600 =
    A.class "sm:focus:to-yellow-600"


smFocusToYellow700 : Html.Attribute msg
smFocusToYellow700 =
    A.class "sm:focus:to-yellow-700"


smFocusToYellow800 : Html.Attribute msg
smFocusToYellow800 =
    A.class "sm:focus:to-yellow-800"


smFocusToYellow900 : Html.Attribute msg
smFocusToYellow900 =
    A.class "sm:focus:to-yellow-900"


smFocusToGreen100 : Html.Attribute msg
smFocusToGreen100 =
    A.class "sm:focus:to-green-100"


smFocusToGreen200 : Html.Attribute msg
smFocusToGreen200 =
    A.class "sm:focus:to-green-200"


smFocusToGreen300 : Html.Attribute msg
smFocusToGreen300 =
    A.class "sm:focus:to-green-300"


smFocusToGreen400 : Html.Attribute msg
smFocusToGreen400 =
    A.class "sm:focus:to-green-400"


smFocusToGreen500 : Html.Attribute msg
smFocusToGreen500 =
    A.class "sm:focus:to-green-500"


smFocusToGreen600 : Html.Attribute msg
smFocusToGreen600 =
    A.class "sm:focus:to-green-600"


smFocusToGreen700 : Html.Attribute msg
smFocusToGreen700 =
    A.class "sm:focus:to-green-700"


smFocusToGreen800 : Html.Attribute msg
smFocusToGreen800 =
    A.class "sm:focus:to-green-800"


smFocusToGreen900 : Html.Attribute msg
smFocusToGreen900 =
    A.class "sm:focus:to-green-900"


smFocusToTeal100 : Html.Attribute msg
smFocusToTeal100 =
    A.class "sm:focus:to-teal-100"


smFocusToTeal200 : Html.Attribute msg
smFocusToTeal200 =
    A.class "sm:focus:to-teal-200"


smFocusToTeal300 : Html.Attribute msg
smFocusToTeal300 =
    A.class "sm:focus:to-teal-300"


smFocusToTeal400 : Html.Attribute msg
smFocusToTeal400 =
    A.class "sm:focus:to-teal-400"


smFocusToTeal500 : Html.Attribute msg
smFocusToTeal500 =
    A.class "sm:focus:to-teal-500"


smFocusToTeal600 : Html.Attribute msg
smFocusToTeal600 =
    A.class "sm:focus:to-teal-600"


smFocusToTeal700 : Html.Attribute msg
smFocusToTeal700 =
    A.class "sm:focus:to-teal-700"


smFocusToTeal800 : Html.Attribute msg
smFocusToTeal800 =
    A.class "sm:focus:to-teal-800"


smFocusToTeal900 : Html.Attribute msg
smFocusToTeal900 =
    A.class "sm:focus:to-teal-900"


smFocusToBlue100 : Html.Attribute msg
smFocusToBlue100 =
    A.class "sm:focus:to-blue-100"


smFocusToBlue200 : Html.Attribute msg
smFocusToBlue200 =
    A.class "sm:focus:to-blue-200"


smFocusToBlue300 : Html.Attribute msg
smFocusToBlue300 =
    A.class "sm:focus:to-blue-300"


smFocusToBlue400 : Html.Attribute msg
smFocusToBlue400 =
    A.class "sm:focus:to-blue-400"


smFocusToBlue500 : Html.Attribute msg
smFocusToBlue500 =
    A.class "sm:focus:to-blue-500"


smFocusToBlue600 : Html.Attribute msg
smFocusToBlue600 =
    A.class "sm:focus:to-blue-600"


smFocusToBlue700 : Html.Attribute msg
smFocusToBlue700 =
    A.class "sm:focus:to-blue-700"


smFocusToBlue800 : Html.Attribute msg
smFocusToBlue800 =
    A.class "sm:focus:to-blue-800"


smFocusToBlue900 : Html.Attribute msg
smFocusToBlue900 =
    A.class "sm:focus:to-blue-900"


smFocusToIndigo100 : Html.Attribute msg
smFocusToIndigo100 =
    A.class "sm:focus:to-indigo-100"


smFocusToIndigo200 : Html.Attribute msg
smFocusToIndigo200 =
    A.class "sm:focus:to-indigo-200"


smFocusToIndigo300 : Html.Attribute msg
smFocusToIndigo300 =
    A.class "sm:focus:to-indigo-300"


smFocusToIndigo400 : Html.Attribute msg
smFocusToIndigo400 =
    A.class "sm:focus:to-indigo-400"


smFocusToIndigo500 : Html.Attribute msg
smFocusToIndigo500 =
    A.class "sm:focus:to-indigo-500"


smFocusToIndigo600 : Html.Attribute msg
smFocusToIndigo600 =
    A.class "sm:focus:to-indigo-600"


smFocusToIndigo700 : Html.Attribute msg
smFocusToIndigo700 =
    A.class "sm:focus:to-indigo-700"


smFocusToIndigo800 : Html.Attribute msg
smFocusToIndigo800 =
    A.class "sm:focus:to-indigo-800"


smFocusToIndigo900 : Html.Attribute msg
smFocusToIndigo900 =
    A.class "sm:focus:to-indigo-900"


smFocusToPurple100 : Html.Attribute msg
smFocusToPurple100 =
    A.class "sm:focus:to-purple-100"


smFocusToPurple200 : Html.Attribute msg
smFocusToPurple200 =
    A.class "sm:focus:to-purple-200"


smFocusToPurple300 : Html.Attribute msg
smFocusToPurple300 =
    A.class "sm:focus:to-purple-300"


smFocusToPurple400 : Html.Attribute msg
smFocusToPurple400 =
    A.class "sm:focus:to-purple-400"


smFocusToPurple500 : Html.Attribute msg
smFocusToPurple500 =
    A.class "sm:focus:to-purple-500"


smFocusToPurple600 : Html.Attribute msg
smFocusToPurple600 =
    A.class "sm:focus:to-purple-600"


smFocusToPurple700 : Html.Attribute msg
smFocusToPurple700 =
    A.class "sm:focus:to-purple-700"


smFocusToPurple800 : Html.Attribute msg
smFocusToPurple800 =
    A.class "sm:focus:to-purple-800"


smFocusToPurple900 : Html.Attribute msg
smFocusToPurple900 =
    A.class "sm:focus:to-purple-900"


smFocusToPink100 : Html.Attribute msg
smFocusToPink100 =
    A.class "sm:focus:to-pink-100"


smFocusToPink200 : Html.Attribute msg
smFocusToPink200 =
    A.class "sm:focus:to-pink-200"


smFocusToPink300 : Html.Attribute msg
smFocusToPink300 =
    A.class "sm:focus:to-pink-300"


smFocusToPink400 : Html.Attribute msg
smFocusToPink400 =
    A.class "sm:focus:to-pink-400"


smFocusToPink500 : Html.Attribute msg
smFocusToPink500 =
    A.class "sm:focus:to-pink-500"


smFocusToPink600 : Html.Attribute msg
smFocusToPink600 =
    A.class "sm:focus:to-pink-600"


smFocusToPink700 : Html.Attribute msg
smFocusToPink700 =
    A.class "sm:focus:to-pink-700"


smFocusToPink800 : Html.Attribute msg
smFocusToPink800 =
    A.class "sm:focus:to-pink-800"


smFocusToPink900 : Html.Attribute msg
smFocusToPink900 =
    A.class "sm:focus:to-pink-900"


smBgOpacity0 : Html.Attribute msg
smBgOpacity0 =
    A.class "sm:bg-opacity-0"


smBgOpacity25 : Html.Attribute msg
smBgOpacity25 =
    A.class "sm:bg-opacity-25"


smBgOpacity50 : Html.Attribute msg
smBgOpacity50 =
    A.class "sm:bg-opacity-50"


smBgOpacity75 : Html.Attribute msg
smBgOpacity75 =
    A.class "sm:bg-opacity-75"


smBgOpacity100 : Html.Attribute msg
smBgOpacity100 =
    A.class "sm:bg-opacity-100"


smHoverBgOpacity0 : Html.Attribute msg
smHoverBgOpacity0 =
    A.class "sm:hover:bg-opacity-0"


smHoverBgOpacity25 : Html.Attribute msg
smHoverBgOpacity25 =
    A.class "sm:hover:bg-opacity-25"


smHoverBgOpacity50 : Html.Attribute msg
smHoverBgOpacity50 =
    A.class "sm:hover:bg-opacity-50"


smHoverBgOpacity75 : Html.Attribute msg
smHoverBgOpacity75 =
    A.class "sm:hover:bg-opacity-75"


smHoverBgOpacity100 : Html.Attribute msg
smHoverBgOpacity100 =
    A.class "sm:hover:bg-opacity-100"


smFocusBgOpacity0 : Html.Attribute msg
smFocusBgOpacity0 =
    A.class "sm:focus:bg-opacity-0"


smFocusBgOpacity25 : Html.Attribute msg
smFocusBgOpacity25 =
    A.class "sm:focus:bg-opacity-25"


smFocusBgOpacity50 : Html.Attribute msg
smFocusBgOpacity50 =
    A.class "sm:focus:bg-opacity-50"


smFocusBgOpacity75 : Html.Attribute msg
smFocusBgOpacity75 =
    A.class "sm:focus:bg-opacity-75"


smFocusBgOpacity100 : Html.Attribute msg
smFocusBgOpacity100 =
    A.class "sm:focus:bg-opacity-100"


smBgBottom : Html.Attribute msg
smBgBottom =
    A.class "sm:bg-bottom"


smBgCenter : Html.Attribute msg
smBgCenter =
    A.class "sm:bg-center"


smBgLeft : Html.Attribute msg
smBgLeft =
    A.class "sm:bg-left"


smBgLeftBottom : Html.Attribute msg
smBgLeftBottom =
    A.class "sm:bg-left-bottom"


smBgLeftTop : Html.Attribute msg
smBgLeftTop =
    A.class "sm:bg-left-top"


smBgRight : Html.Attribute msg
smBgRight =
    A.class "sm:bg-right"


smBgRightBottom : Html.Attribute msg
smBgRightBottom =
    A.class "sm:bg-right-bottom"


smBgRightTop : Html.Attribute msg
smBgRightTop =
    A.class "sm:bg-right-top"


smBgTop : Html.Attribute msg
smBgTop =
    A.class "sm:bg-top"


smBgRepeat : Html.Attribute msg
smBgRepeat =
    A.class "sm:bg-repeat"


smBgNoRepeat : Html.Attribute msg
smBgNoRepeat =
    A.class "sm:bg-no-repeat"


smBgRepeatX : Html.Attribute msg
smBgRepeatX =
    A.class "sm:bg-repeat-x"


smBgRepeatY : Html.Attribute msg
smBgRepeatY =
    A.class "sm:bg-repeat-y"


smBgRepeatRound : Html.Attribute msg
smBgRepeatRound =
    A.class "sm:bg-repeat-round"


smBgRepeatSpace : Html.Attribute msg
smBgRepeatSpace =
    A.class "sm:bg-repeat-space"


smBgAuto : Html.Attribute msg
smBgAuto =
    A.class "sm:bg-auto"


smBgCover : Html.Attribute msg
smBgCover =
    A.class "sm:bg-cover"


smBgContain : Html.Attribute msg
smBgContain =
    A.class "sm:bg-contain"


smBorderCollapse : Html.Attribute msg
smBorderCollapse =
    A.class "sm:border-collapse"


smBorderSeparate : Html.Attribute msg
smBorderSeparate =
    A.class "sm:border-separate"


smBorderTransparent : Html.Attribute msg
smBorderTransparent =
    A.class "sm:border-transparent"


smBorderCurrent : Html.Attribute msg
smBorderCurrent =
    A.class "sm:border-current"


smBorderBlack : Html.Attribute msg
smBorderBlack =
    A.class "sm:border-black"


smBorderWhite : Html.Attribute msg
smBorderWhite =
    A.class "sm:border-white"


smBorderGray100 : Html.Attribute msg
smBorderGray100 =
    A.class "sm:border-gray-100"


smBorderGray200 : Html.Attribute msg
smBorderGray200 =
    A.class "sm:border-gray-200"


smBorderGray300 : Html.Attribute msg
smBorderGray300 =
    A.class "sm:border-gray-300"


smBorderGray400 : Html.Attribute msg
smBorderGray400 =
    A.class "sm:border-gray-400"


smBorderGray500 : Html.Attribute msg
smBorderGray500 =
    A.class "sm:border-gray-500"


smBorderGray600 : Html.Attribute msg
smBorderGray600 =
    A.class "sm:border-gray-600"


smBorderGray700 : Html.Attribute msg
smBorderGray700 =
    A.class "sm:border-gray-700"


smBorderGray800 : Html.Attribute msg
smBorderGray800 =
    A.class "sm:border-gray-800"


smBorderGray900 : Html.Attribute msg
smBorderGray900 =
    A.class "sm:border-gray-900"


smBorderRed100 : Html.Attribute msg
smBorderRed100 =
    A.class "sm:border-red-100"


smBorderRed200 : Html.Attribute msg
smBorderRed200 =
    A.class "sm:border-red-200"


smBorderRed300 : Html.Attribute msg
smBorderRed300 =
    A.class "sm:border-red-300"


smBorderRed400 : Html.Attribute msg
smBorderRed400 =
    A.class "sm:border-red-400"


smBorderRed500 : Html.Attribute msg
smBorderRed500 =
    A.class "sm:border-red-500"


smBorderRed600 : Html.Attribute msg
smBorderRed600 =
    A.class "sm:border-red-600"


smBorderRed700 : Html.Attribute msg
smBorderRed700 =
    A.class "sm:border-red-700"


smBorderRed800 : Html.Attribute msg
smBorderRed800 =
    A.class "sm:border-red-800"


smBorderRed900 : Html.Attribute msg
smBorderRed900 =
    A.class "sm:border-red-900"


smBorderOrange100 : Html.Attribute msg
smBorderOrange100 =
    A.class "sm:border-orange-100"


smBorderOrange200 : Html.Attribute msg
smBorderOrange200 =
    A.class "sm:border-orange-200"


smBorderOrange300 : Html.Attribute msg
smBorderOrange300 =
    A.class "sm:border-orange-300"


smBorderOrange400 : Html.Attribute msg
smBorderOrange400 =
    A.class "sm:border-orange-400"


smBorderOrange500 : Html.Attribute msg
smBorderOrange500 =
    A.class "sm:border-orange-500"


smBorderOrange600 : Html.Attribute msg
smBorderOrange600 =
    A.class "sm:border-orange-600"


smBorderOrange700 : Html.Attribute msg
smBorderOrange700 =
    A.class "sm:border-orange-700"


smBorderOrange800 : Html.Attribute msg
smBorderOrange800 =
    A.class "sm:border-orange-800"


smBorderOrange900 : Html.Attribute msg
smBorderOrange900 =
    A.class "sm:border-orange-900"


smBorderYellow100 : Html.Attribute msg
smBorderYellow100 =
    A.class "sm:border-yellow-100"


smBorderYellow200 : Html.Attribute msg
smBorderYellow200 =
    A.class "sm:border-yellow-200"


smBorderYellow300 : Html.Attribute msg
smBorderYellow300 =
    A.class "sm:border-yellow-300"


smBorderYellow400 : Html.Attribute msg
smBorderYellow400 =
    A.class "sm:border-yellow-400"


smBorderYellow500 : Html.Attribute msg
smBorderYellow500 =
    A.class "sm:border-yellow-500"


smBorderYellow600 : Html.Attribute msg
smBorderYellow600 =
    A.class "sm:border-yellow-600"


smBorderYellow700 : Html.Attribute msg
smBorderYellow700 =
    A.class "sm:border-yellow-700"


smBorderYellow800 : Html.Attribute msg
smBorderYellow800 =
    A.class "sm:border-yellow-800"


smBorderYellow900 : Html.Attribute msg
smBorderYellow900 =
    A.class "sm:border-yellow-900"


smBorderGreen100 : Html.Attribute msg
smBorderGreen100 =
    A.class "sm:border-green-100"


smBorderGreen200 : Html.Attribute msg
smBorderGreen200 =
    A.class "sm:border-green-200"


smBorderGreen300 : Html.Attribute msg
smBorderGreen300 =
    A.class "sm:border-green-300"


smBorderGreen400 : Html.Attribute msg
smBorderGreen400 =
    A.class "sm:border-green-400"


smBorderGreen500 : Html.Attribute msg
smBorderGreen500 =
    A.class "sm:border-green-500"


smBorderGreen600 : Html.Attribute msg
smBorderGreen600 =
    A.class "sm:border-green-600"


smBorderGreen700 : Html.Attribute msg
smBorderGreen700 =
    A.class "sm:border-green-700"


smBorderGreen800 : Html.Attribute msg
smBorderGreen800 =
    A.class "sm:border-green-800"


smBorderGreen900 : Html.Attribute msg
smBorderGreen900 =
    A.class "sm:border-green-900"


smBorderTeal100 : Html.Attribute msg
smBorderTeal100 =
    A.class "sm:border-teal-100"


smBorderTeal200 : Html.Attribute msg
smBorderTeal200 =
    A.class "sm:border-teal-200"


smBorderTeal300 : Html.Attribute msg
smBorderTeal300 =
    A.class "sm:border-teal-300"


smBorderTeal400 : Html.Attribute msg
smBorderTeal400 =
    A.class "sm:border-teal-400"


smBorderTeal500 : Html.Attribute msg
smBorderTeal500 =
    A.class "sm:border-teal-500"


smBorderTeal600 : Html.Attribute msg
smBorderTeal600 =
    A.class "sm:border-teal-600"


smBorderTeal700 : Html.Attribute msg
smBorderTeal700 =
    A.class "sm:border-teal-700"


smBorderTeal800 : Html.Attribute msg
smBorderTeal800 =
    A.class "sm:border-teal-800"


smBorderTeal900 : Html.Attribute msg
smBorderTeal900 =
    A.class "sm:border-teal-900"


smBorderBlue100 : Html.Attribute msg
smBorderBlue100 =
    A.class "sm:border-blue-100"


smBorderBlue200 : Html.Attribute msg
smBorderBlue200 =
    A.class "sm:border-blue-200"


smBorderBlue300 : Html.Attribute msg
smBorderBlue300 =
    A.class "sm:border-blue-300"


smBorderBlue400 : Html.Attribute msg
smBorderBlue400 =
    A.class "sm:border-blue-400"


smBorderBlue500 : Html.Attribute msg
smBorderBlue500 =
    A.class "sm:border-blue-500"


smBorderBlue600 : Html.Attribute msg
smBorderBlue600 =
    A.class "sm:border-blue-600"


smBorderBlue700 : Html.Attribute msg
smBorderBlue700 =
    A.class "sm:border-blue-700"


smBorderBlue800 : Html.Attribute msg
smBorderBlue800 =
    A.class "sm:border-blue-800"


smBorderBlue900 : Html.Attribute msg
smBorderBlue900 =
    A.class "sm:border-blue-900"


smBorderIndigo100 : Html.Attribute msg
smBorderIndigo100 =
    A.class "sm:border-indigo-100"


smBorderIndigo200 : Html.Attribute msg
smBorderIndigo200 =
    A.class "sm:border-indigo-200"


smBorderIndigo300 : Html.Attribute msg
smBorderIndigo300 =
    A.class "sm:border-indigo-300"


smBorderIndigo400 : Html.Attribute msg
smBorderIndigo400 =
    A.class "sm:border-indigo-400"


smBorderIndigo500 : Html.Attribute msg
smBorderIndigo500 =
    A.class "sm:border-indigo-500"


smBorderIndigo600 : Html.Attribute msg
smBorderIndigo600 =
    A.class "sm:border-indigo-600"


smBorderIndigo700 : Html.Attribute msg
smBorderIndigo700 =
    A.class "sm:border-indigo-700"


smBorderIndigo800 : Html.Attribute msg
smBorderIndigo800 =
    A.class "sm:border-indigo-800"


smBorderIndigo900 : Html.Attribute msg
smBorderIndigo900 =
    A.class "sm:border-indigo-900"


smBorderPurple100 : Html.Attribute msg
smBorderPurple100 =
    A.class "sm:border-purple-100"


smBorderPurple200 : Html.Attribute msg
smBorderPurple200 =
    A.class "sm:border-purple-200"


smBorderPurple300 : Html.Attribute msg
smBorderPurple300 =
    A.class "sm:border-purple-300"


smBorderPurple400 : Html.Attribute msg
smBorderPurple400 =
    A.class "sm:border-purple-400"


smBorderPurple500 : Html.Attribute msg
smBorderPurple500 =
    A.class "sm:border-purple-500"


smBorderPurple600 : Html.Attribute msg
smBorderPurple600 =
    A.class "sm:border-purple-600"


smBorderPurple700 : Html.Attribute msg
smBorderPurple700 =
    A.class "sm:border-purple-700"


smBorderPurple800 : Html.Attribute msg
smBorderPurple800 =
    A.class "sm:border-purple-800"


smBorderPurple900 : Html.Attribute msg
smBorderPurple900 =
    A.class "sm:border-purple-900"


smBorderPink100 : Html.Attribute msg
smBorderPink100 =
    A.class "sm:border-pink-100"


smBorderPink200 : Html.Attribute msg
smBorderPink200 =
    A.class "sm:border-pink-200"


smBorderPink300 : Html.Attribute msg
smBorderPink300 =
    A.class "sm:border-pink-300"


smBorderPink400 : Html.Attribute msg
smBorderPink400 =
    A.class "sm:border-pink-400"


smBorderPink500 : Html.Attribute msg
smBorderPink500 =
    A.class "sm:border-pink-500"


smBorderPink600 : Html.Attribute msg
smBorderPink600 =
    A.class "sm:border-pink-600"


smBorderPink700 : Html.Attribute msg
smBorderPink700 =
    A.class "sm:border-pink-700"


smBorderPink800 : Html.Attribute msg
smBorderPink800 =
    A.class "sm:border-pink-800"


smBorderPink900 : Html.Attribute msg
smBorderPink900 =
    A.class "sm:border-pink-900"


smHoverBorderTransparent : Html.Attribute msg
smHoverBorderTransparent =
    A.class "sm:hover:border-transparent"


smHoverBorderCurrent : Html.Attribute msg
smHoverBorderCurrent =
    A.class "sm:hover:border-current"


smHoverBorderBlack : Html.Attribute msg
smHoverBorderBlack =
    A.class "sm:hover:border-black"


smHoverBorderWhite : Html.Attribute msg
smHoverBorderWhite =
    A.class "sm:hover:border-white"


smHoverBorderGray100 : Html.Attribute msg
smHoverBorderGray100 =
    A.class "sm:hover:border-gray-100"


smHoverBorderGray200 : Html.Attribute msg
smHoverBorderGray200 =
    A.class "sm:hover:border-gray-200"


smHoverBorderGray300 : Html.Attribute msg
smHoverBorderGray300 =
    A.class "sm:hover:border-gray-300"


smHoverBorderGray400 : Html.Attribute msg
smHoverBorderGray400 =
    A.class "sm:hover:border-gray-400"


smHoverBorderGray500 : Html.Attribute msg
smHoverBorderGray500 =
    A.class "sm:hover:border-gray-500"


smHoverBorderGray600 : Html.Attribute msg
smHoverBorderGray600 =
    A.class "sm:hover:border-gray-600"


smHoverBorderGray700 : Html.Attribute msg
smHoverBorderGray700 =
    A.class "sm:hover:border-gray-700"


smHoverBorderGray800 : Html.Attribute msg
smHoverBorderGray800 =
    A.class "sm:hover:border-gray-800"


smHoverBorderGray900 : Html.Attribute msg
smHoverBorderGray900 =
    A.class "sm:hover:border-gray-900"


smHoverBorderRed100 : Html.Attribute msg
smHoverBorderRed100 =
    A.class "sm:hover:border-red-100"


smHoverBorderRed200 : Html.Attribute msg
smHoverBorderRed200 =
    A.class "sm:hover:border-red-200"


smHoverBorderRed300 : Html.Attribute msg
smHoverBorderRed300 =
    A.class "sm:hover:border-red-300"


smHoverBorderRed400 : Html.Attribute msg
smHoverBorderRed400 =
    A.class "sm:hover:border-red-400"


smHoverBorderRed500 : Html.Attribute msg
smHoverBorderRed500 =
    A.class "sm:hover:border-red-500"


smHoverBorderRed600 : Html.Attribute msg
smHoverBorderRed600 =
    A.class "sm:hover:border-red-600"


smHoverBorderRed700 : Html.Attribute msg
smHoverBorderRed700 =
    A.class "sm:hover:border-red-700"


smHoverBorderRed800 : Html.Attribute msg
smHoverBorderRed800 =
    A.class "sm:hover:border-red-800"


smHoverBorderRed900 : Html.Attribute msg
smHoverBorderRed900 =
    A.class "sm:hover:border-red-900"


smHoverBorderOrange100 : Html.Attribute msg
smHoverBorderOrange100 =
    A.class "sm:hover:border-orange-100"


smHoverBorderOrange200 : Html.Attribute msg
smHoverBorderOrange200 =
    A.class "sm:hover:border-orange-200"


smHoverBorderOrange300 : Html.Attribute msg
smHoverBorderOrange300 =
    A.class "sm:hover:border-orange-300"


smHoverBorderOrange400 : Html.Attribute msg
smHoverBorderOrange400 =
    A.class "sm:hover:border-orange-400"


smHoverBorderOrange500 : Html.Attribute msg
smHoverBorderOrange500 =
    A.class "sm:hover:border-orange-500"


smHoverBorderOrange600 : Html.Attribute msg
smHoverBorderOrange600 =
    A.class "sm:hover:border-orange-600"


smHoverBorderOrange700 : Html.Attribute msg
smHoverBorderOrange700 =
    A.class "sm:hover:border-orange-700"


smHoverBorderOrange800 : Html.Attribute msg
smHoverBorderOrange800 =
    A.class "sm:hover:border-orange-800"


smHoverBorderOrange900 : Html.Attribute msg
smHoverBorderOrange900 =
    A.class "sm:hover:border-orange-900"


smHoverBorderYellow100 : Html.Attribute msg
smHoverBorderYellow100 =
    A.class "sm:hover:border-yellow-100"


smHoverBorderYellow200 : Html.Attribute msg
smHoverBorderYellow200 =
    A.class "sm:hover:border-yellow-200"


smHoverBorderYellow300 : Html.Attribute msg
smHoverBorderYellow300 =
    A.class "sm:hover:border-yellow-300"


smHoverBorderYellow400 : Html.Attribute msg
smHoverBorderYellow400 =
    A.class "sm:hover:border-yellow-400"


smHoverBorderYellow500 : Html.Attribute msg
smHoverBorderYellow500 =
    A.class "sm:hover:border-yellow-500"


smHoverBorderYellow600 : Html.Attribute msg
smHoverBorderYellow600 =
    A.class "sm:hover:border-yellow-600"


smHoverBorderYellow700 : Html.Attribute msg
smHoverBorderYellow700 =
    A.class "sm:hover:border-yellow-700"


smHoverBorderYellow800 : Html.Attribute msg
smHoverBorderYellow800 =
    A.class "sm:hover:border-yellow-800"


smHoverBorderYellow900 : Html.Attribute msg
smHoverBorderYellow900 =
    A.class "sm:hover:border-yellow-900"


smHoverBorderGreen100 : Html.Attribute msg
smHoverBorderGreen100 =
    A.class "sm:hover:border-green-100"


smHoverBorderGreen200 : Html.Attribute msg
smHoverBorderGreen200 =
    A.class "sm:hover:border-green-200"


smHoverBorderGreen300 : Html.Attribute msg
smHoverBorderGreen300 =
    A.class "sm:hover:border-green-300"


smHoverBorderGreen400 : Html.Attribute msg
smHoverBorderGreen400 =
    A.class "sm:hover:border-green-400"


smHoverBorderGreen500 : Html.Attribute msg
smHoverBorderGreen500 =
    A.class "sm:hover:border-green-500"


smHoverBorderGreen600 : Html.Attribute msg
smHoverBorderGreen600 =
    A.class "sm:hover:border-green-600"


smHoverBorderGreen700 : Html.Attribute msg
smHoverBorderGreen700 =
    A.class "sm:hover:border-green-700"


smHoverBorderGreen800 : Html.Attribute msg
smHoverBorderGreen800 =
    A.class "sm:hover:border-green-800"


smHoverBorderGreen900 : Html.Attribute msg
smHoverBorderGreen900 =
    A.class "sm:hover:border-green-900"


smHoverBorderTeal100 : Html.Attribute msg
smHoverBorderTeal100 =
    A.class "sm:hover:border-teal-100"


smHoverBorderTeal200 : Html.Attribute msg
smHoverBorderTeal200 =
    A.class "sm:hover:border-teal-200"


smHoverBorderTeal300 : Html.Attribute msg
smHoverBorderTeal300 =
    A.class "sm:hover:border-teal-300"


smHoverBorderTeal400 : Html.Attribute msg
smHoverBorderTeal400 =
    A.class "sm:hover:border-teal-400"


smHoverBorderTeal500 : Html.Attribute msg
smHoverBorderTeal500 =
    A.class "sm:hover:border-teal-500"


smHoverBorderTeal600 : Html.Attribute msg
smHoverBorderTeal600 =
    A.class "sm:hover:border-teal-600"


smHoverBorderTeal700 : Html.Attribute msg
smHoverBorderTeal700 =
    A.class "sm:hover:border-teal-700"


smHoverBorderTeal800 : Html.Attribute msg
smHoverBorderTeal800 =
    A.class "sm:hover:border-teal-800"


smHoverBorderTeal900 : Html.Attribute msg
smHoverBorderTeal900 =
    A.class "sm:hover:border-teal-900"


smHoverBorderBlue100 : Html.Attribute msg
smHoverBorderBlue100 =
    A.class "sm:hover:border-blue-100"


smHoverBorderBlue200 : Html.Attribute msg
smHoverBorderBlue200 =
    A.class "sm:hover:border-blue-200"


smHoverBorderBlue300 : Html.Attribute msg
smHoverBorderBlue300 =
    A.class "sm:hover:border-blue-300"


smHoverBorderBlue400 : Html.Attribute msg
smHoverBorderBlue400 =
    A.class "sm:hover:border-blue-400"


smHoverBorderBlue500 : Html.Attribute msg
smHoverBorderBlue500 =
    A.class "sm:hover:border-blue-500"


smHoverBorderBlue600 : Html.Attribute msg
smHoverBorderBlue600 =
    A.class "sm:hover:border-blue-600"


smHoverBorderBlue700 : Html.Attribute msg
smHoverBorderBlue700 =
    A.class "sm:hover:border-blue-700"


smHoverBorderBlue800 : Html.Attribute msg
smHoverBorderBlue800 =
    A.class "sm:hover:border-blue-800"


smHoverBorderBlue900 : Html.Attribute msg
smHoverBorderBlue900 =
    A.class "sm:hover:border-blue-900"


smHoverBorderIndigo100 : Html.Attribute msg
smHoverBorderIndigo100 =
    A.class "sm:hover:border-indigo-100"


smHoverBorderIndigo200 : Html.Attribute msg
smHoverBorderIndigo200 =
    A.class "sm:hover:border-indigo-200"


smHoverBorderIndigo300 : Html.Attribute msg
smHoverBorderIndigo300 =
    A.class "sm:hover:border-indigo-300"


smHoverBorderIndigo400 : Html.Attribute msg
smHoverBorderIndigo400 =
    A.class "sm:hover:border-indigo-400"


smHoverBorderIndigo500 : Html.Attribute msg
smHoverBorderIndigo500 =
    A.class "sm:hover:border-indigo-500"


smHoverBorderIndigo600 : Html.Attribute msg
smHoverBorderIndigo600 =
    A.class "sm:hover:border-indigo-600"


smHoverBorderIndigo700 : Html.Attribute msg
smHoverBorderIndigo700 =
    A.class "sm:hover:border-indigo-700"


smHoverBorderIndigo800 : Html.Attribute msg
smHoverBorderIndigo800 =
    A.class "sm:hover:border-indigo-800"


smHoverBorderIndigo900 : Html.Attribute msg
smHoverBorderIndigo900 =
    A.class "sm:hover:border-indigo-900"


smHoverBorderPurple100 : Html.Attribute msg
smHoverBorderPurple100 =
    A.class "sm:hover:border-purple-100"


smHoverBorderPurple200 : Html.Attribute msg
smHoverBorderPurple200 =
    A.class "sm:hover:border-purple-200"


smHoverBorderPurple300 : Html.Attribute msg
smHoverBorderPurple300 =
    A.class "sm:hover:border-purple-300"


smHoverBorderPurple400 : Html.Attribute msg
smHoverBorderPurple400 =
    A.class "sm:hover:border-purple-400"


smHoverBorderPurple500 : Html.Attribute msg
smHoverBorderPurple500 =
    A.class "sm:hover:border-purple-500"


smHoverBorderPurple600 : Html.Attribute msg
smHoverBorderPurple600 =
    A.class "sm:hover:border-purple-600"


smHoverBorderPurple700 : Html.Attribute msg
smHoverBorderPurple700 =
    A.class "sm:hover:border-purple-700"


smHoverBorderPurple800 : Html.Attribute msg
smHoverBorderPurple800 =
    A.class "sm:hover:border-purple-800"


smHoverBorderPurple900 : Html.Attribute msg
smHoverBorderPurple900 =
    A.class "sm:hover:border-purple-900"


smHoverBorderPink100 : Html.Attribute msg
smHoverBorderPink100 =
    A.class "sm:hover:border-pink-100"


smHoverBorderPink200 : Html.Attribute msg
smHoverBorderPink200 =
    A.class "sm:hover:border-pink-200"


smHoverBorderPink300 : Html.Attribute msg
smHoverBorderPink300 =
    A.class "sm:hover:border-pink-300"


smHoverBorderPink400 : Html.Attribute msg
smHoverBorderPink400 =
    A.class "sm:hover:border-pink-400"


smHoverBorderPink500 : Html.Attribute msg
smHoverBorderPink500 =
    A.class "sm:hover:border-pink-500"


smHoverBorderPink600 : Html.Attribute msg
smHoverBorderPink600 =
    A.class "sm:hover:border-pink-600"


smHoverBorderPink700 : Html.Attribute msg
smHoverBorderPink700 =
    A.class "sm:hover:border-pink-700"


smHoverBorderPink800 : Html.Attribute msg
smHoverBorderPink800 =
    A.class "sm:hover:border-pink-800"


smHoverBorderPink900 : Html.Attribute msg
smHoverBorderPink900 =
    A.class "sm:hover:border-pink-900"


smFocusBorderTransparent : Html.Attribute msg
smFocusBorderTransparent =
    A.class "sm:focus:border-transparent"


smFocusBorderCurrent : Html.Attribute msg
smFocusBorderCurrent =
    A.class "sm:focus:border-current"


smFocusBorderBlack : Html.Attribute msg
smFocusBorderBlack =
    A.class "sm:focus:border-black"


smFocusBorderWhite : Html.Attribute msg
smFocusBorderWhite =
    A.class "sm:focus:border-white"


smFocusBorderGray100 : Html.Attribute msg
smFocusBorderGray100 =
    A.class "sm:focus:border-gray-100"


smFocusBorderGray200 : Html.Attribute msg
smFocusBorderGray200 =
    A.class "sm:focus:border-gray-200"


smFocusBorderGray300 : Html.Attribute msg
smFocusBorderGray300 =
    A.class "sm:focus:border-gray-300"


smFocusBorderGray400 : Html.Attribute msg
smFocusBorderGray400 =
    A.class "sm:focus:border-gray-400"


smFocusBorderGray500 : Html.Attribute msg
smFocusBorderGray500 =
    A.class "sm:focus:border-gray-500"


smFocusBorderGray600 : Html.Attribute msg
smFocusBorderGray600 =
    A.class "sm:focus:border-gray-600"


smFocusBorderGray700 : Html.Attribute msg
smFocusBorderGray700 =
    A.class "sm:focus:border-gray-700"


smFocusBorderGray800 : Html.Attribute msg
smFocusBorderGray800 =
    A.class "sm:focus:border-gray-800"


smFocusBorderGray900 : Html.Attribute msg
smFocusBorderGray900 =
    A.class "sm:focus:border-gray-900"


smFocusBorderRed100 : Html.Attribute msg
smFocusBorderRed100 =
    A.class "sm:focus:border-red-100"


smFocusBorderRed200 : Html.Attribute msg
smFocusBorderRed200 =
    A.class "sm:focus:border-red-200"


smFocusBorderRed300 : Html.Attribute msg
smFocusBorderRed300 =
    A.class "sm:focus:border-red-300"


smFocusBorderRed400 : Html.Attribute msg
smFocusBorderRed400 =
    A.class "sm:focus:border-red-400"


smFocusBorderRed500 : Html.Attribute msg
smFocusBorderRed500 =
    A.class "sm:focus:border-red-500"


smFocusBorderRed600 : Html.Attribute msg
smFocusBorderRed600 =
    A.class "sm:focus:border-red-600"


smFocusBorderRed700 : Html.Attribute msg
smFocusBorderRed700 =
    A.class "sm:focus:border-red-700"


smFocusBorderRed800 : Html.Attribute msg
smFocusBorderRed800 =
    A.class "sm:focus:border-red-800"


smFocusBorderRed900 : Html.Attribute msg
smFocusBorderRed900 =
    A.class "sm:focus:border-red-900"


smFocusBorderOrange100 : Html.Attribute msg
smFocusBorderOrange100 =
    A.class "sm:focus:border-orange-100"


smFocusBorderOrange200 : Html.Attribute msg
smFocusBorderOrange200 =
    A.class "sm:focus:border-orange-200"


smFocusBorderOrange300 : Html.Attribute msg
smFocusBorderOrange300 =
    A.class "sm:focus:border-orange-300"


smFocusBorderOrange400 : Html.Attribute msg
smFocusBorderOrange400 =
    A.class "sm:focus:border-orange-400"


smFocusBorderOrange500 : Html.Attribute msg
smFocusBorderOrange500 =
    A.class "sm:focus:border-orange-500"


smFocusBorderOrange600 : Html.Attribute msg
smFocusBorderOrange600 =
    A.class "sm:focus:border-orange-600"


smFocusBorderOrange700 : Html.Attribute msg
smFocusBorderOrange700 =
    A.class "sm:focus:border-orange-700"


smFocusBorderOrange800 : Html.Attribute msg
smFocusBorderOrange800 =
    A.class "sm:focus:border-orange-800"


smFocusBorderOrange900 : Html.Attribute msg
smFocusBorderOrange900 =
    A.class "sm:focus:border-orange-900"


smFocusBorderYellow100 : Html.Attribute msg
smFocusBorderYellow100 =
    A.class "sm:focus:border-yellow-100"


smFocusBorderYellow200 : Html.Attribute msg
smFocusBorderYellow200 =
    A.class "sm:focus:border-yellow-200"


smFocusBorderYellow300 : Html.Attribute msg
smFocusBorderYellow300 =
    A.class "sm:focus:border-yellow-300"


smFocusBorderYellow400 : Html.Attribute msg
smFocusBorderYellow400 =
    A.class "sm:focus:border-yellow-400"


smFocusBorderYellow500 : Html.Attribute msg
smFocusBorderYellow500 =
    A.class "sm:focus:border-yellow-500"


smFocusBorderYellow600 : Html.Attribute msg
smFocusBorderYellow600 =
    A.class "sm:focus:border-yellow-600"


smFocusBorderYellow700 : Html.Attribute msg
smFocusBorderYellow700 =
    A.class "sm:focus:border-yellow-700"


smFocusBorderYellow800 : Html.Attribute msg
smFocusBorderYellow800 =
    A.class "sm:focus:border-yellow-800"


smFocusBorderYellow900 : Html.Attribute msg
smFocusBorderYellow900 =
    A.class "sm:focus:border-yellow-900"


smFocusBorderGreen100 : Html.Attribute msg
smFocusBorderGreen100 =
    A.class "sm:focus:border-green-100"


smFocusBorderGreen200 : Html.Attribute msg
smFocusBorderGreen200 =
    A.class "sm:focus:border-green-200"


smFocusBorderGreen300 : Html.Attribute msg
smFocusBorderGreen300 =
    A.class "sm:focus:border-green-300"


smFocusBorderGreen400 : Html.Attribute msg
smFocusBorderGreen400 =
    A.class "sm:focus:border-green-400"


smFocusBorderGreen500 : Html.Attribute msg
smFocusBorderGreen500 =
    A.class "sm:focus:border-green-500"


smFocusBorderGreen600 : Html.Attribute msg
smFocusBorderGreen600 =
    A.class "sm:focus:border-green-600"


smFocusBorderGreen700 : Html.Attribute msg
smFocusBorderGreen700 =
    A.class "sm:focus:border-green-700"


smFocusBorderGreen800 : Html.Attribute msg
smFocusBorderGreen800 =
    A.class "sm:focus:border-green-800"


smFocusBorderGreen900 : Html.Attribute msg
smFocusBorderGreen900 =
    A.class "sm:focus:border-green-900"


smFocusBorderTeal100 : Html.Attribute msg
smFocusBorderTeal100 =
    A.class "sm:focus:border-teal-100"


smFocusBorderTeal200 : Html.Attribute msg
smFocusBorderTeal200 =
    A.class "sm:focus:border-teal-200"


smFocusBorderTeal300 : Html.Attribute msg
smFocusBorderTeal300 =
    A.class "sm:focus:border-teal-300"


smFocusBorderTeal400 : Html.Attribute msg
smFocusBorderTeal400 =
    A.class "sm:focus:border-teal-400"


smFocusBorderTeal500 : Html.Attribute msg
smFocusBorderTeal500 =
    A.class "sm:focus:border-teal-500"


smFocusBorderTeal600 : Html.Attribute msg
smFocusBorderTeal600 =
    A.class "sm:focus:border-teal-600"


smFocusBorderTeal700 : Html.Attribute msg
smFocusBorderTeal700 =
    A.class "sm:focus:border-teal-700"


smFocusBorderTeal800 : Html.Attribute msg
smFocusBorderTeal800 =
    A.class "sm:focus:border-teal-800"


smFocusBorderTeal900 : Html.Attribute msg
smFocusBorderTeal900 =
    A.class "sm:focus:border-teal-900"


smFocusBorderBlue100 : Html.Attribute msg
smFocusBorderBlue100 =
    A.class "sm:focus:border-blue-100"


smFocusBorderBlue200 : Html.Attribute msg
smFocusBorderBlue200 =
    A.class "sm:focus:border-blue-200"


smFocusBorderBlue300 : Html.Attribute msg
smFocusBorderBlue300 =
    A.class "sm:focus:border-blue-300"


smFocusBorderBlue400 : Html.Attribute msg
smFocusBorderBlue400 =
    A.class "sm:focus:border-blue-400"


smFocusBorderBlue500 : Html.Attribute msg
smFocusBorderBlue500 =
    A.class "sm:focus:border-blue-500"


smFocusBorderBlue600 : Html.Attribute msg
smFocusBorderBlue600 =
    A.class "sm:focus:border-blue-600"


smFocusBorderBlue700 : Html.Attribute msg
smFocusBorderBlue700 =
    A.class "sm:focus:border-blue-700"


smFocusBorderBlue800 : Html.Attribute msg
smFocusBorderBlue800 =
    A.class "sm:focus:border-blue-800"


smFocusBorderBlue900 : Html.Attribute msg
smFocusBorderBlue900 =
    A.class "sm:focus:border-blue-900"


smFocusBorderIndigo100 : Html.Attribute msg
smFocusBorderIndigo100 =
    A.class "sm:focus:border-indigo-100"


smFocusBorderIndigo200 : Html.Attribute msg
smFocusBorderIndigo200 =
    A.class "sm:focus:border-indigo-200"


smFocusBorderIndigo300 : Html.Attribute msg
smFocusBorderIndigo300 =
    A.class "sm:focus:border-indigo-300"


smFocusBorderIndigo400 : Html.Attribute msg
smFocusBorderIndigo400 =
    A.class "sm:focus:border-indigo-400"


smFocusBorderIndigo500 : Html.Attribute msg
smFocusBorderIndigo500 =
    A.class "sm:focus:border-indigo-500"


smFocusBorderIndigo600 : Html.Attribute msg
smFocusBorderIndigo600 =
    A.class "sm:focus:border-indigo-600"


smFocusBorderIndigo700 : Html.Attribute msg
smFocusBorderIndigo700 =
    A.class "sm:focus:border-indigo-700"


smFocusBorderIndigo800 : Html.Attribute msg
smFocusBorderIndigo800 =
    A.class "sm:focus:border-indigo-800"


smFocusBorderIndigo900 : Html.Attribute msg
smFocusBorderIndigo900 =
    A.class "sm:focus:border-indigo-900"


smFocusBorderPurple100 : Html.Attribute msg
smFocusBorderPurple100 =
    A.class "sm:focus:border-purple-100"


smFocusBorderPurple200 : Html.Attribute msg
smFocusBorderPurple200 =
    A.class "sm:focus:border-purple-200"


smFocusBorderPurple300 : Html.Attribute msg
smFocusBorderPurple300 =
    A.class "sm:focus:border-purple-300"


smFocusBorderPurple400 : Html.Attribute msg
smFocusBorderPurple400 =
    A.class "sm:focus:border-purple-400"


smFocusBorderPurple500 : Html.Attribute msg
smFocusBorderPurple500 =
    A.class "sm:focus:border-purple-500"


smFocusBorderPurple600 : Html.Attribute msg
smFocusBorderPurple600 =
    A.class "sm:focus:border-purple-600"


smFocusBorderPurple700 : Html.Attribute msg
smFocusBorderPurple700 =
    A.class "sm:focus:border-purple-700"


smFocusBorderPurple800 : Html.Attribute msg
smFocusBorderPurple800 =
    A.class "sm:focus:border-purple-800"


smFocusBorderPurple900 : Html.Attribute msg
smFocusBorderPurple900 =
    A.class "sm:focus:border-purple-900"


smFocusBorderPink100 : Html.Attribute msg
smFocusBorderPink100 =
    A.class "sm:focus:border-pink-100"


smFocusBorderPink200 : Html.Attribute msg
smFocusBorderPink200 =
    A.class "sm:focus:border-pink-200"


smFocusBorderPink300 : Html.Attribute msg
smFocusBorderPink300 =
    A.class "sm:focus:border-pink-300"


smFocusBorderPink400 : Html.Attribute msg
smFocusBorderPink400 =
    A.class "sm:focus:border-pink-400"


smFocusBorderPink500 : Html.Attribute msg
smFocusBorderPink500 =
    A.class "sm:focus:border-pink-500"


smFocusBorderPink600 : Html.Attribute msg
smFocusBorderPink600 =
    A.class "sm:focus:border-pink-600"


smFocusBorderPink700 : Html.Attribute msg
smFocusBorderPink700 =
    A.class "sm:focus:border-pink-700"


smFocusBorderPink800 : Html.Attribute msg
smFocusBorderPink800 =
    A.class "sm:focus:border-pink-800"


smFocusBorderPink900 : Html.Attribute msg
smFocusBorderPink900 =
    A.class "sm:focus:border-pink-900"


smBorderOpacity0 : Html.Attribute msg
smBorderOpacity0 =
    A.class "sm:border-opacity-0"


smBorderOpacity25 : Html.Attribute msg
smBorderOpacity25 =
    A.class "sm:border-opacity-25"


smBorderOpacity50 : Html.Attribute msg
smBorderOpacity50 =
    A.class "sm:border-opacity-50"


smBorderOpacity75 : Html.Attribute msg
smBorderOpacity75 =
    A.class "sm:border-opacity-75"


smBorderOpacity100 : Html.Attribute msg
smBorderOpacity100 =
    A.class "sm:border-opacity-100"


smHoverBorderOpacity0 : Html.Attribute msg
smHoverBorderOpacity0 =
    A.class "sm:hover:border-opacity-0"


smHoverBorderOpacity25 : Html.Attribute msg
smHoverBorderOpacity25 =
    A.class "sm:hover:border-opacity-25"


smHoverBorderOpacity50 : Html.Attribute msg
smHoverBorderOpacity50 =
    A.class "sm:hover:border-opacity-50"


smHoverBorderOpacity75 : Html.Attribute msg
smHoverBorderOpacity75 =
    A.class "sm:hover:border-opacity-75"


smHoverBorderOpacity100 : Html.Attribute msg
smHoverBorderOpacity100 =
    A.class "sm:hover:border-opacity-100"


smFocusBorderOpacity0 : Html.Attribute msg
smFocusBorderOpacity0 =
    A.class "sm:focus:border-opacity-0"


smFocusBorderOpacity25 : Html.Attribute msg
smFocusBorderOpacity25 =
    A.class "sm:focus:border-opacity-25"


smFocusBorderOpacity50 : Html.Attribute msg
smFocusBorderOpacity50 =
    A.class "sm:focus:border-opacity-50"


smFocusBorderOpacity75 : Html.Attribute msg
smFocusBorderOpacity75 =
    A.class "sm:focus:border-opacity-75"


smFocusBorderOpacity100 : Html.Attribute msg
smFocusBorderOpacity100 =
    A.class "sm:focus:border-opacity-100"


smRoundedNone : Html.Attribute msg
smRoundedNone =
    A.class "sm:rounded-none"


smRoundedSm : Html.Attribute msg
smRoundedSm =
    A.class "sm:rounded-sm"


smRounded : Html.Attribute msg
smRounded =
    A.class "sm:rounded"


smRoundedMd : Html.Attribute msg
smRoundedMd =
    A.class "sm:rounded-md"


smRoundedLg : Html.Attribute msg
smRoundedLg =
    A.class "sm:rounded-lg"


smRoundedFull : Html.Attribute msg
smRoundedFull =
    A.class "sm:rounded-full"


smRoundedTNone : Html.Attribute msg
smRoundedTNone =
    A.class "sm:rounded-t-none"


smRoundedRNone : Html.Attribute msg
smRoundedRNone =
    A.class "sm:rounded-r-none"


smRoundedBNone : Html.Attribute msg
smRoundedBNone =
    A.class "sm:rounded-b-none"


smRoundedLNone : Html.Attribute msg
smRoundedLNone =
    A.class "sm:rounded-l-none"


smRoundedTSm : Html.Attribute msg
smRoundedTSm =
    A.class "sm:rounded-t-sm"


smRoundedRSm : Html.Attribute msg
smRoundedRSm =
    A.class "sm:rounded-r-sm"


smRoundedBSm : Html.Attribute msg
smRoundedBSm =
    A.class "sm:rounded-b-sm"


smRoundedLSm : Html.Attribute msg
smRoundedLSm =
    A.class "sm:rounded-l-sm"


smRoundedT : Html.Attribute msg
smRoundedT =
    A.class "sm:rounded-t"


smRoundedR : Html.Attribute msg
smRoundedR =
    A.class "sm:rounded-r"


smRoundedB : Html.Attribute msg
smRoundedB =
    A.class "sm:rounded-b"


smRoundedL : Html.Attribute msg
smRoundedL =
    A.class "sm:rounded-l"


smRoundedTMd : Html.Attribute msg
smRoundedTMd =
    A.class "sm:rounded-t-md"


smRoundedRMd : Html.Attribute msg
smRoundedRMd =
    A.class "sm:rounded-r-md"


smRoundedBMd : Html.Attribute msg
smRoundedBMd =
    A.class "sm:rounded-b-md"


smRoundedLMd : Html.Attribute msg
smRoundedLMd =
    A.class "sm:rounded-l-md"


smRoundedTLg : Html.Attribute msg
smRoundedTLg =
    A.class "sm:rounded-t-lg"


smRoundedRLg : Html.Attribute msg
smRoundedRLg =
    A.class "sm:rounded-r-lg"


smRoundedBLg : Html.Attribute msg
smRoundedBLg =
    A.class "sm:rounded-b-lg"


smRoundedLLg : Html.Attribute msg
smRoundedLLg =
    A.class "sm:rounded-l-lg"


smRoundedTFull : Html.Attribute msg
smRoundedTFull =
    A.class "sm:rounded-t-full"


smRoundedRFull : Html.Attribute msg
smRoundedRFull =
    A.class "sm:rounded-r-full"


smRoundedBFull : Html.Attribute msg
smRoundedBFull =
    A.class "sm:rounded-b-full"


smRoundedLFull : Html.Attribute msg
smRoundedLFull =
    A.class "sm:rounded-l-full"


smRoundedTlNone : Html.Attribute msg
smRoundedTlNone =
    A.class "sm:rounded-tl-none"


smRoundedTrNone : Html.Attribute msg
smRoundedTrNone =
    A.class "sm:rounded-tr-none"


smRoundedBrNone : Html.Attribute msg
smRoundedBrNone =
    A.class "sm:rounded-br-none"


smRoundedBlNone : Html.Attribute msg
smRoundedBlNone =
    A.class "sm:rounded-bl-none"


smRoundedTlSm : Html.Attribute msg
smRoundedTlSm =
    A.class "sm:rounded-tl-sm"


smRoundedTrSm : Html.Attribute msg
smRoundedTrSm =
    A.class "sm:rounded-tr-sm"


smRoundedBrSm : Html.Attribute msg
smRoundedBrSm =
    A.class "sm:rounded-br-sm"


smRoundedBlSm : Html.Attribute msg
smRoundedBlSm =
    A.class "sm:rounded-bl-sm"


smRoundedTl : Html.Attribute msg
smRoundedTl =
    A.class "sm:rounded-tl"


smRoundedTr : Html.Attribute msg
smRoundedTr =
    A.class "sm:rounded-tr"


smRoundedBr : Html.Attribute msg
smRoundedBr =
    A.class "sm:rounded-br"


smRoundedBl : Html.Attribute msg
smRoundedBl =
    A.class "sm:rounded-bl"


smRoundedTlMd : Html.Attribute msg
smRoundedTlMd =
    A.class "sm:rounded-tl-md"


smRoundedTrMd : Html.Attribute msg
smRoundedTrMd =
    A.class "sm:rounded-tr-md"


smRoundedBrMd : Html.Attribute msg
smRoundedBrMd =
    A.class "sm:rounded-br-md"


smRoundedBlMd : Html.Attribute msg
smRoundedBlMd =
    A.class "sm:rounded-bl-md"


smRoundedTlLg : Html.Attribute msg
smRoundedTlLg =
    A.class "sm:rounded-tl-lg"


smRoundedTrLg : Html.Attribute msg
smRoundedTrLg =
    A.class "sm:rounded-tr-lg"


smRoundedBrLg : Html.Attribute msg
smRoundedBrLg =
    A.class "sm:rounded-br-lg"


smRoundedBlLg : Html.Attribute msg
smRoundedBlLg =
    A.class "sm:rounded-bl-lg"


smRoundedTlFull : Html.Attribute msg
smRoundedTlFull =
    A.class "sm:rounded-tl-full"


smRoundedTrFull : Html.Attribute msg
smRoundedTrFull =
    A.class "sm:rounded-tr-full"


smRoundedBrFull : Html.Attribute msg
smRoundedBrFull =
    A.class "sm:rounded-br-full"


smRoundedBlFull : Html.Attribute msg
smRoundedBlFull =
    A.class "sm:rounded-bl-full"


smBorderSolid : Html.Attribute msg
smBorderSolid =
    A.class "sm:border-solid"


smBorderDashed : Html.Attribute msg
smBorderDashed =
    A.class "sm:border-dashed"


smBorderDotted : Html.Attribute msg
smBorderDotted =
    A.class "sm:border-dotted"


smBorderDouble : Html.Attribute msg
smBorderDouble =
    A.class "sm:border-double"


smBorderNone : Html.Attribute msg
smBorderNone =
    A.class "sm:border-none"


smBorder0 : Html.Attribute msg
smBorder0 =
    A.class "sm:border-0"


smBorder2 : Html.Attribute msg
smBorder2 =
    A.class "sm:border-2"


smBorder4 : Html.Attribute msg
smBorder4 =
    A.class "sm:border-4"


smBorder8 : Html.Attribute msg
smBorder8 =
    A.class "sm:border-8"


smBorder : Html.Attribute msg
smBorder =
    A.class "sm:border"


smBorderT0 : Html.Attribute msg
smBorderT0 =
    A.class "sm:border-t-0"


smBorderR0 : Html.Attribute msg
smBorderR0 =
    A.class "sm:border-r-0"


smBorderB0 : Html.Attribute msg
smBorderB0 =
    A.class "sm:border-b-0"


smBorderL0 : Html.Attribute msg
smBorderL0 =
    A.class "sm:border-l-0"


smBorderT2 : Html.Attribute msg
smBorderT2 =
    A.class "sm:border-t-2"


smBorderR2 : Html.Attribute msg
smBorderR2 =
    A.class "sm:border-r-2"


smBorderB2 : Html.Attribute msg
smBorderB2 =
    A.class "sm:border-b-2"


smBorderL2 : Html.Attribute msg
smBorderL2 =
    A.class "sm:border-l-2"


smBorderT4 : Html.Attribute msg
smBorderT4 =
    A.class "sm:border-t-4"


smBorderR4 : Html.Attribute msg
smBorderR4 =
    A.class "sm:border-r-4"


smBorderB4 : Html.Attribute msg
smBorderB4 =
    A.class "sm:border-b-4"


smBorderL4 : Html.Attribute msg
smBorderL4 =
    A.class "sm:border-l-4"


smBorderT8 : Html.Attribute msg
smBorderT8 =
    A.class "sm:border-t-8"


smBorderR8 : Html.Attribute msg
smBorderR8 =
    A.class "sm:border-r-8"


smBorderB8 : Html.Attribute msg
smBorderB8 =
    A.class "sm:border-b-8"


smBorderL8 : Html.Attribute msg
smBorderL8 =
    A.class "sm:border-l-8"


smBorderT : Html.Attribute msg
smBorderT =
    A.class "sm:border-t"


smBorderR : Html.Attribute msg
smBorderR =
    A.class "sm:border-r"


smBorderB : Html.Attribute msg
smBorderB =
    A.class "sm:border-b"


smBorderL : Html.Attribute msg
smBorderL =
    A.class "sm:border-l"


smBoxBorder : Html.Attribute msg
smBoxBorder =
    A.class "sm:box-border"


smBoxContent : Html.Attribute msg
smBoxContent =
    A.class "sm:box-content"


smCursorAuto : Html.Attribute msg
smCursorAuto =
    A.class "sm:cursor-auto"


smCursorDefault : Html.Attribute msg
smCursorDefault =
    A.class "sm:cursor-default"


smCursorPointer : Html.Attribute msg
smCursorPointer =
    A.class "sm:cursor-pointer"


smCursorWait : Html.Attribute msg
smCursorWait =
    A.class "sm:cursor-wait"


smCursorText : Html.Attribute msg
smCursorText =
    A.class "sm:cursor-text"


smCursorMove : Html.Attribute msg
smCursorMove =
    A.class "sm:cursor-move"


smCursorNotAllowed : Html.Attribute msg
smCursorNotAllowed =
    A.class "sm:cursor-not-allowed"


smBlock : Html.Attribute msg
smBlock =
    A.class "sm:block"


smInlineBlock : Html.Attribute msg
smInlineBlock =
    A.class "sm:inline-block"


smInline : Html.Attribute msg
smInline =
    A.class "sm:inline"


smFlex : Html.Attribute msg
smFlex =
    A.class "sm:flex"


smInlineFlex : Html.Attribute msg
smInlineFlex =
    A.class "sm:inline-flex"


smTable : Html.Attribute msg
smTable =
    A.class "sm:table"


smTableCaption : Html.Attribute msg
smTableCaption =
    A.class "sm:table-caption"


smTableCell : Html.Attribute msg
smTableCell =
    A.class "sm:table-cell"


smTableColumn : Html.Attribute msg
smTableColumn =
    A.class "sm:table-column"


smTableColumnGroup : Html.Attribute msg
smTableColumnGroup =
    A.class "sm:table-column-group"


smTableFooterGroup : Html.Attribute msg
smTableFooterGroup =
    A.class "sm:table-footer-group"


smTableHeaderGroup : Html.Attribute msg
smTableHeaderGroup =
    A.class "sm:table-header-group"


smTableRowGroup : Html.Attribute msg
smTableRowGroup =
    A.class "sm:table-row-group"


smTableRow : Html.Attribute msg
smTableRow =
    A.class "sm:table-row"


smFlowRoot : Html.Attribute msg
smFlowRoot =
    A.class "sm:flow-root"


smGrid : Html.Attribute msg
smGrid =
    A.class "sm:grid"


smInlineGrid : Html.Attribute msg
smInlineGrid =
    A.class "sm:inline-grid"


smContents : Html.Attribute msg
smContents =
    A.class "sm:contents"


smHidden : Html.Attribute msg
smHidden =
    A.class "sm:hidden"


smFlexRow : Html.Attribute msg
smFlexRow =
    A.class "sm:flex-row"


smFlexRowReverse : Html.Attribute msg
smFlexRowReverse =
    A.class "sm:flex-row-reverse"


smFlexCol : Html.Attribute msg
smFlexCol =
    A.class "sm:flex-col"


smFlexColReverse : Html.Attribute msg
smFlexColReverse =
    A.class "sm:flex-col-reverse"


smFlexWrap : Html.Attribute msg
smFlexWrap =
    A.class "sm:flex-wrap"


smFlexWrapReverse : Html.Attribute msg
smFlexWrapReverse =
    A.class "sm:flex-wrap-reverse"


smFlexNoWrap : Html.Attribute msg
smFlexNoWrap =
    A.class "sm:flex-no-wrap"


smPlaceItemsAuto : Html.Attribute msg
smPlaceItemsAuto =
    A.class "sm:place-items-auto"


smPlaceItemsStart : Html.Attribute msg
smPlaceItemsStart =
    A.class "sm:place-items-start"


smPlaceItemsEnd : Html.Attribute msg
smPlaceItemsEnd =
    A.class "sm:place-items-end"


smPlaceItemsCenter : Html.Attribute msg
smPlaceItemsCenter =
    A.class "sm:place-items-center"


smPlaceItemsStretch : Html.Attribute msg
smPlaceItemsStretch =
    A.class "sm:place-items-stretch"


smPlaceContentCenter : Html.Attribute msg
smPlaceContentCenter =
    A.class "sm:place-content-center"


smPlaceContentStart : Html.Attribute msg
smPlaceContentStart =
    A.class "sm:place-content-start"


smPlaceContentEnd : Html.Attribute msg
smPlaceContentEnd =
    A.class "sm:place-content-end"


smPlaceContentBetween : Html.Attribute msg
smPlaceContentBetween =
    A.class "sm:place-content-between"


smPlaceContentAround : Html.Attribute msg
smPlaceContentAround =
    A.class "sm:place-content-around"


smPlaceContentEvenly : Html.Attribute msg
smPlaceContentEvenly =
    A.class "sm:place-content-evenly"


smPlaceContentStretch : Html.Attribute msg
smPlaceContentStretch =
    A.class "sm:place-content-stretch"


smPlaceSelfAuto : Html.Attribute msg
smPlaceSelfAuto =
    A.class "sm:place-self-auto"


smPlaceSelfStart : Html.Attribute msg
smPlaceSelfStart =
    A.class "sm:place-self-start"


smPlaceSelfEnd : Html.Attribute msg
smPlaceSelfEnd =
    A.class "sm:place-self-end"


smPlaceSelfCenter : Html.Attribute msg
smPlaceSelfCenter =
    A.class "sm:place-self-center"


smPlaceSelfStretch : Html.Attribute msg
smPlaceSelfStretch =
    A.class "sm:place-self-stretch"


smItemsStart : Html.Attribute msg
smItemsStart =
    A.class "sm:items-start"


smItemsEnd : Html.Attribute msg
smItemsEnd =
    A.class "sm:items-end"


smItemsCenter : Html.Attribute msg
smItemsCenter =
    A.class "sm:items-center"


smItemsBaseline : Html.Attribute msg
smItemsBaseline =
    A.class "sm:items-baseline"


smItemsStretch : Html.Attribute msg
smItemsStretch =
    A.class "sm:items-stretch"


smContentCenter : Html.Attribute msg
smContentCenter =
    A.class "sm:content-center"


smContentStart : Html.Attribute msg
smContentStart =
    A.class "sm:content-start"


smContentEnd : Html.Attribute msg
smContentEnd =
    A.class "sm:content-end"


smContentBetween : Html.Attribute msg
smContentBetween =
    A.class "sm:content-between"


smContentAround : Html.Attribute msg
smContentAround =
    A.class "sm:content-around"


smContentEvenly : Html.Attribute msg
smContentEvenly =
    A.class "sm:content-evenly"


smSelfAuto : Html.Attribute msg
smSelfAuto =
    A.class "sm:self-auto"


smSelfStart : Html.Attribute msg
smSelfStart =
    A.class "sm:self-start"


smSelfEnd : Html.Attribute msg
smSelfEnd =
    A.class "sm:self-end"


smSelfCenter : Html.Attribute msg
smSelfCenter =
    A.class "sm:self-center"


smSelfStretch : Html.Attribute msg
smSelfStretch =
    A.class "sm:self-stretch"


smJustifyItemsAuto : Html.Attribute msg
smJustifyItemsAuto =
    A.class "sm:justify-items-auto"


smJustifyItemsStart : Html.Attribute msg
smJustifyItemsStart =
    A.class "sm:justify-items-start"


smJustifyItemsEnd : Html.Attribute msg
smJustifyItemsEnd =
    A.class "sm:justify-items-end"


smJustifyItemsCenter : Html.Attribute msg
smJustifyItemsCenter =
    A.class "sm:justify-items-center"


smJustifyItemsStretch : Html.Attribute msg
smJustifyItemsStretch =
    A.class "sm:justify-items-stretch"


smJustifyStart : Html.Attribute msg
smJustifyStart =
    A.class "sm:justify-start"


smJustifyEnd : Html.Attribute msg
smJustifyEnd =
    A.class "sm:justify-end"


smJustifyCenter : Html.Attribute msg
smJustifyCenter =
    A.class "sm:justify-center"


smJustifyBetween : Html.Attribute msg
smJustifyBetween =
    A.class "sm:justify-between"


smJustifyAround : Html.Attribute msg
smJustifyAround =
    A.class "sm:justify-around"


smJustifyEvenly : Html.Attribute msg
smJustifyEvenly =
    A.class "sm:justify-evenly"


smJustifySelfAuto : Html.Attribute msg
smJustifySelfAuto =
    A.class "sm:justify-self-auto"


smJustifySelfStart : Html.Attribute msg
smJustifySelfStart =
    A.class "sm:justify-self-start"


smJustifySelfEnd : Html.Attribute msg
smJustifySelfEnd =
    A.class "sm:justify-self-end"


smJustifySelfCenter : Html.Attribute msg
smJustifySelfCenter =
    A.class "sm:justify-self-center"


smJustifySelfStretch : Html.Attribute msg
smJustifySelfStretch =
    A.class "sm:justify-self-stretch"


smFlex1 : Html.Attribute msg
smFlex1 =
    A.class "sm:flex-1"


smFlexAuto : Html.Attribute msg
smFlexAuto =
    A.class "sm:flex-auto"


smFlexInitial : Html.Attribute msg
smFlexInitial =
    A.class "sm:flex-initial"


smFlexNone : Html.Attribute msg
smFlexNone =
    A.class "sm:flex-none"


smFlexGrow0 : Html.Attribute msg
smFlexGrow0 =
    A.class "sm:flex-grow-0"


smFlexGrow : Html.Attribute msg
smFlexGrow =
    A.class "sm:flex-grow"


smFlexShrink0 : Html.Attribute msg
smFlexShrink0 =
    A.class "sm:flex-shrink-0"


smFlexShrink : Html.Attribute msg
smFlexShrink =
    A.class "sm:flex-shrink"


smOrder1 : Html.Attribute msg
smOrder1 =
    A.class "sm:order-1"


smOrder2 : Html.Attribute msg
smOrder2 =
    A.class "sm:order-2"


smOrder3 : Html.Attribute msg
smOrder3 =
    A.class "sm:order-3"


smOrder4 : Html.Attribute msg
smOrder4 =
    A.class "sm:order-4"


smOrder5 : Html.Attribute msg
smOrder5 =
    A.class "sm:order-5"


smOrder6 : Html.Attribute msg
smOrder6 =
    A.class "sm:order-6"


smOrder7 : Html.Attribute msg
smOrder7 =
    A.class "sm:order-7"


smOrder8 : Html.Attribute msg
smOrder8 =
    A.class "sm:order-8"


smOrder9 : Html.Attribute msg
smOrder9 =
    A.class "sm:order-9"


smOrder10 : Html.Attribute msg
smOrder10 =
    A.class "sm:order-10"


smOrder11 : Html.Attribute msg
smOrder11 =
    A.class "sm:order-11"


smOrder12 : Html.Attribute msg
smOrder12 =
    A.class "sm:order-12"


smOrderFirst : Html.Attribute msg
smOrderFirst =
    A.class "sm:order-first"


smOrderLast : Html.Attribute msg
smOrderLast =
    A.class "sm:order-last"


smOrderNone : Html.Attribute msg
smOrderNone =
    A.class "sm:order-none"


smFloatRight : Html.Attribute msg
smFloatRight =
    A.class "sm:float-right"


smFloatLeft : Html.Attribute msg
smFloatLeft =
    A.class "sm:float-left"


smFloatNone : Html.Attribute msg
smFloatNone =
    A.class "sm:float-none"


smClearfixAfter : Html.Attribute msg
smClearfixAfter =
    A.class "sm:clearfix:after"


smClearLeft : Html.Attribute msg
smClearLeft =
    A.class "sm:clear-left"


smClearRight : Html.Attribute msg
smClearRight =
    A.class "sm:clear-right"


smClearBoth : Html.Attribute msg
smClearBoth =
    A.class "sm:clear-both"


smClearNone : Html.Attribute msg
smClearNone =
    A.class "sm:clear-none"


smFontSans : Html.Attribute msg
smFontSans =
    A.class "sm:font-sans"


smFontSerif : Html.Attribute msg
smFontSerif =
    A.class "sm:font-serif"


smFontMono : Html.Attribute msg
smFontMono =
    A.class "sm:font-mono"


smFontHairline : Html.Attribute msg
smFontHairline =
    A.class "sm:font-hairline"


smFontThin : Html.Attribute msg
smFontThin =
    A.class "sm:font-thin"


smFontLight : Html.Attribute msg
smFontLight =
    A.class "sm:font-light"


smFontNormal : Html.Attribute msg
smFontNormal =
    A.class "sm:font-normal"


smFontMedium : Html.Attribute msg
smFontMedium =
    A.class "sm:font-medium"


smFontSemibold : Html.Attribute msg
smFontSemibold =
    A.class "sm:font-semibold"


smFontBold : Html.Attribute msg
smFontBold =
    A.class "sm:font-bold"


smFontExtrabold : Html.Attribute msg
smFontExtrabold =
    A.class "sm:font-extrabold"


smFontBlack : Html.Attribute msg
smFontBlack =
    A.class "sm:font-black"


smHoverFontHairline : Html.Attribute msg
smHoverFontHairline =
    A.class "sm:hover:font-hairline"


smHoverFontThin : Html.Attribute msg
smHoverFontThin =
    A.class "sm:hover:font-thin"


smHoverFontLight : Html.Attribute msg
smHoverFontLight =
    A.class "sm:hover:font-light"


smHoverFontNormal : Html.Attribute msg
smHoverFontNormal =
    A.class "sm:hover:font-normal"


smHoverFontMedium : Html.Attribute msg
smHoverFontMedium =
    A.class "sm:hover:font-medium"


smHoverFontSemibold : Html.Attribute msg
smHoverFontSemibold =
    A.class "sm:hover:font-semibold"


smHoverFontBold : Html.Attribute msg
smHoverFontBold =
    A.class "sm:hover:font-bold"


smHoverFontExtrabold : Html.Attribute msg
smHoverFontExtrabold =
    A.class "sm:hover:font-extrabold"


smHoverFontBlack : Html.Attribute msg
smHoverFontBlack =
    A.class "sm:hover:font-black"


smFocusFontHairline : Html.Attribute msg
smFocusFontHairline =
    A.class "sm:focus:font-hairline"


smFocusFontThin : Html.Attribute msg
smFocusFontThin =
    A.class "sm:focus:font-thin"


smFocusFontLight : Html.Attribute msg
smFocusFontLight =
    A.class "sm:focus:font-light"


smFocusFontNormal : Html.Attribute msg
smFocusFontNormal =
    A.class "sm:focus:font-normal"


smFocusFontMedium : Html.Attribute msg
smFocusFontMedium =
    A.class "sm:focus:font-medium"


smFocusFontSemibold : Html.Attribute msg
smFocusFontSemibold =
    A.class "sm:focus:font-semibold"


smFocusFontBold : Html.Attribute msg
smFocusFontBold =
    A.class "sm:focus:font-bold"


smFocusFontExtrabold : Html.Attribute msg
smFocusFontExtrabold =
    A.class "sm:focus:font-extrabold"


smFocusFontBlack : Html.Attribute msg
smFocusFontBlack =
    A.class "sm:focus:font-black"


smH0 : Html.Attribute msg
smH0 =
    A.class "sm:h-0"


smH1 : Html.Attribute msg
smH1 =
    A.class "sm:h-1"


smH2 : Html.Attribute msg
smH2 =
    A.class "sm:h-2"


smH3 : Html.Attribute msg
smH3 =
    A.class "sm:h-3"


smH4 : Html.Attribute msg
smH4 =
    A.class "sm:h-4"


smH5 : Html.Attribute msg
smH5 =
    A.class "sm:h-5"


smH6 : Html.Attribute msg
smH6 =
    A.class "sm:h-6"


smH8 : Html.Attribute msg
smH8 =
    A.class "sm:h-8"


smH10 : Html.Attribute msg
smH10 =
    A.class "sm:h-10"


smH12 : Html.Attribute msg
smH12 =
    A.class "sm:h-12"


smH16 : Html.Attribute msg
smH16 =
    A.class "sm:h-16"


smH20 : Html.Attribute msg
smH20 =
    A.class "sm:h-20"


smH24 : Html.Attribute msg
smH24 =
    A.class "sm:h-24"


smH32 : Html.Attribute msg
smH32 =
    A.class "sm:h-32"


smH40 : Html.Attribute msg
smH40 =
    A.class "sm:h-40"


smH48 : Html.Attribute msg
smH48 =
    A.class "sm:h-48"


smH56 : Html.Attribute msg
smH56 =
    A.class "sm:h-56"


smH64 : Html.Attribute msg
smH64 =
    A.class "sm:h-64"


smHAuto : Html.Attribute msg
smHAuto =
    A.class "sm:h-auto"


smHPx : Html.Attribute msg
smHPx =
    A.class "sm:h-px"


smHFull : Html.Attribute msg
smHFull =
    A.class "sm:h-full"


smHScreen : Html.Attribute msg
smHScreen =
    A.class "sm:h-screen"


smTextXs : Html.Attribute msg
smTextXs =
    A.class "sm:text-xs"


smTextSm : Html.Attribute msg
smTextSm =
    A.class "sm:text-sm"


smTextBase : Html.Attribute msg
smTextBase =
    A.class "sm:text-base"


smTextLg : Html.Attribute msg
smTextLg =
    A.class "sm:text-lg"


smTextXl : Html.Attribute msg
smTextXl =
    A.class "sm:text-xl"


smText2xl : Html.Attribute msg
smText2xl =
    A.class "sm:text-2xl"


smText3xl : Html.Attribute msg
smText3xl =
    A.class "sm:text-3xl"


smText4xl : Html.Attribute msg
smText4xl =
    A.class "sm:text-4xl"


smText5xl : Html.Attribute msg
smText5xl =
    A.class "sm:text-5xl"


smText6xl : Html.Attribute msg
smText6xl =
    A.class "sm:text-6xl"


smLeading3 : Html.Attribute msg
smLeading3 =
    A.class "sm:leading-3"


smLeading4 : Html.Attribute msg
smLeading4 =
    A.class "sm:leading-4"


smLeading5 : Html.Attribute msg
smLeading5 =
    A.class "sm:leading-5"


smLeading6 : Html.Attribute msg
smLeading6 =
    A.class "sm:leading-6"


smLeading7 : Html.Attribute msg
smLeading7 =
    A.class "sm:leading-7"


smLeading8 : Html.Attribute msg
smLeading8 =
    A.class "sm:leading-8"


smLeading9 : Html.Attribute msg
smLeading9 =
    A.class "sm:leading-9"


smLeading10 : Html.Attribute msg
smLeading10 =
    A.class "sm:leading-10"


smLeadingNone : Html.Attribute msg
smLeadingNone =
    A.class "sm:leading-none"


smLeadingTight : Html.Attribute msg
smLeadingTight =
    A.class "sm:leading-tight"


smLeadingSnug : Html.Attribute msg
smLeadingSnug =
    A.class "sm:leading-snug"


smLeadingNormal : Html.Attribute msg
smLeadingNormal =
    A.class "sm:leading-normal"


smLeadingRelaxed : Html.Attribute msg
smLeadingRelaxed =
    A.class "sm:leading-relaxed"


smLeadingLoose : Html.Attribute msg
smLeadingLoose =
    A.class "sm:leading-loose"


smListInside : Html.Attribute msg
smListInside =
    A.class "sm:list-inside"


smListOutside : Html.Attribute msg
smListOutside =
    A.class "sm:list-outside"


smListNone : Html.Attribute msg
smListNone =
    A.class "sm:list-none"


smListDisc : Html.Attribute msg
smListDisc =
    A.class "sm:list-disc"


smListDecimal : Html.Attribute msg
smListDecimal =
    A.class "sm:list-decimal"


smM0 : Html.Attribute msg
smM0 =
    A.class "sm:m-0"


smM1 : Html.Attribute msg
smM1 =
    A.class "sm:m-1"


smM2 : Html.Attribute msg
smM2 =
    A.class "sm:m-2"


smM3 : Html.Attribute msg
smM3 =
    A.class "sm:m-3"


smM4 : Html.Attribute msg
smM4 =
    A.class "sm:m-4"


smM5 : Html.Attribute msg
smM5 =
    A.class "sm:m-5"


smM6 : Html.Attribute msg
smM6 =
    A.class "sm:m-6"


smM8 : Html.Attribute msg
smM8 =
    A.class "sm:m-8"


smM10 : Html.Attribute msg
smM10 =
    A.class "sm:m-10"


smM12 : Html.Attribute msg
smM12 =
    A.class "sm:m-12"


smM16 : Html.Attribute msg
smM16 =
    A.class "sm:m-16"


smM20 : Html.Attribute msg
smM20 =
    A.class "sm:m-20"


smM24 : Html.Attribute msg
smM24 =
    A.class "sm:m-24"


smM32 : Html.Attribute msg
smM32 =
    A.class "sm:m-32"


smM40 : Html.Attribute msg
smM40 =
    A.class "sm:m-40"


smM48 : Html.Attribute msg
smM48 =
    A.class "sm:m-48"


smM56 : Html.Attribute msg
smM56 =
    A.class "sm:m-56"


smM64 : Html.Attribute msg
smM64 =
    A.class "sm:m-64"


smMAuto : Html.Attribute msg
smMAuto =
    A.class "sm:m-auto"


smMPx : Html.Attribute msg
smMPx =
    A.class "sm:m-px"


smNegM1 : Html.Attribute msg
smNegM1 =
    A.class "sm:-m-1"


smNegM2 : Html.Attribute msg
smNegM2 =
    A.class "sm:-m-2"


smNegM3 : Html.Attribute msg
smNegM3 =
    A.class "sm:-m-3"


smNegM4 : Html.Attribute msg
smNegM4 =
    A.class "sm:-m-4"


smNegM5 : Html.Attribute msg
smNegM5 =
    A.class "sm:-m-5"


smNegM6 : Html.Attribute msg
smNegM6 =
    A.class "sm:-m-6"


smNegM8 : Html.Attribute msg
smNegM8 =
    A.class "sm:-m-8"


smNegM10 : Html.Attribute msg
smNegM10 =
    A.class "sm:-m-10"


smNegM12 : Html.Attribute msg
smNegM12 =
    A.class "sm:-m-12"


smNegM16 : Html.Attribute msg
smNegM16 =
    A.class "sm:-m-16"


smNegM20 : Html.Attribute msg
smNegM20 =
    A.class "sm:-m-20"


smNegM24 : Html.Attribute msg
smNegM24 =
    A.class "sm:-m-24"


smNegM32 : Html.Attribute msg
smNegM32 =
    A.class "sm:-m-32"


smNegM40 : Html.Attribute msg
smNegM40 =
    A.class "sm:-m-40"


smNegM48 : Html.Attribute msg
smNegM48 =
    A.class "sm:-m-48"


smNegM56 : Html.Attribute msg
smNegM56 =
    A.class "sm:-m-56"


smNegM64 : Html.Attribute msg
smNegM64 =
    A.class "sm:-m-64"


smNegMPx : Html.Attribute msg
smNegMPx =
    A.class "sm:-m-px"


smMy0 : Html.Attribute msg
smMy0 =
    A.class "sm:my-0"


smMx0 : Html.Attribute msg
smMx0 =
    A.class "sm:mx-0"


smMy1 : Html.Attribute msg
smMy1 =
    A.class "sm:my-1"


smMx1 : Html.Attribute msg
smMx1 =
    A.class "sm:mx-1"


smMy2 : Html.Attribute msg
smMy2 =
    A.class "sm:my-2"


smMx2 : Html.Attribute msg
smMx2 =
    A.class "sm:mx-2"


smMy3 : Html.Attribute msg
smMy3 =
    A.class "sm:my-3"


smMx3 : Html.Attribute msg
smMx3 =
    A.class "sm:mx-3"


smMy4 : Html.Attribute msg
smMy4 =
    A.class "sm:my-4"


smMx4 : Html.Attribute msg
smMx4 =
    A.class "sm:mx-4"


smMy5 : Html.Attribute msg
smMy5 =
    A.class "sm:my-5"


smMx5 : Html.Attribute msg
smMx5 =
    A.class "sm:mx-5"


smMy6 : Html.Attribute msg
smMy6 =
    A.class "sm:my-6"


smMx6 : Html.Attribute msg
smMx6 =
    A.class "sm:mx-6"


smMy8 : Html.Attribute msg
smMy8 =
    A.class "sm:my-8"


smMx8 : Html.Attribute msg
smMx8 =
    A.class "sm:mx-8"


smMy10 : Html.Attribute msg
smMy10 =
    A.class "sm:my-10"


smMx10 : Html.Attribute msg
smMx10 =
    A.class "sm:mx-10"


smMy12 : Html.Attribute msg
smMy12 =
    A.class "sm:my-12"


smMx12 : Html.Attribute msg
smMx12 =
    A.class "sm:mx-12"


smMy16 : Html.Attribute msg
smMy16 =
    A.class "sm:my-16"


smMx16 : Html.Attribute msg
smMx16 =
    A.class "sm:mx-16"


smMy20 : Html.Attribute msg
smMy20 =
    A.class "sm:my-20"


smMx20 : Html.Attribute msg
smMx20 =
    A.class "sm:mx-20"


smMy24 : Html.Attribute msg
smMy24 =
    A.class "sm:my-24"


smMx24 : Html.Attribute msg
smMx24 =
    A.class "sm:mx-24"


smMy32 : Html.Attribute msg
smMy32 =
    A.class "sm:my-32"


smMx32 : Html.Attribute msg
smMx32 =
    A.class "sm:mx-32"


smMy40 : Html.Attribute msg
smMy40 =
    A.class "sm:my-40"


smMx40 : Html.Attribute msg
smMx40 =
    A.class "sm:mx-40"


smMy48 : Html.Attribute msg
smMy48 =
    A.class "sm:my-48"


smMx48 : Html.Attribute msg
smMx48 =
    A.class "sm:mx-48"


smMy56 : Html.Attribute msg
smMy56 =
    A.class "sm:my-56"


smMx56 : Html.Attribute msg
smMx56 =
    A.class "sm:mx-56"


smMy64 : Html.Attribute msg
smMy64 =
    A.class "sm:my-64"


smMx64 : Html.Attribute msg
smMx64 =
    A.class "sm:mx-64"


smMyAuto : Html.Attribute msg
smMyAuto =
    A.class "sm:my-auto"


smMxAuto : Html.Attribute msg
smMxAuto =
    A.class "sm:mx-auto"


smMyPx : Html.Attribute msg
smMyPx =
    A.class "sm:my-px"


smMxPx : Html.Attribute msg
smMxPx =
    A.class "sm:mx-px"


smNegMy1 : Html.Attribute msg
smNegMy1 =
    A.class "sm:-my-1"


smNegMx1 : Html.Attribute msg
smNegMx1 =
    A.class "sm:-mx-1"


smNegMy2 : Html.Attribute msg
smNegMy2 =
    A.class "sm:-my-2"


smNegMx2 : Html.Attribute msg
smNegMx2 =
    A.class "sm:-mx-2"


smNegMy3 : Html.Attribute msg
smNegMy3 =
    A.class "sm:-my-3"


smNegMx3 : Html.Attribute msg
smNegMx3 =
    A.class "sm:-mx-3"


smNegMy4 : Html.Attribute msg
smNegMy4 =
    A.class "sm:-my-4"


smNegMx4 : Html.Attribute msg
smNegMx4 =
    A.class "sm:-mx-4"


smNegMy5 : Html.Attribute msg
smNegMy5 =
    A.class "sm:-my-5"


smNegMx5 : Html.Attribute msg
smNegMx5 =
    A.class "sm:-mx-5"


smNegMy6 : Html.Attribute msg
smNegMy6 =
    A.class "sm:-my-6"


smNegMx6 : Html.Attribute msg
smNegMx6 =
    A.class "sm:-mx-6"


smNegMy8 : Html.Attribute msg
smNegMy8 =
    A.class "sm:-my-8"


smNegMx8 : Html.Attribute msg
smNegMx8 =
    A.class "sm:-mx-8"


smNegMy10 : Html.Attribute msg
smNegMy10 =
    A.class "sm:-my-10"


smNegMx10 : Html.Attribute msg
smNegMx10 =
    A.class "sm:-mx-10"


smNegMy12 : Html.Attribute msg
smNegMy12 =
    A.class "sm:-my-12"


smNegMx12 : Html.Attribute msg
smNegMx12 =
    A.class "sm:-mx-12"


smNegMy16 : Html.Attribute msg
smNegMy16 =
    A.class "sm:-my-16"


smNegMx16 : Html.Attribute msg
smNegMx16 =
    A.class "sm:-mx-16"


smNegMy20 : Html.Attribute msg
smNegMy20 =
    A.class "sm:-my-20"


smNegMx20 : Html.Attribute msg
smNegMx20 =
    A.class "sm:-mx-20"


smNegMy24 : Html.Attribute msg
smNegMy24 =
    A.class "sm:-my-24"


smNegMx24 : Html.Attribute msg
smNegMx24 =
    A.class "sm:-mx-24"


smNegMy32 : Html.Attribute msg
smNegMy32 =
    A.class "sm:-my-32"


smNegMx32 : Html.Attribute msg
smNegMx32 =
    A.class "sm:-mx-32"


smNegMy40 : Html.Attribute msg
smNegMy40 =
    A.class "sm:-my-40"


smNegMx40 : Html.Attribute msg
smNegMx40 =
    A.class "sm:-mx-40"


smNegMy48 : Html.Attribute msg
smNegMy48 =
    A.class "sm:-my-48"


smNegMx48 : Html.Attribute msg
smNegMx48 =
    A.class "sm:-mx-48"


smNegMy56 : Html.Attribute msg
smNegMy56 =
    A.class "sm:-my-56"


smNegMx56 : Html.Attribute msg
smNegMx56 =
    A.class "sm:-mx-56"


smNegMy64 : Html.Attribute msg
smNegMy64 =
    A.class "sm:-my-64"


smNegMx64 : Html.Attribute msg
smNegMx64 =
    A.class "sm:-mx-64"


smNegMyPx : Html.Attribute msg
smNegMyPx =
    A.class "sm:-my-px"


smNegMxPx : Html.Attribute msg
smNegMxPx =
    A.class "sm:-mx-px"


smMt0 : Html.Attribute msg
smMt0 =
    A.class "sm:mt-0"


smMr0 : Html.Attribute msg
smMr0 =
    A.class "sm:mr-0"


smMb0 : Html.Attribute msg
smMb0 =
    A.class "sm:mb-0"


smMl0 : Html.Attribute msg
smMl0 =
    A.class "sm:ml-0"


smMt1 : Html.Attribute msg
smMt1 =
    A.class "sm:mt-1"


smMr1 : Html.Attribute msg
smMr1 =
    A.class "sm:mr-1"


smMb1 : Html.Attribute msg
smMb1 =
    A.class "sm:mb-1"


smMl1 : Html.Attribute msg
smMl1 =
    A.class "sm:ml-1"


smMt2 : Html.Attribute msg
smMt2 =
    A.class "sm:mt-2"


smMr2 : Html.Attribute msg
smMr2 =
    A.class "sm:mr-2"


smMb2 : Html.Attribute msg
smMb2 =
    A.class "sm:mb-2"


smMl2 : Html.Attribute msg
smMl2 =
    A.class "sm:ml-2"


smMt3 : Html.Attribute msg
smMt3 =
    A.class "sm:mt-3"


smMr3 : Html.Attribute msg
smMr3 =
    A.class "sm:mr-3"


smMb3 : Html.Attribute msg
smMb3 =
    A.class "sm:mb-3"


smMl3 : Html.Attribute msg
smMl3 =
    A.class "sm:ml-3"


smMt4 : Html.Attribute msg
smMt4 =
    A.class "sm:mt-4"


smMr4 : Html.Attribute msg
smMr4 =
    A.class "sm:mr-4"


smMb4 : Html.Attribute msg
smMb4 =
    A.class "sm:mb-4"


smMl4 : Html.Attribute msg
smMl4 =
    A.class "sm:ml-4"


smMt5 : Html.Attribute msg
smMt5 =
    A.class "sm:mt-5"


smMr5 : Html.Attribute msg
smMr5 =
    A.class "sm:mr-5"


smMb5 : Html.Attribute msg
smMb5 =
    A.class "sm:mb-5"


smMl5 : Html.Attribute msg
smMl5 =
    A.class "sm:ml-5"


smMt6 : Html.Attribute msg
smMt6 =
    A.class "sm:mt-6"


smMr6 : Html.Attribute msg
smMr6 =
    A.class "sm:mr-6"


smMb6 : Html.Attribute msg
smMb6 =
    A.class "sm:mb-6"


smMl6 : Html.Attribute msg
smMl6 =
    A.class "sm:ml-6"


smMt8 : Html.Attribute msg
smMt8 =
    A.class "sm:mt-8"


smMr8 : Html.Attribute msg
smMr8 =
    A.class "sm:mr-8"


smMb8 : Html.Attribute msg
smMb8 =
    A.class "sm:mb-8"


smMl8 : Html.Attribute msg
smMl8 =
    A.class "sm:ml-8"


smMt10 : Html.Attribute msg
smMt10 =
    A.class "sm:mt-10"


smMr10 : Html.Attribute msg
smMr10 =
    A.class "sm:mr-10"


smMb10 : Html.Attribute msg
smMb10 =
    A.class "sm:mb-10"


smMl10 : Html.Attribute msg
smMl10 =
    A.class "sm:ml-10"


smMt12 : Html.Attribute msg
smMt12 =
    A.class "sm:mt-12"


smMr12 : Html.Attribute msg
smMr12 =
    A.class "sm:mr-12"


smMb12 : Html.Attribute msg
smMb12 =
    A.class "sm:mb-12"


smMl12 : Html.Attribute msg
smMl12 =
    A.class "sm:ml-12"


smMt16 : Html.Attribute msg
smMt16 =
    A.class "sm:mt-16"


smMr16 : Html.Attribute msg
smMr16 =
    A.class "sm:mr-16"


smMb16 : Html.Attribute msg
smMb16 =
    A.class "sm:mb-16"


smMl16 : Html.Attribute msg
smMl16 =
    A.class "sm:ml-16"


smMt20 : Html.Attribute msg
smMt20 =
    A.class "sm:mt-20"


smMr20 : Html.Attribute msg
smMr20 =
    A.class "sm:mr-20"


smMb20 : Html.Attribute msg
smMb20 =
    A.class "sm:mb-20"


smMl20 : Html.Attribute msg
smMl20 =
    A.class "sm:ml-20"


smMt24 : Html.Attribute msg
smMt24 =
    A.class "sm:mt-24"


smMr24 : Html.Attribute msg
smMr24 =
    A.class "sm:mr-24"


smMb24 : Html.Attribute msg
smMb24 =
    A.class "sm:mb-24"


smMl24 : Html.Attribute msg
smMl24 =
    A.class "sm:ml-24"


smMt32 : Html.Attribute msg
smMt32 =
    A.class "sm:mt-32"


smMr32 : Html.Attribute msg
smMr32 =
    A.class "sm:mr-32"


smMb32 : Html.Attribute msg
smMb32 =
    A.class "sm:mb-32"


smMl32 : Html.Attribute msg
smMl32 =
    A.class "sm:ml-32"


smMt40 : Html.Attribute msg
smMt40 =
    A.class "sm:mt-40"


smMr40 : Html.Attribute msg
smMr40 =
    A.class "sm:mr-40"


smMb40 : Html.Attribute msg
smMb40 =
    A.class "sm:mb-40"


smMl40 : Html.Attribute msg
smMl40 =
    A.class "sm:ml-40"


smMt48 : Html.Attribute msg
smMt48 =
    A.class "sm:mt-48"


smMr48 : Html.Attribute msg
smMr48 =
    A.class "sm:mr-48"


smMb48 : Html.Attribute msg
smMb48 =
    A.class "sm:mb-48"


smMl48 : Html.Attribute msg
smMl48 =
    A.class "sm:ml-48"


smMt56 : Html.Attribute msg
smMt56 =
    A.class "sm:mt-56"


smMr56 : Html.Attribute msg
smMr56 =
    A.class "sm:mr-56"


smMb56 : Html.Attribute msg
smMb56 =
    A.class "sm:mb-56"


smMl56 : Html.Attribute msg
smMl56 =
    A.class "sm:ml-56"


smMt64 : Html.Attribute msg
smMt64 =
    A.class "sm:mt-64"


smMr64 : Html.Attribute msg
smMr64 =
    A.class "sm:mr-64"


smMb64 : Html.Attribute msg
smMb64 =
    A.class "sm:mb-64"


smMl64 : Html.Attribute msg
smMl64 =
    A.class "sm:ml-64"


smMtAuto : Html.Attribute msg
smMtAuto =
    A.class "sm:mt-auto"


smMrAuto : Html.Attribute msg
smMrAuto =
    A.class "sm:mr-auto"


smMbAuto : Html.Attribute msg
smMbAuto =
    A.class "sm:mb-auto"


smMlAuto : Html.Attribute msg
smMlAuto =
    A.class "sm:ml-auto"


smMtPx : Html.Attribute msg
smMtPx =
    A.class "sm:mt-px"


smMrPx : Html.Attribute msg
smMrPx =
    A.class "sm:mr-px"


smMbPx : Html.Attribute msg
smMbPx =
    A.class "sm:mb-px"


smMlPx : Html.Attribute msg
smMlPx =
    A.class "sm:ml-px"


smNegMt1 : Html.Attribute msg
smNegMt1 =
    A.class "sm:-mt-1"


smNegMr1 : Html.Attribute msg
smNegMr1 =
    A.class "sm:-mr-1"


smNegMb1 : Html.Attribute msg
smNegMb1 =
    A.class "sm:-mb-1"


smNegMl1 : Html.Attribute msg
smNegMl1 =
    A.class "sm:-ml-1"


smNegMt2 : Html.Attribute msg
smNegMt2 =
    A.class "sm:-mt-2"


smNegMr2 : Html.Attribute msg
smNegMr2 =
    A.class "sm:-mr-2"


smNegMb2 : Html.Attribute msg
smNegMb2 =
    A.class "sm:-mb-2"


smNegMl2 : Html.Attribute msg
smNegMl2 =
    A.class "sm:-ml-2"


smNegMt3 : Html.Attribute msg
smNegMt3 =
    A.class "sm:-mt-3"


smNegMr3 : Html.Attribute msg
smNegMr3 =
    A.class "sm:-mr-3"


smNegMb3 : Html.Attribute msg
smNegMb3 =
    A.class "sm:-mb-3"


smNegMl3 : Html.Attribute msg
smNegMl3 =
    A.class "sm:-ml-3"


smNegMt4 : Html.Attribute msg
smNegMt4 =
    A.class "sm:-mt-4"


smNegMr4 : Html.Attribute msg
smNegMr4 =
    A.class "sm:-mr-4"


smNegMb4 : Html.Attribute msg
smNegMb4 =
    A.class "sm:-mb-4"


smNegMl4 : Html.Attribute msg
smNegMl4 =
    A.class "sm:-ml-4"


smNegMt5 : Html.Attribute msg
smNegMt5 =
    A.class "sm:-mt-5"


smNegMr5 : Html.Attribute msg
smNegMr5 =
    A.class "sm:-mr-5"


smNegMb5 : Html.Attribute msg
smNegMb5 =
    A.class "sm:-mb-5"


smNegMl5 : Html.Attribute msg
smNegMl5 =
    A.class "sm:-ml-5"


smNegMt6 : Html.Attribute msg
smNegMt6 =
    A.class "sm:-mt-6"


smNegMr6 : Html.Attribute msg
smNegMr6 =
    A.class "sm:-mr-6"


smNegMb6 : Html.Attribute msg
smNegMb6 =
    A.class "sm:-mb-6"


smNegMl6 : Html.Attribute msg
smNegMl6 =
    A.class "sm:-ml-6"


smNegMt8 : Html.Attribute msg
smNegMt8 =
    A.class "sm:-mt-8"


smNegMr8 : Html.Attribute msg
smNegMr8 =
    A.class "sm:-mr-8"


smNegMb8 : Html.Attribute msg
smNegMb8 =
    A.class "sm:-mb-8"


smNegMl8 : Html.Attribute msg
smNegMl8 =
    A.class "sm:-ml-8"


smNegMt10 : Html.Attribute msg
smNegMt10 =
    A.class "sm:-mt-10"


smNegMr10 : Html.Attribute msg
smNegMr10 =
    A.class "sm:-mr-10"


smNegMb10 : Html.Attribute msg
smNegMb10 =
    A.class "sm:-mb-10"


smNegMl10 : Html.Attribute msg
smNegMl10 =
    A.class "sm:-ml-10"


smNegMt12 : Html.Attribute msg
smNegMt12 =
    A.class "sm:-mt-12"


smNegMr12 : Html.Attribute msg
smNegMr12 =
    A.class "sm:-mr-12"


smNegMb12 : Html.Attribute msg
smNegMb12 =
    A.class "sm:-mb-12"


smNegMl12 : Html.Attribute msg
smNegMl12 =
    A.class "sm:-ml-12"


smNegMt16 : Html.Attribute msg
smNegMt16 =
    A.class "sm:-mt-16"


smNegMr16 : Html.Attribute msg
smNegMr16 =
    A.class "sm:-mr-16"


smNegMb16 : Html.Attribute msg
smNegMb16 =
    A.class "sm:-mb-16"


smNegMl16 : Html.Attribute msg
smNegMl16 =
    A.class "sm:-ml-16"


smNegMt20 : Html.Attribute msg
smNegMt20 =
    A.class "sm:-mt-20"


smNegMr20 : Html.Attribute msg
smNegMr20 =
    A.class "sm:-mr-20"


smNegMb20 : Html.Attribute msg
smNegMb20 =
    A.class "sm:-mb-20"


smNegMl20 : Html.Attribute msg
smNegMl20 =
    A.class "sm:-ml-20"


smNegMt24 : Html.Attribute msg
smNegMt24 =
    A.class "sm:-mt-24"


smNegMr24 : Html.Attribute msg
smNegMr24 =
    A.class "sm:-mr-24"


smNegMb24 : Html.Attribute msg
smNegMb24 =
    A.class "sm:-mb-24"


smNegMl24 : Html.Attribute msg
smNegMl24 =
    A.class "sm:-ml-24"


smNegMt32 : Html.Attribute msg
smNegMt32 =
    A.class "sm:-mt-32"


smNegMr32 : Html.Attribute msg
smNegMr32 =
    A.class "sm:-mr-32"


smNegMb32 : Html.Attribute msg
smNegMb32 =
    A.class "sm:-mb-32"


smNegMl32 : Html.Attribute msg
smNegMl32 =
    A.class "sm:-ml-32"


smNegMt40 : Html.Attribute msg
smNegMt40 =
    A.class "sm:-mt-40"


smNegMr40 : Html.Attribute msg
smNegMr40 =
    A.class "sm:-mr-40"


smNegMb40 : Html.Attribute msg
smNegMb40 =
    A.class "sm:-mb-40"


smNegMl40 : Html.Attribute msg
smNegMl40 =
    A.class "sm:-ml-40"


smNegMt48 : Html.Attribute msg
smNegMt48 =
    A.class "sm:-mt-48"


smNegMr48 : Html.Attribute msg
smNegMr48 =
    A.class "sm:-mr-48"


smNegMb48 : Html.Attribute msg
smNegMb48 =
    A.class "sm:-mb-48"


smNegMl48 : Html.Attribute msg
smNegMl48 =
    A.class "sm:-ml-48"


smNegMt56 : Html.Attribute msg
smNegMt56 =
    A.class "sm:-mt-56"


smNegMr56 : Html.Attribute msg
smNegMr56 =
    A.class "sm:-mr-56"


smNegMb56 : Html.Attribute msg
smNegMb56 =
    A.class "sm:-mb-56"


smNegMl56 : Html.Attribute msg
smNegMl56 =
    A.class "sm:-ml-56"


smNegMt64 : Html.Attribute msg
smNegMt64 =
    A.class "sm:-mt-64"


smNegMr64 : Html.Attribute msg
smNegMr64 =
    A.class "sm:-mr-64"


smNegMb64 : Html.Attribute msg
smNegMb64 =
    A.class "sm:-mb-64"


smNegMl64 : Html.Attribute msg
smNegMl64 =
    A.class "sm:-ml-64"


smNegMtPx : Html.Attribute msg
smNegMtPx =
    A.class "sm:-mt-px"


smNegMrPx : Html.Attribute msg
smNegMrPx =
    A.class "sm:-mr-px"


smNegMbPx : Html.Attribute msg
smNegMbPx =
    A.class "sm:-mb-px"


smNegMlPx : Html.Attribute msg
smNegMlPx =
    A.class "sm:-ml-px"


smMaxHFull : Html.Attribute msg
smMaxHFull =
    A.class "sm:max-h-full"


smMaxHScreen : Html.Attribute msg
smMaxHScreen =
    A.class "sm:max-h-screen"


smMaxWNone : Html.Attribute msg
smMaxWNone =
    A.class "sm:max-w-none"


smMaxWXs : Html.Attribute msg
smMaxWXs =
    A.class "sm:max-w-xs"


smMaxWSm : Html.Attribute msg
smMaxWSm =
    A.class "sm:max-w-sm"


smMaxWMd : Html.Attribute msg
smMaxWMd =
    A.class "sm:max-w-md"


smMaxWLg : Html.Attribute msg
smMaxWLg =
    A.class "sm:max-w-lg"


smMaxWXl : Html.Attribute msg
smMaxWXl =
    A.class "sm:max-w-xl"


smMaxW2xl : Html.Attribute msg
smMaxW2xl =
    A.class "sm:max-w-2xl"


smMaxW3xl : Html.Attribute msg
smMaxW3xl =
    A.class "sm:max-w-3xl"


smMaxW4xl : Html.Attribute msg
smMaxW4xl =
    A.class "sm:max-w-4xl"


smMaxW5xl : Html.Attribute msg
smMaxW5xl =
    A.class "sm:max-w-5xl"


smMaxW6xl : Html.Attribute msg
smMaxW6xl =
    A.class "sm:max-w-6xl"


smMaxWFull : Html.Attribute msg
smMaxWFull =
    A.class "sm:max-w-full"


smMaxWScreenSm : Html.Attribute msg
smMaxWScreenSm =
    A.class "sm:max-w-screen-sm"


smMaxWScreenMd : Html.Attribute msg
smMaxWScreenMd =
    A.class "sm:max-w-screen-md"


smMaxWScreenLg : Html.Attribute msg
smMaxWScreenLg =
    A.class "sm:max-w-screen-lg"


smMaxWScreenXl : Html.Attribute msg
smMaxWScreenXl =
    A.class "sm:max-w-screen-xl"


smMinH0 : Html.Attribute msg
smMinH0 =
    A.class "sm:min-h-0"


smMinHFull : Html.Attribute msg
smMinHFull =
    A.class "sm:min-h-full"


smMinHScreen : Html.Attribute msg
smMinHScreen =
    A.class "sm:min-h-screen"


smMinW0 : Html.Attribute msg
smMinW0 =
    A.class "sm:min-w-0"


smMinWFull : Html.Attribute msg
smMinWFull =
    A.class "sm:min-w-full"


smObjectContain : Html.Attribute msg
smObjectContain =
    A.class "sm:object-contain"


smObjectCover : Html.Attribute msg
smObjectCover =
    A.class "sm:object-cover"


smObjectFill : Html.Attribute msg
smObjectFill =
    A.class "sm:object-fill"


smObjectNone : Html.Attribute msg
smObjectNone =
    A.class "sm:object-none"


smObjectScaleDown : Html.Attribute msg
smObjectScaleDown =
    A.class "sm:object-scale-down"


smObjectBottom : Html.Attribute msg
smObjectBottom =
    A.class "sm:object-bottom"


smObjectCenter : Html.Attribute msg
smObjectCenter =
    A.class "sm:object-center"


smObjectLeft : Html.Attribute msg
smObjectLeft =
    A.class "sm:object-left"


smObjectLeftBottom : Html.Attribute msg
smObjectLeftBottom =
    A.class "sm:object-left-bottom"


smObjectLeftTop : Html.Attribute msg
smObjectLeftTop =
    A.class "sm:object-left-top"


smObjectRight : Html.Attribute msg
smObjectRight =
    A.class "sm:object-right"


smObjectRightBottom : Html.Attribute msg
smObjectRightBottom =
    A.class "sm:object-right-bottom"


smObjectRightTop : Html.Attribute msg
smObjectRightTop =
    A.class "sm:object-right-top"


smObjectTop : Html.Attribute msg
smObjectTop =
    A.class "sm:object-top"


smOpacity0 : Html.Attribute msg
smOpacity0 =
    A.class "sm:opacity-0"


smOpacity25 : Html.Attribute msg
smOpacity25 =
    A.class "sm:opacity-25"


smOpacity50 : Html.Attribute msg
smOpacity50 =
    A.class "sm:opacity-50"


smOpacity75 : Html.Attribute msg
smOpacity75 =
    A.class "sm:opacity-75"


smOpacity100 : Html.Attribute msg
smOpacity100 =
    A.class "sm:opacity-100"


smHoverOpacity0 : Html.Attribute msg
smHoverOpacity0 =
    A.class "sm:hover:opacity-0"


smHoverOpacity25 : Html.Attribute msg
smHoverOpacity25 =
    A.class "sm:hover:opacity-25"


smHoverOpacity50 : Html.Attribute msg
smHoverOpacity50 =
    A.class "sm:hover:opacity-50"


smHoverOpacity75 : Html.Attribute msg
smHoverOpacity75 =
    A.class "sm:hover:opacity-75"


smHoverOpacity100 : Html.Attribute msg
smHoverOpacity100 =
    A.class "sm:hover:opacity-100"


smFocusOpacity0 : Html.Attribute msg
smFocusOpacity0 =
    A.class "sm:focus:opacity-0"


smFocusOpacity25 : Html.Attribute msg
smFocusOpacity25 =
    A.class "sm:focus:opacity-25"


smFocusOpacity50 : Html.Attribute msg
smFocusOpacity50 =
    A.class "sm:focus:opacity-50"


smFocusOpacity75 : Html.Attribute msg
smFocusOpacity75 =
    A.class "sm:focus:opacity-75"


smFocusOpacity100 : Html.Attribute msg
smFocusOpacity100 =
    A.class "sm:focus:opacity-100"


smOutlineNone : Html.Attribute msg
smOutlineNone =
    A.class "sm:outline-none"


smFocusOutlineNone : Html.Attribute msg
smFocusOutlineNone =
    A.class "sm:focus:outline-none"


smOverflowAuto : Html.Attribute msg
smOverflowAuto =
    A.class "sm:overflow-auto"


smOverflowHidden : Html.Attribute msg
smOverflowHidden =
    A.class "sm:overflow-hidden"


smOverflowVisible : Html.Attribute msg
smOverflowVisible =
    A.class "sm:overflow-visible"


smOverflowScroll : Html.Attribute msg
smOverflowScroll =
    A.class "sm:overflow-scroll"


smOverflowXAuto : Html.Attribute msg
smOverflowXAuto =
    A.class "sm:overflow-x-auto"


smOverflowYAuto : Html.Attribute msg
smOverflowYAuto =
    A.class "sm:overflow-y-auto"


smOverflowXHidden : Html.Attribute msg
smOverflowXHidden =
    A.class "sm:overflow-x-hidden"


smOverflowYHidden : Html.Attribute msg
smOverflowYHidden =
    A.class "sm:overflow-y-hidden"


smOverflowXVisible : Html.Attribute msg
smOverflowXVisible =
    A.class "sm:overflow-x-visible"


smOverflowYVisible : Html.Attribute msg
smOverflowYVisible =
    A.class "sm:overflow-y-visible"


smOverflowXScroll : Html.Attribute msg
smOverflowXScroll =
    A.class "sm:overflow-x-scroll"


smOverflowYScroll : Html.Attribute msg
smOverflowYScroll =
    A.class "sm:overflow-y-scroll"


smScrollingTouch : Html.Attribute msg
smScrollingTouch =
    A.class "sm:scrolling-touch"


smScrollingAuto : Html.Attribute msg
smScrollingAuto =
    A.class "sm:scrolling-auto"


smOverscrollAuto : Html.Attribute msg
smOverscrollAuto =
    A.class "sm:overscroll-auto"


smOverscrollContain : Html.Attribute msg
smOverscrollContain =
    A.class "sm:overscroll-contain"


smOverscrollNone : Html.Attribute msg
smOverscrollNone =
    A.class "sm:overscroll-none"


smOverscrollYAuto : Html.Attribute msg
smOverscrollYAuto =
    A.class "sm:overscroll-y-auto"


smOverscrollYContain : Html.Attribute msg
smOverscrollYContain =
    A.class "sm:overscroll-y-contain"


smOverscrollYNone : Html.Attribute msg
smOverscrollYNone =
    A.class "sm:overscroll-y-none"


smOverscrollXAuto : Html.Attribute msg
smOverscrollXAuto =
    A.class "sm:overscroll-x-auto"


smOverscrollXContain : Html.Attribute msg
smOverscrollXContain =
    A.class "sm:overscroll-x-contain"


smOverscrollXNone : Html.Attribute msg
smOverscrollXNone =
    A.class "sm:overscroll-x-none"


smP0 : Html.Attribute msg
smP0 =
    A.class "sm:p-0"


smP1 : Html.Attribute msg
smP1 =
    A.class "sm:p-1"


smP2 : Html.Attribute msg
smP2 =
    A.class "sm:p-2"


smP3 : Html.Attribute msg
smP3 =
    A.class "sm:p-3"


smP4 : Html.Attribute msg
smP4 =
    A.class "sm:p-4"


smP5 : Html.Attribute msg
smP5 =
    A.class "sm:p-5"


smP6 : Html.Attribute msg
smP6 =
    A.class "sm:p-6"


smP8 : Html.Attribute msg
smP8 =
    A.class "sm:p-8"


smP10 : Html.Attribute msg
smP10 =
    A.class "sm:p-10"


smP12 : Html.Attribute msg
smP12 =
    A.class "sm:p-12"


smP16 : Html.Attribute msg
smP16 =
    A.class "sm:p-16"


smP20 : Html.Attribute msg
smP20 =
    A.class "sm:p-20"


smP24 : Html.Attribute msg
smP24 =
    A.class "sm:p-24"


smP32 : Html.Attribute msg
smP32 =
    A.class "sm:p-32"


smP40 : Html.Attribute msg
smP40 =
    A.class "sm:p-40"


smP48 : Html.Attribute msg
smP48 =
    A.class "sm:p-48"


smP56 : Html.Attribute msg
smP56 =
    A.class "sm:p-56"


smP64 : Html.Attribute msg
smP64 =
    A.class "sm:p-64"


smPPx : Html.Attribute msg
smPPx =
    A.class "sm:p-px"


smPy0 : Html.Attribute msg
smPy0 =
    A.class "sm:py-0"


smPx0 : Html.Attribute msg
smPx0 =
    A.class "sm:px-0"


smPy1 : Html.Attribute msg
smPy1 =
    A.class "sm:py-1"


smPx1 : Html.Attribute msg
smPx1 =
    A.class "sm:px-1"


smPy2 : Html.Attribute msg
smPy2 =
    A.class "sm:py-2"


smPx2 : Html.Attribute msg
smPx2 =
    A.class "sm:px-2"


smPy3 : Html.Attribute msg
smPy3 =
    A.class "sm:py-3"


smPx3 : Html.Attribute msg
smPx3 =
    A.class "sm:px-3"


smPy4 : Html.Attribute msg
smPy4 =
    A.class "sm:py-4"


smPx4 : Html.Attribute msg
smPx4 =
    A.class "sm:px-4"


smPy5 : Html.Attribute msg
smPy5 =
    A.class "sm:py-5"


smPx5 : Html.Attribute msg
smPx5 =
    A.class "sm:px-5"


smPy6 : Html.Attribute msg
smPy6 =
    A.class "sm:py-6"


smPx6 : Html.Attribute msg
smPx6 =
    A.class "sm:px-6"


smPy8 : Html.Attribute msg
smPy8 =
    A.class "sm:py-8"


smPx8 : Html.Attribute msg
smPx8 =
    A.class "sm:px-8"


smPy10 : Html.Attribute msg
smPy10 =
    A.class "sm:py-10"


smPx10 : Html.Attribute msg
smPx10 =
    A.class "sm:px-10"


smPy12 : Html.Attribute msg
smPy12 =
    A.class "sm:py-12"


smPx12 : Html.Attribute msg
smPx12 =
    A.class "sm:px-12"


smPy16 : Html.Attribute msg
smPy16 =
    A.class "sm:py-16"


smPx16 : Html.Attribute msg
smPx16 =
    A.class "sm:px-16"


smPy20 : Html.Attribute msg
smPy20 =
    A.class "sm:py-20"


smPx20 : Html.Attribute msg
smPx20 =
    A.class "sm:px-20"


smPy24 : Html.Attribute msg
smPy24 =
    A.class "sm:py-24"


smPx24 : Html.Attribute msg
smPx24 =
    A.class "sm:px-24"


smPy32 : Html.Attribute msg
smPy32 =
    A.class "sm:py-32"


smPx32 : Html.Attribute msg
smPx32 =
    A.class "sm:px-32"


smPy40 : Html.Attribute msg
smPy40 =
    A.class "sm:py-40"


smPx40 : Html.Attribute msg
smPx40 =
    A.class "sm:px-40"


smPy48 : Html.Attribute msg
smPy48 =
    A.class "sm:py-48"


smPx48 : Html.Attribute msg
smPx48 =
    A.class "sm:px-48"


smPy56 : Html.Attribute msg
smPy56 =
    A.class "sm:py-56"


smPx56 : Html.Attribute msg
smPx56 =
    A.class "sm:px-56"


smPy64 : Html.Attribute msg
smPy64 =
    A.class "sm:py-64"


smPx64 : Html.Attribute msg
smPx64 =
    A.class "sm:px-64"


smPyPx : Html.Attribute msg
smPyPx =
    A.class "sm:py-px"


smPxPx : Html.Attribute msg
smPxPx =
    A.class "sm:px-px"


smPt0 : Html.Attribute msg
smPt0 =
    A.class "sm:pt-0"


smPr0 : Html.Attribute msg
smPr0 =
    A.class "sm:pr-0"


smPb0 : Html.Attribute msg
smPb0 =
    A.class "sm:pb-0"


smPl0 : Html.Attribute msg
smPl0 =
    A.class "sm:pl-0"


smPt1 : Html.Attribute msg
smPt1 =
    A.class "sm:pt-1"


smPr1 : Html.Attribute msg
smPr1 =
    A.class "sm:pr-1"


smPb1 : Html.Attribute msg
smPb1 =
    A.class "sm:pb-1"


smPl1 : Html.Attribute msg
smPl1 =
    A.class "sm:pl-1"


smPt2 : Html.Attribute msg
smPt2 =
    A.class "sm:pt-2"


smPr2 : Html.Attribute msg
smPr2 =
    A.class "sm:pr-2"


smPb2 : Html.Attribute msg
smPb2 =
    A.class "sm:pb-2"


smPl2 : Html.Attribute msg
smPl2 =
    A.class "sm:pl-2"


smPt3 : Html.Attribute msg
smPt3 =
    A.class "sm:pt-3"


smPr3 : Html.Attribute msg
smPr3 =
    A.class "sm:pr-3"


smPb3 : Html.Attribute msg
smPb3 =
    A.class "sm:pb-3"


smPl3 : Html.Attribute msg
smPl3 =
    A.class "sm:pl-3"


smPt4 : Html.Attribute msg
smPt4 =
    A.class "sm:pt-4"


smPr4 : Html.Attribute msg
smPr4 =
    A.class "sm:pr-4"


smPb4 : Html.Attribute msg
smPb4 =
    A.class "sm:pb-4"


smPl4 : Html.Attribute msg
smPl4 =
    A.class "sm:pl-4"


smPt5 : Html.Attribute msg
smPt5 =
    A.class "sm:pt-5"


smPr5 : Html.Attribute msg
smPr5 =
    A.class "sm:pr-5"


smPb5 : Html.Attribute msg
smPb5 =
    A.class "sm:pb-5"


smPl5 : Html.Attribute msg
smPl5 =
    A.class "sm:pl-5"


smPt6 : Html.Attribute msg
smPt6 =
    A.class "sm:pt-6"


smPr6 : Html.Attribute msg
smPr6 =
    A.class "sm:pr-6"


smPb6 : Html.Attribute msg
smPb6 =
    A.class "sm:pb-6"


smPl6 : Html.Attribute msg
smPl6 =
    A.class "sm:pl-6"


smPt8 : Html.Attribute msg
smPt8 =
    A.class "sm:pt-8"


smPr8 : Html.Attribute msg
smPr8 =
    A.class "sm:pr-8"


smPb8 : Html.Attribute msg
smPb8 =
    A.class "sm:pb-8"


smPl8 : Html.Attribute msg
smPl8 =
    A.class "sm:pl-8"


smPt10 : Html.Attribute msg
smPt10 =
    A.class "sm:pt-10"


smPr10 : Html.Attribute msg
smPr10 =
    A.class "sm:pr-10"


smPb10 : Html.Attribute msg
smPb10 =
    A.class "sm:pb-10"


smPl10 : Html.Attribute msg
smPl10 =
    A.class "sm:pl-10"


smPt12 : Html.Attribute msg
smPt12 =
    A.class "sm:pt-12"


smPr12 : Html.Attribute msg
smPr12 =
    A.class "sm:pr-12"


smPb12 : Html.Attribute msg
smPb12 =
    A.class "sm:pb-12"


smPl12 : Html.Attribute msg
smPl12 =
    A.class "sm:pl-12"


smPt16 : Html.Attribute msg
smPt16 =
    A.class "sm:pt-16"


smPr16 : Html.Attribute msg
smPr16 =
    A.class "sm:pr-16"


smPb16 : Html.Attribute msg
smPb16 =
    A.class "sm:pb-16"


smPl16 : Html.Attribute msg
smPl16 =
    A.class "sm:pl-16"


smPt20 : Html.Attribute msg
smPt20 =
    A.class "sm:pt-20"


smPr20 : Html.Attribute msg
smPr20 =
    A.class "sm:pr-20"


smPb20 : Html.Attribute msg
smPb20 =
    A.class "sm:pb-20"


smPl20 : Html.Attribute msg
smPl20 =
    A.class "sm:pl-20"


smPt24 : Html.Attribute msg
smPt24 =
    A.class "sm:pt-24"


smPr24 : Html.Attribute msg
smPr24 =
    A.class "sm:pr-24"


smPb24 : Html.Attribute msg
smPb24 =
    A.class "sm:pb-24"


smPl24 : Html.Attribute msg
smPl24 =
    A.class "sm:pl-24"


smPt32 : Html.Attribute msg
smPt32 =
    A.class "sm:pt-32"


smPr32 : Html.Attribute msg
smPr32 =
    A.class "sm:pr-32"


smPb32 : Html.Attribute msg
smPb32 =
    A.class "sm:pb-32"


smPl32 : Html.Attribute msg
smPl32 =
    A.class "sm:pl-32"


smPt40 : Html.Attribute msg
smPt40 =
    A.class "sm:pt-40"


smPr40 : Html.Attribute msg
smPr40 =
    A.class "sm:pr-40"


smPb40 : Html.Attribute msg
smPb40 =
    A.class "sm:pb-40"


smPl40 : Html.Attribute msg
smPl40 =
    A.class "sm:pl-40"


smPt48 : Html.Attribute msg
smPt48 =
    A.class "sm:pt-48"


smPr48 : Html.Attribute msg
smPr48 =
    A.class "sm:pr-48"


smPb48 : Html.Attribute msg
smPb48 =
    A.class "sm:pb-48"


smPl48 : Html.Attribute msg
smPl48 =
    A.class "sm:pl-48"


smPt56 : Html.Attribute msg
smPt56 =
    A.class "sm:pt-56"


smPr56 : Html.Attribute msg
smPr56 =
    A.class "sm:pr-56"


smPb56 : Html.Attribute msg
smPb56 =
    A.class "sm:pb-56"


smPl56 : Html.Attribute msg
smPl56 =
    A.class "sm:pl-56"


smPt64 : Html.Attribute msg
smPt64 =
    A.class "sm:pt-64"


smPr64 : Html.Attribute msg
smPr64 =
    A.class "sm:pr-64"


smPb64 : Html.Attribute msg
smPb64 =
    A.class "sm:pb-64"


smPl64 : Html.Attribute msg
smPl64 =
    A.class "sm:pl-64"


smPtPx : Html.Attribute msg
smPtPx =
    A.class "sm:pt-px"


smPrPx : Html.Attribute msg
smPrPx =
    A.class "sm:pr-px"


smPbPx : Html.Attribute msg
smPbPx =
    A.class "sm:pb-px"


smPlPx : Html.Attribute msg
smPlPx =
    A.class "sm:pl-px"


smPlaceholderTransparent : Html.Attribute msg
smPlaceholderTransparent =
    A.class "sm:placeholder-transparent"


smPlaceholderCurrent : Html.Attribute msg
smPlaceholderCurrent =
    A.class "sm:placeholder-current"


smPlaceholderBlack : Html.Attribute msg
smPlaceholderBlack =
    A.class "sm:placeholder-black"


smPlaceholderWhite : Html.Attribute msg
smPlaceholderWhite =
    A.class "sm:placeholder-white"


smPlaceholderGray100 : Html.Attribute msg
smPlaceholderGray100 =
    A.class "sm:placeholder-gray-100"


smPlaceholderGray200 : Html.Attribute msg
smPlaceholderGray200 =
    A.class "sm:placeholder-gray-200"


smPlaceholderGray300 : Html.Attribute msg
smPlaceholderGray300 =
    A.class "sm:placeholder-gray-300"


smPlaceholderGray400 : Html.Attribute msg
smPlaceholderGray400 =
    A.class "sm:placeholder-gray-400"


smPlaceholderGray500 : Html.Attribute msg
smPlaceholderGray500 =
    A.class "sm:placeholder-gray-500"


smPlaceholderGray600 : Html.Attribute msg
smPlaceholderGray600 =
    A.class "sm:placeholder-gray-600"


smPlaceholderGray700 : Html.Attribute msg
smPlaceholderGray700 =
    A.class "sm:placeholder-gray-700"


smPlaceholderGray800 : Html.Attribute msg
smPlaceholderGray800 =
    A.class "sm:placeholder-gray-800"


smPlaceholderGray900 : Html.Attribute msg
smPlaceholderGray900 =
    A.class "sm:placeholder-gray-900"


smPlaceholderRed100 : Html.Attribute msg
smPlaceholderRed100 =
    A.class "sm:placeholder-red-100"


smPlaceholderRed200 : Html.Attribute msg
smPlaceholderRed200 =
    A.class "sm:placeholder-red-200"


smPlaceholderRed300 : Html.Attribute msg
smPlaceholderRed300 =
    A.class "sm:placeholder-red-300"


smPlaceholderRed400 : Html.Attribute msg
smPlaceholderRed400 =
    A.class "sm:placeholder-red-400"


smPlaceholderRed500 : Html.Attribute msg
smPlaceholderRed500 =
    A.class "sm:placeholder-red-500"


smPlaceholderRed600 : Html.Attribute msg
smPlaceholderRed600 =
    A.class "sm:placeholder-red-600"


smPlaceholderRed700 : Html.Attribute msg
smPlaceholderRed700 =
    A.class "sm:placeholder-red-700"


smPlaceholderRed800 : Html.Attribute msg
smPlaceholderRed800 =
    A.class "sm:placeholder-red-800"


smPlaceholderRed900 : Html.Attribute msg
smPlaceholderRed900 =
    A.class "sm:placeholder-red-900"


smPlaceholderOrange100 : Html.Attribute msg
smPlaceholderOrange100 =
    A.class "sm:placeholder-orange-100"


smPlaceholderOrange200 : Html.Attribute msg
smPlaceholderOrange200 =
    A.class "sm:placeholder-orange-200"


smPlaceholderOrange300 : Html.Attribute msg
smPlaceholderOrange300 =
    A.class "sm:placeholder-orange-300"


smPlaceholderOrange400 : Html.Attribute msg
smPlaceholderOrange400 =
    A.class "sm:placeholder-orange-400"


smPlaceholderOrange500 : Html.Attribute msg
smPlaceholderOrange500 =
    A.class "sm:placeholder-orange-500"


smPlaceholderOrange600 : Html.Attribute msg
smPlaceholderOrange600 =
    A.class "sm:placeholder-orange-600"


smPlaceholderOrange700 : Html.Attribute msg
smPlaceholderOrange700 =
    A.class "sm:placeholder-orange-700"


smPlaceholderOrange800 : Html.Attribute msg
smPlaceholderOrange800 =
    A.class "sm:placeholder-orange-800"


smPlaceholderOrange900 : Html.Attribute msg
smPlaceholderOrange900 =
    A.class "sm:placeholder-orange-900"


smPlaceholderYellow100 : Html.Attribute msg
smPlaceholderYellow100 =
    A.class "sm:placeholder-yellow-100"


smPlaceholderYellow200 : Html.Attribute msg
smPlaceholderYellow200 =
    A.class "sm:placeholder-yellow-200"


smPlaceholderYellow300 : Html.Attribute msg
smPlaceholderYellow300 =
    A.class "sm:placeholder-yellow-300"


smPlaceholderYellow400 : Html.Attribute msg
smPlaceholderYellow400 =
    A.class "sm:placeholder-yellow-400"


smPlaceholderYellow500 : Html.Attribute msg
smPlaceholderYellow500 =
    A.class "sm:placeholder-yellow-500"


smPlaceholderYellow600 : Html.Attribute msg
smPlaceholderYellow600 =
    A.class "sm:placeholder-yellow-600"


smPlaceholderYellow700 : Html.Attribute msg
smPlaceholderYellow700 =
    A.class "sm:placeholder-yellow-700"


smPlaceholderYellow800 : Html.Attribute msg
smPlaceholderYellow800 =
    A.class "sm:placeholder-yellow-800"


smPlaceholderYellow900 : Html.Attribute msg
smPlaceholderYellow900 =
    A.class "sm:placeholder-yellow-900"


smPlaceholderGreen100 : Html.Attribute msg
smPlaceholderGreen100 =
    A.class "sm:placeholder-green-100"


smPlaceholderGreen200 : Html.Attribute msg
smPlaceholderGreen200 =
    A.class "sm:placeholder-green-200"


smPlaceholderGreen300 : Html.Attribute msg
smPlaceholderGreen300 =
    A.class "sm:placeholder-green-300"


smPlaceholderGreen400 : Html.Attribute msg
smPlaceholderGreen400 =
    A.class "sm:placeholder-green-400"


smPlaceholderGreen500 : Html.Attribute msg
smPlaceholderGreen500 =
    A.class "sm:placeholder-green-500"


smPlaceholderGreen600 : Html.Attribute msg
smPlaceholderGreen600 =
    A.class "sm:placeholder-green-600"


smPlaceholderGreen700 : Html.Attribute msg
smPlaceholderGreen700 =
    A.class "sm:placeholder-green-700"


smPlaceholderGreen800 : Html.Attribute msg
smPlaceholderGreen800 =
    A.class "sm:placeholder-green-800"


smPlaceholderGreen900 : Html.Attribute msg
smPlaceholderGreen900 =
    A.class "sm:placeholder-green-900"


smPlaceholderTeal100 : Html.Attribute msg
smPlaceholderTeal100 =
    A.class "sm:placeholder-teal-100"


smPlaceholderTeal200 : Html.Attribute msg
smPlaceholderTeal200 =
    A.class "sm:placeholder-teal-200"


smPlaceholderTeal300 : Html.Attribute msg
smPlaceholderTeal300 =
    A.class "sm:placeholder-teal-300"


smPlaceholderTeal400 : Html.Attribute msg
smPlaceholderTeal400 =
    A.class "sm:placeholder-teal-400"


smPlaceholderTeal500 : Html.Attribute msg
smPlaceholderTeal500 =
    A.class "sm:placeholder-teal-500"


smPlaceholderTeal600 : Html.Attribute msg
smPlaceholderTeal600 =
    A.class "sm:placeholder-teal-600"


smPlaceholderTeal700 : Html.Attribute msg
smPlaceholderTeal700 =
    A.class "sm:placeholder-teal-700"


smPlaceholderTeal800 : Html.Attribute msg
smPlaceholderTeal800 =
    A.class "sm:placeholder-teal-800"


smPlaceholderTeal900 : Html.Attribute msg
smPlaceholderTeal900 =
    A.class "sm:placeholder-teal-900"


smPlaceholderBlue100 : Html.Attribute msg
smPlaceholderBlue100 =
    A.class "sm:placeholder-blue-100"


smPlaceholderBlue200 : Html.Attribute msg
smPlaceholderBlue200 =
    A.class "sm:placeholder-blue-200"


smPlaceholderBlue300 : Html.Attribute msg
smPlaceholderBlue300 =
    A.class "sm:placeholder-blue-300"


smPlaceholderBlue400 : Html.Attribute msg
smPlaceholderBlue400 =
    A.class "sm:placeholder-blue-400"


smPlaceholderBlue500 : Html.Attribute msg
smPlaceholderBlue500 =
    A.class "sm:placeholder-blue-500"


smPlaceholderBlue600 : Html.Attribute msg
smPlaceholderBlue600 =
    A.class "sm:placeholder-blue-600"


smPlaceholderBlue700 : Html.Attribute msg
smPlaceholderBlue700 =
    A.class "sm:placeholder-blue-700"


smPlaceholderBlue800 : Html.Attribute msg
smPlaceholderBlue800 =
    A.class "sm:placeholder-blue-800"


smPlaceholderBlue900 : Html.Attribute msg
smPlaceholderBlue900 =
    A.class "sm:placeholder-blue-900"


smPlaceholderIndigo100 : Html.Attribute msg
smPlaceholderIndigo100 =
    A.class "sm:placeholder-indigo-100"


smPlaceholderIndigo200 : Html.Attribute msg
smPlaceholderIndigo200 =
    A.class "sm:placeholder-indigo-200"


smPlaceholderIndigo300 : Html.Attribute msg
smPlaceholderIndigo300 =
    A.class "sm:placeholder-indigo-300"


smPlaceholderIndigo400 : Html.Attribute msg
smPlaceholderIndigo400 =
    A.class "sm:placeholder-indigo-400"


smPlaceholderIndigo500 : Html.Attribute msg
smPlaceholderIndigo500 =
    A.class "sm:placeholder-indigo-500"


smPlaceholderIndigo600 : Html.Attribute msg
smPlaceholderIndigo600 =
    A.class "sm:placeholder-indigo-600"


smPlaceholderIndigo700 : Html.Attribute msg
smPlaceholderIndigo700 =
    A.class "sm:placeholder-indigo-700"


smPlaceholderIndigo800 : Html.Attribute msg
smPlaceholderIndigo800 =
    A.class "sm:placeholder-indigo-800"


smPlaceholderIndigo900 : Html.Attribute msg
smPlaceholderIndigo900 =
    A.class "sm:placeholder-indigo-900"


smPlaceholderPurple100 : Html.Attribute msg
smPlaceholderPurple100 =
    A.class "sm:placeholder-purple-100"


smPlaceholderPurple200 : Html.Attribute msg
smPlaceholderPurple200 =
    A.class "sm:placeholder-purple-200"


smPlaceholderPurple300 : Html.Attribute msg
smPlaceholderPurple300 =
    A.class "sm:placeholder-purple-300"


smPlaceholderPurple400 : Html.Attribute msg
smPlaceholderPurple400 =
    A.class "sm:placeholder-purple-400"


smPlaceholderPurple500 : Html.Attribute msg
smPlaceholderPurple500 =
    A.class "sm:placeholder-purple-500"


smPlaceholderPurple600 : Html.Attribute msg
smPlaceholderPurple600 =
    A.class "sm:placeholder-purple-600"


smPlaceholderPurple700 : Html.Attribute msg
smPlaceholderPurple700 =
    A.class "sm:placeholder-purple-700"


smPlaceholderPurple800 : Html.Attribute msg
smPlaceholderPurple800 =
    A.class "sm:placeholder-purple-800"


smPlaceholderPurple900 : Html.Attribute msg
smPlaceholderPurple900 =
    A.class "sm:placeholder-purple-900"


smPlaceholderPink100 : Html.Attribute msg
smPlaceholderPink100 =
    A.class "sm:placeholder-pink-100"


smPlaceholderPink200 : Html.Attribute msg
smPlaceholderPink200 =
    A.class "sm:placeholder-pink-200"


smPlaceholderPink300 : Html.Attribute msg
smPlaceholderPink300 =
    A.class "sm:placeholder-pink-300"


smPlaceholderPink400 : Html.Attribute msg
smPlaceholderPink400 =
    A.class "sm:placeholder-pink-400"


smPlaceholderPink500 : Html.Attribute msg
smPlaceholderPink500 =
    A.class "sm:placeholder-pink-500"


smPlaceholderPink600 : Html.Attribute msg
smPlaceholderPink600 =
    A.class "sm:placeholder-pink-600"


smPlaceholderPink700 : Html.Attribute msg
smPlaceholderPink700 =
    A.class "sm:placeholder-pink-700"


smPlaceholderPink800 : Html.Attribute msg
smPlaceholderPink800 =
    A.class "sm:placeholder-pink-800"


smPlaceholderPink900 : Html.Attribute msg
smPlaceholderPink900 =
    A.class "sm:placeholder-pink-900"


smFocusPlaceholderTransparentFocus : Html.Attribute msg
smFocusPlaceholderTransparentFocus =
    A.class "sm:focus:placeholder-transparent:focus"


smFocusPlaceholderCurrentFocus : Html.Attribute msg
smFocusPlaceholderCurrentFocus =
    A.class "sm:focus:placeholder-current:focus"


smFocusPlaceholderBlackFocus : Html.Attribute msg
smFocusPlaceholderBlackFocus =
    A.class "sm:focus:placeholder-black:focus"


smFocusPlaceholderWhiteFocus : Html.Attribute msg
smFocusPlaceholderWhiteFocus =
    A.class "sm:focus:placeholder-white:focus"


smFocusPlaceholderGray100Focus : Html.Attribute msg
smFocusPlaceholderGray100Focus =
    A.class "sm:focus:placeholder-gray-100:focus"


smFocusPlaceholderGray200Focus : Html.Attribute msg
smFocusPlaceholderGray200Focus =
    A.class "sm:focus:placeholder-gray-200:focus"


smFocusPlaceholderGray300Focus : Html.Attribute msg
smFocusPlaceholderGray300Focus =
    A.class "sm:focus:placeholder-gray-300:focus"


smFocusPlaceholderGray400Focus : Html.Attribute msg
smFocusPlaceholderGray400Focus =
    A.class "sm:focus:placeholder-gray-400:focus"


smFocusPlaceholderGray500Focus : Html.Attribute msg
smFocusPlaceholderGray500Focus =
    A.class "sm:focus:placeholder-gray-500:focus"


smFocusPlaceholderGray600Focus : Html.Attribute msg
smFocusPlaceholderGray600Focus =
    A.class "sm:focus:placeholder-gray-600:focus"


smFocusPlaceholderGray700Focus : Html.Attribute msg
smFocusPlaceholderGray700Focus =
    A.class "sm:focus:placeholder-gray-700:focus"


smFocusPlaceholderGray800Focus : Html.Attribute msg
smFocusPlaceholderGray800Focus =
    A.class "sm:focus:placeholder-gray-800:focus"


smFocusPlaceholderGray900Focus : Html.Attribute msg
smFocusPlaceholderGray900Focus =
    A.class "sm:focus:placeholder-gray-900:focus"


smFocusPlaceholderRed100Focus : Html.Attribute msg
smFocusPlaceholderRed100Focus =
    A.class "sm:focus:placeholder-red-100:focus"


smFocusPlaceholderRed200Focus : Html.Attribute msg
smFocusPlaceholderRed200Focus =
    A.class "sm:focus:placeholder-red-200:focus"


smFocusPlaceholderRed300Focus : Html.Attribute msg
smFocusPlaceholderRed300Focus =
    A.class "sm:focus:placeholder-red-300:focus"


smFocusPlaceholderRed400Focus : Html.Attribute msg
smFocusPlaceholderRed400Focus =
    A.class "sm:focus:placeholder-red-400:focus"


smFocusPlaceholderRed500Focus : Html.Attribute msg
smFocusPlaceholderRed500Focus =
    A.class "sm:focus:placeholder-red-500:focus"


smFocusPlaceholderRed600Focus : Html.Attribute msg
smFocusPlaceholderRed600Focus =
    A.class "sm:focus:placeholder-red-600:focus"


smFocusPlaceholderRed700Focus : Html.Attribute msg
smFocusPlaceholderRed700Focus =
    A.class "sm:focus:placeholder-red-700:focus"


smFocusPlaceholderRed800Focus : Html.Attribute msg
smFocusPlaceholderRed800Focus =
    A.class "sm:focus:placeholder-red-800:focus"


smFocusPlaceholderRed900Focus : Html.Attribute msg
smFocusPlaceholderRed900Focus =
    A.class "sm:focus:placeholder-red-900:focus"


smFocusPlaceholderOrange100Focus : Html.Attribute msg
smFocusPlaceholderOrange100Focus =
    A.class "sm:focus:placeholder-orange-100:focus"


smFocusPlaceholderOrange200Focus : Html.Attribute msg
smFocusPlaceholderOrange200Focus =
    A.class "sm:focus:placeholder-orange-200:focus"


smFocusPlaceholderOrange300Focus : Html.Attribute msg
smFocusPlaceholderOrange300Focus =
    A.class "sm:focus:placeholder-orange-300:focus"


smFocusPlaceholderOrange400Focus : Html.Attribute msg
smFocusPlaceholderOrange400Focus =
    A.class "sm:focus:placeholder-orange-400:focus"


smFocusPlaceholderOrange500Focus : Html.Attribute msg
smFocusPlaceholderOrange500Focus =
    A.class "sm:focus:placeholder-orange-500:focus"


smFocusPlaceholderOrange600Focus : Html.Attribute msg
smFocusPlaceholderOrange600Focus =
    A.class "sm:focus:placeholder-orange-600:focus"


smFocusPlaceholderOrange700Focus : Html.Attribute msg
smFocusPlaceholderOrange700Focus =
    A.class "sm:focus:placeholder-orange-700:focus"


smFocusPlaceholderOrange800Focus : Html.Attribute msg
smFocusPlaceholderOrange800Focus =
    A.class "sm:focus:placeholder-orange-800:focus"


smFocusPlaceholderOrange900Focus : Html.Attribute msg
smFocusPlaceholderOrange900Focus =
    A.class "sm:focus:placeholder-orange-900:focus"


smFocusPlaceholderYellow100Focus : Html.Attribute msg
smFocusPlaceholderYellow100Focus =
    A.class "sm:focus:placeholder-yellow-100:focus"


smFocusPlaceholderYellow200Focus : Html.Attribute msg
smFocusPlaceholderYellow200Focus =
    A.class "sm:focus:placeholder-yellow-200:focus"


smFocusPlaceholderYellow300Focus : Html.Attribute msg
smFocusPlaceholderYellow300Focus =
    A.class "sm:focus:placeholder-yellow-300:focus"


smFocusPlaceholderYellow400Focus : Html.Attribute msg
smFocusPlaceholderYellow400Focus =
    A.class "sm:focus:placeholder-yellow-400:focus"


smFocusPlaceholderYellow500Focus : Html.Attribute msg
smFocusPlaceholderYellow500Focus =
    A.class "sm:focus:placeholder-yellow-500:focus"


smFocusPlaceholderYellow600Focus : Html.Attribute msg
smFocusPlaceholderYellow600Focus =
    A.class "sm:focus:placeholder-yellow-600:focus"


smFocusPlaceholderYellow700Focus : Html.Attribute msg
smFocusPlaceholderYellow700Focus =
    A.class "sm:focus:placeholder-yellow-700:focus"


smFocusPlaceholderYellow800Focus : Html.Attribute msg
smFocusPlaceholderYellow800Focus =
    A.class "sm:focus:placeholder-yellow-800:focus"


smFocusPlaceholderYellow900Focus : Html.Attribute msg
smFocusPlaceholderYellow900Focus =
    A.class "sm:focus:placeholder-yellow-900:focus"


smFocusPlaceholderGreen100Focus : Html.Attribute msg
smFocusPlaceholderGreen100Focus =
    A.class "sm:focus:placeholder-green-100:focus"


smFocusPlaceholderGreen200Focus : Html.Attribute msg
smFocusPlaceholderGreen200Focus =
    A.class "sm:focus:placeholder-green-200:focus"


smFocusPlaceholderGreen300Focus : Html.Attribute msg
smFocusPlaceholderGreen300Focus =
    A.class "sm:focus:placeholder-green-300:focus"


smFocusPlaceholderGreen400Focus : Html.Attribute msg
smFocusPlaceholderGreen400Focus =
    A.class "sm:focus:placeholder-green-400:focus"


smFocusPlaceholderGreen500Focus : Html.Attribute msg
smFocusPlaceholderGreen500Focus =
    A.class "sm:focus:placeholder-green-500:focus"


smFocusPlaceholderGreen600Focus : Html.Attribute msg
smFocusPlaceholderGreen600Focus =
    A.class "sm:focus:placeholder-green-600:focus"


smFocusPlaceholderGreen700Focus : Html.Attribute msg
smFocusPlaceholderGreen700Focus =
    A.class "sm:focus:placeholder-green-700:focus"


smFocusPlaceholderGreen800Focus : Html.Attribute msg
smFocusPlaceholderGreen800Focus =
    A.class "sm:focus:placeholder-green-800:focus"


smFocusPlaceholderGreen900Focus : Html.Attribute msg
smFocusPlaceholderGreen900Focus =
    A.class "sm:focus:placeholder-green-900:focus"


smFocusPlaceholderTeal100Focus : Html.Attribute msg
smFocusPlaceholderTeal100Focus =
    A.class "sm:focus:placeholder-teal-100:focus"


smFocusPlaceholderTeal200Focus : Html.Attribute msg
smFocusPlaceholderTeal200Focus =
    A.class "sm:focus:placeholder-teal-200:focus"


smFocusPlaceholderTeal300Focus : Html.Attribute msg
smFocusPlaceholderTeal300Focus =
    A.class "sm:focus:placeholder-teal-300:focus"


smFocusPlaceholderTeal400Focus : Html.Attribute msg
smFocusPlaceholderTeal400Focus =
    A.class "sm:focus:placeholder-teal-400:focus"


smFocusPlaceholderTeal500Focus : Html.Attribute msg
smFocusPlaceholderTeal500Focus =
    A.class "sm:focus:placeholder-teal-500:focus"


smFocusPlaceholderTeal600Focus : Html.Attribute msg
smFocusPlaceholderTeal600Focus =
    A.class "sm:focus:placeholder-teal-600:focus"


smFocusPlaceholderTeal700Focus : Html.Attribute msg
smFocusPlaceholderTeal700Focus =
    A.class "sm:focus:placeholder-teal-700:focus"


smFocusPlaceholderTeal800Focus : Html.Attribute msg
smFocusPlaceholderTeal800Focus =
    A.class "sm:focus:placeholder-teal-800:focus"


smFocusPlaceholderTeal900Focus : Html.Attribute msg
smFocusPlaceholderTeal900Focus =
    A.class "sm:focus:placeholder-teal-900:focus"


smFocusPlaceholderBlue100Focus : Html.Attribute msg
smFocusPlaceholderBlue100Focus =
    A.class "sm:focus:placeholder-blue-100:focus"


smFocusPlaceholderBlue200Focus : Html.Attribute msg
smFocusPlaceholderBlue200Focus =
    A.class "sm:focus:placeholder-blue-200:focus"


smFocusPlaceholderBlue300Focus : Html.Attribute msg
smFocusPlaceholderBlue300Focus =
    A.class "sm:focus:placeholder-blue-300:focus"


smFocusPlaceholderBlue400Focus : Html.Attribute msg
smFocusPlaceholderBlue400Focus =
    A.class "sm:focus:placeholder-blue-400:focus"


smFocusPlaceholderBlue500Focus : Html.Attribute msg
smFocusPlaceholderBlue500Focus =
    A.class "sm:focus:placeholder-blue-500:focus"


smFocusPlaceholderBlue600Focus : Html.Attribute msg
smFocusPlaceholderBlue600Focus =
    A.class "sm:focus:placeholder-blue-600:focus"


smFocusPlaceholderBlue700Focus : Html.Attribute msg
smFocusPlaceholderBlue700Focus =
    A.class "sm:focus:placeholder-blue-700:focus"


smFocusPlaceholderBlue800Focus : Html.Attribute msg
smFocusPlaceholderBlue800Focus =
    A.class "sm:focus:placeholder-blue-800:focus"


smFocusPlaceholderBlue900Focus : Html.Attribute msg
smFocusPlaceholderBlue900Focus =
    A.class "sm:focus:placeholder-blue-900:focus"


smFocusPlaceholderIndigo100Focus : Html.Attribute msg
smFocusPlaceholderIndigo100Focus =
    A.class "sm:focus:placeholder-indigo-100:focus"


smFocusPlaceholderIndigo200Focus : Html.Attribute msg
smFocusPlaceholderIndigo200Focus =
    A.class "sm:focus:placeholder-indigo-200:focus"


smFocusPlaceholderIndigo300Focus : Html.Attribute msg
smFocusPlaceholderIndigo300Focus =
    A.class "sm:focus:placeholder-indigo-300:focus"


smFocusPlaceholderIndigo400Focus : Html.Attribute msg
smFocusPlaceholderIndigo400Focus =
    A.class "sm:focus:placeholder-indigo-400:focus"


smFocusPlaceholderIndigo500Focus : Html.Attribute msg
smFocusPlaceholderIndigo500Focus =
    A.class "sm:focus:placeholder-indigo-500:focus"


smFocusPlaceholderIndigo600Focus : Html.Attribute msg
smFocusPlaceholderIndigo600Focus =
    A.class "sm:focus:placeholder-indigo-600:focus"


smFocusPlaceholderIndigo700Focus : Html.Attribute msg
smFocusPlaceholderIndigo700Focus =
    A.class "sm:focus:placeholder-indigo-700:focus"


smFocusPlaceholderIndigo800Focus : Html.Attribute msg
smFocusPlaceholderIndigo800Focus =
    A.class "sm:focus:placeholder-indigo-800:focus"


smFocusPlaceholderIndigo900Focus : Html.Attribute msg
smFocusPlaceholderIndigo900Focus =
    A.class "sm:focus:placeholder-indigo-900:focus"


smFocusPlaceholderPurple100Focus : Html.Attribute msg
smFocusPlaceholderPurple100Focus =
    A.class "sm:focus:placeholder-purple-100:focus"


smFocusPlaceholderPurple200Focus : Html.Attribute msg
smFocusPlaceholderPurple200Focus =
    A.class "sm:focus:placeholder-purple-200:focus"


smFocusPlaceholderPurple300Focus : Html.Attribute msg
smFocusPlaceholderPurple300Focus =
    A.class "sm:focus:placeholder-purple-300:focus"


smFocusPlaceholderPurple400Focus : Html.Attribute msg
smFocusPlaceholderPurple400Focus =
    A.class "sm:focus:placeholder-purple-400:focus"


smFocusPlaceholderPurple500Focus : Html.Attribute msg
smFocusPlaceholderPurple500Focus =
    A.class "sm:focus:placeholder-purple-500:focus"


smFocusPlaceholderPurple600Focus : Html.Attribute msg
smFocusPlaceholderPurple600Focus =
    A.class "sm:focus:placeholder-purple-600:focus"


smFocusPlaceholderPurple700Focus : Html.Attribute msg
smFocusPlaceholderPurple700Focus =
    A.class "sm:focus:placeholder-purple-700:focus"


smFocusPlaceholderPurple800Focus : Html.Attribute msg
smFocusPlaceholderPurple800Focus =
    A.class "sm:focus:placeholder-purple-800:focus"


smFocusPlaceholderPurple900Focus : Html.Attribute msg
smFocusPlaceholderPurple900Focus =
    A.class "sm:focus:placeholder-purple-900:focus"


smFocusPlaceholderPink100Focus : Html.Attribute msg
smFocusPlaceholderPink100Focus =
    A.class "sm:focus:placeholder-pink-100:focus"


smFocusPlaceholderPink200Focus : Html.Attribute msg
smFocusPlaceholderPink200Focus =
    A.class "sm:focus:placeholder-pink-200:focus"


smFocusPlaceholderPink300Focus : Html.Attribute msg
smFocusPlaceholderPink300Focus =
    A.class "sm:focus:placeholder-pink-300:focus"


smFocusPlaceholderPink400Focus : Html.Attribute msg
smFocusPlaceholderPink400Focus =
    A.class "sm:focus:placeholder-pink-400:focus"


smFocusPlaceholderPink500Focus : Html.Attribute msg
smFocusPlaceholderPink500Focus =
    A.class "sm:focus:placeholder-pink-500:focus"


smFocusPlaceholderPink600Focus : Html.Attribute msg
smFocusPlaceholderPink600Focus =
    A.class "sm:focus:placeholder-pink-600:focus"


smFocusPlaceholderPink700Focus : Html.Attribute msg
smFocusPlaceholderPink700Focus =
    A.class "sm:focus:placeholder-pink-700:focus"


smFocusPlaceholderPink800Focus : Html.Attribute msg
smFocusPlaceholderPink800Focus =
    A.class "sm:focus:placeholder-pink-800:focus"


smFocusPlaceholderPink900Focus : Html.Attribute msg
smFocusPlaceholderPink900Focus =
    A.class "sm:focus:placeholder-pink-900:focus"


smPlaceholderOpacity0 : Html.Attribute msg
smPlaceholderOpacity0 =
    A.class "sm:placeholder-opacity-0"


smPlaceholderOpacity25 : Html.Attribute msg
smPlaceholderOpacity25 =
    A.class "sm:placeholder-opacity-25"


smPlaceholderOpacity50 : Html.Attribute msg
smPlaceholderOpacity50 =
    A.class "sm:placeholder-opacity-50"


smPlaceholderOpacity75 : Html.Attribute msg
smPlaceholderOpacity75 =
    A.class "sm:placeholder-opacity-75"


smPlaceholderOpacity100 : Html.Attribute msg
smPlaceholderOpacity100 =
    A.class "sm:placeholder-opacity-100"


smFocusPlaceholderOpacity0Focus : Html.Attribute msg
smFocusPlaceholderOpacity0Focus =
    A.class "sm:focus:placeholder-opacity-0:focus"


smFocusPlaceholderOpacity25Focus : Html.Attribute msg
smFocusPlaceholderOpacity25Focus =
    A.class "sm:focus:placeholder-opacity-25:focus"


smFocusPlaceholderOpacity50Focus : Html.Attribute msg
smFocusPlaceholderOpacity50Focus =
    A.class "sm:focus:placeholder-opacity-50:focus"


smFocusPlaceholderOpacity75Focus : Html.Attribute msg
smFocusPlaceholderOpacity75Focus =
    A.class "sm:focus:placeholder-opacity-75:focus"


smFocusPlaceholderOpacity100Focus : Html.Attribute msg
smFocusPlaceholderOpacity100Focus =
    A.class "sm:focus:placeholder-opacity-100:focus"


smPointerEventsNone : Html.Attribute msg
smPointerEventsNone =
    A.class "sm:pointer-events-none"


smPointerEventsAuto : Html.Attribute msg
smPointerEventsAuto =
    A.class "sm:pointer-events-auto"


smStatic : Html.Attribute msg
smStatic =
    A.class "sm:static"


smFixed : Html.Attribute msg
smFixed =
    A.class "sm:fixed"


smAbsolute : Html.Attribute msg
smAbsolute =
    A.class "sm:absolute"


smRelative : Html.Attribute msg
smRelative =
    A.class "sm:relative"


smSticky : Html.Attribute msg
smSticky =
    A.class "sm:sticky"


smInset0 : Html.Attribute msg
smInset0 =
    A.class "sm:inset-0"


smInsetAuto : Html.Attribute msg
smInsetAuto =
    A.class "sm:inset-auto"


smInsetY0 : Html.Attribute msg
smInsetY0 =
    A.class "sm:inset-y-0"


smInsetX0 : Html.Attribute msg
smInsetX0 =
    A.class "sm:inset-x-0"


smInsetYAuto : Html.Attribute msg
smInsetYAuto =
    A.class "sm:inset-y-auto"


smInsetXAuto : Html.Attribute msg
smInsetXAuto =
    A.class "sm:inset-x-auto"


smTop0 : Html.Attribute msg
smTop0 =
    A.class "sm:top-0"


smRight0 : Html.Attribute msg
smRight0 =
    A.class "sm:right-0"


smBottom0 : Html.Attribute msg
smBottom0 =
    A.class "sm:bottom-0"


smLeft0 : Html.Attribute msg
smLeft0 =
    A.class "sm:left-0"


smTopAuto : Html.Attribute msg
smTopAuto =
    A.class "sm:top-auto"


smRightAuto : Html.Attribute msg
smRightAuto =
    A.class "sm:right-auto"


smBottomAuto : Html.Attribute msg
smBottomAuto =
    A.class "sm:bottom-auto"


smLeftAuto : Html.Attribute msg
smLeftAuto =
    A.class "sm:left-auto"


smResizeNone : Html.Attribute msg
smResizeNone =
    A.class "sm:resize-none"


smResizeY : Html.Attribute msg
smResizeY =
    A.class "sm:resize-y"


smResizeX : Html.Attribute msg
smResizeX =
    A.class "sm:resize-x"


smResize : Html.Attribute msg
smResize =
    A.class "sm:resize"


smShadowXs : Html.Attribute msg
smShadowXs =
    A.class "sm:shadow-xs"


smShadowSm : Html.Attribute msg
smShadowSm =
    A.class "sm:shadow-sm"


smShadow : Html.Attribute msg
smShadow =
    A.class "sm:shadow"


smShadowMd : Html.Attribute msg
smShadowMd =
    A.class "sm:shadow-md"


smShadowLg : Html.Attribute msg
smShadowLg =
    A.class "sm:shadow-lg"


smShadowXl : Html.Attribute msg
smShadowXl =
    A.class "sm:shadow-xl"


smShadow2xl : Html.Attribute msg
smShadow2xl =
    A.class "sm:shadow-2xl"


smShadowInner : Html.Attribute msg
smShadowInner =
    A.class "sm:shadow-inner"


smShadowOutline : Html.Attribute msg
smShadowOutline =
    A.class "sm:shadow-outline"


smShadowNone : Html.Attribute msg
smShadowNone =
    A.class "sm:shadow-none"


smHoverShadowXs : Html.Attribute msg
smHoverShadowXs =
    A.class "sm:hover:shadow-xs"


smHoverShadowSm : Html.Attribute msg
smHoverShadowSm =
    A.class "sm:hover:shadow-sm"


smHoverShadow : Html.Attribute msg
smHoverShadow =
    A.class "sm:hover:shadow"


smHoverShadowMd : Html.Attribute msg
smHoverShadowMd =
    A.class "sm:hover:shadow-md"


smHoverShadowLg : Html.Attribute msg
smHoverShadowLg =
    A.class "sm:hover:shadow-lg"


smHoverShadowXl : Html.Attribute msg
smHoverShadowXl =
    A.class "sm:hover:shadow-xl"


smHoverShadow2xl : Html.Attribute msg
smHoverShadow2xl =
    A.class "sm:hover:shadow-2xl"


smHoverShadowInner : Html.Attribute msg
smHoverShadowInner =
    A.class "sm:hover:shadow-inner"


smHoverShadowOutline : Html.Attribute msg
smHoverShadowOutline =
    A.class "sm:hover:shadow-outline"


smHoverShadowNone : Html.Attribute msg
smHoverShadowNone =
    A.class "sm:hover:shadow-none"


smFocusShadowXs : Html.Attribute msg
smFocusShadowXs =
    A.class "sm:focus:shadow-xs"


smFocusShadowSm : Html.Attribute msg
smFocusShadowSm =
    A.class "sm:focus:shadow-sm"


smFocusShadow : Html.Attribute msg
smFocusShadow =
    A.class "sm:focus:shadow"


smFocusShadowMd : Html.Attribute msg
smFocusShadowMd =
    A.class "sm:focus:shadow-md"


smFocusShadowLg : Html.Attribute msg
smFocusShadowLg =
    A.class "sm:focus:shadow-lg"


smFocusShadowXl : Html.Attribute msg
smFocusShadowXl =
    A.class "sm:focus:shadow-xl"


smFocusShadow2xl : Html.Attribute msg
smFocusShadow2xl =
    A.class "sm:focus:shadow-2xl"


smFocusShadowInner : Html.Attribute msg
smFocusShadowInner =
    A.class "sm:focus:shadow-inner"


smFocusShadowOutline : Html.Attribute msg
smFocusShadowOutline =
    A.class "sm:focus:shadow-outline"


smFocusShadowNone : Html.Attribute msg
smFocusShadowNone =
    A.class "sm:focus:shadow-none"


smFillCurrent : Html.Attribute msg
smFillCurrent =
    A.class "sm:fill-current"


smStrokeCurrent : Html.Attribute msg
smStrokeCurrent =
    A.class "sm:stroke-current"


smStroke0 : Html.Attribute msg
smStroke0 =
    A.class "sm:stroke-0"


smStroke1 : Html.Attribute msg
smStroke1 =
    A.class "sm:stroke-1"


smStroke2 : Html.Attribute msg
smStroke2 =
    A.class "sm:stroke-2"


smTableAuto : Html.Attribute msg
smTableAuto =
    A.class "sm:table-auto"


smTableFixed : Html.Attribute msg
smTableFixed =
    A.class "sm:table-fixed"


smTextLeft : Html.Attribute msg
smTextLeft =
    A.class "sm:text-left"


smTextCenter : Html.Attribute msg
smTextCenter =
    A.class "sm:text-center"


smTextRight : Html.Attribute msg
smTextRight =
    A.class "sm:text-right"


smTextJustify : Html.Attribute msg
smTextJustify =
    A.class "sm:text-justify"


smTextTransparent : Html.Attribute msg
smTextTransparent =
    A.class "sm:text-transparent"


smTextCurrent : Html.Attribute msg
smTextCurrent =
    A.class "sm:text-current"


smTextBlack : Html.Attribute msg
smTextBlack =
    A.class "sm:text-black"


smTextWhite : Html.Attribute msg
smTextWhite =
    A.class "sm:text-white"


smTextGray100 : Html.Attribute msg
smTextGray100 =
    A.class "sm:text-gray-100"


smTextGray200 : Html.Attribute msg
smTextGray200 =
    A.class "sm:text-gray-200"


smTextGray300 : Html.Attribute msg
smTextGray300 =
    A.class "sm:text-gray-300"


smTextGray400 : Html.Attribute msg
smTextGray400 =
    A.class "sm:text-gray-400"


smTextGray500 : Html.Attribute msg
smTextGray500 =
    A.class "sm:text-gray-500"


smTextGray600 : Html.Attribute msg
smTextGray600 =
    A.class "sm:text-gray-600"


smTextGray700 : Html.Attribute msg
smTextGray700 =
    A.class "sm:text-gray-700"


smTextGray800 : Html.Attribute msg
smTextGray800 =
    A.class "sm:text-gray-800"


smTextGray900 : Html.Attribute msg
smTextGray900 =
    A.class "sm:text-gray-900"


smTextRed100 : Html.Attribute msg
smTextRed100 =
    A.class "sm:text-red-100"


smTextRed200 : Html.Attribute msg
smTextRed200 =
    A.class "sm:text-red-200"


smTextRed300 : Html.Attribute msg
smTextRed300 =
    A.class "sm:text-red-300"


smTextRed400 : Html.Attribute msg
smTextRed400 =
    A.class "sm:text-red-400"


smTextRed500 : Html.Attribute msg
smTextRed500 =
    A.class "sm:text-red-500"


smTextRed600 : Html.Attribute msg
smTextRed600 =
    A.class "sm:text-red-600"


smTextRed700 : Html.Attribute msg
smTextRed700 =
    A.class "sm:text-red-700"


smTextRed800 : Html.Attribute msg
smTextRed800 =
    A.class "sm:text-red-800"


smTextRed900 : Html.Attribute msg
smTextRed900 =
    A.class "sm:text-red-900"


smTextOrange100 : Html.Attribute msg
smTextOrange100 =
    A.class "sm:text-orange-100"


smTextOrange200 : Html.Attribute msg
smTextOrange200 =
    A.class "sm:text-orange-200"


smTextOrange300 : Html.Attribute msg
smTextOrange300 =
    A.class "sm:text-orange-300"


smTextOrange400 : Html.Attribute msg
smTextOrange400 =
    A.class "sm:text-orange-400"


smTextOrange500 : Html.Attribute msg
smTextOrange500 =
    A.class "sm:text-orange-500"


smTextOrange600 : Html.Attribute msg
smTextOrange600 =
    A.class "sm:text-orange-600"


smTextOrange700 : Html.Attribute msg
smTextOrange700 =
    A.class "sm:text-orange-700"


smTextOrange800 : Html.Attribute msg
smTextOrange800 =
    A.class "sm:text-orange-800"


smTextOrange900 : Html.Attribute msg
smTextOrange900 =
    A.class "sm:text-orange-900"


smTextYellow100 : Html.Attribute msg
smTextYellow100 =
    A.class "sm:text-yellow-100"


smTextYellow200 : Html.Attribute msg
smTextYellow200 =
    A.class "sm:text-yellow-200"


smTextYellow300 : Html.Attribute msg
smTextYellow300 =
    A.class "sm:text-yellow-300"


smTextYellow400 : Html.Attribute msg
smTextYellow400 =
    A.class "sm:text-yellow-400"


smTextYellow500 : Html.Attribute msg
smTextYellow500 =
    A.class "sm:text-yellow-500"


smTextYellow600 : Html.Attribute msg
smTextYellow600 =
    A.class "sm:text-yellow-600"


smTextYellow700 : Html.Attribute msg
smTextYellow700 =
    A.class "sm:text-yellow-700"


smTextYellow800 : Html.Attribute msg
smTextYellow800 =
    A.class "sm:text-yellow-800"


smTextYellow900 : Html.Attribute msg
smTextYellow900 =
    A.class "sm:text-yellow-900"


smTextGreen100 : Html.Attribute msg
smTextGreen100 =
    A.class "sm:text-green-100"


smTextGreen200 : Html.Attribute msg
smTextGreen200 =
    A.class "sm:text-green-200"


smTextGreen300 : Html.Attribute msg
smTextGreen300 =
    A.class "sm:text-green-300"


smTextGreen400 : Html.Attribute msg
smTextGreen400 =
    A.class "sm:text-green-400"


smTextGreen500 : Html.Attribute msg
smTextGreen500 =
    A.class "sm:text-green-500"


smTextGreen600 : Html.Attribute msg
smTextGreen600 =
    A.class "sm:text-green-600"


smTextGreen700 : Html.Attribute msg
smTextGreen700 =
    A.class "sm:text-green-700"


smTextGreen800 : Html.Attribute msg
smTextGreen800 =
    A.class "sm:text-green-800"


smTextGreen900 : Html.Attribute msg
smTextGreen900 =
    A.class "sm:text-green-900"


smTextTeal100 : Html.Attribute msg
smTextTeal100 =
    A.class "sm:text-teal-100"


smTextTeal200 : Html.Attribute msg
smTextTeal200 =
    A.class "sm:text-teal-200"


smTextTeal300 : Html.Attribute msg
smTextTeal300 =
    A.class "sm:text-teal-300"


smTextTeal400 : Html.Attribute msg
smTextTeal400 =
    A.class "sm:text-teal-400"


smTextTeal500 : Html.Attribute msg
smTextTeal500 =
    A.class "sm:text-teal-500"


smTextTeal600 : Html.Attribute msg
smTextTeal600 =
    A.class "sm:text-teal-600"


smTextTeal700 : Html.Attribute msg
smTextTeal700 =
    A.class "sm:text-teal-700"


smTextTeal800 : Html.Attribute msg
smTextTeal800 =
    A.class "sm:text-teal-800"


smTextTeal900 : Html.Attribute msg
smTextTeal900 =
    A.class "sm:text-teal-900"


smTextBlue100 : Html.Attribute msg
smTextBlue100 =
    A.class "sm:text-blue-100"


smTextBlue200 : Html.Attribute msg
smTextBlue200 =
    A.class "sm:text-blue-200"


smTextBlue300 : Html.Attribute msg
smTextBlue300 =
    A.class "sm:text-blue-300"


smTextBlue400 : Html.Attribute msg
smTextBlue400 =
    A.class "sm:text-blue-400"


smTextBlue500 : Html.Attribute msg
smTextBlue500 =
    A.class "sm:text-blue-500"


smTextBlue600 : Html.Attribute msg
smTextBlue600 =
    A.class "sm:text-blue-600"


smTextBlue700 : Html.Attribute msg
smTextBlue700 =
    A.class "sm:text-blue-700"


smTextBlue800 : Html.Attribute msg
smTextBlue800 =
    A.class "sm:text-blue-800"


smTextBlue900 : Html.Attribute msg
smTextBlue900 =
    A.class "sm:text-blue-900"


smTextIndigo100 : Html.Attribute msg
smTextIndigo100 =
    A.class "sm:text-indigo-100"


smTextIndigo200 : Html.Attribute msg
smTextIndigo200 =
    A.class "sm:text-indigo-200"


smTextIndigo300 : Html.Attribute msg
smTextIndigo300 =
    A.class "sm:text-indigo-300"


smTextIndigo400 : Html.Attribute msg
smTextIndigo400 =
    A.class "sm:text-indigo-400"


smTextIndigo500 : Html.Attribute msg
smTextIndigo500 =
    A.class "sm:text-indigo-500"


smTextIndigo600 : Html.Attribute msg
smTextIndigo600 =
    A.class "sm:text-indigo-600"


smTextIndigo700 : Html.Attribute msg
smTextIndigo700 =
    A.class "sm:text-indigo-700"


smTextIndigo800 : Html.Attribute msg
smTextIndigo800 =
    A.class "sm:text-indigo-800"


smTextIndigo900 : Html.Attribute msg
smTextIndigo900 =
    A.class "sm:text-indigo-900"


smTextPurple100 : Html.Attribute msg
smTextPurple100 =
    A.class "sm:text-purple-100"


smTextPurple200 : Html.Attribute msg
smTextPurple200 =
    A.class "sm:text-purple-200"


smTextPurple300 : Html.Attribute msg
smTextPurple300 =
    A.class "sm:text-purple-300"


smTextPurple400 : Html.Attribute msg
smTextPurple400 =
    A.class "sm:text-purple-400"


smTextPurple500 : Html.Attribute msg
smTextPurple500 =
    A.class "sm:text-purple-500"


smTextPurple600 : Html.Attribute msg
smTextPurple600 =
    A.class "sm:text-purple-600"


smTextPurple700 : Html.Attribute msg
smTextPurple700 =
    A.class "sm:text-purple-700"


smTextPurple800 : Html.Attribute msg
smTextPurple800 =
    A.class "sm:text-purple-800"


smTextPurple900 : Html.Attribute msg
smTextPurple900 =
    A.class "sm:text-purple-900"


smTextPink100 : Html.Attribute msg
smTextPink100 =
    A.class "sm:text-pink-100"


smTextPink200 : Html.Attribute msg
smTextPink200 =
    A.class "sm:text-pink-200"


smTextPink300 : Html.Attribute msg
smTextPink300 =
    A.class "sm:text-pink-300"


smTextPink400 : Html.Attribute msg
smTextPink400 =
    A.class "sm:text-pink-400"


smTextPink500 : Html.Attribute msg
smTextPink500 =
    A.class "sm:text-pink-500"


smTextPink600 : Html.Attribute msg
smTextPink600 =
    A.class "sm:text-pink-600"


smTextPink700 : Html.Attribute msg
smTextPink700 =
    A.class "sm:text-pink-700"


smTextPink800 : Html.Attribute msg
smTextPink800 =
    A.class "sm:text-pink-800"


smTextPink900 : Html.Attribute msg
smTextPink900 =
    A.class "sm:text-pink-900"


smHoverTextTransparent : Html.Attribute msg
smHoverTextTransparent =
    A.class "sm:hover:text-transparent"


smHoverTextCurrent : Html.Attribute msg
smHoverTextCurrent =
    A.class "sm:hover:text-current"


smHoverTextBlack : Html.Attribute msg
smHoverTextBlack =
    A.class "sm:hover:text-black"


smHoverTextWhite : Html.Attribute msg
smHoverTextWhite =
    A.class "sm:hover:text-white"


smHoverTextGray100 : Html.Attribute msg
smHoverTextGray100 =
    A.class "sm:hover:text-gray-100"


smHoverTextGray200 : Html.Attribute msg
smHoverTextGray200 =
    A.class "sm:hover:text-gray-200"


smHoverTextGray300 : Html.Attribute msg
smHoverTextGray300 =
    A.class "sm:hover:text-gray-300"


smHoverTextGray400 : Html.Attribute msg
smHoverTextGray400 =
    A.class "sm:hover:text-gray-400"


smHoverTextGray500 : Html.Attribute msg
smHoverTextGray500 =
    A.class "sm:hover:text-gray-500"


smHoverTextGray600 : Html.Attribute msg
smHoverTextGray600 =
    A.class "sm:hover:text-gray-600"


smHoverTextGray700 : Html.Attribute msg
smHoverTextGray700 =
    A.class "sm:hover:text-gray-700"


smHoverTextGray800 : Html.Attribute msg
smHoverTextGray800 =
    A.class "sm:hover:text-gray-800"


smHoverTextGray900 : Html.Attribute msg
smHoverTextGray900 =
    A.class "sm:hover:text-gray-900"


smHoverTextRed100 : Html.Attribute msg
smHoverTextRed100 =
    A.class "sm:hover:text-red-100"


smHoverTextRed200 : Html.Attribute msg
smHoverTextRed200 =
    A.class "sm:hover:text-red-200"


smHoverTextRed300 : Html.Attribute msg
smHoverTextRed300 =
    A.class "sm:hover:text-red-300"


smHoverTextRed400 : Html.Attribute msg
smHoverTextRed400 =
    A.class "sm:hover:text-red-400"


smHoverTextRed500 : Html.Attribute msg
smHoverTextRed500 =
    A.class "sm:hover:text-red-500"


smHoverTextRed600 : Html.Attribute msg
smHoverTextRed600 =
    A.class "sm:hover:text-red-600"


smHoverTextRed700 : Html.Attribute msg
smHoverTextRed700 =
    A.class "sm:hover:text-red-700"


smHoverTextRed800 : Html.Attribute msg
smHoverTextRed800 =
    A.class "sm:hover:text-red-800"


smHoverTextRed900 : Html.Attribute msg
smHoverTextRed900 =
    A.class "sm:hover:text-red-900"


smHoverTextOrange100 : Html.Attribute msg
smHoverTextOrange100 =
    A.class "sm:hover:text-orange-100"


smHoverTextOrange200 : Html.Attribute msg
smHoverTextOrange200 =
    A.class "sm:hover:text-orange-200"


smHoverTextOrange300 : Html.Attribute msg
smHoverTextOrange300 =
    A.class "sm:hover:text-orange-300"


smHoverTextOrange400 : Html.Attribute msg
smHoverTextOrange400 =
    A.class "sm:hover:text-orange-400"


smHoverTextOrange500 : Html.Attribute msg
smHoverTextOrange500 =
    A.class "sm:hover:text-orange-500"


smHoverTextOrange600 : Html.Attribute msg
smHoverTextOrange600 =
    A.class "sm:hover:text-orange-600"


smHoverTextOrange700 : Html.Attribute msg
smHoverTextOrange700 =
    A.class "sm:hover:text-orange-700"


smHoverTextOrange800 : Html.Attribute msg
smHoverTextOrange800 =
    A.class "sm:hover:text-orange-800"


smHoverTextOrange900 : Html.Attribute msg
smHoverTextOrange900 =
    A.class "sm:hover:text-orange-900"


smHoverTextYellow100 : Html.Attribute msg
smHoverTextYellow100 =
    A.class "sm:hover:text-yellow-100"


smHoverTextYellow200 : Html.Attribute msg
smHoverTextYellow200 =
    A.class "sm:hover:text-yellow-200"


smHoverTextYellow300 : Html.Attribute msg
smHoverTextYellow300 =
    A.class "sm:hover:text-yellow-300"


smHoverTextYellow400 : Html.Attribute msg
smHoverTextYellow400 =
    A.class "sm:hover:text-yellow-400"


smHoverTextYellow500 : Html.Attribute msg
smHoverTextYellow500 =
    A.class "sm:hover:text-yellow-500"


smHoverTextYellow600 : Html.Attribute msg
smHoverTextYellow600 =
    A.class "sm:hover:text-yellow-600"


smHoverTextYellow700 : Html.Attribute msg
smHoverTextYellow700 =
    A.class "sm:hover:text-yellow-700"


smHoverTextYellow800 : Html.Attribute msg
smHoverTextYellow800 =
    A.class "sm:hover:text-yellow-800"


smHoverTextYellow900 : Html.Attribute msg
smHoverTextYellow900 =
    A.class "sm:hover:text-yellow-900"


smHoverTextGreen100 : Html.Attribute msg
smHoverTextGreen100 =
    A.class "sm:hover:text-green-100"


smHoverTextGreen200 : Html.Attribute msg
smHoverTextGreen200 =
    A.class "sm:hover:text-green-200"


smHoverTextGreen300 : Html.Attribute msg
smHoverTextGreen300 =
    A.class "sm:hover:text-green-300"


smHoverTextGreen400 : Html.Attribute msg
smHoverTextGreen400 =
    A.class "sm:hover:text-green-400"


smHoverTextGreen500 : Html.Attribute msg
smHoverTextGreen500 =
    A.class "sm:hover:text-green-500"


smHoverTextGreen600 : Html.Attribute msg
smHoverTextGreen600 =
    A.class "sm:hover:text-green-600"


smHoverTextGreen700 : Html.Attribute msg
smHoverTextGreen700 =
    A.class "sm:hover:text-green-700"


smHoverTextGreen800 : Html.Attribute msg
smHoverTextGreen800 =
    A.class "sm:hover:text-green-800"


smHoverTextGreen900 : Html.Attribute msg
smHoverTextGreen900 =
    A.class "sm:hover:text-green-900"


smHoverTextTeal100 : Html.Attribute msg
smHoverTextTeal100 =
    A.class "sm:hover:text-teal-100"


smHoverTextTeal200 : Html.Attribute msg
smHoverTextTeal200 =
    A.class "sm:hover:text-teal-200"


smHoverTextTeal300 : Html.Attribute msg
smHoverTextTeal300 =
    A.class "sm:hover:text-teal-300"


smHoverTextTeal400 : Html.Attribute msg
smHoverTextTeal400 =
    A.class "sm:hover:text-teal-400"


smHoverTextTeal500 : Html.Attribute msg
smHoverTextTeal500 =
    A.class "sm:hover:text-teal-500"


smHoverTextTeal600 : Html.Attribute msg
smHoverTextTeal600 =
    A.class "sm:hover:text-teal-600"


smHoverTextTeal700 : Html.Attribute msg
smHoverTextTeal700 =
    A.class "sm:hover:text-teal-700"


smHoverTextTeal800 : Html.Attribute msg
smHoverTextTeal800 =
    A.class "sm:hover:text-teal-800"


smHoverTextTeal900 : Html.Attribute msg
smHoverTextTeal900 =
    A.class "sm:hover:text-teal-900"


smHoverTextBlue100 : Html.Attribute msg
smHoverTextBlue100 =
    A.class "sm:hover:text-blue-100"


smHoverTextBlue200 : Html.Attribute msg
smHoverTextBlue200 =
    A.class "sm:hover:text-blue-200"


smHoverTextBlue300 : Html.Attribute msg
smHoverTextBlue300 =
    A.class "sm:hover:text-blue-300"


smHoverTextBlue400 : Html.Attribute msg
smHoverTextBlue400 =
    A.class "sm:hover:text-blue-400"


smHoverTextBlue500 : Html.Attribute msg
smHoverTextBlue500 =
    A.class "sm:hover:text-blue-500"


smHoverTextBlue600 : Html.Attribute msg
smHoverTextBlue600 =
    A.class "sm:hover:text-blue-600"


smHoverTextBlue700 : Html.Attribute msg
smHoverTextBlue700 =
    A.class "sm:hover:text-blue-700"


smHoverTextBlue800 : Html.Attribute msg
smHoverTextBlue800 =
    A.class "sm:hover:text-blue-800"


smHoverTextBlue900 : Html.Attribute msg
smHoverTextBlue900 =
    A.class "sm:hover:text-blue-900"


smHoverTextIndigo100 : Html.Attribute msg
smHoverTextIndigo100 =
    A.class "sm:hover:text-indigo-100"


smHoverTextIndigo200 : Html.Attribute msg
smHoverTextIndigo200 =
    A.class "sm:hover:text-indigo-200"


smHoverTextIndigo300 : Html.Attribute msg
smHoverTextIndigo300 =
    A.class "sm:hover:text-indigo-300"


smHoverTextIndigo400 : Html.Attribute msg
smHoverTextIndigo400 =
    A.class "sm:hover:text-indigo-400"


smHoverTextIndigo500 : Html.Attribute msg
smHoverTextIndigo500 =
    A.class "sm:hover:text-indigo-500"


smHoverTextIndigo600 : Html.Attribute msg
smHoverTextIndigo600 =
    A.class "sm:hover:text-indigo-600"


smHoverTextIndigo700 : Html.Attribute msg
smHoverTextIndigo700 =
    A.class "sm:hover:text-indigo-700"


smHoverTextIndigo800 : Html.Attribute msg
smHoverTextIndigo800 =
    A.class "sm:hover:text-indigo-800"


smHoverTextIndigo900 : Html.Attribute msg
smHoverTextIndigo900 =
    A.class "sm:hover:text-indigo-900"


smHoverTextPurple100 : Html.Attribute msg
smHoverTextPurple100 =
    A.class "sm:hover:text-purple-100"


smHoverTextPurple200 : Html.Attribute msg
smHoverTextPurple200 =
    A.class "sm:hover:text-purple-200"


smHoverTextPurple300 : Html.Attribute msg
smHoverTextPurple300 =
    A.class "sm:hover:text-purple-300"


smHoverTextPurple400 : Html.Attribute msg
smHoverTextPurple400 =
    A.class "sm:hover:text-purple-400"


smHoverTextPurple500 : Html.Attribute msg
smHoverTextPurple500 =
    A.class "sm:hover:text-purple-500"


smHoverTextPurple600 : Html.Attribute msg
smHoverTextPurple600 =
    A.class "sm:hover:text-purple-600"


smHoverTextPurple700 : Html.Attribute msg
smHoverTextPurple700 =
    A.class "sm:hover:text-purple-700"


smHoverTextPurple800 : Html.Attribute msg
smHoverTextPurple800 =
    A.class "sm:hover:text-purple-800"


smHoverTextPurple900 : Html.Attribute msg
smHoverTextPurple900 =
    A.class "sm:hover:text-purple-900"


smHoverTextPink100 : Html.Attribute msg
smHoverTextPink100 =
    A.class "sm:hover:text-pink-100"


smHoverTextPink200 : Html.Attribute msg
smHoverTextPink200 =
    A.class "sm:hover:text-pink-200"


smHoverTextPink300 : Html.Attribute msg
smHoverTextPink300 =
    A.class "sm:hover:text-pink-300"


smHoverTextPink400 : Html.Attribute msg
smHoverTextPink400 =
    A.class "sm:hover:text-pink-400"


smHoverTextPink500 : Html.Attribute msg
smHoverTextPink500 =
    A.class "sm:hover:text-pink-500"


smHoverTextPink600 : Html.Attribute msg
smHoverTextPink600 =
    A.class "sm:hover:text-pink-600"


smHoverTextPink700 : Html.Attribute msg
smHoverTextPink700 =
    A.class "sm:hover:text-pink-700"


smHoverTextPink800 : Html.Attribute msg
smHoverTextPink800 =
    A.class "sm:hover:text-pink-800"


smHoverTextPink900 : Html.Attribute msg
smHoverTextPink900 =
    A.class "sm:hover:text-pink-900"


smFocusTextTransparent : Html.Attribute msg
smFocusTextTransparent =
    A.class "sm:focus:text-transparent"


smFocusTextCurrent : Html.Attribute msg
smFocusTextCurrent =
    A.class "sm:focus:text-current"


smFocusTextBlack : Html.Attribute msg
smFocusTextBlack =
    A.class "sm:focus:text-black"


smFocusTextWhite : Html.Attribute msg
smFocusTextWhite =
    A.class "sm:focus:text-white"


smFocusTextGray100 : Html.Attribute msg
smFocusTextGray100 =
    A.class "sm:focus:text-gray-100"


smFocusTextGray200 : Html.Attribute msg
smFocusTextGray200 =
    A.class "sm:focus:text-gray-200"


smFocusTextGray300 : Html.Attribute msg
smFocusTextGray300 =
    A.class "sm:focus:text-gray-300"


smFocusTextGray400 : Html.Attribute msg
smFocusTextGray400 =
    A.class "sm:focus:text-gray-400"


smFocusTextGray500 : Html.Attribute msg
smFocusTextGray500 =
    A.class "sm:focus:text-gray-500"


smFocusTextGray600 : Html.Attribute msg
smFocusTextGray600 =
    A.class "sm:focus:text-gray-600"


smFocusTextGray700 : Html.Attribute msg
smFocusTextGray700 =
    A.class "sm:focus:text-gray-700"


smFocusTextGray800 : Html.Attribute msg
smFocusTextGray800 =
    A.class "sm:focus:text-gray-800"


smFocusTextGray900 : Html.Attribute msg
smFocusTextGray900 =
    A.class "sm:focus:text-gray-900"


smFocusTextRed100 : Html.Attribute msg
smFocusTextRed100 =
    A.class "sm:focus:text-red-100"


smFocusTextRed200 : Html.Attribute msg
smFocusTextRed200 =
    A.class "sm:focus:text-red-200"


smFocusTextRed300 : Html.Attribute msg
smFocusTextRed300 =
    A.class "sm:focus:text-red-300"


smFocusTextRed400 : Html.Attribute msg
smFocusTextRed400 =
    A.class "sm:focus:text-red-400"


smFocusTextRed500 : Html.Attribute msg
smFocusTextRed500 =
    A.class "sm:focus:text-red-500"


smFocusTextRed600 : Html.Attribute msg
smFocusTextRed600 =
    A.class "sm:focus:text-red-600"


smFocusTextRed700 : Html.Attribute msg
smFocusTextRed700 =
    A.class "sm:focus:text-red-700"


smFocusTextRed800 : Html.Attribute msg
smFocusTextRed800 =
    A.class "sm:focus:text-red-800"


smFocusTextRed900 : Html.Attribute msg
smFocusTextRed900 =
    A.class "sm:focus:text-red-900"


smFocusTextOrange100 : Html.Attribute msg
smFocusTextOrange100 =
    A.class "sm:focus:text-orange-100"


smFocusTextOrange200 : Html.Attribute msg
smFocusTextOrange200 =
    A.class "sm:focus:text-orange-200"


smFocusTextOrange300 : Html.Attribute msg
smFocusTextOrange300 =
    A.class "sm:focus:text-orange-300"


smFocusTextOrange400 : Html.Attribute msg
smFocusTextOrange400 =
    A.class "sm:focus:text-orange-400"


smFocusTextOrange500 : Html.Attribute msg
smFocusTextOrange500 =
    A.class "sm:focus:text-orange-500"


smFocusTextOrange600 : Html.Attribute msg
smFocusTextOrange600 =
    A.class "sm:focus:text-orange-600"


smFocusTextOrange700 : Html.Attribute msg
smFocusTextOrange700 =
    A.class "sm:focus:text-orange-700"


smFocusTextOrange800 : Html.Attribute msg
smFocusTextOrange800 =
    A.class "sm:focus:text-orange-800"


smFocusTextOrange900 : Html.Attribute msg
smFocusTextOrange900 =
    A.class "sm:focus:text-orange-900"


smFocusTextYellow100 : Html.Attribute msg
smFocusTextYellow100 =
    A.class "sm:focus:text-yellow-100"


smFocusTextYellow200 : Html.Attribute msg
smFocusTextYellow200 =
    A.class "sm:focus:text-yellow-200"


smFocusTextYellow300 : Html.Attribute msg
smFocusTextYellow300 =
    A.class "sm:focus:text-yellow-300"


smFocusTextYellow400 : Html.Attribute msg
smFocusTextYellow400 =
    A.class "sm:focus:text-yellow-400"


smFocusTextYellow500 : Html.Attribute msg
smFocusTextYellow500 =
    A.class "sm:focus:text-yellow-500"


smFocusTextYellow600 : Html.Attribute msg
smFocusTextYellow600 =
    A.class "sm:focus:text-yellow-600"


smFocusTextYellow700 : Html.Attribute msg
smFocusTextYellow700 =
    A.class "sm:focus:text-yellow-700"


smFocusTextYellow800 : Html.Attribute msg
smFocusTextYellow800 =
    A.class "sm:focus:text-yellow-800"


smFocusTextYellow900 : Html.Attribute msg
smFocusTextYellow900 =
    A.class "sm:focus:text-yellow-900"


smFocusTextGreen100 : Html.Attribute msg
smFocusTextGreen100 =
    A.class "sm:focus:text-green-100"


smFocusTextGreen200 : Html.Attribute msg
smFocusTextGreen200 =
    A.class "sm:focus:text-green-200"


smFocusTextGreen300 : Html.Attribute msg
smFocusTextGreen300 =
    A.class "sm:focus:text-green-300"


smFocusTextGreen400 : Html.Attribute msg
smFocusTextGreen400 =
    A.class "sm:focus:text-green-400"


smFocusTextGreen500 : Html.Attribute msg
smFocusTextGreen500 =
    A.class "sm:focus:text-green-500"


smFocusTextGreen600 : Html.Attribute msg
smFocusTextGreen600 =
    A.class "sm:focus:text-green-600"


smFocusTextGreen700 : Html.Attribute msg
smFocusTextGreen700 =
    A.class "sm:focus:text-green-700"


smFocusTextGreen800 : Html.Attribute msg
smFocusTextGreen800 =
    A.class "sm:focus:text-green-800"


smFocusTextGreen900 : Html.Attribute msg
smFocusTextGreen900 =
    A.class "sm:focus:text-green-900"


smFocusTextTeal100 : Html.Attribute msg
smFocusTextTeal100 =
    A.class "sm:focus:text-teal-100"


smFocusTextTeal200 : Html.Attribute msg
smFocusTextTeal200 =
    A.class "sm:focus:text-teal-200"


smFocusTextTeal300 : Html.Attribute msg
smFocusTextTeal300 =
    A.class "sm:focus:text-teal-300"


smFocusTextTeal400 : Html.Attribute msg
smFocusTextTeal400 =
    A.class "sm:focus:text-teal-400"


smFocusTextTeal500 : Html.Attribute msg
smFocusTextTeal500 =
    A.class "sm:focus:text-teal-500"


smFocusTextTeal600 : Html.Attribute msg
smFocusTextTeal600 =
    A.class "sm:focus:text-teal-600"


smFocusTextTeal700 : Html.Attribute msg
smFocusTextTeal700 =
    A.class "sm:focus:text-teal-700"


smFocusTextTeal800 : Html.Attribute msg
smFocusTextTeal800 =
    A.class "sm:focus:text-teal-800"


smFocusTextTeal900 : Html.Attribute msg
smFocusTextTeal900 =
    A.class "sm:focus:text-teal-900"


smFocusTextBlue100 : Html.Attribute msg
smFocusTextBlue100 =
    A.class "sm:focus:text-blue-100"


smFocusTextBlue200 : Html.Attribute msg
smFocusTextBlue200 =
    A.class "sm:focus:text-blue-200"


smFocusTextBlue300 : Html.Attribute msg
smFocusTextBlue300 =
    A.class "sm:focus:text-blue-300"


smFocusTextBlue400 : Html.Attribute msg
smFocusTextBlue400 =
    A.class "sm:focus:text-blue-400"


smFocusTextBlue500 : Html.Attribute msg
smFocusTextBlue500 =
    A.class "sm:focus:text-blue-500"


smFocusTextBlue600 : Html.Attribute msg
smFocusTextBlue600 =
    A.class "sm:focus:text-blue-600"


smFocusTextBlue700 : Html.Attribute msg
smFocusTextBlue700 =
    A.class "sm:focus:text-blue-700"


smFocusTextBlue800 : Html.Attribute msg
smFocusTextBlue800 =
    A.class "sm:focus:text-blue-800"


smFocusTextBlue900 : Html.Attribute msg
smFocusTextBlue900 =
    A.class "sm:focus:text-blue-900"


smFocusTextIndigo100 : Html.Attribute msg
smFocusTextIndigo100 =
    A.class "sm:focus:text-indigo-100"


smFocusTextIndigo200 : Html.Attribute msg
smFocusTextIndigo200 =
    A.class "sm:focus:text-indigo-200"


smFocusTextIndigo300 : Html.Attribute msg
smFocusTextIndigo300 =
    A.class "sm:focus:text-indigo-300"


smFocusTextIndigo400 : Html.Attribute msg
smFocusTextIndigo400 =
    A.class "sm:focus:text-indigo-400"


smFocusTextIndigo500 : Html.Attribute msg
smFocusTextIndigo500 =
    A.class "sm:focus:text-indigo-500"


smFocusTextIndigo600 : Html.Attribute msg
smFocusTextIndigo600 =
    A.class "sm:focus:text-indigo-600"


smFocusTextIndigo700 : Html.Attribute msg
smFocusTextIndigo700 =
    A.class "sm:focus:text-indigo-700"


smFocusTextIndigo800 : Html.Attribute msg
smFocusTextIndigo800 =
    A.class "sm:focus:text-indigo-800"


smFocusTextIndigo900 : Html.Attribute msg
smFocusTextIndigo900 =
    A.class "sm:focus:text-indigo-900"


smFocusTextPurple100 : Html.Attribute msg
smFocusTextPurple100 =
    A.class "sm:focus:text-purple-100"


smFocusTextPurple200 : Html.Attribute msg
smFocusTextPurple200 =
    A.class "sm:focus:text-purple-200"


smFocusTextPurple300 : Html.Attribute msg
smFocusTextPurple300 =
    A.class "sm:focus:text-purple-300"


smFocusTextPurple400 : Html.Attribute msg
smFocusTextPurple400 =
    A.class "sm:focus:text-purple-400"


smFocusTextPurple500 : Html.Attribute msg
smFocusTextPurple500 =
    A.class "sm:focus:text-purple-500"


smFocusTextPurple600 : Html.Attribute msg
smFocusTextPurple600 =
    A.class "sm:focus:text-purple-600"


smFocusTextPurple700 : Html.Attribute msg
smFocusTextPurple700 =
    A.class "sm:focus:text-purple-700"


smFocusTextPurple800 : Html.Attribute msg
smFocusTextPurple800 =
    A.class "sm:focus:text-purple-800"


smFocusTextPurple900 : Html.Attribute msg
smFocusTextPurple900 =
    A.class "sm:focus:text-purple-900"


smFocusTextPink100 : Html.Attribute msg
smFocusTextPink100 =
    A.class "sm:focus:text-pink-100"


smFocusTextPink200 : Html.Attribute msg
smFocusTextPink200 =
    A.class "sm:focus:text-pink-200"


smFocusTextPink300 : Html.Attribute msg
smFocusTextPink300 =
    A.class "sm:focus:text-pink-300"


smFocusTextPink400 : Html.Attribute msg
smFocusTextPink400 =
    A.class "sm:focus:text-pink-400"


smFocusTextPink500 : Html.Attribute msg
smFocusTextPink500 =
    A.class "sm:focus:text-pink-500"


smFocusTextPink600 : Html.Attribute msg
smFocusTextPink600 =
    A.class "sm:focus:text-pink-600"


smFocusTextPink700 : Html.Attribute msg
smFocusTextPink700 =
    A.class "sm:focus:text-pink-700"


smFocusTextPink800 : Html.Attribute msg
smFocusTextPink800 =
    A.class "sm:focus:text-pink-800"


smFocusTextPink900 : Html.Attribute msg
smFocusTextPink900 =
    A.class "sm:focus:text-pink-900"


smTextOpacity0 : Html.Attribute msg
smTextOpacity0 =
    A.class "sm:text-opacity-0"


smTextOpacity25 : Html.Attribute msg
smTextOpacity25 =
    A.class "sm:text-opacity-25"


smTextOpacity50 : Html.Attribute msg
smTextOpacity50 =
    A.class "sm:text-opacity-50"


smTextOpacity75 : Html.Attribute msg
smTextOpacity75 =
    A.class "sm:text-opacity-75"


smTextOpacity100 : Html.Attribute msg
smTextOpacity100 =
    A.class "sm:text-opacity-100"


smHoverTextOpacity0 : Html.Attribute msg
smHoverTextOpacity0 =
    A.class "sm:hover:text-opacity-0"


smHoverTextOpacity25 : Html.Attribute msg
smHoverTextOpacity25 =
    A.class "sm:hover:text-opacity-25"


smHoverTextOpacity50 : Html.Attribute msg
smHoverTextOpacity50 =
    A.class "sm:hover:text-opacity-50"


smHoverTextOpacity75 : Html.Attribute msg
smHoverTextOpacity75 =
    A.class "sm:hover:text-opacity-75"


smHoverTextOpacity100 : Html.Attribute msg
smHoverTextOpacity100 =
    A.class "sm:hover:text-opacity-100"


smFocusTextOpacity0 : Html.Attribute msg
smFocusTextOpacity0 =
    A.class "sm:focus:text-opacity-0"


smFocusTextOpacity25 : Html.Attribute msg
smFocusTextOpacity25 =
    A.class "sm:focus:text-opacity-25"


smFocusTextOpacity50 : Html.Attribute msg
smFocusTextOpacity50 =
    A.class "sm:focus:text-opacity-50"


smFocusTextOpacity75 : Html.Attribute msg
smFocusTextOpacity75 =
    A.class "sm:focus:text-opacity-75"


smFocusTextOpacity100 : Html.Attribute msg
smFocusTextOpacity100 =
    A.class "sm:focus:text-opacity-100"


smItalic : Html.Attribute msg
smItalic =
    A.class "sm:italic"


smNotItalic : Html.Attribute msg
smNotItalic =
    A.class "sm:not-italic"


smUppercase : Html.Attribute msg
smUppercase =
    A.class "sm:uppercase"


smLowercase : Html.Attribute msg
smLowercase =
    A.class "sm:lowercase"


smCapitalize : Html.Attribute msg
smCapitalize =
    A.class "sm:capitalize"


smNormalCase : Html.Attribute msg
smNormalCase =
    A.class "sm:normal-case"


smUnderline : Html.Attribute msg
smUnderline =
    A.class "sm:underline"


smLineThrough : Html.Attribute msg
smLineThrough =
    A.class "sm:line-through"


smNoUnderline : Html.Attribute msg
smNoUnderline =
    A.class "sm:no-underline"


smHoverUnderline : Html.Attribute msg
smHoverUnderline =
    A.class "sm:hover:underline"


smHoverLineThrough : Html.Attribute msg
smHoverLineThrough =
    A.class "sm:hover:line-through"


smHoverNoUnderline : Html.Attribute msg
smHoverNoUnderline =
    A.class "sm:hover:no-underline"


smFocusUnderline : Html.Attribute msg
smFocusUnderline =
    A.class "sm:focus:underline"


smFocusLineThrough : Html.Attribute msg
smFocusLineThrough =
    A.class "sm:focus:line-through"


smFocusNoUnderline : Html.Attribute msg
smFocusNoUnderline =
    A.class "sm:focus:no-underline"


smAntialiased : Html.Attribute msg
smAntialiased =
    A.class "sm:antialiased"


smSubpixelAntialiased : Html.Attribute msg
smSubpixelAntialiased =
    A.class "sm:subpixel-antialiased"


smOrdinal : Html.Attribute msg
smOrdinal =
    A.class "sm:ordinal"


smSlashedZero : Html.Attribute msg
smSlashedZero =
    A.class "sm:slashed-zero"


smLiningNums : Html.Attribute msg
smLiningNums =
    A.class "sm:lining-nums"


smOldstyleNums : Html.Attribute msg
smOldstyleNums =
    A.class "sm:oldstyle-nums"


smProportionalNums : Html.Attribute msg
smProportionalNums =
    A.class "sm:proportional-nums"


smTabularNums : Html.Attribute msg
smTabularNums =
    A.class "sm:tabular-nums"


smDiagonalFractions : Html.Attribute msg
smDiagonalFractions =
    A.class "sm:diagonal-fractions"


smStackedFractions : Html.Attribute msg
smStackedFractions =
    A.class "sm:stacked-fractions"


smNormalNums : Html.Attribute msg
smNormalNums =
    A.class "sm:normal-nums"


smTrackingTighter : Html.Attribute msg
smTrackingTighter =
    A.class "sm:tracking-tighter"


smTrackingTight : Html.Attribute msg
smTrackingTight =
    A.class "sm:tracking-tight"


smTrackingNormal : Html.Attribute msg
smTrackingNormal =
    A.class "sm:tracking-normal"


smTrackingWide : Html.Attribute msg
smTrackingWide =
    A.class "sm:tracking-wide"


smTrackingWider : Html.Attribute msg
smTrackingWider =
    A.class "sm:tracking-wider"


smTrackingWidest : Html.Attribute msg
smTrackingWidest =
    A.class "sm:tracking-widest"


smSelectNone : Html.Attribute msg
smSelectNone =
    A.class "sm:select-none"


smSelectText : Html.Attribute msg
smSelectText =
    A.class "sm:select-text"


smSelectAll : Html.Attribute msg
smSelectAll =
    A.class "sm:select-all"


smSelectAuto : Html.Attribute msg
smSelectAuto =
    A.class "sm:select-auto"


smAlignBaseline : Html.Attribute msg
smAlignBaseline =
    A.class "sm:align-baseline"


smAlignTop : Html.Attribute msg
smAlignTop =
    A.class "sm:align-top"


smAlignMiddle : Html.Attribute msg
smAlignMiddle =
    A.class "sm:align-middle"


smAlignBottom : Html.Attribute msg
smAlignBottom =
    A.class "sm:align-bottom"


smAlignTextTop : Html.Attribute msg
smAlignTextTop =
    A.class "sm:align-text-top"


smAlignTextBottom : Html.Attribute msg
smAlignTextBottom =
    A.class "sm:align-text-bottom"


smVisible : Html.Attribute msg
smVisible =
    A.class "sm:visible"


smInvisible : Html.Attribute msg
smInvisible =
    A.class "sm:invisible"


smWhitespaceNormal : Html.Attribute msg
smWhitespaceNormal =
    A.class "sm:whitespace-normal"


smWhitespaceNoWrap : Html.Attribute msg
smWhitespaceNoWrap =
    A.class "sm:whitespace-no-wrap"


smWhitespacePre : Html.Attribute msg
smWhitespacePre =
    A.class "sm:whitespace-pre"


smWhitespacePreLine : Html.Attribute msg
smWhitespacePreLine =
    A.class "sm:whitespace-pre-line"


smWhitespacePreWrap : Html.Attribute msg
smWhitespacePreWrap =
    A.class "sm:whitespace-pre-wrap"


smBreakNormal : Html.Attribute msg
smBreakNormal =
    A.class "sm:break-normal"


smBreakWords : Html.Attribute msg
smBreakWords =
    A.class "sm:break-words"


smBreakAll : Html.Attribute msg
smBreakAll =
    A.class "sm:break-all"


smTruncate : Html.Attribute msg
smTruncate =
    A.class "sm:truncate"


smW0 : Html.Attribute msg
smW0 =
    A.class "sm:w-0"


smW1 : Html.Attribute msg
smW1 =
    A.class "sm:w-1"


smW2 : Html.Attribute msg
smW2 =
    A.class "sm:w-2"


smW3 : Html.Attribute msg
smW3 =
    A.class "sm:w-3"


smW4 : Html.Attribute msg
smW4 =
    A.class "sm:w-4"


smW5 : Html.Attribute msg
smW5 =
    A.class "sm:w-5"


smW6 : Html.Attribute msg
smW6 =
    A.class "sm:w-6"


smW8 : Html.Attribute msg
smW8 =
    A.class "sm:w-8"


smW10 : Html.Attribute msg
smW10 =
    A.class "sm:w-10"


smW12 : Html.Attribute msg
smW12 =
    A.class "sm:w-12"


smW16 : Html.Attribute msg
smW16 =
    A.class "sm:w-16"


smW20 : Html.Attribute msg
smW20 =
    A.class "sm:w-20"


smW24 : Html.Attribute msg
smW24 =
    A.class "sm:w-24"


smW32 : Html.Attribute msg
smW32 =
    A.class "sm:w-32"


smW40 : Html.Attribute msg
smW40 =
    A.class "sm:w-40"


smW48 : Html.Attribute msg
smW48 =
    A.class "sm:w-48"


smW56 : Html.Attribute msg
smW56 =
    A.class "sm:w-56"


smW64 : Html.Attribute msg
smW64 =
    A.class "sm:w-64"


smWAuto : Html.Attribute msg
smWAuto =
    A.class "sm:w-auto"


smWPx : Html.Attribute msg
smWPx =
    A.class "sm:w-px"


smW1over2 : Html.Attribute msg
smW1over2 =
    A.class "sm:w-1/2"


smW1over3 : Html.Attribute msg
smW1over3 =
    A.class "sm:w-1/3"


smW2over3 : Html.Attribute msg
smW2over3 =
    A.class "sm:w-2/3"


smW1over4 : Html.Attribute msg
smW1over4 =
    A.class "sm:w-1/4"


smW2over4 : Html.Attribute msg
smW2over4 =
    A.class "sm:w-2/4"


smW3over4 : Html.Attribute msg
smW3over4 =
    A.class "sm:w-3/4"


smW1over5 : Html.Attribute msg
smW1over5 =
    A.class "sm:w-1/5"


smW2over5 : Html.Attribute msg
smW2over5 =
    A.class "sm:w-2/5"


smW3over5 : Html.Attribute msg
smW3over5 =
    A.class "sm:w-3/5"


smW4over5 : Html.Attribute msg
smW4over5 =
    A.class "sm:w-4/5"


smW1over6 : Html.Attribute msg
smW1over6 =
    A.class "sm:w-1/6"


smW2over6 : Html.Attribute msg
smW2over6 =
    A.class "sm:w-2/6"


smW3over6 : Html.Attribute msg
smW3over6 =
    A.class "sm:w-3/6"


smW4over6 : Html.Attribute msg
smW4over6 =
    A.class "sm:w-4/6"


smW5over6 : Html.Attribute msg
smW5over6 =
    A.class "sm:w-5/6"


smW1over12 : Html.Attribute msg
smW1over12 =
    A.class "sm:w-1/12"


smW2over12 : Html.Attribute msg
smW2over12 =
    A.class "sm:w-2/12"


smW3over12 : Html.Attribute msg
smW3over12 =
    A.class "sm:w-3/12"


smW4over12 : Html.Attribute msg
smW4over12 =
    A.class "sm:w-4/12"


smW5over12 : Html.Attribute msg
smW5over12 =
    A.class "sm:w-5/12"


smW6over12 : Html.Attribute msg
smW6over12 =
    A.class "sm:w-6/12"


smW7over12 : Html.Attribute msg
smW7over12 =
    A.class "sm:w-7/12"


smW8over12 : Html.Attribute msg
smW8over12 =
    A.class "sm:w-8/12"


smW9over12 : Html.Attribute msg
smW9over12 =
    A.class "sm:w-9/12"


smW10over12 : Html.Attribute msg
smW10over12 =
    A.class "sm:w-10/12"


smW11over12 : Html.Attribute msg
smW11over12 =
    A.class "sm:w-11/12"


smWFull : Html.Attribute msg
smWFull =
    A.class "sm:w-full"


smWScreen : Html.Attribute msg
smWScreen =
    A.class "sm:w-screen"


smZ0 : Html.Attribute msg
smZ0 =
    A.class "sm:z-0"


smZ10 : Html.Attribute msg
smZ10 =
    A.class "sm:z-10"


smZ20 : Html.Attribute msg
smZ20 =
    A.class "sm:z-20"


smZ30 : Html.Attribute msg
smZ30 =
    A.class "sm:z-30"


smZ40 : Html.Attribute msg
smZ40 =
    A.class "sm:z-40"


smZ50 : Html.Attribute msg
smZ50 =
    A.class "sm:z-50"


smZAuto : Html.Attribute msg
smZAuto =
    A.class "sm:z-auto"


smGap0 : Html.Attribute msg
smGap0 =
    A.class "sm:gap-0"


smGap1 : Html.Attribute msg
smGap1 =
    A.class "sm:gap-1"


smGap2 : Html.Attribute msg
smGap2 =
    A.class "sm:gap-2"


smGap3 : Html.Attribute msg
smGap3 =
    A.class "sm:gap-3"


smGap4 : Html.Attribute msg
smGap4 =
    A.class "sm:gap-4"


smGap5 : Html.Attribute msg
smGap5 =
    A.class "sm:gap-5"


smGap6 : Html.Attribute msg
smGap6 =
    A.class "sm:gap-6"


smGap8 : Html.Attribute msg
smGap8 =
    A.class "sm:gap-8"


smGap10 : Html.Attribute msg
smGap10 =
    A.class "sm:gap-10"


smGap12 : Html.Attribute msg
smGap12 =
    A.class "sm:gap-12"


smGap16 : Html.Attribute msg
smGap16 =
    A.class "sm:gap-16"


smGap20 : Html.Attribute msg
smGap20 =
    A.class "sm:gap-20"


smGap24 : Html.Attribute msg
smGap24 =
    A.class "sm:gap-24"


smGap32 : Html.Attribute msg
smGap32 =
    A.class "sm:gap-32"


smGap40 : Html.Attribute msg
smGap40 =
    A.class "sm:gap-40"


smGap48 : Html.Attribute msg
smGap48 =
    A.class "sm:gap-48"


smGap56 : Html.Attribute msg
smGap56 =
    A.class "sm:gap-56"


smGap64 : Html.Attribute msg
smGap64 =
    A.class "sm:gap-64"


smGapPx : Html.Attribute msg
smGapPx =
    A.class "sm:gap-px"


smGapX0 : Html.Attribute msg
smGapX0 =
    A.class "sm:gap-x-0"


smGapX1 : Html.Attribute msg
smGapX1 =
    A.class "sm:gap-x-1"


smGapX2 : Html.Attribute msg
smGapX2 =
    A.class "sm:gap-x-2"


smGapX3 : Html.Attribute msg
smGapX3 =
    A.class "sm:gap-x-3"


smGapX4 : Html.Attribute msg
smGapX4 =
    A.class "sm:gap-x-4"


smGapX5 : Html.Attribute msg
smGapX5 =
    A.class "sm:gap-x-5"


smGapX6 : Html.Attribute msg
smGapX6 =
    A.class "sm:gap-x-6"


smGapX8 : Html.Attribute msg
smGapX8 =
    A.class "sm:gap-x-8"


smGapX10 : Html.Attribute msg
smGapX10 =
    A.class "sm:gap-x-10"


smGapX12 : Html.Attribute msg
smGapX12 =
    A.class "sm:gap-x-12"


smGapX16 : Html.Attribute msg
smGapX16 =
    A.class "sm:gap-x-16"


smGapX20 : Html.Attribute msg
smGapX20 =
    A.class "sm:gap-x-20"


smGapX24 : Html.Attribute msg
smGapX24 =
    A.class "sm:gap-x-24"


smGapX32 : Html.Attribute msg
smGapX32 =
    A.class "sm:gap-x-32"


smGapX40 : Html.Attribute msg
smGapX40 =
    A.class "sm:gap-x-40"


smGapX48 : Html.Attribute msg
smGapX48 =
    A.class "sm:gap-x-48"


smGapX56 : Html.Attribute msg
smGapX56 =
    A.class "sm:gap-x-56"


smGapX64 : Html.Attribute msg
smGapX64 =
    A.class "sm:gap-x-64"


smGapXPx : Html.Attribute msg
smGapXPx =
    A.class "sm:gap-x-px"


smGapY0 : Html.Attribute msg
smGapY0 =
    A.class "sm:gap-y-0"


smGapY1 : Html.Attribute msg
smGapY1 =
    A.class "sm:gap-y-1"


smGapY2 : Html.Attribute msg
smGapY2 =
    A.class "sm:gap-y-2"


smGapY3 : Html.Attribute msg
smGapY3 =
    A.class "sm:gap-y-3"


smGapY4 : Html.Attribute msg
smGapY4 =
    A.class "sm:gap-y-4"


smGapY5 : Html.Attribute msg
smGapY5 =
    A.class "sm:gap-y-5"


smGapY6 : Html.Attribute msg
smGapY6 =
    A.class "sm:gap-y-6"


smGapY8 : Html.Attribute msg
smGapY8 =
    A.class "sm:gap-y-8"


smGapY10 : Html.Attribute msg
smGapY10 =
    A.class "sm:gap-y-10"


smGapY12 : Html.Attribute msg
smGapY12 =
    A.class "sm:gap-y-12"


smGapY16 : Html.Attribute msg
smGapY16 =
    A.class "sm:gap-y-16"


smGapY20 : Html.Attribute msg
smGapY20 =
    A.class "sm:gap-y-20"


smGapY24 : Html.Attribute msg
smGapY24 =
    A.class "sm:gap-y-24"


smGapY32 : Html.Attribute msg
smGapY32 =
    A.class "sm:gap-y-32"


smGapY40 : Html.Attribute msg
smGapY40 =
    A.class "sm:gap-y-40"


smGapY48 : Html.Attribute msg
smGapY48 =
    A.class "sm:gap-y-48"


smGapY56 : Html.Attribute msg
smGapY56 =
    A.class "sm:gap-y-56"


smGapY64 : Html.Attribute msg
smGapY64 =
    A.class "sm:gap-y-64"


smGapYPx : Html.Attribute msg
smGapYPx =
    A.class "sm:gap-y-px"


smGridFlowRow : Html.Attribute msg
smGridFlowRow =
    A.class "sm:grid-flow-row"


smGridFlowCol : Html.Attribute msg
smGridFlowCol =
    A.class "sm:grid-flow-col"


smGridFlowRowDense : Html.Attribute msg
smGridFlowRowDense =
    A.class "sm:grid-flow-row-dense"


smGridFlowColDense : Html.Attribute msg
smGridFlowColDense =
    A.class "sm:grid-flow-col-dense"


smGridCols1 : Html.Attribute msg
smGridCols1 =
    A.class "sm:grid-cols-1"


smGridCols2 : Html.Attribute msg
smGridCols2 =
    A.class "sm:grid-cols-2"


smGridCols3 : Html.Attribute msg
smGridCols3 =
    A.class "sm:grid-cols-3"


smGridCols4 : Html.Attribute msg
smGridCols4 =
    A.class "sm:grid-cols-4"


smGridCols5 : Html.Attribute msg
smGridCols5 =
    A.class "sm:grid-cols-5"


smGridCols6 : Html.Attribute msg
smGridCols6 =
    A.class "sm:grid-cols-6"


smGridCols7 : Html.Attribute msg
smGridCols7 =
    A.class "sm:grid-cols-7"


smGridCols8 : Html.Attribute msg
smGridCols8 =
    A.class "sm:grid-cols-8"


smGridCols9 : Html.Attribute msg
smGridCols9 =
    A.class "sm:grid-cols-9"


smGridCols10 : Html.Attribute msg
smGridCols10 =
    A.class "sm:grid-cols-10"


smGridCols11 : Html.Attribute msg
smGridCols11 =
    A.class "sm:grid-cols-11"


smGridCols12 : Html.Attribute msg
smGridCols12 =
    A.class "sm:grid-cols-12"


smGridColsNone : Html.Attribute msg
smGridColsNone =
    A.class "sm:grid-cols-none"


smColAuto : Html.Attribute msg
smColAuto =
    A.class "sm:col-auto"


smColSpan1 : Html.Attribute msg
smColSpan1 =
    A.class "sm:col-span-1"


smColSpan2 : Html.Attribute msg
smColSpan2 =
    A.class "sm:col-span-2"


smColSpan3 : Html.Attribute msg
smColSpan3 =
    A.class "sm:col-span-3"


smColSpan4 : Html.Attribute msg
smColSpan4 =
    A.class "sm:col-span-4"


smColSpan5 : Html.Attribute msg
smColSpan5 =
    A.class "sm:col-span-5"


smColSpan6 : Html.Attribute msg
smColSpan6 =
    A.class "sm:col-span-6"


smColSpan7 : Html.Attribute msg
smColSpan7 =
    A.class "sm:col-span-7"


smColSpan8 : Html.Attribute msg
smColSpan8 =
    A.class "sm:col-span-8"


smColSpan9 : Html.Attribute msg
smColSpan9 =
    A.class "sm:col-span-9"


smColSpan10 : Html.Attribute msg
smColSpan10 =
    A.class "sm:col-span-10"


smColSpan11 : Html.Attribute msg
smColSpan11 =
    A.class "sm:col-span-11"


smColSpan12 : Html.Attribute msg
smColSpan12 =
    A.class "sm:col-span-12"


smColStart1 : Html.Attribute msg
smColStart1 =
    A.class "sm:col-start-1"


smColStart2 : Html.Attribute msg
smColStart2 =
    A.class "sm:col-start-2"


smColStart3 : Html.Attribute msg
smColStart3 =
    A.class "sm:col-start-3"


smColStart4 : Html.Attribute msg
smColStart4 =
    A.class "sm:col-start-4"


smColStart5 : Html.Attribute msg
smColStart5 =
    A.class "sm:col-start-5"


smColStart6 : Html.Attribute msg
smColStart6 =
    A.class "sm:col-start-6"


smColStart7 : Html.Attribute msg
smColStart7 =
    A.class "sm:col-start-7"


smColStart8 : Html.Attribute msg
smColStart8 =
    A.class "sm:col-start-8"


smColStart9 : Html.Attribute msg
smColStart9 =
    A.class "sm:col-start-9"


smColStart10 : Html.Attribute msg
smColStart10 =
    A.class "sm:col-start-10"


smColStart11 : Html.Attribute msg
smColStart11 =
    A.class "sm:col-start-11"


smColStart12 : Html.Attribute msg
smColStart12 =
    A.class "sm:col-start-12"


smColStart13 : Html.Attribute msg
smColStart13 =
    A.class "sm:col-start-13"


smColStartAuto : Html.Attribute msg
smColStartAuto =
    A.class "sm:col-start-auto"


smColEnd1 : Html.Attribute msg
smColEnd1 =
    A.class "sm:col-end-1"


smColEnd2 : Html.Attribute msg
smColEnd2 =
    A.class "sm:col-end-2"


smColEnd3 : Html.Attribute msg
smColEnd3 =
    A.class "sm:col-end-3"


smColEnd4 : Html.Attribute msg
smColEnd4 =
    A.class "sm:col-end-4"


smColEnd5 : Html.Attribute msg
smColEnd5 =
    A.class "sm:col-end-5"


smColEnd6 : Html.Attribute msg
smColEnd6 =
    A.class "sm:col-end-6"


smColEnd7 : Html.Attribute msg
smColEnd7 =
    A.class "sm:col-end-7"


smColEnd8 : Html.Attribute msg
smColEnd8 =
    A.class "sm:col-end-8"


smColEnd9 : Html.Attribute msg
smColEnd9 =
    A.class "sm:col-end-9"


smColEnd10 : Html.Attribute msg
smColEnd10 =
    A.class "sm:col-end-10"


smColEnd11 : Html.Attribute msg
smColEnd11 =
    A.class "sm:col-end-11"


smColEnd12 : Html.Attribute msg
smColEnd12 =
    A.class "sm:col-end-12"


smColEnd13 : Html.Attribute msg
smColEnd13 =
    A.class "sm:col-end-13"


smColEndAuto : Html.Attribute msg
smColEndAuto =
    A.class "sm:col-end-auto"


smGridRows1 : Html.Attribute msg
smGridRows1 =
    A.class "sm:grid-rows-1"


smGridRows2 : Html.Attribute msg
smGridRows2 =
    A.class "sm:grid-rows-2"


smGridRows3 : Html.Attribute msg
smGridRows3 =
    A.class "sm:grid-rows-3"


smGridRows4 : Html.Attribute msg
smGridRows4 =
    A.class "sm:grid-rows-4"


smGridRows5 : Html.Attribute msg
smGridRows5 =
    A.class "sm:grid-rows-5"


smGridRows6 : Html.Attribute msg
smGridRows6 =
    A.class "sm:grid-rows-6"


smGridRowsNone : Html.Attribute msg
smGridRowsNone =
    A.class "sm:grid-rows-none"


smRowAuto : Html.Attribute msg
smRowAuto =
    A.class "sm:row-auto"


smRowSpan1 : Html.Attribute msg
smRowSpan1 =
    A.class "sm:row-span-1"


smRowSpan2 : Html.Attribute msg
smRowSpan2 =
    A.class "sm:row-span-2"


smRowSpan3 : Html.Attribute msg
smRowSpan3 =
    A.class "sm:row-span-3"


smRowSpan4 : Html.Attribute msg
smRowSpan4 =
    A.class "sm:row-span-4"


smRowSpan5 : Html.Attribute msg
smRowSpan5 =
    A.class "sm:row-span-5"


smRowSpan6 : Html.Attribute msg
smRowSpan6 =
    A.class "sm:row-span-6"


smRowStart1 : Html.Attribute msg
smRowStart1 =
    A.class "sm:row-start-1"


smRowStart2 : Html.Attribute msg
smRowStart2 =
    A.class "sm:row-start-2"


smRowStart3 : Html.Attribute msg
smRowStart3 =
    A.class "sm:row-start-3"


smRowStart4 : Html.Attribute msg
smRowStart4 =
    A.class "sm:row-start-4"


smRowStart5 : Html.Attribute msg
smRowStart5 =
    A.class "sm:row-start-5"


smRowStart6 : Html.Attribute msg
smRowStart6 =
    A.class "sm:row-start-6"


smRowStart7 : Html.Attribute msg
smRowStart7 =
    A.class "sm:row-start-7"


smRowStartAuto : Html.Attribute msg
smRowStartAuto =
    A.class "sm:row-start-auto"


smRowEnd1 : Html.Attribute msg
smRowEnd1 =
    A.class "sm:row-end-1"


smRowEnd2 : Html.Attribute msg
smRowEnd2 =
    A.class "sm:row-end-2"


smRowEnd3 : Html.Attribute msg
smRowEnd3 =
    A.class "sm:row-end-3"


smRowEnd4 : Html.Attribute msg
smRowEnd4 =
    A.class "sm:row-end-4"


smRowEnd5 : Html.Attribute msg
smRowEnd5 =
    A.class "sm:row-end-5"


smRowEnd6 : Html.Attribute msg
smRowEnd6 =
    A.class "sm:row-end-6"


smRowEnd7 : Html.Attribute msg
smRowEnd7 =
    A.class "sm:row-end-7"


smRowEndAuto : Html.Attribute msg
smRowEndAuto =
    A.class "sm:row-end-auto"


smTransform : Html.Attribute msg
smTransform =
    A.class "sm:transform"


smTransformNone : Html.Attribute msg
smTransformNone =
    A.class "sm:transform-none"


smOriginCenter : Html.Attribute msg
smOriginCenter =
    A.class "sm:origin-center"


smOriginTop : Html.Attribute msg
smOriginTop =
    A.class "sm:origin-top"


smOriginTopRight : Html.Attribute msg
smOriginTopRight =
    A.class "sm:origin-top-right"


smOriginRight : Html.Attribute msg
smOriginRight =
    A.class "sm:origin-right"


smOriginBottomRight : Html.Attribute msg
smOriginBottomRight =
    A.class "sm:origin-bottom-right"


smOriginBottom : Html.Attribute msg
smOriginBottom =
    A.class "sm:origin-bottom"


smOriginBottomLeft : Html.Attribute msg
smOriginBottomLeft =
    A.class "sm:origin-bottom-left"


smOriginLeft : Html.Attribute msg
smOriginLeft =
    A.class "sm:origin-left"


smOriginTopLeft : Html.Attribute msg
smOriginTopLeft =
    A.class "sm:origin-top-left"


smScale0 : Html.Attribute msg
smScale0 =
    A.class "sm:scale-0"


smScale50 : Html.Attribute msg
smScale50 =
    A.class "sm:scale-50"


smScale75 : Html.Attribute msg
smScale75 =
    A.class "sm:scale-75"


smScale90 : Html.Attribute msg
smScale90 =
    A.class "sm:scale-90"


smScale95 : Html.Attribute msg
smScale95 =
    A.class "sm:scale-95"


smScale100 : Html.Attribute msg
smScale100 =
    A.class "sm:scale-100"


smScale105 : Html.Attribute msg
smScale105 =
    A.class "sm:scale-105"


smScale110 : Html.Attribute msg
smScale110 =
    A.class "sm:scale-110"


smScale125 : Html.Attribute msg
smScale125 =
    A.class "sm:scale-125"


smScale150 : Html.Attribute msg
smScale150 =
    A.class "sm:scale-150"


smScaleX0 : Html.Attribute msg
smScaleX0 =
    A.class "sm:scale-x-0"


smScaleX50 : Html.Attribute msg
smScaleX50 =
    A.class "sm:scale-x-50"


smScaleX75 : Html.Attribute msg
smScaleX75 =
    A.class "sm:scale-x-75"


smScaleX90 : Html.Attribute msg
smScaleX90 =
    A.class "sm:scale-x-90"


smScaleX95 : Html.Attribute msg
smScaleX95 =
    A.class "sm:scale-x-95"


smScaleX100 : Html.Attribute msg
smScaleX100 =
    A.class "sm:scale-x-100"


smScaleX105 : Html.Attribute msg
smScaleX105 =
    A.class "sm:scale-x-105"


smScaleX110 : Html.Attribute msg
smScaleX110 =
    A.class "sm:scale-x-110"


smScaleX125 : Html.Attribute msg
smScaleX125 =
    A.class "sm:scale-x-125"


smScaleX150 : Html.Attribute msg
smScaleX150 =
    A.class "sm:scale-x-150"


smScaleY0 : Html.Attribute msg
smScaleY0 =
    A.class "sm:scale-y-0"


smScaleY50 : Html.Attribute msg
smScaleY50 =
    A.class "sm:scale-y-50"


smScaleY75 : Html.Attribute msg
smScaleY75 =
    A.class "sm:scale-y-75"


smScaleY90 : Html.Attribute msg
smScaleY90 =
    A.class "sm:scale-y-90"


smScaleY95 : Html.Attribute msg
smScaleY95 =
    A.class "sm:scale-y-95"


smScaleY100 : Html.Attribute msg
smScaleY100 =
    A.class "sm:scale-y-100"


smScaleY105 : Html.Attribute msg
smScaleY105 =
    A.class "sm:scale-y-105"


smScaleY110 : Html.Attribute msg
smScaleY110 =
    A.class "sm:scale-y-110"


smScaleY125 : Html.Attribute msg
smScaleY125 =
    A.class "sm:scale-y-125"


smScaleY150 : Html.Attribute msg
smScaleY150 =
    A.class "sm:scale-y-150"


smHoverScale0 : Html.Attribute msg
smHoverScale0 =
    A.class "sm:hover:scale-0"


smHoverScale50 : Html.Attribute msg
smHoverScale50 =
    A.class "sm:hover:scale-50"


smHoverScale75 : Html.Attribute msg
smHoverScale75 =
    A.class "sm:hover:scale-75"


smHoverScale90 : Html.Attribute msg
smHoverScale90 =
    A.class "sm:hover:scale-90"


smHoverScale95 : Html.Attribute msg
smHoverScale95 =
    A.class "sm:hover:scale-95"


smHoverScale100 : Html.Attribute msg
smHoverScale100 =
    A.class "sm:hover:scale-100"


smHoverScale105 : Html.Attribute msg
smHoverScale105 =
    A.class "sm:hover:scale-105"


smHoverScale110 : Html.Attribute msg
smHoverScale110 =
    A.class "sm:hover:scale-110"


smHoverScale125 : Html.Attribute msg
smHoverScale125 =
    A.class "sm:hover:scale-125"


smHoverScale150 : Html.Attribute msg
smHoverScale150 =
    A.class "sm:hover:scale-150"


smHoverScaleX0 : Html.Attribute msg
smHoverScaleX0 =
    A.class "sm:hover:scale-x-0"


smHoverScaleX50 : Html.Attribute msg
smHoverScaleX50 =
    A.class "sm:hover:scale-x-50"


smHoverScaleX75 : Html.Attribute msg
smHoverScaleX75 =
    A.class "sm:hover:scale-x-75"


smHoverScaleX90 : Html.Attribute msg
smHoverScaleX90 =
    A.class "sm:hover:scale-x-90"


smHoverScaleX95 : Html.Attribute msg
smHoverScaleX95 =
    A.class "sm:hover:scale-x-95"


smHoverScaleX100 : Html.Attribute msg
smHoverScaleX100 =
    A.class "sm:hover:scale-x-100"


smHoverScaleX105 : Html.Attribute msg
smHoverScaleX105 =
    A.class "sm:hover:scale-x-105"


smHoverScaleX110 : Html.Attribute msg
smHoverScaleX110 =
    A.class "sm:hover:scale-x-110"


smHoverScaleX125 : Html.Attribute msg
smHoverScaleX125 =
    A.class "sm:hover:scale-x-125"


smHoverScaleX150 : Html.Attribute msg
smHoverScaleX150 =
    A.class "sm:hover:scale-x-150"


smHoverScaleY0 : Html.Attribute msg
smHoverScaleY0 =
    A.class "sm:hover:scale-y-0"


smHoverScaleY50 : Html.Attribute msg
smHoverScaleY50 =
    A.class "sm:hover:scale-y-50"


smHoverScaleY75 : Html.Attribute msg
smHoverScaleY75 =
    A.class "sm:hover:scale-y-75"


smHoverScaleY90 : Html.Attribute msg
smHoverScaleY90 =
    A.class "sm:hover:scale-y-90"


smHoverScaleY95 : Html.Attribute msg
smHoverScaleY95 =
    A.class "sm:hover:scale-y-95"


smHoverScaleY100 : Html.Attribute msg
smHoverScaleY100 =
    A.class "sm:hover:scale-y-100"


smHoverScaleY105 : Html.Attribute msg
smHoverScaleY105 =
    A.class "sm:hover:scale-y-105"


smHoverScaleY110 : Html.Attribute msg
smHoverScaleY110 =
    A.class "sm:hover:scale-y-110"


smHoverScaleY125 : Html.Attribute msg
smHoverScaleY125 =
    A.class "sm:hover:scale-y-125"


smHoverScaleY150 : Html.Attribute msg
smHoverScaleY150 =
    A.class "sm:hover:scale-y-150"


smFocusScale0 : Html.Attribute msg
smFocusScale0 =
    A.class "sm:focus:scale-0"


smFocusScale50 : Html.Attribute msg
smFocusScale50 =
    A.class "sm:focus:scale-50"


smFocusScale75 : Html.Attribute msg
smFocusScale75 =
    A.class "sm:focus:scale-75"


smFocusScale90 : Html.Attribute msg
smFocusScale90 =
    A.class "sm:focus:scale-90"


smFocusScale95 : Html.Attribute msg
smFocusScale95 =
    A.class "sm:focus:scale-95"


smFocusScale100 : Html.Attribute msg
smFocusScale100 =
    A.class "sm:focus:scale-100"


smFocusScale105 : Html.Attribute msg
smFocusScale105 =
    A.class "sm:focus:scale-105"


smFocusScale110 : Html.Attribute msg
smFocusScale110 =
    A.class "sm:focus:scale-110"


smFocusScale125 : Html.Attribute msg
smFocusScale125 =
    A.class "sm:focus:scale-125"


smFocusScale150 : Html.Attribute msg
smFocusScale150 =
    A.class "sm:focus:scale-150"


smFocusScaleX0 : Html.Attribute msg
smFocusScaleX0 =


smFocusScaleX50 =


