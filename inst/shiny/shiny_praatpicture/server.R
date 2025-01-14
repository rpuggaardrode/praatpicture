options(shiny.maxRequestSize=50*1024^2)

server <- function(input, output, session) {

  curDev <- grDevices::dev.cur()
  onSessionEnded(function() grDevices::dev.set(curDev))

  observeEvent(input$frames, {
    if ('sound' %in% input$frames) {
      shinyjs::show('wave_color')
      shinyjs::show('wave_channels')
      shinyjs::show('wave_channelNames')
      shinyjs::show('wave_lineWidth')
    } else {
      shinyjs::hide('wave_color')
      shinyjs::hide('wave_channels')
      shinyjs::hide('wave_channelNames')
      shinyjs::hide('wave_lineWidth')
    }

    if ('TextGrid' %in% input$frames) {
      shinyjs::show('tg_file')
    } else {
      shinyjs::hide('tg_file')
    }

    if ('spectrogram' %in% input$frames) {
      shinyjs::show('spec_freqRangeMin')
      shinyjs::show('spec_freqRangeMax')
      shinyjs::show('spec_windowLength')
      shinyjs::show('spec_dynamicRange')
      shinyjs::show('spec_timeStep')
      shinyjs::show('spec_windowShape')
      shinyjs::show('spec_colors')
      shinyjs::show('spec_axisLabel')
      shinyjs::show('spec_channel')
      shinyjs::show('pitch_plotOnSpec')
      shinyjs::show('formant_plotOnSpec')
      shinyjs::show('intensity_plotOnSpec')
    } else {
      shinyjs::hide('spec_freqRangeMin')
      shinyjs::hide('spec_freqRangeMax')
      shinyjs::hide('spec_windowLength')
      shinyjs::hide('spec_dynamicRange')
      shinyjs::hide('spec_timeStep')
      shinyjs::hide('spec_windowShape')
      shinyjs::hide('spec_colors')
      shinyjs::hide('spec_axisLabel')
      shinyjs::hide('spec_channel')
      shinyjs::hide('pitch_plotOnSpec')
      shinyjs::hide('formant_plotOnSpec')
      shinyjs::hide('intensity_plotOnSpec')
    }

    if ('TextGrid' %in% input$frames) {
      shinyjs::show('tg_tiers')
      shinyjs::show('tg_focusTier')
      shinyjs::show('tg_focusTierColor')
      shinyjs::show('tg_focusTierLineType')
      shinyjs::show('tg_tierNames')
      shinyjs::show('tg_alignment')
      shinyjs::show('tg_specialChar')
      shinyjs::show('tg_color')
    } else {
      shinyjs::hide('tg_tiers')
      shinyjs::hide('tg_focusTier')
      shinyjs::hide('tg_focusTierColor')
      shinyjs::hide('tg_focusTierLineType')
      shinyjs::hide('tg_tierNames')
      shinyjs::hide('tg_alignment')
      shinyjs::hide('tg_specialChar')
      shinyjs::hide('tg_color')
    }

    if ('pitch' %in% input$frames) {
      shinyjs::show('pitch_plotType')
      shinyjs::show('pitch_scale')
      shinyjs::show('pitch_freqRangeMin')
      shinyjs::show('pitch_freqRangeMax')
      shinyjs::show('pitch_timeStep')
      shinyjs::show('pitch_floor')
      shinyjs::show('pitch_ceiling')
      shinyjs::show('pitch_color')
      shinyjs::show('pitch_axisLabel')
    } else {
      shinyjs::hide('pitch_plotType')
      shinyjs::hide('pitch_scale')
      shinyjs::hide('pitch_freqRangeMin')
      shinyjs::hide('pitch_freqRangeMax')
      shinyjs::hide('pitch_timeStep')
      shinyjs::hide('pitch_floor')
      shinyjs::hide('pitch_ceiling')
      shinyjs::hide('pitch_color')
      shinyjs::hide('pitch_axisLabel')
    }

    if ('formant' %in% input$frames) {
      shinyjs::show('formant_plotType')
      shinyjs::show('formant_freqRangeMin')
      shinyjs::show('formant_freqRangeMax')
      shinyjs::show('formant_dynamicRange')
      shinyjs::show('formant_timeStep')
      shinyjs::show('formant_windowLength')
      shinyjs::show('formant_maxN')
      shinyjs::show('formant_color')
      shinyjs::show('formant_axisLabel')
      shinyjs::show('formant_number')
    } else {
      shinyjs::hide('formant_plotType')
      shinyjs::hide('formant_freqRangeMin')
      shinyjs::hide('formant_freqRangeMax')
      shinyjs::hide('formant_dynamicRange')
      shinyjs::hide('formant_timeStep')
      shinyjs::hide('formant_windowLength')
      shinyjs::hide('formant_maxN')
      shinyjs::hide('formant_color')
      shinyjs::hide('formant_axisLabel')
      shinyjs::hide('formant_number')
    }

    if ('intensity' %in% input$frames) {
      shinyjs::show('intensity_rangeMin')
      shinyjs::show('intensity_rangeMax')
      shinyjs::show('intensity_minPitch')
      shinyjs::show('intensity_color')
      shinyjs::show('intensity_axisLabel')
    } else {
      shinyjs::hide('intensity_rangeMin')
      shinyjs::hide('intensity_rangeMax')
      shinyjs::hide('intensity_minPitch')
      shinyjs::hide('intensity_color')
      shinyjs::hide('intensity_axisLabel')
    }

    updateTextInput(inputId = 'proportion',
                    value = paste(rep(50, length(input$frames)),
                                  collapse=','))
  })

  observeEvent(input$pitch_plotOnSpec, {
    if (input$pitch_plotOnSpec) {
      shinyjs::show('pitch_plotType')
      shinyjs::show('pitch_scale')
      shinyjs::show('pitch_freqRangeMin')
      shinyjs::show('pitch_freqRangeMax')
      shinyjs::show('pitch_timeStep')
      shinyjs::show('pitch_floor')
      shinyjs::show('pitch_ceiling')
      shinyjs::show('pitch_color')
      shinyjs::show('pitch_axisLabel')
    } else {
      shinyjs::hide('pitch_plotType')
      shinyjs::hide('pitch_scale')
      shinyjs::hide('pitch_freqRangeMin')
      shinyjs::hide('pitch_freqRangeMax')
      shinyjs::hide('pitch_timeStep')
      shinyjs::hide('pitch_floor')
      shinyjs::hide('pitch_ceiling')
      shinyjs::hide('pitch_color')
      shinyjs::hide('pitch_axisLabel')
    }
  })

  observeEvent(input$formant_plotOnSpec, {
    if (input$formant_plotOnSpec) {
      shinyjs::show('formant_plotType')
      shinyjs::show('formant_freqRangeMin')
      shinyjs::show('formant_freqRangeMax')
      shinyjs::show('formant_dynamicRange')
      shinyjs::show('formant_timeStep')
      shinyjs::show('formant_windowLength')
      shinyjs::show('formant_maxN')
      shinyjs::show('formant_color')
      shinyjs::show('formant_axisLabel')
      shinyjs::show('formant_number')
    } else {
      shinyjs::hide('formant_plotType')
      shinyjs::hide('formant_freqRangeMin')
      shinyjs::hide('formant_freqRangeMax')
      shinyjs::hide('formant_dynamicRange')
      shinyjs::hide('formant_timeStep')
      shinyjs::hide('formant_windowLength')
      shinyjs::hide('formant_maxN')
      shinyjs::hide('formant_color')
      shinyjs::hide('formant_axisLabel')
      shinyjs::hide('formant_number')
    }
  })

  observeEvent(input$intensity_plotOnSpec, {
    if (input$intensity_plotOnSpec) {
      shinyjs::show('intensity_rangeMin')
      shinyjs::show('intensity_rangeMax')
      shinyjs::show('intensity_minPitch')
      shinyjs::show('intensity_color')
      shinyjs::show('intensity_axisLabel')
    } else {
      shinyjs::hide('intensity_rangeMin')
      shinyjs::hide('intensity_rangeMax')
      shinyjs::hide('intensity_minPitch')
      shinyjs::hide('intensity_color')
      shinyjs::hide('intensity_axisLabel')
    }
  })

  wave_channels <- reactive({
    if (input$wave_channels != 'all') {
      as.numeric(unlist(strsplit(input$wave_channels, ',')))
    } else {
      'all'
    }
  })

  tg_tiers <- reactive({
    if (input$tg_tiers == 'all') {
      'all'
    } else if (!grepl(paste(0:9, collapse='|'), input$tg_tiers)) {
      unlist(strsplit(input$tg_tiers, ','))
    } else {
      as.numeric(unlist(strsplit(input$tg_tiers, ',')))
    }
  })

  tg_focusTier <- reactive({
    if (input$tg_focusTier == 'all') {
      'all'
    } else if (input$tg_focusTier == 'none') {
      'none'
    } else if (!grepl(paste(0:9, collapse='|'), input$tg_focusTier)) {
      unlist(strsplit(input$tg_focusTier, ','))
    } else {
      as.numeric(unlist(strsplit(input$tg_focusTier, ',')))
    }
  })

  spec_channel <- reactive({
    if (is.na(input$spec_channel)) {
      NULL
    } else {
      input$spec_channel
    }
  })

  observeEvent(input$wave_channelNames, {
    if (input$wave_channelNames) {
      shinyjs::show('wave_channelNamesStr')
    } else {
      shinyjs::hide('wave_ChannelNamesStr')
    }
  })

  channelNames <- reactive({
    if (input$wave_channelNames) {
      if (!is.na(input$wave_channelNamesStr)) {
        unlist(strsplit(input$wave_channelNamesStr, ','))
      } else {
        TRUE
      }
    } else {
      FALSE
    }
  })

  observeEvent(input$tUnit, {
    if (input$tUnit == 'ms') {
      updateTextInput(inputId = 'time_axisLabel',
                      value = 'Time (ms)')
    } else if (input$tUnit == 's') {
      updateTextInput(inputId = 'time_axisLabel',
                      value = 'Time (s)')
    }
  })

  observeEvent(input$pitch_scale, {
    if (input$pitch_scale == 'semitones') {
      shinyjs::show('pitch_semitonesRe')
      updateNumericInput(inputId = 'pitch_freqRangeMin', value = -12)
      updateNumericInput(inputId = 'pitch_freqRangeMax', value = 30)
      updateTextInput(inputId = 'pitch_axisLabel', value = 'Frequency (semitones')
    } else if (input$pitch_scale == 'erb') {
      shinyjs::hide('pitch_semitonesRe')
      updateNumericInput(inputId = 'pitch_freqRangeMin', value = 0)
      updateNumericInput(inputId = 'pitch_freqRangeMax', value = 10)
      updateTextInput(inputId = 'pitch_axisLabel', value = 'Frequency (ERB)')
    } else if (input$pitch_scale == 'mel') {
      shinyjs::hide('pitch_semitonesRe')
      updateNumericInput(inputId = 'pitch_freqRangeMin', value = 50)
      updateNumericInput(inputId = 'pitch_freqRangeMax', value = 500)
      updateTextInput(inputId = 'pitch_axisLabel', value = 'Frequency (mel)')
    } else if (input$pitch_scale == 'logarithmic') {
      shinyjs::hide('pitch_semitonesRe')
      updateNumericInput(inputId = 'pitch_freqRangeMin', value = 50)
      updateNumericInput(inputId = 'pitch_freqRangeMax', value = 500)
      updateTextInput(inputId = 'pitch_axisLabel', value = 'Frequency (log Hz)')
    } else if (input$pitch_scale == 'hz') {
      shinyjs::hide('pitch_semitonesRe')
      updateNumericInput(inputId = 'pitch_freqRangeMin', value = 50)
      updateNumericInput(inputId = 'pitch_freqRangeMax', value = 500)
      updateTextInput(inputId = 'pitch_axisLabel', value = 'Frequency (Hz)')
    }
  })

  observeEvent(input$pitch_floor, {
    updateNumericInput(inputId = 'pitch_timeStep',
                       value = 0.75 / input$pitch_floor)
  })

  observeEvent(input$formant_windowLength, {
    updateNumericInput(inputId = 'formant_timeStep',
                       value = 0.25 * input$formant_windowLength)
  })

  observeEvent(input$formant_maxN, {
    updateNumericInput(inputId = 'formant_number',
                       value = input$formant_maxN,
                       max = input$formant_maxN)
  })

  intensity_range <- reactive({
    if (is.na(input$intensity_rangeMin) & is.na(input$intensity_rangeMax)) {
      NULL
    } else {
      c(as.numeric(input$intensity_rangeMin),
        as.numeric(input$intensity_rangeMax))
    }
  })

  observeEvent(input$globalColor, {
    updateTextInput(inputId = 'wave_color', value = input$globalColor)
    updateTextInput(inputId = 'pitch_color', value = input$globalColor)
    updateTextInput(inputId = 'formant_color', value = input$globalColor)
    updateTextInput(inputId = 'intensity_color', value = input$globalColor)
    updateTextInput(inputId = 'tg_color', value = input$globalColor)
    updateTextInput(inputId = 'tg_focusTierColor', value = input$globalColor)
    updateTextInput(inputId = 'spec_colors',
                    value = paste(input$bgColor, input$globalColor, sep=','))
  })

  observeEvent(input$bgColor, {
    updateTextInput(inputId = 'spec_colors',
                    value = paste(input$bgColor, input$globalColor, sep=','))
  })

  observeEvent(input$highlightBool, {
    if(input$highlightBool) {
      shinyjs::show('highlightComp')
      shinyjs::show('highlightStart')
      shinyjs::show('highlightEnd')
      shinyjs::show('highlightCol')
      shinyjs::show('highlightTGbool')
    } else {
      shinyjs::hide('highlightComp')
      shinyjs::hide('highlightStart')
      shinyjs::hide('highlightEnd')
      shinyjs::hide('highlightCol')
      shinyjs::hide('highlightTGbool')
    }
  })

  observeEvent(input$highlightComp, {
    if (input$highlightComp %in% c('all', 'pitch', 'formant', 'intensity')) {
      shinyjs::show('highlightDrawSize')
      shinyjs::show('highlightSpeckleSize')
    } else {
      shinyjs::hide('highlightDrawSize')
      shinyjs::hide('highlightSpeckleSize')
    }
  })

  observeEvent(input$highlightTGbool, {
    if (input$highlightTGbool) {
      shinyjs::show('highlightTier')
      shinyjs::show('highlightLabel')
    } else {
      shinyjs::hide('highlightTier')
      shinyjs::hide('highlightLabel')
    }
  })

  observeEvent(input$arrowBool, {
    if(input$arrowBool) {
      shinyjs::show('arrowComp')
      shinyjs::show('arrowx0')
      shinyjs::show('arrowx1')
      shinyjs::show('arrowy0')
      shinyjs::show('arrowy1')
      shinyjs::show('arrowLength')
      shinyjs::show('arrowAngle')
      shinyjs::show('arrowColor')
      shinyjs::show('arrowLineType')
      shinyjs::show('arrowLineWidth')
    } else {
      shinyjs::hide('arrowComp')
      shinyjs::hide('arrowx0')
      shinyjs::hide('arrowx1')
      shinyjs::hide('arrowy0')
      shinyjs::hide('arrowy1')
      shinyjs::hide('arrowLength')
      shinyjs::hide('arrowAngle')
      shinyjs::hide('arrowColor')
      shinyjs::hide('arrowLineType')
      shinyjs::hide('arrowLineWidth')
    }
  })

  observeEvent(input$rectBool, {
    if (input$rectBool) {
      shinyjs::show('rectComp')
      shinyjs::show('rectxleft')
      shinyjs::show('rectybottom')
      shinyjs::show('rectxright')
      shinyjs::show('rectytop')
      shinyjs::show('rectFill')
      shinyjs::show('rectColor')
      shinyjs::show('rectLineType')
      shinyjs::show('rectLineWidth')
    } else {
      shinyjs::hide('rectComp')
      shinyjs::hide('rectxleft')
      shinyjs::hide('rectybottom')
      shinyjs::hide('rectxright')
      shinyjs::hide('rectytop')
      shinyjs::hide('rectFill')
      shinyjs::hide('rectColor')
      shinyjs::hide('rectLineType')
      shinyjs::hide('rectLineWidth')
    }
  })

  observeEvent(input$annotBool, {
    if (input$annotBool) {
      shinyjs::show('annotComp')
      shinyjs::show('annotx')
      shinyjs::show('annoty')
      shinyjs::show('annotLabel')
      shinyjs::show('annotColor')
      shinyjs::show('annotFace')
      shinyjs::show('annotSize')
    } else {
      shinyjs::hide('annotComp')
      shinyjs::hide('annotx')
      shinyjs::hide('annoty')
      shinyjs::hide('annotLabel')
      shinyjs::hide('annotColor')
      shinyjs::hide('annotFace')
      shinyjs::hide('annotSize')
    }
  })

  highlight_args <- reactive({
    if (!input$highlightBool | input$highlightComp != 'all') {
      return(NULL)
    } else {
      tmp <- list()
      tmp$start <- as.numeric(unlist(strsplit(input$highlightStart, ',')))
      tmp$end <- as.numeric(unlist(strsplit(input$highlightEnd, ',')))
      if (input$highlightTier != '') {
        tmp$tier <- input$highlightTier
        tmp$label <- input$highlightLabel
      }
      tmp$color <- unlist(strsplit(input$highlightCol, ','))
      tmp$drawSize <- as.numeric(
        unlist(strsplit(input$highlightDrawSize, ',')))
      tmp$speckleSize <- as.numeric(
        unlist(strsplit(input$highlightSpeckleSize, ',')))
      return(tmp)
    }
  })

  waveHighlight_args <- reactive({
    if (!input$highlightBool | input$highlightComp != 'wave') {
      return(NULL)
    } else {
      tmp <- list()
      tmp$start <- as.numeric(unlist(strsplit(input$highlightStart, ',')))
      tmp$end <- as.numeric(unlist(strsplit(input$highlightEnd, ',')))
      if (input$highlightTier != '') {
        tmp$tier <- input$highlightTier
        tmp$label <- input$highlightLabel
      }
      tmp$color <- unlist(strsplit(input$highlightCol, ','))
      return(tmp)
    }
  })

  specHighlight_args <- reactive({
    if (!input$highlightBool | input$highlightComp != 'spectrogram') {
      return(NULL)
    } else {
      tmp <- list()
      tmp$start <- as.numeric(unlist(strsplit(input$highlightStart, ',')))
      tmp$end <- as.numeric(unlist(strsplit(input$highlightEnd, ',')))
      if (input$highlightTier != '') {
        tmp$tier <- input$highlightTier
        tmp$label <- input$highlightLabel
      }
      tmp$colors <- unlist(strsplit(input$highlightCol, ','))
      return(tmp)
    }
  })

  pitchHighlight_args <- reactive({
    if (!input$highlightBool | input$highlightComp != 'pitch') {
      return(NULL)
    } else {
      tmp <- list()
      tmp$start <- as.numeric(unlist(strsplit(input$highlightStart, ',')))
      tmp$end <- as.numeric(unlist(strsplit(input$highlightEnd, ',')))
      if (input$highlightTier != '') {
        tmp$tier <- input$highlightTier
        tmp$label <- input$highlightLabel
      }
      tmp$color <- unlist(strsplit(input$highlightCol, ','))
      tmp$drawSize <- as.numeric(
        unlist(strsplit(input$highlightDrawSize, ',')))
      tmp$speckleSize <- as.numeric(
        unlist(strsplit(input$highlightSpeckleSize, ',')))
      return(tmp)
    }
  })

  formantHighlight_args <- reactive({
    if (!input$highlightBool | input$highlightComp != 'formant') {
      return(NULL)
    } else {
      tmp <- list()
      tmp$start <- as.numeric(unlist(strsplit(input$highlightStart, ',')))
      tmp$end <- as.numeric(unlist(strsplit(input$highlightEnd, ',')))
      if (input$highlightTier != '') {
        tmp$tier <- input$highlightTier
        tmp$label <- input$highlightLabel
      }
      tmp$color <- unlist(strsplit(input$highlightCol, ','))
      tmp$drawSize <- as.numeric(
        unlist(strsplit(input$highlightDrawSize, ',')))
      tmp$speckleSize <- as.numeric(
        unlist(strsplit(input$highlightSpeckleSize, ',')))
      return(tmp)
    }
  })

  intensityHighlight_args <- reactive({
    if (!input$highlightBool | input$highlightComp != 'intensity') {
      return(NULL)
    } else {
      tmp <- list()
      tmp$start <- as.numeric(unlist(strsplit(input$highlightStart, ',')))
      tmp$end <- as.numeric(unlist(strsplit(input$highlightEnd, ',')))
      if (input$highlightTier != '') {
        tmp$tier <- input$highlightTier
        tmp$label <- input$highlightLabel
      }
      tmp$color <- unlist(strsplit(input$highlightCol, ','))
      tmp$drawSize <- as.numeric(
        unlist(strsplit(input$highlightDrawSize, ',')))
      return(tmp)
    }
  })

  arrow_args <- reactive({
    if (!input$arrowBool) {
      return(NULL)
    } else {
      n_args <- length(unlist(strsplit(input$arrowComp, ',')))
      tmp <- list()
      arrowLength <- as.numeric(unlist(strsplit(input$arrowLength, ',')))
      if (length(arrowLength) != n_args) arrowLength <-
        rep(arrowLength[1], n_args)
      arrowAngle <- as.numeric(unlist(strsplit(input$arrowAngle, ',')))
      if (length(arrowAngle) != n_args) arrowAngle <-
        rep(arrowAngle[1], n_args)
      arrowColor <- unlist(strsplit(input$arrowColor, ','))
      if (length(arrowColor) != n_args) arrowColor <-
        rep(arrowColor[1], n_args)
      arrowLineType <- unlist(strsplit(input$arrowLineType, ','))
      if (length(arrowLineType) != n_args) arrowLineType <-
        rep(arrowLineType[1], n_args)
      arrowLineWidth <- as.numeric(unlist(strsplit(input$arrowLineWidth, ',')))
      if (length(arrowLineWidth) != n_args) arrowLineWidth <-
        rep(arrowLineWidth[1], n_args)
      for (i in 1:n_args) {
        tmp[[i]] <- c(
          unlist(strsplit(input$arrowComp, ','))[i],
          x0 = as.numeric(unlist(strsplit(input$arrowx0, ',')))[i],
          x1 = as.numeric(unlist(strsplit(input$arrowx1, ',')))[i],
          y0 = as.numeric(unlist(strsplit(input$arrowy0, ',')))[i],
          y1 = as.numeric(unlist(strsplit(input$arrowy1, ',')))[i],
          length = arrowLength[i],
          angle = arrowAngle[i],
          col = arrowColor[i],
          lty = arrowLineType[i],
          lwd = arrowLineWidth[i]
        )
      }
      return(tmp)
    }
  })

  rect_args <- reactive({
    if (!input$rectBool) {
      return(NULL)
    } else {
      n_args <- length(unlist(strsplit(input$rectComp, ',')))
      tmp <- list()
      rectLength <- as.numeric(unlist(strsplit(input$arrowLength, ',')))
      rectFill <- unlist(strsplit(input$rectFill, ','))
      if (length(rectFill) != n_args) rectFill <-
        rep(rectFill[1], n_args)
      rectColor <- unlist(strsplit(input$rectColor, ','))
      if (length(rectColor) != n_args) rectColor <-
        rep(rectColor[1], n_args)
      rectLineType <- unlist(strsplit(input$rectLineType, ','))
      if (length(rectLineType) != n_args) rectLineType <-
        rep(rectLineType[1], n_args)
      rectLineWidth <- as.numeric(unlist(strsplit(input$rectLineWidth, ',')))
      if (length(rectLineWidth) != n_args) rectLineWidth <-
        rep(rectLineWidth[1], n_args)
      for (i in 1:n_args) {
        tmp[[i]] <- c(
          unlist(strsplit(input$rectComp, ','))[i],
          xleft = as.numeric(unlist(strsplit(input$rectxleft, ',')))[i],
          ybottom = as.numeric(unlist(strsplit(input$rectybottom, ',')))[i],
          xright = as.numeric(unlist(strsplit(input$rectxright, ',')))[i],
          ytop = as.numeric(unlist(strsplit(input$rectytop, ',')))[i],
          col = rectFill[i],
          border = rectColor[i],
          lwd = rectLineWidth[i],
          lty = rectLineType[i]
        )
      }
      return(tmp)
    }
  })

  annot_args <- reactive({
    if (!input$annotBool) {
      return(NULL)
    } else {
      n_args <- length(unlist(strsplit(input$annotComp, ',')))
      tmp <- list()
      annotLabel <- unlist(strsplit(input$annotLabel, ';'))
      annotColor <- unlist(strsplit(input$annotColor, ','))
      if (length(annotColor) != n_args) annotColor <-
        rep(annotColor, n_args)
      annotFace <- as.numeric(unlist(strsplit(input$annotFace, ',')))
      if (length(annotFace) != n_args) annotFace <-
        rep(annotFace, n_args)
      annotSize <- as.numeric(unlist(strsplit(input$annotSize, ',')))
      if (length(annotSize) != n_args) annotSize <-
        rep(annotSize, n_args)
      for (i in 1:n_args) {
        tmp[[i]] <- c(
          unlist(strsplit(input$annotComp, ','))[i],
          x = as.numeric(unlist(strsplit(input$annotx, ',')))[i],
          y = as.numeric(unlist(strsplit(input$annoty, ',')))[i],
          label = annotLabel[i],
          col = annotColor[i],
          font = annotFace[i],
          cex = annotSize[i]
        )
      }
      return(tmp)
    }
  })

  make_fig <- function(outputFile) {
    req(input$sound)
    if ('TextGrid' %in% input$frames) req(input$tg_file)
    grDevices::png(outputFile,
                   width=as.numeric(input$figWidth),
                   height=as.numeric(input$figHeight),
                   res=as.numeric(input$figDPI),
                   pointsize=input$fontSize, units='cm')
    praatpicture::praatpicture(input$sound$datapath,
                               start = as.numeric(input$start),
                               end = as.numeric(input$end),
                               tfrom0 = input$tfrom0,
                               tUnit = input$tUnit,
                               frames = input$frames,
                               proportion = as.numeric(unlist(strsplit(
                                 input$proportion, ','
                               ))),
                               mainTitle = input$mainTitle,
                               mainTitleAlignment = input$mainTitleAlignment,
                               start_end_only = input$start_end_only,
                               min_max_only = input$min_max_only,
                               time_axisLabel = input$time_axisLabel,
                               speckleSize = input$speckleSize,
                               drawSize = input$drawSize,
                               wave_color = input$wave_color,
                               wave_channels = wave_channels(),
                               wave_channelNames = channelNames(),
                               wave_lineWidth = input$wave_lineWidth,
                               wave_highlight = waveHighlight_args(),
                               tg_file = input$tg_file$datapath,
                               tg_tiers = tg_tiers(),
                               tg_focusTier = tg_focusTier(),
                               tg_focusTierColor = unlist(strsplit(
                                 input$tg_focusTierColor, ',')),
                               tg_focusTierLineType = unlist(strsplit(
                                 input$tg_focusTierLineType, ',')),
                               tg_tierNames = input$tg_tierNames,
                               tg_alignment = unlist(strsplit(
                                 input$tg_alignment, ',')),
                               tg_specialChar = input$tg_specialChar,
                               tg_color = unlist(strsplit(
                                 input$tg_color, ',')),
                               spec_freqRange = c(
                                 as.numeric(input$spec_freqRangeMin),
                                 as.numeric(input$spec_freqRangeMax)),
                               spec_windowLength =
                                 as.numeric(input$spec_windowLength),
                               spec_dynamicRange =
                                 as.numeric(input$spec_dynamicRange),
                               spec_timeStep =
                                 as.numeric(input$spec_timeStep),
                               spec_windowShape = input$spec_windowShape,
                               spec_colors = unlist(strsplit(
                                 input$spec_colors, ',')),
                               spec_axisLabel = input$spec_axisLabel,
                               spec_channel = spec_channel(),
                               spec_highlight = specHighlight_args(),
                               pitch_plotType = input$pitch_plotType,
                               pitch_scale = input$pitch_scale,
                               pitch_semitonesRe = input$pitch_semitonesRe,
                               pitch_freqRange = c(
                                 as.numeric(input$pitch_freqRangeMin),
                                 as.numeric(input$pitch_freqRangeMax)),
                               pitch_timeStep = as.numeric(input$pitch_timeStep),
                               pitch_floor = as.numeric(input$pitch_floor),
                               pitch_ceiling = as.numeric(input$pitch_ceiling),
                               pitch_color = unlist(strsplit(
                                 input$pitch_color, ',')),
                               pitch_axisLabel = input$pitch_axisLabel,
                               pitch_plotOnSpec = input$pitch_plotOnSpec,
                               pitch_highlight = pitchHighlight_args(),
                               formant_plotType = input$formant_plotType,
                               formant_freqRange = c(
                                 as.numeric(input$formant_freqRangeMin),
                                 as.numeric(input$formant_freqRangeMax)),
                               formant_dynamicRange = as.numeric(
                                 input$formant_dynamicRange),
                               formant_windowLength = as.numeric(
                                 input$formant_windowLength),
                               formant_timeStep = as.numeric(
                                 input$formant_timeStep),
                               formant_maxN = input$formant_maxN,
                               formant_color = unlist(strsplit(
                                 input$formant_color, ',')),
                               formant_number = input$formant_number,
                               formant_axisLabel = input$formant_axisLabel,
                               formant_plotOnSpec = input$formant_plotOnSpec,
                               formant_highlight = formantHighlight_args(),
                               intensity_range = intensity_range(),
                               intensity_minPitch = as.numeric(
                                 input$intensity_minPitch),
                               intensity_color = unlist(strsplit(
                                 input$intensity_color, ',')),
                               intensity_axisLabel = input$intensity_axisLabel,
                               intensity_plotOnSpec = input$intensity_plotOnSpec,
                               intensity_highlight = intensityHighlight_args(),
                               font = as.numeric(input$fontFace),
                               font.axis = as.numeric(input$fontFace),
                               family = input$fontFamily,
                               col = input$globalColor,
                               col.axis = input$globalColor,
                               bg = input$bgColor,
                               highlight = highlight_args(),
                               draw_arrow = arrow_args(),
                               draw_rectangle = rect_args(),
                               annotate = annot_args()
    )
    dev.off()
    list(src = outputFile,
         width = as.numeric(input$figWidth) * 40,
         height = as.numeric(input$figHeight) * 40)
  }

  output$plot <- renderImage({make_fig(tempfile(fileext='.png'))}, deleteFile = TRUE)
  output$download <- downloadHandler(
    filename = function() {'output.png'},
    content = function(file) {make_fig(file)}
  )

}
