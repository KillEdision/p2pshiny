shinyServer(function(input, output,session) {
  q <- observe({
    if (input$quit == 1) {
      stopApp()
    }
  })

  output$loan_dim <- renderDataTable({
    loan_dim[input$loan_dim_variable]
  })

  output$user_dim <- renderDataTable({
    user_dim[input$user_dim_variable]
  })

  output$fact <- renderDataTable({
    fact[input$fact_variable]
  })

  output$data <- renderDataTable({
    melteddata[input$melt_variable]
  })

  output$melteddata <- renderPlot({
    tableplot(melteddata)
  })

  output$x_var_select <- renderUI({
    selectInput("x_var", "X variable",
                names(melteddata), names(melteddata)[4])
  })

  output$y_var_select <- renderUI({
    selectInput("y_var","Y variable",
                names(melteddata), names(melteddata)[5])
  })

  fontSize <- reactive({
    if (is.null(input$font_size)) {
      0
    } else {
      input$font_size
    }
  })

  size <- reactive({
    if (is.null(input$size)) {
      1
    } else {
      input$size
    }
  })

  output$extraplot <- renderPlot({
    if (is.null(input$x_var) || is.null(input$y_var) ||
        !input$x_var %in% names(melteddata) ||
        !input$y_var %in% names(melteddata)) {
      return(NULL)
    }

    p <-
      ggplot(melteddata, aes_string(input$x_var, input$y_var)) +
      geom_point() +
      theme_bw(fontSize())

    if (input$xtrans == "log") {
      p <- p + scale_x_log10()
    } else if (input$xtrans == "reverse") {
      p <- p + scale_x_reverse()
    }
    if (input$ytrans == "log") {
      p <- p + scale_y_log10()
    } else if (input$ytrans == "reverse") {
      p <- p + scale_y_reverse()
    }

    if (input$show_marginal) {
      p <- ggExtra::ggMarginal(
        p,
        type = input$extratype,
        margins = input$margins,
        size = size(),
        col = input$col,
        fill = input$fill
      )
    }
    p
  })

  output$describe <- renderPrint({
    cat(
      '\n',
      '====================================================================================================================','\n'
    )
    print(summary(melteddata[,1:5]))
    cat(
      '\n',
      '====================================================================================================================','\n'
    )
    print(summary(melteddata[,6:10]))
    cat(
      '\n',
      '====================================================================================================================','\n'
    )
    print(summary(melteddata[,11:15]))
    cat(
      '\n',
      '====================================================================================================================','\n'
    )
  })

  output$gg_x_var_select <- renderUI({
    selectInput("gg_x_var","X variable",
                names(melteddata), names(melteddata)[1])
  })

  output$gg_y_var_select <- renderUI({
    selectInput("gg_y_var","Y variable",
                names(melteddata), names(melteddata)[4])
  })

  output$gg_col_var_select <- renderUI({
    selectInput("gg_col_var","Color variable",
                names(melteddata), names(melteddata)[15])
  })

  output$ggally_plot <- renderPlot({
    if (input$ggallytype == 'box') {
      ggally_box(
        melteddata,
        mapping = ggplot2::aes_string(
          x = input$gg_x_var,y = input$gg_y_var,color = input$gg_col_var
        ),
        outlier.colour = "red",
        outlier.shape = 13,
        outlier.size = 6
      )
    }
    else if (input$ggallytype == 'density') {
      ggally_density(
        melteddata,
        mapping = ggplot2::aes_string(
          x = input$gg_x_var,y = input$gg_y_var, fill = "..level.."
        )
      ) + ggplot2::scale_fill_gradient(breaks = c(0.05, 0.1,0.15,0.2))
    }
    else if (input$ggallytype == 'denstrip') {
      ggally_denstrip(
        melteddata,
        mapping = ggplot2::aes_string(
          x = input$gg_x_var,y = input$gg_y_var, binwidth = "0.2"
        )
      ) + ggplot2::scale_fill_gradient(low = "grey80", high = "black")
    }
    else if (input$ggallytype == 'dot') {
      ggally_dot(
        melteddata,
        mapping = ggplot2::aes_string(
          x = input$gg_x_var,y = input$gg_y_var,color = input$gg_col_var
        )
      ) + ggplot2::scale_shape(solid = FALSE)
    }
    else if (input$ggallytype == 'point') {
      ggally_points(
        melteddata,
        mapping = ggplot2::aes_string(
          x = input$gg_x_var,y = input$gg_y_var,color = input$gg_col_var,size = "yq"
        )
      )
    }
    else if (input$ggallytype == 'ggpairs') {
      ggpairs(melteddata[,c(5,13,15)])
    }
    else if (input$ggallytype == 'ggscatmat') {
      ggscatmat(melteddata, columns = c(4,5,13), color = "yq")
    }

  })

  randforest <- eventReactive(eventExpr = input$calrand,valueExpr = {
    sample_order <-
      sample(1:nrow(melteddata),floor(nrow(melteddata) * input$trainset_size))
    train <- melteddata[sample_order,]
    test <- melteddata[-sample_order,]
    train <- train[which(train$yq != 'unknow'),]
    train$yq <- factor(train$yq,levels = c('ok','fraud'))
    train <- as.data.frame(train)
    test <- as.data.frame(test)

    rf_model <- randomForest(
      x = train[,1:14],
      y = train[,15],
      ntree = input$ntree,
      mtry = input$mtry,
      replace = if (input$replace == 'NO')
        FALSE
      else
        TRUE,
      importance = if (input$importance == 'NO')
        FALSE
      else
        TRUE,
      proximity = if (input$proximity == 'NO')
        FALSE
      else
        TRUE,
      norm.votes = TRUE,
      do.trace = FALSE,
      oob.prox = if (input$oob.prox == 'NO')
        FALSE
      else
        TRUE,
      keep.forest = if (input$keep.forest == 'NO')
        FALSE
      else
        TRUE,
      corr.bias = FALSE,
      keep.inbag = FALSE
    )
    ###########################################################################
    preds <-
      predict(
        rf_model,test[,1:14],type = 'prob',norm.votes = T,predict.all = T
      )
    pre <- as.data.frame(preds$aggregate)
    or <- order(pre[,2],decreasing = TRUE)
    test <- test[or,15]
    pre <- pre[or,2]
    pre <- data.frame(pre,test)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## predictions,labels
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    predictions <- pre[,1]
    labels <- ifelse(pre[,2] == 'fraud',1,0)
    pred <- prediction(predictions,labels)
    pr_Curve <- performance(pred,'prec','rec')
    pr_Curve@y.values <-
      lapply(pr_Curve@y.values,function(x)
        rev(cummax(rev(x))))
    pr_Curve_data <-
      data.frame(
        as.data.frame(pr_Curve@x.values),as.data.frame(pr_Curve@y.values),row.names = NULL
      )
    names(pr_Curve_data) <- c('x','y')
    gg_pr_Curve  <-
      ggplot(data = pr_Curve_data,mapping = aes(x = x,y = y))
    gg_pr_Curve <-
      gg_pr_Curve + geom_line(colour = I('steelblue'),size = I(1.1))
    gg_pr_Curve <-
      gg_pr_Curve + labs(x = 'Recall',y = 'Precision',title = 'Precision/Recall Curve')
    gg_pr_Curve <-
      gg_pr_Curve + theme(
        plot.title = element_text(size = 15,face = 'bold'),axis.title.x = element_text(size =
                                                                                         15),axis.title.y = element_text(size = 15)
      )
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Cumulative_curve
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Cumulative_curve <- performance(pred,'rec','rpp')
    Cumulative_curve_data <-
      data.frame(
        as.data.frame(Cumulative_curve@x.values),as.data.frame(Cumulative_curve@y.values),row.names = NULL
      )
    names(Cumulative_curve_data) <- c('x','y')
    gg_Cumulative_curve  <-
      ggplot(data = Cumulative_curve_data,mapping = aes(x = x,y = y))
    gg_Cumulative_curve <-
      gg_Cumulative_curve + geom_line(colour = I('steelblue'),size = I(1.1))
    gg_Cumulative_curve <-
      gg_Cumulative_curve + labs(x = 'Rate of positive predictions',y = 'Recall',title =
                                   'Cumulative Recall Curve')
    gg_Cumulative_curve <-
      gg_Cumulative_curve + theme(
        plot.title = element_text(size = 15,face = 'bold'),axis.title.x = element_text(size =
                                                                                         15),axis.title.y = element_text(size = 15)
      )
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## ROC_curve
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    roc_curve <- performance(pred,'tpr','fpr')
    roc_curve_data <-
      data.frame(
        as.data.frame(roc_curve@x.values),as.data.frame(roc_curve@y.values),row.names = NULL
      )
    names(roc_curve_data) <- c('x','y')
    gg_roc_curve  <-
      ggplot(data = roc_curve_data,mapping = aes(x = x,y = y))
    gg_roc_curve <-
      gg_roc_curve + geom_line(colour = I('steelblue'),size = I(1.1))
    gg_roc_curve <-
      gg_roc_curve + labs(x = 'False positive rate',y = 'True positive rate',title =
                            'ROC Curve')
    gg_roc_curve <-
      gg_roc_curve + theme(
        plot.title = element_text(size = 15,face = 'bold'),axis.title.x = element_text(size =
                                                                                         15),axis.title.y = element_text(size = 15)
      )
    #print(gg_roc_curve)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Lift_curve
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    lift_curve <- performance(pred,'lift','rpp')
    lift_curve_data <-
      data.frame(
        as.data.frame(lift_curve@x.values),as.data.frame(lift_curve@y.values),row.names = NULL
      )
    names(lift_curve_data) <- c('x','y')
    gg_lift_curve  <-
      ggplot(data = lift_curve_data,mapping = aes(x = x,y = y))
    gg_lift_curve <-
      gg_lift_curve + geom_line(colour = I('steelblue'),size = I(1.1))
    gg_lift_curve <-
      gg_lift_curve + labs(x = 'Rate of positive predictions',y = 'Lift value',title =
                             'Lift Curve')
    gg_lift_curve <-
      gg_lift_curve + theme(
        plot.title = element_text(size = 15,face = 'bold'),axis.title.x = element_text(size =
                                                                                         15),axis.title.y = element_text(size = 15)
      )
    return(
      list(
        rf_model = rf_model,call = rf_model$call,confusion = rf_model$confusion,importance =
          rf_model$importance,
        gg_pr_Curve = gg_pr_Curve,gg_Cumulative_curve = gg_Cumulative_curve,gg_roc_curve =
          gg_roc_curve,gg_lift_curve = gg_lift_curve
      )
    )
  })



  output$randsummary <- renderPrint({
    rand <<- randforest()
    rf_model <- rand['rf_model']

    str(rf_model)
    cat(
      '\n','=====================================================================================================================','\n'
    )
    print(rand$call)
    cat(
      '\n','=====================================================================================================================','\n'
    )
    print(rand$confusion)
    cat(
      '\n','=====================================================================================================================','\n'
    )
    print(rand$importance)
    cat(
      '\n','=====================================================================================================================','\n'
    )
  })


  gg_pr_Curve <-
    eventReactive(eventExpr = input$calpr,valueExpr = {
      rand['gg_pr_Curve']
    })

  gg_Cumulative_curve <-
    eventReactive(eventExpr = input$calc,valueExpr = {
      rand['gg_Cumulative_curve']
    })

  gg_roc_curve <-
    eventReactive(eventExpr = input$calroc,valueExpr = {
      rand['gg_roc_curve']
    })

  gg_lift_curve <-
    eventReactive(eventExpr = input$callift,valueExpr = {
      rand['gg_lift_curve']
    })


  output$gg_pr_Curve <- renderPlot({
    print(gg_pr_Curve())
  })

  output$gg_Cumulative_curve <- renderPlot({
    print(gg_Cumulative_curve())
  })

  output$gg_roc_curve <- renderPlot({
    print(gg_roc_curve())
  })

  output$gg_lift_curve <- renderPlot({
    print(gg_lift_curve())
  })


  output$download <- downloadHandler(
    filename = function() {
      paste('RandomForest', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    content = function(file) {
      out <- render('www/RandomForest.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )

  output$downloadmeasure <- downloadHandler(
    filename = function() {
      paste('performance measure', sep = '.', switch(
        input$format2, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    content = function(file) {
      out <- render('www/performance_measures.Rmd', switch(
        input$format2,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )

  output$leaflet <- renderLeaflet({
    if (input$leafletitem == 'address') {
      leaflet(chengshi) %>% addTiles() %>%  addCircles(
        lng = ~ lng, lat = ~ lat, weight = 1,radius = ~ pop * 300,popup = ~ province,color = rgb(t(col2rgb(palette(
        ))) / 255)
      )
    }
    else{
      leaflet(jiguan) %>% addTiles() %>%  addCircles(
        lng = ~ lng, lat = ~ lat, weight = 1,radius = ~ pop * 300,popup = ~ province,color = rgb(t(col2rgb(palette(
        ))) / 255)
      )
    }

  })


  tree <- eventReactive(eventExpr = input$getTree,valueExpr = {
    rf_model <- rand['rf_model']
    getT <- getTree(rfobj = rf_model,k = input$getTree,labelVar=TRUE)
    list(tree=getT)
  })

  output$getTree <- renderPrint({
    tree()
  })

})