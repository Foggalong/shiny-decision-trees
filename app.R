#!/usr/bin/env R

# Copyright (c) 2020 Joshua H. Fogg
# This file is distributed under the MIT license and provided without
# warranty. It can be found along with the documentation and spambase
# dataset at https://github.com/Foggalong/shiny-decision-trees

library(methods)       # large scale analytics
library(rpart)         # creating decision trees
library(rpart.plot)    # plotting decision trees
library(shiny)         # user interface creation
library(shinyWidgets)  # better Shiny UI widgets



# VARIABLE LISTS
# These are the lists of variables from the UCI data and maps for how they
# link into the apps user interface. The number to the right is the column
# number in the original data set.

word_list = list(
    "make"       = "word_freq_make",        # 1
    "address"    = "word_freq_address",     # 2
    "all"        = "word_freq_all",         # 3
    "3d"         = "word_freq_3d",          # 4
    "our"        = "word_freq_our",         # 5
    "over"       = "word_freq_over",        # 6
    "remove"     = "word_freq_remove",      # 7
    "internet"   = "word_freq_internet",    # 8
    "order"      = "word_freq_order",       # 9
    "mail"       = "word_freq_mail",        # 10
    "receive"    = "word_freq_receive",     # 11
    "will"       = "word_freq_will",        # 12
    "people"     = "word_freq_people",      # 13
    "report"     = "word_freq_report",      # 14
    "addresses"  = "word_freq_addresses",   # 15
    "free"       = "word_freq_free",        # 16
    "business"   = "word_freq_business",    # 17
    "email"      = "word_freq_email",       # 18
    "you"        = "word_freq_you",         # 19
    "credit"     = "word_freq_credit",      # 20
    "your"       = "word_freq_your",        # 21
    "font"       = "word_freq_font",        # 22
    "000"        = "word_freq_000",         # 23
    "money"      = "word_freq_money",       # 24
    "hp"         = "word_freq_hp",          # 25
    "hpl"        = "word_freq_hpl",         # 26
    "george"     = "word_freq_george",      # 27
    "650"        = "word_freq_650",         # 28
    "lab"        = "word_freq_lab",         # 29
    "labs"       = "word_freq_labs",        # 30
    "telnet"     = "word_freq_telnet",      # 31
    "857"        = "word_freq_857",         # 32
    "data"       = "word_freq_data",        # 33
    "415"        = "word_freq_415",         # 34
    "85"         = "word_freq_85",          # 35
    "technology" = "word_freq_technology",  # 36
    "1999"       = "word_freq_1999",        # 37
    "parts"      = "word_freq_parts",       # 38
    "pm"         = "word_freq_pm",          # 39
    "direct"     = "word_freq_direct",      # 40
    "cs"         = "word_freq_cs",          # 41
    "meeting"    = "word_freq_meeting",     # 42
    "original"   = "word_freq_original",    # 43
    "project"    = "word_freq_project",     # 44
    "re"         = "word_freq_re",          # 45
    "edu"        = "word_freq_edu",         # 46
    "table"      = "word_freq_table",       # 47
    "conference" = "word_freq_conference"   # 48
)
char_list = list(
    ";" = "char_freq_semicolon",   # 49
    "(" = "char_freq_prenthesis",  # 50
    "[" = "char_freq_bracket",     # 51
    "!" = "char_freq_bang",        # 52
    "$" = "char_freq_dollar",      # 53
    "#" = "char_freq_hash"         # 54
)
caps_list = list(
    "Average capital sequence length" = "capital_run_length_average",  # 55
    "Longest capital sequence length" = "capital_run_length_longest",  # 56
    "Total number of capital letters" = "capital_run_length_total"     # 57
)
# Variable 58 is the true spam classification boolean

# these are the variables included in the default model, chosen as to give
# not unreasonable but also not amazing results to start students off.
word_list_default = list(
    "word_freq_your",
    "word_freq_george",
    "word_freq_edu"
)
char_list_default = list(
    "char_freq_prenthesis"
)
caps_list_default = list(
    "capital_run_length_total"
)



# MATHEMATICS FUNCTIONS
# The functions carry out the actual heavy lifting of the app. This includes
# working out the actual decision tree but also calculating the accuracy,
# specificity, and more.

makeTree = function(model_vars, min_split, min_bucket, max_depth) {
    # Takes a list of model variables (strings), a minimum split parameter
    # (int), a minimum bucket size parameter (int), and a maximum tree depth
    # parameter (int) as inputs and then returns an rpart classification tree
    # created with those parameters using Gini-indexes without any pruning.

    train_dat = read.csv(file = "training-data.csv", header = TRUE, sep = ",")
    # create an rpart compatible formula for the model from the chosen vars
    f = paste("true_spam_bool ~ ", paste(model_vars, collapse = " + "))
    # rpart is performs the split calculations and returns the tree
    tree = rpart(
        as.formula(f),
        method = "class",  # sets it up as a classification problem
        data = train_dat,
        parms = list(split = "gini"),  # ensures rpart uses gini indexes
        minsplit = min_split,
        minbucket = min_bucket,
        maxdepth = max_depth,
        cp = 0  # complexity parameter, at zero prevents pruning on branches
    )

    return(tree)
}


useTree = function(tree, filename) {
    # Takes a tree generated by rpart and a filename (string) as input and
    # then predicts the labels of the data in that file using the tree. It
    # returns a dataframe with two bool (0,1) columns: prediction and truth.

    data = read.csv(file = filename, header = TRUE, sep = ",")
    prediction = predict(tree, data, type = "class")
    results = as.data.frame(prediction)
    results$truth = data$true_spam_bool

    return(results)
}


calcScores = function(results) {
    # Takes a results dataframe as input and then calculates scores for
    # accuracy, true negative rate, and true positive rate. It returns a
    # list of formatted strings detailing these results.

    results = table(results)
    # calculate the scores on which we'll judge our model to 2 decimal places
    accuracy = round(100 * (results[1] + results[4]) / sum(results), 2)
    true_neg = round(100 * results[1] / sum(results[1, ]), 2)
    true_pos = round(100 * results[4] / sum(results[2, ]), 2)

    # the collapse argument removes the spacing which would otherwise be there
    return(list(
        paste(c("Overall Accuracy: ",   accuracy, "%"), collapse = ""),
        paste(c("True Positive Rate: ", true_pos, "%"), collapse = ""),
        paste(c("True Negative Rate: ", true_neg, "%"), collapse = "")
    ))
}


resultsTable = function(results) {
    # Takes a results dataframe as input and then reconstructs and returns
    # a dataframe which has a similar layout to the command line interface
    # output of R's table(...) function.

    data = table(results)
    Outcomes = c("Predicted Not Spam", "Predicted Spam", "Total")
    # reconstruct the columns of R's table(...) CLI display
    c1 = c(data[, 1], sum(data[, 1]))  # data[, 1] is a length 2 vector
    c2 = c(data[, 2], sum(data[, 1]))  # data[, 2] is a length 2 vector
    c3 = c(sum(data[, 1]), sum(data[2, ]), sum(data))

    # turn these columns back into a dataframe but with proper headers
    output = data.frame(Outcomes)
    output$"Actually Not Spam" = c1
    output$"Actually Spam"     = c2
    output$"Total"             = c3

    return(output)
}


addID = function(df) {
    # Short utility function which takes a dataframe as input and then
    # returns an initial ID column which just contains the row number.
    df = cbind(ID = seq_len(nrow(df)), df)
    return(df)
}



# SERVER LOGIC FUNCTION
# This function takes the output of the mathematical functions above and
# processes them to be displayed as part of the user interface function.

server = function(input, output, session) {
    # INPUT EVENT REACTIONS
    # reconstruct the tree every time createModel is pressed
    tree = eventReactive(
        eventExpr = input$createModel,
        valueExpr = makeTree(
            model_vars = c(input$wordfreq, input$charfreq, input$capsstats),
            input$min_split, input$min_bucket, input$max_depth
        )
    )
    # regenerate training results every time createModel is pressed
    training_results = eventReactive(
        eventExpr = input$createModel,
        valueExpr = useTree(tree(), "training-data.csv")
    )
    # regenerate test results every time createModel is pressed
    test_results = eventReactive(
        eventExpr = input$testModel,
        valueExpr = useTree(tree(), "test-data.csv")
    )

    # OUTPUT DISPLAY PREP
    # assessment scores are each collapsed to display on a new line
    output$training_scores = renderText(
        paste(calcScores(training_results()), collapse = "\n")
    )
    output$test_scores = renderText(
        paste(calcScores(test_results()), collapse = "\n")
    )

    # tables of outcome breakdows are static widgets
    output$training_table = renderTable(
        resultsTable(training_results()),
        align = "lccc",  # left-align first column, centre rest
        striped = TRUE
    )
    output$test_table = renderTable(
        resultsTable(test_results()),
        align = "lccc",  # left-align first column, centre rest
        striped = TRUE
    )

    # frame for a plot of the decision tree
    output$tree_plot = renderPlot(
        # prp takes an output from rpart and plots it (literally Plot RPart)
        prp(
            tree(), roundint = FALSE,
            # neaten up the nodes and edges, remove detailed labels
            extra = 0, branch = 0, varlen = 0,
            # colours spam terminals in red, non-spam terminals in blue
            box.col = c("cornflowerblue", "tomato")[tree()$frame$yval]
        )
    )

    # classification results with ID for each email in data(frame) tables
    output$training_data = renderDataTable(
        addID(training_results()),
        # they're for ID lookup, not browsing results, so cap entries at 5
        options = c(pageLength = 5)
    )
    output$test_data = renderDataTable(
        addID(test_results()),
        # they're for ID lookup, not browsing results, so cap entries at 5
        options = c(pageLength = 5)
    )
}



# USER INTERFACE FUNCTION
# This section handles setting out the user interface of the Shiny app,
# all of the text which goes around the page, and displaying the outputs
# of the server logic function.

ui = fluidPage(
    # top level introductory paragraph
    titlePanel("Email Spam Filtering"),
    helpText(
        "Welcome to the worksheet Shiny app! We recommend accessing it on",
        "desktop or tablet but it should work fine on mobile also. The",
        "notation it uses throughout aligns with that used in the lecture",
        "and on the worksheet so check there if you're not sure what a",
        "particular term or symbol means. If you are having any issues",
        tags$a(
            href = "mailto:j.fogg@sms.ed.ac.uk",
            "drop us an email"
        ),
        "and we'll get back to you as soon as possible.",
        br(), br(),
        "Here you can have a play with some of the concepts which we discussed",
        "in the lecture. Simply press the big blue button to generate the",
        "model and see how well it does. Try adding and removing features",
        "and changing the decision tree hyperparameters to see how they affect",
        "the results. Once you're happy with your results from the training",
        "data press the big red button to run your model with the test data.",
        br(), br(),
        "While it's not needed for the problem sheet, if you're technically",
        "minded the source code for this webpage is",
        tags$a(
            href = "https://github.com/Foggalong/shiny-decision-trees",
            "available on GitHub!"
        ),
        "Both the interface and the underlying analysis are written in",
        tags$a(
            href = "https://en.wikipedia.org/wiki/R_(programming_language)",
            "the R programming language"
        ),
        "which is the industry standard for statistical analysis. You can",
        "find out more information about that in the source code repository",
        "too but you will no doubt come across it again during your degree."
    ),
    br(),

    # partition the rest of the page into controls and output
    sidebarLayout(
        sidebarPanel(
            h2("The Controls"),
            br(),

            actionButton(
                inputId = "createModel",
                label = "Create Model",
                class = "btn-primary"  # makes it blue!
            ),
            helpText(
                "Pressing this blue button creates a decision tree model for",
                "the training data, based on the features you've selected and",
                "the chosen hyperparameter values. Use this while you're still",
                "developing your model; if you change the selected features",
                "just press it again to regenerate the model."
            ),
            br(),
            actionButton(
                inputId = "testModel",
                label = "Test Model",
                class = "btn-danger"  # makes it red!
            ),
            helpText(
                "Confident with your model? Excellent! Press the above red",
                "button to run it on the test data. Only use this once you're",
                "happy and ready to assess your model."
            ),
            br(),

            h3("Model Features"),
            helpText(
                "The below controls allow you to add and remove specific",
                "features from your spam detection model. Have a play with",
                "them and see if you can find which work better than others."
            ),
            br(),
            h4("Word Frequencies"),
            helpText(
                "These features all pertain to how many times a given word",
                "appears in the email. Use the dropdown picker to add and",
                "remove them from your model."
            ),
            pickerInput(
                inputId = "wordfreq",
                label = NULL,  # label given in outer code
                choices = word_list,
                selected = word_list_default,
                options = list(`actions-box` = TRUE),
                multiple = TRUE
            ),
            br(),
            h4("Character Frequencies"),
            helpText(
                "These features all pertain to how many times a given",
                "character appears in the email. Use the dropdown picker to",
                "add and remove them from your model."
            ),
            pickerInput(
                inputId = "charfreq",
                label = NULL,  # label given in outer code
                choices = char_list,
                selected = char_list_default,
                options = list(`actions-box` = TRUE),
                multiple = TRUE
            ),
            br(),
            h4("Capital Statistics"),
            helpText(
                "These features are statistics related to appearances of",
                "uninterrupted sequences of capital letters in the email.",
                "Use the checkboxes to add and remove them from your model."
            ),
            checkboxGroupInput(
                inputId = "capsstats",
                label = NULL,  # label given in outer code
                choices = caps_list,
                selected = caps_list_default
            ),
            br(),

            h3("Decision Tree"),
            helpText(
                "These controls are for setting the hyperparameter values",
                "which partly control the structure of the decision tree.",
                "The default values we've put in should create a fairly safe",
                "tree but try changing them if you're feeling adventurous."
            ),
            br(),
            h4("Minimum Split"),
            helpText(
                "If at a given node N is below this value, that node cannot",
                "be split any further: it is a terminal node of the tree."
            ),
            sliderInput(
                inputId = "min_split",
                label = NULL,  # label given in outer code
                min = 2,       # two is the smallest that could be split
                max = 10,      # chosen to not make the models too wild
                value = 2      # defaults to not having an artifical minimum
            ),
            br(),
            h4("Minimum Bucket Size"),
            helpText(
                "If creating a given split would cause N₁ or N₂ to fall below",
                "this minimum, then that split isn't made part of the",
                "decision tree."
            ),
            sliderInput(
                inputId = "min_bucket",
                label = NULL,  # label given in outer code
                min = 1,       # can't have buckets of size zero
                max = 30,      # rpart default is minbucket = 3*minsplit
                value = 1      # defaults to not having an artifical minimum
            ),
            br(),
            h4("Maximum Tree Depth"),
            helpText(
                "Control the maximum depth that the decision tree can reach.",
                "Note that, depending on what features are being used and the",
                "values of the other parameters, you may end up with a tree",
                "much shallower than the maximum."
            ),
            sliderInput(
                inputId = "max_depth",
                label = NULL,  # label given in outer code
                min = 2,       # a min of 2 allows for at least one split
                max = 30,      # rpart can't do 31+ depth on 32-bit machines
                value = 5      # chosen to not make the default too wild
            )
        ),

        mainPanel(
            # contains the goodness assessments data
            fluidRow(
                label = NULL,
                column(6,
                    h2("Training Results"),
                    helpText(
                        "These are the measures of how good your model was",
                        "when it was ran on the training data set. Recall from",
                        "the lecture how we calculated each of these. Would",
                        "you prefer a false positive or false negative in the",
                        "context of spam detection?"
                    ),
                    # training accuracy, true positive, and true negative
                    tagAppendAttributes(
                        textOutput("training_scores"),
                        # allow linebreaks between scores, larger font here
                        style = "white-space: pre-wrap; font-size: 17px;"
                    ),
                    br(),
                    # training results table matches layout from presentation
                    tableOutput("training_table")
                ),
                column(6,
                    h2("Test Results"),
                    helpText(
                        "These are the measures of how good your model was",
                        "when it was ran on the test data set. Recall what",
                        "was said in lectures about how we interpret the",
                        "differences between measures these and the measures",
                        "from the training data."
                    ),
                    # test accuracy, true positive, and true negative
                    tagAppendAttributes(
                        textOutput("test_scores"),
                        # allow linebreaks between scores, larger font here
                        style = "white-space: pre-wrap; font-size: 17px;"
                    ),
                    br(),
                    # training results table matches layout from presentation
                    tableOutput("test_table")
                )
            ),
            # plot of the decision tree
            h2("Decision Tree"),
            helpText(
                "This is a graphical depiction of the decision tree, the model",
                "that you have created! To visualise how it works, imagine we",
                "have an additional email we wish to classify using this",
                "model. We start at the very top of the tree, somewhat",
                "counterintuitively called the 'root' node. Then we determine",
                "what the answer is to the question at that node for the email",
                "at hand. If the answer is yes we move down to the left and",
                "if no we move down to the right, following it as a flowchart.",
                "We repeat for each new node and question we reach this until",
                "we come to a node which has no edges reaching out downwards.",
                "These are called terminal nodes. If the terminal node has a",
                "zero (blue) then that means the model predicts the email",
                "isn't spam. If the terminal node has a one (red) then the",
                "model has predicted that it is spam."
            ),
            plotOutput(outputId = "tree_plot"),
            br(),

            # contains the tables with inidividal email classifications
            h3("Training Data"),
            helpText(
                "This table contains the training data classifications that",
                "were produced by the model, as compared to the true labels,",
                "where 0 is not spam and 1 is spam. The ID matches with the",
                "row in the",
                tags$a(
                    # short URL to $(REPO)/blob/master/training-data.csv
                    href = "https://git.io/Jfdgu",
                    "training data"
                ),
                "CSV file."
            ),
            dataTableOutput("training_data"),
            h3("Test Data"),
            helpText(
                "This table contains the test data classifications that were",
                "produced by the model, as compared to the true labels, where",
                "0 is not spam and 1 is spam. The ID matches with the row in",
                "the",
                tags$a(
                    # short URL to $(REPO)/blob/master/test-data.csv
                    href = "https://git.io/Jfdg2",
                    "test data"
                ),
                "CSV file."
            ),
            dataTableOutput("test_data")
        )
    )
)



options(shiny.port = 8100)  # when running locally
shinyApp(ui = ui, server = server)
