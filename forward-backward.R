
forward_p_value = function(dtframe, response, exclude, alpha=0.05)
{
  # Specify the columns that you want to exclude from your model
  exclude = c(exclude, response)
  cols = names(dtframe)
  min_val = 0
  sel_cols = ""
  model = NULL
  while (min_val < alpha)
  {
    min_val = 100
    selected_col = ""
    for (col in cols)
    {
      if (!(col %in% exclude))
      {
        col2add = if (sel_cols == "") col else paste(" + ", col)
        formula = paste(response, " ~ ", sel_cols, col2add)
        model = lm(formula = formula, data = dtframe)
        s = summary(model)
        # print(s$coefficients)
        col2retrive = col
        if (is.factor(dtframe[,col])) {
          levels = levels(dtframe[,col])
          if (length(levels) > 1)
            col2retrive =paste(col,levels[2],sep = "")
        }
        p_val = s$coefficients[col2retrive,4]
        adj_r2 = s$adj.r.squared
        if (p_val < min_val)
        {
          min_val = p_val
          selected_col = col
        }
        #print(s$coefficients)
        cat(format(paste("+",col), width = 15), " Adj. R2:", format(adj_r2,width = 15, justify = 'right'),"t-value:", ... = format(s$coefficients[2,2],width = 15, nsmall = 3, digits = 3), " p-value:", p_val, "\n")
      }
    }
    if (min_val  < alpha)
    {
      cat("\n==> Adding ",selected_col, ": p_vlaue:", min_val, "\n\n")
      sel_cols = if (sel_cols != "") paste(sel_cols, " + ", selected_col) else selected_col
    }
    else
    {
       cat("\n==> No column to add (based on the significant level of", alpha, ")\n")
    }
    exclude = c(exclude, selected_col)
  }
  formula = paste(response, " ~ ", sel_cols)
  model = lm(formula = formula, data = dtframe)
  return(model)
}

backward_adj_r2 = function(dtframe, response, exclude)
{
  # Specify the columns that you want to exclude from your model
  exclude = c(exclude, response)
  cols = names(dtframe)
  col_list = c()
  for (col in cols)
  {
    if (!(col %in% exclude))
    { 
      col_list = c(col_list, col)
    }
  }
  all_cols = c(cols,"")
  max_adj_r2 = 0
  old_adj_r2 = -1
  model = NULL
  while (max_adj_r2 > old_adj_r2)
  {
    selected_col = ""
    old_adj_r2 = max_adj_r2
    for (col in all_cols)
    {
      if (!(col %in% exclude))
      {
        sel_cols = setdiff(col_list, col)
        sel_cols = paste(sel_cols,collapse = "+")
        # print(sel_cols)
        model = lm(formula = paste(response, " ~ ",sel_cols) , data = dtframe)
        s = summary(model)
        adj_r2 = s$adj.r.squared
        if (adj_r2 > max_adj_r2)
        {
          max_adj_r2 = adj_r2
          selected_col = col
        }
        #print(s$coefficients)
        col = if(col=="") "NONE" else col
        cat(format(paste("-",col), width = 15), " Adj. R2:", format(adj_r2,width = 15, justify = 'right'),"\n")
      }
    }
    selected_col = if(selected_col=="") "NONE" else selected_col
    cat("\n==> Removing ",selected_col,": Adj R2:",max_adj_r2, "\n\n")
    col_list = setdiff(col_list, selected_col)
    exclude = c(exclude, selected_col)
  }
  return(model)
}

