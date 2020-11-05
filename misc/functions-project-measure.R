library(humanVerseWSU);
library(Hmisc);

 
# https://texblog.org/2019/06/03/control-the-width-of-table-columns-tabular-in-latex/
# https://tex.stackexchange.com/questions/559679/error-package-array-error-illegal-pream-toke-c-used
buildLatexCorrelationTable = function(myData, 
                                myFile = paste0(getwd(),"/","table-correlation.tex"),
                                myLabel = "table:correlation",
                                myCaption = "Descriptive Statistics and Correlation Analysis",
                                myNote = "Pearson pairwise correlations are reported; \\newline a two-side test was performed to report correlation significance.",
                                myNames = colnames(myData),
                                rotateTable=TRUE,
                                rowDivider = TRUE,
                                width.table = 0.99, # percent of textwidth
                                width.names = "35mm", # width of variable names
                                space.M.SD = "1mm",
                                space.SD.corr = "5mm",
                                space.between = "2mm",
                                showOnes = "left" # options are "center" or "left" or NULL
                                )
  {

  ################## prep data ####################
    
  nrow = nrow(myData);
  ncol = ncol(myData);
  
  myCorr = rcorr( myData );
  myM = colMeans(myData, na.rm=TRUE);
  mySD = c();
  for(i in 1:ncol)
    {
    mySD = c(mySD, sd(myData[,i], na.rm=TRUE));
    }
  
  
  
  
  
  
  ################## internal helper functions ####################
  
  
  
  splitValue = function(val, digits=2)
    {
    str = as.character( round( as.numeric(val),digits) );
    tmp = strsplit(str,".",fixed=TRUE)[[1]];
    
    whole = tmp[1];
    if(whole == "0") { whole = ""; }
    if(whole == "-0") { whole = "-"; }
    
    dec = substr(tmp[2],1,digits);
    if(is.na(dec)) { dec = "0"; }
    dec = strPadRight(dec, digits, "0");
    
    paste0(" ",whole,"&.",dec,"");
    }
  
  
  getCorrelationSymbol = function(pval)
    {
    chars = c("\\dagger", "*", "**", "***");
    cuts = c(0.1, 0.05, 0.01, 0.001);
    str = "";
    for(i in 1:length(cuts))
      {
      cut = cuts[i];
      char = chars[i];
      if(pval < cut)
        {
        str = paste0("{$^{",char,"}$} "); 
        }
      }
    str;
    }
  
  
  splitCorrelation = function(myCorr, i, j, digits=2)
    {
    # print(showOnes);
    oneSpace = " \\ ";
    mySpace = paste0( rep(oneSpace, times=5), collapse="");
    # mySpace = " \\  \\  \\  \\  \\  \\  \\ ";  # do we need filler space?
    # mySpace = "";
    if(i == j)
      {
      if(is.null(showOnes))
        {
        empty = paste0(" \\multicolumn{2}{c}{",mySpace,"} ");
        return(empty);
        }
        
      if(showOnes == "center")
        {
        one = " \\multicolumn{2}{c}{1} ";
        return(one);
        }
    
        ## default
        one = " 1& ";
        return(one);
      }
    
    if(i < j)
      {
      empty = paste0(" \\multicolumn{2}{c}{",mySpace,"} ");
      return(empty);
      }
    
    # i > j ... lower triangle
    pval = myCorr$P[i,j];
    pval.s = getCorrelationSymbol(pval);
    
    myc = myCorr$r[i,j];
    myc.s = splitValue(myc,2);
    
    merged = paste0(myc.s,pval.s);
    return(merged);
    }
  ################## good to go ####################
  
  myT = "\\begin{table}[!htbp]";
  if(rotateTable) { myT = "\\begin{sidewaystable}[!htbp]"; }
  
  writeLine(myT, myFile, append=FALSE);
  
  #writeLine(paste0("\\setlength\\tabcolsep{",space.column.separator,"}"), myFile);
  
  writeLine("\\footnotesize", myFile);
  writeLine("\\centering", myFile);
  writeLine(paste0("\\caption{\\textbf{",myCaption,"}}"), myFile);
  writeLine(paste0("\\label{",myLabel,"}"), myFile);
  
  #writeLine(paste0("\\vspace{",vspace.row.separator,"}"), myFile);
  #writeLine(paste0("\\vspace{",vspace.row.separator,"}"), myFile);
  
  # let's compute the space we need ?
  
  #myT = paste0("r@{ \\ \\ } X ");
  myT = paste0("r@{ \\ \\ } ", "p{",width.names,"} ");
  # width.names
  
  myT = paste0(myT, "r@{}l" );  # M
  myT = paste0(myT, paste0("p{",space.M.SD,"} "));
  myT = paste0(myT, "r@{}l ");  # SD
  myT = paste0(myT, paste0("p{",space.SD.corr,"} "));
  
  for(i in 1:(ncol-1))
    {
    myT = paste0(myT, "r@{}l ", "p{",space.between,"} ");
    }
  # myT = paste0(myT, paste0("p{",space.end,"}"));
  #myT = paste0(myT, "{X}"); # end with right space
  #myT = paste0(myT, "{c}"); 
  myT = paste0(myT, "  r@{}l  ");
  
  writeLine(paste0("\\begin{tabularx}{",width.table,"\\textwidth}{{",myT,"}}"), myFile);
  writeLine(" & \\\\", myFile);
  writeLine("\\hline", myFile);
  writeLine(" & \\\\", myFile);
  #writeLine(paste0("\\vspace{",vspace.row.separator,"}"), myFile);
  
  myT = paste0("\\multicolumn{2}{c}{\\textbf{ }} & \\multicolumn{2}{c}{\\textbf{M}} & & \\multicolumn{2}{c}{\\textbf{SD}} & " );
  for(i in 1:(ncol-1))
    {
    myT = paste0(myT, " & \\multicolumn{2}{c}{\\textbf{",i,"}} & ");
    }
  myT = paste0(myT, " & \\\\ ");
  
  writeLine(myT, myFile);
  #writeLine(paste0("\\vspace{",vspace.row.separator,"}"), myFile);
  writeLine(" & \\\\", myFile);
  writeLine("\\hline", myFile);
  writeLine(" & \\\\", myFile);
  
  for(i in 1:ncol)
    {
    myR = paste0("\\textbf{",i,"} & \\textbf{",myNames[i],"} & ", 
                 splitValue(myM[i],1), " &  & ",splitValue(mySD[i],2), " & ");
    
    for(j in 1:(ncol-1))
      {
      myR = paste0(myR, " & ", splitCorrelation(myCorr,i,j,2), " & ");
      }
    myR = paste0(myR, " & \\\\ ");
    writeLine(myR, myFile);
    if(rowDivider) { writeLine(" & \\\\", myFile); }
    #print(i);
    #print(myR);
    }
  
  if(!rowDivider) { writeLine(" & \\\\", myFile); }  # already have a space likely
  
  #writeLine("\\end{tabularx}", myFile);
  
  #writeLine(paste0("\\begin{tabularx}{",width.table,"\\textwidth}{ { r@{}l","p{",space.dagger,"}","r@{}l","p{",space.dagger,"}","r@{}l","p{",space.dagger,"}","r@{}l {X} } }"), myFile);
  
  #myT = paste0(myT, paste0("p{",space.dagger,"}"));
  
  writeLine("\\hline", myFile);
  writeLine(" & \\\\", myFile);
  
  writeLine(paste0("\\multicolumn{",(6+1+3*(ncol-1)),"}{p{",round((0.9*width.table),2),"\\textwidth}}{ ",
                    " \\footnotesize { \\begin{hangparas}{0.75in}{1} \\textbf{\\underline{Notes}:} \\ \\ ",
                    myNote, "  \\end{hangparas} } }  & \\\\  "), myFile);
  
  # writeLine(" & \\\\ ", myFile);
  
  writeLine(paste0( "\\multicolumn{",(6+1+3*(ncol-1)),"}{p{",round((0.9*width.table),2),"\\textwidth}}{ ",
                    " {\\tiny {$^{\\dagger} p < .10$} }  { \ \ \ \ }",
                    " {\\tiny        {$^{*} p < .05$} }  { \ \ \ \ }",
                    " {\\tiny       {$^{**} p < .01$} }  { \ \ \ \ }",
                    " {\\tiny      {$^{***} p < .001$} } { \ \ \ \ }     } & \\\\ "), myFile);
  
  writeLine(" & \\\\", myFile);
  
  #writeLine("\\end{tabularx}", myFile);
  
  #writeLine(paste0("\\begin{tabularx}{",width.table,"\\textwidth}{ { p{",width.table,"\\textwidth} } }"), myFile);
  #writeLine(paste0("\\begin{tabularx}{",width.table,"\\textwidth}{ { l{",width.table,"\\textwidth} } }"), myFile);
  
 
  
  writeLine("\\hline", myFile);
  
  writeLine("\\end{tabularx}", myFile);
  myT = "\\end{table}";
  if(rotateTable) { myT = "\\end{sidewaystable}"; }
  writeLine(myT, myFile);

  }



