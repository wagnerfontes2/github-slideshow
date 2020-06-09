---
layout: slide
title: "Welcome to our second slide!"
---
Very weird way to edit the stupid file. The edit optin is hidden in the three dots! Your text
Use the left arrow to go back!


#
#
#
#
#
  prg <- f.data #recebe os dados normalizados por alguma das funções de normalização (ou um dos dados direto do Progenesis)
  IDs  <- colnames(prg)[1] 
  factor1 <- colnames(prg)[2] #recupera o nome do fator 1 e do fator 2
  factor2 <- colnames(prg)[3]
  accs <- colnames(prg)[-(1:3)] # recupera o código de acesso das proteínas (nomes das colunas, exceto as 3 primeiras)
  #W
  #W ART Align-and-rank data for a nonparametric ANOVA (equivalente ao não paramétrico Kruskal Wallis) para ser usado se a distribuição (dos "residuals") não for normal e tiver 2 fatores (http://depts.washington.edu/acelab/proj/art/index.html). Ver também: https://stats.stackexchange.com/questions/12151/is-there-an-equivalent-to-kruskal-wallis-one-way-test-for-a-two-way-model (outras possibilidades)
  #W
  
  library(ARTool)
  prg$rep_id = factor(prg$rep_id)
  m = art(P81605~sex*activator+(1|rep_id), data=prg)
  anm <- anova(m)
  
  ARTs <- lapply(accs, function(prot_acc) {  #cada termo de accs vai ser tratado como prot_acc neste grupo lapply
    formART <- formula(paste(prot_acc, " ~ ", factor1," * ", factor2, "+ (1|",IDs,")")) # monta o "texto" da fórmula do modelo (m) para o ART
    m.ART <- art(formART, data = prg) # aplica o teste 2-way ANOVA ao dataframe prg
    anova(m.ART)
    #pvalues.res.aov2 <- summary(res.aov2)[[1]][5] #extrai a parte do summary que tem os p-valores
    #pvalues.res.aov2 <- pvalues.res.aov2[-c(4), , drop = FALSE] #tira a coluna excedente, para ficar só com os p-valores
    #names(pvalues.res.aov2)[1] <- prot_acc # coloca o respectivo nome da proteína no nome da coluna referente ao p-valor
    #pvalues.res.aov2 # coloca isso no aovs
  })
  
